# Gateway Routes Specification

**Related Documentation**:
- `docs/GATEWAY_HEADERS_PROPAGATION.md` - Current header propagation status
- `docs/GATEWAY_ADMIN_GRPC_STATUS.md` - Admin gRPC implementation status
- `docs/GATEWAY_CONFLICT_CONTRACT.md` - Error handling priority matrix

Complete specification of all HTTP routes exposed by C-Gateway, including NATS subject mapping, correlation fields, error handling, and rate limiting.

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: CP2-LC

## Table of Contents

1. [Route Inventory](#route-inventory)
2. [HTTP → NATS Mapping](#http--nats-mapping)
3. [Correlation Fields](#correlation-fields)
4. [Error Handling](#error-handling)
5. [Rate Limiting](#rate-limiting)
6. [Request/Response Examples](#requestresponse-examples)

---

## Route Inventory

### Health & Metrics Endpoints

| Method | Path | Description | Auth Required | Rate Limited |
|--------|------|-------------|---------------|--------------|
| `GET` | `/health` or `/_health` | Health check endpoint | ❌ No | ❌ No |
| `GET` | `/_metrics` | JSON metrics endpoint | ❌ No | ❌ No |
| `GET` | `/metrics` | Prometheus metrics endpoint | ❌ No | ❌ No |

**Implementation**: `handle_health()`, `handle_metrics_json()`, `handle_metrics_request()`

### Registry Endpoints

| Method | Path | Description | Auth Required | Rate Limited |
|--------|------|-------------|---------------|--------------|
| `POST` | `/api/v1/registry/blocks/:type/:version` | Create block manifest | ✅ Yes | ✅ Yes (200 req/min) |
| `PUT` | `/api/v1/registry/blocks/:type/:version` | Update block manifest | ✅ Yes | ✅ Yes (200 req/min) |
| `DELETE` | `/api/v1/registry/blocks/:type/:version` | Delete block manifest | ✅ Yes | ✅ Yes (200 req/min) |

**Implementation**: `handle_registry_write_common()`, `handle_registry_delete()`

**Rate Limit**: `RL_ENDPOINT_REGISTRY_BLOCKS` (default: 200 req/min, configurable via `GATEWAY_RATE_LIMIT_REGISTRY_BLOCKS`)

### Routing Endpoints

| Method | Path | Description | Auth Required | Rate Limited |
|--------|------|-------------|---------------|--------------|
| `POST` | `/api/v1/routes/decide` | Request routing decision | ✅ Yes | ✅ Yes (50 req/min) |
| `GET` | `/api/v1/routes/decide/:messageId` | Get cached decision | ✅ Yes | ❌ No |

**Implementation**: `handle_decide()`, `handle_get_decision()`

**Rate Limit**: `RL_ENDPOINT_ROUTES_DECIDE` (default: 50 req/min, configurable via `GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT`)

### Message Endpoints

| Method | Path | Description | Auth Required | Rate Limited |
|--------|------|-------------|---------------|--------------|
| `POST` | `/api/v1/messages` | Create and publish message | ✅ Yes | ✅ Yes (100 req/min) |
| `GET` | `/api/v1/messages/:id` | Get message by ID | ✅ Yes | ❌ No |
| `PUT` | `/api/v1/messages/:id` | Update message | ✅ Yes | ❌ No |
| `DELETE` | `/api/v1/messages/:id` | Delete message | ✅ Yes | ❌ No |
| `GET` | `/api/v1/messages/stream` | SSE stream for real-time updates | ✅ Yes | ❌ No |

**Implementation**: `handle_decide()` (for POST), inline handlers for GET/PUT/DELETE, `sse_register_client()` for SSE

**Rate Limit**: `RL_ENDPOINT_MESSAGES` (default: 100 req/min, configurable via `GATEWAY_RATE_LIMIT_MESSAGES`)

---

## HTTP → NATS Mapping

### POST /api/v1/routes/decide

**NATS Subject**: `beamline.router.v1.decide`  
**Pattern**: Request-Reply (synchronous)  
**Function**: `nats_request_decide(route_req_json, resp_buf, sizeof(resp_buf))`

**Subject Configuration**:
- Environment variable: `ROUTER_DECIDE_SUBJECT` (default: `beamline.router.v1.decide`)
- See: `apps/c-gateway/src/nats_client_real.c:10`

**Request Transformation**:
1. Gateway receives HTTP POST with JSON body
2. `validate_decide_request()` validates required fields (`version`, `tenant_id`, `request_id`, `task`)
3. `build_route_request_json()` transforms HTTP DTO to NATS JSON format:
   - Extracts `tenant_id` from header (`X-Tenant-ID`) or body
   - Extracts `trace_id` from header (`X-Trace-ID`) or body
   - Extracts `run_id` from body (CP2+)
   - Builds `RouteRequest` JSON with `message`, `policy_id`, `context`
4. Publishes to NATS subject `beamline.router.v1.decide`
5. Waits for Router response (timeout: 5000ms, configurable via `ROUTER_REQUEST_TIMEOUT_MS`)

**Response Transformation**:
1. Router returns JSON response (success or error)
2. `map_router_error_status()` maps Router error codes to HTTP status:
   - `invalid_request` → 400
   - `unauthorized` → 401
   - `policy_not_found` → 404
   - `decision_failed` / `internal` → 500
3. Gateway returns HTTP response with mapped status code

**Error Handling**:
- NATS connection failure → `503 Service Unavailable` with `SERVICE_UNAVAILABLE` error code
- Router timeout → `503 Service Unavailable` with `SERVICE_UNAVAILABLE` error code
- Invalid request body → `400 Bad Request` with `invalid_request` error code

**See**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md#router-service-beamlineflowv1` for complete NATS contract

### GET /api/v1/routes/decide/:messageId

**NATS Subject**: `beamline.router.v1.get_decision`  
**Pattern**: Request-Reply (synchronous)  
**Function**: `nats_request_get_decision(tenant_id, message_id, resp_buf, sizeof(resp_buf))`

**Subject Configuration**:
- Environment variable: `ROUTER_GET_DECISION_SUBJECT` (default: `beamline.router.v1.get_decision`)
- See: `apps/c-gateway/src/nats_client_real.c:11`

**Request Transformation**:
1. Gateway extracts `message_id` from URL path parameter
2. Gateway extracts `tenant_id` from header (`X-Tenant-ID`) or context
3. Builds minimal JSON: `{"message_id": "..."}`
4. Publishes to NATS subject `beamline.router.v1.get_decision`
5. Waits for Router response (timeout: 5000ms)

**Response Transformation**:
- Same as `POST /api/v1/routes/decide` (uses `map_router_error_status()`)

**Error Handling**:
- Missing `message_id` → `400 Bad Request` with `invalid_request` error code
- Missing `tenant_id` → `400 Bad Request` with `invalid_request` error code
- NATS connection failure → `503 Service Unavailable` with `internal` error code

---

## Correlation Fields

### Field Sources

All correlation fields are extracted from HTTP headers or request body, with the following priority:

| Field | HTTP Header | Request Body | Auto-Generated | Notes |
|-------|-------------|--------------|----------------|-------|
| `tenant_id` | `X-Tenant-ID` | `tenant_id` | ❌ No | **Required** for all API endpoints |
| `trace_id` | `X-Trace-ID` | `trace_id` | ✅ Yes (CP2+) | Optional in CP1, required in CP2+ |
| `request_id` | ❌ No | `request_id` | ✅ Yes (CP2+) | Generated if missing (CP2+) |
| `run_id` | ❌ No | `run_id` | ❌ No | CP2+ only, optional |
| `flow_id` | ❌ No | `flow_id` | ❌ No | CP2+ only, optional |
| `step_id` | ❌ No | `step_id` | ❌ No | CP2+ only, optional |
| `idempotency_key` | ❌ No | `idempotency_key` | ❌ No | CP2+ only, optional |

**Implementation**: `apps/c-gateway/src/http_server.c:1864-1887` (header extraction), `build_route_request_json()` (body extraction)

### Field Validation

**Required Fields** (enforced at Gateway level):
- `tenant_id`: Must be present in header (`X-Tenant-ID`) or body
  - Missing → `400 Bad Request` with `invalid_request` error code
  - See: `apps/c-gateway/src/http_server.c:2176-2184`

**Optional Fields** (CP2+):
- `trace_id`: Extracted from header or body, auto-generated if missing (CP2+)
- `request_id`: Extracted from body, auto-generated if missing (CP2+)
- `run_id`, `flow_id`, `step_id`, `idempotency_key`: Extracted from body if present

**Field Format**:
- All fields must comply with `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md`
- `tenant_id`: String, max 64 characters
- `trace_id`: W3C Trace Context format (optional in CP1, required in CP2+)
- `request_id`: UUID v4 format (optional in CP1, required in CP2+)

### Field Propagation

**Gateway → Router**:
- All correlation fields are included in NATS JSON payload
- Fields are passed through `build_route_request_json()` to Router
- See: `apps/c-gateway/src/http_server.c:1356-1518`

**Router → Gateway**:
- Router includes correlation fields in response (`context.request_id`, `context.trace_id`)
- Gateway preserves correlation fields in HTTP response
- See: `apps/c-gateway/src/http_server.c:1520-1571` (error mapping)

---

## Error Handling

### Error Code Mapping (Router → Gateway → HTTP)

| Router Error Code | Gateway Error Code | HTTP Status | Description |
|-------------------|-------------------|-------------|-------------|
| `invalid_request` | `invalid_request` | `400 Bad Request` | Invalid request format or missing required fields |
| `unauthorized` | `unauthorized` | `401 Unauthorized` | Missing or invalid credentials |
| `policy_not_found` | `policy_not_found` | `404 Not Found` | Policy not found |
| `decision_failed` | `internal` | `500 Internal Server Error` | Router internal error |
| `internal` | `internal` | `500 Internal Server Error` | Router internal error |
| `rate_limit_exceeded` | `rate_limit_exceeded` | `429 Too Many Requests` | Rate limit exceeded (Gateway-level) |

**Implementation**: `apps/c-gateway/src/http_server.c:1520-1571` (`map_router_error_status()`)

**Router Intake Error Codes** (CP2+):
- Router intake errors are mapped via `router_intake_error_handler:map_intake_error_to_gateway_code/1`:
  - **Note**: Error codes in Erlang are atoms (lowercase: `schema_validation_failed`), but string values in JSON/HTTP are UPPERCASE (`"SCHEMA_VALIDATION_FAILED"`).
  - `schema_validation_failed` (atom) → `"SCHEMA_VALIDATION_FAILED"` (string) → `invalid_request` → 400
  - `version_unsupported` (atom) → `"VERSION_UNSUPPORTED"` (string) → `invalid_request` → 400
  - `correlation_fields_invalid` (atom) → `"CORRELATION_FIELDS_INVALID"` (string) → `invalid_request` → 400
  - `tenant_forbidden` (atom) → `"TENANT_FORBIDDEN"` (string) → `unauthorized` → 401
  - `idempotency_violation` (atom) → `"IDEMPOTENCY_VIOLATION"` (string) → `invalid_request` → 400
  - `internal_validation_error` (atom) → `"INTERNAL_VALIDATION_ERROR"` (string) → `internal` → 500

**Complete Error Code Chain**:

| Router Intake Error Code | Gateway Error Code | HTTP Status | Description |
|--------------------------|-------------------|-------------|-------------|
| `SCHEMA_VALIDATION_FAILED` | `invalid_request` | `400 Bad Request` | Schema validation failed |
| `VERSION_UNSUPPORTED` | `invalid_request` | `400 Bad Request` | Unsupported version |
| `CORRELATION_FIELDS_INVALID` | `invalid_request` | `400 Bad Request` | Invalid correlation fields |
| `TENANT_FORBIDDEN` | `unauthorized` | `401 Unauthorized` | Tenant ACL check failed |
| `IDEMPOTENCY_VIOLATION` | `invalid_request` | `400 Bad Request` | Idempotency violation |
| `INTERNAL_VALIDATION_ERROR` | `internal` | `500 Internal Server Error` | Internal validation error |

**Error Response Format** (Router includes `intake_error_code`):
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",  // Gateway-compatible code
    "message": "Schema validation failed: missing tenant_id",
    "intake_error_code": "SCHEMA_VALIDATION_FAILED",  // Original intake code (for debugging)
    "details": {}
  },
  "context": {
    "request_id": "req-123",
    "trace_id": "trace-456"
  }
}
```

**See**: 
- `apps/otp/router/src/router_intake_error_handler.erl:364-373` for Router error code mapping
- `apps/c-gateway/src/http_server.c:1520-1571` for Gateway HTTP status mapping

### Error Response Format

**Standard Error Response** (JSON):
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "missing tenant_id",
    "details": {}
  },
  "context": {
    "request_id": "req_123",
    "trace_id": "trace_xyz"
  }
}
```

**Rate Limit Error Response** (JSON):
```json
{
  "ok": false,
  "error": {
    "code": "rate_limit_exceeded",
    "message": "Rate limit exceeded for endpoint /api/v1/routes/decide",
    "details": {
      "endpoint": "/api/v1/routes/decide",
      "limit": 50,
      "retry_after_seconds": 60
    }
  },
  "context": {
    "request_id": "req_123",
    "trace_id": "trace_xyz",
    "tenant_id": "tenant_abc"
  }
}
```

**Rate Limit Headers** (429 responses):
- `X-RateLimit-Limit`: Maximum requests per window
- `X-RateLimit-Remaining`: Remaining requests in current window
- `X-RateLimit-Reset`: Unix timestamp when window resets
- `Retry-After`: Seconds until retry is allowed

**Implementation**: `apps/c-gateway/src/http_server.c:846-927` (`send_rate_limit_error()`)

### Gateway-Level Errors

| Error | HTTP Status | Error Code | Description |
|-------|-------------|------------|-------------|
| Missing `X-Tenant-ID` header | `400 Bad Request` | `invalid_request` | Required header missing |
| Missing `Authorization` header (if `GATEWAY_AUTH_REQUIRED=true`) | `401 Unauthorized` | `unauthorized` | Auth required but missing |
| Invalid JSON body | `400 Bad Request` | `invalid_request` | Malformed JSON |
| Route not found | `404 Not Found` | `invalid_request` | HTTP route not recognized |
| NATS unavailable | `503 Service Unavailable` | `SERVICE_UNAVAILABLE` | NATS connection failure or timeout |

---

## Rate Limiting

### Rate Limit Configuration

**Environment Variables**:
- `GATEWAY_RATE_LIMIT_TTL_SECONDS`: Window size in seconds (default: 60)
- `GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT`: Limit for `/api/v1/routes/decide` (default: 50 req/min)
- `GATEWAY_RATE_LIMIT_MESSAGES`: Limit for `/api/v1/messages` (default: 100 req/min)
- `GATEWAY_RATE_LIMIT_REGISTRY_BLOCKS`: Limit for registry endpoints (default: 200 req/min)
- `GATEWAY_RATE_LIMIT_GLOBAL`: Global limit (default: 1000 req/min)

**Algorithm**: Fixed-window rate limiting (CP1)
- Window resets every `TTL_SECONDS`
- Counters are per-endpoint (not per-tenant in CP1)
- In-memory storage (not distributed)

**Implementation**: `apps/c-gateway/src/http_server.c:407-576` (rate limit functions)

**See**: `docs/GATEWAY_RATE_LIMITING.md` for complete rate limiting specification

### Rate Limit Enforcement Points

| Endpoint | Rate Limit ID | Default Limit | Configurable |
|----------|---------------|---------------|--------------|
| `POST /api/v1/routes/decide` | `RL_ENDPOINT_ROUTES_DECIDE` | 50 req/min | ✅ Yes |
| `POST /api/v1/messages` | `RL_ENDPOINT_MESSAGES` | 100 req/min | ✅ Yes |
| `POST /api/v1/registry/blocks/:type/:version` | `RL_ENDPOINT_REGISTRY_BLOCKS` | 200 req/min | ✅ Yes |
| `PUT /api/v1/registry/blocks/:type/:version` | `RL_ENDPOINT_REGISTRY_BLOCKS` | 200 req/min | ✅ Yes |
| `DELETE /api/v1/registry/blocks/:type/:version` | `RL_ENDPOINT_REGISTRY_BLOCKS` | 200 req/min | ✅ Yes |

**Enforcement**: Rate limit check occurs **before** request processing (early rejection)

**Metrics**: Rate limit hits are tracked in `gateway_rate_limit_hits_total` and `gateway_rate_limit_exceeded_total`

---

## Error Handling Priority: Rate Limiting vs Router Intake Errors

**CRITICAL**: Gateway implements a **two-stage error handling process** with strict priority to ensure rate limiting and Router intake errors never conflict.

### Priority Order

1. **Rate Limiting (429) - HIGHEST PRIORITY**
   - Checked **BEFORE** Router validation
   - If rate limit exceeded → Gateway returns `429 Too Many Requests` **immediately**
   - Router is **NOT called** when rate limit exceeded
   - Error code: `rate_limit_exceeded`
   - Response includes rate limit headers (`X-RateLimit-*`, `Retry-After`)
   - **No `intake_error_code`** (Router not involved)

2. **Router Intake Validation (400/401/500) - LOWER PRIORITY**
   - Checked **AFTER** rate limit passes
   - If Router intake validation fails → Gateway returns Router error (400/401/500)
   - Error codes: `invalid_request`, `unauthorized`, `internal`
   - Response includes `intake_error_code` (Router error code for debugging)
   - Rate limit headers may be present, but `Retry-After` should not be relevant

### Error Response Format Distinction

**Rate Limit Error (429)**:
```json
{
  "ok": false,
  "error": {
    "code": "rate_limit_exceeded",
    "message": "Rate limit exceeded for endpoint /api/v1/routes/decide",
    "details": {
      "endpoint": "/api/v1/routes/decide",
      "limit": 50,
      "retry_after_seconds": 45
    }
  },
  "context": {
    "request_id": "...",
    "trace_id": "...",
    "tenant_id": "..."
  }
}
```

**Router Intake Error (400/401/500)**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Schema validation failed: missing tenant_id",
    "intake_error_code": "SCHEMA_VALIDATION_FAILED",
    "details": {
      "field": "tenant_id",
      "reason": "required"
    }
  },
  "context": {
    "request_id": "...",
    "trace_id": "...",
    "tenant_id": "..."
  }
}
```

### Key Invariants

- ✅ Rate limiting is **always checked first** (before Router call)
- ✅ Router intake errors **only occur after** rate limit passes
- ✅ Error types **never conflict** (mutually exclusive)
- ✅ A request cannot return both 429 and 400/401/500
- ✅ Rate limit error responses **never include** `intake_error_code`
- ✅ Router intake error responses **always include** `intake_error_code` (when Router provides it)

### Implementation

**Gateway Code** (`apps/c-gateway/src/http_server.c`):
- Rate limit check: `rate_limit_check_routes_decide()` (line ~2213) - called **BEFORE** Router
- Router call: `handle_decide()` (line ~2241) - called **AFTER** rate limit passes
- Error response: `send_rate_limit_error()` (line ~846) for 429, Router error response for 400/401/500

**See**: `docs/GATEWAY_ROUTER_ERROR_HANDLING_PRIORITY.md` for complete specification and examples.

---

## Request/Response Examples

### POST /api/v1/routes/decide

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
Host: api.beamline.example.com
Content-Type: application/json
X-Tenant-ID: tenant_abc123
X-Trace-ID: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01
Authorization: Bearer <api_key>

{
  "version": "1",
  "tenant_id": "tenant_abc123",
  "request_id": "req_123",
  "trace_id": "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",
  "run_id": "run_456",
  "task": {
    "type": "text.generate",
    "payload": {
      "prompt": "Hello, world!"
    }
  },
  "policy_id": "default",
  "context": {
    "user_id": "user_001"
  }
}
```

**NATS Request** (transformed by Gateway):
```json
{
  "version": "1",
  "tenant_id": "tenant_abc123",
  "request_id": "req_123",
  "trace_id": "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",
  "run_id": "run_456",
  "message": {
    "message_id": null,
    "message_type": null,
    "payload": null,
    "metadata": null
  },
  "policy_id": "default",
  "context": {
    "user_id": "user_001"
  }
}
```

**Success Response**:
```http
HTTP/1.1 200 OK
Content-Type: application/json

{
  "ok": true,
  "decision": {
    "provider_id": "provider-sticky",
    "reason": "sticky",
    "priority": 100,
    "expected_latency_ms": 200,
    "expected_cost": 0.001,
    "metadata": {
      "session_key": "user_001"
    }
  },
  "context": {
    "request_id": "req_123",
    "trace_id": "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
  }
}
```

**Error Response** (Router intake error):
```http
HTTP/1.1 400 Bad Request
Content-Type: application/json

{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "schema validation failed: missing required field 'task'",
    "details": {
      "intake_error_code": "SCHEMA_VALIDATION_FAILED"
    }
  },
  "context": {
    "request_id": "req_123",
    "trace_id": "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
  }
}
```

**Rate Limit Error Response**:
```http
HTTP/1.1 429 Too Many Requests
Content-Type: application/json
X-RateLimit-Limit: 50
X-RateLimit-Remaining: 0
X-RateLimit-Reset: 1700000000
Retry-After: 60

{
  "ok": false,
  "error": {
    "code": "rate_limit_exceeded",
    "message": "Rate limit exceeded for endpoint /api/v1/routes/decide",
    "details": {
      "endpoint": "/api/v1/routes/decide",
      "limit": 50,
      "retry_after_seconds": 60
    }
  },
  "context": {
    "request_id": "req_123",
    "trace_id": "trace_xyz",
    "tenant_id": "tenant_abc123"
  }
}
```

---

## References

- `docs/GATEWAY_ROUTES.md` - High-level route overview
- `docs/GATEWAY_RATE_LIMITING.md` - Rate limiting specification
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - NATS subject mapping
- `docs/ARCHITECTURE/api-registry.md` - REST API DTO specifications
- `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` - Core message fields specification
- `apps/c-gateway/src/http_server.c` - C-Gateway HTTP server implementation
- `apps/c-gateway/src/nats_client_real.c` - NATS client implementation
- `apps/otp/router/src/router_intake_error_handler.erl` - Router error code mapping

