# Gateway Conflict Contract

**Date**: 2025-01-27  
**Status**: ✅ Formal Specification  
**Purpose**: Define explicit priority matrix for conflicting error sources in Gateway to ensure deterministic, consistent, and testable behavior.

## 1. Overview and Goals

This document formalizes the **conflict contract** for `c-gateway`, establishing an **explicit priority matrix** between competing error sources/states. This ensures that behavior in any conflict situation is:

- **Deterministic**: Same inputs always produce same outputs
- **Consistent**: Uniform behavior across all Gateway routes
- **Testable**: Verifiable through integration tests and logs

**Scope**:
- All HTTP endpoints in Gateway (`/api/v1/*`, service endpoints `/_health`, `/metrics`)
- Interactions **Gateway ↔ Router** and **Gateway ↔ external clients**
- Conflicts between rate limiting, authentication/authorization, request validation, Router errors, and internal Gateway errors

## 2. Basic Invariants

These invariants are already established and are formalized as part of the conflict contract:

### 2.1. Rate Limiting Always Checked First

- Gateway checks rate limit **BEFORE** any Router calls
- When rate limit exceeded, Router is **NOT called**
- Response: `429`, `error.code = "rate_limit_exceeded"`, **no `intake_error_code`**

### 2.2. Router Intake Errors Only After Successful Rate Limit

- When rate limit passes, request reaches Router
- Router intake errors → HTTP `400/401/500`, `error.code in {invalid_request, unauthorized, internal}`, **`intake_error_code` required**

### 2.3. Error Types Are Mutually Exclusive

- One HTTP response = one dominant cause
- Mixed responses like "both 429 and intake_error" are impossible; top-level priority dominates

## 3. Conflict Sources

All types of conflicting signals that Gateway may encounter for a single request:

1. **RL** – Rate limiting (Gateway-level)
2. **AUTH_GW** – Authentication/authorization errors at Gateway level
   - Invalid/missing token, forbidden tenant, CORS/Origin policy violations, etc.
3. **REQ_GW** – Local request validation at Gateway level
   - Missing required HTTP headers/parameters, incorrect Content-Type, invalid JSON, etc.
4. **INTAKE_ROUTER** – Router intake errors
   - See Router Intake Errors → Gateway Error Codes mapping in existing documentation
5. **RUNTIME_ROUTER** – Router runtime errors
   - Failures during route processing, timeouts, or internal Router errors
6. **INTERNAL_GW** – Internal Gateway errors
   - OOM, IO errors, unexpected exceptions, etc.

## 4. Priority Matrix

### 4.1. Priority Order

**Strict priority order** when multiple signals occur simultaneously:

1. **RL** – Rate limiting (429) - **HIGHEST PRIORITY**
2. **AUTH_GW** – Authentication/authorization errors (401/403)
3. **REQ_GW** – Request format/validation errors at Gateway level (400)
4. **INTAKE_ROUTER** – Router intake errors (400/401/500)
5. **RUNTIME_ROUTER** – Router runtime errors (500/503)
6. **INTERNAL_GW** – Internal Gateway errors (500) - **LOWEST PRIORITY**

**Rule**:  
If multiple problems potentially exist for one request, **only the highest priority type** is reflected in the client response.

### 4.2. Conflict Priority Matrix

| # | RL | AUTH_GW | REQ_GW | INTAKE_ROUTER | RUNTIME_ROUTER | INTERNAL_GW | HTTP Status | `error.code` | `intake_error_code` | Source | Comment |
|---|----|---------|--------|---------------|----------------|-------------|-------------|--------------|---------------------|--------|---------|
| 1 | 1  | *       | *      | *             | *              | *           | 429 | `rate_limit_exceeded` | `null` | Gateway | Router not called |
| 2 | 0  | 1       | *      | *             | *              | *           | 401/403 | `unauthorized` | `null` | Gateway | Auth error before Router |
| 3 | 0  | 0       | 1      | *             | *              | *           | 400 | `invalid_request` | `null` | Gateway | Bad HTTP/JSON etc. |
| 4 | 0  | 0       | 0      | 1             | *              | *           | 400/401/500 | See intake mapping | Router code | Router→Gateway | Router intake error |
| 5 | 0  | 0       | 0      | 0             | 1              | *           | 500/503 | `internal`/`unavailable` | Router code (optional) | Router→Gateway | Runtime error in Router |
| 6 | 0  | 0       | 0      | 0             | 0              | 1           | 500 | `internal` | `null` | Gateway | Internal Gateway error |

**Legend**:
- `1` – Error of this type occurred
- `*` – May occur, but ignored due to priority
- `0` – Error of this type did not occur (or was ignored)

### 4.3. Error Type Codes

For logging and observability, each conflict type has a standardized code:

| Priority | Error Type Code | Description |
|----------|----------------|-------------|
| 1 | `rate_limit` | Rate limiting exceeded |
| 2 | `auth_gateway` | Gateway authentication/authorization error |
| 3 | `request_gateway` | Gateway request validation error |
| 4 | `router_intake` | Router intake validation error |
| 5 | `router_runtime` | Router runtime/processing error |
| 6 | `internal_gateway` | Internal Gateway error |

## 5. Response Format Requirements

### 5.1. Unified JSON Error Response Structure

For all errors (429, 4xx, 5xx), the structure is unified:

```json
{
  "ok": false,
  "error": {
    "code": "<gateway_error_code>",
    "message": "<human_readable_message>",
    "intake_error_code": "<router_intake_code_or_null>",
    "details": { /* structured fields by error type */ }
  },
  "context": {
    "request_id": "...",
    "trace_id": "...",
    "tenant_id": "..."
  }
}
```

**Rules**:
- `intake_error_code`:
  - **Always present** (but may be `null`) in all error responses
  - For pure Gateway errors (`RL`, `AUTH_GW`, `REQ_GW`, `INTERNAL_GW`) = `null`
  - For Router errors (`INTAKE_ROUTER`, `RUNTIME_ROUTER`) = specific Router code
- `details`:
  - **Required field** (minimum empty object `{}`), never completely omitted
  - Format depends on error subtype (see examples below)

### 5.2. Error Code Mapping by Conflict Type

| Conflict Type | `error.code` | HTTP Status | `intake_error_code` |
|---------------|--------------|-------------|---------------------|
| RL | `rate_limit_exceeded` | 429 | `null` |
| AUTH_GW | `unauthorized` | 401/403 | `null` |
| REQ_GW | `invalid_request` | 400 | `null` |
| INTAKE_ROUTER | See [Router Intake Mapping](#router-intake-error-mapping) | 400/401/500 | Router code |
| RUNTIME_ROUTER | `internal` or `unavailable` | 500/503 | Router code (optional) |
| INTERNAL_GW | `internal` | 500 | `null` |

### 5.3. Router Intake Error Mapping

| Router Intake Error | Gateway Error Code | HTTP Status | Description |
|---------------------|-------------------|-------------|-------------|
| `SCHEMA_VALIDATION_FAILED` | `invalid_request` | 400 | Invalid message schema (missing fields, invalid format) |
| `VERSION_UNSUPPORTED` | `invalid_request` | 400 | Unsupported protocol version |
| `CORRELATION_FIELDS_INVALID` | `invalid_request` | 400 | Invalid correlation fields (UUID format, dependencies) |
| `TENANT_FORBIDDEN` | `unauthorized` | 401 | Tenant not in allowlist |
| `IDEMPOTENCY_VIOLATION` | `invalid_request` | 400 | Duplicate request with conflicting data |
| `INTERNAL_VALIDATION_ERROR` | `internal` | 500 | Internal Router validation error |

**Reference**: See `docs/GATEWAY_ROUTER_ERROR_HANDLING_PRIORITY.md` for complete mapping.

## 6. Logging and Observability Requirements

### 6.1. Required Log Fields

For each error response, Gateway JSON logs **must** include:

- `severity` - `ERROR` or `WARN` (depending on error type)
- `error_type` - One of: `rate_limit`, `auth_gateway`, `request_gateway`, `router_intake`, `router_runtime`, `internal_gateway`
- `http_status` - HTTP status code (429, 400, 401, 403, 500, 503)
- `gateway_error_code` - Gateway error code (e.g., `rate_limit_exceeded`, `unauthorized`)
- `intake_error_code` - Router intake error code (or `null`)
- `conflict_priority_level` - Integer 1-6 (as in matrix)
- Standard fields: `request_id`, `trace_id`, `tenant_id`, `timestamp`, `component`, `subsystem`

### 6.2. Log Examples by Conflict Type

#### Priority 1: Rate Limiting (RL)

```json
{
  "timestamp": "2025-01-27T12:00:00.123Z",
  "level": "WARN",
  "component": "c-gateway",
  "subsystem": "rate_limiter",
  "severity": "WARN",
  "error_type": "rate_limit",
  "http_status": 429,
  "gateway_error_code": "rate_limit_exceeded",
  "intake_error_code": null,
  "conflict_priority_level": 1,
  "request_id": "req-123",
  "trace_id": "trace-456",
  "tenant_id": "tenant-789",
  "message": "Rate limit exceeded for endpoint /api/v1/routes/decide",
  "details": {
    "endpoint": "/api/v1/routes/decide",
    "limit": 50,
    "remaining": 0,
    "retry_after_seconds": 45
  }
}
```

#### Priority 2: Authentication Gateway (AUTH_GW)

```json
{
  "timestamp": "2025-01-27T12:00:00.123Z",
  "level": "WARN",
  "component": "c-gateway",
  "subsystem": "auth",
  "severity": "WARN",
  "error_type": "auth_gateway",
  "http_status": 401,
  "gateway_error_code": "unauthorized",
  "intake_error_code": null,
  "conflict_priority_level": 2,
  "request_id": "req-123",
  "trace_id": "trace-456",
  "tenant_id": null,
  "message": "Missing or invalid Authorization header"
}
```

#### Priority 3: Request Gateway (REQ_GW)

```json
{
  "timestamp": "2025-01-27T12:00:00.123Z",
  "level": "WARN",
  "component": "c-gateway",
  "subsystem": "request_validation",
  "severity": "WARN",
  "error_type": "request_gateway",
  "http_status": 400,
  "gateway_error_code": "invalid_request",
  "intake_error_code": null,
  "conflict_priority_level": 3,
  "request_id": "req-123",
  "trace_id": "trace-456",
  "tenant_id": null,
  "message": "Invalid Content-Type: expected application/json"
}
```

#### Priority 4: Router Intake (INTAKE_ROUTER)

```json
{
  "timestamp": "2025-01-27T12:00:00.123Z",
  "level": "ERROR",
  "component": "c-gateway",
  "subsystem": "router_intake",
  "severity": "ERROR",
  "error_type": "router_intake",
  "http_status": 400,
  "gateway_error_code": "invalid_request",
  "intake_error_code": "SCHEMA_VALIDATION_FAILED",
  "conflict_priority_level": 4,
  "request_id": "req-123",
  "trace_id": "trace-456",
  "tenant_id": "tenant-789",
  "message": "Schema validation failed: missing tenant_id"
}
```

#### Priority 5: Router Runtime (RUNTIME_ROUTER)

```json
{
  "timestamp": "2025-01-27T12:00:00.123Z",
  "level": "ERROR",
  "component": "c-gateway",
  "subsystem": "router_runtime",
  "severity": "ERROR",
  "error_type": "router_runtime",
  "http_status": 500,
  "gateway_error_code": "internal",
  "intake_error_code": "ROUTER_PROCESSING_ERROR",
  "conflict_priority_level": 5,
  "request_id": "req-123",
  "trace_id": "trace-456",
  "tenant_id": "tenant-789",
  "message": "Router processing failed: timeout"
}
```

#### Priority 6: Internal Gateway (INTERNAL_GW)

```json
{
  "timestamp": "2025-01-27T12:00:00.123Z",
  "level": "ERROR",
  "component": "c-gateway",
  "subsystem": "internal",
  "severity": "ERROR",
  "error_type": "internal_gateway",
  "http_status": 500,
  "gateway_error_code": "internal",
  "intake_error_code": null,
  "conflict_priority_level": 6,
  "request_id": "req-123",
  "trace_id": "trace-456",
  "tenant_id": null,
  "message": "Internal Gateway error: memory allocation failed"
}
```

## 7. Examples by Matrix Row

### Row 1: RL Dominates (Rate Limit Exceeded)

**Scenario**: Request with exceeded rate limit AND invalid JSON simultaneously.

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
X-Tenant-ID: tenant-123
Content-Type: application/json

{ invalid json
```

**Response** (429):
```json
{
  "ok": false,
  "error": {
    "code": "rate_limit_exceeded",
    "message": "Rate limit exceeded for endpoint /api/v1/routes/decide",
    "intake_error_code": null,
    "details": {
      "endpoint": "/api/v1/routes/decide",
      "limit": 50,
      "retry_after_seconds": 45
    }
  },
  "context": {
    "request_id": "req-001",
    "trace_id": "trace-001",
    "tenant_id": "tenant-123"
  }
}
```

**Note**: Router is **NOT called** - rate limit check happens first, invalid JSON is ignored.

### Row 2: AUTH_GW Dominates (Authentication Error)

**Scenario**: Expired/invalid token, but request body and rate limit are valid.

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
Authorization: Bearer expired_token
X-Tenant-ID: tenant-123
Content-Type: application/json

{
  "version": "1",
  "tenant_id": "tenant-123",
  "request_id": "req-002",
  "task": {"type": "text.generate", "payload": {}}
}
```

**Response** (401):
```json
{
  "ok": false,
  "error": {
    "code": "unauthorized",
    "message": "Invalid or expired authorization token",
    "intake_error_code": null,
    "details": {}
  },
  "context": {
    "request_id": "req-002",
    "trace_id": "trace-002",
    "tenant_id": "tenant-123"
  }
}
```

**Note**: Router is **NOT called** - authentication check happens before Router.

### Row 3: REQ_GW Dominates (Request Validation Error)

**Scenario**: Invalid Content-Type or malformed JSON; rate limit and authorization OK.

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
X-Tenant-ID: tenant-123
Content-Type: text/plain

invalid json content
```

**Response** (400):
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Invalid Content-Type: expected application/json",
    "intake_error_code": null,
    "details": {
      "expected": "application/json",
      "received": "text/plain"
    }
  },
  "context": {
    "request_id": "req-003",
    "trace_id": "trace-003",
    "tenant_id": "tenant-123"
  }
}
```

**Note**: Router is **NOT called** - request validation happens before Router.

### Row 4: INTAKE_ROUTER (Router Intake Error)

**Scenario**: All Gateway-level checks pass, but Router returns `SCHEMA_VALIDATION_FAILED`.

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
X-Tenant-ID: tenant-123
Content-Type: application/json

{
  "version": "1",
  "request_id": "req-004",
  "task": {"type": "text.generate", "payload": {}}
}
```

**Response** (400):
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
    "request_id": "req-004",
    "trace_id": "trace-004",
    "tenant_id": "tenant-123"
  }
}
```

**Note**: Router **WAS called** and returned intake error.

### Row 5: RUNTIME_ROUTER (Router Runtime Error)

**Scenario**: Valid request, Router encounters runtime error during processing.

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
X-Tenant-ID: tenant-123
Content-Type: application/json

{
  "version": "1",
  "tenant_id": "tenant-123",
  "request_id": "req-005",
  "task": {"type": "text.generate", "payload": {}}
}
```

**Response** (500):
```json
{
  "ok": false,
  "error": {
    "code": "internal",
    "message": "Router processing failed: timeout",
    "intake_error_code": "ROUTER_PROCESSING_ERROR",
    "details": {}
  },
  "context": {
    "request_id": "req-005",
    "trace_id": "trace-005",
    "tenant_id": "tenant-123"
  }
}
```

**Note**: Router **WAS called** but failed during processing.

### Row 6: INTERNAL_GW (Internal Gateway Error)

**Scenario**: Artificially triggered internal Gateway error (e.g., mock OOM).

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
X-Tenant-ID: tenant-123
Content-Type: application/json

{
  "version": "1",
  "tenant_id": "tenant-123",
  "request_id": "req-006",
  "task": {"type": "text.generate", "payload": {}}
}
```

**Response** (500):
```json
{
  "ok": false,
  "error": {
    "code": "internal",
    "message": "Internal Gateway error: memory allocation failed",
    "intake_error_code": null,
    "details": {}
  },
  "context": {
    "request_id": "req-006",
    "trace_id": "trace-006",
    "tenant_id": "tenant-123"
  }
}
```

**Note**: Internal Gateway error, Router may or may not have been called.

## 8. Implementation Requirements

### 8.1. Code Structure

Gateway code **must** have clear sections (comments/code structure) showing compliance with this matrix:

1. **Rate Limiting Check** (Priority 1) - Before all other checks
2. **Authentication/Authorization Check** (Priority 2) - After rate limit, before Router
3. **Request Validation** (Priority 3) - After auth, before Router
4. **Router Call** - Only if all Gateway checks pass
5. **Router Error Handling** (Priority 4-5) - After Router call
6. **Internal Error Handling** (Priority 6) - Catch-all for Gateway errors

### 8.2. Logging Implementation

All error responses **must** log with:
- `error_type` field (from matrix)
- `conflict_priority_level` field (1-6)
- `intake_error_code` field (or `null`)
- Standard observability fields

## 9. Testing Requirements

### 9.1. Matrix Coverage Tests

For each matrix row (1-6), **at least 1 integration test** is required:

- **Row 1 (RL)**: Test with exceeded limit + invalid JSON → expect 429
- **Row 2 (AUTH_GW)**: Test with invalid token + valid body → expect 401/403
- **Row 3 (REQ_GW)**: Test with invalid Content-Type + valid auth → expect 400
- **Row 4 (INTAKE_ROUTER)**: Test with valid Gateway checks + Router intake error → expect 400/401/500 with `intake_error_code`
- **Row 5 (RUNTIME_ROUTER)**: Test with valid request + Router runtime error → expect 500/503
- **Row 6 (INTERNAL_GW)**: Test with artificially triggered Gateway error → expect 500

### 9.2. Conflict Consistency Test

Separate test module `conflict_matrix_consistency` that:
- Generates all scenarios where 2+ error sources could compete (e.g., RL+REQ_GW, AUTH_GW+REQ_GW)
- Verifies that actual response matches **exactly one** type according to matrix
- Checks `error_type` and `conflict_priority_level` in logs

## 10. References

- `docs/GATEWAY_ROUTER_ERROR_HANDLING_PRIORITY.md` - Existing error handling priority documentation
- `docs/GATEWAY_RATE_LIMITING.md` - Rate limiting specification
- `docs/GATEWAY_ROUTES_SPEC.md` - Gateway routes specification
- `docs/ARCHITECTURE/api-registry.md` - Error code mapping and API registry
- `apps/c-gateway/src/http_server.c` - Gateway implementation

## 11. Change History

**v1.0 (2025-01-27)**:
- Initial conflict contract formalization
- Priority matrix definition
- Logging requirements
- Testing requirements

