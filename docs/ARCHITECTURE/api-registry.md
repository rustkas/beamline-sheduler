---
version: 1.1
authors:
  - Agent 2: Architecture/Tech Lead
last_update: 2025-11-18T11:30:00Z
status: approved
changelog:
  - 2025-11-18: Migrated REST API DTOs from TypeScript to JSON format for C-Gateway
---

# API Registry

## Purpose

Centralized registry of all API endpoints (REST, gRPC, NATS) with their DTO structures, error formats, and versioning.

**Related Documentation**:
- `docs/GATEWAY_HEADERS_PROPAGATION.md` - Current header propagation status (actual implementation)
- `docs/GATEWAY_ADMIN_GRPC_STATUS.md` - Admin gRPC implementation status (actual vs planned)
- `docs/GATEWAY_CONFLICT_CONTRACT.md` - Error handling priority matrix

## Core Message Fields

**CRITICAL**: All messages must comply with the **Core Message Fields Specification** (`docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md`).

**Required fields** (see specification for complete details):
- `tenant_id` (required) - Tenant identifier
- `version` / `schema_version` (required) - Schema version
- `trace_id` (optional in CP1, required in CP2+) - Distributed tracing ID
- `idempotency_key` (optional in CP1, required in CP2+) - Idempotency key
- `run_id`, `flow_id`, `step_id` (CP2+ only) - Multi-step workflow identifiers

**See**: `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` for complete field specifications, validation rules, format requirements, and test requirements.

## REST API (C-Gateway)

### Base URL

```
Production: https://api.beamline.example.com/api/v1
Staging: https://api-staging.beamline.example.com/api/v1
```

### Authentication

All endpoints require authentication:

```
Authorization: Bearer <api_key>
```

Or via token headers:
- `access-token`: API access token
- `client`: Client identifier
- `uid`: User identifier
- `token-type`: Bearer

### Correlation Fields

All requests must include (see `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` for complete specification):
- `X-Tenant-ID`: Tenant identifier (required) - Maps to `tenant_id` in request body
- `X-Trace-ID`: Trace identifier (optional in CP1, required in CP2+, auto-generated if not provided) - Maps to `trace_id` in request body

**Core Fields**: All REST DTOs must comply with `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` (format, validation rules, field relationships).

### Endpoints

#### POST /api/v1/messages

**Purpose**: Send message for routing

**Request DTO** (JSON):
```json
{
  "message_id": "string (UUID)",
  "message_type": "chat | completion | embedding",
  "payload": "string (Base64 encoded content)",
  "metadata": {},
  "policy_id": "string (optional)"
}
```

**Response DTO** (JSON):
```json
{
  "message_id": "string",
  "provider_id": "string",
  "reason": "weighted | sticky | fallback | policy",
  "priority": 0,
  "expected_latency_ms": 0,
  "expected_cost": 0.0,
  "currency": "USD",
  "trace_id": "string"
}
```

**Error DTO** (JSON):
```json
{
  "error": {
    "code": "string",
    "message": "string",
    "details": {},
    "trace_id": "string",
    "timestamp": "ISO 8601 string"
  }
}
```

**Error Codes**:
- `INVALID_REQUEST`: Invalid request format (400)
- `UNAUTHORIZED`: Missing or invalid API key (401)
- `FORBIDDEN`: Insufficient permissions (403)
- `NOT_FOUND`: Resource not found (404)
- `RATE_LIMIT_EXCEEDED`: Rate limit exceeded (429)
- `INTERNAL_ERROR`: Internal server error (500)
- `SERVICE_UNAVAILABLE`: Service temporarily unavailable (503)

**Extension Error Codes** (CP2-LC):
- `extension_not_found`: Extension not found in registry (404)
- `extension_timeout`: Extension call timed out (504)
- `validator_blocked`: Validator blocked the request (403)
- `post_processor_failed`: Post-processor failed (500)
- `extension_unavailable`: Extension unavailable (circuit breaker open) (503)
- `extension_error`: Generic extension invocation error (500)

**Extension Error Response Format**:
```json
{
  "ok": false,
  "error": {
    "code": "extension_not_found",
    "message": "Extension not found: normalize_text",
    "details": {
      "extension_id": "normalize_text",
      "error_type": "extension_not_found",
      "timestamp": "2025-01-27T12:00:00Z",
      "policy_id": "default",
      "tenant_id": "tenant_abc"
    }
  },
  "context": {
    "request_id": "req_123",
    "trace_id": "trace_xyz"
  }
}
```

#### POST /api/v1/routes/decide

**Purpose**: Request routing decision for a message

**CP1 Scope**: ✅ This endpoint is part of Gateway CP1-smoke verification. See `docs/archive/dev/GATEWAY_CP1_SMOKE_DETAILED_CHECKLIST.md` for verification scenarios.

**Request DTO** (JSON):

| Field | Type | Required | Description | Source |
|-------|------|----------|-------------|--------|
| `version` | string | ✅ Yes | Protocol version, must be `"1"` | Request body |
| `tenant_id` | string | ✅ Yes | Tenant identifier (or from `X-Tenant-ID` header) | Request body or header |
| `request_id` | string | ✅ Yes | Request correlation ID (UUID) | Request body |
| `trace_id` | string | ❌ Optional | Trace identifier (or from `X-Trace-ID` header, auto-generated if missing) | Request body or header |
| `message_id` | string | ❌ Optional | Message identifier (UUID) | Request body |
| `message_type` | string | ❌ Optional | Message type: `"chat"`, `"completion"`, `"embedding"` | Request body |
| `payload` | object | ❌ Optional | Message payload (JSON object) | Request body |
| `metadata` | object | ❌ Optional | Message metadata (key-value pairs) | Request body |
| `policy_id` | string | ❌ Optional | Routing policy identifier | Request body |
| `context` | object | ❌ Optional | Request context (key-value pairs) | Request body |
| `task` | object | ✅ Yes | Task wrapper (NATS-specific, validated but not forwarded to Router) | Request body |
| `task.type` | string | ✅ Yes | Task type (validated) | Request body |
| `task.payload` | object | ✅ Yes | Task payload (validated) | Request body |

**Request DTO Example**:
```json
{
  "version": "1",
  "tenant_id": "tenant_abc",
  "request_id": "req_123",
  "trace_id": "trace_xyz",
  "message_id": "msg_456",
  "message_type": "chat",
  "payload": {"content": "Hello"},
  "metadata": {"source": "gateway"},
  "policy_id": "default",
  "context": {"user_id": "user_123"},
  "task": {
    "type": "route",
    "payload": {}
  }
}
```

**HTTP Headers**:
- `X-Tenant-ID`: required (if not in request body)
- `X-Trace-ID`: optional (auto-generated if not provided)
- `Authorization: Bearer <api_key>`: required (if auth enabled)

**Request Validation** (Implementation: `apps/c-gateway/src/http_server.c:validate_decide_request()`):
- ✅ `version` must be string `"1"`
- ✅ `tenant_id` must be non-empty string
- ✅ `request_id` must be non-empty string
- ✅ `task` must be object with `type` (string) and `payload` (object)

**NATS Subject**: `beamline.router.v1.decide` (configurable via `ROUTER_DECIDE_SUBJECT` env var)

**NATS Payload Transformation** (Implementation: `apps/c-gateway/src/http_server.c:build_route_request_json()`):
Gateway transforms REST request to NATS JSON format:
- Top-level fields: `version`, `tenant_id`, `request_id`, `trace_id` (from request or headers)
- `message` object: Contains `message_id`, `message_type`, `payload`, `metadata` (from request body)
- `policy_id`: Passed through if present
- `context`: Passed through if present

**See**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md#router-service-beamlineflowv1` for NATS payload structure.

**Response DTO** (JSON, 200 OK):

Gateway forwards Router response directly without transformation. Router response format:

```json
{
  "ok": true,
  "decision": {
    "provider_id": "string",
    "reason": "weighted | sticky | fallback | policy | priority | random",
    "priority": 0,
    "expected_latency_ms": 0,
    "expected_cost": 0.0,
    "metadata": {"key": "value"}
  },
  "context": {
    "request_id": "string",
    "trace_id": "string"
  }
}
```

**Error DTO** (JSON):

Gateway error format (Implementation: `apps/c-gateway/src/http_server.c:send_error_response()`):
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request | SERVICE_UNAVAILABLE | internal",
    "message": "string",
    "details": {}
  },
  "context": {
    "request_id": "string",
    "trace_id": "string"
  }
}
```

Router error format (forwarded directly):
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request | unauthorized | policy_not_found | decision_failed | internal",
    "message": "string",
    "details": {"key": "value"}
  },
  "context": {
    "request_id": "string",
    "trace_id": "string"
  }
}
```

**HTTP Status Code Mapping** (Implementation: `apps/c-gateway/src/http_server.c:map_router_error_status()`):

**Error Code Chain**: Router Intake Error → Gateway Error Code → HTTP Status

| Router Intake Error Code | Gateway Error Code (Router) | HTTP Status | Description |
|--------------------------|----------------------------|-------------|-------------|
| `SCHEMA_VALIDATION_FAILED` | `invalid_request` | `400 Bad Request` | Schema validation failed (protobuf decode, missing fields) |
| `VERSION_UNSUPPORTED` | `invalid_request` | `400 Bad Request` | Unsupported protocol version |
| `CORRELATION_FIELDS_INVALID` | `invalid_request` | `400 Bad Request` | Invalid correlation fields (UUID, W3C Trace Context) |
| `TENANT_FORBIDDEN` | `unauthorized` | `401 Unauthorized` | Tenant not allowed (ACL check failed) |
| `IDEMPOTENCY_VIOLATION` | `invalid_request` | `400 Bad Request` | Idempotency key violation (duplicate detected) |
| `INTERNAL_VALIDATION_ERROR` | `internal` | `500 Internal Server Error` | Internal validation error (exception during validation) |
| `policy_not_found` | `policy_not_found` | `404 Not Found` | Routing policy not found |
| `decision_failed` | `decision_failed` | `500 Internal Server Error` | Routing decision failed |
| `internal` | `internal` | `500 Internal Server Error` | Router internal error |
| `SERVICE_UNAVAILABLE` | `SERVICE_UNAVAILABLE` | `503 Service Unavailable` | NATS/Router unavailable (Gateway-level error) |

**Mapping Implementation**:
1. **Router Intake → Gateway Code**: `router_intake_error_handler:map_intake_error_to_gateway_code/1` (see `apps/otp/router/src/router_intake_error_handler.erl:364-373`)
2. **Gateway Code → HTTP Status**: `map_router_error_status()` (see `apps/c-gateway/src/http_server.c:1520-1571`)

**Error Response Format** (Router includes `intake_error_code` for debugging):
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",  // Gateway-compatible code (mapped from intake code)
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

**Note**: Router maps intake error codes to Gateway-compatible error codes before sending to Gateway. Gateway then maps Gateway error codes to HTTP status codes. The original `intake_error_code` is preserved in the error response for debugging and detailed error tracking.

**Error Handling Priority** (CRITICAL):

Gateway implements a **two-stage error handling process** with strict priority:

1. **Rate Limiting Check (Gateway-level)** - **HIGHEST PRIORITY**
   - Checked **BEFORE** Router validation
   - If rate limit exceeded → Gateway returns `429 Too Many Requests` **immediately**
   - Router is **NOT called** when rate limit exceeded
   - Error code: `rate_limit_exceeded`
   - Response includes rate limit headers (`X-RateLimit-*`, `Retry-After`)
   - **No `intake_error_code`** (Router not involved)

2. **Router Intake Validation (Router-level)** - **LOWER PRIORITY**
   - Checked **AFTER** rate limit passes
   - If Router intake validation fails → Gateway returns Router error (400/401/500)
   - Error codes: `invalid_request`, `unauthorized`, `internal`
   - Response includes `intake_error_code` (Router error code for debugging)
   - Rate limit headers may be present, but `Retry-After` should not be relevant

**Key Invariants**:
- ✅ Rate limiting is **always checked first** (before Router call)
- ✅ Router intake errors **only occur after** rate limit passes
- ✅ Error types **never conflict** (mutually exclusive)
- ✅ A request cannot return both 429 and 400/401/500

**Conflict Contract**: For complete priority matrix including all error sources (rate limiting, authentication, request validation, Router errors, internal errors), see `docs/GATEWAY_CONFLICT_CONTRACT.md`.

**See**: `docs/GATEWAY_ROUTER_ERROR_HANDLING_PRIORITY.md` for complete specification and examples.

**Error Codes**:
- `rate_limit_exceeded`: Rate limit exceeded (429) - Gateway-level error, checked BEFORE Router
- `invalid_request`: Invalid request format (400) - Gateway validation or Router validation
- `SERVICE_UNAVAILABLE`: Router/NATS unavailable (503) - Gateway-level error
- `internal`: Internal server error (500) - Gateway or Router error
- `unauthorized`: Missing or invalid authentication (401) - Router error
- `policy_not_found`: Routing policy not found (404) - Router error
- `decision_failed`: Routing decision failed (500) - Router error

**Implementation**:
- Handler: `apps/c-gateway/src/http_server.c:handle_decide()` (lines 1191-1274)
- Validation: `apps/c-gateway/src/http_server.c:validate_decide_request()` (lines 831-885)
- NATS Build: `apps/c-gateway/src/http_server.c:build_route_request_json()` (lines 887-1031)
- Error Handler: `apps/c-gateway/src/http_server.c:send_error_response()` (lines 782-820)

**Tests**:
- Smoke script: `scripts/gateway_router_cp1_smoke.sh` (happy path, validation error)
- Integration tests: `tests/integration/c-gateway-routes.test.ts` (TODO: add decide endpoint tests)

#### GET /api/v1/messages/:message_id

**Purpose**: Get message processing status

**Response DTO** (JSON):
```json
{
  "message_id": "string",
  "status": "pending | routing | processing | completed | failed",
  "provider_id": "string (optional)",
  "started_at": "ISO 8601 (optional)",
  "completed_at": "ISO 8601 (optional)",
  "error": {
    "code": "string",
    "message": "string"
  },
  "trace_id": "string"
}
```

#### GET /api/v1/policies

**Purpose**: List routing policies

**Query Parameters**:
- `tenant_id`: Filter by tenant (required)
- `active`: Filter active policies only (optional, default: true)

**Response DTO** (JSON):
```json
{
  "policies": [
    {
      "policy_id": "string",
      "name": "string",
      "version": "string",
      "active": true,
      "created_at": "ISO 8601",
      "updated_at": "ISO 8601"
    }
  ],
  "total": 0
}
```

#### GET /api/v1/policies/:tenant_id/:policy_id

**Description**: Get a specific policy by tenant ID and policy ID.

**Request**:
```
GET /api/v1/policies/{tenant_id}/{policy_id}
Authorization: Bearer <api_key>
```

**Response** (200 OK):
```json
{
  "policy_id": "default",
  "tenant_id": "tenant_dev",
  "weights": {...},
  "pre": [...],
  "validators": [...],
  "post": [...]
}
```

#### GET /api/v1/policies/:tenant_id/:policy_id/complexity

**Description**: Get pipeline complexity assessment for a policy.

**⚠️ ADMIN-ONLY**: This endpoint requires admin authentication. See [Authentication](#authentication) and [Admin Endpoints](#admin-endpoints) sections.

**Request**:
```
GET /api/v1/policies/{tenant_id}/{policy_id}/complexity
Authorization: Bearer <api_key>
```

**Response** (200 OK):
```json
{
  "complexity": {
    "total_extensions": 3,
    "pre_count": 1,
    "validators_count": 1,
    "post_count": 1,
    "complexity_score": 45,
    "complexity_level": "medium",
    "estimated_latency_ms": 90,
    "recommended_limits": {
      "max_total": 4,
      "max_pre": 2,
      "max_validators": 2,
      "max_post": 2
    },
    "warnings": [
      "Pipeline has 5 extensions, recommended maximum is 4. This may cause high latency."
    ],
    "recommendations": [
      "Consider reducing total extensions to 4 or fewer for optimal performance"
    ]
  }
}
```

**Complexity Levels**:
- `low`: Score < 30
- `medium`: Score 30-60
- `high`: Score 60-80
- `very_high`: Score >= 80

**Error Responses**:
- `404 Not Found`: Policy not found
- `401 Unauthorized`: Missing or invalid API key

#### GET /api/v1/policies/:policy_id

**Purpose**: Get routing policy

**Response DTO** (JSON):
```json
{
  "policy_id": "string",
  "name": "string",
  "version": "string",
  "policy_json": {
    "version": "string",
    "defaults": {
      "timeout_ms": 0,
      "retry_count": 0,
      "retry_backoff_ms": 0
    },
    "escalate_on": [],
    "weights": {},
    "fallback": {
      "provider": "string",
      "conditions": []
    },
    "sticky": {
      "enabled": true,
      "session_key": "string",
      "ttl_seconds": 0
    }
  },
  "active": true,
  "created_at": "ISO 8601",
  "updated_at": "ISO 8601"
}
```

#### GET /api/v1/extensions/health

**Purpose**: Get health metrics for all extensions.

**⚠️ ADMIN-ONLY**: This endpoint requires admin authentication. See [Authentication](#authentication) and [Admin Endpoints](#admin-endpoints) sections.

**Request**: No body required.

**Response DTO** (JSON):
```json
{
  "health": {
    "extension_id_1": {
      "extension_id": "extension_id_1",
      "status": "healthy|degraded|unhealthy",
      "last_success_ms": 1234567890,
      "last_failure_ms": 0,
      "success_count": 100,
      "failure_count": 0,
      "success_rate": 1.0,
      "avg_latency_ms": 25.5,
      "p50_latency_ms": 20.0,
      "p95_latency_ms": 50.0,
      "p99_latency_ms": 100.0,
      "last_latency_ms": 30.0,
      "circuit_breaker_state": "closed|open|half_open",
      "circuit_breaker_opened_at_ms": 0,
      "updated_at_ms": 1234567890
    }
  }
}
```

**Error DTO** (JSON):
```json
{
  "error": {
    "code": "SERVICE_UNAVAILABLE",
    "message": "Router or NATS unavailable",
    "trace_id": "uuid"
  }
}
```

**Implementation**: `apps/c-gateway/src/http_server.c` → `handle_extensions_health()`

#### GET /api/v1/extensions/circuit-breakers

**Purpose**: Get circuit breaker states for all extensions.

**⚠️ ADMIN-ONLY**: This endpoint requires admin authentication. See [Authentication](#authentication) and [Admin Endpoints](#admin-endpoints) sections.

**Request**: No body required.

**Response DTO** (JSON):
```json
{
  "states": {
    "extension_id_1": {
      "extension_id": "extension_id_1",
      "state": "closed|open|half_open",
      "opened_at_ms": 0
    }
  }
}
```

**Note**: `states` is a map (object) keyed by `extension_id`, not an array.

**Error DTO**: Same as `/api/v1/extensions/health`

**Implementation**: `apps/c-gateway/src/http_server.c` → `handle_circuit_breakers()`

#### POST /api/v1/policies/dry-run

**Purpose**: Execute dry-run of extension pipeline.

**⚠️ ADMIN-ONLY**: This endpoint requires admin authentication. See [Authentication](#authentication) and [Admin Endpoints](#admin-endpoints) sections.

**Request DTO** (JSON):
```json
{
  "tenant_id": "tenant-123",
  "policy_id": "policy-456",
  "payload": {
    "message": "test message"
  },
  "dry_run": true
}
```

**Response DTO** (JSON):
```json
{
  "ok": true,
  "result": {
    "decision": {
      "provider_id": "openai",
      "reason": "weighted",
      "priority": 1
    },
    "executed_extensions": [
      {
        "extension_id": "pre_processor_1",
        "type": "pre",
        "status": "success",
        "latency_ms": 10
      }
    ],
    "final_payload": {
      "message": "processed message"
    }
  }
}
```

**Error DTO** (JSON):
```json
{
  "ok": false,
  "error": {
    "code": "POLICY_NOT_FOUND",
    "message": "Policy 'tenant-123/policy-456' not found",
    "trace_id": "uuid"
  }
}
```

**Implementation**: `apps/c-gateway/src/http_server.c` → `handle_dry_run_pipeline()`

#### GET /api/v1/usage

**Purpose**: Usage metrics

**Query Parameters**:
- `tenant_id`: Filter by tenant (required)
- `start_date`: Start date (ISO 8601, optional)
- `end_date`: End date (ISO 8601, optional)
- `provider_id`: Filter by provider (optional)

**Response DTO** (JSON):
```json
{
  "tenant_id": "string",
  "period": {
    "start": "ISO 8601",
    "end": "ISO 8601"
  },
  "metrics": {
    "total_requests": 0,
    "total_tokens": 0,
    "total_cost": 0.0,
    "currency": "USD",
    "by_provider": [
      {
        "provider_id": "string",
        "requests": 0,
        "tokens": 0,
        "cost": 0.0
      }
    ]
  }
}
```

### SSE Endpoints

#### GET /api/v1/stream/messages

**Purpose**: Server-Sent Events stream for real-time message updates

**Query Parameters**:
- `tenant_id`: Filter by tenant (required)
- `message_id`: Filter by message (optional)

**Event Format**:
```
event: message_update
data: {"message_id":"uuid","status":"processing","provider_id":"openai","trace_id":"uuid"}

event: error
data: {"error":{"code":"INTERNAL_ERROR","message":"...","trace_id":"uuid"}}
```

## gRPC API

### Router Service

**Package**: `beamline.flow.v1`  
**Service**: `Router`

#### Decide

**Request DTO** (Protobuf):
```protobuf
// Package: beamline.flow.v1
message RouteRequest {
  Message message = 1;             // Nested message
  string policy_id = 2;           // Optional
  map<string, string> context = 3; // Optional
}

message Message {
  string message_id = 1;
  string tenant_id = 2;
  string trace_id = 3;            // Optional
  string message_type = 4;
  bytes payload = 5;
  map<string, string> metadata = 6; // Optional
  int64 timestamp_ms = 7;         // Optional
}
```

**Response DTO** (Protobuf):
```protobuf
// Package: beamline.flow.v1
message RouteDecision {
  string provider_id = 1;
  string reason = 2;              // "weighted" | "sticky" | "fallback" | "priority" | "random"
  int32 priority = 3;              // 0-100
  int64 expected_latency_ms = 4;
  double expected_cost = 5;
  map<string, string> metadata = 6; // Optional
}
```

**Proto File**: `proto/beamline/flow/v1/flow.proto`  
**Service**: `Router.Decide(RouteRequest) returns (RouteDecision)`

**Error Format** (gRPC Status):
- `INVALID_ARGUMENT`: Invalid request (code 3)
- `UNAUTHENTICATED`: Missing authentication (code 16)
- `PERMISSION_DENIED`: Insufficient permissions (code 7)
- `NOT_FOUND`: Resource not found (code 5)
- `RESOURCE_EXHAUSTED`: Rate limit exceeded (code 8)
- `INTERNAL`: Internal server error (code 13)
- `UNAVAILABLE`: Service unavailable (code 14)

### Provider Service

**Package**: `beamline.provider.v1`  
**Service**: `Provider`

#### Invoke

**Request DTO** (Protobuf):
```protobuf
// Package: beamline.provider.v1
message ProviderRequest {
  string provider_id = 1;
  string message_id = 2;
  string tenant_id = 3;
  string trace_id = 4;            // Optional
  bytes body = 5;
  map<string, string> parameters = 6; // Optional
  map<string, string> metadata = 7;   // Optional
  int64 timestamp_ms = 8;        // Optional
}
```

**Response DTO** (Protobuf):
```protobuf
// Package: beamline.provider.v1
message ProviderResponse {
  string request_id = 1;
  string status = 2;             // "success" | "error" | "timeout"
  bytes body = 3;
  map<string, string> metadata = 4; // Optional
  int64 latency_ms = 5;
  double cost = 6;
  string error_code = 7;        // Optional
  string error_message = 8;      // Optional
  int64 timestamp_ms = 9;
}
```

**Proto File**: `proto/beamline/provider/v1/provider.proto`  
**Service**: `Provider.Invoke(ProviderRequest) returns (ProviderResponse)`

#### InvokeStream

**Request DTO**: Same as `Invoke`

**Response DTO** (Stream):
```protobuf
// Package: beamline.provider.v1
message StreamChunk {
  string request_id = 1;
  bytes data = 2;
  bool done = 3;
  map<string, string> metadata = 4; // Optional
}
```

**Proto File**: `proto/beamline/provider/v1/provider.proto`  
**Service**: `Provider.InvokeStream(ProviderRequest) returns (stream StreamChunk)`

## Extension Services (NATS-based)

**Note**: Extensions are NATS-based services, not gRPC. They use JSON over NATS request-reply pattern.

**Subject Pattern**: `beamline.ext.{type}.{extension_id}.{version}`

**Types**:
- `pre` - Pre-processor extensions
- `validate` - Validator extensions
- `post` - Post-processor extensions
- `provider` - Custom provider extensions (uses `beamline.provider.{provider_id}.{version}` pattern)

### Pre/Post-processor Extension DTOs

#### Request DTO (JSON over NATS)

**Implementation**: `router_extension_invoker:build_request_payload/2`

```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "payload": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Original text",
    "metadata": {
      "channel": "telegram"
    }
  },
  "metadata": {
    "lang": "en",
    "policy_id": "policy-456"
  }
}
```

**Fields**:
- `trace_id` (optional) - End-to-end trace identifier
- `tenant_id` (optional) - Tenant identifier
- `payload` (required) - Message payload to process
- `metadata` (optional) - Request context and metadata

#### Response DTO (JSON over NATS)

**Implementation**: `router_decider:execute_pre_processors/3`, `router_decider:execute_post_processors/3`

```json
{
  "payload": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Processed text",
    "metadata": {
      "channel": "telegram",
      "normalized": "true"
    }
  },
  "metadata": {
    "lang": "en",
    "detected_lang": "en"
  }
}
```

**Fields**:
- `payload` (optional) - Processed message payload (defaults to original if missing)
- `metadata` (optional) - Enriched context metadata (merged into context if present)

**Router Behavior**:
- Router extracts `payload` and merges `metadata` into context
- Pipeline continues with processed payload and enriched context

### Validator Extension DTOs

#### Request DTO (JSON over NATS)

Same as Pre/Post-processor Request DTO (see above).

#### Response DTO (JSON over NATS)

**Implementation**: `router_decider:execute_validators/3`

**Success Response**:
```json
{
  "status": "ok"
}
```

**Reject Response**:
```json
{
  "status": "reject",
  "reason": "pii_detected",
  "details": {
    "field": "payload",
    "pattern": "credit_card"
  }
}
```

**Fields**:
- `status` (required) - `"ok"` or `"reject"`
- `reason` (optional) - Rejection reason (when `status = "reject"`)
- `details` (optional) - Additional rejection details (when `status = "reject"`)

**Router Behavior**:
- `status = "ok"` (or missing) → Router continues pipeline
- `status = "reject"` → Router applies `on_fail` behavior from policy:
  - `on_fail = "block"` → Fail-closed (stop request, return error)
  - `on_fail = "warn"` → Fail-open with logging (continue, log warning)
  - `on_fail = "ignore"` → Fail-open (continue, ignore result)

### Custom Provider Extension DTOs

**Subject Pattern**: `beamline.provider.{provider_id}.{version}`

#### Request DTO (JSON over NATS, CP2-style)

```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "provider_id": "my_crm_summarizer",
  "prompt": "User message + context",
  "parameters": {
    "max_tokens": 512
  },
  "context": {
    "conversation_id": "conv-1",
    "customer_id": "cust-42"
  }
}
```

#### Response DTO (JSON over NATS, CP2-style)

```json
{
  "provider_id": "my_crm_summarizer",
  "output": "Summarized answer...",
  "usage": {
    "prompt_tokens": 120,
    "completion_tokens": 80
  },
  "metadata": {
    "source": "crm"
  }
}
```

**See**: `docs/EXTENSIONS_API.md` for complete contract specification

## NATS Subjects

### Request-Reply Subjects

#### beamline.router.v1.decide

**Purpose**: Router decision requests  
**Request Format**: JSON (matches `RouteRequest` protobuf structure)  
**Response Format**: JSON (matches `RouteDecision` protobuf structure)  
**Timeout**: 5 seconds (configurable via policy)

**Correlation**:
- `tenant_id`: From request metadata
- `trace_id`: From request metadata or auto-generated

#### beamline.provider.v1.invoke

**Purpose**: Provider invocation requests  
**Request Format**: JSON (matches `ProviderRequest` protobuf structure)  
**Response Format**: JSON (matches `ProviderResponse` protobuf structure)  
**Timeout**: Policy-based (default: 30 seconds)

### Pub-Sub Subjects

#### beamline.ingress.v1.message - DEPRECATED

> **Status**: ⚠️ **DEPRECATED** - Not part of core components. Use C-Gateway REST API instead.

**Purpose**: Incoming message events (Request-Reply) - **DEPRECATED**  
**Request Format**: JSON (matches `IngressMessage` protobuf structure)  
**Response Format**: JSON (matches `IngressAck` protobuf structure)

**Request** (`IngressMessage`):
```json
{
  "message_id": "string",
  "correlation_id": "string",
  "tenant_id": "string",
  "trace_id": "string (optional)",
  "message_type": "string",
  "payload": "string (base64 or UTF-8)",
  "metadata": {"key": "value"},
  "timestamp_ms": 0,
  "policy_id": "string (optional)"
}
```

**Response** (`IngressAck`):
```json
{
  "message_id": "string",
  "correlation_id": "string",
  "status": "accepted|rejected|error",
  "message": "string (optional)",
  "error_code": "string (optional)",
  "metadata": {"key": "value"},
  "timestamp_ms": 0
}
```

**Proto File**: `proto/beamline/ingress/v1/ingress.proto` (deprecated, not implemented)  
**Service**: `Ingress.Send(IngressMessage) returns (IngressAck)` (deprecated)

#### beamline.usage.v1.metered

**Purpose**: Usage metering events  
**Format**: JSON  
**Subscribers**: Usage aggregator, Billing

#### beamline.alert.v1.*

**Purpose**: Alert notifications (wildcard)  
**Format**: JSON  
**Examples**:
- `beamline.alert.v1.rate_limit`: Rate limit exceeded
- `beamline.alert.v1.error`: Error threshold exceeded
- `beamline.alert.v1.quota`: Quota threshold exceeded

## Protobuf Definitions

All protobuf definitions are located in `proto/beamline/*/v1/*.proto`:

- **Flow**: `proto/beamline/flow/v1/flow.proto`
  - Messages: `Message`, `RouteRequest`, `RouteDecision`
  - Service: `Router.Decide`

- **Provider**: `proto/beamline/provider/v1/provider.proto`
  - Messages: `ProviderRequest`, `ProviderResponse`, `StreamChunk`
  - Service: `Provider.Invoke`, `Provider.InvokeStream`

- **Ingress**: `proto/beamline/ingress/v1/ingress.proto` (deprecated, not part of core components - use C-Gateway instead)
  - Messages: `IngressMessage`, `IngressAck` (deprecated)
  - Service: `Ingress.Send` (deprecated)

**See also**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` for detailed mapping between proto and NATS.

## Versioning Strategy

### API Versioning

- **REST API**: URL-based versioning (`/api/v1/*`)
- **gRPC**: Package-based versioning (`beamline.flow.v1`, `beamline.provider.v1`)
- **NATS**: Subject-based versioning (`beamline.router.v1.*`, `beamline.provider.v1.*`)

### Breaking Changes

Following `.trae/manifest.json` compatibility policy:
- **MAJOR**: Breaking changes (remove fields, change types) → new version
- **MINOR**: Backward-compatible additions (new optional fields) → same version
- **PATCH**: Bug fixes, clarifications → same version

### Migration

- Old versions supported for 6 months after new version release
- Migration guides in `docs/MIGRATION.md`
- Deprecation warnings in API responses

## References

- **Core Message Fields**: `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` - Complete specification for all core fields
- `docs/GATEWAY_ROUTES.md`: Detailed Gateway endpoint specifications
- `proto/beamline/flow/v1/flow.proto`: Router gRPC ABI
- `proto/beamline/provider/v1/provider.proto`: Provider gRPC ABI
- `docs/NATS_SUBJECTS.md`: NATS subjects catalog
- `.trae/manifest.json`: Compatibility policy

