# Gateway ↔ Router: Error Handling Priority & Rate Limiting

**Date**: 2025-01-27  
**Status**: ✅ Documented  
**Purpose**: Clarify the interaction between Gateway rate limiting (429) and Router intake errors (400/401/500).

## Overview

Gateway implements a two-stage error handling process:
1. **Rate Limiting Check** (Gateway-level): Applied BEFORE Router validation
2. **Router Intake Validation** (Router-level): Applied AFTER rate limit passes

These error types should **never conflict** because rate limiting is checked first and blocks Router calls when exceeded.

## Error Priority Order

### 1. Rate Limiting (429) - Highest Priority

**When**: Rate limit exceeded for the endpoint  
**HTTP Status**: `429 Too Many Requests`  
**Error Code**: `rate_limit_exceeded`  
**Action**: Gateway returns 429 **immediately**, Router is **NOT called**

**Response Format**:
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

**Headers**:
- `X-RateLimit-Limit: 50`
- `X-RateLimit-Remaining: 0`
- `X-RateLimit-Reset: 1234567890`
- `Retry-After: 45`

**Key Characteristics**:
- ✅ Checked **BEFORE** Router validation
- ✅ Router is **NOT called** when rate limit exceeded
- ✅ No `intake_error_code` in response (Router not involved)
- ✅ Includes rate limit headers (`X-RateLimit-*`, `Retry-After`)

### 2. Router Intake Errors (400/401/500) - Lower Priority

**When**: Rate limit passes, but Router intake validation fails  
**HTTP Status**: `400 Bad Request`, `401 Unauthorized`, or `500 Internal Server Error`  
**Error Code**: `invalid_request`, `unauthorized`, or `internal`  
**Action**: Gateway forwards Router error to client

**Response Format**:
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

**Headers**:
- Rate limit headers may be present (if Gateway includes them for all responses)
- `Retry-After` should **NOT** be present for non-429 errors

**Key Characteristics**:
- ✅ Checked **AFTER** rate limit passes
- ✅ Router **IS called** and validates the request
- ✅ Includes `intake_error_code` (Router error code)
- ✅ Rate limit headers may be present, but `Retry-After` should not be relevant

## Error Code Mapping

### Router Intake Errors → Gateway Error Codes → HTTP Status

| Router Intake Error | Gateway Error Code | HTTP Status | Description |
|---------------------|-------------------|-------------|-------------|
| `SCHEMA_VALIDATION_FAILED` | `invalid_request` | 400 | Invalid message schema (missing fields, invalid format) |
| `VERSION_UNSUPPORTED` | `invalid_request` | 400 | Unsupported protocol version |
| `CORRELATION_FIELDS_INVALID` | `invalid_request` | 400 | Invalid correlation fields (UUID format, dependencies) |
| `TENANT_FORBIDDEN` | `unauthorized` | 401 | Tenant not in allowlist |
| `IDEMPOTENCY_VIOLATION` | `invalid_request` | 400 | Duplicate request with conflicting data |
| `INTERNAL_VALIDATION_ERROR` | `internal` | 500 | Internal Router validation error |

### Rate Limiting Error

| Gateway Error | HTTP Status | Description |
|---------------|-------------|-------------|
| `rate_limit_exceeded` | 429 | Rate limit exceeded for endpoint |

## Request Flow

```
Client Request
    ↓
Gateway Rate Limit Check
    ↓
    ├─ Rate Limit Exceeded? → YES → Return 429 (Router NOT called)
    │
    └─ NO → Forward to Router
            ↓
        Router Intake Validation
            ↓
            ├─ Validation Failed? → YES → Return Router Error (400/401/500)
            │
            └─ NO → Process Request → Return 200/503
```

## Examples

### Example 1: Rate Limit Exceeded (429)

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
X-Tenant-ID: tenant-123
Content-Type: application/json

{
  "version": "1",
  "tenant_id": "tenant-123",
  "request_id": "req-001",
  "task": {
    "type": "text.generate",
    "payload": {}
  }
}
```

**Response** (429):
```http
HTTP/1.1 429 Too Many Requests
Content-Type: application/json
X-RateLimit-Limit: 50
X-RateLimit-Remaining: 0
X-RateLimit-Reset: 1234567890
Retry-After: 45

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
    "request_id": "req-001",
    "trace_id": "...",
    "tenant_id": "tenant-123"
  }
}
```

**Note**: Router is **NOT called** - rate limit check happens first.

### Example 2: Router Intake Error After Rate Limit Passes (400)

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
X-Tenant-ID: tenant-123
Content-Type: application/json

{
  "version": "1",
  // Missing tenant_id - would trigger Router SCHEMA_VALIDATION_FAILED
  "request_id": "req-002",
  "task": {
    "type": "text.generate",
    "payload": {}
  }
}
```

**Response** (400):
```http
HTTP/1.1 400 Bad Request
Content-Type: application/json
X-RateLimit-Limit: 50
X-RateLimit-Remaining: 49

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
    "request_id": "req-002",
    "trace_id": "...",
    "tenant_id": "tenant-123"
  }
}
```

**Note**: Rate limit passed, Router was called and returned intake error.

### Example 3: Rate Limit Exceeded with Invalid Request Data (429)

**Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
X-Tenant-ID: tenant-123
Content-Type: application/json

{
  "version": "1",
  // Missing tenant_id - would trigger Router SCHEMA_VALIDATION_FAILED
  // But rate limit is checked FIRST
  "request_id": "req-003",
  "task": {
    "type": "text.generate",
    "payload": {}
  }
}
```

**Response** (429):
```http
HTTP/1.1 429 Too Many Requests
Content-Type: application/json
X-RateLimit-Limit: 50
X-RateLimit-Remaining: 0
X-RateLimit-Reset: 1234567890
Retry-After: 45

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
    "request_id": "req-003",
    "trace_id": "...",
    "tenant_id": "tenant-123"
  }
}
```

**Note**: Even though request has invalid data, rate limit is checked first, so 429 is returned. Router is **NOT called**.

## Key Invariants

1. **Rate Limiting is Always Checked First**
   - Gateway checks rate limit **BEFORE** calling Router
   - If rate limit exceeded, Gateway returns 429 **immediately**
   - Router is **NOT called** when rate limit exceeded

2. **Router Intake Errors Only Occur After Rate Limit Passes**
   - Router intake validation happens **AFTER** rate limit check
   - Router intake errors (400/401/500) only occur when rate limit is OK
   - Router intake errors include `intake_error_code` for debugging

3. **Error Types Never Conflict**
   - Rate limit error (429) and Router intake error (400/401/500) are **mutually exclusive**
   - A request cannot return both 429 and 400/401/500
   - Error priority ensures deterministic behavior

4. **Response Format Distinction**
   - **429 responses**: `error.code = "rate_limit_exceeded"`, no `intake_error_code`, includes rate limit headers
   - **400/401/500 responses**: `error.code = "invalid_request"/"unauthorized"/"internal"`, includes `intake_error_code`, may include rate limit headers but not `Retry-After`

5. **Rate Limit Counting**
   - Rate limit is typically counted **per request**, regardless of Router validation result
   - Failed Router validation requests may still count toward rate limit (implementation-dependent)
   - Rate limit window resets after TTL expiration

## Testing

See `tests/integration/gateway-router-rate-limiting-error-handling.test.ts` for comprehensive integration tests covering:
- Rate limiting priority over Router intake errors
- Router intake errors after rate limit passes
- Error response format distinction
- Mixed scenarios (rate limit reset after intake errors)
- Error code mapping consistency

## References

- `docs/GATEWAY_CONFLICT_CONTRACT.md`: **Conflict Contract** - Complete priority matrix for all error sources
- `docs/GATEWAY_RATE_LIMITING.md`: Rate limiting specification
- `docs/GATEWAY_ROUTES_SPEC.md`: Gateway routes specification
- `docs/ARCHITECTURE/api-registry.md`: Error code mapping
- `apps/c-gateway/src/http_server.c`: Gateway rate limiting implementation
- `apps/otp/router/src/router_intake_error_handler.erl`: Router intake error handling
- `tests/integration/gateway-router-error-handling.test.ts`: Router intake error tests
- `tests/integration/gateway-router-rate-limiting-error-handling.test.ts`: Rate limiting + intake error tests

**Note**: This document focuses on rate limiting vs Router intake errors. For complete conflict matrix including authentication, request validation, and internal errors, see `docs/GATEWAY_CONFLICT_CONTRACT.md`.

