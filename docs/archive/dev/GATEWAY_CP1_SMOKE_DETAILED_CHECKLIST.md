# Gateway CP1-Smoke Detailed Checklist

**Date**: 2025-01-27  
**Status**: 📋 Implementation Checklist  
**Vector**: Gateway CP1-smoke / contract-smoke with Router  
**Endpoint**: `POST /api/v1/routes/decide`  
**Version**: 1.0

## Scope

This document provides a **point-by-point checklist** for implementing Gateway CP1-smoke verification, breaking down Phase 2/3 from `GATEWAY_CP1_SMOKE_PLAN.md` into specific file changes, tests, scripts, and documentation updates.

## Endpoint Verification: POST /api/v1/routes/decide

### Current State

**Code Location**: `apps/c-gateway/src/http_server.c`
- Handler: `handle_decide()` (lines 1191-1274)
- Validation: `validate_decide_request()` (line 831+)
- NATS Build: `build_route_request_json()` (line 887+)
- Error Handler: `send_error_response()` (line 134+)

**Documentation**:
- `docs/GATEWAY_ROUTES.md` - Has endpoint description (lines 79-107)
- `docs/ARCHITECTURE/api-registry.md` - **Missing** `POST /api/v1/routes/decide` definition (only has `POST /api/v1/messages`)

**Gap**: ~~`api-registry.md` does not define `POST /api/v1/routes/decide` DTO structure.~~ ✅ **RESOLVED** (2025-01-27): Full endpoint block added with DTO structure, validation rules, and implementation references.

## Phase 2: Implementation Checklist

### 2.1. Verify and Fix REST Contracts

**Status**: ✅ **COMPLETED** (2025-01-27)

**Summary**: 
- ✅ Full endpoint block added to `api-registry.md` with comprehensive DTO structure
- ✅ Request/Response DTOs verified against actual code implementation
- ✅ Validation rules documented (matching `validate_decide_request()`)
- ✅ NATS payload transformation documented (matching `build_route_request_json()`)
- ✅ Error handling documented (matching `send_error_response()` and `map_router_error_status()`)
- ✅ CP1 scope notation added with link to Gateway CP1-smoke checklist
- ✅ NATS subject documented with link to `PROTO_NATS_MAPPING.md`
- ✅ Implementation references added (handler, validation, NATS build, error handler)
- ✅ Test references added (smoke script, integration tests)

#### Task 2.1.1: Add POST /api/v1/routes/decide to api-registry.md

**File**: `docs/ARCHITECTURE/api-registry.md`

**Status**: ✅ **COMPLETED** (2025-01-27)

**Action**: Added comprehensive endpoint block with full DTO structure, validation rules, and implementation references.

**Content Added**:
- ✅ Full endpoint block with CP1 scope notation
- ✅ Request DTO table with field types, required/optional flags, and descriptions
- ✅ Request validation rules (matching `validate_decide_request()` implementation)
- ✅ NATS subject and payload transformation documentation
- ✅ Response DTO structure (Router response format, forwarded directly)
- ✅ Error DTO structure (Gateway and Router error formats)
- ✅ HTTP status code mapping (matching `map_router_error_status()` implementation)
- ✅ Implementation references (handler, validation, NATS build, error handler)
- ✅ Test references (smoke script, integration tests)

**Verification**:
- ✅ Section added after `POST /api/v1/messages` (lines 99-220)
- ✅ Request DTO matches actual code implementation (`validate_decide_request`, `build_route_request_json`)
- ✅ Response DTO matches Router response format (forwarded directly)
- ✅ Error DTO matches Gateway error format (`send_error_response`)
- ✅ CP1 scope notation added with link to `GATEWAY_CP1_SMOKE_DETAILED_CHECKLIST.md`
- ✅ NATS subject documented with link to `PROTO_NATS_MAPPING.md`

#### Task 2.1.2: Verify Request DTO Structure in Code

**File**: `apps/c-gateway/src/http_server.c`

**Function**: `validate_decide_request()` (line 831+)

**Status**: ✅ **VERIFIED** (2025-01-27)

**Actual Validation Rules** (from code):
- ✅ Validates `version` is string `"1"` (line 857-860)
- ✅ Validates `tenant_id` is non-empty string (line 862)
- ✅ Validates `request_id` is non-empty string (line 862)
- ✅ Validates `task` is object with `type` (string) and `payload` (object) (line 867-870)

**Note**: Code validates `task` field (NATS-specific), but `build_route_request_json()` does not forward `task` to Router. This is intentional: `task` is validated for Gateway contract but not part of Router proto contract.

**Expected Behavior**:
- ✅ Returns `0` if valid, `-1` if invalid
- ✅ Calls `send_error_response()` with `"invalid_request"` code on validation failure (line 1207-1212)

**Verification Command**:
```bash
grep -A 50 "validate_decide_request" apps/c-gateway/src/http_server.c
```

**Gap Analysis**:
- ❌ **Gap**: `validate_decide_request()` does not validate `message` object structure (only validates top-level `task`)
- ❌ **Gap**: `build_route_request_json()` uses `message_id`, `message_type`, `payload`, `metadata` from request body, but these are not validated by `validate_decide_request()`
- ✅ **Resolution**: Documented actual behavior in `api-registry.md`. For CP1, code is source of truth; validation gaps are acceptable for CP1 scope.

#### Task 2.1.3: Verify Response DTO Mapping

**File**: `apps/c-gateway/src/http_server.c`

**Function**: `handle_decide()` (lines 1191-1274)

**Status**: ✅ **VERIFIED** (2025-01-27)

**Actual Behavior** (from code):
- ✅ Router response (`resp_buf`) is forwarded directly without transformation (line 1273)
- ✅ Response DTO structure matches Router response format:
  - ✅ `ok: true` for success, `ok: false` for errors
  - ✅ `decision` object contains: `provider_id`, `reason`, `priority`, `expected_latency_ms`, `expected_cost`, `metadata`
  - ✅ `context` object contains: `request_id`, `trace_id`
- ✅ Error responses use Router error format (forwarded directly) or Gateway error format (`send_error_response`)

**HTTP Status Code Mapping** (line 1261-1271):
- ✅ `map_router_error_status()` maps Router error codes to HTTP status codes
- ✅ Success response: HTTP 200 OK
- ✅ Error responses: HTTP 400/401/404/500 based on Router error code

**Gap Analysis**:
- ✅ **No Gap**: Gateway forwards Router response directly (no transformation needed)
- ✅ **Documented**: Response format matches Router NATS response format (see `PROTO_NATS_MAPPING.md`)

**Verification Command**:
```bash
grep -A 20 "send_response.*resp_buf" apps/c-gateway/src/http_server.c | grep -A 20 "handle_decide"
```

### 2.2. Verify and Fix NATS Routing

**Status**: ✅ **COMPLETED** (2025-01-27)

**Summary**:
- ✅ NATS subject verified: `beamline.router.v1.decide` (configurable via env var)
- ✅ NATS payload structure verified against `build_route_request_json()` implementation
- ✅ NATS payload transformation documented in `api-registry.md`
- ✅ Cross-reference added: `PROTO_NATS_MAPPING.md` → `api-registry.md`
- ✅ Cross-reference added: `api-registry.md` → `PROTO_NATS_MAPPING.md`

#### Task 2.2.1: Verify NATS Subject

**File**: `apps/c-gateway/src/http_server.c`

**Function**: `handle_decide()` (line 1234-1238)

**Status**: ✅ **VERIFIED** (2025-01-27)

**Checklist**:
- ✅ NATS subject is `beamline.router.v1.decide` (line 1236)
- ✅ Subject can be overridden via `ROUTER_DECIDE_SUBJECT` env var (line 1234-1236)
- ✅ Subject matches `docs/NATS_SUBJECTS.md` and `PROTO_NATS_MAPPING.md`

**Verification**:
```c
const char *subject = getenv("ROUTER_DECIDE_SUBJECT");
if (!subject || subject[0] == '\0') {
    subject = "beamline.router.v1.decide";
}
```

**Documentation**: ✅ NATS subject documented in `api-registry.md` with link to `PROTO_NATS_MAPPING.md`

#### Task 2.2.2: Verify NATS Payload Structure

**File**: `apps/c-gateway/src/http_server.c`

**Function**: `build_route_request_json()` (line 887+)

**Status**: ✅ **VERIFIED** (2025-01-27)

**Checklist**:
- ✅ NATS payload includes `version: "1"` field (line 918-922, if present in request)
- ✅ NATS payload includes `request_id` (from request body, line 946-950)
- ✅ NATS payload includes `message` object (from REST request, lines 973-1004)
  - ✅ `message.message_id` (line 987-990)
  - ✅ `message.message_type` (line 991-994)
  - ✅ `message.payload` (line 995-998, must be object)
  - ✅ `message.metadata` (line 999-1002)
- ✅ NATS payload includes `policy_id` (if provided, line 1007-1011)
- ✅ NATS payload includes `context` (if provided, line 1014-1018)
- ✅ NATS payload includes `tenant_id` (from request or header, lines 924-943)
- ✅ NATS payload includes `trace_id` (from request or header, lines 952-971)

**Actual NATS Payload Structure** (from code):
```json
{
  "version": "1",              // From request body (if present)
  "tenant_id": "string",        // From header (preferred) or request body
  "request_id": "string",       // From request body
  "trace_id": "string",        // From header (preferred) or request body
  "message": {
    "message_id": "string",     // From request body
    "message_type": "string",   // From request body
    "payload": {},              // From request body (must be object)
    "metadata": {}              // From request body
  },
  "policy_id": "string",       // From request body (optional)
  "context": {}                // From request body (optional)
}
```

**Note**: Code does not include NATS-specific fields (`task`, `constraints`, `push_assignment`) in payload sent to Router. This is correct: these fields are validated by Gateway but not part of Router proto contract.

**Verification Command**:
```bash
grep -A 100 "build_route_request_json" apps/c-gateway/src/http_server.c
```

**Documentation**: ✅ NATS payload structure documented in `api-registry.md` with transformation details and link to `PROTO_NATS_MAPPING.md`

#### Task 2.2.3: Verify NATS Request-Reply Pattern

**File**: `apps/c-gateway/src/nats_client_real.c`

**Function**: `nats_request_decide()` (if exists)

**Checklist**:
- [ ] Uses NATS request-reply pattern (not pub-sub)
- [ ] Sets reply-inbox for response
- [ ] Handles timeout (5 seconds default)
- [ ] Returns Router response in `resp_buf`

**Verification Command**:
```bash
grep -A 50 "nats_request_decide" apps/c-gateway/src/nats_client_real.c
```

### 2.3. Verify and Fix Error Handling

#### Task 2.3.1: Verify Validation Error (400)

**File**: `apps/c-gateway/src/http_server.c`

**Function**: `send_error_response()` (line 134+)

**Checklist**:
- [ ] Error code: `"invalid_request"` → HTTP 400
- [ ] Error JSON structure matches `api-registry.md`:
  ```json
  {
    "error": {
      "code": "INVALID_REQUEST",
      "message": "string",
      "details": {},
      "trace_id": "string",
      "timestamp": "ISO 8601 string"
    }
  }
  ```
- [ ] `trace_id` included in error response
- [ ] `timestamp` included in error response (ISO 8601)

**Verification Command**:
```bash
grep -A 50 "send_error_response" apps/c-gateway/src/http_server.c | head -100
```

**Action**: Verify error JSON structure matches spec exactly.

#### Task 2.3.2: Verify Router Error (503)

**File**: `apps/c-gateway/src/http_server.c`

**Function**: `handle_decide()` (lines 1251-1258)

**Checklist**:
- [ ] NATS/Router unavailable → HTTP 503
- [ ] Error code: `"SERVICE_UNAVAILABLE"` (or `"internal"` - needs verification)
- [ ] Error message: `"router or NATS unavailable"`
- [ ] Error JSON structure matches spec

**Current Code**:
```c
if (rc != 0) {
    send_error_response(client_fd,
                        "HTTP/1.1 503 Service Unavailable",
                        "internal",
                        "router or NATS unavailable",
                        ctx);
    return;
}
```

**Gap**: Error code is `"internal"` but should be `"SERVICE_UNAVAILABLE"` per spec.

**Action**: Change error code from `"internal"` to `"SERVICE_UNAVAILABLE"`.

#### Task 2.3.3: Verify NATS Timeout (503)

**File**: `apps/c-gateway/src/nats_client_real.c`

**Function**: `nats_request_decide()` (timeout handling)

**Checklist**:
- [ ] NATS timeout (5 seconds) → HTTP 503
- [ ] Error code: `"SERVICE_UNAVAILABLE"`
- [ ] Error message: `"Request timeout"`
- [ ] Error JSON structure matches spec

**Action**: Verify timeout handling and error response format.

#### Task 2.3.4: Verify Router Error Response Mapping

**File**: `apps/c-gateway/src/http_server.c`

**Function**: `map_router_error_status()` (line 1261)

**Checklist**:
- [ ] Router `ErrorResponse.ok: false` → HTTP status code mapping:
  - [ ] `error.code: "invalid_request"` → HTTP 400
  - [ ] `error.code: "unauthorized"` → HTTP 401
  - [ ] `error.code: "policy_not_found"` → HTTP 404
  - [ ] `error.code: "decision_failed"` → HTTP 500
  - [ ] `error.code: "internal"` → HTTP 500
- [ ] Router error response is mapped to REST error DTO format

**Verification Command**:
```bash
grep -A 20 "map_router_error_status" apps/c-gateway/src/http_server.c
```

**Action**: Verify error mapping function exists and works correctly.

### 2.4. Verify and Fix Observability

#### Task 2.4.1: Verify JSON Log Format

**File**: `apps/c-gateway/src/http_server.c`

**Function**: Logging calls in `handle_decide()`

**Checklist**:
- [ ] Logs use JSON format (if JSON logging enabled)
- [ ] Required log fields present:
  - [ ] `timestamp`: ISO 8601 format (UTC)
  - [ ] `level`: ERROR, WARN, INFO, DEBUG
  - [ ] `component`: "gateway"
  - [ ] `message`: Human-readable message
  - [ ] `context`: Additional structured context (optional)
  - [ ] `trace_id`: Trace identifier (when available)
  - [ ] `tenant_id`: Tenant identifier (when available)
- [ ] PII/secrets filtered (no `api_key`, `password`, etc. in logs)

**Verification Command**:
```bash
grep -E "log_|LOG_" apps/c-gateway/src/http_server.c | grep -A 5 "handle_decide"
```

**Action**: Verify log format matches `docs/OBSERVABILITY.md` / `docs/OBSERVABILITY_CONVENTIONS.md`.

#### Task 2.4.2: Verify Health Endpoint

**File**: `apps/c-gateway/src/http_server.c`

**Function**: `GET /_health` handler

**Checklist**:
- [ ] Health endpoint returns HTTP 200 OK
- [ ] Health JSON structure matches spec:
  ```json
  {
    "status": "ok" | "degraded" | "unhealthy",
    "nats": {
      "connected": boolean
    },
    "timestamp_ms": integer
  }
  ```
- [ ] `nats.connected` reflects actual NATS connection status

**Verification Command**:
```bash
grep -A 30 "GET.*health\|/_health" apps/c-gateway/src/http_server.c
```

**Action**: Verify health endpoint JSON structure matches spec.

### 2.5. Enhance Smoke Tooling

#### Task 2.5.1: Enhance smoke_c_gateway.sh

**File**: `scripts/smoke_c_gateway.sh`

**Checklist**:
- [ ] Health endpoint test (already present)
- [ ] Happy path test (already present, needs verification)
- [ ] Validation error test (needs addition)
- [ ] Router error test (needs addition)
- [ ] NATS timeout test (needs addition)
- [ ] Log format verification (needs addition)
- [ ] Health endpoint JSON verification (needs addition)

**Current Coverage**:
- ✅ `GET /_health` check (line 52+)
- ✅ `POST /api/v1/routes/decide` happy path (line 52+)

**Gaps**:
- ❌ Validation error test (400)
- ❌ Router error test (503)
- ❌ NATS timeout test (503)
- ❌ Log format verification
- ❌ Health endpoint JSON structure verification

**Action**: Add missing test scenarios to `smoke_c_gateway.sh`.

#### Task 2.5.2: Enhance gateway_router_cp1_smoke.sh

**File**: `scripts/gateway_router_cp1_smoke.sh`

**Checklist**:
- [ ] Health endpoint test (already present, line 100+)
- [ ] Happy path test (already present, line 125+)
- [ ] Validation error test (already present, line 232+)
- [ ] Router error test (needs addition)
- [ ] NATS timeout test (needs addition)
- [ ] Log format verification (needs addition)
- [ ] Response DTO structure verification (enhance existing)

**Current Coverage**:
- ✅ Health endpoint (line 100+)
- ✅ Happy path (line 125+)
- ✅ Validation error (line 232+)

**Gaps**:
- ❌ Router error test (503)
- ❌ NATS timeout test (503)
- ❌ Log format verification
- ❌ Enhanced response DTO verification (check all fields)

**Action**: Add missing test scenarios and enhance existing tests.

## Phase 3: Validation Checklist

### 3.1. Run Full Smoke Cycle

#### Task 3.1.1: Local Smoke Test

**Command**:
```bash
# Start services
docker-compose up -d nats router gateway

# Wait for services to be ready
sleep 5

# Run smoke test
bash scripts/gateway_router_cp1_smoke.sh --full
```

**Checklist**:
- [ ] All services start successfully
- [ ] Health endpoint test passes
- [ ] Happy path test passes
- [ ] Validation error test passes
- [ ] Router error test passes (if Router stopped)
- [ ] NATS timeout test passes (if NATS stopped)
- [ ] Log format verification passes
- [ ] Health endpoint JSON verification passes

#### Task 3.1.2: Fix Flakiness

**Checklist**:
- [ ] All tests are deterministic (no race conditions)
- [ ] Timeouts are appropriate (5 seconds for NATS)
- [ ] Retry logic (if needed) is implemented
- [ ] Error messages are clear and actionable

### 3.2. Update Documentation

#### Task 3.2.1: Update CP1_ACCEPTANCE_REPORT.md

**File**: `docs/CP1_ACCEPTANCE_REPORT.md`

**Action**: Add Gateway CP1-smoke status section

**Content to Add** (after Router section):
```markdown
## Gateway (apps/c-gateway) — CP1 Status

Gateway CP1-smoke verification completed:
- Health endpoint (`GET /_health`) returns 200 JSON matching spec
- Happy path (`POST /api/v1/routes/decide`) works end-to-end
- Error paths return predictable responses (400, 503)
- Observability baseline (JSON logs, health endpoint) meets CP1 requirements

**Detailed Reports**:
- [Gateway CP1-Smoke Implementation Report](GATEWAY_CP1_SMOKE_IMPLEMENTATION_REPORT.md)
- [Gateway DTO Consistency Report](GATEWAY_DTO_CONSISTENCY.md)
```

**Checklist**:
- [ ] Section added after Router section
- [ ] Links to detailed reports included
- [ ] Status clearly stated (completed/in-progress)

#### Task 3.2.2: Update API_CONTRACTS.md (if needed)

**File**: `docs/API_CONTRACTS.md`

**Action**: Verify Gateway REST API section is up-to-date

**Checklist**:
- [ ] `POST /api/v1/routes/decide` endpoint documented
- [ ] Request/Response DTOs match `api-registry.md`
- [ ] Error DTOs match `api-registry.md`
- [ ] NATS subject mapping documented

#### Task 3.2.3: Update PROTO_NATS_MAPPING.md (if needed)

**File**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

**Action**: Verify Gateway → Router mapping is documented

**Checklist**:
- [ ] Gateway REST → NATS mapping documented
- [ ] NATS payload structure matches Router expectations
- [ ] Response mapping (Router → Gateway REST) documented

## Summary Checklist

### Phase 2: Implementation

- [ ] **2.1 REST Contracts**: api-registry.md updated, DTOs verified
- [ ] **2.2 NATS Routing**: Subject verified, payload structure verified
- [ ] **2.3 Error Handling**: All error scenarios verified and fixed
- [ ] **2.4 Observability**: Log format verified, health endpoint verified
- [ ] **2.5 Smoke Tooling**: Scripts enhanced with all test scenarios

### Phase 3: Validation

- [ ] **3.1 Full Smoke Cycle**: All tests pass locally
- [ ] **3.2 Documentation**: CP1_ACCEPTANCE_REPORT.md updated, other docs verified

## Acceptance Criteria

**Gateway CP1-Smoke is complete when**:
1. ✅ All Phase 2 tasks completed (REST contracts, NATS routing, error handling, observability, smoke tooling)
2. ✅ All Phase 3 tasks completed (full smoke cycle, documentation updates)
3. ✅ All smoke tests pass consistently (no flakiness)
4. ✅ Documentation is up-to-date and accurate

## References

- `docs/archive/dev/GATEWAY_CP1_SMOKE_PLAN.md` - High-level plan
- `docs/ARCHITECTURE/api-registry.md` - REST API DTO definitions
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto ↔ NATS mapping
- `docs/OBSERVABILITY.md` - Observability requirements
- `apps/c-gateway/src/http_server.c` - Gateway implementation
- `scripts/smoke_c_gateway.sh` - Basic smoke script
- `scripts/gateway_router_cp1_smoke.sh` - CP1-smoke script

