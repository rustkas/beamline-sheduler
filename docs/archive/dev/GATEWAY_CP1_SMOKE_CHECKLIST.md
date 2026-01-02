# Gateway CP1-Smoke Detailed Checklist

**Date**: 2025-01-27  
**Status**: 📋 Implementation Checklist  
**Vector**: Gateway CP1-smoke / contract-smoke with Router  
**Endpoint**: `POST /api/v1/routes/decide`  
**Version**: 1.0

## Purpose

This document provides a detailed, file-by-file checklist for Gateway CP1-smoke verification. Each item maps to specific code locations, test scenarios, and validation steps.

## Checklist Structure

- **Code Files**: Specific functions and lines to verify
- **Test Scenarios**: Exact test cases to run
- **Documentation**: DTO structures to compare
- **Scripts**: Smoke test commands to execute

---

## 1. REST Contracts Verification

### 1.1 Request DTO Structure

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `validate_decide_request()` (lines 831-885)  
**Reference**: `docs/ARCHITECTURE/api-registry.md` (lines 48-75)

**Checklist**:
- [ ] **Field: `message`** (nested object)
  - [ ] `message.message_id` (string, UUID) - ✅ Present in validation (line 982: `msg_id`)
  - [ ] `message.tenant_id` (string, required) - ✅ Present in validation (line 849: `tenant_id`)
  - [ ] `message.trace_id` (string, optional) - ✅ Present in validation (line 966: `trace_id`)
  - [ ] `message.message_type` (string: "chat" | "completion" | "embedding") - ✅ Present in validation (line 983: `msg_type`)
  - [ ] `message.payload` (string, Base64 encoded) - ✅ Present in validation (line 984: `payload`)
  - [ ] `message.metadata` (object, optional) - ✅ Present in validation (line 985: `metadata`)
  - [ ] `message.timestamp_ms` (number, optional) - ⚠️ **GAP**: Not validated in `validate_decide_request()`

- [ ] **Field: `policy_id`** (string, optional) - ✅ Present in validation (line 1001: `policy_id`)

- [ ] **Field: `context`** (object, optional) - ✅ Present in validation (line 1002: `context`)

**Current Implementation**:
- `validate_decide_request()` validates: `version`, `tenant_id`, `request_id`, `task.type`, `task.payload`
- **GAP**: Current validation expects `task` object, but `api-registry.md` expects `message` object
- **GAP**: `timestamp_ms` not validated

**Action Items**:
- [ ] Compare actual request structure in `build_route_request_json()` (lines 887-1031) with `api-registry.md`
- [ ] Verify if Gateway uses `task` or `message` structure
- [ ] Add validation for `timestamp_ms` if required

### 1.2 Response DTO Structure

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `handle_decide()` (lines 1191-1274)  
**Reference**: `docs/ARCHITECTURE/api-registry.md` (lines 63-75)

**Checklist**:
- [ ] **Field: `message_id`** (string) - ⚠️ **GAP**: Need to verify mapping from Router response
- [ ] **Field: `provider_id`** (string) - ⚠️ **GAP**: Need to verify mapping from Router `decision.provider_id`
- [ ] **Field: `reason`** (string: "weighted" | "sticky" | "fallback" | "policy") - ⚠️ **GAP**: Need to verify mapping from Router `decision.reason`
- [ ] **Field: `priority`** (number) - ⚠️ **GAP**: Need to verify mapping from Router `decision.priority`
- [ ] **Field: `expected_latency_ms`** (number) - ⚠️ **GAP**: Need to verify mapping from Router `decision.expected_latency_ms`
- [ ] **Field: `expected_cost`** (number) - ⚠️ **GAP**: Need to verify mapping from Router `decision.expected_cost`
- [ ] **Field: `currency`** (string, default: "USD") - ⚠️ **GAP**: Need to verify if Router provides or Gateway adds
- [ ] **Field: `trace_id`** (string) - ⚠️ **GAP**: Need to verify mapping from Router `context.trace_id`

**Current Implementation**:
- `handle_decide()` calls `nats_request_decide()` and receives `resp_buf` (Router response)
- Router response is sent directly via `send_response()` (line 1273)
- **GAP**: No explicit mapping from Router response to REST DTO structure

**Action Items**:
- [ ] Verify Router response structure matches `PROTO_NATS_MAPPING.md` (lines 105-122)
- [ ] Add mapping function to transform Router response to REST DTO format
- [ ] Verify all required fields are present in final REST response

### 1.3 Error DTO Structure

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `send_error_response()` (lines 782-829)  
**Reference**: `docs/ARCHITECTURE/api-registry.md` (lines 77-88)

**Checklist**:
- [ ] **Field: `error.code`** (string) - ✅ Present (line 799: `error_code`)
- [ ] **Field: `error.message`** (string) - ✅ Present (line 799: `message`)
- [ ] **Field: `error.details`** (object) - ✅ Present (line 799: `"details":{}`)
- [ ] **Field: `error.trace_id`** (string) - ⚠️ **GAP**: Not in `error` object, but in `context.trace_id`
- [ ] **Field: `error.timestamp`** (ISO 8601 string) - ⚠️ **GAP**: Not present in error response
- [ ] **Field: `context.request_id`** (string) - ✅ Present (line 800: `context.request_id`)
- [ ] **Field: `context.trace_id`** (string) - ✅ Present (line 800: `context.trace_id`)

**Current Implementation**:
```json
{
  "ok": false,
  "error": {
    "code": "...",
    "message": "...",
    "details": {}
  },
  "context": {
    "request_id": "...",
    "trace_id": "..."
  }
}
```

**GAP**: `api-registry.md` expects `error.trace_id` and `error.timestamp`, but Gateway uses `context.trace_id` and no timestamp.

**Action Items**:
- [ ] Add `error.trace_id` to error object (or document that it's in `context`)
- [ ] Add `error.timestamp` (ISO 8601 format)
- [ ] Verify error codes match `api-registry.md` (lines 90-97)

### 1.4 Error Codes Mapping

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `map_router_error_status()` (lines 1033-1084)  
**Reference**: `docs/ARCHITECTURE/api-registry.md` (lines 90-97)

**Checklist**:
- [ ] `INVALID_REQUEST` → 400 - ✅ Mapped (line 1062: `invalid_request` → 400)
- [ ] `UNAUTHORIZED` → 401 - ✅ Mapped (line 1066: `unauthorized` → 401)
- [ ] `FORBIDDEN` → 403 - ⚠️ **GAP**: Not mapped
- [ ] `NOT_FOUND` → 404 - ✅ Mapped (line 1070: `policy_not_found` → 404)
- [ ] `RATE_LIMIT_EXCEEDED` → 429 - ✅ Handled separately in `send_rate_limit_error()` (lines 608-689)
- [ ] `INTERNAL_ERROR` → 500 - ✅ Mapped (line 1074: `internal` → 500)
- [ ] `SERVICE_UNAVAILABLE` → 503 - ✅ Handled in `handle_decide()` (line 1252: NATS unavailable → 503)

**Action Items**:
- [ ] Add `FORBIDDEN` → 403 mapping if Router can return `forbidden` error code
- [ ] Verify all error codes from Router are mapped correctly

---

## 2. NATS Routing Verification

### 2.1 NATS Subject

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `handle_decide()` (line 1234-1239)  
**Reference**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (line 85)

**Checklist**:
- [ ] **Subject**: `beamline.router.v1.decide` - ✅ Default (line 1236)
- [ ] **Environment Variable**: `ROUTER_DECIDE_SUBJECT` - ✅ Supported (line 1234)
- [ ] **Subject in OpenTelemetry Span**: ✅ Set as attribute (line 1238)

**Action Items**:
- [ ] Verify subject matches `PROTO_NATS_MAPPING.md` exactly
- [ ] Test with custom subject via environment variable

### 2.2 NATS Payload Structure

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `build_route_request_json()` (lines 887-1031)  
**Reference**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (lines 87-103)

**Checklist**:
- [ ] **Field: `version`** (string, "1") - ✅ Present (line 918-922)
- [ ] **Field: `request_id`** (string, UUID) - ✅ Present (line 946-950)
- [ ] **Field: `message`** (object) - ✅ Present (lines 973-1000)
  - [ ] `message.message_id` - ✅ Present (line 982)
  - [ ] `message.tenant_id` - ✅ Present (lines 924-943)
  - [ ] `message.trace_id` - ✅ Present (lines 952-971)
  - [ ] `message.message_type` - ✅ Present (line 983)
  - [ ] `message.payload` - ✅ Present (line 984)
  - [ ] `message.metadata` - ✅ Present (line 985)
  - [ ] `message.timestamp_ms` - ⚠️ **GAP**: Not added to NATS payload
- [ ] **Field: `policy_id`** (string, optional) - ✅ Present (line 1001)
- [ ] **Field: `context`** (object, optional) - ✅ Present (line 1002)

**Current Implementation**:
- `build_route_request_json()` builds NATS payload with: `version`, `tenant_id`, `request_id`, `trace_id`, `message.*`, `policy_id`, `context`
- **GAP**: `timestamp_ms` not added to NATS payload

**Action Items**:
- [ ] Add `timestamp_ms` to NATS payload if required by Router
- [ ] Verify payload structure matches `PROTO_NATS_MAPPING.md` exactly

### 2.3 NATS Response Structure

**File**: `apps/c-gateway/src/nats_client_real.c`  
**Function**: `nats_request_decide()`  
**Reference**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (lines 105-122)

**Checklist**:
- [ ] **Field: `ok`** (boolean) - ✅ Present (verified in `map_router_error_status()`, line 1053)
- [ ] **Field: `decision`** (object) - ⚠️ **GAP**: Need to verify structure
  - [ ] `decision.provider_id` - ⚠️ **GAP**: Need to verify
  - [ ] `decision.reason` - ⚠️ **GAP**: Need to verify
  - [ ] `decision.priority` - ⚠️ **GAP**: Need to verify
  - [ ] `decision.expected_latency_ms` - ⚠️ **GAP**: Need to verify
  - [ ] `decision.expected_cost` - ⚠️ **GAP**: Need to verify
  - [ ] `decision.metadata` - ⚠️ **GAP**: Need to verify
- [ ] **Field: `context`** (object) - ⚠️ **GAP**: Need to verify
  - [ ] `context.request_id` - ⚠️ **GAP**: Need to verify
  - [ ] `context.trace_id` - ⚠️ **GAP**: Need to verify
- [ ] **Field: `error`** (object, if `ok == false`) - ✅ Present (verified in `map_router_error_status()`, line 1056)

**Action Items**:
- [ ] Read Router response structure from `nats_client_real.c`
- [ ] Verify Router response matches `PROTO_NATS_MAPPING.md` exactly
- [ ] Test with actual Router response

---

## 3. Error Handling Verification

### 3.1 Validation Error (400 Bad Request)

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `handle_decide()` (lines 1205-1213)  
**Test Scenario**: Send invalid request body

**Checklist**:
- [ ] **HTTP Status**: 400 Bad Request - ✅ (line 1208)
- [ ] **Error Code**: `invalid_request` - ✅ (line 1209)
- [ ] **Error Message**: "invalid decide request DTO" - ✅ (line 1210)
- [ ] **Error JSON Structure**: Matches `api-registry.md` - ⚠️ **GAP**: Missing `error.timestamp`

**Test Command**:
```bash
curl -X POST http://localhost:8080/api/v1/routes/decide \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: tenant-test" \
  -d '{"invalid": "request"}'
```

**Expected Response**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "invalid decide request DTO",
    "details": {},
    "trace_id": "...",
    "timestamp": "2025-01-27T12:00:00Z"
  },
  "context": {
    "request_id": "...",
    "trace_id": "..."
  }
}
```

**Action Items**:
- [ ] Add `error.timestamp` to error response
- [ ] Run test and verify response structure

### 3.2 Router Error (503 Service Unavailable)

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `handle_decide()` (lines 1251-1258)  
**Test Scenario**: NATS unavailable or Router timeout

**Checklist**:
- [ ] **HTTP Status**: 503 Service Unavailable - ✅ (line 1253)
- [ ] **Error Code**: `internal` - ✅ (line 1254)
- [ ] **Error Message**: "router or NATS unavailable" - ✅ (line 1255)
- [ ] **Error JSON Structure**: Matches `api-registry.md` - ⚠️ **GAP**: Missing `error.timestamp`

**Test Command**:
```bash
# Stop NATS or Router, then:
curl -X POST http://localhost:8080/api/v1/routes/decide \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: tenant-test" \
  -d '{"version": "1", "tenant_id": "tenant-test", "request_id": "req-1", "message_id": "msg-1", "message_type": "chat", "payload": "dGVzdA=="}'
```

**Expected Response**:
```json
{
  "ok": false,
  "error": {
    "code": "SERVICE_UNAVAILABLE",
    "message": "router or NATS unavailable",
    "details": {},
    "trace_id": "...",
    "timestamp": "2025-01-27T12:00:00Z"
  },
  "context": {
    "request_id": "...",
    "trace_id": "..."
  }
}
```

**Action Items**:
- [ ] Change error code from `internal` to `SERVICE_UNAVAILABLE` (or document mapping)
- [ ] Add `error.timestamp` to error response
- [ ] Run test and verify response structure

### 3.3 NATS Timeout (503 Service Unavailable)

**File**: `apps/c-gateway/src/nats_client_real.c`  
**Function**: `nats_request_decide()`  
**Test Scenario**: NATS request timeout

**Checklist**:
- [ ] **HTTP Status**: 503 Service Unavailable - ✅ (handled in `handle_decide()`, line 1252)
- [ ] **Error Code**: `SERVICE_UNAVAILABLE` - ⚠️ **GAP**: Currently `internal`
- [ ] **Error Message**: "Request timeout" - ⚠️ **GAP**: Currently "router or NATS unavailable"
- [ ] **Error JSON Structure**: Matches `api-registry.md` - ⚠️ **GAP**: Missing `error.timestamp`

**Action Items**:
- [ ] Distinguish NATS timeout from NATS unavailable in error handling
- [ ] Add `error.timestamp` to error response
- [ ] Run test and verify response structure

---

## 4. Observability Verification

### 4.1 JSON Log Format

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `log_error()` (lines 577-598)  
**Reference**: `docs/OBSERVABILITY.md`

**Checklist**:
- [ ] **Field: `timestamp`** (ISO 8601, UTC) - ⚠️ **GAP**: Not present in log
- [ ] **Field: `level`** (string: "ERROR" | "WARN" | "INFO" | "DEBUG") - ✅ Present (line 590: `"level":"error"`)
- [ ] **Field: `component`** (string) - ✅ Present (line 590: `"component":"c-gateway"`)
- [ ] **Field: `message`** (string) - ✅ Present (line 591: `"message":"..."`)
- [ ] **Field: `context`** (object) - ✅ Present (lines 592-593: `request_id`, `trace_id`)
- [ ] **Field: `trace_id`** (string, optional) - ✅ Present (line 593)
- [ ] **Field: `tenant_id`** (string, optional) - ⚠️ **GAP**: Not present in log

**Current Implementation**:
```json
{
  "level": "error",
  "component": "c-gateway",
  "stage": "...",
  "error_code": "...",
  "message": "...",
  "request_id": "...",
  "trace_id": "..."
}
```

**GAP**: Missing `timestamp`, `tenant_id`, and `context` wrapper object.

**Action Items**:
- [ ] Add `timestamp` (ISO 8601, UTC) to log format
- [ ] Add `tenant_id` to log format
- [ ] Wrap `request_id` and `trace_id` in `context` object (or document current structure)
- [ ] Verify log format matches `OBSERVABILITY.md` exactly

### 4.2 Health Endpoint

**File**: `apps/c-gateway/src/http_server.c`  
**Function**: `handle_health()` (lines 1086-1089)  
**Reference**: `docs/OBSERVABILITY.md`

**Checklist**:
- [ ] **HTTP Status**: 200 OK - ✅ (line 1088)
- [ ] **Content-Type**: `application/json` - ✅ (via `send_response()`)
- [ ] **Field: `status`** (string: "ok" | "degraded" | "unhealthy") - ✅ Present (line 1087: `"status":"ok"`)
- [ ] **Field: `nats.connected`** (boolean) - ⚠️ **GAP**: Not present
- [ ] **Field: `timestamp_ms`** (number) - ⚠️ **GAP**: Not present

**Current Implementation**:
```json
{
  "status": "ok"
}
```

**GAP**: Missing `nats.connected` and `timestamp_ms` fields.

**Action Items**:
- [ ] Add `nats.connected` (check NATS connection status)
- [ ] Add `timestamp_ms` (current timestamp in milliseconds)
- [ ] Verify health endpoint JSON structure matches `OBSERVABILITY.md`

---

## 5. Smoke Tooling

### 5.1 Enhanced Smoke Script

**File**: `scripts/smoke_c_gateway.sh`  
**Status**: Basic smoke test exists, needs enhancement

**Checklist**:
- [ ] **Health Check**: `GET /_health` - ✅ Present (lines 9-15)
- [ ] **Happy Path**: `POST /api/v1/routes/decide` - ✅ Present (lines 17-34)
- [ ] **Error Path: Validation Error** - ⚠️ **GAP**: Not present
- [ ] **Error Path: Router Error** - ⚠️ **GAP**: Not present
- [ ] **Error Path: NATS Timeout** - ⚠️ **GAP**: Not present
- [ ] **Log Verification** - ⚠️ **GAP**: Not present
- [ ] **Health Endpoint JSON Verification** - ⚠️ **GAP**: Not present
- [ ] **Clear Report** - ⚠️ **GAP**: Basic output only

**Action Items**:
- [ ] Add validation error test (invalid request body)
- [ ] Add Router error test (NATS unavailable)
- [ ] Add NATS timeout test (simulate timeout)
- [ ] Add log format verification
- [ ] Add health endpoint JSON structure verification
- [ ] Add clear pass/fail report with details

### 5.2 Unified Gateway↔Router Smoke Script

**File**: `scripts/gateway_router_cp1_smoke.sh` (new)  
**Status**: Create new unified script

**Checklist**:
- [ ] **Service Startup**: Start NATS, Router, Gateway
- [ ] **Happy Path E2E**: REST → NATS → Router → NATS → REST
- [ ] **Error Path E2E**: Validation error, Router error, NATS timeout
- [ ] **Contract Verification**: DTO structure, NATS subject, payload structure
- [ ] **Observability Verification**: Log format, health endpoint
- [ ] **Clear Report**: Pass/fail with details

**Action Items**:
- [ ] Create `scripts/gateway_router_cp1_smoke.sh`
- [ ] Implement service startup logic
- [ ] Implement E2E test scenarios
- [ ] Implement contract verification
- [ ] Implement observability verification
- [ ] Implement clear reporting

---

## 6. Documentation Verification

### 6.1 API Registry Alignment

**File**: `docs/ARCHITECTURE/api-registry.md`  
**Status**: Verify Gateway implementation matches documentation

**Checklist**:
- [ ] **Request DTO**: Gateway accepts structure from `api-registry.md` (lines 48-75)
- [ ] **Response DTO**: Gateway returns structure from `api-registry.md` (lines 63-75)
- [ ] **Error DTO**: Gateway returns structure from `api-registry.md` (lines 77-88)
- [ ] **Error Codes**: Gateway uses codes from `api-registry.md` (lines 90-97)

**Action Items**:
- [ ] Compare Gateway code with `api-registry.md` line by line
- [ ] Document any discrepancies
- [ ] Update either code or documentation to match

### 6.2 PROTO_NATS_MAPPING Alignment

**File**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`  
**Status**: Verify Gateway NATS payload matches documentation

**Checklist**:
- [ ] **NATS Subject**: Gateway uses subject from `PROTO_NATS_MAPPING.md` (line 85)
- [ ] **NATS Request Payload**: Gateway builds payload from `PROTO_NATS_MAPPING.md` (lines 87-103)
- [ ] **NATS Response Payload**: Gateway expects response from `PROTO_NATS_MAPPING.md` (lines 105-122)

**Action Items**:
- [ ] Compare Gateway NATS code with `PROTO_NATS_MAPPING.md` line by line
- [ ] Document any discrepancies
- [ ] Update either code or documentation to match

---

## 7. Summary of Gaps

### Critical Gaps (Blocking CP1-Smoke)

1. **Response DTO Mapping**: Gateway sends Router response directly, no mapping to REST DTO format
2. **Error DTO Structure**: Missing `error.timestamp` and `error.trace_id` (currently in `context`)
3. **Log Format**: Missing `timestamp`, `tenant_id`, and `context` wrapper
4. **Health Endpoint**: Missing `nats.connected` and `timestamp_ms`

### Non-Critical Gaps (Documentation/Enhancement)

1. **Request DTO**: `timestamp_ms` not validated/added
2. **NATS Payload**: `timestamp_ms` not added to NATS payload
3. **Error Codes**: `FORBIDDEN` → 403 not mapped
4. **Smoke Scripts**: Need error path tests and log verification

---

## 8. Next Steps

1. **Phase 1**: Fix critical gaps (Response DTO mapping, Error DTO structure, Log format, Health endpoint)
2. **Phase 2**: Enhance smoke scripts (error paths, log verification, clear reporting)
3. **Phase 3**: Create DTO consistency report (`GATEWAY_DTO_CONSISTENCY.md`)
4. **Phase 4**: Run full smoke cycle and update documentation

---

## References

- `docs/ARCHITECTURE/api-registry.md` - REST API DTO definitions
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto ↔ NATS subject mapping
- `docs/OBSERVABILITY.md` - Observability requirements
- `apps/c-gateway/src/http_server.c` - Gateway HTTP handler implementation
- `apps/c-gateway/src/nats_client_real.c` - NATS client implementation
- `scripts/smoke_c_gateway.sh` - Existing Gateway smoke script

