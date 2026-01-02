# Gateway DTO Consistency Report

**Date**: 2025-01-27  
**Step**: Gateway CP1-smoke DTO Verification  
**Status**: Analysis Complete  
**Version**: 1.0

## Executive Summary

Gateway DTO consistency verification completed for CP1-smoke. **REST request DTO structure has discrepancies** between `api-registry.md` and actual Gateway implementation (uses `task` object instead of `message` object). **REST response DTO mapping is missing** (Gateway sends Router response directly without transformation). **Error DTO structure has minor gaps** (missing `error.timestamp`, `error.trace_id` in error object). **NATS payload structure is mostly consistent** with `PROTO_NATS_MAPPING.md` (missing `timestamp_ms`).

**Key Findings**:
- ⚠️ **Request DTO**: Gateway expects `task` object, but `api-registry.md` specifies `message` object
- ❌ **Response DTO**: Gateway sends Router response directly, no mapping to REST DTO format
- ⚠️ **Error DTO**: Missing `error.timestamp` and `error.trace_id` (currently in `context`)
- ⚠️ **NATS Payload**: Missing `timestamp_ms` field
- ✅ **NATS Subject**: Correct (`beamline.router.v1.decide`)
- ✅ **Error Codes**: Mostly correct (missing `FORBIDDEN` → 403 mapping)

**CP1 Readiness**: ⚠️ **NEEDS FIXES** - Response DTO mapping is critical for CP1-smoke. Request DTO discrepancy needs clarification.

**Report Structure**:
- **Section 1**: Source of Truth
- **Section 2**: REST Request DTO Verification
- **Section 3**: REST Response DTO Verification
- **Section 4**: Error DTO Verification
- **Section 5**: NATS Payload Verification
- **Section 6**: Summary and Action Items
- **Section 7**: References

---

## 1. Source of Truth

### REST API Sources
- **API Registry**: `docs/ARCHITECTURE/api-registry.md` - REST API DTO definitions
- **API Contracts**: `docs/API_CONTRACTS.md` - Router ↔ CAF contracts (includes Gateway REST API)
- **Gateway Implementation**: `apps/c-gateway/src/http_server.c` - Gateway HTTP handler

### NATS Sources
- **NATS Subjects**: `docs/NATS_SUBJECTS.md`
- **Proto-NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- **NATS Client**: `apps/c-gateway/src/nats_client_real.c` - NATS client implementation

### Observability Sources
- **Observability Spec**: `docs/OBSERVABILITY.md` - Log format and health endpoint requirements
- **Gateway Logging**: `apps/c-gateway/src/http_server.c` - `log_error()` function (lines 577-598)
- **Gateway Health**: `apps/c-gateway/src/http_server.c` - `handle_health()` function (lines 1086-1089)

---

## 2. REST Request DTO Verification

### 2.1 Expected Structure (api-registry.md)

**Reference**: `docs/ARCHITECTURE/api-registry.md` (lines 48-75)

**Expected Request DTO**:
```json
{
  "message": {
    "message_id": "string (UUID)",
    "tenant_id": "string (required)",
    "trace_id": "string (optional)",
    "message_type": "chat | completion | embedding",
    "payload": "string (Base64 encoded)",
    "metadata": {},
    "timestamp_ms": "number (optional)"
  },
  "policy_id": "string (optional)",
  "context": {}
}
```

### 2.2 Actual Implementation (http_server.c)

**Reference**: `apps/c-gateway/src/http_server.c`

**Function**: `validate_decide_request()` (lines 831-885)

**Actual Validation**:
```c
// Validates:
- version (string, must be "1")
- tenant_id (string, required)
- request_id (string, required)
- task.type (string, required)
- task.payload (object, required)
```

**Function**: `build_route_request_json()` (lines 887-1031)

**Actual Request Structure**:
```json
{
  "version": "1",
  "tenant_id": "string",
  "request_id": "string",
  "trace_id": "string",
  "message": {
    "message_id": "string",
    "message_type": "string",
    "payload": "object",  // Note: object, not Base64 string
    "metadata": "object"
  },
  "policy_id": "string",
  "context": "object"
}
```

### 2.3 Discrepancies

| Field | Expected (api-registry.md) | Actual (Gateway) | Status |
|-------|---------------------------|------------------|--------|
| **Top-level structure** | `message` object | ✅ `message` object | ✅ Match |
| `message.message_id` | string (UUID) | ✅ string | ✅ Match |
| `message.tenant_id` | string (required) | ✅ string (from header or body) | ✅ Match |
| `message.trace_id` | string (optional) | ✅ string (from header or body) | ✅ Match |
| `message.message_type` | "chat" \| "completion" \| "embedding" | ✅ string | ✅ Match |
| `message.payload` | string (Base64 encoded) | ⚠️ object | ⚠️ **GAP**: Gateway accepts object, not Base64 string |
| `message.metadata` | object (optional) | ✅ object | ✅ Match |
| `message.timestamp_ms` | number (optional) | ❌ Not validated/added | ❌ **GAP**: Missing |
| `policy_id` | string (optional) | ✅ string (optional) | ✅ Match |
| `context` | object (optional) | ✅ object (optional) | ✅ Match |
| **Additional fields** | - | ⚠️ `version`, `request_id` | ⚠️ **GAP**: Gateway adds NATS-specific fields at REST level |

**Critical Finding**: Gateway validation (`validate_decide_request()`) expects `task` object, but `build_route_request_json()` uses `message` object. This suggests validation and building use different structures.

**Action Items**:
- [ ] Clarify: Does Gateway accept `task` or `message` structure?
- [ ] If `message`: Update validation to match `build_route_request_json()`
- [ ] If `task`: Update `build_route_request_json()` to use `task` or document the transformation
- [ ] Add `timestamp_ms` validation/adding if required
- [ ] Clarify `payload` format: Base64 string vs object

---

## 3. REST Response DTO Verification

### 3.1 Expected Structure (api-registry.md)

**Reference**: `docs/ARCHITECTURE/api-registry.md` (lines 63-75)

**Expected Response DTO**:
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

### 3.2 Actual Implementation (http_server.c)

**Reference**: `apps/c-gateway/src/http_server.c`

**Function**: `handle_decide()` (lines 1191-1274)

**Actual Response**:
```c
// Gateway receives Router response via nats_request_decide()
// Router response is sent directly via send_response() (line 1273)
// No mapping/transformation performed
```

**Router Response Structure** (from `PROTO_NATS_MAPPING.md`):
```json
{
  "ok": true,
  "decision": {
    "provider_id": "string",
    "reason": "weighted | sticky | fallback | policy",
    "priority": 0,
    "expected_latency_ms": 0,
    "expected_cost": 0.0,
    "metadata": {}
  },
  "context": {
    "request_id": "string",
    "trace_id": "string"
  }
}
```

### 3.3 Discrepancies

| Field | Expected (api-registry.md) | Actual (Router Response) | Status |
|-------|---------------------------|--------------------------|--------|
| `message_id` | string | ❌ Not in Router response | ❌ **GAP**: Missing (needs mapping from request) |
| `provider_id` | string | ✅ `decision.provider_id` | ⚠️ **GAP**: Needs mapping from `decision.provider_id` |
| `reason` | string | ✅ `decision.reason` | ⚠️ **GAP**: Needs mapping from `decision.reason` |
| `priority` | number | ✅ `decision.priority` | ⚠️ **GAP**: Needs mapping from `decision.priority` |
| `expected_latency_ms` | number | ✅ `decision.expected_latency_ms` | ⚠️ **GAP**: Needs mapping from `decision.expected_latency_ms` |
| `expected_cost` | number | ✅ `decision.expected_cost` | ⚠️ **GAP**: Needs mapping from `decision.expected_cost` |
| `currency` | string (default: "USD") | ❌ Not in Router response | ❌ **GAP**: Missing (Gateway should add default) |
| `trace_id` | string | ✅ `context.trace_id` | ⚠️ **GAP**: Needs mapping from `context.trace_id` |

**Critical Finding**: Gateway sends Router response directly without mapping to REST DTO format. This is a **critical gap** for CP1-smoke.

**Action Items**:
- [ ] Add mapping function to transform Router response to REST DTO format
- [ ] Map `decision.*` fields to top-level REST DTO fields
- [ ] Add `message_id` from request context
- [ ] Add `currency` field (default: "USD")
- [ ] Map `context.trace_id` to `trace_id`

---

## 4. Error DTO Verification

### 4.1 Expected Structure (api-registry.md)

**Reference**: `docs/ARCHITECTURE/api-registry.md` (lines 77-88)

**Expected Error DTO**:
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

### 4.2 Actual Implementation (http_server.c)

**Reference**: `apps/c-gateway/src/http_server.c`

**Function**: `send_error_response()` (lines 782-829)

**Actual Error Structure**:
```json
{
  "ok": false,
  "error": {
    "code": "string",
    "message": "string",
    "details": {}
  },
  "context": {
    "request_id": "string",
    "trace_id": "string"
  }
}
```

### 4.3 Discrepancies

| Field | Expected (api-registry.md) | Actual (Gateway) | Status |
|-------|---------------------------|------------------|--------|
| `error.code` | string | ✅ string | ✅ Match |
| `error.message` | string | ✅ string | ✅ Match |
| `error.details` | object | ✅ object | ✅ Match |
| `error.trace_id` | string | ❌ Not in `error` object | ❌ **GAP**: In `context.trace_id` instead |
| `error.timestamp` | ISO 8601 string | ❌ Not present | ❌ **GAP**: Missing |
| `ok` | - | ⚠️ `ok: false` | ⚠️ **GAP**: Gateway adds `ok` field (not in spec) |
| `context.request_id` | - | ⚠️ Present | ⚠️ **GAP**: Gateway adds `context` object (not in spec) |
| `context.trace_id` | - | ⚠️ Present | ⚠️ **GAP**: Gateway adds `context` object (not in spec) |

**Critical Finding**: Gateway error structure has `ok` and `context` fields that are not in `api-registry.md`. Also missing `error.timestamp` and `error.trace_id`.

**Action Items**:
- [ ] Add `error.timestamp` (ISO 8601 format)
- [ ] Add `error.trace_id` (or document that it's in `context.trace_id`)
- [ ] Clarify: Should Gateway include `ok` and `context` fields? (may be for consistency with Router response)
- [ ] Update `api-registry.md` or Gateway code to match

### 4.4 Error Codes Mapping

**Reference**: `docs/ARCHITECTURE/api-registry.md` (lines 90-97)

**Expected Error Codes**:
- `INVALID_REQUEST` → 400
- `UNAUTHORIZED` → 401
- `FORBIDDEN` → 403
- `NOT_FOUND` → 404
- `RATE_LIMIT_EXCEEDED` → 429
- `INTERNAL_ERROR` → 500
- `SERVICE_UNAVAILABLE` → 503

**Actual Implementation**: `map_router_error_status()` (lines 1033-1084)

| Error Code | HTTP Status | Status |
|------------|-------------|--------|
| `invalid_request` | 400 | ✅ Mapped |
| `unauthorized` | 401 | ✅ Mapped |
| `forbidden` | 403 | ❌ **GAP**: Not mapped |
| `policy_not_found` | 404 | ✅ Mapped (as `NOT_FOUND`) |
| `rate_limit_exceeded` | 429 | ✅ Handled separately |
| `internal` | 500 | ✅ Mapped (as `INTERNAL_ERROR`) |
| Router/NATS unavailable | 503 | ✅ Handled in `handle_decide()` |

**Action Items**:
- [ ] Add `forbidden` → 403 mapping if Router can return `forbidden` error code

---

## 5. NATS Payload Verification

### 5.1 Expected Structure (PROTO_NATS_MAPPING.md)

**Reference**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (lines 87-103)

**Expected NATS Request Payload**:
```json
{
  "version": "1",
  "request_id": "uuid",
  "message": {
    "message_id": "string",
    "tenant_id": "string",
    "trace_id": "string",
    "message_type": "chat",
    "payload": "base64_encoded",
    "metadata": {}
  },
  "policy_id": "string (optional)",
  "context": {}
}
```

### 5.2 Actual Implementation (http_server.c)

**Reference**: `apps/c-gateway/src/http_server.c`

**Function**: `build_route_request_json()` (lines 887-1031)

**Actual NATS Payload**:
```json
{
  "version": "1",
  "tenant_id": "string",
  "request_id": "string",
  "trace_id": "string",
  "message": {
    "message_id": "string",
    "message_type": "string",
    "payload": "object",  // Note: object, not Base64 string
    "metadata": "object"
  },
  "policy_id": "string",
  "context": "object"
}
```

### 5.3 Discrepancies

| Field | Expected (PROTO_NATS_MAPPING.md) | Actual (Gateway) | Status |
|-------|--------------------------------|------------------|--------|
| `version` | "1" | ✅ "1" | ✅ Match |
| `request_id` | uuid | ✅ string | ✅ Match |
| `message.message_id` | string | ✅ string | ✅ Match |
| `message.tenant_id` | string | ✅ string (top-level `tenant_id`) | ⚠️ **GAP**: In top-level, not in `message` |
| `message.trace_id` | string | ✅ string (top-level `trace_id`) | ⚠️ **GAP**: In top-level, not in `message` |
| `message.message_type` | "chat" | ✅ string | ✅ Match |
| `message.payload` | base64_encoded | ⚠️ object | ⚠️ **GAP**: Gateway sends object, not Base64 string |
| `message.metadata` | object | ✅ object | ✅ Match |
| `message.timestamp_ms` | number (optional) | ❌ Not added | ❌ **GAP**: Missing |
| `policy_id` | string (optional) | ✅ string (optional) | ✅ Match |
| `context` | object (optional) | ✅ object (optional) | ✅ Match |
| **Additional fields** | - | ⚠️ `tenant_id`, `trace_id` at top-level | ⚠️ **GAP**: Gateway adds fields at top-level |

**Critical Finding**: Gateway sends `tenant_id` and `trace_id` at top-level, but `PROTO_NATS_MAPPING.md` expects them in `message` object. Also missing `timestamp_ms`.

**Action Items**:
- [ ] Clarify: Should `tenant_id` and `trace_id` be in `message` object or top-level?
- [ ] Add `timestamp_ms` to NATS payload if required
- [ ] Clarify `payload` format: Base64 string vs object (Router may accept both)

### 5.4 NATS Subject

**Reference**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (line 85)

**Expected Subject**: `beamline.router.v1.decide`

**Actual Implementation**: `handle_decide()` (lines 1234-1239)

```c
const char *subject = getenv("ROUTER_DECIDE_SUBJECT");
if (!subject || subject[0] == '\0') {
    subject = "beamline.router.v1.decide";
}
```

**Status**: ✅ **MATCH** - Gateway uses correct NATS subject.

---

## 6. Observability Verification

### 6.1 Log Format

**Reference**: `docs/OBSERVABILITY.md`

**Expected Log Format**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "ERROR",
  "component": "gateway",
  "message": "Human-readable message",
  "context": {
    "request_id": "string",
    "trace_id": "string",
    "tenant_id": "string"
  }
}
```

**Actual Implementation**: `log_error()` (lines 577-598)

**Actual Log Format**:
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

**Discrepancies**:
- ❌ Missing `timestamp` (ISO 8601, UTC)
- ✅ `level` present (but lowercase "error" vs uppercase "ERROR")
- ✅ `component` present
- ✅ `message` present
- ⚠️ `context` not wrapped (fields at top-level)
- ❌ Missing `tenant_id` in log

**Action Items**:
- [ ] Add `timestamp` (ISO 8601, UTC)
- [ ] Wrap `request_id` and `trace_id` in `context` object (or document current structure)
- [ ] Add `tenant_id` to log

### 6.2 Health Endpoint

**Reference**: `docs/OBSERVABILITY.md`

**Expected Health Response**:
```json
{
  "status": "ok" | "degraded" | "unhealthy",
  "nats": {
    "connected": boolean
  },
  "timestamp_ms": integer
}
```

**Actual Implementation**: `handle_health()` (lines 1086-1089)

**Actual Health Response**:
```json
{
  "status": "ok"
}
```

**Discrepancies**:
- ✅ `status` present
- ❌ Missing `nats.connected`
- ❌ Missing `timestamp_ms`

**Action Items**:
- [ ] Add `nats.connected` (check NATS connection status)
- [ ] Add `timestamp_ms` (current timestamp in milliseconds)

---

## 7. Summary and Action Items

### 7.1 Critical Gaps (Blocking CP1-Smoke)

1. **Response DTO Mapping** ❌
   - **Issue**: Gateway sends Router response directly, no mapping to REST DTO format
   - **Impact**: REST response doesn't match `api-registry.md`
   - **Action**: Add mapping function to transform Router response to REST DTO

2. **Request DTO Structure** ⚠️
   - **Issue**: Gateway validation expects `task` object, but `build_route_request_json()` uses `message` object
   - **Impact**: Unclear which structure Gateway actually accepts
   - **Action**: Clarify and align validation with building function

### 7.2 Non-Critical Gaps (Documentation/Enhancement)

1. **Error DTO Structure** ⚠️
   - **Issue**: Missing `error.timestamp` and `error.trace_id` (in `context` instead)
   - **Action**: Add `error.timestamp`, clarify `error.trace_id` location

2. **NATS Payload Structure** ⚠️
   - **Issue**: `tenant_id` and `trace_id` at top-level vs in `message` object, missing `timestamp_ms`
   - **Action**: Clarify structure, add `timestamp_ms` if required

3. **Log Format** ⚠️
   - **Issue**: Missing `timestamp`, `tenant_id`, and `context` wrapper
   - **Action**: Add missing fields, wrap context fields

4. **Health Endpoint** ⚠️
   - **Issue**: Missing `nats.connected` and `timestamp_ms`
   - **Action**: Add missing fields

### 7.3 Priority Actions for CP1-Smoke

**High Priority**:
1. Add Response DTO mapping function
2. Clarify Request DTO structure (task vs message)
3. Add `error.timestamp` to error responses

**Medium Priority**:
4. Add `nats.connected` and `timestamp_ms` to health endpoint
5. Add `timestamp` and `tenant_id` to logs
6. Clarify NATS payload structure (tenant_id/trace_id location)

**Low Priority**:
7. Add `timestamp_ms` to NATS payload
8. Add `forbidden` → 403 error code mapping

---

## 8. References

- **API Registry**: `docs/ARCHITECTURE/api-registry.md` - REST API DTO definitions
- **Proto-NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto ↔ NATS subject mapping
- **Observability**: `docs/OBSERVABILITY.md` - Observability requirements
- **Gateway Implementation**: `apps/c-gateway/src/http_server.c` - Gateway HTTP handler
- **NATS Client**: `apps/c-gateway/src/nats_client_real.c` - NATS client implementation
- **CP1 Smoke Plan**: `docs/archive/dev/GATEWAY_CP1_SMOKE_PLAN.md` - CP1-smoke plan
- **CP1 Smoke Checklist**: `docs/archive/dev/GATEWAY_CP1_SMOKE_CHECKLIST.md` - Detailed checklist

---

## 9. Related Documents

- **Action Plan**: `docs/archive/dev/GATEWAY_DTO_ACTION_PLAN.md` (to be created) - Prioritized action items
- **CP1 Detailed Plan**: `docs/archive/dev/GATEWAY_DTO_CP1_DETAILED_PLAN.md` (to be created) - Step-by-step CP1 instructions
- **Summary**: `docs/archive/dev/GATEWAY_DTO_CONSISTENCY_SUMMARY.md` (to be created) - Brief version for CP reports

