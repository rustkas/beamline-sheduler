# Router CP1 Gap Analysis

**Date**: 2025-01-27  
**Status**: Analysis Complete  
**Version**: 1.0

## Purpose

This document identifies gaps between CP1 requirements and current Router implementation, mapping requirements to code and tests.

---

## CP1 Requirements Matrix

### Behavioral Invariants

| CP1 Requirement | Code Implementation | Test Coverage | Gap Status |
|----------------|-------------------|---------------|------------|
| **Retry Policy** | `router_caf_adapter.erl:130-180` (exponential backoff) | ⚠️ Partial | ⚠️ CP2+ feature, needs CP1 scope clarification |
| **Error Handling** | `router_error.erl` (gRPC status mapping) | ✅ `router_error_SUITE.erl` | ✅ Complete |
| **Idempotency** | `router_idempotency.erl` | ⚠️ `router_idempotency_SUITE.erl` (CP2+) | ⚠️ **CP2+ feature, needs CP1 scope separation** |
| **Routing Rules** | `router_core.erl`, `router_decider.erl` | ✅ `router_core_SUITE.erl`, `router_decider_SUITE.erl` | ✅ Complete |

### Error Scenarios

| Error Scenario | Code Implementation | Test Coverage | Gap Status |
|---------------|-------------------|---------------|------------|
| **NATS Subject Not Available** | ⚠️ Not explicitly handled | ❌ No test | ⚠️ **GAP: Needs implementation** |
| **NATS Unavailable** | ✅ `router_nats.erl:54-58` (falls back to mock) | ⚠️ Partial | ⚠️ **GAP: Needs explicit error response** |
| **Invalid Payload (Schema Error)** | ✅ `router_nats_subscriber.erl:89-119` (JSON parse error) | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **Internal Router Error** | ✅ `router_core.erl:116-148` (error telemetry) | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **No Route Found** | ✅ `router_core.erl:72-84` (policy_not_found) | ✅ `router_core_SUITE.erl:test_policy_not_found` | ✅ Complete |

---

## Detailed Gap Analysis

### Gap 1: NATS Subject Not Available

**Requirement**: Router should handle case when NATS subject is not available (e.g., subject doesn't exist, subscription fails)

**Current State**:
- `router_nats_subscriber.erl:30` - Subscribes to subject, but if subscription fails, `init/1` returns `{stop, Error}`
- No explicit error response sent to client
- No test coverage

**Gap**:
- ❌ No explicit handling for subscription failure
- ❌ No error response sent to client
- ❌ No test coverage

**Priority**: **HIGH**

**Action Required**:
- Add explicit error handling in `router_nats_subscriber.erl:init/1`
- Send error response if subscription fails
- Add test in `router_error_SUITE.erl`

---

### Gap 2: NATS Unavailable - Explicit Error Response

**Requirement**: When NATS is unavailable, Router should return explicit error response (not just fall back to mock)

**Current State**:
- `router_nats.erl:54-58` - Falls back to mock mode on connection failure
- `router_nats_subscriber.erl` - Uses mock mode if NATS unavailable
- No explicit error response sent to client

**Gap**:
- ⚠️ Falls back to mock mode (silent degradation)
- ❌ No explicit error response for NATS unavailability
- ⚠️ Partial test coverage (mock mode tests exist, but not error response)

**Priority**: **MEDIUM**

**Action Required**:
- Add explicit error response when NATS unavailable (instead of silent mock fallback)
- Return `service_unavailable` error code
- Add test in `router_error_SUITE.erl`

---

### Gap 3: Invalid Payload - Explicit Test

**Requirement**: Router should handle invalid payloads (malformed JSON, schema errors) with explicit error response

**Current State**:
- `router_nats_subscriber.erl:89-119` - Handles JSON parse errors
- `router_nats_subscriber.erl:86` - Handles payload size errors
- Sends `invalid_request` error response
- ⚠️ No explicit test for invalid payload scenarios

**Gap**:
- ✅ Error handling exists
- ❌ No explicit test coverage

**Priority**: **MEDIUM**

**Action Required**:
- Add test in `router_error_SUITE.erl` for invalid payload scenarios
- Test malformed JSON
- Test payload size exceeded
- Test schema validation errors

---

### Gap 4: Internal Router Error - Explicit Test

**Requirement**: Router should handle internal errors gracefully with explicit error response

**Current State**:
- `router_core.erl:116-148` - Handles errors and emits telemetry
- Error mapping in `router_error.erl`
- ⚠️ No explicit test for internal errors

**Gap**:
- ✅ Error handling exists
- ❌ No explicit test coverage for internal errors

**Priority**: **LOW**

**Action Required**:
- Add test in `router_error_SUITE.erl` for internal errors
- Test error mapping to `internal` error code

---

### Gap 5: CP1 Idempotency Scope

**Requirement**: Clarify CP1 idempotency scope vs CP2+ idempotency features

**Current State**:
- `router_idempotency.erl` - Full idempotency layer (CP2+ feature)
- `beamline_router.app.src:67` - `idempotency_enabled` flag (CP2+)
- Used in `router_result_consumer.erl`, `router_ack_consumer.erl` (CP2+ features)

**Gap**:
- ⚠️ Idempotency is CP2+ feature, but CP1 requirements may need minimal guarantees
- ❌ CP1 idempotency scope not clearly defined
- ❌ No CP1-specific idempotency tests

**Priority**: **HIGH**

**Action Required**:
- Review CP1 acceptance reports for idempotency requirements
- Separate CP1 idempotency scope from CP2+ features
- Document CP1 idempotency guarantees (if any)
- Add CP1-specific idempotency tests (if required)

---

## Test Coverage Gaps

### Missing Tests

1. **NATS Subject Not Available**:
   - ❌ No test in `router_error_SUITE.erl`
   - ❌ No test in `router_nats_subscriber_SUITE.erl` (if exists)

2. **NATS Unavailable - Error Response**:
   - ⚠️ Mock mode tests exist, but no explicit error response test
   - ❌ No test for `service_unavailable` error code

3. **Invalid Payload**:
   - ⚠️ Partial coverage (JSON parse error handling exists, but no explicit test)
   - ❌ No test for malformed JSON
   - ❌ No test for payload size exceeded
   - ❌ No test for schema validation errors

4. **Internal Router Error**:
   - ❌ No explicit test for internal error handling
   - ❌ No test for error mapping to `internal` error code

5. **CP1 Idempotency**:
   - ❌ No CP1-specific idempotency tests (if required)
   - ⚠️ Only CP2+ idempotency tests exist

---

## Implementation Priority

### High Priority (Blocking CP1)

1. **Gap 5: CP1 Idempotency Scope** - Clarify CP1 vs CP2+ scope
2. **Gap 1: NATS Subject Not Available** - Add explicit error handling

### Medium Priority (Non-blocking)

3. **Gap 2: NATS Unavailable - Explicit Error Response** - Add explicit error response
4. **Gap 3: Invalid Payload - Explicit Test** - Add test coverage

### Low Priority (Nice to Have)

5. **Gap 4: Internal Router Error - Explicit Test** - Add test coverage

---

## Code References

### Error Handling

- **Module**: `router_error.erl`
  - **Function**: `to_grpc/1`, `to_grpc/2` - Error mapping to gRPC status codes
  - **Location**: `apps/otp/router/src/router_error.erl:38-56`
  - **Test**: `router_error_SUITE.erl`

- **Module**: `router_core.erl`
  - **Function**: `route/2` - Main routing logic with error handling
  - **Location**: `apps/otp/router/src/router_core.erl:14-154`
  - **Test**: `router_core_SUITE.erl`

- **Module**: `router_nats_subscriber.erl`
  - **Function**: `handle_nats_message/2` - NATS message handling with error responses
  - **Location**: `apps/otp/router/src/router_nats_subscriber.erl:74-120`
  - **Test**: ⚠️ No explicit test suite

### NATS Integration

- **Module**: `router_nats.erl`
  - **Function**: `init/1` - NATS connection with fallback to mock
  - **Location**: `apps/otp/router/src/router_nats.erl:30-63`
  - **Test**: ⚠️ No explicit test suite

### Idempotency

- **Module**: `router_idempotency.erl`
  - **Function**: `check_and_mark/2`, `check_and_mark/3` - Idempotency checks
  - **Location**: `apps/otp/router/src/router_idempotency.erl:28-38`
  - **Test**: `router_idempotency_SUITE.erl` (CP2+)
  - **Status**: ⚠️ CP2+ feature, needs CP1 scope clarification

---

## Test Suite Coverage

### Existing Test Suites

1. **router_core_SUITE.erl**:
   - ✅ `test_policy_parsing` - Policy parsing
   - ✅ `test_basic_decision` - Basic routing decision
   - ✅ `test_missing_tenant_id` - Missing tenant_id error
   - ✅ `test_empty_tenant_id` - Empty tenant_id error
   - ✅ `test_policy_not_found` - Policy not found error
   - ✅ `test_weighted_routing` - Weighted routing
   - ✅ `test_fallback` - Fallback routing
   - ⚠️ Missing: Internal error test

2. **router_error_SUITE.erl**:
   - ✅ `test_error_mapping_table` - Error mapping table structure
   - ✅ `test_to_grpc_basic` - Basic error mapping
   - ✅ `test_to_grpc_with_context` - Error mapping with context
   - ✅ `test_to_grpc_unknown` - Unknown error mapping
   - ⚠️ Missing: NATS unavailable test
   - ⚠️ Missing: Invalid payload test
   - ⚠️ Missing: Internal error test

3. **router_e2e_smoke_SUITE.erl**:
   - ✅ `test_happy_path_rbac_to_audit` - E2E happy path
   - ⚠️ Missing: Error path tests

---

## Recommendations

### Immediate Actions (Before CP1 Acceptance)

1. **Clarify CP1 Idempotency Scope**:
   - Review CP1 acceptance reports
   - Document CP1 idempotency guarantees (if any)
   - Separate from CP2+ idempotency features

2. **Add NATS Subject Not Available Handling**:
   - Add explicit error handling in `router_nats_subscriber.erl:init/1`
   - Send error response if subscription fails
   - Add test coverage

### Short-term Actions (CP1 Enhancement)

3. **Add NATS Unavailable Error Response**:
   - Add explicit error response (instead of silent mock fallback)
   - Return `service_unavailable` error code
   - Add test coverage

4. **Add Invalid Payload Tests**:
   - Add tests for malformed JSON
   - Add tests for payload size exceeded
   - Add tests for schema validation errors

5. **Add Internal Error Tests**:
   - Add test for internal error handling
   - Add test for error mapping to `internal` error code

---

## References

- **CP1 Acceptance Report**: `docs/CP1_ACCEPTANCE_REPORT.md`
- **Router CP1 Report**: `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md`
- **CP1 Boundaries**: `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`
- **Error Handling**: `apps/otp/router/src/router_error.erl`
- **Core Routing**: `apps/otp/router/src/router_core.erl`
- **NATS Integration**: `apps/otp/router/src/router_nats.erl`
- **NATS Subscriber**: `apps/otp/router/src/router_nats_subscriber.erl`

