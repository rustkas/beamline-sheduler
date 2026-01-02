# Gateway ↔ Router CP1 Integration Gap Analysis

**Date**: 2025-01-27  
**Status**: Analysis Complete  
**Version**: 1.0

## Purpose

This document identifies gaps between CP1 Gateway↔Router integration requirements and current implementation, mapping scenarios to smoke/E2E tests.

---

## CP1 Integration Scenarios Matrix

### Success Paths

| Scenario | Smoke Test | E2E Test | Gap Status |
|----------|-----------|----------|------------|
| **Basic Routing: Gateway → NATS → Router → Response** | ⚠️ Partial | ⚠️ Partial | ⚠️ **GAP: Needs full E2E test** |
| **Policy-based Routing: Different policies** | ❌ No test | ❌ No test | ⚠️ **GAP: Needs test** |
| **Sticky Sessions: Session affinity** | ❌ No test | ❌ No test | ⚠️ **GAP: Needs test (if CP1)** |

### Error Scenarios

| Scenario | Smoke Test | E2E Test | Gap Status |
|----------|-----------|----------|------------|
| **Bad Request (REST → NATS): Invalid JSON body** | ⚠️ Partial | ❌ No test | ⚠️ **GAP: Needs explicit test** |
| **Router Errors: Router returns error → Gateway propagates** | ⚠️ Partial | ❌ No test | ⚠️ **GAP: Needs explicit test** |
| **NATS Unavailable: Gateway handles NATS connection failure** | ❌ No test | ❌ No test | ⚠️ **GAP: Needs test** |
| **Timeout: Router timeout → Gateway returns timeout error** | ❌ No test | ❌ No test | ⚠️ **GAP: Needs test** |

---

## Detailed Gap Analysis

### Gap 1: Full E2E Test Not Implemented

**Requirement**: Full end-to-end test (Gateway → NATS → Router → Response)

**Current State**:
- `scripts/gateway_router_contract_smoke.sh:204-220` - `run_full_e2e()` function exists but not implemented
- `router_gateway_contract_smoke_SUITE.erl` - Router contract test exists
- Gateway contract test may exist (needs verification)

**Gap**:
- ❌ Full E2E test not implemented
- ⚠️ Only Router contract test exists
- ⚠️ Gateway contract test may not exist

**Priority**: **HIGH**

**Action Required**:
- Implement full E2E test in `scripts/gateway_router_contract_smoke.sh:run_full_e2e()`
- Start NATS server (or use mock)
- Start Router in mock mode
- Start Gateway (or use test client)
- Send DecideRequest via NATS
- Verify DecideResponse structure and headers

---

### Gap 2: Policy-based Routing Test

**Requirement**: Test routing with different policies

**Current State**:
- Router has policy-based routing in `router_core.erl`
- No Gateway↔Router test for different policies

**Gap**:
- ❌ No test for policy-based routing in Gateway↔Router integration
- ⚠️ Router tests exist, but not Gateway↔Router integration

**Priority**: **MEDIUM**

**Action Required**:
- Add test in `router_gateway_contract_smoke_SUITE.erl` for different policies
- Test policy selection and routing decisions

---

### Gap 3: Sticky Sessions Test (if CP1)

**Requirement**: Test sticky session affinity (if part of CP1)

**Current State**:
- Router has sticky sessions in `router_sticky_store.erl`
- No Gateway↔Router test for sticky sessions

**Gap**:
- ❌ No test for sticky sessions in Gateway↔Router integration
- ⚠️ Router tests exist, but not Gateway↔Router integration

**Priority**: **MEDIUM** (if CP1)

**Action Required**:
- Verify if sticky sessions are CP1 or CP2+ feature
- If CP1, add test in `router_gateway_contract_smoke_SUITE.erl`
- Test session affinity and routing decisions

---

### Gap 4: Bad Request (Invalid JSON) Test

**Requirement**: Test Gateway handling of invalid JSON body

**Current State**:
- Router handles invalid JSON in `router_nats_subscriber.erl:89-119`
- No Gateway↔Router test for invalid JSON

**Gap**:
- ❌ No Gateway↔Router test for invalid JSON
- ⚠️ Router tests exist, but not Gateway↔Router integration

**Priority**: **MEDIUM**

**Action Required**:
- Add test in `router_gateway_contract_smoke_SUITE.erl` for invalid JSON
- Test error response propagation from Router to Gateway

---

### Gap 5: Router Error Propagation Test

**Requirement**: Test Router error propagation to Gateway

**Current State**:
- Router has error handling in `router_error.erl`
- No Gateway↔Router test for error propagation

**Gap**:
- ❌ No Gateway↔Router test for error propagation
- ⚠️ Router tests exist, but not Gateway↔Router integration

**Priority**: **HIGH**

**Action Required**:
- Add test in `router_gateway_contract_smoke_SUITE.erl` for error propagation
- Test various Router errors (policy_not_found, missing_tenant_id, etc.)
- Verify Gateway receives correct error response

---

### Gap 6: NATS Unavailable Test

**Requirement**: Test Gateway handling of NATS unavailability

**Current State**:
- Router falls back to mock mode on NATS unavailability
- No Gateway↔Router test for NATS unavailability

**Gap**:
- ❌ No Gateway↔Router test for NATS unavailability
- ⚠️ Router tests exist, but not Gateway↔Router integration

**Priority**: **MEDIUM**

**Action Required**:
- Add test in `router_gateway_contract_smoke_SUITE.erl` for NATS unavailability
- Test Gateway error handling when NATS is unavailable
- Verify error response format

---

### Gap 7: Timeout Test

**Requirement**: Test Router timeout → Gateway timeout error

**Current State**:
- Router has timeout handling in `router_nats.erl`
- No Gateway↔Router test for timeout

**Gap**:
- ❌ No Gateway↔Router test for timeout
- ⚠️ Router tests exist, but not Gateway↔Router integration

**Priority**: **MEDIUM**

**Action Required**:
- Add test in `router_gateway_contract_smoke_SUITE.erl` for timeout
- Test Gateway timeout handling
- Verify error response format

---

## Test Coverage Gaps

### Missing Smoke Tests

1. **Full E2E Test**:
   - ❌ `run_full_e2e()` not implemented in `scripts/gateway_router_contract_smoke.sh`
   - ⚠️ Only Router contract test exists

2. **Policy-based Routing**:
   - ❌ No test for different policies
   - ⚠️ Router tests exist, but not Gateway↔Router integration

3. **Sticky Sessions** (if CP1):
   - ❌ No test for sticky sessions
   - ⚠️ Router tests exist, but not Gateway↔Router integration

### Missing Error Tests

4. **Bad Request (Invalid JSON)**:
   - ❌ No Gateway↔Router test for invalid JSON
   - ⚠️ Router tests exist, but not Gateway↔Router integration

5. **Router Error Propagation**:
   - ❌ No Gateway↔Router test for error propagation
   - ⚠️ Router tests exist, but not Gateway↔Router integration

6. **NATS Unavailable**:
   - ❌ No Gateway↔Router test for NATS unavailability
   - ⚠️ Router tests exist, but not Gateway↔Router integration

7. **Timeout**:
   - ❌ No Gateway↔Router test for timeout
   - ⚠️ Router tests exist, but not Gateway↔Router integration

---

## Implementation Priority

### High Priority (Blocking CP1)

1. **Gap 1: Full E2E Test** - Implement full E2E test
2. **Gap 5: Router Error Propagation Test** - Add error propagation test

### Medium Priority (Non-blocking)

3. **Gap 2: Policy-based Routing Test** - Add policy-based routing test
4. **Gap 3: Sticky Sessions Test** - Add sticky sessions test (if CP1)
5. **Gap 4: Bad Request Test** - Add invalid JSON test
6. **Gap 6: NATS Unavailable Test** - Add NATS unavailable test
7. **Gap 7: Timeout Test** - Add timeout test

---

## Code References

### Smoke Test Script

- **File**: `scripts/gateway_router_contract_smoke.sh`
  - **Function**: `run_full_e2e()` - Full E2E test (not implemented)
  - **Location**: `scripts/gateway_router_contract_smoke.sh:204-220`
  - **Status**: ⚠️ Not implemented

- **File**: `scripts/gateway_router_contract_smoke.sh`
  - **Function**: `run_router_test()` - Router contract test
  - **Location**: `scripts/gateway_router_contract_smoke.sh:145-171`
  - **Status**: ✅ Implemented

### Router Contract Test

- **File**: `apps/otp/router/test/router_gateway_contract_smoke_SUITE.erl` (if exists)
  - **Status**: ⚠️ Needs verification

### Gateway Contract Test

- **File**: `apps/gateway/test/contract/router-contract.spec.ts` (if exists)
  - **Status**: ⚠️ Needs verification

---

## Integration Test Files

### Existing Integration Tests

1. **`tests/integration/router-gateway-caf.e2e.test.ts`**:
   - ⚠️ E2E test for Router↔Gateway↔CAF
   - Status: Needs review for CP1 scenarios

2. **`tests/integration/nats_router_gateway.test.ts`**:
   - ⚠️ NATS Router↔Gateway test
   - Status: Needs review for CP1 scenarios

3. **`apps/ui_web/test/ui_web/integration/gateway_contract_test.exs`**:
   - ⚠️ Gateway contract test (Elixir)
   - Status: Needs review for CP1 scenarios

---

## Recommendations

### Immediate Actions (Before CP1 Acceptance)

1. **Implement Full E2E Test**:
   - Implement `run_full_e2e()` in `scripts/gateway_router_contract_smoke.sh`
   - Start NATS server (or use mock)
   - Start Router in mock mode
   - Start Gateway (or use test client)
   - Send DecideRequest via NATS
   - Verify DecideResponse structure and headers

2. **Add Router Error Propagation Test**:
   - Add test in `router_gateway_contract_smoke_SUITE.erl` for error propagation
   - Test various Router errors
   - Verify Gateway receives correct error response

### Short-term Actions (CP1 Enhancement)

3. **Add Policy-based Routing Test**:
   - Add test for different policies
   - Test policy selection and routing decisions

4. **Add Error Scenario Tests**:
   - Add test for invalid JSON
   - Add test for NATS unavailability
   - Add test for timeout

5. **Add Sticky Sessions Test** (if CP1):
   - Verify if sticky sessions are CP1 or CP2+ feature
   - If CP1, add test for sticky sessions

---

## References

- **CP1 Acceptance Report**: `docs/CP1_ACCEPTANCE_REPORT.md`
- **Gateway↔Router Contract Smoke**: `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md`
- **Smoke Test Script**: `scripts/gateway_router_contract_smoke.sh`
- **Proto/NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- **API Contracts**: `docs/API_CONTRACTS.md`

