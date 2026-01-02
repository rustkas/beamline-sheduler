# ACL E2E/Integration Test Plan (CP3/Pre-Release)

**Date**: 2025-01-27  
**Status**: 📋 **Test Plan**  
**Scope**: ⏭ **CP3/Pre-Release** (not CP2)  
**Purpose**: Define minimal E2E/integration test set for ACL (tenant validation)  
**Related**: `apps/otp/router/docs/ACL_MODEL.md`, `apps/otp/router/src/router_tenant_validator.erl`

## Scope: CP3/Pre-Release

**CP2 Status**: ✅ **COMPLETE** (unit tests and basic integration)

**CP3/Pre-Release Scope**: E2E/integration tests for tenant validation in typical routes

**Rationale**: CP2 includes unit tests and basic integration. E2E tests are deferred to CP3/Pre-Release to keep CP2 focused and complete.

## ACL Model Reference

**Source of Truth**: `apps/otp/router/docs/ACL_MODEL.md`

**Key Points**:
- **Policy Registry** (`router_policy_store`) and **Allowlist** are sources of truth for permissions
- **Single entry point**: `router_tenant_validator:validate_tenant/2`
- **Deprecated**: `router_acl.erl` (not used in production code paths)

## Test Scenarios

### 1. Tenant Validation in Router → Gateway Flow

**Purpose**: Verify tenant validation works in typical Router → Gateway request flow

**Test Case**: `test_tenant_validation_router_gateway_flow/1`

**Steps**:
1. Gateway receives request with `tenant_id` in headers
2. Gateway forwards request to Router via NATS
3. Router validates tenant using `router_tenant_validator:validate_tenant/2`
4. Router processes request if tenant is allowed
5. Router returns response to Gateway

**Verifications**:
- ✅ Allowed tenant: Request processed successfully
- ✅ Denied tenant: Request rejected with proper error code
- ✅ Unknown tenant: Request rejected with proper error code
- ✅ Metrics: `router_tenant_audit_total` incremented on deny
- ✅ Logs: Audit entries created on deny

**Test Files**:
- `apps/otp/router/test/router_tenant_validation_e2e_SUITE.erl` (new)
- `tests/integration/router-gateway-tenant-validation.test.ts` (new)

---

### 2. Tenant Validation in Result Consumer

**Purpose**: Verify tenant validation in result handling path

**Test Case**: `test_tenant_validation_result_consumer/1`

**Steps**:
1. Result message arrives at Router via NATS
2. `router_result_consumer` validates tenant using `router_tenant_validator:validate_tenant/2`
3. Result processed if tenant is allowed
4. Result rejected if tenant is denied

**Verifications**:
- ✅ Allowed tenant: Result processed successfully
- ✅ Denied tenant: Result rejected, metric `router_results_tenant_rejected_total` incremented
- ✅ Unknown tenant: Result rejected with proper error code
- ✅ Audit entries created on deny

**Test Files**:
- `apps/otp/router/test/router_result_consumer_tenant_validation_SUITE.erl` (new)
- Integration with existing `router_result_consumer_SUITE.erl`

---

### 3. Tenant Validation in ACK Consumer

**Purpose**: Verify tenant validation in ACK handling path

**Test Case**: `test_tenant_validation_ack_consumer/1`

**Steps**:
1. ACK message arrives at Router via NATS
2. `router_ack_consumer` validates tenant using `router_tenant_validator:validate_tenant/2`
3. ACK processed if tenant is allowed
4. ACK rejected if tenant is denied

**Verifications**:
- ✅ Allowed tenant: ACK processed successfully
- ✅ Denied tenant: ACK rejected, metric `router_acks_tenant_rejected_total` incremented
- ✅ Unknown tenant: ACK rejected with proper error code
- ✅ Audit entries created on deny

**Test Files**:
- `apps/otp/router/test/router_ack_consumer_tenant_validation_SUITE.erl` (new)
- Integration with existing `router_ack_consumer_SUITE.erl`

---

### 4. Negative Cases: Unknown Tenant

**Purpose**: Verify handling of unknown tenants (not in allowlist, no policy)

**Test Case**: `test_unknown_tenant_rejection/1`

**Steps**:
1. Request arrives with `tenant_id` not in allowlist
2. No policy exists for tenant
3. Router validates tenant using `router_tenant_validator:validate_tenant/2`
4. Router rejects request

**Verifications**:
- ✅ Request rejected with proper error code
- ✅ Metric `router_tenant_audit_total` incremented
- ✅ Audit entry created with `action: "tenant_denied"`, `reason: "unknown_tenant"`
- ✅ Log entry created with tenant_id and rejection reason

**Test Files**:
- `apps/otp/router/test/router_tenant_validation_negative_SUITE.erl` (new)

---

### 5. Negative Cases: Policy Deny

**Purpose**: Verify handling of tenants with explicit deny policy

**Test Case**: `test_policy_deny_rejection/1`

**Steps**:
1. Request arrives with `tenant_id` in allowlist
2. Policy exists for tenant with `allow: false`
3. Router validates tenant using `router_tenant_validator:validate_tenant/2`
4. Router rejects request

**Verifications**:
- ✅ Request rejected with proper error code
- ✅ Metric `router_tenant_audit_total` incremented
- ✅ Audit entry created with `action: "tenant_denied"`, `reason: "policy_deny"`
- ✅ Log entry created with tenant_id, policy_id, and rejection reason

**Test Files**:
- `apps/otp/router/test/router_tenant_validation_negative_SUITE.erl` (new)

---

### 6. Policy Updates and Validation

**Purpose**: Verify tenant validation reflects policy updates

**Test Case**: `test_policy_update_validation/1`

**Steps**:
1. Tenant initially allowed (policy `allow: true`)
2. Request processed successfully
3. Policy updated to `allow: false`
4. New request rejected

**Verifications**:
- ✅ Initial request processed successfully
- ✅ Policy update reflected in validation
- ✅ New request rejected with proper error code
- ✅ Audit entries created for both allow and deny

**Test Files**:
- `apps/otp/router/test/router_tenant_validation_policy_update_SUITE.erl` (new)

---

## Test Implementation Plan

### Phase 1: Router E2E Tests (CP3/Pre-Release)

**Priority**: High

**Test Suites**:
1. `router_tenant_validation_e2e_SUITE.erl` - E2E tenant validation tests
2. `router_tenant_validation_negative_SUITE.erl` - Negative test cases
3. `router_tenant_validation_policy_update_SUITE.erl` - Policy update tests

**Integration Points**:
- `router_result_consumer_SUITE.erl` - Add tenant validation test cases
- `router_ack_consumer_SUITE.erl` - Add tenant validation test cases

### Phase 2: Gateway → Router Integration Tests (CP3/Pre-Release)

**Priority**: Medium

**Test Suites**:
1. `tests/integration/router-gateway-tenant-validation.test.ts` - Gateway → Router E2E tests

**Verifications**:
- Gateway forwards tenant_id correctly
- Router validates tenant correctly
- Response codes match expected behavior

### Phase 3: Full E2E Flow Tests (CP3/Pre-Release)

**Priority**: Low

**Test Scenarios**:
- Full flow: Gateway → Router → Worker → Usage with tenant validation
- Multiple tenants in parallel
- Policy updates during active requests

## Acceptance Criteria

### CP3/Pre-Release Completion

**✅ All Test Scenarios Implemented**:
- ✅ Tenant validation in Router → Gateway flow
- ✅ Tenant validation in result consumer
- ✅ Tenant validation in ACK consumer
- ✅ Negative cases (unknown tenant, policy deny)
- ✅ Policy updates and validation

**✅ Test Coverage**:
- ✅ All typical routes covered (decide, result, ACK)
- ✅ Negative cases covered (unknown tenant, policy deny)
- ✅ Policy update scenarios covered

**✅ Integration**:
- ✅ Router E2E tests pass
- ✅ Gateway → Router integration tests pass
- ✅ Full E2E flow tests pass (if implemented)

## References

- `apps/otp/router/docs/ACL_MODEL.md`: ACL model specification (source of truth)
- `apps/otp/router/src/router_tenant_validator.erl`: Primary ACL implementation
- `apps/otp/router/src/router_policy_store.erl`: Policy registry (source of truth for policies)
- `apps/otp/router/src/router_acl.erl`: **Deprecated** (not used, kept for backward compatibility)
- `docs/archive/dev/ACL_MODEL_FORMALIZATION_REPORT.md`: ACL formalization report

