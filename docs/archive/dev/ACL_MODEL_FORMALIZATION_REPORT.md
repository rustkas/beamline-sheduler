# ACL Model Formalization Report

**Date**: 2025-01-27  
**Status**: ✅ **Complete**  
**Purpose**: Formalize ACL model and eliminate ambiguity between `router_acl.erl` and `router_tenant_validator.erl`  
**Related**: `apps/otp/router/docs/ACL_MODEL.md`, `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md`

## Summary

Completed formalization of ACL model for Router, eliminating ambiguity and closing "partially implemented" status in CP2 checklist.

**Key Decisions**:
- ✅ `router_tenant_validator.erl` is the **single source of truth** for ACL decisions
- ✅ `router_acl.erl` is **deprecated** (not used in production code paths)
- ✅ Formal ACL model documented in `apps/otp/router/docs/ACL_MODEL.md`

## Task 1.1: Formal ACL Model

### Created: `apps/otp/router/docs/ACL_MODEL.md`

**Content**:
- **Source of Truth**: Policy Registry (`router_policy_store`) + Allowlist (static config)
- **Decision Flow**: `router_tenant_validator:validate_tenant/2` is the single entry point
- **Deprecated Module**: `router_acl.erl` marked as deprecated
- **Implementation Guidelines**: Migration path from `router_acl` to `router_tenant_validator`
- **Testing**: Test coverage requirements

**Key Points**:
1. **Policy Registry** (`router_policy_store.erl`):
   - Primary source for tenant validation
   - Policy existence = tenant is valid
   - Policies indexed by `{TenantId, PolicyId}`

2. **Allowlist** (static configuration):
   - Secondary source for tenant validation
   - Optional: if not configured, check passes
   - Used for static tenant filtering

3. **`router_tenant_validator`**:
   - **Single entry point** for tenant validation
   - Combines allowlist and policy registry checks
   - Returns `{ok, TenantId}` or `{error, Reason, Context}`

## Task 1.2: Code Alignment

### Updated: `apps/otp/router/src/router_acl.erl`

**Changes**:
- Added deprecation notice at module level
- Marked as **DEPRECATED / TECH DEBT**
- Added migration path documentation
- Reference to `ACL_MODEL.md`

**Status**: Module exists but is **not used** in any production code paths.

**Current Usage**:
- ✅ `router_tenant_validator.erl` used in:
  - `router_result_consumer.erl`
  - `router_ack_consumer.erl`
  - `router_intake_validator.erl`
- ❌ `router_acl.erl` **not used** anywhere

### Updated: `apps/otp/router/test/router_acl_SUITE.erl`

**Changes**:
- Added deprecation notice at module level
- Marked as **DEPRECATED**
- Reference to `router_tenant_validator_SUITE.erl` for new tests

**Status**: Test suite kept for backward compatibility only.

## Task 1.3: Tests and CP2_GAP_REPORT Updates

### Updated: `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md`

**Changes**:
- **Section 3. ACL (Tenant/Roles)**:
  - Status changed from ⚠️ **PARTIALLY IMPLEMENTED** to ✅ **IMPLEMENTED**
  - Added formal ACL model reference
  - Clarified `router_tenant_validator.erl` as primary implementation
  - Marked `router_acl.erl` as deprecated

- **Summary Table**:
  - ACL status: ✅ **IMPLEMENTED**
  - Code refs updated to reflect primary implementation
  - Delta: Complete (formal ACL model, single source of truth)

- **Overall Assessment**:
  - Removed ACL from "Partially Implemented" list
  - Updated priority actions (ACL marked as complete)

- **Recommendations**:
  - Updated "Immediate Actions" to reflect ACL completion
  - Removed "Verify ACL vs Tenant Validator" task

**Status**: ACL checklist item now marked as **fully closed**.

## Implementation Details

### Source of Truth Hierarchy

1. **Policy Registry** (`router_policy_store.erl`):
   - Primary source for tenant validation
   - Policy existence = tenant is valid
   - Policy content = routing rules (not ACL allow/deny)

2. **Allowlist** (static configuration):
   - Secondary source for tenant validation
   - Optional: if not configured, check passes
   - Used for static tenant filtering

3. **`router_tenant_validator`**:
   - **Single entry point** for tenant validation
   - Combines allowlist and policy registry checks
   - Returns `{ok, TenantId}` or `{error, Reason, Context}`

### Decision Flow

```
Request with tenant_id
    ↓
router_tenant_validator:validate_tenant/2
    ↓
Format Validation (tenant_id present, binary, not empty)
    ↓
Allowlist Check (if configured)
    ↓
Policy Registry Check (router_policy_store:load_policy/2)
    ↓
Decision: {ok, TenantId} | {error, Reason, Context}
```

### Audit Events

**Module**: `router_tenant_validator.erl`

**Function**: `emit_audit_event/3`

**Event Types**:
- `tenant_missing`: `tenant_id` is `undefined`
- `tenant_empty`: `tenant_id` is empty
- `tenant_not_allowed`: Tenant not in allowlist
- `tenant_policy_not_found`: Policy not found
- `tenant_invalid_format`: `tenant_id` format invalid

**Telemetry**:
- `[router_tenant_validator, audit]`: Audit event
- `router_tenant_audit_total`: Counter for audit events

## Files Created/Modified

### Created Files

1. **`apps/otp/router/docs/ACL_MODEL.md`**:
   - Formal ACL model specification
   - Source of truth definition
   - Decision flow documentation
   - Implementation guidelines
   - Migration path from `router_acl`

2. **`docs/archive/dev/ACL_MODEL_FORMALIZATION_REPORT.md`** (this file):
   - Summary of formalization work
   - Task completion status
   - Implementation details

### Modified Files

1. **`apps/otp/router/src/router_acl.erl`**:
   - Added deprecation notice
   - Marked as DEPRECATED / TECH DEBT
   - Added migration path documentation

2. **`apps/otp/router/test/router_acl_SUITE.erl`**:
   - Added deprecation notice
   - Marked as DEPRECATED
   - Reference to new test suite

3. **`docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md`**:
   - Updated ACL section (PARTIALLY → IMPLEMENTED)
   - Updated summary table
   - Updated overall assessment
   - Updated recommendations

## Test Coverage

### Existing Tests

**Tenant Validation Tests**:
- ✅ `apps/otp/router/test/router_tenant_validator_SUITE.erl`: Tenant validation tests
- ✅ `apps/otp/router/test/router_result_consumer_SUITE.erl`: Result consumer with tenant validation
- ✅ `apps/otp/router/test/router_ack_consumer_SUITE.erl`: ACK consumer with tenant validation
- ✅ `apps/otp/router/test/router_intake_validator_SUITE.erl`: Intake validator with tenant validation

**Deprecated Tests**:
- ⚠️ `apps/otp/router/test/router_acl_SUITE.erl`: **DEPRECATED** (tests deprecated module, kept for backward compatibility)

### Test Requirements

**Required Coverage**:
- ✅ Tenant format validation (missing, empty, invalid format)
- ✅ Allowlist validation (configured, not configured, tenant in/out)
- ✅ Policy registry validation (policy exists, not found)
- ✅ Audit event emission (all event types)
- ✅ Integration with consumers (result, ack, intake)

## Acceptance Criteria

### Task 1.1: Formal ACL Model ✅

- ✅ Document created: `apps/otp/router/docs/ACL_MODEL.md`
- ✅ Source of truth defined: Policy Registry + Allowlist
- ✅ Decision flow documented: `router_tenant_validator:validate_tenant/2`
- ✅ Deprecated module documented: `router_acl.erl`

### Task 1.2: Code Alignment ✅

- ✅ `router_acl.erl` marked as deprecated
- ✅ `router_tenant_validator.erl` confirmed as single source of truth
- ✅ No production code uses `router_acl.erl`
- ✅ Migration path documented

### Task 1.3: Tests and CP2_GAP_REPORT ✅

- ✅ `router_acl_SUITE.erl` marked as deprecated
- ✅ CP2_GAP_REPORT updated: ACL = IMPLEMENTED
- ✅ Summary table updated
- ✅ Recommendations updated

## Status

✅ **Complete**: All tasks implemented:
- ✅ 1.1: Formal ACL model documented
- ✅ 1.2: Code aligned with model (`router_acl.erl` deprecated)
- ✅ 1.3: Tests and CP2_GAP_REPORT updated

## Next Steps

**Future Cleanup** (not required for CP2):
- Remove `router_acl.erl` entirely (after ensuring no external dependencies)
- Remove `router_acl_SUITE.erl` (after ensuring no test dependencies)
- Update any external documentation referencing `router_acl.erl`

## References

- `apps/otp/router/docs/ACL_MODEL.md`: Formal ACL model specification
- `apps/otp/router/src/router_tenant_validator.erl`: Primary ACL implementation
- `apps/otp/router/src/router_policy_store.erl`: Policy registry
- `apps/otp/router/src/router_acl.erl`: **Deprecated** (not used)
- `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md`: CP2 checklist gap analysis

