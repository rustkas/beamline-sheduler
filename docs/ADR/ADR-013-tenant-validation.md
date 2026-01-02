---
version: 1.0
status: accepted
date: 2025-01-27
deciders:
  - CP1-ROUTER Implementation Team
related_adrs:
  - ADR-011: JetStream E2E with Durable Subscriptions
  - ADR-012: Idempotency Layer
supersedes: []
superseded_by: []
---

# ADR-013: Tenant Validation for Multi-Tenancy

## Status

accepted

## Context

Router processes messages from CAF with tenant identifiers:
- Execution results (`caf.exec.result.v1`) include `tenant_id`
- Assignment acknowledgments (`caf.exec.assign.v1.ack`) include `tenant_id`
- Usage events require validated `tenant_id` for billing

**Problem**: Need to validate tenant identifiers to:
- Prevent unauthorized tenant access
- Ensure billing accuracy
- Maintain audit integrity
- Enforce tenant isolation

**Requirements**:
- Validate `tenant_id` against allowlist (if configured)
- Validate `tenant_id` against policy registry
- Emit audit events for validation failures
- Support controlled redelivery on validation failures (NAK)

## Decision

Implement **tenant validation layer** with allowlist and policy registry checks, with audit logging and controlled redelivery.

**Key Components**:

1. **Validation Checks**:
   - **Allowlist Check**: Validate against configured tenant allowlist
     - Config: `result_ack_allowed_tenants` or `caf_push_assignment_allowed_tenants`
     - If allowlist configured: only listed tenants allowed
     - If allowlist not configured: all tenants allowed
   - **Policy Registry Check**: Validate that policy exists for tenant
     - Check: `router_policy_store:load_policy(TenantId, <<"default">>)`
     - If policy found: tenant is valid
     - If policy not found: allow but emit audit event (configurable behavior)

2. **Validation Context**:
   - Includes: `assignment_id`, `request_id`, `provider_id`, `job_type`, `trace_id`, `source`
   - Used for audit logging and error context

3. **Error Handling**:
   - **Tenant Missing**: `{error, tenant_missing, Context}`
   - **Tenant Empty**: `{error, tenant_empty, Context}`
   - **Tenant Not Allowed**: `{error, tenant_not_allowed, Context}` (not in allowlist)
   - **Tenant Invalid Format**: `{error, tenant_invalid_format, Context}` (not binary)

4. **Audit Events**:
   - Emit telemetry: `router_tenant_validator.audit`
   - Emit counter: `router_tenant_audit_total`
   - Log warning: `router_logger:warn`
   - Include full context for debugging

5. **Controlled Redelivery**:
   - On validation failure: NAK message (respects MaxDeliver)
   - Check MaxDeliver exhaustion before NAK
   - Emit redelivery metric: `router_jetstream_redelivery_total`
   - Allows retry after tenant configuration update

**Implementation**:
- `router_tenant_validator.erl`: Validation logic
- Used by: `router_result_consumer.erl`, `router_ack_consumer.erl`

## Consequences

### Positive

- **Security**: Prevents unauthorized tenant access
- **Billing Accuracy**: Ensures only valid tenants are billed
- **Audit Trail**: Complete audit log of validation events
- **Controlled Redelivery**: Allows retry after configuration updates
- **Flexible Configuration**: Allowlist optional, policy check optional
- **Fail-Open for Policy**: Allows tenants without policies (with audit)

### Negative

- **Configuration Overhead**: Requires allowlist and policy configuration
- **Performance**: Policy lookup adds latency
- **Complexity**: Multiple validation checks and error paths
- **Redelivery Overhead**: Failed validations trigger redelivery

### Neutral

- **Monitoring**: Need to monitor validation failures and redeliveries
- **Configuration Management**: Need to manage allowlists and policies

## Alternatives Considered

### Alternative 1: No Tenant Validation

**Description**: Accept all tenant IDs without validation

**Pros**:
- Simplest implementation
- No configuration required
- No performance overhead

**Cons**:
- Security risk (unauthorized access)
- Billing inaccuracy
- No audit trail

**Why not chosen**: Security and billing requirements demand validation

### Alternative 2: Database-backed Tenant Registry

**Description**: Use database for tenant registry and validation

**Pros**:
- Centralized tenant management
- Rich query capabilities
- Persistent across restarts

**Cons**:
- Additional infrastructure dependency
- Higher latency
- Network overhead
- More complex failure handling

**Why not chosen**: Policy registry provides sufficient validation for CP1

### Alternative 3: Strict Policy Requirement

**Description**: Require policy for all tenants (fail closed)

**Pros**:
- Stronger validation
- Ensures all tenants have policies

**Cons**:
- Blocks legitimate tenants without policies
- Requires policy creation before tenant use
- Less flexible

**Why not chosen**: Fail-open with audit provides better flexibility

## Implementation Notes

**Validation Flow**:
```erlang
case router_tenant_validator:validate_tenant(TenantId, ValidationContext) of
    {ok, ValidatedTenantId} ->
        process_message(ValidatedTenantId);
    {error, Reason, ErrorContext} ->
        log_validation_error(Reason, ErrorContext),
        emit_audit_event(Reason, TenantId, ErrorContext),
        nak_message(MsgId)  % Controlled redelivery
end
```

**Allowlist Configuration**:
- `result_ack_allowed_tenants`: For result/ACK processing
- `caf_push_assignment_allowed_tenants`: For assignment publishing
- Format: List of tenant IDs or map with tenant IDs as keys

**Policy Registry Check**:
- Uses `router_policy_store:load_policy(TenantId, <<"default">>)`
- If policy found: tenant valid
- If policy not found: allow with audit (fail-open)

**Audit Events**:
- Event types: `tenant_missing`, `tenant_empty`, `tenant_not_allowed`, `tenant_policy_not_found`, `tenant_invalid_format`
- Includes full context for debugging

## References

- `apps/otp/router/src/router_tenant_validator.erl`: Implementation
- `apps/otp/router/src/router_result_consumer.erl`: Usage in result processing
- `apps/otp/router/src/router_ack_consumer.erl`: Usage in ACK processing
- `apps/otp/router/src/router_policy_store.erl`: Policy registry
- ADR-011: JetStream E2E (NAK for redelivery)
- ADR-012: Idempotency Layer (validation before idempotency check)

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy
- ✅ Respects security constraints
- ✅ Integrates with STATE/HISTORY

