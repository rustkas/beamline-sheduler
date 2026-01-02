# Circuit Breaker Rollout Execution Guide

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: 📋 **Execution Guide** (For Production Rollout)

## Purpose

This document provides step-by-step instructions for executing the Circuit Breaker rollout in production, including load test execution, phased rollout, and handling production signals.

## Pre-Rollout Checklist

### 1. Load Test Execution

**Before starting rollout, execute load tests to validate CB behavior:**

```bash
cd apps/otp/router
rebar3 ct --suite test/router_circuit_breaker_load_SUITE --verbose
```

**Expected Results**:
- All 4 test scenarios pass
- No unexpected errors or crashes
- Circuit state transitions work correctly
- Error rate calculations are accurate

**Document Results**:
- Record actual results in `CIRCUIT_BREAKER_LOAD_TEST_REPORT.md`
- Note any deviations from expected behavior
- Verify performance targets are met

**If Tests Fail**:
- Do not proceed with rollout
- Investigate and fix issues
- Re-run tests until all pass

### 2. Environment Preparation

**Verify**:
- ✅ Monitoring dashboards are set up (see `CIRCUIT_BREAKER_DASHBOARD_SPEC.md`)
- ✅ Alert rules are configured (see `PROMETHEUS_ALERTS.md`)
- ✅ Rollback procedures are understood (see `CIRCUIT_BREAKER_ROLLOUT_PLAN.md`)
- ✅ Team is trained on CB operations

### 3. Policy Configuration

**Verify**:
- ✅ Default CB state is **DISABLED** (`"enabled": false` or omitted)
- ✅ Policies can be updated via admin API or config
- ✅ Policy reload mechanism works

## Phase 1: Canary Rollout

### Step 1: Select Canary Tenant

**Criteria**:
- Low-traffic tenant (< 100 req/min)
- Non-critical workload
- Responsive tenant owner (for coordination)

**Action**:
1. Identify candidate tenant
2. Notify tenant owner
3. Document tenant selection

### Step 2: Enable CB for Canary Policy

**Policy JSON Update**:
```json
{
  "policy_id": "canary_policy",
  "circuit_breaker": {
    "enabled": true,
    "failure_threshold": 5,
    "error_rate_threshold": 0.5,
    "error_rate_window_seconds": 60,
    "timeout_ms": 30000,
    "success_threshold": 2
  }
}
```

**Action**:
1. Update policy JSON
2. Reload policy via admin API or config update
3. Verify policy loaded correctly

### Step 3: Monitor (3-7 days)

**Daily Checks**:
- Monitor `router_circuit_breaker_state_transitions_total` for state changes
- Monitor `router_circuit_breaker_events_total` for success/failure/fail_fast events
- Monitor `router_circuit_breaker_error_rate` for error rate trends
- Check alert rules: `RouterCircuitBreakerOpened`, `RouterCircuitBreakerHighFailureRate`

**Success Criteria** (must meet all):
- ✅ No unexpected circuit opens (only during actual provider failures)
- ✅ Circuit closes successfully after provider recovery
- ✅ No increase in overall error rate for tenant
- ✅ No performance degradation (latency < 5% increase)
- ✅ Metrics and logs are accurate and useful

**If Issues Arise**:
- Use rollback procedure (set `"enabled": false`)
- Create operational ticket (see `CIRCUIT_BREAKER_PRODUCTION_NOTES.md`)
- Document issue and resolution

### Step 4: Proceed to Phase 2

**Decision Point**:
- If all success criteria met for 3+ days → Proceed to Phase 2
- If issues persist → Extend Phase 1 or rollback

## Phase 2: Limited Expansion

### Step 1: Select 5-10 Tenants

**Criteria**:
- Mix of low and medium-traffic tenants
- Non-critical workloads
- Responsive tenant owners

**Action**:
1. Identify candidate tenants
2. Notify tenant owners
3. Document tenant selection

### Step 2: Enable CB for Selected Tenants

**Action**:
1. Update policy JSON for each tenant
2. Reload policies
3. Verify policies loaded correctly

### Step 3: Monitor (7-14 days)

**Daily Checks**:
- Aggregate metrics across all enabled tenants
- Per-tenant error rate trends
- Circuit open frequency per tenant
- Alert on `RouterCircuitBreakerMultipleProvidersOpen`

**Success Criteria** (must meet all):
- ✅ All Phase 1 criteria met across all tenants
- ✅ No tenant-wide issues (all providers open simultaneously)
- ✅ Circuit breaker behavior consistent across tenants
- ✅ No false positives (circuits opening unnecessarily)

**If Issues Arise**:
- Use per-tenant rollback (set `"enabled": false` for affected tenant)
- Create operational ticket
- Document issue and resolution

### Step 4: Proceed to Phase 3

**Decision Point**:
- If all success criteria met for 7+ days → Proceed to Phase 3
- If issues persist → Extend Phase 2 or rollback

## Phase 3: Broader Rollout

### Step 1: Select ~50% of Tenants

**Criteria**:
- Include medium-traffic tenants
- Consider tenant-specific thresholds based on Phase 2 learnings
- Mix of critical and non-critical workloads

**Action**:
1. Identify candidate tenants
2. Adjust thresholds if needed (based on Phase 2 learnings)
3. Notify tenant owners
4. Document tenant selection and threshold adjustments

### Step 2: Enable CB with Tenant-Specific Tuning

**Example Policy (Medium-Traffic Tenant)**:
```json
{
  "policy_id": "medium_traffic_policy",
  "circuit_breaker": {
    "enabled": true,
    "failure_threshold": 10,
    "error_rate_threshold": 0.6,
    "error_rate_window_seconds": 120,
    "timeout_ms": 60000,
    "success_threshold": 2
  }
}
```

**Action**:
1. Update policy JSON for each tenant (with tuning if needed)
2. Reload policies
3. Verify policies loaded correctly

### Step 3: Monitor (14-21 days)

**Daily Checks**:
- Tenant-level dashboards
- Provider-level error rate trends
- Circuit open/close frequency
- Impact on overall system reliability

**Success Criteria** (must meet all):
- ✅ All Phase 2 criteria met
- ✅ Circuit breaker reduces wasted resources (fewer calls to unhealthy providers)
- ✅ No increase in customer-reported issues
- ✅ Positive impact on system stability metrics

**If Issues Arise**:
- Use per-tenant rollback
- Create operational ticket
- Document issue and resolution
- Consider threshold adjustments

### Step 4: Proceed to Phase 4

**Decision Point**:
- If all success criteria met for 14+ days → Proceed to Phase 4
- If issues persist → Extend Phase 3 or rollback

## Phase 4: Full Rollout

### Step 1: Enable for Remaining Tenants

**Action**:
1. Enable for remaining tenants (high-traffic tenants last)
2. Use tenant-specific tuning based on Phase 3 learnings
3. Notify tenant owners
4. Document tenant selection and tuning

### Step 2: Monitor (7-14 days)

**Daily Checks**:
- System-wide metrics
- Provider health trends
- Circuit breaker effectiveness
- Customer-reported issues

**Success Criteria** (must meet all):
- ✅ All Phase 3 criteria met
- ✅ Circuit breaker is default behavior (can be disabled per policy if needed)
- ✅ Documentation and runbooks updated
- ✅ Team trained on CB operations

### Step 3: Mark Rollout Complete

**Action**:
1. Update `CIRCUIT_BREAKER_ROLLOUT_PLAN.md` with completion status
2. Document lessons learned
3. Update operational runbooks
4. Archive rollout execution notes

## Handling Production Signals

### When to Create Operational Tickets

See `CIRCUIT_BREAKER_PRODUCTION_NOTES.md` for detailed guidelines.

**Common Signals**:
1. **Threshold Inaccuracies**: Circuit opens too frequently or not frequently enough
2. **Alert Noise**: Too many alerts or missing alerts
3. **Performance Issues**: CB checks adding latency or memory issues
4. **Configuration Issues**: Thresholds need tuning
5. **Observability Gaps**: Missing metrics or dashboard issues

### Ticket Creation Process

1. **Identify Signal**: Observe production behavior
2. **Document**: Use ticket template from `CIRCUIT_BREAKER_PRODUCTION_NOTES.md`
3. **Create Ticket**: Use format `[CB-OPS] <Brief Description>`
4. **Prioritize**: Assign priority (Low/Medium/High/Critical)
5. **Track**: Monitor ticket resolution

### Example Tickets

See `CIRCUIT_BREAKER_PRODUCTION_NOTES.md` for example tickets:
- Threshold too sensitive
- Alert noise
- Missing metrics

## Rollback Procedures

### Per-Policy Rollback

1. **Update Policy JSON**:
   ```json
   {
     "circuit_breaker": {
       "enabled": false
     }
   }
   ```

2. **Reload Policy**: Via admin API or config update

3. **Verify**: Check circuit breaker state cleared

### Per-Tenant Rollback

1. **Disable CB for All Policies in Tenant**: Update all policy JSONs

2. **Verify**: No open circuits for tenant

### Global Rollback (Emergency)

1. **Set Global Feature Flag** (if implemented): `ROUTER_CIRCUIT_BREAKER_ENABLED=false`

2. **Restart Router**: If needed

3. **Verify**: All circuits cleared

## Monitoring During Rollout

### Key Metrics Dashboard

**Use**: `CIRCUIT_BREAKER_DASHBOARD_SPEC.md` for dashboard setup

**Daily Review**:
- Circuit Breaker State Overview
- Error Rate Trends
- Circuit Open Events
- Fail-Fast Events
- Alert Summary

### Alert Response

**Alert**: `RouterCircuitBreakerOpened`
- **Action**: Check provider health, verify circuit open is expected
- **If Unexpected**: Create operational ticket

**Alert**: `RouterCircuitBreakerHighFailureRate`
- **Action**: Investigate provider issues, check threshold configuration
- **If Threshold Issue**: Create operational ticket

**Alert**: `RouterCircuitBreakerMultipleProvidersOpen`
- **Action**: Investigate tenant-wide issues
- **If Configuration Issue**: Create operational ticket

## Post-Rollout

### Documentation Updates

1. **Update Runbooks**: Add CB troubleshooting procedures
2. **Update Training**: Include CB operations in team training
3. **Archive Notes**: Save rollout execution notes for future reference

### Continuous Improvement

1. **Monitor**: Continue monitoring CB effectiveness
2. **Optimize**: Adjust thresholds based on production learnings
3. **Enhance**: Add new metrics or dashboard panels as needed

## References

- `docs/archive/dev/CIRCUIT_BREAKER_ROLLOUT_PLAN.md` - Rollout plan
- `docs/archive/dev/CIRCUIT_BREAKER_LOAD_TEST_REPORT.md` - Load test results template
- `docs/archive/dev/CIRCUIT_BREAKER_DASHBOARD_SPEC.md` - Dashboard specification
- `docs/archive/dev/CIRCUIT_BREAKER_PRODUCTION_NOTES.md` - Operational guide and ticket templates
- `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` - Metrics and logging

