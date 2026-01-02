# Circuit Breaker Rollout Plan

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: ✅ **Ready for Production Rollout**

## Purpose

This document defines a phased rollout plan for Circuit Breaker (CB) feature in Router Policy DSL, ensuring safe and gradual deployment with monitoring and rollback capabilities.

## Overview

Circuit Breaker is a **CP2 feature** that prevents cascading failures by opening circuits when providers experience sustained failures or high error rates. This rollout plan ensures:

- **Gradual enablement** per tenant/policy
- **Monitoring and observability** at each phase
- **Clear stability criteria** before expanding
- **Safe rollback** mechanisms

## Rollout Phases

### Phase 0: Pre-Rollout (Current State)

**Status**: ✅ **Complete**

- Circuit Breaker implementation complete (Phase 1-3)
- Unit and integration tests passing
- Load tests validated
- Documentation complete
- Metrics and alerting configured

**Action Items**:
- ✅ All CB code merged to `main`
- ✅ Default CB state: **DISABLED** (opt-in only)
- ✅ Monitoring dashboards ready (see `CIRCUIT_BREAKER_DASHBOARD_SPEC.md`)

### Phase 1: Canary (Single Tenant, Single Policy)

**Duration**: 3-7 days  
**Target**: 1 tenant, 1 policy, 1 provider

**Enablement**:
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

**Monitoring**:
- Monitor `router_circuit_breaker_state_transitions_total` for state changes
- Monitor `router_circuit_breaker_events_total` for success/failure/fail_fast events
- Monitor `router_circuit_breaker_error_rate` for error rate trends
- Check alert rules: `RouterCircuitBreakerOpened`, `RouterCircuitBreakerHighFailureRate`

**Success Criteria**:
- ✅ No unexpected circuit opens (only during actual provider failures)
- ✅ Circuit closes successfully after provider recovery
- ✅ No increase in overall error rate for tenant
- ✅ No performance degradation (latency < 5% increase)
- ✅ Metrics and logs are accurate and useful

**Rollback**:
- Set `"enabled": false` in policy JSON
- Reload policy via admin API or config update
- Verify circuit breaker state cleared (no open circuits)

### Phase 2: Limited Expansion (5-10 Tenants, Multiple Policies)

**Duration**: 7-14 days  
**Target**: 5-10 tenants, 2-5 policies per tenant

**Enablement**:
- Enable CB for selected tenants (low-traffic, non-critical first)
- Use same configuration as Phase 1
- Monitor per-tenant and per-policy

**Monitoring**:
- Aggregate metrics across all enabled tenants
- Per-tenant error rate trends
- Circuit open frequency per tenant
- Alert on `RouterCircuitBreakerMultipleProvidersOpen` (multiple providers open in same tenant)

**Success Criteria**:
- ✅ All Phase 1 criteria met across all tenants
- ✅ No tenant-wide issues (all providers open simultaneously)
- ✅ Circuit breaker behavior consistent across tenants
- ✅ No false positives (circuits opening unnecessarily)

**Rollback**:
- Disable CB per tenant (update policy JSON)
- Or disable globally via feature flag (if implemented)

### Phase 3: Broader Rollout (50% of Tenants)

**Duration**: 14-21 days  
**Target**: ~50% of production tenants

**Enablement**:
- Enable for medium-traffic tenants
- Consider tenant-specific thresholds (adjust `failure_threshold`, `error_rate_threshold` based on traffic patterns)
- Monitor for tenant-specific patterns

**Monitoring**:
- Tenant-level dashboards
- Provider-level error rate trends
- Circuit open/close frequency
- Impact on overall system reliability

**Success Criteria**:
- ✅ All Phase 2 criteria met
- ✅ Circuit breaker reduces wasted resources (fewer calls to unhealthy providers)
- ✅ No increase in customer-reported issues
- ✅ Positive impact on system stability metrics

**Rollback**:
- Per-tenant disable (same as Phase 2)
- Or feature flag disable for specific tenant groups

### Phase 4: Full Rollout (All Tenants)

**Duration**: 7-14 days  
**Target**: 100% of production tenants

**Enablement**:
- Enable for remaining tenants
- High-traffic tenants last (after validation in Phase 3)
- Consider tenant-specific tuning based on Phase 3 learnings

**Monitoring**:
- System-wide metrics
- Provider health trends
- Circuit breaker effectiveness (reduction in failed requests to unhealthy providers)

**Success Criteria**:
- ✅ All Phase 3 criteria met
- ✅ Circuit breaker is default behavior (can be disabled per policy if needed)
- ✅ Documentation and runbooks updated
- ✅ Team trained on CB operations

## Feature Flag / Configuration

### Policy-Level Enablement (Current)

**Default**: CB is **disabled** (`"enabled": false` or omitted)

**Enablement**: Set `"enabled": true` in policy JSON

**Example**:
```json
{
  "policy_id": "my_policy",
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

### Tenant-Level Enablement (Future Enhancement)

**Proposed**: Add tenant-level feature flag to enable/disable CB for all policies in a tenant

**Implementation** (Future):
- Add `circuit_breaker_enabled` flag to tenant configuration
- Override policy-level `enabled` if tenant-level flag is set
- Admin API endpoint to toggle tenant-level flag

### Global Feature Flag (Future Enhancement)

**Proposed**: Add application-level feature flag to disable CB globally (emergency rollback)

**Implementation** (Future):
- Environment variable: `ROUTER_CIRCUIT_BREAKER_ENABLED=false`
- Application config: `{circuit_breaker_enabled, false}`
- Admin API endpoint: `POST /admin/circuit-breaker/disable`

## Monitoring and Alerting

### Key Metrics to Monitor

1. **Circuit State Transitions**:
   - `router_circuit_breaker_state_transitions_total{state="open"}` - Circuit opens
   - `router_circuit_breaker_state_transitions_total{state="closed"}` - Circuit closes
   - `router_circuit_breaker_state_transitions_total{state="half_open"}` - Half-open transitions

2. **Error Rates**:
   - `router_circuit_breaker_error_rate` - Current error rate per provider
   - `router_circuit_breaker_window_requests_total` - Requests in window
   - `router_circuit_breaker_window_failures_total` - Failures in window

3. **Events**:
   - `router_circuit_breaker_events_total{event_type="fail_fast"}` - Fail-fast events
   - `router_circuit_breaker_events_total{event_type="failure"}` - Failure events
   - `router_circuit_breaker_events_total{event_type="success"}` - Success events

### Alert Rules

See `apps/otp/router/docs/PROMETHEUS_ALERTS.md` for detailed alert rules:

- `RouterCircuitBreakerOpened` - Circuit opened (warning)
- `RouterCircuitBreakerHighFailureRate` - High fail-fast rate (critical)
- `RouterCircuitBreakerMultipleProvidersOpen` - Multiple providers open (critical)
- `RouterCircuitBreakerHalfOpenProbesFailed` - Half-open probes failed (warning)

### Dashboard

See `CIRCUIT_BREAKER_DASHBOARD_SPEC.md` for dashboard specification.

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

2. **Reload Policy**:
   - Via admin API: `POST /admin/policies/{tenant_id}/{policy_id}/reload`
   - Or restart Router with updated config

3. **Verify**:
   - Check circuit breaker state cleared (no open circuits for provider)
   - Monitor metrics to confirm no new CB events

### Per-Tenant Rollback

1. **Disable CB for All Policies in Tenant**:
   - Update all policy JSONs for tenant
   - Or use tenant-level feature flag (if implemented)

2. **Verify**:
   - No open circuits for tenant
   - Metrics show no CB activity for tenant

### Global Rollback (Emergency)

1. **Set Global Feature Flag** (if implemented):
   - `ROUTER_CIRCUIT_BREAKER_ENABLED=false`
   - Restart Router

2. **Or Disable via Admin API** (if implemented):
   - `POST /admin/circuit-breaker/disable`

3. **Verify**:
   - All circuits cleared
   - No new CB events

## Stability Criteria

### Phase 1 → Phase 2

- ✅ No unexpected circuit opens for 3+ days
- ✅ Circuit closes successfully after provider recovery
- ✅ No increase in error rate
- ✅ Metrics and logs accurate

### Phase 2 → Phase 3

- ✅ All Phase 1 criteria met across 5-10 tenants
- ✅ No tenant-wide issues
- ✅ Consistent behavior across tenants
- ✅ No false positives

### Phase 3 → Phase 4

- ✅ All Phase 2 criteria met
- ✅ Circuit breaker reduces wasted resources
- ✅ No increase in customer-reported issues
- ✅ Positive impact on system stability

## Configuration Tuning

### Recommended Thresholds

**Low-Traffic Tenants** (< 100 req/min):
- `failure_threshold`: 3-5
- `error_rate_threshold`: 0.5
- `error_rate_window_seconds`: 60
- `timeout_ms`: 30000

**Medium-Traffic Tenants** (100-1000 req/min):
- `failure_threshold`: 5-10
- `error_rate_threshold`: 0.5-0.6
- `error_rate_window_seconds`: 60-120
- `timeout_ms`: 30000-60000

**High-Traffic Tenants** (> 1000 req/min):
- `failure_threshold`: 10-20
- `error_rate_threshold`: 0.6-0.7
- `error_rate_window_seconds`: 120-300
- `timeout_ms`: 60000-120000

### Tuning Guidelines

1. **Start Conservative**: Lower thresholds, shorter windows
2. **Monitor Closely**: Watch for false positives
3. **Adjust Gradually**: Increase thresholds if too sensitive
4. **Document Changes**: Track threshold changes and their impact

## References

- `docs/ROUTING_POLICY.md` - Circuit Breaker DSL specification
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Design document
- `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` - Metrics and logging
- `docs/archive/dev/CIRCUIT_BREAKER_DASHBOARD_SPEC.md` - Dashboard specification
- `docs/archive/dev/CIRCUIT_BREAKER_ROLLOUT_EXECUTION.md` - Step-by-step execution guide
- `docs/archive/dev/CIRCUIT_BREAKER_PRODUCTION_NOTES.md` - Operational guide
- `docs/archive/dev/CIRCUIT_BREAKER_PRODUCTION_TICKETS_EXAMPLES.md` - Example tickets
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Alert rules

