# Circuit Breaker Production Notes

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: 📋 **Production Operations Guide**

## Purpose

This document provides operational notes and guidelines for using Circuit Breaker (CB) in production, including how to handle production signals and when to create operational improvement tickets.

## Using Load Tests and Rollout Plan

### Load Tests

**Test Suite**: `apps/otp/router/test/router_circuit_breaker_load_SUITE.erl`

**Usage During Rollout**:
- Run load tests before each rollout phase to validate CB behavior
- Use test results to verify thresholds and window configurations
- Document actual results in `CIRCUIT_BREAKER_LOAD_TEST_REPORT.md`

**Execution**:
```bash
cd apps/otp/router
rebar3 ct --suite test/router_circuit_breaker_load_SUITE
```

### Rollout Plan

**Document**: `docs/archive/dev/CIRCUIT_BREAKER_ROLLOUT_PLAN.md`

**Usage**:
- Follow phased rollout (Canary → Limited → Broader → Full)
- Monitor metrics and alerts at each phase
- Use stability criteria to determine when to proceed to next phase
- Use rollback procedures if issues arise

## Production Signals and Operational Improvements

### When to Create Operational Tickets

Create operational improvement tickets when you observe:

1. **Threshold Inaccuracies**:
   - Circuit opens too frequently (false positives)
   - Circuit doesn't open when it should (false negatives)
   - Error rate calculations seem incorrect

2. **Alert Noise**:
   - Too many alerts for the same issue
   - Alerts firing unnecessarily
   - Missing alerts for actual problems

3. **Performance Issues**:
   - CB checks adding significant latency
   - Memory usage growing unexpectedly
   - ETS table size concerns

4. **Configuration Issues**:
   - Thresholds need tenant-specific tuning
   - Window sizes not optimal for traffic patterns
   - Timeout values causing issues

5. **Observability Gaps**:
   - Missing metrics for troubleshooting
   - Dashboard panels not showing needed information
   - Logs missing critical context

### Ticket Template

**Title**: `[CB-OPS] <Brief Description>`

**Labels**: `circuit-breaker`, `operational-improvement`, `production`

**Description Template**:
```markdown
## Issue
[Describe the production signal observed]

## Context
- **Tenant(s)**: [List affected tenants]
- **Provider(s)**: [List affected providers]
- **Policy(ies)**: [List affected policies]
- **Timeframe**: [When observed]

## Current Behavior
[What is happening now]

## Expected Behavior
[What should happen]

## Metrics/Logs
[Relevant metrics, log excerpts, or dashboard screenshots]

## Proposed Solution
[If you have ideas, otherwise leave for investigation]

## Priority
- [ ] Low (nice to have)
- [ ] Medium (should fix soon)
- [ ] High (affecting operations)
- [ ] Critical (blocking rollout)
```

### Examples

#### Example 1: Threshold Too Sensitive

**Title**: `[CB-OPS] Circuit breaker opening too frequently for high-traffic tenant`

**Description**:
```markdown
## Issue
Circuit breaker for provider `openai` in tenant `tenant_123` opens too frequently, causing unnecessary fail-fast behavior.

## Context
- **Tenant**: `tenant_123`
- **Provider**: `openai`
- **Policy**: `default_policy`
- **Timeframe**: Last 24 hours

## Current Behavior
- Circuit opens 5-10 times per hour
- Most opens are false positives (provider is actually healthy)
- Current threshold: `failure_threshold: 5`, `error_rate_threshold: 0.5`

## Expected Behavior
- Circuit should only open when provider is actually unhealthy
- False positive rate should be < 5%

## Metrics/Logs
- Error rate: 0.3-0.4 (below threshold, but circuit still opens)
- Circuit opens triggered by `failure_threshold` (5 consecutive failures)
- Provider health checks show provider is healthy

## Proposed Solution
- Increase `failure_threshold` from 5 to 10
- Or adjust `error_rate_threshold` from 0.5 to 0.6
- Or increase `error_rate_window_seconds` from 60 to 120

## Priority
- [x] Medium (affecting operations but not blocking)
```

#### Example 2: Alert Noise

**Title**: `[CB-OPS] RouterCircuitBreakerOpened alert firing too frequently`

**Description**:
```markdown
## Issue
`RouterCircuitBreakerOpened` alert fires 20+ times per day, causing alert fatigue.

## Context
- **Tenant(s)**: Multiple tenants
- **Provider(s)**: `openai`, `anthropic`
- **Timeframe**: Last 7 days

## Current Behavior
- Alert fires every time circuit opens (even for brief, recoverable issues)
- Most circuits close within 1-2 minutes
- Alert doesn't distinguish between brief vs. sustained issues

## Expected Behavior
- Alert should only fire for sustained issues (circuit open > 5 minutes)
- Brief opens should not trigger alerts

## Metrics/Logs
- Alert expression: `rate(router_circuit_breaker_state_transitions_total{state="open"}[10m]) > 0`
- Average circuit open duration: 1-2 minutes
- Alert frequency: 20+ per day

## Proposed Solution
- Add duration filter to alert (only fire if circuit open > 5 minutes)
- Or create separate alert for brief opens (info level)

## Priority
- [x] Medium (alert fatigue, but not critical)
```

#### Example 3: Missing Metrics

**Title**: `[CB-OPS] Need metric for circuit breaker effectiveness`

**Description**:
```markdown
## Issue
Cannot measure how effective circuit breaker is at reducing wasted requests to unhealthy providers.

## Context
- **All tenants/providers**
- **Timeframe**: Ongoing

## Current Behavior
- Have metrics for circuit state, error rates, events
- Cannot see: "How many requests were saved by failing fast?"
- Cannot see: "What would error rate be without CB?"

## Expected Behavior
- Metric showing requests blocked by open circuit (fail-fast count)
- Metric showing estimated requests saved (would have failed if sent to provider)
- Dashboard panel showing CB effectiveness

## Metrics/Logs
- Current: `router_circuit_breaker_events_total{event_type="fail_fast"}` (shows fail-fast events)
- Missing: Comparison metric (requests that would have failed vs. actually failed)

## Proposed Solution
- Add metric: `router_circuit_breaker_requests_saved_total` (estimated)
- Or enhance dashboard to calculate effectiveness from existing metrics

## Priority
- [ ] Low (nice to have for optimization)
```

## Monitoring During Rollout

### Key Metrics to Watch

1. **Circuit State Transitions**:
   - Monitor `router_circuit_breaker_state_transitions_total`
   - Watch for unexpected opens (false positives)
   - Watch for circuits not opening when they should (false negatives)

2. **Error Rates**:
   - Monitor `router_circuit_breaker_error_rate`
   - Compare with actual provider error rates
   - Verify error rate calculation is accurate

3. **Fail-Fast Events**:
   - Monitor `router_circuit_breaker_events_total{event_type="fail_fast"}`
   - Verify fail-fast is working correctly
   - Check for excessive fail-fast (may indicate threshold issues)

4. **Alert Frequency**:
   - Monitor alert firing frequency
   - Watch for alert noise
   - Verify alerts are actionable

### Dashboard Usage

**Dashboard**: See `CIRCUIT_BREAKER_DASHBOARD_SPEC.md` for full specification

**During Rollout**:
- Use dashboard to monitor CB state across all tenants/providers
- Watch for patterns (multiple providers open, specific tenants affected)
- Use filters to focus on specific tenants/providers during each phase

## Rollback Procedures

See `CIRCUIT_BREAKER_ROLLOUT_PLAN.md` for detailed rollback procedures.

**Quick Rollback**:
1. Set `"enabled": false` in policy JSON
2. Reload policy via admin API or config update
3. Verify circuit breaker state cleared
4. Monitor metrics to confirm no new CB events

## References

- `docs/archive/dev/CIRCUIT_BREAKER_ROLLOUT_PLAN.md` - Rollout plan
- `docs/archive/dev/CIRCUIT_BREAKER_ROLLOUT_EXECUTION.md` - Step-by-step execution guide
- `docs/archive/dev/CIRCUIT_BREAKER_LOAD_TEST_REPORT.md` - Load test results template
- `docs/archive/dev/CIRCUIT_BREAKER_DASHBOARD_SPEC.md` - Dashboard specification
- `docs/archive/dev/CIRCUIT_BREAKER_PRODUCTION_TICKETS_EXAMPLES.md` - Example tickets
- `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` - Metrics and logging
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Alert rules

