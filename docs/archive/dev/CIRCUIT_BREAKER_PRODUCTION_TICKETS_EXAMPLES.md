# Circuit Breaker Production Tickets - Examples

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: 📋 **Example Tickets** (For Reference)

## Purpose

This document provides real-world examples of operational tickets that may be created during Circuit Breaker rollout and operation, based on production signals.

## Ticket Format

**Title**: `[CB-OPS] <Brief Description>`

**Labels**: `circuit-breaker`, `operational-improvement`, `production`

**Priority**: `Low` | `Medium` | `High` | `Critical`

---

## Example 1: Threshold Too Sensitive (False Positives)

**Title**: `[CB-OPS] Circuit breaker opening too frequently for high-traffic tenant tenant_123`

**Labels**: `circuit-breaker`, `operational-improvement`, `production`, `threshold-tuning`

**Priority**: `Medium`

**Description**:
```markdown
## Issue
Circuit breaker for provider `openai` in tenant `tenant_123` opens too frequently, causing unnecessary fail-fast behavior and impacting user experience.

## Context
- **Tenant**: `tenant_123`
- **Provider**: `openai`
- **Policy**: `default_policy`
- **Timeframe**: Last 24 hours
- **Traffic**: ~500 req/min (high-traffic tenant)

## Current Behavior
- Circuit opens 5-10 times per hour
- Most opens are false positives (provider is actually healthy)
- Current threshold: `failure_threshold: 5`, `error_rate_threshold: 0.5`, `error_rate_window_seconds: 60`
- Provider health checks show provider is healthy (99.5% success rate)
- Error rate calculated by CB: 0.3-0.4 (below threshold, but circuit still opens due to `failure_threshold`)

## Expected Behavior
- Circuit should only open when provider is actually unhealthy
- False positive rate should be < 5%
- Circuit should not open for brief, recoverable issues

## Metrics/Logs
- Error rate: 0.3-0.4 (below threshold, but circuit still opens)
- Circuit opens triggered by `failure_threshold` (5 consecutive failures)
- Provider health checks: 99.5% success rate
- Circuit open duration: 1-2 minutes (brief, recoverable)
- Fail-fast events: 50-100 per hour (unnecessary)

## Proposed Solution
- Increase `failure_threshold` from 5 to 10 (less sensitive to brief failures)
- Or adjust `error_rate_threshold` from 0.5 to 0.6 (allow higher error rate)
- Or increase `error_rate_window_seconds` from 60 to 120 (longer window for error rate calculation)

## Impact
- **User Impact**: Medium (unnecessary fail-fast causes some requests to fail)
- **System Impact**: Low (circuit opens/closes quickly, no cascading failures)
- **Business Impact**: Medium (some user requests fail unnecessarily)

## Resolution
- [ ] Investigate root cause (why 5 consecutive failures occur)
- [ ] Adjust thresholds based on investigation
- [ ] Monitor for 24-48 hours after adjustment
- [ ] Document threshold tuning decision
```

---

## Example 2: Alert Noise

**Title**: `[CB-OPS] RouterCircuitBreakerOpened alert firing too frequently, causing alert fatigue`

**Labels**: `circuit-breaker`, `operational-improvement`, `production`, `alerting`

**Priority**: `Medium`

**Description**:
```markdown
## Issue
`RouterCircuitBreakerOpened` alert fires 20+ times per day, causing alert fatigue and reducing alert effectiveness.

## Context
- **Tenant(s)**: Multiple tenants (`tenant_123`, `tenant_456`, `tenant_789`)
- **Provider(s)**: `openai`, `anthropic`
- **Timeframe**: Last 7 days
- **Alert Frequency**: 20+ alerts per day

## Current Behavior
- Alert fires every time circuit opens (even for brief, recoverable issues)
- Most circuits close within 1-2 minutes
- Alert doesn't distinguish between brief vs. sustained issues
- Alert expression: `rate(router_circuit_breaker_state_transitions_total{state="open"}[10m]) > 0`
- Average circuit open duration: 1-2 minutes
- Most opens are brief and recover automatically

## Expected Behavior
- Alert should only fire for sustained issues (circuit open > 5 minutes)
- Brief opens should not trigger alerts (or use lower severity)
- Alert should be actionable (indicate when intervention is needed)

## Metrics/Logs
- Alert expression: `rate(router_circuit_breaker_state_transitions_total{state="open"}[10m]) > 0`
- Average circuit open duration: 1-2 minutes
- Alert frequency: 20+ per day
- Circuit close events: 20+ per day (most circuits close quickly)
- False positive rate: ~80% (most alerts are for brief issues)

## Proposed Solution
- **Option 1**: Add duration filter to alert (only fire if circuit open > 5 minutes)
  - New expression: `rate(router_circuit_breaker_state_transitions_total{state="open"}[10m]) > 0 AND router_circuit_breaker_timeout_remaining_ms < 25000`
- **Option 2**: Create separate alert for brief opens (info level)
  - Brief opens: `info` severity
  - Sustained opens: `warning` severity
- **Option 3**: Increase alert threshold (only fire if multiple circuits open)

## Impact
- **User Impact**: None (alert noise doesn't affect users)
- **System Impact**: Low (alert fatigue reduces operational efficiency)
- **Business Impact**: Low (but reduces alert effectiveness)

## Resolution
- [ ] Analyze alert patterns (duration, frequency, tenants)
- [ ] Implement alert filtering or separate alerts
- [ ] Test alert changes in staging
- [ ] Monitor alert frequency after changes
- [ ] Document alert tuning decision
```

---

## Example 3: Missing Metrics

**Title**: `[CB-OPS] Need metric for circuit breaker effectiveness (requests saved vs. failed)`

**Labels**: `circuit-breaker`, `operational-improvement`, `production`, `observability`

**Priority**: `Low`

**Description**:
```markdown
## Issue
Cannot measure how effective circuit breaker is at reducing wasted requests to unhealthy providers.

## Context
- **All tenants/providers**
- **Timeframe**: Ongoing
- **Use Case**: Optimization and reporting

## Current Behavior
- Have metrics for circuit state, error rates, events
- Cannot see: "How many requests were saved by failing fast?"
- Cannot see: "What would error rate be without CB?"
- Cannot measure: "ROI of circuit breaker" (reduction in failed requests)

## Expected Behavior
- Metric showing requests blocked by open circuit (fail-fast count)
- Metric showing estimated requests saved (would have failed if sent to provider)
- Dashboard panel showing CB effectiveness
- Ability to compare: "requests with CB" vs. "requests without CB" (estimated)

## Metrics/Logs
- Current: `router_circuit_breaker_events_total{event_type="fail_fast"}` (shows fail-fast events)
- Missing: Comparison metric (requests that would have failed vs. actually failed)
- Missing: Effectiveness metric (percentage of requests saved)

## Proposed Solution
- **Option 1**: Add metric: `router_circuit_breaker_requests_saved_total` (estimated)
  - Calculate: `fail_fast_count * estimated_failure_rate`
- **Option 2**: Enhance dashboard to calculate effectiveness from existing metrics
  - Formula: `(fail_fast_count / total_requests) * error_rate_before_cb`
- **Option 3**: Add metric: `router_circuit_breaker_effectiveness_ratio` (gauge)
  - Calculate: `requests_saved / (requests_saved + requests_failed)`

## Impact
- **User Impact**: None (doesn't affect functionality)
- **System Impact**: None (doesn't affect performance)
- **Business Impact**: Low (nice to have for optimization and reporting)

## Resolution
- [ ] Design effectiveness metric calculation
- [ ] Implement metric (if Option 1 or 3)
- [ ] Or enhance dashboard (if Option 2)
- [ ] Test metric accuracy
- [ ] Document metric usage
```

---

## Example 4: Configuration Tuning Needed

**Title**: `[CB-OPS] Circuit breaker thresholds need tenant-specific tuning for tenant_456`

**Labels**: `circuit-breaker`, `operational-improvement`, `production`, `configuration`

**Priority**: `Medium`

**Description**:
```markdown
## Issue
Circuit breaker thresholds are not optimal for tenant `tenant_456` traffic patterns, causing suboptimal behavior.

## Context
- **Tenant**: `tenant_456`
- **Provider**: `anthropic`
- **Policy**: `default_policy`
- **Traffic Pattern**: Bursty (high traffic during business hours, low traffic at night)
- **Timeframe**: Last 14 days

## Current Behavior
- Current threshold: `failure_threshold: 5`, `error_rate_threshold: 0.5`, `error_rate_window_seconds: 60`
- During high traffic: Circuit opens too frequently (false positives)
- During low traffic: Circuit doesn't open when it should (false negatives)
- Window size (60s) is too short for bursty traffic pattern

## Expected Behavior
- Thresholds should be optimized for tenant's traffic pattern
- Circuit should open appropriately during both high and low traffic periods
- Window size should account for bursty traffic

## Metrics/Logs
- High traffic period: 1000+ req/min, error rate: 0.3-0.4, circuit opens frequently
- Low traffic period: 10-50 req/min, error rate: 0.6-0.7, circuit doesn't open
- Window size: 60s (too short for bursty pattern)
- False positive rate: ~30% during high traffic
- False negative rate: ~20% during low traffic

## Proposed Solution
- **Option 1**: Increase window size for bursty traffic
  - `error_rate_window_seconds: 120` or `180` (longer window for bursty pattern)
- **Option 2**: Adjust thresholds for traffic pattern
  - High traffic: `failure_threshold: 10`, `error_rate_threshold: 0.6`
  - Low traffic: `failure_threshold: 3`, `error_rate_threshold: 0.4`
- **Option 3**: Implement adaptive thresholds (future enhancement)
  - Adjust thresholds based on traffic volume

## Impact
- **User Impact**: Medium (suboptimal CB behavior affects user experience)
- **System Impact**: Medium (inefficient resource usage)
- **Business Impact**: Medium (some requests fail unnecessarily or don't fail when they should)

## Resolution
- [ ] Analyze traffic patterns for tenant
- [ ] Test threshold adjustments in staging
- [ ] Apply optimized thresholds
- [ ] Monitor for 48-72 hours
- [ ] Document threshold tuning decision
```

---

## Example 5: Performance Issue

**Title**: `[CB-OPS] Circuit breaker checks adding significant latency to routing decisions`

**Labels**: `circuit-breaker`, `operational-improvement`, `production`, `performance`

**Priority**: `High`

**Description**:
```markdown
## Issue
Circuit breaker checks are adding 10-20ms latency to routing decisions, impacting overall system performance.

## Context
- **All tenants/providers**
- **Timeframe**: Last 3 days
- **Traffic**: High (1000+ req/min per tenant)
- **Performance Impact**: 10-20ms added latency per request

## Current Behavior
- CB check latency: 10-20ms per request
- Total routing latency: 50-70ms (including CB check)
- CB check is synchronous (blocks routing decision)
- ETS lookup for CB state is fast, but error rate calculation is slow

## Expected Behavior
- CB check latency should be < 5ms
- Total routing latency should be < 50ms
- CB check should not significantly impact routing performance

## Metrics/Logs
- CB check latency: 10-20ms (measured via `router_policy_application_latency_ms`)
- Error rate calculation: 5-10ms (sliding window calculation)
- ETS lookup: < 1ms (fast)
- Total routing latency: 50-70ms (target: < 50ms)

## Proposed Solution
- **Option 1**: Optimize error rate calculation
  - Cache error rate calculation (update every N requests, not every request)
  - Use approximate error rate (faster calculation)
- **Option 2**: Make CB check asynchronous (future enhancement)
  - Non-blocking CB check
  - Fallback to allow if CB check is slow
- **Option 3**: Optimize sliding window data structure
  - Use more efficient data structure for window events
  - Limit window size (max events in window)

## Impact
- **User Impact**: High (increased latency affects user experience)
- **System Impact**: High (performance degradation)
- **Business Impact**: High (slower response times)

## Resolution
- [ ] Profile CB check performance (identify bottleneck)
- [ ] Implement optimization (Option 1 or 3)
- [ ] Test performance improvement
- [ ] Monitor latency after optimization
- [ ] Document performance optimization
```

---

## Ticket Workflow

### 1. Create Ticket
- Use ticket template from `CIRCUIT_BREAKER_PRODUCTION_NOTES.md`
- Fill in all relevant sections
- Assign priority based on impact

### 2. Triage
- Review ticket details
- Verify production signal is real (not false positive)
- Assign to appropriate team member

### 3. Investigation
- Analyze metrics and logs
- Reproduce issue if possible
- Identify root cause

### 4. Resolution
- Implement fix or workaround
- Test in staging if possible
- Deploy to production
- Monitor for 24-48 hours

### 5. Documentation
- Document resolution
- Update runbooks if needed
- Close ticket

## References

- `docs/archive/dev/CIRCUIT_BREAKER_PRODUCTION_NOTES.md` - Operational guide and ticket template
- `docs/archive/dev/CIRCUIT_BREAKER_ROLLOUT_PLAN.md` - Rollout plan
- `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` - Metrics and logging

