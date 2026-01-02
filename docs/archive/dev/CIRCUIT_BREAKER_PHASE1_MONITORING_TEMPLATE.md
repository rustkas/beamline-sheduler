# Circuit Breaker Phase 1 Monitoring Template

**Purpose**: Daily monitoring checklist and log for Phase 1 (Canary) rollout

## Daily Monitoring Checklist

### Metrics to Check

1. **Circuit State Transitions**:
   ```promql
   sum by (tenant_id, provider_id, state) (router_circuit_breaker_state_transitions_total)
   ```
   - Expected: Transitions only during actual provider failures
   - Action if unexpected: Create ticket `[CB-OPS] Unexpected circuit opens`

2. **Error Rates**:
   ```promql
   router_circuit_breaker_error_rate{tenant_id="<canary_tenant>", provider_id="<provider>"}
   ```
   - Expected: Error rate matches actual provider error rate
   - Action if mismatch: Create ticket `[CB-OPS] Error rate calculation inaccurate`

3. **Fail-Fast Events**:
   ```promql
   sum by (tenant_id, provider_id) (rate(router_circuit_breaker_events_total{event_type="fail_fast"}[5m]))
   ```
   - Expected: Fail-fast only when circuit is open
   - Action if excessive: Create ticket `[CB-OPS] Excessive fail-fast events`

4. **Window Statistics**:
   ```promql
   router_circuit_breaker_window_requests_total{tenant_id="<canary_tenant>"}
   router_circuit_breaker_window_failures_total{tenant_id="<canary_tenant>"}
   ```
   - Expected: Window statistics are accurate
   - Action if inaccurate: Create ticket `[CB-OPS] Window statistics inaccurate`

### Alerts to Monitor

1. **RouterCircuitBreakerOpened** (Warning)
   - Expected: Only during actual provider failures
   - Action if unexpected: Investigate and create ticket

2. **RouterCircuitBreakerHighFailureRate** (Critical)
   - Expected: Should not fire during normal operation
   - Action if fires: Investigate provider health and create ticket

3. **RouterCircuitBreakerMultipleProvidersOpen** (Critical)
   - Expected: Should not fire (only 1 provider in canary)
   - Action if fires: Investigate tenant-wide issues

4. **RouterCircuitBreakerHalfOpenProbesFailed** (Warning)
   - Expected: Only if provider fails to recover
   - Action if fires: Investigate provider recovery

## Daily Log Template

### Date: YYYY-MM-DD

**Time**: `[HH:MM]`

**Metrics Snapshot**:
- Circuit State: `[closed | open | half_open]`
- Error Rate: `[0.0-1.0]`
- Fail-Fast Events (last 5m): `[count]`
- Window Requests: `[count]`
- Window Failures: `[count]`

**Alerts**:
- `RouterCircuitBreakerOpened`: `[fired | not fired]`
- `RouterCircuitBreakerHighFailureRate`: `[fired | not fired]`
- Other alerts: `[list]`

**Issues Observed**:
- `[Describe any issues]`

**Actions Taken**:
- `[Describe actions taken]`

**Tickets Created**:
- `[List ticket IDs and descriptions]`

**Notes**:
- `[Any additional observations]`

---

## Weekly Summary Template

### Week of YYYY-MM-DD to YYYY-MM-DD

**Overall Status**: `[✅ Stable | ⚠️ Issues | ❌ Problems]`

**Circuit Opens**:
- Total: `[count]`
- Expected: `[count]`
- Unexpected: `[count]`

**Error Rate**:
- Average: `[0.0-1.0]`
- Peak: `[0.0-1.0]`
- Matches Provider Error Rate: `[Yes | No]`

**Performance**:
- Latency Impact: `[< 5% | > 5%]`
- Throughput Impact: `[< 5% | > 5%]`

**Tickets Created**: `[count]`
- Threshold Issues: `[count]`
- Alert Issues: `[count]`
- Other: `[count]`

**Recommendations**:
- `[List recommendations for threshold tuning or configuration changes]`

**Decision**:
- [ ] Proceed to Phase 2
- [ ] Extend Phase 1
- [ ] Rollback

