# Circuit Breaker Monitoring Dashboard Specification

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: ✅ **Dashboard Specification Ready** (Ready for Implementation During Rollout)

## Purpose

This document specifies a minimal monitoring dashboard for Circuit Breaker (CB) functionality in Router Policy DSL. The dashboard provides visibility into CB state, error rates, transitions, and overall health.

## Dashboard Overview

**Target Platform**: Grafana (or compatible)  
**Data Source**: Prometheus  
**Update Frequency**: 30 seconds (configurable)

## Dashboard Panels

### Panel 1: Circuit Breaker State Overview

**Title**: "Circuit Breaker States by Provider"

**Type**: Stat Panel

**Query**:
```promql
sum by (tenant_id, provider_id, state) (router_circuit_breaker_state_transitions_total)
```

**Visualization**:
- Show current state (Closed / Open / Half-Open) per provider
- Color coding:
  - 🟢 Green: `closed`
  - 🔴 Red: `open`
  - 🟡 Yellow: `half_open`
- Group by `tenant_id`, then `provider_id`

**Refresh**: 30s

---

### Panel 2: Error Rate Trends

**Title**: "Error Rate Over Time"

**Type**: Time Series Graph

**Queries**:
```promql
# Error rate per provider
router_circuit_breaker_error_rate{tenant_id="<tenant_id>", provider_id="<provider_id>"}

# Error rate threshold line (0.5)
0.5
```

**Visualization**:
- Line graph: Error rate over time
- Horizontal line: Error rate threshold (0.5 or configured value)
- Y-axis: 0.0 to 1.0
- Multiple series: One per provider (if multiple)
- Legend: Show `tenant_id`, `provider_id`

**Alert Threshold**: Error rate > threshold (shown as red zone)

**Refresh**: 30s

---

### Panel 3: Circuit Open Events

**Title**: "Circuit Opens (Last 24 Hours)"

**Type**: Time Series Graph

**Query**:
```promql
sum by (tenant_id, provider_id) (
  rate(router_circuit_breaker_state_transitions_total{state="open"}[5m])
)
```

**Visualization**:
- Line graph: Circuit open events per 5 minutes
- Multiple series: One per provider
- Legend: Show `tenant_id`, `provider_id`
- Y-axis: Events per 5 minutes

**Refresh**: 30s

---

### Panel 4: Fail-Fast Events

**Title**: "Fail-Fast Events (Circuit Open)"

**Type**: Time Series Graph

**Query**:
```promql
sum by (tenant_id, provider_id) (
  rate(router_circuit_breaker_events_total{event_type="fail_fast"}[5m])
)
```

**Visualization**:
- Line graph: Fail-fast events per 5 minutes
- Multiple series: One per provider
- Legend: Show `tenant_id`, `provider_id`
- Y-axis: Events per 5 minutes

**Alert Threshold**: High fail-fast rate (see `RouterCircuitBreakerHighFailureRate` alert)

**Refresh**: 30s

---

### Panel 5: State Transitions Summary

**Title**: "State Transitions (Last 1 Hour)"

**Type**: Table

**Query**:
```promql
sum by (tenant_id, provider_id, state) (
  increase(router_circuit_breaker_state_transitions_total[1h])
)
```

**Visualization**:
- Table columns:
  - `tenant_id`
  - `provider_id`
  - `state` (closed / open / half_open)
  - `count` (number of transitions)
- Sort by: `count` (descending)
- Color coding: Same as Panel 1

**Refresh**: 1m

---

### Panel 6: Window Statistics

**Title**: "Sliding Window Statistics"

**Type**: Stat Panel

**Queries**:
```promql
# Total requests in window
sum by (tenant_id, provider_id) (router_circuit_breaker_window_requests_total)

# Total failures in window
sum by (tenant_id, provider_id) (router_circuit_breaker_window_failures_total)

# Error rate
router_circuit_breaker_error_rate
```

**Visualization**:
- Three stat panels side-by-side:
  1. **Requests in Window**: Total requests in current sliding window
  2. **Failures in Window**: Total failures in current sliding window
  3. **Error Rate**: Current error rate (0.0 to 1.0)
- Group by: `tenant_id`, `provider_id`
- Color thresholds:
  - Error Rate: Green (< 0.3), Yellow (0.3-0.5), Red (> 0.5)

**Refresh**: 30s

---

### Panel 7: Timeout Remaining

**Title**: "Time Until Half-Open (Open Circuits)"

**Type**: Gauge

**Query**:
```promql
router_circuit_breaker_timeout_remaining_ms{state="open"}
```

**Visualization**:
- Gauge: Time remaining (milliseconds) until circuit transitions from Open to Half-Open
- Only show for circuits in `open` state
- Color thresholds:
  - Green: > 50% remaining
  - Yellow: 25-50% remaining
  - Red: < 25% remaining
- Unit: Milliseconds (convert to seconds for display)

**Refresh**: 30s

---

### Panel 8: Provider Health Matrix

**Title**: "Provider Health Matrix"

**Type**: Heatmap

**Query**:
```promql
# Error rate per provider (bucketed)
histogram_quantile(0.95, 
  sum by (tenant_id, provider_id, le) (
    rate(router_circuit_breaker_error_rate_bucket[5m])
  )
)
```

**Alternative** (if histogram not available):
```promql
# Use error rate directly
router_circuit_breaker_error_rate
```

**Visualization**:
- Heatmap: Error rate per provider
- X-axis: `provider_id`
- Y-axis: `tenant_id`
- Color scale: Green (0.0) → Yellow (0.5) → Red (1.0)
- Tooltip: Show exact error rate value

**Refresh**: 1m

---

### Panel 9: Alert Summary

**Title**: "Active Alerts"

**Type**: Alert List

**Alerts**:
- `RouterCircuitBreakerOpened` (Warning)
- `RouterCircuitBreakerHighFailureRate` (Critical)
- `RouterCircuitBreakerMultipleProvidersOpen` (Critical)
- `RouterCircuitBreakerHalfOpenProbesFailed` (Warning)

**Visualization**:
- List of active alerts
- Group by severity
- Show: Alert name, tenant_id, provider_id, duration, description

**Refresh**: 30s

---

## Dashboard Layout

### Row 1: Overview
- Panel 1: Circuit Breaker State Overview (Full width)

### Row 2: Error Rates
- Panel 2: Error Rate Trends (Full width)

### Row 3: Events
- Panel 3: Circuit Open Events (50% width)
- Panel 4: Fail-Fast Events (50% width)

### Row 4: Statistics
- Panel 5: State Transitions Summary (Full width)

### Row 5: Window & Timeout
- Panel 6: Window Statistics (66% width)
- Panel 7: Timeout Remaining (34% width)

### Row 6: Health Matrix
- Panel 8: Provider Health Matrix (Full width)

### Row 7: Alerts
- Panel 9: Active Alerts (Full width)

## Variables (Filters)

### Tenant Filter
- **Name**: `tenant_id`
- **Type**: Query
- **Query**: `label_values(router_circuit_breaker_error_rate, tenant_id)`
- **Multi-value**: Yes
- **Include "All"**: Yes

### Provider Filter
- **Name**: `provider_id`
- **Type**: Query
- **Query**: `label_values(router_circuit_breaker_error_rate{tenant_id=~"$tenant_id"}, provider_id)`
- **Multi-value**: Yes
- **Include "All"**: Yes

### Time Range
- **Default**: Last 1 hour
- **Options**: 15m, 30m, 1h, 6h, 24h, 7d

## Dashboard JSON (Grafana)

**Note**: Full Grafana dashboard JSON will be generated during implementation phase. This specification serves as the design document.

## Metrics Reference

All metrics are documented in `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md`:

- `router_circuit_breaker_events_total`
- `router_circuit_breaker_state_transitions_total`
- `router_circuit_breaker_error_rate`
- `router_circuit_breaker_window_requests_total`
- `router_circuit_breaker_window_failures_total`
- `router_circuit_breaker_timeout_transitions_total`
- `router_circuit_breaker_timeout_remaining_ms`

## Alert Rules Reference

All alert rules are documented in `apps/otp/router/docs/PROMETHEUS_ALERTS.md`:

- `RouterCircuitBreakerOpened`
- `RouterCircuitBreakerHighFailureRate`
- `RouterCircuitBreakerMultipleProvidersOpen`
- `RouterCircuitBreakerHalfOpenProbesFailed`

## Implementation Notes

### Phase 1: Basic Dashboard (MVP)

**Priority Panels**:
1. Circuit Breaker State Overview (Panel 1)
2. Error Rate Trends (Panel 2)
3. Circuit Open Events (Panel 3)
4. Alert Summary (Panel 9)

### Phase 2: Enhanced Dashboard

**Additional Panels**:
5. Fail-Fast Events (Panel 4)
6. State Transitions Summary (Panel 5)
7. Window Statistics (Panel 6)
8. Timeout Remaining (Panel 7)
9. Provider Health Matrix (Panel 8)

### Future Enhancements

- **Historical Trends**: Long-term error rate trends (7d, 30d)
- **Provider Comparison**: Side-by-side comparison of multiple providers
- **Tenant-Level Aggregation**: Aggregate metrics across all providers in a tenant
- **Custom Thresholds**: Display tenant-specific thresholds on graphs
- **Circuit Breaker Effectiveness**: Metrics showing reduction in failed requests

## References

- `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` - Metrics and logging specification
- `docs/archive/dev/CIRCUIT_BREAKER_ROLLOUT_PLAN.md` - Rollout plan
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Alert rules
- `docs/ROUTING_POLICY.md` - Circuit Breaker DSL specification

