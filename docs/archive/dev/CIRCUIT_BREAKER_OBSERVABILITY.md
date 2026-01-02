# Circuit Breaker Observability

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: ✅ **CP2 IMPLEMENTED** (Phase 3 Complete)

## Purpose

This document defines observability requirements for Circuit Breaker functionality in Router Policy DSL, including metrics, logging, and alerting rules.

## Metrics

### Exported Metrics (Telemetry)

All metrics are exported via `router_metrics:emit_metric/3` and available through Telemetry.

#### 1. Circuit Breaker Events Counter

**Metric**: `router_circuit_breaker_events_total`

**Type**: Counter

**Labels**:
- `tenant_id` (string): Tenant identifier
- `provider_id` (string): Provider identifier
- `event_type` (string): Event type (`success` | `failure`)

**Description**: Total count of circuit breaker events (success/failure) per provider per tenant.

**Example**:
```erlang
router_metrics:emit_metric(router_circuit_breaker_events_total, #{count => 1}, #{
    tenant_id => <<"tenant_123">>,
    provider_id => <<"openai">>,
    event_type => <<"success">>
}).
```

**Prometheus Query**:
```promql
sum(rate(router_circuit_breaker_events_total[5m])) by (tenant_id, provider_id, event_type)
```

#### 2. Circuit Breaker State Transitions Counter

**Metric**: `router_circuit_breaker_state_transitions_total`

**Type**: Counter

**Labels**:
- `tenant_id` (string): Tenant identifier
- `provider_id` (string): Provider identifier
- `state` (string): Target state (`closed` | `open` | `half_open`)

**Description**: Total count of circuit breaker state transitions per provider per tenant.

**Example**:
```erlang
router_metrics:emit_metric(router_circuit_breaker_state_transitions_total, #{count => 1}, #{
    tenant_id => <<"tenant_123">>,
    provider_id => <<"openai">>,
    state => <<"open">>
}).
```

**Prometheus Query**:
```promql
sum(rate(router_circuit_breaker_state_transitions_total[5m])) by (tenant_id, provider_id, state)
```

#### 3. Circuit Breaker Error Rate (Gauge)

**Metric**: `router_circuit_breaker_error_rate`

**Type**: Gauge

**Labels**:
- `tenant_id` (string): Tenant identifier
- `provider_id` (string): Provider identifier

**Description**: Current error rate (failures / total requests) over the sliding window for a provider.

**Example**:
```erlang
router_metrics:emit_metric(router_circuit_breaker_error_rate, #{value => 0.65}, #{
    tenant_id => <<"tenant_123">>,
    provider_id => <<"openai">>
}).
```

**Prometheus Query**:
```promql
router_circuit_breaker_error_rate{tenant_id="tenant_123", provider_id="openai"}
```

#### 4. Circuit Breaker Window Requests Counter

**Metric**: `router_circuit_breaker_window_requests_total`

**Type**: Counter

**Labels**:
- `tenant_id` (string): Tenant identifier
- `provider_id` (string): Provider identifier

**Description**: Total requests in the current sliding window for a provider.

**Example**:
```erlang
router_metrics:emit_metric(router_circuit_breaker_window_requests_total, #{count => 100}, #{
    tenant_id => <<"tenant_123">>,
    provider_id => <<"openai">>
}).
```

**Prometheus Query**:
```promql
router_circuit_breaker_window_requests_total{tenant_id="tenant_123", provider_id="openai"}
```

#### 5. Circuit Breaker Window Failures Counter

**Metric**: `router_circuit_breaker_window_failures_total`

**Type**: Counter

**Labels**:
- `tenant_id` (string): Tenant identifier
- `provider_id` (string): Provider identifier

**Description**: Total failures in the current sliding window for a provider.

**Example**:
```erlang
router_metrics:emit_metric(router_circuit_breaker_window_failures_total, #{count => 65}, #{
    tenant_id => <<"tenant_123">>,
    provider_id => <<"openai">>
}).
```

**Prometheus Query**:
```promql
router_circuit_breaker_window_failures_total{tenant_id="tenant_123", provider_id="openai"}
```

#### 6. Circuit Breaker Timeout Transitions Counter

**Metric**: `router_circuit_breaker_timeout_transitions_total`

**Type**: Counter

**Labels**:
- `tenant_id` (string): Tenant identifier
- `provider_id` (string): Provider identifier
- `transition_type` (string): Type of timeout transition (`open_to_half_open`)

**Description**: Total count of timeout-based state transitions per provider per tenant.

**Example**:
```erlang
router_metrics:emit_metric(router_circuit_breaker_timeout_transitions_total, #{count => 1}, #{
    tenant_id => <<"tenant_123">>,
    provider_id => <<"openai">>,
    transition_type => <<"open_to_half_open">>
}).
```

**Prometheus Query**:
```promql
sum(rate(router_circuit_breaker_timeout_transitions_total[5m])) by (tenant_id, provider_id, transition_type)
```

#### 7. Circuit Breaker Timeout Remaining (Gauge)

**Metric**: `router_circuit_breaker_timeout_remaining_ms`

**Type**: Gauge

**Labels**:
- `tenant_id` (string): Tenant identifier
- `provider_id` (string): Provider identifier

**Description**: Time remaining (in milliseconds) until the next timeout-based transition (Open → Half-Open). Only emitted when circuit is in `open` state.

**Example**:
```erlang
router_metrics:emit_metric(router_circuit_breaker_timeout_remaining_ms, #{value => 45000}, #{
    tenant_id => <<"tenant_123">>,
    provider_id => <<"openai">>
}).
```

**Prometheus Query**:
```promql
router_circuit_breaker_timeout_remaining_ms{tenant_id="tenant_123", provider_id="openai"}
```

### Internal State Metrics (ETS)

**Note**: These metrics are tracked internally in ETS and not directly exported. They can be accessed via admin API or exposed as gauges in future enhancements.

- `failure_count`: Consecutive failure count (reset on success)
- `success_count`: Consecutive success count (in half-open state)
- `error_rate`: Error rate over time window (failures / total requests)
- `state`: Current state (`closed` | `open` | `half_open`)
- `half_open_calls_count`: Current calls in half-open state
- `total_requests`: Total requests to provider (over window)
- `total_failures`: Total failures (over window)
- `total_successes`: Total successes (over window)

## Logging

### Log Events

All circuit breaker events should be logged with structured JSON format following `docs/OBSERVABILITY_CONVENTIONS.md`.

#### 1. Circuit Breaker Opened

**Event**: `circuit_breaker_opened`

**Level**: `WARN`

**Required Fields**:
- `timestamp`: ISO 8601 timestamp
- `level`: `WARN`
- `component`: `router`
- `message`: `"Circuit breaker opened for provider"`
- `tenant_id`: Tenant identifier
- `provider_id`: Provider identifier

**Optional Fields**:
- `policy_id`: Policy identifier (if available)
- `reason`: Reason for opening (`failure_threshold` | `error_rate_threshold`)
- `failure_count`: Current failure count
- `error_rate`: Current error rate
- `trace_id`: Trace identifier (if available)

**Example**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "WARN",
  "component": "router",
  "message": "Circuit breaker opened for provider",
  "tenant_id": "tenant_123",
  "provider_id": "openai",
  "policy_id": "default",
  "reason": "failure_threshold",
  "failure_count": 5,
  "error_rate": 0.6,
  "trace_id": "trace_abc123def4567890abcdef1234567890"
}
```

#### 2. Circuit Breaker Half-Opened

**Event**: `circuit_breaker_half_opened`

**Level**: `INFO`

**Required Fields**:
- `timestamp`: ISO 8601 timestamp
- `level`: `INFO`
- `component`: `router`
- `message`: `"Circuit breaker half-opened for provider"`
- `tenant_id`: Tenant identifier
- `provider_id`: Provider identifier

**Optional Fields**:
- `policy_id`: Policy identifier (if available)
- `timeout_elapsed_ms`: Time elapsed since opening
- `trace_id`: Trace identifier (if available)

**Example**:
```json
{
  "timestamp": "2025-01-27T12:01:00.123456Z",
  "level": "INFO",
  "component": "router",
  "message": "Circuit breaker half-opened for provider",
  "tenant_id": "tenant_123",
  "provider_id": "openai",
  "policy_id": "default",
  "timeout_elapsed_ms": 60000,
  "trace_id": "trace_abc123def4567890abcdef1234567890"
}
```

#### 3. Circuit Breaker Closed

**Event**: `circuit_breaker_closed`

**Level**: `INFO`

**Required Fields**:
- `timestamp`: ISO 8601 timestamp
- `level`: `INFO`
- `component`: `router`
- `message`: `"Circuit breaker closed for provider"`
- `tenant_id`: Tenant identifier
- `provider_id`: Provider identifier

**Optional Fields**:
- `policy_id`: Policy identifier (if available)
- `success_count`: Consecutive successes that triggered closing
- `trace_id`: Trace identifier (if available)

**Example**:
```json
{
  "timestamp": "2025-01-27T12:02:00.123456Z",
  "level": "INFO",
  "component": "router",
  "message": "Circuit breaker closed for provider",
  "tenant_id": "tenant_123",
  "provider_id": "openai",
  "policy_id": "default",
  "success_count": 2,
  "trace_id": "trace_abc123def4567890abcdef1234567890"
}
```

#### 4. Circuit Breaker Fail-Fast

**Event**: `circuit_breaker_fail_fast`

**Level**: `WARN`

**Required Fields**:
- `timestamp`: ISO 8601 timestamp
- `level`: `WARN`
- `component`: `router`
- `message`: `"Circuit breaker fail-fast: request rejected"`
- `tenant_id`: Tenant identifier
- `provider_id`: Provider identifier

**Optional Fields**:
- `policy_id`: Policy identifier (if available)
- `state`: Current circuit state (`open` | `half_open`)
- `fallback_provider`: Fallback provider used (if available)
- `trace_id`: Trace identifier (if available)

**Example**:
```json
{
  "timestamp": "2025-01-27T12:00:30.123456Z",
  "level": "WARN",
  "component": "router",
  "message": "Circuit breaker fail-fast: request rejected",
  "tenant_id": "tenant_123",
  "provider_id": "openai",
  "policy_id": "default",
  "state": "open",
  "fallback_provider": "anthropic",
  "trace_id": "trace_abc123def4567890abcdef1234567890"
}
```

### Explanation Format

Circuit breaker information should be included in decision explanations when circuit breaker is active.

**Explanation Step Format**:
```json
{
  "step": "circuit_breaker_check",
  "action": "checked",
  "result": "allowed" | "rejected",
  "state": "closed" | "open" | "half_open",
  "provider_id": "openai",
  "context": {
    "failure_count": 0,
    "error_rate": 0.0,
    "reason": "circuit_closed"
  }
}
```

**Example Explanation with Circuit Breaker**:
```json
{
  "reason": "circuit_breaker_fallback",
  "provider_id": "anthropic",
  "policy_id": "default",
  "steps": [
    {
      "step": "circuit_breaker_check",
      "action": "checked",
      "result": "rejected",
      "state": "open",
      "provider_id": "openai",
      "context": {
        "failure_count": 5,
        "error_rate": 0.6,
        "reason": "circuit_open"
      }
    },
    {
      "step": "fallback",
      "action": "selected",
      "provider_id": "anthropic",
      "context": {
        "fallback_reason": "circuit_breaker_open"
      }
    }
  ],
  "context": {
    "tenant_id": "tenant_123",
    "circuit_breaker_state": "open",
    "original_provider": "openai"
  }
}
```

## Alerting Rules

### Prometheus Alert Rules

Alert rules follow OBS2 profile conventions (see `docs/OBSERVABILITY_CP1_INVARIANTS.md`).

#### 1. Circuit Breaker Opened (Warning)

**Alert**: `RouterCircuitBreakerOpened`

**Severity**: `warning`

**Condition**: Circuit breaker opened for any provider

**PromQL**:
```promql
sum(rate(router_circuit_breaker_state_transitions_total{state="open"}[5m])) by (tenant_id, provider_id) > 0
```

**For**: `10m`

**Labels**:
- `severity`: `warning`
- `service`: `router`
- `component`: `circuit_breaker`

**Annotations**:
- `summary`: `"Circuit breaker opened for provider {{ $labels.provider_id }}"`
- `description`: `"Circuit breaker opened for provider {{ $labels.provider_id }} in tenant {{ $labels.tenant_id }}. Provider may be unhealthy. Check provider health and error rates."`

#### 2. Circuit Breaker High Failure Rate (Critical)

**Alert**: `RouterCircuitBreakerHighFailureRate`

**Severity**: `critical`

**Condition**: High failure rate for circuit breaker events

**PromQL**:
```promql
sum(rate(router_circuit_breaker_events_total{event_type="failure"}[5m])) by (tenant_id, provider_id) 
/ 
sum(rate(router_circuit_breaker_events_total[5m])) by (tenant_id, provider_id) 
> 0.5
```

**For**: `5m`

**Labels**:
- `severity`: `critical`
- `service`: `router`
- `component`: `circuit_breaker`

**Annotations**:
- `summary`: `"High failure rate for provider {{ $labels.provider_id }}"`
- `description`: `"Failure rate > 50% for provider {{ $labels.provider_id }} in tenant {{ $labels.tenant_id }}. Circuit breaker may open soon. Investigate provider health."`

#### 3. Circuit Breaker Multiple Providers Open (Critical)

**Alert**: `RouterCircuitBreakerMultipleProvidersOpen`

**Severity**: `critical`

**Condition**: Multiple providers have open circuit breakers

**PromQL**:
```promql
count(sum(rate(router_circuit_breaker_state_transitions_total{state="open"}[5m])) by (tenant_id, provider_id) > 0) > 2
```

**For**: `5m`

**Labels**:
- `severity`: `critical`
- `service`: `router`
- `component`: `circuit_breaker`

**Annotations**:
- `summary`: `"Multiple circuit breakers open"`
- `description`: `"{{ $value }} providers have open circuit breakers. System may be experiencing widespread provider issues. Check provider health and fallback policies."`

#### 4. Circuit Breaker Half-Open Probes Failed (Warning)

**Alert**: `RouterCircuitBreakerHalfOpenProbesFailed`

**Severity**: `warning`

**Condition**: All half-open probes failed (circuit re-opened immediately)

**PromQL**:
```promql
sum(rate(router_circuit_breaker_state_transitions_total{state="open"}[5m])) by (tenant_id, provider_id) 
> 
sum(rate(router_circuit_breaker_state_transitions_total{state="half_open"}[5m])) by (tenant_id, provider_id)
```

**For**: `10m`

**Labels**:
- `severity`: `warning`
- `service`: `router`
- `component`: `circuit_breaker`

**Annotations**:
- `summary`: `"Circuit breaker half-open probes failing for provider {{ $labels.provider_id }}"`
- `description`: `"Circuit breaker for provider {{ $labels.provider_id }} in tenant {{ $labels.tenant_id }} is repeatedly opening after half-open attempts. Provider may not have recovered. Check provider health."`

### Alert Runbook

#### RouterCircuitBreakerOpened

**Investigation Steps**:
1. Check provider health endpoint (if available)
2. Review recent error logs for provider
3. Check error rate metrics: `sum(rate(router_circuit_breaker_events_total{event_type="failure"}[5m])) by (provider_id)`
4. Verify fallback policies are configured correctly
5. Check if provider is experiencing known issues (status page, monitoring)

**Resolution**:
- If provider is healthy: Check circuit breaker configuration (thresholds may be too low)
- If provider is unhealthy: Wait for provider recovery or update fallback policies
- If persistent: Consider adjusting circuit breaker thresholds or provider selection

#### RouterCircuitBreakerHighFailureRate

**Investigation Steps**:
1. Check provider error types (timeout, 5xx, connection errors)
2. Review provider latency metrics
3. Check for rate limiting or quota issues
4. Verify provider API status

**Resolution**:
- If transient: Monitor for recovery
- If persistent: Update fallback policies or provider selection
- If provider issue: Contact provider support or switch to alternative provider

#### RouterCircuitBreakerMultipleProvidersOpen

**Investigation Steps**:
1. Check if all providers are experiencing issues
2. Review system-wide error patterns
3. Check for network or infrastructure issues
4. Verify fallback policies are working

**Resolution**:
- If all providers down: Check infrastructure (network, DNS, etc.)
- If specific providers: Update fallback policies to exclude unhealthy providers
- If persistent: Review provider selection strategy

## Integration with Existing Observability

### CP1 Correlation Fields

All circuit breaker log events must include CP1 correlation fields when available:
- `tenant_id`: Always included
- `trace_id`: Included when trace context is available
- `run_id`: Included when run context is available (for routing decisions)
- `flow_id`: Included when flow context is available (for routing decisions)

### Telemetry Integration

Circuit breaker metrics are integrated with Router telemetry system:
- Metrics exported via `router_metrics:emit_metric/3`
- Available through Telemetry events: `[router_metrics, router_circuit_breaker_events_total]`
- Compatible with Prometheus exporter (when implemented)

### Logging Integration

Circuit breaker log events follow `docs/OBSERVABILITY_CONVENTIONS.md`:
- Structured JSON format
- Required fields: `timestamp`, `level`, `component`, `message`
- Optional fields: `context`, `metadata`
- PII filtering applied (no sensitive data in logs)

## References

- `docs/OBSERVABILITY_CONVENTIONS.md` - Logging conventions
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Prometheus alert rules
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Circuit breaker design document
- `docs/ROUTING_POLICY.md` - Routing policy specification

