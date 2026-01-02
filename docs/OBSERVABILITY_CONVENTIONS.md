# Observability Conventions

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Author**: Agent 10 - Observability/Telemetry

## Purpose

This document defines conventions for logging, metrics, and tracing across all BeamLine Constructor components. These conventions ensure consistent observability and enable effective monitoring and debugging.

## Logging Conventions

### Log Format

All logs must be structured JSON with the following required fields:

```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "router",
  "message": "Routing request started",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "trace_id": "abc123def4567890abcdef1234567890",
  "context": {
    "policy_id": "default",
    "latency_ms": 45.2
  }
}
```

### Required Fields (Always Present)

- **timestamp**: ISO 8601 format with microsecond precision
- **level**: One of `ERROR`, `WARN`, `INFO`, `DEBUG`
- **component**: Component identifier (`router`, `provider`, `gateway`, `worker`, `usage`)
- **message**: Human-readable message describing the event

### Optional Fields

- **context**: Additional structured context (JSON object)
- **metadata**: Additional metadata for debugging

### CP1 Cross-Cutting Invariants (Required When Context Available)

**CRITICAL**: These fields are not always required, but **must be included** when the corresponding context is available in the component:

- **tenant_id** (string): Tenant identifier - **Required when tenant context is available** (all components)
- **run_id** (string): Run identifier for tracking execution flow - **Required when run context is available** (Router, Worker, Gateway)
- **flow_id** (string): Flow identifier for workflow tracking - **Required when flow context is available** (Router, Worker)
- **step_id** (string): Step identifier within a flow - **Required when step context is available** (Worker, Router)
- **trace_id** (string): Trace identifier for distributed tracing - **Required when trace context is available** (all components)
- **error_code** (string): Error code for structured error handling - **Optional** (for ERROR level)
- **latency_ms** (number): Operation latency in milliseconds - **Optional** (for performance tracking)

**Component-Specific Requirements:**

- **Router**: Must include `tenant_id`, `run_id`, `flow_id`, `trace_id` when available
- **Gateway**: Must include `tenant_id`, `run_id`, `trace_id` when available
- **Worker**: Must include `tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id` when available
- **Provider**: Must include `tenant_id`, `trace_id` when available

### Log Levels

#### ERROR

Critical errors requiring immediate attention:

- Service failures
- Database connection errors
- Authentication failures
- Data corruption
- Unhandled exceptions

**Example:**
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "ERROR",
  "component": "router",
  "message": "Policy load failed",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "error_code": "ROUTER_POLICY_LOAD_FAILED",
  "context": {
    "policy_id": "default",
    "error": "database_connection_failed",
    "latency_ms": 5000.0
  }
}
```

#### WARN

Warnings and potential issues:

- Fallback provider used
- Cache misses
- Approaching rate limits
- Degraded performance

**Example:**
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "WARN",
  "component": "router",
  "message": "No provider available",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "context": {
    "latency_ms": 150.5,
    "fallback_used": true
  }
}
```

#### INFO

Informational messages for normal operations:

- Request started/completed
- Policy loaded
- Provider selected
- Successful operations

**Example:**
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "router",
  "message": "Routing decision made",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "latency_ms": 45,
  "context": {
    "provider_id": "openai",
    "reason": "weighted",
    "total_latency_ms": 45.2,
    "decision_latency_ms": 12.5
  }
}
```

#### DEBUG

Detailed debugging information:

- Cache hits/misses
- Decision algorithm steps
- Policy parsing details
- Internal state transitions

**Example:**
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "DEBUG",
  "component": "router",
  "message": "Policy loaded from cache",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "context": {
    "policy_id": "default",
    "source": "cache",
    "latency_ms": 0.5
  }
}
```

### PII and Secret Filtering

**CRITICAL**: Never log PII (Personally Identifiable Information) or secrets.

#### Filtered Fields

The following fields are automatically filtered and replaced with `[REDACTED]`:

- `password`
- `api_key`
- `secret`
- `token`
- `access_token`
- `refresh_token`
- `authorization`
- `credit_card`
- `ssn`
- `email` (optional, configurable)
- `phone` (optional, configurable)

#### Example

**Input:**
```erlang
Context = #{
    <<"tenant_id">> => <<"tenant_123">>,
    <<"api_key">> => <<"sk-1234567890abcdef">>,
    <<"message">> => <<"Hello">>
}
```

**Output (logged):**
```json
{
  "context": {
    "tenant_id": "tenant_123",
    "api_key": "[REDACTED]",
    "message": "Hello"
  }
}
```

## Metrics Conventions

### Metric Naming

All metrics follow Prometheus naming conventions:

- Use `snake_case`
- Include unit suffix (`_total`, `_seconds`, `_bytes`)
- Prefix with component name (`beamline_router_`, `beamline_provider_`)

### Metric Types

#### Counter

For cumulative values that only increase:

- `beamline_router_requests_total{provider, status}`
- `beamline_router_errors_total{provider, error_type}`
- `beamline_router_policy_loads_total{tenant_id, source}`

#### Histogram

For latency and duration measurements:

- `beamline_router_latency_seconds{provider}` - buckets: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0]
- `beamline_router_decision_latency_seconds{decision_type}` - buckets: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5]

#### Gauge

For values that can increase or decrease:

- `beamline_router_active_requests`

### Key Metrics

#### Router Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `beamline_router_requests_total` | Counter | `provider`, `status` | Total routing requests |
| `beamline_router_latency_seconds` | Histogram | `provider` | Request latency |
| `beamline_router_errors_total` | Counter | `provider`, `error_type` | Total errors |
| `beamline_router_active_requests` | Gauge | - | Active requests |
| `beamline_router_policy_loads_total` | Counter | `tenant_id`, `source` | Policy loads (cache/database) |
| `beamline_router_cache_hits_total` | Counter | `cache_type` | Cache hits |
| `beamline_router_cache_misses_total` | Counter | `cache_type` | Cache misses |
| `beamline_router_decision_latency_seconds` | Histogram | `decision_type` | Decision algorithm latency |

### Metric Labels

**Best Practices:**

- Use consistent label values across metrics
- Avoid high cardinality labels (e.g., user IDs)
- Use tenant_id only for aggregated metrics
- Keep label names short and descriptive

## Tracing Conventions

### Span Names

Use hierarchical span names:

- `beamline.router.decide` - Main routing decision
- `beamline.router.policy.load` - Policy loading
- `beamline.router.decision` - Decision algorithm
- `beamline.router.provider.select` - Provider selection

### Span Attributes

Include relevant attributes:

- `tenant_id` (string)
- `policy_id` (string)
- `provider_id` (string)
- `decision_reason` (string)
- `policy_version` (string)

### Trace Context Propagation

#### gRPC

Extract/inject trace context from/to gRPC metadata:

- Header: `trace_id`
- Header: `span_id`
- Header: `X-Trace-Id` (alternative)
- Header: `X-Span-Id` (alternative)

#### NATS

Extract/inject trace context from/to NATS message headers:

- Header: `trace_id`
- Header: `span_id`

#### HTTP

Extract/inject trace context from/to HTTP headers:

- Header: `X-Trace-Id`
- Header: `X-Span-Id`

### Sampling Strategy

- **100% sampling** for errors
- **10% sampling** for successful requests
- **100% sampling** for critical operations (policy loads, fallbacks)

## Component-Specific Conventions

### Router Component

**Logging Points:**

1. Request received (INFO)
2. Policy load started (DEBUG)
3. Policy loaded from cache/database (DEBUG/INFO)
4. Decision algorithm started (DEBUG)
5. Provider selected (INFO/DEBUG)
6. Decision made (INFO)
7. Request completed (INFO)
8. Errors (ERROR)

**Metrics:**

- Request counters
- Latency histograms
- Error counters
- Active requests gauge
- Policy load counters
- Cache hit/miss counters

**Tracing:**

- Main span: `beamline.router.decide`
- Child spans: policy load, decision algorithm

### Provider Component

**Logging Points:**

1. Provider invocation started (INFO)
2. Provider response received (INFO)
3. Provider errors (ERROR)
4. Cost calculation (DEBUG)

**Metrics:**

- Invocation counters
- Latency histograms
- Cost counters
- Error counters

**Tracing:**

- Main span: `beamline.provider.invoke`
- Child spans: request preparation, response processing

## Best Practices

### Performance

- Use DEBUG level sparingly in production
- Batch log writes when possible
- Use async logging for high-throughput scenarios
- Sample traces appropriately

### Security

- Never log secrets or PII
- Sanitize all user input in logs
- Use structured logging to avoid injection
- Mask sensitive data automatically

### Debugging

- Include correlation IDs (trace_id) in all logs
- Log request/response pairs with same trace_id
- Include latency measurements in logs
- Log decision paths for debugging

### Monitoring

- Define SLOs based on metrics
- Set up alerts for error rates
- Monitor latency percentiles (p50, p95, p99)
- Track cache hit rates

## R10 Circuit Breaker Metrics

### Overview

R10 (Retry Logic + Circuit Breaker) metrics are centralized in `router_r10_metrics.erl` module. All metric reading must go through this module - **never access ETS directly**.

### Metric Access Layer

**Public API for Tests**:
- `get_metric_value/2` - Read any metric value by name and labels
- `get_latest_trigger_reason/2` - Get latest trigger reason for tenant/provider
- `wait_for_trigger_reason/4` - Wait for trigger reason to appear with timeout (recommended, P2.2)
- `assert_trigger_reason_in/3` - Assert trigger reason is in allowed list (instant check)
- `get_publish_attempts_total/0` - Get total publish attempts
- `get_publish_errors_total/0` - Get total publish errors
- `get_publish_attempts_delta/1` - Get attempts delta before/after action
- `get_publish_errors_delta/1` - Get errors delta before/after action
- `dump_metrics/0` - Dump all metrics for debugging

### Circuit Breaker Metrics

**State Metrics**:
- `router_circuit_breaker_state` - Current state (closed/open/half_open) as gauge
- `router_circuit_breaker_state_transitions_total` - Total state transitions as counter

**Trigger Metrics**:
- `router_circuit_breaker_trigger_reason` - Reason for opening (failure_threshold_exceeded, error_rate_threshold_exceeded, latency_threshold_exceeded, half_open_failure, timeout_elapsed)

**Labels**:
- `tenant_id` - Tenant identifier
- `provider_id` - Provider identifier
- `state` - Circuit breaker state (for state metrics)
- `reason` - Trigger reason (for trigger metrics)

### Trigger Reason Constants

All trigger reason values are available as constants in `router_r10_metrics`:
- `trigger_reason_failure_threshold()` - `<<"failure_threshold_exceeded">>`
- `trigger_reason_error_rate()` - `<<"error_rate_threshold_exceeded">>`
- `trigger_reason_latency()` - `<<"latency_threshold_exceeded">>`
- `trigger_reason_half_open_failure()` - `<<"half_open_failure">>`
- `trigger_reason_timeout()` - `<<"timeout_elapsed">>`

### Best Practices

1. **Always use constants** instead of hardcoded binaries
2. **Accept multiple valid reasons** when checking trigger_reason (e.g., failure_threshold OR error_rate)
3. **Use `wait_for_trigger_reason/4`** for waiting with timeout (recommended, P2.2)
4. **Use `assert_trigger_reason_in/3`** for instant checks (after state transition confirmed)
5. **Check state first, then metrics** in E2E tests
6. **Use appropriate timeouts**: 3-5 seconds for state, 2-3 seconds for metrics

### Module Responsibilities

- **`router_test_utils`**: Lifecycle management (start/stop app, ensure processes alive, waiters)
- **`router_r10_metrics`**: All metric reading and assertions (single source of truth)

## Risk Test Metrics Pattern (X_rN_metrics Template)

### Overview

R10 establishes a pattern for risk-based testing with centralized metric access. This pattern **MUST be replicated** for future risk themes (R11, R12, etc.).

**CRITICAL**: New risk tests must go through their own `*_rN_metrics` module. Direct ETS access in tests is **FORBIDDEN**.

### Pattern Requirements

**MANDATORY for new risk themes** (R11, R12, etc.):

1. **Create `<module>_rN_metrics.erl`** following `router_r10_metrics.erl` pattern:
   - Centralize all metric names as constants
   - Provide helper functions for reading metrics
   - Export assertion helpers
   - **Never expose direct ETS access**
   - Document all metric names and labels

2. **Enforce no direct ETS access** (MANDATORY):
   - Tests **MUST** use `<module>_rN_metrics` functions
   - **FORBIDDEN**: `ets:lookup`, `ets:info`, `ets:tab2list` in test code
   - **FORBIDDEN**: Direct ETS table access in test helpers
   - All metric reading **MUST** go through the metrics module

3. **Lifecycle helpers**:
   - Keep lifecycle management in `*_test_utils.erl`
   - Keep metric reading in `<module>_rN_metrics.erl`
   - Clear separation of concerns
   - Provide `clear_metrics/0` or `reset_metrics/0` for test cleanup

### Example Structure

```erlang
%% <module>_rN_metrics.erl
-module(<module>_rN_metrics).

-export([
    %% Metric name constants
    metric_<name>/0,
    
    %% Reading helpers
    get_metric_value/2,
    get_latest_<specific_metric>/2,
    
    %% Assertion helpers
    assert_<condition>/3,
    
    %% Debugging
    dump_metrics/0
]).
```

### Migration Checklist

When creating new risk test module:

- [ ] Create `<module>_rN_metrics.erl` with constants and helpers
- [ ] Update tests to use metrics module (no direct ETS)
- [ ] Document in `OBSERVABILITY_CONVENTIONS.md`
- [ ] Add to test plan documentation
- [ ] Verify CI profiles if needed

### Template and Documentation

**Template**: See `../apps/otp/router/docs/dev/METRICS_MODULE_TEMPLATE.md` for complete template with code examples.

**Reference Implementation**: `apps/otp/router/src/router_r10_metrics.erl` demonstrates the full pattern.

**Related Patterns**: See `../apps/otp/router/docs/dev/GEN_SERVER_RESET_LIFECYCLE_PATTERN.md` for gen_server lifecycle management.

## Router Intake Backpressure Framework

### Overview

Router intake backpressure detection and handling is implemented in `router_intake_backpressure.erl`. The framework provides real-time overload detection using JetStream consumer info queries and P95 latency calculation from histogram metrics.

### Framework Structure

**Current Implementation (CP2)**:
- ⚠️ **STUB IMPLEMENTATION**: Real-time queries and P95 calculations are stubs with fallback to cached ETS values
- Framework structure is in place for CP3/Release implementation
- All functions have clear STUB markers and implementation notes

**Key Functions**:
- `check_backpressure/1` - Check backpressure status for a subject
- `get_jetstream_pending/1` - Get JetStream pending messages (stub with ETS fallback)
- `get_processing_latency_p95/1` - Get processing latency P95 (stub with ETS fallback)
- `get_inflight_count/1` - Get in-flight message count

### Framework Pattern

**Real-time Query Framework**:
```erlang
%% Pattern: Try real-time query first, fallback to cached values
get_jetstream_pending(Subject) ->
    case try_real_time_jetstream_query(Subject) of
        {ok, Pending} ->
            update_pending_cache(Subject, Pending),
            Pending;
        {error, nats_unavailable} ->
            get_jetstream_pending_cached(Subject);
        {error, _Reason} ->
            get_jetstream_pending_cached(Subject)
    end.
```

**P95 Calculation Framework**:
```erlang
%% Pattern: Try real-time calculation first, fallback to cached values
get_processing_latency_p95(Subject) ->
    case try_calculate_p95_from_histogram(Subject) of
        {ok, P95Ms} ->
            update_latency_cache(Subject, P95Ms),
            P95Ms;
        {error, histogram_unavailable} ->
            get_processing_latency_p95_cached(Subject);
        {error, _Reason} ->
            get_processing_latency_p95_cached(Subject)
    end.
```

### Implementation Notes

**CP3/Release Requirements**:
- Real-time JetStream consumer info queries via NATS API (`$JS.API.CONSUMER.INFO.{stream}.{consumer}`)
- P95 calculation from histogram metrics (Prometheus histogram or local samples)
- Complete Gateway → Router backpressure integration
- Production-ready backpressure policies

**Current Behavior**:
- Stub implementations return cached ETS values
- Framework structure supports future real-time implementation
- All stubs are clearly marked and documented

### Metrics

**Backpressure Status Metrics**:
- `router_intake_backpressure_active` - Backpressure active status (0/1)
- `router_intake_backpressure_pending_messages` - JetStream pending messages
- `router_intake_backpressure_latency_p95_ms` - Processing latency P95
- `router_intake_backpressure_inflight_messages` - In-flight message count

**Labels**:
- `subject` - NATS subject identifier

### Best Practices

1. **Use framework functions** - Always use `get_jetstream_pending/1` and `get_processing_latency_p95/1` instead of direct ETS access
2. **Handle fallbacks** - Framework automatically falls back to cached values when real-time queries unavailable
3. **Cache updates** - Framework updates cache with real-time values when available
4. **STUB awareness** - Be aware that current implementation uses stubs (documented in code)

## References

- [Prometheus Best Practices](https://prometheus.io/docs/practices/naming/)
- [OpenTelemetry Semantic Conventions](https://opentelemetry.io/docs/specs/otel/)
- [Structured Logging Best Practices](https://www.structlog.org/en/stable/why.html)
- `docs/LIFECYCLE_RESET_PATTERN.md` - Gen server lifecycle and reset pattern


