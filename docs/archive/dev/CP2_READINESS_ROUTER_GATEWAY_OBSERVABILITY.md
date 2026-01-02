---
version: 1.0
authors:
  - WORKER wrk-9: Documentation & Developer Experience
last_update: 2025-01-27T00:00:00Z
status: approved
rule_version: v10
message_protocol: v1
---

# CP2 Readiness: Router + Gateway Observability

**Checkpoint**: CP2-LC  
**Components**: `apps/otp/router` (Erlang/OTP) + Gateway (NestJS/C++)  
**Status**: ✅ **COMPLETE**  
**Focus**: OpenTelemetry tracing, Prometheus metrics, enhanced logging, alerting

## Purpose

This document provides detailed specifications for observability in CP2-LC. It complements the overview document (`docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md`) with observability-specific requirements, implementation details, and test coverage.

**Reference**: `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md` for overall CP2 readiness context.

---

## Observability Overview

### What Observability Adds in CP2

**CP1 Baseline** (for comparison):
- Structured JSON logging with PII filtering
- Health endpoint (gRPC health service on port 9000)
- Basic trace_id in logs

**CP2 Observability**:
- **OpenTelemetry Tracing**: Distributed tracing with span creation and context propagation
- **Prometheus Metrics**: Comprehensive metrics collection (latency, throughput, errors, redelivery)
- **Alert Rules**: Prometheus alert rules for Router health, errors, redelivery exhaustion
- **Enhanced Logging**: Extended log format with CP2 fields (run_id, flow_id, step_id, span_id, idempotency_key)
- **Log Correlation**: Correlation between Gateway and Router logs via trace_id and correlation_id

### Architecture Decision

**ADR-014**: Metrics and Tracing (`docs/ADR/ADR-014-metrics-tracing.md`)

**Key Decision**: Router and Gateway use OpenTelemetry for distributed tracing and Prometheus for metrics collection.

---

## OpenTelemetry Tracing

### Router Tracing Implementation

**Module**: `apps/otp/router/src/router_trace.erl`

**Key Functions**:
- `router_trace:start_span/2` - Start new span with name and attributes
- `router_trace:end_span/1` - End span and record duration
- `router_trace:set_attributes/2` - Set span attributes
- `router_trace:propagate_context/1` - Propagate trace context to downstream components

### Span Names

**Router Spans**:
- `beamline.router.decide` - Main routing decision span
- `beamline.router.policy.load` - Policy loading span
- `beamline.router.provider.select` - Provider selection span
- `beamline.router.emit.usage` - Usage event emission span

**Span Hierarchy**:
```
beamline.router.decide (parent)
  ├── beamline.router.policy.load (child)
  ├── beamline.router.provider.select (child)
  └── beamline.router.emit.usage (child)
```

### Trace Context Propagation

**Headers** (propagated via NATS messages):
- `trace_id` - Trace identifier (UUID)
- `span_id` - Current span identifier (UUID)
- `parent_span_id` - Parent span identifier (UUID)
- `trace_flags` - Trace flags (sampling decision)

**Propagation Flow**:
1. Gateway creates initial span for HTTP request
2. Gateway extracts trace context and includes in NATS headers
3. Router receives trace context and creates child spans
4. Router propagates trace context to Provider and Usage components
5. Gateway receives trace context from Router and closes spans

### Span Attributes

**Common Attributes** (all spans):
- `tenant_id` - Tenant identifier
- `message_id` - Message identifier
- `policy_id` - Policy identifier
- `provider_id` - Provider identifier (if selected)

**Router-Specific Attributes**:
- `router.decision.reason` - Decision reason (weighted, sticky, fallback)
- `router.decision.latency_ms` - Decision latency in milliseconds
- `router.decision.cost` - Expected cost
- `router.policy.load.latency_ms` - Policy load latency
- `router.provider.select.latency_ms` - Provider selection latency
- `router.usage.subject` - Usage event subject
- `router.usage.latency_ms` - Usage event emission latency
- `router.usage.cost` - Usage event cost

### Gateway Tracing Integration

**Gateway Implementation**:
- Gateway creates initial span for HTTP request
- Gateway extracts trace context from HTTP headers (`X-Trace-ID`, `X-Span-ID`)
- Gateway propagates trace context to Router via NATS headers
- Gateway receives trace context from Router and closes spans

**Gateway Spans**:
- `beamline.gateway.http.request` - HTTP request span
- `beamline.gateway.nats.publish` - NATS publish span
- `beamline.gateway.nats.reply` - NATS reply span
- `beamline.gateway.http.response` - HTTP response span

---

## Prometheus Metrics

### Router Metrics

**Module**: `apps/otp/router/src/router_metrics.erl`

**Key Metrics**:
- `router_decisions_total` - Total routing decisions (counter)
- `router_decisions_latency_seconds` - Decision latency (histogram)
- `router_errors_total` - Total errors (counter)
- `router_redelivery_total` - Total redelivery count (counter)
- `router_redelivery_exhausted_total` - MaxDeliver exhaustion count (counter)
- `router_policy_loads_total` - Total policy loads (counter)
- `router_policy_load_latency_seconds` - Policy load latency (histogram)
- `router_provider_selections_total` - Total provider selections (counter)
- `router_usage_emissions_total` - Total usage event emissions (counter)

**Labels**:
- `tenant_id` - Tenant identifier
- `policy_id` - Policy identifier
- `provider_id` - Provider identifier
- `error_code` - Error code (for error metrics)
- `decision_reason` - Decision reason (weighted, sticky, fallback)

### Gateway Metrics

**Gateway Implementation**:
- Gateway exposes `/metrics` endpoint for Prometheus scraping
- Gateway metrics include Router-related metrics (latency, errors, redelivery)

**Gateway Metrics**:
- `gateway_http_requests_total` - Total HTTP requests (counter)
- `gateway_http_request_latency_seconds` - HTTP request latency (histogram)
- `gateway_nats_requests_total` - Total NATS requests (counter)
- `gateway_nats_request_latency_seconds` - NATS request latency (histogram)
- `gateway_errors_total` - Total errors (counter)

**Labels**:
- `method` - HTTP method (GET, POST, etc.)
- `endpoint` - HTTP endpoint path
- `status_code` - HTTP status code
- `error_code` - Error code (for error metrics)

### Metrics Endpoint

**Router**: Router does not expose HTTP `/metrics` endpoint (metrics exported via OpenTelemetry collector or Prometheus exporter)

**Gateway**: Gateway exposes `/metrics` endpoint for Prometheus scraping:
- **Path**: `/metrics`
- **Format**: Prometheus text format
- **Content-Type**: `text/plain; version=0.0.4`

---

## Enhanced Logging

### Extended Log Format (CP2)

**CP1 Baseline Fields** (still required):
- `timestamp` - ISO 8601 format (UTC)
- `level` - ERROR, WARN, INFO, DEBUG
- `component` - "router" or "gateway"
- `message` - Human-readable message
- `trace_id` - Trace identifier (when available)
- `context` - Additional structured context (JSON object)

**CP2 Additional Fields**:
- `run_id` - Run identifier (UUID)
- `flow_id` - Flow identifier (UUID)
- `step_id` - Step identifier (UUID)
- `span_id` - Span identifier (UUID)
- `idempotency_key` - Idempotency key (when available)
- `correlation_id` - Correlation identifier (Gateway request ID)

**Enhanced Context** (CP2):
- `tenant_id` - Tenant identifier
- `provider_id` - Provider identifier (if selected)
- `policy_id` - Policy identifier
- `latency_ms` - Processing latency in milliseconds
- `cost` - Expected cost
- `decision_reason` - Decision reason (weighted, sticky, fallback)
- `error_code` - Error code (for error logs)
- `error_message` - Error message (for error logs)

### Log Correlation

**Gateway → Router Correlation**:
- Gateway logs include `correlation_id` (Gateway request ID)
- Router logs include `correlation_id` (from Gateway)
- Both Gateway and Router logs include `trace_id` (for distributed tracing)
- Logs can be correlated via `trace_id` and `correlation_id`

**Example Log Correlation**:
```json
// Gateway log
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "INFO",
  "component": "gateway",
  "message": "HTTP request received",
  "correlation_id": "req-123",
  "trace_id": "trace-456",
  "context": {
    "method": "POST",
    "endpoint": "/api/v1/routes/decide",
    "tenant_id": "tenant-1"
  }
}

// Router log
{
  "timestamp": "2025-01-27T12:00:01Z",
  "level": "INFO",
  "component": "router",
  "message": "Routing decision made",
  "correlation_id": "req-123",
  "trace_id": "trace-456",
  "context": {
    "tenant_id": "tenant-1",
    "provider_id": "openai",
    "decision_reason": "weighted",
    "latency_ms": 250
  }
}
```

### PII/Secret Filtering

**Filtered Fields** (same as CP1):
- `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`, `authorization`, `credit_card`, `ssn`, `email`, `phone`

**Replacement**: `[REDACTED]`

**Implementation**: `apps/otp/router/src/router_logger.erl` (extended for CP2)

---

## Alert Rules

### Prometheus Alert Rules

**File**: `docs/observability/router-alert-rules.yaml`

**Key Alerts**:

1. **Router Unhealthy**:
   - **Condition**: Router health check fails
   - **Severity**: Critical
   - **Runbook**: `docs/ADR/ADR-015-router-devstate-integration.md`

2. **High Error Rate**:
   - **Condition**: Error rate exceeds threshold (e.g., > 1% of requests)
   - **Severity**: Warning
   - **Runbook**: `docs/ADR/ADR-015-router-devstate-integration.md#cp1-minimal-mode-enforcement`

3. **Redelivery Exhaustion**:
   - **Condition**: MaxDeliver exhaustion rate exceeds threshold (e.g., > 0.1% of messages)
   - **Severity**: Critical
   - **Runbook**: `docs/ADR/ADR-011-jetstream-e2e.md`

4. **High Latency**:
   - **Condition**: P95 latency exceeds threshold (e.g., > 100ms)
   - **Severity**: Warning
   - **Runbook**: `docs/ADR/ADR-014-metrics-tracing.md`

5. **High Redelivery Rate**:
   - **Condition**: Redelivery rate exceeds threshold (e.g., > 5% of messages)
   - **Severity**: Warning
   - **Runbook**: `docs/ADR/ADR-011-jetstream-e2e.md`

### Alert Configuration

**Alert Labels**:
- `component` - "router" or "gateway"
- `severity` - "critical", "warning", "info"
- `runbook_adr` - ADR reference for runbook

**Alert Annotations**:
- `summary` - Alert summary
- `description` - Alert description
- `runbook_url` - Runbook URL (if available)

---

## Test Coverage

### Router Observability Tests

**Test Suite**: `apps/otp/router/test/router_observability_SUITE.erl` (extended for CP2)

**Test Cases**:
1. **Log Format (CP2 Fields)**: Verify CP2 fields (run_id, flow_id, step_id, span_id, idempotency_key) are included in logs
2. **PII Filtering**: Verify PII/secret filtering works correctly
3. **Health Endpoint**: Verify health endpoint works correctly
4. **Tracing Spans**: Verify OpenTelemetry spans are created correctly
5. **Span Attributes**: Verify span attributes are set correctly
6. **Trace Context Propagation**: Verify trace context is propagated correctly
7. **Metrics Collection**: Verify Prometheus metrics are collected correctly
8. **Log Correlation**: Verify logs can be correlated via trace_id and correlation_id

**Test Suite**: `apps/otp/router/test/router_tracing_SUITE.erl`

**Test Cases**:
1. **Span Creation**: Verify spans are created for routing decisions
2. **Span Hierarchy**: Verify span hierarchy is correct
3. **Span Attributes**: Verify span attributes are set correctly
4. **Trace Context Propagation**: Verify trace context is propagated to downstream components

**Test Suite**: `apps/otp/router/test/router_metrics_SUITE.erl`

**Test Cases**:
1. **Metrics Collection**: Verify metrics are collected correctly
2. **Metrics Labels**: Verify metrics labels are set correctly
3. **Metrics Values**: Verify metrics values are correct

### Gateway Observability Tests

**Test Suite**: Gateway observability tests (if exists)

**Test Cases**:
1. **Tracing Integration**: Verify Gateway creates spans and propagates trace context
2. **Metrics Collection**: Verify Gateway metrics are collected correctly
3. **Log Correlation**: Verify Gateway logs can be correlated with Router logs

### E2E Observability Tests

**Test Suite**: `apps/otp/router/test/router_tracing_e2e_SUITE.erl`

**Test Cases**:
1. **Full Trace**: HTTP request → Gateway → Router → Provider → Usage → Gateway → HTTP response
2. **Trace Context Propagation**: Verify trace context is propagated through entire chain
3. **Span Correlation**: Verify spans can be correlated across components
4. **Log Correlation**: Verify logs can be correlated via trace_id and correlation_id

---

## Configuration

### Environment Variables

**Router**:
- `ROUTER_TRACING_ENABLED` - Enable OpenTelemetry tracing (default: `true`)
- `ROUTER_TRACING_ENDPOINT` - OpenTelemetry collector endpoint (default: `http://localhost:4317`)
- `ROUTER_METRICS_ENABLED` - Enable Prometheus metrics (default: `true`)
- `ROUTER_METRICS_PORT` - Prometheus metrics port (default: `9090`)

**Gateway**:
- `GATEWAY_TRACING_ENABLED` - Enable OpenTelemetry tracing (default: `true`)
- `GATEWAY_TRACING_ENDPOINT` - OpenTelemetry collector endpoint (default: `http://localhost:4317`)
- `GATEWAY_METRICS_ENABLED` - Enable Prometheus metrics (default: `true`)
- `GATEWAY_METRICS_PORT` - Prometheus metrics port (default: `9090`)

---

## References

### Core Documentation

- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OVERVIEW.md` - CP2 readiness overview
- `docs/ADR/ADR-014-metrics-tracing.md` - Metrics and tracing architecture decision
- `docs/OBSERVABILITY_CONVENTIONS.md` - Observability conventions

### Implementation References

- `apps/otp/router/src/router_trace.erl` - Router tracing implementation
- `apps/otp/router/src/router_metrics.erl` - Router metrics implementation
- `apps/otp/router/src/router_logger.erl` - Router logging implementation
- `apps/otp/router/test/router_observability_SUITE.erl` - Observability tests
- `apps/otp/router/test/router_tracing_SUITE.erl` - Tracing tests
- `apps/otp/router/test/router_metrics_SUITE.erl` - Metrics tests

### Operational Guides

- `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - Router operational guide (observability section)
- `docs/observability/router-alert-rules.yaml` - Prometheus alert rules

---

## Change History

**v1.0 (2025-01-27)**:
- Initial version with detailed observability specifications
- OpenTelemetry tracing, Prometheus metrics, enhanced logging
- Test coverage and configuration
- Alert rules and monitoring

