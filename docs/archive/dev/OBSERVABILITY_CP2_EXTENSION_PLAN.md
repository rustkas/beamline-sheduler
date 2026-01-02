# Observability CP2 Extension Plan

⚠️ **LEGACY**: Early planning document. See `docs/archive/dev/EXTENSIONS_CP2_OBSERVABILITY_ALIGNMENT_REPORT.md` for current alignment.

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: ⚠️ **LEGACY** (Early Planning)  
**Control Point**: CP2-LC  
**WORKER**: `wrk-obs1` (Observability CP2)  
**Current Phase**: CP1 (Complete) → CP2 (Planned)  
**Current Source of Truth**: `docs/archive/dev/EXTENSIONS_CP2_OBSERVABILITY_ALIGNMENT_REPORT.md`

---

## Purpose

This document defines the high-level specification for extending CP1 observability (structured JSON logs and health endpoints) with **Prometheus metrics** and **OpenTelemetry distributed tracing** in CP2.

**Key Principle**: CP2 observability features must build upon and extend CP1 invariants without breaking existing CP1 contracts.

---

## CP1 Foundation

### CP1 Observability Features (Complete)

**Reference**: `docs/OBSERVABILITY_CP1_INVARIANTS.md`

#### 1. Structured JSON Logging

**Required Fields** (always present):
- `timestamp` (ISO 8601, UTC, microseconds precision)
- `level` (ERROR, WARN, INFO, DEBUG)
- `component` (router, gateway, worker, provider, usage)
- `message` (human-readable text)

**CP1 Correlation Fields** (when context available):
- `tenant_id` - Tenant identifier
- `run_id` - Run identifier
- `flow_id` - Flow identifier
- `step_id` - Step identifier
- `trace_id` - Trace identifier (W3C Trace Context format)
- `error_code` - Error code (for ERROR level)
- `latency_ms` - Performance tracking (optional)

**Context Object** (optional structured data):
- Component-specific context (provider, policy_id, status_code, etc.)

#### 2. Health Endpoints

**Component-Specific Endpoints**:

| Component | Protocol | Path | Port | Response Format |
|-----------|----------|------|------|-----------------|
| Router | gRPC | `grpc.health.v1.Health/Check` | 9000 | gRPC health status |
| Gateway | HTTP | `GET /_health` | 3000 | JSON with status, timestamp, checks |
| Worker | HTTP | `GET /_health` | 8080 | JSON with status, timestamp, checks |

#### 3. PII/Secret Filtering

**Automatic filtering** of sensitive fields before logging:
- `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`
- `authorization`, `credit_card`, `ssn`, `email`, `phone`
- Replacement: `[REDACTED]`

---

## CP2 Extension: Metrics and Tracing

### Overview

CP2 extends CP1 observability with:

1. **Prometheus Metrics**: Export metrics via HTTP `/metrics` endpoint
2. **OpenTelemetry Tracing**: Distributed tracing with context propagation
3. **Dashboards**: Grafana dashboards for visualization
4. **Alerting**: Alertmanager rules for production monitoring

**Critical**: All CP2 features must use CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`) as labels/dimensions.

---

## Mapping: CP1 Logs → Prometheus Metrics

### Metric Categories

#### 1. Request Metrics

**Source**: CP1 log fields `latency_ms`, `status_code`, `component`, `context.method`, `context.path`

**Metrics**:

```prometheus
# HTTP Request Rate
http_requests_total{component="gateway|router", method="GET|POST|PUT|DELETE", path="/api/v1/...", status_code="200|400|500", tenant_id="..."} <counter>

# HTTP Request Duration
http_request_duration_seconds{component="gateway|router", method="GET|POST|PUT|DELETE", path="/api/v1/...", status_code="200|400|500", tenant_id="..."} <histogram>

# HTTP Request Size
http_request_size_bytes{component="gateway|router", method="GET|POST|PUT|DELETE", path="/api/v1/...", tenant_id="..."} <histogram>

# HTTP Response Size
http_response_size_bytes{component="gateway|router", method="GET|POST|PUT|DELETE", path="/api/v1/...", status_code="200|400|500", tenant_id="..."} <histogram>
```

**CP1 Field Mapping**:
- `component` → `component` label
- `context.method` → `method` label
- `context.path` → `path` label
- `context.status_code` → `status_code` label
- `tenant_id` → `tenant_id` label (when available)
- `latency_ms` → `http_request_duration_seconds` histogram value (convert ms to seconds)

#### 2. Routing Metrics

**Source**: CP1 log fields `component="router"`, `context.provider`, `context.policy_id`, `context.decision_reason`

**Metrics**:

```prometheus
# Routing Decisions
router_decisions_total{policy_id="default|...", provider="openai|anthropic|...", decision_reason="weighted_selection|fallback|...", tenant_id="...", run_id="..."} <counter>

# Routing Decision Duration
router_decision_duration_seconds{policy_id="default|...", provider="openai|anthropic|...", decision_reason="weighted_selection|fallback|...", tenant_id="...", run_id="..."} <histogram>

# Provider Selection Count
router_provider_selections_total{provider="openai|anthropic|...", policy_id="default|...", tenant_id="...", run_id="..."} <counter>

# Policy Evaluation Duration
router_policy_evaluation_duration_seconds{policy_id="default|...", tenant_id="...", run_id="..."} <histogram>
```

**CP1 Field Mapping**:
- `context.provider` → `provider` label
- `context.policy_id` → `policy_id` label
- `context.decision_reason` → `decision_reason` label
- `tenant_id` → `tenant_id` label (when available)
- `run_id` → `run_id` label (when available)
- `latency_ms` → `router_decision_duration_seconds` histogram value

#### 3. Worker Execution Metrics

**Source**: CP1 log fields `component="worker"`, `context.step_type`, `context.execution_status`, `step_id`, `flow_id`

**Metrics**:

```prometheus
# Step Executions
worker_step_executions_total{step_type="llm_call|http_request|condition|...", execution_status="success|error|timeout", tenant_id="...", run_id="...", flow_id="...", step_id="..."} <counter>

# Step Execution Duration
worker_step_execution_duration_seconds{step_type="llm_call|http_request|condition|...", execution_status="success|error|timeout", tenant_id="...", run_id="...", flow_id="...", step_id="..."} <histogram>

# Step Errors
worker_step_errors_total{step_type="llm_call|http_request|condition|...", error_code="...", tenant_id="...", run_id="...", flow_id="...", step_id="..."} <counter>

# Flow Execution Duration
worker_flow_execution_duration_seconds{tenant_id="...", run_id="...", flow_id="..."} <histogram>
```

**CP1 Field Mapping**:
- `context.step_type` → `step_type` label
- `context.execution_status` → `execution_status` label
- `error_code` → `error_code` label (for error metrics)
- `tenant_id` → `tenant_id` label (when available)
- `run_id` → `run_id` label (when available)
- `flow_id` → `flow_id` label (when available)
- `step_id` → `step_id` label (when available)
- `latency_ms` → `worker_step_execution_duration_seconds` histogram value

#### 4. Error Metrics

**Source**: CP1 log fields `level="ERROR"`, `error_code`, `component`

**Metrics**:

```prometheus
# Component Errors
component_errors_total{component="router|gateway|worker", error_code="...", tenant_id="...", run_id="..."} <counter>

# Error Rate (per component)
component_error_rate{component="router|gateway|worker", tenant_id="..."} <gauge>
```

**CP1 Field Mapping**:
- `component` → `component` label
- `error_code` → `error_code` label
- `tenant_id` → `tenant_id` label (when available)
- `run_id` → `run_id` label (when available)

#### 5. Health Metrics

**Source**: CP1 health endpoint responses

**Metrics**:

```prometheus
# Component Health Status
component_health_status{component="router|gateway|worker", check="database|nats|..."} <gauge>  # 1 = healthy, 0 = unhealthy

# Health Check Duration
component_health_check_duration_seconds{component="router|gateway|worker", check="database|nats|..."} <histogram>
```

**CP1 Field Mapping**:
- Health endpoint `status` → `component_health_status` gauge (1 = "healthy", 0 = "degraded"/"unhealthy")
- Health endpoint `checks[].status` → `component_health_status` gauge per check
- Health check latency → `component_health_check_duration_seconds` histogram

### Label Cardinality Considerations

**High Cardinality Labels** (use sparingly):
- `run_id` - High cardinality (unique per run)
- `step_id` - High cardinality (unique per step)
- `flow_id` - Medium cardinality (unique per flow)

**Recommendation**: 
- Include `run_id`, `step_id`, `flow_id` only in detailed metrics (e.g., `worker_step_executions_total`)
- Exclude from aggregate metrics (e.g., `component_error_rate`)
- Use `tenant_id` for multi-tenant aggregation

---

## Mapping: CP1 Logs → OpenTelemetry Traces

### Trace Structure

**Source**: CP1 correlation fields `trace_id`, `run_id`, `flow_id`, `step_id`, `tenant_id`

### Trace Context Propagation

**CP1 `trace_id` Format**: W3C Trace Context format
- Format: `{trace_id}-{span_id}-{flags}`
- Example: `4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01`

**CP1 Fields → OTEL Trace Attributes**:

| CP1 Field | OTEL Attribute | Type | Description |
|-----------|----------------|------|-------------|
| `trace_id` | `trace.trace_id` | string | W3C Trace Context trace ID |
| `run_id` | `run.id` | string | Run identifier |
| `flow_id` | `flow.id` | string | Flow identifier |
| `step_id` | `step.id` | string | Step identifier |
| `tenant_id` | `tenant.id` | string | Tenant identifier |
| `component` | `service.name` | string | Component name (router, gateway, worker) |
| `error_code` | `error.code` | string | Error code (for error spans) |
| `latency_ms` | `duration_ms` | int64 | Duration in milliseconds |

### Span Hierarchy

**CP2 Trace Structure**:

```
Trace (trace_id)
├── Span: Gateway Request (run_id)
│   ├── Span: Router Decision (run_id, flow_id)
│   │   ├── Span: Policy Evaluation (run_id, flow_id)
│   │   └── Span: Provider Selection (run_id, flow_id)
│   └── Span: Worker Execution (run_id, flow_id)
│       ├── Span: Step 1 (run_id, flow_id, step_id)
│       │   ├── Span: LLM Call (run_id, flow_id, step_id)
│       │   └── Span: Response Processing (run_id, flow_id, step_id)
│       ├── Span: Step 2 (run_id, flow_id, step_id)
│       └── Span: Step N (run_id, flow_id, step_id)
```

### Span Attributes

**Gateway Request Span**:
```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "00f067aa0ba902b7",
  "name": "gateway.request",
  "kind": "SERVER",
  "attributes": {
    "service.name": "gateway",
    "run.id": "run_abc123",
    "tenant.id": "tenant_123",
    "http.method": "POST",
    "http.path": "/api/v1/messages",
    "http.status_code": 200,
    "duration_ms": 120
  }
}
```

**Router Decision Span**:
```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "a3ce929d0e0e4736",
  "name": "router.decision",
  "kind": "INTERNAL",
  "parent_span_id": "00f067aa0ba902b7",
  "attributes": {
    "service.name": "router",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "tenant.id": "tenant_123",
    "policy.id": "default",
    "provider.selected": "openai",
    "decision.reason": "weighted_selection",
    "duration_ms": 45
  }
}
```

**Worker Step Span**:
```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "def4567890abcdef",
  "name": "worker.step.execution",
  "kind": "INTERNAL",
  "parent_span_id": "a3ce929d0e0e4736",
  "attributes": {
    "service.name": "worker",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "step.id": "step_001",
    "tenant.id": "tenant_123",
    "step.type": "llm_call",
    "execution.status": "success",
    "duration_ms": 250
  }
}
```

### Trace Context Injection

**CP1 Logs → OTEL Context**:

1. **Extract `trace_id` from CP1 logs**:
   - Parse W3C Trace Context format
   - Extract `trace_id`, `span_id`, `flags`

2. **Propagate context via headers**:
   - HTTP: `traceparent` header (W3C Trace Context)
   - gRPC: Metadata with `traceparent` key
   - NATS: Message headers with `traceparent` key

3. **Create child spans**:
   - Use `trace_id` from parent
   - Generate new `span_id` for child
   - Maintain parent-child relationship via `parent_span_id`

### Error Spans

**CP1 ERROR logs → OTEL Error Spans**:

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "error_span_id",
  "name": "router.policy.load.error",
  "kind": "INTERNAL",
  "status": {
    "code": "ERROR",
    "message": "Failed to load policy"
  },
  "attributes": {
    "service.name": "router",
    "error.code": "POLICY_LOAD_ERROR",
    "error.message": "Failed to load policy: policy_not_found",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123"
  }
}
```

**CP1 Field Mapping**:
- `level="ERROR"` → `status.code="ERROR"`
- `error_code` → `error.code` attribute
- `message` → `error.message` attribute
- `context` → additional error attributes

---

## Dashboards

### Dashboard Categories

#### 1. Request Dashboard

**Metrics**:
- HTTP request rate (requests/sec)
- HTTP request duration (p50, p95, p99)
- HTTP error rate (errors/sec)
- HTTP status code distribution

**Filters**:
- `component` (gateway, router)
- `tenant_id` (multi-tenant filtering)
- `method` (GET, POST, PUT, DELETE)
- `path` (API endpoint)

**CP1 Data Sources**:
- `http_requests_total` (Prometheus)
- `http_request_duration_seconds` (Prometheus)
- `component_errors_total` (Prometheus)

#### 2. Routing Dashboard

**Metrics**:
- Routing decision rate (decisions/sec)
- Routing decision duration (p50, p95, p99)
- Provider selection distribution
- Policy evaluation duration

**Filters**:
- `policy_id` (policy filtering)
- `provider` (provider filtering)
- `tenant_id` (multi-tenant filtering)
- `run_id` (run-level filtering)

**CP1 Data Sources**:
- `router_decisions_total` (Prometheus)
- `router_decision_duration_seconds` (Prometheus)
- `router_provider_selections_total` (Prometheus)

#### 3. Worker Execution Dashboard

**Metrics**:
- Step execution rate (steps/sec)
- Step execution duration (p50, p95, p99)
- Step error rate (errors/sec)
- Flow execution duration

**Filters**:
- `step_type` (llm_call, http_request, condition, etc.)
- `execution_status` (success, error, timeout)
- `tenant_id` (multi-tenant filtering)
- `run_id` (run-level filtering)
- `flow_id` (flow-level filtering)

**CP1 Data Sources**:
- `worker_step_executions_total` (Prometheus)
- `worker_step_execution_duration_seconds` (Prometheus)
- `worker_step_errors_total` (Prometheus)

#### 4. Error Dashboard

**Metrics**:
- Component error rate (errors/sec)
- Error code distribution
- Error rate by component

**Filters**:
- `component` (router, gateway, worker)
- `error_code` (error code filtering)
- `tenant_id` (multi-tenant filtering)

**CP1 Data Sources**:
- `component_errors_total` (Prometheus)
- `component_error_rate` (Prometheus)

#### 5. Health Dashboard

**Metrics**:
- Component health status (healthy/unhealthy)
- Health check duration
- Health check failures

**Filters**:
- `component` (router, gateway, worker)
- `check` (database, nats, etc.)

**CP1 Data Sources**:
- `component_health_status` (Prometheus)
- `component_health_check_duration_seconds` (Prometheus)

#### 6. Distributed Tracing Dashboard

**Metrics**:
- Trace duration (p50, p95, p99)
- Trace error rate
- Span count per trace
- Service dependency graph

**Filters**:
- `trace_id` (trace-level filtering)
- `run_id` (run-level filtering)
- `tenant_id` (multi-tenant filtering)
- `service.name` (component filtering)

**CP1 Data Sources**:
- OpenTelemetry traces (Tempo/Jaeger)
- Trace attributes from CP1 correlation fields

---

## Component Changes (CP2)

### Router Changes

**Required Changes**:

1. **Prometheus Metrics Export**:
   - Add HTTP `/metrics` endpoint (port 9001, separate from gRPC port 9000)
   - Export metrics: `router_decisions_total`, `router_decision_duration_seconds`, `router_provider_selections_total`, `router_policy_evaluation_duration_seconds`
   - Use CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`) as labels

2. **OpenTelemetry Integration**:
   - Add OTEL SDK (Erlang/OTP: `opentelemetry-erlang`)
   - Create spans for routing decisions
   - Propagate trace context via NATS message headers
   - Export traces to OTEL collector

3. **Log Enhancement** (backward compatible):
   - Ensure CP1 correlation fields are always included when available
   - Add `span_id` to logs for trace correlation (optional CP2 enhancement)

**Files to Modify** (CP2):
- `apps/otp/router/src/router_observability_stub.erl` → `router_observability.erl` (implementation)
- `apps/otp/router/src/router_metrics.erl` (new file)
- `apps/otp/router/src/router_tracing.erl` (new file)
- `apps/otp/router/config/sys.config` (OTEL configuration)

**No Changes Required** (CP1 invariants preserved):
- JSON log format (unchanged)
- Health endpoint (unchanged)
- PII filtering (unchanged)

### Gateway Changes

**Required Changes**:

1. **Prometheus Metrics Export**:
   - Add HTTP `/metrics` endpoint (port 3001, separate from API port 3000)
   - Export metrics: `http_requests_total`, `http_request_duration_seconds`, `http_request_size_bytes`, `http_response_size_bytes`
   - Use CP1 correlation fields (`tenant_id`, `run_id`) as labels

2. **OpenTelemetry Integration**:
   - Add OTEL SDK (NestJS: `@opentelemetry/api`, `@opentelemetry/sdk-node`)
   - Create spans for HTTP requests
   - Extract/inject trace context via HTTP headers (`traceparent`)
   - Export traces to OTEL collector

3. **Log Enhancement** (backward compatible):
   - Ensure CP1 correlation fields are always included when available
   - Add `span_id` to logs for trace correlation (optional CP2 enhancement)

**Files to Modify** (CP2):
- `apps/c-gateway/src/observability/observability.module.ts` (new file)
- `apps/c-gateway/src/observability/metrics.service.ts` (new file)
- `apps/c-gateway/src/observability/tracing.service.ts` (new file)
- `apps/c-gateway/src/main.ts` (OTEL initialization)

**No Changes Required** (CP1 invariants preserved):
- JSON log format (unchanged)
- Health endpoint (unchanged)
- PII filtering (unchanged)

### Worker Changes

**Required Changes**:

1. **Prometheus Metrics Export**:
   - Add HTTP `/metrics` endpoint (port 8081, separate from API port 8080)
   - Export metrics: `worker_step_executions_total`, `worker_step_execution_duration_seconds`, `worker_step_errors_total`, `worker_flow_execution_duration_seconds`
   - Use CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `step_id`) as labels

2. **OpenTelemetry Integration**:
   - Add OTEL SDK (C++: `opentelemetry-cpp`)
   - Create spans for step executions
   - Extract/inject trace context via NATS message headers
   - Export traces to OTEL collector

3. **Log Enhancement** (backward compatible):
   - Ensure CP1 correlation fields are always included when available
   - Add `span_id` to logs for trace correlation (optional CP2 enhancement)

**Files to Modify** (CP2):
- `apps/caf/processor/src/observability/metrics.cpp` (new file)
- `apps/caf/processor/src/observability/tracing.cpp` (new file)
- `apps/caf/processor/src/observability/observability.h` (new file)

**No Changes Required** (CP1 invariants preserved):
- JSON log format (unchanged)
- Health endpoint (unchanged)
- PII filtering (unchanged)

---

## CP2 Implementation Tasks

### Task CP2.1: Prometheus Metrics Export

**WORKER**: `wrk-obs1`  
**Priority**: High  
**Dependencies**: CP1 observability complete

**Subtasks**:
1. Router: Implement `/metrics` endpoint with routing metrics
2. Gateway: Implement `/metrics` endpoint with HTTP request metrics
3. Worker: Implement `/metrics` endpoint with step execution metrics
4. Validation: Verify metrics export and label cardinality

**Acceptance Criteria**:
- ✅ All components export Prometheus metrics via `/metrics` endpoint
- ✅ Metrics use CP1 correlation fields as labels
- ✅ Label cardinality is acceptable (no high-cardinality explosion)
- ✅ Metrics are validated via `scripts/observability/validate_metrics.sh`

### Task CP2.2: OpenTelemetry Tracing

**WORKER**: `wrk-obs1`  
**Priority**: High  
**Dependencies**: CP2.1 (metrics export)

**Subtasks**:
1. Router: Integrate OTEL SDK and create routing decision spans
2. Gateway: Integrate OTEL SDK and create HTTP request spans
3. Worker: Integrate OTEL SDK and create step execution spans
4. Context Propagation: Implement trace context propagation via NATS/HTTP headers
5. Validation: Verify trace export and span hierarchy

**Acceptance Criteria**:
- ✅ All components create OTEL spans for key operations
- ✅ Trace context is propagated across components (Gateway → Router → Worker)
- ✅ Spans include CP1 correlation fields as attributes
- ✅ Traces are exported to OTEL collector
- ✅ Traces are validated via `scripts/observability/validate_traces.sh`

### Task CP2.3: Grafana Dashboards

**WORKER**: `wrk-obs1`  
**Priority**: Medium  
**Dependencies**: CP2.1 (metrics export), CP2.2 (tracing)

**Subtasks**:
1. Request Dashboard: Create dashboard for HTTP request metrics
2. Routing Dashboard: Create dashboard for routing decision metrics
3. Worker Execution Dashboard: Create dashboard for step execution metrics
4. Error Dashboard: Create dashboard for error metrics
5. Health Dashboard: Create dashboard for health check metrics
6. Tracing Dashboard: Create dashboard for distributed traces

**Acceptance Criteria**:
- ✅ All dashboards are created and documented
- ✅ Dashboards use CP1 correlation fields as filters
- ✅ Dashboards are validated via `scripts/observability/validate_dashboards.sh`

### Task CP2.4: Alerting Rules

**WORKER**: `wrk-obs1`  
**Priority**: Medium  
**Dependencies**: CP2.1 (metrics export)

**Subtasks**:
1. Error Rate Alerts: Create alerts for high error rates
2. Latency Alerts: Create alerts for high latency (p95, p99)
3. Health Check Alerts: Create alerts for unhealthy components
4. Validation: Verify alert rules and notification channels

**Acceptance Criteria**:
- ✅ Alert rules are created and documented
- ✅ Alerts use CP1 correlation fields for filtering
- ✅ Alert rules are validated via `scripts/observability/validate_alerts.sh`

---

## CP1 Invariant Preservation

### Backward Compatibility

**CRITICAL**: All CP2 changes must preserve CP1 invariants:

1. **JSON Log Format**: Unchanged
   - CP1 required fields remain required
   - CP1 correlation fields remain optional (when context available)
   - No breaking changes to log structure

2. **Health Endpoints**: Unchanged
   - All CP1 health endpoints remain functional
   - Response format unchanged
   - No breaking changes to health check protocol

3. **PII Filtering**: Unchanged
   - All CP1 PII filtering rules remain active
   - No new sensitive fields exposed

### CP2 Extensions (Non-Breaking)

**CP2 adds** (does not modify CP1):
- Prometheus `/metrics` endpoints (new endpoints, separate ports)
- OpenTelemetry trace export (new feature, does not affect logs)
- Grafana dashboards (external tooling, does not affect components)
- Alerting rules (external tooling, does not affect components)

**CP2 enhances** (backward compatible):
- Logs may include `span_id` field (optional, for trace correlation)
- Trace context propagation (does not affect log format)

---

## Validation and Testing

### CP2 Validation Scripts

**Required Scripts** (to be created in CP2):

1. `scripts/observability/validate_metrics.sh`:
   - Verify Prometheus metrics export
   - Check label cardinality
   - Validate metric names and types

2. `scripts/observability/validate_traces.sh`:
   - Verify OTEL trace export
   - Check span hierarchy
   - Validate trace context propagation

3. `scripts/observability/validate_dashboards.sh`:
   - Verify Grafana dashboard configuration
   - Check dashboard queries
   - Validate dashboard filters

4. `scripts/observability/validate_alerts.sh`:
   - Verify Alertmanager alert rules
   - Check alert conditions
   - Validate notification channels

### CP2 Testing Requirements

**Integration Tests**:
- Verify metrics export from all components
- Verify trace export from all components
- Verify trace context propagation (Gateway → Router → Worker)
- Verify dashboard queries return expected data

**E2E Tests**:
- Verify end-to-end trace (Gateway request → Router decision → Worker execution)
- Verify metrics aggregation across components
- Verify alert firing conditions

---

## Documentation Updates

### CP2 Documentation

**Required Documentation** (to be created in CP2):

1. `docs/OBSERVABILITY_CP2_METRICS.md`:
   - Prometheus metrics reference
   - Metric naming conventions
   - Label cardinality guidelines

2. `docs/OBSERVABILITY_CP2_TRACING.md`:
   - OpenTelemetry tracing reference
   - Trace context propagation guide
   - Span naming conventions

3. `docs/OBSERVABILITY_CP2_DASHBOARDS.md`:
   - Grafana dashboard reference
   - Dashboard usage guide
   - Custom dashboard creation guide

4. `docs/OBSERVABILITY_CP2_ALERTS.md`:
   - Alertmanager alert rules reference
   - Alert configuration guide
   - Alert troubleshooting guide

### CP1 Documentation Updates

**Update Existing Documentation**:

1. `docs/OBSERVABILITY_CP1_INVARIANTS.md`:
   - Add note: "CP2 extends these invariants with metrics and tracing"
   - Reference CP2 extension plan

2. `docs/OBSERVABILITY.md`:
   - Add CP2 section with metrics and tracing overview
   - Reference CP2 extension plan

---

## Dependencies and Prerequisites

### CP2 Prerequisites

**Must be complete before CP2 observability work**:
- ✅ CP1 observability complete (structured JSON logs, health endpoints)
- ✅ CP1 invariants validated and documented
- ✅ CP1 correlation fields implemented in all components

### CP2 Dependencies

**External Dependencies** (to be set up in CP2):
- Prometheus (metrics collection)
- OpenTelemetry Collector (trace collection)
- Grafana (dashboards)
- Alertmanager (alerting)
- Tempo or Jaeger (trace storage, optional)

**Internal Dependencies**:
- CP1 observability stubs (Router: `router_observability_stub.erl`)
- CP1 correlation fields (all components)
- CP1 health endpoints (all components)

---

## Risk Mitigation

### High Cardinality Risk

**Risk**: High-cardinality labels (`run_id`, `step_id`, `flow_id`) may cause Prometheus cardinality explosion.

**Mitigation**:
- Use high-cardinality labels only in detailed metrics (e.g., `worker_step_executions_total`)
- Exclude from aggregate metrics (e.g., `component_error_rate`)
- Monitor label cardinality via `prometheus_tsdb_head_series` metric
- Set label cardinality limits in Prometheus configuration

### Trace Volume Risk

**Risk**: High trace volume may overwhelm OTEL collector and trace storage.

**Mitigation**:
- Implement sampling (e.g., 10% of traces)
- Use head-based sampling for high-volume traces
- Set trace retention limits
- Monitor trace export rate

### Performance Impact Risk

**Risk**: Metrics and tracing may impact component performance.

**Mitigation**:
- Use async metric/trace export (non-blocking)
- Benchmark performance impact before/after CP2
- Set performance budgets (e.g., <5% latency increase)
- Monitor component performance metrics

---

## Summary

### CP2 Observability Extension

**CP2 extends CP1 observability with**:
1. **Prometheus Metrics**: Export metrics via `/metrics` endpoints
2. **OpenTelemetry Tracing**: Distributed tracing with context propagation
3. **Grafana Dashboards**: Visualization of metrics and traces
4. **Alerting Rules**: Production monitoring and alerting

**Key Principles**:
- ✅ Build upon CP1 invariants (no breaking changes)
- ✅ Use CP1 correlation fields as labels/attributes
- ✅ Preserve backward compatibility
- ✅ Maintain CP1 log format and health endpoints

**WORKER**: `wrk-obs1` (Observability CP2)  
**Control Point**: CP2-LC  
**Status**: 📋 **PLANNING DOCUMENT** (implementation in CP2)

---

## References

- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/OBSERVABILITY.md` - General observability documentation
- `apps/otp/router/docs/OBSERVABILITY_CP2_PLANNING.md` - Router-specific CP2 planning
- `.cursor/rules/agents/wrk-10-observability-telemetry.mdc` - WORKER wrk-10 rules (CP1)
- `.cursor/rules/agents/wrk-obs1-observability-cp2.mdc` - WORKER wrk-obs1 rules (CP2, to be created)

---

**Document Status**: 📋 **PLANNING** (CP2)  
**Last Updated**: 2025-01-27  
**Next Steps**: CP2 implementation by `wrk-obs1`

