# CP2 Observability Plan

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-obs1 (Observability CP2)  
**Status**: 🚦 **Pre‑release** (CP2)

---

## Executive Summary

This document defines the **high-level CP2 Observability Plan** for wrk-obs1, extending CP1 observability (structured JSON logs and health endpoints) with **Prometheus metrics**, **OpenTelemetry distributed tracing**, and **Grafana dashboards**.

**Key Principle**: CP2 observability features must build upon and extend CP1 invariants without breaking existing CP1 contracts. All CP2 features must use CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`) as labels/dimensions.

**Foundation**: CP1 observability provides structured JSON logging, CP1 correlation fields, health endpoints, and PII filtering. CP2 extends this with metrics, tracing, and dashboards.

**Reference**: 
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/CP1_CORE_PROFILE_OBSERVABILITY.md` - CP1 observability core profile
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - Detailed CP2 extension plan

---

## CP1 Foundation (Complete)

### CP1 Observability Features

**Structured JSON Logging**:
- ✅ Required fields: `timestamp`, `level`, `component`, `message`
- ✅ CP1 correlation fields: `tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id` (when context available)
- ✅ Context object for component-specific data
- ✅ ISO 8601 timestamps with microseconds precision

**Health Endpoints**:
- ✅ Router: gRPC health on port 9000
- ✅ Gateway: HTTP `/_health` on port 3000
- ✅ Worker: HTTP `/_health` on port 9091

**PII/Secret Filtering**:
- ✅ Automatic filtering of sensitive fields
- ✅ Replacement with `[REDACTED]`

**Validation**:
- ✅ `scripts/observability/validate_observability.sh` - General validation
- ✅ `scripts/observability/validate_observability_e2e.sh` - E2E validation
- ✅ `scripts/observability/run_cp1_profile.sh` - Unified CP1 test profile

---

## CP2 Observability Tasks

### Task CP2.1: Prometheus Metrics Export

**Worker**: wrk-obs1  
**Priority**: High  
**Estimate**: 5 days

**Objective**: Export Prometheus metrics via HTTP `/metrics` endpoint for all components (Router, Gateway, Worker).

**Subtasks**:

1. **Router Metrics Export**:
   - Implement Prometheus metrics collection
   - Expose `/metrics` endpoint (HTTP, port 9001)
   - Metrics: Request rate, routing decisions, policy evaluation, errors

2. **Gateway Metrics Export**:
   - Implement Prometheus metrics collection
   - Expose `/metrics` endpoint (HTTP, port 3001)
   - Metrics: HTTP request rate, request duration, rate limiting, idempotency

3. **Worker Metrics Export**:
   - Implement Prometheus metrics collection
   - Expose `/metrics` endpoint (HTTP, port 9092)
   - Metrics: Step execution rate, execution duration, errors, queue depth

4. **CP1 Field Integration**:
   - Use CP1 correlation fields as Prometheus labels
   - Labels: `tenant_id`, `run_id`, `flow_id`, `step_id` (when available)
   - Label cardinality management (avoid high-cardinality explosion)

**Metric Categories**:

**Request Metrics**:
- `http_requests_total{component, method, path, status_code, tenant_id}` (Counter)
- `http_request_duration_seconds{component, method, path, status_code, tenant_id}` (Histogram)

**Routing Metrics**:
- `router_decisions_total{policy_id, provider, decision_reason, tenant_id, run_id}` (Counter)
- `router_decision_duration_seconds{policy_id, provider, tenant_id, run_id}` (Histogram)

**Worker Execution Metrics**:
- `worker_step_executions_total{step_type, execution_status, tenant_id, run_id, flow_id, step_id}` (Counter)
- `worker_step_execution_duration_seconds{step_type, execution_status, tenant_id, run_id, flow_id, step_id}` (Histogram)

**Error Metrics**:
- `component_errors_total{component, error_code, tenant_id, run_id}` (Counter)

**Health Metrics**:
- `component_health_status{component, check}` (Gauge)

**Artifacts**:
- `apps/otp/router/src/router_metrics.erl` - Router metrics implementation
- `apps/c-gateway/src/metrics/prometheus.c` - Gateway metrics implementation
- `apps/caf/processor/src/observability/metrics.cpp` - Worker metrics implementation
- `scripts/observability/validate_metrics.sh` - Metrics validation script
- `docs/archive/dev/METRICS_IMPLEMENTATION_REPORT.md` - Implementation report

**Acceptance Criteria**:
- ✅ All components expose `/metrics` endpoint
- ✅ Metrics use CP1 correlation fields as labels
- ✅ Label cardinality is acceptable (no high-cardinality explosion)
- ✅ Metrics are validated via `scripts/observability/validate_metrics.sh`
- ✅ Unit tests for metrics collection
- ✅ Integration tests for metrics export

**Reference**: `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md#mapping-cp1-logs--prometheus-metrics`

---

### Task CP2.2: OpenTelemetry Distributed Tracing

**Worker**: wrk-obs1  
**Priority**: High  
**Estimate**: 6 days

**Objective**: Implement OpenTelemetry distributed tracing with context propagation across components (Gateway → Router → Worker).

**Subtasks**:

1. **OTEL SDK Integration**:
   - Router: Integrate OTEL SDK (Erlang/OTP)
   - Gateway: Integrate OTEL SDK (C)
   - Worker: Integrate OTEL SDK (C++)

2. **Span Creation**:
   - Router: Create spans for routing decisions, policy evaluation
   - Gateway: Create spans for HTTP requests, rate limiting
   - Worker: Create spans for step execution, block execution

3. **Context Propagation**:
   - Propagate trace context via NATS headers (`traceparent`, `tracestate`)
   - Propagate trace context via HTTP headers (`traceparent`, `tracestate`)
   - W3C Trace Context format support

4. **CP1 Field Integration**:
   - Include CP1 correlation fields as span attributes
   - Attributes: `tenant.id`, `run.id`, `flow.id`, `step.id`, `trace.id`
   - Span links for correlation across components

5. **OTLP Export**:
   - Export traces via OTLP (HTTP/gRPC) to OTEL collector
   - Configurable OTEL collector endpoint
   - Batch export with configurable batch size and timeout

**Span Hierarchy**:

**Gateway → Router → Worker Flow**:
```
HTTP Request (Gateway)
  └─ Routing Decision (Router)
      └─ Step Execution (Worker)
          └─ Block Execution (Worker)
```

**Span Attributes**:
- `service.name`: Component name (router, gateway, worker)
- `tenant.id`: Tenant identifier (from CP1)
- `run.id`: Run identifier (from CP1)
- `flow.id`: Flow identifier (from CP1)
- `step.id`: Step identifier (from CP1)
- `trace.id`: Trace identifier (from CP1)

**Artifacts**:
- `apps/otp/router/src/router_tracing.erl` - Router tracing implementation
- `apps/c-gateway/src/tracing/otel.c` - Gateway tracing implementation
- `apps/caf/processor/src/observability/tracing.cpp` - Worker tracing implementation
- `scripts/observability/validate_traces.sh` - Trace validation script
- `docs/archive/dev/TRACING_IMPLEMENTATION_REPORT.md` - Implementation report

**Acceptance Criteria**:
- ✅ All components create OTEL spans for key operations
- ✅ Trace context is propagated across components (Gateway → Router → Worker)
- ✅ Spans include CP1 correlation fields as attributes
- ✅ Traces are exported to OTEL collector
- ✅ Traces are validated via `scripts/observability/validate_traces.sh`
- ✅ Unit tests for span creation
- ✅ Integration tests for context propagation

**Reference**: `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md#mapping-cp1-logs--opentelemetry-traces`

---

### Task CP2.3: Grafana Dashboards

**Worker**: wrk-obs1  
**Priority**: Medium  
**Estimate**: 4 days

**Objective**: Create Grafana dashboards for visualization of metrics and traces.

**Subtasks**:

1. **Request Dashboard**:
   - HTTP request rate (requests/sec)
   - HTTP request duration (p50, p95, p99)
   - HTTP error rate (errors/sec)
   - HTTP status code distribution
   - Filters: `component`, `tenant_id`, `method`, `path`

2. **Routing Dashboard**:
   - Routing decision rate (decisions/sec)
   - Routing decision duration (p50, p95, p99)
   - Provider selection distribution
   - Policy evaluation duration
   - Filters: `policy_id`, `provider`, `tenant_id`, `run_id`

3. **Worker Execution Dashboard**:
   - Step execution rate (steps/sec)
   - Step execution duration (p50, p95, p99)
   - Step error rate (errors/sec)
   - Flow execution duration
   - Filters: `step_type`, `execution_status`, `tenant_id`, `run_id`, `flow_id`

4. **Error Dashboard**:
   - Error rate by component
   - Error rate by error code
   - Error rate by tenant
   - Error trends over time
   - Filters: `component`, `error_code`, `tenant_id`

5. **Health Dashboard**:
   - Component health status
   - Health check duration
   - Health check failures
   - Filters: `component`, `check`

6. **Tracing Dashboard**:
   - Trace duration distribution
   - Trace error rate
   - Service dependency graph
   - Trace search and filtering
   - Filters: `service.name`, `tenant.id`, `run.id`, `flow.id`

**CP1 Field Integration**:
- All dashboards use CP1 correlation fields as filters
- Filters: `tenant_id`, `run_id`, `flow_id`, `step_id` (when available)
- Dashboard variables for dynamic filtering

**Artifacts**:
- `docs/observability/router-dashboard-grafana.json` - Router dashboard
- `docs/observability/gateway-dashboard-grafana.json` - Gateway dashboard
- `docs/observability/worker-dashboard-grafana.json` - Worker dashboard
- `docs/observability/error-dashboard-grafana.json` - Error dashboard
- `docs/observability/health-dashboard-grafana.json` - Health dashboard
- `docs/observability/tracing-dashboard-grafana.json` - Tracing dashboard
- `scripts/observability/validate_dashboards.sh` - Dashboard validation script
- `docs/archive/dev/DASHBOARDS_IMPLEMENTATION_REPORT.md` - Implementation report

**Acceptance Criteria**:
- ✅ All dashboards are created and documented
- ✅ Dashboards use CP1 correlation fields as filters
- ✅ Dashboards are validated via `scripts/observability/validate_dashboards.sh`
- ✅ Dashboard JSON files are valid Grafana format
- ✅ Dashboards are tested with real metrics/traces

**Reference**: 
- `apps/otp/router/docs/OBSERVABILITY_DASHBOARD.md` - Router dashboard planning
- `apps/c-gateway/docs/OBSERVABILITY_DASHBOARD.md` - Gateway dashboard planning
- `apps/caf/processor/docs/OBSERVABILITY_DASHBOARD.md` - Worker dashboard planning

---

### Task CP2.4: Alerting Rules

**Worker**: wrk-obs1  
**Priority**: Medium  
**Estimate**: 3 days

**Objective**: Create Alertmanager rules for production monitoring.

**Subtasks**:

1. **Error Rate Alerts**:
   - High error rate alert (error rate > 5% for 5 minutes)
   - Component-specific error alerts
   - Error code-specific alerts

2. **Latency Alerts**:
   - High latency alert (p95 latency > 1s for 5 minutes)
   - Component-specific latency alerts
   - Endpoint-specific latency alerts

3. **Health Check Alerts**:
   - Unhealthy component alert
   - Health check failure alert
   - Health check timeout alert

4. **Resource Alerts**:
   - High queue depth alert (queue depth > 80% capacity)
   - High memory usage alert
   - High CPU usage alert

**Alert Labels**:
- Use CP1 correlation fields as alert labels
- Labels: `tenant_id`, `run_id`, `component`, `error_code`

**Artifacts**:
- `docs/observability/alert-rules.yaml` - Alertmanager rules
- `scripts/observability/validate_alerts.sh` - Alert validation script
- `docs/archive/dev/ALERTING_IMPLEMENTATION_REPORT.md` - Implementation report

**Acceptance Criteria**:
- ✅ Alert rules are created and documented
- ✅ Alert rules use CP1 correlation fields as labels
- ✅ Alert rules are validated via `scripts/observability/validate_alerts.sh`
- ✅ Alert rules are tested with Alertmanager

**Reference**: `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md#alerting-rules`

---

## CP2 Acceptance Criteria

### Functional Requirements

- ✅ Prometheus metrics exported for all components
- ✅ OpenTelemetry tracing implemented with context propagation
- ✅ Grafana dashboards created for all metrics and traces
- ✅ Alertmanager rules created for production monitoring
- ✅ CP1 correlation fields integrated into all CP2 features

### Non-Functional Requirements

- ✅ **Performance**: Metrics collection overhead < 5% CPU
- ✅ **Performance**: Trace export success rate > 99%
- ✅ **Observability**: All CP2 features use CP1 correlation fields
- ✅ **Compatibility**: CP2 features don't break CP1 contracts

### Test Coverage

- ✅ Metrics export tests (all components)
- ✅ Tracing tests (span creation, context propagation)
- ✅ Dashboard tests (JSON validation, filter functionality)
- ✅ Alert tests (rule validation, notification channels)

---

## Dependencies

### Internal

- ✅ CP1 observability completed (structured logs, health endpoints)
- ✅ CP1 correlation fields implemented (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`)
- ✅ Validation scripts working (`validate_observability.sh`, `validate_observability_e2e.sh`)

### External

- Prometheus server for metrics collection
- OpenTelemetry collector for trace collection
- Grafana for dashboard visualization
- Alertmanager for alerting

---

## Risks and Mitigations

### Risk 1: Label Cardinality Explosion

**Risk**: High-cardinality labels (e.g., `step_id` for every step) can cause Prometheus performance issues.

**Mitigation**:
- Limit high-cardinality labels to essential dimensions
- Use label filtering in dashboards
- Monitor Prometheus cardinality metrics

### Risk 2: Trace Export Overhead

**Risk**: Trace export may add significant overhead to request processing.

**Mitigation**:
- Batch trace export with configurable batch size
- Async trace export (non-blocking)
- Sampling for high-volume traces

### Risk 3: Dashboard Maintenance

**Risk**: Dashboards may become outdated as metrics change.

**Mitigation**:
- Dashboard validation scripts
- Dashboard versioning
- Documentation for dashboard updates

---

## References

### CP1 Documentation
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/CP1_CORE_PROFILE_OBSERVABILITY.md` - CP1 observability core profile
- `docs/OBSERVABILITY.md` - General observability requirements

### CP2 Planning Documents
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - Detailed CP2 extension plan
- `apps/otp/router/docs/OBSERVABILITY_CP2_PLANNING.md` - Router CP2 planning
- `apps/c-gateway/docs/OBSERVABILITY_DASHBOARD.md` - Gateway dashboard planning
- `apps/caf/processor/docs/OBSERVABILITY_DASHBOARD.md` - Worker dashboard planning

### Component Documentation
- `apps/otp/router/docs/OBSERVABILITY.md` - Router observability
- `apps/c-gateway/docs/OBSERVABILITY.md` - Gateway observability
- `apps/caf/processor/docs/OBSERVABILITY.md` - Worker observability

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Observability Plan
- Prometheus metrics export specification
- OpenTelemetry distributed tracing specification
- Grafana dashboards specification
- Alerting rules specification

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Planning Document
