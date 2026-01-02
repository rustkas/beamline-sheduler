# CP2 Observability Wave 1

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-obs1 (Observability CP2)  
**Status**: 📋 **WAVE 1 SPECIFICATION** (CP2)

---

## Executive Summary

This document formally defines **Wave 1** for CP2 Observability, focusing on **Prometheus metrics export** as the minimal but useful foundation for observability.

**Wave 1 Goal**: Enable Prometheus metrics export for all components (Router, Gateway, Worker) with CP1 correlation fields as labels, providing essential monitoring capabilities without complicating deployment or breaking CP1 contracts.

**Reference**: `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - Complete backlog with all tickets and priorities

**Dry-Run Plan**: `docs/archive/dev/CP2_OBSERVABILITY_WAVE1_DRYRUN.md` - Dry-run scenario for Wave 1 (Prometheus infrastructure and metrics validation, no code changes)

---

## Wave 1 Scope

### Tickets Included

**Wave 1 consists of 7 tickets (all P1 Critical, CP2.1: Prometheus Metrics Export)**:

| Ticket ID | Title | Estimate | Priority | Business Risk |
|-----------|-------|----------|----------|---------------|
| **O1-1.1** | Router Metrics Collection | 1d | P1 | Cannot monitor routing performance, identify bottlenecks, or track provider selection patterns |
| **O1-1.2** | Router Metrics Endpoint | 1d | P1 | Metrics cannot be scraped by Prometheus without `/metrics` endpoint |
| **O1-1.3** | Gateway Metrics Collection | 1d | P1 | Cannot monitor HTTP request performance, rate limiting effectiveness, or idempotency cache hits |
| **O1-1.4** | Gateway Metrics Endpoint | 1d | P1 | Metrics cannot be scraped by Prometheus without `/metrics` endpoint |
| **O1-1.5** | Worker Metrics Collection | 1d | P1 | Cannot monitor step execution performance, identify slow steps, or track queue depth |
| **O1-1.6** | Worker Metrics Endpoint | 1d | P1 | Metrics cannot be scraped by Prometheus without `/metrics` endpoint |
| **O1-1.7** | Metrics Validation Script | 1d | P1 | Cannot ensure metrics are correctly exported and scraped by Prometheus |

**Total Estimate**: ~7 days (~1.5 weeks)

### Feature Flag

**Wave 1 introduces 1 feature flag**:

- **`CP2_OBSERVABILITY_METRICS_ENABLED`** (default: `false`)
  - Gates: All Prometheus metrics collection and export
  - Tickets: O1-1.1 through O1-1.7

**CRITICAL**: Feature flag defaults to `false` to preserve CP1 baseline behavior.

---

## Wave 1 Goal

### What Changes for Users

**Before Wave 1 (CP1)**:
- ❌ No metrics export - cannot monitor system performance via Prometheus
- ❌ No quantitative performance data - only logs and health endpoints
- ❌ No request rate tracking - cannot see request throughput
- ❌ No latency percentiles - cannot see p50/p95/p99 latency
- ❌ No error rate tracking - cannot see error rates over time
- ❌ No provider selection tracking - cannot see routing decisions

**After Wave 1 (CP2)**:
- ✅ Prometheus metrics export for all components (Router, Gateway, Worker)
- ✅ Request rate metrics (`http_requests_total`, `router_decisions_total`, `worker_step_executions_total`)
- ✅ Latency metrics (`http_request_duration_seconds`, `router_decision_duration_seconds`, `worker_step_execution_duration_seconds`)
- ✅ Error rate metrics (`component_errors_total`, `worker_step_errors_total`)
- ✅ Provider selection metrics (`router_provider_selections_total`)
- ✅ Queue depth metrics (`worker_queue_depth`)
- ✅ CP1 correlation fields as labels (`tenant_id`, `run_id`, `flow_id`, `step_id`)

### What Changes for System

**Observability Improvements**:
- **Quantitative Monitoring**: Metrics enable quantitative monitoring of system performance
- **Performance Tracking**: Latency percentiles enable performance tracking and optimization
- **Error Tracking**: Error rate metrics enable proactive error detection
- **Routing Insights**: Provider selection metrics enable routing decision analysis
- **Resource Monitoring**: Queue depth metrics enable resource utilization monitoring

**Operational Improvements**:
- **Prometheus Integration**: Components can be scraped by Prometheus
- **Grafana Dashboards**: Metrics can be visualized in Grafana (Wave 2)
- **Alerting**: Metrics can be used for alerting (Wave 2)
- **Multi-Tenant Monitoring**: CP1 correlation fields enable tenant-level monitoring

**Deployment Improvements**:
- **No Breaking Changes**: All changes are additive (new endpoints, separate ports)
- **Feature Flag Protection**: CP1 baseline preserved when feature flag disabled
- **Backward Compatible**: CP1 logs and health endpoints remain unchanged

---

## Main Risks

### Risk 1: Label Cardinality Explosion (Critical)

**Current State (CP1)**:
- No metrics, so no cardinality risk
- CP1 correlation fields (`run_id`, `step_id`, `flow_id`) have high cardinality

**Wave 1 Mitigation**:
- ✅ Use high-cardinality labels (`run_id`, `step_id`, `flow_id`) only in detailed metrics
- ✅ Exclude high-cardinality labels from aggregate metrics
- ✅ Monitor label cardinality via validation script
- ✅ Set label cardinality limits in Prometheus configuration

**Business Impact**: **CRITICAL** - High cardinality can cause Prometheus performance issues and memory exhaustion.

### Risk 2: Feature Flag Regression (High)

**Current State (CP1)**:
- CP1 baseline must be preserved
- Feature flags must work correctly

**Wave 1 Mitigation**:
- ✅ All CP2 features gated behind feature flag
- ✅ Feature flag defaults to `false` (CP1 baseline)
- ✅ Comprehensive tests for both CP1 and CP2 modes

**Business Impact**: **HIGH** - Feature flag regression can break existing deployments.

### Risk 3: Port Conflicts (Medium)

**Current State (CP1)**:
- Router: Port 9000 (gRPC health)
- Gateway: Port 3000 (HTTP API)
- Worker: Port 9091 (HTTP health)

**Wave 1 Mitigation**:
- ✅ Metrics endpoints on separate ports (Router: 9001, Gateway: 3001, Worker: 9092)
- ✅ No conflicts with CP1 ports
- ✅ Port configuration documented

**Business Impact**: **MEDIUM** - Port conflicts can prevent metrics export.

### Risk 4: Performance Impact (Medium)

**Current State (CP1)**:
- No metrics collection overhead
- Components have predictable performance

**Wave 1 Mitigation**:
- ✅ Async metrics collection (non-blocking)
- ✅ Performance benchmarks before/after
- ✅ Performance budget: <5% latency increase

**Business Impact**: **MEDIUM** - Performance impact can degrade user experience.

---

## Dependencies

### Dependencies on Router

**Required Changes**: **NONE** (all changes are internal to Router)

**Rationale**:
- Metrics collection is internal to Router
- Metrics endpoint is on separate port (9001, not 9000)
- No changes to Router contracts or APIs
- CP1 health endpoint (port 9000) remains unchanged

### Dependencies on Gateway

**Required Changes**: **NONE** (all changes are internal to Gateway)

**Rationale**:
- Metrics collection is internal to Gateway
- Metrics endpoint is on separate port (3001, not 3000)
- No changes to Gateway contracts or APIs
- CP1 health endpoint (port 3000) remains unchanged

### Dependencies on Worker

**Required Changes**: **NONE** (all changes are internal to Worker)

**Rationale**:
- Metrics collection is internal to Worker
- Metrics endpoint is on separate port (9092, not 9091)
- No changes to Worker contracts or APIs
- CP1 health endpoint (port 9091) remains unchanged

### Dependencies on Infrastructure

**Required Infrastructure**:

1. **Prometheus Server**:
   - Prometheus must be available for metrics scraping
   - Prometheus configuration must include scrape targets (Router: 9001, Gateway: 3001, Worker: 9092)
   - **Wave 1**: Prometheus can be local (Docker Compose) or external
   - **Reference**: `tools/observability/prometheus.yml` - Prometheus configuration template

2. **Configuration**:
   - Feature flags can be set via environment variables or config files
   - **Wave 1**: Basic configuration support required

3. **Monitoring** (Optional for Wave 1):
   - Grafana for visualization (Wave 2)
   - Alertmanager for alerting (Wave 2)

**Infrastructure Changes Required**: **NONE** (Prometheus can be set up independently)

### Dependencies on Other Components

**Required Changes**: **NONE**

**Rationale**:
- Wave 1 changes are internal to each component
- All changes are backward compatible (feature flags default to `false`)
- No breaking changes to contracts or APIs

---

## Metrics Endpoints

### Component Endpoints

**Wave 1 adds HTTP `/metrics` endpoints on separate ports**:

| Component | Protocol | Path | Port | CP1 Port | Purpose |
|-----------|----------|------|------|----------|---------|
| Router | HTTP | `GET /metrics` | 9001 | 9000 (gRPC health) | Prometheus metrics export |
| Gateway | HTTP | `GET /metrics` | 3001 | 3000 (HTTP API) | Prometheus metrics export |
| Worker | HTTP | `GET /metrics` | 9092 | 9091 (HTTP health) | Prometheus metrics export |

**Key Principle**: CP2 metrics endpoints are **separate** from CP1 health endpoints. No conflicts.

### Metrics Format

**Prometheus Exposition Format** (standard):
```
# HELP http_requests_total Total HTTP requests
# TYPE http_requests_total counter
http_requests_total{component="gateway",method="POST",path="/api/v1/messages",status_code="200",tenant_id="tenant_123"} 42

# HELP http_request_duration_seconds HTTP request duration
# TYPE http_request_duration_seconds histogram
http_request_duration_seconds_bucket{component="gateway",method="POST",path="/api/v1/messages",le="0.1"} 10
http_request_duration_seconds_bucket{component="gateway",method="POST",path="/api/v1/messages",le="0.5"} 35
http_request_duration_seconds_bucket{component="gateway",method="POST",path="/api/v1/messages",le="1.0"} 40
http_request_duration_seconds_bucket{component="gateway",method="POST",path="/api/v1/messages",le="+Inf"} 42
http_request_duration_seconds_sum{component="gateway",method="POST",path="/api/v1/messages"} 8.5
http_request_duration_seconds_count{component="gateway",method="POST",path="/api/v1/messages"} 42
```

---

## Core Metrics (Wave 1)

### Router Metrics

**Routing Decision Metrics**:
- `router_decisions_total{policy_id, provider, decision_reason, tenant_id, run_id}` (Counter)
- `router_decision_duration_seconds{policy_id, provider, tenant_id, run_id}` (Histogram)
- `router_provider_selections_total{provider, policy_id, tenant_id, run_id}` (Counter)
- `router_policy_evaluation_duration_seconds{policy_id, tenant_id, run_id}` (Histogram)

**Error Metrics**:
- `router_errors_total{error_code, tenant_id, run_id}` (Counter)

**Health Metrics**:
- `router_health_status{check}` (Gauge) - 1 = healthy, 0 = unhealthy

### Gateway Metrics

**HTTP Request Metrics**:
- `gateway_http_requests_total{method, route, status_code, tenant_id}` (Counter)
- `gateway_http_request_duration_seconds{method, route, status_code, tenant_id}` (Histogram)
- `gateway_http_request_size_bytes{method, route, tenant_id}` (Histogram)
- `gateway_http_response_size_bytes{method, route, status_code, tenant_id}` (Histogram)

**Rate Limiting Metrics**:
- `gateway_rate_limit_hits_total{tenant_id, endpoint}` (Counter)
- `gateway_rate_limit_misses_total{tenant_id, endpoint}` (Counter)

**Idempotency Metrics**:
- `gateway_idempotency_hits_total` (Counter)
- `gateway_idempotency_misses_total` (Counter)

**Connection Metrics**:
- `gateway_active_connections` (Gauge)

### Worker Metrics

**Step Execution Metrics**:
- `worker_step_executions_total{step_type, execution_status, tenant_id, run_id, flow_id, step_id}` (Counter)
- `worker_step_execution_duration_seconds{step_type, execution_status, tenant_id, run_id, flow_id, step_id}` (Histogram)
- `worker_step_errors_total{step_type, error_code, tenant_id, run_id, flow_id, step_id}` (Counter)

**Flow Execution Metrics**:
- `worker_flow_execution_duration_seconds{tenant_id, run_id, flow_id}` (Histogram)

**Queue Metrics**:
- `worker_queue_depth{resource_pool}` (Gauge)
- `worker_active_tasks{resource_pool}` (Gauge)

**Health Metrics**:
- `worker_health_status{check}` (Gauge) - 1 = healthy, 0 = unhealthy

### Cross-Component Metrics

**Error Metrics**:
- `component_errors_total{component, error_code, tenant_id, run_id}` (Counter)

**Health Metrics**:
- `component_health_status{component, check}` (Gauge) - 1 = healthy, 0 = unhealthy

---

## Dashboards and Visualization (Wave 1)

### Local Grafana Setup

**Wave 1 provides metrics export, but dashboards are deferred to Wave 2**.

**For Wave 1 validation**, developers can use:

1. **Prometheus UI** (localhost:9090):
   - Query metrics directly
   - View metric values and labels
   - Test PromQL queries

2. **Local Grafana** (optional, for Wave 1):
   - Connect to Prometheus data source
   - Create basic dashboards manually
   - **Note**: Official Grafana dashboards are Wave 2

**Reference**: `tools/observability/docker-compose.observability.yml` - Docker Compose setup for local Prometheus and Grafana

### Metrics Queries (Examples)

**Request Rate**:
```promql
rate(http_requests_total[5m])
```

**Error Rate**:
```promql
rate(component_errors_total[5m])
```

**Latency (p95)**:
```promql
histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m]))
```

**Provider Selection Distribution**:
```promql
sum(rate(router_provider_selections_total[5m])) by (provider)
```

---

## Trace Path (Wave 1)

### Trace Path Status

**Wave 1**: **No distributed tracing** (deferred to Wave 2)

**Rationale**:
- Prometheus metrics provide immediate value without tracing complexity
- Tracing requires OTEL SDK integration and context propagation
- Tracing can be added in Wave 2 after metrics foundation is established

**Wave 2 will add**:
- OpenTelemetry distributed tracing
- Trace context propagation (Gateway → Router → Worker)
- Trace visualization in Grafana/Tempo

---

## Systems Consuming Wave 1 Data

### Prometheus (Required)

**Purpose**: Metrics collection and storage

**Configuration**:
- Scrape targets: Router (9001), Gateway (3001), Worker (9092)
- Scrape interval: 15s (default)
- Storage: Local TSDB or remote storage

**Reference**: `tools/observability/prometheus.yml` - Prometheus configuration template

**Deployment**:
- **Local Development**: Docker Compose (`tools/observability/docker-compose.observability.yml`)
- **Production**: Prometheus server with configured scrape targets

### Grafana (Optional for Wave 1, Required for Wave 2)

**Purpose**: Metrics visualization

**Configuration**:
- Data source: Prometheus (localhost:9090 or remote)
- Dashboards: Manual creation for Wave 1, official dashboards in Wave 2

**Deployment**:
- **Local Development**: Docker Compose (`tools/observability/docker-compose.observability.yml`)
- **Production**: Grafana server with Prometheus data source

**Wave 1 Status**: Metrics can be visualized, but official dashboards are Wave 2

### Alertmanager (Wave 2)

**Purpose**: Alerting based on metrics

**Wave 1 Status**: Metrics available, but alerting rules are Wave 2

---

## Implementation Order

### Recommended Sequence

**Week 1: Metrics Collection and Export**

1. **Day 1**: O1-1.1 (Router Metrics Collection) - Foundation for Router metrics
2. **Day 2**: O1-1.2 (Router Metrics Endpoint) - Router metrics export
3. **Day 3**: O1-1.3 (Gateway Metrics Collection) - Foundation for Gateway metrics
4. **Day 4**: O1-1.4 (Gateway Metrics Endpoint) - Gateway metrics export
5. **Day 5**: O1-1.5 (Worker Metrics Collection) - Foundation for Worker metrics
6. **Day 6**: O1-1.6 (Worker Metrics Endpoint) - Worker metrics export
7. **Day 7**: O1-1.7 (Metrics Validation Script) - Validation and testing

**Total**: ~7 days (~1.5 weeks)

---

## Acceptance Criteria

### Functional Requirements

**Metrics Export**:
- ✅ Router exposes `/metrics` endpoint on port 9001
- ✅ Gateway exposes `/metrics` endpoint on port 3001
- ✅ Worker exposes `/metrics` endpoint on port 9092
- ✅ All endpoints return Prometheus text format
- ✅ All required metrics are exported correctly
- ✅ CP1 correlation fields used as labels (`tenant_id`, `run_id`, `flow_id`, `step_id`)
- ✅ Feature flag gates CP2 behavior (`CP2_OBSERVABILITY_METRICS_ENABLED`)

**Metrics Collection**:
- ✅ Router metrics collection implemented (routing decisions, policy evaluation, provider selection, errors)
- ✅ Gateway metrics collection implemented (HTTP requests, request duration, rate limiting, idempotency)
- ✅ Worker metrics collection implemented (step executions, execution duration, errors, queue depth)

**Validation**:
- ✅ Metrics validation script works (`scripts/observability/validate_metrics.sh`)
- ✅ Script validates Prometheus format compliance
- ✅ Script verifies CP1 correlation fields in labels
- ✅ Script checks label cardinality limits

### Non-Functional Requirements

**Performance**:
- ✅ Metrics collection overhead < 5% CPU
- ✅ Metrics endpoint response time < 100ms
- ✅ No performance regression when feature flag disabled

**Compatibility**:
- ✅ CP1 baseline preserved when feature flag disabled
- ✅ CP1 logs and health endpoints unchanged
- ✅ No breaking changes to contracts or APIs

**Observability**:
- ✅ Metrics use CP1 correlation fields as labels
- ✅ Label cardinality is acceptable (no high-cardinality explosion)
- ✅ Metrics are accessible via Prometheus scraping

### Test Coverage

**Unit Tests**:
- ✅ Metrics collection unit tests (all components)
- ✅ Metrics endpoint unit tests (all components)
- ✅ Metrics formatter unit tests

**Integration Tests**:
- ✅ Metrics endpoint integration tests (all components)
- ✅ Prometheus scraping integration tests
- ✅ Feature flag toggle tests

**Validation Tests**:
- ✅ Metrics validation script tests
- ✅ Label cardinality validation tests
- ✅ CP1 correlation fields validation tests

---

## Success Metrics

### Before Wave 1 (CP1 Baseline)

- **Metrics Export**: ❌ None
- **Performance Monitoring**: ⚠️ Limited (logs only)
- **Quantitative Data**: ❌ None
- **Prometheus Integration**: ❌ None

### After Wave 1 (CP2)

- **Metrics Export**: ✅ All components export Prometheus metrics
- **Performance Monitoring**: ✅ Quantitative metrics (request rate, latency, error rate)
- **Quantitative Data**: ✅ Request rates, latencies, error rates, queue depths
- **Prometheus Integration**: ✅ All components scraped by Prometheus

### Key Metrics

| Metric | CP1 Baseline | Wave 1 Target | Measurement |
|--------|--------------|---------------|-------------|
| **Metrics Export** | ❌ None | ✅ All components | `/metrics` endpoints accessible |
| **Request Rate Tracking** | ❌ None | ✅ All components | `http_requests_total`, `router_decisions_total`, `worker_step_executions_total` |
| **Latency Tracking** | ⚠️ Logs only | ✅ Histograms | `http_request_duration_seconds`, `router_decision_duration_seconds`, `worker_step_execution_duration_seconds` |
| **Error Rate Tracking** | ⚠️ Logs only | ✅ Counters | `component_errors_total`, `worker_step_errors_total` |
| **Prometheus Integration** | ❌ None | ✅ Complete | Prometheus scraping all components |

---

## Rollout Strategy

### Phase 1: Feature Flag Enablement (Week 1)

**Goal**: Enable feature flag in development/staging environment.

**Steps**:
1. Deploy components with Wave 1 code (feature flag disabled by default)
2. Enable feature flag in staging: `CP2_OBSERVABILITY_METRICS_ENABLED=true`
3. Run integration tests
4. Monitor metrics export (Prometheus scraping)
5. Validate metrics format and label cardinality

**Success Criteria**:
- ✅ All tests pass
- ✅ No crashes or errors
- ✅ Metrics exported correctly
- ✅ Prometheus scraping successful

### Phase 2: Production Rollout (Week 2)

**Goal**: Enable feature flag in production with gradual rollout.

**Steps**:
1. Enable feature flag for 10% of traffic (canary deployment)
2. Monitor metrics export for 24 hours
3. If successful, increase to 50% of traffic
4. Monitor metrics export for 24 hours
5. If successful, enable for 100% of traffic

**Success Criteria**:
- ✅ No increase in error rate
- ✅ Metrics exported correctly
- ✅ Prometheus scraping successful
- ✅ No performance regression

### Phase 3: Validation (Week 3)

**Goal**: Validate Wave 1 success and prepare for Wave 2.

**Steps**:
1. Collect metrics for 1 week
2. Validate success metrics (metrics export, Prometheus integration)
3. Document lessons learned
4. Prepare for Wave 2 implementation (tracing)

**Success Criteria**:
- ✅ All success metrics met
- ✅ No production incidents
- ✅ Ready for Wave 2

---

## Risks and Mitigations

### Risk 1: Label Cardinality Explosion

**Risk**: High-cardinality labels (`run_id`, `step_id`, `flow_id`) can cause Prometheus performance issues.

**Mitigation**:
- Use high-cardinality labels only in detailed metrics
- Exclude from aggregate metrics
- Monitor label cardinality via validation script
- Set label cardinality limits in Prometheus configuration

### Risk 2: Feature Flag Regression

**Risk**: Feature flag doesn't work correctly, CP1 baseline broken.

**Mitigation**:
- Comprehensive tests for both CP1 and CP2 modes
- Feature flag defaults to `false` (CP1 baseline)
- Can disable feature flags if issues found

### Risk 3: Port Conflicts

**Risk**: Metrics endpoint ports conflict with existing services.

**Mitigation**:
- Metrics endpoints on separate ports (Router: 9001, Gateway: 3001, Worker: 9092)
- No conflicts with CP1 ports
- Port configuration documented

### Risk 4: Performance Impact

**Risk**: Metrics collection adds overhead, performance degrades.

**Mitigation**:
- Async metrics collection (non-blocking)
- Performance benchmarks before/after
- Performance budget: <5% latency increase
- Can disable feature flags if performance issues

---

## References

### Primary Documents
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - Complete backlog with all tickets and priorities
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - Detailed CP2 extension plan
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants

### Implementation Files
- `apps/otp/router/src/router_metrics.erl` - Router metrics implementation
- `apps/c-gateway/src/metrics/prometheus.c` - Gateway metrics implementation
- `apps/caf/processor/src/observability/metrics.cpp` - Worker metrics implementation
- `scripts/observability/validate_metrics.sh` - Metrics validation script

### Infrastructure
- `tools/observability/prometheus.yml` - Prometheus configuration template
- `tools/observability/docker-compose.observability.yml` - Docker Compose setup for local Prometheus and Grafana

### Specifications
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - Prometheus metrics specification
- `docs/OBSERVABILITY_COMPATIBILITY_RULES.md` - CP1↔CP2 compatibility rules

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Observability Wave 1 specification
- Wave 1 tickets defined (7 tickets, P1 Critical)
- Goal, risks, and dependencies documented
- Implementation order and acceptance criteria specified

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Wave 1 Specification Ready for Implementation

