# CP2 Observability Test Profile

**Version**: 0.1 (Draft)  
**Last Updated**: 2025-01-27  
**Status**: 🚦 **Pre‑release** (CP2)  
**Purpose**: Define unified CP2 observability test profile extending CP1 tests with metrics, tracing, dashboards, and alerting

## Overview

This document defines the **CP2 Observability Test Profile** - a unified set of tests that validate CP2 observability compliance across all key components (Router, Gateway, Worker). CP2 extends CP1 observability with Prometheus metrics, OpenTelemetry tracing, Grafana dashboards, and Alertmanager rules.

**Key Principle**: CP2 tests **extend** CP1 tests but do **not replace** them. All CP1 core tests remain mandatory, and CP2 tests add validation for CP2-specific features.

**CP1 Foundation**: This profile builds upon `docs/OBSERVABILITY_CP1_TEST_PROFILE.md`. All CP1 core tests must pass before CP2 tests are executed.

---

## Profile Structure

The CP2 test profile consists of:

1. **CP1 Core Tests** - All CP1 tests (mandatory, inherited from CP1 profile)
2. **CP2.1 Tests** - Prometheus Metrics Export validation
3. **CP2.2 Tests** - OpenTelemetry Distributed Tracing validation
4. **CP2.3 Tests** - Grafana Dashboards validation
5. **CP2.4 Tests** - Alerting Rules validation
6. **CP2 E2E Test Scripts** - End-to-end validation for CP2 features

---

## CP1 Foundation (Inherited)

**Reference**: `docs/OBSERVABILITY_CP1_TEST_PROFILE.md`

All CP1 core tests are **mandatory** and must pass before CP2 tests are executed:

- ✅ Structured JSON logging validation
- ✅ CP1 correlation fields validation (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`)
- ✅ Health endpoint validation
- ✅ PII/secret filtering validation
- ✅ Component-specific unit and integration tests

**CP1 Test Profile Script**: `scripts/observability/run_cp1_profile.sh`

**CP1 Test Summary**:
- Router: 18+ tests (unit, integration, E2E)
- Gateway: 21+ tests (unit, integration, E2E)
- Worker: 12+ tests (unit, integration, E2E)
- General: 2 validation scripts

---

## CP2.1: Prometheus Metrics Export

### Overview

CP2.1 adds Prometheus metrics export via HTTP `/metrics` endpoint for all components.

**Reference Documents**:
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - CP2 extension plan
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - Prometheus metrics specification
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - CP2 observability plan

### Router (Erlang/OTP)

**Component**: `apps/otp/router/`

#### Metrics Endpoint Tests

**Test Suite**: `router_metrics_SUITE.erl` (planned)

**Location**: `apps/otp/router/test/router_metrics_SUITE.erl` (planned)

**CP2.1 Tests** (planned):
- `test_metrics_endpoint_available` - Validates `/metrics` endpoint is available (HTTP, port 9001)
- `test_metrics_prometheus_format` - Validates Prometheus text format
- `test_metrics_required_metrics` - Validates required metrics presence
- `test_metrics_cp1_labels` - Validates CP1 correlation fields as labels
- `test_metrics_cardinality_limits` - Validates label cardinality constraints

**Required Metrics** (from specification):
- `router_decisions_total{policy_id, provider, decision_reason, tenant_id, run_id}` (Counter)
- `router_decision_duration_seconds{policy_id, provider, tenant_id, run_id}` (Histogram)
- `router_errors_total{error_code, tenant_id, run_id}` (Counter)
- `router_health_status{check}` (Gauge)

**Run Command** (planned):
```bash
cd apps/otp/router
rebar3 ct --suite test/router_metrics_SUITE
```

#### Integration Tests

**Test Suite**: `router_metrics_integration_SUITE.erl` (planned)

**CP2.1 Tests** (planned):
- `test_metrics_endpoint_http` - Validates HTTP endpoint accessibility
- `test_metrics_endpoint_response_format` - Validates Prometheus text format
- `test_metrics_endpoint_performance` - Validates response time < 100ms
- `test_metrics_collection_overhead` - Validates metrics collection overhead < 5% CPU

---

### Gateway (C-Gateway)

**Component**: `apps/c-gateway/`

#### Metrics Endpoint Tests

**Test Suite**: `test_metrics.c` (planned)

**Location**: `apps/c-gateway/tests/test_metrics.c` (planned)

**CP2.1 Tests** (planned):
- `test_metrics_endpoint_available` - Validates `/metrics` endpoint is available (HTTP, port 3001)
- `test_metrics_prometheus_format` - Validates Prometheus text format
- `test_metrics_required_metrics` - Validates required metrics presence
- `test_metrics_cp1_labels` - Validates CP1 correlation fields as labels

**Required Metrics** (from specification):
- `gateway_http_requests_total{method, route, status_code, tenant_id}` (Counter)
- `gateway_http_request_duration_seconds{method, route, status_code, tenant_id}` (Histogram)
- `gateway_idempotency_hits_total` (Counter)
- `gateway_idempotency_misses_total` (Counter)
- `gateway_rate_limit_hits_total{tenant_id, endpoint}` (Counter)
- `gateway_active_connections` (Gauge)

**Run Command** (planned):
```bash
cd apps/c-gateway
make test-metrics
# Or using CMake:
cd build
make c-gateway-metrics-test
./c-gateway-metrics-test
```

---

### Worker (CAF/C++)

**Component**: `apps/caf/processor/`

#### Metrics Endpoint Tests

**Test Suite**: `test_metrics.cpp` (planned)

**Location**: `apps/caf/processor/tests/test_metrics.cpp` (planned)

**CP2.1 Tests** (planned):
- `test_metrics_endpoint_available()` - Validates `/metrics` endpoint is available (HTTP, port 9092)
- `test_metrics_prometheus_format()` - Validates Prometheus text format
- `test_metrics_required_metrics()` - Validates required metrics presence
- `test_metrics_cp1_labels()` - Validates CP1 correlation fields as labels

**Required Metrics** (from specification):
- `worker_step_executions_total{step_type, execution_status, tenant_id, run_id, flow_id, step_id}` (Counter)
- `worker_step_execution_duration_seconds{step_type, execution_status, tenant_id, run_id, flow_id, step_id}` (Histogram)
- `worker_errors_total{error_code, tenant_id, run_id}` (Counter)
- `worker_queue_depth{queue_name}` (Gauge)

**Run Command** (planned):
```bash
cd apps/caf/processor
mkdir -p build
cd build
cmake ..
make test_metrics
./test_metrics
```

---

## CP2.2: OpenTelemetry Distributed Tracing

### Overview

CP2.2 adds OpenTelemetry distributed tracing with OTLP export for all components.

**Reference Documents**:
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - CP2 extension plan
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - CP2 observability plan

### Router (Erlang/OTP)

**Component**: `apps/otp/router/`

#### Tracing Tests

**Test Suite**: `router_tracing_SUITE.erl` (planned)

**Location**: `apps/otp/router/test/router_tracing_SUITE.erl` (planned)

**CP2.2 Tests** (planned):
- `test_trace_export_otlp` - Validates OTLP trace export
- `test_trace_context_propagation` - Validates W3C Trace Context propagation
- `test_trace_cp1_correlation` - Validates CP1 correlation fields in traces
- `test_trace_span_creation` - Validates span creation for routing decisions
- `test_trace_export_performance` - Validates export latency < 50ms (p95)

**Run Command** (planned):
```bash
cd apps/otp/router
rebar3 ct --suite test/router_tracing_SUITE
```

---

### Gateway (C-Gateway)

**Component**: `apps/c-gateway/`

#### Tracing Tests

**Test Suite**: `test_tracing.c` (planned)

**Location**: `apps/c-gateway/tests/test_tracing.c` (planned)

**CP2.2 Tests** (planned):
- `test_trace_export_otlp` - Validates OTLP trace export
- `test_trace_context_propagation` - Validates W3C Trace Context propagation
- `test_trace_cp1_correlation` - Validates CP1 correlation fields in traces
- `test_trace_auto_instrumentation` - Validates HTTP request auto-instrumentation

**Run Command** (planned):
```bash
cd apps/c-gateway
make test-tracing
```

---

### Worker (CAF/C++)

**Component**: `apps/caf/processor/`

#### Tracing Tests

**Test Suite**: `test_tracing.cpp` (planned)

**Location**: `apps/caf/processor/tests/test_tracing.cpp` (planned)

**CP2.2 Tests** (planned):
- `test_trace_export_otlp()` - Validates OTLP trace export
- `test_trace_context_propagation()` - Validates W3C Trace Context propagation
- `test_trace_cp1_correlation()` - Validates CP1 correlation fields in traces
- `test_trace_span_creation()` - Validates span creation for step execution

**Run Command** (planned):
```bash
cd apps/caf/processor
mkdir -p build
cd build
cmake ..
make test_tracing
./test_tracing
```

---

## CP2.3: Grafana Dashboards

### Overview

CP2.3 adds Grafana dashboards for observability visualization.

**Reference Documents**:
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - CP2 extension plan
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - CP2 observability plan

### Dashboard Validation Tests

**Test Suite**: `test_dashboards.sh` (planned)

**Location**: `scripts/observability/test_dashboards.sh` (planned)

**CP2.3 Tests** (planned):
- `test_dashboard_configuration` - Validates dashboard JSON configuration
- `test_dashboard_queries` - Validates Prometheus queries in dashboards
- `test_dashboard_variables` - Validates dashboard variables
- `test_dashboard_rendering` - Validates dashboard rendering (if Grafana API available)

**Run Command** (planned):
```bash
bash scripts/observability/test_dashboards.sh
```

---

## CP2.4: Alerting Rules

### Overview

CP2.4 adds Alertmanager rules for production monitoring.

**Reference Documents**:
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - CP2 extension plan
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - CP2 observability plan

### Alerting Rules Validation Tests

**Test Suite**: `test_alerting_rules.sh` (planned)

**Location**: `scripts/observability/test_alerting_rules.sh` (planned)

**CP2.4 Tests** (planned):
- `test_alert_rule_format` - Validates Alertmanager rule YAML format
- `test_alert_rule_queries` - Validates Prometheus queries in alert rules
- `test_alert_rule_conditions` - Validates alert condition logic
- `test_alert_rule_labels` - Validates alert labels and annotations

**Run Command** (planned):
```bash
bash scripts/observability/test_alerting_rules.sh
```

---

## CP2 E2E Test Scripts

### E2E Metrics Validation

**Script**: `scripts/observability/test_metrics_e2e.sh` (planned)

**Purpose**: End-to-end validation of Prometheus metrics export across all components

**Tests** (planned):
- Metrics endpoint availability (all components)
- Prometheus format validation
- Required metrics presence
- CP1 correlation fields as labels
- Metrics collection performance

**Run Command** (planned):
```bash
bash scripts/observability/test_metrics_e2e.sh
```

### E2E Tracing Validation

**Script**: `scripts/observability/test_tracing_e2e.sh` (planned)

**Purpose**: End-to-end validation of OpenTelemetry tracing across all components

**Tests** (planned):
- OTLP trace export validation
- Trace context propagation across components
- CP1 correlation fields in traces
- Trace export performance

**Run Command** (planned):
```bash
bash scripts/observability/test_tracing_e2e.sh
```

---

## Unified CP2 Test Profile

### Wrapper Script

**Script**: `scripts/observability/run_cp2_profile.sh` (planned)

**Purpose**: Run all CP1 core tests + CP2 extension tests as a unified "package"

**Usage** (planned):
```bash
# Run all CP1 + CP2 tests
bash scripts/observability/run_cp2_profile.sh

# Run CP2 tests only (assumes CP1 tests already passed)
bash scripts/observability/run_cp2_profile.sh --cp2-only

# Run specific CP2 feature tests
bash scripts/observability/run_cp2_profile.sh --cp2-feature metrics
bash scripts/observability/run_cp2_profile.sh --cp2-feature tracing
bash scripts/observability/run_cp2_profile.sh --cp2-feature dashboards
bash scripts/observability/run_cp2_profile.sh --cp2-feature alerting
```

**Exit Codes** (planned):
- `0` - All CP1 + CP2 tests passed
- `1` - One or more tests failed
- `2` - Services not running (E2E tests skipped)

### Test Execution Order

1. **CP1 Core Tests** (mandatory)
   - Run `scripts/observability/run_cp1_profile.sh`
   - All CP1 tests must pass before CP2 tests

2. **CP2.1: Prometheus Metrics** (if enabled)
   - Router metrics tests
   - Gateway metrics tests
   - Worker metrics tests
   - E2E metrics validation

3. **CP2.2: OpenTelemetry Tracing** (if enabled)
   - Router tracing tests
   - Gateway tracing tests
   - Worker tracing tests
   - E2E tracing validation

4. **CP2.3: Grafana Dashboards** (if enabled)
   - Dashboard configuration validation
   - Dashboard query validation

5. **CP2.4: Alerting Rules** (if enabled)
   - Alert rule format validation
   - Alert rule query validation

---

## Test Summary

### CP2 Tests (Planned)

| Component | CP2.1 Metrics | CP2.2 Tracing | CP2.3 Dashboards | CP2.4 Alerting | Total |
|-----------|---------------|---------------|------------------|----------------|-------|
| **Router** | 8+ tests | 5+ tests | - | - | 13+ tests |
| **Gateway** | 6+ tests | 4+ tests | - | - | 10+ tests |
| **Worker** | 6+ tests | 4+ tests | - | - | 10+ tests |
| **General** | 1 E2E script | 1 E2E script | 1 script | 1 script | 4 scripts |
| **Total** | **20+ tests** | **13+ tests** | **1 script** | **1 script** | **33+ tests + 4 scripts** |

### Combined CP1 + CP2 Tests

| Phase | Tests | Scripts | Total |
|-------|-------|---------|-------|
| **CP1 Core** | 51+ tests | 5 scripts | 56+ items |
| **CP2 Extensions** | 33+ tests | 4 scripts | 37+ items |
| **Total** | **84+ tests** | **9 scripts** | **93+ items** |

---

## References

- `docs/OBSERVABILITY_CP1_TEST_PROFILE.md` - CP1 observability test profile (foundation)
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/OBSERVABILITY.md` - General observability requirements
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - CP2 extension plan
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - Prometheus metrics specification
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - CP2 observability plan
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - CP2 observability backlog
- `config/observability/logging.json` - Log format schema
- `scripts/observability/run_cp1_profile.sh` - CP1 test profile script
- `scripts/observability/run_cp2_profile.sh` - CP2 test profile script (planned)

---

## Change History

**v0.1 (2025-01-27)** (Draft):
- Initial CP2 observability test profile draft
- CP2.1: Prometheus metrics tests (planned)
- CP2.2: OpenTelemetry tracing tests (planned)
- CP2.3: Grafana dashboards tests (planned)
- CP2.4: Alerting rules tests (planned)
- Unified CP2 test profile script specification (planned)

---

## Status

**Current Status**: 📋 **DRAFT** - Planning phase

**Next Steps**:
1. Implement CP2.1: Prometheus metrics export tests
2. Implement CP2.2: OpenTelemetry tracing tests
3. Implement CP2.3: Grafana dashboards tests
4. Implement CP2.4: Alerting rules tests
5. Create unified CP2 test profile script (`run_cp2_profile.sh`)
6. Integrate CP2 tests into CI/CD pipelines

**Note**: This document is a draft and will be updated as CP2 observability features are implemented.
