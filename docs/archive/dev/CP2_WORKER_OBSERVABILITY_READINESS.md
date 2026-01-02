# CP2 Worker & Observability Readiness Criteria

**Version**: 2.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Workers**: wrk-3 (Worker Reliability), wrk-obs1 (Observability CP2)  
**Status**: 📋 **READINESS CRITERIA** (CP2)

---

## Executive Summary

This document defines **unified readiness criteria** that must be met before starting CP2 Worker Reliability and CP2 Observability implementation. It combines results from W3-N2 (Worker Reliability Readiness) and OBS-N2 (Observability Readiness) into a single comprehensive readiness framework.

**Go/No-Go Decision**: `docs/archive/dev/CP2_WAVE1_GO_NO_GO.md` - Use this document to determine when Wave 1 implementation can start

**CRITICAL PRINCIPLE**: **CP1 baseline must remain green**. All CP2 changes must be **additive** and **backward compatible**. CP1 core profiles must continue to pass all tests and validation.

**Key Requirements**:
- All format, port, and configuration decisions must be finalized
- External dependencies (Prometheus, OTEL collector) must be available
- Test environments must be set up
- CP1 compatibility rules must be understood and followed
- Feature flag strategy must be defined
- Rollback strategy must be documented

**References**:
- `docs/OBSERVABILITY_COMPATIBILITY_RULES.md` - CP1↔CP2 compatibility rules (including Anti-Patterns)
- `docs/CP1_CORE_PROFILE_OBSERVABILITY.md` - CP1 observability core profile (baseline must remain green)
- `docs/CP1_CORE_PROFILE_CONTRACTS.md` - CP1 contracts core profile (baseline must remain green)
- `docs/CP1_CORE_PROFILE_TESTS.md` - CP1 tests core profile (baseline must remain green)
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - CP2 Worker Reliability Plan
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - CP2 Observability Plan

---

## CP1 Baseline Preservation

### CP1 Baseline Must Remain Green

**CRITICAL**: All CP1 core profiles must continue to pass all tests and validation after CP2 implementation.

**CP1 Core Profiles** (baseline must remain green):

1. **CP1 Core Profile: Observability** (`docs/CP1_CORE_PROFILE_OBSERVABILITY.md`):
   - ✅ Structured JSON logging (required fields: `timestamp`, `level`, `component`, `message`)
   - ✅ CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`)
   - ✅ Health endpoints (Router: gRPC port 9000, Gateway: HTTP port 3000, Worker: HTTP port 9091)
   - ✅ PII/secret filtering
   - ✅ Validation scripts (`scripts/observability/validate_observability.sh`)

2. **CP1 Core Profile: Contracts** (`docs/CP1_CORE_PROFILE_CONTRACTS.md`):
   - ✅ Router ↔ Gateway contracts (DecideRequest, DecideResponse)
   - ✅ Router ↔ Worker contracts (ExecAssignment, ExecResult, ExecAssignmentAck)
   - ✅ Status mappings and error codes
   - ✅ Correlation fields propagation

3. **CP1 Core Profile: Tests** (`docs/CP1_CORE_PROFILE_TESTS.md`):
   - ✅ Contract tests (Worker ↔ Router)
   - ✅ Core functionality tests (all components)
   - ✅ Observability tests (all components)
   - ✅ Integration tests (E2E flows)

**Validation**:
- All CP1 core profile tests must pass before CP2 implementation
- All CP1 core profile tests must continue to pass after CP2 implementation
- CP1 validation scripts must continue to work

**Reference**: `docs/OBSERVABILITY_COMPATIBILITY_RULES.md` - Comprehensive compatibility rules and Anti-Patterns

---

## Common Invariants

### 1. Compatibility Rules

**CRITICAL**: All CP2 changes must follow compatibility rules defined in `docs/OBSERVABILITY_COMPATIBILITY_RULES.md`.

#### 1.1. Anti-Patterns (FORBIDDEN)

**Reference**: `docs/OBSERVABILITY_COMPATIBILITY_RULES.md#anti-patterns-what-not-to-do-in-cp2`

**Log Format Anti-Patterns**:
- ❌ **DO NOT** change required log field names (`timestamp`, `level`, `component`, `message`)
- ❌ **DO NOT** change log field types or timestamp format
- ❌ **DO NOT** change CP1 correlation field names or locations
- ❌ **DO NOT** remove required fields from logs

**Health Endpoint Anti-Patterns**:
- ❌ **DO NOT** change health endpoint paths or ports
- ❌ **DO NOT** remove required health response fields
- ❌ **DO NOT** make CP1 health endpoints depend on CP2 features

**PII Filtering Anti-Patterns**:
- ❌ **DO NOT** remove fields from PII filter list
- ❌ **DO NOT** change PII replacement value (`[REDACTED]`)

**Validation Script Anti-Patterns**:
- ❌ **DO NOT** modify CP1 validation checks
- ❌ **DO NOT** change exit codes for CP1 validation
- ❌ **DO NOT** remove CP1 validation logic

**Dependency Anti-Patterns**:
- ❌ **DO NOT** make CP1 functionality depend on CP2 features
- ❌ **DO NOT** require CP2 metrics/tracing for CP1 logs to work
- ❌ **DO NOT** require CP2 endpoints for CP1 health checks to pass

**Summary**: CP2 changes must be **additive only**. All CP1 invariants must remain unchanged. CP1 must work independently of CP2.

#### 1.2. CP1 Invariants (Immutable)

**Reference**: `docs/OBSERVABILITY_COMPATIBILITY_RULES.md#cp1-invariants-immutable`

**Immutable CP1 Invariants**:
- JSON log format structure (required fields, types, locations)
- Health endpoint contracts (paths, ports, response formats)
- PII/secret filtering rules
- CP1 correlation fields (names, types, locations)
- Validation script exit codes and logic

**CP2 Extensions (Allowed)**:
- ✅ Add new `/metrics` endpoints (separate ports)
- ✅ Add optional log fields (e.g., `span_id`)
- ✅ Extend `context` object with new fields
- ✅ Add OTEL tracing (does not affect logs)

---

### 2. Feature Flag Strategy

**CRITICAL**: All CP2 features must be gated behind feature flags to preserve CP1 baseline.

#### 2.1. Feature Flag Pattern

**All Components**:
- Feature flags default to `false` (CP1 baseline mode)
- Feature flags can be toggled via environment variables or config files
- Feature flag checks are fast (no performance impact)
- Feature flags can be disabled at runtime (rollback capability)

**Router (Erlang/OTP)**:
```erlang
% Feature flag check
-spec is_cp2_feature_enabled(FeatureName :: string()) -> boolean().
is_cp2_feature_enabled(FeatureName) ->
    case application:get_env(router, {cp2, FeatureName, enabled}, false) of
        true -> true;
        _ -> false
    end.

% Usage in code
case is_cp2_feature_enabled("metrics") of
    true -> router_metrics:record_decision(PolicyId, Provider);
    false -> ok  % CP1 baseline: no metrics
end.
```

**Gateway (C11)**:
```c
// Feature flag check
bool is_cp2_feature_enabled(const char* feature_name) {
    const char* env_var = getenv("CP2_OBSERVABILITY_ENABLED");
    if (env_var && strcmp(env_var, "true") == 0) {
        return true;
    }
    // Default: CP2 features disabled (CP1 baseline)
    return false;
}
```

**Worker (C++/CAF)**:
```cpp
// Feature flag check
bool is_cp2_feature_enabled(const std::string& feature_name) {
    auto env_value = std::getenv(("CP2_OBSERVABILITY_" + feature_name + "_ENABLED").c_str());
    if (env_value && std::string(env_value) == "true") {
        return true;
    }
    // Default: CP2 features disabled (CP1 baseline)
    return false;
}
```

#### 2.2. Feature Flags (Worker Reliability)

**Worker Reliability Feature Flags**:
- `CP2_ADVANCED_RETRY_ENABLED` (default: `false`) - Exponential backoff, error classification, retry budget
- `CP2_COMPLETE_TIMEOUT_ENABLED` (default: `false`) - FS timeouts, HTTP connection timeout, total timeout
- `CP2_QUEUE_MANAGEMENT_ENABLED` (default: `false`) - Bounded queue, queue monitoring, queue rejection

**Reference**: `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1.md`

#### 2.3. Feature Flags (Observability)

**Observability Feature Flags**:
- `CP2_OBSERVABILITY_METRICS_ENABLED` (default: `false`) - Prometheus metrics export
- `CP2_OBSERVABILITY_TRACING_ENABLED` (default: `false`) - OpenTelemetry tracing

**Reference**: `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md`

---

### 3. Rollback Strategy

**CRITICAL**: Rollback strategy must be defined and tested before CP2 implementation.

#### 3.1. Rollback Mechanism

**Primary Rollback**: Feature flags
- Disable CP2 feature flags (set to `false`)
- Components revert to CP1 baseline behavior
- No code changes required
- Immediate rollback (no restart required for some flags)

**Secondary Rollback**: Code deployment
- Revert to previous CP1-compatible code version
- Requires code deployment and restart
- Use only if feature flag rollback fails

#### 3.2. Rollback Testing

**Before CP2 Implementation**:
- ✅ Test feature flag disable (CP2 features disabled, CP1 baseline active)
- ✅ Test CP1 core profile tests pass with feature flags disabled
- ✅ Test CP1 validation scripts pass with feature flags disabled
- ✅ Document rollback procedure

**After CP2 Implementation**:
- ✅ Verify rollback procedure works
- ✅ Verify CP1 baseline restored after rollback
- ✅ Verify no data loss or corruption after rollback

#### 3.3. Rollback Validation

**Rollback Success Criteria**:
- ✅ All CP1 core profile tests pass
- ✅ All CP1 validation scripts pass
- ✅ CP1 health endpoints functional
- ✅ CP1 logs format unchanged
- ✅ No CP2 features active

---

## Worker Reliability Readiness (wrk-3)

**Worker**: wrk-3 (Worker Reliability)  
**Reference**: `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1.md`

### 1. Feature Flag Decisions

#### 1.1. Retry Feature Flag

**Decision**: ✅ **DECIDED**

**Feature Flag**:
- Name: `CP2_ADVANCED_RETRY_ENABLED` or `worker.retries.v2.enabled`
- Default: `false` (CP1 baseline)
- Environment variable: `CP2_ADVANCED_RETRY_ENABLED`

**Gates**:
- Exponential backoff
- Error classification
- Retry budget management

**Reference**: `docs/archive/dev/CP2_WORKER_RETRY_DESIGN.md`

#### 1.2. Timeout Feature Flag

**Decision**: ✅ **DECIDED**

**Feature Flag**:
- Name: `CP2_COMPLETE_TIMEOUT_ENABLED`
- Default: `false` (CP1 baseline)
- Environment variable: `CP2_COMPLETE_TIMEOUT_ENABLED`

**Gates**:
- FS operation timeouts
- HTTP connection timeout
- Total timeout across retries

**Reference**: `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md`

#### 1.3. Queue Management Feature Flag

**Decision**: ✅ **DECIDED**

**Feature Flag**:
- Name: `CP2_QUEUE_MANAGEMENT_ENABLED` or `worker.queue.v2.enabled`
- Default: `false` (CP1 baseline)
- Environment variable: `CP2_QUEUE_MANAGEMENT_ENABLED`

**Gates**:
- Bounded queue implementation
- Queue depth monitoring
- Queue rejection handling

**Reference**: `docs/archive/dev/CP2_WORKER_BACKPRESSURE_DESIGN.md`

---

### 2. Configuration Format Decisions

#### 2.1. Retry Policy Configuration

**Decision**: ✅ **DECIDED**

**Configuration Format**:
```json
{
  "worker": {
    "retries": {
      "v2": {
        "enabled": false,
        "default_policy": {
          "base_delay_ms": 100,
          "max_delay_ms": 30000,
          "jitter_type": "full",
          "retry_budget_ms": 30000
        },
        "category_policies": {
          "network": {
            "retryable": true,
            "max_retries": 3
          },
          "validation": {
            "retryable": false,
            "max_retries": 0
          }
        }
      }
    }
  }
}
```

**Reference**: `docs/archive/dev/CP2_WORKER_RETRY_DESIGN.md`

#### 2.2. Queue Management Configuration

**Decision**: ✅ **DECIDED**

**Configuration Format**:
```json
{
  "worker": {
    "queue": {
      "v2": {
        "enabled": false,
        "max_queue_size": 1000,
        "rejection_policy": "reject_new",
        "overload_thresholds": {
          "degraded": 0.5,
          "overloaded": 0.8,
          "critical": 1.0
        }
      }
    }
  }
}
```

**Reference**: `docs/archive/dev/CP2_WORKER_BACKPRESSURE_DESIGN.md`

---

### 3. Test Environment Decisions

#### 3.1. Worker Test Environment

**Decision**: ✅ **READY**

**Requirements**:
- Worker running locally or in Docker
- Router available for ExecAssignment messages
- NATS available for message exchange
- Test scenarios: Retry policies, timeout enforcement, queue management

**Reference**: `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1.md`

---

## Observability Readiness (wrk-obs1)

**Worker**: wrk-obs1 (Observability CP2)  
**Reference**: `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md`

### 1. Format Decisions

#### 1.1. Prometheus Metrics Format

**Decision**: ✅ **DECIDED**

**Specification**:
- Format: Prometheus text format (standard)
- Endpoint: HTTP `GET /metrics`
- Content-Type: `text/plain; version=0.0.4`

**Reference**: `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md`

#### 1.2. OpenTelemetry Trace Format

**Decision**: ✅ **DECIDED**

**Specification**:
- Format: OTLP (OpenTelemetry Protocol)
- Export modes: OTLP/gRPC (port 4317) or OTLP/HTTP (port 4318)
- Trace context: W3C Trace Context format (`traceparent`, `tracestate`)

**Reference**: `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md`

#### 1.3. Metric Naming Conventions

**Decision**: ✅ **DECIDED**

**Specification**:
- Naming: `snake_case` (e.g., `http_requests_total`, `router_decisions_total`)
- Suffixes: `_total` (counter), `_seconds` (histogram), `_bytes` (histogram)
- Prefixes: `component_*` for cross-component metrics

**Reference**: `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md`

#### 1.4. Label Naming Conventions

**Decision**: ✅ **DECIDED**

**Specification**:
- Naming: `snake_case` (e.g., `tenant_id`, `run_id`, `flow_id`, `step_id`)
- CP1 correlation fields: `tenant_id`, `run_id`, `flow_id`, `step_id` (never use `trace_id` in metrics)
- Component fields: `component`, `method`, `path`, `status_code`, `provider`, `policy_id`, `step_type`, `execution_status`

**Reference**: `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md`

---

### 2. Port Decisions

#### 2.1. Metrics Endpoint Ports

**Decision**: ✅ **DECIDED**

**Port Assignments**:

| Component | Protocol | Path | Port | CP1 Port | Rationale |
|-----------|----------|------|------|----------|-----------|
| Router | HTTP | `GET /metrics` | 9001 | 9000 (gRPC health) | Separate from gRPC health port |
| Gateway | HTTP | `GET /metrics` | 3001 | 3000 (HTTP API) | Separate from API port |
| Worker | HTTP | `GET /metrics` | 9092 | 9091 (HTTP health) | Separate from health port |

**Key Principle**: CP2 metrics endpoints are **separate** from CP1 health endpoints. No conflicts.

#### 2.2. OTEL Collector Ports

**Decision**: ✅ **DECIDED**

**Port Assignments**:

| Protocol | Port | Endpoint | Usage |
|----------|------|----------|-------|
| OTLP/gRPC | 4317 | `http://localhost:4317` | gRPC trace export (default) |
| OTLP/HTTP | 4318 | `http://localhost:4318/v1/traces` | HTTP trace export (alternative) |

**Configuration**:
- Environment variable: `OTLP_ENDPOINT` (default: `http://localhost:4317`)
- Fallback: `OTEL_EXPORTER_OTLP_ENDPOINT`
- Auto-append `/v1/traces` for HTTP mode if not present

#### 2.3. Prometheus Port

**Decision**: ✅ **DECIDED**

**Port Assignment**:
- Prometheus UI: `http://localhost:9090`
- Prometheus API: `http://localhost:9090/api/v1/`

**Reference**: `tools/observability/prometheus.yml`

#### 2.4. Grafana Port

**Decision**: ⚠️ **CONFLICT DETECTED - RESOLUTION NEEDED**

**Current Assignment**:
- Grafana UI: `http://localhost:3000` (conflicts with Gateway API port 3000)

**Resolution**:
- **Change Grafana port to 3002** (recommended)
- Update `tools/observability/docker-compose.observability.yml`
- Update documentation

**Action Required**:
1. Update Docker Compose: Change Grafana port to 3002
2. Update documentation: Document Grafana port 3002
3. Verify no other conflicts

---

### 3. Metric Decisions

#### 3.1. Core Metrics (Wave 1)

**Decision**: ✅ **DECIDED**

**Core Metrics** (from `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md`):

**Router Metrics**:
- `router_decisions_total{policy_id, provider, decision_reason, tenant_id, run_id}` (Counter)
- `router_decision_duration_seconds{policy_id, provider, tenant_id, run_id}` (Histogram)
- `router_provider_selections_total{provider, policy_id, tenant_id, run_id}` (Counter)
- `router_errors_total{error_code, tenant_id, run_id}` (Counter)

**Gateway Metrics**:
- `gateway_http_requests_total{method, route, status_code, tenant_id}` (Counter)
- `gateway_http_request_duration_seconds{method, route, status_code, tenant_id}` (Histogram)
- `gateway_rate_limit_hits_total{tenant_id, endpoint}` (Counter)
- `gateway_idempotency_hits_total` (Counter)

**Worker Metrics**:
- `worker_step_executions_total{step_type, execution_status, tenant_id, run_id, flow_id, step_id}` (Counter)
- `worker_step_execution_duration_seconds{step_type, execution_status, tenant_id, run_id, flow_id, step_id}` (Histogram)
- `worker_step_errors_total{step_type, error_code, tenant_id, run_id, flow_id, step_id}` (Counter)
- `worker_queue_depth{resource_pool}` (Gauge)

#### 3.2. Label Cardinality Limits

**Decision**: ✅ **DECIDED**

**Cardinality Guidelines**:

| Label | Cardinality | Usage | Recommendation |
|-------|-------------|-------|----------------|
| `tenant_id` | Medium (10-1000) | All metrics (when available) | ✅ Safe for all metrics |
| `run_id` | High (10,000-1,000,000) | Detailed metrics only | ⚠️ Use only in detailed metrics, exclude from aggregates |
| `flow_id` | Medium-High (100-10,000) | Detailed metrics only | ⚠️ Use only in detailed metrics, exclude from aggregates |
| `step_id` | Very High (>1,000,000) | Detailed metrics only | ⚠️ Use only in detailed metrics, never in aggregates |
| `trace_id` | Very High (>1,000,000) | **Never use in metrics** | ❌ Use only in logs/traces |
| `error_code` | Low (<100) | Error metrics | ✅ Safe for all metrics |
| `component` | Low (<10) | All metrics | ✅ Safe for all metrics |

**Reference**: `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md`

---

### 4. OpenTelemetry Decisions

#### 4.1. OTEL SDK Versions

**Decision**: ✅ **DECIDED**

**SDK Versions**:

| Component | Language | SDK | Version | Reference |
|-----------|----------|-----|---------|-----------|
| Router | Erlang/OTP | `opentelemetry-erlang` | Latest stable | `apps/otp/router/rebar.config` |
| Gateway | C | `opentelemetry-c` | Latest stable | `apps/c-gateway/CMakeLists.txt` |
| Worker | C++ | `opentelemetry-cpp` | Latest stable | `apps/caf/processor/CMakeLists.txt` |

#### 4.2. OTEL Export Mode

**Decision**: ✅ **DECIDED**

**Export Mode**:
- **Default**: OTLP/gRPC (port 4317)
- **Alternative**: OTLP/HTTP (port 4318)
- **Configuration**: Via `OTLP_ENDPOINT` environment variable

**Rationale**:
- gRPC is more efficient for high-volume traces
- HTTP is more compatible with firewalls and proxies
- Both modes supported, gRPC recommended for production

#### 4.3. OTEL Batch Settings

**Decision**: ✅ **DECIDED**

**Batch Settings**:
- Batch size: 512 spans (default)
- Batch timeout: 5 seconds (default)
- Max queue size: 2048 spans (default)

**Configuration**:
- Environment variables: `OTEL_BSP_MAX_EXPORT_BATCH_SIZE`, `OTEL_BSP_EXPORT_TIMEOUT`, `OTEL_BSP_MAX_QUEUE_SIZE`
- Config files: Component-specific configuration

#### 4.4. OTEL Sampling

**Decision**: ✅ **DECIDED**

**Sampling Strategy**:
- **Wave 1**: No sampling (100% of traces)
- **Wave 2**: Head-based sampling (10% of traces for high-volume scenarios)
- **Configuration**: Via `OTEL_TRACES_SAMPLER` environment variable

**Rationale**:
- Wave 1: Full sampling for validation
- Wave 2: Sampling for production (reduce trace volume)

---

### 5. External Dependencies

#### 5.1. Prometheus Server

**Decision**: ✅ **READY**

**Requirements**:
- Prometheus server must be available for metrics scraping
- Prometheus configuration must include scrape targets:
  - Router: `localhost:9001` (or configured host)
  - Gateway: `localhost:3001` (or configured host)
  - Worker: `localhost:9092` (or configured host)

**Local Development**:
- Docker Compose: `tools/observability/docker-compose.observability.yml`
- Prometheus config: `tools/observability/prometheus.yml`
- Port: 9090

**Production**:
- Prometheus server with configured scrape targets
- Prometheus configuration managed via config management

**Reference**: `tools/observability/prometheus.yml`

#### 5.2. OpenTelemetry Collector

**Decision**: ✅ **READY**

**Requirements**:
- OTEL collector must be available for trace collection
- OTEL collector endpoint: `http://localhost:4317` (gRPC) or `http://localhost:4318` (HTTP)
- OTEL collector must be configured to receive OTLP traces

**Local Development**:
- Docker Compose: `tools/observability/docker-compose.observability.yml` (Jaeger all-in-one)
- OTEL collector endpoint: `http://localhost:4317` (gRPC) or `http://localhost:4318` (HTTP)
- Alternative: Jaeger all-in-one (port 14268 for HTTP, port 6831/6832 for UDP)

**Production**:
- OTEL collector with configured OTLP receiver
- OTEL collector endpoint: Configurable via environment variable

**Reference**: `tools/observability/docker-compose.observability.yml`

#### 5.3. Grafana (Optional for Wave 1)

**Decision**: ⚠️ **PORT CONFLICT - RESOLUTION NEEDED**

**Requirements**:
- Grafana server must be available for dashboard visualization
- Grafana data source: Prometheus (localhost:9090 or remote)
- Grafana port: **CONFLICT** with Gateway API port 3000

**Resolution**:
- **Change Grafana port to 3002** (recommended)
- Update Docker Compose configuration
- Update documentation

**Action Required**:
1. Update `tools/observability/docker-compose.observability.yml`: Change Grafana port to 3002
2. Update documentation: Document Grafana port 3002
3. Verify no other conflicts

#### 5.4. Alertmanager (Wave 2)

**Decision**: 📋 **DEFERRED TO WAVE 2**

**Requirements**:
- Alertmanager server must be available for alerting
- Alertmanager configuration: Alert rules and notification channels
- **Wave 1**: Not required (alerting is Wave 2)

---

### 6. Test Environments

#### 6.1. Local Development Environment

**Decision**: ✅ **READY**

**Requirements**:
- Docker Compose setup for observability stack
- Prometheus: `localhost:9090`
- Grafana: `localhost:3002` (after port conflict resolution)
- OTEL Collector/Jaeger: `localhost:4317` (gRPC) or `localhost:4318` (HTTP)
- Components: Router, Gateway, Worker (all running locally)

**Setup**:
```bash
# Start observability stack
cd tools/observability
docker-compose -f docker-compose.observability.yml up -d

# Verify services
curl http://localhost:9090/-/healthy  # Prometheus
curl http://localhost:3002/api/health  # Grafana (after port change)
curl http://localhost:4318/v1/traces  # OTEL Collector (HTTP)
```

**Reference**: `tools/observability/docker-compose.observability.yml`

#### 6.2. Integration Test Environment

**Decision**: ✅ **READY**

**Requirements**:
- Prometheus available for metrics scraping
- OTEL Collector available for trace collection
- Components: Router, Gateway, Worker (all running in test environment)
- Test data: Synthetic requests and traces

**Setup**:
- Use same Docker Compose setup as local development
- Configure test-specific scrape targets
- Configure test-specific OTEL collector endpoint

#### 6.3. E2E Test Environment

**Decision**: ✅ **READY**

**Requirements**:
- Full observability stack (Prometheus, Grafana, OTEL Collector)
- Components: Router, Gateway, Worker (all running)
- E2E test scenarios: Request flow (Gateway → Router → Worker)
- Validation: Metrics export, trace export, context propagation

**Setup**:
- Use Docker Compose setup
- Run E2E test scripts
- Validate metrics and traces

**Reference**: `scripts/observability/validate_observability_e2e.sh`

---

## Summary: Readiness Checklist

### CP1 Baseline Preservation

- ✅ CP1 core profile tests documented and must pass
- ✅ CP1 compatibility rules understood (`docs/OBSERVABILITY_COMPATIBILITY_RULES.md`)
- ✅ CP1 anti-patterns documented and understood
- ✅ CP1 validation scripts must continue to work
- ✅ CP1 baseline must remain green after CP2 implementation

### Common Invariants

- ✅ Compatibility rules documented and understood
- ✅ Feature flag strategy defined (all flags default to `false`)
- ✅ Rollback strategy defined and tested
- ✅ CP1 invariants immutable (logs, health endpoints, PII filtering)

### Worker Reliability Readiness (wrk-3)

- ✅ Feature flag decisions: Retry, Timeout, Queue Management
- ✅ Configuration format decisions: Retry policy, Queue management
- ✅ Test environment decisions: Worker test environment

### Observability Readiness (wrk-obs1)

- ✅ Format decisions: Prometheus format, OTEL format, naming conventions
- ✅ Port decisions: Metrics endpoints, OTEL collector, Prometheus, Grafana (⚠️ port conflict needs resolution)
- ✅ Metric decisions: Core metrics, label cardinality limits
- ✅ OTEL decisions: SDK versions, export mode, batch settings, sampling
- ✅ External dependencies: Prometheus (ready), OTEL collector (ready), Grafana (⚠️ port conflict), Alertmanager (Wave 2)
- ✅ Test environments: Local development, integration tests, E2E tests

---

## Blockers and Resolutions

### Blocker 1: Grafana Port Conflict

**Issue**: Grafana port 3000 conflicts with Gateway API port 3000

**Resolution**: Change Grafana port to 3002

**Action Required**:
1. Update `tools/observability/docker-compose.observability.yml`: Change Grafana port to 3002
2. Update `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md`: Document Grafana port 3002
3. Update this document: Document resolution

**Status**: ⚠️ **RESOLUTION NEEDED**

---

## Acceptance Criteria

### CP1 Baseline Preservation

- ✅ All CP1 core profile tests pass before CP2 implementation
- ✅ All CP1 core profile tests continue to pass after CP2 implementation
- ✅ CP1 validation scripts continue to work
- ✅ CP1 health endpoints functional
- ✅ CP1 logs format unchanged

### Common Invariants

- ✅ Compatibility rules followed (no anti-patterns violated)
- ✅ Feature flags default to `false` (CP1 baseline)
- ✅ Rollback strategy tested and documented

### Worker Reliability Readiness

- ✅ All feature flag decisions documented
- ✅ All configuration format decisions documented
- ✅ Test environments documented

### Observability Readiness

- ✅ All format decisions documented
- ✅ All port decisions documented (Grafana port conflict resolved)
- ✅ All metric decisions documented
- ✅ All OTEL decisions documented
- ✅ External dependencies available (Prometheus, OTEL collector)
- ✅ Test environments documented

---

## References

### CP1 Baseline Documents
- `docs/CP1_CORE_PROFILE_OBSERVABILITY.md` - CP1 observability core profile (baseline must remain green)
- `docs/CP1_CORE_PROFILE_CONTRACTS.md` - CP1 contracts core profile (baseline must remain green)
- `docs/CP1_CORE_PROFILE_TESTS.md` - CP1 tests core profile (baseline must remain green)
- `docs/OBSERVABILITY_COMPATIBILITY_RULES.md` - CP1↔CP2 compatibility rules (including Anti-Patterns)

### Worker Reliability Documents
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - CP2 Worker Reliability Plan
- `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1.md` - Wave 1 specification
- `docs/archive/dev/CP2_WORKER_RETRY_DESIGN.md` - Retry policy design
- `docs/archive/dev/CP2_WORKER_BACKPRESSURE_DESIGN.md` - Backpressure protocol design

### Observability Documents
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md` - Wave 1 specification
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - Detailed CP2 extension plan
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - Prometheus metrics specification
- `docs/OBSERVABILITY_CP2_TEST_PROFILE.md` - CP2 test profile

### Infrastructure
- `tools/observability/prometheus.yml` - Prometheus configuration template
- `tools/observability/docker-compose.observability.yml` - Docker Compose setup

---

## Change History

**v2.0 (2025-01-27)**:
- Unified CP2 Worker & Observability Readiness Criteria
- Added CP1 Baseline Preservation section (explicit requirement: CP1 baseline must remain green)
- Added Common Invariants section (compatibility rules, feature flags, rollback strategy)
- Separated Worker Reliability (wrk-3) and Observability (wrk-obs1) sections
- Added explicit references to CP1 core profiles and compatibility rules
- Grafana port conflict identified (needs resolution)

**v1.0 (2025-01-27)**:
- Initial CP2 Worker & Observability Readiness Criteria
- Format, port, and metric decisions documented
- OTEL decisions documented
- External dependencies documented
- Test environments documented

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Readiness Criteria Documented (Grafana port conflict needs resolution)
