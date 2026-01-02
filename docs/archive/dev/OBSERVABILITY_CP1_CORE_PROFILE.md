# CP1 Observability Core Profile

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Purpose**: Define minimal mandatory observability requirements for CP1 (Router/Gateway/Worker)  
**Status**: ✅ **ACTIVE**

---

## Overview

This document defines the **minimal mandatory observability profile** for CP1. It explicitly separates CP1 requirements from CP2+ features (performance tests, dashboards, advanced CI/CD, metrics, tracing) to simplify CP1 validation and prevent CP2+ elements from becoming de facto mandatory.

**Relationship to Other Documents**:
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - Defines CP1 observability invariants (cross-cutting requirements)
- This document - Defines CP1 core profile (minimal mandatory set for validation)

**Key Principle**: CP1 observability is **minimal and focused** on structured logging, health endpoints, and basic validation. Advanced features (metrics, tracing, dashboards) are explicitly **CP2+**.

---

## CP1 Core Profile (Mandatory)

### 1. Structured JSON Logging

**Requirement**: All components (Router, Gateway, Worker) must produce structured JSON logs.

**Required Fields** (always present):
- `timestamp` (ISO 8601, UTC, microseconds precision)
- `level` (ERROR, WARN, INFO, DEBUG)
- `component` (router, gateway, worker)
- `message` (human-readable text)

**Schema**: `config/observability/logging.json`

**Validation**: `scripts/observability/validate_observability.sh`

**CP1 Scope**: Log format validation only. No log aggregation (Loki), no log analysis dashboards.

---

### 2. CP1 Correlation Fields

**Requirement**: Include correlation fields in logs when context is available.

**CP1 Required Fields** (when context available):

| Field | Required When | Components |
|-------|---------------|------------|
| `tenant_id` | Tenant context available | All components |
| `run_id` | Run context available | Router, Worker, Gateway |
| `flow_id` | Flow context available | Router, Worker |
| `step_id` | Step context available | Worker, Router |
| `trace_id` | Trace context available | All components |

**Component-Specific Requirements**:
- **Router**: `tenant_id`, `run_id`, `flow_id`, `trace_id` when available
- **Gateway**: `tenant_id`, `run_id`, `trace_id` when available
- **Worker**: `tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id` when available

**CP1 Scope**: Field presence validation only. No distributed tracing (OpenTelemetry), no trace visualization.

---

### 3. Health Endpoints

**Requirement**: All components must provide health check endpoints.

**Component-Specific Endpoints**:

| Component | Protocol | Path | Port | Verification |
|-----------|----------|------|------|--------------|
| Router | gRPC | `grpc.health.v1.Health/Check` | 9000 | `grpc_health_probe -addr=localhost:9000` |
| Gateway | HTTP | `GET /_health` | 3000 | `curl http://localhost:3000/_health` |
| Worker | HTTP | `GET /_health` | 9091 | `curl http://localhost:9091/_health` |

**Response Format** (HTTP components):
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00.123456Z"
}
```

**Status Values**:
- `healthy` - Service operating normally
- `degraded` - Service operating with limitations
- `unhealthy` - Service unavailable or critically broken

**CP1 Scope**: Health endpoint availability and format validation only. No health metrics collection, no health dashboards.

---

### 4. PII/Secret Filtering

**Requirement**: All components must automatically filter sensitive data before logging.

**Filtered Fields**:
- `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`
- `authorization`, `credit_card`, `ssn`, `email`, `phone`

**Replacement**: `[REDACTED]`

**CP1 Scope**: PII filtering validation only. No secret leak detection automation, no compliance dashboards.

---

### 5. Validation Scripts

**Requirement**: Basic validation scripts must exist and pass.

**Primary Script**: `scripts/observability/validate_observability.sh`

**CP1 Checks**:
1. Logging configuration validation (`config/observability/logging.json`)
2. JSON log format validation
3. CP1 correlation fields validation (when context available)
4. Health endpoint availability and format (if services running)
5. PII/secret detection in logs

**Exit Codes**:
- `0` - Success (may have warnings)
- `2` - External endpoints unavailable (services not running - expected in local validation)
- `3` - Local configs missing or invalid

**PowerShell Alternative**: `scripts/observability/validate_observability.ps1`

**E2E Validation**: `scripts/observability/validate_observability_e2e.sh` (optional for CP1, recommended for CI/CD)

**CP1 Scope**: Basic validation only. No advanced CI/CD integration, no performance benchmarking, no code coverage analysis.

---

## CP2+ Exclusions (Explicitly Not CP1)

The following features are **explicitly excluded** from CP1 core profile. They may be implemented but are **not mandatory** for CP1 validation.

### Metrics and Monitoring (CP2+)

**Excluded from CP1**:
- ❌ Prometheus metrics export (`/metrics` endpoint)
- ❌ Metrics collection (counters, histograms, gauges)
- ❌ Metrics dashboards (Grafana)
- ❌ Alerting rules (Alertmanager)
- ❌ Metrics aggregation and storage

**CP1 Alternative**: Use log analysis for basic metrics (not recommended for production, but acceptable for CP1).

**Reference**: `apps/otp/router/docs/OBSERVABILITY_CP2_PLANNING.md` for CP2 metrics planning.

---

### Distributed Tracing (CP2+)

**Excluded from CP1**:
- ❌ OpenTelemetry tracing integration
- ❌ Span creation and context propagation
- ❌ Trace visualization (Jaeger, Tempo)
- ❌ Trace analysis and debugging tools

**CP1 Alternative**: Use `trace_id` in logs for manual correlation (no automated tracing).

**Reference**: `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OBSERVABILITY.md` for CP2 tracing planning.

---

### Log Aggregation (CP2+)

**Excluded from CP1**:
- ❌ Centralized log collection (Loki, ELK Stack)
- ❌ Log aggregation infrastructure
- ❌ Log search and analysis tools
- ❌ Log retention policies (beyond basic file rotation)

**CP1 Alternative**: File-based logs only. Manual log collection if needed.

---

### Advanced Testing (CP2+)

**Excluded from CP1**:
- ❌ Performance tests (benchmarking, load testing)
- ❌ Code coverage analysis (gcov, lcov, cover)
- ❌ Performance profiling
- ❌ Memory leak detection
- ❌ Stress testing

**CP1 Scope**: Basic unit tests and integration tests for observability features only.

**Note**: Performance tests may exist (e.g., `test_performance.c` in Gateway), but they are **not mandatory** for CP1 validation.

---

### Advanced CI/CD (CP2+)

**Excluded from CP1**:
- ❌ Multi-platform CI/CD (GitLab CI, Drone CI beyond basic GitHub Actions)
- ❌ Advanced CI/CD workflows (beyond basic validation)
- ❌ Automated coverage reports
- ❌ Performance regression testing
- ❌ Advanced deployment automation

**CP1 Scope**: Basic CI gate (GitHub Actions) with observability validation only.

**Note**: Advanced CI/CD configurations may exist, but they are **not mandatory** for CP1 validation.

---

### Dashboards and Visualization (CP2+)

**Excluded from CP1**:
- ❌ Grafana dashboards
- ❌ Metrics visualization
- ❌ Alerting dashboards
- ❌ Custom observability dashboards

**CP1 Alternative**: Use log analysis tools (manual) or basic log viewing.

**Reference**: `apps/c-gateway/docs/OBSERVABILITY_DASHBOARD.md` for CP2 dashboard planning.

---

### Production Logging Infrastructure (CP2+)

**Excluded from CP1**:
- ❌ Production log rotation (systemd, logrotate, Docker, Kubernetes)
- ❌ Log aggregation setup (Loki, ELK Stack)
- ❌ Production logging best practices automation

**CP1 Scope**: Basic logging to files only. Production logging infrastructure is CP2+.

**Note**: Production logging guides may exist (e.g., `apps/c-gateway/docs/PRODUCTION_LOGGING.md`), but they are **planning documents** for CP2, not CP1 requirements.

---

## Component-Specific CP1 Requirements

### Router (Erlang/OTP)

**CP1 Mandatory**:
- ✅ Structured JSON logs with required fields
- ✅ CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `trace_id`) when available
- ✅ gRPC health endpoint on port 9000
- ✅ PII/secret filtering
- ✅ Validation script compliance

**CP1 Optional** (may exist, not mandatory):
- Unit tests for observability
- Integration tests for health endpoint
- E2E test script

**CP2+ Excluded**:
- Prometheus metrics (planned in CP2)
- OpenTelemetry tracing (planned in CP2)
- Performance tests
- Code coverage analysis
- Advanced CI/CD workflows

**Reference**: `apps/otp/router/docs/OBSERVABILITY.md` for CP1 implementation details.

---

### Gateway (C-Gateway)

**CP1 Mandatory**:
- ✅ Structured JSON logs with required fields
- ✅ CP1 correlation fields (`tenant_id`, `run_id`, `trace_id`) when available
- ✅ HTTP `/_health` endpoint on port 3000
- ✅ PII/secret filtering
- ✅ Validation script compliance

**CP1 Optional** (may exist, not mandatory):
- Unit tests for observability
- Integration tests for health endpoint
- E2E test script

**CP2+ Excluded**:
- Prometheus metrics (planned in CP2)
- Performance tests (may exist, not mandatory for CP1)
- Code coverage analysis (may exist, not mandatory for CP1)
- Advanced CI/CD workflows (may exist, not mandatory for CP2)
- Grafana dashboards (planned in CP2)

**Reference**: `apps/c-gateway/docs/OBSERVABILITY.md` for CP1 implementation details.

---

### Worker (CAF/C++)

**CP1 Mandatory**:
- ✅ Structured JSON logs with required fields
- ✅ CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`) when available
- ✅ HTTP `/_health` endpoint on port 9091
- ✅ PII/secret filtering
- ✅ Validation script compliance

**CP1 Optional** (may exist, not mandatory):
- Unit tests for observability
- Integration tests for health endpoint
- E2E test script

**CP2+ Excluded**:
- Prometheus metrics (planned in CP2)
- Performance tests
- Code coverage analysis
- Advanced CI/CD workflows
- Grafana dashboards (planned in CP2)

**Reference**: `apps/caf/processor/docs/OBSERVABILITY.md` for CP1 implementation details.

---

## CP1 Validation Checklist

### For CP1 Validation, Verify:

**Logging**:
- [ ] All components produce structured JSON logs
- [ ] Required fields present (`timestamp`, `level`, `component`, `message`)
- [ ] CP1 correlation fields included when context available
- [ ] PII/secret filtering working (no secrets in logs)

**Health Endpoints**:
- [ ] Router: gRPC health endpoint on port 9000
- [ ] Gateway: HTTP `/_health` endpoint on port 3000
- [ ] Worker: HTTP `/_health` endpoint on port 9091
- [ ] Health endpoints return CP1-compliant format

**Validation Scripts**:
- [ ] `scripts/observability/validate_observability.sh` exists and passes
- [ ] `scripts/observability/validate_observability.ps1` exists (PowerShell alternative)
- [ ] Exit codes correct (0 for success, 2 for external unavailable, 3 for local errors)

**Documentation**:
- [ ] `docs/OBSERVABILITY_CP1_INVARIANTS.md` exists and is up-to-date
- [ ] Component-specific observability documentation exists

### Not Required for CP1 (CP2+):

- [ ] Prometheus metrics export
- [ ] OpenTelemetry tracing
- [ ] Grafana dashboards
- [ ] Performance tests
- [ ] Code coverage analysis
- [ ] Advanced CI/CD workflows
- [ ] Log aggregation infrastructure
- [ ] Production logging automation

---

## Synchronization with OBSERVABILITY_CP1_INVARIANTS.md

This document is **synchronized** with `docs/OBSERVABILITY_CP1_INVARIANTS.md`:

**OBSERVABILITY_CP1_INVARIANTS.md**:
- Defines **cross-cutting invariants** (requirements that all components must satisfy)
- Provides **detailed specifications** (field definitions, examples, validation procedures)
- Includes **component compliance checklists**

**OBSERVABILITY_CP1_CORE_PROFILE.md** (this document):
- Defines **minimal mandatory profile** for CP1 validation
- Explicitly **separates CP1 from CP2+** features
- Provides **validation checklist** focused on CP1 core requirements

**Key Differences**:
- `OBSERVABILITY_CP1_INVARIANTS.md` is **comprehensive** (all CP1 requirements)
- `OBSERVABILITY_CP1_CORE_PROFILE.md` is **minimal** (core profile for validation)

**Both documents must be kept in sync** when CP1 requirements change.

---

## Enforcement

**For CP1 Validation**:
- ✅ Verify CP1 core profile requirements only
- ✅ Do not require CP2+ features (metrics, tracing, dashboards, performance tests)
- ✅ Accept basic validation scripts (no advanced CI/CD required)
- ✅ Accept file-based logs (no log aggregation required)

**For CP2+ Planning**:
- 📋 Reference CP2 planning documents for advanced features
- 📋 Do not make CP2+ features mandatory for CP1
- 📋 Clearly mark CP2+ features as "planned" or "optional"

---

## References

### CP1 Documentation
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants (comprehensive)
- `docs/OBSERVABILITY.md` - General observability requirements
- `docs/OBSERVABILITY_CONVENTIONS.md` - Detailed conventions
- `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md` - Health endpoint specifications
- `config/observability/logging.json` - Log format schema

### CP2 Planning Documents
- `apps/otp/router/docs/OBSERVABILITY_CP2_PLANNING.md` - Router CP2 planning
- `apps/c-gateway/docs/OBSERVABILITY_DASHBOARD.md` - Gateway CP2 dashboard planning
- `apps/caf/processor/docs/OBSERVABILITY_DASHBOARD.md` - Worker CP2 dashboard planning
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OBSERVABILITY.md` - CP2 observability readiness

### Component Documentation
- `apps/otp/router/docs/OBSERVABILITY.md` - Router observability
- `apps/c-gateway/docs/OBSERVABILITY.md` - Gateway observability
- `apps/caf/processor/docs/OBSERVABILITY.md` - Worker observability

### Validation Scripts
- `scripts/observability/validate_observability.sh` - Primary validation script
- `scripts/observability/validate_observability.ps1` - PowerShell alternative
- `scripts/observability/validate_observability_e2e.sh` - E2E validation (optional for CP1)

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP1 core profile definition
- Explicit separation of CP1 and CP2+ features
- Synchronization with `OBSERVABILITY_CP1_INVARIANTS.md`
- Component-specific CP1 requirements
- CP1 validation checklist

---

## Priority

This document defines **critical priority** CP1 observability core profile. All components must comply with CP1 core requirements. CP2+ features are explicitly excluded from CP1 validation.

