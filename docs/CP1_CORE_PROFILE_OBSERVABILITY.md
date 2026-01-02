# CP1 Core Profile: Observability

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

**Example**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "router",
  "message": "Routing decision made"
}
```

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
| `trace_id` | Trace context available | All components (distributed tracing) |

**Component-Specific Requirements**:
- **Router**: Must include `tenant_id`, `run_id`, `flow_id`, `trace_id` when available
- **Gateway**: Must include `tenant_id`, `run_id`, `trace_id` when available
- **Worker**: Must include `tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id` when available

**CP1 Format**: CP1 fields must be at **top level** in JSON logs (not in `correlation` object).

**Example (Router with full CP1 context)**:
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
    "provider": "openai",
    "policy_id": "default",
    "decision_reason": "weighted_selection"
  }
}
```

**Example (Gateway with run context)**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "gateway",
  "message": "Request processed successfully",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "latency_ms": 120,
  "context": {
    "method": "POST",
    "path": "/api/v1/messages",
    "status_code": 200
  }
}
```

**Example (Worker with full CP1 context)**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "worker",
  "message": "Step execution completed",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "step_id": "step_001",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "latency_ms": 150,
  "context": {
    "block_type": "http.request",
    "status": "success"
  }
}
```

---

### 3. Health Endpoints

**Requirement**: All components must provide health check endpoints for monitoring and orchestration.

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
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "checks": {
    "database": {
      "status": "ok",
      "message": "Connection successful"
    }
  }
}
```

**Status Values**:
- `healthy` - Service operating normally
- `degraded` - Service operating with limitations
- `unhealthy` - Service unavailable or critically broken

**CP1 Scope**: Basic health endpoint only. No advanced health checks (database, external services) required.

**Validation**: `scripts/observability/validate_observability.sh` (checks health endpoint availability and format)

---

### 4. PII/Secret Filtering

**Requirement**: All components must automatically filter sensitive data before logging.

**Filtered Fields**:
- `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`
- `authorization`, `credit_card`, `ssn`, `email`, `phone`

**Replacement**: `[REDACTED]`

**Example**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "gateway",
  "message": "Authentication successful",
  "context": {
    "user_id": "user_123",
    "api_key": "[REDACTED]"
  }
}
```

**CP1 Scope**: Basic PII filtering only. No advanced filtering rules or custom patterns required.

---

## CP1 Observability Compliance Checklist

### Router (Erlang/OTP)

- [x] Structured JSON logs with required fields
- [x] CP1 correlation fields (tenant_id, run_id, flow_id, trace_id) when available
- [x] gRPC health endpoint on port 9000
- [x] PII/secret filtering
- [x] Validation script compliance

### Gateway (C-Gateway)

- [x] Structured JSON logs with required fields
- [x] CP1 correlation fields (tenant_id, run_id, trace_id) when available
- [x] HTTP `/_health` endpoint on port 3000
- [x] PII/secret filtering
- [x] Validation script compliance

### Worker (CAF/C++)

- [x] Structured JSON logs with required fields
- [x] CP1 correlation fields (tenant_id, run_id, flow_id, step_id, trace_id) when available
- [x] HTTP `/_health` endpoint on port 9091
- [x] PII/secret filtering
- [x] Validation script compliance

---

## CP1 Validation

### Automated Validation Scripts

**Primary Script**: `scripts/observability/validate_observability.sh`

**Checks Performed**:
1. Logging configuration validation (`config/observability/logging.json`)
2. JSON log format validation
3. CP1 invariants validation (correlation fields)
4. Health endpoint availability and format
5. Secret detection in logs

**Exit Codes**:
- `0` - Success (may have warnings)
- `2` - External endpoints unavailable (services not running - expected in local validation)
- `3` - Local configs missing or invalid

**PowerShell Alternative**: `scripts/observability/validate_observability.ps1`

### E2E Validation

**Script**: `scripts/observability/validate_observability_e2e.sh`

**Checks**:
- Health endpoint availability (all components)
- Log format compliance (sample logs)
- Correlation field presence (when context available)
- PII filtering (no secrets in logs)

**Unified CP1 Test Profile**:
```bash
# Run all CP1 observability tests
bash scripts/observability/run_cp1_profile.sh
```

---

## CP2+ Optional/Enhancement Features

### Advanced Observability (CP2+)

**CP2+**:
- 📋 Prometheus metrics endpoint (`/metrics`)
- 📋 OpenTelemetry tracing integration
- 📋 Advanced metrics (histograms, percentiles, custom metrics)
- 📋 Grafana dashboards
- 📋 Alertmanager integration
- 📋 Distributed tracing with context propagation

**Excluded from CP1**:
- ❌ Prometheus metrics endpoint (`/metrics`)
- ❌ OpenTelemetry tracing integration
- ❌ Advanced metrics (histograms, percentiles)
- ❌ Grafana dashboards
- ❌ Alertmanager integration
- ❌ Distributed tracing with context propagation

### Advanced Testing (CP2+)

**CP2+**:
- 📋 Performance tests (latency benchmarks)
- 📋 Load tests (throughput validation)
- 📋 Stress tests (resource exhaustion)
- 📋 Edge case tests (very large inputs, extreme timeouts)
- 📋 Fault injection tests (network failures)

**Excluded from CP1**:
- ❌ Performance tests (latency benchmarks)
- ❌ Load tests (throughput validation)
- ❌ Stress tests (resource exhaustion)
- ❌ Edge case tests (very large inputs, extreme timeouts)
- ❌ Fault injection tests (network failures)

### Advanced Logging (CP2+)

**CP2+**:
- 📋 Log aggregation (Loki, ELK Stack)
- 📋 Log analysis dashboards
- 📋 Advanced log rotation (systemd, logrotate, Docker, Kubernetes)
- 📋 Log retention policies

**Excluded from CP1**:
- ❌ Log aggregation (Loki, ELK Stack)
- ❌ Log analysis dashboards
- ❌ Advanced log rotation (basic file-based logs are sufficient)
- ❌ Log retention policies (basic file-based logs are sufficient)

---

## CP1 Acceptance Criteria

### Functional Requirements

- ✅ All components produce structured JSON logs with required fields
- ✅ All components include CP1 correlation fields when context available
- ✅ All components provide health endpoints (gRPC for Router, HTTP for Gateway/Worker)
- ✅ All components filter PII/secrets before logging
- ✅ All components pass observability validation scripts

### Non-Functional Requirements

- ✅ **Stability**: Observability is consistent and reliable
- ✅ **Predictability**: Log format is consistent across components
- ✅ **Observability**: CP1 fields enable effective tracing
- ✅ **Security**: PII/secrets are filtered before logging

### Test Coverage

- ✅ Log format validation tests
- ✅ CP1 fields extraction tests
- ✅ Health endpoint tests
- ✅ Integration tests for CP1 fields propagation

---

## Integration with CI/CD

**CI Gate**: Observability validation must pass before merge.

**GitHub Actions**: `.github/workflows/validate.yml` includes observability checks

**Local Validation**: Run `bash scripts/observability/validate_observability.sh` before committing

**Unified CP1 Test Profile**: `bash scripts/observability/run_cp1_profile.sh`

---

## Relationship to Other Documents

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
- `scripts/observability/validate_observability.sh` - Validation script
- `scripts/observability/run_cp1_profile.sh` - Unified CP1 test profile script

### CP2 Planning Documents
- `apps/otp/router/docs/OBSERVABILITY_CP2_PLANNING.md` - Router CP2 planning
- `apps/c-gateway/docs/OBSERVABILITY_DASHBOARD.md` - Gateway CP2 dashboard planning
- `apps/caf/processor/docs/OBSERVABILITY_DASHBOARD.md` - Worker CP2 dashboard planning
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_OBSERVABILITY.md` - CP2 observability readiness

### Component Documentation
- `apps/otp/router/docs/OBSERVABILITY.md` - Router observability
- `apps/c-gateway/docs/OBSERVABILITY.md` - Gateway observability
- `apps/caf/processor/docs/OBSERVABILITY.md` - Worker observability

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP1 observability core profile definition
- Structured JSON logging requirements
- CP1 correlation fields specification
- Health endpoint requirements
- PII/secret filtering requirements
- CP1 vs CP2+ separation

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP1-LC  
**Status**: Core Profile Definition

