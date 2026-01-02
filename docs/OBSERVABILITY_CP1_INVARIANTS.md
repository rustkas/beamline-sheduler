# CP1 Observability Invariants

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Purpose**: Define CP1 observability invariants as cross-cutting system requirements

## Overview

CP1 observability invariants are **mandatory requirements** that all key components (Router, Gateway, Worker) must satisfy. These invariants ensure consistent observability across the entire system and enable effective monitoring, debugging, and tracing.

## Invariant Categories

### 1. Unified JSON Log Format

**Requirement**: All components must produce structured JSON logs with a unified format.

**Required Fields** (always present):
- `timestamp` (ISO 8601, UTC)
- `level` (ERROR, WARN, INFO, DEBUG)
- `component` (router, gateway, worker, provider, usage)
- `message` (human-readable text)

**Schema Definition**: `config/observability/logging.json`

**Validation**: `scripts/observability/validate_observability.sh`

### 2. CP1 Correlation Fields

**Requirement**: All components must include correlation fields in logs when the corresponding context is available.

**CP1 Required Fields** (when context available):

| Field | Required When | Components |
|-------|---------------|------------|
| `tenant_id` | Tenant context available | All components |
| `run_id` | Run context available | Router, Worker, Gateway |
| `flow_id` | Flow context available | Router, Worker |
| `step_id` | Step context available | Worker, Router |
| `trace_id` | Trace context available | All components (distributed tracing) |
| `error_code` | ERROR level | All components (optional, but recommended) |
| `latency_ms` | Performance tracking | All components (optional) |

**Component-Specific Requirements**:

- **Router**: Must include `tenant_id`, `run_id`, `flow_id`, `trace_id` when available
- **Gateway**: Must include `tenant_id`, `run_id`, `trace_id` when available
- **Worker**: Must include `tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id` when available

**Example Logs with CP1 Fields**:

**Router Example (full CP1 context):**
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

**Gateway Example (run context available):**
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

**Worker Example (full CP1 context with step_id):**
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

**Error Example with CP1 fields:**
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "ERROR",
  "component": "router",
  "message": "Failed to load policy",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "error_code": "ROUTER_POLICY_LOAD_FAILED",
  "context": {
    "policy_id": "default",
    "error": "database_connection_failed"
  }
}
```

### 3. Health Endpoints

**Requirement**: All components must provide health check endpoints for monitoring and orchestration.

**Component-Specific Endpoints**:

| Component | Protocol | Path | Port | Verification |
|-----------|----------|------|------|--------------|
| Router | gRPC | `grpc.health.v1.Health/Check` | 9000 | `grpc_health_probe -addr=localhost:9000` |
| Gateway | HTTP | `GET /_health` | 3000 | `curl http://localhost:3000/_health` |
| Worker | HTTP | `GET /_health` | TBD | `curl http://localhost:<port>/_health` |

**Response Format** (HTTP components):
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z",
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

**Validation**: `scripts/observability/validate_observability.sh` (checks health endpoint availability and format)

### 4. PII/Secret Filtering

**Requirement**: All components must automatically filter sensitive data before logging.

**Filtered Fields**:
- `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`
- `authorization`, `credit_card`, `ssn`, `email`, `phone`

**Replacement**: `[REDACTED]`

**Example**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "INFO",
  "component": "gateway",
  "message": "Authentication successful",
  "context": {
    "user_id": "user_123",
    "api_key": "[REDACTED]"
  }
}
```

## Validation

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

## Component Compliance Checklist

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
- [x] HTTP `/_health` endpoint
- [x] PII/secret filtering
- [x] Validation script compliance

## Integration with CI/CD

**CI Gate**: Observability validation must pass before merge.

**GitHub Actions**: `.github/workflows/validate.yml` includes observability checks

**Local Validation**: Run `bash scripts/observability/validate_observability.sh` before committing

## Documentation References

- `docs/OBSERVABILITY.md` - General observability requirements
- `docs/OBSERVABILITY_CONVENTIONS.md` - Detailed conventions
- `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md` - Health endpoint specifications
- `docs/OBSERVABILITY_CP1_TEST_PROFILE.md` - **CP1 observability test profile** (unified test suite)
- `config/observability/logging.json` - Log format schema
- `scripts/observability/validate_observability.sh` - Validation script
- `scripts/observability/run_cp1_profile.sh` - **CP1 test profile runner** (unified test execution)

## Enforcement

**All components must**:
- ✅ Produce structured JSON logs with required fields
- ✅ Include CP1 correlation fields when context is available
- ✅ Provide health endpoints (gRPC for Router, HTTP for others)
- ✅ Filter PII/secrets before logging
- ✅ Pass observability validation scripts

**Violations**:
- Missing correlation fields → Validation fails (exit code 3)
- Invalid log format → Validation fails (exit code 3)
- Health endpoint unavailable → Warning (exit code 2, expected if services not running)
- Secrets in logs → Validation fails (exit code 3)

## Priority

This document defines **critical priority** observability invariants for CP1. All components must comply with these requirements.

## Change History

**v1.0 (2025-01-27)**:
- Initial CP1 observability invariants definition
- Unified JSON log format requirements
- CP1 correlation fields specification
- Health endpoint requirements
- PII/secret filtering requirements
- Validation procedures

