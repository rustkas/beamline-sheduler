# Observability

## Observability Canon

This document defines the unified observability canon for BeamLine components, structured into tiers to ensure clarity across development phases.

### CP1 Baseline (Minimum Viable Observability)
**Goal**: Operational visibility for debugging and basic health checks.
- **JSON Structured Logs**: Mandatory for all components (Router, Gateway, Worker).
- **Health Endpoints**: `/_health` (HTTP) or gRPC health checks.
- **Correlation IDs**: `trace_id`, `tenant_id` propagated where available.

### CP2+ Expansion (Advanced Observability)
**Goal**: Performance monitoring, distributed tracing, and production readiness.
- **Prometheus Metrics**: High-detail metrics (histograms, counters) with correlation labels.
- **OpenTelemetry**: End-to-end tracing with context propagation (OTLP).
- **Dashboards**: Grafana visualizations for component health and error rates.

---

This document defines unified observability requirements for BeamLine components (Router, Gateway, Worker, Ingress).

## CP2 Pre‑Release Status
- Extended observability is tracked under CP2/Pre‑release.
- Full OpenTelemetry: end‑to‑end span creation and context propagation, OTLP export via collector.
- High‑detail Prometheus metrics: rich counters/histograms with CP1 correlation labels and controlled cardinality.
- Complex Grafana dashboards: component, error, health, tracing views with CP1 filters.
- References: `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md`, `docs/OBSERVABILITY_CP2_TEST_PROFILE.md`, `docs/observability/*`.

**CP1 Observability Invariants**: See `docs/OBSERVABILITY_CP1_INVARIANTS.md` for mandatory CP1 cross-cutting observability requirements.

**Key Requirements**:
- Structured logging with required fields and sanitization of sensitive data.
- Health endpoints for all services with a unified response format.
- CP1 correlation fields (tenant_id, run_id, flow_id, step_id, trace_id) when context is available.
- Metrics and tracing best practices for integration.

## Logging

### Requirements

All components must produce structured JSON logs with the following fields:

**Required fields (always present):**
- `timestamp` (ISO 8601)
- `level` (`DEBUG` | `INFO` | `WARN` | `ERROR`)
- `component` (e.g., `router`, `gateway`, `worker`, `provider:<name>`)
- `message` (human-readable text)

**Optional fields:**
- `context` (object with key/value pairs)

**CP1 Cross-Cutting Invariants (required when context is available):**

**CRITICAL**: These fields are **mandatory CP1 observability invariants**. All components must include these fields in logs when the corresponding context is available.

**Required Fields** (when context available):
- `tenant_id` - **Required when tenant context is available** (all components)
- `run_id` - **Required when run context is available** (Router, Worker, Gateway)
- `flow_id` - **Required when flow context is available** (Router, Worker)
- `step_id` - **Required when step context is available** (Worker, Router)
- `trace_id` - **Required when trace context is available** (all components for distributed tracing)

**Optional Fields**:
- `error_code` - **Optional** (for ERROR level, structured error handling)
- `latency_ms` - **Optional** (for performance tracking)

**Component-Specific Requirements**:
- **Router**: Must include `tenant_id`, `run_id`, `flow_id`, `trace_id` when available
- **Gateway**: Must include `tenant_id`, `run_id`, `trace_id` when available
- **Worker**: Must include `tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id` when available

**Validation**: See `docs/OBSERVABILITY_CP1_INVARIANTS.md` for complete CP1 invariants specification and validation procedures.

### CP1 Fields Examples by Component

#### Router Component Examples

**Example 1: Routing decision with full CP1 context:**
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

**Example 2: Policy load with CP1 fields:**
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
    "latency_ms": 2
  }
}
```

**Example 3: Error with CP1 fields:**
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

#### Gateway Component Examples

**Example 1: Request processing with CP1 fields:**
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

**Example 2: Authentication with CP1 fields:**
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "gateway",
  "message": "Authentication successful",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "context": {
    "user_id": "user_123",
    "api_key": "[REDACTED]"
  }
}
```

#### Worker Component Examples

**Example 1: Step execution with full CP1 context:**
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
    "status": "success",
    "response_code": 200
  }
}
```

**Example 2: Step error with CP1 fields:**
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "ERROR",
  "component": "worker",
  "message": "Step execution failed",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "step_id": "step_001",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "error_code": "WORKER_STEP_EXECUTION_FAILED",
  "context": {
    "block_type": "http.request",
    "error": "connection_timeout"
  }
}
```

### Levels

- `DEBUG`: development details; disabled in production by default
- `INFO`: normal operational messages
- `WARN`: pre-error conditions, degradation warnings
- `ERROR`: errors and failures

### Sanitization

Sensitive fields must be filtered out from logs.

- Fields to sanitize: `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`, `authorization`, `credit_card`, `ssn`, `email`, `phone`
- Replacement: `[REDACTED]`

Example:

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

## Health Endpoint

### Requirements

All components must expose health endpoints for status checks.

**For HTTP components** (Gateway, Worker, Ingress):
- Path: `GET /_health` or `GET /health`
- HTTP status: `200 OK` on success
- Content-Type: `application/json`
- Response must include `status` and `timestamp`

**For gRPC components** (Router):
- Protocol: gRPC
- Service: `grpc.health.v1.Health/Check`
- Port: 9000
- Response: Protobuf with `status` enum

**Detailed documentation**: See `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md` for component-specific requirements.

Response format:

```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z",
  "checks": {
    "database": {
      "status": "ok",
      "message": "Connection successful"
    },
    "cache": {
      "status": "ok",
      "message": "Cache operational"
    }
  }
}
```

Statuses:

- `healthy`: service is operating normally
- `degraded`: service is operating with limitations
- `unhealthy`: service is unavailable or critically failing

Fields:

- `status` (string, required): overall service status
- `timestamp` (string, required): check timestamp (ISO 8601)
- `checks` (object, optional): detailed component checks

### Components and Ports

**CP1 Components Health Endpoints:**

See `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md` for detailed component-specific health endpoint documentation.

**Quick Reference:**

- **Router (Erlang/OTP)**: gRPC health check on port 9000 (`grpc_health_probe -addr=localhost:9000`)
- **Gateway (C-Gateway)**: HTTP `GET /_health` on port 3000
- **Worker CAF (C++/CAF)**: HTTP `GET /_health` (port TBD, check component config)
- **Ingress (Python)**: HTTP `GET /_health` (liveness), `GET /_readyz` (readiness)

**Validation**: 
- Use `scripts/observability/validate_observability.sh` for automated health endpoint validation
- See `docs/OBSERVABILITY_CP1_INVARIANTS.md` for complete CP1 observability invariants and validation procedures

## Router Observability Dashboard

For Router-specific observability dashboards focusing on DevState/CP health, fallback monitoring, and NATS contract violations, see:

- **Router Dashboard Specification**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`

## Metrics Monitoring for Load/Chaos/Rate-Limit Scenarios

For comprehensive guidance on monitoring Router and Gateway metrics during load tests, chaos tests, and rate limiting scenarios, see:

- **Metrics Monitoring Guide**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`
  - Complete metric catalog for Router intake and Gateway rate limiting
  - Monitoring scenarios for load/chaos/rate-limit tests
  - Dashboard panel descriptions and Prometheus queries
  - Health indicators and alert thresholds
  - DevState/CP health panels
  - NATS contract violation monitoring
  - Router summary metrics
  - Example PromQL queries and alert rules

## Metrics and Tracing (Summary)

- Use Prometheus-compatible metrics with stable naming and low cardinality.
- Prefer labels like `service`, `provider`, and error type; avoid `correlation_id` in metrics.
- Export traces with OpenTelemetry; include `x-correlation-id` in spans and logs.

CP2/Pre‑release expands this summary to full implementation across components. See the CP2 plan and test profile for acceptance criteria and validation.

See `docs/OBSERVABILITY_DETAILS.md` for extended guidance if present.

## OBS-1 Log Sources

For OBS-1 conformance validation (JSON logs), the project uses JSON Lines (`.jsonl`) files under `reports/dry-run-logs/obs1/` during development:

- `reports/dry-run-logs/obs1/gateway.jsonl` — Gateway sample structured logs
- `reports/dry-run-logs/obs1/router.jsonl` — Router sample structured logs

Each log line includes required keys used by validators:

- `timestamp`, `level`, `component`, `message`, `context` (schema-aligned)
- `trace_id`, `tenant_id`, and `msg` for tool-based validators

Validation can be executed via:

- `node scripts/obs1_node_validate.mjs <log_file>`
- `./scripts/obs1_jq_validate.sh <log_file>`
- `python3 tests/utils/log_conformance_validator.py --paths reports/dry-run-logs/obs1/*.jsonl`

### Validator Bundle

For convenience, a wrapper script is available to collect logs and run all validators, writing summaries to `.windsurf/reports/`:

- `bash scripts/observability/run_obs1_validators.sh --collect`
- `bash scripts/observability/run_obs1_validators.sh --paths "reports/dry-run-logs/obs1/*.jsonl" "./.windsurf/reports/*.jsonl"`

This script generates:

- `.windsurf/reports/obs1-node-summary.txt`
- `.windsurf/reports/obs1-jq-summary.txt`
- `.windsurf/reports/obs1-py-summary.txt`

It is also integrated into `scripts/run_checks.sh`, which will execute the validators and include their status in the summary.

## References

- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - **CP1 observability invariants** (mandatory requirements for all components)
- `docs/OBSERVABILITY_CONVENTIONS.md` - Detailed observability conventions
- `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md` - Health endpoint specifications
- `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md` - Metrics monitoring guide for load/chaos/rate-limit scenarios
- `docs/OPS_RUNBOOK_ROUTER_INTAKE.md` - Router intake operations runbook (troubleshooting DLQ, validation errors, NATS failures, backpressure)
- `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md` - Gateway rate limiting operations runbook (configuration, mass 429, abuse detection)
- `config/observability/logging.json` - Log format schema with CP1 fields
- `scripts/observability/validate_observability.sh` - Automated validation script
- `scripts/observability/validate_observability.ps1` - PowerShell validation script
- `scripts/observability/validate_observability_e2e.sh` - E2E validation script
