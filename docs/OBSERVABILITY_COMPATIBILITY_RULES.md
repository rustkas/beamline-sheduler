# Observability CP1↔CP2 Compatibility Rules

**Version**: 1.1  
**Last Updated**: 2025-01-27  
**Purpose**: Formally define compatibility rules ensuring CP2 observability changes do not break CP1 contracts  
**Status**: ✅ **ACTIVE**  
**WORKER**: `wrk-obs1` (Observability/Telemetry)

---

## Overview

This document defines **formal compatibility rules** between CP1 and CP2 observability features. It ensures that CP2 extensions (Prometheus metrics, OpenTelemetry tracing) build upon CP1 invariants without breaking existing CP1 contracts.

**Key Principle**: CP2 observability features must be **additive** and **backward compatible**. CP1 invariants must remain unchanged and functional.

**References**:
- `docs/OBSERVABILITY_CP1_CORE_PROFILE.md` - CP1 core profile (mandatory requirements)
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants (cross-cutting requirements)
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - CP2 extension plan (metrics, tracing, dashboards)

---

## Anti-Patterns: What NOT to Do in CP2

**CRITICAL**: The following changes are **FORBIDDEN** in CP2. Violating these rules will break CP1 contracts and cause compatibility issues.

### Log Format Anti-Patterns

- ❌ **DO NOT** change required log field names (`timestamp`, `level`, `component`, `message`)
- ❌ **DO NOT** change log field types (string → number, enum values → other values)
- ❌ **DO NOT** change timestamp format (must remain ISO 8601, UTC, microseconds: `2025-01-27T12:00:00.123456Z`)
- ❌ **DO NOT** move required fields to nested objects (e.g., `{"nested": {"timestamp": "..."}}`)
- ❌ **DO NOT** remove required fields from logs
- ❌ **DO NOT** change CP1 correlation field names (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`)
- ❌ **DO NOT** change CP1 correlation field types or locations (must remain at top level)

### Health Endpoint Anti-Patterns

- ❌ **DO NOT** change health endpoint paths (`/_health` must remain for HTTP, `grpc.health.v1.Health/Check` for gRPC)
- ❌ **DO NOT** change CP1 health endpoint ports (Gateway: 3000, Worker: 9091, Router: 9000)
- ❌ **DO NOT** remove required health response fields (`status`, `timestamp` for HTTP; `SERVING`/`NOT_SERVING`/`UNKNOWN` for gRPC)
- ❌ **DO NOT** change health status enum values (`healthy`, `degraded`, `unhealthy` for HTTP; `SERVING`, `NOT_SERVING`, `UNKNOWN` for gRPC)
- ❌ **DO NOT** make CP1 health endpoints depend on CP2 features (metrics/tracing must be optional)

### PII Filtering Anti-Patterns

- ❌ **DO NOT** remove fields from PII filter list (`password`, `api_key`, `secret`, `token`, etc.)
- ❌ **DO NOT** change PII replacement value (must remain `[REDACTED]`)
- ❌ **DO NOT** disable filtering for CP1 filtered fields

### Validation Script Anti-Patterns

- ❌ **DO NOT** modify CP1 validation checks in `scripts/observability/validate_observability.sh`
- ❌ **DO NOT** change exit codes for CP1 validation (0 = success, 2 = external unavailable, 3 = local invalid)
- ❌ **DO NOT** remove CP1 validation logic

### Dependency Anti-Patterns

- ❌ **DO NOT** make CP1 functionality depend on CP2 features (CP1 must work independently)
- ❌ **DO NOT** require CP2 metrics/tracing for CP1 logs to work
- ❌ **DO NOT** require CP2 endpoints for CP1 health checks to pass

### Context Object Anti-Patterns

- ❌ **DO NOT** remove existing CP1 `context` fields (e.g., `provider`, `policy_id`, `status_code`, `method`, `path`)
- ❌ **DO NOT** change CP1 `context` field types
- ❌ **DO NOT** make CP1 `context` fields required (they must remain optional)

**Summary**: CP2 changes must be **additive only**. All CP1 invariants (field names, types, locations, formats, ports, paths) must remain unchanged. CP1 must work independently of CP2.

---

## CP1 Invariants (Immutable)

### 1. JSON Log Format Structure

**CRITICAL**: The CP1 JSON log format structure **must not be changed** in CP2.

#### Required Fields (Always Present)

**These fields are immutable** and must remain at the top level:

```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",  // ISO 8601, UTC, microseconds
  "level": "ERROR|WARN|INFO|DEBUG",            // Log level
  "component": "router|gateway|worker",        // Component identifier
  "message": "Human-readable message"          // Message text
}
```

**CP2 Changes FORBIDDEN**:
- ❌ **DO NOT** change field names (`timestamp`, `level`, `component`, `message`)
- ❌ **DO NOT** change field types (string, enum values)
- ❌ **DO NOT** change timestamp format (ISO 8601, UTC, microseconds)
- ❌ **DO NOT** move required fields to nested objects
- ❌ **DO NOT** remove required fields

**CP2 Changes ALLOWED**:
- ✅ **ALLOWED**: Add optional fields at top level (e.g., `span_id` for trace correlation)
- ✅ **ALLOWED**: Extend `context` object with new fields
- ✅ **ALLOWED**: Add new optional top-level fields (must be optional, not required)

#### CP1 Correlation Fields (When Context Available)

**These fields are immutable** when context is available:

```json
{
  "tenant_id": "tenant_123",      // When tenant context available
  "run_id": "run_abc123",          // When run context available
  "flow_id": "flow_xyz789",        // When flow context available (Router, Worker)
  "step_id": "step_001",           // When step context available (Worker, Router)
  "trace_id": "trace_def456...",   // When trace context available
  "error_code": "ERROR_CODE",      // When ERROR level
  "latency_ms": 250                // Optional performance tracking
}
```

**CP2 Changes FORBIDDEN**:
- ❌ **DO NOT** change field names (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`)
- ❌ **DO NOT** change field types (all strings, except `latency_ms` which is number)
- ❌ **DO NOT** change field locations (must remain at top level, not in nested objects)
- ❌ **DO NOT** make optional fields required
- ❌ **DO NOT** remove fields

**CP2 Changes ALLOWED**:
- ✅ **ALLOWED**: Add optional CP2 fields at top level (e.g., `span_id`, `parent_span_id`)
- ✅ **ALLOWED**: Extend `context` object with CP2-specific fields
- ✅ **ALLOWED**: Add new optional correlation fields (must be optional)

#### Context Object (Optional Structured Data)

**The `context` object structure is extensible**:

```json
{
  "context": {
    // CP1 fields (component-specific)
    "provider": "openai",
    "policy_id": "default",
    "status_code": 200,
    "method": "POST",
    "path": "/api/v1/messages",
    
    // CP2 fields (allowed additions)
    "span_id": "span_abc123",           // CP2: OpenTelemetry span ID
    "parent_span_id": "span_def456",    // CP2: Parent span ID
    "metric_labels": {...}              // CP2: Prometheus label hints
  }
}
```

**CP2 Changes FORBIDDEN**:
- ❌ **DO NOT** remove existing CP1 `context` fields
- ❌ **DO NOT** change CP1 `context` field types
- ❌ **DO NOT** make CP1 `context` fields required

**CP2 Changes ALLOWED**:
- ✅ **ALLOWED**: Add new fields to `context` object
- ✅ **ALLOWED**: Extend `context` with CP2-specific fields
- ✅ **ALLOWED**: Add nested objects in `context` (e.g., `context.metrics`, `context.trace`)

### 2. Health Endpoint Contracts

**CRITICAL**: CP1 health endpoint contracts **must not be changed** in CP2.

#### HTTP Health Endpoints (Gateway, Worker)

**CP1 Contract** (immutable):

**Path**: `GET /_health`  
**Port**: Gateway (3000), Worker (9091)  
**Response Format**:
```json
{
  "status": "healthy|degraded|unhealthy",
  "timestamp": "2025-01-27T12:00:00.123456Z"
}
```

**CP2 Changes FORBIDDEN**:
- ❌ **DO NOT** change path (`/_health` must remain)
- ❌ **DO NOT** change port (Gateway: 3000, Worker: 9091)
- ❌ **DO NOT** change required fields (`status`, `timestamp`)
- ❌ **DO NOT** change field types (`status` is string enum, `timestamp` is ISO 8601)
- ❌ **DO NOT** change status values (`healthy`, `degraded`, `unhealthy`)
- ❌ **DO NOT** remove required fields

**CP2 Changes ALLOWED**:
- ✅ **ALLOWED**: Add optional fields to response (e.g., `checks`, `metrics`, `traces`)
- ✅ **ALLOWED**: Extend response with CP2-specific information
- ✅ **ALLOWED**: Add new health endpoints on different ports (e.g., `/metrics` on port 3001)

**Example CP2 Extension** (backward compatible):
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "checks": {
    "database": {"status": "ok"},
    "nats": {"status": "ok"}
  },
  "metrics": {
    "prometheus": {"status": "ok", "endpoint": "/metrics"}
  },
  "traces": {
    "opentelemetry": {"status": "ok", "endpoint": "/traces"}
  }
}
```

#### gRPC Health Endpoint (Router)

**CP1 Contract** (immutable):

**Service**: `grpc.health.v1.Health/Check`  
**Port**: 9000  
**Response**: gRPC health status (`SERVING`, `NOT_SERVING`, `UNKNOWN`)

**CP2 Changes FORBIDDEN**:
- ❌ **DO NOT** change service name (`grpc.health.v1.Health/Check`)
- ❌ **DO NOT** change port (9000)
- ❌ **DO NOT** change response format (gRPC health status)
- ❌ **DO NOT** change status values (`SERVING`, `NOT_SERVING`, `UNKNOWN`)

**CP2 Changes ALLOWED**:
- ✅ **ALLOWED**: Add new gRPC services (e.g., metrics service)
- ✅ **ALLOWED**: Add new HTTP endpoints on different ports (e.g., `/metrics` on port 9001)

### 3. PII/Secret Filtering

**CRITICAL**: CP1 PII/secret filtering rules **must not be changed** in CP2.

#### Filtered Fields (Immutable)

**CP1 Filtered Fields** (must remain filtered):
- `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`
- `authorization`, `credit_card`, `ssn`, `email`, `phone`

**Replacement**: `[REDACTED]`

**CP2 Changes FORBIDDEN**:
- ❌ **DO NOT** remove fields from filter list
- ❌ **DO NOT** change replacement value (`[REDACTED]`)
- ❌ **DO NOT** disable filtering for CP1 fields

**CP2 Changes ALLOWED**:
- ✅ **ALLOWED**: Add new fields to filter list (e.g., `session_token`, `api_secret`)
- ✅ **ALLOWED**: Extend filtering to metrics/traces (CP2-specific)

### 4. Validation Scripts

**CRITICAL**: CP1 validation scripts **must continue to work** in CP2.

#### CP1 Validation Scripts (Must Pass)

**Primary Script**: `scripts/observability/validate_observability.sh`

**CP1 Checks** (must remain functional):
1. Logging configuration validation (`config/observability/logging.json`)
2. JSON log format validation
3. CP1 correlation fields validation (when context available)
4. Health endpoint availability and format
5. PII/secret detection in logs

**Exit Codes** (immutable):
- `0` - Success (may have warnings)
- `2` - External endpoints unavailable (services not running)
- `3` - Local configs missing or invalid

**CP2 Changes FORBIDDEN**:
- ❌ **DO NOT** change CP1 validation checks
- ❌ **DO NOT** change exit codes for CP1 checks
- ❌ **DO NOT** remove CP1 validation logic

**CP2 Changes ALLOWED**:
- ✅ **ALLOWED**: Add new CP2 validation scripts (e.g., `validate_metrics.sh`, `validate_traces.sh`)
- ✅ **ALLOWED**: Extend existing scripts with optional CP2 checks
- ✅ **ALLOWED**: Add CP2-specific validation (must not affect CP1 validation)

---

## CP2 Extensions (Additive)

### 1. Prometheus Metrics Endpoints

**CP2 Addition**: New HTTP `/metrics` endpoints on separate ports.

#### New Endpoints (CP2)

**Component-Specific Endpoints**:

| Component | Protocol | Path | Port | CP1 Port |
|-----------|----------|------|------|----------|
| Router | HTTP | `GET /metrics` | 9001 | 9000 (gRPC) |
| Gateway | HTTP | `GET /metrics` | 3001 | 3000 (HTTP) |
| Worker | HTTP | `GET /metrics` | 9092 | 9091 (HTTP) |

**Key Principle**: CP2 metrics endpoints are **separate** from CP1 health endpoints. No conflicts.

#### Metrics Format

**Prometheus Exposition Format** (standard):
```
# HELP http_requests_total Total HTTP requests
# TYPE http_requests_total counter
http_requests_total{component="gateway",method="POST",path="/api/v1/messages",status_code="200",tenant_id="tenant_123"} 42
```

**CP1 Field Mapping**:
- CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `step_id`) → Prometheus labels
- CP1 log fields (`component`, `context.method`, `context.path`) → Prometheus labels
- CP1 `latency_ms` → Prometheus histogram values (converted to seconds)

**Compatibility Guarantee**:
- ✅ Metrics endpoints do not affect CP1 health endpoints
- ✅ Metrics endpoints do not affect CP1 log format
- ✅ Metrics use CP1 correlation fields as labels (backward compatible)

### 2. OpenTelemetry Tracing

**CP2 Addition**: Distributed tracing with context propagation.

#### Trace Context Propagation

**CP1 `trace_id` Format**: W3C Trace Context format
- Format: `{trace_id}-{span_id}-{flags}`
- Example: `4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01`

**CP2 Enhancement**:
- CP1 `trace_id` → OTEL trace context
- CP1 correlation fields → OTEL span attributes
- CP1 logs → OTEL span events (optional)

**Compatibility Guarantee**:
- ✅ Tracing does not affect CP1 log format
- ✅ Tracing does not affect CP1 health endpoints
- ✅ CP1 `trace_id` format is preserved and used by OTEL

#### Span Attributes

**CP1 Fields → OTEL Attributes** (mapping):

| CP1 Field | OTEL Attribute | Type | Description |
|-----------|----------------|------|-------------|
| `trace_id` | `trace.trace_id` | string | W3C Trace Context trace ID |
| `run_id` | `run.id` | string | Run identifier |
| `flow_id` | `flow.id` | string | Flow identifier |
| `step_id` | `step.id` | string | Step identifier |
| `tenant_id` | `tenant.id` | string | Tenant identifier |
| `component` | `service.name` | string | Component name |
| `error_code` | `error.code` | string | Error code (for error spans) |
| `latency_ms` | `duration_ms` | int64 | Duration in milliseconds |

**Compatibility Guarantee**:
- ✅ CP1 fields are preserved in OTEL spans
- ✅ CP1 log format unchanged
- ✅ CP1 health endpoints unchanged

### 3. Log Enhancements (Optional)

**CP2 Addition**: Optional log fields for trace correlation.

#### Optional CP2 Fields

**New Optional Fields** (CP2, not required):

```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "router",
  "message": "Routing decision made",
  
  // CP1 fields (when context available)
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "trace_id": "trace_def456...",
  
  // CP2 optional fields (for trace correlation)
  "span_id": "span_abc123",           // Optional: OTEL span ID
  "parent_span_id": "span_def456",     // Optional: Parent span ID
  
  "context": {
    // CP1 context fields
    "provider": "openai",
    "policy_id": "default",
    
    // CP2 context fields (optional)
    "trace.sampled": true,             // Optional: Trace sampling decision
    "trace.flags": "01"                // Optional: Trace flags
  }
}
```

**Compatibility Guarantee**:
- ✅ CP2 fields are **optional** (not required)
- ✅ CP1 validation scripts ignore unknown fields
- ✅ CP1 log format unchanged (CP2 fields are additions)

---

## Rollback Strategy

### CP2 Feature Disablement

**CRITICAL**: CP2 features must be **disableable** without affecting CP1 functionality.

#### Disablement Mechanisms

**1. Configuration-Based Disablement**:

**Router** (Erlang/OTP):
```erlang
% config/sys.config
[
  {router, [
    {observability, [
      {metrics_enabled, false},      % Disable Prometheus metrics
      {tracing_enabled, false}       % Disable OpenTelemetry tracing
    ]}
  ]}
].
```

**Gateway** (C):
```c
// config/observability.h
#define METRICS_ENABLED 0  // Disable Prometheus metrics
#define TRACING_ENABLED 0  // Disable OpenTelemetry tracing
```

**Worker** (C++):
```cpp
// config/observability.hpp
constexpr bool METRICS_ENABLED = false;  // Disable Prometheus metrics
constexpr bool TRACING_ENABLED = false;  // Disable OpenTelemetry tracing
```

**2. Runtime Disablement**:

**Environment Variables**:
```bash
export BEAMLINE_METRICS_ENABLED=false
export BEAMLINE_TRACING_ENABLED=false
```

**3. Feature Flags**:

**Component-Specific Flags**:
- Router: `router.metrics.enabled`, `router.tracing.enabled`
- Gateway: `gateway.metrics.enabled`, `gateway.tracing.enabled`
- Worker: `worker.metrics.enabled`, `worker.tracing.enabled`

#### Rollback Verification

**After Disabling CP2 Features**:

1. **CP1 Logs**: Must continue to work (structured JSON, CP1 fields)
2. **CP1 Health Endpoints**: Must continue to work (`/_health` on CP1 ports)
3. **CP1 Validation**: Must pass (`scripts/observability/validate_observability.sh`)
4. **CP2 Endpoints**: Must be unavailable (`/metrics` endpoints return 404 or not listening)

**Verification Script**:
```bash
# Verify CP1 functionality after CP2 disablement
bash scripts/observability/validate_observability.sh

# Verify CP2 endpoints are disabled
curl http://localhost:9001/metrics  # Should return 404 or connection refused
curl http://localhost:3001/metrics  # Should return 404 or connection refused
curl http://localhost:9092/metrics  # Should return 404 or connection refused
```

### CP2 Dependency Isolation

**CRITICAL**: CP2 features must not create **hard dependencies** on CP1 functionality.

#### Dependency Rules

**CP2 → CP1 Dependencies** (allowed):
- ✅ CP2 metrics use CP1 correlation fields as labels
- ✅ CP2 traces use CP1 `trace_id` format
- ✅ CP2 logs include CP1 fields

**CP1 → CP2 Dependencies** (forbidden):
- ❌ **DO NOT** require CP2 features for CP1 functionality
- ❌ **DO NOT** make CP1 logs depend on CP2 metrics/tracing
- ❌ **DO NOT** make CP1 health endpoints depend on CP2 features

**Isolation Guarantee**:
- ✅ CP1 can work **independently** of CP2
- ✅ CP2 can be **disabled** without breaking CP1
- ✅ CP2 can be **removed** without affecting CP1

---

## Compatibility Matrix

### CP1 Features → CP2 Compatibility

| CP1 Feature | CP2 Change | Compatibility | Notes |
|-------------|------------|---------------|-------|
| JSON log format | Add optional fields | ✅ Compatible | CP2 fields are optional |
| CP1 correlation fields | Use as labels/attributes | ✅ Compatible | CP2 uses CP1 fields |
| Health endpoints | Add optional fields | ✅ Compatible | CP2 extends response |
| Health endpoints | New `/metrics` endpoints | ✅ Compatible | Separate ports |
| PII filtering | Extend filter list | ✅ Compatible | CP2 adds new fields |
| Validation scripts | Add CP2 checks | ✅ Compatible | CP1 checks unchanged |

### CP2 Features → CP1 Compatibility

| CP2 Feature | CP1 Impact | Compatibility | Notes |
|-------------|-----------|---------------|-------|
| Prometheus metrics | None | ✅ Compatible | Separate endpoints |
| OpenTelemetry tracing | None | ✅ Compatible | Uses CP1 `trace_id` |
| Log enhancements | None | ✅ Compatible | Optional fields only |
| Dashboards | None | ✅ Compatible | External tooling |
| Alerting | None | ✅ Compatible | External tooling |

---

## Validation and Testing

### CP1 Validation (Must Pass)

**After CP2 Implementation**:

1. **CP1 Log Format Validation**:
   ```bash
   bash scripts/observability/validate_observability.sh
   # Must pass with exit code 0
   ```

2. **CP1 Health Endpoint Validation**:
   ```bash
   curl http://localhost:3000/_health  # Gateway
   curl http://localhost:9091/_health  # Worker
   grpc_health_probe -addr=localhost:9000  # Router
   # Must return CP1-compliant format
   ```

3. **CP1 Correlation Fields Validation**:
   ```bash
   # Verify CP1 fields are present in logs when context available
   grep -q "tenant_id" logs/router.log
   grep -q "run_id" logs/gateway.log
   grep -q "flow_id" logs/worker.log
   ```

### CP2 Validation (Optional)

**CP2-Specific Validation**:

1. **Metrics Endpoint Validation**:
   ```bash
   curl http://localhost:9001/metrics  # Router
   curl http://localhost:3001/metrics  # Gateway
   curl http://localhost:9092/metrics  # Worker
   # Must return Prometheus format
   ```

2. **Tracing Validation**:
   ```bash
   bash scripts/observability/validate_traces.sh
   # Must verify trace export and context propagation
   ```

### Rollback Testing

**Test CP2 Disablement**:

1. **Disable CP2 Features**:
   ```bash
   export BEAMLINE_METRICS_ENABLED=false
   export BEAMLINE_TRACING_ENABLED=false
   ```

2. **Verify CP1 Still Works**:
   ```bash
   bash scripts/observability/validate_observability.sh
   # Must pass (CP1 functionality preserved)
   ```

3. **Verify CP2 Disabled**:
   ```bash
   curl http://localhost:9001/metrics  # Should return 404
   # CP2 endpoints must be unavailable
   ```

---

## Migration Guide

### CP1 → CP2 Migration

**Step 1: Deploy CP2 Features** (additive):
- Deploy components with CP2 features enabled
- CP1 features continue to work
- CP2 features are optional

**Step 2: Enable CP2 Features** (gradual):
- Enable metrics export per component
- Enable tracing per component
- Monitor CP1 functionality

**Step 3: Validate Compatibility**:
- Run CP1 validation scripts (must pass)
- Verify CP2 features work independently
- Test rollback (disable CP2, verify CP1)

### CP2 → CP1 Rollback

**Step 1: Disable CP2 Features**:
- Set `METRICS_ENABLED=false`
- Set `TRACING_ENABLED=false`
- Restart components

**Step 2: Verify CP1 Functionality**:
- Run CP1 validation scripts
- Verify health endpoints
- Verify log format

**Step 3: Remove CP2 Dependencies** (if needed):
- Remove CP2 endpoints (optional)
- Remove CP2 configuration (optional)
- CP1 must work independently

---

## Enforcement

### Pre-Deployment Checks

**Before Deploying CP2 Features**:

1. ✅ **CP1 Validation Must Pass**: `scripts/observability/validate_observability.sh`
2. ✅ **CP1 Health Endpoints Must Work**: All CP1 endpoints functional
3. ✅ **CP1 Log Format Must Be Valid**: JSON format, required fields present
4. ✅ **CP1 Correlation Fields Must Work**: Fields present when context available

### Post-Deployment Checks

**After Deploying CP2 Features**:

1. ✅ **CP1 Validation Still Passes**: CP1 functionality preserved
2. ✅ **CP2 Features Are Optional**: Can be disabled without affecting CP1
3. ✅ **CP2 Endpoints Are Separate**: No conflicts with CP1 endpoints
4. ✅ **Rollback Works**: CP2 can be disabled, CP1 continues to work

### CI/CD Gates

**Required Checks**:

1. **CP1 Compatibility Gate**:
   ```bash
   bash scripts/observability/validate_observability.sh
   # Must pass (exit code 0)
   ```

2. **CP2 Rollback Gate** (if CP2 features enabled):
   ```bash
   # Disable CP2 features
   export BEAMLINE_METRICS_ENABLED=false
   export BEAMLINE_TRACING_ENABLED=false
   
   # Verify CP1 still works
   bash scripts/observability/validate_observability.sh
   # Must pass (exit code 0)
   ```

---

## Summary

### CP1 Invariants (Immutable)

✅ **JSON Log Format**: Required fields (`timestamp`, `level`, `component`, `message`) must remain unchanged  
✅ **CP1 Correlation Fields**: Field names, types, locations must remain unchanged  
✅ **Health Endpoints**: Paths, ports, response formats must remain unchanged  
✅ **PII Filtering**: Filtered fields and replacement value must remain unchanged  
✅ **Validation Scripts**: CP1 checks must continue to work

### CP2 Extensions (Additive)

✅ **Prometheus Metrics**: New `/metrics` endpoints on separate ports  
✅ **OpenTelemetry Tracing**: Uses CP1 `trace_id` format, adds span attributes  
✅ **Log Enhancements**: Optional fields for trace correlation  
✅ **Dashboards/Alerting**: External tooling, no impact on CP1

### Rollback Strategy

✅ **Configuration-Based**: Environment variables, feature flags  
✅ **Runtime Disablement**: CP2 features can be disabled without restart  
✅ **Dependency Isolation**: CP1 works independently of CP2  
✅ **Verification**: CP1 validation must pass after CP2 disablement

---

## References

- `docs/OBSERVABILITY_CP1_CORE_PROFILE.md` - CP1 core profile
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - CP2 extension plan
- `docs/ARCHITECTURE/compatibility-rules.md` - General compatibility rules
- `scripts/observability/validate_observability.sh` - CP1 validation script

---

## Change History

**v1.1 (2025-01-27)**:
- Added Anti-Patterns section with explicit forbidden changes for CP2
- Organized anti-patterns by category (Log Format, Health Endpoints, PII Filtering, Validation Scripts, Dependencies, Context Object)
- Added summary statement emphasizing additive-only changes

**v1.0 (2025-01-27)**:
- Initial CP1↔CP2 observability compatibility rules
- CP1 invariants definition (immutable)
- CP2 extensions definition (additive)
- Rollback strategy and verification procedures
- Compatibility matrix and migration guide

---

## Priority

This document defines **critical priority** compatibility rules for CP1↔CP2 observability. All CP2 changes must comply with these rules to ensure CP1 contracts are not broken.

