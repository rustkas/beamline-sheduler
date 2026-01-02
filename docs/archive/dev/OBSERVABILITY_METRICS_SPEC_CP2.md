# Prometheus Metrics Specification (CP2)

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: 📋 **SPECIFICATION DOCUMENT** (CP2)  
**Control Point**: CP2-LC  
**WORKER**: `wrk-obs1` (Observability CP2)  
**Current Phase**: CP1 (Complete) → CP2 (Planned)

---

## Purpose

This document defines the **strict specification** for Prometheus metrics in CP2, including:
- Complete list of metrics (names, types: counter/gauge/histogram)
- Label definitions (tenant_id, run_id, flow_id, step_id, status, error_code, etc.)
- Cardinality invariants and constraints

**Input Documents**:
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - High-level CP2 extension plan
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 correlation fields

**Key Principle**: All CP2 metrics must use CP1 correlation fields as labels/dimensions when available.

---

## CP1 Correlation Fields Reference

**CP1 Required Fields** (when context available):

| Field | Required When | Components | Cardinality |
|-------|---------------|------------|-------------|
| `tenant_id` | Tenant context available | All components | **Medium** (10-1000 tenants) |
| `run_id` | Run context available | Router, Worker, Gateway | **High** (unique per run) |
| `flow_id` | Flow context available | Router, Worker | **Medium** (unique per flow) |
| `step_id` | Step context available | Worker, Router | **Very High** (unique per step) |
| `trace_id` | Trace context available | All components | **Very High** (unique per trace) |
| `error_code` | ERROR level | All components | **Low** (limited error codes) |
| `latency_ms` | Performance tracking | All components | N/A (metric value, not label) |

**Cardinality Guidelines**:
- **Low**: < 100 unique values (safe for all metrics)
- **Medium**: 100-10,000 unique values (use with caution, aggregate when possible)
- **High**: 10,000-1,000,000 unique values (use only in detailed metrics, exclude from aggregates)
- **Very High**: > 1,000,000 unique values (use only in detailed metrics, never in aggregates)

---

## Metric Categories

### 1. HTTP Request Metrics

**Component**: Gateway, Router (HTTP endpoints)

#### 1.1. HTTP Request Rate

**Metric Name**: `http_requests_total`  
**Type**: `counter`  
**Description**: Total number of HTTP requests processed  
**Unit**: requests

**Labels**:
- `component` (string, required): Component name (`gateway`, `router`)
- `method` (string, required): HTTP method (`GET`, `POST`, `PUT`, `DELETE`, `PATCH`)
- `path` (string, required): HTTP path (normalized, e.g., `/api/v1/messages`)
- `status_code` (string, required): HTTP status code (`200`, `400`, `500`, etc.)
- `tenant_id` (string, optional): Tenant identifier (when available, **medium cardinality**)

**Cardinality Constraints**:
- `component`: Low (2 values)
- `method`: Low (5-10 values)
- `path`: Medium (10-100 normalized paths)
- `status_code`: Low (10-20 values)
- `tenant_id`: Medium (10-1000 tenants)

**Estimated Cardinality**: ~10,000-100,000 series (with tenant_id)

**Example**:
```prometheus
http_requests_total{component="gateway", method="POST", path="/api/v1/messages", status_code="200", tenant_id="tenant_123"} 1234
```

#### 1.2. HTTP Request Duration

**Metric Name**: `http_request_duration_seconds`  
**Type**: `histogram`  
**Description**: HTTP request duration in seconds  
**Unit**: seconds

**Labels**:
- `component` (string, required): Component name (`gateway`, `router`)
- `method` (string, required): HTTP method (`GET`, `POST`, `PUT`, `DELETE`, `PATCH`)
- `path` (string, required): HTTP path (normalized)
- `status_code` (string, required): HTTP status code
- `tenant_id` (string, optional): Tenant identifier (when available)

**Histogram Buckets**: `[0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]` seconds

**Cardinality Constraints**: Same as `http_requests_total`

**Example**:
```prometheus
http_request_duration_seconds_bucket{component="gateway", method="POST", path="/api/v1/messages", status_code="200", tenant_id="tenant_123", le="0.1"} 1000
http_request_duration_seconds_bucket{component="gateway", method="POST", path="/api/v1/messages", status_code="200", tenant_id="tenant_123", le="0.5"} 1200
http_request_duration_seconds_sum{component="gateway", method="POST", path="/api/v1/messages", status_code="200", tenant_id="tenant_123"} 45.2
http_request_duration_seconds_count{component="gateway", method="POST", path="/api/v1/messages", status_code="200", tenant_id="tenant_123"} 1234
```

#### 1.3. HTTP Request Size

**Metric Name**: `http_request_size_bytes`  
**Type**: `histogram`  
**Description**: HTTP request body size in bytes  
**Unit**: bytes

**Labels**:
- `component` (string, required): Component name
- `method` (string, required): HTTP method
- `path` (string, required): HTTP path (normalized)
- `tenant_id` (string, optional): Tenant identifier

**Histogram Buckets**: `[128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072]` bytes

**Cardinality Constraints**: Same as `http_requests_total` (without status_code)

**Example**:
```prometheus
http_request_size_bytes_bucket{component="gateway", method="POST", path="/api/v1/messages", tenant_id="tenant_123", le="4096"} 800
```

#### 1.4. HTTP Response Size

**Metric Name**: `http_response_size_bytes`  
**Type**: `histogram`  
**Description**: HTTP response body size in bytes  
**Unit**: bytes

**Labels**:
- `component` (string, required): Component name
- `method` (string, required): HTTP method
- `path` (string, required): HTTP path (normalized)
- `status_code` (string, required): HTTP status code
- `tenant_id` (string, optional): Tenant identifier

**Histogram Buckets**: `[128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288]` bytes

**Cardinality Constraints**: Same as `http_request_duration_seconds`

**Example**:
```prometheus
http_response_size_bytes_bucket{component="gateway", method="POST", path="/api/v1/messages", status_code="200", tenant_id="tenant_123", le="8192"} 1000
```

---

### 2. Routing Metrics

**Component**: Router

#### 2.1. Routing Decisions

**Metric Name**: `router_decisions_total`  
**Type**: `counter`  
**Description**: Total number of routing decisions made  
**Unit**: decisions

**Labels**:
- `policy_id` (string, required): Policy identifier (`default`, `custom_policy_1`, etc.)
- `provider` (string, required): Selected provider (`openai`, `anthropic`, `custom_provider_1`, etc.)
- `decision_reason` (string, required): Decision reason (`weighted_selection`, `fallback`, `circuit_breaker`, `health_check`, etc.)
- `tenant_id` (string, optional): Tenant identifier (when available)
- `run_id` (string, optional): Run identifier (when available, **high cardinality**)

**Cardinality Constraints**:
- `policy_id`: Low (1-50 policies)
- `provider`: Low (5-20 providers)
- `decision_reason`: Low (5-10 reasons)
- `tenant_id`: Medium (10-1000 tenants)
- `run_id`: High (unique per run, **use only in detailed metrics**)

**Estimated Cardinality**: 
- Without `run_id`: ~1,000-10,000 series
- With `run_id`: **Very High** (use only in detailed metrics, exclude from aggregates)

**Example**:
```prometheus
router_decisions_total{policy_id="default", provider="openai", decision_reason="weighted_selection", tenant_id="tenant_123", run_id="run_abc123"} 567
```

#### 2.2. Routing Decision Duration

**Metric Name**: `router_decision_duration_seconds`  
**Type**: `histogram`  
**Description**: Routing decision duration in seconds  
**Unit**: seconds

**Labels**:
- `policy_id` (string, required): Policy identifier
- `provider` (string, required): Selected provider
- `decision_reason` (string, required): Decision reason
- `tenant_id` (string, optional): Tenant identifier
- `run_id` (string, optional): Run identifier (when available, **high cardinality**)

**Histogram Buckets**: `[0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0]` seconds

**Cardinality Constraints**: Same as `router_decisions_total`

**Example**:
```prometheus
router_decision_duration_seconds_bucket{policy_id="default", provider="openai", decision_reason="weighted_selection", tenant_id="tenant_123", run_id="run_abc123", le="0.05"} 500
```

#### 2.3. Provider Selection Count

**Metric Name**: `router_provider_selections_total`  
**Type**: `counter`  
**Description**: Total number of provider selections  
**Unit**: selections

**Labels**:
- `provider` (string, required): Selected provider
- `policy_id` (string, required): Policy identifier
- `tenant_id` (string, optional): Tenant identifier
- `run_id` (string, optional): Run identifier (when available, **high cardinality**)

**Cardinality Constraints**:
- `provider`: Low (5-20 providers)
- `policy_id`: Low (1-50 policies)
- `tenant_id`: Medium (10-1000 tenants)
- `run_id`: High (unique per run, **use only in detailed metrics**)

**Estimated Cardinality**: 
- Without `run_id`: ~500-5,000 series
- With `run_id`: **Very High** (use only in detailed metrics)

**Example**:
```prometheus
router_provider_selections_total{provider="openai", policy_id="default", tenant_id="tenant_123", run_id="run_abc123"} 567
```

#### 2.4. Policy Evaluation Duration

**Metric Name**: `router_policy_evaluation_duration_seconds`  
**Type**: `histogram`  
**Description**: Policy evaluation duration in seconds  
**Unit**: seconds

**Labels**:
- `policy_id` (string, required): Policy identifier
- `tenant_id` (string, optional): Tenant identifier
- `run_id` (string, optional): Run identifier (when available, **high cardinality**)

**Histogram Buckets**: `[0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5]` seconds

**Cardinality Constraints**:
- `policy_id`: Low (1-50 policies)
- `tenant_id`: Medium (10-1000 tenants)
- `run_id`: High (unique per run, **use only in detailed metrics**)

**Example**:
```prometheus
router_policy_evaluation_duration_seconds_bucket{policy_id="default", tenant_id="tenant_123", run_id="run_abc123", le="0.01"} 550
```

---

### 3. Worker Execution Metrics

**Component**: Worker

#### 3.1. Step Executions

**Metric Name**: `worker_step_executions_total`  
**Type**: `counter`  
**Description**: Total number of step executions  
**Unit**: executions

**Labels**:
- `step_type` (string, required): Step type (`llm_call`, `http_request`, `condition`, `transform`, `loop`, etc.)
- `execution_status` (string, required): Execution status (`success`, `error`, `timeout`, `cancelled`)
- `tenant_id` (string, optional): Tenant identifier (when available)
- `run_id` (string, optional): Run identifier (when available, **high cardinality**)
- `flow_id` (string, optional): Flow identifier (when available, **medium cardinality**)
- `step_id` (string, optional): Step identifier (when available, **very high cardinality**)

**Cardinality Constraints**:
- `step_type`: Low (5-20 step types)
- `execution_status`: Low (4-5 statuses)
- `tenant_id`: Medium (10-1000 tenants)
- `run_id`: High (unique per run, **use only in detailed metrics**)
- `flow_id`: Medium (unique per flow, **use only in detailed metrics**)
- `step_id`: Very High (unique per step, **use only in detailed metrics, exclude from aggregates**)

**Estimated Cardinality**: 
- Without high-cardinality labels: ~1,000-10,000 series
- With `run_id`, `flow_id`, `step_id`: **Extremely High** (use only in detailed metrics, never in aggregates)

**Example**:
```prometheus
worker_step_executions_total{step_type="llm_call", execution_status="success", tenant_id="tenant_123", run_id="run_abc123", flow_id="flow_xyz789", step_id="step_001"} 1234
```

#### 3.2. Step Execution Duration

**Metric Name**: `worker_step_execution_duration_seconds`  
**Type**: `histogram`  
**Description**: Step execution duration in seconds  
**Unit**: seconds

**Labels**:
- `step_type` (string, required): Step type
- `execution_status` (string, required): Execution status
- `tenant_id` (string, optional): Tenant identifier
- `run_id` (string, optional): Run identifier (when available, **high cardinality**)
- `flow_id` (string, optional): Flow identifier (when available, **medium cardinality**)
- `step_id` (string, optional): Step identifier (when available, **very high cardinality**)

**Histogram Buckets**: `[0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0, 30.0, 60.0]` seconds

**Cardinality Constraints**: Same as `worker_step_executions_total`

**Example**:
```prometheus
worker_step_execution_duration_seconds_bucket{step_type="llm_call", execution_status="success", tenant_id="tenant_123", run_id="run_abc123", flow_id="flow_xyz789", step_id="step_001", le="1.0"} 1000
```

#### 3.3. Step Errors

**Metric Name**: `worker_step_errors_total`  
**Type**: `counter`  
**Description**: Total number of step execution errors  
**Unit**: errors

**Labels**:
- `step_type` (string, required): Step type
- `error_code` (string, required): Error code (`TIMEOUT`, `NETWORK_ERROR`, `VALIDATION_ERROR`, `PROVIDER_ERROR`, etc.)
- `tenant_id` (string, optional): Tenant identifier
- `run_id` (string, optional): Run identifier (when available, **high cardinality**)
- `flow_id` (string, optional): Flow identifier (when available, **medium cardinality**)
- `step_id` (string, optional): Step identifier (when available, **very high cardinality**)

**Cardinality Constraints**:
- `step_type`: Low (5-20 step types)
- `error_code`: Low (10-50 error codes)
- `tenant_id`: Medium (10-1000 tenants)
- `run_id`: High (unique per run, **use only in detailed metrics**)
- `flow_id`: Medium (unique per flow, **use only in detailed metrics**)
- `step_id`: Very High (unique per step, **use only in detailed metrics, exclude from aggregates**)

**Estimated Cardinality**: 
- Without high-cardinality labels: ~500-5,000 series
- With `run_id`, `flow_id`, `step_id`: **Extremely High** (use only in detailed metrics)

**Example**:
```prometheus
worker_step_errors_total{step_type="llm_call", error_code="TIMEOUT", tenant_id="tenant_123", run_id="run_abc123", flow_id="flow_xyz789", step_id="step_001"} 5
```

#### 3.4. Flow Execution Duration

**Metric Name**: `worker_flow_execution_duration_seconds`  
**Type**: `histogram`  
**Description**: Flow execution duration in seconds  
**Unit**: seconds

**Labels**:
- `tenant_id` (string, optional): Tenant identifier
- `run_id` (string, optional): Run identifier (when available, **high cardinality**)
- `flow_id` (string, optional): Flow identifier (when available, **medium cardinality**)

**Histogram Buckets**: `[0.1, 0.5, 1.0, 2.5, 5.0, 10.0, 30.0, 60.0, 120.0, 300.0]` seconds

**Cardinality Constraints**:
- `tenant_id`: Medium (10-1000 tenants)
- `run_id`: High (unique per run, **use only in detailed metrics**)
- `flow_id`: Medium (unique per flow, **use only in detailed metrics**)

**Estimated Cardinality**: 
- Without `run_id`: ~1,000-10,000 series
- With `run_id`: **High** (use only in detailed metrics)

**Example**:
```prometheus
worker_flow_execution_duration_seconds_bucket{tenant_id="tenant_123", run_id="run_abc123", flow_id="flow_xyz789", le="10.0"} 800
```

---

### 4. Error Metrics

**Component**: All components (Router, Gateway, Worker)

#### 4.1. Component Errors

**Metric Name**: `component_errors_total`  
**Type**: `counter`  
**Description**: Total number of component errors  
**Unit**: errors

**Labels**:
- `component` (string, required): Component name (`router`, `gateway`, `worker`)
- `error_code` (string, required): Error code (component-specific error codes)
- `tenant_id` (string, optional): Tenant identifier (when available)
- `run_id` (string, optional): Run identifier (when available, **high cardinality**)

**Cardinality Constraints**:
- `component`: Low (3 components)
- `error_code`: Low (10-100 error codes per component)
- `tenant_id`: Medium (10-1000 tenants)
- `run_id`: High (unique per run, **use only in detailed metrics, exclude from aggregates**)

**Estimated Cardinality**: 
- Without `run_id`: ~300-3,000 series
- With `run_id`: **Very High** (use only in detailed metrics)

**Example**:
```prometheus
component_errors_total{component="router", error_code="POLICY_LOAD_ERROR", tenant_id="tenant_123", run_id="run_abc123"} 10
```

#### 4.2. Component Error Rate

**Metric Name**: `component_error_rate`  
**Type**: `gauge`  
**Description**: Component error rate (errors per second)  
**Unit**: errors/second

**Labels**:
- `component` (string, required): Component name
- `tenant_id` (string, optional): Tenant identifier (when available)

**Cardinality Constraints**:
- `component`: Low (3 components)
- `tenant_id`: Medium (10-1000 tenants)
- **CRITICAL**: Do NOT include `run_id`, `flow_id`, `step_id` (high-cardinality labels excluded from aggregates)

**Estimated Cardinality**: ~30-3,000 series

**Example**:
```prometheus
component_error_rate{component="router", tenant_id="tenant_123"} 0.5
```

---

### 5. Health Metrics

**Component**: All components (Router, Gateway, Worker)

#### 5.1. Component Health Status

**Metric Name**: `component_health_status`  
**Type**: `gauge`  
**Description**: Component health status (1 = healthy, 0 = unhealthy/degraded)  
**Unit**: status (0 or 1)

**Labels**:
- `component` (string, required): Component name (`router`, `gateway`, `worker`)
- `check` (string, required): Health check name (`database`, `nats`, `memory`, `cpu`, etc.)

**Cardinality Constraints**:
- `component`: Low (3 components)
- `check`: Low (5-20 checks per component)

**Estimated Cardinality**: ~15-60 series

**Example**:
```prometheus
component_health_status{component="router", check="database"} 1
component_health_status{component="router", check="nats"} 1
component_health_status{component="gateway", check="nats"} 0
```

#### 5.2. Health Check Duration

**Metric Name**: `component_health_check_duration_seconds`  
**Type**: `histogram`  
**Description**: Health check duration in seconds  
**Unit**: seconds

**Labels**:
- `component` (string, required): Component name
- `check` (string, required): Health check name

**Histogram Buckets**: `[0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5]` seconds

**Cardinality Constraints**: Same as `component_health_status`

**Example**:
```prometheus
component_health_check_duration_seconds_bucket{component="router", check="database", le="0.01"} 100
```

---

### 6. Extension Metrics

#### 6.1. Extension Invocations

**Metric Name**: `router_extension_invocations_total`  
**Metric Type**: Counter  
**Description**: Total extension invocations by status

**Labels**:
- `extension_id` (string): Extension identifier (e.g., `"normalize_text"`)
- `extension_type` (string): Extension type (`pre`, `validator`, `post`, `provider`)
- `status` (string): Invocation status (`success`, `error`, `timeout`, `max_retries_exceeded`, `circuit_open`)
- `tenant_id` (string, optional): Tenant identifier (when available, Medium cardinality)
- `policy_id` (string, optional): Policy identifier (when available, Low cardinality)

**Cardinality**:
- Without `tenant_id`/`policy_id`: ~1,000-10,000 series (Low-Medium)
- With `tenant_id`/`policy_id`: ~10,000-100,000 series (Medium-High, detailed metrics only)

**Example**:
```
router_extension_invocations_total{extension_id="normalize_text",extension_type="pre",status="success",tenant_id="tenant_123",policy_id="default"} 150
router_extension_invocations_total{extension_id="normalize_text",extension_type="pre",status="timeout",tenant_id="tenant_123",policy_id="default"} 5
```

#### 6.2. Extension Invocation Duration

**Metric Name**: `router_extension_invocation_duration_seconds`  
**Metric Type**: Histogram  
**Description**: Extension invocation latency distribution

**Labels**:
- `extension_id` (string): Extension identifier
- `extension_type` (string): Extension type (`pre`, `validator`, `post`, `provider`)
- `status` (string): Invocation status (`success`, `error`, `timeout`)
- `tenant_id` (string, optional): Tenant identifier (when available, Medium cardinality)
- `policy_id` (string, optional): Policy identifier (when available, Low cardinality)

**Buckets**: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0]

**Cardinality**: Same as `router_extension_invocations_total`

**Example**:
```
router_extension_invocation_duration_seconds_bucket{extension_id="normalize_text",extension_type="pre",status="success",le="0.01"} 100
router_extension_invocation_duration_seconds_bucket{extension_id="normalize_text",extension_type="pre",status="success",le="0.05"} 145
router_extension_invocation_duration_seconds_bucket{extension_id="normalize_text",extension_type="pre",status="success",le="+Inf"} 150
router_extension_invocation_duration_seconds_sum{extension_id="normalize_text",extension_type="pre",status="success"} 3.5
router_extension_invocation_duration_seconds_count{extension_id="normalize_text",extension_type="pre",status="success"} 150
```

#### 6.3. Extension Retries

**Metric Name**: `router_extension_retries_total`  
**Metric Type**: Counter  
**Description**: Total retry attempts for extension invocations

**Labels**:
- `extension_id` (string): Extension identifier
- `extension_type` (string): Extension type (`pre`, `validator`, `post`, `provider`)
- `retry_reason` (string): Retry reason (`timeout`, `error`)

**Cardinality**: ~500-5,000 series (Low-Medium)

**Example**:
```
router_extension_retries_total{extension_id="normalize_text",extension_type="pre",retry_reason="timeout"} 10
router_extension_retries_total{extension_id="normalize_text",extension_type="pre",retry_reason="error"} 2
```

#### 6.4. Extension Registry Lookups

**Metric Name**: `router_extension_registry_lookups_total`  
**Metric Type**: Counter  
**Description**: Total extension registry lookups

**Labels**:
- `lookup_status` (string): Lookup result (`success`, `not_found`, `error`)

**Cardinality**: ~3-10 series (Low)

**Example**:
```
router_extension_registry_lookups_total{lookup_status="success"} 1000
router_extension_registry_lookups_total{lookup_status="not_found"} 5
```

#### 6.5. Extension Registry Cache

**Metric Name**: `router_extension_registry_cache_hits_total`  
**Metric Type**: Counter  
**Description**: Extension registry cache hits

**Labels**: None

**Cardinality**: 1 series (Low)

**Example**:
```
router_extension_registry_cache_hits_total 950
```

**Metric Name**: `router_extension_registry_cache_misses_total`  
**Metric Type**: Counter  
**Description**: Extension registry cache misses

**Labels**: None

**Cardinality**: 1 series (Low)

**Example**:
```
router_extension_registry_cache_misses_total 50
```

#### 6.6. Extension Registry Load Duration

**Metric Name**: `router_extension_registry_load_duration_seconds`  
**Metric Type**: Histogram  
**Description**: Extension registry load operation latency

**Labels**:
- `load_source` (string): Load source (`database`, `cache`, `fixtures`)

**Buckets**: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5]

**Cardinality**: ~3-10 series (Low)

**Example**:
```
router_extension_registry_load_duration_seconds_bucket{load_source="database",le="0.01"} 100
router_extension_registry_load_duration_seconds_bucket{load_source="database",le="0.05"} 100
router_extension_registry_load_duration_seconds_sum{load_source="database"} 0.5
router_extension_registry_load_duration_seconds_count{load_source="database"} 100
```

#### 6.7. Circuit Breaker State

**Metric Name**: `router_extension_circuit_breaker_state`  
**Metric Type**: Gauge  
**Description**: Current circuit breaker state per extension

**Labels**:
- `extension_id` (string): Extension identifier
- `state` (string): Circuit breaker state (`closed`, `open`, `half_open`)

**Cardinality**: ~100-1,000 series (Low-Medium)

**Example**:
```
router_extension_circuit_breaker_state{extension_id="normalize_text",state="closed"} 1
router_extension_circuit_breaker_state{extension_id="normalize_text",state="open"} 0
```

#### 6.8. Circuit Breaker Transitions

**Metric Name**: `router_extension_circuit_breaker_transitions_total`  
**Metric Type**: Counter  
**Description**: Total circuit breaker state transitions

**Labels**:
- `extension_id` (string): Extension identifier
- `transition` (string): Transition type (`open`, `close`, `half_open`)

**Cardinality**: ~300-3,000 series (Low-Medium)

**Example**:
```
router_extension_circuit_breaker_transitions_total{extension_id="normalize_text",transition="open"} 5
router_extension_circuit_breaker_transitions_total{extension_id="normalize_text",transition="close"} 3
```

#### 6.9. Circuit Breaker Success/Failure

**Metric Name**: `router_extension_circuit_breaker_success_total`  
**Metric Type**: Counter  
**Description**: Total successful invocations recorded by circuit breaker

**Labels**:
- `extension_id` (string): Extension identifier

**Cardinality**: ~100-1,000 series (Low-Medium)

**Example**:
```
router_extension_circuit_breaker_success_total{extension_id="normalize_text"} 150
```

**Metric Name**: `router_extension_circuit_breaker_failure_total`  
**Metric Type**: Counter  
**Description**: Total failed invocations recorded by circuit breaker

**Labels**:
- `extension_id` (string): Extension identifier

**Cardinality**: ~100-1,000 series (Low-Medium)

**Example**:
```
router_extension_circuit_breaker_failure_total{extension_id="normalize_text"} 5
```

### 7. Gateway-Specific Metrics

**Component**: Gateway

#### 6.1. Rate Limiting Metrics

**Metric Name**: `gateway_rate_limit_hits_total`  
**Type**: `counter`  
**Description**: Total number of rate limit hits (requests that hit rate limit)  
**Unit**: hits

**Labels**:
- `endpoint` (string, required): Endpoint identifier (`/api/v1/messages`, `/api/v1/health`, etc.)
- `tenant_id` (string, optional): Tenant identifier (when available)

**Cardinality Constraints**:
- `endpoint`: Low (5-20 endpoints)
- `tenant_id`: Medium (10-1000 tenants)

**Estimated Cardinality**: ~50-20,000 series

**Example**:
```prometheus
gateway_rate_limit_hits_total{endpoint="/api/v1/messages", tenant_id="tenant_123"} 100
```

**Metric Name**: `gateway_rate_limit_allowed_total`  
**Type**: `counter`  
**Description**: Total number of requests allowed by rate limiter  
**Unit**: requests

**Labels**:
- `endpoint` (string, required): Endpoint identifier
- `tenant_id` (string, optional): Tenant identifier (when available)

**Cardinality Constraints**: Same as `gateway_rate_limit_hits_total`

**Example**:
```prometheus
gateway_rate_limit_allowed_total{endpoint="/api/v1/messages", tenant_id="tenant_123"} 10000

#### 6.2. Router Rate Limiting Metrics

**Metric Name**: `router_rate_limit_allowed_total`  
**Type**: `counter`  
**Description**: Total number of requests allowed by Router rate limiting  
**Unit**: requests

**Labels**:
- `scope` (string, required): Rate limit scope (`policy`, `tenant`, `global`)
- `tenant_id` (string, required): Tenant identifier (**medium cardinality**)
- `policy_id` (string, optional): Policy identifier (when scope is `policy`, **medium cardinality**)

**Cardinality Constraints**:
- `scope`: Low (3 values: policy, tenant, global)
- `tenant_id`: Medium (10-1000 tenants)
- `policy_id`: Medium (10-1000 policies per tenant)

**Estimated Cardinality**: ~30,000-300,000 series (with policy_id)

**Example**:
```prometheus
router_rate_limit_allowed_total{scope="policy", tenant_id="tenant_123", policy_id="default"} 5000
router_rate_limit_allowed_total{scope="tenant", tenant_id="tenant_123"} 10000
```

**Metric Name**: `router_rate_limit_exceeded_total`  
**Type**: `counter`  
**Description**: Total number of requests rejected by Router rate limiting  
**Unit**: requests

**Labels**: Same as `router_rate_limit_allowed_total`

**Cardinality Constraints**: Same as `router_rate_limit_allowed_total`

**Example**:
```prometheus
router_rate_limit_exceeded_total{scope="policy", tenant_id="tenant_123", policy_id="default"} 50
router_rate_limit_exceeded_total{scope="tenant", tenant_id="tenant_123"} 100
```

**Reference**: `apps/otp/router/src/router_rate_limit_store.erl`, `docs/archive/dev/RATE_LIMITING_OBSERVABILITY_SPEC.md`
```

#### 6.2. NATS Metrics

**Metric Name**: `gateway_nats_messages_sent_total`  
**Type**: `counter`  
**Description**: Total number of NATS messages sent  
**Unit**: messages

**Labels**:
- `subject` (string, required): NATS subject (normalized, e.g., `beamline.router.request`)
- `tenant_id` (string, optional): Tenant identifier (when available)

**Cardinality Constraints**:
- `subject`: Low (5-20 normalized subjects)
- `tenant_id`: Medium (10-1000 tenants)

**Estimated Cardinality**: ~50-20,000 series

**Example**:
```prometheus
gateway_nats_messages_sent_total{subject="beamline.router.request", tenant_id="tenant_123"} 5000
```

**Metric Name**: `gateway_nats_messages_received_total`  
**Type**: `counter`  
**Description**: Total number of NATS messages received  
**Unit**: messages

**Labels**:
- `subject` (string, required): NATS subject (normalized)
- `tenant_id` (string, optional): Tenant identifier (when available)

**Cardinality Constraints**: Same as `gateway_nats_messages_sent_total`

**Example**:
```prometheus
gateway_nats_messages_received_total{subject="beamline.router.response", tenant_id="tenant_123"} 5000
```

**Metric Name**: `gateway_nats_publish_failures_total`  
**Type**: `counter`  
**Description**: Total number of NATS publish failures  
**Unit**: failures

**Labels**:
- `subject` (string, required): NATS subject (normalized)
- `tenant_id` (string, optional): Tenant identifier (when available)

**Cardinality Constraints**: Same as `gateway_nats_messages_sent_total`

**Example**:
```prometheus
gateway_nats_publish_failures_total{subject="beamline.router.request", tenant_id="tenant_123"} 2
```

#### 6.3. Idempotency Metrics

**Metric Name**: `gateway_idempotency_hits_total`  
**Type**: `counter`  
**Description**: Total number of idempotency cache hits  
**Unit**: hits

**Labels**:
- `endpoint` (string, required): Endpoint identifier
- `tenant_id` (string, optional): Tenant identifier (when available)

**Cardinality Constraints**:
- `endpoint`: Low (5-20 endpoints)
- `tenant_id`: Medium (10-1000 tenants)

**Estimated Cardinality**: ~50-20,000 series

**Example**:
```prometheus
gateway_idempotency_hits_total{endpoint="/api/v1/messages", tenant_id="tenant_123"} 200
```

**Metric Name**: `gateway_idempotency_misses_total`  
**Type**: `counter`  
**Description**: Total number of idempotency cache misses  
**Unit**: misses

**Labels**:
- `endpoint` (string, required): Endpoint identifier
- `tenant_id` (string, optional): Tenant identifier (when available)

**Cardinality Constraints**: Same as `gateway_idempotency_hits_total`

**Example**:
```prometheus
gateway_idempotency_misses_total{endpoint="/api/v1/messages", tenant_id="tenant_123"} 800
```

---

## Cardinality Invariants

### High-Cardinality Label Rules

**CRITICAL**: High-cardinality labels (`run_id`, `flow_id`, `step_id`) must follow these rules:

1. **Detailed Metrics Only**: Include high-cardinality labels only in detailed metrics (e.g., `worker_step_executions_total`, `router_decisions_total`)

2. **Exclude from Aggregates**: Never include high-cardinality labels in aggregate metrics (e.g., `component_error_rate`, `component_health_status`)

3. **Cardinality Limits**: Monitor label cardinality via `prometheus_tsdb_head_series` metric
   - **Warning threshold**: 100,000 series per metric
   - **Critical threshold**: 1,000,000 series per metric

4. **Tenant Label Allowlist** (Optional): For very high tenant counts, use allowlist:
   ```bash
   METRICS_TENANT_LABEL_ALLOWLIST=premium-1,premium-2,premium-3
   ```
   - Only include `tenant_id` label for allowlisted tenants
   - Aggregate other tenants as `tenant="other"`

### Label Cardinality Summary

| Label | Cardinality | Usage Rule |
|-------|-------------|------------|
| `component` | Low (< 10) | All metrics |
| `method` | Low (< 10) | HTTP metrics |
| `path` | Medium (10-100) | HTTP metrics (normalized) |
| `status_code` | Low (< 20) | HTTP metrics |
| `policy_id` | Low (< 50) | Router metrics |
| `provider` | Low (< 20) | Router metrics |
| `decision_reason` | Low (< 10) | Router metrics |
| `step_type` | Low (< 20) | Worker metrics |
| `execution_status` | Low (< 5) | Worker metrics |
| `error_code` | Low (< 100) | Error metrics |
| `endpoint` | Low (< 20) | Gateway metrics |
| `subject` | Low (< 20) | Gateway/NATS metrics |
| `check` | Low (< 20) | Health metrics |
| `tenant_id` | Medium (10-1000) | All metrics (when available, use allowlist for very high counts) |
| `run_id` | High (10K-1M) | **Detailed metrics only, exclude from aggregates** |
| `flow_id` | Medium-High (1K-100K) | **Detailed metrics only, exclude from aggregates** |
| `step_id` | Very High (> 1M) | **Detailed metrics only, exclude from aggregates** |
| `trace_id` | Very High (> 1M) | **Never use in metrics (use only in logs/traces)** |

### Cardinality Estimation

**Total Estimated Series** (without high-cardinality labels):
- HTTP Request Metrics: ~10,000-100,000 series
- Routing Metrics: ~1,000-10,000 series
- Worker Execution Metrics: ~1,000-10,000 series (without run_id/flow_id/step_id)
- Error Metrics: ~300-3,000 series
- Health Metrics: ~15-60 series
- Gateway-Specific Metrics: ~200-40,000 series

**Total**: ~12,500-163,000 series (acceptable for Prometheus)

**With High-Cardinality Labels** (detailed metrics only):
- Can reach millions of series (monitor via `prometheus_tsdb_head_series`)
- Use cardinality limits and aggregation strategies

---

## Metric Naming Conventions

### Naming Rules

1. **Metric Names**: Use lowercase with underscores (`snake_case`)
   - ✅ `http_requests_total`
   - ❌ `httpRequestsTotal`, `HTTP_REQUESTS_TOTAL`

2. **Suffixes**:
   - `_total` for counters
   - `_seconds` for duration histograms
   - `_bytes` for size histograms
   - `_status` for gauge status metrics
   - `_rate` for gauge rate metrics

3. **Component Prefixes**:
   - `http_*` for HTTP request metrics (Gateway, Router)
   - `router_*` for router-specific metrics
   - `worker_*` for worker-specific metrics
   - `gateway_*` for gateway-specific metrics
   - `component_*` for cross-component metrics

4. **Label Names**: Use lowercase with underscores (`snake_case`)
   - ✅ `tenant_id`, `run_id`, `flow_id`, `step_id`
   - ❌ `tenantId`, `runId`, `TENANT_ID`

---

## CP1 Field Mapping

**CP1 Correlation Fields → Prometheus Labels**:

| CP1 Field | Prometheus Label | Required | Cardinality | Usage |
|-----------|------------------|----------|-------------|-------|
| `tenant_id` | `tenant_id` | Optional | Medium | All metrics (when available) |
| `run_id` | `run_id` | Optional | High | Detailed metrics only |
| `flow_id` | `flow_id` | Optional | Medium-High | Detailed metrics only |
| `step_id` | `step_id` | Optional | Very High | Detailed metrics only |
| `trace_id` | N/A | N/A | Very High | **Never use in metrics** (use only in logs/traces) |
| `error_code` | `error_code` | Required (error metrics) | Low | Error metrics |
| `latency_ms` | N/A | N/A | N/A | Histogram value (convert to seconds) |
| `component` | `component` | Required | Low | All metrics |
| `context.method` | `method` | Required (HTTP) | Low | HTTP metrics |
| `context.path` | `path` | Required (HTTP) | Medium | HTTP metrics (normalized) |
| `context.status_code` | `status_code` | Required (HTTP) | Low | HTTP metrics |
| `context.provider` | `provider` | Required (Router) | Low | Router metrics |
| `context.policy_id` | `policy_id` | Required (Router) | Low | Router metrics |
| `context.decision_reason` | `decision_reason` | Required (Router) | Low | Router metrics |
| `context.step_type` | `step_type` | Required (Worker) | Low | Worker metrics |
| `context.execution_status` | `execution_status` | Required (Worker) | Low | Worker metrics |

---

## Implementation Requirements

### Component Endpoints

**All components must export Prometheus metrics via HTTP `/metrics` endpoint**:

| Component | Port | Path | Protocol |
|-----------|------|------|----------|
| Router | 9001 | `/metrics` | HTTP (separate from gRPC port 9000) |
| Gateway | 3001 | `/metrics` | HTTP (separate from API port 3000) |
| Worker | 8081 | `/metrics` | HTTP (separate from API port 8080) |

### Response Format

**Prometheus text format** (RFC 4180):
```
# HELP metric_name Description
# TYPE metric_name counter
metric_name{label1="value1", label2="value2"} 1234
```

### Validation

**Validation Script**: `scripts/observability/validate_metrics.sh`

**Checks**:
1. All required metrics are exported
2. All required labels are present
3. Label cardinality is within limits
4. Metric types are correct (counter/gauge/histogram)
5. Histogram buckets are configured correctly

---

## Summary

### Metrics Summary

| Category | Metrics Count | Estimated Series (without high-cardinality) |
|----------|---------------|--------------------------------------------|
| HTTP Request Metrics | 4 | ~10,000-100,000 |
| Routing Metrics | 4 | ~1,000-10,000 |
| Worker Execution Metrics | 4 | ~1,000-10,000 |
| Error Metrics | 2 | ~300-3,000 |
| Health Metrics | 2 | ~15-60 |
| Extension Metrics | 9 | ~2,000-120,000 |
| Gateway-Specific Metrics | 7 | ~200-40,000 |
| **Total** | **32** | **~15,500-283,000** |

### Key Principles

1. ✅ **Use CP1 correlation fields as labels** when available
2. ✅ **High-cardinality labels** (`run_id`, `flow_id`, `step_id`) only in detailed metrics
3. ✅ **Exclude high-cardinality labels** from aggregate metrics
4. ✅ **Never use `trace_id`** in metrics (use only in logs/traces)
5. ✅ **Monitor cardinality** via `prometheus_tsdb_head_series`
6. ✅ **Use tenant allowlist** for very high tenant counts

---

## References

- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - High-level CP2 extension plan
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 correlation fields
- [Prometheus Best Practices](https://prometheus.io/docs/practices/naming/) - Metric naming conventions
- [Prometheus Histograms](https://prometheus.io/docs/practices/histograms/) - Histogram bucket configuration

---

**WORKER**: `wrk-obs1` (Observability CP2)  
**Control Point**: CP2-LC  
**Status**: 📋 **SPECIFICATION DOCUMENT** (ready for implementation)

