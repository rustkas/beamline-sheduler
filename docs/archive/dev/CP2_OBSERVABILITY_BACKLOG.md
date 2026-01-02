# CP2 Observability Backlog

**Version**: 2.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-obs1 (Observability CP2)  
**Status**: 📋 **EXECUTABLE BACKLOG** (CP2)

---

## Executive Summary

This document breaks down the high-level CP2 Observability Plan (`CP2_OBSERVABILITY_PLAN.md` and `OBSERVABILITY_CP2_EXTENSION_PLAN.md`) into atomic, sprint-ready tickets. Each ticket is:

- **Atomic**: 0.5–2 days of work
- **Scoped**: Clear deliverables (code/tests/docs/configs/dashboards)
- **Gated**: Feature flags to protect CP1 baseline
- **Testable**: Acceptance criteria defined

**Foundation**: CP1 observability provides structured JSON logging, CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`), health endpoints, and PII filtering. CP2 extends this with Prometheus metrics, OpenTelemetry tracing, Grafana dashboards, and Alertmanager rules.

**References**:
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - Detailed CP2 extension plan
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/archive/dev/OBSERVABILITY_CP1_COMPLETION_REPORT.md` - CP1 observability completion report

---

## Feature Gate Strategy

### CP1 Baseline Protection

**CRITICAL**: All CP2 features must be gated behind feature flags to prevent breaking CP1 baseline.

**Feature Flag Pattern**:

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

// Usage in code
if (is_cp2_feature_enabled("metrics")) {
    metrics_record_request(method, path, status_code);
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

// Usage in code
if (is_cp2_feature_enabled("metrics")) {
    metrics::record_step_execution(step_type, status);
}
```

**Feature Flags**:
- `CP2_OBSERVABILITY_METRICS_ENABLED` - Prometheus metrics export (CP2.1)
- `CP2_OBSERVABILITY_TRACING_ENABLED` - OpenTelemetry tracing (CP2.2)
- `CP2_OBSERVABILITY_DASHBOARDS_ENABLED` - Grafana dashboards (CP2.3)
- `CP2_OBSERVABILITY_ALERTING_ENABLED` - Alertmanager rules (CP2.4)

**Default**: All CP2 feature flags are `false` (CP1 baseline mode).

---

## CP2.1: Prometheus Metrics Export

**Total Estimate**: 7 days  
**Priority**: High  
**Feature Flag**: `CP2_OBSERVABILITY_METRICS_ENABLED`  
**Wave**: Wave 1 (P1 Critical)

**For detailed Wave 1 specification**, see `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md`.

### Ticket O1-1.1: Router Metrics Collection

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without metrics, cannot monitor routing performance, identify bottlenecks, or track provider selection patterns  
**Wave**: Wave 1  
**Dependencies**: None (CP1 observability complete)

**Scope**:
- Implement metrics collection for routing decisions
- Implement metrics collection for policy evaluation
- Implement metrics collection for provider selection
- Implement metrics collection for errors
- Use CP1 correlation fields as Prometheus labels (`tenant_id`, `run_id`)
- Feature flag: `CP2_OBSERVABILITY_METRICS_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/otp/router/src/router_metrics.erl` - Router metrics collection module
  - `apps/otp/router/src/router_metrics_collector.erl` - Metrics collector implementation
  - `apps/otp/router/src/router_core.erl` - Integration with metrics collection
- **Tests**: 
  - `apps/otp/router/test/router_metrics_tests.erl` - Metrics collection unit tests
- **Config**: 
  - `apps/otp/router/config/sys.config` - Feature flag configuration
- **Docs**: 
  - `apps/otp/router/docs/METRICS.md` - Router metrics documentation

**Feature Gate**:
```erlang
% In router_core.erl::handle_routing_decision()
case is_cp2_feature_enabled("metrics") of
    true -> 
        router_metrics:record_decision(PolicyId, Provider, DecisionReason, TenantId, RunId);
    false -> 
        ok  % CP1 baseline: no metrics
end.
```

**Acceptance Criteria**:
- ✅ Metrics collection implemented for routing decisions
- ✅ Metrics collection implemented for policy evaluation
- ✅ Metrics collection implemented for provider selection
- ✅ Metrics collection implemented for errors
- ✅ CP1 correlation fields used as labels (`tenant_id`, `run_id`)
- ✅ Feature flag gates CP2 behavior (CP1 baseline preserved)
- ✅ Unit tests pass for metrics collection

---

### Ticket O1-1.2: Router Metrics Endpoint

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without `/metrics` endpoint, metrics cannot be scraped by Prometheus  
**Wave**: Wave 1  
**Dependencies**: O1-1.1 (Router Metrics Collection)

**Scope**:
- Add HTTP `/metrics` endpoint (port 9001, separate from gRPC port 9000)
- Implement Prometheus text format export
- Ensure endpoint is accessible and validated
- Feature flag: `CP2_OBSERVABILITY_METRICS_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/otp/router/src/router_metrics_endpoint.erl` - HTTP `/metrics` endpoint handler
  - `apps/otp/router/src/router_metrics_formatter.erl` - Prometheus text format formatter
  - `apps/otp/router/src/router_sup.erl` - HTTP server startup integration
- **Tests**: 
  - `apps/otp/router/test/router_metrics_endpoint_tests.erl` - Endpoint integration tests
- **Config**: 
  - `apps/otp/router/config/sys.config` - HTTP server configuration (port 9001)
- **Docs**: 
  - `apps/otp/router/docs/METRICS.md` - Endpoint documentation

**Feature Gate**:
```erlang
% In router_sup.erl::init()
case is_cp2_feature_enabled("metrics") of
    true -> 
        start_http_server(9001, router_metrics_endpoint);
    false -> 
        ok  % CP1 baseline: no metrics endpoint
end.
```

**Acceptance Criteria**:
- ✅ Router exposes `/metrics` endpoint on port 9001
- ✅ Endpoint returns Prometheus text format
- ✅ All routing metrics are exported correctly
- ✅ Feature flag gates CP2 behavior
- ✅ Integration tests pass for metrics export
- ✅ Endpoint is accessible and validated

---

### Ticket O1-1.3: Gateway Metrics Collection

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without metrics, cannot monitor HTTP request performance, rate limiting effectiveness, or idempotency cache hits  
**Wave**: Wave 1  
**Dependencies**: None (CP1 observability complete)

**Scope**:
- Implement metrics collection for HTTP requests
- Implement metrics collection for request duration
- Implement metrics collection for rate limiting
- Implement metrics collection for idempotency
- Use CP1 correlation fields as Prometheus labels (`tenant_id`, `run_id`)
- Feature flag: `CP2_OBSERVABILITY_METRICS_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/c-gateway/src/metrics/prometheus.c` - Gateway metrics collection
  - `apps/c-gateway/src/metrics/metrics_collector.c` - Metrics collector implementation
  - `apps/c-gateway/src/metrics/metrics.h` - Metrics header file
  - `apps/c-gateway/src/http_handler.c` - Integration with metrics collection
- **Tests**: 
  - `apps/c-gateway/test/metrics_test.c` - Metrics collection unit tests
- **Config**: 
  - `apps/c-gateway/config/metrics.conf` - Feature flag configuration
- **Docs**: 
  - `apps/c-gateway/docs/METRICS.md` - Gateway metrics documentation

**Feature Gate**:
```c
// In http_handler.c::handle_request()
if (is_cp2_feature_enabled("metrics")) {
    metrics_record_request(method, path, status_code, tenant_id, run_id);
    metrics_record_duration(method, path, duration_ms);
}
```

**Acceptance Criteria**:
- ✅ Metrics collection implemented for HTTP requests
- ✅ Metrics collection implemented for request duration
- ✅ Metrics collection implemented for rate limiting
- ✅ Metrics collection implemented for idempotency
- ✅ CP1 correlation fields used as labels (`tenant_id`, `run_id`)
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for metrics collection

---

### Ticket O1-1.4: Gateway Metrics Endpoint

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without `/metrics` endpoint, metrics cannot be scraped by Prometheus  
**Wave**: Wave 1  
**Dependencies**: O1-1.3 (Gateway Metrics Collection)

**Scope**:
- Add HTTP `/metrics` endpoint (port 3001, separate from API port 3000)
- Implement Prometheus text format export
- Ensure endpoint is accessible and validated
- Feature flag: `CP2_OBSERVABILITY_METRICS_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/c-gateway/src/metrics/metrics_endpoint.c` - HTTP `/metrics` endpoint handler
  - `apps/c-gateway/src/metrics/metrics_formatter.c` - Prometheus text format formatter
  - `apps/c-gateway/src/server.c` - HTTP server route registration
- **Tests**: 
  - `apps/c-gateway/test/metrics_endpoint_test.c` - Endpoint integration tests
- **Config**: 
  - `apps/c-gateway/config/metrics.conf` - HTTP server configuration (port 3001)
- **Docs**: 
  - `apps/c-gateway/docs/METRICS.md` - Endpoint documentation

**Feature Gate**:
```c
// In server.c::start_server()
if (is_cp2_feature_enabled("metrics")) {
    register_route("/metrics", handle_metrics_endpoint);
}
```

**Acceptance Criteria**:
- ✅ Gateway exposes `/metrics` endpoint on port 3001
- ✅ Endpoint returns Prometheus text format
- ✅ All HTTP metrics are exported correctly
- ✅ Feature flag gates CP2 behavior
- ✅ Integration tests pass for metrics export
- ✅ Endpoint is accessible and validated

---

### Ticket O1-1.5: Worker Metrics Collection

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without metrics, cannot monitor step execution performance, identify slow steps, or track queue depth  
**Wave**: Wave 1  
**Dependencies**: None (CP1 observability complete)

**Scope**:
- Implement metrics collection for step executions
- Implement metrics collection for execution duration
- Implement metrics collection for errors
- Implement metrics collection for queue depth
- Use CP1 correlation fields as Prometheus labels (`tenant_id`, `run_id`, `flow_id`, `step_id`)
- Feature flag: `CP2_OBSERVABILITY_METRICS_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/observability/metrics.cpp` - Worker metrics collection
  - `apps/caf/processor/src/observability/metrics_collector.cpp` - Metrics collector implementation
  - `apps/caf/processor/src/observability/metrics.h` - Metrics header file
  - `apps/caf/processor/src/worker_actor.cpp` - Integration with metrics collection
- **Tests**: 
  - `apps/caf/processor/test/metrics_test.cpp` - Metrics collection unit tests
- **Config**: 
  - `apps/caf/processor/config/metrics.conf` - Feature flag configuration
- **Docs**: 
  - `apps/caf/processor/docs/METRICS.md` - Worker metrics documentation

**Feature Gate**:
```cpp
// In worker_actor.cpp::execute_step()
if (is_cp2_feature_enabled("metrics")) {
    metrics::record_step_execution(step_type, execution_status, tenant_id, run_id, flow_id, step_id);
    metrics::record_step_duration(step_type, duration_ms, tenant_id, run_id, flow_id, step_id);
}
```

**Acceptance Criteria**:
- ✅ Metrics collection implemented for step executions
- ✅ Metrics collection implemented for execution duration
- ✅ Metrics collection implemented for errors
- ✅ Metrics collection implemented for queue depth
- ✅ CP1 correlation fields used as labels (`tenant_id`, `run_id`, `flow_id`, `step_id`)
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for metrics collection

---

### Ticket O1-1.6: Worker Metrics Endpoint

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without `/metrics` endpoint, metrics cannot be scraped by Prometheus  
**Wave**: Wave 1  
**Dependencies**: O1-1.5 (Worker Metrics Collection)

**Scope**:
- Add HTTP `/metrics` endpoint (port 9092, separate from API port 8080)
- Implement Prometheus text format export
- Ensure endpoint is accessible and validated
- Feature flag: `CP2_OBSERVABILITY_METRICS_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/observability/metrics_endpoint.cpp` - HTTP `/metrics` endpoint handler
  - `apps/caf/processor/src/observability/metrics_formatter.cpp` - Prometheus text format formatter
  - `apps/caf/processor/src/server.cpp` - HTTP server route registration
- **Tests**: 
  - `apps/caf/processor/test/metrics_endpoint_test.cpp` - Endpoint integration tests
- **Config**: 
  - `apps/caf/processor/config/metrics.conf` - HTTP server configuration (port 9092)
- **Docs**: 
  - `apps/caf/processor/docs/METRICS.md` - Endpoint documentation

**Feature Gate**:
```cpp
// In server.cpp::start_server()
if (is_cp2_feature_enabled("metrics")) {
    register_route("/metrics", handle_metrics_endpoint);
}
```

**Acceptance Criteria**:
- ✅ Worker exposes `/metrics` endpoint on port 9092
- ✅ Endpoint returns Prometheus text format
- ✅ All step execution metrics are exported correctly
- ✅ Feature flag gates CP2 behavior
- ✅ Integration tests pass for metrics export
- ✅ Endpoint is accessible and validated

---

### Ticket O1-1.7: Metrics Validation Script

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without validation, cannot ensure metrics are correctly exported and scraped by Prometheus  
**Wave**: Wave 1  
**Dependencies**: O1-1.2, O1-1.4, O1-1.6 (All metrics endpoints)

**Scope**:
- Create bash script for metrics validation
- Create PowerShell script for Windows compatibility
- Validate Prometheus text format
- Check label cardinality
- Verify CP1 correlation fields in labels
- Validate endpoint accessibility (Router: 9001, Gateway: 3001, Worker: 9092)

**Artifacts**:
- **Scripts**: 
  - `scripts/observability/validate_metrics.sh` - Bash validation script
  - `scripts/observability/validate_metrics.ps1` - PowerShell validation script
- **Docs**: 
  - `docs/observability/METRICS_VALIDATION.md` - Metrics validation guide

**Acceptance Criteria**:
- ✅ Validation scripts work on Linux, macOS, Windows (WSL)
- ✅ Scripts validate all component metrics endpoints
- ✅ Scripts check Prometheus format compliance
- ✅ Scripts verify CP1 correlation fields in labels
- ✅ Scripts check label cardinality limits
- ✅ Scripts provide clear error messages

---

## CP2.2: OpenTelemetry Distributed Tracing

**Total Estimate**: 7 days  
**Priority**: High  
**Feature Flag**: `CP2_OBSERVABILITY_TRACING_ENABLED`  
**Wave**: Wave 2 (P1 Critical)

### Ticket O1-2.1: Router OTEL SDK Integration

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without OTEL SDK, cannot create spans or export traces  
**Wave**: Wave 2  
**Dependencies**: O1-1.1, O1-1.2 (Router metrics)

**Scope**:
- Add OTEL SDK dependency (`opentelemetry-erlang`)
- Initialize OTEL SDK in Router startup
- Configure OTEL exporter (OTLP HTTP/gRPC)
- Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/otp/router/rebar.config` - OTEL SDK dependency
  - `apps/otp/router/src/router_tracing.erl` - OTEL SDK initialization
  - `apps/otp/router/src/router_sup.erl` - OTEL SDK startup integration
- **Config**: 
  - `apps/otp/router/config/sys.config` - OTEL configuration (collector endpoint, batch settings)
  - `apps/otp/router/config/tracing.config` - Tracing configuration
- **Tests**: 
  - `apps/otp/router/test/router_tracing_tests.erl` - OTEL SDK initialization tests
- **Docs**: 
  - `apps/otp/router/docs/TRACING.md` - OTEL SDK integration documentation

**Feature Gate**:
```erlang
% In router_sup.erl::init()
case is_cp2_feature_enabled("tracing") of
    true -> 
        router_tracing:init(OTELCollectorEndpoint);
    false -> 
        ok  % CP1 baseline: no tracing
end.
```

**Acceptance Criteria**:
- ✅ OTEL SDK dependency added
- ✅ OTEL SDK initialized in Router startup
- ✅ OTEL exporter configured (OTLP HTTP/gRPC)
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for OTEL SDK initialization

---

### Ticket O1-2.2: Router Span Creation

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without spans, cannot trace routing decisions or policy evaluation  
**Wave**: Wave 2  
**Dependencies**: O1-2.1 (Router OTEL SDK Integration)

**Status**: ✅ **PARTIALLY COMPLETE** (CP2 wrk-3 implementation)

**Implemented** (2025-01-27):
- ✅ Spans for routing decisions (`beamline.router.route` in `router_core.erl`)
- ✅ Spans for policy evaluation (`beamline.router.policy.apply` in `router_policy_applier.erl`)
- ✅ Spans for provider selection (`beamline.router.provider.select` in `router_decider.erl`)
- ✅ CP1 correlation fields as span attributes (`tenant_id`, `policy_id`, `message_id`)
- ✅ Trace context propagation (extraction from headers/context)
- ✅ Span status and error handling
- ✅ Integration tests in `router_observability_SUITE.erl`

**Deferred**:
- 📅 Spans for NATS message handling (separate ticket)
- 📅 Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED` (implementation ready, flag integration deferred)

**Scope**:
- Create spans for routing decisions ✅
- Create spans for policy evaluation ✅
- Create spans for provider selection ✅
- Create spans for NATS message handling 📅
- Include CP1 correlation fields as span attributes (`tenant.id`, `run.id`, `flow.id`, `trace.id`) ✅
- Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED` 📅

**Artifacts**:
- **Code**: 
  - `apps/otp/router/src/router_tracing_span.erl` - Span creation helpers
  - `apps/otp/router/src/router_core.erl` - Span creation integration
- **Tests**: 
  - `apps/otp/router/test/router_tracing_span_tests.erl` - Span creation unit tests
- **Docs**: 
  - `apps/otp/router/docs/TRACING.md` - Span creation documentation

**Feature Gate**:
```erlang
% In router_core.erl::handle_routing_decision()
case is_cp2_feature_enabled("tracing") of
    true -> 
        Span = router_tracing_span:start_span("routing_decision", [
            {tenant_id, TenantId},
            {run_id, RunId},
            {policy_id, PolicyId}
        ]),
        % ... routing logic ...
        router_tracing_span:end_span(Span);
    false -> 
        % CP1 baseline: no tracing
        handle_routing_decision_internal(PolicyId, Provider)
end.
```

**Acceptance Criteria**:
- ✅ Spans created for routing decisions
- ✅ Spans created for policy evaluation
- ✅ Spans created for provider selection
- ✅ Spans created for NATS message handling
- ✅ CP1 correlation fields included as span attributes
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for span creation

---

### Ticket O1-2.3: Router Context Propagation

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without context propagation, traces cannot be correlated across components  
**Wave**: Wave 2  
**Dependencies**: O1-2.2 (Router Span Creation)

**Status**: ✅ **COMPLETE** (CP2 wrk-3 implementation)

**Implemented** (2025-01-27):
- ✅ Trace context extraction from request context (`extract_trace_context_from_request` in `router_core.erl`, `router_policy_applier.erl`)
- ✅ Trace context extraction using `router_tracing:extract_trace_context/1` (supports W3C Trace Context format)
- ✅ Parent context propagation to nested spans
- ✅ Integration with existing `router_tracing.erl` module

**Scope**:
- Extract trace context from NATS message headers (`traceparent`, `tracestate`) ✅
- Propagate context to nested spans ✅
- Inject trace context into NATS message headers
- Support W3C Trace Context format
- Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/otp/router/src/router_tracing_context.erl` - Trace context propagation
  - `apps/otp/router/src/router_nats_handler.erl` - NATS header integration
- **Tests**: 
  - `apps/otp/router/test/router_tracing_context_tests.erl` - Context propagation unit tests
  - `apps/otp/router/test/router_tracing_integration_tests.erl` - Integration tests
- **Docs**: 
  - `apps/otp/router/docs/TRACING.md` - Context propagation documentation

**Feature Gate**:
```erlang
% In router_nats_handler.erl::handle_message()
case is_cp2_feature_enabled("tracing") of
    true -> 
        TraceContext = router_tracing_context:extract_from_nats_headers(Headers),
        Span = router_tracing_span:start_span_with_context("routing_decision", TraceContext),
        % ... routing logic ...
        NewHeaders = router_tracing_context:inject_to_nats_headers(Span),
        publish_to_nats(Message, NewHeaders);
    false -> 
        % CP1 baseline: no tracing
        handle_message_internal(Message)
end.
```

**Acceptance Criteria**:
- ✅ Trace context extracted from NATS headers
- ✅ Trace context injected into NATS headers
- ✅ W3C Trace Context format supported
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for context propagation
- ✅ Integration tests pass for cross-component tracing

---

### Ticket O1-2.4: Gateway OTEL SDK Integration

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without OTEL SDK, cannot create spans or export traces  
**Wave**: Wave 2  
**Dependencies**: O1-1.3, O1-1.4 (Gateway metrics)

**Scope**:
- Add OTEL SDK dependency (C: `opentelemetry-c`)
- Initialize OTEL SDK in Gateway startup
- Configure OTEL exporter (OTLP HTTP/gRPC)
- Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/c-gateway/CMakeLists.txt` - OTEL SDK dependency
  - `apps/c-gateway/src/tracing/otel.c` - OTEL SDK initialization
  - `apps/c-gateway/src/tracing/tracing.h` - Tracing header file
  - `apps/c-gateway/src/server.c` - OTEL SDK startup integration
- **Config**: 
  - `apps/c-gateway/config/tracing.conf` - OTEL configuration (collector endpoint, batch settings)
- **Tests**: 
  - `apps/c-gateway/test/tracing_test.c` - OTEL SDK initialization tests
- **Docs**: 
  - `apps/c-gateway/docs/TRACING.md` - OTEL SDK integration documentation

**Feature Gate**:
```c
// In server.c::start_server()
if (is_cp2_feature_enabled("tracing")) {
    otel_init(otel_collector_endpoint);
}
```

**Acceptance Criteria**:
- ✅ OTEL SDK dependency added
- ✅ OTEL SDK initialized in Gateway startup
- ✅ OTEL exporter configured (OTLP HTTP/gRPC)
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for OTEL SDK initialization

---

### Ticket O1-2.5: Gateway Span Creation

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without spans, cannot trace HTTP requests or rate limiting  
**Wave**: Wave 2  
**Dependencies**: O1-2.4 (Gateway OTEL SDK Integration)

**Scope**:
- Create spans for HTTP requests
- Create spans for rate limiting
- Create spans for idempotency checks
- Create spans for NATS message publishing
- Include CP1 correlation fields as span attributes (`tenant.id`, `run.id`, `trace.id`)
- Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/c-gateway/src/tracing/tracing_span.c` - Span creation helpers
  - `apps/c-gateway/src/http_handler.c` - Span creation integration
- **Tests**: 
  - `apps/c-gateway/test/tracing_span_test.c` - Span creation unit tests
- **Docs**: 
  - `apps/c-gateway/docs/TRACING.md` - Span creation documentation

**Feature Gate**:
```c
// In http_handler.c::handle_request()
if (is_cp2_feature_enabled("tracing")) {
    otel_span_t* span = otel_start_span("http_request", attributes);
    // ... request handling ...
    otel_end_span(span);
}
```

**Acceptance Criteria**:
- ✅ Spans created for HTTP requests
- ✅ Spans created for rate limiting
- ✅ Spans created for idempotency checks
- ✅ Spans created for NATS message publishing
- ✅ CP1 correlation fields included as span attributes
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for span creation

---

### Ticket O1-2.6: Gateway Context Propagation

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without context propagation, traces cannot be correlated across components  
**Wave**: Wave 2  
**Dependencies**: O1-2.5 (Gateway Span Creation)

**Scope**:
- Extract trace context from HTTP headers (`traceparent`, `tracestate`)
- Inject trace context into HTTP response headers
- Inject trace context into NATS message headers
- Support W3C Trace Context format
- Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/c-gateway/src/tracing/tracing_context.c` - Trace context propagation
  - `apps/c-gateway/src/http_handler.c` - HTTP header integration
  - `apps/c-gateway/src/nats_client.c` - NATS header integration
- **Tests**: 
  - `apps/c-gateway/test/tracing_context_test.c` - Context propagation unit tests
  - `apps/c-gateway/test/tracing_integration_test.c` - Integration tests
- **Docs**: 
  - `apps/c-gateway/docs/TRACING.md` - Context propagation documentation

**Feature Gate**:
```c
// In http_handler.c::handle_request()
if (is_cp2_feature_enabled("tracing")) {
    otel_context_t* context = otel_extract_from_http_headers(request_headers);
    otel_span_t* span = otel_start_span_with_context("http_request", context);
    // ... request handling ...
    otel_inject_to_http_headers(response_headers, span);
    otel_inject_to_nats_headers(nats_headers, span);
}
```

**Acceptance Criteria**:
- ✅ Trace context extracted from HTTP headers
- ✅ Trace context injected into HTTP response headers
- ✅ Trace context injected into NATS headers
- ✅ W3C Trace Context format supported
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for context propagation
- ✅ Integration tests pass for cross-component tracing

---

### Ticket O1-2.7: Worker OTEL SDK Integration

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without OTEL SDK, cannot create spans or export traces  
**Wave**: Wave 2  
**Dependencies**: O1-1.5, O1-1.6 (Worker metrics)

**Scope**:
- Add OTEL SDK dependency (C++: `opentelemetry-cpp`)
- Initialize OTEL SDK in Worker startup
- Configure OTEL exporter (OTLP HTTP/gRPC)
- Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/CMakeLists.txt` - OTEL SDK dependency
  - `apps/caf/processor/src/observability/tracing.cpp` - OTEL SDK initialization
  - `apps/caf/processor/src/observability/tracing.h` - Tracing header file
  - `apps/caf/processor/src/worker_main.cpp` - OTEL SDK startup integration
- **Config**: 
  - `apps/caf/processor/config/tracing.conf` - OTEL configuration (collector endpoint, batch settings)
- **Tests**: 
  - `apps/caf/processor/test/tracing_test.cpp` - OTEL SDK initialization tests
- **Docs**: 
  - `apps/caf/processor/docs/TRACING.md` - OTEL SDK integration documentation

**Feature Gate**:
```cpp
// In worker_main.cpp::main()
if (is_cp2_feature_enabled("tracing")) {
    otel::init(otel_collector_endpoint);
}
```

**Acceptance Criteria**:
- ✅ OTEL SDK dependency added
- ✅ OTEL SDK initialized in Worker startup
- ✅ OTEL exporter configured (OTLP HTTP/gRPC)
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for OTEL SDK initialization

---

### Ticket O1-2.8: Worker Span Creation

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without spans, cannot trace step execution or block execution  
**Wave**: Wave 2  
**Dependencies**: O1-2.7 (Worker OTEL SDK Integration)

**Scope**:
- Create spans for step executions
- Create spans for block executions (HTTP, FS, SQL, Human approval)
- Create spans for retry attempts
- Create spans for error handling
- Include CP1 correlation fields as span attributes (`tenant.id`, `run.id`, `flow.id`, `step.id`, `trace.id`)
- Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/observability/tracing_span.cpp` - Span creation helpers
  - `apps/caf/processor/src/worker_actor.cpp` - Span creation integration
- **Tests**: 
  - `apps/caf/processor/test/tracing_span_test.cpp` - Span creation unit tests
- **Docs**: 
  - `apps/caf/processor/docs/TRACING.md` - Span creation documentation

**Feature Gate**:
```cpp
// In worker_actor.cpp::execute_step()
if (is_cp2_feature_enabled("tracing")) {
    auto span = otel::start_span("step_execution", {
        {"tenant.id", tenant_id},
        {"run.id", run_id},
        {"flow.id", flow_id},
        {"step.id", step_id}
    });
    // ... step execution ...
    otel::end_span(span);
}
```

**Acceptance Criteria**:
- ✅ Spans created for step executions
- ✅ Spans created for block executions
- ✅ Spans created for retry attempts
- ✅ Spans created for error handling
- ✅ CP1 correlation fields included as span attributes
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for span creation

---

### Ticket O1-2.9: Worker Context Propagation

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without context propagation, traces cannot be correlated across components  
**Wave**: Wave 2  
**Dependencies**: O1-2.8 (Worker Span Creation)

**Scope**:
- Extract trace context from NATS message headers (`traceparent`, `tracestate`)
- Inject trace context into NATS message headers
- Support W3C Trace Context format
- Feature flag: `CP2_OBSERVABILITY_TRACING_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/observability/tracing_context.cpp` - Trace context propagation
  - `apps/caf/processor/src/nats_handler.cpp` - NATS header integration
- **Tests**: 
  - `apps/caf/processor/test/tracing_context_test.cpp` - Context propagation unit tests
  - `apps/caf/processor/test/tracing_integration_test.cpp` - Integration tests
- **Docs**: 
  - `apps/caf/processor/docs/TRACING.md` - Context propagation documentation

**Feature Gate**:
```cpp
// In nats_handler.cpp::handle_message()
if (is_cp2_feature_enabled("tracing")) {
    auto context = otel::extract_from_nats_headers(message_headers);
    auto span = otel::start_span_with_context("step_execution", context);
    // ... step execution ...
    auto new_headers = otel::inject_to_nats_headers(span);
    publish_to_nats(message, new_headers);
}
```

**Acceptance Criteria**:
- ✅ Trace context extracted from NATS headers
- ✅ Trace context injected into NATS headers
- ✅ W3C Trace Context format supported
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass for context propagation
- ✅ Integration tests pass for cross-component tracing

---

### Ticket O1-2.10: Trace Validation Script

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without validation, cannot ensure traces are correctly exported and correlated  
**Wave**: Wave 2  
**Dependencies**: O1-2.3, O1-2.6, O1-2.9 (All context propagation)

**Scope**:
- Create bash script for trace validation
- Create PowerShell script for Windows compatibility
- Validate trace export to OTEL collector
- Check span hierarchy
- Verify CP1 correlation fields in span attributes
- Verify context propagation across components

**Artifacts**:
- **Scripts**: 
  - `scripts/observability/validate_traces.sh` - Bash validation script
  - `scripts/observability/validate_traces.ps1` - PowerShell validation script
- **Docs**: 
  - `docs/observability/TRACING_VALIDATION.md` - Tracing validation guide

**Acceptance Criteria**:
- ✅ Validation scripts work on Linux, macOS, Windows (WSL)
- ✅ Scripts validate trace export to OTEL collector
- ✅ Scripts check span hierarchy correctness
- ✅ Scripts verify context propagation across components
- ✅ Scripts verify CP1 correlation fields in span attributes
- ✅ Scripts provide clear error messages

---

## CP2.3: Grafana Dashboards

**Total Estimate**: 7 days  
**Priority**: Medium  
**Feature Flag**: `CP2_OBSERVABILITY_DASHBOARDS_ENABLED`  
**Wave**: Wave 3 (P2 Important)

### Ticket O1-3.1: Request Dashboard

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without dashboards, cannot visualize HTTP request performance or identify trends  
**Wave**: Wave 3  
**Dependencies**: O1-1.1–O1-1.6 (All metrics endpoints)

**Scope**:
- Create Grafana dashboard for HTTP request metrics
- Dashboard panels: Request rate, request duration (p50/p95/p99), error rate, status code distribution
- Dashboard filters: `component`, `tenant_id`, `method`, `path` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_DASHBOARDS_ENABLED`

**Artifacts**:
- **Dashboard JSON**: 
  - `docs/observability/request-dashboard-grafana.json` - Request dashboard JSON
- **Config**: 
  - `config/grafana/dashboards/request-dashboard.json` - Dashboard configuration (if needed)
- **Docs**: 
  - `docs/observability/DASHBOARDS.md` - Dashboard documentation (Request section)

**Feature Gate**:
```json
{
  "dashboard": {
    "tags": ["cp2", "observability"],
    "editable": true,
    "templating": {
      "list": [
        {
          "name": "component",
          "type": "query",
          "query": "label_values(http_requests_total, component)"
        },
        {
          "name": "tenant_id",
          "type": "query",
          "query": "label_values(http_requests_total, tenant_id)"
        }
      ]
    }
  }
}
```

**Acceptance Criteria**:
- ✅ Dashboard JSON is valid Grafana format
- ✅ Dashboard uses CP1 correlation fields as filters
- ✅ Dashboard panels display all required metrics
- ✅ Dashboard is tested with real metrics
- ✅ Feature flag gates CP2 behavior (dashboard deployment)

---

### Ticket O1-3.2: Routing Dashboard

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without dashboards, cannot visualize routing decision performance or provider selection patterns  
**Wave**: Wave 3  
**Dependencies**: O1-1.1, O1-1.2 (Router metrics)

**Scope**:
- Create Grafana dashboard for routing decision metrics
- Dashboard panels: Decision rate, decision duration (p50/p95/p99), provider selection distribution, policy evaluation duration
- Dashboard filters: `policy_id`, `provider`, `tenant_id`, `run_id` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_DASHBOARDS_ENABLED`

**Artifacts**:
- **Dashboard JSON**: 
  - `docs/observability/routing-dashboard-grafana.json` - Routing dashboard JSON
- **Config**: 
  - `config/grafana/dashboards/routing-dashboard.json` - Dashboard configuration (if needed)
- **Docs**: 
  - `docs/observability/DASHBOARDS.md` - Dashboard documentation (Routing section)

**Feature Gate**:
```json
{
  "dashboard": {
    "tags": ["cp2", "observability", "router"],
    "templating": {
      "list": [
        {
          "name": "policy_id",
          "type": "query",
          "query": "label_values(router_decisions_total, policy_id)"
        },
        {
          "name": "tenant_id",
          "type": "query",
          "query": "label_values(router_decisions_total, tenant_id)"
        }
      ]
    }
  }
}
```

**Acceptance Criteria**:
- ✅ Dashboard JSON is valid Grafana format
- ✅ Dashboard uses CP1 correlation fields as filters
- ✅ Dashboard panels display all required metrics
- ✅ Dashboard is tested with real metrics
- ✅ Feature flag gates CP2 behavior

---

### Ticket O1-3.3: Worker Execution Dashboard

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without dashboards, cannot visualize step execution performance or identify slow steps  
**Wave**: Wave 3  
**Dependencies**: O1-1.5, O1-1.6 (Worker metrics)

**Scope**:
- Create Grafana dashboard for step execution metrics
- Dashboard panels: Step execution rate, step execution duration (p50/p95/p99), step error rate, flow execution duration, queue depth
- Dashboard filters: `step_type`, `execution_status`, `tenant_id`, `run_id`, `flow_id` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_DASHBOARDS_ENABLED`

**Artifacts**:
- **Dashboard JSON**: 
  - `docs/observability/worker-execution-dashboard-grafana.json` - Worker execution dashboard JSON
- **Config**: 
  - `config/grafana/dashboards/worker-execution-dashboard.json` - Dashboard configuration (if needed)
- **Docs**: 
  - `docs/observability/DASHBOARDS.md` - Dashboard documentation (Worker Execution section)

**Feature Gate**:
```json
{
  "dashboard": {
    "tags": ["cp2", "observability", "worker"],
    "templating": {
      "list": [
        {
          "name": "step_type",
          "type": "query",
          "query": "label_values(worker_step_executions_total, step_type)"
        },
        {
          "name": "tenant_id",
          "type": "query",
          "query": "label_values(worker_step_executions_total, tenant_id)"
        }
      ]
    }
  }
}
```

**Acceptance Criteria**:
- ✅ Dashboard JSON is valid Grafana format
- ✅ Dashboard uses CP1 correlation fields as filters
- ✅ Dashboard panels display all required metrics
- ✅ Dashboard is tested with real metrics
- ✅ Feature flag gates CP2 behavior

---

### Ticket O1-3.4: Error Dashboard

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without dashboards, cannot visualize error rates or identify error patterns  
**Wave**: Wave 3  
**Dependencies**: O1-1.1–O1-1.6 (All metrics endpoints)

**Scope**:
- Create Grafana dashboard for error metrics
- Dashboard panels: Error rate by component, error rate by error code, error rate by tenant, error trends
- Dashboard filters: `component`, `error_code`, `tenant_id` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_DASHBOARDS_ENABLED`

**Artifacts**:
- **Dashboard JSON**: 
  - `docs/observability/error-dashboard-grafana.json` - Error dashboard JSON
- **Config**: 
  - `config/grafana/dashboards/error-dashboard.json` - Dashboard configuration (if needed)
- **Docs**: 
  - `docs/observability/DASHBOARDS.md` - Dashboard documentation (Error section)

**Feature Gate**:
```json
{
  "dashboard": {
    "tags": ["cp2", "observability", "errors"],
    "templating": {
      "list": [
        {
          "name": "component",
          "type": "query",
          "query": "label_values(component_errors_total, component)"
        },
        {
          "name": "error_code",
          "type": "query",
          "query": "label_values(component_errors_total, error_code)"
        }
      ]
    }
  }
}
```

**Acceptance Criteria**:
- ✅ Dashboard JSON is valid Grafana format
- ✅ Dashboard uses CP1 correlation fields as filters
- ✅ Dashboard panels display all required metrics
- ✅ Dashboard is tested with real metrics
- ✅ Feature flag gates CP2 behavior

---

### Ticket O1-3.5: Health Dashboard

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without dashboards, cannot visualize component health status or health check performance  
**Wave**: Wave 3  
**Dependencies**: O1-1.1–O1-1.6 (All metrics endpoints)

**Scope**:
- Create Grafana dashboard for health check metrics
- Dashboard panels: Component health status, health check duration, health check failures
- Dashboard filters: `component`, `check` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_DASHBOARDS_ENABLED`

**Artifacts**:
- **Dashboard JSON**: 
  - `docs/observability/health-dashboard-grafana.json` - Health dashboard JSON
- **Config**: 
  - `config/grafana/dashboards/health-dashboard.json` - Dashboard configuration (if needed)
- **Docs**: 
  - `docs/observability/DASHBOARDS.md` - Dashboard documentation (Health section)

**Feature Gate**:
```json
{
  "dashboard": {
    "tags": ["cp2", "observability", "health"],
    "templating": {
      "list": [
        {
          "name": "component",
          "type": "query",
          "query": "label_values(component_health_status, component)"
        }
      ]
    }
  }
}
```

**Acceptance Criteria**:
- ✅ Dashboard JSON is valid Grafana format
- ✅ Dashboard uses CP1 correlation fields as filters
- ✅ Dashboard panels display all required metrics
- ✅ Dashboard is tested with real metrics
- ✅ Feature flag gates CP2 behavior

---

### Ticket O1-3.6: Tracing Dashboard

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without dashboards, cannot visualize trace duration or service dependency graphs  
**Wave**: Wave 3  
**Dependencies**: O1-2.1–O1-2.9 (All tracing implementation)

**Scope**:
- Create Grafana dashboard for distributed tracing
- Dashboard panels: Trace duration distribution, trace error rate, service dependency graph, trace search
- Dashboard filters: `service.name`, `tenant.id`, `run.id`, `flow.id` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_DASHBOARDS_ENABLED`

**Artifacts**:
- **Dashboard JSON**: 
  - `docs/observability/tracing-dashboard-grafana.json` - Tracing dashboard JSON
- **Config**: 
  - `config/grafana/dashboards/tracing-dashboard.json` - Dashboard configuration (if needed)
- **Docs**: 
  - `docs/observability/DASHBOARDS.md` - Dashboard documentation (Tracing section)

**Feature Gate**:
```json
{
  "dashboard": {
    "tags": ["cp2", "observability", "tracing"],
    "templating": {
      "list": [
        {
          "name": "service_name",
          "type": "query",
          "query": "label_values(trace_duration_seconds, service.name)"
        },
        {
          "name": "tenant_id",
          "type": "query",
          "query": "label_values(trace_duration_seconds, tenant.id)"
        }
      ]
    }
  }
}
```

**Acceptance Criteria**:
- ✅ Dashboard JSON is valid Grafana format
- ✅ Dashboard uses CP1 correlation fields as filters
- ✅ Dashboard panels display all required metrics
- ✅ Dashboard is tested with real traces
- ✅ Feature flag gates CP2 behavior

---

### Ticket O1-3.7: Dashboard Validation Script

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without validation, cannot ensure dashboards are correctly formatted or use CP1 correlation fields  
**Wave**: Wave 3  
**Dependencies**: O1-3.1–O1-3.6 (All dashboards)

**Scope**:
- Create bash script for dashboard validation
- Create PowerShell script for Windows compatibility
- Validate Grafana JSON format
- Check dashboard structure
- Verify CP1 correlation fields in filters

**Artifacts**:
- **Scripts**: 
  - `scripts/observability/validate_dashboards.sh` - Bash validation script
  - `scripts/observability/validate_dashboards.ps1` - PowerShell validation script
- **Docs**: 
  - `docs/observability/DASHBOARD_VALIDATION.md` - Dashboard validation guide

**Acceptance Criteria**:
- ✅ Validation scripts work on Linux, macOS, Windows (WSL)
- ✅ Scripts validate all dashboard JSON files
- ✅ Scripts check Grafana format compliance
- ✅ Scripts verify CP1 correlation fields in filters
- ✅ Scripts provide clear error messages

---

## CP2.5: Extensions Observability Slice

**Status**: 📋 **PLANNED**  
**Estimated Effort**: 3-4 days  
**Dependencies**: CP2.1 (Metrics), CP2.2 (Tracing)  
**Priority**: High (required for CP2-LC Extensions support)  
**Feature Flag**: `CP2_OBSERVABILITY_METRICS_ENABLED`, `CP2_OBSERVABILITY_TRACING_ENABLED`  
**Wave**: Wave 1-2 (with CP2.1 and CP2.2)

### Overview

This section covers observability instrumentation for the Extensions Pipeline, including:
- Extension Invoker metrics and spans
- Extension Registry metrics
- Circuit Breaker metrics and spans
- Integration with CP2 OTEL/Prometheus specifications

**References**:
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - CP2 metrics specification (Extension Metrics section)
- `docs/archive/dev/OBSERVABILITY_TRACING_SPEC_CP2.md` - CP2 tracing specification (Extension Spans section)
- `docs/archive/dev/EXTENSIONS_TRACE_WALKTHROUGH.md` - Complete trace-walkthrough example
- `docs/archive/dev/EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md` - Extensions implementation details

### Ticket O1-5.1: Extension Invoker Metrics

**Status**: 📋 **PLANNED**  
**Estimated Effort**: 1 day  
**Dependencies**: O1-1.1 (Router Metrics Collection)  
**Priority**: High  
**Wave**: Wave 1 (with CP2.1)

**Scope**:
- Add Prometheus metrics for extension invocations
- Align with CP2 metrics specification
- Use CP1 correlation fields (tenant_id, policy_id) as labels
- Support extension type, status, and latency tracking

**Required Metrics**:
1. **`router_extension_invocations_total`** (Counter)
   - Labels: `extension_id`, `extension_type`, `status`, `tenant_id` (optional), `policy_id` (optional)
   - Description: Total extension invocations by status (success, error, timeout, max_retries_exceeded)

2. **`router_extension_invocation_duration_seconds`** (Histogram)
   - Labels: `extension_id`, `extension_type`, `status`, `tenant_id` (optional), `policy_id` (optional)
   - Buckets: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0]
   - Description: Extension invocation latency distribution

3. **`router_extension_retries_total`** (Counter)
   - Labels: `extension_id`, `extension_type`, `retry_reason`
   - Description: Total retry attempts (timeout, error)

**Current Telemetry Events**:
- `[router_extension_invoker, invocation_total]` - Legacy telemetry event
- Metadata: `extension_id`, `type`, `subject`, `status`, `latency_ms`, `retries_used`, `tenant_id`, `policy_id`

**Mapping to CP2 Metrics**:
- `invocation_total` → `router_extension_invocations_total` (counter)
- `latency_ms` → `router_extension_invocation_duration_seconds` (histogram)
- `retries_used` → `router_extension_retries_total` (counter)

**Artifacts**:
- **Code**: Update `router_extension_invoker.erl` to emit Prometheus metrics
- **Tests**: Unit tests for metric emission
- **Docs**: Update `OBSERVABILITY_METRICS_SPEC_CP2.md` with extension metrics section

**Acceptance Criteria**:
- ✅ All extension invocations emit Prometheus metrics
- ✅ Metrics use CP2 naming conventions (`router_extension_*`)
- ✅ CP1 correlation fields (`tenant_id`, `policy_id`) included as labels when available
- ✅ Histogram buckets match CP2 specification
- ✅ Legacy telemetry events remain for backward compatibility

---

### Ticket O1-5.2: Extension Registry Metrics

**Status**: 📋 **PLANNED**  
**Estimated Effort**: 0.5 days  
**Dependencies**: O1-1.1 (Router Metrics Collection)  
**Priority**: Medium  
**Wave**: Wave 1 (with CP2.1)

**Scope**:
- Add Prometheus metrics for extension registry operations
- Track registry lookups, cache hits/misses, load operations

**Required Metrics**:
1. **`router_extension_registry_lookups_total`** (Counter)
   - Labels: `lookup_status` (success, not_found, error)
   - Description: Total extension registry lookups

2. **`router_extension_registry_cache_hits_total`** (Counter)
   - Labels: None
   - Description: Extension registry cache hits

3. **`router_extension_registry_cache_misses_total`** (Counter)
   - Labels: None
   - Description: Extension registry cache misses

4. **`router_extension_registry_load_duration_seconds`** (Histogram)
   - Labels: `load_source` (database, cache, fixtures)
   - Buckets: [0.001, 0.005, 0.01, 0.05, 0.1, 0.5]
   - Description: Extension registry load operation latency

**Artifacts**:
- **Code**: Update `router_extension_registry.erl` to emit Prometheus metrics
- **Tests**: Unit tests for metric emission
- **Docs**: Update `OBSERVABILITY_METRICS_SPEC_CP2.md`

**Acceptance Criteria**:
- ✅ Registry operations emit Prometheus metrics
- ✅ Cache hit/miss tracking works correctly
- ✅ Load duration tracked for all load sources

---

### Ticket O1-5.3: Circuit Breaker Metrics

**Status**: 📋 **PLANNED**  
**Estimated Effort**: 0.5 days  
**Dependencies**: O1-1.1 (Router Metrics Collection)  
**Priority**: Medium  
**Wave**: Wave 1 (with CP2.1)

**Scope**:
- Add Prometheus metrics for circuit breaker state transitions
- Track circuit breaker opens, closes, half-open attempts

**Required Metrics**:
1. **`router_extension_circuit_breaker_state`** (Gauge)
   - Labels: `extension_id`, `state` (closed, open, half_open)
   - Description: Current circuit breaker state per extension

2. **`router_extension_circuit_breaker_transitions_total`** (Counter)
   - Labels: `extension_id`, `transition` (open, close, half_open)
   - Description: Total circuit breaker state transitions

3. **`router_extension_circuit_breaker_success_total`** (Counter)
   - Labels: `extension_id`
   - Description: Total successful invocations recorded by circuit breaker

4. **`router_extension_circuit_breaker_failure_total`** (Counter)
   - Labels: `extension_id`
   - Description: Total failed invocations recorded by circuit breaker

**Current Telemetry Events**:
- `[router_extension_circuit_breaker, success_total]` - Legacy telemetry event
- `[router_extension_circuit_breaker, failure_total]` - Legacy telemetry event

**Mapping to CP2 Metrics**:
- `success_total` → `router_extension_circuit_breaker_success_total` (counter)
- `failure_total` → `router_extension_circuit_breaker_failure_total` (counter)
- Add new gauge for state tracking

**Artifacts**:
- **Code**: Update `router_extension_circuit_breaker.erl` to emit Prometheus metrics
- **Tests**: Unit tests for metric emission
- **Docs**: Update `OBSERVABILITY_METRICS_SPEC_CP2.md`

**Acceptance Criteria**:
- ✅ Circuit breaker state tracked as gauge metric
- ✅ State transitions tracked as counter metric
- ✅ Success/failure counters match legacy telemetry events
- ✅ Legacy telemetry events remain for backward compatibility

---

### Ticket O1-5.4: Extension Invoker Spans

**Status**: 📋 **PLANNED**  
**Estimated Effort**: 1 day  
**Dependencies**: O1-2.2 (Router Span Creation), O1-2.3 (Router Context Propagation)  
**Priority**: High  
**Wave**: Wave 2 (with CP2.2)

**Scope**:
- Add OpenTelemetry spans for extension invocations
- Integrate with router.decide span hierarchy
- Propagate trace context to extension services via NATS

**Required Spans**:
1. **`router.extension.invoke`** (Child of `router.decide`)
   - Span Kind: `CLIENT`
   - Parent: `router.decide`
   - Attributes:
     - `service.name`: `"router"`
     - `tenant.id`: From context
     - `run.id`: From context (optional)
     - `flow.id`: From context (optional)
     - `extension.id`: Extension identifier
     - `extension.type`: Extension type (pre, validator, post, provider)
     - `extension.subject`: NATS subject
     - `extension.status`: Invocation status (success, error, timeout)
     - `extension.retries_used`: Number of retries
     - `policy.id`: Policy identifier (optional)

2. **`router.extension.registry.lookup`** (Child of `router.extension.invoke`)
   - Span Kind: `INTERNAL`
   - Parent: `router.extension.invoke`
   - Attributes:
     - `service.name`: `"router"`
     - `extension.id`: Extension identifier
     - `lookup.status`: Lookup result (success, not_found, error)
     - `lookup.source`: Lookup source (cache, database, fixtures)

**Context Propagation**:
- Inject `traceparent` header in NATS message headers
- Extract `traceparent` from extension service responses
- Support W3C Trace Context format

**Artifacts**:
- **Code**: Update `router_extension_invoker.erl` to create OTEL spans
- **Code**: Update NATS message building to include trace context
- **Tests**: Unit tests for span creation and context propagation
- **Docs**: Update `OBSERVABILITY_TRACING_SPEC_CP2.md` with extension spans

**Acceptance Criteria**:
- ✅ Extension invocations create OTEL spans
- ✅ Spans are children of `router.decide` span
- ✅ Trace context propagated to extension services via NATS
- ✅ All CP1 correlation fields included as span attributes
- ✅ Span status set correctly (OK, ERROR) based on invocation result

---

### Ticket O1-5.5: Extension Trace-Walkthrough Documentation

**Status**: 📋 **PLANNED**  
**Estimated Effort**: 0.5 days  
**Dependencies**: O1-5.4 (Extension Invoker Spans)  
**Priority**: Medium  
**Wave**: Wave 2 (with CP2.2)

**Scope**:
- Create trace-walkthrough example showing end-to-end trace through Gateway → Router → Extension → Provider
- Document span hierarchy and attributes
- Provide example trace JSON

**Artifacts**:
- **Docs**: `docs/archive/dev/EXTENSIONS_TRACE_WALKTHROUGH.md` - Complete trace-walkthrough example (✅ **CREATED**)
- **Examples**: Example trace JSON with all spans and attributes

**Acceptance Criteria**:
- ✅ Trace-walkthrough shows complete flow: Gateway → Router → Extension → Provider
- ✅ All CP1 correlation fields shown in span attributes
- ✅ Example trace JSON is valid and complete
- ✅ Documentation includes span hierarchy diagram

---

## CP2.4: Alerting Rules

**Total Estimate**: 5 days  
**Priority**: Medium  
**Feature Flag**: `CP2_OBSERVABILITY_ALERTING_ENABLED`  
**Wave**: Wave 4 (P2 Important)

### Ticket O1-4.1: Error Rate Alerts

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without alerts, cannot be notified of high error rates or component failures  
**Wave**: Wave 4  
**Dependencies**: O1-1.1–O1-1.6 (All metrics endpoints)

**Scope**:
- Create Alertmanager rules for error rate monitoring
- Alert rules: High error rate, component high error rate, error code high error rate
- Alert labels: `component`, `error_code`, `tenant_id`, `run_id` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_ALERTING_ENABLED`

**Artifacts**:
- **Alert Rules**: 
  - `docs/observability/alert-rules.yaml` - Alertmanager rules (Error Rate section)
- **Config**: 
  - `config/alertmanager/alert-rules.yaml` - Alert rules configuration (if needed)
- **Docs**: 
  - `docs/observability/ALERTING.md` - Alerting documentation (Error Rate section)

**Feature Gate**:
```yaml
# In alert-rules.yaml
groups:
  - name: cp2_error_rate_alerts
    rules:
      - alert: HighErrorRate
        expr: rate(component_errors_total[5m]) > 0.05
        labels:
          severity: critical
          feature_flag: CP2_OBSERVABILITY_ALERTING_ENABLED
        annotations:
          summary: "High error rate detected"
          description: "Error rate > 5% for 5 minutes"
```

**Acceptance Criteria**:
- ✅ Alert rules are valid Alertmanager format
- ✅ Alert rules use CP1 correlation fields as labels
- ✅ Alert rules are tested with Alertmanager
- ✅ Feature flag gates CP2 behavior (alert rule deployment)

---

### Ticket O1-4.2: Latency Alerts

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without alerts, cannot be notified of high latency or performance degradation  
**Wave**: Wave 4  
**Dependencies**: O1-1.1–O1-1.6 (All metrics endpoints)

**Scope**:
- Create Alertmanager rules for latency monitoring
- Alert rules: High latency (p95), high latency (p99), component high latency
- Alert labels: `component`, `tenant_id`, `run_id` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_ALERTING_ENABLED`

**Artifacts**:
- **Alert Rules**: 
  - `docs/observability/alert-rules.yaml` - Alertmanager rules (Latency section)
- **Config**: 
  - `config/alertmanager/alert-rules.yaml` - Alert rules configuration (if needed)
- **Docs**: 
  - `docs/observability/ALERTING.md` - Alerting documentation (Latency section)

**Feature Gate**:
```yaml
# In alert-rules.yaml
groups:
  - name: cp2_latency_alerts
    rules:
      - alert: HighLatency
        expr: histogram_quantile(0.95, http_request_duration_seconds) > 1.0
        labels:
          severity: warning
          feature_flag: CP2_OBSERVABILITY_ALERTING_ENABLED
        annotations:
          summary: "High latency detected"
          description: "p95 latency > 1s for 5 minutes"
```

**Acceptance Criteria**:
- ✅ Alert rules are valid Alertmanager format
- ✅ Alert rules use CP1 correlation fields as labels
- ✅ Alert rules are tested with Alertmanager
- ✅ Feature flag gates CP2 behavior

---

### Ticket O1-4.3: Health Check Alerts

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without alerts, cannot be notified of component health failures or health check timeouts  
**Wave**: Wave 4  
**Dependencies**: O1-1.1–O1-1.6 (All metrics endpoints)

**Scope**:
- Create Alertmanager rules for health check monitoring
- Alert rules: Unhealthy component, health check failure, health check timeout
- Alert labels: `component`, `check` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_ALERTING_ENABLED`

**Artifacts**:
- **Alert Rules**: 
  - `docs/observability/alert-rules.yaml` - Alertmanager rules (Health Check section)
- **Config**: 
  - `config/alertmanager/alert-rules.yaml` - Alert rules configuration (if needed)
- **Docs**: 
  - `docs/observability/ALERTING.md` - Alerting documentation (Health Check section)

**Feature Gate**:
```yaml
# In alert-rules.yaml
groups:
  - name: cp2_health_check_alerts
    rules:
      - alert: UnhealthyComponent
        expr: component_health_status == 0
        labels:
          severity: critical
          feature_flag: CP2_OBSERVABILITY_ALERTING_ENABLED
        annotations:
          summary: "Component is unhealthy"
          description: "Component health status is 0"
```

**Acceptance Criteria**:
- ✅ Alert rules are valid Alertmanager format
- ✅ Alert rules use CP1 correlation fields as labels
- ✅ Alert rules are tested with Alertmanager
- ✅ Feature flag gates CP2 behavior

---

### Ticket O1-4.4: Resource Alerts

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without alerts, cannot be notified of resource exhaustion or queue depth issues  
**Wave**: Wave 4  
**Dependencies**: O1-1.1–O1-1.6 (All metrics endpoints)

**Scope**:
- Create Alertmanager rules for resource monitoring
- Alert rules: High queue depth, high memory usage, high CPU usage
- Alert labels: `component`, `tenant_id` (CP1 correlation fields)
- Feature flag: `CP2_OBSERVABILITY_ALERTING_ENABLED`

**Artifacts**:
- **Alert Rules**: 
  - `docs/observability/alert-rules.yaml` - Alertmanager rules (Resource section)
- **Config**: 
  - `config/alertmanager/alert-rules.yaml` - Alert rules configuration (if needed)
- **Docs**: 
  - `docs/observability/ALERTING.md` - Alerting documentation (Resource section)

**Feature Gate**:
```yaml
# In alert-rules.yaml
groups:
  - name: cp2_resource_alerts
    rules:
      - alert: HighQueueDepth
        expr: worker_queue_depth > 80
        labels:
          severity: warning
          feature_flag: CP2_OBSERVABILITY_ALERTING_ENABLED
        annotations:
          summary: "High queue depth detected"
          description: "Worker queue depth > 80% capacity"
```

**Acceptance Criteria**:
- ✅ Alert rules are valid Alertmanager format
- ✅ Alert rules use CP1 correlation fields as labels
- ✅ Alert rules are tested with Alertmanager
- ✅ Feature flag gates CP2 behavior

---

### Ticket O1-4.5: Alert Validation Script

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without validation, cannot ensure alert rules are correctly formatted or use CP1 correlation fields  
**Wave**: Wave 4  
**Dependencies**: O1-4.1–O1-4.4 (All alert rules)

**Scope**:
- Create bash script for alert validation
- Create PowerShell script for Windows compatibility
- Validate Alertmanager YAML format
- Check alert rule structure
- Verify CP1 correlation fields in labels

**Artifacts**:
- **Scripts**: 
  - `scripts/observability/validate_alerts.sh` - Bash validation script
  - `scripts/observability/validate_alerts.ps1` - PowerShell validation script
- **Docs**: 
  - `docs/observability/ALERT_VALIDATION.md` - Alert validation guide

**Acceptance Criteria**:
- ✅ Validation scripts work on Linux, macOS, Windows (WSL)
- ✅ Scripts validate all alert rule YAML files
- ✅ Scripts check Alertmanager format compliance
- ✅ Scripts verify CP1 correlation fields in labels
- ✅ Scripts provide clear error messages

---

## Task Dependencies Graph

```
CP2.1: Prometheus Metrics Export (7d)
├─ O1-1.1: Router Metrics Collection (1d)
├─ O1-1.2: Router Metrics Endpoint (1d) [Depends: O1-1.1]
├─ O1-1.3: Gateway Metrics Collection (1d)
├─ O1-1.4: Gateway Metrics Endpoint (1d) [Depends: O1-1.3]
├─ O1-1.5: Worker Metrics Collection (1d)
├─ O1-1.6: Worker Metrics Endpoint (1d) [Depends: O1-1.5]
└─ O1-1.7: Metrics Validation Script (1d) [Depends: O1-1.2, O1-1.4, O1-1.6]

CP2.2: OpenTelemetry Distributed Tracing (7d)
├─ O1-2.1: Router OTEL SDK Integration (1d) [Depends: O1-1.1, O1-1.2]
├─ O1-2.2: Router Span Creation (1d) [Depends: O1-2.1]
├─ O1-2.3: Router Context Propagation (1d) [Depends: O1-2.2]
├─ O1-2.4: Gateway OTEL SDK Integration (1d) [Depends: O1-1.3, O1-1.4]
├─ O1-2.5: Gateway Span Creation (1d) [Depends: O1-2.4]
├─ O1-2.6: Gateway Context Propagation (1d) [Depends: O1-2.5]
├─ O1-2.7: Worker OTEL SDK Integration (1d) [Depends: O1-1.5, O1-1.6]
├─ O1-2.8: Worker Span Creation (1d) [Depends: O1-2.7]
├─ O1-2.9: Worker Context Propagation (1d) [Depends: O1-2.8]
└─ O1-2.10: Trace Validation Script (1d) [Depends: O1-2.3, O1-2.6, O1-2.9]

CP2.3: Grafana Dashboards (7d)
├─ O1-3.1: Request Dashboard (1d) [Depends: O1-1.1–O1-1.6]
├─ O1-3.2: Routing Dashboard (1d) [Depends: O1-1.1, O1-1.2]
├─ O1-3.3: Worker Execution Dashboard (1d) [Depends: O1-1.5, O1-1.6]
├─ O1-3.4: Error Dashboard (1d) [Depends: O1-1.1–O1-1.6]
├─ O1-3.5: Health Dashboard (1d) [Depends: O1-1.1–O1-1.6]
├─ O1-3.6: Tracing Dashboard (1d) [Depends: O1-2.1–O1-2.9]
└─ O1-3.7: Dashboard Validation Script (1d) [Depends: O1-3.1–O1-3.6]

CP2.4: Alerting Rules (5d)
├─ O1-4.1: Error Rate Alerts (1d) [Depends: O1-1.1–O1-1.6]
├─ O1-4.2: Latency Alerts (1d) [Depends: O1-1.1–O1-1.6]
├─ O1-4.3: Health Check Alerts (1d) [Depends: O1-1.1–O1-1.6]
├─ O1-4.4: Resource Alerts (1d) [Depends: O1-1.1–O1-1.6]
└─ O1-4.5: Alert Validation Script (1d) [Depends: O1-4.1–O1-4.4]

CP2.5: Extensions Observability Slice (3-4d)
├─ O1-5.1: Extension Invoker Metrics (1d) [Depends: O1-1.1]
├─ O1-5.2: Extension Registry Metrics (0.5d) [Depends: O1-1.1]
├─ O1-5.3: Circuit Breaker Metrics (0.5d) [Depends: O1-1.1]
├─ O1-5.4: Extension Invoker Spans (1d) [Depends: O1-2.2, O1-2.3]
└─ O1-5.5: Extension Trace-Walkthrough Documentation (0.5d) [Depends: O1-5.4]
```

---

## Implementation Order Recommendation

### Phase 1: Metrics Foundation (Week 1-2)
1. **O1-1.1**: Router Metrics Collection (1d)
2. **O1-1.2**: Router Metrics Endpoint (1d)
3. **O1-1.3**: Gateway Metrics Collection (1d)
4. **O1-1.4**: Gateway Metrics Endpoint (1d)
5. **O1-1.5**: Worker Metrics Collection (1d)
6. **O1-1.6**: Worker Metrics Endpoint (1d)
7. **O1-1.7**: Metrics Validation Script (1d)

**Total**: 7 days

### Phase 2: Distributed Tracing (Week 3)
1. **O1-2.1**: Router OTEL SDK Integration (1d)
2. **O1-2.2**: Router Span Creation (1d)
3. **O1-2.3**: Router Context Propagation (1d)
4. **O1-2.4**: Gateway OTEL SDK Integration (1d)
5. **O1-2.5**: Gateway Span Creation (1d)
6. **O1-2.6**: Gateway Context Propagation (1d)
7. **O1-2.7**: Worker OTEL SDK Integration (1d)
8. **O1-2.8**: Worker Span Creation (1d)
9. **O1-2.9**: Worker Context Propagation (1d)
10. **O1-2.10**: Trace Validation Script (1d)

**Total**: 10 days (can be parallelized across components)

### Phase 3: Dashboards (Week 4)
1. **O1-3.1**: Request Dashboard (1d)
2. **O1-3.2**: Routing Dashboard (1d)
3. **O1-3.3**: Worker Execution Dashboard (1d)
4. **O1-3.4**: Error Dashboard (1d)
5. **O1-3.5**: Health Dashboard (1d)
6. **O1-3.6**: Tracing Dashboard (1d)
7. **O1-3.7**: Dashboard Validation Script (1d)

**Total**: 7 days

### Phase 4: Alerting (Week 5)
1. **O1-4.1**: Error Rate Alerts (1d)
2. **O1-4.2**: Latency Alerts (1d)
3. **O1-4.3**: Health Check Alerts (1d)
4. **O1-4.4**: Resource Alerts (1d)
5. **O1-4.5**: Alert Validation Script (1d)

**Total**: 5 days

**Grand Total**: 29 days (~6 weeks, with parallelization ~4-5 weeks)

---

## Acceptance Criteria Summary

### CP2.1: Prometheus Metrics Export
- ✅ All components expose `/metrics` endpoint
- ✅ Metrics use CP1 correlation fields as labels
- ✅ Label cardinality is acceptable
- ✅ Metrics validation scripts pass
- ✅ Feature flags protect CP1 baseline

### CP2.2: OpenTelemetry Distributed Tracing
- ✅ All components create OTEL spans
- ✅ Trace context is propagated across components
- ✅ Spans include CP1 correlation fields as attributes
- ✅ Traces are exported to OTEL collector
- ✅ Trace validation scripts pass
- ✅ Feature flags protect CP1 baseline

### CP2.3: Grafana Dashboards
- ✅ All dashboards are created and documented
- ✅ Dashboards use CP1 correlation fields as filters
- ✅ Dashboard JSON files are valid Grafana format
- ✅ Dashboards are tested with real metrics/traces
- ✅ Dashboard validation scripts pass
- ✅ Feature flags protect CP1 baseline

### CP2.4: Alerting Rules
- ✅ Alert rules are created and documented
- ✅ Alert rules use CP1 correlation fields as labels
- ✅ Alert rules are tested with Alertmanager
- ✅ Alert validation scripts pass
- ✅ Feature flags protect CP1 baseline

---

## References

- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - High-level CP2 observability plan
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - Detailed CP2 extension plan
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/archive/dev/OBSERVABILITY_CP1_COMPLETION_REPORT.md` - CP1 observability completion report
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` - Worker reliability backlog (structure reference)

---

## Change History

**v2.0 (2025-01-27)**:
- Restructured as executable backlog with tickets O1-X.Y
- Added feature gate strategy with code examples
- Added detailed ticket structure (Estimate, Priority, Wave, Dependencies, Scope, Artifacts, Feature Gate, Acceptance Criteria)
- Organized by component (Router/Gateway/Worker) and feature (Metrics/Tracing/Dashboards/Alerting)
- Added implementation order recommendation

**v1.0 (2025-01-27)**:
- Initial backlog creation
- Breakdown of CP2.1–CP2.4 into executable tasks by component
- Artifact descriptions for each task
- Task dependencies and implementation order
