---
version: 1.0
status: accepted
date: 2025-01-27
deciders:
  - CP1-ROUTER Implementation Team
related_adrs:
  - ADR-004: Erlang/OTP for Router (mentioned metrics)
  - ADR-010: Target Architecture (mentioned tracing)
supersedes: []
superseded_by: []
---

# ADR-014: Metrics and Distributed Tracing

## Status

accepted

## Context

Router needs observability for:
- Performance monitoring (latency, throughput)
- Error tracking and debugging
- Distributed tracing across services
- Business metrics (usage, costs)
- Operational metrics (RBAC, quota, rate limiting)

**Requirements**:
- Metrics collection and export
- Distributed tracing with context propagation
- Integration with OpenTelemetry standards
- Low overhead
- Structured logging

## Decision

Implement **unified observability layer** using:
1. **Telemetry** (Erlang/OTP): For metrics and events
2. **OpenTelemetry**: For distributed tracing
3. **Structured Logging**: For operational logs

**Key Components**:

1. **Metrics (Telemetry)**:
   - **Router Metrics**: Request counts, latencies, errors
   - **RBAC Metrics**: Permission checks, cache hits/misses
   - **Quota Metrics**: Violations, usage
   - **Rate Limiting Metrics**: Violations, checks
   - **Audit Metrics**: Log entries, write duration
   - **Idempotency Metrics**: Hits, misses, errors
   - **JetStream Metrics**: Redeliveries, MaxDeliver exhaustion
   - **Tenant Validation Metrics**: Audit events, rejections

2. **Distributed Tracing (OpenTelemetry)**:
   - **Span Management**: Start/end spans with attributes
   - **Context Propagation**: Extract/inject trace context (W3C Trace Context)
   - **Span Attributes**: Add metadata to spans
   - **Span Status**: Set success/error status
   - **Parent Context**: Support parent-child span relationships

3. **Structured Logging**:
   - **JSON Format**: Structured logs with metadata
   - **Log Levels**: ERROR, WARN, INFO, DEBUG
   - **Context**: Include trace_id, tenant_id, correlation IDs
   - **PII Filtering**: Filter sensitive data before logging

4. **Integration Points**:
   - **NATS Headers**: Extract/inject trace context in headers
   - **gRPC Metadata**: Extract/inject trace context in metadata
   - **Process Dictionary**: Store trace_id for logging

**Implementation**:
- `router_telemetry_helper.erl`: Telemetry wrapper and helpers
- `router_tracing.erl`: OpenTelemetry tracing integration
- `router_metrics.erl`: Metrics definitions
- `router_prometheus.erl`: Prometheus text format export (CP2)
- `router_metrics_http.erl`: HTTP `/metrics` endpoint on port 9001 (CP2)
- `router_logger.erl`: Structured logging

**CP2 Prometheus Metrics**:
- HTTP endpoint: `GET /metrics` on port 9001 (separate from gRPC port 9000)
- Format: Prometheus text format (RFC 4180) with HELP and TYPE headers
- Base metrics contract: JetStream (`router_jetstream_ack_total`, `router_jetstream_redelivery_total`, `router_dlq_total`), Idempotency (`router_idem_hits_total`, `router_idem_miss_total`), ACL (`router_acl_allowed_total`, `router_acl_denied_total`)
- **Note**: `router_jetstream_redelivery_total` supports labels (`assignment_id`, `request_id`, `reason`, `source`) for detailed observability (2025-01-27)
- Export contract: Primary HTTP endpoint for production scraping, secondary file dump for testing
- Synchronization: Router (port 9001) and Gateway (port 3001) use same Prometheus format

## Consequences

### Positive

- **Observability**: Complete visibility into system behavior
- **Debugging**: Distributed tracing enables end-to-end debugging
- **Performance Monitoring**: Metrics enable performance analysis
- **Standards Compliance**: OpenTelemetry and Telemetry are industry standards
- **Low Overhead**: Erlang/OTP telemetry is efficient
- **Context Propagation**: Trace context flows across services

### Negative

- **Complexity**: Multiple observability systems to manage
- **Configuration**: Requires OpenTelemetry collector configuration
- **Storage**: Traces and metrics require storage infrastructure
- **Overhead**: Tracing adds slight performance overhead

### Neutral

- **Tooling**: Need OpenTelemetry collector and visualization tools
- **Sampling**: May need trace sampling for high-volume systems

## Alternatives Considered

### Alternative 1: Prometheus Only

**Description**: Use only Prometheus for metrics, no tracing

**Pros**:
- Simpler implementation
- Mature ecosystem
- Good for metrics

**Cons**:
- No distributed tracing
- Limited debugging capabilities
- No context propagation

**Why not chosen**: Distributed tracing is essential for debugging

### Alternative 2: OpenTelemetry Only

**Description**: Use only OpenTelemetry for both metrics and tracing

**Pros**:
- Unified observability
- Single standard
- Rich context

**Cons**:
- Less mature in Erlang ecosystem
- Higher overhead
- More complex integration

**Why not chosen**: Erlang/OTP Telemetry is more efficient for metrics

### Alternative 3: Custom Observability

**Description**: Build custom observability solution

**Pros**:
- Full control
- Optimized for specific needs

**Cons**:
- High development cost
- Maintenance burden
- Not standardized

**Why not chosen**: Standards provide better ecosystem support

## Implementation Notes

**Telemetry Events**:
```erlang
router_telemetry_helper:execute([router_rbac, check_total], #{count => 1}, #{
    user_id => UserId,
    tenant_id => TenantId,
    action => Action,
    resource => Resource,
    allowed => Result
})
```

**Tracing Spans**:
```erlang
router_tracing:with_span(?SPAN_ROUTER_PROCESS_RESULT, Attributes, ParentContext, fun() ->
    process_result()
end)
```

**Context Propagation**:
- **NATS**: Inject trace context in headers (`traceparent`, `tracestate`)
- **gRPC**: Extract trace context from metadata
- **Process Dictionary**: Store `trace_id` for logging

**Span Names**:
- `beamline.router.decide`: Routing decision
- `beamline.router.process.result`: Result processing
- `beamline.router.process.ack`: ACK processing
- `beamline.router.emit.usage`: Usage event emission

**Metrics Categories**:
- Router: `router_grpc.*`, `router_nats.*`
- RBAC: `router_rbac.*`
- Quota: `router_quota.*`
- Rate Limiting: `router_rate_limiter.*`
- Audit: `router_audit.*`
- Idempotency: `router_idempotency.*`
- Tenant Validation: `router_tenant_validator.*`
- JetStream: `router_jetstream.*`

## References

- `apps/otp/router/src/router_telemetry_helper.erl`: Telemetry implementation
- `apps/otp/router/src/router_tracing.erl`: OpenTelemetry tracing
- `apps/otp/router/src/router_metrics.erl`: Metrics definitions
- `apps/otp/router/src/router_logger.erl`: Structured logging
- `docs/OBSERVABILITY.md`: Observability specification
- ADR-004: Erlang/OTP for Router (mentioned metrics)
- ADR-010: Target Architecture (mentioned tracing)

## Updates

**2025-01-27**: Enhanced `router_jetstream_redelivery_total` metric with full label support:
- Added labels: `assignment_id`, `request_id`, `reason`, `source`
- ETS storage with labeled metrics support (`{{Name, LabelsKey}, Value}`)
- Prometheus export with labeled format: `metric_name{label1="value1",...} value`
- Structured logging for correlation: `"Message redelivery requested"` (INFO level)
- Full fault injection coverage (tenant validation, backpressure, ACK/NAK errors)
- See: `../../apps/otp/router/docs/archive/dev_reports/OBSERVABILITY_REDELIVERY_METRICS_TASK_COMPLETE.md`

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy
- ✅ Respects security constraints
- ✅ Integrates with STATE/HISTORY

