# Extensions Trace-Walkthrough

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: 📋 **SPECIFICATION DOCUMENT** (CP2)  
**Control Point**: CP2-LC  
**WORKER**: `wrk-obs1` (Observability CP2) + `wrk-2` (Router OTP)

---

## Purpose

This document provides a complete trace-walkthrough example showing end-to-end distributed tracing through the Extensions Pipeline: Gateway → Router → Extension → Provider.

**Key Features**:
- Complete span hierarchy with all CP1 correlation fields
- Extension-specific spans (`router.extension.invoke`, `router.extension.registry.lookup`)
- Trace context propagation via NATS
- Example trace JSON with all attributes

**References**:
- `docs/archive/dev/OBSERVABILITY_TRACING_SPEC_CP2.md` - CP2 tracing specification
- `docs/archive/dev/EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md` - Extensions implementation
- `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` - CP1 correlation fields

---

## Trace Scenario

**Request Flow**:
1. Gateway receives HTTP POST `/api/v1/messages` with message payload
2. Gateway forwards to Router via NATS (`beamline.router.v1.decide`)
3. Router loads policy and executes extension pipeline:
   - Pre-processor: `normalize_text` (NATS subject: `beamline.ext.pre.normalize_text.v1`)
   - Validator: `pii_guard` (NATS subject: `beamline.ext.validate.pii_guard.v1`)
   - Provider selection: `openai:gpt-4o`
   - Post-processor: `mask_pii` (NATS subject: `beamline.ext.post.mask_pii.v1`)
4. Router returns decision to Gateway
5. Gateway returns HTTP 200 OK response

**CP1 Correlation Fields**:
- `tenant_id`: `"tenant_123"`
- `run_id`: `"run_abc123"`
- `flow_id`: `"flow_xyz789"`
- `trace_id`: `"4bf92f3577b34da6a3ce929d0e0e4736"` (W3C Trace Context format)
- `policy_id`: `"default"`

---

## Complete Trace Structure

```
Trace (trace_id: 4bf92f3577b34da6a3ce929d0e0e4736)
│
├── Span: gateway.request (span_id: 00f067aa0ba902b7)
│   │ Kind: SERVER
│   │ Parent: None (root span)
│   │ Attributes: tenant.id, run.id, http.method, http.path, http.status_code
│   │
│   ├── Span: router.decide (span_id: a3ce929d0e0e4736)
│   │   │ Kind: INTERNAL
│   │   │ Parent: gateway.request
│   │   │ Attributes: tenant.id, run.id, flow.id, subject, route
│   │   │
│   │   ├── Span: router.policy.load (span_id: b4df03ae1f5e5847)
│   │   │   │ Kind: INTERNAL
│   │   │   │ Parent: router.decide
│   │   │   │ Attributes: tenant.id, run.id, policy.id
│   │   │
│   │   ├── Span: router.extension.invoke (span_id: c5ef14bf2g6f6958)
│   │   │   │ Kind: CLIENT
│   │   │   │ Parent: router.decide
│   │   │   │ Attributes: tenant.id, run.id, flow.id, extension.id, extension.type, extension.subject, extension.status, policy.id
│   │   │   │
│   │   │   ├── Span: router.extension.registry.lookup (span_id: d6fg25cg3h7g7a69)
│   │   │   │   │ Kind: INTERNAL
│   │   │   │   │ Parent: router.extension.invoke
│   │   │   │   │ Attributes: extension.id, lookup.status, lookup.source
│   │   │   │
│   │   │   └── Span: extension.normalize_text (span_id: e7gh36dh4i8h8b7a)
│   │   │       │ Kind: SERVER (extension service)
│   │   │       │ Parent: router.extension.invoke (via NATS traceparent)
│   │   │       │ Attributes: tenant.id, run.id, flow.id, extension.id, extension.type
│   │   │
│   │   ├── Span: router.extension.invoke (span_id: f8hi47ei5j9i9c8b)
│   │   │   │ Kind: CLIENT
│   │   │   │ Parent: router.decide
│   │   │   │ Attributes: tenant.id, run.id, flow.id, extension.id="pii_guard", extension.type="validator", extension.status="success", policy.id
│   │   │   │
│   │   │   └── Span: extension.pii_guard (span_id: g9ij58fj6k0j0d9c)
│   │   │       │ Kind: SERVER (extension service)
│   │   │       │ Parent: router.extension.invoke (via NATS traceparent)
│   │   │       │ Attributes: tenant.id, run.id, flow.id, extension.id, extension.type
│   │   │
│   │   ├── Span: router.provider.select (span_id: h0jk69gk7l1k1e0d)
│   │   │   │ Kind: INTERNAL
│   │   │   │ Parent: router.decide
│   │   │   │ Attributes: tenant.id, run.id, flow.id, provider.selected, decision.reason
│   │   │
│   │   └── Span: router.extension.invoke (span_id: i1kl70hl8m2l2f1e)
│   │       │ Kind: CLIENT
│   │       │ Parent: router.decide
│   │       │ Attributes: tenant.id, run.id, flow.id, extension.id="mask_pii", extension.type="post", extension.status="success", policy.id
│   │       │
│   │       └── Span: extension.mask_pii (span_id: j2lm81im9n3m3g2f)
│   │           │ Kind: SERVER (extension service)
│   │           │ Parent: router.extension.invoke (via NATS traceparent)
│   │           │ Attributes: tenant.id, run.id, flow.id, extension.id, extension.type
│   │
│   └── Span: router.publish.assignment (span_id: k3mn92jn0o4n4h3g)
│       │ Kind: PRODUCER
│       │ Parent: router.decide
│       │ Attributes: tenant.id, run.id, flow.id, subject, assignment_id
```

---

## Detailed Span Examples

### 1. Root Span: `gateway.request`

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "00f067aa0ba902b7",
  "name": "gateway.request",
  "kind": "SERVER",
  "parent_span_id": null,
  "start_time": "2025-01-27T12:00:00.000000Z",
  "end_time": "2025-01-27T12:00:00.250000Z",
  "attributes": {
    "service.name": "gateway",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "http.method": "POST",
    "http.path": "/api/v1/messages",
    "http.status_code": 200,
    "http.request.size": 1024,
    "http.response.size": 512
  },
  "status": {
    "code": "OK"
  }
}
```

### 2. Router Decide Span: `router.decide`

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "a3ce929d0e0e4736",
  "name": "router.decide",
  "kind": "INTERNAL",
  "parent_span_id": "00f067aa0ba902b7",
  "start_time": "2025-01-27T12:00:00.010000Z",
  "end_time": "2025-01-27T12:00:00.240000Z",
  "attributes": {
    "service.name": "router",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "subject": "beamline.router.v1.decide",
    "route": "route_default",
    "duration_ms": 230
  },
  "status": {
    "code": "OK"
  }
}
```

### 3. Extension Invoke Span: `router.extension.invoke` (Pre-processor)

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "c5ef14bf2g6f6958",
  "name": "router.extension.invoke",
  "kind": "CLIENT",
  "parent_span_id": "a3ce929d0e0e4736",
  "start_time": "2025-01-27T12:00:00.030000Z",
  "end_time": "2025-01-27T12:00:00.080000Z",
  "attributes": {
    "service.name": "router",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "extension.id": "normalize_text",
    "extension.type": "pre",
    "extension.subject": "beamline.ext.pre.normalize_text.v1",
    "extension.status": "success",
    "extension.retries_used": 0,
    "extension.latency_ms": 50.0,
    "policy.id": "default"
  },
  "status": {
    "code": "OK"
  }
}
```

### 4. Extension Registry Lookup Span: `router.extension.registry.lookup`

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "d6fg25cg3h7g7a69",
  "name": "router.extension.registry.lookup",
  "kind": "INTERNAL",
  "parent_span_id": "c5ef14bf2g6f6958",
  "start_time": "2025-01-27T12:00:00.031000Z",
  "end_time": "2025-01-27T12:00:00.032000Z",
  "attributes": {
    "service.name": "router",
    "extension.id": "normalize_text",
    "lookup.status": "success",
    "lookup.source": "cache",
    "lookup.duration_ms": 1.0
  },
  "status": {
    "code": "OK"
  }
}
```

### 5. Extension Service Span: `extension.normalize_text` (Extension Service)

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "e7gh36dh4i8h8b7a",
  "name": "extension.normalize_text",
  "kind": "SERVER",
  "parent_span_id": "c5ef14bf2g6f6958",
  "start_time": "2025-01-27T12:00:00.035000Z",
  "end_time": "2025-01-27T12:00:00.075000Z",
  "attributes": {
    "service.name": "extension",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "extension.id": "normalize_text",
    "extension.type": "pre",
    "extension.subject": "beamline.ext.pre.normalize_text.v1",
    "extension.processing.duration_ms": 40.0
  },
  "status": {
    "code": "OK"
  }
}
```

### 6. Extension Invoke Span: `router.extension.invoke` (Validator)

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "f8hi47ei5j9i9c8b",
  "name": "router.extension.invoke",
  "kind": "CLIENT",
  "parent_span_id": "a3ce929d0e0e4736",
  "start_time": "2025-01-27T12:00:00.090000Z",
  "end_time": "2025-01-27T12:00:00.140000Z",
  "attributes": {
    "service.name": "router",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "extension.id": "pii_guard",
    "extension.type": "validator",
    "extension.subject": "beamline.ext.validate.pii_guard.v1",
    "extension.status": "success",
    "extension.retries_used": 0,
    "extension.latency_ms": 50.0,
    "policy.id": "default"
  },
  "status": {
    "code": "OK"
  }
}
```

### 7. Extension Invoke Span: `router.extension.invoke` (Post-processor)

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "i1kl70hl8m2l2f1e",
  "name": "router.extension.invoke",
  "kind": "CLIENT",
  "parent_span_id": "a3ce929d0e0e4736",
  "start_time": "2025-01-27T12:00:00.200000Z",
  "end_time": "2025-01-27T12:00:00.230000Z",
  "attributes": {
    "service.name": "router",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "extension.id": "mask_pii",
    "extension.type": "post",
    "extension.subject": "beamline.ext.post.mask_pii.v1",
    "extension.status": "success",
    "extension.retries_used": 0,
    "extension.latency_ms": 30.0,
    "policy.id": "default"
  },
  "status": {
    "code": "OK"
  }
}
```

---

## Trace Context Propagation

### NATS Message Headers

**Router → Extension Service** (NATS request):

```json
{
  "headers": {
    "traceparent": "00-4bf92f3577b34da6a3ce929d0e0e4736-c5ef14bf2g6f6958-01",
    "tracestate": "tenant=tenant_123,run=run_abc123,flow=flow_xyz789",
    "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "span_id": "c5ef14bf2g6f6958"
  },
  "payload": {
    "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "tenant_id": "tenant_123",
    "payload": {
      "message": "Hello, world!"
    },
    "metadata": {
      "policy_id": "default"
    }
  }
}
```

**Extension Service → Router** (NATS response):

```json
{
  "headers": {
    "traceparent": "00-4bf92f3577b34da6a3ce929d0e0e4736-e7gh36dh4i8h8b7a-01"
  },
  "payload": {
    "payload": {
      "message": "HELLO, WORLD!"
    },
    "metadata": {
      "normalized": "true"
    }
  }
}
```

---

## CP1 Correlation Fields Mapping

### Span Attributes

| CP1 Field | OTEL Attribute | Source | Required |
|-----------|----------------|--------|----------|
| `tenant_id` | `tenant.id` | Request context | ✅ Yes |
| `run_id` | `run.id` | Request context | ✅ Yes |
| `flow_id` | `flow.id` | Request context | Optional |
| `trace_id` | `trace.id` | W3C Trace Context | ✅ Yes |
| `policy_id` | `policy.id` | Policy context | Optional |

### Extension-Specific Attributes

| Attribute | Type | Source | Required |
|-----------|------|--------|----------|
| `extension.id` | string | Extension registry | ✅ Yes |
| `extension.type` | string | Extension registry | ✅ Yes |
| `extension.subject` | string | Extension registry | ✅ Yes |
| `extension.status` | string | Invocation result | ✅ Yes |
| `extension.retries_used` | int64 | Invocation result | Optional |
| `extension.latency_ms` | float64 | Invocation result | Optional |

---

## Error Scenarios

### Extension Timeout

**Span**: `router.extension.invoke` (timeout)

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "c5ef14bf2g6f6958",
  "name": "router.extension.invoke",
  "kind": "CLIENT",
  "parent_span_id": "a3ce929d0e0e4736",
  "start_time": "2025-01-27T12:00:00.030000Z",
  "end_time": "2025-01-27T12:00:00.130000Z",
  "attributes": {
    "service.name": "router",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "extension.id": "normalize_text",
    "extension.type": "pre",
    "extension.subject": "beamline.ext.pre.normalize_text.v1",
    "extension.status": "timeout",
    "extension.retries_used": 2,
    "extension.latency_ms": 100.0,
    "extension.timeout_ms": 50,
    "policy.id": "default"
  },
  "status": {
    "code": "ERROR",
    "message": "Extension invocation timeout after 2 retries"
  },
  "events": [
    {
      "name": "extension.retry",
      "time": "2025-01-27T12:00:00.080000Z",
      "attributes": {
        "retry.attempt": 1,
        "retry.reason": "timeout"
      }
    },
    {
      "name": "extension.retry",
      "time": "2025-01-27T12:00:00.105000Z",
      "attributes": {
        "retry.attempt": 2,
        "retry.reason": "timeout"
      }
    }
  ]
}
```

### Extension Circuit Breaker Open

**Span**: `router.extension.invoke` (circuit breaker)

```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "c5ef14bf2g6f6958",
  "name": "router.extension.invoke",
  "kind": "CLIENT",
  "parent_span_id": "a3ce929d0e0e4736",
  "start_time": "2025-01-27T12:00:00.030000Z",
  "end_time": "2025-01-27T12:00:00.031000Z",
  "attributes": {
    "service.name": "router",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "extension.id": "normalize_text",
    "extension.type": "pre",
    "extension.subject": "beamline.ext.pre.normalize_text.v1",
    "extension.status": "circuit_open",
    "extension.circuit_breaker.state": "open",
    "extension.circuit_breaker.opened_at_ms": 1234567890,
    "policy.id": "default"
  },
  "status": {
    "code": "ERROR",
    "message": "Extension circuit breaker is open"
  }
}
```

---

## Implementation Notes

### Span Creation in Router

**Location**: `apps/otp/router/src/router_extension_invoker.erl`

**Code Pattern**:
```erlang
%% Create extension invoke span
SpanCtx = otel_tracer:start_span(
    Tracer,
    <<"router.extension.invoke">>,
    #{
        kind => client,
        attributes => #{
            <<"service.name">> => <<"router">>,
            <<"tenant.id">> => TenantId,
            <<"run.id">> => RunId,
            <<"flow.id">> => FlowId,
            <<"extension.id">> => ExtensionId,
            <<"extension.type">> => ExtensionType,
            <<"extension.subject">> => ExtensionSubject,
            <<"policy.id">> => PolicyId
        }
    },
    ParentSpanCtx
),

%% Inject trace context into NATS message
TraceParent = otel_trace:to_traceparent(SpanCtx),
NatsHeaders = #{
    <<"traceparent">> => TraceParent
},

%% Invoke extension via NATS
Result = router_nats:request(Subject, PayloadJson, TimeoutMs, NatsHeaders),

%% Update span with result
case Result of
    {ok, Response} ->
        otel_span:set_status(SpanCtx, ok),
        otel_span:set_attribute(SpanCtx, <<"extension.status">>, <<"success">>);
    {error, timeout} ->
        otel_span:set_status(SpanCtx, {error, <<"timeout">>}),
        otel_span:set_attribute(SpanCtx, <<"extension.status">>, <<"timeout">>);
    {error, Reason} ->
        otel_span:set_status(SpanCtx, {error, Reason}),
        otel_span:set_attribute(SpanCtx, <<"extension.status">>, <<"error">>)
end,

%% End span
otel_span:end_span(SpanCtx).
```

### Context Propagation via NATS

**Router Side** (inject):
```erlang
%% Extract trace context from parent span
ParentSpanCtx = otel_tracer:current_span_ctx(Tracer),
TraceParent = otel_trace:to_traceparent(ParentSpanCtx),

%% Build NATS message with trace context
NatsHeaders = #{
    <<"traceparent">> => TraceParent,
    <<"tracestate">> => build_tracestate(TenantId, RunId, FlowId)
},

%% Send NATS request
router_nats:request(Subject, PayloadJson, TimeoutMs, NatsHeaders).
```

**Extension Service Side** (extract):
```erlang
%% Extract trace context from NATS message headers
TraceParent = maps:get(<<"traceparent">>, Headers, undefined),
ParentSpanCtx = case TraceParent of
    undefined -> undefined;
    _ -> otel_trace:from_traceparent(TraceParent)
end,

%% Create child span
SpanCtx = otel_tracer:start_span(
    Tracer,
    <<"extension.", ExtensionId/binary>>,
    #{kind => server},
    ParentSpanCtx
),

%% Process extension request
Result = process_extension_request(Payload),

%% End span
otel_span:end_span(SpanCtx).
```

---

## Summary

### Key Points

1. **Span Hierarchy**: Extension invocations create `router.extension.invoke` spans as children of `router.decide`
2. **Context Propagation**: Trace context propagated via NATS `traceparent` header (W3C Trace Context format)
3. **CP1 Fields**: All CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `trace_id`, `policy_id`) included as span attributes
4. **Extension Attributes**: Extension-specific attributes (`extension.id`, `extension.type`, `extension.status`) included in all extension spans
5. **Error Handling**: Error spans include status code and error messages

### Trace Completeness

✅ **Complete trace coverage**:
- Gateway request (root span)
- Router decide (child of gateway)
- Extension invocations (children of router.decide)
- Extension registry lookups (children of extension.invoke)
- Extension service processing (children of extension.invoke via NATS)
- Provider selection (child of router.decide)

✅ **All CP1 fields present**:
- `tenant_id` → `tenant.id` attribute
- `run_id` → `run.id` attribute
- `flow_id` → `flow.id` attribute
- `trace_id` → `trace.id` attribute
- `policy_id` → `policy.id` attribute

---

## References

- `docs/archive/dev/OBSERVABILITY_TRACING_SPEC_CP2.md` - CP2 tracing specification
- `docs/archive/dev/EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md` - Extensions implementation
- `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` - CP1 correlation fields
- `apps/otp/router/src/router_extension_invoker.erl` - Extension invoker implementation

