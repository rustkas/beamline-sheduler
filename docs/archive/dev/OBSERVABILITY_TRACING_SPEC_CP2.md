# OpenTelemetry Tracing Specification (CP2)

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: 📋 **SPECIFICATION DOCUMENT** (CP2)  
**Control Point**: CP2-LC  
**WORKER**: `wrk-obs1` (Observability CP2)  
**Reference**: `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md`

---

## Purpose

This document defines the **OpenTelemetry (OTEL) distributed tracing specification** for CP2, including:

- **Span hierarchy** (root and child spans per component)
- **Attribute mapping** (CP1 fields → OTEL attributes)
- **Context propagation** (NATS, HTTP, gRPC)
- **Span naming conventions**
- **Error span handling**

**Key Principle**: CP2 tracing builds upon CP1 correlation fields (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`) and extends them with OpenTelemetry distributed tracing capabilities.

---

## CP1 Foundation

### CP1 Correlation Fields

**Reference**: `docs/OBSERVABILITY_CP1_INVARIANTS.md`

CP1 provides the following correlation fields that are used as OTEL span attributes:

| CP1 Field | Required When | Components |
|-----------|---------------|------------|
| `tenant_id` | Tenant context available | All components |
| `run_id` | Run context available | Router, Worker, Gateway |
| `flow_id` | Flow context available | Router, Worker |
| `step_id` | Step context available | Worker, Router |
| `trace_id` | Trace context available | All components (W3C Trace Context format) |
| `error_code` | ERROR level | All components (optional) |
| `latency_ms` | Performance tracking | All components (optional) |

**CP1 `trace_id` Format**: W3C Trace Context format
- Format: `{version}-{trace_id}-{span_id}-{flags}`
- Example: `00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01`
- Full format: `traceparent: 00-{32_hex_chars}-{16_hex_chars}-{2_hex_chars}`

---

## Span Hierarchy

### Complete Trace Structure

**CP2 Trace Structure** (end-to-end request flow):

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
│   │   ├── Span: router.policy.evaluate (span_id: c5ef14bf2g6f6958)
│   │   │   │ Kind: INTERNAL
│   │   │   │ Parent: router.decide
│   │   │   │ Attributes: tenant.id, run.id, flow.id, policy.id
│   │   │
│   │   ├── Span: router.extension.invoke (span_id: d6fg25cg3h7g7a69)
│   │   │   │ Kind: CLIENT
│   │   │   │ Parent: router.decide
│   │   │   │ Attributes: tenant.id, run.id, flow.id, extension.id, extension.type, extension.status, policy.id
│   │   │   │
│   │   │   ├── Span: router.extension.registry.lookup (span_id: e7gh36dh4i8h8b7a)
│   │   │   │   │ Kind: INTERNAL
│   │   │   │   │ Parent: router.extension.invoke
│   │   │   │   │ Attributes: extension.id, lookup.status, lookup.source
│   │   │   │
│   │   │   └── Span: extension.normalize_text (span_id: f8hi47ei5j9i9c8b)
│   │   │       │ Kind: SERVER (extension service)
│   │   │       │ Parent: router.extension.invoke (via NATS traceparent)
│   │   │       │ Attributes: tenant.id, run.id, flow.id, extension.id, extension.type
│   │   │
│   │   └── Span: router.provider.select (span_id: g9ij58fj6k0j0d9c)
│   │       │ Kind: INTERNAL
│   │       │ Parent: router.decide
│   │       │ Attributes: tenant.id, run.id, flow.id, provider.selected, decision.reason
│   │
│   └── Span: router.publish.assignment (span_id: e7gh36dh4i8h8b7a)
│       │ Kind: PRODUCER
│       │ Parent: router.decide
│       │ Attributes: tenant.id, run.id, flow.id, subject, assignment_id
│       │
│       └── Span: worker.execution (span_id: f8hi47ei5j9i9c8b)
│           │ Kind: CONSUMER
│           │ Parent: router.publish.assignment
│           │ Attributes: tenant.id, run.id, flow.id, assignment_id
│           │
│           ├── Span: worker.step.execution (span_id: g9ij58fj6k0j0d9c)
│           │   │ Kind: INTERNAL
│           │   │ Parent: worker.execution
│           │   │ Attributes: tenant.id, run.id, flow.id, step.id, step.type, execution.status
│           │   │
│           │   ├── Span: worker.llm.call (span_id: h0jk69gk7l1k1e0d)
│           │   │   │ Kind: CLIENT
│           │   │   │ Parent: worker.step.execution
│           │   │   │ Attributes: tenant.id, run.id, flow.id, step.id, provider.id, model.id
│           │   │
│           │   └── Span: worker.response.process (span_id: i1kl70hl8m2l2f1e)
│           │       │ Kind: INTERNAL
│           │       │ Parent: worker.step.execution
│           │       │ Attributes: tenant.id, run.id, flow.id, step.id, response.status
│           │
│           ├── Span: worker.step.execution (span_id: j2lm81im9n3m3g2f)
│           │   │ Kind: INTERNAL
│           │   │ Parent: worker.execution
│           │   │ Attributes: tenant.id, run.id, flow.id, step.id, step.type, execution.status
│           │
│           └── Span: worker.publish.result (span_id: k3mn92jn0o4n4h3g)
│               │ Kind: PRODUCER
│               │ Parent: worker.execution
│               │ Attributes: tenant.id, run.id, flow.id, step.id, subject, result.status
│
└── Span: router.process.result (span_id: l4no03ko1p5o5i4h)
    │ Kind: CONSUMER
    │ Parent: worker.publish.result (via NATS)
    │ Attributes: tenant.id, run.id, flow.id, subject, assignment_id, result.status
```

---

## Component-Specific Spans

### Gateway Spans

#### 1. Root Span: `gateway.request`

**Span Name**: `gateway.request`  
**Span Kind**: `SERVER`  
**Parent**: None (root span)  
**When Created**: On incoming HTTP request to Gateway

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"gateway"` |
| `tenant.id` | string | ✅ | HTTP header/body | Tenant identifier |
| `run.id` | string | ✅ | HTTP header/body | Run identifier |
| `trace.id` | string | ✅ | Generated/extracted | W3C Trace Context trace ID |
| `http.method` | string | ✅ | HTTP request | HTTP method (GET, POST, PUT, DELETE) |
| `http.path` | string | ✅ | HTTP request | HTTP path (e.g., `/api/v1/messages`) |
| `http.status_code` | int | ✅ | HTTP response | HTTP status code (200, 400, 500, etc.) |
| `http.request.size` | int64 | Optional | HTTP request | Request body size in bytes |
| `http.response.size` | int64 | Optional | HTTP response | Response body size in bytes |
| `duration_ms` | int64 | Optional | Calculated | Request duration in milliseconds |

**Example**:
```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "00f067aa0ba902b7",
  "name": "gateway.request",
  "kind": "SERVER",
  "start_time": "2025-01-27T12:00:00.123456Z",
  "end_time": "2025-01-27T12:00:00.243456Z",
  "attributes": {
    "service.name": "gateway",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "http.method": "POST",
    "http.path": "/api/v1/messages",
    "http.status_code": 200,
    "http.request.size": 1024,
    "http.response.size": 2048,
    "duration_ms": 120
  },
  "status": {
    "code": "OK"
  }
}
```

#### 2. Child Span: `gateway.rate.limit` (Optional)

**Span Name**: `gateway.rate.limit`  
**Span Kind**: `INTERNAL`  
**Parent**: `gateway.request`  
**When Created**: When rate limiting check is performed

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"gateway"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `rate.limit.check` | string | ✅ | Calculated | Rate limit check result (`allowed`, `denied`) |
| `rate.limit.remaining` | int64 | Optional | Rate limiter | Remaining requests in window |

---

### Router Spans

#### 1. Child Span: `router.decide`

**Span Name**: `router.decide`  
**Span Kind**: `INTERNAL`  
**Parent**: `gateway.request` (via HTTP/gRPC)  
**When Created**: On incoming DecideRequest (NATS or gRPC)

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `tenant.id` | string | ✅ | DecideRequest | Tenant identifier |
| `run.id` | string | ✅ | DecideRequest | Run identifier |
| `flow.id` | string | Optional | DecideRequest | Flow identifier |
| `trace.id` | string | ✅ | Extracted | W3C Trace Context trace ID |
| `subject` | string | ✅ | NATS message | NATS subject (e.g., `router.decide.v1`) |
| `route` | string | Optional | Calculated | Route identifier |
| `duration_ms` | int64 | Optional | Calculated | Decision duration in milliseconds |

**Example**:
```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "a3ce929d0e0e4736",
  "name": "router.decide",
  "kind": "INTERNAL",
  "parent_span_id": "00f067aa0ba902b7",
  "start_time": "2025-01-27T12:00:00.125000Z",
  "end_time": "2025-01-27T12:00:00.170000Z",
  "attributes": {
    "service.name": "router",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "trace.id": "4bf92f3577b34da6a3ce929d0e0e4736",
    "subject": "router.decide.v1",
    "route": "route_default",
    "duration_ms": 45
  },
  "status": {
    "code": "OK"
  }
}
```

#### 2. Child Span: `router.policy.load`

**Span Name**: `router.policy.load`  
**Span Kind**: `INTERNAL`  
**Parent**: `router.decide`  
**When Created**: When policy is loaded from storage

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `policy.id` | string | ✅ | Policy lookup | Policy identifier (e.g., `"default"`) |
| `policy.load.status` | string | ✅ | Result | Policy load status (`success`, `not_found`, `error`) |

#### 3. Child Span: `router.policy.evaluate`

**Span Name**: `router.policy.evaluate`  
**Span Kind**: `INTERNAL`  
**Parent**: `router.decide`  
**When Created**: When policy evaluation is performed

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `flow.id` | string | Optional | From parent | Flow identifier |
| `policy.id` | string | ✅ | Policy | Policy identifier |
| `policy.evaluation.result` | string | ✅ | Result | Evaluation result (`match`, `no_match`, `error`) |

#### 4. Child Span: `router.extension.invoke`

**Span Name**: `router.extension.invoke`  
**Span Kind**: `CLIENT`  
**Parent**: `router.decide`  
**When Created**: When extension is invoked via NATS

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `flow.id` | string | Optional | From parent | Flow identifier |
| `trace.id` | string | ✅ | From parent | W3C Trace Context trace ID |
| `extension.id` | string | ✅ | Extension registry | Extension identifier (e.g., `"normalize_text"`) |
| `extension.type` | string | ✅ | Extension registry | Extension type (`pre`, `validator`, `post`, `provider`) |
| `extension.subject` | string | ✅ | Extension registry | NATS subject (e.g., `"beamline.ext.pre.normalize_text.v1"`) |
| `extension.status` | string | ✅ | Invocation result | Invocation status (`success`, `error`, `timeout`, `max_retries_exceeded`, `circuit_open`) |
| `extension.retries_used` | int64 | Optional | Invocation result | Number of retries used |
| `extension.latency_ms` | float64 | Optional | Invocation result | Invocation latency in milliseconds |
| `policy.id` | string | Optional | Policy context | Policy identifier |

**Example**:
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

#### 4.1. Child Span: `router.extension.registry.lookup`

**Span Name**: `router.extension.registry.lookup`  
**Span Kind**: `INTERNAL`  
**Parent**: `router.extension.invoke`  
**When Created**: When extension is looked up in registry

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `extension.id` | string | ✅ | Lookup request | Extension identifier |
| `lookup.status` | string | ✅ | Lookup result | Lookup result (`success`, `not_found`, `error`) |
| `lookup.source` | string | ✅ | Lookup result | Lookup source (`cache`, `database`, `fixtures`) |
| `lookup.duration_ms` | float64 | Optional | Calculated | Lookup duration in milliseconds |

**Example**:
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

#### 5. Child Span: `router.provider.select`

**Span Name**: `router.provider.select`  
**Span Kind**: `INTERNAL`  
**Parent**: `router.decide`  
**When Created**: When provider selection is performed

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `flow.id` | string | Optional | From parent | Flow identifier |
| `provider.selected` | string | ✅ | Selection result | Selected provider (e.g., `"openai:gpt-4o"`) |
| `decision.reason` | string | ✅ | Selection logic | Decision reason (`weighted_selection`, `fallback`, `best_score`) |
| `provider.score` | float64 | Optional | Selection logic | Provider selection score |

**Example**:
```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "d6fg25cg3h7g7a69",
  "name": "router.provider.select",
  "kind": "INTERNAL",
  "parent_span_id": "a3ce929d0e0e4736",
  "attributes": {
    "service.name": "router",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "provider.selected": "openai:gpt-4o",
    "decision.reason": "weighted_selection",
    "provider.score": 0.95
  },
  "status": {
    "code": "OK"
  }
}
```

#### 5. Child Span: `router.publish.assignment`

**Span Name**: `router.publish.assignment`  
**Span Kind**: `PRODUCER`  
**Parent**: `router.decide`  
**When Created**: When assignment is published to NATS

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `flow.id` | string | Optional | From parent | Flow identifier |
| `subject` | string | ✅ | NATS | NATS subject (e.g., `caf.exec.assign.v1`) |
| `assignment.id` | string | ✅ | Assignment | Assignment identifier |
| `publish.result` | string | ✅ | Result | Publication result (`ok`, `error`) |
| `publish.retries` | int64 | Optional | Retry logic | Number of retry attempts |

#### 6. Child Span: `router.process.result`

**Span Name**: `router.process.result`  
**Span Kind**: `CONSUMER`  
**Parent**: `worker.publish.result` (via NATS)  
**When Created**: When result is received from Worker via NATS

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `tenant.id` | string | ✅ | NATS message | Tenant identifier |
| `run.id` | string | ✅ | NATS message | Run identifier |
| `flow.id` | string | Optional | NATS message | Flow identifier |
| `subject` | string | ✅ | NATS message | NATS subject (e.g., `caf.exec.result.v1`) |
| `assignment.id` | string | ✅ | Result message | Assignment identifier |
| `result.status` | string | ✅ | Result | Result status (`success`, `error`, `timeout`) |

#### 7. Child Span: `router.process.ack`

**Span Name**: `router.process.ack`  
**Span Kind**: `CONSUMER`  
**Parent**: `worker.publish.ack` (via NATS)  
**When Created**: When ACK is received from Worker via NATS

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `tenant.id` | string | ✅ | NATS message | Tenant identifier |
| `run.id` | string | ✅ | NATS message | Run identifier |
| `subject` | string | ✅ | NATS message | NATS subject (e.g., `caf.exec.assign.v1.ack`) |
| `assignment.id` | string | ✅ | ACK message | Assignment identifier |
| `ack.status` | string | ✅ | ACK | ACK status (`ok`, `error`) |

#### 8. Child Span: `router.emit.usage`

**Span Name**: `router.emit.usage`  
**Span Kind**: `PRODUCER`  
**Parent**: `router.process.result`  
**When Created**: When usage event is emitted

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"router"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `subject` | string | ✅ | NATS | NATS subject (e.g., `usage.events.v1`) |
| `usage.event.type` | string | ✅ | Event | Usage event type |
| `provider.id` | string | ✅ | Event | Provider identifier |

---

### Worker Spans

#### 1. Child Span: `worker.execution`

**Span Name**: `worker.execution`  
**Span Kind**: `CONSUMER`  
**Parent**: `router.publish.assignment` (via NATS)  
**When Created**: When assignment is received from Router via NATS

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"worker"` |
| `tenant.id` | string | ✅ | Assignment | Tenant identifier |
| `run.id` | string | ✅ | Assignment | Run identifier |
| `flow.id` | string | Optional | Assignment | Flow identifier |
| `assignment.id` | string | ✅ | Assignment | Assignment identifier |
| `execution.status` | string | ✅ | Result | Execution status (`started`, `completed`, `error`) |

**Example**:
```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "f8hi47ei5j9i9c8b",
  "name": "worker.execution",
  "kind": "CONSUMER",
  "parent_span_id": "e7gh36dh4i8h8b7a",
  "attributes": {
    "service.name": "worker",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "assignment.id": "a1234567890-12345",
    "execution.status": "started"
  },
  "status": {
    "code": "OK"
  }
}
```

#### 2. Child Span: `worker.step.execution`

**Span Name**: `worker.step.execution`  
**Span Kind**: `INTERNAL`  
**Parent**: `worker.execution`  
**When Created**: When step execution starts

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"worker"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `flow.id` | string | Optional | From parent | Flow identifier |
| `step.id` | string | ✅ | Step | Step identifier (e.g., `"step_001"`) |
| `step.type` | string | ✅ | Step | Step type (`llm_call`, `http_request`, `condition`, etc.) |
| `execution.status` | string | ✅ | Result | Execution status (`success`, `error`, `timeout`) |
| `duration_ms` | int64 | Optional | Calculated | Step execution duration in milliseconds |

**Example**:
```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "g9ij58fj6k0j0d9c",
  "name": "worker.step.execution",
  "kind": "INTERNAL",
  "parent_span_id": "f8hi47ei5j9i9c8b",
  "attributes": {
    "service.name": "worker",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "flow.id": "flow_xyz789",
    "step.id": "step_001",
    "step.type": "llm_call",
    "execution.status": "success",
    "duration_ms": 250
  },
  "status": {
    "code": "OK"
  }
}
```

#### 3. Child Span: `worker.llm.call`

**Span Name**: `worker.llm.call`  
**Span Kind**: `CLIENT`  
**Parent**: `worker.step.execution`  
**When Created**: When LLM API call is made

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"worker"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `flow.id` | string | Optional | From parent | Flow identifier |
| `step.id` | string | ✅ | From parent | Step identifier |
| `provider.id` | string | ✅ | LLM call | Provider identifier (e.g., `"openai"`) |
| `model.id` | string | ✅ | LLM call | Model identifier (e.g., `"gpt-4o"`) |
| `llm.call.status` | string | ✅ | Result | LLM call status (`success`, `error`, `rate_limited`) |
| `llm.tokens.input` | int64 | Optional | Response | Input tokens count |
| `llm.tokens.output` | int64 | Optional | Response | Output tokens count |

#### 4. Child Span: `worker.response.process`

**Span Name**: `worker.response.process`  
**Span Kind**: `INTERNAL`  
**Parent**: `worker.step.execution`  
**When Created**: When response is processed

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"worker"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `flow.id` | string | Optional | From parent | Flow identifier |
| `step.id` | string | ✅ | From parent | Step identifier |
| `response.status` | string | ✅ | Result | Response processing status (`success`, `error`) |

#### 5. Child Span: `worker.publish.result`

**Span Name**: `worker.publish.result`  
**Span Kind**: `PRODUCER`  
**Parent**: `worker.execution`  
**When Created**: When result is published to NATS

**Attributes**:

| Attribute | Type | Required | Source | Description |
|-----------|------|----------|--------|-------------|
| `service.name` | string | ✅ | Fixed | `"worker"` |
| `tenant.id` | string | ✅ | From parent | Tenant identifier |
| `run.id` | string | ✅ | From parent | Run identifier |
| `flow.id` | string | Optional | From parent | Flow identifier |
| `step.id` | string | Optional | From parent | Step identifier |
| `subject` | string | ✅ | NATS | NATS subject (e.g., `caf.exec.result.v1`) |
| `result.status` | string | ✅ | Result | Result status (`success`, `error`, `timeout`) |

---

## CP1 Fields → OTEL Attributes Mapping

### Standard Attribute Mapping

| CP1 Field | OTEL Attribute | Type | Required | Description |
|-----------|----------------|------|----------|-------------|
| `tenant_id` | `tenant.id` | string | ✅ | Tenant identifier |
| `run_id` | `run.id` | string | ✅ | Run identifier |
| `flow_id` | `flow.id` | string | Optional | Flow identifier |
| `step_id` | `step.id` | string | Optional | Step identifier |
| `trace_id` | `trace.id` | string | ✅ | W3C Trace Context trace ID |
| `component` | `service.name` | string | ✅ | Component name (router, gateway, worker) |
| `error_code` | `error.code` | string | Optional | Error code (for error spans) |
| `latency_ms` | `duration_ms` | int64 | Optional | Duration in milliseconds |

### Component-Specific Attributes

#### Gateway Attributes

| Attribute | Type | Source | Description |
|-----------|------|--------|-------------|
| `http.method` | string | HTTP request | HTTP method |
| `http.path` | string | HTTP request | HTTP path |
| `http.status_code` | int | HTTP response | HTTP status code |
| `http.request.size` | int64 | HTTP request | Request body size |
| `http.response.size` | int64 | HTTP response | Response body size |

#### Router Attributes

| Attribute | Type | Source | Description |
|-----------|------|--------|-------------|
| `subject` | string | NATS message | NATS subject |
| `route` | string | Calculated | Route identifier |
| `policy.id` | string | Policy | Policy identifier |
| `provider.selected` | string | Selection result | Selected provider |
| `decision.reason` | string | Selection logic | Decision reason |
| `assignment.id` | string | Assignment | Assignment identifier |

#### Worker Attributes

| Attribute | Type | Source | Description |
|-----------|------|--------|-------------|
| `step.type` | string | Step | Step type |
| `execution.status` | string | Result | Execution status |
| `provider.id` | string | LLM call | Provider identifier |
| `model.id` | string | LLM call | Model identifier |
| `result.status` | string | Result | Result status |

---

## Context Propagation

### W3C Trace Context Format

**Standard Format**: `traceparent` header

```
traceparent: {version}-{trace_id}-{span_id}-{flags}
```

**Example**:
```
traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01
```

**Format Details**:
- `version`: `00` (W3C Trace Context version)
- `trace_id`: 32 hex characters (128 bits)
- `span_id`: 16 hex characters (64 bits)
- `flags`: 2 hex characters (8 bits, `01` = sampled)

**Optional Format**: `tracestate` header (vendor-specific data)

```
tracestate: tenant=tenant_123,run=run_abc123
```

### HTTP Context Propagation

**Headers**:
- `traceparent`: W3C Trace Context format (required)
- `tracestate`: Vendor-specific data (optional)

**Extraction** (Gateway → Router):
1. Gateway receives HTTP request
2. Extract `traceparent` header (if present)
3. Parse W3C Trace Context format
4. Create parent context for Router spans
5. If `traceparent` missing, generate new trace

**Injection** (Gateway → Router):
1. Gateway creates root span `gateway.request`
2. Extract/inject `traceparent` in HTTP request to Router (if Router called via HTTP)
3. Router extracts `traceparent` and creates child span `router.decide`

**Example HTTP Request**:
```http
POST /api/v1/routes/decide HTTP/1.1
Host: router:9000
Content-Type: application/json
traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01
tracestate: tenant=tenant_123,run=run_abc123

{
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  ...
}
```

### gRPC Context Propagation

**Metadata Keys**:
- `traceparent`: W3C Trace Context format (required)
- `tracestate`: Vendor-specific data (optional)

**Extraction** (Gateway → Router via gRPC):
1. Gateway creates root span `gateway.request`
2. Inject `traceparent` in gRPC metadata
3. Router extracts `traceparent` from gRPC metadata
4. Router creates child span `router.decide` with parent context

**Example gRPC Metadata**:
```erlang
Metadata = #{
    <<"traceparent">> => <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>,
    <<"tracestate">> => <<"tenant=tenant_123,run=run_abc123">>
}
```

### NATS Context Propagation

**Headers** (NATS/1.0 format):
- `traceparent`: W3C Trace Context format (required)
- `tracestate`: Vendor-specific data (optional)
- `trace_id`: Custom format (compatibility, same as trace_id from traceparent)
- `span_id`: Custom format (compatibility, same as span_id from traceparent)
- `X-Trace-Id`: Compatibility header (same as trace_id)
- `X-Span-Id`: Compatibility header (same as span_id)

**Extraction** (Router → Worker via NATS):
1. Router creates span `router.publish.assignment`
2. Inject `traceparent` in NATS message headers
3. Worker extracts `traceparent` from NATS message headers
4. Worker creates child span `worker.execution` with parent context

**Injection** (Worker → Router via NATS):
1. Worker creates span `worker.publish.result`
2. Inject `traceparent` in NATS message headers
3. Router extracts `traceparent` from NATS message headers
4. Router creates child span `router.process.result` with parent context

**Example NATS Message Headers**:
```
NATS/1.0\r\n
traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01\r\n
tracestate: tenant=tenant_123,run=run_abc123\r\n
trace_id: 4bf92f3577b34da6a3ce929d0e0e4736\r\n
span_id: 00f067aa0ba902b7\r\n
X-Trace-Id: 4bf92f3577b34da6a3ce929d0e0e4736\r\n
X-Span-Id: 00f067aa0ba902b7\r\n
tenant_id: tenant_123\r\n
version: 1\r\n
\r\n
```

**NATS Header Priority**:
1. **Primary**: `traceparent` (W3C Trace Context format)
2. **Fallback**: `trace_id`, `span_id` (custom format)
3. **Compatibility**: `X-Trace-Id`, `X-Span-Id` (legacy headers)

---

## Error Spans

### Error Span Structure

**CP1 ERROR logs → OTEL Error Spans**:

When a CP1 log has `level="ERROR"`, the corresponding span must:

1. **Set span status to ERROR**:
   ```json
   {
     "status": {
       "code": "ERROR",
       "message": "Error message from CP1 log"
     }
   }
   ```

2. **Include error attributes**:
   - `error.code`: Error code from CP1 log
   - `error.message`: Error message from CP1 log
   - `error.type`: Error type (optional)

**Example Error Span**:
```json
{
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "span_id": "error_span_id",
  "name": "router.policy.load",
  "kind": "INTERNAL",
  "parent_span_id": "a3ce929d0e0e4736",
  "start_time": "2025-01-27T12:00:00.125000Z",
  "end_time": "2025-01-27T12:00:00.126000Z",
  "attributes": {
    "service.name": "router",
    "tenant.id": "tenant_123",
    "run.id": "run_abc123",
    "policy.id": "default",
    "error.code": "POLICY_LOAD_ERROR",
    "error.message": "Failed to load policy: policy_not_found",
    "error.type": "NotFoundError"
  },
  "status": {
    "code": "ERROR",
    "message": "Failed to load policy: policy_not_found"
  },
  "events": [
    {
      "name": "exception",
      "time": "2025-01-27T12:00:00.125500Z",
      "attributes": {
        "exception.type": "NotFoundError",
        "exception.message": "Policy not found: default"
      }
    }
  ]
}
```

### Error Attribute Mapping

| CP1 Field | OTEL Attribute | Type | Required | Description |
|-----------|----------------|------|----------|-------------|
| `level="ERROR"` | `status.code="ERROR"` | string | ✅ | Span status |
| `error_code` | `error.code` | string | ✅ | Error code |
| `message` | `error.message` | string | ✅ | Error message |
| `context.error` | `error.type` | string | Optional | Error type |

---

## Span Naming Conventions

### Naming Format

**Format**: `{component}.{operation}`

**Examples**:
- `gateway.request`
- `router.decide`
- `router.policy.load`
- `router.provider.select`
- `worker.execution`
- `worker.step.execution`
- `worker.llm.call`

### Component Prefixes

| Component | Prefix | Examples |
|-----------|--------|----------|
| Gateway | `gateway.` | `gateway.request`, `gateway.rate.limit` |
| Router | `router.` | `router.decide`, `router.policy.load`, `router.provider.select` |
| Worker | `worker.` | `worker.execution`, `worker.step.execution`, `worker.llm.call` |

### Operation Names

- Use lowercase with dots as separators
- Use verb-noun format (e.g., `policy.load`, `provider.select`)
- Use descriptive names (e.g., `step.execution`, not `step`)

---

## Implementation Guidelines

### Span Creation Rules

1. **Root Span**: Created at entry point (Gateway HTTP request)
2. **Child Spans**: Created for each significant operation
3. **Span Duration**: Measure from start to end of operation
4. **Span Status**: Set to OK on success, ERROR on failure
5. **Span Attributes**: Include all relevant CP1 fields and component-specific attributes

### Context Propagation Rules

1. **Extract First**: Always extract trace context from incoming message/request
2. **Create Parent Context**: Use extracted trace context as parent for child spans
3. **Inject Before Send**: Inject trace context into outgoing message/request headers
4. **Fallback**: If trace context missing, create new trace (root span)

### Attribute Rules

1. **Required Attributes**: Always include `service.name`, `tenant.id`, `run.id`, `trace.id`
2. **Optional Attributes**: Include `flow.id`, `step.id` when available
3. **Component-Specific**: Include component-specific attributes (e.g., `http.method`, `subject`, `step.type`)
4. **Error Attributes**: Include `error.code`, `error.message` for error spans

---

## Validation

### Trace Validation Checklist

- ✅ All spans include required attributes (`service.name`, `tenant.id`, `run.id`, `trace.id`)
- ✅ Span hierarchy is correct (parent-child relationships)
- ✅ Trace context is propagated correctly (NATS/HTTP/gRPC)
- ✅ Error spans have `status.code="ERROR"` and error attributes
- ✅ Span names follow naming conventions
- ✅ W3C Trace Context format is used for `traceparent` header

### Validation Script

**Script**: `scripts/observability/validate_traces.sh`

**Checks**:
1. Trace export to OTEL collector
2. Span hierarchy validation
3. Trace context propagation validation
4. Attribute presence validation
5. Error span validation

---

## References

- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - CP2 extension plan
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `apps/otp/router/src/router_tracing.erl` - Router tracing implementation
- [W3C Trace Context Specification](https://www.w3.org/TR/trace-context/)
- [OpenTelemetry Specification](https://opentelemetry.io/docs/specs/otel/)

---

**Document Status**: 📋 **SPECIFICATION** (CP2)  
**Last Updated**: 2025-01-27  
**Next Steps**: CP2 implementation by `wrk-obs1`

