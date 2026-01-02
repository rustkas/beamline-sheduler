# Protobuf to NATS Subject Mapping

This document describes the mapping between protobuf messages/services and NATS subjects.

## Structure

- **Protobuf**: Message and service definitions in `proto/beamline/*/v1/*.proto`
- **NATS Subjects**: Format `beamline.{component}.{version}.{action}`
- **Mapping**: Correspondence between proto messages and NATS JSON payload

## Core Message Fields

**CRITICAL**: All messages must comply with the **Core Message Fields Specification** (`docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md`).

**Required fields** (see specification for complete details):
- `tenant_id` (required) - Tenant identifier
- `version` / `schema_version` (required) - Schema version
- `trace_id` (optional in CP1, required in CP2+) - Distributed tracing ID
- `idempotency_key` (optional in CP1, required in CP2+) - Idempotency key
- `run_id`, `flow_id`, `step_id` (CP2+ only) - Multi-step workflow identifiers

**See**: `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` for complete field specifications, validation rules, format requirements, and test requirements.

## Router Service (beamline.flow.v1)

### Proto Service: `Router.Decide`

**Package**: `beamline.flow.v1`  
**Service**: `Router`  
**RPC**: `Decide(RouteRequest) returns (RouteDecision)`

### Proto Contract (Wire Protocol / ABI)

**Source of Truth**: Generated code (`apps/otp/router/src/flow_pb.erl`, `apps/otp/router/include/flow_pb.hrl`)

**Proto Message Definitions**:

```protobuf
message Message {
  string message_id = 1;        // optional
  string tenant_id = 2;         // optional
  string trace_id = 3;          // optional
  string message_type = 4;      // optional
  bytes payload = 5;             // optional
  map<string, string> metadata = 6;  // repeated
  int64 timestamp_ms = 7;       // optional
}

message RouteRequest {
  Message message = 1;           // optional
  string policy_id = 2;          // optional
  map<string, string> context = 3; // repeated
}

message RouteDecision {
  string provider_id = 1;        // optional
  string reason = 2;             // optional
  int32 priority = 3;            // optional
  int64 expected_latency_ms = 4; // optional
  double expected_cost = 5;      // optional
  map<string, string> metadata = 6; // repeated
}
```

**Note**: All fields are optional in Proto (protobuf v3 semantics). Router enforces required fields at runtime (see Field Optionality section).

### NATS Subject: `beamline.router.v1.decide`

**Type**: Request-Reply (JetStream)  
**Request**: `RouteRequest` (protobuf) → JSON payload  
**Response**: `RouteDecision` (protobuf) → JSON payload

**Consumer**: `router_decide_consumer.erl` (CP2+)
- **JetStream**: Durable consumer with explicit ack policy
- **Durable Group**: `router-decide-consumer` (configurable)
- **Queue Group**: `router-decide-group` (configurable, for horizontal scaling)
- **Ack Policy**: `explicit` (messages ACKed after successful processing)
- **MaxDeliver**: 3 (configurable, prevents infinite retries)
- **Delivery Count Tracking**: ETS table for MaxDeliver exhaustion detection
- **DLQ**: Invalid messages sent to `beamline.router.v1.decide.dlq` (configurable via `dlq_subject_pattern`)
- **DLQ Payload Format**: See [DLQ Message Format](#dlq-message-format) section below

**REST API Endpoint**: `POST /api/v1/routes/decide` (C-Gateway)  
**See**: `docs/ARCHITECTURE/api-registry.md#post-apiv1routesdecide` for REST API contract and Gateway → NATS transformation.

**Migration Note**: 
- **Deprecated**: `router_nats_subscriber.erl` (regular NATS subscription, no JetStream)
- **Current**: `router_decide_consumer.erl` (JetStream durable consumer)
- Old subscriber will be removed in a future release.

### NATS JSON Format (Logical Payload)

**Source of Truth**: This document (`PROTO_NATS_MAPPING.md`)

**Important**: NATS JSON format includes **Proto fields + NATS-specific fields** added at the adapter layer.

**Field Mapping**:

| NATS JSON Field | Proto Field | Source | Notes |
|----------------|-------------|--------|-------|
| `version` | ❌ Not in Proto | NATS adapter layer | NATS message version, used for protocol negotiation |
| `request_id` | ❌ Not in Proto | NATS adapter layer | NATS correlation ID for request-reply pattern |
| `task` | ❌ Not in Proto | NATS adapter layer | Task wrapper for NATS routing (may map to `message.payload`) |
| `constraints` | ❌ Not in Proto | NATS adapter layer | Routing constraints (may map to `context` or metadata) |
| `push_assignment` | ❌ Not in Proto | NATS adapter layer | Flag to trigger `ExecAssignment` publication |
| `message` | ✅ `RouteRequest.message` | Proto | Direct mapping from Proto |
| `policy_id` | ✅ `RouteRequest.policy_id` | Proto | Direct mapping from Proto |
| `context` | ✅ `RouteRequest.context` | Proto | Direct mapping from Proto |

**Request Format (NATS JSON)**:
```json
{
  "version": "1",              // ⚠️ NATS layer field (not in Proto)
  "request_id": "uuid",        // ⚠️ NATS layer field (not in Proto)
  "task": {...},               // ⚠️ NATS layer field (not in Proto)
  "constraints": {...},        // ⚠️ NATS layer field (not in Proto)
  "push_assignment": false,    // ⚠️ NATS layer field (not in Proto)
  "message": {...},            // ✅ Proto field (RouteRequest.message)
  "policy_id": "default",     // ✅ Proto field (RouteRequest.policy_id)
  "context": {...}             // ✅ Proto field (RouteRequest.context)
}
```

**Legend**:
- ⚠️ **NATS layer field**: Added at NATS adapter layer, not in Proto contract
- ✅ **Proto field**: Direct mapping from Proto message definition

**Response Format (NATS JSON)**:
```json
{
  "ok": true,
  "decision": {
    "provider_id": "string",    // ✅ Proto field (RouteDecision.provider_id)
    "reason": "string",         // ✅ Proto field (RouteDecision.reason)
    "priority": 0,              // ✅ Proto field (RouteDecision.priority)
    "expected_latency_ms": 0,   // ✅ Proto field (RouteDecision.expected_latency_ms)
    "expected_cost": 0.0,       // ✅ Proto field (RouteDecision.expected_cost)
    "metadata": {"key": "value"} // ✅ Proto field (RouteDecision.metadata)
  },
  "context": {
    "request_id": "string",     // ⚠️ NATS layer field (not in Proto)
    "trace_id": "string"        // ⚠️ NATS layer field (not in Proto)
  }
}
```

**Error Response Format (NATS JSON)**:
```json
{
  "ok": false,
  "error": {
    "code": "string",
    "message": "string",
    "details": {"key": "value (optional)"}
  },
  "context": {
    "request_id": "string",     // ⚠️ NATS layer field (not in Proto)
    "trace_id": "string (optional)" // ⚠️ NATS layer field (not in Proto)
  }
}
```

**CP2+ Optional Fields** (backward compatible, see `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` for complete specification):
- `message.run_id` - Run identifier for multi-step workflows (CP2+, see Core Fields Spec)
- `message.flow_id` - Flow definition identifier (CP2+, see Core Fields Spec)
- `message.step_id` - Step identifier within a flow (CP2+, see Core Fields Spec)
- `message.idempotency_key` - Message-level idempotency key (CP2+, see Core Fields Spec)
- `message.span_id` - Span identifier for distributed tracing (CP2+)
- `idempotency_key` (top-level) - Request-level idempotency key (CP2+, see Core Fields Spec)

**Note**: NATS JSON format includes additional fields (`version`, `request_id`, `task`, `constraints`, `push_assignment`, `context.request_id`, `context.trace_id`) that are handled at the NATS adapter layer. These fields are **not** part of the Proto contract and should **not** be added to Proto files. See Section 1.5 in `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` for detailed explanation of two-level contract architecture.

**Core Fields Validation**: All core fields must comply with `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` (format, validation rules, field relationships).

**Reply Subject**: `beamline.router.v1.decide.reply`

## Field Optionality vs Runtime Requirements

### Proto Contract (Wire Protocol)

**Protobuf v3 Semantics**: All fields in Proto messages are **optional** by default. This allows backward compatibility when adding new fields.

**Actual Proto Fields** (from generated code `flow_pb.erl`):
- `Message`: All fields (1-7) are `optional`
- `RouteRequest`: All fields (1-3) are `optional`
- `RouteDecision`: All fields (1-6) are `optional`

### Runtime Validation (Router Layer)

**Router enforces required fields at runtime**, even though Proto contract allows optional fields. This provides:
- **Backward compatibility**: Proto contract allows optional fields
- **Runtime safety**: Router validates required fields and returns errors for missing fields

**Required Fields** (enforced at runtime):
- `Message`: `tenant_id`, `message_type`, `payload` (required)
- `RouteRequest`: `message` (required)
- `RouteDecision`: `provider_id`, `reason` (required)

**Validation Behavior**:
- Missing required fields → Router returns `ErrorResponse` with `code: "invalid_request"`
- Optional fields can be omitted → Router uses defaults or ignores
- Proto contract allows optional → No breaking changes when adding new optional fields

**Example - Minimal Valid Request**:
```json
{
  "message": {
    "message_id": "msg_123",
    "tenant_id": "tenant_abc",      // Required at runtime
    "message_type": "chat",          // Required at runtime
    "payload": "Hello"               // Required at runtime
  }
}
```

**Example - Full Request with Optional Fields**:
```json
{
  "message": {
    "message_id": "msg_123",
    "tenant_id": "tenant_abc",
    "trace_id": "trace_xyz",          // Optional
    "message_type": "chat",
    "payload": "Hello",
    "metadata": {"source": "gateway"}, // Optional
    "timestamp_ms": 1704067200000      // Optional
  },
  "policy_id": "default",             // Optional
  "context": {"user_id": "user_123"}   // Optional
}
```

**Error Response for Missing Required Field**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Missing required field: tenant_id",
    "details": {"field": "message.tenant_id"}
  },
  "context": {
    "request_id": "req_123"
  }
}
```

## Extension Services (beamline.ext.*)

### Extension Subjects Pattern

**Pattern**: `beamline.ext.{type}.{extension_id}.{version}`

**Types**:
- `pre` - Pre-processor extensions
- `validate` - Validator extensions
- `post` - Post-processor extensions

**Custom Provider Extensions**: Use `beamline.provider.{provider_id}.{version}` pattern (see Custom Provider Extensions section below)

**Versioning**: Mandatory (`.v1`, `.v2`, etc.)

**Examples**:
- `beamline.ext.pre.normalize_text.v1` - Text normalization pre-processor
- `beamline.ext.validate.pii_guard.v1` - PII validation
- `beamline.ext.post.mask_pii.v1` - PII masking post-processor

### Extension Request-Reply Contract

**Type**: Synchronous request-reply (NATS request-reply pattern)

**Implementation**: `router_extension_invoker:invoke/3`

**Request Format** (JSON):
```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "payload": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Original text",
    "metadata": {}
  },
  "metadata": {
    "lang": "en",
    "policy_id": "policy-456"
  }
}
```

**Response Format** (JSON):

**Pre/Post-processors**:
```json
{
  "payload": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Processed text",
    "metadata": {
      "normalized": "true"
    }
  },
  "metadata": {
    "lang": "en",
    "detected_lang": "en"
  }
}
```

**Validators**:
```json
{
  "status": "ok"
}
```
or
```json
{
  "status": "reject",
  "reason": "pii_detected",
  "details": {
    "field": "payload",
    "pattern": "credit_card"
  }
}
```

**Timeout**: Configurable per extension (from Extension Registry, default: 5000ms)

**Retry**: Configurable per extension (from Extension Registry, default: 0, exponential backoff: 100ms, 200ms, 400ms, ...)

**Error Handling**:
- Timeout: Retry if configured, otherwise return error
- Network error: Retry if configured, otherwise return error
- Invalid response: Return error (no retry)

**See**: `docs/EXTENSIONS_API.md` for complete contract specification

### Custom Provider Extensions

**Pattern**: `beamline.provider.{provider_id}.{version}`

**Type**: Synchronous request-reply (NATS request-reply pattern)

**Request Format** (JSON, CP2-style):
```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "provider_id": "my_crm_summarizer",
  "prompt": "User message + context",
  "parameters": {
    "max_tokens": 512
  },
  "context": {
    "conversation_id": "conv-1",
    "customer_id": "cust-42"
  }
}
```

**Response Format** (JSON, CP2-style):
```json
{
  "provider_id": "my_crm_summarizer",
  "output": "Summarized answer...",
  "usage": {
    "prompt_tokens": 120,
    "completion_tokens": 80
  },
  "metadata": {
    "source": "crm"
  }
}
```

**See**: `docs/EXTENSIONS_API.md#5-custom-provider-extensions` for complete contract specification

## Provider Service (beamline.provider.v1)

### Proto Service: `Provider.Invoke`

**Package**: `beamline.provider.v1`  
**Service**: `Provider`  
**RPC**: `Invoke(ProviderRequest) returns (ProviderResponse)`

### NATS Subject: `beamline.provider.v1.invoke`

**Type**: Request-Reply  
**Request**: `ProviderRequest` (protobuf) → JSON payload  
**Response**: `ProviderResponse` (protobuf) → JSON payload

**Request Mapping**:
```json
{
  "provider_id": "string",
  "message_id": "string",
  "tenant_id": "string",
  "trace_id": "string",
  "body": "string (base64 or UTF-8)",
  "parameters": {"key": "value"},
  "metadata": {"key": "value"},
  "timestamp_ms": 0
}
```

**Response Mapping**:
```json
{
  "request_id": "string",
  "status": "string",
  "body": "string (base64 or UTF-8)",
  "metadata": {"key": "value"},
  "latency_ms": 0,
  "cost": 0.0,
  "error_code": "string (optional)",
  "error_message": "string (optional)",
  "timestamp_ms": 0
}
```

### Proto Service: `Provider.InvokeStream`

**RPC**: `InvokeStream(ProviderRequest) returns (stream StreamChunk)`

### NATS Subject: `beamline.provider.v1.invoke.stream`

**Type**: Request-Stream  
**Request**: `ProviderRequest` (protobuf) → JSON payload  
**Response**: `StreamChunk` (protobuf) → JSON payload (multiple messages)

**Stream Chunk Mapping**:
```json
{
  "request_id": "string",
  "data": "string (base64 or UTF-8)",
  "done": false,
  "metadata": {"key": "value"}
}
```

## Ingress Service (beamline.ingress.v1) - DEPRECATED

> **Status**: ⚠️ **DEPRECATED** - This service is not part of core components. Ingress functionality is handled by C-Gateway (`apps/c-gateway/`).  
> **Note**: This section is kept for reference only. For actual implementation, see C-Gateway documentation.

### Proto Service: `Ingress.Send` (Deprecated)

**Package**: `beamline.ingress.v1`  
**Service**: `Ingress`  
**RPC**: `Send(IngressMessage) returns (IngressAck)`

### NATS Subject: `beamline.ingress.v1.message`

**Type**: Request-Reply  
**Request**: `IngressMessage` (protobuf) → JSON payload  
**Response**: `IngressAck` (protobuf) → JSON payload

**Request Mapping**:
```json
{
  "message_id": "string",
  "correlation_id": "string",
  "tenant_id": "string",
  "trace_id": "string (optional)",
  "message_type": "string",
  "payload": "string (base64 or UTF-8)",
  "metadata": {"key": "value"},
  "timestamp_ms": 0,
  "policy_id": "string (optional)"
}
```

**Response Mapping**:
```json
{
  "message_id": "string",
  "correlation_id": "string",
  "status": "accepted|rejected|error",
  "message": "string (optional)",
  "error_code": "string (optional)",
  "metadata": {"key": "value"},
  "timestamp_ms": 0
}
```

## Worker Service (beamline.worker.v1)

### ExecResult Contract

**Status**: ✅ **Protobuf definitions created, JSON fallback supported**

**Source of Truth**: Protobuf definition in `proto/beamline/worker/v1/result.proto`  
**JSON Contract**: `apps/otp/router/docs/API_CONTRACTS.md` (lines 443-545) - used as fallback

**NATS Subject**: `caf.exec.result.v1`

**Type**: Publish-Subscribe (JetStream)  
**Message**: `ExecResult` (protobuf primary, JSON fallback for backward compatibility)

**Consumer**: `router_result_consumer.erl`
- **JetStream**: Durable consumer with explicit ack policy
- **Durable Group**: `router-result-consumer` (configurable)
- **Queue Group**: `router-result-group` (configurable, for horizontal scaling)
- **Ack Policy**: `explicit` (messages ACKed after successful processing)
- **MaxDeliver**: 3 (configurable, prevents infinite retries)
- **DLQ**: Invalid messages sent to `caf.exec.result.v1.dlq` (configurable via `dlq_subject_pattern`)

**JSON Format** (current, proto TBD):
```json
{
  "assignment_id": "uuid-string",
  "request_id": "uuid-string",
  "status": "success|error|timeout|cancelled",
  "provider_id": "string",
  "job": {
    "type": "string"
  },
  "latency_ms": "number",
  "cost": "number",
  "error_code": "string (optional)",
  "error_message": "string (optional)",
  "payload": "any (optional)",
  "metadata": {"key": "value (optional)"},
  "tenant_id": "string (optional)",
  "trace_id": "string (optional)",
  "timestamp": "number (optional, milliseconds since epoch)"
}
```

**Field Requirements**:
- `assignment_id`: UUID string (required) - From `ExecAssignment.assignment_id`
- `request_id`: UUID string (required) - From `DecideRequest.request_id`
- `status`: `"success"`, `"error"`, `"timeout"`, or `"cancelled"` (required)
- `provider_id`: string (required) - Provider that executed the job
- `job.type`: string (required) - From `ExecAssignment.job.type`
- `latency_ms`: number (required) - Actual execution latency in milliseconds (>= 0)
- `cost`: number (required) - Actual execution cost (>= 0)
- `error_code`: string (optional) - Error code if `status: "error"`
- `error_message`: string (optional) - Human-readable error message
- `payload`: any (optional) - Execution result payload
- `metadata`: object (optional) - Arbitrary metadata
- `tenant_id`: string (optional) - From `ExecAssignment.tenant_id`
- `trace_id`: string (optional) - From `ExecAssignment.correlation.trace_id`
- `timestamp`: number (optional) - Result timestamp in milliseconds (default: current time)

**StepResult → ExecResult Mapping** (from CAF Worker):
- `StepStatus::ok` → `ExecResult.status = "success"`
- `StepStatus::error` → `ExecResult.status = "error"`
- `StepStatus::timeout` → `ExecResult.status = "timeout"`
- `StepStatus::cancelled` → `ExecResult.status = "cancelled"`
- `ErrorCode` (1xxx-5xxx) → `ExecResult.error_code` (string format)
- `ResultMetadata` (trace_id, flow_id, step_id, tenant_id) → `ExecResult` correlation fields

**Implementation**: `router_intake_validator.erl` uses protobuf decode with JSON fallback:
- Primary: `worker_pb:decode_msg(Payload, 'ExecResult')` → `convert_exec_result_to_map/1`
- Fallback: `jsx:decode(Payload, [return_maps])` (if protobuf decode fails or worker_pb not available)

**See**: `apps/otp/router/docs/API_CONTRACTS.md#execresult` for complete field definitions and examples.

### ExecAssignmentAck Contract

**Status**: ✅ **Protobuf definitions created, JSON fallback supported**

**Source of Truth**: Protobuf definition in `proto/beamline/worker/v1/ack.proto`  
**JSON Contract**: `apps/otp/router/docs/API_CONTRACTS.md` (lines 408-441) - used as fallback

**NATS Subject**: `caf.exec.assign.v1.ack`

**Type**: Publish-Subscribe (JetStream, optional)  
**Message**: `ExecAssignmentAck` (protobuf primary, JSON fallback for backward compatibility)

**Consumer**: `router_ack_consumer.erl`
- **JetStream**: Durable consumer with explicit ack policy (optional)
- **Durable Group**: `router-ack-consumer` (configurable)
- **Queue Group**: `router-ack-group` (configurable, for horizontal scaling)
- **Ack Policy**: `explicit` (messages ACKed after successful processing)
- **MaxDeliver**: 3 (configurable, prevents infinite retries)
- **DLQ**: Invalid messages sent to `caf.exec.assign.v1.ack.dlq` (configurable via `dlq_subject_pattern`)

**Protobuf Definition**:
```protobuf
message ExecAssignmentAck {
  string assignment_id = 1;
  string status = 2;  // "accepted", "rejected", "error"
  string message = 3;  // optional
  string request_id = 4;  // optional
  string trace_id = 5;  // optional
  string tenant_id = 6;  // optional
  int64 acknowledged_at = 7;  // optional, milliseconds since epoch
}
```

**JSON Format** (fallback for backward compatibility):
```json
{
  "assignment_id": "uuid-string",
  "status": "accepted|rejected|error",
  "message": "string (optional)"
}
```

**Field Requirements**:
- `assignment_id`: UUID string (required) - From `ExecAssignment.assignment_id`
- `status`: `"accepted"`, `"rejected"`, or `"error"` (required)
- `message`: string (optional) - Status message

**Note**: Current JSON contract in `API_CONTRACTS.md` shows minimal fields. Additional fields (e.g., `request_id`, `error_code`, `acknowledged_at`, `tenant_id`, `trace_id`) may be added in future versions or via headers.

**Implementation**: `router_intake_validator.erl` uses protobuf decode with JSON fallback:
- Primary: `worker_pb:decode_msg(Payload, 'ExecAssignmentAck')` → `convert_exec_assignment_ack_to_map/1`
- Fallback: `jsx:decode(Payload, [return_maps])` (if protobuf decode fails or worker_pb not available)

**See**: `apps/otp/router/docs/API_CONTRACTS.md#execassignmentack` for complete field definitions and examples.

## Message Types Summary

### Flow Package (beamline.flow.v1)

- `Message`: Incoming message for routing
- `RouteRequest`: Request for routing decision
- `RouteDecision`: Router decision

**NATS Mapping**:
- `beamline.router.v1.decide`: RouteRequest ↔ RouteDecision

### Provider Package (beamline.provider.v1)

- `ProviderRequest`: Request to provider
- `ProviderResponse`: Provider response
- `StreamChunk`: Streaming response chunk

**NATS Mapping**:
- `beamline.provider.v1.invoke`: ProviderRequest ↔ ProviderResponse
- `beamline.provider.v1.invoke.stream`: ProviderRequest → StreamChunk (stream)

### Worker Package (beamline.worker.v1)

- `ExecResult`: Worker execution result (protobuf primary, JSON fallback)
- `ExecAssignmentAck`: Worker acknowledgment (protobuf primary, JSON fallback)

**NATS Mapping**:
- `caf.exec.result.v1`: ExecResult (Worker → Router)
- `caf.exec.assign.v1.ack`: ExecAssignmentAck (Worker → Router)

**Note**: Proto definitions for `ExecResult` and `ExecAssignmentAck` are planned for CP2+. Current JSON contracts are documented in `apps/otp/router/docs/API_CONTRACTS.md`.

### Ingress Package (beamline.ingress.v1) - DEPRECATED

> **Status**: ⚠️ **DEPRECATED** - Not part of core components. Use C-Gateway instead.

- `IngressMessage`: Message to send via Ingress (deprecated)
- `IngressAck`: Message receipt acknowledgment (deprecated)

**NATS Mapping** (deprecated):
- `beamline.ingress.v1.message`: IngressMessage ↔ IngressAck (not used)

## Type Conversions

### Protobuf → NATS JSON

- `string` → JSON string
- `int32`, `int64` → JSON number
- `double` → JSON number
- `bytes` → JSON string (UTF-8 or base64)
- `map<string, string>` → JSON object
- `Message` (nested) → JSON object

### NATS JSON → Protobuf

- JSON string → `string` or `bytes` (UTF-8)
- JSON number → `int32`, `int64`, `double`
- JSON object → `map<string, string>` or nested `Message`
- Base64 string → `bytes`

## Versioning

- **Protobuf**: Package versioning (`beamline.flow.v1`)
- **NATS**: Subject versioning (`beamline.router.v1.*`)
- **Breaking Changes**: New package version → new subject version (e.g., `v2`)

## Runtime Contract Validation

Router performs runtime validation of NATS headers against the documented contract:

- **Validation**: Headers are validated according to `docs/NATS_SUBJECTS.md` and `docs/API_CONTRACTS.md`
- **Violations**: Contract violations are logged with `contract_violation: true` and metrics are emitted
- **Backward Compat**: Empty headers (CP1 baseline) do not trigger violations
- **Metrics**: `router_nats_contract_violations_total` counter tracks violations
- **Tests**: See `router_nats_contract_validation_SUITE.erl` for validation test cases

**Validated Headers** (for `caf.exec.result.v1`):
- `trace_id` (optional): Must be binary if present, cannot be empty
- `tenant_id` (optional): Must be binary if present, cannot be empty
- `version` (optional): Must be `"1"` if present
- `Nats-Msg-Id` (required for JetStream): Must be binary if headers are present

**See**: `apps/otp/router/src/router_result_consumer.erl` - `validate_nats_headers/3` function

## Dead-Letter Queue (DLQ) Payload Format

**CP2+**: Messages that exhaust MaxDeliver are sent to DLQ with full context for debugging and reprocessing.

### DLQ Subject Pattern

- **Default**: `{original_subject}.dlq` (e.g., `beamline.router.v1.decide.dlq`)
- **Configurable**: `dlq_subject_pattern` (e.g., `beamline.router.v1.intake.dlq` for unified DLQ)
- **Can be disabled**: `dlq_enabled: false`

### DLQ Message Format

**Source of Truth**: `apps/otp/router/src/router_jetstream.erl` → `build_dlq_payload/2`

**CP2+ DLQ Payload** (JSON):

```json
{
  "original_subject": "beamline.router.v1.decide",
  "msg_id": "nats-msg-id-123",
  "reason": "maxdeliver_exhausted",
  "error_code": "MAXDELIVER_EXHAUSTED",
  "timestamp": 1704067200000,
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "tenant_id": "tenant-xyz",
  "message": {
    "id": "nats-msg-id-123",
    "subject": "beamline.router.v1.decide",
    "headers": {
      "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
      "tenant_id": "tenant-xyz",
      "version": "1",
      "Nats-Msg-Id": "nats-msg-id-123"
    },
    "payload": "..."
  }
}
```

**Note**: The `message` field contains the full original message. For security, this can be disabled via `dlq_include_full_message: false` configuration. When disabled, only the context fields (`trace_id`, `tenant_id`, `original_subject`, `error_code`) are included.

**DLQ Headers**:

```json
{
  "x-dlq-reason": "maxdeliver_exhausted",
  "x-original-msg-id": "nats-msg-id-123",
  "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
  "tenant_id": "tenant-xyz"
}
```

### DLQ Payload Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `original_subject` | string | Yes | Original NATS subject (e.g., `beamline.router.v1.decide`) |
| `msg_id` | string | Yes | JetStream message ID |
| `reason` | string | Yes | Reason for DLQ: `maxdeliver_exhausted`, `validation_failed`, `processing_error` |
| `error_code` | string | Yes | Error code derived from reason: `MAXDELIVER_EXHAUSTED`, `VALIDATION_FAILED`, `PROCESSING_ERROR` |
| `timestamp` | integer | Yes | Timestamp in milliseconds (Unix epoch) |
| `trace_id` | string | Optional | Trace ID extracted from original message headers or payload |
| `tenant_id` | string | Optional | Tenant ID extracted from original message headers or payload |
| `message` | object | Optional | Full original message (can be disabled via `dlq_include_full_message: false` config) |
| `message.id` | string | Yes (if `message` present) | Message ID |
| `message.subject` | string | Yes (if `message` present) | Original subject |
| `message.headers` | object | Optional (if `message` present) | Original headers (including `trace_id`, `tenant_id`, `version`) |
| `message.payload` | string | Optional (if `message` present) | Original payload (may be truncated or hashed for security) |

### DLQ Examples

#### Example 1: MaxDeliver Exhausted (Decide Request)

**Original Subject**: `beamline.router.v1.decide`  
**DLQ Subject**: `beamline.router.v1.decide.dlq`

**DLQ Payload**:

```json
{
  "original_subject": "beamline.router.v1.decide",
  "msg_id": "nats-msg-id-decide-123",
  "reason": "maxdeliver_exhausted",
  "timestamp": 1704067200000,
  "message": {
    "id": "nats-msg-id-decide-123",
    "subject": "beamline.router.v1.decide",
    "headers": {
      "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
      "tenant_id": "tenant-xyz",
      "version": "1",
      "Nats-Msg-Id": "nats-msg-id-decide-123"
    },
    "payload": "{\"version\":\"1\",\"tenant_id\":\"tenant-xyz\",\"request_id\":\"req-123\",\"task\":{...}}"
  }
}
```

#### Example 2: MaxDeliver Exhausted (ExecResult)

**Original Subject**: `caf.exec.result.v1`  
**DLQ Subject**: `caf.exec.result.v1.dlq`

**DLQ Payload**:

```json
{
  "original_subject": "caf.exec.result.v1",
  "msg_id": "nats-msg-id-result-456",
  "reason": "maxdeliver_exhausted",
  "timestamp": 1704067201000,
  "message": {
    "id": "nats-msg-id-result-456",
    "subject": "caf.exec.result.v1",
    "headers": {
      "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
      "tenant_id": "tenant-xyz",
      "version": "1",
      "Nats-Msg-Id": "nats-msg-id-result-456"
    },
    "payload": "{\"assignment_id\":\"assign-123\",\"status\":\"success\",\"provider_id\":\"openai\",...}"
  }
}
```

#### Example 3: Tenant Validation Failed (ExecAssignmentAck)

**Original Subject**: `caf.exec.assign.v1.ack`  
**DLQ Subject**: `caf.exec.assign.v1.ack.dlq`

**DLQ Payload**:

```json
{
  "original_subject": "caf.exec.assign.v1.ack",
  "msg_id": "nats-msg-id-ack-789",
  "reason": "maxdeliver_exhausted",
  "timestamp": 1704067202000,
  "message": {
    "id": "nats-msg-id-ack-789",
    "subject": "caf.exec.assign.v1.ack",
    "headers": {
      "trace_id": "4bf92f3577b34da6a3ce929d0e0e4736",
      "tenant_id": "tenant-xyz",
      "version": "1",
      "Nats-Msg-Id": "nats-msg-id-ack-789"
    },
    "payload": "{\"assignment_id\":\"assign-123\",\"status\":\"accepted\",...}"
  }
}
```

### DLQ Context Preservation

**CP2+ Requirement**: DLQ messages must preserve full context for debugging and reprocessing:

- **Trace Context**: `trace_id` from headers (for distributed tracing)
- **Tenant Context**: `tenant_id` from headers (for tenant isolation)
- **Correlation IDs**: `request_id`, `assignment_id` from payload (for request correlation)
- **Error Context**: `reason`, `error_code` (if available) for failure analysis

**Implementation**: `router_jetstream.erl` → `build_dlq_payload/2` and `build_dlq_headers/2` functions preserve all context fields.

### DLQ Processing

**CP2+**: DLQ messages can be:
- **Reprocessed**: After fixing underlying issues (tenant validation, schema errors, etc.)
- **Analyzed**: For failure patterns and error rates
- **Archived**: For compliance and audit trails

**See**: `apps/otp/router/src/router_jetstream.erl` for DLQ implementation details.

## References

- **Core Message Fields**: `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` - Complete specification for all core fields
- **Proto Definitions**: `proto/beamline/*/v1/*.proto`
- **NATS Subjects**: `docs/NATS_SUBJECTS.md`
- **API Contracts**: `docs/API_CONTRACTS.md`
- **API Registry**: `docs/ARCHITECTURE/api-registry.md` (REST API contracts, including `POST /api/v1/routes/decide`)
- **Compatibility Rules**: `docs/ARCHITECTURE/compatibility-rules.md`
- **Contract Validation Tests**: `apps/otp/router/test/router_nats_contract_validation_SUITE.erl`
- **Gateway CP1-Smoke**: `docs/archive/dev/GATEWAY_CP1_SMOKE_DETAILED_CHECKLIST.md` (Gateway CP1-smoke verification scenarios)
