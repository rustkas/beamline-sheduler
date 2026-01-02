# API Contracts (Router ↔ CAF) — Updated Strict Requirements

Defines JSON message formats for integrating Router and CAF over NATS: `DecideRequest`, `DecideResponse`, `ErrorResponse`, `ExecAssignment`, `ExecAssignmentAck`, `ExecResult`, and `WorkerHeartbeat`. Includes examples, validation rules, and CP1 test scenarios.

**Related Documentation:**
- Extensions API: see [docs/EXTENSIONS_API.md](EXTENSIONS_API.md) (EN) / [docs/EXTENSIONS_API_RU.md](EXTENSIONS_API_RU.md) (RU)

## REST API Documentation

**Gateway REST API**: Interactive documentation is available at `/api` (Swagger UI) when the Gateway is running. A static OpenAPI file in `docs/` is not provided.

The Gateway provides REST endpoints for:
- Message routing: `POST /api/v1/routes/decide`
- Message management: `POST /api/v1/messages`, `GET /api/v1/messages`
- Policy management: `GET /api/v1/policies`, `POST /api/v1/policies`
- Flow validation: `POST /api/v1/flows/validate`
- Health checks: `GET /_health`

Interactive API documentation is available at `http://localhost:3000/api` (Swagger UI) when Gateway is running.

## Overview
- Format: JSON.
- `version`: string, current `"1"`.
- Correlation: `request_id` (required), `trace_id` (recommended).
- Multi-tenancy: `tenant_id` (string) for segmentation and authorization.
- Errors: unified `ErrorResponse` format.

## Field Requirements: Proto vs Runtime

### Proto Contract Level

All fields in Proto messages are **optional** (protobuf v3 semantics). This allows:
- Backward compatibility when adding new fields
- Gradual migration of clients
- Flexible message construction

### Runtime Validation Level

Router enforces **required fields at runtime** via validation logic. Missing required fields result in `invalid_request` error.

**Required Fields**:
- `DecideRequest.message`: `tenant_id`, `message_type`, `payload` (required)
- `DecideRequest`: `message` (required)
- `DecideResponse.decision`: `provider_id`, `reason` (required)

**Validation Rules**:
1. Router validates all incoming requests against runtime requirements
2. Missing required fields → `ErrorResponse` with `code: "invalid_request"`
3. Optional fields can be omitted → Router handles gracefully
4. Proto contract remains optional → No breaking changes for Proto consumers

**Note**: This two-level approach (Proto optional, runtime required) is intentional and allows backward compatibility while ensuring data quality at runtime.

## Proto Contract vs NATS JSON Format

### Two-Level Architecture

Router uses a **two-level contract architecture**:
1. **Proto Contract** (wire protocol / ABI): Defines Proto message fields only
2. **NATS JSON Format** (logical payload): Includes Proto fields + NATS-specific fields

### NATS-Specific Fields

The following fields are **added at NATS adapter layer** and are **not** in Proto contract:
- `version` - NATS message version
- `request_id` - NATS correlation ID
- `task` - NATS-specific task wrapper
- `constraints` - NATS-specific routing constraints
- `push_assignment` - Flag to trigger ExecAssignment publication

**Source of Truth**:
- **Proto Contract**: Generated code (`flow_pb.erl`, `flow_pb.hrl`)
- **NATS JSON Format**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

**See**: `docs/archive/dev/ROUTER_PROTO_NATS_CONSISTENCY.md` Section 1.5 for complete explanation.

## DecideRequest (strict)
Fields:
- `version`: must be `"1"`
- `tenant_id`: string
- `request_id`: UUID string
- `trace_id?`: string
- `task`: `{ type: "chat"|"completion"|"embedding", payload: object }`
- `policy_id?`: string
- `constraints?`: object
- `metadata?`: object
- `push_assignment?`: boolean
- `assignment_subject?`: string (default `caf.exec.assign.v1`)
- `context?`: `{ session_id?: string, user_id?: string }`

Payload Schemas:
- `chat`: `{ text: string, role?: "user"|"system"|"assistant", metadata?: object }`
- `completion`: `{ prompt: string, max_tokens?: number, temperature?: number }`
- `embedding`: `{ input: string | string[], metadata?: object }`

## DecideResponse
Fields:
- `ok`: boolean
- `decision`: `{ provider_id: string, priority: number, expected_latency_ms?: number, expected_cost?: number, reason: string, policy_id?: string, metadata?: object }`
- `context`: `{ request_id: string, trace_id?: string }`

## ErrorResponse
```json
{
  "ok": false,
  "error": {
    "code": "unauthorized|invalid_request|policy_not_found|denied|decision_failed|internal",
    "message": "<string>",
    "details": { "any": "map" }
  },
  "context": { "request_id": "<uuid>", "trace_id": "<string>" }
}
```

## ExecAssignment (Router → CAF)
```json
{
  "version": "1",
  "assignment_id": "<uuid>",
  "request_id": "<uuid>",
  "executor": { "provider_id": "<string>", "channel": "nats|grpc", "endpoint": "<string>" },
  "job": { "type": "<string>", "payload_ref": "<uri>", "payload": null },
  "options": { "priority": 50, "deadline_ms": 5000, "retry": { "max_attempts": 2, "backoff_ms": 200 } },
  "correlation": { "trace_id": "<string>" },
  "decision": { "provider_id": "<string>", "priority": 50, "expected_latency_ms": 850, "expected_cost": 0.012, "reason": "best_score", "metadata": { "any": "map" } },
  "metadata": { "any": "map" },
  "tenant_id": "<string>"
}
```

**Idempotency Note**:

- HTTP operations use a composite signature: `tenant_id + assignment_id|ack_id|usage_id`.
- TTL is configurable via `IDEMPOTENCY_TTL_MS` (Gateway) and enforced for duplicate suppression.
- On duplicate, responses are `200 OK` with the same result; conflicting payload fields → `409 Conflict`.
- Metrics: `beamline_gateway_idempotency_hits_total`, `beamline_gateway_idempotency_miss_total`, `beamline_gateway_idempotency_store_errors_total`.
- Router scope: `assignment_id` is unique; CAF maintains a TTL store to prevent duplicate processing of assignments/results.

## ExecAssignmentAck (CAF → Router)
```json
{
  "version": "1",
  "assignment_id": "<uuid>",
  "status": "accepted|rejected",
  "reason": "<string>",
  "correlation": { "trace_id": "<string>" },
  "tenant_id": "<string>",
  "metadata": { "any": "map" }
}
```

## ExecResult (CAF → Router)

**Runtime Contract Validation**: Router validates headers at runtime according to `docs/NATS_SUBJECTS.md`. Violations are logged with `contract_violation: true` and metrics are emitted (`router_nats_contract_violations_total`). See `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` for details.

**CP1 StepResult Contract**: CAF Worker internally uses a unified `StepResult` type (C++ struct) for all block executions. This type is converted to `ExecResult` JSON format via `ResultConverter::to_exec_result_json()` before publishing to NATS. See `apps/caf/processor/docs/ARCHITECTURE_ROLE.md#43-stepresult-contract-cp1-invariant` for the complete StepResult contract definition.

**StepResult → ExecResult Mapping**:
- `StepStatus::ok` → `ExecResult.status = "success"`
- `StepStatus::error` → `ExecResult.status = "error"`
- `StepStatus::timeout` → `ExecResult.status = "timeout"`
- `StepStatus::cancelled` → `ExecResult.status = "cancelled"`
- `ErrorCode` (1xxx-5xxx) → `ExecResult.error_code` (string format)
- `ResultMetadata` (trace_id, flow_id, step_id, tenant_id) → `ExecResult` correlation fields

**Implementation**: `apps/caf/processor/include/beamline/worker/result_converter.hpp` (ResultConverter class)

**Payload Format (JSON)**:
```json
{
  "version": "1",
  "assignment_id": "<uuid>",
  "request_id": "<uuid>",
  "status": "success|error|timeout|cancelled",
  "provider_id": "<string>",
  "job": {
    "type": "<string>"
  },
  "latency_ms": 0,
  "cost": 0.0,
  "timestamp": 0,
  "trace_id": "<string>",
  "tenant_id": "<string>",
  "msg_id": "<string>"
}
```

**Headers (JetStream, priority over payload)**:
- `trace_id` (optional): Trace identifier
- `tenant_id` (optional): Tenant identifier
- `version` (optional): Message version
- `Nats-Msg-Id`: Message ID for ACK/NAK operations
- `traceparent` (optional): W3C trace context, used for OTel propagation

**Validation**:
- At least one of `assignment_id` or `request_id` must be present
- `status` is required and must be one of: `"success"`, `"error"`, `"timeout"`, `"cancelled"`
- `provider_id` is required
- `timestamp` is in milliseconds (integer)
- Headers have priority over payload for `trace_id`, `tenant_id`, `version`
- `msg_id` is used for ACK/NAK operations (from headers or payload)

**Note**: This is the simplified format used by Router. CAF may publish a more detailed format with nested `result` and `error` fields, but Router extracts the fields above.

## WorkerHeartbeat (CAF → Router)
```json
{
  "version": "1",
  "worker_id": "<string>",
  "timestamp": "ISO 8601",
  "status": "ready|busy|degraded",
  "resources": { "cpu": 0.9, "gpu": 0.5, "mem_bytes": 123456789, "jobs_in_flight": 3 },
  "tenant_id": "<string>",
  "metadata": { "any": "map" }
}
```

## Validation (CP1/B)
- `version` == `"1"`.
- `request_id`/`assignment_id`: non-empty strings (UUID preferred, any unique ID allowed).
- `policy_id`: string or default policy applied by Router if missing.
- `context`: map; extra keys allowed.
- `decision.provider_id`: required; if missing → `ErrorResponse` with `decision_failed`.
- `assignment_subject`: string; default `caf.exec.assign.v1`.
- `job.type`: string; CAF must verify type is supported.
- `payload_ref` vs `payload`: allow either; prefer `payload_ref` if both present.
- Ack: `status` in {`accepted`,`rejected`}; `assignment_id` required.
- Result: `status` in {`success`,`error`,`timeout`,`canceled`}; include `error.code` when status=`error`.
- Heartbeat: `worker_id` non-empty; `status` one of allowed values.
 - Headers validated in Router subscribers: `version`, `tenant_id`, `trace_id`, `traceparent`; fallback to payload if missing; JetStream requires `Nats-Msg-Id` for ACK/NAK.

### Runtime Validation Rules

Router validates all incoming requests against Proto contract and runtime rules.

**Required Fields Validation**:
- Missing required fields → `ErrorResponse` with `code: "invalid_request"`
- Field type mismatches → `ErrorResponse` with `code: "invalid_request"`
- Field value out of range → `ErrorResponse` with `code: "invalid_request"`

**Error Codes for Validation Failures**:
- `invalid_request`: Missing required fields, invalid types, out of range values
- `unauthorized`: Authentication/authorization failed (if RBAC enabled)
- `internal`: Any other validation error

**Note**: Gateway HTTP API implements rate limiting (429) that is checked **BEFORE** Router validation. See `docs/GATEWAY_ROUTES_SPEC.md` and `docs/GATEWAY_ROUTER_ERROR_HANDLING_PRIORITY.md` for complete error handling priority specification.

**Example Error Response**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Missing required field: tenant_id",
    "details": {
      "field": "message.tenant_id",
      "type": "required_field_missing"
    }
  },
  "context": {
    "request_id": "req_123",
    "trace_id": "trace_xyz"
  }
}
```

**Validation Order**:
1. JSON parsing (malformed JSON → `invalid_request`)
2. Required fields check (missing required → `invalid_request`)
3. Field type validation (type mismatch → `invalid_request`)
4. Field value validation (out of range → `invalid_request`)
5. Business logic validation (policy not found → `policy_not_found`)

## Error Codes
- `unauthorized`: authentication/authorization failed.
- `invalid_request`: malformed JSON or missing required fields.
- `policy_not_found`: policy not found.
- `decision_failed`: Router failed to make a decision.
- `internal`: any other error.

## Examples
- Minimal `DecideRequest`:
```json
{ "version":"1", "request_id":"e3b0c442-...", "policy_id":"policy:default", "context":{} }
```
- Successful `DecideResponse`:
```json
{ "ok":true, "decision":{ "provider_id":"openai:gpt-4o", "priority":50, "expected_latency_ms":850, "expected_cost":0.012, "reason":"best_score" }, "context":{ "request_id":"e3b0c442-..." } }
```
- `ExecAssignment`:
```json
{ "version":"1", "assignment_id":"a1f5b1d2-...", "request_id":"e3b0c442-...", "executor":{ "provider_id":"openai:gpt-4o", "channel":"nats" }, "job":{ "type":"text.generate", "payload_ref":"s3://bucket/key" }, "options":{ "priority":50 }, "correlation":{ "trace_id":"tr-123" }, "decision":{ "provider_id":"openai:gpt-4o", "expected_latency_ms":850, "expected_cost":0.012, "reason":"best_score" }, "metadata":{} }
```

- `ExecAssignmentAck` (accepted):
```json
{ "version":"1", "assignment_id":"a1f5b1d2-...", "status":"accepted", "correlation":{ "trace_id":"tr-123" }, "tenant_id":"t-1" }
```

- `ExecResult` (success):
```json
{ "version":"1", "assignment_id":"a1f5b1d2-...", "request_id":"e3b0c442-...", "status":"success", "result":{ "payload":{ "text":"Hello" }, "metrics":{ "latency_ms": 321 } }, "correlation":{ "trace_id":"tr-123" }, "tenant_id":"t-1" }
```

- `WorkerHeartbeat`:
```json
{ "version":"1", "worker_id":"caf-01", "timestamp":"2025-11-10T12:00:00Z", "status":"ready", "resources":{ "cpu":0.12, "gpu":0.0, "mem_bytes":134217728, "jobs_in_flight":0 }, "tenant_id":"t-1" }
```

## Test Scenarios (CP1)
- Unit (Router):
  - Validate incoming `DecideRequest`: required fields, types.
  - Build `DecideResponse`: presence of `provider_id`, expected metrics.
  - Errors: missing `policy_id`, empty `context`, malformed JSON → `invalid_request`.
- Integration (Router↔CAF, mock NATS):
  - With `push_assignment=true`, Router publishes valid `ExecAssignment` to `assignment_subject`.
  - CAF receives assignment, checks `job.type`, returns ACK.
  - Negative: CAF rejects assignment (unknown `job.type`) → Router logs rejection.
  - CAF publishes `ExecResult` on success; Router correlates and emits usage event.
  - Heartbeat stream validates worker liveness and capacity reporting.
- Load (minimal):
  - 1000 sequential `DecideRequest` in mock NATS → all responses without timeouts.
- Security:
  - Deny on invalid `tenant_id` when ACLs enabled on NATS.

## Evolution Notes
- Extending `job` for provider-specific structures is allowed in new versions.
- Switching to gRPC for CAF is reflected via `executor.channel` and a separate gRPC contract (post-CP1).
## Gateway SSE Results Streaming (Quickstart)

Steps:
- Enable NATS subscriber for results:
  - `ENABLE_RESULTS_SUBSCRIBER=true`
  - `NATS_URL=nats://localhost:4222`
  - `CAF_RESULT_SUBJECT=caf.exec.result.v1`
- Start Gateway and open SSE stream:
  - `GET /api/v1/results/stream?assignment_id=<id>&tenant_id=<t>&provider_id=<p>&heartbeat_ms=15000`
- Filters:
  - `tenant_id` and `provider_id` filter incoming `ExecResult` events
  - `heartbeat_ms` adds periodic heartbeat messages to keep the connection alive
- Event format:
  - Data object mirrors `ExecResult` payload fields (`assignment_id`, `request_id`, `status`, `provider_id`, `latency_ms`, `cost`, `timestamp`, `tenant_id`, `trace_id`)
### Traceparent Example
Header:
```
traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01
```

### HTTP Idempotency Signature
- Signature: `tenant_id + assignment_id|ack_id|usage_id`
- TTL: `IDEMPOTENCY_TTL_MS` (default 300000 ms)

### JetStream/Metrics ENV
- `NATS_MAX_DELIVER` — max redeliveries
- `NATS_ACK_WAIT_MS` — ack wait (ms)
- `NATS_BACKOFF_MS` — backoff intervals (ms, comma‑separated)
- `NATS_SUBJECT_ALLOWLIST` — allowed subjects; others NAK’d
- `METRICS_TENANT_LABEL_ALLOWLIST` — limit tenant label cardinality in metrics
