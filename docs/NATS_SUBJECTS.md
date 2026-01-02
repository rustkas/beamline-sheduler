# NATS Subjects

This document defines NATS subjects used across BeamLine components and the payload formats.

## Router

### beamline.router.v1.decide

- Purpose: synchronous request-reply for routing decisions
- Request: JSON `DecideRequest` (see `docs/API_CONTRACTS.md`)
- Reply: JSON `DecideResponse` or `ErrorResponse` (see `docs/API_CONTRACTS.md`)
- **Consumer**: `router_decide_consumer.erl` (JetStream durable consumer)
  - Durable group: `router-decide-consumer` (configurable via `nats_js_durable_group_decide`)
  - Queue group: `router-decide-group` (configurable via `nats_js_deliver_group_decide`)
  - Ack policy: `explicit` (messages ACKed after successful processing)
  - MaxDeliver: 3 (configurable via `nats_js_max_deliver`, default: 3)
  - Backoff schedule: `[1, 2, 4]` seconds (configurable via `nats_js_backoff_seconds`)
  - **CP2+**: Redelivery with exponential backoff; DLQ on MaxDeliver exhaustion
- Headers:
  - `trace_id`: hex-32 (optional)
  - `tenant_id`: string (optional)
  - `version`: `1`
  - `traceparent`: W3C Trace Context
  - `Nats-Msg-Id` (JetStream): Message ID for ACK/NAK operations
- Notes: 
  - **CP2+**: Uses JetStream durable consumer for reliable message delivery
  - Supports JSON for NATS payloads
  - `push_assignment` flag triggers ExecAssignment publication
  - Request fields: `version`, `tenant_id`, `request_id`, `trace_id` (optional), `task`, `policy_id` (optional), `constraints`, `metadata`, `push_assignment`
  - Response includes `context` with `request_id` and `trace_id` (if provided)
  - Messages are ACKed after successful processing or error response sent to Gateway
  - **CP2+ Redelivery Policy**:
    - Messages are NAKed (negative acknowledged) for retry on processing errors
    - Redelivery uses exponential backoff based on delivery count
    - MaxDeliver exhaustion: after N delivery attempts, message is ACKed and sent to DLQ
    - DLQ subject: `{original_subject}.dlq` (e.g., `beamline.router.v1.decide.dlq`)
  - **CP2+ Durability**: Durable consumer survives Router restarts; messages are redelivered after restart
  - **CP2+ Metrics**: `router_jetstream_ack_total`, `router_redelivery_total`, `router_dlq_total`

### beamline.router.v1.decide.reply

- Purpose: reply subject for DecideResponse/ErrorResponse
- Payload: JSON `DecideResponse` or `ErrorResponse`
- Headers: same as `decide` (optional)
- Notes: reply-inbox pattern for request-reply
 - Gateway can set `NATS_REPLY_SUBJECT` to use explicit reply inbox and subscribe for a single message

### beamline.router.v1.decide.dlq (Dead-Letter Queue)

- Purpose: DLQ for invalid decide messages (validation failures)
- Payload: JSON DLQ message (see DLQ Message Format below)
- Headers: None (standard NATS publish)
- Notes:
  - Published by Router when validation fails (schema errors, version errors, correlation errors, tenant errors)
  - DLQ subject pattern: `{original_subject}.dlq` (default) or configurable via `dlq_subject_pattern`
  - DLQ message contains payload hash (SHA256) instead of full payload (security)
  - DLQ publication is best-effort (errors are logged but don't fail processing)
  - DLQ can be disabled via `dlq_enabled` configuration (default: true)

### beamline.router.v1.admin.get_extension_health

- Purpose: Get health metrics for all extensions (admin endpoint)
- Request: JSON `{}` (empty body, optional `api_key` for auth)
- Reply: JSON `GetExtensionHealthResponse` (see `docs/ARCHITECTURE/api-registry.md`)
- **Handler**: `router_admin_nats.erl` → `handle_get_extension_health/1`
- Notes:
  - Request-reply pattern (synchronous)
  - Returns health metrics for all registered extensions
  - Health status: `healthy` (success_rate >= 0.95), `degraded` (>= 0.80), `unhealthy` (< 0.80)
  - Includes latency percentiles (p50, p95, p99), success/failure counts, circuit breaker state

### beamline.router.v1.admin.get_circuit_breaker_states

- Purpose: Get circuit breaker states for all extensions (admin endpoint)
- Request: JSON `{}` (empty body, optional `api_key` for auth)
- Reply: JSON `GetCircuitBreakerStatesResponse` (see `docs/ARCHITECTURE/api-registry.md`)
- **Handler**: `router_admin_nats.erl` → `handle_get_circuit_breaker_states/1`
- Notes:
  - Request-reply pattern (synchronous)
  - Returns circuit breaker state for each extension
  - States: `closed`, `open`, `half_open`
  - Includes `opened_at_ms` timestamp when circuit was opened

### beamline.router.v1.admin.dry_run_pipeline

- Purpose: Execute dry-run of extension pipeline (admin endpoint)
- Request: JSON `DryRunPipelineRequest` (see `docs/ARCHITECTURE/api-registry.md`)
- Reply: JSON `DryRunPipelineResponse` (see `docs/ARCHITECTURE/api-registry.md`)
- **Handler**: `router_admin_nats.erl` → `handle_dry_run_pipeline/1`
- Notes:
  - Request-reply pattern (synchronous)
  - Executes pipeline without actual provider invocation
  - Returns decision, executed extensions, and final payload
  - Used for testing and debugging pipeline configuration

### beamline.router.v1.admin.get_pipeline_complexity

- Purpose: Get pipeline complexity assessment (admin endpoint)
- Request: JSON `{"tenant_id": "...", "policy_id": "..."}` (optional `api_key` for auth)
- Reply: JSON `GetPipelineComplexityResponse` (see `docs/ARCHITECTURE/api-registry.md`)
- **Handler**: `router_admin_nats.erl` → `handle_get_pipeline_complexity/1`
- Notes:
  - Request-reply pattern (synchronous)
  - Returns complexity score, extension counts, estimated latency, warnings, recommendations
  - Used for pipeline optimization and monitoring

**DLQ Message Format**:

**Actual Implementation** (from `router_jetstream.erl`):
```json
{
  "original_subject": "beamline.router.v1.decide",
  "msg_id": "msg-uuid",
  "reason": "maxdeliver_exhausted",
  "error_code": "MAXDELIVER_EXHAUSTED",
  "timestamp": 1706367600123,
  "trace_id": "trace_def456",
  "tenant_id": "tenant_123",
  "message": {
    "id": "msg-uuid",
    "subject": "beamline.router.v1.decide",
    "headers": {
      "trace_id": "trace_def456",
      "tenant_id": "tenant_123"
    },
    "payload": {
      "request_id": "req-uuid",
      "tenant_id": "tenant_123",
      "trace_id": "trace_def456"
    }
  }
}
```

**DLQ Headers** (from `router_jetstream.erl`):
```json
{
  "x-dlq-reason": "maxdeliver_exhausted",
  "x-original-msg-id": "msg-uuid",
  "trace_id": "trace_def456",
  "tenant_id": "tenant_123"
}
```

**Fields**:
- `original_subject` (required): Original NATS subject
- `msg_id` (required): Message ID
- `reason` (required): Reason for DLQ (e.g., `maxdeliver_exhausted`, `validation_failed`)
- `error_code` (required): Error code derived from reason (e.g., `MAXDELIVER_EXHAUSTED`, `VALIDATION_FAILED`)
- `timestamp` (required): Timestamp when message was sent to DLQ (milliseconds since epoch)
- `trace_id` (optional): Trace ID from original message headers/payload
- `tenant_id` (optional): Tenant ID from original message headers/payload
- `message` (optional): Full original message (can be disabled via `dlq_include_full_message` config)

**Note**: The `message` field contains the full original message. For security, this can be disabled via `dlq_include_full_message: false` configuration. When disabled, only the context fields (`trace_id`, `tenant_id`, `original_subject`, `error_code`) are included.

**Legacy Format** (for reference, used by `router_intake_error_handler.erl`):
```json
{
  "original_subject": "beamline.router.v1.decide",
  "original_payload_hash": "sha256-hex-string",
  "validation_error": {
    "code": "SCHEMA_VALIDATION_FAILED",
    "message": "Schema validation failed: missing tenant_id",
    "severity": "error"
  },
  "context": {
    "tenant_id": "tenant_123",
    "run_id": "run_abc123",
    "trace_id": "trace_def456",
    "msg_id": "msg_uuid",
    "validation_stage": "schema"
  },
  "received_at": 1706367600123,
  "router_node_id": "router@node1"
}
```

**DLQ Subject Configuration**:
- Default: `{original_subject}.dlq` (e.g., `beamline.router.v1.decide.dlq`)
- Configurable: `dlq_subject_pattern` (e.g., `beamline.router.v1.intake.dlq` for unified DLQ)
- Can be disabled: `dlq_enabled: false`

## Extensions

### Extension Subjects Pattern

**Pattern**: `beamline.ext.{type}.{extension_id}.{version}`

**Types**:
- `pre` - Pre-processor extensions
- `validate` - Validator extensions
- `post` - Post-processor extensions
- `provider` - Custom provider extensions (uses `beamline.provider.*` pattern)

**Versioning**: Mandatory (`.v1`, `.v2`, etc.)

**Examples**:
- `beamline.ext.pre.normalize_text.v1` - Text normalization pre-processor
- `beamline.ext.validate.pii_guard.v1` - PII validation
- `beamline.ext.post.mask_pii.v1` - PII masking post-processor
- `beamline.provider.my_crm_summarizer.v1` - Custom CRM provider

### Extension Request-Reply Contract

**Type**: Synchronous request-reply (NATS request-reply pattern)

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
- **Pre/Post-processors**: Returns modified `payload` and enriched `metadata`
- **Validators**: Returns `status` field (`"ok"` or `"reject"`)

**Timeout**: Configurable per extension (from Extension Registry, default: 5000ms)

**Retry**: Configurable per extension (from Extension Registry, default: 0)

**See**: `docs/EXTENSIONS_API.md` for complete contract specification

## Providers

### beamline.provider.v1.invoke

- Purpose: synchronous request-reply for invoking a provider
- Request: provider-specific payload (JSON or protobuf)
- Reply: provider-specific response (JSON or protobuf)

### beamline.provider.{provider_id}.{version}

- Purpose: Custom provider extensions (NATS-based providers)
- Pattern: `beamline.provider.{provider_id}.{version}` (e.g., `beamline.provider.my_crm_summarizer.v1`)
- Request: Provider-specific payload (CP2-style, JSON over NATS)
- Reply: Provider-specific response (CP2-style, JSON over NATS)
- **See**: `docs/EXTENSIONS_API.md#5-custom-provider-extensions` for contract details

## Ingress

### beamline.ingress.v1.message

- Purpose: incoming messages to the system
- Payload (JSON):

```json
{
  "tenant_id": "string",
  "message_id": "string",
  "trace_id": "string",
  "type": "chat|completion|embedding|image|audio",
  "payload": {},
  "timestamp": "ISO 8601",
  "metadata": {}
}
```

- Usage: published by external clients or gateways; consumed by Router/Gateway

## Usage/Metering

### beamline.usage.v1.metered

- Purpose: usage events and metering
- Payload (JSON):

```json
{
  "version": "1",
  "tenant_id": "string",
  "provider_id": "string",
  "event_type": "string",
  "latency_ms": 0,
  "cost": 0.0,
  "status": "string",
  "trace_id": "string (optional)",
  "timestamp": 0,
  "assignment_id": "string (optional)",
  "request_id": "string (optional)"
}
```

- Headers: None (standard NATS publish)
- Usage: published by Router/Provider; consumed by Usage service
- Notes:
  - `timestamp` is in milliseconds (integer)
  - At least one of `assignment_id` or `request_id` should be present for correlation

## Alerts

### beamline.alert.v1.critical

- Purpose: critical alerts
- Payload:

```json
{
  "alert_id": "uuid",
  "severity": "critical",
  "component": "string",
  "message": "string",
  "timestamp": "ISO 8601",
  "metadata": {}
}
```

- Usage: published by all components; consumed by Alerting

### beamline.alert.v1.warning

- Purpose: warnings
- Payload: same as `critical`; `severity: "warning"`
- Usage: published by all components; consumed by Alerting

### beamline.alert.v1.info

- Purpose: informational alerts
- Payload: same as `critical`; `severity: "info"`
- Usage: published by all components; consumed by Alerting

## CAF (Execution Assignment)

### caf.exec.assign.v1

- Purpose: Router → CAF execution assignment push
- Payload: JSON `ExecAssignment` (see `docs/API_CONTRACTS.md`)
- Headers (JetStream):
  - `trace_id` (optional): Trace identifier
  - `tenant_id` (optional): Tenant identifier
  - `version`: Message version (default: `"1"`)
  - OpenTelemetry trace context headers (W3C Trace Context format)
- Notes: 
  - Published by Router when `push_assignment: true` in DecideRequest
  - Configurable via `assignment_subject`
  - Uses JetStream publish with acknowledgment for guaranteed delivery
  - Headers include trace context for distributed tracing

### caf.exec.assign.v1.ack

- Purpose: CAF → Router assignment acknowledgment (optional)
- Payload: `ExecAssignmentAck` (protobuf primary, JSON fallback) - see `proto/beamline/worker/v1/ack.proto` and `docs/API_CONTRACTS.md`
- Headers (JetStream):
  - `trace_id` (optional): Trace identifier (priority over payload)
  - `tenant_id` (optional): Tenant identifier (priority over payload)
  - `version` (optional): Message version (priority over payload)
  - `Nats-Msg-Id` (JetStream): Message ID for ACK/NAK operations
- **Consumer**: `router_ack_consumer.erl` (JetStream durable consumer, optional)
  - Durable group: `router-acks` (configurable via `nats_js_durable_group_acks`)
  - Queue group: `router-acks-group` (configurable via `nats_js_deliver_group_acks`)
  - Ack policy: `explicit` (messages ACKed after successful processing)
  - MaxDeliver: 3 (configurable via `nats_js_max_deliver`, default: 3)
  - Backoff schedule: `[1, 2, 4]` seconds (configurable via `nats_js_backoff_seconds`)
  - **CP2+**: Redelivery with exponential backoff; DLQ on MaxDeliver exhaustion
  - Enabled via `ack_enabled` configuration (default: false)
- Notes: 
  - Optional acknowledgment of assignment acceptance/rejection
  - Headers have priority over payload for `trace_id`, `tenant_id`, `version`
  - Uses JetStream durable subscription with explicit ACK policy
  - **CP2+ Redelivery Policy**:
    - Messages are NAKed (negative acknowledged) for retry on tenant validation failures or processing errors
    - Redelivery uses exponential backoff based on delivery count
    - MaxDeliver exhaustion: after N delivery attempts, message is ACKed and sent to DLQ
    - DLQ subject: `{original_subject}.dlq` (e.g., `caf.exec.assign.v1.ack.dlq`)
  - **CP2+ Durability**: Durable consumer survives Router restarts; messages are redelivered after restart
  - **CP2+ Metrics**: `router_jetstream_ack_total`, `router_redelivery_total`, `router_dlq_total`

## Subscription Patterns

### Request-Reply

- `beamline.router.v1.decide`: synchronous request-reply (DecideRequest → DecideResponse)
- `beamline.router.v1.decide.reply`: reply subject for DecideResponse/ErrorResponse
- `beamline.provider.v1.invoke`: synchronous request-reply

### Publish-Subscribe

- `beamline.ingress.v1.message`: publish, multiple subscribers
- `beamline.usage.v1.metered`: publish, Usage subscriber
- `beamline.alert.v1.*`: publish, Alerting subscriber
- `caf.exec.assign.v1`: Router → CAF (push assignment)
- `caf.exec.assign.v1.ack`: CAF → Router (optional acknowledgment)
- `caf.exec.result.v1`: CAF → Router (execution result)
- `caf.worker.heartbeat.v1`: CAF → Router (heartbeat)
- `caf.exec.dlq.v1`: CAF → DLQ (non-retriable)
### caf.exec.result.v1

- Purpose: CAF → Router execution results
- Payload: `ExecResult` (protobuf primary, JSON fallback) - see `proto/beamline/worker/v1/result.proto` and `docs/API_CONTRACTS.md`
- Headers (JetStream):
  - `trace_id` (optional): Trace identifier (priority over payload)
  - `tenant_id` (optional): Tenant identifier (priority over payload)
  - `version` (optional): Message version (priority over payload)
  - `Nats-Msg-Id` (JetStream): Message ID for ACK/NAK operations
  - `traceparent` (optional): W3C Trace Context; used for OpenTelemetry propagation
  - `cp_phase` (optional): CP‑phase label (`cp1|cp2`)
- Payload Fields:
  - `assignment_id` (optional): Assignment correlation ID
  - `request_id` (optional): Request correlation ID (at least one of `assignment_id` or `request_id` required)
  - `status`: `"success"` | `"error"` | `"timeout"` | `"cancelled"`
  - `provider_id`: Provider identifier
  - `job.type`: Job type (e.g., `"chat"`, `"completion"`, `"embedding"`)
  - `latency_ms`: Execution latency in milliseconds
  - `cost`: Execution cost
  - `timestamp`: Timestamp in milliseconds
  - `trace_id` (optional): Trace identifier (fallback if not in headers)
  - `tenant_id` (optional): Tenant identifier (fallback if not in headers)
  - `version` (optional): Message version (fallback if not in headers)
  - `msg_id` (optional): Message ID (fallback if not in headers)
- **Consumer**: `router_result_consumer.erl` (JetStream durable consumer)
  - Durable group: `router-results` (configurable via `nats_js_durable_group_results`)
  - Queue group: `router-results-group` (configurable via `nats_js_deliver_group_results`)
  - Ack policy: `explicit` (messages ACKed after successful processing)
  - MaxDeliver: 10 (configurable via `nats_js_max_deliver`, default: 10 for results)
  - Backoff schedule: `[1, 2, 4]` seconds (configurable via `nats_js_backoff_seconds`)
  - **CP2+**: Redelivery with exponential backoff; DLQ on MaxDeliver exhaustion
- Notes: 
  - Correlated by `assignment_id`/`request_id`
  - Router may emit usage events to `beamline.usage.v1.metered`
  - Usage emission is mandatory unless explicitly disabled via `ENABLE_USAGE_PUBLISH=false`
  - Headers have priority over payload for `trace_id`, `tenant_id`, `version`
  - Uses JetStream durable subscription with explicit ACK policy
  - Tracks delivery count per `msg_id` for MaxDeliver exhaustion detection
  - ACK/NAK operations use `msg_id` from headers or payload
  - **CP2+ Redelivery Policy**:
    - Messages are NAKed (negative acknowledged) for retry on tenant validation failures or processing errors
    - Redelivery uses exponential backoff based on delivery count
    - MaxDeliver exhaustion: after N delivery attempts, message is ACKed and sent to DLQ
    - DLQ subject: `{original_subject}.dlq` (e.g., `caf.exec.result.v1.dlq`)
  - **CP2+ Durability**: Durable consumer survives Router restarts; messages are redelivered after restart
  - **CP2+ Metrics**: `router_jetstream_ack_total`, `router_redelivery_total`, `router_dlq_total`
  - **Runtime Contract Validation**: Router validates headers at runtime. Violations are logged with `contract_violation: true` and metrics are emitted. See `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` for details.

#### Examples
- `traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01`
- `cp_phase: cp2`

#### ENV and Allowlist
- `NATS_SUBJECT_ALLOWLIST`: allowed subjects; rejects others (NAK)
- `NATS_MAX_DELIVER`, `NATS_ACK_WAIT_MS`, `NATS_BACKOFF_MS`: JetStream runtime tuning
- `METRICS_TENANT_LABEL_ALLOWLIST`: limit tenant label cardinality in metrics

### caf.worker.heartbeat.v1

- Purpose: worker heartbeat and capacity reporting
- Payload: JSON `WorkerHeartbeat` (see `docs/API_CONTRACTS.md`)
- Notes: emitted at fixed interval (e.g., 5s); used for liveness checks

### caf.exec.dlq.v1

- Purpose: dead-letter queue for non-retriable assignments
- Payload: JSON `ExecResult` with `status: "error"` and root cause
- Notes: monitored by Ops; triggers alerts and manual replay procedures
