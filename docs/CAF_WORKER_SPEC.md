# Worker Runtime Specification (CP1-LC Draft)

Describes the Rust-based Worker subsystem responsible for executing heavy tasks (media, AI inference, crypto, HTTP streaming) in coordination with the Erlang Router via NATS. This draft aligns with CP1-LC and extends toward CP3-LC.

## Goals
- High-performance execution outside Erlang VM with predictable resource control.
- Loosely coupled integration over NATS using JSON contracts (proto planned).
- Robust observability: metrics, tracing, structured logs.
- Idempotent, auditable assignment processing.

## Architecture
- Components:
  - `ingress`: NATS consumer for assignments; publishes ACK/Result; heartbeats.
  - `processor`: BlockExecutor pool (CPU/GPU/IO typed) with adapters: `bl_media`, `bl_http`, `bl_crypto` (stubs initially).
- Integration:
  - Router publishes `ExecAssignment` to `caf.exec.assign.v1` when `push_assignment` is enabled.
  - Worker validates, acknowledges (`caf.exec.assign.v1.ack`), executes, and publishes `ExecResult`.

## Contracts
- Source: `docs/API_CONTRACTS.md`.
- Required:
  - `ExecAssignment.version == "1"`.
  - `assignment_id`, `request_id`, `tenant_id` non-empty strings.
  - `executor.provider_id` and `job.type` supported.
- Outputs:
  - `ExecResult` with `status: success|error|timeout|canceled`, payload or error.
  - `ExecAssignmentAck` with `status: accepted|rejected` and reason.

## NATS Subjects
- Input:
  - `caf.exec.assign.v1`: Router → Worker assignments.
- Output:
  - `caf.exec.assign.v1.ack`: Worker → Router ACK.
  - `caf.exec.result.v1`: Worker → Router execution results.
  - `caf.worker.heartbeat.v1`: Worker → Router heartbeat.
  - `caf.exec.dlq.v1`: Worker → DLQ for non-retriable failures.

## Processing Flow
1. Subscribe to `caf.exec.assign.v1` with durable queue group (JetStream recommended).
2. Parse and validate `ExecAssignment`.
3. Idempotency check on `assignment_id` (TTL cache or persistent store).
4. Publish ACK: `accepted` or `rejected` with `reason`.
5. Route job by `job.type` to appropriate BlockExecutor.
6. Execute with deadline/retry per `options`.
7. Publish `ExecResult` with metrics and correlation.
8. On irrecoverable errors, publish to `caf.exec.dlq.v1` and emit alert.

## Observability
- Metrics (Prometheus):
  - Execution counters and latencies per job type
  - Resource usage metrics (CPU/memory)
  - Heartbeat counters
- Tracing (OpenTelemetry):
  - Propagate `trace_id` from assignment; create spans for consume, execute, publish.
- Logs: JSON with `trace_id`, `assignment_id`, `tenant_id`, `status`.

## Security
- NATS: user-level ACLs; enforce `tenant_id` authorization.
- Validation: reject unknown `job.type` or unsupported executor capability.
- Secrets: payload refs resolved via preconfigured credentials (not embedded in messages).

## Failure Handling
- Retries: exponential backoff from `options.retry`.
- Timeouts: enforce `options.deadline_ms`.
- DLQ: publish to `caf.exec.dlq.v1` with root cause.
- Idempotency: ignore duplicate `assignment_id`.

## Versioning and Evolution
- Start with JSON; add protobuf in `proto/beamline/ingress/v1` as contracts mature.
- Extend `job.type` namespace: `text.generate`, `media.render`, `crypto.sign`, `http.stream`.
- Support canary via `options.priority` and Router policies.

## Acceptance Criteria (CP1-LC)
- Worker stubs compile; NATS subjects documented; contracts defined.
- Basic ingestion and ACK publication with validation and idempotency cache.
- Metrics and heartbeat counters exposed.
- Tests: mock NATS integration covering happy path and rejections.

## Risks and Mitigations
- Tight coupling risk: mitigated with NATS and JSON contracts.
- Performance variance: bound via executors and resource-type pools.
- Security lapses: ACLs, validation, secrets isolation.

## Next Steps
- Implement minimal NATS consumer in `apps/worker/ingress` and executor stubs in `apps/worker/processor`.
- Add protobuf for `ExecAssignment`/`ExecResult` under `proto/beamline/ingress/v1`.
