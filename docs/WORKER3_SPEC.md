# Worker 3 (CAF/C++) Technical Specification — CP3-LC

## Overview
Worker 3 is the high-performance runtime for executing Flow DSL steps (Blocks) with strong guarantees on atomicity, idempotency, isolation, and observability. It is implemented using C++ with CAF (C++ Actor Framework) and integrates with the Erlang/OTP Orchestrator and the Universal Gateway. The runtime executes provider-agnostic blocks (HTTP, FS, SQL, Media, AI, Human approval) and emits standardized events for routing, audit, and FinOps.

## Goals
- Execute flow steps at scale with ≥500 tasks/s throughput (CP3-LC gate).
- Enforce idempotency, retries, and timeouts per step.
- Provide sandbox execution, resource classification (CPU/GPU/IO), and isolation by tenant.
- Emit structured metrics, logs, and traces with correlation (`trace_id`, `flow_id`, `tenant_id`).
- Verify plugin/block signatures and enforce RBAC/policies.
- Support DLQ/Replay with tamper-evident audit (HMAC chain).

## Scope
- Core actor runtime (CAF) with resource-aware scheduling.
- Block executor interfaces and adapters for common blocks.
- Sandbox mode for dry-run and testing.
- Observability (Prometheus metrics, OTel traces, JSON logs).
- Multi-tenant isolation and quotas.
- Job protocol for Orchestrator ↔ Worker.

## Architecture
- **Actors**: A pool per resource class (`cpu_pool`, `gpu_pool`, `io_pool`). Each pool hosts BlockExecutors for compatible block types.
- **Scheduler**: Assigns step jobs to pools based on `resources` in `StepSpec` (CPU/GPU/IO, memory, concurrency). Enforces `timeout_ms` and `retry` policy.
- **Sandbox**: Executes blocks in constrained environment with mock I/O for `dry_run` and validation.
- **Event Bus**: Publishes job lifecycle events (accepted, started, progress, completed, failed) to NATS subjects defined in `docs/NATS_SUBJECTS.md`.
- **Audit**: Appends immutable entries to audit trail with HMAC chain. Integrates with Orchestrator’s audit.

## Interfaces
- **BlockExecutor**
  - `init(const BlockContext& ctx) -> Status`
  - `execute(const StepRequest& req) -> StepResult`
  - `cancel(const StepCancel& req) -> Status`
  - `metrics() -> BlockMetrics`

- **BlockContext**: `{ tenant_id, trace_id, flow_id, step_id, sandbox: bool, rbac_scopes: string[] }`
- **StepRequest**: `{ type, inputs, resources, timeout_ms, retry, guardrails }`
- **StepResult**: `{ status: ok|error|timeout|canceled, outputs, error?, latency_ms, retries_used }`
- **BlockMetrics**: per-step counters (latency, cpu_time_ms, mem_bytes, success_count, error_count)

## Supported Blocks (Phase 1)
- `http.request`: generic HTTP call with streaming support; TLS/mTLS per policy.
- `fs.blob_put` / `fs.blob_get`: local/remote blob storage adapters.
- `sql.query`: parameterized queries with safe execution and timeouts.
- `human.approval` (stub): emit approval-needed event and await external signal.

## Extended Blocks (Phase 2)
- `media.render`: image/video render or conversion (C module `bl_media`).
- `ai.generate`: model inference via provider adapters (C module `bl_ai`).
- `crypto.sign/hash`: signature verification and hashing (C module `bl_crypto`).
- `http.stream_upload`: efficient I/O via `io_uring` (C module `bl_http`).

## Security & Governance
- **RBAC & Policies**: Validate access to blocks and secrets before execution.
- **Signature Verification**: Verify `BlockManifest` signatures before loading plugins.
- **Tenant Isolation**: Per-tenant ETS-like cache (C++ map) and quotas; resource isolation.
- **Idempotency**: Step deduplication via `(tenant_id, flow_id, step_id, attempt)` key.
- **DLQ/Replay**: On hard failure, publish to DLQ subject; support replay with guardrails.

## Observability
- **Metrics (Prometheus)**: `worker_tasks_total`, `worker_task_latency_ms`, `worker_task_failures_total`, `worker_pool_queue_depth`, `worker_resource_cpu_time_ms`, `worker_resource_mem_bytes`.
- **Tracing (OpenTelemetry)**: Span per step execution, propagated headers; attributes: `tenant_id`, `flow_id`, `step_id`, `block_type`.
- **Logs (JSON)**: Structured logs at INFO/WARN/ERROR correlated with `trace_id`.

## Protocol & Integration
- **Job Submission**: Orchestrator sends `StepRequest` via gRPC/NATS; Worker ACKs and schedules.
- **Events**: Worker publishes lifecycle events to `beamline.worker.events.<tenant_id>`.
- **Results**: Return `StepResult` with outputs and metrics. For streaming, publish progress chunks.
- **Subjects**: See `docs/NATS_SUBJECTS.md` for unified naming.

## Performance Targets
- Throughput ≥ 500 tasks/s (CP3-LC gate).
- p95 latency budget:
  - `http.request` ≤ 500 ms internal (excluding external server latency)
  - `fs.blob_get/put` ≤ 200 ms on local dev
  - `sql.query` ≤ 300 ms for simple selects
- Queueing under high load: bounded by `worker_pool_queue_depth` with backpressure to Orchestrator.

## Testing & QA
- Unit tests for each BlockExecutor (success, timeout, retry, error).
- Integration tests for Orchestrator ↔ Worker job protocol.
- Sandbox tests (dry-run consistency with Gateway `flows/dry-run`).
- Benchmarks to validate throughput/latency targets.
- Chaos tests: actor crash, resource starvation, and recovery.

## Deliverables (CP3-LC)
- `apps/caf/processor/` Worker runtime with actor pools and scheduler.
- `BlockExecutor` interface and implementations for Phase 1 blocks.
- C modules stubs: `bl_media`, `bl_ai`, `bl_crypto`, `bl_http` (Phase 2 planned).
- Observability: metrics exported, traces emitted, structured logs.
- Documentation: runtime design, integration, and configuration.

## Acceptance Criteria (CP3-LC)
- All Phase 1 blocks execute with retries, timeouts, and idempotency.
- Metrics and traces visible; logs structured and correlated.
- Multi-tenant isolation verified with quotas enforced.
- DLQ/Replay functioning for failure scenarios.
- Throughput ≥ 500 tasks/s with test report.

## Dependencies
- Orchestrator job protocol (ABI flow/provider v1).
- Universal Gateway specification `docs/GATEWAY_UNIVERSAL_SPEC.md` for DTO alignment.
- NATS subjects registry `docs/NATS_SUBJECTS.md`.

## Migration & Versioning
- All new fields optional; default-safe values.
- Version tags for block implementations; semantic versioning for plugins.
- Rolling updates with canary% support and rollback.

## Risks & Mitigations
- External service latency variance → circuit breakers and timeouts.
- Resource contention → dynamic pool sizing and backpressure.
- Plugin security → signature verification and sandboxing.

## Roadmap
- Phase 1 (CP3-LC): core runtime, Phase 1 blocks, observability, DLQ/Replay.
- Phase 2 (CP4+): advanced C modules, streaming I/O, AI adapters, media pipeline.

