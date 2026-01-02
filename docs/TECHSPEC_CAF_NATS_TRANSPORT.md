# TECHSPEC: Worker NATS/JetStream Transport (assign/result/heartbeat)

> **Snapshot**: This document is part of the [CP2-LC Architecture Snapshot](./ARCHITECTURE_SNAPSHOT_CP2_LC.md).

## 0. Goal

Сделать `apps/worker` полноценным участником шины NATS/JetStream:

- **consume** assignments из `caf.exec.assign.v1`
- **publish** results в `caf.exec.result.v1`
- **publish** heartbeat в `caf.worker.heartbeat.v1`

Цель: включить реальный end-to-end контур `router → worker → router`.

---

## 1. Context / Current State (AS-IS)

### 1.1 Router side is ready

Router уже содержит:

- Publisher assignments: `apps/otp/router/src/router_caf_adapter.erl` публикует `ExecAssignment` в `caf.exec.assign.v1` через `router_nats:publish_with_ack/3`.
- Consumer results: `apps/otp/router/src/router_result_consumer.erl` подписан на `caf.exec.result.v1`.

### 1.2 Worker side lacks transport

В `apps/worker` есть runtime, executors и конфиг `WorkerConfig.nats_url`, но **нет** кода:

- subscribe to `caf.exec.assign.v1`
- publish to `caf.exec.result.v1`
- worker heartbeat

---

## 2. Requirements

### 2.1 Functional

- **R1**: Worker MUST subscribe to `caf.exec.assign.v1`.
- **R2**: On incoming assignment Worker MUST parse payload into internal execution request.
- **R3**: Worker MUST execute job and publish result to `caf.exec.result.v1`.
- **R4**: Worker MUST periodically publish heartbeat to `caf.worker.heartbeat.v1`.
- **R5**: Worker MUST propagate correlation fields (`request_id`, `assignment_id`, `trace_id`, `tenant_id`, `run_id/flow_id/step_id` where available).
- **R6**: Worker MUST handle invalid messages safely (DLQ or error result), without crashing.

### 2.2 Reliability / Delivery

- **R7**: Assignments consumption MUST be at-least-once (JetStream recommended). Worker must ACK after successful processing.
- **R8**: For retryable failures worker SHOULD NAK with backoff; for non-retryable failures worker SHOULD ACK + publish error result.
- **R9**: Result publishing SHOULD be durable (JetStream publish ack) if stream is configured.

### 2.3 Observability

- **R10**: Structured logs on consume/publish (assignment_id, request_id, tenant_id, trace_id).
- **R11**: Metrics: processed_total, success_total, failure_total, latency_ms histograms, retries_total.
- **R12**: Trace: if trace context is present in assignment headers/payload, continue trace.

---

## 3. NATS subjects (contracts)

- `caf.exec.assign.v1` — ExecAssignment
- `caf.exec.result.v1` — ExecResult
- `caf.worker.heartbeat.v1` — WorkerHeartbeat

Canonical description: `docs/NATS_SUBJECTS.md`.

---

## 4. Message contracts

### 4.1 ExecAssignment (input)

Minimum required fields (as consumed by CAF):

- `version`
- `assignment_id`
- `request_id`
- `tenant_id`
- `job` (type + payload)

Optional:

- `trace_id`, `run_id`, `flow_id`, `step_id`
- resource hints (cpu/gpu/io)

### 4.2 ExecResult (output)

Minimum required fields:

- `version`
- `assignment_id`
- `request_id`
- `status` in {success,error,timeout,cancelled}
- `provider_id` (or executor_id)
- `job` (echo type)
- `latency_ms`
- `cost` (может быть 0.0 на раннем этапе)

Optional:

- `trace_id`, `tenant_id`, `run_id`
- `error_code`, `error_message`

Note: в CAF уже есть helper: `apps/caf/processor/include/beamline/worker/result_converter.hpp`.

---

## 5. Implementation Plan (proposed)

### 5.1 Add transport module

Добавить слой транспорта (варианты):

- **Option A (recommended)**: отдельный компонент `worker_nats_transport` (Rust), который:
  - держит NATS connection
  - подписывается на JetStream consumer для `caf.exec.assign.v1`
  - публикует results/heartbeat

Transport должен быть независим от Worker runtime: transport принимает assignment → преобразует в `StepRequest`/`BlockContext` → отправляет в Worker runtime.

### 5.2 JetStream consumer setup

- Создать durable consumer group (например `caf-worker-<id>` или `caf-worker-group`).
- AckPolicy: explicit.
- MaxDeliver + backoff: configurable.

### 5.3 Worker identity

- Ввести `worker_id` (env или генерить по hostname/pid).
- Heartbeat payload должен включать `worker_id`.

### 5.4 Config / env

- `NATS_URL` (уже есть как `WorkerConfig.nats_url`)
- `CAF_ASSIGN_SUBJECT` default `caf.exec.assign.v1`
- `CAF_RESULT_SUBJECT` default `caf.exec.result.v1`
- `CAF_HEARTBEAT_SUBJECT` default `caf.worker.heartbeat.v1`
- `CAF_HEARTBEAT_INTERVAL_MS` default e.g. 5000

---

## 6. Acceptance Criteria

- **AC1**: При публикации Router assignment в `caf.exec.assign.v1` CAF worker получает сообщение и логирует receipt.
- **AC2**: CAF публикует валидный `ExecResult` в `caf.exec.result.v1`, Router его принимает.
- **AC3**: Heartbeat идёт минимум раз в `CAF_HEARTBEAT_INTERVAL_MS`.
- **AC4**: На невалидный assignment CAF не падает, публикует `ExecResult` со статусом `error` либо пишет в DLQ (если DLQ подключён).

---

## 7. Open Questions

- Q1: Используем ли JetStream streams для `caf.exec.*` или достаточно core NATS pub/sub?
- Q2: Где хранить retry/backoff policy (CAF или Router)?
- Q3: Формат trace context: headers vs payload?
