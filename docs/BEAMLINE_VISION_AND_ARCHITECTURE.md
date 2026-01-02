---
# BeamLine Vision and Architecture Overview

> **Snapshot**: This document is part of the [CP2-LC Architecture Snapshot](./ARCHITECTURE_SNAPSHOT_CP2_LC.md).

> **IMPORTANT**: DevState is a SEPARATE, SIDE PRODUCT that is NOT related to the goal of the BeamLine Constructor project. DevState serves only for development state management and is not part of the AI factory or AI agent orchestration.

## Purpose

This document describes what BeamLine is **as a product** and how the current
CP1/CP2 architecture in this repository maps to that vision.
It combines:
- The **target concept** of BeamLine as an "AI Production Line" / AI orchestrator.
- The **current implementation**: Gateway, Router, Providers, NATS, and docs (DevState is excluded as it's a side product for development state management only).

The goal is to be a single entry point for understanding **what we are building**
and **how the pieces fit together**.

## How to Use This Document

This document is meant to be a **high-level map**, not a detailed spec.

Use it as:
- **Product context** – to explain what BeamLine is and why it exists.
- **Architecture overview** – to see how Gateway, Router, Providers,
  NATS, Proto fit together, and how external tooling (TRAE/DevState for
  managing development state) integrates into the development process.
- **Onboarding guide** – for new contributors to understand the big
  picture before diving into ADRs or code.
- **Reference during PRs** – to check that new changes align with the
  overall BeamLine vision and CP1/CP2 architecture.

When you need concrete details (contracts, routes, invariants), follow
the links in this document to dedicated specs (API registry, NATS
subjects, routing policy, observability, etc.).

## 1. What BeamLine Is (Product Vision)

BeamLine is **not** a classic BPMN editor.
It is an **operating system for AI factories**:

- **Not diagrams, but live execution.**
  Processes are executed by *live executors* (AI agents, APIs, humans),
  not just modeled as static BPMN graphs.
  The system drives data and event flows between these executors.

- **AI Production Line.**
  Think of it as an "AI Production Line" where content, code, decisions and
  communications move along lines like products on a factory conveyor.

- **Orchestrator for agents and content factories.**
  BeamLine unifies a **workflow engine**, a **multi‑agent system** and an
  **event‑driven core** into one platform.

- **From BPMN to living runs.**
  BPMN (or any process model) is a *description*.
  BeamLine turns this description into **living runs** executed by:
  - AI modules and LLM‑based agents,
  - external and internal APIs,
  - human participants,
  - compute‑heavy workers.

Short description:

> BeamLine is an AI orchestrator for the age of automation —
> an operating system for AI factories where processes are
> living flows of interacting agents, not static diagrams.

## 2. High‑Level Architecture

At a high level we split the system into two main concerns:

- **Orchestration & control plane** – built on **Erlang/OTP** + NATS.
- **Execution & heavy compute** – built on **CAF/C++** and other
  language‑specific workers.

Communication between orchestration and workers is normalized through
**gRPC + Protobuf** contracts (ABI layer) and **NATS subjects** for
messaging inside the platform.

### 2.1 Current core components

The monorepo already implements a large part of this architecture:

- **Gateway (C11)**
  - REST/HTTP entry point for external clients (high-performance C11 implementation).
  - Validates public DTOs against
    `docs/ARCHITECTURE/api-registry.md`.
  - Translates HTTP requests into NATS messages using Proto contracts
    and versioned NATS subjects.
  - Exposes `/ _health` and structured JSON logs.

- **Router (Erlang/OTP)**
  - Subscribes to versioned NATS subjects (see `docs/NATS_SUBJECTS.md`
    and `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`).
  - Routes messages to appropriate providers/services.
  - Implements validation, retries with backoff, and idempotency logic.
  - **CP2 Features Implemented**: JetStream, tenant validation/ACL, full tracing, circuit breakers.

- **Providers (Erlang/OTP)**
  - Domain‑specific business logic services (e.g. usage, OTP provider).
  - Consume messages from their NATS subjects and produce responses
    or events.
  - In CP1 they can be thin stubs; in CP2+ they evolve into rich
    domain services.

- **UI / Frontend (Svelte/SPA/SSR)**
  - Simple UI for smoke testing and demonstrating Gateway behaviour.
  - Routes are documented in `docs/UI_ROUTES.md`.

- **CAF / worker specifications**
  - `docs/CAF_WORKER_SPEC.md` and `docs/WORKER3_SPEC.md` describe
    how C++/CAF workers integrate as high‑performance execution nodes.

- **State and governance (Erlang/OTP implementation)**
  - **Runtime state**: ETS/Mnesia tables for high-performance operations (policy cache, sticky sessions, rate counters, idempotency)
  - **Persistent state**: PostgreSQL tables for reliable storage (projects, api_keys, policies, audit_logs)
  - **Audit trails**: Erlang-managed immutable audit logs with tenant isolation and retention policies
  - **Event sourcing**: NATS message flows for state change propagation
  - **Checkpoint system**: CP-based feature gating with graceful fallbacks to CP1-baseline mode
  - **Note**: Development state management and HMAC-chain audit are TRAE IDE tooling only, NOT part of product architecture

- **Observability**
  - `docs/OBSERVABILITY*.md` define JSON logging, health endpoints and
    tracing conventions.
  - CP1 requires JSON logs and `/ _health`; CP2+ adds full OTEL and
    richer metrics.

Together these components already form a **CP1‑grade backbone** of the
BeamLine orchestrator.

## 3. BeamLine as an AI Orchestrator

### 3.1 Conceptual model

From the product point of view, BeamLine coordinates **runs** of
workflows where each step is executed by one of:

- an AI agent or model,
- an external/internal API,
- a human participant,
- a compute‑heavy worker.

The system:

- Maintains the **state of the run** (which steps completed, which are
  waiting or failed).
- Routes **events and data** between steps through NATS and workers.
- Enforces **idempotency, retries and error handling** on the
  orchestration level.
- Records **structured logs and traces** for each run and step.

### 3.2 BeamLine vs classic BPMN systems

Compared to a traditional BPMN automation tool, BeamLine:

- Focuses on **runtime orchestration**, not diagram editing.
- Treats process models as **inputs** to generate and execute runs,
  not as the primary UI artifact.
- Is natively **event‑driven and agent‑centric**:
  agents, models and workers are first‑class citizens of the platform.

## 4. ABI and Contracts Layer

To connect Erlang/OTP orchestration with CAF/C++ workers and other
services, we use a **single ABI package** based on gRPC + Protobuf.

### 4.1 ABI package

- Mono‑repo package `beamline/abi` (conceptual; actual layout is under
  `proto/beamline` and related folders).
- Versioning strategy: `v1alpha` → `v1`, controlled through
  `proto/buf.yaml` and the `scripts/check_proto*.sh` tooling.

Two key service families (naming may vary by concrete .proto):

- **Orchestrator service** (Erlang/OTP side)
  - Start and control runs.
  - Query statuses.
  - Stream events (run‑level and step‑level).

- **Worker service** (CAF/C++ and other executors)
  - Execute a step via unary or bidirectional streaming RPC.
  - Send heartbeat and health information.
  - Advertise capabilities (what step types the worker can handle).

### 4.2 Message design

Every message in this ABI is expected to include at least:

- `run_id` – unique identifier of the run.
- `flow_id` – identifier of the process/flow definition.
- `step_id` – identifier of the particular step.
- `idempotency_key` – key for safe retries.
- `trace_id` (and optionally `span_id`) – for distributed tracing.

Large artefacts (files, datasets, media) are **not** sent directly
through gRPC payloads. Instead we pass **`blob_uri`** references
(S3/GCS/other storage), which workers fetch independently.

### 4.3 Idempotency and retries

- For unary RPCs, servers must treat `idempotency_key` as a contract:
  repeated calls with the same key must return the same logical result.
- Client‑side automatic retries are allowed only for safe, transient
  failures, such as:
  - `UNAVAILABLE`,
  - `DEADLINE_EXCEEDED`,
  - `RESOURCE_EXHAUSTED`.

### 4.4 Streaming and backpressure

For live, streaming steps (ASR, media processing, CV pipelines), the
ABI is designed around **bidirectional streaming** with explicit
backpressure:

- The client announces credit (e.g. `max_inflight`) in metadata or
  initial message.
- Workers periodically send ack/nack or water‑level information to
  signal queue pressure.
- Orchestrator adapts the sending rate based on these signals.

### 4.5 Error model

Errors are represented via `google.rpc.Status` with rich
`error_details`:

- Business validation → `INVALID_ARGUMENT`.
- Limits/quotas → `RESOURCE_EXHAUSTED`.
- Temporary unavailability → `UNAVAILABLE`.
- Step timeout → `DEADLINE_EXCEEDED`.
- Uncaught/fatal errors → `INTERNAL`.

The repository already enforces strict Proto compatibility and
breaking‑change rules via `buf` and scripts such as `scripts/check_proto.sh`.

## 5. Security Model

BeamLine is designed with a clear separation between external clients
and internal services.

- **Service‑to‑service security**
  - mTLS between internal services (e.g. Gateway, Router, Providers,
    workers), backed by SPIFFE/SPIRE or an equivalent certificate
    management system.

- **Client authentication**
  - External clients (UI, integrations) authenticate via JWT passed in
    gRPC/HTTP metadata (e.g. `Authorization` header).

- **Authorization**
  - Erlang‑based gateways/routers enforce authorization in terms of
    `tenant`, `project`, `run` scopes.

The existing `docs/SECURITY_GUIDE.md` and `docs/ENVIRONMENT.md` describe
secret handling, HMAC rules, and leak‑prevention tooling that apply to
BeamLine as well.

## 6. Observability

Observability is a first‑class concern of the BeamLine architecture.

- **Tracing**
  - Every ABI message carries `trace_id` (and optionally `span_id`).
  - IDs are propagated through gRPC metadata (e.g. `traceparent`).
  - Erlang/OTP and C++ components integrate with OpenTelemetry, export
    traces via OTLP.

- **Logging**
  - All services use **structured JSON logs** with key fields mirrored
    from requests (run/flow/step IDs, tenant, subject, etc.).
  - Logs must not contain secrets or PII; sensitive fields are masked
    as `[REDACTED]` / `[MASKED]`.

- **Metrics and health**
  - CP1 requires `/ _health` endpoints and basic metrics for Router and
    Gateway.
  - CP2+ adds richer Prometheus‑style metrics and alerting (see
    `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`).

## 7. NATS and Internal Messaging

NATS is the backbone for internal event and command routing.

- Subjects are **versioned**, e.g. `beamline.usage.v1.*`.
- `docs/NATS_SUBJECTS.md` and
  `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` document which Proto
  messages map to which subjects.
- The Router enforces compatibility and routing rules in line with
  these documents.

This makes NATS subjects and Proto DTOs a stable contract between
Gateway, Router, Providers and workers.

## 8. Checkpoints and Project Lifecycle

BeamLine development in this repository is managed through **checkpoints**:

- **Checkpoint tracking**: Current checkpoint status (e.g. CP1‑LC) and progress tracking through Erlang/OTP state management
- **Transition history**: State transition history managed through Erlang audit system (separate from TRAE development tooling)
- `docs/CP1_*.md`, `docs/CP2_*.md` and related reports describe what is
  required and what has been implemented for each checkpoint.

This aligns the concrete, incremental work (Router, Gateway, Provider,
observability, docs) with the long‑term BeamLine vision described in
this document.

## 9. Roadmap & Checkpoints

The broader BeamLine product vision is organised around a series of
checkpoints (CP0–CP8) that gradually evolve the system from scaffolding
to a full AI factory platform. This repository currently focuses on a
subset of that roadmap (primarily CP1/CP2 around Router/Gateway and
messaging), but the long‑term direction is larger.

### 9.1 High‑Level BeamLine Roadmap (CP0–CP8)

- **CP0 – Scaffold Ready**
  - Repositories and folders in place.
  - Tooling configured (buf, rebar3, CMake, frontend toolchain).
  - Pre‑commit hooks and CI lint/build pipelines passing.

- **CP1 – ABI v1alpha Frozen**
  - Core `.proto` definitions for orchestrator/worker/router contracts.
  - Buf lint and breaking‑change checks in place.
  - Code generation for Erlang, C++ and TypeScript established.

- **CP2 – Transport Online**
  - Envoy (or equivalent) providing mTLS, gRPC and gRPC‑Web fronting.
  - Orchestrator gRPC server responds to basic requests (e.g. `ping`).
  - Worker gRPC server starts and sends heartbeats.

- **CP3 – Production Readiness & Hardening**
  - Focus on stability, reliability, and operations (CI/CD, Monitoring).
  - Full end-to-end tracing and backpressure implementation.
  - Verification of `StartRun → ExecuteStep` path under load (Soak/Chaos).
  - Infrastructure hardening (Ingress/Gateway/K8s).
  - (Previously "Echo Step End-to-End" - functional verification is part of hardening).

- **CP4 – UI Connected to Stream & HTTP Gateway**
  - Run inspector UI consumes `StreamExecutionEvents` in real time.
  - Basic operator actions: retry and replay from checkpoint.

- **CP5 – DLQ/Replay**
  - Failures move steps into DLQ with reason and retry policy.
  - UI supports `Rehydrate & Replay` flows.
  - Idempotency guarantees validated.

- **CP6 – HITL Approvals**
  - Human‑in‑the‑loop checkpoints (`human_approval`).
  - Approval inbox, artefact diff view, approve/request‑changes flows.

- **CP7 – FinOps**
  - Token/cost accounting per step/run/project.
  - Budget alerts and cost dashboards.

- **CP8 – Load & Reliability**
  - Load tests (ghz/k6) with latency and throughput targets.
  - Fault‑injection scenarios (worker crashes, network issues) with
    DLQ/Replay and idempotency validated under stress.

Across all checkpoints:

- Contracts live in Protobuf and are the single source of truth.
- All RPCs/events include `run_id`, `flow_id`, `step_id`,
  `idempotency_key`, `trace_id`.
- Idempotency is enforced server‑side; clients only retry on specific
  transient gRPC status codes.
- Observability (traces, metrics, logs) and security (mTLS, JWT/OIDC)
  are treated as first‑class concerns.

### 9.2 How This Repository Fits into the Roadmap

This `aigroup` monorepo focuses on the **messaging and routing core**
for BeamLine and currently aligns most strongly with:

- **CP0/CP1** in the context of Router/Gateway and NATS:
  - Repository structure and basic tooling; **State management is implemented through Erlang/OTP mechanisms** (ETS/Mnesia for runtime state, PostgreSQL for persistence, NATS for event sourcing) completely separate from TRAE development tooling.
  - Proto/NATS contracts for Router/Gateway and CP1 isolation rules.
  - CP1 acceptance criteria and reports in `docs/CP1_*.md`.

- Pieces of **CP2/CP3** at the transport and reliability level:
  - JetStream integration, idempotency and retries in Router.
  - Structured observability and tracing foundations.

-- Planning for **CP4‑INTEGRATION (Gateway & UI)**:
orchestrator/worker gRPC ABI, rich React GUI, full
FinOps and load‑testing stacks are part of the **future evolution** of
BeamLine and can be implemented either in this monorepo or in a closely
aligned workspace using the same principles.

When introducing new features or architectural changes, they should be
checked against both:

- The **local CP1/CP2 contracts and docs** in this repository.
- The **global BeamLine roadmap** outlined above and in dedicated
  dev‑level design documents.

#### 9.2.x CP2 and ABI bridge details / Детали CP2 и ABI bridge

For a detailed, implementation-level view of CP2 for the Router and the Gateway, see:

- [docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md](archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md) — CP2 readiness and feature inventory for the Router/Gateway.
- `docs/archive/dev/ORCHESTRATOR_ROUTER_ABI_BRIDGE_UPDATED.md` — ABI evolution bridge between the Orchestrator and the Router.

For a detailed, practical view of CP2 for Router and Gateway, see:

- [docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md](archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md) — обзор готовности CP2 и перечень фич для Router/Gateway.
- `docs/archive/dev/ORCHESTRATOR_ROUTER_ABI_BRIDGE_UPDATED.md` — description of ABI evolution between Orchestrator and Router.

## 10. Summary

- BeamLine is **not** a BPMN diagram editor, but an **AI orchestration
  platform** where workflows become living runs of agents, APIs,
  humans and workers.
- The current monorepo already contains a strong CP1/CP2 backbone for
  this vision: Gateway, Router, Providers, NATS, Proto contracts and
  observability; **State and history management is implemented through Erlang/OTP components** (ETS tables, PostgreSQL audit logs, NATS event streams) as part of the product architecture, completely separate from TRAE/DevState development tooling.
- The ABI layer based on gRPC + Protobuf, combined with Erlang/OTP and
  CAF/C++, provides a scalable and fault‑tolerant foundation for
  AI factories.

Future work will refine this document with links to concrete ADRs and
implementation details as new checkpoints are reached.
