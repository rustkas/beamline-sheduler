# BeamLine Roadmap and Repository Mappings

## Purpose

This document ties together the **BeamLine product roadmap (CP0–CP8)**
with the current `aigroup` monorepo implementation.

It should be read together with:

- `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md` – product vision and
  architecture overview.
- `docs/CP1_*.md`, `docs/CP2_*.md` – checkpoint‑specific reports.

## 1. Roadmap Summary (CP0–CP8)

See `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md`, section
**“9. Roadmap & Checkpoints”** for the canonical high‑level description
of CP0–CP8. This document does not re‑define the roadmap; it explains
how this repository maps onto it.

## 2. Mapping CPs to Current Repository

### 2.1 CP0 – Scaffold Ready

**Goal**: basic workspace, tooling and CI are ready.

**In this repo:**

- Repository structure and architecture described in:
  - `docs/ARCHITECTURE/repository-structure.md`
  - `docs/ARCHITECTURE/context-maps.md`
- Tooling:
  - Proto tooling via `proto/buf.yaml` and scripts:
    - `scripts/check_proto.sh`
    - `scripts/check_proto_sync*.sh`
  - Erlang/OTP, TS, Svelte, CAF projects under `apps/`.
  - DevState and `.trae` integration:
    - `docs/DEVSTATE.md`
    - `.trae/state.json`, `.trae/history.json`.
- CI‑like and local validation:
  - `scripts/run_checks.sh`
  - `scripts/dry_run_ci.sh`
  - Workflows like `/ci-like-build`.

### 2.2 CP1 – ABI v1alpha Frozen (Router/Gateway Focus)

**Global vision**: core ABI messages and services defined in Protobuf,
with lint/breaking checks and codegen.

**In this repo, CP1 is scoped around Router/Gateway and NATS:**

- Core Router/Gateway contracts:
  - `proto/beamline/flow/v1/flow.proto` and related files.
  - `docs/CP1_ROUTER_SPEC.md` – CP1 Router specification.
  - `docs/NATS_SUBJECTS.md` and
    `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` – subject ↔ DTO mapping.
  - `docs/ARCHITECTURE/api-registry.md` – public REST DTO registry.
- CP1 guidance:
  - `docs/archive/dev/CP1_ARCHITECTURE_CHECKLIST.md`.
  - `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`.
  - `docs/CP1_BASELINE.md`, `docs/CP1_ACCEPTANCE_REPORT.md`.
- Proto validation and breaking rules:
  - `scripts/check_proto.sh` and `scripts/check_proto_nats_compatibility.sh`.

In terms of the broader BeamLine roadmap, this corresponds to **CP1
v1alpha ABI**, but currently concentrated on Router/Gateway and NATS
rather than full orchestrator/worker services.

### 2.3 CP2 – Transport Online

**Global vision**: Envoy (mTLS, gRPC/gRPC‑Web) in front of Orchestrator
and Workers; heartbeat flows online.

**In this repo:**

- Gateway application (`apps/gateway`) provides the HTTP/REST surface
  and NATS transport towards Router.
- Router (`apps/otp/router`) implements NATS‑based routing with
  Proto‑validated messages.
- Parts of CP2‑like transport behaviour are implemented via NATS and
  JetStream rather than Envoy/gRPC:
  - JetStream integration, retries and idempotency.
  - Tenant validation and ACLs (CP2+ work for Router).

Envoy, full gRPC‑Web and orchestrator/worker services are **future
extensions** that can reuse the same ABI principles and observability
conventions.

#### CP2-LC: Router (apps/otp/router) — Enhanced Features

**Status**: ✅ **COMPLETE** - All CP2-core features implemented and production-ready.

**CP2-Core Features** (Required for CP2-LC):
1. **JetStream Integration** ✅ - Real NATS/JetStream client with durable subscriptions, ACK/NAK, and redelivery
2. **Idempotency Layer** ✅ - ETS-based idempotency checks with TTL to prevent duplicate processing
3. **OpenTelemetry Tracing** ✅ - Distributed tracing with span creation and trace context propagation
4. **Tenant Validation/ACL** ✅ - Tenant allowlist and policy registry validation with audit events
5. **NAK on Errors** ✅ - Automatic NAK on validation failures with controlled redelivery
6. **Headers Support** ✅ - Headers in assignments and messages (trace_id, tenant_id, version)
7. **JetStream Redelivery** ✅ - Redelivery tracking and metrics with MaxDeliver exhaustion detection

**CP2+ / Optional Features** (Deferred to CP3/Pre-Release):
- Advanced Observability (Grafana dashboards, Prometheus alerting, k6 load tests) - Deferred to Pre-Release
- Proto Source Files Restoration - Deferred to CP2+
- CP2+ Fields in Proto (run_id, flow_id, step_id, idempotency_key, span_id) - Deferred to CP2+

**Key Documents**:
- **CP2 Plan**: `docs/archive/dev/CP2_ROUTER_PLAN.md` - Complete CP2-LC plan with scope and criteria
- **CP2 Specification**: `docs/archive/dev/CP2_ROUTER_GATEWAY_SPEC.md` - Formal CP2 specification
- **CP2 Implementation Report**: `../../../apps/otp/router/docs/archive/dev_reports/CP2_COMPLETE_IMPLEMENTATION_REPORT.md` - Implementation details
- **ADR-011**: `docs/ADR/ADR-011-jetstream-e2e.md` - JetStream E2E architecture decision
- **ADR-012**: `docs/ADR/ADR-012-idempotency-layer.md` - Idempotency layer architecture decision
- **Operational Guide**: `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - CP2-LC operational features
- **Proto Changes Plan**: `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` - Detailed instructions for Proto wire-level changes (CP2+ phase)

**Proto Changes Policy**:
- Proto wire-level changes (restore source files, add CP2+ fields) are executed **only** after `current_cp` transitions to `CP2-LC` in `.trae/state.json`
- See `docs/archive/dev/CP2_ROUTER_PLAN.md` for Proto changes policy and `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` for detailed step-by-step procedures

**Acceptance Criteria**:
- ✅ JetStream E2E tests pass (`router_jetstream_e2e_SUITE.erl`)
- ✅ Idempotency layer prevents duplicate processing under load
- ✅ Tenant validation works with policy registry and audit events
- ✅ Distributed tracing works across main routes
- ✅ NAK on errors works with controlled redelivery and MaxDeliver exhaustion detection
- ✅ All required metrics emitted (redelivery, duplicates, tenant rejections, MaxDeliver exhaustion)
- ✅ Health endpoint works (gRPC health service on port 9000)
- ✅ Structured JSON logging with PII filtering (CP1 baseline maintained)

### 2.4 CP3–CP8 – Future Work in This Context

The following checkpoints are **not yet fully implemented** in this
monorepo, but the current design keeps them achievable.

- **CP3 – Echo step end‑to‑end**
  - Today: Router/Gateway echo scenarios and smoke flows exist through
    NATS.
  - Future: dedicated OrchestratorService and WorkerService with
    `StartRun → ExecuteStep` flows and artefact handling over
    object storage.

- **CP4 – UI connected to stream**
  - Today: Svelte UI under `apps/ui` and `frontend` with smoke routes
    (`docs/UI_ROUTES.md`).
  - Future: richer React/TS UI that consumes
    `StreamExecutionEvents` from the orchestrator, with DAG views and
    live step panels.

- **CP5 – DLQ/Replay**
  - Today: CP2+ Router work already introduces JetStream, idempotency
    and retry mechanics.
  - Future: explicit DLQ buckets, replay APIs and UI controls tied to
    orchestrator state.

- **CP6 – HITL approvals**
  - Future: human‑approval checkpoints and UI inbox using the same
    orchestrator/worker ABI and tracing model.

- **CP7 – FinOps**
  - Future: cost/token accounting built on top of run/step metadata and
    existing observability stack.

- **CP8 – Load & reliability**
  - Today: Router regression and snapshot scripts in `scripts/` and
    `docs/archive/dev/ROUTER_*` reports.
  - Future: unified ghz/k6 scenarios covering the full orchestrator →
    worker → UI path, with SLOs and chaos testing.

## 3. Agents and Responsibilities (Conceptual)

The multi‑agent split from the high‑level BeamLine plan (ABI, Orchestrator,
Workers, Gateway/Security, UI, Observability/FinOps, Data/State,
DevOps, QA) can be mapped to this repository as follows:

- **ABI/Contracts** – owners of `proto/beamline/*`, buf configs and
  `docs/ARCHITECTURE/api-registry.md`, `docs/NATS_SUBJECTS.md`.
- **Router/Orchestrator Core** – owners of `apps/otp/router`,
  `docs/CP1_ROUTER_SPEC.md`, CP1 architecture/checklist docs.
- **Workers/Usage** – owners of `apps/otp/usage` and `apps/caf/*`.
- **Gateway/UI** – owners of `apps/gateway`, `apps/ui`, `frontend` and
  `docs/GATEWAY_ROUTES.md`, `docs/UI_ROUTES.md`.
- **Extensions** – owners of Custom Provider Extensions (separate NATS services),
  `docs/EXTENSIONS_API.md`.
- **Observability & FinOps** – owners of `docs/OBSERVABILITY*.md`,
  Router dashboard docs, future FinOps docs.
- **State & History** – owners of `.trae/*`, `docs/STATE.schema.json`,
  `docs/HISTORY.schema.json`, `docs/DEVSTATE.md`.
- **DevOps & QA** – owners of `infra/`, `docker-compose.yml`, CI
  scripts in `scripts/`, and QA test plans in `docs/archive/dev/*`.

This document is intentionally high‑level and should be updated when new
CPs (e.g. orchestrator gRPC, CAF workers, full UI) are implemented in
this or related repositories.
