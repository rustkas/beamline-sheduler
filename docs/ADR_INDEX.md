---
version: 1.0
authors:
  - Agent 1: Repo/State Bootstrap (CP0-LC)
last_update: 2025-01-27T12:00:00Z
status: approved
---

# ADR Index

## Purpose
Index of Architecture Decision Records (ADRs) for the BeamLine Constructor project.

## ADR Format
Each ADR must follow this format:

```markdown
---
version: 1.0
status: proposed|accepted|deprecated|superseded
date: YYYY-MM-DD
deciders: [list of decision makers]
---

# ADR-XXX: Short decision title

## Context
Description of the problem and decision context.

## Decision
Description of the chosen solution.

## Consequences
Positive and negative consequences.

## Alternatives
Considered alternatives and why they were rejected.
```

## ADR List

### CP0-LC (Repo/State Bootstrap)

Status: All ADRs are being prepared.

- ADR-001: Monorepo structure
  - Status: accepted
  - Date: 2025-11-05
  - File: `docs/adr/ADR-001-monorepo-structure.md` (planned)
  - Decision: Use a monorepo with `apps/`, `proto/`, `sql/`, `docs/`.

- ADR-002: State management via `.trae/state.json` and `.trae/history.json`
  - Status: accepted
  - Date: 2025-11-05
  - File: `docs/adr/ADR-002-state-management.md` (planned)
  - Decision: Use JSON files with an HMAC chain to ensure integrity.

- ADR-003: JSON Schema validation for `state` and `history`
  - Status: accepted
  - Date: 2025-11-05
  - File: `docs/adr/ADR-003-json-schema-validation.md` (planned)
  - Decision: Validate with `docs/STATE.schema.json` and `docs/HISTORY.schema.json`.

### CP1-ROUTER (Router Core)

- ADR-004: Erlang/OTP for Router
  - Status: accepted
  - Date: 2025-11-05
  - File: `docs/ADR/ADR-004-erlang-otp-router.md`
  - Decision: Use Erlang/OTP for Router implementation

- ADR-005: Mnesia Caching
  - Status: accepted
  - Date: 2025-11-05
  - File: `docs/ADR/ADR-005-mnesia-caching.md`
  - Decision: Use Mnesia for caching (mentioned idempotency)

- ADR-006: NATS as the message bus
  - Status: accepted
  - Date: 2025-11-05
  - File: `docs/ADR/ADR-006-nats-inter-service-communication.md`
  - Decision: Use NATS (JetStream) for inter-service communication

- ADR-015: Router ↔ DevState State Management
  - Status: accepted
  - Date: 2025-11-14
  - File: `docs/ADR/ADR-015-router-devstate-integration.md`
  - Decision: Formalize Router read-only consumption of DevState-exported `.trae/*`; DevState as single writer; CP1 fallback on invalid/missing state

### CP2+ Features (Implemented in CP1)

- ADR-011: JetStream E2E with Durable Subscriptions and ACK/NAK
  - Status: accepted
  - Date: 2025-01-27
  - File: `docs/ADR/ADR-011-jetstream-e2e.md`
  - Decision: Use JetStream durable subscriptions with explicit ACK policy and delivery count tracking
  - Related: ADR-006

- ADR-012: Idempotency Layer for Message Processing
  - Status: accepted
  - Date: 2025-01-27
  - File: `docs/ADR/ADR-012-idempotency-layer.md`
  - Decision: Implement idempotency layer using ETS with TTL to prevent duplicate processing
  - Related: ADR-011, ADR-005

- ADR-013: Tenant Validation for Multi-Tenancy
  - Status: accepted
  - Date: 2025-01-27
  - File: `docs/ADR/ADR-013-tenant-validation.md`
  - Decision: Implement tenant validation with allowlist and policy registry checks
  - Related: ADR-011, ADR-012

- ADR-014: Metrics and Distributed Tracing
  - Status: accepted
  - Date: 2025-01-27
  - File: `docs/ADR/ADR-014-metrics-tracing.md`
  - Decision: Use Telemetry for metrics and OpenTelemetry for distributed tracing
  - Related: ADR-004, ADR-010

### CP1-GATEWAY (C-Gateway)

Status: Implemented in CP1-LC

- **ADR-016: C-Gateway Migration from TypeScript to C11**
  - Status: accepted
  - Date: 2025-11-18
  - File: `docs/ADR/ADR-016-c-gateway-migration.md`
  - Decision: Use C11 HTTP Gateway instead of TypeScript/NestJS or C++/CAF
  - Technology: C11 + libnats + jansson
  - Features: HTTP server, NATS integration, JSON DTO parsing, rate limiting, health checks
  - Build modes: stub (default) and real NATS client (`-DUSE_NATS_LIB=ON`)
  - Related: ADR-006, ADR-010

### CP4-INTEGRATION (UI)

Status: ADRs will be created during implementation.

- ADR-009: SvelteKit for the UI
  - Status: proposed
  - Planned: during CP4-INTEGRATION implementation

### CP4-UI (Phoenix LiveView)

Status: Proposed

- **ADR-017: Phoenix LiveView UI Migration**
  - Status: proposed
  - Date: 2025-11-20
  - File: `docs/ADR/ADR-017-phoenix-liveview-ui.md`
  - Decision: Migrate UI from SvelteKit to Phoenix LiveView
  - Technology: Elixir + Phoenix + LiveView
  - Benefits: Unified BEAM stack, simplified architecture, better real-time
  - Migration: 12 days estimated

### Extensions Architecture

- **ADR-023: Remove apps/otp/provider in Favor of Custom Provider Extensions**
  - Status: accepted
  - Date: 2025-11-22
  - File: `docs/ADR/ADR-023-remove-provider-use-extensions.md`
  - Decision: Remove `apps/otp/provider` and use Custom Provider Extensions pattern from Extensions API
  - Rationale: Eliminate duplication, follow pluggable architecture, enable multi-language providers
  - Impact: Provider adapters (OpenAI, Anthropic, etc.) become separate NATS services registered in Extension Registry
  - Related: Extensions API (docs/EXTENSIONS_API.md)

- **ADR-024: Extensions Pipeline Parallelization Strategy**
  - Status: proposed
  - Date: 2025-01-27
  - File: `docs/ADR/ADR-024-extensions-pipeline-parallelization.md`
  - Decision: Proposes two options: (A) Strictly sequential with optimizations, (B) Parallel extension groups with constraints
  - Rationale: Address performance bottleneck (sequential execution) identified in PERF_REPORT
  - Impact: Option A recommended for CP2-LC (optimizations), Option B for CP3+ (parallel execution)
  - Related: EXTENSIONS_PIPELINE_PERF_REPORT.md, EXTENSIONS_API.md

### CP3 (Production Readiness)

- **ADR-026: Post-CP2 Change Policy**
  - Status: accepted
  - Date: 2025-12-21
  - File: `docs/ADR/ADR-026-post-cp2-change-policy.md`
  - Decision: Implement strict change policy (Allowed vs Restricted vs Forbidden) based on CP2-LC Snapshot.
  - Rationale: Prevent definition drift and ensure stability for hardening phase.

---

## Statistics

- Total ADRs: 20
- Accepted: 13
- Proposed: 7
- Deprecated: 0
- Superseded: 0

## ADR Process

1. Identification: determine the need for an ADR
2. Creation: create `docs/adr/ADR-XXX-short-name.md`
3. Discussion: discuss the decision with the team
4. Status: set status (proposed  accepted)
5. Indexing: add a record to this index
6. Updates: update the status when the decision changes

## Links

- ADR Template: `docs/adr/TEMPLATE.md` (planned)
- ADR Guidelines: https://adr.github.io/
- Naming: `ADR-XXX` where XXX is a 3-digit number starting from 001
