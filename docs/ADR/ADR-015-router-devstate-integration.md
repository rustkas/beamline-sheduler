---
version: 1.0
status: accepted
date: 2025-11-14
deciders:
  - System Orchestrator
related_adrs:
  - ADR-002: State management
  - ADR-004: Erlang/OTP router
  - ADR-006: NATS inter-service communication
---

# ADR-015: Router ↔ DevState State Management

## Context
BeamLine uses DevState as the Source-of-Truth for `.trae/state.json` and `.trae/history.json` to enforce No-Drift and HMAC-audited history. The Erlang/OTP Router must respect these invariants and consume state in a read-only fashion.

## Decision
- Router NEVER writes to `.trae/state.json` or `.trae/history.json`.
- DevState is the ONLY entity that updates state and appends history (HMAC chain), via HTTP/CLI with locks and verification.
- Router reads ONLY DevState-exported state (`.trae/state.json`) and gates features by `current_cp` (CP1 baseline, CP2+ optional flags).
- If state is missing/invalid, Router falls back to CP1 baseline with CP2+ features disabled.

## Router Expectations (from `docs/STATE.schema.json`)
- `current_cp`: string matching `CP\d+-[A-Z]+` → determines enabled components/features.
- `no_drift`: boolean → must be `true` for CP transitions to proceed.
- `agents[]` (optional): may be used to inspect Router agent status.

## CP vs Features (code-level gates)
- CP1 baseline (always on): `router_telemetry_handler`, `router_nats`, `router_rbac` (if `rbac_enabled`), `router_rate_limiter`, `router_grpc_sup`, `router_nats_subscriber`, `router_result_consumer`.
- CP2+ gated features:
  - `ack_enabled` → starts `router_ack_consumer` (`beamline_router_sup.erl:66–73`).
  - `idempotency_enabled` → starts `router_idempotency` (`beamline_router_sup.erl:75–84`).
  - `admin_grpc_enabled` → enables RouterAdmin (`router_grpc_sup.erl:115`).
  - `tracing_enabled` → tracing code paths in consumers/adapters (`router_result_consumer.erl:311`, `router_caf_adapter.erl:183`).
  - `nats_js_*` → JetStream tuning (`router_nats.erl:488–490`).
  - `disable_heir` → policy store heir behavior (`router_policy_store.erl:43`).

## CP1 Minimal Mode Enforcement

**CRITICAL**: Router enforces CP1 minimal mode by gating CP2+ features with `current_cp` from DevState.

**Behavior**:
- Even if `application:get_env` flags are set (e.g., `ack_enabled=true`, `idempotency_enabled=true`), CP2+ features will **NOT** start unless `current_cp >= CP2-LC`.
- This prevents accidental activation of CP2+ features in CP1 environments.
- Implementation: `router_state:is_cp2_plus_allowed()` checks `current_cp` and returns `true` only if `current_cp >= CP2-LC`.
- If `state.json` is missing or invalid, Router falls back to CP1 baseline (CP2+ features disabled).

**Gating Logic**:
- `beamline_router_sup.erl`: Checks `router_state:is_cp2_plus_allowed()` before starting `router_ack_consumer` or `router_idempotency`.
- `router_grpc_sup.erl`: Checks `router_state:is_cp2_plus_allowed()` before enabling RouterAdmin service.
- All CP2+ feature flags are effectively: `EnvFlag andalso is_cp2_plus_allowed()`.

**Testing**:
- `router_cp1_minimal_mode_SUITE.erl`: Verifies that CP2+ features do NOT start when `current_cp=CP1-LC`, even with env flags enabled.

## Alignment with CP Docs
- See `docs/CP1_BASELINE.md` for CP1 mandatory components and feature flags.
- See `docs/CP2_PROVIDER_PREPARATION.md` for CP2+ features and their gates.

All CP2+ components are activated strictly via documented `application:get_env` gates **AND** `current_cp >= CP2-LC` from DevState; no implicit activation.

## Workflow
1. Change CP via DevState HTTP/CLI (`POST /v1/devstate/state`, locks, verify).
2. DevState exports `.trae/state.json` and `.trae/history.json`.
3. Router reloads/reads state and applies CP/feature gating.

## Consequences
- Preserves HMAC chain and No-Drift policy.
- Prevents silent CP changes via manual file edits.
- Aligns CI/IDE gates with Router runtime behavior.

## References
- `docs/STATE.schema.json`
- `docs/ADR/ADR-002-state-management.md`
- `devstate/docs/DEVSTATE_OVERVIEW.md`
- `docs/CP1_ROUTER_SPEC.md`
- `docs/CP1_CHECKLIST.md`
- `docs/CP1_BASELINE.md`

## Fallback and Observability
- Scenarios: missing `state.json`, invalid JSON, invalid schema, `no_drift=false`.
- Logging: structured JSON with `cp_fallback="CP1-baseline"`, no secrets.
- Metrics: `router_cp_state_errors_total` and `router_cp_state_fallback_total` increment on failures/fallback.
- Tests: `apps/otp/router/test/router_state_observability_SUITE.erl` covers scenarios; manual procedure available via log inspection.
- **E2E Scenarios**: `docs/archive/dev/ROUTER_DEVSTATE_E2E_SCENARIOS.md` - Detailed manual testing scenarios for QA/Dev/On-call teams.