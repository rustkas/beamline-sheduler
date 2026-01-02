# Router Dev Quickstart

## Goals
- Run Router + DevState + NATS locally
- Switch CP and enable/disable CP2+ features
- Run essential tests before PR
- Know where contracts and ADRs live

## Prerequisites
- Docker and Docker Compose installed
- Erlang/OTP tooling (`rebar3`)

## Start DevState and NATS
- Start DevState standalone: `make devstate-up`
- Health: `curl -fsS http://localhost:3080/health`
- Optional NATS (if not already running): run local NATS via your environment (default Router expects `nats://localhost:4222`)

## Switch CP via DevState
- Set CP1: `make devstate-set-cp CP=CP1-LC`
- Set CP2: `make devstate-set-cp CP=CP2-LC`
- What happens:
  - DevState updates `current_cp` via HTTP
  - Verifies HMAC chain
  - Exports `.trae/state.json` and `.trae/history.json`
- Router behavior:
  - Reads `.trae/state.json` (documented)
  - If invalid/missing or `no_drift=false`, falls back to CP1 baseline and logs structured JSON with `cp_fallback: "CP1-baseline"`

## Enable CP2+ Features (gates)
- Edit `apps/otp/router/src/beamline_router.app.src` or use env vars:
```bash
export BEAMLINE_ROUTER_IDEMPOTENCY_ENABLED=true
export BEAMLINE_ROUTER_TRACING_ENABLED=true
export BEAMLINE_ROUTER_ADMIN_GRPC_ENABLED=true
export BEAMLINE_ROUTER_ACK_ENABLED=true
```
- Key gates:
  - `ack_enabled` → `router_ack_consumer`
  - `idempotency_enabled` → `router_idempotency`
  - `admin_grpc_enabled` → RouterAdmin
  - `tracing_enabled` → tracing in consumers/adapters
  - `nats_js_*` → JetStream tuning

## Run Tests (pre-PR)
- Fast eunit suites:
  - `cd apps/otp/router && rebar3 eunit -m router_state_observability_SUITE`
  - Add other smoke suites as needed (e.g., `router_result_consumer_SUITE`)

## Contracts and ADRs
- NATS subjects: `docs/NATS_SUBJECTS.md`
- **E2E Scenarios**: `docs/archive/dev/ROUTER_DEVSTATE_E2E_SCENARIOS.md` - Manual testing scenarios for DevState ↔ Router interaction
- Routing policy DSL: `docs/ROUTING_POLICY.md`
- ADRs: `docs/ADR_INDEX.md`, Router decision `docs/ADR/ADR-004-erlang-otp-router.md`

## Guided Workflows

**New to Router?** Start here: `docs/archive/dev/ROUTER_ONBOARDING_SCENARIOS.md`

This document provides guided flows for common scenarios:
- **Dev Feature Flow**: Adding a new feature and verifying it
- **Incident Flow**: Diagnosing CP/DevState issues
- **Contract Flow**: Verifying Gateway↔Router contract
- **Test Profile Flow**: Profiling test suites for CI decisions

Each scenario includes:
- Prerequisites and verification steps
- Step-by-step commands
- Expected results
- Troubleshooting tips
- References to relevant documentation

## CI Pipeline Profiles

**Setting up CI/CD?** See: `docs/archive/dev/CI_PROFILES_ROUTER.md`

This document defines three CI pipeline profiles:
- **Fast CI**: Every PR (< 5 minutes) - CP1 smoke + fast contract tests
- **Extended CI**: Merge to main (10-20 minutes) - CP1 smoke + selected slow/JetStream + DevState
- **Nightly CI**: Scheduled (30-60 minutes) - All tests including load, property, fault injection

Each profile includes:
- Commands and scripts
- Test suites included
- Expected duration
- GitHub Actions examples
- Recommendations for when to use
- DevState and Router integration: `docs/ADR/ADR-015-router-devstate-integration.md`

## Troubleshooting
- If CP change fails: ensure DevState up; check `devstate/scripts/devstate_set_cp.sh` output
- If Router doesn’t pick up state: restart Router; inspect logs for `cp_fallback`