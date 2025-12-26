# WRK‑1 Checklist — CP1 Smoke + CP2 E2E (Router/Gateway)

**Date:** 2025-11-17  
**Status:** ready to run

---

## Preconditions

- Verify Router compiles:
  - `cd apps/otp/router`
  - `rebar3 compile`
- (Optional) Verify Gateway toolchain if used in smoke:
  - `cd apps/gateway`
  - `npm test` or your standard lint/test commands
- Run meta‑checks (optional):
  - `cd /home/rustkas/aigroup`
  - `bash scripts/run_checks.sh`

---

## CP1 Smoke

### Gateway ↔ Router Contract Smoke

- Command:
  - `cd /home/rustkas/aigroup`
  - `bash scripts/gateway_router_contract_smoke.sh --router-only`
- Expected:
  - Exit code 0; Router contract suite passes
- Record in docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY.md

### DevState / Router Fallback Smoke

- Command:
  - `cd /home/rustkas/aigroup`
  - `bash scripts/devstate_router_fallback_smoke.sh --scenario all`
- Expected:
  - Exit code 0; scenarios missing_state, invalid_json, no_drift_false pass
- Record in docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY.md

---

## CP2 E2E — Router

### Router Test Profile (includes CP2 suites)

- Command:
  - `cd /home/rustkas/aigroup`
  - `bash scripts/router_test_profile.sh --jetstream`
- Notes:
  - Profiles JetStream/headers/idempotency-related suites; report saved under `reports/router/test_profiles/`
- Alternative direct run:
  - `cd apps/otp/router`
  - `rebar3 ct --suite router_result_consumer_SUITE`

---

## CP2 Validation Script

- Command:
  - `cd /home/rustkas/aigroup`
  - `bash scripts/validate_cp2.sh`
- Expected:
  - Exit code 0; feature flags, JetStream, idempotency, tracing, multi‑tenant checks green
- Record in docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY.md

---

## Go/No‑Go CP2‑LC

- Update in `docs/archive/dev/CP2_TRANSITION_PLAN_ROUTER.md`:
  - Mark CP1 smoke ✔, CP2 E2E ✔, validate_cp2 ✔
- Optional summary report:
  - `reports/wrk-1-cp2-test-execution-report.md`
  - Include commands, suites, results, and final recommendation