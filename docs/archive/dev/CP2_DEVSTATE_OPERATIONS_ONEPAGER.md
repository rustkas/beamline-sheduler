# CP2 / DevState Hooks — Daily Operations One‑Pager

Purpose: practical how‑to for daily work with CP2 Router/metrics and DevState hooks. Keeps it short and actionable; refer to full docs for details.

## Quick Start — Run CP2 Router and Metrics
- Router E2E (CP2 features together):
  - `cd apps/otp/router && rebar3 ct --suite test/router_cp2_features_e2e_SUITE`
- Prometheus (optional):
  - `docker compose -f docker-compose.prometheus.yml up -d`
  - `curl -fsS http://localhost:9090/api/v1/targets | jq '.data.activeTargets[] | select(.health == "up")'`
- Router metrics quick check:
  - `curl -fsS http://localhost:9001/metrics | grep -E 'router_(jetstream|idem|acl|circuit_breaker|rate_limit)' | head -10`

## DevState Hooks — What They Do
- Pre‑commit (fast gate):
  - Runs only when `.trae/state.json` or `.trae/history.json` are staged.
  - Verifies locally (and via API if DevState service is up).
  - Messages: `[OK]` pass, `[FAIL]` blocks commit, `[WARN]` informational.
  - Verbose mode: `export VERBOSE=true` for OS hints and extra context.
- Pre‑push (strict gate):
  - Blocks if any `.trae/*` files are tracked in Git.
  - If DevState files are included in push, runs full validation: `devstate_verify.sh`, `validate_state.sh`, HMAC chain.
  - Bypass policy: allowed only in CI (`CI=true`) or with `DEVSTATE_BYPASS_ROLE=admin`; disabled on protected branches (`main`, `develop`).
  - Configure protected branches: `export PROTECTED_BRANCHES="main develop"` (default already set).

## Interpreting Failures — Quick Actions
- `[FAIL] DevState verification failed` (commit/push blocked):
  - Run locally: `bash devstate-tools/devstate/scripts/devstate_verify.sh`
  - Validate schema/checksums: `bash scripts/validate_state.sh`
  - Verify HMAC chain: `python3 scripts/verify_hmac_chain.py --verbose`
  - If state/history changed outside DevState: re‑export via `bash devstate-tools/devstate/scripts/devstate_export.sh` and re‑validate.
- `Error: .trae files are tracked in Git` (pre‑push):
  - `git rm --cached .trae/state.json .trae/history.json`
  - Ensure `.trae/` is ignored and managed only via DevState.
- DevState service not running:
  - Hooks will perform local validation; start service for API checks: `make devstate-up`, health: `make devstate-health`.
- Missing local tools:
  - Linux: `sudo apt-get update && sudo apt-get install -y jq && pip install jsonschema`
  - macOS: `brew install jq && pip3 install jsonschema`

## Emergency — When Bypass Is Justified
- Allowed only in CI or by admins: `DEVSTATE_BYPASS_ROLE=admin` or `CI=true`.
- Not allowed on protected branches (`main`, `develop`).
- Command: `SKIP_DEVSTATE_GATES=true git push` (use sparingly; fix root cause ASAP).
- Always capture reason in PR description and run CI DevState gates.

## Cheat Sheet — Daily Commands
- Start DevState: `make devstate-up`
- Check health: `make devstate-health`
- Verify DevState (local + API): `bash devstate-tools/devstate/scripts/devstate_verify.sh`
- Validate state/history: `bash scripts/validate_state.sh`
- Verify HMAC chain: `python3 scripts/verify_hmac_chain.py --verbose`
- Verbose hooks: `export VERBOSE=true`

## References
- `docs/archive/dev/CP2_SYSTEM_VALIDATION_RUN.md`
- `docs/archive/dev/CP3_DEVSTATE_HOOKS_PLAN.md`
- `devstate-tools/devstate/docs/HOOKS.md`
- `devstate-tools/devstate/docs/IDE_INTEGRATION.md`
