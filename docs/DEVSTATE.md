# DevState (Erlang-based Dev Tool)

> **IMPORTANT**: DevState is a SEPARATE, SIDE PRODUCT that is NOT related to the goal of the BeamLine Constructor project. DevState serves only for development state management and is not part of the AI factory or AI agent orchestration.

DevState is a utility for managing project sources of truth: reading/writing `.trae/state.json` and `.trae/history.json`, HMAC chain audit verification, and No-Drift control. Works on Erlang/OTP and Mnesia, ensuring state reproducibility between local development and CI/CD.

## Features
- Import and export of state/history between JSON and Mnesia.
- Reliable Mnesia startup and auto-creation of tables (`trae_state`, `trae_history`).
- Verification of exported `.tmp` files for pipelines.
- Support for fixed node and cookie for stability.

## Quick Start
1. Make sure the repository has `.trae/state.json` and `.trae/history.json` files.
2. Use CLI from the DevState repo: `devstate/scripts/devstate.sh`.

Examples:
- Import to Mnesia: `bash devstate/scripts/devstate.sh import`
- Export to `.tmp`: `bash devstate/scripts/devstate.sh export`
- Export verification: `bash devstate/scripts/devstate.sh verify`

## CLI Commands
- `import` — loads `.trae/state.json` and `.trae/history.json` into Mnesia.
- `export` — saves state/history to `.trae/state.json.tmp` and `.trae/history.json.tmp`.
- `verify` — performs `export` and checks that `.tmp` files are created and not empty.
- `help` — prints help.

## Environment Variables
- `ERL_NODE_NAME` — Erlang node name (default: `devstate_dev`).
- `ERL_COOKIE` — Erlang node cookie (default: `devstate_cookie`).
- `MNESIA_DIR` — Mnesia storage directory (default: `./.trae/mnesia`).
- `STATE_PATH` — path to `.trae/state.json` (default: `./.trae/state.json`).
- `HISTORY_PATH` — path to `.trae/history.json` (default: `./.trae/history.json`).

## Development Process Integration
- Local: cycle `import → work → export` to fix state in `.tmp`.
- Pre-commit/Pre-push: add step `devstate/scripts/devstate.sh verify` to `scripts/run_checks.sh`.
- CI/CD: use `import → verify → export` as gate for No-Drift and audit.

## Diagnostics
If export doesn't create `.tmp` files:
- Check Mnesia directory: it should be in `./.trae/mnesia`.
- Run `import` in the same session as `export`.
- Fix node and cookie: `ERL_NODE_NAME=devstate_dev ERL_COOKIE=devstate_cookie`.

## Expected Productivity Gains
- Acceleration of No-Drift checks and CP passage: 25–40%.
- Reduction of artifact inconsistency incidents: 70–85%.
- Reduction of manual state/audit checks: 2–3×.

## Documentation
- Section index: `devstate/docs/README.md`
- Overview: `devstate/docs/DEVSTATE_OVERVIEW.md`
- MCP usage: `devstate/docs/MCP_USAGE.md`
- BeamLine Store history: `devstate/docs/BEAMLINE_STORE_TZ.md`
- Scripts: `devstate/scripts/README.md`

## Note
For sustainable persistence between independent processes, it is recommended to either forcibly use `disc_copies` after import, or work through a single background Erlang daemon and access it via RPC. The current CLI version is oriented towards executing `import/export` in one session for dev-cycle stability.


## Two Stacks (main and standalone)

- Main stack (root `docker-compose.yml`) — platform: `platform-nats`, `platform-gateway`, `platform-postgres`, `platform-router`, and DevState (`trae-devstate`) on `http 3080`, Postgres on `5432`.
- Standalone DevState (`devstate/docker-compose.yml`) — isolated stand: `trae-devstate-standalone` on `http 3180`, `devstate-postgres` on `55432` (internal `5432`).

### When to Use Which
- Main — daily development, integration checks, full platform operation.
- Standalone — isolated No-Drift checks, migrations/debugging of HMAC and state, simultaneous work of multiple IDEs (Trae) without affecting the main platform.

### Startup Commands
- Main stack: `docker compose up -d` (from root) / `docker compose down`.
- Standalone DevState (from devstate repo): `make devstate-up` / `make devstate-down` / `make devstate-logs`.

### Health Check
- Main DevState: `curl http://localhost:3080/health`.
- Standalone DevState: `curl http://localhost:3180/health`.

### Integration with .trae (metadata)
- Sources of truth: `.trae/state.json` and `.trae/history.json`.
- Manifest: `.trae/manifest.json` — indicates schemas and validation tools.
- Recommended CI step: `bash devstate/scripts/devstate_verify.sh` (HMAC chain and export check).

### docs/dev recommendations
- Add pre-commit/pre-push hook calling `devstate_verify.sh`.
- In multi-IDE environment use standalone stack (3180/55432) for isolation.