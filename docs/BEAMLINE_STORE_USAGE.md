# BeamLine Store Import/Export and Meta-Rules

This document defines how the Erlang/Mnesia-backed `beamline_store` integrates with `.trae/state.json` and `.trae/history.json`, describes the meta-rules for usage, and provides examples.

## Goals

- Preserve `.trae/*` as the single source of truth for state/history across environments.
- Use Mnesia (`beamline_store`) as a fast, transactional runtime cache in Dev/CI.
- Ensure atomic export writes and HMAC chain integrity before validators.

## Key Script

- `scripts/beamline_store_export.sh`
  - `import`: load `.trae/state.json` and `.trae/history.json` into Mnesia.
  - `export`: write Mnesia content back to `.trae/*` atomically (`.tmp → rename`).
  - `import_state` / `import_history` and `export_state` / `export_history` for granular operations.
  - `verify_chain`: verify HMAC chain via Erlang module.

## Hooks and CI Integration

- `scripts/dry_run_ci.sh` pre-hooks an export right before `validate_state.sh` in Development only:
  - Dev-only: runs `export` if `scripts/beamline_store_export.sh` and `apps/otp/beamline_store` exist.
  - CI/Prod: hook disabled, validators use `.trae/*` directly.
  - On failure, logs a warning and continues (non-blocking).
  - Honors `BEAMLINE_MNESIA_CANONICAL_DEV` toggle in Dev.

## Meta-Rules

- Source of Truth
  - Default: `.trae/*` are canonical in all environments.
  - Dev Canonical Option: If `BEAMLINE_MNESIA_CANONICAL_DEV=true`, treat Mnesia as canonical during Dev sessions; export only when validators run.

- Secrets
  - `BEAMLINE_HMAC_SECRET` must be set in CI/Prod; Dev defaults to `beamline-secret-key-v1` if absent.
  - Never write real secrets to repo or logs. Masking validated by `check_hmac_masking.sh`.

- Export Timing
  - Export runs prior to `validate_state.sh` in Dev dry-run to ensure `.trae/*` reflect current runtime cache.
  - Exports use atomic write (`target.tmp → mv`), avoiding partial files.

- Concurrency
  - Import/export initiated only by explicit script calls; no background writes.
  - Avoid concurrent export/import while validators run.

- HMAC Chain
  - HMAC is recomputed per entry using `BEAMLINE_HMAC_SECRET`.
  - First entry has empty `hmac_prev`; subsequent entries must match previous HMAC.

- Checksums
  - State checksum fields must match recorded artifact hashes; validators check consistency.

- Fail-Safe
  - If `beamline_store` or `rebar3` are missing, hooks skip and do not block Dev/CI.
  - Export failures are logged as warnings; validators still run against existing `.trae/*`.

## Usage Examples

```bash
# Import .trae/* into Mnesia runtime cache
bash scripts/beamline_store_export.sh import

# Export Mnesia to .trae/* atomically (used by CI hook)
bash scripts/beamline_store_export.sh export

# Verify HMAC chain via Erlang
bash scripts/beamline_store_export.sh verify_chain

# Granular operations
bash scripts/beamline_store_export.sh import_state
bash scripts/beamline_store_export.sh export_history
```

## Development Toggle

- To make Mnesia canonical in Dev:

```bash
export BEAMLINE_MNESIA_CANONICAL_DEV=true
bash scripts/dry_run_ci.sh all
```

In this mode, `.trae/*` are updated only for validators, reducing file churn during local iterations while keeping validation gates aligned.

## Troubleshooting

- `rebar3 not found`: install rebar3 or skip using Mnesia; CI hook logs a warning and continues.
- Compile failure: export script exits with code 2; `dry_run_ci.sh` continues.
- `apps/otp/beamline_store` missing: hook is skipped.

## Acceptance Criteria Alignment

- No-Drift: `validate_state.sh` passes; `.trae/*` match runtime state and history when export runs.
- Audit: history chain verified via Python and Erlang validators; masking rules enforced.
- Checkpoints: CP gates remain unchanged; this integration is additive and non-blocking.
