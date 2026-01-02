# CP3: DevState CLI Auto-Repair — Specification

## Purpose
- Define the CP3 increment for `devstate_auto_repair.sh` without implementation.
- Align with DevState policies, No-Drift rules, and `CP3_DELTA_CHECKLIST.md`.

## Supported Problem Types
- Drift: artifact checksums in `.trae/state.json` do not match on-disk files.
- HMAC break: broken HMAC chain in `.trae/history.json` (missing links, wrong digest).
- Schema errors: `.trae/state.json` violates `docs/STATE.schema.json` or required invariants.

## Safety Rules
- Always back up `.trae/state.json` and `.trae/history.json` before any changes.
- Record a dry-run report that shows the planned modifications.
- Show a unified diff and list of affected artifacts prior to apply.
- Prefer operations via DevState API when available (`/v1/devstate/*`).
- Append audit entry to `.trae/history.json` with HMAC chain update.
- Never commit secrets or change `BEAMLINE_HMAC_SECRET` source; require env var.

## UX and Modes
- Interactive: prompt user to confirm each repair step; display diffs.
- Non-interactive: execute repairs with flags; return non-zero exit code on failure.
- Exit codes: `0` success, `1` validation failure, `2` API unavailable, `3` partial repair, `4` safety gate blocked.
- Integration: invoke `devstate_verify.sh`, `scripts/validate_state.sh`, and HMAC check pre/post.

## CLI Outline
- `devstate_auto_repair.sh [--dry-run] [--interactive] [--non-interactive] [--api-url <URL>] [--limit <N>] [--force] [--backup-dir <DIR>] [--repair {drift|hmac|schema|all}]`
- Steps:
  - Preflight: verify tools (`jq`, `python3`, `jsonschema`), env (`BEAMLINE_HMAC_SECRET`), repo cleanliness.
  - Backup: copy `.trae/state.json`, `.trae/history.json` to timestamped directory.
  - Detect issues: reuse `scripts/validate_state.sh` and `devstate_verify.sh` outputs.
  - Plan: compute patch set for chosen repair.
  - Present: show diffs and affected artifacts; interactive confirm.
  - Apply: prefer DevState API; fallback local file edits gated by checks.
  - Post-verify: run `devstate_verify.sh` and `validate_state.sh`; return code reflects final state.

## Repairs Details
- Drift:
  - Recalculate checksums for listed artifacts and update `artifact_checksums` in state.
  - Validate presence of files; missing files → mark failure with clear output.
  - API preferred: `POST /v1/devstate/state` with updated checksums; else local write.
- HMAC break:
  - Rebuild HMAC chain using `BEAMLINE_HMAC_SECRET`; ensure `hmac_prev` links.
  - API preferred: `POST /v1/devstate/history` batch repair; else local rewrite.
- Schema errors:
  - Validate against `docs/STATE.schema.json`; propose minimal edits to satisfy required fields.
  - Do not alter business semantics fields; only fix structural/schema violations.

## Safety Gates
- Abort if `.trae/*` files are tracked in Git index (policy violation).
- Abort on protected branches for non-interactive `--force` unless explicit override.
- All local writes must be followed by HMAC recompute and audit entry.

## DevState API Priority
- Health: `GET /health` (probe with short timeouts).
- Verify: `GET /v1/devstate/verify?limit=<N>` for integrity.
- Read state: `GET /v1/devstate/state` for baseline.
- Update state: `POST /v1/devstate/state` with patch.
- Append history: `POST /v1/devstate/history` with entry.
- Export/import: use as fallback when batch changes are needed.

## Integration Points
- `devstate_verify.sh`: used pre and post; honors `SKIP_LOCAL_VALIDATION`.
- `scripts/validate_state.sh`: schema + checksum verification pre and post.
- HMAC checker: ensure chain integrity and exit codes consistent.

## Telemetry and Audit
- Emit structured JSON logs for each step with `trace_id` and timestamps.
- Append repair operation to `.trae/history.json` with updated digest chain.
- Provide a summary file `reports/devstate-auto-repair/summary.json`.

## Alignment with CP3_DELTA_CHECKLIST.md
- The tool must not change product artifacts beyond `.trae/*` and documented checksums.
- Follows No-Drift policy; runs only with explicit flags in CI.
- Provides dry-run mode for approval gates.
- Matches CP3 Delta item "DevState CLI UX and Auto-Repair" deliverables:
  - `scripts/devstate_auto_repair.sh` with backup and diff preview
  - `devstate-tools/devstate/docs/CLI_UX.md` usage and recovery flows
  - Optional CI hint step when drift detected

## Non-Goals
- No automatic mutation of protobufs/SQL/docs.
- No silent repairs; always visible diffs and audit.

## Example Flows
- Drift only (non-interactive): backup → detect → patch checksums via API → post-verify → exit 0.
- HMAC break (interactive): backup → compute chain → show diff → apply via API → post-verify → exit code per result.
- Schema errors: backup → compute minimal fix → confirm → apply → verify.
