# Docs DevEx Setup

Guidelines and tooling to keep documentation links valid and integrate link checks into local dry-run CI.

## Scripts
- Bash: `scripts/check_links.sh`
- PowerShell: `scripts/check_links.ps1` (run with `-ExecutionPolicy Bypass`)

## What is checked
- HTTP(S) links: HEAD request, 10s timeout
- Local links: relative to the file and repo root (`docs/`, `.cursor/`, `README.md`, `.github/`, `scripts/`, `reports/`)
- Ignored: `mailto:`, `data:`, anchors `#...`
- Secret/token masking in output (`[MASKED]`)

## Exit codes
- `0`  all links are valid
- `2`  broken external links found
- `3`  broken local links found
- `1`  other errors (e.g., execution error)

## Run examples
PowerShell:
```
PowerShell -ExecutionPolicy Bypass -File .\scripts\check_links.ps1 docs .cursor README.md
```

Bash (WSL/Git Bash):
```
bash ./scripts/check_links.sh docs .cursor README.md
```

## Dry-run CI integration
- Logs: `reports/dry-run-logs/docs/link_check.log`
- Steps: run the script, save log, produce summary in `docs/archive/dev/DRY_RUN_LOGS.md`

## Link recommendations
- Inside `docs/archive/dev/` use relative links with `dev/` prefix (e.g. `dev/CI_SECRETS_SETUP.md`)
- Inside `docs/` use relative links without the `docs/` prefix (e.g. `API_CONTRACTS.md`)
- For links from the repo root, `docs/...` and `.github/...` are allowed  the validator resolves them from the root
