# ADR-001: Documentation Link Checking Policy

Date: 2025-11-06
Status: Accepted

## Context
Documentation is actively evolving, links change frequently. Need to ensure:
- Cross-platform checking (Bash/PowerShell)
- Secret masking in logs
- Predictable exit codes for CI
- Local integration in dry-run CI

## Decision
- Introduce scripts: `scripts/check_links.sh` and `scripts/check_links.ps1`
- Ignore `mailto:`, `data:`, anchors `#...`
- Mask tokens in output (`[MASKED]`)
- Exit codes: `0` ok, `2` external, `3` local, `1` other errors
- Integrate into local dry-run, save log: `reports/dry-run-logs/docs/link_check.log`

## Consequences
- Improved reliability of links in docs and README
- Clear signaling for CI and local checking

## Path Resolution Notes
- Links like `docs/...`, `.cursor/...`, `.github/...` are treated as relative to repository root
- Within `docs/` directory, it's better to use relative links without `docs/` prefix
