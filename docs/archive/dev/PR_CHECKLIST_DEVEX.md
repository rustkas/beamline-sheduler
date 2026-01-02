# PR Checklist (Docs & DevEx)

Ensure documentation and developer experience remain clean and consistent.

## Checklist
- Links valid: local and external checked by scripts
- Secrets masked: validators hide tokens in logs
- ADRs updated: policy changes reflected in `docs/ADR_INDEX.md`
- Dry-run CI logs collected: `reports/dry-run-logs/...`
- **Architectural changes**: If modifying Router/Gateway/Proto, run `bash scripts/check_cp1_contracts.sh` and review `docs/archive/dev/CP1_ARCHITECTURE_CHECKLIST.md`

## Scripts
- `scripts/check_links.ps1` / `scripts/check_links.sh`
- `scripts/validate_docs.ps1` (markdown style, headings, code blocks language)

## Exit Codes
- `0`  all checks passed
- `2`  broken links
- `4`  style violations
- `1`  other errors

## References
- `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md`  BeamLine vision & architecture overview (context for docs/DevEx changes)
- `docs/archive/dev/CP1_ARCHITECTURE_CHECKLIST.md`  CP1 architecture review checklist (for architectural changes)
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`  CP1 module boundaries and contracts (for boundary changes)
- `docs/archive/dev/DOCS_DEVEX_SETUP.md`  link checker usage
- `docs/TYPOGRAPHY_STYLE.md`  style rules and markdownlint integration
- `docs/ADR_INDEX.md`  ADRs to update on policy changes
