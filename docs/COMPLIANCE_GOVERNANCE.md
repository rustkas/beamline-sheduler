# Compliance & Governance (MVP)

Goal: minimal compliance artifacts for local development.

## Required Files

- `compliance/licenses/REGISTRY.md`  registry of licenses for dependencies and components.
- `compliance/privacy/POLICY.md`  basic privacy policy for the MVP.
- `compliance/sbom/SBOM.template.json`  template Software Bill of Materials (SBOM).

## Minimum Requirements

- All files are present and accessible in the repository.
- Structures follow minimal templates (title, versions, list of items).
- Artifacts contain no real secrets or personal data (PII).

## Validator

- Scripts: `scripts/compliance/validate_compliance.ps1` and `.sh`.
- Checks: presence of files, basic structure, and absence of secrets/PII.
- Exit codes: `0`  OK; `2`  external failures (e.g., linked external resources unavailable); `3`  local issues (missing file/structure).

## Notes

- Populate the SBOM template as dependencies are added.
- Record licenses in `REGISTRY.md` with source and version.
