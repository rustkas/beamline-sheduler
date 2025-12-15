# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial project structure and specifications
- State management with `.trae/state.json` and `.trae/history.json`
- Schema versioning policy (see `docs/SCHEMA_VERSIONING.md`)
- CI/CD validation gates for schema changes
- HMAC chain for audit trail integrity
- Release & Change Management (wrk-8) artifacts
- Link validation scripts (`scripts/check_links.sh`, `scripts/check_links.ps1`)
- Version gates validation (`scripts/check_version_gates.sh`)
- Release simulation script (`scripts/simulate_release.sh`)
- **Router Observability**: Enhanced `router_jetstream_redelivery_total` metric with full label support (`assignment_id`, `request_id`, `reason`, `source`) for detailed observability and fault injection coverage
- **Router Observability**: Structured logging for redelivery events (`"Message redelivery requested"` at INFO level)
- **Router Observability**: ETS and Prometheus export support for labeled metrics
- **Router Observability**: Comprehensive validation scripts and staging validation guide

### Changed
- Initial release - no changes from previous version
- **Router Observability**: `router_jetstream_redelivery_total` metric now supports labels for filtering and alerting by source/reason (backward compatible)

### Deprecated
- Nothing deprecated yet

### Removed
- Nothing removed yet

### Fixed
- No bug fixes in initial release

### Security
- HMAC masking policy for documentation
- Secret management guidelines

## [1.0.0] - 2025-01-27

### Added
- **CP0-LC: Repo/State Bootstrap** - Initial repository structure
- **State Management**: `.trae/state.json` with artifact checksums
- **History Tracking**: `.trae/history.json` with HMAC chain
- **Schema Definitions**:
  - `docs/STATE.schema.json` (v1.0.0)
  - `docs/HISTORY.schema.json` (v1.0.0)
- **Protocol Definitions**:
  - `proto/beamline/flow/v1/flow.proto` - Router ABI
  - `proto/beamline/provider/v1/provider.proto` - Provider ABI
- **Database Schema**: `sql/000_init.sql` - Initial DDL
- **Documentation**:
  - `docs/SCHEMA_VERSIONING.md` - Versioning policy
  - `docs/CI_VALIDATION.md` - CI validation guide
  - `docs/PR_CHECKLIST.md` - PR review checklist
  - `docs/CP_TRANSITION_GUIDE.md` - Checkpoint transition guide
- **Validation Scripts**:
  - `scripts/validate_state.sh` - State validation
  - `scripts/verify_hmac_chain.py` - HMAC chain verification
  - `scripts/check_schema_changes.sh` - Schema changes validation
  - `scripts/check_hmac_masking.sh` - HMAC masking check
- **Manifest**: `.trae/manifest.json` - Single source of truth for schema versions

### Changed
- Initial release - no changes from previous version

### Deprecated
- Nothing deprecated yet

### Removed
- Nothing removed yet

### Fixed
- No bug fixes in initial release

### Security
- HMAC secret management policy
- HMAC masking in documentation (format: `16+...`)

---

## Version Format

This project uses [Semantic Versioning](https://semver.org/):
- **MAJOR**: Breaking changes requiring migration
- **MINOR**: Backward-compatible additions
- **PATCH**: Bug fixes and clarifications

## Schema Versions

Current schema versions (see `.trae/manifest.json` for details):

- **STATE Schema**: `1.0.0`
- **HISTORY Schema**: `1.0.0`
- **artifact_checksums Format**: `array_object_v1` (v1.0.0)

## Release Process

See `docs/RELEASE_PROCESS.md` for detailed release procedures.

## Links

- **Schema Versioning**: `docs/SCHEMA_VERSIONING.md`
- **Manifest**: `.trae/manifest.json`
- **PR Checklist**: `docs/PR_CHECKLIST.md`

