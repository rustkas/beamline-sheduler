# BeamLine Constructor Documentation

## Quick Navigation

### Core Documentation

- **[specs/AGENTS_CONTRACT.md](specs/AGENTS_CONTRACT.md)** - Agent contract and responsibilities
- **[specs/AGENT_ASSIGNMENT.md](specs/AGENT_ASSIGNMENT.md)** - Agent assignment and roles
- **[guides/LOCAL_CHECKS.md](guides/LOCAL_CHECKS.md)** - Quick developer commands for local validation
  - **CP1 Guardrail**: For architectural changes, run `bash scripts/check_cp1_contracts.sh` and review `guides/CP1_ARCHITECTURE_CHECKLIST.md`

### State & History Management

- **[STATE.schema.json](STATE.schema.json)** - State validation schema (version 1.0.0)
- **[HISTORY.schema.json](HISTORY.schema.json)** - History validation schema (version 1.0.0)
- **STATE_HISTORY_VERSIONING.md** - Versioning process for state and history
- **SCHEMA_VERSIONING.md** - Schema versioning guide

**Reference**: `.trae/manifest.json` - Single source of truth for schema versions

### CI/CD & Validation

- **CI_VALIDATION.md** - CI validation process and requirements
- **[guides/CI_SECRETS_SETUP.md](guides/CI_SECRETS_SETUP.md)** - Secret configuration and HMAC setup
- **CI_CD.md** - CI/CD pipeline documentation
- **[guides/DRY_RUN_LOGS.md](guides/DRY_RUN_LOGS.md)** - Dry-run validation logs and examples
- **[guides/PR_CHECKLIST.md](guides/PR_CHECKLIST.md)** - Pull request checklist
  - **CP1 Guardrail**: Mandatory for architectural/contract changes (see "Architectural/Contract Changes" section)

### Architecture & Design

- **[BEAMLINE_VISION_AND_ARCHITECTURE.md](BEAMLINE_VISION_AND_ARCHITECTURE.md)** - BeamLine product vision and architecture overview
  - Before making architectural changes, verify alignment with this vision and roadmap
- **[ROADMAP.md](ROADMAP.md)** - Current project status and future plans (CP3+, Infra, Ops)
- **[ADR/](ADR/)** - Architecture Decision Records
  - **[ADR_INDEX.md](ADR_INDEX.md)** - ADR index and overview
  - **[ADR-001-monorepo-structure.md](ADR/ADR-001-monorepo-structure.md)** - Monorepo structure
  - **[ADR-002-state-management.md](ADR/ADR-002-state-management.md)** - State management
  - **[ADR-004-erlang-otp-router.md](ADR/ADR-004-erlang-otp-router.md)** - Erlang/OTP router
  - **[ADR-005-mnesia-caching.md](ADR/ADR-005-mnesia-caching.md)** - Mnesia caching
  - **[ADR-006-nats-inter-service-communication.md](ADR/ADR-006-nats-inter-service-communication.md)** - NATS communication
  - **[ADR-010-target-architecture.md](ADR/ADR-010-target-architecture.md)** - Target architecture

- **[ARCHITECTURE/](ARCHITECTURE/)** - Architecture documentation
  - **[api-registry.md](ARCHITECTURE/api-registry.md)** - API endpoint registry
  - **[compatibility-rules.md](ARCHITECTURE/compatibility-rules.md)** - API compatibility rules
  - **[repository-structure.md](ARCHITECTURE/repository-structure.md)** - Repository structure

### Release & Process

- **[RELEASE_PROCESS.md](RELEASE_PROCESS.md)** - Release process and versioning
- **CP_TRANSITION_GUIDE.md** - Checkpoint transition guide
- **[CP1_CHECKLIST.md](archive/dev/CP1_CHECKLIST.md)** - CP1 completion checklist
- **[CP1_ROUTER_SPEC.md](archive/dev/CP1_ROUTER_SPEC.md)** - CP1 router specification
- **guides/CP1_ARCHITECTURE_CHECKLIST.md** - CP1 architecture review checklist (for PRs)
- **[specs/CP1_BOUNDARIES_AND_CONTRACTS.md](specs/CP1_BOUNDARIES_AND_CONTRACTS.md)** - CP1 module boundaries and contracts

### Component Documentation

- **[ROUTING_POLICY.md](ROUTING_POLICY.md)** - Routing policy and JSON-DSL
- **[NATS_SUBJECTS.md](NATS_SUBJECTS.md)** - NATS subjects registry
- **[OBSERVABILITY.md](OBSERVABILITY.md)** - Observability and telemetry
- **Router NATS Failure Handling**:
  - `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Complete behavior specification for publish/publish_with_ack failures
  - `apps/otp/router/docs/NATS_PUBLISH_FAILURE_MONITORING.md` - Operational monitoring and alerting guide (SRE recommendations)
- **[GATEWAY_PURPOSE.md](GATEWAY_PURPOSE.md)** - Gateway purpose and design
- **[GATEWAY_ROUTES.md](GATEWAY_ROUTES.md)** - Gateway routes specification
- **[UI_ROUTES.md](UI_ROUTES.md)** - UI routes specification

### Format & Standards

- **[guides/ARTIFACT_CHECKSUMS_FORMAT.md](guides/ARTIFACT_CHECKSUMS_FORMAT.md)** - Artifact checksums format
- **[TYPOGRAPHY_STYLE.md](TYPOGRAPHY_STYLE.md)** - Typography and style guide

## Key References

### Single Source of Truth

- **`.trae/manifest.json`** - Schema versions, validation rules, compatibility policy
  - Schema versions: `schema_versions.state.version` and `schema_versions.history.version`
  - Validation tools: `validation_tools.*.script`
  - Compatibility policy: `compatibility_policy.*`
  - Security: `security.hmac_secret`, `security.hmac_masking`

### Validation Scripts

All validation scripts are defined in `.trae/manifest.json`:

- `scripts/validate_state.sh` - State validation (schema + checksums)
- `scripts/verify_hmac_chain.py` - HMAC chain verification
- `scripts/check_schema_changes.sh` - Schema changes validation
- `scripts/check_hmac_masking.sh` - HMAC masking verification
- `scripts/check_secret_compatibility.py` - Secret compatibility check
- `scripts/check_secret_leaks.sh` - Secret leak detection

See **[guides/LOCAL_CHECKS.md](guides/LOCAL_CHECKS.md)** for quick commands.

## Documentation Standards

1. **Always reference `.trae/manifest.json`** for schema versions and validation rules
2. **Use relative paths** for internal links (e.g., `[STATE.schema.json](STATE.schema.json)`)
3. **Mask HMAC values** in documentation (format: `16+...` - first 16 hex chars + `...`)
4. **Keep cross-references valid** - verify links point to existing files
5. **Align with ADR decisions** - documentation should not contradict ADR

## Getting Started

1. Read **[dev/AGENTS_CONTRACT.md](specs/AGENTS_CONTRACT.md)** to understand agent responsibilities
2. Check **[dev/LOCAL_CHECKS.md](guides/LOCAL_CHECKS.md)** for quick validation commands
3. Review **CI_VALIDATION.md** for validation process
4. Consult **[ADR_INDEX.md](ADR_INDEX.md)** for architectural decisions

## Maintenance

This documentation is maintained by **WORKER wrk-9: Documentation & Developer Experience**.

For updates or issues, refer to:
- `.cursor/rules/agents/wrk-9-docs-devex.mdc` - WORKER wrk-9 rules
- `.trae/manifest.json` - Single source of truth for versions and rules


