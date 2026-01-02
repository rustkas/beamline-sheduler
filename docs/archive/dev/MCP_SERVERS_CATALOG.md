# MCP Servers Catalog

Complete catalog of all MCP servers in the BeamLine project with their purpose, capabilities, and tools.

## Overview

The BeamLine project includes **15 MCP servers** providing **69 tools** that wrap real scripts and commands from the repository.

**Total**: 15 servers, 69 tools

---

## Server Catalog

### 1. DevState Management Server

**ID**: `devstate`  
**Location**: `tools/devstate/`  
**Type**: TypeScript (Node.js)  
**Tools**: 8

**Purpose**: Manages DevState service lifecycle and operations. DevState is the state management system that tracks project state, checkpoints, and HMAC chain integrity.

**Capabilities**:
- Start/stop DevState Docker services
- Check health status
- Verify HMAC chain integrity
- Export state and history files
- Set checkpoints (CP1-LC, CP2-LC, etc.)
- Mnesia import/export (Erlang DevState)

**Tools**:
1. `devstate_up` - Start DevState service (docker compose up)
2. `devstate_down` - Stop DevState service (docker compose down)
3. `devstate_health` - Check DevState health endpoint
4. `devstate_verify` - Verify DevState HMAC chain via HTTP
5. `devstate_export` - Export state.json and history.json from DevState
6. `devstate_set_cp` - Set checkpoint via DevState (requires CP parameter)
7. `devstate_mnesia_import` - Import state/history into Mnesia (Erlang DevState)
8. `devstate_mnesia_export` - Export state/history from Mnesia to .tmp files

**Wrapped Scripts**:
- `make devstate-up`, `make devstate-down`, `make devstate-health`, etc.
- `devstate/scripts/devstate_export.sh`
- `devstate/scripts/devstate_verify.sh`
- `devstate/scripts/devstate_set_cp.sh`
- `devstate/scripts/devstate.sh`

**Notes**: DevState has two implementations: HTTP API (Go, primary) and Mnesia (Erlang). HTTP API is the primary implementation.

---

### 2. State & History Validation Server

**ID**: `state-history`  
**Location**: `tools/state-history/`  
**Type**: TypeScript (Node.js)  
**Tools**: 9

**Purpose**: Validates and manages `.trae/state.json` and `.trae/history.json` files. Critical for No-Drift policy enforcement and HMAC chain integrity.

**Capabilities**:
- Validate state/history files (schema, checksums, HMAC)
- Verify HMAC chain integrity
- Recalculate HMAC chain with new secret
- Recompute artifact checksums
- Sign history entries
- Import/export via beamline_store (Mnesia)

**Tools**:
1. `validate_state` - Validate .trae/state.json and .trae/history.json (schema, checksums, HMAC)
2. `verify_hmac_chain` - Verify HMAC chain integrity in .trae/history.json (optional: secret, verbose)
3. `recalculate_hmac_chain` - Recalculate HMAC chain with a new secret (requires secret, optional: rebuild, backup)
4. `recompute_checksums` - Recompute artifact checksums in state.json (optional: specific files array)
5. `sign_history` - Sign latest history entry with HMAC
6. `beamline_store_import` - Import state/history into Mnesia via beamline_store (optional: target, profile)
7. `beamline_store_export` - Export state/history from Mnesia via beamline_store (optional: target, profile)
8. `beamline_store_verify_chain` - Verify HMAC chain using beamline_store (optional: profile)

**Wrapped Scripts**:
- `scripts/validate_state.sh`
- `scripts/verify_hmac_chain.py`
- `scripts/recalculate_hmac_chain.py`
- `scripts/reports/recompute_checksums.sh`
- `scripts/reports/sign_history.sh`
- `scripts/beamline_store_export.sh`

**Notes**: State/history validation is critical for No-Drift policy. HMAC chain must remain intact.

---

### 3. Schema Validation Server

**ID**: `schema-validation`  
**Location**: `tools/schema-validation/`  
**Type**: TypeScript (Node.js)  
**Tools**: 1

**Purpose**: Validates schema version consistency between manifest and actual schema files.

**Capabilities**:
- Check that STATE.schema.json and HISTORY.schema.json versions match .trae/manifest.json

**Tools**:
1. `check_schema_changes` - Validate schema version consistency (manifest vs actual schemas)

**Wrapped Scripts**:
- `scripts/check_schema_changes.sh`
- `scripts/check_schema_changes.ps1`

**Notes**: Checks that STATE.schema.json and HISTORY.schema.json versions match .trae/manifest.json

---

### 4. Protobuf Tools Server

**ID**: `protobuf`  
**Location**: `tools/protobuf/`  
**Type**: TypeScript (Node.js)  
**Tools**: 7

**Purpose**: Protobuf validation, generation, and synchronization checking. Ensures protobuf files are valid and synchronized across the project.

**Capabilities**:
- Validate protobuf files (buf lint + buf build)
- Generate protobuf stubs for Erlang and TypeScript
- Check proto sync between files and generated code
- Fast proto sync check using SHA256 digests
- Check protobuf NATS compatibility
- Update API registry documentation

**Tools**:
1. `check_proto` - Validate protobuf files (buf lint + buf build)
2. `generate_proto_stubs` - Generate protobuf stubs for Erlang and TypeScript
3. `check_proto_sync` - Check protobuf sync between proto files and generated code
4. `check_proto_sync_fast` - Fast proto sync check using SHA256 digests
5. `check_proto_nats_compatibility` - Check protobuf NATS compatibility
6. `update_api_registry` - Update api-registry.md with message names
7. `update_api_registry_from_proto` - Update api-registry.md with real message names from proto files

**Wrapped Scripts**:
- `scripts/check_proto.sh`
- `scripts/generate_proto_stubs.sh`
- `scripts/check_proto_sync.sh`
- `scripts/check_proto_sync_fast.sh`
- `scripts/check_proto_nats_compatibility.sh`
- `scripts/update_api_registry.sh`
- `scripts/update_api_registry_from_proto.sh`

**Notes**: Protobuf tools require buf CLI. Some tools check proto sync between root and router directories.

---

### 5. Security & Secrets Server

**ID**: `security`  
**Location**: `tools/security/`  
**Type**: TypeScript (Node.js)  
**Tools**: 3

**Purpose**: Security validation, secret leak detection, and HMAC masking checks. Ensures no secrets are exposed in the repository.

**Capabilities**:
- Check HMAC masking in documentation
- Detect potential secret leaks
- Check secret compatibility with HMAC chain

**Tools**:
1. `check_hmac_masking` - Check HMAC masking in documentation (optional: files array)
2. `check_secret_leaks` - Detect potential secret leaks in repository
3. `check_secret_compatibility` - Check secret compatibility with HMAC chain (requires secret, optional: verbose)

**Wrapped Scripts**:
- `scripts/check_hmac_masking.sh`
- `scripts/check_secret_leaks.sh`
- `scripts/check_secret_compatibility.py`

**Notes**: Security tools use secret_patterns.json for leak detection patterns. HMAC masking follows 16+... format.

---

### 6. Compliance Validation Server

**ID**: `compliance`  
**Location**: `tools/compliance/`  
**Type**: TypeScript (Node.js)  
**Tools**: 2

**Purpose**: Validates compliance artifacts including licenses, privacy policies, and SBOM (Software Bill of Materials).

**Capabilities**:
- Validate compliance artifacts (licenses, privacy, SBOM)
- Check license compliance for all dependencies

**Tools**:
1. `validate_compliance` - Validate compliance artifacts (licenses, privacy, SBOM)
2. `check_license_compliance` - Check license compliance for all dependencies

**Wrapped Scripts**:
- `scripts/compliance/validate_compliance.sh`
- `scripts/check_license_compliance.sh`

**Notes**: Compliance validation checks file presence, structure, and absence of real secrets. Exit codes: 0=OK, 2=External broken, 3=Local broken.

---

### 7. Observability Validation Server

**ID**: `observability`  
**Location**: `tools/observability/`  
**Type**: TypeScript (Node.js)  
**Tools**: 4

**Purpose**: Validates observability configuration including JSON log format and health endpoints.

**Capabilities**:
- Validate observability configuration (JSON logs, health endpoints)
- Run OBS-1 validators (Node, jq, Python) on log files
- Validate JSON log conformance using jq
- Validate JSON log conformance using Node.js

**Tools**:
1. `validate_observability` - Validate observability configuration (JSON logs, health endpoints)
2. `run_obs1_validators` - Run OBS-1 validators (Node, jq, Python) on log files (optional: collect, paths)
3. `obs1_jq_validate` - Validate JSON log conformance using jq (requires log_file)
4. `obs1_node_validate` - Validate JSON log conformance using Node.js (requires log_file)

**Wrapped Scripts**:
- `scripts/observability/validate_observability.sh`
- `scripts/observability/run_obs1_validators.sh`
- `scripts/obs1_jq_validate.sh`
- `scripts/obs1_node_validate.mjs`

**Notes**: OBS-1 validators check required keys (timestamp, level, msg, trace_id, tenant_id) and basic type validation. Exit codes: 0=success, 2=external broken, 3=local broken.

---

### 8. Infrastructure Management Server

**ID**: `infrastructure`  
**Location**: `tools/infrastructure/`  
**Type**: TypeScript (Node.js)  
**Tools**: 9

**Purpose**: Manages local development infrastructure including Docker services, database initialization, and environment validation.

**Capabilities**:
- Validate infrastructure configuration
- Verify all services are up and responding
- Initialize database schema
- Start/stop/build local development services
- View service logs
- Set up local development environment
- Validate environment configuration

**Tools**:
1. `validate_infra` - Validate infrastructure configuration
2. `verify_services` - Verify all services are up and responding correctly
3. `init_db` - Initialize database schema
4. `local_up` - Start local development services (docker compose up)
5. `local_down` - Stop local development services (docker compose down)
6. `local_build` - Build local development services (docker compose build)
7. `local_logs` - View local development service logs (optional: service name)
8. `setup_local` - Set up local development environment
9. `validate_env` - Validate environment configuration (.env file, required variables)

**Wrapped Scripts**:
- `scripts/infra/validate_infra.sh`
- `scripts/infra/verify-services.sh`
- `scripts/infra/init-db.sh`
- `scripts/local/up.sh`
- `scripts/local/down.sh`
- `scripts/local/build.sh`
- `scripts/local/logs.sh`
- `scripts/setup-local.sh`
- `scripts/validate-env.sh`

**Notes**: Infrastructure tools manage Docker Compose services, database initialization, and environment validation.

---

### 9. Documentation Tools Server

**ID**: `documentation`  
**Location**: `tools/documentation/`  
**Type**: TypeScript (Node.js)  
**Tools**: 1

**Purpose**: Validates documentation links and cross-references.

**Capabilities**:
- Validate links in documentation (internal and external)

**Tools**:
1. `check_links` - Validate links in documentation (optional: directory, default: docs/)

**Wrapped Scripts**:
- `scripts/check_links.sh`
- `scripts/check_links.ps1`

**Notes**: Link checker validates internal and external links in markdown files.

---

### 10. Release Management Server

**ID**: `release-management`  
**Location**: `tools/release-management/`  
**Type**: TypeScript (Node.js)  
**Tools**: 2

**Purpose**: Manages versions, CHANGELOG, and release processes. Ensures version gates are met before release.

**Capabilities**:
- Check version gates before release
- Simulate release process

**Tools**:
1. `check_version_gates` - Check version gates before release (optional: version)
2. `simulate_release` - Simulate release process (requires version)

**Wrapped Scripts**:
- `scripts/check_version_gates.sh`
- `scripts/simulate_release.sh`

**Notes**: Version gates check schema consistency, state validation, HMAC chain, and documentation. Exit codes: 101=Invalid SemVer, 102=Version inconsistency, 103=Documentation errors.

---

### 11. CI/CD Validation Server

**ID**: `ci-validation`  
**Location**: `tools/ci-validation/`  
**Type**: TypeScript (Node.js)  
**Tools**: 2

**Purpose**: Comprehensive CI/CD validation gates for local testing. Runs all CI checks locally before pushing.

**Capabilities**:
- Run comprehensive CI/CD validation gates locally
- Quick local checks (Router compile/test, Gateway tests, ABI checks)

**Tools**:
1. `dry_run_ci` - Run comprehensive CI/CD validation gates locally (optional: step - schema, hmac, security, backend, frontend, qa, compliance, summary, all)
2. `run_checks` - Quick local checks (Router compile/test, Gateway tests, ABI checks)

**Wrapped Scripts**:
- `scripts/dry_run_ci.sh`
- `scripts/run_checks.sh`

**Notes**: dry_run_ci.sh runs all CI gates locally. Steps can be run individually. Exit codes: 0=success, 1=schema error, 2=secrets error, 3=build error, 4=compliance error, 101-103=version errors.

---

### 12. Checkpoint Validation Server

**ID**: `cp-validation`  
**Location**: `tools/cp-validation/`  
**Type**: TypeScript (Node.js)  
**Tools**: 4

**Purpose**: Validates CP1 and CP2 contracts, implementation, and behavior. Ensures checkpoint requirements are met.

**Capabilities**:
- Verify CP1 contracts (production-friendly diagnostics)
- Check CP2 implementation (documentation, alerts, E2E tests, metrics)
- Verify CP2 behavior (supervisor wiring, NATS adapter, JetStream)
- Validate all sub-projects for CP1

**Tools**:
1. `check_cp1_contracts` - Verify CP1 contracts (production-friendly diagnostics)
2. `check_cp2_implementation` - Check CP2 implementation (documentation, alerts, E2E tests, metrics)
3. `check_cp2_behavior` - Verify CP2 behavior (supervisor wiring, NATS adapter, JetStream)
4. `validate_all_projects` - Validate all sub-projects for CP1

**Wrapped Scripts**:
- `scripts/check_cp1_contracts.sh`
- `scripts/check_cp2_implementation.sh`
- `scripts/check_cp2_behavior.sh`
- `scripts/validate_all_projects.sh`

**Notes**: CP validation scripts emit VS Code/Windsurf-friendly diagnostics in 'file:line:col: error|warn: message' format.

---

### 13. Router Testing Server

**ID**: `router-testing`  
**Location**: `tools/router-testing/`  
**Type**: TypeScript (Node.js)  
**Tools**: 10

**Purpose**: Router test execution, profiling, and contract validation. Comprehensive testing tools for the Router component.

**Capabilities**:
- Run fast/slow/CP1 smoke test suites
- List test suites by tag expression
- Profile test suites (measure execution time and size)
- Static contract snapshot checks
- Regression snapshot reports
- Gateway ↔ Router contract smoke tests
- DevState Router fallback behavior tests
- Check test run location

**Tools**:
1. `router_test_fast` - Run fast Router test suites (optional: verbose)
2. `router_test_slow` - Run slow Router test suites (JetStream E2E, property, load) (optional: verbose)
3. `router_test_cp1_smoke` - Run CP1 baseline smoke tests (optional: verbose)
4. `router_list_tests_by_tag` - List test suites by tag expression (requires tag_expr)
5. `router_test_profile` - Profile Router test suites to measure execution time and size (optional: mode, suites)
6. `router_contract_snapshot` - Static check that documented contracts match Router implementation (optional: verbose, output)
7. `router_regression_snapshot` - Generate regression snapshot report of Router metrics and log patterns (optional: baseline, compare)
8. `gateway_router_contract_smoke` - E2E smoke test for Gateway ↔ Router contract verification (optional: mode)
9. `devstate_router_fallback_smoke` - Verify Router fallback behavior in various DevState conditions (optional: scenario)
10. `router_check_test_location` - Check if tests are being run from correct directory

**Wrapped Scripts**:
- `apps/otp/router/scripts/test_fast.sh`
- `apps/otp/router/scripts/test_slow.sh`
- `apps/otp/router/scripts/test_cp1_smoke.sh`
- `apps/otp/router/scripts/list_tests_by_tag.sh`
- `scripts/router_test_profile.sh`
- `scripts/router_contract_snapshot.sh`
- `scripts/router_regression_snapshot.sh`
- `scripts/gateway_router_contract_smoke.sh`
- `scripts/devstate_router_fallback_smoke.sh`
- `apps/otp/router/scripts/check_test_run_location.sh`

**Notes**: Router testing tools require rebar3 and Erlang/OTP. Test profiling helps identify heavy suites for CI optimization.

---

### 14. Project Status Server

**ID**: `project-status`  
**Location**: `tools/project-status/`  
**Type**: TypeScript (Node.js)  
**Tools**: 1

**Purpose**: Generates project status reports and diagnostics.

**Capabilities**:
- Generate comprehensive project status report

**Tools**:
1. `generate_project_status` - Generate project status report

**Wrapped Scripts**:
- `scripts/generate_project_status.sh`

**Notes**: Project status report includes current CP, no_drift flag, compilation status, tests, Dialyzer, and other metrics.

---

### 15. Windsurf Hooks MCP Server

**ID**: `windsurf-hooks-mcp`  
**Location**: `tools/windsurf-hooks-mcp/`  
**Type**: TypeScript (Node.js)  
**Tools**: 5

**Purpose**: MCP server for managing Windsurf IDE hooks and running CP validation checks.

**Capabilities**:
- Recommend AI model based on changed files (Windsurf hook)
- Block read access outside allowlist directories (Windsurf policy hook)
- List Windsurf hooks configuration
- Run CP1 contracts check
- Run CP2 behavior check

**Tools**:
1. `windsurf_recommend_model` - Recommend AI model based on changed files (optional: stdin_data)
2. `windsurf_block_read` - Block read access outside allowlist directories (optional: stdin_data)
3. `list_windsurf_hooks` - List all Windsurf hooks from `.windsurf/hooks.json`
4. `run_check_cp1_contracts` - Run CP1 contracts check script
5. `run_check_cp2_behavior` - Run CP2 behavior check script

**Wrapped Scripts**:
- `scripts/windsurf_hooks/recommend_model.py`
- `scripts/windsurf_hooks/block_read_outside_allow.py`
- `scripts/windsurf_hooks/list_hooks.sh`
- `scripts/check_cp1_contracts.sh`
- `scripts/check_cp2_behavior.sh`

**Notes**: 
- This is a **full MCP server** (implements `initialize`, `tools/list`, `tools/call`)
- Replaces the old `windsurf-hooks/server.py` which was NOT an MCP server
- Old `tools/windsurf-hooks/server.py` still exists for Windsurf IDE hooks (via `.windsurf/hooks.json`)
- This MCP server provides tools for managing hooks via Trae/Cursor IDE

---

## Summary Table

| Server ID | Tools | Type | Purpose |
|-----------|-------|------|---------|
| `devstate` | 8 | TypeScript | DevState service management |
| `state-history` | 9 | TypeScript | State/history validation |
| `schema-validation` | 1 | TypeScript | Schema version validation |
| `protobuf` | 7 | TypeScript | Protobuf tools |
| `security` | 3 | TypeScript | Security & secrets |
| `compliance` | 2 | TypeScript | Compliance validation |
| `observability` | 4 | TypeScript | Observability validation |
| `infrastructure` | 9 | TypeScript | Infrastructure management |
| `documentation` | 1 | TypeScript | Documentation tools |
| `release-management` | 2 | TypeScript | Release management |
| `ci-validation` | 2 | TypeScript | CI/CD validation |
| `cp-validation` | 4 | TypeScript | Checkpoint validation |
| `router-testing` | 10 | TypeScript | Router testing |
| `project-status` | 1 | TypeScript | Project status |
| `windsurf-hooks-mcp` | 5 | TypeScript | Windsurf hooks MCP |

**Total**: 15 servers, 69 tools

---

## Related Documentation

- **[MCP_ARCHITECTURE.md](./MCP_ARCHITECTURE.md)** - Architecture and file organization
- **[MCP_MAINTENANCE.md](./MCP_MAINTENANCE.md)** - Maintenance guide
- **[MCP_FILE_MAP.md](./MCP_FILE_MAP.md)** - File location reference
- **[MCP_INDEX.md](./MCP_INDEX.md)** - Documentation index

## Complete Inventory

For complete technical details including input schemas and script mappings, see:
- `mcp-inventory.json` (project root) - Complete JSON inventory

