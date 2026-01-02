# PR Checklist (General)

Release and validation checklist across versioning, state, and CI gates.

## Pre-release
- Run `scripts/release.sh` locally
- Check version gates output

## Standard Checks
- Schema changes: if `STATE.schema.json` modified, update version
- State validation: `.trae/state.json` matches manifest and checksum format
- HMAC masking: validators mask secrets in logs

## Architectural/Contract Changes (CP1 Guardrail)

**MANDATORY** for PRs that modify:
- Proto files (`proto/beamline/**`)
- Router/Gateway boundaries (`apps/gateway/**`, `apps/otp/router/**`)
- NATS subjects or message formats
- API contracts or DTOs

**Required checks**:
1. Run CP1 contracts validation:
   ```bash
   bash scripts/check_cp1_contracts.sh
   ```
   - Must pass with exit code 0
   - Verifies Gateway ↔ Router boundaries
   - Validates Proto/NATS/DTO sync

2. Review CP1 architecture checklist:
   - `docs/archive/dev/CP1_ARCHITECTURE_CHECKLIST.md` - Verify CP1 compliance
   - `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` - Verify boundaries respected

**If validation fails**:
- Fix boundary violations (Gateway must not know routing logic, Router must not know HTTP)
- Fix Proto/NATS/DTO inconsistencies
- Re-run validation before submitting PR

## R10 Circuit Breaker (Router)

**MANDATORY** for PRs that modify:
- Circuit breaker logic (`router_circuit_breaker.erl`)
- R10 metrics (`router_r10_metrics.erl`)
- R10 test suites (`router_circuit_breaker_SUITE.erl`, `router_publish_failure_e2e_SUITE.erl`)
- Circuit breaker configuration or thresholds

**Required checks**:
1. **Run R10 unit tests**: `rebar3 ct --suite test/router_circuit_breaker_SUITE`
   - Must pass with exit code 0
   - Verifies circuit breaker logic correctness

2. **Run R10 E2E tests (CI profile)**: `rebar3 ct --suite test/router_publish_failure_e2e_SUITE`
   - Must pass with exit code 0
   - Verifies end-to-end R10 scenarios

3. **Run R10 E2E tests (heavy profile)**: `R10_PROFILE=heavy rebar3 ct --suite test/router_publish_failure_e2e_SUITE`
   - Recommended for significant changes
   - Can be run locally or wait for nightly CI job

4. **Verify no direct ETS access**: Search for `ets:lookup(router_metrics` in test files
   - All metric reading must go through `router_r10_metrics`
   - Exception: Only `ets:info(router_metrics)` and `ets:delete_all_objects(router_metrics)` for cleanup

5. **Verify no hardcoded trigger reasons**: Search for trigger reason binaries
   - All trigger reasons must use constants from `router_r10_metrics:trigger_reason_*()`

6. **Run protective rails validation** (optional but recommended):
   ```bash
   bash apps/otp/router/scripts/check_r10_protective_rails.sh
   ```
   - Automatically checks for direct ETS access and hardcoded trigger reasons
   - CI will run this automatically, but local check saves time

**If validation fails**:
- Fix direct ETS access (use `router_r10_metrics` API)
- Fix hardcoded trigger reasons (use constants)
- Update tests if behavior changed
- Re-run validation before submitting PR

**Reference**: `apps/otp/router/test/R10_MAINTENANCE_CHECKLIST.md` for detailed guidelines

## CP2+ Projects (Router, Gateway)

**MANDATORY** for PRs that modify:
- CP2 feature flags (`apps/otp/router/src/beamline_router.app.src`)
- CP2 modules (`router_idempotency.erl`, `router_tracing.erl`, etc.)
- CP2 configuration (JetStream, idempotency, tracing)

**Required checks**:
1. **CP2 Validation**: Run `bash scripts/validate_cp2.sh` and ensure all checks pass
   - Feature flags enabled
   - Runtime validations passed (JetStream, idempotency, tracing, tenant)
   - No blocking failures

2. **Test Coverage**: CP2 test suites pass (`router_idempotency_SUITE`, `router_tenant_allowlist_SUITE`)

3. Review CP2 readiness:
   - `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md` - CP2 readiness assessment

**If validation fails**:
- Fix feature flags in app.src

## JetStream/NATS/OBS Changes

**CRITICAL**: Any changes to JetStream, NATS, or Observability (OBS) components **MUST** update the formal coverage documentation.

**MANDATORY** for PRs that modify:
- JetStream message handling (`apps/otp/router/src/router_jetstream.erl`)
- NATS operations (`apps/otp/router/src/router_nats.erl`)
- Metrics emission (`apps/otp/router/src/router_metrics.erl`)
- Alert rules (`apps/otp/router/docs/observability/router-alert-rules.yaml`)
- Dashboard panels (`docs/OBSERVABILITY_ROUTER_DASHBOARD.md`)
- Fault injection tests (`apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl`)
- Test scenarios (`apps/otp/router/docs/archive/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`)

**Required updates**:

1. **Coverage Matrix** (`apps/otp/router/docs/archive/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md`):
   - Add/update scenario row with tests, metrics, alerts, dashboards
   - Update coverage status (covered/partial/none)
   - Add comments and future work if needed

2. **Fault Injection Test Scenarios** (`apps/otp/router/docs/archive/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`):
   - Add/update scenario section with:
     - Scenario ID (S1-S3, JS-XXX, NATS-XXX, PERF-XXX)
     - Expected observability (metrics, alerts, dashboard panels)
     - Test implementations with file paths and line numbers
   - Link to coverage matrix

3. **Alert Rules Scenario IDs** (`apps/otp/router/docs/observability/router-alert-rules.yaml`):
   - Add scenario ID comment at top of alert definition
   - Add "Related Scenarios" to alert description
   - Add "Coverage Matrix" link to alert description

4. **Dashboard Panel Scenario IDs** (`docs/OBSERVABILITY_ROUTER_DASHBOARD.md`):
   - Add scenario IDs to panel description
   - Add coverage matrix link
   - Add alert reference (if applicable)

**Required validation**:

```bash
# Run metrics labels and coverage validation
bash apps/otp/router/scripts/validate_metrics_labels.sh
```

**This checks**:
- Helper function exports
- Metric emission with labels
- Dashboard PromQL queries
- Test files exist
- Documentation references
- Scenario-to-test mapping (S1, S2, S3)
- Fault injection test documentation OBS links

**Expected Result**: All checks pass (may have warnings for future work items)

**CI Integration**: Validation runs automatically in `.github/workflows/router-observability-validation.yml`

**If validation fails**:
- Update coverage matrix with new/updated scenarios
- Add scenario IDs to alert rules and dashboard panels
- Link scenarios to tests in fault injection documentation
- Re-run validation before submitting PR

**See**: `CONTRIBUTING.md#jetstreamnatsob-changes` for detailed instructions
- Ensure current_cp >= CP2-LC in `.trae/state.json` (if enabling CP2 features)
- Add missing CP2 modules
- Fix JetStream configuration
- Fix runtime test failures (check `/tmp/*_test.log` files)
- Re-run validation before submitting PR

## Mandatory for release
- Update versions: `.trae/state.json`, `manifest.json`, `CHANGELOG.md`
- Create release notes in `.github/RELEASE_NOTES.md`
- For major versions: add migration scripts under `scripts/migrations/`

## CI Gates
- All gates must pass: schema, state, checksums, manifest
- Link check for `docs/` directory

## Publish Error Tests (JetStream Consumers)

**MANDATORY** for PRs that modify:
- JetStream consumers that use `router_nats:publish/2`
- Publish error handling logic
- Error logging or metrics for publish operations

**Required checks**:
1. **Publish Error Tests**: If adding/modifying a JetStream consumer that uses `router_nats:publish/2`:
   - Add minimum required test scenarios (error return + exception + resilience)
   - Verify consumer process remains alive after publish errors
   - Verify error logging (if applicable)
   - Verify metrics tracking (if applicable)

2. **Update Requirements Mapping**: Update `apps/otp/router/docs/archive/dev/PUBLISH_ERROR_TESTS_REQUIREMENTS_MAPPING.md`:
   - Add consumer to test coverage tables
   - Update coverage summary
   - Add test cases to requirements mapping

**Test Naming Convention**: `test_{consumer_type}_publish_error_{scenario}`

**References**:
- `apps/otp/router/docs/TESTING_RECOMMENDATIONS.md` - Publish error tests section
- `apps/otp/router/docs/archive/dev/PUBLISH_ERROR_TESTS_REQUIREMENTS_MAPPING.md` - Requirements mapping
- `apps/otp/router/docs/archive/dev/PUBLISH_ERROR_TESTS_IMPLEMENTATION_SUMMARY.md` - Implementation summary

## Optional: DevState ↔ Router Fallback Smoke Test

**Optional but recommended**: Run the DevState ↔ Router fallback smoke test locally before pushing:

```bash
# Ensure DevState is running
make devstate-up

# Run default scenario (missing_state)
bash scripts/devstate_router_fallback_smoke.sh

# Or run specific scenario
bash scripts/devstate_router_fallback_smoke.sh --scenario invalid_json
bash scripts/devstate_router_fallback_smoke.sh --scenario no_drift_false

# Or run all scenarios
bash scripts/devstate_router_fallback_smoke.sh --scenario all
```

**What it checks**:
- Router correctly falls back to CP1 baseline when state file is missing (Scenario 2)
- Router correctly falls back when state file contains invalid JSON (Scenario 3)
- Router correctly falls back when `no_drift=false` (Scenario 4)
- Router correctly recovers when state file is restored
- DevState integration is working correctly

**Available scenarios**:
- `missing_state` (default) - Missing state file
- `invalid_json` - Invalid JSON in state file
- `no_drift_false` - no_drift=false (drift detected)
- `all` - Run all scenarios sequentially

**Note**: This test is also run automatically in CI (see `.github/workflows/ci.yml`). Running locally helps catch issues early. CI currently runs the default scenario (`missing_state`). Extended scenarios can be enabled by modifying CI to use `--scenario all`.

**References**:
- `scripts/devstate_router_fallback_smoke.sh` - Smoke test script
- `docs/archive/dev/ROUTER_DEVSTATE_E2E_SCENARIOS.md` - E2E scenarios documentation

## References
- `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md`  product vision and architecture overview for BeamLine
- `docs/archive/dev/CP1_ARCHITECTURE_CHECKLIST.md`  CP1 architecture review checklist
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`  CP1 module boundaries and contracts
- `docs/archive/dev/SCHEMA_CHANGES_TEST.md`  schema change gate behavior
- `docs/archive/dev/ARTIFACT_CHECKSUMS_FORMAT.md`  checksums format in state
- `docs/archive/dev/CI_READINESS_REPORT.md`  local dry-run gates and readiness
- [ ] CP2 validation suite passes (`bash scripts/validate_cp2.sh`)
- `docs/archive/dev/DOCS_DEVEX_SETUP.md`  link checker usage
