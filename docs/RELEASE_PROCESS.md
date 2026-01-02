# Release Process

This document describes the release process for BeamLine Constructor project.

## Versioning Scheme

The project uses [Semantic Versioning (SemVer)](https://semver.org/) in format `MAJOR.MINOR.PATCH`:

- **MAJOR**: Breaking changes requiring migration
- **MINOR**: Backward-compatible additions
- **PATCH**: Bug fixes and clarifications

## Release Types

### Major Release (X.0.0)

**Triggered when**:
- Breaking changes to schemas (STATE, HISTORY, artifact_checksums)
- Breaking changes to ABI (protobuf)
- Breaking changes to DDL (SQL)
- Breaking changes to NATS subjects
- Major architectural changes

**Required actions**:
1. ✅ Update MAJOR version in all artifacts
2. ✅ Create migration scripts
3. ✅ Update documentation
4. ✅ Update `.trae/manifest.json`
5. ✅ Update `CHANGELOG.md` with migration guide
6. ✅ Run full validation suite
7. ✅ Update all agents for new version

### Minor Release (0.X.0)

**Triggered when**:
- Adding optional fields to schemas
- Adding new features (backward-compatible)
- Adding new NATS subjects
- Adding new API endpoints
- Extending enum values

**Required actions**:
1. ✅ Update MINOR version in affected artifacts
2. ✅ Update `.trae/manifest.json`
3. ✅ Update `CHANGELOG.md`
4. ✅ Run validation suite
5. ✅ Update documentation

### Patch Release (0.0.X)

**Triggered when**:
- Bug fixes
- Documentation updates
- Validation pattern fixes
- Typo corrections

**Required actions**:
1. ✅ Update PATCH version (if needed)
2. ✅ Update `CHANGELOG.md`
3. ✅ Run validation suite

## CP2-LC Router Release Criteria

**Checkpoint**: CP2-LC (Baseline)  
**Component**: Router (apps/otp/router)  
**Status**: ✅ **COMPLETE** - All CP2-core features implemented and production-ready

### Test Requirements

**Required Test Suites** (must pass):
- ✅ `router_jetstream_e2e_SUITE.erl` - JetStream E2E tests
- ✅ `router_idempotency_SUITE.erl` - Idempotency tests
- ✅ `router_core_SUITE.erl` - Core routing tests (CP1 baseline)
- ✅ `router_error_SUITE.erl` - Error handling tests (CP1 baseline)
- ✅ `router_gateway_contract_smoke_SUITE.erl` - Gateway↔Router contract tests (CP1 baseline)
- ✅ `router_observability_SUITE.erl` - Observability tests (CP1 baseline)

**Load Tests**:
- ✅ Idempotency under load (verify no duplicate processing)
- ✅ JetStream redelivery under load (verify MaxDeliver exhaustion detection)

### Documentation Requirements

**Required Documents** (must be updated):
- ✅ `docs/archive/dev/CP2_ROUTER_PLAN.md` - CP2-LC plan with scope and criteria
- ✅ `docs/archive/dev/CP2_ROUTER_GATEWAY_SPEC.md` - CP2 specification
- ✅ `../apps/otp/router/docs/archive/dev_reports/CP2_COMPLETE_IMPLEMENTATION_REPORT.md` - Implementation report
- ✅ `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - Operational guide with CP2-LC features
- ✅ `docs/ADR/ADR-011-jetstream-e2e.md` - JetStream ADR (includes CP2 rollout)
- ✅ `docs/ADR/ADR-012-idempotency-layer.md` - Idempotency ADR (CP2-LC scope)

### Observability Requirements

**Required Metrics** (must be emitted):
- ✅ `router_jetstream_redelivery_total` - Redelivery count
- ✅ `router_jetstream_maxdeliver_exhausted_total` - MaxDeliver exhaustion count
- ✅ `router_results_duplicate_total` - Duplicate result detection
- ✅ `router_acks_duplicate_total` - Duplicate ACK detection
- ✅ `router_results_tenant_rejected_total` - Tenant rejection count
- ✅ `router_acks_tenant_rejected_total` - Tenant rejection count
- ✅ `router_tenant_audit_total` - Tenant audit events

**Required Health Checks**:
- ✅ gRPC health service on port 9000 (CP1 baseline maintained)

**Required Logging**:
- ✅ Structured JSON logging with PII filtering (CP1 baseline maintained)
- ✅ OpenTelemetry tracing spans for key operations

### Feature Verification

**CP2-Core Features** (must be functional):
- ✅ JetStream Integration: Durable subscriptions, ACK/NAK, MaxDeliver tracking
- ✅ Idempotency Layer: ETS-based checks with TTL, duplicate detection
- ✅ Tenant Validation/ACL: Policy registry validation, audit events
- ✅ OpenTelemetry Tracing: Span creation, trace context propagation
- ✅ NAK on Errors: Controlled redelivery, MaxDeliver exhaustion detection
- ✅ Headers Support: Headers in assignments and messages

**CP2+ Features** (deferred, not required for CP2-LC):
- 📅 Proto Source Files Restoration: Deferred to CP2+ (see `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md`)
- 📅 CP2+ Fields in Proto: Deferred to CP2+ (backward compatible additions)
- 📅 Advanced Observability: Deferred to Pre-Release phase

**Reference**: `docs/archive/dev/CP2_ROUTER_PLAN.md` for complete feature list, acceptance criteria, and Proto changes policy.

## Pre-Release Checklist

Before creating a release:

### 1. Version Validation

```bash
# Check schema versions consistency
bash scripts/check_schema_changes.sh

# Validate state
bash scripts/validate_state.sh

# Check HMAC masking
bash scripts/check_hmac_masking.sh docs/
```

### 2. Documentation Updates

- [ ] `CHANGELOG.md` updated with all changes
- [ ] Version numbers updated in all artifacts
- [ ] `.trae/manifest.json` updated
- [ ] Migration guides created (for MAJOR releases)

### 3. State Validation

- [ ] `.trae/state.json` valid against schema
- [ ] All artifact checksums updated
- [ ] `no_drift: true` flag set
- [ ] HMAC chain integrity verified

### 4. CI/CD Validation

- [ ] All CI gates pass
- [ ] Schema changes validation passes
- [ ] State validation passes
- [ ] HMAC masking check passes

### 5. SLO Verification (CP3+/Release)

**Required for production releases** (CP3+):

- [ ] SLO verification tests pass (load, chaos, overload)
- [ ] SLO metrics verified (all metrics exist and queryable)
- [ ] SLO alerts verified (alert rules valid and thresholds correct)
- [ ] SLO documentation verified (complete and accurate)

**CI/CD Integration**:
- SLO verification runs automatically in GitHub Actions workflow (`.github/workflows/slo-verification.yml`)
- **Advisory mode** (default for PRs): Warning if SLO gates fail, but does not block merge
- **Blocking mode** (for releases): Blocks release if SLO gates fail

**Manual Verification**:
```bash
# Run SLO verification locally
bash scripts/run_router_slo_verification.sh
bash scripts/verify_slo_metrics.sh
bash scripts/verify_slo_alerts.sh
bash scripts/verify_slo_docs.sh
```

**See**: `docs/ARCHITECTURE/SLI_SLO_ROUTER_GATEWAY.md` for complete SLO/SLI definitions and requirements.

## Release Process Steps

### Step 1: Prepare Release Branch

```bash
# Create release branch
git checkout -b release/v1.0.0

# Update versions in all artifacts
# (use scripts/release_bump_version.sh)
```

### Step 2: Update Versions

**Manual updates required**:
1. Update version in `.trae/state.json`:
   ```json
   {
     "version": "1.0.0"
   }
   ```

2. Update schema versions in `.trae/manifest.json`:
   ```json
   {
     "schema_versions": {
       "state": { "version": "1.0.0" },
       "history": { "version": "1.0.0" }
     }
   }
   ```

3. Update `CHANGELOG.md`:
   - Move `[Unreleased]` changes to new version section
   - Add release date
   - Add migration notes (for MAJOR releases)

### Step 3: Run Validation

```bash
# Full validation suite
bash scripts/release_validate.sh
```

**Checks**:
- Schema version consistency
- State validation
- HMAC chain integrity
- Artifact checksums
- Documentation completeness

### Step 4: Create Release Notes

Use template from `.github/RELEASE_NOTES_TEMPLATE.md`:

```bash
# Generate release notes
bash scripts/generate_release_notes.sh v1.0.0
```

### Step 5: Create Release Commit

```bash
# Commit all changes
git add .
git commit -m "chore: release v1.0.0"

# Tag release
git tag -a v1.0.0 -m "Release v1.0.0"
```

### Step 6: Push and Create Release

```bash
# Push branch and tags
git push origin release/v1.0.0
git push origin v1.0.0

# Create GitHub release (or use CI/CD)
# Include release notes and migration guide (if MAJOR)
```

### Step 7: Post-Release

1. **Merge release branch to main**:
   ```bash
   git checkout main
   git merge release/v1.0.0
   git push origin main
   ```

2. **Update development version**:
   - Update version to next development version (e.g., `1.1.0-dev`)
   - Reset `CHANGELOG.md` `[Unreleased]` section

3. **Notify agents**:
   - Update agent compatibility matrix
   - Notify agents about new version

## Release Simulation

Test release process without creating actual release:

```bash
# Simulate release
bash scripts/simulate_release.sh v1.0.0

# Check results
cat reports/release_simulation_v1.0.0.json
```

**Simulation checks**:
- Version consistency across all artifacts
- Schema validation
- State validation
- HMAC chain integrity
- Documentation completeness
- CI/CD gate compatibility

## Emergency Releases

For critical security fixes:

1. **Create hotfix branch**:
   ```bash
   git checkout -b hotfix/v1.0.1
   ```

2. **Apply fix and update PATCH version**

3. **Run minimal validation**:
   ```bash
   bash scripts/validate_state.sh
   ```

4. **Create release** (follow normal process)

5. **Merge to main and release branch**

## Version Gates

### SLO Verification Gate (CP3+/Release)

**Purpose**: Verify Service Level Objectives (SLO) are met before release

**Blocks release if**:
- SLO verification tests fail (load, chaos, overload)
- SLO metrics missing or invalid
- SLO alert rules invalid or thresholds incorrect
- SLO documentation incomplete or inaccurate

**CI/CD Integration**:
- **Workflow**: `.github/workflows/slo-verification.yml`
- **Triggers**: Pull requests, pushes to main/develop, manual dispatch
- **Mode**: Advisory (PRs) or Blocking (releases)
- **Artifacts**: SLO verification summary JSON uploaded to GitHub Actions artifacts

**Checks**:
1. **Gate 1: SLO Verification Tests**:
   - Load tests (10,000 messages, success rate ≥ 99.9%)
   - Chaos tests (success rate ≥ 99.9% excluding NATS outages)
   - Overload tests (backpressure triggers correctly)

2. **Gate 2: Metrics Verification**:
   - Router metrics exist and queryable
   - Gateway metrics exist and queryable
   - SLI queries return valid results

3. **Gate 3: Alert Rules Verification**:
   - Alert rules syntax valid
   - Alert thresholds match SLO targets

4. **Gate 4: Documentation Verification**:
   - SLO/SLI definitions complete
   - Test coverage documented
   - Pre-release gates documented

**When SLO Gate Should Be Green**:
- ✅ All SLO targets met in test environment
- ✅ All metrics verified and queryable
- ✅ All alert rules valid
- ✅ Documentation complete

**What to Do If SLO Gate Is Red**:
1. **Review SLO Verification Summary**: Check GitHub Actions artifacts for detailed results
2. **Identify Failed Gates**: Determine which gates failed (tests, metrics, alerts, docs)
3. **Fix Issues**:
   - **Tests Failed**: Fix test failures, review SLO targets, adjust test configuration
   - **Metrics Missing**: Add missing metrics, verify Prometheus configuration
   - **Alerts Invalid**: Fix alert rule syntax, adjust thresholds to match SLO targets
   - **Docs Incomplete**: Complete SLO/SLI documentation, update test coverage
4. **Re-run Verification**: Re-run SLO verification workflow after fixes
5. **Consume Error Budget** (if applicable): If SLO targets cannot be met, document error budget consumption
6. **Postpone Release** (if blocking): If SLO gates fail in blocking mode, postpone release until all gates pass

**Error Budget Consumption**:
- If SLO targets cannot be met, document error budget consumption in release notes
- Calculate error budget: `Error Budget = (1 - SLO) × Measurement Window`
- Example: If SLO = 99.9% and SLI = 99.8%, error budget consumed = 0.1% of measurement window

**See**: `docs/ARCHITECTURE/SLI_SLO_ROUTER_GATEWAY.md` for complete SLO/SLI definitions and error budget management.

### Schema Changes Gate

**Blocks release if**:
- Schema changed without version bump
- Version changed without manifest update
- MAJOR change without migration script
- MINOR change without documentation update

**Check**:
```bash
bash scripts/check_schema_changes.sh
```

### State Validation Gate

**Blocks release if**:
- `no_drift: false`
- Invalid JSON
- Schema validation fails
- Checksum mismatch
- HMAC chain broken

**Check**:
```bash
bash scripts/validate_state.sh
```

### Documentation Gate

**Blocks release if**:
- HMAC values not masked in docs
- Missing migration guide (for MAJOR)
- Incomplete CHANGELOG

**Check**:
```bash
bash scripts/check_hmac_masking.sh docs/
```

## Release Templates

### Release Notes Template

See `.github/RELEASE_NOTES_TEMPLATE.md` for release notes format.

### Version Bump Script

Use `scripts/release_bump_version.sh` to automate version updates:

```bash
# Bump MAJOR version
bash scripts/release_bump_version.sh major

# Bump MINOR version
bash scripts/release_bump_version.sh minor

# Bump PATCH version
bash scripts/release_bump_version.sh patch
```

## References

- **Schema Versioning**: `docs/SCHEMA_VERSIONING.md`
- **PR Checklist**: `docs/archive/dev/PR_CHECKLIST.md`
- **Manifest**: `.trae/manifest.json`
- **CHANGELOG**: `CHANGELOG.md`


