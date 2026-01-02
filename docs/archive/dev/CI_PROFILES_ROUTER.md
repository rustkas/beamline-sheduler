# Router CI Pipeline Profiles

## Purpose

This document defines three CI pipeline profiles for Router testing, based on test classification, profiling data, and operational requirements. These profiles enable efficient CI/CD workflows with appropriate test coverage for different scenarios.

**Target Audience**:
- DevOps engineers setting up CI/CD pipelines
- Release managers planning test strategies
- Developers understanding CI test coverage

## CI Profiles Overview

| Profile | When to Use | Duration | Test Coverage |
|---------|-------------|----------|---------------|
| **fast** | Every PR | < 5 minutes | CP1 smoke + fast contract tests |
| **extended** | Merge to main/staging | 10-20 minutes | CP1 smoke + selected slow/JetStream + DevState |
| **nightly** | Scheduled (daily) | 30-60 minutes | All tests including load, property, fault injection |

## Profile 1: Fast CI (PR Checks)

**Goal**: Quick feedback on code changes for every pull request.

**When to use**:
- Every PR to main/develop branches
- Feature branch validation
- Quick local validation before pushing

**Duration**: < 5 minutes (target: 3-5 minutes)

### Commands and Scripts

```bash
# 1. Fast test suites (CP1 smoke + fast contract tests)
cd apps/otp/router
make test-fast
# Or: ./scripts/test_fast.sh

# 2. Contract smoke test (Router-side only)
./scripts/gateway_router_contract_smoke.sh --router-only

# 3. Minimal regression snapshot (baseline generation, optional)
./scripts/router_regression_snapshot.sh --baseline
```

### Test Suites Included

**Fast Test Suites** (15 suites, ~2-3 minutes):
- `router_core_SUITE` - Core routing decisions
- `router_e2e_smoke_SUITE` - End-to-end smoke test
- `router_rbac_SUITE` - Role-Based Access Control
- `router_policy_enforcement_SUITE` - Policy enforcement
- `router_decider_SUITE` - Decision engine
- `router_policy_store_SUITE` - Policy store operations
- `router_error_SUITE` - Error mapping
- `router_grpc_SUITE` - gRPC service
- `router_grpc_integration_SUITE` - gRPC integration
- `router_caf_adapter_unit_SUITE` - CAF adapter unit tests
- `router_core_telemetry_contract_SUITE` - Telemetry contract
- `router_secrets_logging_SUITE` - Secrets masking
- `router_nats_contract_validation_SUITE` - NATS contract validation
- `router_gateway_contract_smoke_SUITE` - Gateway contract smoke
- `router_cp1_minimal_mode_SUITE` - CP1 minimal mode enforcement

**Contract Tests**:
- Router contract smoke test (error scenarios included)

### Expected Results

✅ **All fast tests pass**:
- No compilation errors
- All test cases pass
- No contract violations

✅ **Contract compliance verified**:
- DecideRequest/DecideResponse structure matches contract
- Error responses follow contract
- Headers propagate correctly

### GitHub Actions Example

```yaml
router-fast-ci:
  name: Router Fast CI (PR Checks)
  runs-on: ubuntu-latest
  if: github.event_name == 'pull_request'
  steps:
    - uses: actions/checkout@v4
    - uses: erlef/setup-beam@v1
      with:
        otp-version: '26.0'
    
    - name: Compile Router
      working-directory: apps/otp/router
      run: rebar3 compile
    
    - name: Run Fast Tests
      working-directory: apps/otp/router
      run: make test-fast
    
    - name: Run Contract Smoke Test
      working-directory: .
      run: ./scripts/gateway_router_contract_smoke.sh --router-only
```

### Makefile Target

```makefile
test-fast-ci:
	@echo "Running fast CI profile..."
	@cd apps/otp/router && make test-fast
	@./scripts/gateway_router_contract_smoke.sh --router-only
```

---

## Profile 2: Extended CI (Merge to Main/Staging)

**Goal**: Comprehensive validation before merging to main or staging branches.

**When to use**:
- Merge to main branch
- Merge to staging/release branches
- Pre-release validation
- After significant changes

**Duration**: 10-20 minutes (target: 15 minutes)

### Commands and Scripts

```bash
# 1. CP1 smoke tests (minimal baseline)
cd apps/otp/router
make test-cp1-smoke
# Or: ./scripts/test_cp1_smoke.sh

# 2. Selected slow/JetStream tests (critical E2E scenarios)
cd apps/otp/router
rebar3 ct --suite router_jetstream_e2e_SUITE \
          --suite router_result_consumer_SUITE \
          --suite router_caf_adapter_SUITE \
          --suite router_jetstream_fault_injection_SUITE

# 3. DevState fallback smoke (all scenarios)
./scripts/devstate_router_fallback_smoke.sh --scenario all

# 4. Contract smoke test (full)
./scripts/gateway_router_contract_smoke.sh --full

# 5. CAF adapter load threshold tests
cd apps/otp/router
rebar3 ct --suite router_caf_adapter_load_thresholds_SUITE

# 6. Regression snapshot (compare against baseline)
./scripts/router_regression_snapshot.sh --compare reports/router/snapshots/baseline_file.md
```

### Test Suites Included

**CP1 Smoke Tests** (7 suites, ~2-3 minutes):
- All CP1 baseline test suites

**Selected Slow/JetStream Tests** (4 suites, ~8-12 minutes):
- `router_jetstream_e2e_SUITE` - Critical JetStream E2E scenarios
- `router_result_consumer_SUITE` - Result consumer with JetStream
- `router_caf_adapter_SUITE` - CAF adapter with JetStream
- `router_jetstream_fault_injection_SUITE` - Fault injection tests

**CAF Adapter Load Tests** (1 suite, ~2-3 minutes):
- `router_caf_adapter_load_thresholds_SUITE` - CAF load thresholds

**DevState Tests** (all scenarios, ~3-5 minutes):
- Missing state file
- Invalid JSON
- no_drift=false

**Contract Tests** (full, ~1-2 minutes):
- Router + Gateway contract tests (if available)

### Expected Results

✅ **All CP1 smoke tests pass**:
- CP1 baseline functionality verified

✅ **Critical JetStream scenarios pass**:
- E2E scenarios work correctly
- Fault injection handled gracefully

✅ **DevState integration verified**:
- All fallback scenarios work correctly
- Recovery after state restoration works

✅ **Contract compliance verified**:
- Full contract test suite passes
- Error scenarios follow contract

✅ **No regressions detected**:
- Regression snapshot comparison shows no significant changes

### GitHub Actions Example

```yaml
router-extended-ci:
  name: Router Extended CI (Merge to Main)
  runs-on: ubuntu-latest
  if: github.event_name == 'push' && github.ref == 'refs/heads/main'
  steps:
    - uses: actions/checkout@v4
    - uses: erlef/setup-beam@v1
      with:
        otp-version: '26.0'
    
    - name: Setup DevState
      run: make devstate-up
      env:
        DATABASE_URL: ${{ secrets.DATABASE_URL }}
        HMAC_SECRET: ${{ secrets.HMAC_SECRET }}
    
    - name: Compile Router
      working-directory: apps/otp/router
      run: rebar3 compile
    
    - name: Run CP1 Smoke Tests
      working-directory: apps/otp/router
      run: make test-cp1-smoke
    
    - name: Run Selected JetStream Tests
      working-directory: apps/otp/router
      run: |
        rebar3 ct --suite router_jetstream_e2e_SUITE \
                  --suite router_result_consumer_SUITE \
                  --suite router_caf_adapter_SUITE \
                  --suite router_jetstream_fault_injection_SUITE
    
    - name: Run CAF Adapter Load Tests
      working-directory: apps/otp/router
      run: rebar3 ct --suite router_caf_adapter_load_thresholds_SUITE
    
    - name: Run DevState Fallback Smoke
      working-directory: .
      run: ./scripts/devstate_router_fallback_smoke.sh --scenario all
    
    - name: Run Contract Smoke Test
      working-directory: .
      run: ./scripts/gateway_router_contract_smoke.sh --full
```

### Makefile Target

```makefile
test-extended-ci:
	@echo "Running extended CI profile..."
	@cd apps/otp/router && make test-cp1-smoke
	@cd apps/otp/router && rebar3 ct --suite router_jetstream_e2e_SUITE --suite router_result_consumer_SUITE --suite router_caf_adapter_SUITE --suite router_jetstream_fault_injection_SUITE
	@cd apps/otp/router && rebar3 ct --suite router_caf_adapter_load_thresholds_SUITE
	@./scripts/devstate_router_fallback_smoke.sh --scenario all
	@./scripts/gateway_router_contract_smoke.sh --full
```

---

## Profile 3: Nightly CI (Full Suite)

**Goal**: Complete test coverage including all slow tests, property-based tests, and load tests.

**When to use**:
- Scheduled daily runs
- Pre-production validation
- Release candidate validation
- Performance regression detection

**Duration**: 30-60 minutes (target: 45 minutes)

### Commands and Scripts

```bash
# 1. All fast tests
cd apps/otp/router
make test-fast

# 2. All slow tests (JetStream E2E, property, load)
cd apps/otp/router
make test-slow
# Or: ./scripts/test_slow.sh

# 3. All JetStream tests (including fault injection)
cd apps/otp/router
./scripts/router_test_profile.sh --jetstream

# 4. DevState fallback smoke (all scenarios)
./scripts/devstate_router_fallback_smoke.sh --scenario all

# 5. Contract smoke test (full)
./scripts/gateway_router_contract_smoke.sh --full

# 6. CAF adapter load threshold tests
cd apps/otp/router
rebar3 ct --suite router_caf_adapter_load_thresholds_SUITE

# 7. Generate comprehensive test profile
./scripts/router_test_profile.sh --all

# 8. Regression snapshot (compare against baseline)
./scripts/router_regression_snapshot.sh --compare reports/router/snapshots/baseline_file.md
```

### Test Suites Included

**All Fast Tests** (15 suites, ~2-3 minutes):
- All fast test suites from Profile 1

**All Slow Tests** (13+ suites, ~25-40 minutes):
- `router_jetstream_e2e_SUITE` - Heavy JetStream E2E tests
- `router_delivery_count_tracking_SUITE` - Delivery count tracking
- `router_result_consumer_SUITE` - Result consumer
- `router_caf_adapter_SUITE` - CAF adapter
- `router_caf_adapter_enhanced_SUITE` - Enhanced CAF adapter
- `router_nats_subscriber_caf_SUITE` - NATS/JetStream integration
- `router_jetstream_fault_injection_SUITE` - Fault injection tests
- `router_caf_adapter_load_thresholds_SUITE` - CAF load thresholds
- `router_decider_prop_SUITE` - Property-based tests (decider)
- `router_policy_store_prop_SUITE` - Property-based tests (policy store)
- `router_normalize_boolean_prop_SUITE` - Property-based tests (boolean)
- `router_options_merge_prop_SUITE` - Property-based tests (options)
- `router_policy_store_load_SUITE` - Load tests

**DevState Tests** (all scenarios, ~3-5 minutes):
- All DevState fallback scenarios

**Contract Tests** (full, ~1-2 minutes):
- Full contract test suite

**Test Profiling** (~5-10 minutes):
- Comprehensive test profile generation

### Expected Results

✅ **All tests pass**:
- Fast tests pass
- Slow tests pass
- Property-based tests pass
- Load tests pass

✅ **Complete coverage verified**:
- All functionality tested
- All edge cases covered
- Performance within acceptable limits

✅ **No regressions detected**:
- Regression snapshot comparison shows no significant changes
- Test profile shows acceptable performance

### GitHub Actions Example

```yaml
router-nightly-ci:
  name: Router Nightly CI (Full Suite)
  runs-on: ubuntu-latest
  if: github.event_name == 'schedule' || github.event_name == 'workflow_dispatch'
  steps:
    - uses: actions/checkout@v4
    - uses: erlef/setup-beam@v1
      with:
        otp-version: '26.0'
    
    - name: Setup DevState
      run: make devstate-up
      env:
        DATABASE_URL: ${{ secrets.DATABASE_URL }}
        HMAC_SECRET: ${{ secrets.HMAC_SECRET }}
    
    - name: Compile Router
      working-directory: apps/otp/router
      run: rebar3 compile
    
    - name: Run All Fast Tests
      working-directory: apps/otp/router
      run: make test-fast
    
    - name: Run All Slow Tests
      working-directory: apps/otp/router
      run: make test-slow
    
    - name: Run DevState Fallback Smoke
      working-directory: .
      run: ./scripts/devstate_router_fallback_smoke.sh --scenario all
    
    - name: Run Contract Smoke Test
      working-directory: .
      run: ./scripts/gateway_router_contract_smoke.sh --full
    
    - name: Generate Test Profile
      working-directory: .
      run: ./scripts/router_test_profile.sh --all
    
    - name: Upload Test Profile
      uses: actions/upload-artifact@v4
      if: always()
      with:
        name: router-test-profile-nightly
        path: reports/router/test_profiles/
        retention-days: 30
```

### Makefile Target

```makefile
test-nightly-ci:
	@echo "Running nightly CI profile..."
	@cd apps/otp/router && make test-fast
	@cd apps/otp/router && make test-slow
	@./scripts/devstate_router_fallback_smoke.sh --scenario all
	@./scripts/gateway_router_contract_smoke.sh --full
	@./scripts/router_test_profile.sh --all
```

---

## Profile Comparison Table

| Aspect | Fast CI | Extended CI | Nightly CI |
|--------|---------|-------------|------------|
| **Trigger** | Every PR | Merge to main/staging | Scheduled (daily) |
| **Duration** | < 5 min | 10-20 min | 30-60 min |
| **Fast Tests** | ✅ All (15 suites) | ✅ CP1 smoke (7 suites) | ✅ All (15 suites) |
| **Slow Tests** | ❌ None | ✅ Selected (4 suites) | ✅ All (13+ suites) |
| **JetStream E2E** | ❌ None | ✅ Critical scenarios | ✅ All scenarios |
| **Property Tests** | ❌ None | ❌ None | ✅ All (4 suites) |
| **Load Tests** | ❌ None | ❌ None | ✅ All (1 suite) |
| **Fault Injection** | ❌ None | ✅ Basic | ✅ All |
| **CAF Load Tests** | ❌ None | ✅ Yes | ✅ Yes |
| **DevState Smoke** | ❌ None | ✅ All scenarios | ✅ All scenarios |
| **Contract Tests** | ✅ Router-only | ✅ Full | ✅ Full |
| **Test Profile** | ⚠️ Optional | ⚠️ Optional | ✅ Yes |
| **Regression Snapshot** | ⚠️ Optional | ✅ Compare | ✅ Compare |

## Recommendations

### When to Use Fast CI

**Use for**:
- Every pull request
- Feature branch validation
- Quick local validation before pushing
- Pre-commit hooks (optional)

**Don't use for**:
- Release validation
- Performance testing
- Comprehensive regression testing

### When to Use Extended CI

**Use for**:
- Merge to main branch
- Merge to staging/release branches
- Pre-release validation
- After significant architectural changes
- Before creating release candidates

**Don't use for**:
- Every PR (too slow)
- Property-based testing (not included)
- Load testing (not included)

### When to Use Nightly CI

**Use for**:
- Scheduled daily runs
- Pre-production validation
- Release candidate validation
- Performance regression detection
- Comprehensive test coverage verification

**Don't use for**:
- PR validation (too slow)
- Quick feedback loops

## Implementation Strategy

### Phase 1: Documentation (Current)

1. ✅ Document CI profiles (this document)
2. ✅ Define commands and scripts for each profile
3. ✅ Provide GitHub Actions examples

### Phase 2: CI Refactoring (Optional)

1. **Update `.github/workflows/ci.yml`**:
   - Add `fast` profile for PR checks
   - Add `extended` profile for merge to main
   - Add `nightly` profile for scheduled runs

2. **Create Makefile targets**:
   - `make test-fast-ci` - Fast CI profile
   - `make test-extended-ci` - Extended CI profile
   - `make test-nightly-ci` - Nightly CI profile

3. **Add workflow triggers**:
   - Fast CI: `pull_request`
   - Extended CI: `push` to main/staging
   - Nightly CI: `schedule` (daily at 2 AM UTC)

### Phase 3: Optimization (Future)

1. **Parallelize test execution**:
   - Run fast tests in parallel
   - Run slow tests in separate jobs

2. **Test result caching**:
   - Cache test results for unchanged code
   - Skip tests if only documentation changed

3. **Incremental testing**:
   - Run only affected test suites based on code changes
   - Use test profiling to identify critical paths

## Monitoring and Metrics

### Key Metrics to Track

1. **Test Duration**:
   - Fast CI: Target < 5 minutes
   - Extended CI: Target 10-20 minutes
   - Nightly CI: Target 30-60 minutes

2. **Test Pass Rate**:
   - Fast CI: > 95% (critical for PR flow)
   - Extended CI: > 90% (some flaky tests acceptable)
   - Nightly CI: > 85% (comprehensive coverage)

3. **Test Coverage**:
   - Fast CI: CP1 baseline + contracts
   - Extended CI: CP1 + critical JetStream
   - Nightly CI: Full coverage

### Alerting

**Set up alerts for**:
- Fast CI duration > 10 minutes (blocking PRs)
- Extended CI duration > 30 minutes (blocking merges)
- Nightly CI duration > 90 minutes (needs optimization)
- Test pass rate < 80% (investigate failures)

## Troubleshooting

### Fast CI Taking Too Long

**Symptoms**: Fast CI duration > 5 minutes

**Solutions**:
1. Profile test suites: `./scripts/router_test_profile.sh --fast`
2. Identify slow suites and optimize
3. Consider excluding some suites from fast CI
4. Parallelize test execution

### Extended CI Failing

**Symptoms**: Extended CI tests failing intermittently

**Solutions**:
1. Check DevState service health
2. Verify NATS/JetStream availability
3. Review test logs for flaky tests
4. Consider retry logic for flaky tests

### Nightly CI Timeout

**Symptoms**: Nightly CI exceeds time limit

**Solutions**:
1. Profile all tests: `./scripts/router_test_profile.sh --all`
2. Identify and optimize slowest suites
3. Consider splitting into multiple jobs
4. Run property/load tests separately

## References

- `docs/archive/dev/ROUTER_TEST_PROFILE.md` - Test profiling guide
- `apps/otp/router/docs/TEST_CLASSIFICATION.md` - Test classification
- `docs/archive/dev/ROUTER_ONBOARDING_SCENARIOS.md` - Onboarding scenarios
- `apps/otp/router/Makefile` - Makefile targets
- `scripts/router_test_profile.sh` - Test profiling script
- `scripts/devstate_router_fallback_smoke.sh` - DevState smoke tests
- `scripts/gateway_router_contract_smoke.sh` - Contract smoke tests
- `scripts/router_regression_snapshot.sh` - Regression snapshots

## Quick Reference

### Fast CI Commands

```bash
cd apps/otp/router && make test-fast
./scripts/gateway_router_contract_smoke.sh --router-only
```

### Extended CI Commands

```bash
cd apps/otp/router && make test-cp1-smoke
cd apps/otp/router && rebar3 ct --suite router_jetstream_e2e_SUITE --suite router_result_consumer_SUITE --suite router_caf_adapter_SUITE --suite router_jetstream_fault_injection_SUITE
cd apps/otp/router && rebar3 ct --suite router_caf_adapter_load_thresholds_SUITE
./scripts/devstate_router_fallback_smoke.sh --scenario all
./scripts/gateway_router_contract_smoke.sh --full
```

### Nightly CI Commands

```bash
cd apps/otp/router && make test-fast
cd apps/otp/router && make test-slow
./scripts/devstate_router_fallback_smoke.sh --scenario all
./scripts/gateway_router_contract_smoke.sh --full
./scripts/router_test_profile.sh --all
```

