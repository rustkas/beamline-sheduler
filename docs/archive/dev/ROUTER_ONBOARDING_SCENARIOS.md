# Router Onboarding Scenarios

## Purpose

This document provides guided flows for typical Router development and operational scenarios. Use this as a starting point for new Erlang developers or on-call engineers working with Router.

**Target Audience**:
- New Erlang developers joining the Router team
- On-call engineers diagnosing Router issues
- QA engineers running Router tests
- DevOps engineers setting up Router CI/CD

## Quick Navigation

- [Scenario 1: Dev Feature Flow](#scenario-1-dev-feature-flow) - Adding a new feature and verifying it
- [Scenario 2: Incident Flow](#scenario-2-incident-flow) - Diagnosing CP/DevState issues
- [Scenario 3: Contract Flow](#scenario-3-contract-flow) - Verifying Gateway↔Router contract
- [Scenario 4: Test Profile Flow](#scenario-4-test-profile-flow) - Profiling test suites for CI decisions

---

## Scenario 1: Dev Feature Flow

**Goal**: Add a new feature to Router and verify it works correctly.

**When to use**: You're implementing a new Router feature (e.g., new routing policy, new telemetry event, new error handling).

### Prerequisites

- Router development environment set up
- Erlang/OTP 26+ installed
- rebar3 installed
- DevState service running (if feature interacts with DevState)
- Access to Router documentation

**Verify prerequisites**:
```bash
# Check Erlang version
erl -version

# Check rebar3
rebar3 version

# Check DevState (if needed)
curl -fsS http://localhost:3080/health
```

### Step-by-Step Flow

#### Step 1: Understand the Feature Requirements

1. **Read relevant documentation**:
   - `docs/CP1_BASELINE.md` - CP1 baseline features
   - `docs/CP1_ROUTER_SPEC.md` - Router specification
   - `docs/ARCHITECTURE/` - Architecture decisions
   - Relevant ADRs in `docs/ADR/`

2. **Check if feature is CP1 or CP2+**:
   ```bash
   # Check current CP
   cat .trae/state.json | jq '.current_cp'
   
   # If CP1-LC, ensure feature is in CP1 baseline
   # If CP2-LC, feature can use CP2+ capabilities
   ```

3. **Review existing similar features**:
   - Look for similar implementations in `apps/otp/router/src/`
   - Check test patterns in `apps/otp/router/test/`

#### Step 2: Implement the Feature

1. **Create/modify source files**:
   ```bash
   cd apps/otp/router/src
   # Create new module or modify existing
   ```

2. **Follow Router coding standards**:
   - Use structured logging: `router_logger:info/2`
   - Emit telemetry events: `router_telemetry_helper:execute/3`
   - Handle errors gracefully: `router_error:to_grpc/1`
   - Filter PII from logs: `router_logger:filter_pii/1`

3. **Update documentation** (if needed):
   - Update relevant ADRs
   - Update API contracts if changing interfaces
   - Update observability docs if adding new metrics

#### Step 3: Write Tests

1. **Create test suite** (if new feature):
   ```bash
   cd apps/otp/router/test
   # Create new_SUITE.erl following existing patterns
   ```

2. **Mark test suite with tags**:
   ```erlang
   %% @test_category fast  % or slow, cp1_smoke, etc.
   -module(new_SUITE).
   ```

3. **Run fast tests locally**:
   ```bash
   cd apps/otp/router
   ./scripts/test_fast.sh
   ```

4. **Run CP1 smoke tests** (if CP1 feature):
   ```bash
   cd apps/otp/router
   ./scripts/test_cp1_smoke.sh
   ```

#### Step 4: Verify Contract Compliance

1. **Run contract smoke tests**:
   ```bash
   ./scripts/gateway_router_contract_smoke.sh --router-only
   ```

2. **Check for contract violations**:
   - Review test output for any contract violations
   - Verify headers (trace_id, tenant_id, version) pass through correctly

#### Step 5: Generate Test Profile (Optional)

1. **Profile your test suite**:
   ```bash
   ./scripts/router_test_profile.sh --suites new_SUITE
   ```

2. **Check execution time**:
   - If > 30 seconds, consider optimization
   - If > 5 minutes, mark as `slow` and exclude from fast CI

#### Step 6: Run Regression Snapshot (Optional)

1. **Generate baseline snapshot**:
   ```bash
   ./scripts/router_regression_snapshot.sh --baseline
   ```

2. **Compare after changes**:
   ```bash
   ./scripts/router_regression_snapshot.sh --compare reports/router/snapshots/baseline_file.md
   ```

#### Step 7: Verify DevState Integration (If Applicable)

1. **Run DevState fallback smoke tests**:
   ```bash
   ./scripts/devstate_router_fallback_smoke.sh --scenario all
   ```

2. **Verify CP1 minimal mode** (if CP2+ feature):
   ```bash
   cd apps/otp/router
   rebar3 ct --suite router_cp1_minimal_mode_SUITE
   ```

### Expected Results

✅ **All fast tests pass**:
- No compilation errors
- All test cases pass
- No contract violations

✅ **Feature works as expected**:
- Feature behaves according to specification
- Telemetry events emitted correctly
- Logs are structured and PII-filtered

✅ **Documentation updated**:
- ADRs updated (if architectural change)
- API contracts updated (if interface change)
- Observability docs updated (if new metrics)

### Troubleshooting

**Issue**: Tests fail with compilation errors
- **Solution**: Check Erlang syntax, ensure all dependencies are available
- **Command**: `rebar3 compile`

**Issue**: Contract smoke tests fail
- **Solution**: Verify message structure matches `API_CONTRACTS.md`
- **Reference**: `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md`

**Issue**: DevState tests fail
- **Solution**: Ensure DevState service is running: `make devstate-up`
- **Reference**: `docs/archive/dev/ROUTER_DEVSTATE_E2E_SCENARIOS.md`

### References

- `docs/CP1_BASELINE.md` - CP1 baseline features
- `docs/CP1_ROUTER_SPEC.md` - Router specification
- `docs/archive/dev/ROUTER_DEV_QUICKSTART.md` - Quick start guide
- `docs/TEST_CLASSIFICATION.md` - Test classification
- `apps/otp/router/scripts/test_fast.sh` - Fast test runner
- `apps/otp/router/scripts/test_cp1_smoke.sh` - CP1 smoke test runner

---

## Scenario 2: Incident Flow

**Goal**: Diagnose and resolve Router issues related to CP/DevState, contract violations, or performance problems.

**When to use**: Router is behaving unexpectedly, alerts are firing, or users report issues.

### Prerequisites

- Access to Router logs (structured JSON)
- Access to Router metrics (Prometheus/Grafana or Telemetry)
- Access to DevState service
- Access to alert rules and dashboards
- Basic understanding of Router architecture

**Verify prerequisites**:
```bash
# Check Router logs location
ls -la .windsurf/reports/router_*.jsonl

# Check DevState health
curl -fsS http://localhost:3080/health

# Check alert rules
cat docs/observability/router-alert-rules.yaml
```

### Step-by-Step Flow

#### Step 1: Gather Initial Information

1. **Check alert rules**:
   ```bash
   # Review active alerts
   cat docs/observability/router-alert-rules.yaml
   
   # Common alert types:
   # - RouterCPStateErrors: DevState/CP state errors
   # - RouterContractViolations: NATS contract violations
   # - RouterCAFHighRetryRate: CAF adapter issues
   # - RouterJetStreamRedeliveryHigh: JetStream redelivery issues
   ```

2. **Check Router logs for errors**:
   ```bash
   # Find recent errors
   grep -i "error\|failed\|exception" .windsurf/reports/router_*.jsonl | tail -20
   
   # Check for CP fallback
   grep "cp_fallback\|CP1-baseline" .windsurf/reports/router_*.jsonl | tail -20
   
   # Check for contract violations
   grep "contract_violation" .windsurf/reports/router_*.jsonl | tail -20
   ```

3. **Check DevState status**:
   ```bash
   # Verify DevState is running
   curl -fsS http://localhost:3080/health
   
   # Check HMAC chain
   bash devstate/scripts/devstate_verify.sh
   
   # Check current CP
   cat .trae/state.json | jq '.current_cp, .no_drift'
   ```

#### Step 2: Diagnose CP/DevState Issues

1. **Run DevState fallback smoke tests**:
   ```bash
   # Test missing state scenario
   ./scripts/devstate_router_fallback_smoke.sh --scenario missing_state
   
   # Test invalid JSON scenario
   ./scripts/devstate_router_fallback_smoke.sh --scenario invalid_json
   
   # Test no_drift=false scenario
   ./scripts/devstate_router_fallback_smoke.sh --scenario no_drift_false
   
   # Run all scenarios
   ./scripts/devstate_router_fallback_smoke.sh --scenario all
   ```

2. **Check Router state detection**:
   ```bash
   cd apps/otp/router
   # Use erl to check Router state
   erl -pa _build/default/lib/*/ebin -eval "router_state:check_and_log_cp_state()." -noshell
   ```

3. **Verify CP1 minimal mode** (if CP2+ features unexpectedly enabled):
   ```bash
   cd apps/otp/router
   rebar3 ct --suite router_cp1_minimal_mode_SUITE
   ```

#### Step 3: Diagnose Contract Violations

1. **Check NATS contract validation**:
   ```bash
   # Look for contract violations in logs
   grep "contract_violation" .windsurf/reports/router_*.jsonl | jq '.'
   
   # Check contract validation metrics
   # (if Prometheus available)
   curl http://localhost:9090/api/v1/query?query=router_nats_contract_violations_total
   ```

2. **Run contract smoke tests**:
   ```bash
   ./scripts/gateway_router_contract_smoke.sh --router-only
   ```

3. **Verify message structure**:
   - Check `docs/API_CONTRACTS.md` for expected message formats
   - Check `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` for NATS mapping
   - Verify headers (trace_id, tenant_id, version) are present

#### Step 4: Diagnose Performance Issues

1. **Generate test profile**:
   ```bash
   # Profile slow tests to identify bottlenecks
   ./scripts/router_test_profile.sh --slow
   ```

2. **Check regression snapshot**:
   ```bash
   # Compare current state against baseline
   ./scripts/router_regression_snapshot.sh --compare reports/router/snapshots/baseline_file.md
   ```

3. **Review metrics**:
   - Check `router_assignment_retry_total` (CAF adapter retries)
   - Check `router_jetstream_redelivery_total` (JetStream redelivery)
   - Check `router_cp_state_errors_total` (DevState errors)

#### Step 5: Diagnose CAF Adapter Issues

1. **Check CAF adapter metrics**:
   ```bash
   # Look for retry exhaustion
   grep "retry_exhausted\|publish_failures" .windsurf/reports/router_*.jsonl
   
   # Check alert rules
   cat docs/observability/router-alert-rules.yaml | grep -A 10 "RouterCAF"
   ```

2. **Run CAF adapter load threshold tests**:
   ```bash
   cd apps/otp/router
   rebar3 ct --suite router_caf_adapter_load_thresholds_SUITE
   ```

3. **Review ADR-011**:
   - Check `docs/ADR/ADR-011-jetstream-e2e.md` for CAF adapter behavior
   - Understand timeout handling and retry logic

### Expected Results

✅ **Root cause identified**:
- Clear understanding of the issue
- Relevant logs and metrics collected
- Test results confirm the issue

✅ **Issue resolved or workaround applied**:
- Fix implemented or workaround documented
- Tests pass after fix
- Monitoring/alerting updated if needed

### Troubleshooting

**Issue**: DevState not running
- **Solution**: Start DevState service: `make devstate-up`
- **Verify**: `curl -fsS http://localhost:3080/health`

**Issue**: HMAC chain broken
- **Solution**: Re-export state from database: `bash devstate/scripts/devstate_export.sh`
- **Reference**: `devstate/docs/IDE_INTEGRATION.md`

**Issue**: Contract violations increasing
- **Solution**: Check Gateway and Router message formats match `API_CONTRACTS.md`
- **Reference**: `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md`

**Issue**: High retry rate
- **Solution**: Check CAF service health, review timeout settings
- **Reference**: `docs/ADR/ADR-011-jetstream-e2e.md`

### References

- `docs/observability/router-alert-rules.yaml` - Alert rules
- `docs/archive/dev/ROUTER_DEVSTATE_E2E_SCENARIOS.md` - DevState scenarios
- `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md` - Contract smoke tests
- `docs/ADR/ADR-011-jetstream-e2e.md` - JetStream E2E and CAF adapter
- `docs/ADR/ADR-015-router-devstate-integration.md` - DevState integration
- `scripts/devstate_router_fallback_smoke.sh` - DevState smoke tests

---

## Scenario 3: Contract Flow

**Goal**: Verify that Gateway↔Router contract is correct and error handling follows the contract.

**When to use**: After Gateway or Router changes, before releases, or when investigating contract-related issues.

### Prerequisites

- Router compiled and ready to test
- Gateway code available (optional, for full E2E)
- Understanding of API contracts
- Access to contract documentation

**Verify prerequisites**:
```bash
# Check Router is compiled
cd apps/otp/router && rebar3 compile

# Check contract documentation
cat docs/API_CONTRACTS.md | head -50
```

### Step-by-Step Flow

#### Step 1: Understand the Contract

1. **Read contract documentation**:
   - `docs/API_CONTRACTS.md` - API contracts and message formats
   - `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Protobuf to NATS mapping
   - `docs/NATS_SUBJECTS.md` - NATS subjects and headers

2. **Review error response format**:
   ```bash
   # Check ErrorResponse structure
   cat docs/API_CONTRACTS.md | grep -A 20 "ErrorResponse"
   ```

3. **Review contract smoke test documentation**:
   - `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md` - Contract smoke test guide

#### Step 2: Run Basic Contract Smoke Tests

1. **Run Router contract tests**:
   ```bash
   # Run Router-side contract tests only
   ./scripts/gateway_router_contract_smoke.sh --router-only
   ```

2. **Verify test results**:
   - All tests should pass
   - Check test logs: `apps/otp/router/_build/test/logs/`

#### Step 3: Run Error Contract Tests

1. **Run error contract smoke tests**:
   ```bash
   cd apps/otp/router
   # Run contract smoke suite (includes error scenarios)
   rebar3 ct --suite router_gateway_contract_smoke_SUITE
   ```

2. **Verify error scenarios**:
   - Invalid request (missing fields) → `invalid_request` error
   - Wrong version → `invalid_request` error
   - Tenant rejected → `unauthorized` or `invalid_request` error
   - Internal error → `internal` or `decision_failed` error

3. **Check error response structure**:
   - Verify `ErrorResponse` matches `API_CONTRACTS.md`
   - Verify HTTP status code mapping (Gateway responsibility)
   - Verify error codes are standardized

#### Step 4: Verify Header Propagation

1. **Check headers in contract tests**:
   ```bash
   # Contract tests verify:
   # - trace_id passes through
   # - tenant_id passes through
   # - version passes through
   # - Nats-Msg-Id is set by NATS
   ```

2. **Review header documentation**:
   - `docs/NATS_SUBJECTS.md` - Header requirements
   - `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md` - Header verification

#### Step 5: Run Full E2E Contract Test (If Gateway Available)

1. **Run full E2E test**:
   ```bash
   # Run both Router and Gateway tests
   ./scripts/gateway_router_contract_smoke.sh --full
   ```

2. **Verify end-to-end flow**:
   - Gateway sends `DecideRequest` via NATS
   - Router processes and returns `DecideResponse`
   - Gateway receives and validates response

#### Step 6: Check for Contract Violations

1. **Review contract violation metrics**:
   ```bash
   # Check logs for contract violations
   grep "contract_violation" .windsurf/reports/router_*.jsonl | jq '.'
   ```

2. **Run NATS contract validation tests**:
   ```bash
   cd apps/otp/router
   rebar3 ct --suite router_nats_contract_validation_SUITE
   ```

### Expected Results

✅ **All contract tests pass**:
- Router contract tests pass
- Error contract tests pass
- Header propagation verified

✅ **No contract violations**:
- No violations in logs
- Metrics show zero violations (in normal operation)

✅ **Error responses follow contract**:
- Error codes match `API_CONTRACTS.md`
- Error messages are clear and helpful
- HTTP status codes mapped correctly (Gateway)

### Troubleshooting

**Issue**: Contract tests fail
- **Solution**: Check message structure matches `API_CONTRACTS.md`
- **Reference**: `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md`

**Issue**: Headers not propagating
- **Solution**: Verify Router extracts headers from NATS message headers (not payload)
- **Reference**: `docs/NATS_SUBJECTS.md`

**Issue**: Error responses don't match contract
- **Solution**: Check `router_nats_subscriber:build_error_response/3` implementation
- **Reference**: `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md#error-contract-scenarios`

### References

- `docs/API_CONTRACTS.md` - API contracts and message formats
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Protobuf to NATS mapping
- `docs/NATS_SUBJECTS.md` - NATS subjects and headers
- `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md` - Contract smoke test guide
- `scripts/gateway_router_contract_smoke.sh` - Contract smoke test script
- `apps/otp/router/test/router_gateway_contract_smoke_SUITE.erl` - Contract test suite

---

## Scenario 4: Test Profile Flow

**Goal**: Profile test suites to understand execution time and make CI pipeline decisions.

**When to use**: Setting up CI/CD pipelines, optimizing test execution, or investigating slow tests.

### Prerequisites

- Router compiled
- rebar3 installed
- Understanding of test classification

**Verify prerequisites**:
```bash
# Check Router is compiled
cd apps/otp/router && rebar3 compile

# Check test classification
cat apps/otp/router/docs/TEST_CLASSIFICATION.md | head -50
```

### Step-by-Step Flow

#### Step 1: Understand Test Classification

1. **Read test classification**:
   - `apps/otp/router/docs/TEST_CLASSIFICATION.md` - Test categories and tags
   - Understand: `fast`, `slow`, `cp1_smoke`, `jetstream`, `property`, `load`

2. **Review test profile documentation**:
   - `docs/archive/dev/ROUTER_TEST_PROFILE.md` - Test profile guide

#### Step 2: Profile Fast Tests

1. **Profile fast test suites**:
   ```bash
   ./scripts/router_test_profile.sh --fast
   ```

2. **Review profile report**:
   ```bash
   # Report saved to: reports/router/test_profiles/test_profile_fast_*.md
   cat reports/router/test_profiles/test_profile_fast_*.md
   ```

3. **Identify fast CI candidates**:
   - Suites with duration < 30 seconds
   - Total duration < 5 minutes (for fast CI)

#### Step 3: Profile CP1 Smoke Tests

1. **Profile CP1 smoke tests**:
   ```bash
   ./scripts/router_test_profile.sh --cp1-smoke
   ```

2. **Review profile report**:
   - Check total duration
   - Verify all suites pass
   - Identify optimization targets

#### Step 4: Profile Slow Tests

1. **Profile slow test suites**:
   ```bash
   ./scripts/router_test_profile.sh --slow
   ```

2. **Review profile report**:
   - Identify suites with duration > 5 minutes
   - Check for optimization opportunities
   - Plan for nightly CI only

#### Step 5: Profile All Tests

1. **Profile all test suites**:
   ```bash
   ./scripts/router_test_profile.sh --all
   ```

2. **Review comprehensive profile**:
   - Total duration for full test suite
   - Success rate
   - Recommendations for CI pipeline

#### Step 6: Make CI Pipeline Decisions

1. **For Fast CI (PR checks)**:
   - Select suites with duration < 30 seconds
   - Total duration should be < 5 minutes
   - Focus on CP1 smoke tests and fast contract tests

2. **For Nightly CI (comprehensive)**:
   - Include all test suites
   - Total duration acceptable for nightly run (< 30 minutes)
   - Monitor duration trends over time

3. **For Optimization**:
   - Identify suites with duration > 10 minutes
   - Consider splitting large suites
   - Optimize slow test cases

### Expected Results

✅ **Profile reports generated**:
- Reports saved to `reports/router/test_profiles/`
- Clear duration and test count information
- Recommendations provided

✅ **CI pipeline decisions made**:
- Fast CI suite list defined
- Nightly CI suite list defined
- Optimization targets identified

### Troubleshooting

**Issue**: Profile script fails
- **Solution**: Check prerequisites (Router compiled, rebar3 available)
- **Reference**: `docs/archive/dev/ROUTER_TEST_PROFILE.md`

**Issue**: Tests take too long
- **Solution**: Review profile report for optimization targets
- **Reference**: `docs/archive/dev/ROUTER_TEST_PROFILE.md#optimization-targets`

### References

- `docs/archive/dev/ROUTER_TEST_PROFILE.md` - Test profile guide
- `apps/otp/router/docs/TEST_CLASSIFICATION.md` - Test classification
- `scripts/router_test_profile.sh` - Test profile script
- `apps/otp/router/scripts/test_fast.sh` - Fast test runner
- `apps/otp/router/scripts/test_cp1_smoke.sh` - CP1 smoke test runner

---

## Quick Reference

### Common Commands

```bash
# Fast tests
cd apps/otp/router && ./scripts/test_fast.sh

# CP1 smoke tests
cd apps/otp/router && ./scripts/test_cp1_smoke.sh

# Contract smoke tests
./scripts/gateway_router_contract_smoke.sh --router-only

# DevState fallback tests
./scripts/devstate_router_fallback_smoke.sh --scenario all

# Test profile
./scripts/router_test_profile.sh --fast

# Regression snapshot
./scripts/router_regression_snapshot.sh --baseline
```

### Key Documentation

- `docs/CP1_BASELINE.md` - CP1 baseline features
- `docs/CP1_ROUTER_SPEC.md` - Router specification
- `docs/API_CONTRACTS.md` - API contracts
- `docs/archive/dev/ROUTER_DEV_QUICKSTART.md` - Quick start guide
- `docs/archive/dev/ROUTER_DEVSTATE_E2E_SCENARIOS.md` - DevState scenarios
- `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md` - Contract smoke tests
- `docs/archive/dev/ROUTER_TEST_PROFILE.md` - Test profile guide
- `docs/archive/dev/ROUTER_REGRESSION_SNAPSHOTS.md` - Regression snapshots
- `docs/archive/dev/CI_PROFILES_ROUTER.md` - CI pipeline profiles (fast/extended/nightly)

### Key Scripts

- `scripts/gateway_router_contract_smoke.sh` - Contract smoke tests
- `scripts/devstate_router_fallback_smoke.sh` - DevState fallback tests
- `scripts/router_test_profile.sh` - Test profiling
- `scripts/router_regression_snapshot.sh` - Regression snapshots
- `apps/otp/router/scripts/test_fast.sh` - Fast test runner
- `apps/otp/router/scripts/test_cp1_smoke.sh` - CP1 smoke test runner

---

## Getting Help

If you're stuck or need clarification:

1. **Check documentation**: Start with `docs/archive/dev/ROUTER_DEV_QUICKSTART.md`
2. **Run with --help**: All scripts support `--help` flag
3. **Review ADRs**: Check `docs/ADR/` for architectural decisions
4. **Check test examples**: Look at existing test suites in `apps/otp/router/test/`

## Next Steps

After completing a scenario:

1. **Document your findings**: Update relevant documentation if you discover gaps
2. **Share knowledge**: Update this document if you find better workflows
3. **Optimize**: Use profile data to optimize slow tests
4. **Automate**: Consider automating repetitive steps in CI/CD

