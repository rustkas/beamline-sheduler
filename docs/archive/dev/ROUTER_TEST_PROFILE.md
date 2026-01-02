# Router Test Profile

## Purpose

This document describes the Router Test Profile tool, which measures execution time and size for Router test suites. The profile helps identify heavy test suites for optimization and make informed decisions about CI pipeline configuration.

## Overview

The test profile tool:
- Runs test suites and measures execution time
- Collects test statistics (total tests, passed, failed, skipped)
- Generates Markdown reports with recommendations
- Helps identify which tests to run in fast CI vs nightly CI

## Usage

### Basic Usage

```bash
# Show help and available options
./scripts/router_test_profile.sh --help

# Profile fast test suites (default)
./scripts/router_test_profile.sh --fast

# Profile CP1 smoke test suites
./scripts/router_test_profile.sh --cp1-smoke

# Profile slow test suites (JetStream E2E, property, load)
./scripts/router_test_profile.sh --slow

# Profile JetStream E2E test suites
./scripts/router_test_profile.sh --jetstream

# Profile all test suites
./scripts/router_test_profile.sh --all

# Profile specific test suites
./scripts/router_test_profile.sh --suites router_core_SUITE,router_e2e_smoke_SUITE
```

### Help Output

```bash
$ ./scripts/router_test_profile.sh --help
Router Test Profile Script

DESCRIPTION:
  Profiles Router test suites to measure execution time and size.
  Helps identify heavy test suites for optimization and CI pipeline decisions.

USAGE:
  ./scripts/router_test_profile.sh [OPTIONS]

OPTIONS:
  --fast              Profile fast test suites (default)
  --cp1-smoke         Profile CP1 smoke test suites
  --slow              Profile slow test suites (JetStream E2E, property, load)
  --jetstream         Profile JetStream E2E test suites
  --all               Profile all test suites (fast + slow)
  --suites SUITE1,SUITE2  Profile specific test suites (comma-separated)
  -h, --help          Show this help message

EXAMPLES:
  # Profile fast test suites
  ./scripts/router_test_profile.sh --fast

  # Profile CP1 smoke test suites
  ./scripts/router_test_profile.sh --cp1-smoke

  # Profile slow test suites
  ./scripts/router_test_profile.sh --slow

  # Profile JetStream E2E test suites
  ./scripts/router_test_profile.sh --jetstream

  # Profile all test suites
  ./scripts/router_test_profile.sh --all

  # Profile specific test suites
  ./scripts/router_test_profile.sh --suites router_core_SUITE,router_e2e_smoke_SUITE

EXIT CODES:
  0  - Success (profile generated)
  1  - Test execution failed (one or more tests failed)
  2  - Prerequisites not met (see error message for details)
  3  - Invalid arguments (see usage above)

OUTPUT:
  Profile report saved to: reports/router/test_profiles/
  Format: test_profile_<mode>_<commit>_<timestamp>.md

For more information, see: docs/archive/dev/ROUTER_TEST_PROFILE.md
```

## Profile Report Format

### Report Structure

The profile report is saved as a Markdown file in `reports/router/test_profiles/` with the format:
```
test_profile_<mode>_<commit>_<timestamp>.md
```

Example: `test_profile_fast_abc123_2025-01-27_12-00-00.md`

### Report Contents

1. **Header**: Generation timestamp, commit hash, branch, mode, total suites
2. **Summary**: Purpose and usage recommendations
3. **Test Suite Profiles**: For each suite:
   - Duration (seconds)
   - Total tests
   - Passed tests
   - Failed tests
   - Skipped tests
   - Exit code
4. **Summary Statistics**: Total duration, total suites, failed suites, success rate
5. **Recommendations**: Guidance for CI pipeline decisions

### Example Report

```markdown
# Router Test Profile Report

**Generated**: 2025-01-27T12:00:00Z
**Commit**: abc123
**Branch**: main
**Mode**: fast
**Total Suites**: 15

## Summary

This profile measures execution time and size for Router test suites.
Use this data to:
- Identify heavy test suites for optimization
- Make CI pipeline decisions (fast CI vs nightly)
- Track test performance over time

## Test Suite Profiles

### router_core_SUITE

- **Duration**: 12.34 seconds
- **Total Tests**: 25
- **Passed**: 25
- **Failed**: 0
- **Skipped**: 0
- **Exit Code**: 0

### router_e2e_smoke_SUITE

- **Duration**: 8.56 seconds
- **Total Tests**: 10
- **Passed**: 10
- **Failed**: 0
- **Skipped**: 0
- **Exit Code**: 0

...

## Summary Statistics

- **Total Duration**: 180.50 seconds (3.01 minutes)
- **Total Suites**: 15
- **Failed Suites**: 0
- **Success Rate**: 100.0%

## Recommendations

### For Fast CI (PR checks)
- Run suites with duration < 30 seconds
- Focus on CP1 smoke tests and fast contract tests

### For Nightly CI (comprehensive)
- Run all test suites including slow/JetStream E2E tests
- Monitor duration trends over time

### For Optimization
- Identify suites with duration > 5 minutes
- Consider splitting large suites or optimizing slow tests
```

## Reading the Profile

### Key Metrics

1. **Duration**: Execution time per suite (in seconds)
   - Fast tests: < 30 seconds
   - Slow tests: > 5 minutes

2. **Total Tests**: Number of test cases in the suite
   - Helps identify large suites that might benefit from splitting

3. **Success Rate**: Percentage of suites that passed
   - Should be 100% for stable codebase

4. **Total Duration**: Sum of all suite durations
   - Helps estimate CI pipeline time

### Interpreting Results

**Fast Tests (< 30 seconds)**:
- Suitable for fast CI (PR checks)
- Should run on every commit
- Examples: `router_core_SUITE`, `router_e2e_smoke_SUITE`

**Slow Tests (> 5 minutes)**:
- Suitable for nightly CI only
- May need optimization or splitting
- Examples: `router_jetstream_e2e_SUITE`, `router_policy_store_load_SUITE`

**Medium Tests (30 seconds - 5 minutes)**:
- Can be included in fast CI if total duration is acceptable
- Monitor for performance regressions
- Examples: Some integration tests

## Using Profile for CI Decisions

### Fast CI (PR Checks)

**Goal**: Quick feedback on code changes (< 5 minutes total)

**Recommended Suites**:
- CP1 smoke tests (7 suites, ~2-3 minutes total)
- Fast contract tests (8 suites, ~1-2 minutes total)

**Command**:
```bash
./scripts/router_test_profile.sh --cp1-smoke
```

**Decision Criteria**:
- Total duration < 5 minutes
- All suites pass consistently
- Covers critical functionality

### Nightly CI (Comprehensive)

**Goal**: Full test coverage including slow tests

**Recommended Suites**:
- All fast tests
- All slow tests (JetStream E2E, property, load)

**Command**:
```bash
./scripts/router_test_profile.sh --all
```

**Decision Criteria**:
- Total duration acceptable for nightly run (< 30 minutes)
- All suites pass consistently
- Covers all functionality

### Optimization Targets

**Identify Heavy Suites**:
```bash
# Profile slow tests to find optimization targets
./scripts/router_test_profile.sh --slow
```

**Look for**:
- Suites with duration > 10 minutes
- Suites with many test cases (> 100)
- Suites with high failure rate

**Optimization Strategies**:
1. Split large suites into smaller ones
2. Parallelize independent tests
3. Optimize slow test cases
4. Use mocks instead of real services where possible

## Integration with CI

### GitHub Actions Example

```yaml
# Fast CI (PR checks)
test-fast:
  name: Fast Tests
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v3
    - uses: erlef/setup-beam@v1
      with:
        otp-version: '26.0'
    - name: Run Fast Tests
      run: |
        cd apps/otp/router
        rebar3 ct --suite router_core_SUITE --suite router_e2e_smoke_SUITE

# Nightly CI (comprehensive)
test-nightly:
  name: All Tests
  runs-on: ubuntu-latest
  if: github.event_name == 'schedule' || github.event_name == 'workflow_dispatch'
  steps:
    - uses: actions/checkout@v3
    - uses: erlef/setup-beam@v1
      with:
        otp-version: '26.0'
    - name: Generate Test Profile
      run: ./scripts/router_test_profile.sh --all
    - name: Upload Profile Report
      uses: actions/upload-artifact@v3
      with:
        name: test-profile
        path: reports/router/test_profiles/
```

## Troubleshooting

### Exit Code 2 (Prerequisites not met)

**Common Issues**:
- Router directory not found
- Router not compiled
- rebar3 not found

**Solutions**:
```bash
# Compile Router
cd apps/otp/router && rebar3 compile

# Verify rebar3
rebar3 version
```

### Exit Code 1 (Test execution failed)

**Common Issues**:
- One or more test suites failed
- Test environment not set up correctly

**Solutions**:
- Check test logs: `apps/otp/router/_build/test/logs/`
- Verify prerequisites (DevState, NATS, etc.)
- Run individual suites to isolate failures

### Exit Code 3 (Invalid arguments)

**Common Issues**:
- Invalid mode specified
- Invalid suite names

**Solutions**:
- Use `--help` to see available options
- Check suite names match actual test files
- Verify mode is one of: `fast`, `cp1-smoke`, `slow`, `jetstream`, `all`

## Best Practices

1. **Regular Profiling**: Run profiles periodically to track performance trends
2. **Baseline Comparison**: Compare profiles before/after major changes
3. **CI Integration**: Include profile generation in nightly CI
4. **Documentation**: Update CI pipeline decisions based on profile data
5. **Optimization**: Use profile data to identify optimization targets

## References

- `scripts/router_test_profile.sh`: Profile generation script
- `docs/TEST_CLASSIFICATION.md`: Test classification and tags
- `apps/otp/router/scripts/test_fast.sh`: Fast test runner
- `apps/otp/router/scripts/test_slow.sh`: Slow test runner
- `docs/archive/dev/CI_PROFILES_ROUTER.md`: CI pipeline profiles (fast/extended/nightly)

