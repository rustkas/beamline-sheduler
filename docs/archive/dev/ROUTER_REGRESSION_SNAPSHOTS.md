# Router Regression Snapshot Reports

## Purpose

This document describes the framework for generating snapshot reports of Router's key metrics and behavior patterns. These snapshots help catch regressions between releases by providing a baseline for comparison.

## Overview

The snapshot framework:
- Runs minimal test scenarios (reuses existing E2E smoke tests)
- Collects key metrics and log patterns
- Saves snapshots with commit hash and timestamp
- Enables comparison between baseline and current state

## Usage

### Generate Baseline Snapshot

Generate a baseline snapshot after a stable release or before making significant changes:

```bash
# Show help and available options
./scripts/router_regression_snapshot.sh --help

# Generate baseline snapshot
./scripts/router_regression_snapshot.sh --baseline
./scripts/router_regression_snapshot.sh  # (same as --baseline)
```

This will:
1. Run minimal test scenarios (CP1 smoke tests, contract smoke tests)
2. Collect metrics snapshot
3. Collect log patterns snapshot
4. Collect test results snapshot
5. Save to `reports/router/snapshots/router_snapshot_{commit}_{timestamp}.md`

**Example output file**: `reports/router/snapshots/router_snapshot_a1b2c3d_2025-01-27_12-00-00.md`

**Help Output Example**:
```bash
$ ./scripts/router_regression_snapshot.sh --help
Router Regression Snapshot Report

DESCRIPTION:
  Generates a snapshot report of key Router metrics and log patterns to help
  catch regressions between releases. Captures:
  - DevState/CP state metrics
  - NATS contract validation metrics
  - JetStream metrics (redelivery, MaxDeliver)
  - Processing metrics (results, errors)
  - CAF adapter metrics
  - Log patterns (cp_fallback, contract_violation, MaxDeliver, errors)
  - Test results summary

USAGE:
  ./scripts/router_regression_snapshot.sh [OPTIONS]

OPTIONS:
  --baseline              Generate a new baseline snapshot (default)
  --compare BASELINE_FILE Compare current state against a baseline snapshot
  -h, --help              Show this help message

EXAMPLES:
  # Generate a baseline snapshot
  ./scripts/router_regression_snapshot.sh --baseline
  ./scripts/router_regression_snapshot.sh  # (same as --baseline)

  # Compare current state against a baseline
  ./scripts/router_regression_snapshot.sh --compare reports/router/snapshots/router_snapshot_abc123_2025-01-27_12-00-00.md

EXIT CODES:
  0  - Success (snapshot generated or comparison completed)
  1  - Test failure (tests failed during snapshot generation)
  2  - Prerequisites not met (see error message for details)
  3  - Invalid arguments (see usage above)

TROUBLESHOOTING:
  Exit code 2 (Prerequisites not met):
    - Router directory not found: Check that apps/otp/router exists
    - Router not compiled: Run 'cd apps/otp/router && rebar3 compile'
    - rebar3 not found: Install Erlang/OTP and rebar3

  Exit code 3 (Invalid arguments):
    - Check that mode is one of: --baseline, --compare BASELINE_FILE
    - For --compare, ensure baseline file path is provided and file exists
    - Use --help to see all available options

OUTPUT:
  Snapshots are saved to: reports/router/snapshots/
  Format: router_snapshot_<commit>_<timestamp>.md

For more information, see: docs/archive/dev/ROUTER_REGRESSION_SNAPSHOTS.md
```

### Compare Snapshots

Compare current state against a baseline:

```bash
./scripts/router_regression_snapshot.sh --compare reports/router/snapshots/router_snapshot_a1b2c3d_2025-01-27_12-00-00.md
```

This will:
1. Generate current snapshot
2. Compare key numbers between baseline and current
3. Show differences in:
   - CP Fallback patterns
   - Contract violations
   - MaxDeliver exhaustion
   - Error logs

## Snapshot Contents

### 1. Metrics Snapshot

Documents key metrics that should be monitored:

- **DevState/CP State Metrics**:
  - `router_cp_state_errors_total`: Should be 0 in normal operation
  - `router_cp_state_fallback_total`: Should be 0 in normal operation

- **NATS Contract Validation Metrics**:
  - `router_nats_contract_violations_total`: Should be 0 in normal operation

- **JetStream Metrics**:
  - `router_jetstream_redelivery_total`: Monitor for backpressure
  - `router_jetstream_maxdeliver_exhausted_total`: Should be 0 (indicates message loss)

- **Processing Metrics**:
  - `router_results_total`: Total results processed
  - `router_results_parse_failed_total`: Should be minimal
  - `router_results_tenant_rejected_total`: Should be minimal

- **CAF Adapter Metrics**:
  - `router_assignment_retry_total`: Monitor for CAF overload
  - `router_retry_exhausted_total`: Should be 0 (indicates CAF unavailable)
  - `router_assignment_publish_failures_total`: Should be minimal

**Note**: In CP1, metrics are collected from telemetry events and logs. Future versions may include a `/metrics` endpoint.

### 2. Log Patterns Snapshot

Searches log files for key patterns:

- **CP Fallback Patterns**: Searches for `cp_fallback`, `CP1-baseline`
  - **Expected**: 0 occurrences in normal operation
  - **Warning**: Non-zero indicates DevState issues

- **Contract Violation Patterns**: Searches for `contract_violation`, `violation`
  - **Expected**: 0 occurrences in normal operation
  - **Warning**: Non-zero indicates NATS header contract violations

- **MaxDeliver Exhaustion Patterns**: Searches for `MaxDeliver`, `maxdeliver_exhausted`
  - **Expected**: 0 occurrences in normal operation
  - **Warning**: Non-zero indicates message loss (messages exceeded MaxDeliver limit)

- **Error Patterns**: Searches for `ERROR`, `error`, `failed`
  - **Expected**: Minimal (only expected errors)
  - **Warning**: High count indicates processing issues

### 3. Test Results Snapshot

Summarizes test execution:

- Total test suites run
- Test pass/fail counts per suite
- Overall test health

## Comparing Snapshots

### Manual Comparison

1. **Open both snapshot files** (baseline and current)
2. **Compare key numbers**:
   - CP Fallback occurrences: Should be same or lower
   - Contract violations: Should be same or lower
   - MaxDeliver exhaustions: Should be same or lower
   - Error log count: Should be same or lower
   - Test pass rate: Should be same or higher

3. **Look for regressions**:
   - **⚠️ INCREASED**: Metric increased (potential regression)
   - **✅ DECREASED**: Metric decreased (improvement)
   - **➡️ SAME**: Metric unchanged (no change)

### Automated Comparison

Use the `--compare` mode:

```bash
./scripts/router_regression_snapshot.sh --compare BASELINE_FILE
```

The script will:
1. Generate current snapshot
2. Extract key numbers from both snapshots
3. Display comparison table
4. Highlight increases (potential regressions)

### Key Numbers to Monitor

1. **CP State Errors**: Should be 0 in normal operation
   - Increase indicates DevState validation issues

2. **Contract Violations**: Should be 0 in normal operation
   - Increase indicates NATS header format issues

3. **MaxDeliver Exhaustions**: Should be 0 in normal operation
   - Increase indicates message loss (critical)

4. **Test Pass Rate**: Should be 100%
   - Decrease indicates test failures (regression)

5. **Error Log Count**: Should be minimal
   - Increase indicates processing issues

## Integration with CI

### Optional CI Stage

Add to `.github/workflows/ci.yml`:

```yaml
  router-regression-snapshot:
    name: Router Regression Snapshot
    runs-on: ubuntu-latest
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v3
      - name: Setup Erlang
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'
      - name: Run Snapshot
        run: ./scripts/router_regression_snapshot.sh --baseline
      - name: Upload Snapshot
        uses: actions/upload-artifact@v3
        with:
          name: router-snapshot
          path: reports/router/snapshots/*.md
```

### Pre-Release Snapshot

Generate snapshot before release:

```bash
# Before release
./scripts/router_regression_snapshot.sh --baseline

# After release, compare
./scripts/router_regression_snapshot.sh --compare reports/router/snapshots/router_snapshot_{commit}_{timestamp}.md
```

## Limitations

1. **Metrics Collection**: In CP1, metrics are collected from logs. Future versions may include a `/metrics` endpoint for direct metric collection.

2. **Log File Location**: Script searches for logs in `.windsurf/reports/`. Adjust `LOG_DIR` in script if logs are in a different location.

3. **Test Coverage**: Snapshot uses minimal test scenarios (CP1 smoke tests). Full regression testing requires running all test suites.

4. **Manual Comparison**: Detailed comparison requires manual review of snapshot files. Automated comparison provides only high-level numbers.

## Future Enhancements

1. **Metrics Endpoint**: Add `/metrics` endpoint to Router for direct metric collection
2. **Automated Diff**: Generate detailed diff report between snapshots
3. **Trend Analysis**: Track metrics over time across multiple snapshots
4. **Alerting**: Automatically alert on significant metric increases

## References

- `scripts/router_regression_snapshot.sh`: Snapshot generation script
- `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`: Router metrics documentation
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`: NATS contract documentation
- `docs/NATS_SUBJECTS.md`: NATS subjects and headers

