# Router Full Test Suite CI/CD Integration

**Date**: 2025-01-27  
**Status**: ✅ Implementation Complete  
**Purpose**: Regular execution of Router test suite (e2e, chaos, load) with real NATS and artifact preservation for CP2 readiness verification.

**AS-IS NOTE (repo audit)**:

- This document (and `scripts/run_router_full_test_suite.sh`) reference suites such as `router_intake_e2e_SUITE`, `router_intake_chaos_SUITE`, and `router_cp2_features_e2e_SUITE`.
- In the current repository state, some of these suites are **missing** from `apps/otp/router/test/`, and some exist only as **placeholders** (empty suites).
- JetStream-related suites that do exist (e.g. `router_jetstream_e2e_integration_SUITE.erl`) frequently run with `nats_mode=mock` and mock `router_nats`.

Treat the “all tests with real NATS” claim as **OUTDATED** until the runner script and suite inventory are reconciled.

## Overview

The Router Full Test Suite provides automated, regular execution of all Router tests with real NATS infrastructure, generating comprehensive test reports and artifacts for CP2 readiness verification.

## Components

### 1. Test Suite Runner Script (`scripts/run_router_full_test_suite.sh`)

**Purpose**: Unified script to run all Router test suites (e2e, chaos, load) with real NATS.

**Features**:
- Prerequisites checking (NATS availability, Erlang/OTP, rebar3)
- Configurable test execution (e2e-only, chaos-only, load-only, or all)
- Comprehensive logging and artifact generation
- JSON and Markdown summary reports
- Environment information capture

**Usage**:
```bash
# Run all tests
./scripts/run_router_full_test_suite.sh

# Run only E2E tests
./scripts/run_router_full_test_suite.sh --e2e-only

# Run only Chaos tests
./scripts/run_router_full_test_suite.sh --chaos-only

# Run only Load tests
./scripts/run_router_full_test_suite.sh --load-only

# Custom output directory
./scripts/run_router_full_test_suite.sh --output-dir /path/to/reports
```

**Environment Variables**:
- `NATS_URL`: NATS server URL (default: `nats://localhost:4222`)
- `ROUTER_TEST_TIMEOUT`: Test timeout in seconds (default: `300`)
- `LOAD_TEST_MESSAGE_COUNT`: Number of messages for load tests (default: `2000`)
- `LOAD_TEST_PARALLEL_WORKERS`: Number of parallel workers (default: `1`)

**Generated Artifacts**:
- `test-run.log`: Complete test execution log
- `e2e-test.log`: E2E test execution log
- `chaos-test.log`: Chaos test execution log
- `load-test.log`: Load test execution log
- `test-summary.json`: Machine-readable test summary
- `test-summary.md`: Human-readable test summary
- `environment.txt`: Environment information
- `*-status.txt`: Individual test suite status files
- `*-ct-logs/`: Common Test logs for each suite

### 2. GitHub Actions Workflow (`.github/workflows/router-full-test-suite.yml`)

**Purpose**: Automated CI/CD integration for regular test execution.

**Triggers**:
1. **Scheduled (Nightly)**: Runs daily at 2 AM UTC
2. **Manual**: `workflow_dispatch` with configurable options
3. **Push to main/master**: On Router code changes (optional)
4. **Pull Request**: On Router code changes (optional)

**Configuration**:
- Uses NATS service container (Docker)
- Sets up Erlang/OTP 26.2
- Installs rebar3 and NATS CLI
- Configurable test suite selection
- Configurable load test parameters

**Artifacts**:
- **Test Results**: Complete test logs and Common Test output (30 days retention)
- **Test Summary**: JSON and Markdown summaries (90 days retention)
- **GitHub Step Summary**: Inline test results in GitHub Actions UI

**Manual Trigger Options**:
- `test_suite`: `all`, `e2e-only`, `chaos-only`, `load-only`
- `message_count`: Load test message count (default: `2000`)
- `parallel_workers`: Load test parallel workers (default: `1`)

## Test Suites

### E2E Tests (`router_intake_e2e_SUITE`)

**Purpose**: End-to-end validation of Router message intake, validation, and error handling.

**Coverage**:
- Message validation (schema, version, correlation fields)
- Tenant validation and ACL
- Idempotency handling
- DLQ publication
- Audit logging
- Metrics emission
- Error response format
- Network flakiness scenarios
- NATS unavailability scenarios
- Partial DLQ failure scenarios

**Execution**:
```bash
cd apps/otp/router
rebar3 ct --suite router_intake_e2e_SUITE
```

AS-IS: `router_intake_e2e_SUITE.erl` was **not found** in `apps/otp/router/test/` during repository audit.

### Chaos Tests (`router_intake_chaos_SUITE`)

**Purpose**: Resilience testing with NATS failures and restarts.

**Coverage**:
- Single NATS restart (mild chaos)
- Multiple NATS restarts (moderate chaos)
- Randomized NATS failures (hard chaos)
- NATS failure during active processing
- Recovery verification after NATS restart

**Execution**:
```bash
cd apps/otp/router
rebar3 ct --suite router_intake_chaos_SUITE
```

AS-IS: `router_intake_chaos_SUITE.erl` exists but is currently a **placeholder** (empty suite).

**Prerequisites**:
- NATS running in Docker (for `docker stop/start` commands)
- `nats_chaos.sh` script available

### Load Tests (`router_intake_e2e_SUITE` - `load_tests` group)

**Purpose**: High-volume stress testing to verify stability and correctness.

**Coverage**:
- High-volume success flood (1000-10000 valid messages)
- High-volume error flood (1000-10000 invalid messages)
- Mixed success/error stream (70% valid, 30% invalid)
- Idempotency stress (repeating idempotency keys)

**Execution**:
```bash
cd apps/otp/router
LOAD_TEST_MESSAGE_COUNT=2000 LOAD_TEST_PARALLEL_WORKERS=1 \
  rebar3 ct --suite router_intake_e2e_SUITE --group load_tests
```

AS-IS: suite referenced here is **not present**; load tests may exist under other suite names.

**Configuration**:
- `LOAD_TEST_MESSAGE_COUNT`: Number of messages (default: `2000`, max: `10000`)
- `LOAD_TEST_PARALLEL_WORKERS`: Parallel workers (default: `1`, max: `10`)

## Artifact Structure

```
reports/router-test-results/
└── YYYYMMDD_HHMMSS/
    ├── test-run.log                    # Complete execution log
    ├── test-summary.json               # Machine-readable summary
    ├── test-summary.md                 # Human-readable summary
    ├── environment.txt                 # Environment information
    ├── e2e-status.txt                  # E2E test status
    ├── e2e-test.log                    # E2E test log
    ├── e2e-ct-logs/                    # Common Test logs
    ├── chaos-status.txt                # Chaos test status
    ├── chaos-test.log                  # Chaos test log
    ├── chaos-ct-logs/                  # Common Test logs
    ├── load-status.txt                 # Load test status
    ├── load-test.log                   # Load test log
    └── load-ct-logs/                   # Common Test logs
```

## Test Summary Format

### JSON Summary (`test-summary.json`)

```json
{
  "timestamp": "20250127_020000",
  "overall_status": "PASSED",
  "test_suites": {
    "e2e": {
      "status": "PASSED",
      "enabled": true
    },
    "chaos": {
      "status": "PASSED",
      "enabled": true
    },
    "load": {
      "status": "PASSED",
      "enabled": true
    }
  },
  "configuration": {
    "nats_url": "nats://localhost:4222",
    "timeout": "600",
    "load_test_message_count": "2000",
    "load_test_parallel_workers": "1"
  },
  "artifacts": {
    "report_dir": "/path/to/reports/router-test-results/20250127_020000",
    "log_file": "/path/to/reports/router-test-results/20250127_020000/test-run.log"
  }
}
```

### Markdown Summary (`test-summary.md`)

```markdown
# Router Test Suite Results

**Timestamp**: 20250127_020000  
**Overall Status**: **PASSED**

## Test Suite Status

| Suite | Status | Enabled |
|-------|--------|---------|
| E2E Tests | PASSED | true |
| Chaos Tests | PASSED | true |
| Load Tests | PASSED | true |

## Configuration

- **NATS URL**: nats://localhost:4222
- **Test Timeout**: 600s
- **Load Test Message Count**: 2000
- **Load Test Parallel Workers**: 1

## Artifacts

- **Report Directory**: `reports/router-test-results/20250127_020000`
- **Test Log**: `reports/router-test-results/20250127_020000/test-run.log`
- ...
```

## Local Execution

### Prerequisites

1. **NATS Server**: Running on `localhost:4222`
   ```bash
   docker-compose up -d nats
   # or
   nats-server
   ```

2. **Erlang/OTP**: Version 26.2 or compatible
   ```bash
   erl -version
   ```

3. **Rebar3**: Latest version
   ```bash
   rebar3 version
   ```

4. **NATS CLI**: For NATS health checks
   ```bash
   go install github.com/nats-io/natscli/nats@latest
   ```

### Running Tests Locally

```bash
# Run all tests
./scripts/run_router_full_test_suite.sh

# Run with custom configuration
NATS_URL=nats://localhost:4222 \
  LOAD_TEST_MESSAGE_COUNT=5000 \
  LOAD_TEST_PARALLEL_WORKERS=5 \
  ./scripts/run_router_full_test_suite.sh

# Run only E2E tests
./scripts/run_router_full_test_suite.sh --e2e-only
```

## CI/CD Integration

### GitHub Actions

The workflow is automatically triggered:
- **Nightly**: Daily at 2 AM UTC
- **Manual**: Via GitHub Actions UI
- **On Push/PR**: When Router code changes (optional)

### Manual Trigger

1. Go to GitHub Actions → "Router Full Test Suite"
2. Click "Run workflow"
3. Select:
   - Test suite: `all`, `e2e-only`, `chaos-only`, or `load-only`
   - Message count: Number of messages for load tests
   - Parallel workers: Number of parallel workers
4. Click "Run workflow"

### Artifact Download

1. Go to GitHub Actions → "Router Full Test Suite" → Latest run
2. Scroll to "Artifacts" section
3. Download:
   - `router-test-results-*`: Complete test logs (30 days)
   - `router-test-summary-*`: Test summaries (90 days)

## CP2 Readiness Verification

The test suite provides evidence for CP2 readiness:

1. **E2E Tests**: Verify production-ready intake validation and error handling
2. **Chaos Tests**: Verify resilience to NATS failures
3. **Load Tests**: Verify stability under high volumes
4. **Artifacts**: Preserve test results for audit and compliance

### Success Criteria

- ✅ All E2E tests pass
- ✅ All Chaos tests pass (Router recovers from NATS failures)
- ✅ All Load tests pass (no memory leaks, correct metrics)
- ✅ Test artifacts preserved for 30-90 days
- ✅ Test summary shows `overall_status: "PASSED"`

## Troubleshooting

### NATS Not Available

**Error**: `NATS server not available at nats://localhost:4222`

**Solution**:
```bash
# Start NATS with Docker Compose
docker-compose up -d nats

# Or start NATS server directly
nats-server
```

### Test Timeout

**Error**: Tests timeout after 300 seconds

**Solution**:
```bash
# Increase timeout
ROUTER_TEST_TIMEOUT=600 ./scripts/run_router_full_test_suite.sh
```

### Load Tests Fail

**Error**: Load tests fail with memory/process errors

**Solution**:
- Reduce `LOAD_TEST_MESSAGE_COUNT`
- Reduce `LOAD_TEST_PARALLEL_WORKERS`
- Check system resources (memory, CPU)

### Chaos Tests Fail

**Error**: Chaos tests fail with Docker errors

**Solution**:
- Ensure Docker is running
- Ensure NATS container is accessible
- Check `nats_chaos.sh` script permissions

## References

- `scripts/run_router_full_test_suite.sh`: Test suite runner script
- `.github/workflows/router-full-test-suite.yml`: GitHub Actions workflow
- `apps/otp/router/test/router_intake_e2e_SUITE.erl`: E2E test suite
- `apps/otp/router/test/router_intake_chaos_SUITE.erl`: Chaos test suite
- `scripts/nats_chaos.sh`: NATS chaos orchestration script
- `docs/archive/dev/ROUTER_INTAKE_E2E_TEST_CHECKLIST.md`: E2E test checklist
- `docs/archive/dev/ROUTER_CHAOS_TESTS_SPEC.md`: Chaos tests specification
- `docs/archive/dev/ROUTER_INTAKE_LOAD_TESTS_SPEC.md`: Load tests specification

