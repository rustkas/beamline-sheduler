# Router Intake Chaos Tests - Implementation Report

**Date**: 2025-01-27  
**Status**: ✅ **Implementation Complete**

## Summary

Chaos tests for Router Intake have been successfully implemented to verify Router resilience to NATS failures and recoveries. The implementation includes 5 test scenarios, chaos orchestration script, test runner, CI/CD integration, and comprehensive documentation.

## Implementation Details

### 1. ✅ Test Suite Created

**File**: `apps/otp/router/test/router_intake_chaos_SUITE.erl`

**Test Scenarios**:
1. **Mild Chaos** (`test_chaos_nats_single_restart/1`): Single NATS restart verification
2. **Moderate Chaos** (`test_chaos_nats_multiple_restarts/1`): Multiple NATS restarts (3 cycles)
3. **Hard Chaos** (`test_chaos_nats_randomized_failures/1`): Randomized failures over 60 seconds
4. **NATS Failure During Processing** (`test_chaos_nats_during_message_processing/1`): NATS failure during active message processing
5. **Recovery Verification** (`test_chaos_nats_recovery_verification/1`): Router recovery after NATS restoration

**Features**:
- Uses real NATS (not mocks) for authentic failure scenarios
- Docker integration for NATS container control
- Process stability checks (process count, memory)
- Metric tracking (connection errors, reconnections)
- Recovery verification

### 2. ✅ Chaos Orchestration Script

**File**: `scripts/nats_chaos.sh`

**Modes**:
- `mild`: Single NATS restart
- `moderate`: Multiple restarts during test
- `hard`: Randomized failures

**Intensity Levels**:
- `low`: Short outages (5-10 seconds)
- `medium`: Medium outages (20-30 seconds)
- `high`: Long outages (30-60 seconds)

**Usage**:
```bash
# Mild chaos (single restart)
./scripts/nats_chaos.sh mild low 300

# Moderate chaos (multiple restarts)
./scripts/nats_chaos.sh moderate medium 300

# Hard chaos (randomized failures)
./scripts/nats_chaos.sh hard high 300
```

**Features**:
- Configurable chaos modes and intensity
- Automatic NATS container management
- Signal handling (INT/TERM) for graceful shutdown
- Colored logging for better visibility
- Container existence verification

### 3. ✅ Test Runner Script

**File**: `scripts/run_router_chaos_tests.sh`

**Features**:
- NATS container verification
- Automatic NATS startup if needed
- Environment variable configuration
- Test execution with verbose output
- Cleanup (ensures NATS is running at end)

**Usage**:
```bash
# Default (nats container)
./scripts/run_router_chaos_tests.sh

# Custom container name
./scripts/run_router_chaos_tests.sh my-nats-container
```

### 4. ✅ CI/CD Integration

**File**: `.github/workflows/router-chaos-tests.yml`

**Triggers**:
- Push to `main` or `develop` branches (Router code changes)
- Pull requests to `main` or `develop` branches
- Manual workflow dispatch
- Scheduled (nightly at 2 AM UTC)

**Features**:
- Automatic NATS container startup
- Test execution with timeout (15 minutes)
- Test result artifact upload
- Non-blocking for PRs (`continue-on-error: true`)
- Cleanup (ensures NATS is stopped/removed)

### 5. ✅ Documentation

**File**: `docs/archive/dev/ROUTER_CHAOS_TESTS_SPEC.md`

**Contents**:
- Purpose and goals
- Test scenarios (detailed)
- Implementation details
- Running chaos tests (local and CI/CD)
- Expected behavior (during outage, after recovery)
- Metrics and observability
- Success criteria
- Limitations and future enhancements

## Test Scenarios Details

### Scenario 1: Mild Chaos (Single NATS Restart)

**Purpose**: Verify Router survives a single NATS restart

**Steps**:
1. Stop NATS for 5-10 seconds
2. Verify Router remains alive
3. Start NATS
4. Verify Router recovers and can process messages

**Verifications**:
- Router process remains alive during outage
- Router process remains alive after recovery
- Connection error metrics are logged
- Process stability (growth < 20%)

**Duration**: ~20 seconds

### Scenario 2: Moderate Chaos (Multiple Restarts)

**Purpose**: Verify Router survives multiple NATS restarts

**Steps**:
1. Perform 3 NATS restart cycles
2. Each cycle: 5-10 second outage, 5 second recovery
3. Verify Router survives all restarts

**Verifications**:
- Router survives all restarts
- Connection error metrics accumulate correctly
- No process leaks between restarts
- Router can process messages after each recovery

**Duration**: ~60 seconds

### Scenario 3: Hard Chaos (Randomized Failures)

**Purpose**: Verify Router survives randomized NATS failures

**Steps**:
1. Random NATS failures over 60 seconds
2. Three failure types:
   - Long outage (5-15 seconds)
   - Short freeze (2 seconds)
   - Normal operation (5-15 seconds)

**Verifications**:
- Router survives randomized failures
- Failure count matches expected
- Process stability maintained
- No unbounded resource growth

**Duration**: 60 seconds

### Scenario 4: NATS Failure During Message Processing

**Purpose**: Verify Router handles NATS failures during active message processing

**Steps**:
1. Start processing messages
2. Stop NATS mid-processing
3. Continue sending messages (should fail gracefully)
4. Restart NATS
5. Verify Router can process new messages

**Verifications**:
- Router doesn't crash during active processing
- Messages sent during outage are handled gracefully
- Router can process new messages after recovery
- No message loss (or loss only per MaxDeliver rules)

**Duration**: ~30 seconds

### Scenario 5: Recovery Verification

**Purpose**: Verify Router properly recovers after NATS restoration

**Steps**:
1. Stop NATS for 10 seconds
2. Start NATS
3. Verify Router reconnects and resumes

**Verifications**:
- Reconnect metrics are emitted
- Connection established metrics are emitted
- Process stability (growth < 20%)
- Router can process messages after recovery

**Duration**: ~30 seconds

## Invariants Verified

All chaos tests verify the following invariants:

1. ✅ **Router Process Alive**: Router does not crash (`is_process_alive`)
2. ✅ **Graceful Degradation**: Router handles failures without hanging
3. ✅ **Recovery**: Router automatically reconnects after NATS restoration
4. ✅ **Process Stability**: No unbounded process/memory growth
5. ✅ **Observability**: Metrics and logs accurately reflect failures

## Running Chaos Tests

### Local Execution

**Basic**:
```bash
cd apps/otp/router
rebar3 ct --suite router_intake_chaos_SUITE
```

**With Test Runner**:
```bash
./scripts/run_router_chaos_tests.sh
```

**With External Chaos Script**:
```bash
# Terminal 1: Start chaos script
./scripts/nats_chaos.sh moderate medium 300 &

# Terminal 2: Run tests
cd apps/otp/router
rebar3 ct --suite router_intake_chaos_SUITE
```

### CI/CD Execution

**Automatic**: Triggered on Router code changes or scheduled (nightly)

**Manual**: Use GitHub Actions workflow dispatch

## Success Criteria

**All chaos tests must pass** for production readiness:

1. ✅ Router survives all NATS failures
2. ✅ Router recovers after NATS restoration
3. ✅ No process/memory leaks
4. ✅ Metrics accurately reflect failures
5. ✅ Logs provide actionable information

## Files Created/Modified

**Created**:
- `apps/otp/router/test/router_intake_chaos_SUITE.erl` - Test suite (5 tests)
- `scripts/nats_chaos.sh` - Chaos orchestration script
- `scripts/run_router_chaos_tests.sh` - Test runner script
- `.github/workflows/router-chaos-tests.yml` - CI/CD workflow
- `docs/archive/dev/ROUTER_CHAOS_TESTS_SPEC.md` - Test specification
- `docs/archive/dev/ROUTER_CHAOS_TESTS_IMPLEMENTATION_REPORT.md` - This file

**No modifications** to existing files (chaos tests are separate from e2e tests)

## Integration with Existing Tests

**Relationship to E2E Tests**:
- E2E tests (`router_intake_e2e_SUITE.erl`): Verify correct behavior in normal conditions
- Chaos tests (`router_intake_chaos_SUITE.erl`): Verify resilience to failures

**Complementary**:
- E2E tests use mocks for network failures
- Chaos tests use real NATS failures
- Both verify Router behavior, but from different angles

## Limitations

1. **Docker Dependency**: Tests require Docker and NATS container
2. **Environment Specific**: May not work in all CI environments
3. **Timing Dependent**: Recovery times may vary by environment
4. **Resource Intensive**: Chaos tests consume more resources than unit tests

## Future Enhancements

1. **Network Partition**: Simulate network partitions (not just container stop)
2. **NATS Slowdown**: Simulate slow NATS responses (not just outages)
3. **Message Loss Verification**: Verify message loss handling per MaxDeliver rules
4. **Consumer State Verification**: Verify JetStream consumer state after recovery
5. **Metrics Dashboard**: Visualize chaos test results over time

## References

- `apps/otp/router/test/router_intake_chaos_SUITE.erl` - Test implementation
- `scripts/nats_chaos.sh` - Chaos orchestration script
- `docs/archive/dev/ROUTER_CHAOS_TESTS_SPEC.md` - Test specification
- `apps/otp/router/test/router_intake_e2e_SUITE.erl` - E2E tests (baseline)

