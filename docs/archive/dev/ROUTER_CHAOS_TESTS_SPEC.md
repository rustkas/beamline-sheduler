# Router Intake Chaos Tests Specification

**Date**: 2025-01-27  
**Status**: ✅ **Implementation Complete**  
**Location**: `apps/otp/router/test/router_intake_chaos_SUITE.erl`

## Purpose

Chaos tests verify Router resilience to NATS failures and recoveries. Unlike performance tests, chaos tests focus on **fault tolerance** and **graceful degradation** when NATS is unavailable or unstable.

## Goals

### Primary Goals

1. **Router Survival**: Router does not crash when NATS fails
2. **Graceful Degradation**: Router handles NATS failures without hanging or blocking
3. **Recovery**: Router automatically reconnects and resumes operation after NATS recovery
4. **Message Integrity**: Messages are not lost (or lost only per MaxDeliver/DLQ rules)
5. **Observability**: Logs and metrics accurately reflect NATS failures

### Invariants

- ✅ Router process remains alive (`is_process_alive`)
- ✅ Subscriptions are restored after NATS recovery
- ✅ No hanging messages or infinite retries
- ✅ DLQ failures don't block Router
- ✅ Process/memory stability (no unbounded growth)

## Test Scenarios

### 1. Mild Chaos: Single NATS Restart

**Test**: `test_chaos_nats_single_restart/1`

**Scenario**:
- NATS is stopped once for 5-10 seconds
- NATS is restarted
- Router should survive and recover

**Verifications**:
- Router process remains alive during outage
- Router process remains alive after recovery
- Connection error metrics are logged
- Process stability (growth < 20%)

**Duration**: ~20 seconds

### 2. Moderate Chaos: Multiple NATS Restarts

**Test**: `test_chaos_nats_multiple_restarts/1`

**Scenario**:
- NATS is stopped and restarted 3 times
- Each cycle: 5-10 second outage, 5 second recovery
- Router should survive all restarts

**Verifications**:
- Router survives all restarts
- Connection error metrics accumulate correctly
- No process leaks between restarts
- Router can process messages after each recovery

**Duration**: ~60 seconds

### 3. Hard Chaos: Randomized Failures

**Test**: `test_chaos_nats_randomized_failures/1`

**Scenario**:
- Random NATS failures over 60 seconds
- Three failure types:
  - Long outage (5-15 seconds)
  - Short freeze (2 seconds)
  - Normal operation (5-15 seconds)
- Router should survive all failures

**Verifications**:
- Router survives randomized failures
- Failure count matches expected
- Process stability maintained
- No unbounded resource growth

**Duration**: 60 seconds

### 4. NATS Failure During Message Processing

**Test**: `test_chaos_nats_during_message_processing/1`

**Scenario**:
- Router is processing messages
- NATS fails mid-processing
- Router continues gracefully
- NATS recovers
- Router resumes processing

**Verifications**:
- Router doesn't crash during active processing
- Messages sent during outage are handled gracefully
- Router can process new messages after recovery
- No message loss (or loss only per MaxDeliver rules)

**Duration**: ~30 seconds

### 5. Recovery Verification

**Test**: `test_chaos_nats_recovery_verification/1`

**Scenario**:
- NATS fails for 10 seconds
- NATS recovers
- Verify Router reconnects and resumes

**Verifications**:
- Reconnect metrics are emitted
- Connection established metrics are emitted
- Process stability (growth < 20%)
- Router can process messages after recovery

**Duration**: ~30 seconds

## Implementation

### Test Suite

**File**: `apps/otp/router/test/router_intake_chaos_SUITE.erl`

**Features**:
- Uses real NATS (not mocks) for chaos tests
- Docker integration for NATS container control
- Process stability checks
- Metric tracking
- Recovery verification

### Chaos Script

**File**: `scripts/nats_chaos.sh`

**Modes**:
- `mild`: Single NATS restart
- `moderate`: Multiple restarts during test
- `hard`: Randomized failures

**Intensity**:
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

## Running Chaos Tests

### Prerequisites

1. **Docker**: NATS container must be available
2. **NATS Container**: Default name `nats` (configurable via `NATS_CONTAINER` env var)
3. **Router**: Must be configured to use real NATS (`nats_mode: real`)

### Local Execution

**Basic**:
```bash
cd apps/otp/router
rebar3 ct --suite router_intake_chaos_SUITE
```

**With NATS Container Name**:
```bash
export NATS_CONTAINER=my-nats-container
cd apps/otp/router
rebar3 ct --suite router_intake_chaos_SUITE
```

### External Chaos Script

**Approach A: External Orchestration**

```bash
# 1. Start services
docker-compose up -d router gateway nats

# 2. Start chaos script in background
./scripts/nats_chaos.sh moderate medium 300 &

# 3. Run tests
cd apps/otp/router
rebar3 ct --suite router_intake_chaos_SUITE
```

### Test-Controlled Chaos

**Approach B: Test-Controlled**

Tests use `os:cmd/1` to control NATS container:
- `docker stop nats` - Stop NATS
- `docker start nats` - Start NATS
- Tests verify Router behavior at each step

## Expected Behavior

### During NATS Outage

**Router Should**:
- ✅ Remain alive (no crash)
- ✅ Log connection errors
- ✅ Emit connection error metrics
- ✅ Handle message send failures gracefully
- ✅ Not block on DLQ publication failures

**Router Should NOT**:
- ❌ Crash or hang
- ❌ Create infinite retry loops
- ❌ Leak processes or memory
- ❌ Block on NATS operations

### After NATS Recovery

**Router Should**:
- ✅ Automatically reconnect
- ✅ Restore JetStream subscriptions
- ✅ Resume message processing
- ✅ Emit reconnection metrics
- ✅ Process new messages normally

**Router Should NOT**:
- ❌ Require manual restart
- ❌ Create duplicate consumers
- ❌ Lose subscription state
- ❌ Fail to process new messages

## Metrics and Observability

### Metrics to Track

1. **Connection Errors**: `router_nats.connection_error_total`
2. **Reconnections**: `router_nats.reconnect_total`
3. **Connection Established**: `router_nats.connection_established_total`
4. **DLQ Failures**: `router_intake.dlq_publish_failed_total`
5. **Process Count**: `erlang:system_info(process_count)`
6. **Memory Usage**: `erlang:memory(processes_used)`

### Logs to Verify

1. Connection loss logs
2. Reconnection logs
3. Error handling logs
4. Recovery confirmation logs

## Integration with CI/CD

### Recommended CI Integration

**Option 1: Separate Chaos Test Job**
- Run chaos tests as separate CI job
- Trigger on Router code changes
- Allow longer timeout (5-10 minutes)
- Mark as non-blocking for PRs (informational)

**Option 2: Nightly Chaos Tests**
- Run chaos tests in nightly builds
- Use hard chaos mode
- Collect metrics and report trends

### CI Configuration

```yaml
# Example GitHub Actions
- name: Run Chaos Tests
  env:
    NATS_CONTAINER: nats
  run: |
    docker-compose up -d nats
    cd apps/otp/router
    rebar3 ct --suite router_intake_chaos_SUITE
```

## Success Criteria

**All chaos tests must pass** for production readiness:

1. ✅ Router survives all NATS failures
2. ✅ Router recovers after NATS restoration
3. ✅ No process/memory leaks
4. ✅ Metrics accurately reflect failures
5. ✅ Logs provide actionable information

## Limitations

1. **Docker Dependency**: Tests require Docker and NATS container
2. **Environment Specific**: May not work in all CI environments
3. **Timing Dependent**: Recovery times may vary by environment
4. **Resource Intensive**: Chaos tests consume more resources than unit tests

## Future Enhancements

1. **Network Partition**: Simulate network partitions (not just container stop)
2. **NATS Slowdown**: Simulate slow NATS responses (not just outages)
3. **Message Loss**: Verify message loss handling per MaxDeliver rules
4. **Consumer State**: Verify JetStream consumer state after recovery
5. **Metrics Dashboard**: Visualize chaos test results over time

## References

- `apps/otp/router/test/router_intake_chaos_SUITE.erl` - Test implementation
- `scripts/nats_chaos.sh` - Chaos script
- `apps/otp/router/test/router_intake_e2e_SUITE.erl` - E2E tests (baseline)

