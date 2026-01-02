# Router Intake Load Tests Specification

**Date**: 2025-01-27  
**Status**: ✅ **Tests Implemented**  
**Location**: `apps/otp/router/test/router_intake_e2e_SUITE.erl` (group: `load_tests`)

## Purpose

Load tests for Router intake validation layer to prove production readiness:

- **Stability**: Router works reliably under high-volume message streams
- **Correctness**: All messages are ACKed/NAKed correctly, no message loss
- **Resource Management**: No memory leaks, process leaks, or unbounded growth
- **Observability**: DLQ, metrics, and audit logs work correctly at scale

## Test Scenarios

### 1. High-Volume Success Flood

**Test**: `test_load_decide_success_flood/1`

**Configuration**:
- **Message Count**: 2000 fully valid `decide` messages
- **Pattern**: Sequential sending with small delays (10ms every 100 messages)

**Verifications**:
- ✅ **100% ACK**: All messages are ACKed by JetStream
- ✅ **No DLQ**: Zero DLQ publications (all messages valid)
- ✅ **Metrics**: `router_intake_messages_total{status="ok"}` matches message count
- ✅ **Process Stability**: Process count growth < 10%
- ✅ **Router Alive**: Router process remains alive after test

**Expected Results**:
- All 2000 messages ACKed
- No DLQ publications
- Metrics correctly reflect success count
- Process count stable (minimal growth)

### 2. High-Volume Error Flood

**Test**: `test_load_decide_error_flood/1`

**Configuration**:
- **Message Count**: 2000 invalid `decide` messages (missing required fields)
- **Pattern**: Sequential sending with small delays

**Verifications**:
- ✅ **100% ACK + DLQ**: All messages ACKed and published to DLQ
- ✅ **Error Metrics**: `router_intake_validation_errors_total` matches error count
- ✅ **Audit Logs**: All errors logged with correct `error_code`
- ✅ **No Crashes**: Router doesn't crash or hang
- ✅ **No Runaway Retries**: MaxDeliver exhaustion handled correctly
- ✅ **Process Stability**: Process count growth < 10%

**Expected Results**:
- All 2000 messages ACKed
- All 2000 messages in DLQ
- Error metrics match message count
- Audit entries match message count
- Router remains stable

### 3. Mixed Success/Error Stream

**Test**: `test_load_decide_mixed_stream/1`

**Configuration**:
- **Total Messages**: 2000
- **Distribution**: 70% valid (1400), 30% invalid (600)
- **Pattern**: Randomly shuffled sequence

**Verifications**:
- ✅ **Metrics Match Distribution**: 
  - `router_intake_messages_total{status="ok"}` ≈ 1400
  - `router_intake_validation_errors_total` ≈ 600
- ✅ **DLQ Accuracy**: 
  - No valid messages in DLQ
  - All invalid messages in DLQ (600)
- ✅ **ACK Accuracy**: All 2000 messages ACKed
- ✅ **Process Stability**: Process count growth < 10%

**Expected Results**:
- 1400 valid messages processed successfully
- 600 invalid messages sent to DLQ
- Metrics accurately reflect distribution
- Router remains stable

### 4. Idempotency Stress

**Test**: `test_load_decide_idempotency_stress/1`

**Configuration**:
- **Unique Keys**: 100
- **Repeats Per Key**: 10
- **Total Messages**: 1000 (100 unique keys × 10 repeats)
- **Pattern**: AAA, AAA, AAA, BBB, BBB, CCC, ... (sequential repeats)

**Verifications**:
- ✅ **First Message Per Key**: 
  - Status: `not_seen` (new)
  - Normal processing (routing decision)
- ✅ **Duplicate Messages**: 
  - Status: `seen` (duplicate)
  - Fast ACK without reprocessing
- ✅ **Idempotency Metrics**:
  - `router_idempotency_miss_total` ≥ 100 (one miss per unique key)
  - `router_idempotency_hit_total` ≥ 900 (all repeats are hits)
- ✅ **ETS Table Stability**: Idempotency table doesn't grow unbounded
- ✅ **Process Stability**: Process count growth < 10%

**Expected Results**:
- 100 unique keys processed as "new"
- 900 duplicate messages processed as "seen"
- Idempotency metrics match expected distribution
- Router remains stable

## Implementation Details

### Helper Functions

**Location**: End of `router_intake_e2e_SUITE.erl`

**Functions**:
- `generate_valid_decide_request/1`: Creates valid decide request with unique IDs
- `generate_invalid_decide_request/1`: Creates invalid decide request (missing fields)
- `generate_idempotency_key/1`: Generates unique idempotency key

### Statistics Tracking

**ETS Tables** (per test):
- `ack_count`: Tracks ACK calls
- `dlq_count`: Tracks DLQ publications
- `metric_events`: Tracks telemetry events
- `audit_events`: Tracks audit log entries
- `idempotency_hits/misses`: Tracks idempotency metrics

### Process Stability Checks

**Before Test**:
- `erlang:system_info(process_count)` - Process count
- `erlang:memory(processes_used)` - Memory usage

**After Test**:
- Verify process count growth < 10%
- Verify Router process is alive
- Verify no unbounded ETS table growth

### Timing

**Test Timeout**: 30-60 seconds per test (configured via `ct` timeout)

**Delays**:
- Small delay (10ms) every 100 messages to prevent overwhelming
- 2-second wait after all messages sent for processing to complete

## Running Load Tests

### Run All Load Tests

```bash
cd apps/otp/router
rebar3 ct --suite router_intake_e2e_SUITE --group load_tests
```

### Run Specific Load Test

```bash
cd apps/otp/router
rebar3 ct --suite router_intake_e2e_SUITE --case test_load_decide_success_flood
```

### Run with Verbose Output

```bash
cd apps/otp/router
rebar3 ct --suite router_intake_e2e_SUITE --group load_tests --verbose
```

## Test Results Interpretation

### Success Criteria

**All tests must pass**:
- ✅ All messages ACKed
- ✅ DLQ count matches expected (0 for success, N for errors)
- ✅ Metrics match message counts
- ✅ Process stability (growth < 10%)
- ✅ Router remains alive

### Failure Indicators

**Red flags**:
- ❌ Message count mismatch (ACK count ≠ sent count)
- ❌ DLQ count mismatch (DLQ count ≠ error count)
- ❌ Process count growth > 10%
- ❌ Router process dies
- ❌ Metrics don't match expected counts
- ❌ Memory growth > 50%

## Integration with CI/CD

**Recommended CI Integration**:
- Run load tests as part of CP2+ acceptance criteria
- Fail CI if any load test fails
- Monitor test duration (should complete within timeout)
- Track process stability metrics over time

## Future Enhancements

1. **Performance Benchmarking**:
   - Latency percentiles (p50, p95, p99)
   - Throughput (messages/second)
   - CPU profiling

2. **Advanced Scenarios**:
   - Network flakiness + high volume
   - NATS unavailability + high volume
   - Partial DLQ failure + high volume

3. **Parallel Execution**:
   - Multiple concurrent senders
   - Higher message rates
   - Stress testing under concurrency

## References

- `apps/otp/router/test/router_intake_e2e_SUITE.erl` - Test implementation
- `docs/archive/dev/ROUTER_INTAKE_E2E_TEST_CHECKLIST.md` - E2E test checklist
- `apps/otp/router/docs/INTAKE_ERROR_HANDLING.md` - Error handling documentation

