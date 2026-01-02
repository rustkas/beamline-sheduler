# Observability Metrics Monitoring Implementation Report

**Date**: 2025-01-27  
**Status**: ✅ Implementation Complete  
**Purpose**: Metrics monitoring layer for load and chaos test scenarios.

## Summary

Created a comprehensive metrics monitoring framework for Router and Gateway load/chaos test scenarios, including:
- **Metric Catalog**: Complete inventory of Router and Gateway metrics
- **Monitoring Scenarios**: 8 detailed scenarios with expected metrics and health indicators
- **Verification Scripts**: Automated metrics verification after tests
- **Health Indicators**: Critical and warning thresholds
- **Alert Thresholds**: When to alert on metric anomalies

## Components Created

### 1. Metrics Monitoring Guide (`docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`)

**Purpose**: Comprehensive guide for using metrics to monitor load and chaos scenarios.

**Contents**:
- **Metric Catalog**: All Router and Gateway metrics with descriptions
- **8 Monitoring Scenarios**:
  1. High-Volume Success Flood
  2. High-Volume Error Flood
  3. Mixed Success/Error Stream
  4. Idempotency Stress
  5. NATS Chaos (Single Restart)
  6. NATS Chaos (Multiple Restarts)
  7. NATS Chaos (Randomized Failures)
  8. Rate Limiting Stress
- **Health Indicators**: Critical and warning indicators for each scenario
- **Verification Checklists**: Step-by-step checks for each scenario
- **Metric Verification Scripts**: Example scripts for Router and Gateway
- **Best Practices**: Baseline metrics, sampling, correlation

### 2. Metrics Verification Script (`scripts/verify_metrics_after_tests.sh`)

**Purpose**: Automated verification of Router and Gateway metrics after load/chaos tests.

**Features**:
- Fetches metrics from Router and Gateway endpoints
- Verifies invariants (DLQ failures, error rates, rate limits)
- Scenario-specific verification (load_success, load_error, chaos_nats, etc.)
- Health indicator checks
- Summary report with pass/fail status

**Usage**:
```bash
# Verify metrics after load test
./scripts/verify_metrics_after_tests.sh --scenario load_success

# Verify metrics after chaos test
./scripts/verify_metrics_after_tests.sh --scenario chaos_nats_single

# Custom endpoints
./scripts/verify_metrics_after_tests.sh \
  --router-url http://localhost:3081/_metrics \
  --gateway-url http://localhost:8080/_metrics \
  --scenario load_mixed
```

**Environment Variables**:
- `ROUTER_METRICS_URL`: Router metrics endpoint (default: `http://localhost:3081/_metrics`)
- `GATEWAY_METRICS_URL`: Gateway metrics endpoint (default: `http://localhost:8080/_metrics`)
- `TEST_SCENARIO`: Test scenario name (load_success, load_error, chaos_nats, etc.)

## Metric Catalog

### Router Metrics

#### Intake Metrics
- `router_intake_messages_total{subject, status}`: Total messages processed
- `router_intake_validation_errors_total{error_code, subject, tenant_id}`: Validation errors
- `router_intake_dlq_messages_total{reason, error_code, subject}`: Messages sent to DLQ
- `router_intake_dlq_publish_failed_total{reason, error_code, subject, failure_reason}`: DLQ publication failures
- `router_intake_idempotent_duplicate_total{subject, tenant_id}`: Duplicate messages detected

#### JetStream Metrics
- `router_jetstream_redelivery_total{subject, reason}`: Message redeliveries
- `router_jetstream_maxdeliver_exhausted_total{subject, reason}`: MaxDeliver exhaustion

#### NATS Connection Metrics
- `router_nats_connection_errors_total{reason}`: NATS connection errors
- `router_nats_reconnect_total`: NATS reconnection events

### Gateway Metrics

#### Rate Limiting Metrics
- `gateway_rate_limit_hits_total{endpoint, tenant_id}`: Total rate limit checks
- `gateway_rate_limit_allowed_total{endpoint, tenant_id}`: Requests allowed
- `gateway_rate_limit_exceeded_total{endpoint, tenant_id}`: Rate limit exceeded

**JSON Metrics** (via `/_metrics` endpoint):
- `rate_limit.total_hits`: Total rate limit checks
- `rate_limit.total_exceeded`: Total rate limit exceeded
- `rate_limit.exceeded_by_endpoint.routes_decide`: Exceeded for `/api/v1/routes/decide`
- `rate_limit.exceeded_by_endpoint.messages`: Exceeded for `/api/v1/messages`

#### HTTP Metrics
- `gateway_http_requests_total{method, path, status}`: Total HTTP requests
- `gateway_http_request_duration_seconds{method, path}`: HTTP request latency
- `gateway_http_requests_by_status{status}`: HTTP requests by status code

#### NATS Metrics
- `gateway_nats_messages_sent_total{subject}`: NATS messages sent
- `gateway_nats_publish_failures_total{subject, error}`: NATS publish failures
- `gateway_nats_connection_status`: NATS connection status (1=connected, 0=disconnected)

## Monitoring Scenarios

### Scenario 1: High-Volume Success Flood

**Expected Metrics**:
- `router_intake_messages_total{status="ok"}` ≈ N
- `router_intake_messages_total{status="failed"}` = 0
- `router_intake_validation_errors_total` = 0
- `router_intake_dlq_messages_total` = 0

**Health Indicators**:
- ✅ All messages processed
- ✅ No validation errors
- ✅ No DLQ messages
- ✅ Stable latency

### Scenario 2: High-Volume Error Flood

**Expected Metrics**:
- `router_intake_messages_total{status="failed"}` ≈ N
- `router_intake_validation_errors_total` ≈ N
- `router_intake_dlq_messages_total` ≈ N
- `router_intake_dlq_publish_failed_total` = 0

**Health Indicators**:
- ✅ All invalid messages rejected
- ✅ All errors sent to DLQ
- ✅ No DLQ publication failures
- ✅ Router process alive

### Scenario 3: Mixed Success/Error Stream

**Expected Metrics**:
- `router_intake_messages_total{status="ok"}` ≈ 70% of total
- `router_intake_messages_total{status="failed"}` ≈ 30% of total
- `router_intake_dlq_messages_total` ≈ 30% of total (only invalid)

**Health Indicators**:
- ✅ Metrics reflect exact distribution
- ✅ No valid messages in DLQ
- ✅ All invalid messages in DLQ

### Scenario 4: Idempotency Stress

**Expected Metrics**:
- `router_intake_idempotent_duplicate_total` ≈ N_duplicates
- `router_intake_messages_total{status="ok"}` ≈ N_unique

**Health Indicators**:
- ✅ First message per key processed
- ✅ Duplicates detected and fast-ACKed
- ✅ ETS tables stable (no unbounded growth)

### Scenario 5-7: NATS Chaos

**Expected Metrics**:
- `router_nats_reconnect_total` = N_restarts
- `router_nats_connection_errors_total` > 0
- `router_intake_messages_total` continues after reconnect

**Health Indicators**:
- ✅ Router reconnects after each restart
- ✅ Messages eventually processed
- ✅ No message loss
- ✅ Process stable

### Scenario 8: Rate Limiting Stress

**Expected Metrics**:
- `gateway_rate_limit_exceeded_total` > 0
- `gateway_http_requests_total{status="429"}` ≈ N_exceeded
- `router_intake_messages_total` ≈ N_allowed (not N_total)

**Health Indicators**:
- ✅ Rate limit metrics reflect exceeded limits
- ✅ 429 responses for exceeded requests
- ✅ Router only processes requests that passed rate limit

## Health Indicators

### Critical Health Indicators

| Indicator | Metric | Healthy Value | Unhealthy Value |
|-----------|--------|---------------|-----------------|
| Router Process Alive | `is_process_alive(RouterPid)` | `true` | `false` |
| Message Processing | `router_intake_messages_total{status="ok"}` | ≈ N_sent | < N_sent |
| Error Rate | `router_intake_validation_errors_total` / `router_intake_messages_total` | < 5% | > 10% |
| DLQ Failures | `router_intake_dlq_publish_failed_total` | = 0 | > 0 |
| NATS Connection | `router_nats_connection_status` | = 1 | = 0 |
| Memory Leaks | Process memory growth | < 10% | > 50% |

### Warning Indicators

| Indicator | Metric | Warning Threshold |
|-----------|--------|-------------------|
| High Error Rate | `router_intake_validation_errors_total` / `router_intake_messages_total` | > 5% |
| DLQ Backlog | `router_intake_dlq_messages_total` | > 1000 |
| High Latency | `router_intake_latency_ms` (p95) | > 1000ms |
| Rate Limit Hits | `gateway_rate_limit_exceeded_total` | > 10% of requests |

## Alert Thresholds

### Critical Alerts

- **Router Process Down**: `is_process_alive(RouterPid) = false`
- **DLQ Publication Failures**: `router_intake_dlq_publish_failed_total > 0`
- **NATS Connection Lost**: `router_nats_connection_status = 0` for > 30s
- **Memory Leak**: Process memory growth > 50% over test duration

### Warning Alerts

- **High Error Rate**: `router_intake_validation_errors_total` / `router_intake_messages_total` > 5%
- **DLQ Backlog**: `router_intake_dlq_messages_total` > 1000
- **High Latency**: `router_intake_latency_ms` (p95) > 1000ms
- **Rate Limit Exceeded**: `gateway_rate_limit_exceeded_total` > 10% of requests

## Integration with Test Suites

### Load Tests

**Location**: `apps/otp/router/test/router_intake_e2e_SUITE.erl` (load_tests group)

**Metrics Verification**:
- After each load test, verify metrics match expected values
- Check process stability (memory, process count)
- Verify DLQ content matches error count

### Chaos Tests

**Location**: `apps/otp/router/test/router_intake_chaos_SUITE.erl`

**Metrics Verification**:
- After each chaos test, verify reconnection metrics
- Check that messages are eventually processed
- Verify no message loss

## Usage Examples

### Verify Metrics After Load Test

```bash
# Run load test
cd apps/otp/router
LOAD_TEST_MESSAGE_COUNT=2000 rebar3 ct --suite router_intake_e2e_SUITE --group load_tests

# Verify metrics
./scripts/verify_metrics_after_tests.sh --scenario load_success
```

### Verify Metrics After Chaos Test

```bash
# Run chaos test
cd apps/otp/router
rebar3 ct --suite router_intake_chaos_SUITE

# Verify metrics
./scripts/verify_metrics_after_tests.sh --scenario chaos_nats_single
```

### Verify Metrics After Full Test Suite

```bash
# Run full test suite
./scripts/run_router_full_test_suite.sh

# Verify metrics for each scenario
./scripts/verify_metrics_after_tests.sh --scenario load_success
./scripts/verify_metrics_after_tests.sh --scenario load_error
./scripts/verify_metrics_after_tests.sh --scenario chaos_nats_multiple
```

## Best Practices

### 1. Baseline Metrics

**Before Test**:
- Record baseline metrics (process count, memory, connection status)
- Clear/reset counters if needed
- Note initial metric values

**After Test**:
- Compare final metrics to baseline
- Calculate deltas (growth, changes)
- Verify invariants

### 2. Metric Sampling

**During Test**:
- Sample metrics at regular intervals (e.g., every 10 seconds)
- Record metrics at key events (NATS restart, rate limit hit)
- Store metrics for post-test analysis

**Post-Test**:
- Aggregate metrics over test duration
- Calculate rates (messages/second, errors/second)
- Identify trends and anomalies

### 3. Metric Correlation

**Cross-Component**:
- Correlate Gateway metrics with Router metrics
- Verify Gateway → Router flow (requests → messages)
- Check error propagation (Gateway 429 → Router not called)

**Temporal**:
- Correlate metrics with test events (NATS restart, rate limit hit)
- Identify cause-effect relationships
- Verify expected metric changes

## References

- `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`: Complete monitoring guide
- `scripts/verify_metrics_after_tests.sh`: Metrics verification script
- `apps/otp/router/src/router_metrics.erl`: Router metrics definitions
- `apps/c-gateway/src/metrics/metrics_registry.c`: Gateway metrics definitions
- `docs/archive/dev/ROUTER_INTAKE_LOAD_TESTS_SPEC.md`: Load test scenarios
- `docs/archive/dev/ROUTER_CHAOS_TESTS_SPEC.md`: Chaos test scenarios
- `apps/otp/router/test/router_intake_e2e_SUITE.erl`: Load test implementation
- `apps/otp/router/test/router_intake_chaos_SUITE.erl`: Chaos test implementation

