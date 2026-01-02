# Observability Metrics Monitoring Guide

**Date**: 2025-01-27  
**Status**: ✅ Complete  
**Purpose**: Guide for using Router and Gateway metrics to monitor load and chaos test scenarios.

## Overview

This guide provides a comprehensive framework for monitoring Router and Gateway metrics during load and chaos testing scenarios. It defines:
- **Metric Catalog**: All available metrics from Router and Gateway
- **Monitoring Scenarios**: How to interpret metrics for different test scenarios
- **Health Indicators**: Key metrics that indicate system health
- **Alert Thresholds**: When metrics indicate problems
- **Verification Checklists**: Step-by-step checks for each scenario type

## Metric Catalog

### Router Metrics

#### Intake Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `router_intake_messages_total` | Counter | `subject`, `status` (`ok`, `failed`) | Total messages processed by Router intake |
| `router_intake_validation_errors_total` | Counter | `error_code`, `subject`, `tenant_id` | Validation errors during intake |
| `router_intake_dlq_messages_total` | Counter | `reason`, `error_code`, `subject` | Messages sent to DLQ |
| `router_intake_dlq_publish_failed_total` | Counter | `reason`, `error_code`, `subject`, `failure_reason` | Failed DLQ publications |
| `router_intake_idempotent_duplicate_total` | Counter | `subject`, `tenant_id` | Duplicate messages detected (idempotency) |

#### JetStream Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `router_jetstream_redelivery_total` | Counter | `subject`, `reason` | Message redeliveries |
| `router_jetstream_maxdeliver_exhausted_total` | Counter | `subject`, `reason` | Messages that exhausted MaxDeliver |
| `router_jetstream_pending_messages` | Gauge | `subject`, `consumer` | Pending messages in JetStream consumer |
| `router_jetstream_ack_pending_messages` | Gauge | `subject`, `consumer` | Messages awaiting ACK |
| `router_jetstream_delivered_messages_total` | Counter | `subject`, `consumer` | Total delivered messages |
| `router_jetstream_redelivered_messages_total` | Counter | `subject`, `consumer` | Total redelivered messages |

#### Processing Latency Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `router_intake_processing_latency_seconds` | Histogram | `subject`, `status` | Processing latency per message |
| `router_intake_processing_latency_p50` | Gauge | `subject` | 50th percentile latency |
| `router_intake_processing_latency_p95` | Gauge | `subject` | 95th percentile latency |
| `router_intake_processing_latency_p99` | Gauge | `subject` | 99th percentile latency |

#### In-Flight Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `router_intake_inflight_messages` | Gauge | `subject` | Messages currently being processed |
| `router_intake_inflight_messages_max` | Gauge | `subject` | Maximum in-flight messages (peak) |

#### Backpressure Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `router_intake_backpressure_active` | Gauge | `subject` | Backpressure status (1=active, 0=inactive) |
| `router_intake_backpressure_triggered_total` | Counter | `subject`, `trigger` | Backpressure trigger events |
| `router_intake_backpressure_rejections_total` | Counter | `subject`, `reason` | Rejected messages due to backpressure |

#### NATS Connection Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `router_nats_connection_errors_total` | Counter | `reason` | NATS connection errors |
| `router_nats_reconnect_total` | Counter | - | NATS reconnection events |

### Gateway Metrics

#### Rate Limiting Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `gateway_rate_limit_hits_total` | Counter | `endpoint`, `tenant_id` | Total rate limit checks |
| `gateway_rate_limit_allowed_total` | Counter | `endpoint`, `tenant_id` | Requests allowed by rate limiter |
| `gateway_rate_limit_exceeded_total` | Counter | `endpoint`, `tenant_id` | Rate limit exceeded events |

**Note**: Gateway also exposes JSON metrics via `/_metrics` endpoint:
- `rate_limit.total_hits`: Total rate limit checks
- `rate_limit.total_exceeded`: Total rate limit exceeded
- `rate_limit.exceeded_by_endpoint.routes_decide`: Exceeded for `/api/v1/routes/decide`
- `rate_limit.exceeded_by_endpoint.messages`: Exceeded for `/api/v1/messages`

#### HTTP Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `gateway_http_requests_total` | Counter | `method`, `path`, `status` | Total HTTP requests |
| `gateway_http_request_duration_seconds` | Histogram | `method`, `path` | HTTP request latency |
| `gateway_http_requests_by_status` | Counter | `status` | HTTP requests by status code |

#### NATS Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `gateway_nats_messages_sent_total` | Counter | `subject` | NATS messages sent |
| `gateway_nats_publish_failures_total` | Counter | `subject`, `error` | NATS publish failures |
| `gateway_nats_connection_status` | Gauge | - | NATS connection status (1=connected, 0=disconnected) |

## Monitoring Scenarios

### Scenario 1: High-Volume Success Flood

**Test**: Send 1000-10000 valid `decide` messages  
**Goal**: Verify Router handles high volumes without degradation

#### Expected Metrics

**Router**:
- `router_intake_messages_total{subject="beamline.router.v1.decide", status="ok"}` ≈ N (message count)
- `router_intake_messages_total{subject="beamline.router.v1.decide", status="failed"}` = 0
- `router_intake_validation_errors_total` = 0
- `router_intake_dlq_messages_total` = 0
- `router_intake_dlq_publish_failed_total` = 0

**Gateway** (if used):
- `gateway_http_requests_total{method="POST", path="/api/v1/routes/decide", status="200"}` ≈ N
- `gateway_rate_limit_exceeded_total` = 0 (if rate limit not hit)
- `gateway_nats_publish_failures_total` = 0

#### Health Indicators

✅ **Healthy**:
- All messages processed (`status="ok"` ≈ N)
- No validation errors
- No DLQ messages
- Stable latency (no degradation over time)
- No connection errors

❌ **Unhealthy**:
- `status="failed"` > 0
- Validation errors > 0
- DLQ messages > 0
- Latency increasing over time
- Connection errors > 0

#### Verification Checklist

- [ ] `router_intake_messages_total{status="ok"}` matches sent message count
- [ ] `router_intake_messages_total{status="failed"}` = 0
- [ ] `router_intake_validation_errors_total` = 0
- [ ] `router_intake_dlq_messages_total` = 0
- [ ] No memory leaks (process memory stable)
- [ ] No process count growth
- [ ] Latency stable (p50, p95, p99 within expected range)

### Scenario 2: High-Volume Error Flood

**Test**: Send 1000-10000 invalid messages (corrupt protobuf/JSON, missing fields)  
**Goal**: Verify Router handles errors gracefully without crashes

#### Expected Metrics

**Router**:
- `router_intake_messages_total{subject="beamline.router.v1.decide", status="failed"}` ≈ N (error count)
- `router_intake_validation_errors_total{error_code="SCHEMA_VALIDATION_FAILED"}` ≈ N
- `router_intake_dlq_messages_total{reason="validation_failed"}` ≈ N (if DLQ enabled)
- `router_intake_dlq_publish_failed_total` = 0 (DLQ publication should succeed)

**Gateway** (if used):
- `gateway_http_requests_total{status="400"}` ≈ N (or 429 if rate limit hit)
- `gateway_rate_limit_exceeded_total` may be > 0 (if rate limit hit)

#### Health Indicators

✅ **Healthy**:
- All invalid messages rejected (`status="failed"` ≈ N)
- All errors sent to DLQ (if enabled)
- No DLQ publication failures
- Router process alive
- No runaway retries (MaxDeliver respected)

❌ **Unhealthy**:
- Router process crashed
- DLQ publication failures > 0
- Infinite retries (MaxDeliver not respected)
- Memory leaks
- Process count growth

#### Verification Checklist

- [ ] `router_intake_messages_total{status="failed"}` matches sent error count
- [ ] `router_intake_validation_errors_total` matches error count
- [ ] `router_intake_dlq_messages_total` ≈ N (if DLQ enabled)
- [ ] `router_intake_dlq_publish_failed_total` = 0
- [ ] Router process alive (`is_process_alive`)
- [ ] No memory leaks
- [ ] MaxDeliver exhaustion handled correctly

### Scenario 3: Mixed Success/Error Stream

**Test**: Send mixed sequence (e.g., 70% valid, 30% invalid)  
**Goal**: Verify Router correctly distinguishes valid from invalid messages

#### Expected Metrics

**Router**:
- `router_intake_messages_total{status="ok"}` ≈ N_valid (70% of total)
- `router_intake_messages_total{status="failed"}` ≈ N_invalid (30% of total)
- `router_intake_validation_errors_total` ≈ N_invalid
- `router_intake_dlq_messages_total` ≈ N_invalid (only invalid messages in DLQ)
- `router_intake_dlq_messages_total` should NOT include valid messages

**Gateway** (if used):
- `gateway_http_requests_total{status="200"}` ≈ N_valid
- `gateway_http_requests_total{status="400"}` ≈ N_invalid
- `gateway_rate_limit_exceeded_total` may be > 0

#### Health Indicators

✅ **Healthy**:
- Metrics reflect exact distribution (70% ok, 30% failed)
- No valid messages in DLQ
- All invalid messages in DLQ
- Correct error codes for invalid messages

❌ **Unhealthy**:
- Valid messages in DLQ
- Invalid messages not in DLQ
- Metrics don't match distribution
- Wrong error codes

#### Verification Checklist

- [ ] `router_intake_messages_total{status="ok"}` ≈ 70% of total
- [ ] `router_intake_messages_total{status="failed"}` ≈ 30% of total
- [ ] `router_intake_dlq_messages_total` ≈ 30% of total (only invalid)
- [ ] No valid messages in DLQ (verify DLQ content)
- [ ] Error codes match expected types
- [ ] Metrics sum correctly: `ok + failed = total`

### Scenario 4: Idempotency Stress

**Test**: Send hundreds/thousands of messages with repeating `idempotency_key`  
**Goal**: Verify idempotency handling and metrics

#### Expected Metrics

**Router**:
- `router_intake_idempotent_duplicate_total` ≈ N_duplicates
- `router_intake_messages_total{status="ok"}` ≈ N_unique (first message per key)
- First message per key: `status="ok"`, processed normally
- Duplicate messages: Fast ACK, not re-routed

**Gateway** (if used):
- `gateway_idempotency_hits_total` ≈ N_duplicates (if Gateway tracks idempotency)
- `gateway_idempotency_misses_total` ≈ N_unique

#### Health Indicators

✅ **Healthy**:
- First message per key processed normally
- Duplicates detected and fast-ACKed
- Idempotency metrics reflect duplicates
- ETS/idempotency tables don't grow unboundedly (TTL/cleanup working)

❌ **Unhealthy**:
- Duplicates processed multiple times
- Idempotency metrics incorrect
- ETS tables grow unboundedly
- Memory leaks from idempotency tracking

#### Verification Checklist

- [ ] `router_intake_idempotent_duplicate_total` ≈ N_duplicates
- [ ] `router_intake_messages_total{status="ok"}` ≈ N_unique
- [ ] First message per key processed (not duplicate)
- [ ] Duplicate messages fast-ACKed (not re-routed)
- [ ] ETS/idempotency tables stable (no unbounded growth)
- [ ] No memory leaks

### Scenario 5: NATS Chaos (Single Restart)

**Test**: NATS restarts once during test  
**Goal**: Verify Router recovers and metrics reflect reconnection

#### Expected Metrics

**Router**:
- `router_nats_connection_errors_total` > 0 (during restart)
- `router_nats_reconnect_total` = 1 (after restart)
- `router_intake_messages_total` continues after reconnect
- No message loss (all messages eventually processed)

**Gateway**:
- `gateway_nats_connection_status` = 0 (during restart), then 1 (after reconnect)
- `gateway_nats_publish_failures_total` may increase during restart
- `gateway_http_requests_total` may show 503 (Service Unavailable) during restart

#### Health Indicators

✅ **Healthy**:
- Router reconnects after NATS restart
- Metrics reflect reconnection events
- Messages processed after reconnect
- No message loss
- Process alive throughout

❌ **Unhealthy**:
- Router doesn't reconnect
- Messages lost
- Router process crashed
- Infinite connection errors

#### Verification Checklist

- [ ] `router_nats_reconnect_total` = 1
- [ ] `router_nats_connection_errors_total` > 0 (during restart)
- [ ] Router process alive throughout
- [ ] Messages processed after reconnect
- [ ] No message loss
- [ ] Connection status returns to 1

### Scenario 6: NATS Chaos (Multiple Restarts)

**Test**: NATS restarts multiple times during test  
**Goal**: Verify Router handles repeated failures gracefully

#### Expected Metrics

**Router**:
- `router_nats_reconnect_total` = N_restarts
- `router_nats_connection_errors_total` > 0 (multiple events)
- `router_intake_messages_total` continues after each reconnect
- JetStream consumers reactivate after each reconnect

**Gateway**:
- `gateway_nats_connection_status` oscillates (0 → 1 → 0 → 1)
- `gateway_nats_publish_failures_total` increases with each restart
- `gateway_http_requests_total{status="503"}` increases during restarts

#### Health Indicators

✅ **Healthy**:
- Router reconnects after each restart
- Metrics reflect all reconnection events
- No zombie consumers
- Messages eventually processed
- Process stable

❌ **Unhealthy**:
- Router doesn't reconnect after some restarts
- Zombie consumers accumulate
- Metrics don't reflect all reconnections
- Process crashes

#### Verification Checklist

- [ ] `router_nats_reconnect_total` = N_restarts
- [ ] Router process alive throughout
- [ ] JetStream consumers reactivate after each reconnect
- [ ] No zombie consumers
- [ ] Messages eventually processed
- [ ] Connection status returns to 1 after each reconnect

### Scenario 7: NATS Chaos (Randomized Failures)

**Test**: NATS fails randomly over 60 seconds  
**Goal**: Verify Router remains stable under randomized failures

#### Expected Metrics

**Router**:
- `router_nats_connection_errors_total` > 0 (multiple events)
- `router_nats_reconnect_total` = N_successful_reconnects
- `router_intake_messages_total` continues (with gaps during failures)
- Metrics reflect failure/recovery cycles

**Gateway**:
- `gateway_nats_connection_status` oscillates randomly
- `gateway_nats_publish_failures_total` increases
- `gateway_http_requests_total{status="503"}` increases during failures

#### Health Indicators

✅ **Healthy**:
- Router remains stable throughout
- Reconnects after each failure
- No process crashes
- No memory leaks
- Graceful degradation (temporary unavailability, not crashes)

❌ **Unhealthy**:
- Router crashes
- Memory leaks
- Process count growth
- Infinite connection attempts

#### Verification Checklist

- [ ] Router process alive throughout
- [ ] `router_nats_reconnect_total` = N_successful_reconnects
- [ ] No process crashes
- [ ] No memory leaks
- [ ] Connection status eventually returns to 1
- [ ] Messages processed after recovery

### Scenario 8: Rate Limiting Stress

**Test**: Send requests that exceed Gateway rate limits  
**Goal**: Verify rate limiting metrics and Router behavior

#### Expected Metrics

**Gateway**:
- `gateway_rate_limit_exceeded_total{endpoint="/api/v1/routes/decide"}` > 0
- `gateway_rate_limit_hits_total` > 0
- `gateway_http_requests_total{status="429"}` ≈ N_exceeded
- `gateway_http_requests_total{status="200"}` ≈ N_allowed

**Router**:
- `router_intake_messages_total` ≈ N_allowed (only requests that passed rate limit)
- `router_intake_messages_total` should NOT include rate-limited requests

#### Health Indicators

✅ **Healthy**:
- Rate limit metrics reflect exceeded limits
- 429 responses for exceeded requests
- Router only processes requests that passed rate limit
- No Router errors from rate-limited requests

❌ **Unhealthy**:
- Router processes rate-limited requests
- Rate limit metrics incorrect
- Router errors from rate-limited requests

#### Verification Checklist

- [ ] `gateway_rate_limit_exceeded_total` > 0
- [ ] `gateway_http_requests_total{status="429"}` ≈ N_exceeded
- [ ] `router_intake_messages_total` ≈ N_allowed (not N_total)
- [ ] Router doesn't process rate-limited requests
- [ ] Rate limit headers present in 429 responses

## Metric Verification Scripts

### Router Metrics Verification

```bash
#!/bin/bash
# verify_router_metrics.sh
# Verifies Router metrics after load/chaos tests

ROUTER_METRICS_URL="${ROUTER_METRICS_URL:-http://localhost:3081/_metrics}"

# Fetch metrics
METRICS=$(curl -s "${ROUTER_METRICS_URL}")

# Extract key metrics
INTAKE_OK=$(echo "${METRICS}" | grep 'router_intake_messages_total{status="ok"}' | awk '{print $2}')
INTAKE_FAILED=$(echo "${METRICS}" | grep 'router_intake_messages_total{status="failed"}' | awk '{print $2}')
VALIDATION_ERRORS=$(echo "${METRICS}" | grep 'router_intake_validation_errors_total' | awk '{sum+=$2} END {print sum}')
DLQ_MESSAGES=$(echo "${METRICS}" | grep 'router_intake_dlq_messages_total' | awk '{sum+=$2} END {print sum}')
DLQ_FAILED=$(echo "${METRICS}" | grep 'router_intake_dlq_publish_failed_total' | awk '{sum+=$2} END {print sum}')

echo "Router Metrics Summary:"
echo "  Intake OK: ${INTAKE_OK}"
echo "  Intake Failed: ${INTAKE_FAILED}"
echo "  Validation Errors: ${VALIDATION_ERRORS}"
echo "  DLQ Messages: ${DLQ_MESSAGES}"
echo "  DLQ Publish Failed: ${DLQ_FAILED}"

# Verify invariants
if [ "${DLQ_FAILED}" -gt 0 ]; then
    echo "⚠️  WARNING: DLQ publication failures detected"
    exit 1
fi

if [ "${INTAKE_FAILED}" -gt 0 ] && [ "${DLQ_MESSAGES}" -eq 0 ]; then
    echo "⚠️  WARNING: Failed messages not sent to DLQ"
    exit 1
fi

echo "✅ Metrics verification passed"
```

### Gateway Metrics Verification

```bash
#!/bin/bash
# verify_gateway_metrics.sh
# Verifies Gateway metrics after load/chaos tests

GATEWAY_METRICS_URL="${GATEWAY_METRICS_URL:-http://localhost:8081/_metrics}"

# Fetch metrics (JSON format)
METRICS_JSON=$(curl -s "${GATEWAY_METRICS_URL}")

# Extract rate limiting metrics
RATE_LIMIT_HITS=$(echo "${METRICS_JSON}" | jq -r '.rate_limit.total_hits // 0')
RATE_LIMIT_EXCEEDED=$(echo "${METRICS_JSON}" | jq -r '.rate_limit.total_exceeded // 0')
RATE_LIMIT_DECIDE=$(echo "${METRICS_JSON}" | jq -r '.rate_limit.exceeded_by_endpoint.routes_decide // 0')

echo "Gateway Metrics Summary:"
echo "  Rate Limit Hits: ${RATE_LIMIT_HITS}"
echo "  Rate Limit Exceeded: ${RATE_LIMIT_EXCEEDED}"
echo "  Rate Limit Exceeded (decide): ${RATE_LIMIT_DECIDE}"

# Verify invariants
if [ "${RATE_LIMIT_EXCEEDED}" -gt "${RATE_LIMIT_HITS}" ]; then
    echo "⚠️  WARNING: Rate limit exceeded > hits (invalid)"
    exit 1
fi

echo "✅ Metrics verification passed"
```

## Health Indicators Summary

### Critical Health Indicators

| Indicator | Metric | Healthy Value | Unhealthy Value |
|-----------|--------|---------------|-----------------|
| **Router Process Alive** | `is_process_alive(RouterPid)` | `true` | `false` |
| **Message Processing** | `router_intake_messages_total{status="ok"}` | ≈ N_sent | < N_sent |
| **Error Rate** | `router_intake_validation_errors_total` / `router_intake_messages_total` | < 5% | > 10% |
| **DLQ Failures** | `router_intake_dlq_publish_failed_total` | = 0 | > 0 |
| **NATS Connection** | `router_nats_connection_status` | = 1 | = 0 |
| **NATS Reconnects** | `router_nats_reconnect_total` | = N_restarts | < N_restarts |
| **Memory Leaks** | Process memory growth | < 10% | > 50% |
| **Process Count** | `erlang:system_info(process_count)` | Stable | Growing |

### Warning Indicators

| Indicator | Metric | Warning Threshold |
|-----------|--------|-------------------|
| **High Error Rate** | `router_intake_validation_errors_total` / `router_intake_messages_total` | > 5% |
| **DLQ Backlog** | `router_intake_dlq_messages_total` | > 1000 |
| **High Latency** | `router_intake_latency_ms` (p95) | > 1000ms |
| **Rate Limit Hits** | `gateway_rate_limit_exceeded_total` | > 10% of requests |

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

### Load Tests Integration

**Location**: `apps/otp/router/test/router_intake_e2e_SUITE.erl` (load_tests group)

**Metrics Verification**:
- After each load test, verify metrics match expected values
- Check process stability (memory, process count)
- Verify DLQ content matches error count

**Example**:
```erlang
verify_load_test_metrics(ExpectedValid, ExpectedInvalid) ->
    %% Fetch metrics (via telemetry or ETS)
    IntakeOk = get_metric_value(router_intake_messages_total, [{status, ok}]),
    IntakeFailed = get_metric_value(router_intake_messages_total, [{status, failed}]),
    
    %% Verify
    true = IntakeOk =:= ExpectedValid,
    true = IntakeFailed =:= ExpectedInvalid,
    
    %% Verify DLQ
    DLQCount = get_metric_value(router_intake_dlq_messages_total, []),
    true = DLQCount =:= ExpectedInvalid,
    ok.
```

### Chaos Tests Integration

**Location**: `apps/otp/router/test/router_intake_chaos_SUITE.erl`

**Metrics Verification**:
- After each chaos test, verify reconnection metrics
- Check that messages are eventually processed
- Verify no message loss

**Example**:
```erlang
verify_chaos_test_metrics(ExpectedReconnects) ->
    %% Verify reconnection metrics
    Reconnects = get_metric_value(router_nats_reconnect_total, []),
    true = Reconnects =:= ExpectedReconnects,
    
    %% Verify connection status
    ConnectionStatus = get_metric_value(router_nats_connection_status, []),
    true = ConnectionStatus =:= 1,  %% Connected after recovery
    
    %% Verify process alive
    RouterPid = whereis(router_decide_consumer),
    true = is_process_alive(RouterPid),
    ok.
```

## Metric Collection Best Practices

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

## Dashboard Panels for Load/Chaos/Rate-Limit Scenarios

### Load Test Scenarios

#### Panel 1: Message Processing Rate
**Query**: `rate(router_intake_messages_total[5m])`
**Visualization**: Time series graph
**What to Look For**:
- **High-Volume Success**: Steady rate matching sent message count, no drops
- **High-Volume Error**: Rate matches error count, all errors sent to DLQ
- **Mixed Stream**: Rate reflects distribution (70% ok, 30% failed)

#### Panel 2: Error Rate Over Time
**Query**: `rate(router_intake_validation_errors_total[5m])`
**Visualization**: Time series graph
**What to Look For**:
- **Success Flood**: Should be 0 throughout
- **Error Flood**: Should match error message rate
- **Mixed Stream**: Should reflect 30% error rate

#### Panel 3: DLQ Publication Status
**Query**: `rate(router_intake_dlq_messages_total[5m])` and `rate(router_intake_dlq_publish_failed_total[5m])`
**Visualization**: Stacked area chart
**What to Look For**:
- **Success Flood**: Both metrics = 0
- **Error Flood**: `dlq_messages_total` ≈ error count, `dlq_publish_failed_total` = 0
- **DLQ Failures**: If `dlq_publish_failed_total` > 0, investigate NATS/DLQ issues

#### Panel 4: Process Stability (Memory/Process Count)
**Query**: `process_resident_memory_bytes` and `erlang_vm_process_count`
**Visualization**: Time series graph
**What to Look For**:
- **Stable**: Memory and process count remain stable (< 10% growth)
- **Leak**: Memory or process count growing continuously (> 50% growth)

#### Panel 5: Idempotency Metrics
**Query**: `rate(router_idempotency_hit_total[5m])` and `rate(router_idempotency_miss_total[5m])`
**Visualization**: Stacked bar chart
**What to Look For**:
- **Idempotency Stress**: `hit_total` ≈ duplicates, `miss_total` ≈ unique messages
- **Ratio**: `hit_total / (hit_total + miss_total)` should match duplicate ratio

### Chaos Test Scenarios

#### Panel 6: NATS Connection Status
**Query**: `router_nats_connection_status` and `rate(router_nats_reconnect_total[5m])`
**Visualization**: Time series graph (status) + counter (reconnects)
**What to Look For**:
- **Single Restart**: Status drops to 0, then returns to 1; `reconnect_total` = 1
- **Multiple Restarts**: Status oscillates; `reconnect_total` = N_restarts
- **Recovery**: Status returns to 1 after each restart

#### Panel 7: Connection Errors
**Query**: `rate(router_nats_connection_errors_total[5m])`
**Visualization**: Time series graph
**What to Look For**:
- **During Restart**: Errors spike during NATS downtime
- **After Recovery**: Errors return to 0
- **No Infinite Errors**: Errors don't continue after recovery

#### Panel 8: Message Processing During Chaos
**Query**: `rate(router_intake_messages_total[5m])`
**Visualization**: Time series graph
**What to Look For**:
- **During Restart**: May drop to 0 or decrease
- **After Recovery**: Resumes normal rate
- **No Message Loss**: Total messages processed = messages sent (after recovery)

#### Panel 9: Gateway NATS Connection
**Query**: `gateway_nats_connection_status` and `rate(gateway_nats_publish_failures_total[5m])`
**Visualization**: Time series graph
**What to Look For**:
- **During Restart**: Connection status = 0, publish failures increase
- **After Recovery**: Connection status = 1, publish failures return to 0
- **HTTP 503**: `gateway_http_requests_total{status="503"}` may increase during restart

### Rate Limiting Scenarios

#### Panel 10: Rate Limit Hits vs Allowed
**Query**: `rate(gateway_rate_limit_hits_total[5m])` and `rate(gateway_rate_limit_allowed_total[5m])`
**Visualization**: Stacked area chart
**What to Look For**:
- **Under Limit**: `allowed_total` ≈ `hits_total`, `exceeded_total` = 0
- **At Limit**: `allowed_total` = limit, `exceeded_total` > 0
- **Ratio**: `exceeded_total / hits_total` should match expected rate limit hit ratio

#### Panel 11: HTTP Status Codes (429 vs Others)
**Query**: `rate(gateway_http_requests_total[5m])` by `status`
**Visualization**: Stacked area chart
**What to Look For**:
- **Rate Limit Exceeded**: `status="429"` > 0 when limit exceeded
- **Router Errors**: `status="400"`, `status="401"`, `status="500"` when Router errors occur
- **Priority**: 429 should appear BEFORE Router errors (rate limit checked first)

#### Panel 12: Router Intake vs Rate Limiting
**Query**: `rate(router_intake_messages_total[5m])` and `rate(gateway_rate_limit_exceeded_total[5m])`
**Visualization**: Dual Y-axis time series graph
**What to Look For**:
- **Rate Limit Active**: When `exceeded_total` > 0, `router_intake_messages_total` should be lower (Router not called)
- **No Conflict**: Router intake errors don't occur when rate limit exceeded
- **Correlation**: `router_intake_messages_total` ≈ `gateway_rate_limit_allowed_total`

### Combined Scenarios

#### Panel 13: Load + Rate Limiting
**Query**: Multiple metrics combined
**Visualization**: Multi-panel dashboard
**What to Look For**:
- **High Volume + Rate Limit**: Rate limit may be hit, Router processes allowed messages
- **Metrics Alignment**: `router_intake_messages_total` ≈ `gateway_rate_limit_allowed_total`
- **No Degradation**: Router latency stable even when rate limit active

#### Panel 14: Chaos + Rate Limiting
**Query**: NATS connection + rate limiting metrics
**Visualization**: Multi-panel dashboard
**What to Look For**:
- **NATS Restart**: Connection errors, reconnects
- **Rate Limit**: Continues to work during NATS issues (Gateway-level)
- **Recovery**: Both NATS and rate limiting resume after recovery

## Dashboard Configuration

### Grafana Dashboard JSON Structure

```json
{
  "dashboard": {
    "title": "Router & Gateway: Load/Chaos/Rate-Limit Monitoring",
    "panels": [
      {
        "title": "Message Processing Rate",
        "targets": [
          {
            "expr": "rate(router_intake_messages_total[5m])",
            "legendFormat": "{{status}}"
          }
        ]
      },
      {
        "title": "Error Rate",
        "targets": [
          {
            "expr": "rate(router_intake_validation_errors_total[5m])",
            "legendFormat": "{{error_code}}"
          }
        ]
      },
      {
        "title": "DLQ Status",
        "targets": [
          {
            "expr": "rate(router_intake_dlq_messages_total[5m])",
            "legendFormat": "DLQ Messages"
          },
          {
            "expr": "rate(router_intake_dlq_publish_failed_total[5m])",
            "legendFormat": "DLQ Failures"
          }
        ]
      },
      {
        "title": "Rate Limiting",
        "targets": [
          {
            "expr": "rate(gateway_rate_limit_hits_total[5m])",
            "legendFormat": "Hits"
          },
          {
            "expr": "rate(gateway_rate_limit_allowed_total[5m])",
            "legendFormat": "Allowed"
          },
          {
            "expr": "rate(gateway_rate_limit_exceeded_total[5m])",
            "legendFormat": "Exceeded"
          }
        ]
      },
      {
        "title": "NATS Connection",
        "targets": [
          {
            "expr": "router_nats_connection_status",
            "legendFormat": "Connection Status"
          },
          {
            "expr": "rate(router_nats_reconnect_total[5m])",
            "legendFormat": "Reconnects"
          }
        ]
      }
    ]
  }
}
```

#### Scenario 9: Backpressure & Overload (End-to-End)

**Test**: Router overloaded (high backlog, latency, in-flight messages)  
**Goal**: Verify backpressure detection, Router rejection, and Gateway response

**End-to-End Flow**:
1. Overload Router (send many messages or slow down processing)
2. Router detects backpressure (queue depth > 1000, latency > 5000ms, or in-flight > 500)
3. Router rejects new messages with NAK (increments `router_intake_backpressure_rejections_total`)
4. Gateway reads Router backpressure status via metrics endpoint
5. Gateway returns HTTP 503 Service Unavailable with `Retry-After` header
6. Router recovers (queue depth decreases, latency improves)
7. Gateway resumes normal processing (returns 200 OK)

#### Expected Metrics

**Router**:
- `router_jetstream_pending_messages{subject="beamline.router.v1.decide"}` > 1000 (overload threshold)
- `router_intake_processing_latency_p95{subject="beamline.router.v1.decide"}` > 5000 (overload threshold)
- `router_intake_backpressure_active{subject="beamline.router.v1.decide"}` == 1 (backpressure active)
- `router_intake_backpressure_rejections_total{subject="beamline.router.v1.decide"}` > 0 (rejections occurred)

**Gateway**:
- `gateway_http_requests_total{status="503"}` > 0 (503 responses due to backpressure)
- Gateway logs show "Router is overloaded" messages
- `Retry-After` header present in 503 responses
- `router_intake_inflight_messages{subject="beamline.router.v1.decide"}` > 500 (overload threshold)
- `router_intake_backpressure_active{subject="beamline.router.v1.decide"}` = 1 (active)
- `router_intake_backpressure_triggered_total{trigger="overload"}` > 0
- `router_intake_backpressure_rejections_total` > 0 (if rejection enabled)

**Gateway** (if monitoring Router metrics):
- `gateway_rate_limit_backpressure_adjustment_total` > 0 (if rate limit adjustment enabled)
- Rate limits reduced (if adjustment enabled)

#### Health Indicators

✅ **Healthy** (before overload):
- `pending_messages` < 100
- `latency_p95` < 500ms
- `inflight_messages` < 50
- `backpressure_active` = 0

⚠️ **Warning** (approaching overload):
- `pending_messages` > 100 (warning threshold)
- `latency_p95` > 500ms (warning threshold)
- `inflight_messages` > 50 (warning threshold)
- `backpressure_active` = 0 (not yet active)

❌ **Critical** (overload):
- `pending_messages` > 1000 (overload threshold)
- `latency_p95` > 5000ms (overload threshold)
- `inflight_messages` > 500 (overload threshold)
- `backpressure_active` = 1 (active)
- New requests rejected (HTTP 503)

#### Verification Checklist

- [ ] `router_jetstream_pending_messages` tracked correctly
- [ ] `router_intake_processing_latency_p95` tracked correctly
- [ ] `router_intake_inflight_messages` tracked correctly
- [ ] `router_intake_backpressure_active` = 1 when overloaded
- [ ] `router_intake_backpressure_triggered_total` incremented
- [ ] New requests rejected with HTTP 503 (if rejection enabled)
- [ ] Alerts emitted (backpressure active, queue depth high, latency high)
- [ ] Router continues processing (doesn't crash)
- [ ] Metrics show recovery after overload resolved

## Prometheus Queries Reference

**Load Test Queries**:
```promql
# Message processing rate
rate(router_intake_messages_total[5m])

# Error rate
rate(router_intake_validation_errors_total[5m])

# DLQ status
rate(router_intake_dlq_messages_total[5m])
rate(router_intake_dlq_publish_failed_total[5m])

# Idempotency
rate(router_idempotency_hit_total[5m])
rate(router_idempotency_miss_total[5m])
```

**Chaos Test Queries**:
```promql
# NATS connection
router_nats_connection_status
rate(router_nats_reconnect_total[5m])
rate(router_nats_connection_errors_total[5m])

# Message processing during chaos
rate(router_intake_messages_total[5m])
```

**Rate Limiting Queries**:
```promql
# Rate limit metrics
rate(gateway_rate_limit_hits_total[5m])
rate(gateway_rate_limit_allowed_total[5m])
rate(gateway_rate_limit_exceeded_total[5m])

# HTTP status codes
rate(gateway_http_requests_total[5m]) by (status)

# Router intake vs rate limiting
rate(router_intake_messages_total[5m])
rate(gateway_rate_limit_allowed_total[5m])
```

**Backpressure Queries**:
```promql
# Queue depth
router_jetstream_pending_messages{subject="beamline.router.v1.decide"}
router_jetstream_ack_pending_messages{subject="beamline.router.v1.decide"}

# Processing latency
router_intake_processing_latency_p95{subject="beamline.router.v1.decide"}
router_intake_processing_latency_p99{subject="beamline.router.v1.decide"}

# In-flight messages
router_intake_inflight_messages{subject="beamline.router.v1.decide"}
router_intake_inflight_messages_max{subject="beamline.router.v1.decide"}

# Backpressure status
router_intake_backpressure_active{subject="beamline.router.v1.decide"}
rate(router_intake_backpressure_triggered_total[5m])
rate(router_intake_backpressure_rejections_total[5m])
```

## References

- `config/observability/metrics.catalog.yaml`: Complete metrics catalog
- `apps/otp/router/src/router_metrics.erl`: Router metrics definitions
- `apps/otp/router/src/router_intake_error_handler.erl`: Intake error metrics
- `apps/c-gateway/src/metrics/metrics_registry.c`: Gateway metrics definitions
- `apps/c-gateway/src/http_server.c`: Gateway rate limiting metrics
- `docs/archive/dev/ROUTER_INTAKE_LOAD_TESTS_SPEC.md`: Load test scenarios
- `docs/archive/dev/ROUTER_CHAOS_TESTS_SPEC.md`: Chaos test scenarios
- `apps/otp/router/test/router_intake_e2e_SUITE.erl`: Load test implementation
- `apps/otp/router/test/router_intake_chaos_SUITE.erl`: Chaos test implementation
- `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`: Router intake operations runbook (troubleshooting procedures)
- `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`: Gateway rate limiting operations runbook (configuration and troubleshooting)

