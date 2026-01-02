# Router Intake Backpressure Policy

**Scope**: ⏭ **CP3/Pre-Release** (detection and metrics in CP2, complete integration in CP3)

**CP2 Baseline**:
- ✅ Backpressure detection: `apps/otp/router/src/router_intake_backpressure.erl`
- ✅ Metrics: Queue depth, processing latency, in-flight messages
- ⚠️ Gateway reaction: PoC implementation (`apps/c-gateway/src/backpressure_client.c`)

**CP3/Pre-Release Requirements**:
- Complete Gateway → Router backpressure integration
- End-to-end overload scenarios testing
- Production-ready backpressure policies
- Full observability integration

**Reference**: `docs/archive/dev/TECH_DEBT_ROUTER_GATEWAY_INTAKE_RATE_LIMIT.md` (Backpressure section), `docs/archive/dev/CP2_TECH_DEBT_SUMMARY.md`

---

**Date**: 2025-11-26  
**Status**: 📋 **Design Document**  
**Purpose**: Define backpressure and queue management policies for Router intake  
**Related**: `docs/ARCHITECTURE/gateway-distributed-rate-limiting.md`

## Executive Summary

Router intake currently has validation, DLQ, and MaxDeliver handling, but lacks formalized backpressure/overload policies. This document defines:
- **Backpressure Triggers**: When to apply backpressure (JetStream backlog, processing latency)
- **Backpressure Actions**: What to do when overloaded (rate limiting, rejection, alerts)
- **Metrics & Monitoring**: Metrics for queue depth, processing latency, in-flight messages
- **SLO/SLI**: Service level objectives for intake processing
- **Overload Scenarios**: E2E test scenarios for overload handling

## Current State

### Existing Mechanisms

**Validation & Error Handling**:
- ✅ Intake validation (`router_intake_validator.erl`)
- ✅ DLQ support (configurable DLQ subject)
- ✅ MaxDeliver exhaustion detection (ETS-based tracking)
- ✅ Error codes and audit logging

**JetStream Integration**:
- ✅ Durable subscriptions with explicit ACK policy
- ✅ Queue groups for horizontal scaling
- ✅ MaxDeliver configuration (default: 3)
- ✅ Backoff configuration

**Missing**:
- ❌ No formal backpressure policy
- ❌ No queue depth monitoring
- ❌ No processing latency tracking
- ❌ No overload detection
- ❌ No automatic rate limiting adjustment
- ❌ No overload alerts

## Backpressure Triggers

### Trigger 1: JetStream Backlog Threshold

**Definition**: Number of pending messages in JetStream consumer exceeds threshold

**Metrics**:
- `router_jetstream_pending_messages` (gauge) - Pending messages per subject
- `router_jetstream_delivered_messages` (counter) - Delivered messages per subject
- `router_jetstream_ack_pending_messages` (gauge) - Messages awaiting ACK

**Thresholds**:
- **Warning**: `pending_messages > queue_warn` (default: 10)
- **Critical**: `pending_messages > queue_crit` (default: 100)
- **Overload**: `pending_messages > queue_overload` (default: 1000)

**Configuration**:
```erlang
{queue_warn, 10},      %% Warning threshold
{queue_crit, 100},     %% Critical threshold
{queue_overload, 1000} %% Overload threshold (trigger backpressure)
```

### Trigger 2: Processing Latency Threshold

**Definition**: Message processing latency exceeds threshold

**Metrics**:
- `router_intake_processing_latency_seconds` (histogram) - Processing latency per message
- `router_intake_processing_latency_p50` (gauge) - 50th percentile latency
- `router_intake_processing_latency_p95` (gauge) - 95th percentile latency
- `router_intake_processing_latency_p99` (gauge) - 99th percentile latency

**Thresholds**:
- **Warning**: `p95_latency > latency_warn_ms` (default: 500ms)
- **Critical**: `p95_latency > latency_crit_ms` (default: 2000ms)
- **Overload**: `p95_latency > latency_overload_ms` (default: 5000ms)

**Configuration**:
```erlang
{latency_warn_ms, 500},      %% Warning threshold (500ms)
{latency_crit_ms, 2000},     %% Critical threshold (2s)
{latency_overload_ms, 5000}  %% Overload threshold (5s)
```

### Trigger 3: In-Flight Messages Threshold

**Definition**: Number of messages currently being processed exceeds threshold

**Metrics**:
- `router_intake_inflight_messages` (gauge) - Messages currently being processed
- `router_intake_inflight_messages_max` (gauge) - Maximum in-flight messages

**Thresholds**:
- **Warning**: `inflight_messages > inflight_warn` (default: 50)
- **Critical**: `inflight_messages > inflight_crit` (default: 200)
- **Overload**: `inflight_messages > inflight_overload` (default: 500)

**Configuration**:
```erlang
{inflight_warn, 50},      %% Warning threshold
{inflight_crit, 200},     %% Critical threshold
{inflight_overload, 500}  %% Overload threshold
```

## Gateway Response to Backpressure

### Gateway Backpressure Client

**Implementation**: `apps/c-gateway/src/backpressure_client.c`

**Functionality**:
- Reads Router backpressure status via Prometheus metrics endpoint
- Caches status to avoid excessive HTTP requests (default: 5 seconds)
- Provides three status levels: `BACKPRESSURE_INACTIVE`, `BACKPRESSURE_WARNING`, `BACKPRESSURE_ACTIVE`

**Configuration**:
```bash
GATEWAY_ROUTER_METRICS_URL=http://localhost:8080/_metrics  # Router metrics endpoint
GATEWAY_BACKPRESSURE_CHECK_INTERVAL_SECONDS=5              # Cache TTL
GATEWAY_BACKPRESSURE_TIMEOUT_MS=1000                       # HTTP timeout
```

**Metrics Parsing**:
- Looks for `router_intake_backpressure_active` metric (1 = active, 0 = inactive)
- Checks `router_jetstream_pending_messages` and `router_intake_processing_latency_p95` for warning indicators

### Gateway Response Actions

**When `BACKPRESSURE_ACTIVE`**:
- **Action**: Return HTTP 503 Service Unavailable
- **Error Code**: `service_overloaded`
- **Message**: "Router is overloaded, please retry later"
- **Headers**: `Retry-After: 30` (seconds)
- **Priority**: Backpressure takes precedence over rate limiting

**When `BACKPRESSURE_WARNING`**:
- **Action**: Continue processing but apply stricter rate limiting
- **Behavior**: Rate limiter automatically applies stricter limits
- **Priority**: Warning does not block requests, only reduces rate limits

**When `BACKPRESSURE_INACTIVE`**:
- **Action**: Process requests normally
- **Behavior**: No special handling required

### Route Priority Policy

**Which routes are affected first**:

1. **High Priority Routes** (last to be affected):
   - `GET /_health` - Health checks (never blocked)
   - `GET /_metrics` - Metrics endpoint (never blocked)
   - `GET /api/v1/routes/decide/:messageId` - Read-only operations

2. **Medium Priority Routes** (affected after high priority):
   - `POST /api/v1/routes/decide` - Main routing endpoint (affected first)
   - `POST /api/v1/messages` - Message operations
   - `POST /api/v1/registry/blocks` - Registry operations

3. **Low Priority Routes** (affected first):
   - Admin endpoints (if any)
   - Non-critical operations

**Implementation**:
- Gateway checks backpressure status **before** rate limiting
- If backpressure is active, Gateway returns 503 immediately (no rate limit check)
- If backpressure is warning, Gateway applies stricter rate limiting

## Backpressure Actions

### Action 1: Rate Limiting Adjustment (Gateway)

**When**: JetStream backlog > `queue_overload` OR processing latency > `latency_overload_ms`

**Action**: Signal Gateway to reduce rate limits

**Mechanism**:
1. Router emits metric: `router_intake_backpressure_active` (gauge, 1 = active, 0 = inactive)
2. Gateway monitors this metric
3. Gateway reduces rate limits by configurable factor (e.g., 50% reduction)
4. Gateway logs rate limit adjustment

**Configuration**:
```erlang
{backpressure_rate_limit_reduction, 0.5},  %% Reduce rate limits by 50%
{backpressure_rate_limit_min, 10}         %% Minimum rate limit (req/min)
```

**Gateway Response**:
- Reduce `GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT` by 50%
- Log: `"Backpressure active: reducing rate limits"`
- Emit metric: `gateway_rate_limit_backpressure_adjustment_total`

### Action 2: Temporary Rejection (HTTP 503/429)

**When**: JetStream backlog > `queue_overload` AND processing latency > `latency_overload_ms`

**Action**: Router rejects new `decide` requests with HTTP 503

**Mechanism**:
1. Router checks backpressure status before processing
2. If backpressure active → return error response immediately
3. Error response: `{"ok": false, "error": {"code": "service_overloaded", "message": "Router is overloaded, please retry later"}}`
4. HTTP status: `503 Service Unavailable`
5. Headers: `Retry-After: 30` (seconds)

**Implementation**:
```erlang
check_backpressure() ->
    Pending = get_jetstream_pending_messages(),
    Latency = get_processing_latency_p95(),
    
    case {Pending > queue_overload(), Latency > latency_overload_ms()} of
        {true, true} -> {backpressure_active, 30};  %% Retry after 30s
        {true, false} -> {backpressure_warning, 0};
        {false, true} -> {backpressure_warning, 0};
        {false, false} -> {backpressure_inactive, 0}
    end.
```

### Action 3: External Alerts

**When**: Any backpressure trigger activated

**Action**: Emit alert to external monitoring system

**Alerts**:
1. **Backpressure Active Alert**: `router_intake_backpressure_active == 1`
2. **Queue Depth Alert**: `router_jetstream_pending_messages > queue_crit`
3. **Latency Alert**: `router_intake_processing_latency_p95 > latency_crit_ms`
4. **In-Flight Alert**: `router_intake_inflight_messages > inflight_crit`

**Alert Format**:
```json
{
  "alert_name": "router_intake_backpressure_active",
  "severity": "warning|critical",
  "message": "Router intake backpressure active: pending=1500, latency_p95=6000ms",
  "labels": {
    "subject": "beamline.router.v1.decide",
    "pending_messages": 1500,
    "latency_p95_ms": 6000,
    "inflight_messages": 450
  },
  "timestamp": "2025-11-26T12:00:00Z"
}
```

## Metrics & Monitoring

### New Metrics

#### Queue Depth Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
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

### Metric Collection

**JetStream Queue Depth**:
- Query JetStream consumer info via NATS API
- Update metrics every 5 seconds (configurable)
- Background process: `router_jetstream_monitor.erl`

**Processing Latency**:
- Track start time when message received
- Track end time when message ACKed
- Calculate latency: `end_time - start_time`
- Update histogram and percentiles

**In-Flight Messages**:
- Increment counter when message processing starts
- Decrement counter when message processing ends (ACK/NAK)
- Track maximum in-flight (peak)

## SLO/SLI for Intake

### Service Level Objectives (SLO)

**Availability**:
- **Target**: 99.9% availability (Router intake processing messages)
- **Measurement**: `(total_messages - failed_messages) / total_messages`
- **Window**: Rolling 30-day window

**Latency**:
- **Target**: P95 latency < 500ms for 99% of messages
- **Measurement**: `router_intake_processing_latency_p95 < 500ms`
- **Window**: Rolling 1-hour window

**Queue Depth**:
- **Target**: Pending messages < 100 for 99% of time
- **Measurement**: `router_jetstream_pending_messages < 100`
- **Window**: Rolling 1-hour window

### Service Level Indicators (SLI)

**Intake Processing SLI**:
```
SLI = (messages_processed_successfully) / (total_messages_received)
Target: SLI >= 0.999 (99.9%)
```

**Latency SLI**:
```
SLI = (messages_with_latency < 500ms) / (total_messages_processed)
Target: SLI >= 0.99 (99%)
```

**Queue Depth SLI**:
```
SLI = (time_with_pending < 100) / (total_time)
Target: SLI >= 0.99 (99%)
```

## Monitoring Guide Updates

### Dashboard Panels

**Queue Depth Panel**:
- Query: `router_jetstream_pending_messages{subject="beamline.router.v1.decide"}`
- Visualization: Line graph
- Thresholds: Warning (10), Critical (100), Overload (1000)
- Alert: When > 1000 for > 5 minutes

**Processing Latency Panel**:
- Query: `router_intake_processing_latency_p95{subject="beamline.router.v1.decide"}`
- Visualization: Line graph
- Thresholds: Warning (500ms), Critical (2000ms), Overload (5000ms)
- Alert: When > 2000ms for > 5 minutes

**In-Flight Messages Panel**:
- Query: `router_intake_inflight_messages{subject="beamline.router.v1.decide"}`
- Visualization: Line graph
- Thresholds: Warning (50), Critical (200), Overload (500)
- Alert: When > 200 for > 5 minutes

**Backpressure Status Panel**:
- Query: `router_intake_backpressure_active{subject="beamline.router.v1.decide"}`
- Visualization: Gauge (0 = inactive, 1 = active)
- Alert: When == 1 for > 1 minute

### Alert Rules

**Backpressure Active Alert**:
```yaml
alert: RouterIntakeBackpressureActive
expr: router_intake_backpressure_active{subject="beamline.router.v1.decide"} == 1
for: 1m
labels:
  severity: warning
annotations:
  summary: "Router intake backpressure is active"
  description: "Router intake is overloaded: pending={{ $value.pending }}, latency_p95={{ $value.latency_p95 }}ms"
```

**Queue Depth Alert**:
```yaml
alert: RouterIntakeQueueDepthHigh
expr: router_jetstream_pending_messages{subject="beamline.router.v1.decide"} > 100
for: 5m
labels:
  severity: critical
annotations:
  summary: "Router intake queue depth is high"
  description: "Pending messages: {{ $value }} (threshold: 100)"
```

**Latency Alert**:
```yaml
alert: RouterIntakeLatencyHigh
expr: router_intake_processing_latency_p95{subject="beamline.router.v1.decide"} > 2000
for: 5m
labels:
  severity: critical
annotations:
  summary: "Router intake processing latency is high"
  description: "P95 latency: {{ $value }}ms (threshold: 2000ms)"
```

## E2E Overload Scenarios

### Scenario 1: JetStream Backlog Overload

**Setup**:
- Send 2000 valid `decide` messages rapidly
- Simulate slow processing (add 100ms delay per message)

**Expected Behavior**:
- ✅ Router continues processing (doesn't crash)
- ✅ JetStream backlog grows (pending messages > 1000)
- ✅ Backpressure triggered (`router_intake_backpressure_active == 1`)
- ✅ Gateway rate limits reduced (if monitoring enabled)
- ✅ Alerts emitted (backpressure active, queue depth high)
- ✅ Metrics show overload state

**Verification**:
- `router_jetstream_pending_messages > 1000`
- `router_intake_backpressure_active == 1`
- `router_intake_backpressure_triggered_total > 0`
- Alerts received in monitoring system

### Scenario 2: Processing Latency Overload

**Setup**:
- Send 1000 valid `decide` messages
- Simulate very slow processing (add 2s delay per message)

**Expected Behavior**:
- ✅ Router continues processing (doesn't crash)
- ✅ Processing latency grows (p95 > 5000ms)
- ✅ Backpressure triggered (`router_intake_backpressure_active == 1`)
- ✅ New requests rejected with HTTP 503
- ✅ Alerts emitted (latency high, backpressure active)
- ✅ Metrics show overload state

**Verification**:
- `router_intake_processing_latency_p95 > 5000`
- `router_intake_backpressure_active == 1`
- `router_intake_backpressure_rejections_total > 0`
- HTTP 503 responses for new requests

### Scenario 3: In-Flight Messages Overload

**Setup**:
- Send 1000 valid `decide` messages concurrently
- Simulate slow processing (add 1s delay per message)

**Expected Behavior**:
- ✅ Router continues processing (doesn't crash)
- ✅ In-flight messages grow (inflight > 500)
- ✅ Backpressure triggered (`router_intake_backpressure_active == 1`)
- ✅ New requests rejected with HTTP 503
- ✅ Alerts emitted (in-flight high, backpressure active)
- ✅ Metrics show overload state

**Verification**:
- `router_intake_inflight_messages > 500`
- `router_intake_backpressure_active == 1`
- `router_intake_backpressure_rejections_total > 0`
- HTTP 503 responses for new requests

### Scenario 4: Combined Overload (Backlog + Latency)

**Setup**:
- Send 3000 valid `decide` messages rapidly
- Simulate very slow processing (add 3s delay per message)

**Expected Behavior**:
- ✅ Router continues processing (doesn't crash)
- ✅ All overload triggers activated (backlog, latency, in-flight)
- ✅ Backpressure triggered (`router_intake_backpressure_active == 1`)
- ✅ Gateway rate limits reduced (if monitoring enabled)
- ✅ New requests rejected with HTTP 503
- ✅ All alerts emitted
- ✅ Metrics show overload state

**Verification**:
- `router_jetstream_pending_messages > 1000`
- `router_intake_processing_latency_p95 > 5000`
- `router_intake_inflight_messages > 500`
- `router_intake_backpressure_active == 1`
- `router_intake_backpressure_rejections_total > 0`
- HTTP 503 responses for new requests

## Implementation Plan

### Phase 1: Metrics Collection

**Tasks**:
1. Add JetStream queue depth monitoring (`router_jetstream_monitor.erl`)
2. Add processing latency tracking (start/end timestamps)
3. Add in-flight message tracking (increment/decrement counters)
4. Add backpressure status metric

**Deliverables**:
- `router_jetstream_monitor.erl` - Background process for queue depth monitoring
- Updated `router_decide_consumer.erl` - Latency and in-flight tracking
- Updated `router_metrics.erl` - New metrics definitions

### Phase 2: Backpressure Detection

**Tasks**:
1. Implement backpressure trigger logic
2. Add backpressure status calculation
3. Emit backpressure metrics
4. Log backpressure events

**Deliverables**:
- `router_intake_backpressure.erl` - Backpressure detection module
- Updated consumers - Backpressure checks before processing

### Phase 3: Backpressure Actions

**Tasks**:
1. Implement HTTP 503 rejection for overload
2. Add Gateway rate limit adjustment signal (metric-based)
3. Implement alert emission
4. Add retry-after headers

**Deliverables**:
- Updated `router_decide_consumer.erl` - Rejection logic
- Gateway monitoring - Rate limit adjustment based on Router metrics
- Alert integration - External alert emission

### Phase 4: E2E Testing

**Tasks**:
1. Create overload test scenarios
2. Implement test helpers (slow processing simulation)
3. Add verification checks
4. Document test procedures

**Deliverables**:
- `router_intake_overload_SUITE.erl` - Overload test suite
- Test documentation - Overload scenario descriptions

## Configuration

### Application Configuration

```erlang
{queue_warn, 10},                    %% Warning threshold
{queue_crit, 100},                   %% Critical threshold
{queue_overload, 1000},              %% Overload threshold
{latency_warn_ms, 500},              %% Warning threshold (500ms)
{latency_crit_ms, 2000},             %% Critical threshold (2s)
{latency_overload_ms, 5000},         %% Overload threshold (5s)
{inflight_warn, 50},                 %% Warning threshold
{inflight_crit, 200},                %% Critical threshold
{inflight_overload, 500},            %% Overload threshold
{backpressure_rate_limit_reduction, 0.5},  %% Reduce rate limits by 50%
{backpressure_rate_limit_min, 10},   %% Minimum rate limit (req/min)
{backpressure_retry_after_seconds, 30}  %% Retry-After header value
```

## References

- **Intake Implementation**: `apps/otp/router/src/router_decide_consumer.erl`
- **Metrics**: `apps/otp/router/src/router_metrics.erl`
- **Observability Guide**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`
- **Rate Limiting**: `docs/ARCHITECTURE/gateway-distributed-rate-limiting.md`

