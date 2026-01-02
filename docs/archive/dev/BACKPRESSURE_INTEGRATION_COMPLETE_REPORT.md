# Backpressure Integration Complete Report

**Date**: 2025-01-27  
**Status**: ✅ Complete  
**Scope**: Phase "Actions" - Complete backpressure integration (detection → action)

## Summary

Completed all tasks for backpressure integration:

- ✅ **H.1**: Router - интеграция backpressure-сигнала в consumers
- ✅ **H.2**: Gateway - реакция на backpressure
- ✅ **H.3**: End-to-end overload / backpressure сценарий

## Implementation Details

### H.1: Router - Интеграция Backpressure в Consumers

**Files Modified**:
- `apps/otp/router/src/router_decide_consumer.erl`

**Changes**:

1. **Backpressure Check Before Processing**:
   - Added backpressure check at the start of `handle_decide_message_internal/4`
   - Checks backpressure status before any message processing

2. **NAK on Active Backpressure**:
   - When `backpressure_active`: NAK message with `Retry-After` delay
   - Increments `router_intake_backpressure_rejections_total` metric
   - Logs warning: "Router intake backpressure active - rejecting message"

3. **Warning State Handling**:
   - When `backpressure_warning`: Continue processing but log warning
   - Allows processing to continue with awareness of stress

4. **New Functions**:
   - `nak_message_if_needed/2`: NAK message with optional retry after delay
   - `process_message_with_backpressure_warning/4`: Process message with warning awareness
   - `process_message_normal/4`: Normal message processing (refactored from original code)

**Integration Points**:
- `router_intake_backpressure:check_backpressure/1` - Checks backpressure status
- `router_nats:nak_message/1` - NAKs message for redelivery
- `telemetry:execute/3` - Emits rejection metrics

**Behavior**:
- **Active Backpressure**: Message is NAK'd immediately, no processing
- **Warning Backpressure**: Message is processed but with warning logged
- **Inactive Backpressure**: Normal processing continues

### H.2: Gateway - Реакция на Backpressure

**Files Created**:
- `apps/c-gateway/src/backpressure_client.h`
- `apps/c-gateway/src/backpressure_client.c`

**Files Modified**:
- `apps/c-gateway/src/http_server.c`
- `apps/c-gateway/CMakeLists.txt`

**Changes**:

1. **Backpressure Client Implementation**:
   - Reads Router backpressure status via Prometheus metrics endpoint
   - Simple HTTP client (socket-based, no curl dependency)
   - Caching mechanism (default: 5 seconds TTL)
   - Parses `router_intake_backpressure_active` metric

2. **Gateway Response Actions**:
   - **BACKPRESSURE_ACTIVE**: Returns HTTP 503 Service Unavailable with `Retry-After: 30` header
   - **BACKPRESSURE_WARNING**: Continues processing but applies stricter rate limiting
   - **BACKPRESSURE_INACTIVE**: Normal processing

3. **Integration in HTTP Handler**:
   - Checks backpressure status before rate limiting
   - If active: Returns 503 immediately (no rate limit check)
   - If warning: Applies stricter rate limiting
   - If inactive: Normal processing

4. **New Functions**:
   - `backpressure_client_init/1`: Initialize backpressure client
   - `backpressure_client_check_router_status/0`: Check Router backpressure status (with caching)
   - `backpressure_client_get_cached_status/0`: Get cached status without HTTP request
   - `send_error_response_with_retry_after/5`: Send error response with Retry-After header
   - `send_response_with_headers/4`: Send response with custom headers

**Configuration**:
```bash
GATEWAY_ROUTER_METRICS_URL=http://localhost:8080/_metrics  # Router metrics endpoint
GATEWAY_BACKPRESSURE_CHECK_INTERVAL_SECONDS=5              # Cache TTL
GATEWAY_BACKPRESSURE_TIMEOUT_MS=1000                       # HTTP timeout
```

**Route Priority Policy**:
- **High Priority** (never blocked): `GET /_health`, `GET /_metrics`
- **Medium Priority** (affected after high): `POST /api/v1/routes/decide`, `POST /api/v1/messages`
- **Low Priority** (affected first): Admin endpoints, non-critical operations

**Priority Order**:
1. Backpressure check (if active → 503, no further checks)
2. Abuse detection check
3. Rate limiting check
4. Request processing

### H.3: End-to-End Overload / Backpressure Сценарий

**Files Created**:
- `tests/integration/gateway-router-backpressure.test.ts`

**Files Modified**:
- `apps/otp/router/test/router_intake_overload_SUITE.erl`
- `docs/ARCHITECTURE/router-intake-backpressure-policy.md`
- `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`

**Changes**:

1. **Router Tests**:
   - `test_backpressure_nak_rejection/1`: Verifies NAK rejection on active backpressure
   - `test_backpressure_recovery/1`: Verifies recovery from backpressure

2. **Integration Tests**:
   - `gateway-router-backpressure.test.ts`: End-to-end tests for backpressure handling
     - Backpressure detection and 503 response
     - Stricter rate limiting on warning
     - Recovery from backpressure
     - Priority over rate limiting

3. **Documentation Updates**:
   - `router-intake-backpressure-policy.md`: Added Gateway response section
   - `OBSERVABILITY_METRICS_MONITORING_GUIDE.md`: Added end-to-end flow description

**E2E Test Scenarios**:
1. **Backpressure Active**: Router overloaded → Gateway returns 503 with Retry-After
2. **Backpressure Warning**: Router under stress → Gateway applies stricter rate limiting
3. **Backpressure Recovery**: Router recovers → Gateway resumes normal processing
4. **Priority Test**: Backpressure takes precedence over rate limiting

## Complete Flow

### Normal Processing
1. Gateway receives HTTP request
2. Gateway checks backpressure status (cached, 5s TTL)
3. If inactive: Continue to rate limiting → Router
4. Router processes message normally

### Backpressure Active
1. Router detects overload (queue > 1000, latency > 5000ms, or in-flight > 500)
2. Router sets `router_intake_backpressure_active = 1`
3. Router rejects new messages with NAK (increments `router_intake_backpressure_rejections_total`)
4. Gateway reads Router metrics (via HTTP)
5. Gateway detects `router_intake_backpressure_active = 1`
6. Gateway returns HTTP 503 Service Unavailable with `Retry-After: 30` header
7. Client receives 503 and waits before retry

### Backpressure Warning
1. Router detects warning conditions (single threshold exceeded)
2. Router sets `router_intake_backpressure_active = 0` but logs warning
3. Gateway reads Router metrics
4. Gateway detects warning indicators (queue depth or latency high)
5. Gateway applies stricter rate limiting (reduces limits by 50%)
6. Gateway continues processing requests (with stricter limits)

### Recovery
1. Router queue depth decreases (< 1000)
2. Router latency improves (< 5000ms)
3. Router in-flight messages decrease (< 500)
4. Router sets `router_intake_backpressure_active = 0`
5. Gateway reads Router metrics
6. Gateway detects backpressure inactive
7. Gateway resumes normal processing (normal rate limits)

## Metrics

### Router Metrics

**Backpressure Status**:
- `router_intake_backpressure_active{subject}`: Gauge (1 = active, 0 = inactive)
- `router_intake_backpressure_triggered_total{subject, trigger}`: Counter
- `router_intake_backpressure_rejections_total{subject, reason}`: Counter

**Queue Depth**:
- `router_jetstream_pending_messages{subject, consumer}`: Gauge
- `router_jetstream_ack_pending_messages{subject, consumer}`: Gauge

**Processing Latency**:
- `router_intake_processing_latency_p95{subject}`: Gauge
- `router_intake_processing_latency_seconds{subject}`: Histogram

**In-Flight Messages**:
- `router_intake_inflight_messages{subject}`: Gauge

### Gateway Metrics

**HTTP Status Codes**:
- `gateway_http_requests_total{status="503"}`: Counter (503 responses due to backpressure)

**Backpressure Adjustments** (Future):
- `gateway_rate_limit_backpressure_adjustment_total`: Counter (rate limit adjustments)

## Configuration

### Router Configuration

```erlang
{beamline_router, [
    {queue_overload, 1000},        %% Overload threshold (pending messages)
    {latency_overload_ms, 5000},   %% Overload threshold (p95 latency, ms)
    {inflight_overload, 500}       %% Overload threshold (in-flight messages)
]}
```

### Gateway Configuration

```bash
# Backpressure client
GATEWAY_ROUTER_METRICS_URL=http://localhost:8080/_metrics
GATEWAY_BACKPRESSURE_CHECK_INTERVAL_SECONDS=5
GATEWAY_BACKPRESSURE_TIMEOUT_MS=1000
```

## Testing

### Router Tests

**Unit Tests** (`router_intake_overload_SUITE.erl`):
- `test_backpressure_nak_rejection/1`: Verifies NAK rejection on active backpressure
- `test_backpressure_recovery/1`: Verifies recovery from backpressure

**Integration Tests**:
- Overload scenarios trigger backpressure
- Messages are NAK'd when backpressure active
- Metrics are emitted correctly

### Gateway Tests

**Integration Tests** (`gateway-router-backpressure.test.ts`):
- Gateway returns 503 when Router backpressure is active
- Gateway applies stricter rate limiting when Router backpressure is warning
- Gateway resumes normal processing when Router backpressure is inactive
- Backpressure takes precedence over rate limiting

## Documentation

### Updated Documents

1. **`docs/ARCHITECTURE/router-intake-backpressure-policy.md`**:
   - Added "Gateway Response to Backpressure" section
   - Added "Route Priority Policy" section
   - Documented Gateway backpressure client implementation

2. **`docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`**:
   - Updated "Scenario 9: Backpressure & Overload" with end-to-end flow
   - Added Gateway metrics and verification checklist
   - Added recovery verification steps

## Next Steps (Future Enhancements)

1. **Enhanced Backpressure Detection**:
   - Real-time JetStream consumer info queries (not cached ETS)
   - More sophisticated overload detection algorithms
   - Adaptive thresholds based on system capacity

2. **Gateway Enhancements**:
   - NATS pub/sub for backpressure status (instead of HTTP polling)
   - gRPC health check integration for backpressure status
   - Dynamic rate limit adjustment based on backpressure severity

3. **Observability**:
   - Real-time backpressure dashboard
   - Backpressure event replay and analysis
   - Integration with alerting systems

4. **Testing**:
   - Full e2e tests with actual Router overload
   - Chaos tests for backpressure recovery
   - Load tests to verify backpressure effectiveness

## References

- `docs/ARCHITECTURE/router-intake-backpressure-policy.md`: Backpressure policy specification
- `apps/otp/router/src/router_intake_backpressure.erl`: Backpressure detection module
- `apps/c-gateway/src/backpressure_client.c`: Gateway backpressure client
- `apps/otp/router/test/router_intake_overload_SUITE.erl`: Router overload tests
- `tests/integration/gateway-router-backpressure.test.ts`: Gateway ↔ Router integration tests
- `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`: Observability guide with backpressure scenarios

## Status

✅ **Complete**: All Phase "Actions" tasks implemented:
- ✅ H.1: Router - интеграция backpressure в consumers (NAK rejection)
- ✅ H.2: Gateway - реакция на backpressure (503/Retry-After, stricter rate limiting)
- ✅ H.3: End-to-end overload / backpressure сценарий (tests and documentation)

