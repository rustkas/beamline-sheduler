# Abuse Detection Implementation Report (Phase 1-2)

**Date**: 2025-01-27  
**Status**: ✅ Complete  
**Scope**: Gateway and Router abuse detection logic (Phase 1-2)

## Summary

Implemented abuse detection logic for Gateway and Router as specified in `docs/ARCHITECTURE/gateway-router-abuse-scenarios.md`. This includes:

- **Gateway**: Aggregate tracking and detection (API key, IP, tenant)
- **Router**: Payload content/size distribution tracking
- **Logging**: Abuse event logging in both Gateway and Router

## Implementation Details

### G.1. Gateway: Aggregate/Detection in Code

**Files Modified**:
- `apps/c-gateway/src/http_server.c`
- `apps/c-gateway/src/abuse_detection.c` (already exists)
- `apps/c-gateway/src/metrics/metrics_registry.c`
- `apps/c-gateway/src/metrics/metrics_registry.h`

**Changes**:

1. **Client IP Extraction**:
   - Added `getpeername()` call in `handle_client()` to extract client IP address
   - Added `getpeername()` call in `handle_decide()` for abuse detection

2. **Abuse Detection Integration**:
   - Added `abuse_detection_track_request()` call in `handle_decide()` to track requests per tenant, API key, and IP
   - Added `abuse_detection_check_patterns()` call to detect abuse patterns
   - Added `abuse_detection_log_event()` call when abuse pattern is detected

3. **Metrics**:
   - Added `metric_abuse_events_total` counter in `metrics_registry.c`
   - Added `metrics_record_abuse_event()` function to record abuse events
   - Integrated abuse metric recording in `handle_decide()`

**Detection Patterns**:
- **Empty Payload**: Detected when `payload_size < min_payload_size`
- **Targeted Tenant Attack**: Detected when request rate exceeds `targeted_tenant_rate_threshold`
- **Rate Limit Evasion**: Detected when API key count or IP count exceeds thresholds
- **Heavy Payload**: Detected when large payload ratio exceeds threshold

**Configuration**:
- `ABUSE_DETECTION_ENABLED` (default: disabled)
- `ABUSE_MIN_PAYLOAD_SIZE` (default: 0)
- `ABUSE_LARGE_PAYLOAD_THRESHOLD` (default: 524288 bytes)
- `ABUSE_LARGE_PAYLOAD_RATIO_THRESHOLD` (default: 80%)
- `ABUSE_TARGETED_TENANT_RATE_THRESHOLD` (default: 100 req/min)
- `ABUSE_EVASION_API_KEYS_THRESHOLD` (default: 5)
- `ABUSE_EVASION_IPS_THRESHOLD` (default: 5)
- `ABUSE_RETENTION_WINDOW_SECONDS` (default: 300 seconds)

### G.2. Router: Payload Content/Size Distribution Tracking

**Files Modified**:
- `apps/otp/router/src/router_payload_tracker.erl` (already exists)
- `apps/otp/router/src/router_decide_consumer.erl`

**Changes**:

1. **Payload Tracking**:
   - `router_payload_tracker:track_payload/3` tracks payload size per tenant/endpoint
   - Maintains statistics: total requests, large payload count, total payload size, min/max payload size

2. **Abuse Pattern Detection**:
   - `router_payload_tracker:check_abuse_pattern/2` checks for heavy payload patterns
   - Returns `{abuse, heavy_payload, AbuseContext}` when pattern detected
   - Returns `{ok, normal}` when no abuse detected

3. **Integration in Consumer**:
   - `router_decide_consumer.erl` calls `router_payload_tracker:track_payload/3` for each message
   - Calls `router_payload_tracker:check_abuse_pattern/2` after tracking
   - Logs abuse event and emits metric when abuse detected

**Configuration**:
- `abuse_large_payload_threshold` (default: 524288 bytes)
- `abuse_large_payload_ratio_threshold` (default: 80%)
- `abuse_min_requests_for_pattern` (default: 10)
- `abuse_retention_window_seconds` (default: 300 seconds)

### G.3. Logging Abuse Events

**Gateway Logging**:
- `abuse_detection_log_event()` logs abuse events with:
  - Event type (`abuse.empty_payload`, `abuse.targeted_tenant`, etc.)
  - Tenant ID
  - Client IP
  - Request ID
  - Trace ID
  - Endpoint
  - Message (human-readable)

**Router Logging**:
- `log_abuse_event/7` in `router_decide_consumer.erl` logs abuse events with:
  - Event type (`abuse.heavy_payload`)
  - Tenant ID
  - Request ID
  - Trace ID
  - Subject
  - Message ID
  - Context (payload stats)

**Log Format**:
```json
{
  "event_type": "abuse.heavy_payload",
  "tenant_id": "tenant-123",
  "request_id": "req-456",
  "trace_id": "trace-789",
  "subject": "beamline.router.v1.decide",
  "msg_id": "msg-abc",
  "context": {
    "total_requests": 100,
    "large_payload_count": 85,
    "large_payload_ratio": 85,
    "avg_payload_size": 600000,
    "min_payload_size": 1000,
    "max_payload_size": 1000000
  }
}
```

## Metrics

### Gateway Metrics

**New Metric**:
- `gateway_abuse_events_total`: Total number of abuse events detected
  - Labels: `abuse_type`, `tenant_id` (reserved for CP2)

### Router Metrics

**Existing Metrics** (via Telemetry):
- `router_abuse.heavy_payload`: Abuse event for heavy payload pattern
  - Metadata: `tenant_id`, `total_requests`, `large_payload_count`, `large_payload_ratio`, etc.

## Testing

### Gateway Tests

**Existing Tests**:
- `tests/integration/gateway-router-abuse.test.ts`: Integration tests for abuse scenarios
  - Empty payload flood
  - Targeted tenant attack
  - Rate limit evasion
  - Heavy payload attacks
  - Multi-tenant flood
  - Combined attacks

### Router Tests

**Existing Tests**:
- `apps/otp/router/test/router_abuse_SUITE.erl`: Erlang test suite for abuse scenarios
  - Empty payload flood
  - Heavy payload attack
  - Targeted tenant attack
  - Multi-tenant flood
  - Payload size distribution tracking

## Configuration

### Gateway Environment Variables

```bash
# Enable abuse detection
ABUSE_DETECTION_ENABLED=1

# Payload thresholds
ABUSE_MIN_PAYLOAD_SIZE=0
ABUSE_LARGE_PAYLOAD_THRESHOLD=524288  # 500KB
ABUSE_LARGE_PAYLOAD_RATIO_THRESHOLD=80  # 80%

# Rate thresholds
ABUSE_TARGETED_TENANT_RATE_THRESHOLD=100  # req/min
ABUSE_EVASION_API_KEYS_THRESHOLD=5
ABUSE_EVASION_IPS_THRESHOLD=5

# Retention
ABUSE_RETENTION_WINDOW_SECONDS=300  # 5 minutes
```

### Router Configuration

```erlang
%% In sys.config or application environment
{beamline_router, [
    {abuse_large_payload_threshold, 524288},  %% 500KB
    {abuse_large_payload_ratio_threshold, 80},  %% 80%
    {abuse_min_requests_for_pattern, 10},
    {abuse_retention_window_seconds, 300}  %% 5 minutes
]}
```

## Next Steps (Phase 3-4)

1. **Alerts**: Add Prometheus alerts for abuse patterns
2. **Enhanced Detection**: Implement multi-tenant flood detection
3. **Response Actions**: Add automatic response actions (e.g., temporary blocking)
4. **Dashboard**: Create Grafana dashboard for abuse metrics

## References

- `docs/ARCHITECTURE/gateway-router-abuse-scenarios.md`: Abuse scenarios specification
- `tests/integration/gateway-router-abuse.test.ts`: Gateway abuse integration tests
- `apps/otp/router/test/router_abuse_SUITE.erl`: Router abuse test suite
- `docs/SECURITY_GUIDE.md`: Security guide with abuse scenarios section

## Status

✅ **Complete**: All Phase 1-2 tasks implemented:
- ✅ G.1. Gateway aggregate/detection in code
- ✅ G.2. Router payload content/size distribution tracking
- ✅ G.3. Logging abuse events

