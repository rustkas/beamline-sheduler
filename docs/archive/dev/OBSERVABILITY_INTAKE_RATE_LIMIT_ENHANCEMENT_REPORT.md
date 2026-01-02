# Observability Enhancement: Intake & Rate Limiting Metrics

**Date**: 2025-11-26  
**Status**: ✅ **Complete**  
**Purpose**: Enhance observability around Router intake and Gateway rate limiting to make load/chaos/rate-limit scenarios visible in metrics and logs, not just test reports.

## Executive Summary

Observability around Router intake validation and Gateway rate limiting has been enhanced. All relevant metrics are now documented in the metrics catalog, and comprehensive dashboard guidance has been added for load, chaos, and rate-limit test scenarios.

## Completed Tasks

### 4.1. ✅ Router Metrics Verification and Catalog Update

**Metrics Verified**:

1. **Intake Validation Metrics** (from `router_intake_error_handler.erl`):
   - `router_intake_validation_errors_total` - Counter with labels: `error_code`, `subject`, `tenant_id`
   - `router_intake_messages_total` - Counter with labels: `subject`, `status` (ok/failed)
   - `router_intake_dlq_messages_total` - Counter with labels: `reason`, `error_code`, `subject`
   - `router_intake_dlq_publish_failed_total` - Counter with labels: `reason`, `error_code`, `subject`, `failure_reason`

2. **Idempotency Metrics** (from `router_idempotency.erl`):
   - `router_idempotency_hit_total` - Counter with labels: `key_type`
   - `router_idempotency_miss_total` - Counter with labels: `key_type`

3. **NATS Connection Metrics** (from `router_nats.erl`):
   - `router_nats_connection_errors_total` - Counter with labels: `reason`
   - `router_nats_reconnect_total` - Counter (no labels)

**Catalog Updated**: `config/observability/metrics.catalog.yaml`
- Added Router intake metrics section
- Added Router idempotency metrics section
- Added Router NATS connection metrics section
- All metrics include: name, type, labels, description, component, language, module

### 4.2. ✅ Gateway Metrics Verification and Catalog Update

**Metrics Verified**:

1. **Rate Limiting Metrics** (from `metrics_registry.c`):
   - `gateway_rate_limit_hits_total` - Counter with labels: `endpoint`, `tenant_id`
   - `gateway_rate_limit_allowed_total` - Counter with labels: `endpoint`, `tenant_id`
   - `gateway_rate_limit_exceeded_total` - Counter with labels: `endpoint`, `tenant_id` (derived from hits - allowed)

2. **HTTP Metrics** (from `metrics_registry.c`):
   - `gateway_http_requests_total` - Counter with labels: `method`, `path`, `status`
   - `gateway_http_request_duration_seconds` - Histogram with labels: `method`, `path`
   - `gateway_http_requests_by_status` - Counter with labels: `status`

3. **NATS Metrics** (from `metrics_registry.c`):
   - `gateway_nats_messages_sent_total` - Counter with labels: `subject`
   - `gateway_nats_publish_failures_total` - Counter with labels: `subject`, `error`
   - `gateway_nats_connection_status` - Gauge (1=connected, 0=disconnected)

**Catalog Updated**: `config/observability/metrics.catalog.yaml`
- Added Gateway section with rate limiting, HTTP, and NATS metrics
- All metrics include: name, type, labels, description, component, language, module

### 4.3. ✅ Documentation and Dashboard Updates

**Files Updated**:

1. **`docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`**:
   - Added section "Dashboard Panels for Load/Chaos/Rate-Limit Scenarios"
   - Documented 14 dashboard panels with:
     - Panel title and type
     - Prometheus queries
     - Visualization recommendations
     - "What to Look For" guidance for each scenario type
   - Added section "Dashboard Configuration" with Grafana dashboard JSON structure
   - Added section "Prometheus Queries Reference" with categorized queries

2. **`apps/otp/router/docs/OBSERVABILITY_DASHBOARD.md`**:
   - Added section "Dashboard Panels for Load/Chaos/Rate-Limit Scenarios"
   - Documented panels for:
     - Load test scenarios (intake message processing, validation errors, DLQ status, idempotency)
     - Chaos test scenarios (NATS connection status, connection errors, message processing during chaos)

3. **`apps/c-gateway/docs/OBSERVABILITY_DASHBOARD.md`**:
   - Enhanced "Rate Limit Hits vs Allowed" panel with "What to Look For" guidance
   - Added panel "HTTP Status Codes (429 vs Others)" with rate limiting priority verification
   - Added panel "Router Intake vs Rate Limiting" with correlation verification

4. **`docs/OBSERVABILITY.md`**:
   - Added section "Metrics Monitoring for Load/Chaos/Rate-Limit Scenarios"
   - Added reference to `OBSERVABILITY_METRICS_MONITORING_GUIDE.md`

## Dashboard Panels Summary

### Load Test Scenarios (5 Panels)

1. **Message Processing Rate**: `rate(router_intake_messages_total[5m])` - Verify steady rate matching sent count
2. **Error Rate Over Time**: `rate(router_intake_validation_errors_total[5m])` - Verify error distribution
3. **DLQ Publication Status**: `rate(router_intake_dlq_messages_total[5m])` and `rate(router_intake_dlq_publish_failed_total[5m])` - Verify DLQ success
4. **Process Stability**: Memory and process count - Verify no leaks
5. **Idempotency Metrics**: `rate(router_idempotency_hit_total[5m])` and `rate(router_idempotency_miss_total[5m])` - Verify duplicate detection

### Chaos Test Scenarios (3 Panels)

6. **NATS Connection Status**: `router_nats_connection_status` and `rate(router_nats_reconnect_total[5m])` - Verify reconnection
7. **Connection Errors**: `rate(router_nats_connection_errors_total[5m])` - Verify error spikes during restart
8. **Message Processing During Chaos**: `rate(router_intake_messages_total[5m])` - Verify message processing recovery

### Rate Limiting Scenarios (3 Panels)

9. **Rate Limit Hits vs Allowed**: `rate(gateway_rate_limit_hits_total[5m])`, `rate(gateway_rate_limit_allowed_total[5m])`, `rate(gateway_rate_limit_exceeded_total[5m])` - Verify rate limit behavior
10. **HTTP Status Codes (429 vs Others)**: `rate(gateway_http_requests_total[5m])` by `status` - Verify 429 priority
11. **Router Intake vs Rate Limiting**: Correlation between `router_intake_messages_total` and `gateway_rate_limit_allowed_total` - Verify Router not called when rate limit exceeded

### Combined Scenarios (2 Panels)

12. **Load + Rate Limiting**: Combined metrics for high volume with rate limiting
13. **Chaos + Rate Limiting**: Combined metrics for NATS failures with rate limiting

## Metrics Catalog Updates

### Router Metrics Added

```yaml
# Intake validation metrics (CP2+)
- name: router_intake_messages_total
  type: counter
  labels: [subject, status]
  
- name: router_intake_validation_errors_total
  type: counter
  labels: [error_code, subject, tenant_id]
  
- name: router_intake_dlq_messages_total
  type: counter
  labels: [reason, error_code, subject]
  
- name: router_intake_dlq_publish_failed_total
  type: counter
  labels: [reason, error_code, subject, failure_reason]

# Idempotency metrics (CP2+)
- name: router_idempotency_hit_total
  type: counter
  labels: [key_type]
  
- name: router_idempotency_miss_total
  type: counter
  labels: [key_type]

# NATS connection metrics (CP2+)
- name: router_nats_connection_errors_total
  type: counter
  labels: [reason]
  
- name: router_nats_reconnect_total
  type: counter
  labels: []
```

### Gateway Metrics Added

```yaml
# Rate limiting metrics
- name: gateway_rate_limit_hits_total
  type: counter
  labels: [endpoint, tenant_id]
  
- name: gateway_rate_limit_allowed_total
  type: counter
  labels: [endpoint, tenant_id]

# HTTP metrics
- name: gateway_http_requests_total
  type: counter
  labels: [method, path, status]
  
- name: gateway_http_request_duration_seconds
  type: histogram
  labels: [method, path]
  
- name: gateway_http_requests_by_status
  type: counter
  labels: [status]

# NATS metrics
- name: gateway_nats_messages_sent_total
  type: counter
  labels: [subject]
  
- name: gateway_nats_publish_failures_total
  type: counter
  labels: [subject, error]
  
- name: gateway_nats_connection_status
  type: gauge
  labels: []
```

## Key Insights for Each Scenario Type

### Load Test Scenarios

**What to Look For**:
- **High-Volume Success**: All messages processed (`status="ok"` ≈ N), no errors, no DLQ
- **High-Volume Error**: All errors sent to DLQ, no DLQ failures, Router process alive
- **Mixed Stream**: Metrics reflect distribution (70% ok, 30% failed), DLQ only for errors
- **Idempotency Stress**: Duplicates detected, first message processed, duplicates fast-ACKed

**Key Metrics**:
- `router_intake_messages_total{status="ok"}` vs `router_intake_messages_total{status="failed"}`
- `router_intake_dlq_messages_total` vs error count
- `router_idempotency_hit_total` vs `router_idempotency_miss_total`
- Process memory and process count stability

### Chaos Test Scenarios

**What to Look For**:
- **NATS Restart**: Connection status drops to 0, then returns to 1; reconnects = N_restarts
- **Connection Errors**: Spike during restart, return to 0 after recovery
- **Message Processing**: Drops during restart, resumes after recovery; no message loss
- **Recovery**: All metrics return to normal after NATS recovery

**Key Metrics**:
- `router_nats_connection_status` (1=connected, 0=disconnected)
- `router_nats_reconnect_total` (should match number of restarts)
- `router_nats_connection_errors_total` (spikes during restart)
- `router_intake_messages_total` (resumes after recovery)

### Rate Limiting Scenarios

**What to Look For**:
- **Under Limit**: `allowed_total` ≈ `hits_total`, `exceeded_total` = 0
- **At Limit**: `allowed_total` = limit, `exceeded_total` > 0
- **Priority**: 429 responses appear BEFORE Router errors (rate limit checked first)
- **Correlation**: `router_intake_messages_total` ≈ `gateway_rate_limit_allowed_total` (Router not called when rate limit exceeded)

**Key Metrics**:
- `gateway_rate_limit_hits_total` vs `gateway_rate_limit_allowed_total` vs `gateway_rate_limit_exceeded_total`
- `gateway_http_requests_total{status="429"}` (rate limit exceeded)
- `gateway_http_requests_total{status="400"}` (Router intake errors)
- `router_intake_messages_total` (should be lower when rate limit exceeded)

## Prometheus Queries Reference

### Load Test Queries

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

### Chaos Test Queries

```promql
# NATS connection
router_nats_connection_status
rate(router_nats_reconnect_total[5m])
rate(router_nats_connection_errors_total[5m])

# Message processing during chaos
rate(router_intake_messages_total[5m])
```

### Rate Limiting Queries

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

## Verification

### Metrics Catalog Verification

- ✅ All Router intake metrics documented in `metrics.catalog.yaml`
- ✅ All Gateway rate limiting metrics documented in `metrics.catalog.yaml`
- ✅ All metrics include required fields (name, type, labels, description, component, language, module)

### Documentation Verification

- ✅ `OBSERVABILITY_METRICS_MONITORING_GUIDE.md` contains dashboard panel descriptions
- ✅ Router dashboard document contains load/chaos scenario panels
- ✅ Gateway dashboard document contains rate limiting scenario panels
- ✅ Main `OBSERVABILITY.md` references monitoring guide

### Code Verification

- ✅ Router metrics match implementation in `router_intake_error_handler.erl`
- ✅ Gateway metrics match implementation in `metrics_registry.c`
- ✅ All telemetry events documented in metrics catalog

## Summary

All tasks completed:

1. ✅ **Router metrics verified and catalog updated** - All `router_intake_*` metrics added to catalog
2. ✅ **Gateway metrics verified and catalog updated** - All rate limiting and HTTP metrics added to catalog
3. ✅ **Documentation and dashboards updated** - Comprehensive dashboard guidance added for load/chaos/rate-limit scenarios

**Result**: Load/chaos/rate-limit scenarios are now fully observable through metrics and logs, with clear dashboard guidance for monitoring each scenario type.

## References

- **Metrics Catalog**: `config/observability/metrics.catalog.yaml`
- **Monitoring Guide**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`
- **Router Dashboard**: `apps/otp/router/docs/OBSERVABILITY_DASHBOARD.md`
- **Gateway Dashboard**: `apps/c-gateway/docs/OBSERVABILITY_DASHBOARD.md`
- **Router Metrics Module**: `apps/otp/router/src/router_metrics.erl`
- **Router Intake Error Handler**: `apps/otp/router/src/router_intake_error_handler.erl`
- **Gateway Metrics Registry**: `apps/c-gateway/src/metrics/metrics_registry.c`
- **Load Test Spec**: `docs/archive/dev/ROUTER_INTAKE_LOAD_TESTS_SPEC.md`
- **Chaos Test Spec**: `docs/archive/dev/ROUTER_CHAOS_TESTS_SPEC.md`

