# Rate Limiting Observability Specification

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: CP2-LC  
**Components**: Router (`apps/otp/router/`) + Gateway (`apps/c-gateway/`)

## Purpose

This document defines the observability contract for rate limiting across Router and Gateway components, ensuring consistent metrics, logs, and traceability.

## Metrics Specification

### Router Rate Limiting Metrics

**Base Metrics** (from `router_rate_limit_store.erl`):

1. **`router_rate_limit_allowed_total`**
   - **Type**: Counter
   - **Labels**: `scope` (policy|tenant), `tenant_id`
   - **Description**: Total number of requests allowed by Router rate limiting
   - **Emitted**: On every allowed request (token available)
   - **Location**: `apps/otp/router/src/router_rate_limit_store.erl:102-105`

2. **`router_rate_limit_exceeded_total`**
   - **Type**: Counter
   - **Labels**: `scope` (policy|tenant), `tenant_id`
   - **Description**: Total number of requests rejected by Router rate limiting
   - **Emitted**: On every exceeded request (no tokens available)
   - **Location**: `apps/otp/router/src/router_rate_limit_store.erl:116-119`

**Future Metrics** (CP2+):
- `router_rate_limit_current{scope, tenant_id}` - Gauge: Current token count
- `router_rate_limit_limit{scope, tenant_id}` - Gauge: Configured limit (RPS)
- `router_rate_limit_burst{scope, tenant_id}` - Gauge: Configured burst capacity

### Gateway Rate Limiting Metrics

**Base Metrics** (from Gateway Redis rate limiting):

1. **`gateway_rate_limit_hits_total`**
   - **Type**: Counter
   - **Labels**: `endpoint` (/api/v1/routes/decide, /api/v1/messages), `tenant_id`
   - **Description**: Total number of rate limit checks performed by Gateway
   - **Emitted**: On every rate limit check (both allowed and exceeded)

2. **`gateway_rate_limit_exceeded_total`**
   - **Type**: Counter
   - **Labels**: `endpoint`, `tenant_id`
   - **Description**: Total number of requests rejected by Gateway rate limiting
   - **Emitted**: On every exceeded request (429 response)

3. **`gateway_rate_limit_allowed_total`**
   - **Type**: Counter
   - **Labels**: `endpoint`, `tenant_id`
   - **Description**: Total number of requests allowed by Gateway rate limiting
   - **Emitted**: On every allowed request (proceeds to Router)

**Redis-Specific Metrics** (CP2+):
- `gateway_rate_limit_redis_errors_total{endpoint}` - Counter: Redis connection/operation errors
- `gateway_rate_limit_redis_fallback_total{endpoint}` - Counter: Fallback to local mode
- `gateway_rate_limit_redis_latency_ms{endpoint}` - Histogram: Redis operation latency

### Combined Metrics (Router + Gateway)

**Cross-Component Metrics**:

1. **`rate_limit_layer_blocked_total`**
   - **Type**: Counter
   - **Labels**: `layer` (gateway|router), `endpoint`, `tenant_id`, `scope` (for Router)
   - **Description**: Total number of requests blocked by each layer
   - **Purpose**: Identify which layer (Gateway or Router) blocked the request

2. **`rate_limit_double_drop_total`**
   - **Type**: Counter
   - **Labels**: `endpoint`, `tenant_id`
   - **Description**: Total number of requests blocked by both layers (should be 0)
   - **Purpose**: Detect configuration issues (double-dropping)

## Logging Specification

### Router Rate Limiting Logs

**Log Format** (structured JSON):

```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "WARN",
  "component": "router",
  "module": "router_rate_limit_store",
  "message": "Rate limit exceeded",
  "context": {
    "scope": "policy",
    "tenant_id": "tenant_123",
    "policy_id": "default",
    "limit": 100,
    "burst": 50,
    "retry_after_seconds": 30,
    "request_id": "req-123"
  }
}
```

**Log Levels**:
- **ERROR**: Rate limit store unavailable, configuration errors
- **WARN**: Rate limit exceeded (expected behavior, but worth logging)
- **INFO**: Rate limit configuration changes, bucket resets
- **DEBUG**: Token refill, bucket status queries

### Gateway Rate Limiting Logs

**Log Format** (structured JSON):

```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "WARN",
  "component": "gateway",
  "module": "rate_limiter",
  "message": "Rate limit exceeded",
  "context": {
    "endpoint": "/api/v1/routes/decide",
    "tenant_id": "tenant_123",
    "limit": 100,
    "window_seconds": 60,
    "retry_after_seconds": 30,
    "request_id": "req-123"
  }
}
```

**Log Levels**:
- **ERROR**: Redis unavailable, fallback to local mode, configuration errors
- **WARN**: Rate limit exceeded (expected behavior)
- **INFO**: Rate limit configuration changes, Redis connection status
- **DEBUG**: Rate limit check details, Redis operation details

## Traceability

### Identifying Which Layer Blocked

**Gateway Block**:
- **HTTP Status**: 429
- **Error Format**: `{ error: "rate_limit_exceeded", ... }` (no `intake_error_code`)
- **Headers**: `X-RateLimit-*`, `Retry-After`
- **Metrics**: `gateway_rate_limit_exceeded_total` incremented
- **Logs**: Gateway logs "Rate limit exceeded" with endpoint context

**Router Block**:
- **HTTP Status**: 429 (forwarded by Gateway)
- **Error Format**: `{ ok: false, error: { code: "rate_limit_exceeded", details: {...} } }`
- **Error Details**: Includes `scope`, `policy_id`, `tenant_id`
- **Metrics**: `router_rate_limit_exceeded_total` incremented
- **Logs**: Router logs "Rate limit exceeded" with policy/tenant context

### Detecting Double-Dropping

**Indicators**:
1. **Metrics**: `rate_limit_double_drop_total > 0` (should always be 0)
2. **Logs**: Both Gateway and Router log rate limit exceeded for same request
3. **Error Format**: Ambiguous error format (both Gateway and Router error formats present)

**Investigation**:
1. Check Gateway rate limit configuration (too low?)
2. Check Router policy rate limit configuration (too low?)
3. Verify execution order (Gateway → Router, not parallel)
4. Check for race conditions in rate limit checks

## Observability Integration

### Prometheus Export

**Router**:
- **Endpoint**: `http://router:9001/metrics`
- **Format**: Prometheus text format
- **Metrics**: `router_rate_limit_*` counters

**Gateway**:
- **Endpoint**: `http://gateway:8080/_metrics` (or `/metrics`)
- **Format**: Prometheus text format
- **Metrics**: `gateway_rate_limit_*` counters

### Grafana Dashboards

**Recommended Dashboards**:

1. **Rate Limiting Overview**:
   - Gateway rate limit hits/exceeded (by endpoint)
   - Router rate limit allowed/exceeded (by scope)
   - Combined rate limit blocks (by layer)

2. **Rate Limiting by Tenant**:
   - Gateway rate limit hits/exceeded (by tenant)
   - Router rate limit allowed/exceeded (by tenant)
   - Tenant-level rate limit trends

3. **Rate Limiting by Policy**:
   - Router rate limit allowed/exceeded (by policy)
   - Policy-level rate limit trends
   - Burst capacity utilization

### Alerting Rules

**Recommended Alerts**:

1. **High Rate Limit Exceeded Rate**:
   - **Metric**: `rate(router_rate_limit_exceeded_total[5m]) > 10`
   - **Severity**: Warning
   - **Description**: Router rate limit exceeded more than 10 times per second

2. **Gateway Rate Limit Exceeded**:
   - **Metric**: `rate(gateway_rate_limit_exceeded_total[5m]) > 50`
   - **Severity**: Warning
   - **Description**: Gateway rate limit exceeded more than 50 times per second

3. **Double-Dropping Detected**:
   - **Metric**: `rate_limit_double_drop_total > 0`
   - **Severity**: Critical
   - **Description**: Requests blocked by both Gateway and Router (configuration issue)

4. **Redis Rate Limiting Unavailable**:
   - **Metric**: `rate(gateway_rate_limit_redis_errors_total[5m]) > 5`
   - **Severity**: Warning
   - **Description**: Redis rate limiting errors (fallback to local mode)

## Test Coverage

### Metrics Verification

**Test Suite**: `apps/otp/router/test/router_rate_limit_store_SUITE.erl`

**Test Cases**:
- `test_rate_limit_metrics`: Verifies `router_rate_limit_allowed_total` and `router_rate_limit_exceeded_total` are emitted

**E2E Test**: `tests/integration/gateway-router-rate-limiting-e2e.test.ts`

**Test Cases**:
- Verifies Gateway and Router metrics are emitted correctly
- Verifies no double-dropping (metrics don't conflict)

### Log Verification

**Test Cases**:
- Verify Gateway logs rate limit exceeded with correct context
- Verify Router logs rate limit exceeded with correct context
- Verify log format matches specification

## Integration with OBSERVABILITY_METRICS_SPEC_CP2

**Router Rate Limiting Metrics** (Section 6.2):
- `router_rate_limit_allowed_total{scope, tenant_id, policy_id}` - Counter
- `router_rate_limit_exceeded_total{scope, tenant_id, policy_id}` - Counter

**Gateway Rate Limiting Metrics** (Section 6.1):
- `gateway_rate_limit_hits_total{endpoint, tenant_id}` - Counter
- `gateway_rate_limit_allowed_total{endpoint, tenant_id}` - Counter
- `gateway_rate_limit_exceeded_total{endpoint, tenant_id}` - Counter

**Reference**: `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` (Sections 6.1 and 6.2)

## References

- `apps/otp/router/src/router_rate_limit_store.erl` - Router implementation
- `apps/c-gateway/src/rate_limiter_redis.c` - Gateway implementation
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - General metrics specification (Sections 6.1, 6.2)
- `docs/archive/dev/RATE_LIMIT_BOUNDARIES_ROUTER_VS_GATEWAY.md` - Boundaries document
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Alert rules

