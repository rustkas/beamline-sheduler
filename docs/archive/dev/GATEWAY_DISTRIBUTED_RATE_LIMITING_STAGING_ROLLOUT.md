# Gateway Distributed Rate Limiting: Staging Rollout Plan

**Date**: 2025-01-27  
**Status**: 📋 **Staging Rollout Plan**  
**Purpose**: Plan for rolling out distributed rate limiting (Redis backend) in staging environment  
**Related**: `docs/GATEWAY_RATE_LIMITING.md`, `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`

## Executive Summary

This document describes the staging rollout plan for distributed rate limiting in Gateway. The plan includes:
- Integration into http_server.c (F.1)
- Feature flags and configuration (F.2)
- Staging rollout phases (F.3)
- Test scenarios and monitoring
- Rollback procedures

## F.1: Integration into http_server.c

### Current State

**Status**: ✅ **Complete**

**Changes Made**:
1. **Rate Limiter API Integration**:
   - Replaced direct in-memory RL usage with `rate_limiter_*` interface
   - Global rate limiter instance: `g_rate_limiter`
   - Initialization: `rate_limit_init_if_needed()`
   - Check function: `rate_limit_check()`

2. **Fallback Logic**:
   - Automatic fallback to in-memory mode when Redis unavailable
   - Fallback behavior configurable via `GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL`
   - Fallback tracking and logging

3. **Mode Logging**:
   - Mode tracking: `rl_mode` variable (`memory` | `redis` | `fallback` | `error`)
   - Startup logging: `INFO: Rate limiter initialized in {mode} mode`
   - Fallback logging: `WARNING: Rate limiter error, checking fallback`
   - Mode change logging: `INFO: Rate limiter mode changed to fallback`

**Code Locations**:
- `apps/c-gateway/src/http_server.c`: Lines 431-642
- `apps/c-gateway/src/rate_limiter.c`: Configuration parsing
- `apps/c-gateway/src/rate_limiter_redis.c`: Redis backend implementation

### Verification

**Fallback Behavior**:
- ✅ Fallback to in-memory when Redis unavailable
- ✅ Logging of mode changes
- ✅ Fallback count tracking (logs every 100 requests)
- ✅ Graceful degradation (allows requests on fallback)

**Mode Logging**:
- ✅ Startup mode logged
- ✅ Fallback events logged
- ✅ Mode changes logged
- ✅ Redis connection details logged (host, port, timeout)

## F.2: Feature Flags / Configuration

### Environment Variables

**Core Configuration**:
- `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED` (default: `false`)
  - Enable distributed rate limiting
  - Values: `true` | `false` | `1` | `0`

- `GATEWAY_RATE_LIMIT_MODE` (default: `local`)
  - Rate limiting mode selection (takes precedence over `GATEWAY_RATE_LIMIT_BACKEND`)
  - Values: `local` | `redis` | `hybrid`
  - `local`: In-memory mode (CP1, default)
  - `redis`: Distributed mode with Redis backend (CP2+)
  - `hybrid`: Redis backend with local cache (CP2+, experimental)

- `GATEWAY_RATE_LIMIT_BACKEND` (default: `memory`)
  - Backend selection (used if `GATEWAY_RATE_LIMIT_MODE` not set)
  - Values: `memory` | `redis`

**Redis Configuration**:
- `GATEWAY_RATE_LIMIT_REDIS_URI` (optional, **recommended for staging/production**)
  - Full Redis URI: `redis://host:port` or `redis://:password@host:port`
  - **Priority**: `GATEWAY_RATE_LIMIT_REDIS_URI` > `GATEWAY_RATE_LIMIT_REDIS_HOST:PORT`
  - If set, overrides `GATEWAY_RATE_LIMIT_REDIS_HOST` and `GATEWAY_RATE_LIMIT_REDIS_PORT`

- `GATEWAY_RATE_LIMIT_REDIS_HOST` (default: `localhost`)
  - Redis hostname or IP address
  - Used if `GATEWAY_RATE_LIMIT_REDIS_URI` not set

- `GATEWAY_RATE_LIMIT_REDIS_PORT` (default: `6379`)
  - Redis port number
  - Used if `GATEWAY_RATE_LIMIT_REDIS_URI` not set

- `GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS` (default: `1000`)
  - Redis connection timeout in milliseconds
  - Recommended: `2000` for staging

**Fallback Configuration**:
- `GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL` (default: `true`)
  - Enable fallback to in-memory mode when Redis unavailable
  - Values: `true` | `false` | `1` | `0`
  - **Note**: If `false`, requests will return 503 Service Unavailable when Redis is unavailable

**Local Cache Configuration** (for hybrid mode):
- `GATEWAY_RATE_LIMIT_LOCAL_CACHE_TTL_SECONDS` (default: `10`)
  - Local cache TTL in seconds

- `GATEWAY_RATE_LIMIT_SYNC_INTERVAL_SECONDS` (default: `5`)
  - Background sync interval in seconds

### Configuration Examples

**Staging Mode (Recommended)**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-staging.example.com:6379
export GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS=2000
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
export GATEWAY_LOG_LEVEL=INFO
```

**Production Mode (After Staging Validation)**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-prod.example.com:6379
export GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS=1000
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
```

**Strict Mode (No Fallback)**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-staging.example.com:6379
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=false
```

### Documentation Updates

**Updated Files**:
- ✅ `docs/GATEWAY_RATE_LIMITING.md`: Added `GATEWAY_RATE_LIMIT_MODE` and `GATEWAY_RATE_LIMIT_REDIS_URI`
- ✅ `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`: Added distributed rate limiting configuration section

## F.3: Staging Rollout Plan

### Overview

**Goal**: Roll out distributed rate limiting to one staging stack, validate behavior, and collect metrics.

**Timeline**: 2-3 weeks (6 phases)

**Success Criteria**:
- ✅ All integration tests pass
- ✅ Load tests pass (latency < 5ms overhead)
- ✅ Fallback works correctly when Redis unavailable
- ✅ No increase in error rate
- ✅ Metrics and logs show correct behavior

### Phase 1: Infrastructure Setup (Day 1)

**Tasks**:
1. **Deploy Redis to Staging**:
   - Deploy Redis instance (or use existing staging Redis)
   - Verify connectivity from Gateway instances
   - Configure Redis persistence (if needed)

2. **Configure Gateway Staging Stack**:
   - Set environment variables for distributed rate limiting
   - Deploy Gateway with new configuration
   - Verify startup logs show "Rate limiter initialized in redis mode"

3. **Health Checks**:
   - Verify Gateway health endpoint responds
   - Verify Gateway metrics endpoint shows rate limiting metrics
   - Verify Redis connectivity

**Configuration**:
```bash
# Staging Gateway environment
GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
GATEWAY_RATE_LIMIT_MODE=redis
GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-staging:6379
GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS=2000
GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
GATEWAY_LOG_LEVEL=INFO
```

**Verification**:
- ✅ Gateway starts successfully
- ✅ Logs show: "INFO: Rate limiter initialized in redis mode"
- ✅ Logs show: "INFO: Redis backend: redis-staging:6379 (timeout: 2000ms)"
- ✅ Health endpoint: `GET /_health` returns 200
- ✅ Metrics endpoint: `GET /_metrics` shows rate limiting metrics

### Phase 2: Integration Tests (Day 2-3)

**Tasks**:
1. **Run Integration Tests**:
   - Run existing Gateway rate limiting tests
   - Run Gateway ↔ Router integration tests
   - Verify all tests pass

2. **Test Scenarios**:
   - **Scenario 1**: Under-limit requests (should pass)
   - **Scenario 2**: At-limit requests (should pass)
   - **Scenario 3**: Over-limit requests (should return 429)
   - **Scenario 4**: Multi-tenant requests (should be isolated)
   - **Scenario 5**: Redis unavailable (should fallback to local)
   - **Scenario 6**: Redis recovery (should return to redis mode)

**Test Commands**:
```bash
# Integration tests
cd tests
pnpm test tests/integration/gateway-rate-limiting.test.ts

# Gateway ↔ Router integration
pnpm test tests/integration/gateway-router-error-handling.test.ts
```

**Success Criteria**:
- ✅ All integration tests pass
- ✅ Scenario 1-4: Correct rate limiting behavior
- ✅ Scenario 5: Fallback works (logs show "fallback" mode)
- ✅ Scenario 6: Recovery works (logs show "redis" mode restored)

### Phase 3: Load Tests (Day 4-5)

**Tasks**:
1. **Baseline Load Test** (in-memory mode):
   - Run load test with in-memory rate limiting
   - Measure: latency, throughput, error rate
   - Record baseline metrics

2. **Distributed Load Test** (Redis mode):
   - Run same load test with Redis rate limiting
   - Measure: latency, throughput, error rate
   - Compare with baseline

3. **Multi-Instance Load Test**:
   - Run load test with 2-3 Gateway instances
   - Verify consistent rate limiting across instances
   - Measure: latency, throughput, error rate

**Load Test Configuration**:
```bash
# Load test script
RATE_LIMIT=100  # Requests per minute
TENANT_COUNT=10
REQUESTS_PER_TENANT=150
GATEWAY_URL=http://gateway-staging:8080
```

**Success Criteria**:
- ✅ Latency overhead < 5ms (compared to in-memory)
- ✅ Throughput matches baseline (within 5%)
- ✅ Error rate < 0.1%
- ✅ Multi-instance consistency: rate limits enforced correctly

### Phase 4: Redis Failure Testing (Day 6-7)

**Tasks**:
1. **Redis Unavailable Test**:
   - Stop Redis instance
   - Send requests to Gateway
   - Verify fallback behavior
   - Measure: latency, error rate, fallback count

2. **Redis Recovery Test**:
   - Restart Redis instance
   - Send requests to Gateway
   - Verify recovery behavior
   - Measure: latency, error rate, mode restoration

3. **Partial Redis Failure Test**:
   - Simulate Redis timeout (slow responses)
   - Send requests to Gateway
   - Verify fallback behavior
   - Measure: latency, error rate, fallback count

**Test Scenarios**:
- **Scenario A**: Redis down at startup (Gateway should start in fallback mode)
- **Scenario B**: Redis down during operation (Gateway should switch to fallback mode)
- **Scenario C**: Redis recovery (Gateway should return to redis mode)
- **Scenario D**: Redis timeout (Gateway should fallback after timeout)

**Success Criteria**:
- ✅ Scenario A: Gateway starts successfully in fallback mode
- ✅ Scenario B: Gateway switches to fallback mode (logs show mode change)
- ✅ Scenario C: Gateway returns to redis mode (logs show mode restoration)
- ✅ Scenario D: Gateway falls back after timeout (logs show fallback)

### Phase 5: Monitoring and Observability (Day 8-10)

**Tasks**:
1. **Metrics Collection**:
   - Collect metrics for 48 hours
   - Analyze: latency, throughput, error rate, fallback count
   - Compare with baseline (in-memory mode)

2. **Log Analysis**:
   - Analyze logs for mode changes
   - Analyze logs for fallback events
   - Analyze logs for Redis errors

3. **Alert Configuration**:
   - Configure alerts for fallback events
   - Configure alerts for Redis errors
   - Configure alerts for high latency

**Metrics to Monitor**:
- `gateway_rate_limit_hits_total{endpoint,tenant}`: Rate limit checks
- `gateway_rate_limit_exceeded_total{endpoint,tenant}`: Rate limit exceeded
- `gateway_http_request_duration_seconds`: Request latency
- `gateway_http_requests_total{status}`: Request counts
- Custom metrics: `gateway_rate_limit_mode` (gauge), `gateway_rate_limit_fallback_count` (counter)

**Success Criteria**:
- ✅ Metrics show correct rate limiting behavior
- ✅ Logs show mode changes and fallback events
- ✅ Alerts configured and tested
- ✅ No anomalies in metrics (compared to baseline)

### Phase 6: Production Readiness Assessment (Day 11-14)

**Tasks**:
1. **Performance Analysis**:
   - Analyze latency overhead (target: < 5ms)
   - Analyze throughput impact (target: < 5% reduction)
   - Analyze error rate (target: < 0.1%)

2. **Reliability Analysis**:
   - Analyze fallback behavior (target: 100% success rate)
   - Analyze recovery behavior (target: < 10s recovery time)
   - Analyze Redis error handling (target: graceful degradation)

3. **Documentation Review**:
   - Review operational runbook
   - Review configuration documentation
   - Review monitoring guide

**Success Criteria**:
- ✅ Latency overhead < 5ms
- ✅ Throughput impact < 5%
- ✅ Error rate < 0.1%
- ✅ Fallback success rate = 100%
- ✅ Recovery time < 10s
- ✅ Documentation complete

### Rollback Procedures

**Immediate Rollback** (if critical issues):
```bash
# Disable distributed rate limiting
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=false
# Or set mode to local
export GATEWAY_RATE_LIMIT_MODE=local
# Restart Gateway
```

**Gradual Rollback** (if performance issues):
```bash
# Enable fallback (already enabled by default)
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
# Increase Redis timeout to reduce fallback triggers
export GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS=5000
# Restart Gateway
```

**Full Rollback** (if stability issues):
```bash
# Revert to in-memory mode
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=false
export GATEWAY_RATE_LIMIT_MODE=local
# Restart Gateway
# Remove Redis configuration
```

## Test Scenarios

### Scenario 1: Under-Limit Requests

**Setup**:
- Rate limit: 100 req/min
- Requests: 50 requests

**Expected**:
- All requests return 200 OK
- No 429 responses
- Metrics: `gateway_rate_limit_hits_total` = 50
- Metrics: `gateway_rate_limit_exceeded_total` = 0

### Scenario 2: At-Limit Requests

**Setup**:
- Rate limit: 100 req/min
- Requests: 100 requests

**Expected**:
- All requests return 200 OK
- Last request shows `X-RateLimit-Remaining: 0`
- Metrics: `gateway_rate_limit_hits_total` = 100
- Metrics: `gateway_rate_limit_exceeded_total` = 0

### Scenario 3: Over-Limit Requests

**Setup**:
- Rate limit: 100 req/min
- Requests: 150 requests

**Expected**:
- First 100 requests return 200 OK
- Next 50 requests return 429 Too Many Requests
- Headers: `X-RateLimit-Remaining: 0`, `Retry-After: 60`
- Metrics: `gateway_rate_limit_hits_total` = 150
- Metrics: `gateway_rate_limit_exceeded_total` = 50

### Scenario 4: Multi-Tenant Requests

**Setup**:
- Rate limit: 100 req/min per tenant
- Tenants: tenant-a, tenant-b
- Requests: 150 requests per tenant

**Expected**:
- Each tenant limited independently
- tenant-a: 100 OK, 50 429
- tenant-b: 100 OK, 50 429
- Metrics: `gateway_rate_limit_exceeded_total{tenant="tenant-a"}` = 50
- Metrics: `gateway_rate_limit_exceeded_total{tenant="tenant-b"}` = 50

### Scenario 5: Redis Unavailable

**Setup**:
- Rate limit: 100 req/min
- Redis: Stopped
- Requests: 50 requests

**Expected**:
- Gateway starts in fallback mode (logs show "fallback")
- All requests return 200 OK (fallback allows)
- Logs: "WARNING: Rate limiter error, checking fallback"
- Logs: "INFO: Fallback to local mode enabled, allowing request"
- Metrics: `gateway_rate_limit_fallback_count` increases

### Scenario 6: Redis Recovery

**Setup**:
- Rate limit: 100 req/min
- Redis: Stopped, then restarted
- Requests: 50 requests before, 50 requests after

**Expected**:
- Before restart: Fallback mode, all requests allowed
- After restart: Redis mode restored (logs show "redis")
- All requests return 200 OK
- Logs: "INFO: Rate limiter mode changed to redis"

## Monitoring and Observability

### Key Metrics

**Rate Limiting Metrics**:
- `gateway_rate_limit_hits_total{endpoint,tenant}`: Total rate limit checks
- `gateway_rate_limit_exceeded_total{endpoint,tenant}`: Total rate limit exceeded
- `gateway_rate_limit_mode` (gauge): Current mode (0=memory, 1=redis, 2=fallback)
- `gateway_rate_limit_fallback_count` (counter): Total fallback events

**Performance Metrics**:
- `gateway_http_request_duration_seconds{endpoint,status}`: Request latency
- `gateway_http_requests_total{endpoint,status}`: Request counts
- `gateway_rate_limit_check_duration_seconds` (histogram): Rate limit check latency

**Redis Metrics** (if available):
- `gateway_redis_queries_total`: Total Redis queries
- `gateway_redis_errors_total`: Total Redis errors
- `gateway_redis_latency_seconds`: Redis query latency

### Log Patterns

**Startup**:
```
INFO: Rate limiter initialized in redis mode
INFO: Redis backend: redis-staging:6379 (timeout: 2000ms)
```

**Fallback**:
```
WARNING: Rate limiter error (mode: redis), checking fallback
INFO: Fallback to local mode enabled, allowing request (fallback count: 1)
INFO: Rate limiter mode changed to fallback due to errors
```

**Recovery**:
```
INFO: Rate limiter mode changed to redis
INFO: Redis backend: redis-staging:6379 (timeout: 2000ms)
```

### Alert Rules

**Fallback Alert** (Warning):
```yaml
- alert: GatewayRateLimitFallbackActive
  expr: gateway_rate_limit_mode == 2
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "Gateway rate limiting in fallback mode"
    description: "Rate limiting has fallen back to local mode for {{ $value }} minutes"
```

**Redis Error Alert** (Critical):
```yaml
- alert: GatewayRateLimitRedisErrorsHigh
  expr: rate(gateway_redis_errors_total[5m]) > 10
  for: 2m
  labels:
    severity: critical
  annotations:
    summary: "High Redis error rate for rate limiting"
    description: "Redis error rate: {{ $value }} errors/sec"
```

## Success Criteria

### Functional Requirements

- ✅ All integration tests pass
- ✅ All load tests pass
- ✅ Fallback works correctly (100% success rate)
- ✅ Recovery works correctly (< 10s recovery time)
- ✅ Multi-instance consistency (rate limits enforced correctly)

### Performance Requirements

- ✅ Latency overhead < 5ms (compared to in-memory)
- ✅ Throughput impact < 5% (compared to in-memory)
- ✅ Error rate < 0.1%

### Operational Requirements

- ✅ Metrics available and accurate
- ✅ Logs show correct mode and fallback events
- ✅ Alerts configured and tested
- ✅ Documentation complete

## Next Steps

After successful staging rollout:

1. **Production Rollout**:
   - Apply same configuration to production
   - Monitor for 1 week
   - Collect metrics and logs

2. **Optimization**:
   - Optimize Redis connection pooling
   - Optimize local cache (if hybrid mode)
   - Optimize fallback behavior

3. **Feature Enhancements**:
   - Add connection pooling
   - Add retry logic with exponential backoff
   - Add circuit breaker
   - Add metrics for Redis operations

## References

- `docs/GATEWAY_RATE_LIMITING.md`: Complete rate limiting specification
- `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`: Operational runbook
- `apps/c-gateway/src/http_server.c`: HTTP server implementation
- `apps/c-gateway/src/rate_limiter_redis.c`: Redis backend implementation
- `docs/archive/dev/TECH_DEBT_ROUTER_GATEWAY_INTAKE_RATE_LIMIT.md`: Tech debt items
