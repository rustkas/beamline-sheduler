# Gateway Rate Limiting Migration Plan: CP1 → CP2+

**Date**: 2025-11-26  
**Status**: 📋 **Migration Plan**  
**Purpose**: Detailed migration strategy from CP1 (in-memory) to CP2+ (distributed) rate limiting  
**Related**: `docs/ARCHITECTURE/gateway-distributed-rate-limiting.md`

## Executive Summary

This document outlines the migration strategy from CP1 in-memory rate limiting to CP2+ distributed rate limiting. The migration is designed to be:
- **Gradual**: Feature toggle allows gradual rollout
- **Backward Compatible**: CP1 mode remains available as fallback
- **Low Risk**: Graceful degradation if distributed backend unavailable
- **Testable**: Can be tested in production with feature toggle

## Current State (CP1)

### Implementation

**Location**: `apps/c-gateway/src/http_server.c`

**Characteristics**:
- In-memory fixed-window rate limiting
- Per-endpoint limits (no per-tenant/per-key)
- Single-instance only (no cross-instance coordination)
- Simple implementation (~100 lines of code)

**Limitations**:
- ❌ Inconsistent limits across multiple Gateway instances
- ❌ No per-tenant or per-API-key limits
- ❌ Counters reset on Gateway restart
- ❌ Fixed-window algorithm only

## Target State (CP2+)

### Implementation

**Characteristics**:
- Distributed rate limiting with Redis backend
- Per-endpoint, per-tenant, per-API-key limits
- Consistent limits across all Gateway instances
- Hybrid mode (local cache + Redis sync)
- Graceful degradation to CP1 mode

**Benefits**:
- ✅ Consistent limits across instances
- ✅ Per-tenant and per-API-key limits
- ✅ Persistent counters (survive Gateway restarts)
- ✅ Future: Sliding-window and token bucket algorithms

## Migration Phases

### Phase 1: Preparation (CP2 Foundation)

**Goal**: Prepare codebase for distributed rate limiting without breaking CP1

**Tasks**:
1. **Refactor Rate Limiting Interface**
   - Extract rate limiting logic into separate module
   - Create abstract interface: `rate_limiter_t` with methods:
     - `init()` - Initialize rate limiter
     - `check(endpoint, tenant_id, api_key, remaining_out)` - Check rate limit
     - `cleanup()` - Cleanup resources
   - Keep CP1 implementation as `rate_limiter_memory.c`

2. **Add Feature Toggle**
   - Environment variable: `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED` (default: `false`)
   - Runtime selection: CP1 (memory) vs CP2 (distributed)
   - No code changes to existing HTTP handlers

3. **Add Configuration Structure**
   ```c
   typedef struct {
       int enabled;                    // Enable distributed rate limiting
       const char *backend;            // "redis" | "nats" | "memory"
       const char *redis_host;         // Redis host
       int redis_port;                 // Redis port
       int local_cache_ttl_seconds;   // Local cache TTL
       int sync_interval_seconds;      // Background sync interval
       int fallback_to_local;          // Fallback to local-only
   } distributed_rl_config_t;
   ```

**Deliverables**:
- `apps/c-gateway/src/rate_limiter.h` - Abstract interface
- `apps/c-gateway/src/rate_limiter_memory.c` - CP1 implementation (refactored)
- `apps/c-gateway/src/http_server.c` - Updated to use abstract interface

**Testing**:
- ✅ Verify CP1 mode still works (backward compatibility)
- ✅ Verify feature toggle works (can switch between modes)
- ✅ No performance regression in CP1 mode

### Phase 2: Redis Backend Implementation (CP2)

**Goal**: Implement Redis-based distributed rate limiting

**Tasks**:
1. **Redis Client Integration**
   - Choose Redis client library: `hiredis` (lightweight, C-native)
   - Add Redis connection management (connection pool)
   - Handle Redis connection failures gracefully

2. **Distributed Rate Limiter**
   - Implement `rate_limiter_redis.c` with Redis backend
   - Fixed-window algorithm using Redis INCR + EXPIRE
   - Key format: `rate_limit:{endpoint}:{tenant_id}:{api_key}:{window_start}`
   - Atomic operations: `INCR key`, `EXPIRE key ttl`

3. **Local Cache Layer**
   - Implement `rate_limiter_cache.c` for local caching
   - Cache structure: `{key: count, ttl: expiration_time}`
   - Cache invalidation on window reset
   - Background sync thread (optional, for soft limits)

4. **Hybrid Mode**
   - Combine local cache + Redis
   - Fast path: Check local cache first
   - Slow path: Check Redis if cache miss or over limit
   - Background sync: Periodically sync local cache with Redis

**Deliverables**:
- `apps/c-gateway/src/rate_limiter_redis.c` - Redis backend
- `apps/c-gateway/src/rate_limiter_cache.c` - Local cache
- `apps/c-gateway/src/rate_limiter_distributed.c` - Hybrid mode
- `apps/c-gateway/CMakeLists.txt` - Add hiredis dependency

**Testing**:
- ✅ Unit tests for Redis operations
- ✅ Integration tests with Redis (single instance)
- ✅ Multi-instance tests (2-3 Gateway instances + Redis)

### Phase 3: Feature Toggle & Gradual Rollout (CP2)

**Goal**: Enable distributed rate limiting with feature toggle

**Tasks**:
1. **Feature Toggle Implementation**
   - Environment variable: `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true`
   - Runtime selection: Memory vs Redis backend
   - Configuration validation (Redis connection, etc.)

2. **Graceful Degradation**
   - Detect Redis unavailability
   - Fallback to CP1 (memory) mode automatically
   - Log warnings when fallback occurs
   - Resume distributed mode when Redis recovers

3. **Monitoring & Observability**
   - Metrics: `gateway_rate_limit_backend_requests_total{backend="redis|memory"}`
   - Metrics: `gateway_rate_limit_backend_latency_seconds{backend="redis|memory"}`
   - Metrics: `gateway_rate_limit_fallback_total` (fallback events)
   - Logs: Rate limiter mode changes, Redis connection status

**Deliverables**:
- Feature toggle implementation
- Graceful degradation logic
- Monitoring metrics and logs

**Testing**:
- ✅ Feature toggle works (can enable/disable)
- ✅ Graceful degradation works (Redis failure → CP1 mode)
- ✅ Monitoring metrics are emitted correctly

### Phase 4: Production Validation (CP2)

**Goal**: Validate distributed rate limiting in production

**Tasks**:
1. **Canary Deployment**
   - Enable for single Gateway instance
   - Monitor latency, accuracy, Redis load
   - Compare with CP1 mode (baseline)

2. **Gradual Rollout**
   - Enable for 10% of Gateway instances
   - Monitor for 24-48 hours
   - Enable for 50% of instances
   - Monitor for 24-48 hours
   - Enable for 100% of instances

3. **Validation Criteria**
   - ✅ Latency: p95 < 5ms (Redis queries)
   - ✅ Accuracy: ±5% of configured limit
   - ✅ Redis load: < 50% CPU, < 80% memory
   - ✅ No increase in error rate
   - ✅ Graceful degradation works (test Redis failure)

**Deliverables**:
- Production validation report
- Performance benchmarks
- Rollback plan (if needed)

### Phase 5: Full Migration (CP2+)

**Goal**: Complete migration to distributed rate limiting

**Tasks**:
1. **Remove CP1 Mode** (Optional, CP2+)
   - Keep CP1 as fallback only (not primary mode)
   - Or remove CP1 entirely if distributed mode is stable

2. **Enhancements**
   - Per-tenant limits (CP2+)
   - Per-API-key limits (CP2+)
   - Sliding-window algorithm (CP2+)
   - Token bucket algorithm (CP2+)

**Deliverables**:
- Full distributed rate limiting
- Enhanced features (per-tenant, per-key)
- Advanced algorithms (sliding-window, token bucket)

## Feature Toggle Design

### Configuration

**Environment Variables**:
```bash
# Enable distributed rate limiting (default: false = CP1 mode)
GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true

# Backend selection (default: "memory" = CP1 mode)
GATEWAY_RATE_LIMIT_BACKEND=redis  # redis | memory

# Redis configuration (required if backend=redis)
GATEWAY_RATE_LIMIT_REDIS_HOST=localhost
GATEWAY_RATE_LIMIT_REDIS_PORT=6379
GATEWAY_RATE_LIMIT_REDIS_PASSWORD=  # Optional

# Local cache configuration (for hybrid mode)
GATEWAY_RATE_LIMIT_LOCAL_CACHE_TTL_SECONDS=10
GATEWAY_RATE_LIMIT_SYNC_INTERVAL_SECONDS=5

# Fallback behavior
GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true  # Fallback to CP1 if Redis unavailable
```

### Runtime Selection

**Code Structure**:
```c
// Initialize rate limiter based on configuration
rate_limiter_t *limiter = NULL;

if (distributed_rl_config.enabled && strcmp(distributed_rl_config.backend, "redis") == 0) {
    // CP2: Distributed mode (Redis)
    limiter = rate_limiter_redis_create(&distributed_rl_config);
} else {
    // CP1: Memory mode (backward compatible)
    limiter = rate_limiter_memory_create();
}

// Use limiter interface (same for both modes)
int result = limiter->check(limiter, endpoint, tenant_id, api_key, &remaining);
```

### Fallback Strategy

**Flow**:
```
1. Check if distributed mode enabled
   - If disabled → Use CP1 (memory) mode

2. Check if Redis backend available
   - If unavailable and fallback enabled → Use CP1 (memory) mode
   - If unavailable and fallback disabled → Return error (503 Service Unavailable)

3. Use distributed mode (Redis)
   - Check local cache first (if hybrid mode)
   - Check Redis if cache miss or over limit
   - Update local cache
```

## Testing Strategy

### Unit Tests

**Test Files**:
- `apps/c-gateway/tests/test_rate_limiter_memory.c` - CP1 mode tests
- `apps/c-gateway/tests/test_rate_limiter_redis.c` - Redis backend tests
- `apps/c-gateway/tests/test_rate_limiter_cache.c` - Local cache tests

**Test Scenarios**:
- ✅ Under limit: Request allowed
- ✅ At limit: Request allowed (last in window)
- ✅ Over limit: Request rejected (429)
- ✅ Window reset: Counter resets correctly
- ✅ Cache hit/miss: Local cache works correctly
- ✅ Redis failure: Graceful degradation works

### Integration Tests

**Test Infrastructure**: Docker Compose
- 3 Gateway instances
- 1 Redis instance
- Load testing client

**Test Scenarios**:
1. **Single Instance (CP1 Mode)**
   - Verify backward compatibility
   - Verify rate limiting works as before

2. **Multi-Instance (CP2 Mode)**
   - Send requests to 3 Gateway instances
   - Verify consistent limits across instances
   - Verify total rate limit = configured limit (not 3×limit)

3. **Redis Failure**
   - Stop Redis container
   - Verify Gateway falls back to CP1 mode
   - Verify Gateway continues operating
   - Restart Redis
   - Verify Gateway resumes CP2 mode

4. **High Load**
   - Send 1000+ req/s to 3 Gateway instances
   - Verify latency (p95 < 5ms)
   - Verify accuracy (±5% of limit)
   - Verify Redis load (CPU < 50%, memory < 80%)

### Chaos Tests

**Scenarios**:
- Redis restart during active traffic
- Network partition (Gateway ↔ Redis)
- Redis memory exhaustion
- Redis connection pool exhaustion

**Expected Behavior**:
- ✅ Graceful degradation to CP1 mode
- ✅ No request failures (429 still works, just inconsistent limits)
- ✅ Automatic recovery when Redis available

## Rollback Plan

### Rollback Triggers

**Conditions for Rollback**:
- Latency p95 > 10ms (target: < 5ms)
- Accuracy error > 10% (target: ±5%)
- Redis load > 80% CPU or memory
- Error rate increase > 1%
- Graceful degradation not working

### Rollback Procedure

**Steps**:
1. **Immediate**: Set `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=false` (all instances)
2. **Verify**: Confirm all instances using CP1 mode
3. **Monitor**: Check error rate, latency return to baseline
4. **Investigate**: Root cause analysis
5. **Fix**: Address issues before re-enabling

**Rollback Time**: < 5 minutes (environment variable change + restart)

## Success Criteria

### Phase 2 (Redis Implementation)

- ✅ Redis backend implemented and tested
- ✅ Local cache implemented and tested
- ✅ Hybrid mode works correctly
- ✅ Unit tests pass (100% coverage)
- ✅ Integration tests pass (multi-instance)

### Phase 3 (Feature Toggle)

- ✅ Feature toggle works (can enable/disable)
- ✅ Graceful degradation works (Redis failure → CP1)
- ✅ Monitoring metrics are emitted
- ✅ No performance regression in CP1 mode

### Phase 4 (Production Validation)

- ✅ Latency: p95 < 5ms (Redis queries)
- ✅ Accuracy: ±5% of configured limit
- ✅ Redis load: < 50% CPU, < 80% memory
- ✅ No increase in error rate
- ✅ Graceful degradation tested and working

### Phase 5 (Full Migration)

- ✅ All Gateway instances using distributed mode
- ✅ CP1 mode removed or deprecated
- ✅ Enhanced features implemented (per-tenant, per-key)
- ✅ Advanced algorithms implemented (sliding-window, token bucket)

## Timeline

| Phase | Duration | Status |
|-------|----------|--------|
| **Phase 1: Preparation** | 1-2 weeks | 📋 Planned |
| **Phase 2: Redis Implementation** | 2-3 weeks | 📋 Planned |
| **Phase 3: Feature Toggle** | 1 week | 📋 Planned |
| **Phase 4: Production Validation** | 2-4 weeks | 📋 Planned |
| **Phase 5: Full Migration** | 1-2 weeks | 📋 Planned |

**Total Estimated Duration**: 7-12 weeks

## Dependencies

### External Dependencies

- **Redis**: Redis 6.0+ (for improved performance)
- **hiredis**: Redis client library for C
- **Docker Compose**: For multi-instance testing

### Internal Dependencies

- **Gateway HTTP Server**: Rate limiting integration point
- **Metrics System**: Rate limiting metrics emission
- **Configuration System**: Environment variable parsing

## Risks & Mitigations

### Risk 1: Redis Latency

**Risk**: Redis queries add significant latency (> 10ms)

**Mitigation**:
- Use local cache (hybrid mode) to reduce Redis queries
- Deploy Redis close to Gateway instances (same datacenter)
- Use Redis connection pooling
- Monitor latency metrics and alert if > 5ms

### Risk 2: Redis Availability

**Risk**: Redis becomes unavailable, breaking rate limiting

**Mitigation**:
- Implement graceful degradation (fallback to CP1 mode)
- Use Redis HA (sentinel or cluster)
- Monitor Redis health and alert on failures
- Test fallback behavior regularly

### Risk 3: Redis Load

**Risk**: High rate limit check rate overwhelms Redis

**Mitigation**:
- Use local cache to reduce Redis queries (90%+ cache hit rate)
- Scale Redis horizontally (cluster mode)
- Monitor Redis load and scale proactively
- Set Redis connection pool limits

### Risk 4: Inconsistent Limits During Migration

**Risk**: Some instances use CP1, others use CP2, causing inconsistent limits

**Mitigation**:
- Use feature toggle for gradual rollout
- Monitor rate limit metrics per instance
- Complete migration quickly (within 1-2 days)
- Test multi-instance consistency before rollout

## References

- **Architecture Document**: `docs/ARCHITECTURE/gateway-distributed-rate-limiting.md`
- **Current Implementation**: `apps/c-gateway/src/http_server.c` (lines 400-600)
- **Rate Limiting Spec**: `docs/GATEWAY_RATE_LIMITING.md`
- **Gateway Routes**: `docs/GATEWAY_ROUTES_SPEC.md`

