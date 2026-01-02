# Gateway Distributed Rate Limiting Architecture

**Scope**: ⏭ **CP3/Pre-Release** (not CP2)

**CP2 Baseline**:
- ✅ PoC implementation: `apps/c-gateway/src/rate_limiter_redis.c`
- ✅ Integration into HTTP pipeline: `apps/c-gateway/src/http_server.c`
- ✅ Feature flags: `GATEWAY_RATE_LIMIT_MODE`, `GATEWAY_RATE_LIMIT_REDIS_URI`
- ✅ Staging rollout plan: `docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_STAGING_ROLLOUT.md`

**CP3/Pre-Release Requirements**:
- Production-ready distributed rate limiting
- Connection pooling, retry logic, circuit breaker
- Full observability (metrics, alerts, dashboards)
- Production rollout and validation

**Reference**: `docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_STAGING_ROLLOUT.md`, `docs/archive/dev/CP2_TECH_DEBT_SUMMARY.md`

---

**Date**: 2025-11-26  
**Status**: 📋 **Design Document**  
**Purpose**: Design distributed rate limiting for Gateway to support multi-instance deployments  
**Current State**: CP1 - In-memory fixed-window rate limiting (single instance)  
**Target State**: CP2+ - Distributed rate limiting with shared state

## Executive Summary

Current Gateway rate limiting (CP1) uses in-memory fixed-window counters per process. This works for single-instance deployments but breaks with scale-out: each Gateway instance maintains independent counters, leading to inconsistent limits (N instances = N×limit effective rate).

This document designs a distributed rate limiting solution that:
- Maintains consistent limits across multiple Gateway instances
- Supports gradual migration from CP1 (in-memory) to CP2+ (distributed)
- Provides configurable backends (Redis, NATS JetStream, SQL)
- Balances latency, accuracy, and operational complexity

## Current Implementation (CP1)

### Architecture

**Location**: `apps/c-gateway/src/http_server.c`

**Algorithm**: Fixed-window rate limiting
- Window size: `GATEWAY_RATE_LIMIT_TTL_SECONDS` (default: 60 seconds)
- Per-endpoint limits: `GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT` (default: 50 req/min)
- Storage: In-memory arrays (`rl_counters[RL_ENDPOINT_MAX]`)
- Window tracking: `rl_window_started_at` (time_t)

**Key Limitations**:
1. **No cross-instance coordination**: Each Gateway instance has independent counters
2. **No tenant isolation**: Global counters per endpoint (no per-tenant limits)
3. **No persistence**: Counters reset on Gateway restart
4. **Fixed window only**: No sliding window or token bucket support

**Code Structure**:
```c
static unsigned int rl_counters[RL_ENDPOINT_MAX] = {0};
static time_t rl_window_started_at = 0;
static rate_limit_config_t rl_config = {0};

static int rate_limit_check(rl_endpoint_id_t endpoint, const char *tenant_id, unsigned int *remaining_out);
```

## Requirements

### Functional Requirements

1. **Consistent Limits**: Rate limits must be consistent across all Gateway instances
2. **Per-Tenant Limits**: Support per-tenant rate limiting (CP2+)
3. **Per-API-Key Limits**: Support per-API-key rate limiting (CP2+)
4. **Per-Endpoint Limits**: Maintain per-endpoint limits (existing CP1 feature)
5. **Backward Compatibility**: CP1 in-memory mode must remain available

### Non-Functional Requirements

1. **Latency**: Rate limit check must add < 5ms overhead (p95)
2. **Accuracy**: Rate limit enforcement must be accurate within 5% of configured limit
3. **Availability**: Rate limiting must degrade gracefully if shared store unavailable
4. **Scalability**: Support 10+ Gateway instances with 1000+ req/s per instance
5. **Operational Simplicity**: Minimal operational overhead (no complex infrastructure)

## Design Options

### Option 1: Redis (Recommended for CP2)

**Algorithm**: Fixed-window or sliding-window using Redis INCR + EXPIRE

**Pros**:
- ✅ Low latency (< 1ms for Redis local, < 5ms for Redis remote)
- ✅ High throughput (100k+ ops/sec)
- ✅ Atomic operations (INCR, EXPIRE)
- ✅ Built-in TTL support
- ✅ Widely used, mature ecosystem
- ✅ Simple operations (GET, INCR, EXPIRE)

**Cons**:
- ❌ Additional infrastructure dependency
- ❌ Network latency for remote Redis
- ❌ Requires Redis HA setup for production

**Implementation**:
```c
// Pseudo-code
redis_key = "rate_limit:{endpoint}:{tenant_id}:{window_start}"
current = redis_incr(redis_key)
if (current == 1) redis_expire(redis_key, window_ttl)
if (current > limit) return RATE_LIMIT_EXCEEDED
```

**Latency**: ~1-5ms (local Redis) to ~5-20ms (remote Redis)

**Accuracy**: High (atomic operations, no race conditions)

### Option 2: NATS JetStream Key-Value Store

**Algorithm**: Fixed-window using JetStream KV operations

**Pros**:
- ✅ No additional infrastructure (NATS already required)
- ✅ Low latency (same network as NATS messaging)
- ✅ Atomic operations (Put, Get)
- ✅ Built-in replication (JetStream cluster)

**Cons**:
- ❌ Less mature than Redis for rate limiting
- ❌ Limited operations (no native INCR)
- ❌ Requires manual TTL management
- ❌ Higher latency than Redis for simple operations

**Implementation**:
```c
// Pseudo-code
kv_key = "rate_limit.{endpoint}.{tenant_id}.{window_start}"
current = kv_get(kv_key)
if (current == NULL) current = 0
new_value = current + 1
kv_put(kv_key, new_value, ttl)
if (new_value > limit) return RATE_LIMIT_EXCEEDED
```

**Latency**: ~5-15ms (same network as NATS)

**Accuracy**: Medium (requires read-modify-write, potential race conditions)

### Option 3: SQL Database (PostgreSQL/MySQL)

**Algorithm**: Fixed-window using SQL UPDATE with row-level locking

**Pros**:
- ✅ Strong consistency (ACID transactions)
- ✅ Existing infrastructure (if already using SQL)
- ✅ Rich query capabilities

**Cons**:
- ❌ High latency (10-50ms per query)
- ❌ Database load (high write rate)
- ❌ Connection pool management
- ❌ Not suitable for high-throughput scenarios

**Implementation**:
```sql
-- Pseudo-code
UPDATE rate_limits 
SET count = count + 1, updated_at = NOW()
WHERE endpoint = ? AND tenant_id = ? AND window_start = ?
RETURNING count;
```

**Latency**: ~10-50ms (database query)

**Accuracy**: High (ACID transactions)

### Option 4: Specialized Rate Limiting Service

**Algorithm**: External service (e.g., Kong, Envoy Rate Limit Service)

**Pros**:
- ✅ Purpose-built for rate limiting
- ✅ Advanced algorithms (token bucket, sliding window)
- ✅ Rich configuration options

**Cons**:
- ❌ Additional infrastructure
- ❌ Network latency
- ❌ Operational complexity
- ❌ Overkill for simple use cases

**Latency**: ~5-20ms (network RTT)

**Accuracy**: High (specialized service)

## Recommended Approach: Hybrid (In-Memory + Redis)

### Architecture

**Two-Tier Rate Limiting**:
1. **Local Cache (In-Memory)**: Fast path for recent requests
2. **Shared Store (Redis)**: Coordination across instances

**Algorithm**: Fixed-window with local cache + Redis sync

**Flow**:
```
1. Check local cache (in-memory)
   - If cache hit and under limit → allow
   - If cache hit and over limit → check Redis
   - If cache miss → check Redis

2. Check Redis (shared store)
   - INCR key: "rate_limit:{endpoint}:{tenant_id}:{window_start}"
   - If count > limit → reject
   - Update local cache

3. Periodic sync (background)
   - Sync local cache with Redis every N seconds
   - Handle cache invalidation on window reset
```

**Benefits**:
- ✅ Low latency (local cache hit: < 1ms)
- ✅ Consistent limits (Redis coordination)
- ✅ Graceful degradation (fallback to local-only if Redis unavailable)
- ✅ Reduced Redis load (local cache reduces Redis queries)

**Configuration**:
```c
typedef struct {
    int enabled;                    // Enable distributed rate limiting
    const char *backend;            // "redis" | "nats" | "sql" | "memory"
    const char *redis_host;         // Redis host (if backend=redis)
    int redis_port;                 // Redis port
    int local_cache_ttl_seconds;   // Local cache TTL
    int sync_interval_seconds;      // Background sync interval
    int fallback_to_local;          // Fallback to local-only if backend unavailable
} distributed_rl_config_t;
```

## Key Design Decisions

### 1. Rate Limit Key Model

**Format**: `rate_limit:{endpoint}:{tenant_id}:{api_key}:{window_start}`

**Components**:
- `endpoint`: Endpoint identifier (e.g., `routes_decide`, `messages`)
- `tenant_id`: Tenant identifier (optional, for per-tenant limits)
- `api_key`: API key identifier (optional, for per-key limits)
- `window_start`: Window start timestamp (seconds since epoch, rounded to window size)

**Examples**:
- Global endpoint limit: `rate_limit:routes_decide:::{window_start}`
- Per-tenant limit: `rate_limit:routes_decide:tenant_123::{window_start}`
- Per-API-key limit: `rate_limit:routes_decide:tenant_123:api_key_456:{window_start}`

### 2. Window Management

**Fixed-Window Algorithm** (CP2 initial):
- Window start: `floor(current_time / window_size) * window_size`
- Window end: `window_start + window_size`
- Key TTL: `window_size + 10 seconds` (grace period for cleanup)

**Future: Sliding-Window Algorithm** (CP2+):
- Use Redis sorted sets (ZSET) with timestamps
- Remove expired entries: `ZREMRANGEBYSCORE key -inf (current_time - window_size)`
- Count active entries: `ZCARD key`

### 3. Latency vs Accuracy Trade-off

**Soft Limit** (Recommended for CP2):
- Local cache allows slight over-limit (e.g., 5% tolerance)
- Redis sync happens asynchronously
- **Latency**: < 1ms (local cache)
- **Accuracy**: ±5% of configured limit

**Hard Limit** (Future CP2+):
- All requests check Redis synchronously
- No local cache tolerance
- **Latency**: 1-5ms (Redis query)
- **Accuracy**: ±1% of configured limit

### 4. Graceful Degradation

**Fallback Strategy**:
1. **Redis Available**: Use distributed rate limiting (Redis + local cache)
2. **Redis Unavailable**: Fallback to local-only (CP1 mode)
3. **Redis Recovered**: Resume distributed mode automatically

**Configuration**:
```c
// Fallback behavior
if (redis_unavailable && fallback_to_local) {
    // Use local-only rate limiting (CP1 mode)
    return rate_limit_check_local(endpoint, tenant_id, remaining_out);
}
```

## Implementation Plan

### Phase 1: Architecture & Design (Current)

- ✅ Create architecture document (this document)
- ✅ Evaluate backend options
- ✅ Design key model and algorithms
- ✅ Plan migration strategy

### Phase 2: Redis Backend Implementation (CP2)

**Components**:
1. **Redis Client**: Lightweight Redis client for C (hiredis or custom)
2. **Rate Limiter Executor**: New module `rate_limiter_distributed.c`
3. **Local Cache**: In-memory cache with TTL
4. **Background Sync**: Periodic sync thread/process

**Files**:
- `apps/c-gateway/src/rate_limiter_distributed.c` (new)
- `apps/c-gateway/src/rate_limiter_redis.c` (new)
- `apps/c-gateway/src/rate_limiter_cache.c` (new)
- `apps/c-gateway/src/http_server.c` (modify to use distributed limiter)

### Phase 3: Testing & Validation (CP2)

**Test Scenarios**:
1. **Single Instance**: Verify backward compatibility (CP1 mode)
2. **Multi-Instance**: Verify consistent limits across 2-3 instances
3. **Redis Failure**: Verify graceful degradation to local-only
4. **High Load**: Verify latency and accuracy under load

**Test Infrastructure**:
- Docker Compose with 3 Gateway instances + Redis
- Load testing with multiple clients
- Chaos testing (Redis restarts, network failures)

### Phase 4: Production Deployment (CP2+)

**Rollout Strategy**:
1. Feature toggle: `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=false` (default: CP1 mode)
2. Gradual rollout: Enable for specific endpoints/tenants
3. Monitoring: Track latency, accuracy, Redis load
4. Full rollout: Enable for all endpoints

## Migration Plan: CP1 → CP2+

See `docs/ARCHITECTURE/gateway-rate-limiting-migration-plan.md` for detailed migration strategy.

**High-Level Steps**:
1. **CP1 (Current)**: In-memory fixed-window (single instance)
2. **CP2 (Transition)**: Hybrid mode (local cache + Redis, feature toggle)
3. **CP2+ (Target)**: Full distributed mode (Redis primary, local cache secondary)

## Performance Characteristics

### Latency (p95)

| Mode | Latency | Notes |
|------|---------|-------|
| **CP1 (Local-only)** | < 0.1ms | In-memory lookup |
| **CP2 (Hybrid, cache hit)** | < 1ms | Local cache hit |
| **CP2 (Hybrid, cache miss)** | 1-5ms | Redis query |
| **CP2+ (Hard limit)** | 1-5ms | Always Redis query |

### Throughput

| Mode | Throughput | Notes |
|------|------------|-------|
| **CP1 (Local-only)** | 100k+ req/s | Limited by Gateway CPU |
| **CP2 (Hybrid)** | 50k+ req/s | Limited by Redis throughput |
| **CP2+ (Hard limit)** | 20k+ req/s | Limited by Redis latency |

### Accuracy

| Mode | Accuracy | Notes |
|------|----------|-------|
| **CP1 (Local-only)** | ±N×limit | N = number of instances |
| **CP2 (Hybrid, soft)** | ±5% | Local cache tolerance |
| **CP2+ (Hard limit)** | ±1% | Redis atomic operations |

## Operational Considerations

### Redis Setup

**Requirements**:
- Redis 6.0+ (for improved performance)
- Redis HA (sentinel or cluster) for production
- Memory: ~100MB per 1M rate limit keys
- Network: Low latency (< 5ms) to Gateway instances

**Monitoring**:
- Redis latency (p50, p95, p99)
- Redis memory usage
- Redis connection pool status
- Rate limit hit/miss ratios

### Configuration

**Environment Variables**:
```bash
# Enable distributed rate limiting
GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true

# Backend selection
GATEWAY_RATE_LIMIT_BACKEND=redis  # redis | nats | sql | memory

# Redis configuration
GATEWAY_RATE_LIMIT_REDIS_HOST=localhost
GATEWAY_RATE_LIMIT_REDIS_PORT=6379
GATEWAY_RATE_LIMIT_REDIS_PASSWORD=  # Optional

# Local cache configuration
GATEWAY_RATE_LIMIT_LOCAL_CACHE_TTL_SECONDS=10
GATEWAY_RATE_LIMIT_SYNC_INTERVAL_SECONDS=5

# Fallback behavior
GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
```

## Future Enhancements (Post-CP2)

1. **Sliding-Window Algorithm**: More accurate than fixed-window
2. **Token Bucket Algorithm**: Burst support with refill rate
3. **Per-Tenant Limits**: Different limits per tenant
4. **Per-API-Key Limits**: Different limits per API key
5. **Dynamic Limits**: Adjust limits based on load/health
6. **Rate Limit Analytics**: Historical data and trends

## References

- **Current Implementation**: `apps/c-gateway/src/http_server.c` (lines 400-600)
- **Rate Limiting Spec**: `docs/GATEWAY_RATE_LIMITING.md`
- **Gateway Routes**: `docs/GATEWAY_ROUTES_SPEC.md`
- **Migration Plan**: `docs/ARCHITECTURE/gateway-rate-limiting-migration-plan.md` (to be created)

