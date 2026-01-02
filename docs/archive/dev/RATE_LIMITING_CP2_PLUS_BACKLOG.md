# Rate Limiting CP2+ Backlog

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: 📋 **BACKLOG** (CP2+ / CP3)  
**Component**: Router + Gateway

## Purpose

This document defines the backlog for rate limiting enhancements beyond CP2, including global scope, multi-level checks, advanced policies, and integration with usage/billing systems.

## Current State (CP2)

**✅ Implemented**:
- Token bucket algorithm with burst support
- Per-policy and per-tenant rate limiting
- Enabled/disabled flag support
- Metrics emission (`router_rate_limit_allowed_total`, `router_rate_limit_exceeded_total`)
- Clock skew protection
- ETS-based storage (in-memory)
- Gateway Redis rate limiting (distributed)

**⚠️ Limitations**:
- Global scope not implemented (only policy and tenant scopes)
- Multi-level checks not implemented (only policy-level check)
- Configuration change handling requires manual reset
- No integration with usage/billing systems

## Backlog Items

### 1. Global Scope Rate Limiting 🔴 **REQUIRED for CP2+**

**Priority**: High  
**Target**: CP2+ (Post-CP2, Pre-CP3)

**Description**: Implement global rate limiting scope that applies across all tenants and all policies.

**Requirements**:
- Global rate limit configuration (system-wide)
- Global bucket management (ETS or Redis)
- Integration with multi-level checks (global → tenant → policy)
- Metrics: `router_rate_limit_allowed_total{scope="global"}`

**Implementation**:
- Add `global` scope to `router_rate_limit_store:build_key/2`
- Add global rate limit configuration to system config
- Add global rate limit check before tenant/policy checks
- Update `router_policy_applier.erl` to check global limit first

**Dependencies**:
- System configuration store (for global limits)
- Redis-backed storage (for distributed global limits)

**Reference**: `docs/archive/dev/RATE_LIMIT_IMPLEMENTATION_AUDIT.md` (Gap: Global scope)

### 2. Multi-Level Rate Limit Checks 🔴 **REQUIRED for CP2+**

**Priority**: High  
**Target**: CP2+ (Post-CP2, Pre-CP3)

**Description**: Implement sequential rate limit checks at multiple scopes (global → tenant → policy → provider).

**Requirements**:
- Check global limit first (if configured)
- Check tenant limit second (if configured)
- Check policy limit third (if configured)
- Check provider limit fourth (if configured, CP3+)
- Short-circuit on first failure (don't check remaining limits)
- Clear error messages indicating which level blocked

**Execution Order**:
```
1. Global Rate Limit Check
   ├─ If exceeded → 429, stop
   └─ If allowed → proceed

2. Tenant Rate Limit Check
   ├─ If exceeded → 429, stop
   └─ If allowed → proceed

3. Policy Rate Limit Check
   ├─ If exceeded → 429, stop
   └─ If allowed → proceed

4. Provider Rate Limit Check (CP3+)
   ├─ If exceeded → 429, stop
   └─ If allowed → proceed
```

**Implementation**:
- Update `router_policy_applier.erl` to check all levels sequentially
- Add `check_global_rate_limit/0`, `check_tenant_rate_limit/2`, `check_policy_rate_limit/3`
- Update error response to include blocked level
- Add metrics for each level

**Dependencies**:
- Global scope rate limiting (Item 1)
- Tenant rate limit configuration store

**Reference**: `docs/archive/dev/RATE_LIMIT_IMPLEMENTATION_AUDIT.md` (Gap: Multi-level checks)

### 3. Per-Provider Rate Limiting 🟡 **OPTIONAL (CP3+)**

**Priority**: Medium  
**Target**: CP3+ (Future waves)

**Description**: Implement rate limiting at provider level (within policy).

**Requirements**:
- Provider-level rate limit configuration in policy JSON
- Provider bucket management (ETS or Redis)
- Integration with multi-level checks (after policy check)
- Metrics: `router_rate_limit_allowed_total{scope="provider", provider_id="..."}`

**Use Cases**:
- Limit requests to specific providers (e.g., OpenAI: 100 req/s, Anthropic: 50 req/s)
- Prevent provider overload
- Enforce provider-specific quotas

**Implementation**:
- Add `provider` scope to `router_rate_limit_store:build_key/2`
- Add provider rate limit configuration to policy JSON
- Add provider rate limit check after policy check
- Update `router_policy_applier.erl` to check provider limit

**Dependencies**:
- Multi-level rate limit checks (Item 2)
- Provider selection logic (to identify provider)

**Reference**: `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` (Future: Per-Provider)

### 4. Advanced Rate Limiting Policies 🟡 **OPTIONAL (CP3+)**

**Priority**: Medium  
**Target**: CP3+ (Future waves)

**Description**: Support more sophisticated rate limiting policies beyond simple token bucket.

**Features**:

#### 4.1. Weighted Rate Limits

**Description**: Different weights for different request types or tenants.

**Example**:
```json
{
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "weights": {
      "chat": 1.0,
      "embedding": 0.5,
      "image": 2.0
    }
  }
}
```

**Use Cases**:
- Image generation consumes more resources (weight 2.0)
- Embedding requests are lightweight (weight 0.5)

#### 4.2. Burst Pools

**Description**: Separate burst pools for different request types.

**Example**:
```json
{
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "burst": 50,
    "burst_pools": {
      "premium": 100,
      "standard": 50
    }
  }
}
```

**Use Cases**:
- Premium tenants get larger burst capacity
- Standard tenants get standard burst

#### 4.3. Priority Traffic

**Description**: Prioritize certain requests over others.

**Example**:
```json
{
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "priority_traffic": {
      "high": 0.2,  // 20% of capacity reserved for high priority
      "normal": 0.8  // 80% for normal priority
    }
  }
}
```

**Use Cases**:
- Critical requests (e.g., admin operations) get priority
- Background jobs get lower priority

**Implementation**:
- Extend token bucket algorithm to support weights/pools/priority
- Add configuration parsing for advanced policies
- Update `router_rate_limit_store.erl` to handle advanced policies

**Dependencies**:
- Token bucket algorithm (already implemented)
- Policy DSL extensions

**Reference**: `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` (Future enhancements)

### 5. Redis-Backed Storage for Router 🟡 **OPTIONAL (CP2+ / CP3)**

**Priority**: Medium  
**Target**: CP2+ (Post-CP2) or CP3

**Description**: Replace ETS storage with Redis for distributed rate limiting across Router instances.

**Requirements**:
- Redis backend for `router_rate_limit_store.erl`
- Fallback to ETS if Redis unavailable
- Distributed token bucket (shared across Router instances)
- Configuration: `ROUTER_RATE_LIMIT_STORAGE=ets|redis`
- Configuration: `ROUTER_RATE_LIMIT_REDIS_URI=redis://...`

**Benefits**:
- Consistent rate limits across multiple Router instances
- Horizontal scalability
- Shared state for global/tenant limits

**Implementation**:
- Create `router_rate_limit_redis.erl` module
- Update `router_rate_limit_store.erl` to support Redis backend
- Add Redis connection management
- Add fallback logic (ETS if Redis unavailable)

**Dependencies**:
- Redis infrastructure
- Redis client library for Erlang

**Reference**: `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` (Future: Redis-backed storage)

### 6. Configuration Change Handling 🔴 **REQUIRED for CP2+**

**Priority**: High  
**Target**: CP2+ (Post-CP2, Pre-CP3)

**Description**: Automatically update or reset buckets when rate limit configuration changes.

**Requirements**:
- Detect configuration changes (RPS, burst changes)
- Update existing buckets with new configuration
- Reset buckets if configuration incompatible
- Handle configuration changes during active rate limiting

**Implementation**:
- Add configuration change detection in `router_rate_limit_store.erl`
- Add bucket update logic (adjust tokens based on new limits)
- Add bucket reset logic (if configuration incompatible)
- Add configuration version tracking

**Dependencies**:
- Configuration change notification mechanism
- Bucket update/reset logic

**Reference**: `../../../apps/otp/router/docs/dev/RATE_LIMIT_INVARIANTS.md` (Invariant 5: Configuration Change Policy)

### 7. Integration with Usage/Billing Systems 🟢 **OPTIONAL (CP3+)**

**Priority**: Low  
**Target**: CP3+ (Future waves)

**Description**: Integrate rate limiting with usage tracking and billing systems.

**Features**:
- Track usage per tenant/policy/provider
- Export usage data to billing systems
- Rate limits based on billing quotas (e.g., tier limits)
- Usage-based rate limit adjustments

**Use Cases**:
- Premium tier: 1000 req/s, Basic tier: 100 req/s
- Usage-based billing: rate limits adjust based on usage
- Quota enforcement: block requests when quota exhausted

**Implementation**:
- Integrate with usage tracking system
- Add billing quota checks before rate limit checks
- Export usage metrics to billing systems
- Add quota-based rate limit configuration

**Dependencies**:
- Usage tracking system
- Billing system integration
- Quota management system

**Reference**: Future integration with usage/billing systems

## Priority Classification

### 🔴 **REQUIRED for CP2+** (Must have before CP3)

1. **Global Scope Rate Limiting** (Item 1)
2. **Multi-Level Rate Limit Checks** (Item 2)
3. **Configuration Change Handling** (Item 6)

**Rationale**: These features are foundational for production use and are referenced in CP2 implementation gaps.

### 🟡 **OPTIONAL (CP2+ / CP3)** (Nice to have)

4. **Per-Provider Rate Limiting** (Item 3) - CP3+
5. **Advanced Rate Limiting Policies** (Item 4) - CP3+
6. **Redis-Backed Storage for Router** (Item 5) - CP2+ or CP3

**Rationale**: These features enhance rate limiting but are not critical for basic functionality.

### 🟢 **OPTIONAL (CP3+)** (Future waves)

7. **Integration with Usage/Billing Systems** (Item 7) - CP3+

**Rationale**: This feature requires integration with external systems and can be deferred to future waves.

## ADR References

### ADR-014: Metrics and Distributed Tracing

**Reference**: `docs/ADR/ADR-014-metrics-tracing.md`

**Relevance**: Rate limiting metrics are part of observability layer:
- `router_rate_limit_allowed_total`, `router_rate_limit_exceeded_total`
- `gateway_rate_limit_hits_total`, `gateway_rate_limit_exceeded_total`

**Future Enhancements**:
- Additional metrics for global scope, multi-level checks
- Metrics for advanced policies (weighted, burst pools, priority)

### ADR-025: Gateway Admin gRPC

**Reference**: `docs/ADR/ADR-025-gateway-admin-grpc.md`

**Relevance**: Admin API could expose rate limit status and configuration:
- `GET /admin/rate-limit/status` - Current rate limit status
- `POST /admin/rate-limit/config` - Update rate limit configuration

**Future Enhancements**:
- Admin API for global/tenant rate limit configuration
- Admin API for rate limit status queries

## Roadmap Integration

### CP2+ (Post-CP2, Pre-CP3)

**Required Features**:
- ✅ Global scope rate limiting
- ✅ Multi-level rate limit checks
- ✅ Configuration change handling

**Optional Features**:
- Redis-backed storage for Router (if horizontal scaling needed)

### CP3 (Future waves)

**Optional Features**:
- Per-provider rate limiting
- Advanced rate limiting policies (weighted, burst pools, priority)
- Integration with usage/billing systems

**Reference**: `docs/archive/dev/CP2_RATE_LIMITING_SUMMARY.md` (Known Limitations section)

## Implementation Notes

### Global Scope Implementation

**Key Changes**:
1. Add `global` scope to `router_rate_limit_store:build_key/2`
2. Add global rate limit configuration to system config
3. Add `check_global_rate_limit/0` to `router_policy_applier.erl`
4. Update error response to include global scope context

**Configuration**:
```erlang
%% System config
#{rate_limit => #{
    global => #{
        enabled => true,
        requests_per_second => 10000,
        burst => 5000
    }
}}
```

### Multi-Level Checks Implementation

**Key Changes**:
1. Update `router_policy_applier.erl` to check all levels sequentially
2. Add short-circuit logic (stop on first failure)
3. Update error response to indicate blocked level
4. Add metrics for each level

**Code Structure**:
```erlang
check_all_rate_limits(TenantId, PolicyId, Policy) ->
    %% 1. Check global limit
    case check_global_rate_limit() of
        {ok, allow} -> ok;
        {error, _} = Error -> Error
    end,
    
    %% 2. Check tenant limit
    case check_tenant_rate_limit(TenantId) of
        {ok, allow} -> ok;
        {error, _} = Error -> Error
    end,
    
    %% 3. Check policy limit
    check_policy_rate_limit(Policy, TenantId, PolicyId).
```

## Roadmap Integration

### CP2+ / CP3 Roadmap

**Status**: No dedicated ROADMAP document found in codebase

**Integration**: This backlog document serves as the roadmap for rate limiting enhancements post-CP2.

**Future Integration**: When a CP2+ / CP3 roadmap document is created, this backlog should be referenced in the rate limiting section.

**Sections to Reference**:
- CP2+ (Post-CP2, Pre-CP3): Required features (Items 1, 2, 6)
- CP3 (Future waves): Optional features (Items 3, 4, 5, 7)

## References

- `docs/archive/dev/RATE_LIMIT_IMPLEMENTATION_AUDIT.md` - Implementation gaps
- `../../../apps/otp/router/docs/dev/RATE_LIMIT_INVARIANTS.md` - Invariants
- `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` - Design document
- `docs/archive/dev/RATE_LIMIT_BOUNDARIES_ROUTER_VS_GATEWAY.md` - Boundaries
- `docs/ADR/ADR-014-metrics-tracing.md` - Metrics ADR (rate limiting metrics mentioned)
- `docs/ADR/ADR-025-gateway-admin-grpc.md` - Admin API ADR (potential rate limit status API)
- `docs/archive/dev/CP2_RATE_LIMITING_SUMMARY.md` - CP2 summary (known limitations)

