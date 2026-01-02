# Rate Limit in Policy DSL Design

## Purpose

This document defines the design and specification for Rate Limiting functionality in Router Policy DSL. Rate Limiting controls request throughput at policy and tenant levels to prevent resource exhaustion and ensure fair usage.

## Status

📅 **CP2 Enhancement** (Design Document)

**Current State**: Design and specification only. Implementation deferred to CP2-LC.

## Overview

### Problem

Without rate limiting at policy/tenant level:
- Single tenant can exhaust Router resources (high request volume)
- No per-policy quotas (all policies share same resources)
- No burst protection (sudden traffic spikes can overwhelm system)
- Difficult to enforce SLA tiers (premium vs basic tenants)

### Solution

Multi-level rate limiting:
1. **Gateway-level** (CP1): Endpoint-based rate limiting (already implemented)
2. **Router-level** (CP2): Policy-based and tenant-based rate limiting
3. **Scope-based**: Global, per-tenant, per-policy, per-provider

### Integration with Gateway Rate Limiting

**Gateway (CP1)**:
- **Scope**: Per-endpoint rate limiting (`/api/v1/routes/decide`, `/api/v1/messages`, etc.)
- **Storage**: In-memory (fixed-window algorithm)
- **Purpose**: Protect Gateway from overload, basic DDoS protection
- **Enforcement**: Before Router call (early rejection)

**Router (CP2)**:
- **Scope**: Per-policy and per-tenant rate limiting
- **Storage**: ETS (in-memory) or Redis (distributed, CP2+)
- **Purpose**: Enforce per-policy quotas, tenant SLA tiers, burst protection
- **Enforcement**: After Gateway rate limit passes, before provider selection

**Execution Order**:
```
1. Gateway Rate Limit Check (endpoint-level)
   ├─ If exceeded → 429, Router NOT called
   └─ If allowed → proceed to step 2

2. Router Rate Limit Check (policy/tenant-level)
   ├─ If exceeded → 429, provider selection NOT performed
   └─ If allowed → proceed to step 3

3. Provider Selection (router_policy_applier)
   └─ Normal routing logic
```

## Architecture

### Scope: Multi-Level Rate Limiting

**Decision**: Rate limiting operates at multiple scopes simultaneously

**Scopes**:
1. **Global**: System-wide limit (all tenants, all policies)
2. **Per-Tenant**: Limit per tenant (across all policies)
3. **Per-Policy**: Limit per policy (within tenant)
4. **Per-Provider** (Future CP2+): Limit per provider (within policy)

**Priority**: Most restrictive limit applies (short-circuit evaluation)

### Rate Limit Storage

**ETS Table**: `router_rate_limit` (in-memory, CP2)

**Key Structure**: `{Scope, Identifier}`

**Scopes and Keys**:
- **Global**: `{global, <<"global">>}`
- **Per-Tenant**: `{tenant, TenantId}`
- **Per-Policy**: `{policy, {TenantId, PolicyId}}`
- **Per-Provider** (Future): `{provider, {TenantId, PolicyId, ProviderId}}`

**Value Structure**:
```erlang
#{
    limit => integer(),           % Requests per window
    window_seconds => integer(),  % Time window in seconds
    burst => integer(),           % Burst capacity
    current_count => integer(),   % Current request count
    burst_count => integer(),     % Current burst count
    window_start => integer(),    % Unix timestamp (seconds)
    last_request => integer()     % Unix timestamp (seconds)
}
```

**Future Enhancement (CP2+)**: Redis-backed storage for distributed rate limiting

## JSON-DSL Structure

### Per-Policy Configuration

**Location**: Top-level `rate_limit` object in policy

**Format**:
```json
{
  "version": "1.0",
  "providers": [
    { "name": "provider_a", "weight": 70 },
    { "name": "provider_b", "weight": 30 }
  ],
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "burst": 50,
    "scope": "policy"
  }
}
```

### Per-Tenant Configuration (Future Enhancement)

**Location**: Tenant configuration (outside policy JSON)

**Format** (conceptual, not in policy JSON):
```json
{
  "tenant_id": "tenant_123",
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 1000,
    "burst": 200,
    "scope": "tenant"
  }
}
```

**Storage**: Separate tenant configuration store (database, config file, etc.)

## Configuration Fields

### Policy-Level Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `enabled` | boolean | `false` | Enable rate limiting for this policy |
| `requests_per_second` | integer | `100` | Maximum requests per second (sustained rate) |
| `burst` | integer | `50` | Burst capacity (additional requests allowed above sustained rate) |
| `scope` | string | `"policy"` | Rate limit scope: `"policy"` (per-policy) or `"tenant"` (per-tenant, if tenant config exists) |

### Alternative: Per-Minute Configuration

**Option 1**: `requests_per_second` (current schema)
- **Pros**: Fine-grained control, standard unit
- **Cons**: Higher counter update frequency

**Option 2**: `requests_per_minute` (alternative)
- **Pros**: Lower counter update frequency, easier to reason about
- **Cons**: Less fine-grained control

**Decision**: Support both via `window_seconds` and `limit`:
```json
{
  "rate_limit": {
    "enabled": true,
    "limit": 6000,              // Total requests per window
    "window_seconds": 60,       // 60 seconds = per-minute
    "burst": 500
  }
}
```

**Default**: `requests_per_second` for backward compatibility, but `limit` + `window_seconds` is more flexible

### Scope Values

| Scope | Description | Key Format |
|-------|-------------|------------|
| `"policy"` | Per-policy rate limit (within tenant) | `{policy, {TenantId, PolicyId}}` |
| `"tenant"` | Per-tenant rate limit (across all policies) | `{tenant, TenantId}` |
| `"global"` | Global rate limit (all tenants, all policies) | `{global, <<"global">>}` |

**Note**: `"tenant"` and `"global"` scopes require configuration outside policy JSON (tenant config, system config)

## Rate Limiting Algorithms

### Algorithm 1: Token Bucket (Recommended)

**Description**: Token bucket algorithm with burst support

**Parameters**:
- `limit`: Sustained rate (tokens per second)
- `burst`: Burst capacity (maximum tokens)
- `window_seconds`: Time window (1 second for per-second, 60 for per-minute)

**Behavior**:
- Tokens refill at `limit` rate (e.g., 100 tokens/second)
- Maximum tokens: `limit + burst` (e.g., 100 + 50 = 150)
- Request consumes 1 token
- If tokens available → allow request, decrement token
- If no tokens → reject request (429)

**Example**:
```json
{
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "burst": 50
  }
}
```

**Behavior**:
- Sustained rate: 100 req/s
- Burst capacity: 50 additional requests
- Maximum: 150 requests in first second (if bucket full)
- After burst: 100 req/s sustained

### Algorithm 2: Sliding Window (Alternative)

**Description**: Sliding window algorithm (more precise, higher overhead)

**Parameters**:
- `limit`: Maximum requests per window
- `window_seconds`: Time window size
- `burst`: Not applicable (sliding window doesn't support burst)

**Behavior**:
- Track requests in time window (e.g., last 60 seconds)
- Count requests in window
- If count < limit → allow request
- If count >= limit → reject request (429)

**Trade-off**: More precise but higher memory/CPU overhead

**Decision**: Use **Token Bucket** for CP2 (simpler, supports burst, lower overhead)

## Integration with Gateway Rate Limiting

### Responsibility Separation

**Gateway Responsibilities**:
- ✅ Endpoint-level rate limiting (`/api/v1/routes/decide`, `/api/v1/messages`)
- ✅ Global Gateway rate limit (protect Gateway from overload)
- ✅ Early rejection (before Router call)
- ✅ Standard 429 response with headers (`X-RateLimit-*`, `Retry-After`)

**Router Responsibilities**:
- ✅ Policy-level rate limiting (per-policy quotas)
- ✅ Tenant-level rate limiting (per-tenant quotas, SLA tiers)
- ✅ Burst protection (token bucket algorithm)
- ✅ Rate limit enforcement after Gateway check
- ✅ Rate limit metrics (per-policy, per-tenant)

### Execution Flow

```
Request → Gateway
  ├─ Gateway Rate Limit Check (endpoint-level)
  │   ├─ If exceeded → 429, stop (Router NOT called)
  │   └─ If allowed → proceed
  │
  └─ Router Call (NATS/gRPC)
      ├─ Router Rate Limit Check (policy/tenant-level)
      │   ├─ If exceeded → 429, stop (provider selection NOT performed)
      │   └─ If allowed → proceed
      │
      └─ Provider Selection (router_policy_applier)
          └─ Normal routing logic
```

### Error Response Format

**Gateway 429** (endpoint-level):
```json
{
  "ok": false,
  "error": {
    "code": "rate_limit_exceeded",
    "message": "Rate limit exceeded for endpoint /api/v1/routes/decide",
    "details": {
      "endpoint": "/api/v1/routes/decide",
      "limit": 50,
      "retry_after_seconds": 45
    }
  }
}
```

**Router 429** (policy/tenant-level):
```json
{
  "ok": false,
  "error": {
    "code": "rate_limit_exceeded",
    "message": "Rate limit exceeded for policy default",
    "details": {
      "scope": "policy",
      "policy_id": "default",
      "tenant_id": "tenant_123",
      "limit": 100,
      "window_seconds": 60,
      "retry_after_seconds": 30
    }
  }
}
```

**Key Difference**: Router 429 includes `scope`, `policy_id`, `tenant_id` (policy/tenant context)

## Scenarios

### Scenario 1: Global Rate Limit

**Use Case**: System-wide protection (all tenants, all policies)

**Configuration**:
```json
{
  "rate_limit": {
    "enabled": true,
    "limit": 10000,
    "window_seconds": 60,
    "scope": "global"
  }
}
```

**Storage Key**: `{global, <<"global">>}`

**Behavior**:
- All requests count toward global limit
- If global limit exceeded → 429 for all requests
- Protects system from total overload

**Enforcement**: Router-level (after Gateway check)

### Scenario 2: Per-Tenant Rate Limit

**Use Case**: Tenant SLA tiers (premium: 1000 req/s, basic: 100 req/s)

**Configuration** (tenant config, not policy JSON):
```json
{
  "tenant_id": "tenant_premium",
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 1000,
    "burst": 200,
    "scope": "tenant"
  }
}
```

**Storage Key**: `{tenant, <<"tenant_premium">>}`

**Behavior**:
- All policies for this tenant share the same limit
- If tenant limit exceeded → 429 for all policies in tenant
- Enforces tenant SLA tier

**Enforcement**: Router-level (after Gateway check, before policy check)

### Scenario 3: Per-Policy Rate Limit

**Use Case**: Per-policy quotas (different limits for different policies)

**Configuration** (in policy JSON):
```json
{
  "version": "1.0",
  "policy_id": "high_volume_policy",
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 500,
    "burst": 100,
    "scope": "policy"
  },
  "providers": [...]
}
```

**Storage Key**: `{policy, {<<"tenant_123">>, <<"high_volume_policy">>}}`

**Behavior**:
- Each policy has its own rate limit
- If policy limit exceeded → 429 for this policy only
- Other policies in same tenant unaffected

**Enforcement**: Router-level (after Gateway and tenant checks)

### Scenario 4: Combined Limits (Global + Tenant + Policy)

**Use Case**: Multi-level protection

**Configuration**:
- Global: 10000 req/min
- Tenant: 1000 req/min
- Policy: 100 req/min

**Behavior**:
- **Most restrictive limit applies** (short-circuit evaluation)
- Evaluation order: Global → Tenant → Policy
- If any limit exceeded → 429 immediately

**Example**:
```
Request arrives:
1. Check global limit (10000 req/min) → OK (5000 used)
2. Check tenant limit (1000 req/min) → OK (500 used)
3. Check policy limit (100 req/min) → EXCEEDED (101 used)
   → 429, stop (policy limit exceeded)
```

### Scenario 5: Burst Protection

**Use Case**: Allow traffic spikes while maintaining sustained rate

**Configuration**:
```json
{
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "burst": 50
  }
}
```

**Behavior**:
- Sustained rate: 100 req/s
- Burst capacity: 50 additional requests
- Maximum: 150 requests in first second (if bucket full)
- After burst: 100 req/s sustained

**Token Bucket Algorithm**:
- Tokens refill at 100 tokens/second
- Maximum tokens: 150 (100 + 50 burst)
- Request consumes 1 token
- If tokens available → allow
- If no tokens → 429

## Rate Limit Enforcement

### Enforcement Points

**Gateway** (CP1, already implemented):
- **Location**: `apps/c-gateway/src/http_server.c:rate_limit_check()`
- **Scope**: Per-endpoint
- **Algorithm**: Fixed-window
- **Storage**: In-memory counters
- **Response**: 429 with `X-RateLimit-*` headers

**Router** (CP2, to be implemented):
- **Location**: `apps/otp/router/src/router_rate_limiter.erl` (extend existing module)
- **Scope**: Per-policy, per-tenant, global
- **Algorithm**: Token bucket (with burst support)
- **Storage**: ETS (in-memory) or Redis (distributed, CP2+)
- **Response**: 429 via Router error response (NATS/gRPC)

### Enforcement Order

**Priority** (most restrictive first):
1. **Gateway Rate Limit** (endpoint-level) - checked first
2. **Global Rate Limit** (system-wide) - checked second
3. **Tenant Rate Limit** (per-tenant) - checked third
4. **Policy Rate Limit** (per-policy) - checked last

**Short-Circuit**: If any limit exceeded, return 429 immediately (don't check remaining limits)

### Rate Limit Check Implementation

**Module**: `router_rate_limiter.erl` (extend existing)

**Function**: `check_rate_limit/3`
```erlang
-spec check_rate_limit(Scope :: atom(), Identifier :: binary() | tuple(), Config :: map()) ->
    {ok, allow} | {error, {rate_limit_exceeded, Details :: map()}}.
```

**Parameters**:
- `Scope`: `global | tenant | policy`
- `Identifier`: `<<"global">> | TenantId | {TenantId, PolicyId}`
- `Config`: Rate limit configuration from policy or tenant config

**Returns**:
- `{ok, allow}`: Rate limit not exceeded, request allowed
- `{error, {rate_limit_exceeded, Details}}`: Rate limit exceeded, request rejected

**Details Map**:
```erlang
#{
    scope => atom(),
    identifier => binary() | tuple(),
    limit => integer(),
    current => integer(),
    window_seconds => integer(),
    retry_after_seconds => integer()
}
```

## Metrics and Observability

### Prometheus Metrics

**Rate Limit Checks**:
```
router_rate_limit_checks_total{scope, identifier, decision}
```
- `decision`: `allowed` or `exceeded`

**Rate Limit Current**:
```
router_rate_limit_current{scope, identifier}
```
- Gauge: Current request count in window

**Rate Limit Limit**:
```
router_rate_limit_limit{scope, identifier}
```
- Gauge: Configured limit

**Rate Limit Burst**:
```
router_rate_limit_burst{scope, identifier}
```
- Gauge: Configured burst capacity

### Logging

**Structured JSON logs** (per `OBSERVABILITY_CONVENTIONS.md`):

**Rate Limit Exceeded**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "WARN",
  "component": "router",
  "message": "Rate limit exceeded",
  "context": {
    "tenant_id": "tenant_123",
    "policy_id": "default",
    "scope": "policy",
    "limit": 100,
    "current": 101,
    "window_seconds": 60,
    "retry_after_seconds": 30
  }
}
```

## Examples

### Example 1: Basic Per-Policy Rate Limit

```json
{
  "version": "1.0",
  "policy_id": "default",
  "providers": [
    { "name": "provider_a", "weight": 70 },
    { "name": "provider_b", "weight": 30 }
  ],
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "burst": 50,
    "scope": "policy"
  }
}
```

**Behavior**:
- Policy `default` limited to 100 req/s sustained, 50 burst
- If limit exceeded → 429 for this policy
- Other policies unaffected

### Example 2: Per-Minute Rate Limit

```json
{
  "version": "1.0",
  "rate_limit": {
    "enabled": true,
    "limit": 6000,
    "window_seconds": 60,
    "burst": 1000,
    "scope": "policy"
  }
}
```

**Behavior**:
- Policy limited to 6000 req/min (100 req/s average)
- Burst: 1000 additional requests
- Maximum: 7000 requests in first minute (if bucket full)

### Example 3: Tenant SLA Tier (Conceptual)

**Tenant Config** (not in policy JSON):
```json
{
  "tenant_id": "tenant_premium",
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 1000,
    "burst": 200,
    "scope": "tenant"
  }
}
```

**Policy Config** (in policy JSON):
```json
{
  "version": "1.0",
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "burst": 50,
    "scope": "policy"
  }
}
```

**Behavior**:
- Tenant limit: 1000 req/s (across all policies)
- Policy limit: 100 req/s (for this policy)
- **Most restrictive applies**: 100 req/s (policy limit)
- If policy limit exceeded → 429
- If tenant limit exceeded → 429 (even if policy limit OK)

## Future Enhancements (CP2+)

### Per-Provider Rate Limiting

Allow rate limiting per provider within policy:
```json
{
  "providers": [
    {
      "name": "provider_a",
      "rate_limit": {
        "requests_per_second": 50,
        "burst": 25
      }
    }
  ]
}
```

### Redis-Backed Distributed Rate Limiting

- Shared rate limit state across Router instances
- Horizontal scalability
- Consistent limits in multi-instance deployments

### Dynamic Rate Limit Adjustment

- Runtime configuration changes (without policy reload)
- A/B testing different limit values
- Auto-scaling based on traffic patterns

### Rate Limit Metrics Dashboard

- Visualize rate limit usage per policy/tenant
- Track rate limit hits over time
- Monitor burst consumption patterns

## Risks and Non-Goals

### Risks

#### 1. Impact on CP1 Invariants

**Risk**: Rate limit implementation must not break CP1 policy engine invariants.

**CP1 Invariants to Preserve**:
- ✅ **Provider Selection Logic**: Sticky sessions, weighted routing, fallbacks must continue to work
- ✅ **Explanation Format**: Decision explanations must remain compatible with CP1 format
- ✅ **Extension Pipeline**: Pre/validators/post extensions must execute regardless of rate limit status
- ✅ **Backward Compatibility**: Legacy policy formats must continue to work (rate limit is optional)

**Mitigation**:
- Rate limit is **additive** - it only adds checks, doesn't modify existing logic
- Rate limit check happens **before** provider selection, but doesn't change selection algorithm
- Rate limit errors are handled like other errors (return error, don't break routing)

#### 2. Infrastructure Dependencies

**Risk**: Rate limit requires additional infrastructure components.

**Affected Components**:
- **NATS**: No direct impact (rate limit is Router-internal)
- **Extensions**: No impact (extensions execute independently)
- **Metrics**: Requires metrics infrastructure (Prometheus) for rate limit metrics
- **Storage**: Requires ETS or external store (Redis) for rate limit counters
- **Gateway**: Requires coordination protocol for global rate limits

**Mitigation**:
- Use ETS for rate limit counters (no external dependencies for CP2)
- Redis can be added later for distributed rate limiting (CP3+)
- Gateway coordination is optional (Router can work independently)
- Metrics are optional (rate limit works without metrics)

#### 3. Performance Impact

**Risk**: Rate limit checks add latency to provider selection.

**Impact Areas**:
- **Rate Limit Check**: ETS lookup and token bucket update
- **Counter Updates**: ETS writes after each request
- **Gateway Coordination**: Network calls for global rate limits (if implemented)

**Mitigation**:
- ETS operations are O(1) and very fast (< 1ms)
- Token bucket updates are atomic (single ETS operation)
- Gateway coordination is async (doesn't block request path)
- Rate limit check is early exit (fails fast if limit exceeded)

#### 4. Distributed Rate Limiting

**Risk**: Rate limit counters may be inconsistent across Router instances.

**Affected Scenarios**:
- **Multi-Instance Router**: Each instance has its own rate limit counters
- **Global Rate Limits**: Shared limits across instances require coordination

**Mitigation**:
- Per-instance rate limits are sufficient for CP2 (eventual consistency)
- Global rate limits require Gateway coordination (deferred to CP3+)
- Token bucket algorithm is tolerant of slight inconsistencies

#### 5. Gateway Coordination Complexity

**Risk**: Gateway↔Router rate limit coordination adds complexity.

**Affected Areas**:
- **Protocol Design**: Coordination protocol between Gateway and Router
- **State Synchronization**: Shared rate limit state
- **Error Handling**: Coordination failures

**Mitigation**:
- Gateway coordination is optional (Router can work independently)
- Simple protocol (HTTP/gRPC calls for rate limit checks)
- Fallback to Router-only rate limiting if coordination fails

### Non-Goals

#### 1. Distributed Rate Limit Storage (CP2)

**Non-Goal**: Shared rate limit counters across Router instances (Redis, etc.).

**Rationale**:
- Per-instance rate limits are sufficient for CP2
- Distributed storage adds complexity (consensus, synchronization, network calls)
- ETS is fast and sufficient for single-instance rate limiting

**Future**: Distributed storage (Redis) may be added in CP3+ for global rate limits.

#### 2. Rate Limit for Extensions

**Non-Goal**: Rate limiting for extension execution.

**Rationale**:
- Extensions are already handled by extension pipeline
- Extension rate limiting is separate concern (not provider selection)
- Rate limit is for provider requests, not extension calls

**Future**: Extension rate limiting may be added separately if needed.

#### 3. Dynamic Rate Limit Configuration

**Non-Goal**: Runtime changes to rate limit configuration without policy update.

**Rationale**:
- Rate limit configuration is part of policy (versioned, audited)
- Dynamic configuration adds complexity (hot reload, validation)
- Policy updates are sufficient for configuration changes

**Future**: May be added in CP3+ for operational flexibility.

#### 4. Advanced Rate Limit Algorithms

**Non-Goal**: Complex rate limit algorithms beyond Token Bucket and Sliding Window.

**Rationale**:
- Token Bucket is sufficient for CP2 (simple, efficient, well-understood)
- Sliding Window is alternative (if needed)
- Complex algorithms add unnecessary complexity

**Future**: Additional algorithms may be added if needed.

#### 5. Rate Limit Metrics Dashboard

**Non-Goal**: Built-in dashboard for rate limit metrics.

**Rationale**:
- Metrics are exported to Prometheus (external dashboard can be used)
- Dashboard is operational tooling, not core functionality
- CP2 focuses on core rate limiting functionality

**Future**: Dashboard may be added in CP3+ or as separate tooling.

## References

- `docs/ROUTING_POLICY.md` - Main routing policy specification
- `docs/GATEWAY_RATE_LIMITING.md` - Gateway rate limiting specification
- `docs/archive/dev/RATE_LIMIT_BOUNDARIES_ROUTER_VS_GATEWAY.md` - **Router vs Gateway boundaries** (defines responsibilities)
- `apps/otp/router/src/router_rate_limiter.erl` - Existing rate limiter module (reference)
- `docs/archive/dev/POLICY_DSL_GAPS_CP_CLASSIFICATION.md` - CP classification
- `docs/OBSERVABILITY_CONVENTIONS.md` - Logging conventions

## Change History

**v1.0 (2025-01-27)**:
- Initial design document
- Multi-level rate limiting architecture (global, tenant, policy)
- Integration with Gateway rate limiting
- Token bucket algorithm specification
- Scenarios and examples

