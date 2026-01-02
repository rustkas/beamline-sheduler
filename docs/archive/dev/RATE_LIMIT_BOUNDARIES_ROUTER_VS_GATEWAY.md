# Rate Limiting Boundaries: Router vs Gateway

## Purpose

This document defines and fixes the boundaries of responsibility for rate limiting between Router and Gateway components. It clarifies what each component does, when it applies, and how they integrate.

## Status

📅 **CP2 Design Document** (Boundaries Definition)

**Current State**: Design and specification only. Implementation deferred to CP2-LC.

## Executive Summary

**Gateway** handles **ingress rate limiting** (endpoint-level protection, DDoS mitigation, early rejection).

**Router** handles **policy-aware rate limiting** (per-policy quotas, tenant SLA tiers, burst protection, metrics).

**Execution Order**: Gateway check → Router check → Provider selection.

## Gateway Rate Limiting (Ingress Protection)

### Scope and Purpose

**Gateway rate limiting** protects the Gateway ingress layer from overload and provides basic DDoS protection.

**Key Characteristics**:
- ✅ **Endpoint-level**: Per-endpoint limits (`/api/v1/routes/decide`, `/api/v1/messages`, etc.)
- ✅ **Global Gateway limits**: System-wide protection (all endpoints combined)
- ✅ **Per-tenant quotas** (CP2+): Tenant-aware limits with class-based overrides
- ✅ **Early rejection**: Before Router call (saves Router resources)
- ✅ **Ingress protection**: Protects Gateway from overload

### Implementation Details

**Location**: `apps/c-gateway/src/http_server.c` (C Gateway) or `apps/gateway/src/common/guards/rate-limit.guard.ts` (NestJS Gateway)

**Algorithm**:
- **CP1**: Fixed-window (in-memory)
- **CP2+**: Sliding window (Redis-backed, distributed)

**Storage**:
- **CP1**: In-memory counters (`rl_counters[RL_ENDPOINT_MAX]`)
- **CP2+**: Redis-backed distributed counters

**Configuration**:
- Environment variables: `GATEWAY_RATE_LIMIT_*`
- Per-endpoint limits: `GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT`, `GATEWAY_RATE_LIMIT_MESSAGES`
- Global limit: `GATEWAY_RATE_LIMIT_GLOBAL`
- Per-tenant (CP2+): `GATEWAY_RATE_LIMIT_PER_TENANT_DEFAULT`, `GATEWAY_RATE_LIMIT_PER_TENANT_OVERRIDES`

**Enforcement Point**:
- **Before Router call**: Gateway checks rate limit first
- **If exceeded**: Returns 429 immediately, Router NOT called
- **If allowed**: Proceeds to Router call

**Response Format**:
```json
{
  "error": "rate_limit_exceeded",
  "message": "Too many requests",
  "tenant_id": "t-123",
  "endpoint": "/api/v1/messages",
  "retry_after_seconds": 60
}
```

**Headers**:
- `Retry-After: <seconds>`
- `X-RateLimit-Limit: <limit>`
- `X-RateLimit-Remaining: 0`
- `X-RateLimit-Reset: <epoch_seconds>`

**Metrics**:
- `gateway_rate_limit_hits_total{endpoint,tenant}`
- `gateway_rate_limit_exceeded_total{endpoint,tenant}`
- `gateway_rate_limit_window_active{endpoint}`

### Gateway Responsibilities Summary

| Responsibility | Description | CP Version |
|---------------|-------------|-----------|
| Endpoint-level limits | Per-endpoint rate limiting (`/api/v1/routes/decide`, `/api/v1/messages`) | CP1+ |
| Global Gateway limits | System-wide protection (all endpoints) | CP1+ |
| Per-tenant quotas | Tenant-aware limits with class-based overrides | CP2+ |
| Early rejection | Before Router call (saves Router resources) | CP1+ |
| Standard 429 headers | `X-RateLimit-*`, `Retry-After` headers | CP1+ |
| Ingress protection | Protect Gateway from overload, DDoS mitigation | CP1+ |
| Distributed rate limiting | Redis-backed shared counters (multi-instance) | CP2+ |

## Router Rate Limiting (Policy-Aware)

### Scope and Purpose

**Router rate limiting** enforces per-policy quotas, tenant SLA tiers, and provides burst protection based on routing policy configuration.

**Key Characteristics**:
- ✅ **Policy-level**: Per-policy quotas (configured in Policy JSON DSL)
- ✅ **Tenant-level**: Per-tenant SLA tiers (premium vs basic tenants)
- ✅ **Global**: System-wide limits (all tenants, all policies)
- ✅ **Policy-aware**: Rate limits are part of routing policy configuration
- ✅ **Burst protection**: Token bucket algorithm with burst capacity
- ✅ **Metrics**: Per-policy and per-tenant rate limit metrics
- ✅ **After Gateway check**: Router checks after Gateway allows request

### Implementation Details

**Location**: `apps/otp/router/src/router_rate_limiter.erl` (to be extended)

**Algorithm**:
- **Token Bucket**: With burst support (sustained rate + burst capacity)
- **Alternative**: Sliding window (future enhancement)

**Storage**:
- **CP2**: ETS (in-memory, per-instance)
- **CP2+**: Redis-backed distributed storage (future enhancement)

**Configuration**:
- **Policy JSON DSL**: `rate_limit` block in policy JSON
- **Tenant config**: Separate tenant configuration store (database, config file)
- **System config**: Global rate limits (system-wide)

**Policy JSON Format**:
```json
{
  "version": "1.0",
  "policy_id": "default",
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "burst": 50,
    "scope": "policy"
  },
  "providers": [...]
}
```

**Enforcement Point**:
- **After Gateway check**: Router checks after Gateway allows request
- **Before provider selection**: Rate limit check happens before `router_policy_applier`
- **If exceeded**: Returns 429 via Router error response (NATS/gRPC), provider selection NOT performed
- **If allowed**: Proceeds to provider selection

**Response Format** (Router 429):
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

**Metrics**:
- `router_rate_limit_checks_total{scope, identifier, decision}` (decision: `allowed` or `exceeded`)
- `router_rate_limit_current{scope, identifier}` (gauge: current request count)
- `router_rate_limit_limit{scope, identifier}` (gauge: configured limit)
- `router_rate_limit_burst{scope, identifier}` (gauge: configured burst capacity)

**Logging**:
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

### Router Responsibilities Summary

| Responsibility | Description | CP Version |
|---------------|-------------|-----------|
| Policy-level limits | Per-policy quotas (configured in Policy JSON DSL) | CP2+ |
| Tenant-level limits | Per-tenant SLA tiers (premium vs basic) | CP2+ |
| Global limits | System-wide limits (all tenants, all policies) | CP2+ |
| Burst protection | Token bucket algorithm with burst capacity | CP2+ |
| Policy-aware enforcement | Rate limits are part of routing policy | CP2+ |
| Rate limit metrics | Per-policy and per-tenant metrics | CP2+ |
| After Gateway check | Router checks after Gateway allows request | CP2+ |
| Mapping to decisions | Rate limit context in routing decisions | CP2+ |

## Execution Flow

### Request Processing Order

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

### Enforcement Priority

**Priority** (most restrictive first):
1. **Gateway Rate Limit** (endpoint-level) - checked first
2. **Global Rate Limit** (system-wide) - checked second (Router)
3. **Tenant Rate Limit** (per-tenant) - checked third (Router)
4. **Policy Rate Limit** (per-policy) - checked last (Router)

**Short-Circuit**: If any limit exceeded, return 429 immediately (don't check remaining limits)

## Key Differences

### Gateway vs Router Rate Limiting

| Aspect | Gateway | Router |
|--------|---------|--------|
| **Scope** | Endpoint-level (`/api/v1/routes/decide`, `/api/v1/messages`) | Policy-level, tenant-level, global |
| **Purpose** | Ingress protection, DDoS mitigation | Per-policy quotas, tenant SLA tiers |
| **Configuration** | Environment variables | Policy JSON DSL, tenant config |
| **Algorithm** | Fixed-window (CP1) or sliding window (CP2+) | Token bucket (with burst) |
| **Storage** | In-memory (CP1) or Redis (CP2+) | ETS (CP2) or Redis (CP2+) |
| **Enforcement** | Before Router call (early rejection) | After Gateway check, before provider selection |
| **Response** | Standard 429 with `X-RateLimit-*` headers | Router error response with policy context |
| **Metrics** | `gateway_rate_limit_*` | `router_rate_limit_*` |
| **Context** | Endpoint, tenant | Policy, tenant, scope |
| **Awareness** | Endpoint-aware | Policy-aware |

### When to Use Gateway vs Router Rate Limiting

**Use Gateway Rate Limiting** when:
- ✅ Protecting Gateway from overload (ingress protection)
- ✅ Basic DDoS protection (endpoint-level)
- ✅ Per-endpoint limits (different limits for different endpoints)
- ✅ Early rejection (before Router call, saves Router resources)
- ✅ Standard HTTP 429 responses with headers

**Use Router Rate Limiting** when:
- ✅ Per-policy quotas (different limits for different policies)
- ✅ Tenant SLA tiers (premium vs basic tenants)
- ✅ Burst protection (token bucket with burst capacity)
- ✅ Policy-aware enforcement (rate limits are part of routing policy)
- ✅ Per-policy and per-tenant metrics
- ✅ Rate limit context in routing decisions

## Integration Points

### Gateway → Router Communication

**Gateway** calls Router via NATS/gRPC after Gateway rate limit check passes.

**Router** receives request and performs Router rate limit check before provider selection.

**Error Handling**:
- Gateway 429: Router NOT called (early rejection)
- Router 429: Router returns error response (NATS/gRPC), Gateway forwards to client

### Coordination (Future CP2+)

**Global Rate Limits**:
- Gateway and Router may coordinate for global rate limits
- Gateway can query Router for global rate limit status
- Router can notify Gateway of global rate limit exhaustion

**Note**: Global rate limit coordination is optional (Router can work independently)

## Scenarios

### Scenario 1: Gateway Endpoint Limit Exceeded

**Request**: `POST /api/v1/routes/decide` (50 req/min limit)

**Flow**:
1. Gateway checks endpoint rate limit → **EXCEEDED** (51 requests in window)
2. Gateway returns 429 immediately
3. Router NOT called (saves Router resources)

**Response**:
```json
{
  "error": "rate_limit_exceeded",
  "message": "Too many requests",
  "endpoint": "/api/v1/routes/decide",
  "retry_after_seconds": 30
}
```

### Scenario 2: Router Policy Limit Exceeded

**Request**: `POST /api/v1/routes/decide` (Gateway allows, Router checks policy)

**Flow**:
1. Gateway checks endpoint rate limit → **ALLOWED** (10 requests in window)
2. Router called via NATS/gRPC
3. Router checks policy rate limit → **EXCEEDED** (101 requests, limit: 100 req/s)
4. Router returns 429 error response
5. Gateway forwards 429 to client

**Response**:
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

### Scenario 3: Both Limits Pass

**Request**: `POST /api/v1/routes/decide` (both Gateway and Router allow)

**Flow**:
1. Gateway checks endpoint rate limit → **ALLOWED** (10 requests in window)
2. Router called via NATS/gRPC
3. Router checks policy rate limit → **ALLOWED** (50 requests, limit: 100 req/s)
4. Router proceeds to provider selection
5. Normal routing logic executes

**Response**: Normal routing decision response

### Scenario 4: Multi-Level Limits (Global + Tenant + Policy)

**Request**: `POST /api/v1/routes/decide` (multiple limits configured)

**Configuration**:
- Global: 10000 req/min
- Tenant: 1000 req/min
- Policy: 100 req/min

**Flow**:
1. Gateway checks endpoint rate limit → **ALLOWED**
2. Router called via NATS/gRPC
3. Router checks global limit → **ALLOWED** (5000 used)
4. Router checks tenant limit → **ALLOWED** (500 used)
5. Router checks policy limit → **EXCEEDED** (101 used, limit: 100)
6. Router returns 429 immediately (short-circuit)

**Response**: Router 429 with policy context

## References

- `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` - Router rate limiting design
- `docs/GATEWAY_RATE_LIMITING.md` - Gateway rate limiting specification
- `docs/archive/dev/CP2_ROUTER_PLAN.md` - CP2 Router plan (pages 279-293, 450-465)
- `docs/GATEWAY_ROUTES_SPEC.md` - Gateway routes specification

## Change History

**v1.0 (2025-01-27)**:
- Initial boundaries definition
- Gateway vs Router responsibilities clarified
- Execution flow documented
- Scenarios added

