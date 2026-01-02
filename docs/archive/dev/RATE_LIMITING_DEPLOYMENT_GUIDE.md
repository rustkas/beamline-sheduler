# Rate Limiting Deployment Guide

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: CP2-LC  
**Components**: Router + Gateway

## Purpose

This document provides guidance on when to enable Gateway rate limiting, Router rate limiting, or both, and how to configure them for optimal protection without conflicts.

## Deployment Scenarios

### Scenario 1: Gateway Rate Limiting Only (CP1)

**Use Case**: Basic ingress protection, DDoS mitigation, endpoint-level limits

**Configuration**:
```bash
# Gateway
GATEWAY_RATE_LIMIT_MODE=local  # or redis for CP2+
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=100
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_TTL_SECONDS=60

# Router
# No rate limiting configured (rate_limit.enabled=false in policies)
```

**When to Use**:
- ✅ Protecting Gateway from overload
- ✅ Basic DDoS protection
- ✅ Per-endpoint limits (different limits for different endpoints)
- ✅ Early rejection (before Router call, saves Router resources)
- ❌ No per-policy quotas needed
- ❌ No tenant SLA tiers needed

**Behavior**:
- Gateway checks rate limit before Router call
- If exceeded → 429 from Gateway, Router NOT called
- If allowed → Router processes request normally

### Scenario 2: Router Rate Limiting Only (CP2)

**Use Case**: Per-policy quotas, tenant SLA tiers, policy-aware rate limiting

**Configuration**:
```bash
# Gateway
# No rate limiting (or very high limits to allow all requests)

# Router
# Rate limiting configured in Policy JSON:
{
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "burst": 50
  }
}
```

**When to Use**:
- ✅ Per-policy quotas (different limits for different policies)
- ✅ Tenant SLA tiers (premium vs basic tenants)
- ✅ Burst protection (token bucket with burst capacity)
- ✅ Policy-aware enforcement (rate limits are part of routing policy)
- ❌ No Gateway-level protection needed
- ❌ No endpoint-level limits needed

**Behavior**:
- Gateway allows all requests (or has very high limits)
- Router checks rate limit before provider selection
- If exceeded → 429 from Router, Gateway forwards to client
- If allowed → Router proceeds to provider selection

### Scenario 3: Both Gateway and Router Rate Limiting (Recommended for CP2+)

**Use Case**: Multi-layer protection, defense in depth, different scopes

**Configuration**:
```bash
# Gateway
GATEWAY_RATE_LIMIT_MODE=redis  # CP2+ distributed rate limiting
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=1000  # High limit (endpoint-level)
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_TTL_SECONDS=60

# Router
# Rate limiting configured in Policy JSON:
{
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,  # Lower limit (policy-level)
    "burst": 50
  }
}
```

**When to Use**:
- ✅ Multi-layer protection (defense in depth)
- ✅ Different scopes (Gateway: endpoint-level, Router: policy-level)
- ✅ Gateway protects from DDoS, Router enforces quotas
- ✅ Production environments with high traffic
- ✅ Need both ingress protection and policy-aware limits

**Behavior**:
1. Gateway checks rate limit (endpoint-level, high limit)
   - If exceeded → 429 from Gateway, Router NOT called
   - If allowed → proceed to Router
2. Router checks rate limit (policy-level, lower limit)
   - If exceeded → 429 from Router, Gateway forwards to client
   - If allowed → proceed to provider selection

**Key Points**:
- ✅ **No conflicts**: Each layer checks independently
- ✅ **No double-dropping**: Gateway checks first, Router checks second
- ✅ **Transparent**: Clear which layer blocked (error format differs)
- ✅ **Predictable**: Execution order is fixed (Gateway → Router)

## Recommended Configuration

### Development Environment

**Configuration**: Gateway rate limiting only (local mode)

```bash
# Gateway
GATEWAY_RATE_LIMIT_MODE=local
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=1000
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_TTL_SECONDS=60

# Router
# Rate limiting disabled in policies (rate_limit.enabled=false)
```

**Rationale**: Simple setup, Gateway protection is sufficient for development.

### Staging Environment

**Configuration**: Both Gateway and Router rate limiting

```bash
# Gateway
GATEWAY_RATE_LIMIT_MODE=redis
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=500
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_TTL_SECONDS=60
GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-staging:6379

# Router
# Rate limiting enabled in policies (rate_limit.enabled=true)
# Policy limits: 100 req/s, burst 50
```

**Rationale**: Test both layers together, verify no conflicts, validate distributed rate limiting.

### Production Environment

**Configuration**: Both Gateway and Router rate limiting (optimized)

```bash
# Gateway
GATEWAY_RATE_LIMIT_MODE=redis
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=10000  # High limit (endpoint-level)
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_TTL_SECONDS=60
GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-prod:6379
GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true

# Router
# Rate limiting enabled in policies (rate_limit.enabled=true)
# Policy limits: Varies by policy (100-1000 req/s, burst 50-500)
```

**Rationale**: 
- Gateway: High limit for DDoS protection (endpoint-level)
- Router: Policy-specific limits for quota enforcement (policy-level)
- Defense in depth: Gateway protects from overload, Router enforces quotas

## Configuration Guidelines

### Gateway Rate Limit Configuration

**Endpoint-Level Limits**:
- `/api/v1/routes/decide`: 1000-10000 req/min (depending on traffic)
- `/api/v1/messages`: 1000-10000 req/min
- Global limit: 50000-100000 req/min (all endpoints combined)

**Per-Tenant Limits** (CP2+):
- Default: 1000 req/min per tenant
- Premium tier: 10000 req/min per tenant
- Basic tier: 100 req/min per tenant

**Rationale**: Gateway limits should be **higher** than Router limits to allow Router to enforce policy-specific quotas.

### Router Rate Limit Configuration

**Policy-Level Limits**:
- Default policy: 100 req/s, burst 50
- High-volume policy: 500 req/s, burst 200
- Low-volume policy: 10 req/s, burst 5

**Tenant-Level Limits** (CP2+):
- Premium tenant: 1000 req/s, burst 500
- Basic tenant: 100 req/s, burst 50

**Rationale**: Router limits should be **lower** than Gateway limits to enforce per-policy quotas.

### Ratio Guidelines

**Recommended Ratio**: Gateway limit : Router limit = 10:1 to 100:1

**Example**:
- Gateway: 10000 req/min (endpoint-level)
- Router: 100 req/s = 6000 req/min (policy-level)
- Ratio: ~1.67:1 (acceptable, but Gateway should be higher)

**Better Example**:
- Gateway: 10000 req/min (endpoint-level)
- Router: 100 req/s = 6000 req/min (policy-level)
- Gateway per-tenant: 5000 req/min
- Router per-policy: 100 req/s = 6000 req/min
- Ratio: ~0.83:1 (Router higher, but different scopes)

**Best Practice**: Gateway limit should be **higher** than Router limit to allow Router to enforce quotas.

## Troubleshooting

### Issue: Double-Dropping

**Symptoms**: Requests blocked by both Gateway and Router (same request gets 429 twice)

**Causes**:
1. Gateway limit too low (lower than Router limit)
2. Both layers checking in parallel (race condition)
3. Configuration mismatch (Gateway and Router limits conflict)

**Solution**:
1. Increase Gateway limit (should be higher than Router limit)
2. Verify execution order (Gateway → Router, not parallel)
3. Check configuration (Gateway: endpoint-level, Router: policy-level)

### Issue: Router Never Called

**Symptoms**: All requests blocked by Gateway, Router never processes requests

**Causes**:
1. Gateway limit too low
2. Gateway rate limiting misconfigured

**Solution**:
1. Increase Gateway limit
2. Check Gateway rate limiting configuration
3. Verify Gateway allows requests to Router

### Issue: Router Rate Limiting Not Working

**Symptoms**: Router processes all requests, rate limiting not enforced

**Causes**:
1. Rate limiting disabled in policies (`rate_limit.enabled=false`)
2. Rate limit store not started
3. Policy not loaded correctly

**Solution**:
1. Check policy configuration (`rate_limit.enabled=true`)
2. Verify `router_rate_limit_store` is running
3. Check policy loading logs

## Monitoring

### Key Metrics to Monitor

1. **Gateway Rate Limit Exceeded**: `gateway_rate_limit_exceeded_total`
   - **Alert**: If > 100/min, investigate Gateway limit configuration

2. **Router Rate Limit Exceeded**: `router_rate_limit_exceeded_total`
   - **Alert**: If > 50/min, investigate Router limit configuration

3. **Double-Dropping**: `rate_limit_double_drop_total`
   - **Alert**: If > 0, critical configuration issue

4. **Redis Rate Limiting Errors**: `gateway_rate_limit_redis_errors_total`
   - **Alert**: If > 10/min, investigate Redis connectivity

### Recommended Alerts

1. **High Gateway Rate Limit Exceeded**:
   - Metric: `rate(gateway_rate_limit_exceeded_total[5m]) > 50`
   - Severity: Warning
   - Action: Review Gateway limit configuration

2. **High Router Rate Limit Exceeded**:
   - Metric: `rate(router_rate_limit_exceeded_total[5m]) > 10`
   - Severity: Warning
   - Action: Review Router limit configuration

3. **Double-Dropping Detected**:
   - Metric: `rate_limit_double_drop_total > 0`
   - Severity: Critical
   - Action: Fix configuration immediately

## References

- `docs/archive/dev/RATE_LIMIT_BOUNDARIES_ROUTER_VS_GATEWAY.md` - Boundaries document
- `docs/archive/dev/RATE_LIMITING_OBSERVABILITY_SPEC.md` - Observability specification
- `docs/GATEWAY_RATE_LIMITING.md` - Gateway rate limiting specification
- `docs/ROUTING_POLICY.md` - Router rate limiting specification

