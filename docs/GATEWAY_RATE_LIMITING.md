# Gateway Rate Limiting (Universal) — CP1/CP2 Technical Specification

**Related**: See `docs/GATEWAY_CONFLICT_CONTRACT.md` for complete priority matrix including rate limiting vs other error sources (authentication, request validation, Router errors).

## Purpose
Implement universal, provider-agnostic rate limiting in the Gateway to protect APIs, enforce per-tenant quotas, and ensure fair usage across tenants and endpoints. This specification is independent of OpenAI-specific endpoints and applies to core routes under `/api/v1/*`.

## Design Overview
- Enforcement sits at Gateway ingress via a global guard, with endpoint-level overrides using decorators.
- CP1 favors simplicity and low latency (in-memory fixed window via `@nestjs/throttler`).
- CP2 introduces precision and horizontal scalability (Redis-backed sliding window, per-tenant quotas, admin introspection).
- Correlation IDs (`tenant_id`, `trace_id`, `message_id`) flow through guard decisions and logs.

## Scope
- Global and per-endpoint limits (CP1/CP2).
- Per-tenant quotas and class-based overrides (CP2).
- Sliding window counters (CP2) and fixed window (CP1).
- Deterministic 429 responses with `Retry-After` and rate-limit headers.
- Configuration via environment variables and a typed config provider.
- Storage backends: memory (CP1) and Redis (CP2).
- Observability (metrics, logs, traces) and admin introspection (CP2).

## Principles
- Universal and provider-agnostic.
- Deterministic enforcement with minimal latency overhead.
- Tenant awareness: `tenant_id` sourced from JWT or header `x-tenant-id`.
- Safe defaults, graceful degradation, and explicit error semantics.

## Configuration Reference
Environment variables (examples):
- `RATE_LIMIT_TTL_SECONDS=60` — window size in seconds.
- `RATE_LIMIT_GLOBAL=1000` — global requests per window.
- `RATE_LIMIT_ENDPOINT_MESSAGES=100` — `/api/v1/messages` requests per window.
- `RATE_LIMIT_ENDPOINT_ROUTES_DECIDE=50` — `/api/v1/routes/decide` per window.
- `RATE_LIMIT_PER_TENANT_DEFAULT=100` — default per-tenant limit (CP2).
- `RATE_LIMIT_PER_TENANT_OVERRIDES=premium:1000,basic:50` — class-based overrides (CP2).
- `RATE_LIMIT_STORAGE=memory|redis` — backend selection.
- `RATE_LIMIT_REDIS_URL=redis://localhost:6379` — Redis connection URI (CP2).

Config provider: `apps/gateway/src/config/rate-limit.config.ts`
- Parses envs, validates types, provides typed config used in guard/middleware.
- Exposes: `{ ttlSeconds, globalLimit, endpoints: { messages, routesDecide }, storage, redisUrl, tenant: { defaultLimit, overrides } }`.

## Keys and Dimensions
- `tenant_id` — JWT claim or `x-tenant-id` header; fallback `anonymous`.
- `endpoint_id` — canonical route id (e.g., `messages`, `routes_decide`).
- `method` — optional dimension for finer limits.
- `bucket_ts` — fixed window start timestamp.
- Composite key: `tenant_id:endpoint_id:bucket_ts`.

## Algorithms
- CP1 Minimal (fixed window / throttler):
  - Use `@nestjs/throttler` for a global limit and per-endpoint decorators.
  - Fast path in-memory counters; stateless between instances.
  - Note: CP1 is not horizontally consistent; acceptable for staging.
- CP2 Full (sliding window / Redis):
  - Maintain second-level buckets for `now..now-K`.
  - Weighted sum over window for precise counts.
  - Redis keys: `rl:{tenant}:{endpoint}:{bucket}` with expiry `ttlSeconds`.
  - Lua script for atomic increment + read (optional), or `MULTI/EXEC`.

## Enforcement Points
- Global guard: applies base config before controllers.
- Controller decorators: endpoint overrides for `messages` and `routes/decide`.
- Tenant-aware logic: missing tenant → `anonymous` with stricter defaults.

## Response Semantics
On limit exceeded:
- Status: `429 Too Many Requests`.
- Headers:
  - `Retry-After: <seconds>`
  - `X-RateLimit-Limit: <limit>`
  - `X-RateLimit-Remaining: 0`
  - `X-RateLimit-Reset: <epoch_seconds>`
- Body (JSON):
```json
{
  "error": "rate_limit_exceeded",
  "message": "Too many requests",
  "tenant_id": "t-123",
  "endpoint": "/api/v1/messages",
  "retry_after_seconds": 60
}
```

## Observability
- Metrics (Prometheus):
  - `gateway_rate_limit_hits_total{endpoint,tenant}`
  - `gateway_rate_limit_exceeded_total{endpoint,tenant}`
  - `gateway_rate_limit_window_active{endpoint}`
- Logs: JSON, include `trace_id`, `tenant_id`, `endpoint`, `limit`, `remaining`, `decision`.
- Traces: annotate spans with `rate_limit_applied=true`, `limit`, `remaining`.

## Admin Introspection (CP2)
- `GET /api/v1/admin/rate-limit/usage?tenant=<id>` — current counters summary.
- `POST /api/v1/admin/rate-limit/overrides` — update tenant class limits (admin RBAC).

## Implementation Plan
- Dependencies: `@nestjs/throttler` (CP1), optional `redis` (CP2).
- Files:
  - `apps/gateway/src/common/guards/rate-limit.guard.ts` — global guard.
  - `apps/gateway/src/common/middleware/rate-limit.middleware.ts` — optional header hints.
  - `apps/gateway/src/config/rate-limit.config.ts` — config provider.
- Module changes:
  - `app.module.ts` — import Throttler with base config; provide rate-limit config.
  - `main.ts` — apply global guard.
  - `messages.controller.ts`, `routes.controller.ts` — per-endpoint decorators.

## Acceptance Criteria
- CP1 Minimal (3–4 hours):
  - Global limit enforced and configurable via env.
  - Per-endpoint decorators for `/api/v1/messages` and `/api/v1/routes/decide`.
  - Deterministic 429 responses with `Retry-After` and rate-limit headers.
  - Basic metrics counters incremented.
- CP2 Full (1–2 days):
  - Per-tenant quotas with overrides and Redis-backed sliding window.
  - Admin introspection endpoints.
  - Extended metrics and trace annotations.

## Testing Strategy
- Unit: guard logic (allow vs block), tenant extraction, header calculation.
- Integration (e2e): hit endpoints rapidly to trigger 429 and validate headers/body.
- Config-driven: verify env/defaults/overrides mapping.
- Performance: overhead target `< 1 ms` per check (in-memory CP1).

## Security & RBAC
- Rate limits immutable by regular tenants.
- Admin-only operations restricted by JWT roles.
- Tenant extraction priority: JWT → header → default `anonymous`.

## Operational Playbook

**For detailed Gateway rate limiting operational procedures, see**:
- **`docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`** - Complete runbook covering:
  - Health checks (rate limiting status, metrics, configuration)
  - Configuration (changing limits via env/config/K8s)
  - Common symptoms (mass 429, inconsistent limits, rate limiting not working)
  - Diagnosis procedures (step-by-step troubleshooting)
  - Remediation steps (fixing root causes)
  - Distinguishing abuse vs strict limits (decision matrix)
  - Emergency procedures (mass 429, distributed rate limiting failure)
  - Monitoring and alerts (key metrics, alert rules)

**Quick Reference**:
- Hot-config updates via env reload (CP1) or admin overrides (CP2).
- Canary testing: enable stricter limits for subset of tenants.
- Incident handling: temporarily increase `Retry-After` and global limit; monitor `exceeded_total`.

## Versioning & Migration
- Start CP1 with memory backend; migrate to Redis in CP2.
- Maintain backward compatibility for headers and response shape.
- Document changes in `CHANGELOG.md` under Gateway.

## Distributed Rate Limiting (CP2+)

### Feature Flags

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
- `GATEWAY_RATE_LIMIT_REDIS_HOST` (default: `localhost`)
  - Redis hostname or IP address

- `GATEWAY_RATE_LIMIT_REDIS_PORT` (default: `6379`)
  - Redis port number

- `GATEWAY_RATE_LIMIT_REDIS_URI` (optional, **recommended for staging/production**)
  - Full Redis URI: `redis://host:port` or `redis://:password@host:port`
  - If set, overrides `GATEWAY_RATE_LIMIT_REDIS_HOST` and `GATEWAY_RATE_LIMIT_REDIS_PORT`
  - **Priority**: `GATEWAY_RATE_LIMIT_REDIS_URI` > `GATEWAY_RATE_LIMIT_REDIS_HOST:PORT`

- `GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS` (default: `1000`)
  - Redis connection timeout in milliseconds

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

**CP1 Mode (Default)**:
```bash
# No environment variables needed (defaults to memory mode)
# Or explicitly:
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=false
export GATEWAY_RATE_LIMIT_BACKEND=memory
```

**CP2+ Distributed Mode (Redis)**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_HOST=redis-staging.example.com
export GATEWAY_RATE_LIMIT_REDIS_PORT=6379
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
```

**CP2+ Distributed Mode (Redis with URI - Recommended)**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-staging.example.com:6379
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
```

**CP2+ Hybrid Mode (Redis with Local Cache - Experimental)**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=hybrid
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-staging.example.com:6379
export GATEWAY_RATE_LIMIT_LOCAL_CACHE_TTL_SECONDS=10
export GATEWAY_RATE_LIMIT_SYNC_INTERVAL_SECONDS=5
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
```

**CP2+ Strict Mode (No Fallback)**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-staging.example.com:6379
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=false
```

**Staging Mode (Recommended for CP2+ Rollout)**:
```bash
export GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true
export GATEWAY_RATE_LIMIT_MODE=redis
export GATEWAY_RATE_LIMIT_REDIS_URI=redis://redis-staging.example.com:6379
export GATEWAY_RATE_LIMIT_REDIS_TIMEOUT_MS=2000
export GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true
# Enable detailed logging for staging
export GATEWAY_LOG_LEVEL=INFO
```

### Fallback Behavior

**When Redis Unavailable**:
1. **If `GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=true`** (default):
   - Logs warning: "Rate limiter error, fallback to local mode"
   - Sets mode to "fallback"
   - Allows request (graceful degradation)
   - Continues operating in in-memory mode

2. **If `GATEWAY_RATE_LIMIT_FALLBACK_TO_LOCAL=false`**:
   - Logs error: "Rate limiter error and fallback disabled"
   - Returns 503 Service Unavailable
   - Rejects request (strict mode)

### Logging

**Mode Logging**:
- Startup: `INFO: Rate limiter initialized in {memory|redis} mode`
- Fallback: `WARNING: Rate limiter error (mode: redis), checking fallback`
- Fallback enabled: `INFO: Fallback to local mode enabled, allowing request`
- Fallback disabled: `ERROR: Rate limiter error and fallback disabled, rejecting request`

### Staging Rollout

**For detailed staging rollout plan, see**:
- **`docs/archive/dev/GATEWAY_DISTRIBUTED_RATE_LIMITING_STAGING_ROLLOUT.md`** - Complete staging rollout plan covering:
  - Integration into http_server.c
  - Feature flags and configuration
  - Staging rollout phases (6 phases)
  - Test scenarios (6 scenarios)
  - Monitoring and observability
  - Rollback procedures
  - Success criteria

## Notes
- Universal specification; no OpenAI-specific routes.
- Aligns with `docs/GATEWAY_UNIVERSAL_SPEC.md` and provider-agnostic DTOs.
