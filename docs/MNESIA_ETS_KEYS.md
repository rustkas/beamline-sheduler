# Mnesia/ETS Keys (Router)

Tables used by the Router for caching, rate limiting, sticky sessions, and idempotency.

## policy_cache
Purpose: cache compiled routing policies per tenant
- Key: `tenant_id`
- Fields: `policy_version`, `compiled_rules`, `ts`
- TTL: 5 minutes, refreshed on policy change

## sticky_sessions
Purpose: maintain stickiness to a provider for a session
- Key: `session_key` (e.g., `user_id` or custom)
- Fields: `tenant_id`, `provider`, `expires_at`
- Index: by `tenant_id`
- TTL: configured in policy (`sticky.ttl`)

## rate_counters
Purpose: per-tenant/per-endpoint rate limiting
- Key: `{tenant_id}:{endpoint}`
- Fields: `window_start`, `count`
- TTL: matches sliding window duration

## idempotency
Purpose: deduplicate message processing
- Key: `message_id`
- Fields: `tenant_id`, `status`, `result_ref`, `ts`
- TTL: 24h

## Cleanup
- Periodic sweeper removes expired entries
- Indexes support fast lookup by tenant

## References
- `docs/ROUTING_POLICY.md`  sticky session and weights
- `docs/GATEWAY_ROUTES.md`  endpoints participating in rate limiting
