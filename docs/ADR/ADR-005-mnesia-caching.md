---
version: 1.0
status: accepted
date: 2025-11-05
deciders:
  - Agent 2: Architecture/Tech Lead
related_adrs:
  - ADR-004: Erlang/OTP for router
supersedes: []
superseded_by: []
---

# ADR-005: Mnesia for Policy Caching

## Status

accepted

## Context

Router needs to:
- Load routing policies from PostgreSQL
- Cache policies for fast access (hot path)
- Support distributed caching across Erlang nodes
- Handle policy updates and invalidation
- Maintain consistency across nodes

Requirements:
- Low latency (cache hit < 1ms)
- High availability (cache available even if PostgreSQL is down)
- Distributed consistency
- TTL-based expiration (5 minutes)
- Policy versioning

## Decision

Use **Mnesia** (Erlang's built-in distributed database) for policy caching.

**Cache Structure**:
```erlang
-record(policy_cache, {
    key,           % {tenant_id, policy_id}
    policy_json,   % Parsed policy JSON
    version,       % Policy version
    cached_at,     % Timestamp
    expires_at     % TTL expiration
}).
```

**Cache Strategy**:
- **Load**: From PostgreSQL on cache miss
- **Store**: In Mnesia with TTL (5 minutes)
- **Invalidate**: On policy update or TTL expiration
- **Distribute**: Replicate across Erlang cluster nodes

**Tables**:
- `policy_cache`: Routing policies
- `sticky_sessions`: Session stickiness
- `rate_counters`: Rate limiting (optional for CP1)
- `idempotency`: Idempotency tracking (optional for CP1)

## Consequences

### Positive

- **Low latency**: In-memory access (< 1ms)
- **Distribution**: Native replication across nodes
- **Fault tolerance**: Survives node failures
- **No external dependency**: Built into Erlang/OTP
- **ACID guarantees**: Transactional consistency

### Negative

- **Memory usage**: Policies stored in memory
- **Complexity**: Need to handle replication and consistency
- **Erlang-specific**: Only works with Erlang/OTP

### Neutral

- **TTL management**: Need to implement expiration logic
- **Update propagation**: Need to handle policy updates

## Alternatives Considered

### Alternative 1: Redis

**Description**: Use Redis for policy caching

**Pros**:
- Industry standard
- Rich data structures
- Good performance

**Cons**:
- External dependency
- Network latency
- Additional infrastructure

**Why not chosen**: Mnesia is built-in and provides better integration with Erlang

### Alternative 2: ETS (Erlang Term Storage)

**Description**: Use ETS for local caching

**Pros**:
- Very fast (in-process)
- Simple API
- No distribution overhead

**Cons**:
- Not distributed (local to node)
- Lost on node restart
- No replication

**Why not chosen**: Need for distributed caching across cluster nodes

### Alternative 3: PostgreSQL Only

**Description**: Query PostgreSQL directly (no cache)

**Pros**:
- Simple (no cache layer)
- Always up-to-date
- No cache invalidation

**Cons**:
- Higher latency (network + DB query)
- Database load
- Single point of failure

**Why not chosen**: Performance requirements (hot path) require caching

## Implementation Notes

- Mnesia tables defined in `docs/MNESIA_ETS_KEYS.md`
- Cache TTL: 5 minutes
- Replication: `ram_copies` for performance
- Consistency: Eventual consistency across nodes
- Policy updates: Invalidate cache on PostgreSQL update

## References

- `docs/MNESIA_ETS_KEYS.md`: Mnesia table definitions
- `docs/CP1_ROUTER_SPEC.md`: Router implementation
- `docs/ROUTING_POLICY.md`: Policy format

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy
- ✅ Respects security constraints
- ✅ Integrates with STATE/HISTORY


