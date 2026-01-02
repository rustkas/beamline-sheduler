---
version: 1.0
status: accepted
date: 2025-01-27
deciders:
  - CP1-ROUTER Implementation Team
related_adrs:
  - ADR-011: JetStream E2E with Durable Subscriptions
  - ADR-005: Mnesia Caching (mentioned idempotency)
supersedes: []
superseded_by: []
---

# ADR-012: Idempotency Layer for Message Processing

## Status

accepted

## Context

Router processes messages from multiple sources:
- CAF execution results (`caf.exec.result.v1`)
- CAF assignment acknowledgments (`caf.exec.assign.v1.ack`)
- Usage events (`beamline.usage.v1.metered`)

**Problem**: Messages may be delivered multiple times due to:
- JetStream redelivery on NAK
- Network retries
- Consumer restarts
- Duplicate publications

**Requirement**: Prevent duplicate processing of:
- Execution results (avoid duplicate usage events)
- Assignment acknowledgments (avoid duplicate logging)
- Usage events (avoid duplicate billing)

## Decision

Implement **idempotency layer** using ETS with TTL to track processed messages.

**Key Components**:

1. **Idempotency Keys**:
   - `assignment_id`: For execution results and ACKs
   - `request_id`: Alternative correlation ID
   - `usage_id`: For usage events (derived from assignment_id or request_id)
   - `ack_id`: For assignment acknowledgments

2. **Storage**:
   - ETS table: `router_idempotency`
   - Key: `{KeyType, MessageId}`
   - Value: `#{message_id, key_type, processed_at, expires_at, additional_data}`
   - TTL: Configurable (default: 3600 seconds / 1 hour)

3. **Operations**:
   - `check_and_mark/3`: Check if message is already processed, mark as processed if not
   - Returns: `{ok, not_seen}` | `{ok, seen}` | `{error, Reason}`
   - Atomic operation: check and insert in single gen_server call

4. **Fail-Open Strategy**:
   - On idempotency check error, still process message (fail open)
   - Prevents idempotency system from blocking operations
   - Logs errors for monitoring

5. **Telemetry**:
   - `router_idempotency_miss_total`: New message (not seen)
   - `router_idempotency_hit_total`: Duplicate message (seen)
   - `router_idempotency_error_total`: Check errors

**Implementation**:
- `router_idempotency.erl`: Idempotency gen_server with ETS table
- Used by: `router_result_consumer.erl`, `router_ack_consumer.erl`

## Consequences

### Positive

- **Duplicate Prevention**: Prevents duplicate processing of messages
- **Billing Protection**: Prevents duplicate usage events
- **Audit Integrity**: Prevents duplicate audit logs
- **Performance**: ETS provides fast lookups
- **TTL Management**: Automatic cleanup of expired entries
- **Fail-Open**: Does not block operations on errors

### Negative

- **Memory Usage**: ETS table grows with message volume
- **TTL Window**: Messages within TTL window are protected, outside window may be reprocessed
- **Single Node**: ETS is local to node (not distributed)
- **Clock Dependency**: TTL depends on system clock

### Neutral

- **Configuration**: Requires TTL tuning based on message patterns
- **Monitoring**: Need to monitor hit/miss ratios

## Alternatives Considered

### Alternative 1: Database-backed Idempotency

**Description**: Use database (PostgreSQL, Redis) for idempotency tracking

**Pros**:
- Distributed across nodes
- Persistent across restarts
- Rich query capabilities

**Cons**:
- Higher latency
- Additional infrastructure dependency
- Network overhead
- More complex failure handling

**Why not chosen**: ETS provides sufficient performance and simplicity for CP2-LC

### Alternative 2: Message Deduplication in NATS

**Description**: Use NATS message deduplication features

**Pros**:
- No additional code
- Handled by infrastructure

**Cons**:
- Limited to NATS message ID
- Does not handle application-level duplicates
- Not suitable for cross-message idempotency (e.g., usage events)

**Why not chosen**: Need application-level idempotency control

### Alternative 3: No Idempotency

**Description**: Accept duplicate processing

**Pros**:
- Simplest implementation
- No overhead

**Cons**:
- Duplicate billing
- Duplicate audit logs
- Data inconsistency

**Why not chosen**: Business requirements demand idempotency

## Implementation Notes

**Idempotency Check**:
```erlang
case router_idempotency:check_and_mark(<<"assignment_id">>, AssignmentId, #{
    status => Status,
    job_type => JobType,
    provider_id => ProviderId
}) of
    {ok, seen} -> skip_processing();
    {ok, not_seen} -> process_message();
    {error, _} -> process_message()  % Fail open
end
```

**TTL Configuration**:
- Default: 3600 seconds (1 hour)
- Configurable via `idempotency_ttl_seconds`
- Should exceed maximum message processing time + redelivery window

**Cleanup**:
- Automatic via TTL expiration
- Periodic cleanup not required (ETS handles expiration)

## References

- `apps/otp/router/src/router_idempotency.erl`: Implementation
- `apps/otp/router/src/router_result_consumer.erl`: Usage in result processing
- `apps/otp/router/src/router_ack_consumer.erl`: Usage in ACK processing
- ADR-011: JetStream E2E (redelivery context)
- ADR-005: Mnesia Caching (mentioned idempotency)

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy
- ✅ Respects security constraints
- ✅ Integrates with STATE/HISTORY

