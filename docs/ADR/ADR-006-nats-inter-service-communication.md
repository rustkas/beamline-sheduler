---
version: 1.0
status: accepted
date: 2025-11-05
deciders:
  - Agent 2: Architecture/Tech Lead
related_adrs:
  - ADR-004: Erlang/OTP for router (proposed)
  - ADR-008: NestJS for Gateway (proposed)
supersedes: []
superseded_by: []
---

# ADR-006: NATS for Inter-Service Communication

## Status

accepted

## Context

BeamLine Constructor has multiple components that need to communicate:
- NestJS Gateway ? Erlang/OTP Router
- Erlang/OTP Router ? Erlang/OTP Provider
- Erlang/OTP ? C++ CAF (indirectly)
- Real-time streaming requirements
- High throughput requirements
- Fault tolerance needs

Communication patterns needed:
- Request-Reply (synchronous)
- Publish-Subscribe (asynchronous)
- Streaming (Server-Sent Events)

## Decision

Use **NATS (JetStream)** as the primary message bus for inter-service communication.

**Communication Patterns**:

1. **Request-Reply**:
   - Gateway ? Router: `beamline.router.v1.decide` (request-reply)
   - Router ? Provider: `beamline.provider.v1.invoke` (request-reply)

2. **Publish-Subscribe**:
   - Usage events: `beamline.usage.v1.metered` (pub-sub)
   - Alerts: `beamline.alert.v1.*` (pub-sub)

3. **Streaming**:
   - Provider responses: `beamline.provider.v1.invoke.stream` (streaming)

**Subject Naming**:
```
beamline.{component}.{version}.{action}
```

**Components**:
- `router`: Router decisions
- `provider`: Provider invocations
- `ingress`: Ingress messages
- `usage`: Usage metering
- `alert`: Alert notifications

## Consequences

### Positive

- **Decoupling**: Services don't need direct connections
- **Scalability**: NATS handles high throughput
- **Fault tolerance**: NATS provides message persistence (JetStream)
- **Real-time**: Supports streaming patterns
- **Language agnostic**: Works with Erlang, C++, Node.js

### Negative

- **Additional infrastructure**: Requires NATS server
- **Network dependency**: Services depend on NATS availability
- **Message format**: Need to standardize message formats

### Neutral

- **Learning curve**: Team needs to learn NATS patterns
- **Monitoring**: Need to monitor NATS performance

## Alternatives Considered

### Alternative 1: Direct gRPC

**Description**: Use gRPC for all inter-service communication

**Pros**:
- Type-safe contracts (protobuf)
- High performance
- Built-in streaming

**Cons**:
- Requires direct connections
- Harder to scale
- No pub-sub pattern
- Complex service discovery

**Why not chosen**: Need for pub-sub and decoupling favor message bus approach

### Alternative 2: HTTP/REST

**Description**: Use HTTP/REST for all communication

**Pros**:
- Simple and familiar
- No additional infrastructure
- Works everywhere

**Cons**:
- Not suitable for high throughput
- No built-in pub-sub
- Tight coupling
- No streaming support

**Why not chosen**: Performance and pattern requirements favor NATS

### Alternative 3: Apache Kafka

**Description**: Use Kafka as message bus

**Pros**:
- High throughput
- Message persistence
- Strong ordering guarantees

**Cons**:
- Heavyweight
- Complex setup
- Overkill for current requirements

**Why not chosen**: NATS is lighter and sufficient for requirements

## Implementation Notes

- NATS subjects cataloged in `docs/NATS_SUBJECTS.md`
- Subject naming convention defined
- Integration patterns documented
- JetStream used for persistence and replay

## References

- `docs/NATS_SUBJECTS.md`: NATS subjects catalog
- `proto/beamline/flow/v1/flow.proto`: gRPC ABI (used alongside NATS)
- Architecture overview: see `docs/ADR/ADR-010-target-architecture.md`

## Compliance

- ? Aligns with `.trae/manifest.json`
- ? Follows compatibility policy
- ? Respects security constraints
- ? Integrates with STATE/HISTORY




