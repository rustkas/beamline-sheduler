---
version: 1.0
status: accepted
date: 2025-11-05
deciders:
  - Agent 2: Architecture/Tech Lead
related_adrs:
  - ADR-006: NATS for inter-service communication
  - ADR-005: Mnesia for caching (proposed)
supersedes: []
superseded_by: []
---

# ADR-004: Erlang/OTP for Router Core

## Status

accepted

## Context

The router core needs to:
- Handle high-throughput routing decisions
- Apply routing policies (weights, sticky, fallback)
- Cache policies for performance
- Integrate with NATS for communication
- Provide fault tolerance and supervision
- Support distributed deployment

Requirements:
- High availability (99.9% uptime)
- Low latency (p95 < 50ms)
- Fault tolerance (supervision trees)
- Distributed state (Mnesia)
- Policy caching (hot path)

## Decision

Use **Erlang/OTP** for router core implementation.

**Rationale**:
- **Fault tolerance**: OTP supervision trees provide automatic recovery
- **Concurrency**: Erlang's actor model handles high concurrency
- **Distribution**: Native support for distributed systems
- **Mnesia**: Built-in distributed database for caching
- **NATS integration**: Erlang has good NATS client libraries
- **gRPC support**: grpcbox provides gRPC server capabilities

**Architecture**:
```
beamline_router/
+-- router_core.erl          # Core routing logic
+-- router_policy.erl        # Policy management
+-- router_decider.erl       # Decision engine
+-- router_metrics.erl       # Prometheus metrics
+-- router_grpc.erl          # gRPC service
+-- router_sup.erl           # Supervisor tree
```

**Key Components**:
- **Supervisor tree**: Automatic restart on failure
- **Mnesia cache**: Policy caching (5 min TTL)
- **NATS client**: Request-reply pattern
- **gRPC server**: Router.Decide service
- **Metrics**: Prometheus export

## Consequences

### Positive

- **Fault tolerance**: Supervision trees provide automatic recovery
- **High concurrency**: Erlang handles thousands of concurrent requests
- **Distribution**: Native support for distributed deployment
- **Mnesia**: Built-in distributed caching
- **Mature ecosystem**: Well-established patterns and libraries

### Negative

- **Learning curve**: Team needs Erlang expertise
- **GC pauses**: Erlang GC can cause latency spikes (mitigated by tuning)
- **Memory usage**: Per-process memory overhead

### Neutral

- **Language choice**: Erlang is specialized but powerful
- **Deployment**: Requires Erlang runtime

## Alternatives Considered

### Alternative 1: Go

**Description**: Implement router in Go

**Pros**:
- Familiar language
- Good performance
- Good gRPC support

**Cons**:
- No built-in fault tolerance (need to implement)
- No built-in distributed state
- More complex supervision logic

**Why not chosen**: Erlang's built-in fault tolerance and distribution are critical requirements

### Alternative 2: Rust

**Description**: Implement router in Rust

**Pros**:
- High performance
- Memory safety
- Good concurrency

**Cons**:
- No built-in fault tolerance
- No built-in distributed state
- More complex implementation

**Why not chosen**: Erlang's OTP provides better fault tolerance and distribution out of the box

### Alternative 3: Node.js

**Description**: Implement router in Node.js

**Pros**:
- JavaScript/TypeScript
- Good NATS support
- Easy integration with Gateway

**Cons**:
- Single-threaded (limited concurrency)
- No built-in fault tolerance
- GC pauses can be significant

**Why not chosen**: Erlang's concurrency model and fault tolerance are superior

## Implementation Notes

- Router core specification in `docs/CP1_ROUTER_SPEC.md`
- gRPC service: `Router.Decide` from `proto/beamline/flow/v1/flow.proto`
- NATS integration: `beamline.router.v1.decide` subject
- Mnesia tables: `policy_cache`, `sticky_sessions`
- Metrics: Prometheus format via `prometheus.erl`

## References

- `docs/CP1_ROUTER_SPEC.md`: Router implementation specification
- `proto/beamline/flow/v1/flow.proto`: gRPC ABI
- `docs/NATS_SUBJECTS.md`: NATS subjects
- Architecture overview: see `docs/ADR/ADR-010-target-architecture.md`

## Compliance

- ? Aligns with `.trae/manifest.json`
- ? Follows compatibility policy
- ? Respects security constraints
- ? Integrates with STATE/HISTORY




