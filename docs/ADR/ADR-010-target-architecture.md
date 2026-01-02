---
version: 1.0
status: accepted
date: 2025-11-05
deciders:
  - Agent 2: Architecture/Tech Lead
related_adrs:
  - ADR-001-monorepo-structure.md
  - ADR-002-state-management.md
  - ADR-004-erlang-otp-router.md
  - ADR-006-nats-inter-service-communication.md
supersedes: []
superseded_by: []
---

# ADR-010: Target Architecture and Module Boundaries

## Status

accepted

## Context

BeamLine Constructor is a message routing and processing system that needs to:
- Route messages between multiple AI providers (OpenAI, Anthropic, Google, etc.)
- Apply routing policies (weights, sticky sessions, fallback)
- Collect usage metrics and enforce quotas
- Provide REST/SSE APIs for external clients
- Maintain audit trail with HMAC chain
- Support distributed state management

The system must handle:
- High throughput (1000+ req/s per component)
- Low latency (p95 < 500ms for routing)
- High availability (99.9% uptime)
- Multi-tenant isolation
- Policy-based routing decisions

## Decision

Adopt a **layered microservices architecture** with clear module boundaries:

### Architecture Layers

1. **Presentation Layer** (NestJS Gateway, SvelteKit UI)
   - HTTP/REST/SSE interfaces
   - Authentication and authorization
   - Request/response transformation

2. **Routing Layer** (Erlang/OTP Router)
   - Policy evaluation
   - Provider selection
   - Decision making

3. **Execution Layer** (C++ CAF Provider, Erlang/OTP Provider)
   - Provider invocation
   - Stream handling
   - Retry logic

4. **Data Layer** (PostgreSQL, Mnesia/ETS)
   - Source of Truth (PostgreSQL)
   - Hot path caching (Mnesia/ETS)

### Module Boundaries

#### Boundary 1: Gateway ↔ Router (NATS)

**Protocol**: NATS (Request-Reply)  
**Subject**: `beamline.router.v1.decide`  
**Data Format**: JSON → Protobuf  
**Responsibilities**:
- Gateway: HTTP → NATS transformation, correlation (tenant_id, trace_id)
- Router: Policy loading, decision making, response formatting

**Failure Handling**: Behavior during NATS publish/publish_with_ack errors is explicitly specified in `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` and `apps/otp/router/docs/NATS_PUBLISH_FAILURE_MONITORING.md`.

**Integration with STATE/HISTORY**:
- Router reads policies from PostgreSQL (Source of Truth)
- Router caches policies in Mnesia for hot path
- All decisions logged with trace_id for audit

#### Boundary 2: Router ↔ Provider (gRPC)

**Protocol**: gRPC  
**Services**: `Provider.Invoke`, `Provider.InvokeStream`  
**Data Format**: Protobuf  
**Responsibilities**:
- Router: Sends routing decision to provider
- Provider: Executes request, streams response

**Integration with STATE/HISTORY**:
- Provider invocations logged with usage metrics
- Trace_id propagated through gRPC metadata

#### Boundary 3: Gateway ↔ UI (HTTP/REST, SSE)

**Protocol**: HTTP/REST, Server-Sent Events  
**Endpoints**: `/api/v1/*`  
**Data Format**: JSON  
**Responsibilities**:
- Gateway: API provision, SSE streaming
- UI: User interface, real-time updates

#### Boundary 4: All Components ↔ Data Layer

**PostgreSQL** (Source of Truth):
- Policies, API keys, usage events, audit logs
- Accessed via epgsql/pgo (Erlang) or native drivers (NestJS)

**Mnesia/ETS** (Hot Path):
- Policy cache (router)
- Rate limit counters
- Sticky session mappings
- Idempotency keys

**Integration with STATE/HISTORY**:
- All state changes validated against `.trae/state.json` schema
- Significant changes recorded in `.trae/history.json` with HMAC
- Artifact checksums maintained for No-Drift principle

### Communication Patterns

1. **Request-Reply** (NATS): Gateway → Router
2. **Streaming** (gRPC): Router → Provider
3. **Pub-Sub** (NATS): Usage events, alerts
4. **HTTP/REST**: External clients → Gateway
5. **SSE**: Gateway → UI (real-time updates)

## Consequences

### Positive

- **Clear separation**: Each layer has distinct responsibilities
- **Scalability**: Components can scale independently
- **Technology fit**: Erlang for routing (concurrency), C++ for performance, NestJS for API
- **Testability**: Clear boundaries enable isolated testing
- **Maintainability**: Changes in one layer don't affect others

### Negative

- **Complexity**: Multiple technologies and protocols
- **Latency**: Multiple hops (Gateway → NATS → Router → gRPC → Provider)
- **Debugging**: Distributed tracing required
- **Deployment**: Multiple services to deploy and coordinate

### Neutral

- **State management**: Requires careful coordination between PostgreSQL and Mnesia
- **Versioning**: API versioning must be coordinated across layers

## Alternatives Considered

### Alternative 1: Monolithic Architecture

**Description**: Single application with all components

**Pros**:
- Simpler deployment
- Lower latency (no network hops)
- Easier debugging

**Cons**:
- Harder to scale individual components
- Technology lock-in (can't use best tool for each job)
- Tight coupling

**Why not chosen**: Need for independent scaling and technology diversity

### Alternative 2: Event-Driven Only

**Description**: All communication via events (no request-reply)

**Pros**:
- Loose coupling
- Better scalability
- Async processing

**Cons**:
- Harder to handle synchronous requests
- More complex error handling
- Latency for request-response patterns

**Why not chosen**: Need for synchronous request-response for routing decisions

## Implementation Notes

- Architecture aligns with `.trae/manifest.json` schema versions
- All module boundaries documented in `docs/ARCHITECTURE/context-maps.md`
- API contracts defined in `proto/beamline/*/v1/*.proto`
- Integration points specified in `docs/GATEWAY_ROUTES.md` and `docs/UI_ROUTES.md`
- State management follows `.cursor/rules/agents/state-and-history-management.mdc`

## References

- `docs/ARCHITECTURE/context-maps.md`: Context maps and diagrams
- `docs/GATEWAY_ROUTES.md`: Gateway API endpoints
- `docs/UI_ROUTES.md`: UI routes and integration
- `proto/beamline/flow/v1/flow.proto`: Router ABI
- `proto/beamline/provider/v1/provider.proto`: Provider ABI
- `.trae/manifest.json`: Schema versions and validation rules

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy (semver)
- ✅ Respects security constraints (HMAC masking, secret storage)
- ✅ Integrates with STATE/HISTORY (artifact checksums, HMAC chain)


