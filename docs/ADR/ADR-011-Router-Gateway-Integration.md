---
version: 1.0
authors:
  - WORKER wrk-2: Architecture/Tech Lead
last_update: 2025-01-27T12:00:00Z
status: draft
rule_version: v10
message_protocol: v1
---

# ADR-014: Router-Gateway Integration Architecture

## Status

**Status**: Draft  
**Date**: 2025-01-27  
**Deciders**: WORKER wrk-2 (Architecture/Tech Lead)

## Context

Currently, Gateway (NestJS) and Router (Erlang/OTP) have duplicate logic:

1. **Authentication/Authorization**:
   - Gateway uses local allowlists (`GATEWAY_TENANT_ALLOWLIST`, `GATEWAY_POLICY_ALLOWLIST`)
   - Gateway has hardcoded `roleMatrix` for RBAC
   - Router has centralized RBAC system via `RouterAdmin` service

2. **Policy Management**:
   - Gateway stores policies in local in-memory `Map`
   - Router has centralized policy storage via `RouterAdmin` service
   - No synchronization between Gateway and Router

3. **Protocol Communication**:
   - Gateway supports HTTP, NATS, Mock for Router communication
   - gRPC mode not implemented (only stub)
   - Router supports gRPC (port 9000) and NATS

**Problem**: Duplicate logic leads to:
- Inconsistency between Gateway and Router
- Maintenance burden (changes needed in two places)
- No single source of truth
- Difficult to scale and maintain

## Decision

**Unify Gateway and Router through Router Admin gRPC integration**:

1. **Router as Single Source of Truth**:
   - All RBAC checks go through Router Admin gRPC
   - All policies managed via Router Admin gRPC
   - All tenant quotas and rate limits via Router Admin gRPC

2. **Gateway as Adapter Layer**:
   - Gateway adapts HTTP/REST to Router protocols (gRPC/NATS)
   - No business logic in Gateway
   - All decisions delegated to Router

3. **Protocol Flexibility**:
   - Support both gRPC and NATS
   - Auto-selection based on payload size and latency
   - gRPC for reliable, structured communication
   - NATS for high-throughput scenarios

4. **Gradual Migration**:
   - Feature flags for gradual rollout
   - Fallback to local logic if Router unavailable
   - No breaking changes to existing API contracts

## Consequences

### Positive

- ✅ **Single Source of Truth**: Router is authoritative for all policies and RBAC
- ✅ **Consistency**: No more duplicate logic between Gateway and Router
- ✅ **Maintainability**: Changes in one place (Router) propagate automatically
- ✅ **Scalability**: Router can handle multiple Gateway instances
- ✅ **Observability**: Centralized logging and metrics in Router
- ✅ **Security**: Centralized RBAC and audit trail

### Negative

- ⚠️ **Dependency**: Gateway depends on Router availability
- ⚠️ **Latency**: Additional network hop for RBAC/policy checks
- ⚠️ **Complexity**: More complex error handling and retry logic
- ⚠️ **Migration Effort**: Requires careful migration plan

### Mitigations

- **Dependency**: Circuit breaker and graceful degradation
- **Latency**: Caching layer (60s for RBAC, 5min for policies)
- **Complexity**: Well-defined error contracts and mapping
- **Migration**: Feature flags and gradual rollout

## Alternatives Considered

### Alternative 1: Keep Duplicate Logic

**Pros**:
- No dependency on Router
- Lower latency (no network hop)

**Cons**:
- Inconsistency between Gateway and Router
- Maintenance burden
- No single source of truth

**Decision**: Rejected - inconsistency and maintenance burden outweigh benefits

### Alternative 2: Gateway as Primary, Router Reads from Gateway

**Pros**:
- Gateway controls all logic
- Router is stateless

**Cons**:
- Gateway becomes bottleneck
- Difficult to scale
- Router loses control over routing logic

**Decision**: Rejected - violates CP1 boundaries (Router is core routing engine)

### Alternative 3: Shared Database

**Pros**:
- Both Gateway and Router read from same source
- No network dependency

**Cons**:
- Database becomes bottleneck
- Requires database schema changes
- Violates microservices boundaries

**Decision**: Rejected - adds database dependency and violates boundaries

## Implementation Plan

### Phase 1: Infrastructure (Week 1)
- Generate TypeScript types from proto
- Create Router Admin gRPC client interface
- Set up gRPC client library

### Phase 2: Auth Integration (Week 1-2)
- Implement RouterAuthService
- Update RBACGuard with feature flag
- Remove local allowlists

### Phase 3: Policy Integration (Week 2)
- Implement RouterPoliciesService
- Update PoliciesService with feature flag
- Export/import existing policies

### Phase 4: Protocol Unification (Week 2-3)
- Implement RouterProtocolService
- Complete gRPC client
- Add protocol metrics

### Phase 5: Validation (Week 3-4)
- Integration tests
- Load testing
- Gradual rollout

## Compliance

- ✅ **CP1 Boundaries**: Gateway does not contain routing logic, Router does not know HTTP
- ✅ **No-Drift**: All contracts defined in proto files
- ✅ **Source of Truth**: Router is authoritative for policies and RBAC
- ✅ **Observability**: Distributed tracing and metrics
- ✅ **Backward Compatibility**: Feature flags allow gradual migration

## References

- `docs/archive/dev/ROUTER_GATEWAY_INTEGRATION_ARCHITECTURE.md` - Detailed architecture specification
- `docs/ROUTER_GATEWAY_INTEGRATION_SPEC.md` - Technical specification
- `proto/beamline/flow/v1/flow.proto` - Router and RouterAdmin service definitions
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` - CP1 module boundaries

