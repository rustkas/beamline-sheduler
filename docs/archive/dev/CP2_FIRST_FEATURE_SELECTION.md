# CP2 First Feature Selection and Implementation Checklist

## Purpose

This document compares Circuit Breaker vs Rate Limit for CP2 implementation, selects the first feature to implement, and provides a detailed implementation checklist.

## Status

✅ **DECISION MADE** - Circuit Breaker selected as first CP2 feature

**Date**: 2025-01-27

## Feature Comparison

### Circuit Breaker vs Rate Limit

| Aspect | Circuit Breaker | Rate Limit |
|--------|----------------|------------|
| **Complexity** | Medium | High |
| **Gateway Dependencies** | None (Router-internal) | Requires coordination protocol |
| **Storage** | ETS (per-provider state) | ETS (per-policy/tenant counters) |
| **Integration Points** | Policy applier, fallbacks | Policy applier, Gateway coordination |
| **State Management** | Per-provider (simple) | Multi-level (global/tenant/policy) |
| **Algorithm** | State machine (3 states) | Token bucket (counters) |
| **Test Complexity** | Medium (state transitions) | High (multi-level, coordination) |
| **Risk to CP1** | Low (additive, isolated) | Medium (Gateway coordination) |
| **Implementation Tasks** | 7 tasks | 6 tasks (+ Gateway coordination) |

### Risk Analysis

#### Circuit Breaker Risks

**Low Risk**:
- ✅ **No Gateway Dependencies**: Router-internal, no external coordination needed
- ✅ **Isolated State**: Per-provider state in ETS, no shared state
- ✅ **Additive Feature**: Doesn't modify existing logic, only adds checks
- ✅ **Simple Integration**: Check before provider selection, update after call
- ✅ **Well-Defined States**: 3 states (closed/open/half-open), clear transitions

**Medium Risk**:
- ⚠️ **State Consistency**: Per-instance state (acceptable for CP2)
- ⚠️ **Performance Impact**: ETS lookups (minimal, < 1ms)

**Mitigation**:
- ETS lookups are O(1) and very fast
- State is per-instance (eventual consistency acceptable)
- Circuit breaker is optional (backward compatible)

#### Rate Limit Risks

**Medium Risk**:
- ⚠️ **Gateway Coordination**: Requires coordination protocol (HTTP/gRPC)
- ⚠️ **Multi-Level Complexity**: Global/tenant/policy scopes add complexity
- ⚠️ **State Synchronization**: Shared limits across instances (if distributed)
- ⚠️ **Integration Complexity**: Multiple enforcement points (Gateway + Router)

**Low Risk**:
- ✅ **Additive Feature**: Doesn't modify existing logic
- ✅ **Optional Feature**: Can be disabled per policy
- ✅ **Well-Defined Algorithm**: Token bucket is standard

**Mitigation**:
- Gateway coordination is optional (Router can work independently)
- Per-instance rate limits sufficient for CP2
- ETS storage is fast and sufficient

### Impact Analysis

#### Circuit Breaker Impact

**Affected Components**:
- `router_policy_applier.erl` - Add circuit breaker check
- `router_policy_store.erl` - Parse circuit breaker config
- `router_decider.erl` - Skip providers with open circuit
- New: `router_circuit_breaker.erl` - State machine

**CP1 Invariants**:
- ✅ **No Breaking Changes**: Circuit breaker is optional, backward compatible
- ✅ **Provider Selection**: Doesn't change selection algorithm, only filters
- ✅ **Explanation Format**: Adds circuit breaker info, doesn't break format
- ✅ **Extension Pipeline**: No impact on extensions

**Infrastructure**:
- ✅ **No External Dependencies**: ETS storage, no Gateway coordination
- ✅ **Minimal Metrics**: Standard Prometheus metrics
- ✅ **Simple Logging**: State transition logs

#### Rate Limit Impact

**Affected Components**:
- `router_policy_applier.erl` - Add rate limit check
- `router_policy_store.erl` - Parse rate limit config
- New: `router_rate_limit_store.erl` - Token bucket storage
- New: `router_rate_limit_gateway.erl` - Gateway coordination (optional)

**CP1 Invariants**:
- ✅ **No Breaking Changes**: Rate limit is optional, backward compatible
- ⚠️ **Gateway Coordination**: Requires protocol design and implementation
- ✅ **Explanation Format**: Adds rate limit info, doesn't break format

**Infrastructure**:
- ⚠️ **Gateway Coordination**: Requires HTTP/gRPC protocol
- ✅ **Storage**: ETS (can add Redis later)
- ✅ **Metrics**: Standard Prometheus metrics

## Decision: Circuit Breaker Selected

### Rationale

1. **Lower Complexity**: Simpler state machine vs multi-level rate limiting
2. **No Gateway Dependencies**: Router-internal, no coordination needed
3. **Isolated Implementation**: Can be implemented independently
4. **Lower Risk**: Fewer integration points, simpler testing
5. **Foundation for Rate Limit**: Circuit breaker patterns can inform rate limit design

### Implementation Order

**Phase 1**: Circuit Breaker (selected)
- **Rationale**: Simpler, isolated, lower risk
- **Timeline**: 2-3 weeks

**Phase 2**: Rate Limit (after Circuit Breaker)
- **Rationale**: Can leverage Circuit Breaker patterns
- **Timeline**: 2-3 weeks

## Circuit Breaker Implementation Checklist

### Overview

**Total Tasks**: 7 tasks
**Estimated Time**: 2-3 weeks
**Dependencies**: None (can start immediately)

### Task Breakdown

#### Phase 1: Foundation (Parallel Execution)

**Task 1: Circuit Breaker State Machine Module** ✅ Foundation

**Module**: `router_circuit_breaker.erl` (new)

**Files**:
- `apps/otp/router/src/router_circuit_breaker.erl`
- `apps/otp/router/include/router_circuit_breaker.hrl` (if needed)

**Sub-tasks**:
1. Define circuit breaker state record (`#circuit_breaker_state{}`)
2. Implement state transitions (Closed → Open → Half-Open → Closed)
3. Implement state persistence (ETS table `router_provider_circuit_breaker`)
4. Implement state query functions (`get_state/2`, `is_open/2`, `is_half_open/2`)
5. Implement state transition functions (`open_circuit/3`, `close_circuit/3`, `half_open_circuit/3`)

**Dependencies**: None (can start immediately)

**Test Suite**: `router_circuit_breaker_SUITE.erl` (new)

**Acceptance Criteria**:
- ✅ State transitions work correctly (Closed → Open → Half-Open → Closed)
- ✅ State persistence in ETS table
- ✅ State query functions return correct state
- ✅ Unit tests pass

**Estimated Time**: 3-4 days

---

**Task 4: Circuit Breaker Configuration Parsing** ✅ Foundation

**Module**: `router_policy_store.erl` (modify)

**Files**:
- `apps/otp/router/src/router_policy_store.erl`

**Sub-tasks**:
1. Parse `circuit_breaker` block from policy JSON
2. Store circuit breaker config in `#policy{}` record
3. Validate circuit breaker configuration (thresholds, timeouts)
4. Handle default values (if not specified)

**Dependencies**: None (can be done in parallel with Task 1)

**Test Suite**: `router_policy_store_SUITE.erl` (update)

**Acceptance Criteria**:
- ✅ Circuit breaker config parsed from JSON
- ✅ Config stored in `#policy{}` record
- ✅ Validation works (positive integers, reasonable ranges)
- ✅ Default values applied correctly

**Estimated Time**: 1-2 days

---

**Task 5: Circuit Breaker Schema and Fixtures** ✅ Foundation

**Files**:
- `apps/otp/router/docs/schemas/policy.schema.json` (update)
- `apps/otp/router/priv/fixtures/policies/*/circuit_breaker*.json` (new)

**Sub-tasks**:
1. Add `circuit_breaker` to policy schema
2. Create fixtures with circuit breaker configuration
3. Update schema validation tests
4. Validate fixtures against schema

**Dependencies**: Task 4 (needs configuration structure)

**Test Suite**: `router_policy_validator_SUITE.erl` (update)

**Acceptance Criteria**:
- ✅ Schema includes `circuit_breaker` block
- ✅ Fixtures created (basic, with overrides, error rate based)
- ✅ Schema validation passes
- ✅ Fixtures validate against schema

**Estimated Time**: 1-2 days

---

#### Phase 2: Metrics and Integration (Sequential)

**Task 2: Circuit Breaker Metrics Collection** ✅ Integration

**Module**: `router_circuit_breaker_metrics.erl` (new, or integrate into `router_circuit_breaker.erl`)

**Files**:
- `apps/otp/router/src/router_circuit_breaker.erl` (extend)

**Sub-tasks**:
1. Track error counts per provider
2. Track timeout counts per provider
3. Track error rate (errors / total requests)
4. Implement sliding window for error rate calculation
5. Emit metrics to Prometheus (via `router_metrics.erl`)

**Dependencies**: Task 1 (needs circuit breaker state)

**Test Suite**: `router_circuit_breaker_SUITE.erl` (extend)

**Acceptance Criteria**:
- ✅ Error counts tracked per provider
- ✅ Error rate calculated correctly (over time window)
- ✅ Metrics emitted to Prometheus
- ✅ Metrics tests pass

**Estimated Time**: 2-3 days

---

**Task 3: Circuit Breaker Integration with Policy Applier** ✅ Integration

**Module**: `router_policy_applier.erl` (modify)

**Files**:
- `apps/otp/router/src/router_policy_applier.erl`

**Sub-tasks**:
1. Check circuit breaker state before provider selection
2. Skip providers with open circuit breaker
3. Update circuit breaker state after provider call (success/failure)
4. Integrate with fallback logic (circuit open → fallback)
5. Update explanation to include circuit breaker state

**Dependencies**: Task 1, Task 2 (needs state machine and metrics)

**Test Suite**: `router_policy_applier_dsl_SUITE.erl` (update), `router_policy_integration_SUITE.erl` (update)

**Acceptance Criteria**:
- ✅ Circuit breaker check before provider selection
- ✅ Providers with open circuit skipped
- ✅ Circuit breaker state updated after calls
- ✅ Fallback triggered when circuit open
- ✅ Explanation includes circuit breaker info
- ✅ Integration tests pass

**Estimated Time**: 3-4 days

---

#### Phase 3: Observability and Documentation (Final)

**Task 6: Circuit Breaker Observability** ✅ Observability

**Module**: `router_audit.erl` (modify)

**Files**:
- `apps/otp/router/src/router_audit.erl`

**Sub-tasks**:
1. Log circuit breaker state transitions
2. Include circuit breaker state in decision explanations
3. Emit circuit breaker events to audit trail
4. Format logs per `OBSERVABILITY_CONVENTIONS.md`

**Dependencies**: Task 1, Task 3 (needs state machine and integration)

**Test Suite**: `router_policy_decision_logging_SUITE.erl` (new or update)

**Acceptance Criteria**:
- ✅ State transitions logged (structured JSON)
- ✅ Decision explanations include circuit breaker state
- ✅ Audit events emitted correctly
- ✅ Logging tests pass

**Estimated Time**: 1-2 days

---

**Task 7: Circuit Breaker Documentation** ✅ Documentation

**Files**:
- `docs/ROUTING_POLICY.md` (update - already has section)
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` (update with implementation notes)

**Sub-tasks**:
1. Update ROUTING_POLICY.md with implementation details
2. Add operational guide section for circuit breaker
3. Update API documentation
4. Add troubleshooting section

**Dependencies**: All previous tasks (documentation after implementation)

**Test Suite**: N/A (documentation)

**Acceptance Criteria**:
- ✅ ROUTING_POLICY.md updated with implementation details
- ✅ Operational guide added
- ✅ API documentation updated
- ✅ Troubleshooting section added

**Estimated Time**: 1 day

---

## Implementation Order Summary

### Phase 1: Foundation (Parallel, Week 1)

**Can be done in parallel**:
- ✅ Task 1: State Machine Module (3-4 days)
- ✅ Task 4: Configuration Parsing (1-2 days)
- ✅ Task 5: Schema and Fixtures (1-2 days)

**Total**: 3-4 days (parallel execution)

### Phase 2: Integration (Sequential, Week 2)

**Sequential order**:
1. ✅ Task 2: Metrics Collection (2-3 days) → depends on Task 1
2. ✅ Task 3: Policy Applier Integration (3-4 days) → depends on Task 1, Task 2

**Total**: 5-7 days

### Phase 3: Observability and Documentation (Week 3)

**Sequential order**:
1. ✅ Task 6: Observability (1-2 days) → depends on Task 1, Task 3
2. ✅ Task 7: Documentation (1 day) → depends on all tasks

**Total**: 2-3 days

### Overall Timeline

**Total Estimated Time**: 2-3 weeks (10-14 days)

**Critical Path**:
- Task 1 (3-4 days) → Task 2 (2-3 days) → Task 3 (3-4 days) → Task 6 (1-2 days) → Task 7 (1 day)
- **Total Critical Path**: 10-14 days

## Feature Readiness Criteria

### Circuit Breaker Feature is "Ready" When:

#### 1. All Test Suites Pass ✅

**Required Test Suites**:
- ✅ `router_circuit_breaker_SUITE.erl` - State machine tests (all test cases pass)
- ✅ `router_policy_store_SUITE.erl` - Config parsing tests (circuit breaker config)
- ✅ `router_policy_validator_SUITE.erl` - Schema validation tests (circuit breaker schema)
- ✅ `router_policy_applier_dsl_SUITE.erl` - DSL tests (circuit breaker integration)
- ✅ `router_policy_integration_SUITE.erl` - Integration tests (end-to-end circuit breaker)
- ✅ `router_policy_decision_logging_SUITE.erl` - Observability tests (logging and metrics)

**Test Coverage**:
- ✅ State transitions (Closed → Open → Half-Open → Closed)
- ✅ Error tracking (failure_count, error_rate)
- ✅ Provider filtering (skip open circuits)
- ✅ Fallback integration (circuit open → fallback)
- ✅ Explanation format (circuit breaker state included)
- ✅ Metrics emission (Prometheus metrics)
- ✅ Logging (structured JSON logs)

#### 2. All Metrics Emitted ✅

**Required Metrics**:
- ✅ `router_provider_circuit_breaker_state{tenant_id, provider_id, state}` (gauge)
- ✅ `router_provider_circuit_breaker_opened_total{tenant_id, provider_id, reason}` (counter)
- ✅ `router_provider_circuit_breaker_closed_total{tenant_id, provider_id}` (counter)
- ✅ `router_provider_circuit_breaker_half_opened_total{tenant_id, provider_id}` (counter)
- ✅ `router_provider_circuit_breaker_failures_total{tenant_id, provider_id, error_type}` (counter)
- ✅ `router_provider_circuit_breaker_requests_total{tenant_id, provider_id, state}` (counter)

**Verification**:
- ✅ Metrics defined in `router_metrics.erl`
- ✅ Metrics emitted in `router_circuit_breaker.erl`
- ✅ Metrics visible in Prometheus (if Prometheus enabled)
- ✅ Metrics tests pass

#### 3. All Documentation Updated ✅

**Required Documentation**:
- ✅ `docs/ROUTING_POLICY.md` - Circuit breaker section updated with implementation details
- ✅ `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Implementation notes added
- ✅ `docs/OPERATIONAL_GUIDE.md` - Circuit breaker operational guide (if exists)
- ✅ `docs/OBSERVABILITY.md` - Circuit breaker metrics and logging (if exists)

**Verification**:
- ✅ All documentation files updated
- ✅ Examples match implementation
- ✅ API documentation accurate
- ✅ Troubleshooting section added

#### 4. Backward Compatibility Verified ✅

**Required Checks**:
- ✅ Legacy policies (without circuit_breaker) work correctly
- ✅ Circuit breaker disabled by default (backward compatible)
- ✅ Existing tests pass (no regressions)
- ✅ Explanation format compatible with CP1 format

**Verification**:
- ✅ Legacy policy tests pass
- ✅ Backward compatibility tests pass
- ✅ No breaking changes to existing APIs

#### 5. Performance Validated ✅

**Required Checks**:
- ✅ Circuit breaker check adds < 1ms latency (ETS lookup)
- ✅ State updates don't block request path
- ✅ No memory leaks (circuit breaker state cleanup)
- ✅ No process leaks (circuit breaker processes)

**Verification**:
- ✅ Performance tests pass
- ✅ Latency measurements within targets
- ✅ Memory stability confirmed

## Module Order (Implementation Sequence)

### 1. State Machine → Parsing → Integration → Tests → Observability

**Recommended Order**:

1. **State Machine** (`router_circuit_breaker.erl`)
   - Foundation for all other components
   - Can be tested independently
   - No dependencies

2. **Parsing** (`router_policy_store.erl`)
   - Can be done in parallel with state machine
   - Needed for integration
   - Validates configuration

3. **Schema** (`policy.schema.json`, fixtures)
   - Can be done in parallel with state machine
   - Needed for validation
   - Provides test data

4. **Metrics** (`router_circuit_breaker.erl` extend)
   - Depends on state machine
   - Needed for observability
   - Validates state tracking

5. **Integration** (`router_policy_applier.erl`)
   - Depends on state machine and metrics
   - Core functionality
   - Most complex integration

6. **Tests** (all test suites)
   - Depends on all modules
   - Validates correctness
   - Ensures quality

7. **Observability** (`router_audit.erl`)
   - Depends on integration
   - Logging and metrics
   - Operational visibility

8. **Documentation** (all docs)
   - Depends on all implementation
   - Final step
   - User-facing documentation

## Quick Reference Checklist

### Phase 1: Foundation
- [ ] Task 1: State Machine Module (3-4 days)
- [ ] Task 4: Configuration Parsing (1-2 days)
- [ ] Task 5: Schema and Fixtures (1-2 days)

### Phase 2: Integration
- [ ] Task 2: Metrics Collection (2-3 days)
- [ ] Task 3: Policy Applier Integration (3-4 days)

### Phase 3: Observability and Documentation
- [ ] Task 6: Observability (1-2 days)
- [ ] Task 7: Documentation (1 day)

### Feature Readiness
- [ ] All test suites pass
- [ ] All metrics emitted
- [ ] All documentation updated
- [ ] Backward compatibility verified
- [ ] Performance validated

## References

- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Complete circuit breaker design
- `docs/archive/dev/CP2_POLICY_IMPLEMENTATION_TASKS.md` - Detailed task breakdown
- `docs/archive/dev/CP2_ROUTER_PLAN.md` - CP2 Router plan
- `docs/ROUTING_POLICY.md` - Routing policy specification
- `docs/OBSERVABILITY_CONVENTIONS.md` - Logging conventions

## Change History

**v1.0 (2025-01-27)**:
- Feature comparison (Circuit Breaker vs Rate Limit)
- Circuit Breaker selected as first CP2 feature
- Detailed implementation checklist
- Feature readiness criteria
- Module order and timeline

