# CP2 Worker Reliability Wave 1

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-3 (Worker Reliability)  
**Status**: 📋 **WAVE 1 SPECIFICATION** (CP2)

---

## Executive Summary

This document formally defines **Wave 1** for CP2 Worker Reliability, focusing on **P1 Critical** production blockers that must be addressed before production deployment.

**Wave 1 Goal**: Address critical production blockers that can cause system crashes, memory exhaustion, thread exhaustion, and poor user experience.

**Reference**: `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` - Complete backlog with all tickets and priorities

**Dry-Run Plan**: `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1_DRYRUN.md` - Dry-run scenario for Wave 1 (configuration and feature flag level, no code changes)

---

## Wave 1 Scope

### Tickets Included

**Wave 1 consists of 9 tickets (all P1 Critical)**:

| Ticket ID | Title | Estimate | Priority | Business Risk |
|-----------|-------|----------|----------|---------------|
| **W3-1.1** | Exponential Backoff Implementation | 1d | P1 | Fixed backoff causes thundering herd, wastes resources |
| **W3-1.3** | Error Classification | 1.5d | P1 | Retrying non-retryable errors wastes resources, increases latency |
| **W3-1.4** | Retry Budget Management | 1d | P1 | Retries can exceed user timeout expectations |
| **W3-2.1** | FS Operation Timeouts | 1d | P1 | FS operations can block indefinitely, causing thread exhaustion |
| **W3-2.2** | HTTP Connection Timeout | 0.5d | P1 | Connection attempts can hang indefinitely |
| **W3-2.3** | Total Timeout Across Retries | 1.5d | P1 | Missing total timeout causes unpredictable behavior |
| **W3-4.1** | Bounded Queue Implementation | 1d | P1 | Unbounded queue growth can cause memory exhaustion |
| **W3-4.2** | Queue Depth Monitoring | 1d | P1 | Cannot detect queue growth before memory exhaustion |
| **W3-4.3** | Queue Rejection Handling | 1d | P1 | Router needs feedback for backpressure |

**Total Estimate**: ~9.5 days (~2 weeks)

### Feature Flags

**Wave 1 introduces 2 feature flags**:

1. **`CP2_ADVANCED_RETRY_ENABLED`** (default: `false`)
   - Gates: Exponential backoff, error classification, retry budget
   - Tickets: W3-1.1, W3-1.3, W3-1.4

2. **`CP2_COMPLETE_TIMEOUT_ENABLED`** (default: `false`)
   - Gates: FS timeouts, HTTP connection timeout, total timeout
   - Tickets: W3-2.1, W3-2.2, W3-2.3

3. **`CP2_QUEUE_MANAGEMENT_ENABLED`** (default: `false`)
   - Gates: Bounded queue, queue monitoring, queue rejection
   - Tickets: W3-4.1, W3-4.2, W3-4.3

**CRITICAL**: All feature flags default to `false` to preserve CP1 baseline behavior.

---

## Wave 1 Goal

### What Changes for Users

**Before Wave 1 (CP1)**:
- ❌ Fixed retry backoff (100ms, 200ms, 300ms) - inefficient, can cause thundering herd
- ❌ Retries all errors (including non-retryable 4xx HTTP errors) - wastes resources
- ❌ No timeout enforcement for FS operations - can block indefinitely
- ❌ Unbounded queue growth - risk of memory exhaustion
- ❌ No queue rejection - Router doesn't know when Worker is overloaded

**After Wave 1 (CP2)**:
- ✅ Exponential retry backoff (100ms, 200ms, 400ms, 800ms, ...) - efficient, prevents thundering herd
- ✅ Smart error classification - only retries retryable errors (network, 5xx HTTP)
- ✅ Complete timeout enforcement - FS operations, HTTP connections, total timeout across retries
- ✅ Bounded queue with rejection - prevents memory exhaustion
- ✅ Queue depth monitoring - enables proactive response to overload
- ✅ Queue rejection with feedback - Router receives overload signals

### What Changes for System

**Reliability Improvements**:
- **Memory Safety**: Bounded queue prevents memory exhaustion under high load
- **Thread Safety**: FS operation timeouts prevent thread exhaustion
- **Resource Efficiency**: Error classification reduces wasted retries
- **Predictability**: Total timeout enforcement ensures predictable behavior
- **Observability**: Queue depth monitoring enables proactive monitoring

**Performance Improvements**:
- **Faster Recovery**: Exponential backoff provides faster recovery from transient failures
- **Reduced Load**: Smart error classification reduces unnecessary retries
- **Better Resource Utilization**: Timeout enforcement prevents resource waste

**Operational Improvements**:
- **Backpressure Signaling**: Router can adjust routing based on Worker overload status
- **Proactive Monitoring**: Queue depth metrics enable early detection of issues
- **Graceful Degradation**: Queue rejection prevents system crashes

---

## Main Risks

### Risk 1: Memory Exhaustion (Critical)

**Current State (CP1)**:
- Unbounded queue growth under high load
- No queue size limits
- Risk of system crash due to memory exhaustion

**Wave 1 Mitigation**:
- ✅ Bounded queue with configurable `max_queue_size`
- ✅ Queue rejection when full
- ✅ Queue depth monitoring for early detection

**Business Impact**: **CRITICAL** - System crashes can cause production outages and data loss.

### Risk 2: Thread Exhaustion (Critical)

**Current State (CP1)**:
- FS operations can block indefinitely on slow disks
- No timeout enforcement for file I/O
- Risk of thread pool exhaustion

**Wave 1 Mitigation**:
- ✅ FS operation timeouts with thread-based timeout mechanism
- ✅ Configurable timeout per operation type (read, write, delete)

**Business Impact**: **CRITICAL** - Thread exhaustion can cause system unresponsiveness.

### Risk 3: Resource Waste (High)

**Current State (CP1)**:
- Retries all errors (including non-retryable 4xx HTTP errors)
- Fixed backoff causes thundering herd
- Wastes resources and increases latency

**Wave 1 Mitigation**:
- ✅ Error classification (retryable vs non-retryable)
- ✅ Exponential backoff (prevents thundering herd)

**Business Impact**: **HIGH** - Resource waste increases costs and degrades user experience.

### Risk 4: Poor User Experience (High)

**Current State (CP1)**:
- Missing timeout enforcement causes unpredictable behavior
- Retries can exceed user timeout expectations
- No feedback to Router about overload

**Wave 1 Mitigation**:
- ✅ Total timeout enforcement across retries
- ✅ HTTP connection timeout
- ✅ Queue rejection with overload signaling

**Business Impact**: **HIGH** - Poor user experience leads to customer dissatisfaction.

### Risk 5: Feature Flag Regression (Medium)

**Current State (CP1)**:
- CP1 baseline must be preserved
- Feature flags must work correctly

**Wave 1 Mitigation**:
- ✅ All CP2 features gated behind feature flags
- ✅ Feature flags default to `false` (CP1 baseline)
- ✅ Comprehensive tests for both CP1 and CP2 modes

**Business Impact**: **MEDIUM** - Feature flag regression can break existing deployments.

---

## Dependencies

### Dependencies on Router

**Required Changes** (Optional for Wave 1, Required for Wave 2):

1. **ExecAssignmentAck Processing**:
   - Router should process `overload_status` field in `ExecAssignmentAck` (optional, CP2+)
   - Router can ignore `overload_status` for CP1 compatibility
   - **Wave 1**: Router can ignore `overload_status` (backward compatible)

2. **Backpressure Protocol**:
   - Router should adjust routing based on Worker overload status (Wave 2)
   - **Wave 1**: Router continues normal routing (no changes required)

3. **Queue Rejection Handling**:
   - Router should handle `ExecAssignmentAck` with `status: "rejected"` and `reason: "queue_full"`
   - **Wave 1**: Router must handle rejection (basic requirement, already supported in CP1)

**Router Changes Required for Wave 1**: **NONE** (all changes are backward compatible)

**Router Changes Required for Wave 2**: Backpressure protocol implementation (see `docs/archive/dev/CP2_WORKER_BACKPRESSURE_DESIGN.md`)

### Dependencies on Gateway

**Required Changes**: **NONE**

**Rationale**:
- Gateway → Router → Worker (Gateway doesn't directly interact with Worker)
- Wave 1 changes are internal to Worker
- No Gateway changes required

### Dependencies on Infrastructure

**Required Infrastructure**:

1. **NATS**:
   - NATS must be available for `ExecAssignment` and `ExecResult` messages
   - JetStream recommended for durable subscriptions (CP1 requirement)
   - **Wave 1**: No new NATS requirements

2. **Configuration**:
   - Feature flags can be set via environment variables or config files
   - **Wave 1**: Basic configuration support required

3. **Monitoring**:
   - Queue depth metrics should be exposed (for monitoring)
   - **Wave 1**: Basic metrics exposure required

**Infrastructure Changes Required**: **NONE** (all infrastructure requirements met in CP1)

### Dependencies on Other Components

**Required Changes**: **NONE**

**Rationale**:
- Wave 1 changes are internal to Worker
- All changes are backward compatible (feature flags default to `false`)
- No breaking changes to contracts or APIs

---

## Implementation Order

### Recommended Sequence

**Week 1: Retry & Timeout Foundation**

1. **Day 1**: W3-1.1 (Exponential Backoff) - Foundation for retry improvements
2. **Day 2**: W3-1.3 (Error Classification) - Smart retry decisions
3. **Day 3**: W3-1.4 (Retry Budget) - Total timeout enforcement
4. **Day 4**: W3-2.1 (FS Timeouts) - Prevent thread exhaustion
5. **Day 5**: W3-2.2 (HTTP Connection Timeout) + W3-2.3 (Total Timeout) - Complete timeout enforcement

**Week 2: Queue Management**

6. **Day 6**: W3-4.1 (Bounded Queue) - Prevent memory exhaustion
7. **Day 7**: W3-4.2 (Queue Depth Monitoring) - Enable proactive monitoring
8. **Day 8**: W3-4.3 (Queue Rejection Handling) - Enable backpressure signaling
9. **Day 9**: Integration testing and validation
10. **Day 10**: Documentation and handoff

**Total**: ~9.5 days (~2 weeks)

---

## Acceptance Criteria

### Functional Requirements

**Retry Policies**:
- ✅ Exponential backoff implemented (`base * 2^attempt`)
- ✅ Error classification working (retryable vs non-retryable)
- ✅ Retry budget management working (total timeout across retries)
- ✅ All features gated behind `CP2_ADVANCED_RETRY_ENABLED`

**Timeout Enforcement**:
- ✅ FS operations have timeout enforcement
- ✅ HTTP connection timeout configured
- ✅ Total timeout across retries enforced
- ✅ All features gated behind `CP2_COMPLETE_TIMEOUT_ENABLED`

**Queue Management**:
- ✅ Bounded queue size limits implemented
- ✅ Queue rejection when full
- ✅ Queue depth monitoring working
- ✅ All features gated behind `CP2_QUEUE_MANAGEMENT_ENABLED`

### Non-Functional Requirements

**Reliability**:
- ✅ No memory exhaustion under high load (bounded queue)
- ✅ No thread exhaustion (FS timeouts)
- ✅ No crashes or errors when toggling feature flags

**Performance**:
- ✅ No performance regression (feature flag check is fast)
- ✅ Exponential backoff reduces retry load
- ✅ Error classification reduces wasted retries

**Observability**:
- ✅ Queue depth metrics exposed
- ✅ Retry metrics exposed (retries per error code)
- ✅ Timeout metrics exposed

### Test Coverage

**Unit Tests**:
- ✅ Exponential backoff calculation tests
- ✅ Error classification tests
- ✅ Retry budget calculation tests
- ✅ Timeout enforcement tests
- ✅ Queue management tests

**Integration Tests**:
- ✅ HTTP block retry with exponential backoff
- ✅ HTTP block error classification
- ✅ FS block timeout enforcement
- ✅ Queue rejection scenarios
- ✅ Feature flag toggle tests

---

## Success Metrics

### Before Wave 1 (CP1 Baseline)

- **Memory Usage**: Unbounded (can grow indefinitely)
- **Thread Usage**: Can block indefinitely (FS operations)
- **Retry Efficiency**: Low (retries all errors, fixed backoff)
- **User Experience**: Unpredictable (missing timeouts)
- **Observability**: Limited (no queue depth monitoring)

### After Wave 1 (CP2)

- **Memory Usage**: Bounded (max_queue_size limit)
- **Thread Usage**: Bounded (FS operation timeouts)
- **Retry Efficiency**: High (smart error classification, exponential backoff)
- **User Experience**: Predictable (complete timeout enforcement)
- **Observability**: Complete (queue depth monitoring, retry metrics)

### Key Metrics

| Metric | CP1 Baseline | Wave 1 Target | Measurement |
|--------|--------------|---------------|-------------|
| **Memory Safety** | ❌ Unbounded | ✅ Bounded | Queue depth < max_queue_size |
| **Thread Safety** | ❌ Can block | ✅ Timeout enforced | FS operations timeout < configured limit |
| **Retry Efficiency** | ⚠️ Low (retries all) | ✅ High (smart retry) | Retry success rate, retry count per error type |
| **Timeout Compliance** | ⚠️ Partial | ✅ Complete | All operations respect timeout |
| **Observability** | ⚠️ Limited | ✅ Complete | Queue depth metrics, retry metrics |

---

## Rollout Strategy

### Phase 1: Feature Flag Enablement (Week 1)

**Goal**: Enable feature flags in development/staging environment.

**Steps**:
1. Deploy Worker with Wave 1 code (feature flags disabled by default)
2. Enable feature flags in staging: `CP2_ADVANCED_RETRY_ENABLED=true`, `CP2_COMPLETE_TIMEOUT_ENABLED=true`, `CP2_QUEUE_MANAGEMENT_ENABLED=true`
3. Run integration tests
4. Monitor metrics (queue depth, retry success rate, timeout compliance)

**Success Criteria**:
- ✅ All tests pass
- ✅ No crashes or errors
- ✅ Metrics show improvement (retry efficiency, timeout compliance)

### Phase 2: Production Rollout (Week 2)

**Goal**: Enable feature flags in production with gradual rollout.

**Steps**:
1. Enable feature flags for 10% of traffic (canary deployment)
2. Monitor metrics for 24 hours
3. If successful, increase to 50% of traffic
4. Monitor metrics for 24 hours
5. If successful, enable for 100% of traffic

**Success Criteria**:
- ✅ No increase in error rate
- ✅ Improved retry efficiency
- ✅ Improved timeout compliance
- ✅ No memory or thread exhaustion

### Phase 3: Validation (Week 3)

**Goal**: Validate Wave 1 success and prepare for Wave 2.

**Steps**:
1. Collect metrics for 1 week
2. Validate success metrics (memory safety, thread safety, retry efficiency)
3. Document lessons learned
4. Prepare for Wave 2 implementation

**Success Criteria**:
- ✅ All success metrics met
- ✅ No production incidents
- ✅ Ready for Wave 2

---

## Risks and Mitigations

### Risk 1: Feature Flag Regression

**Risk**: Feature flag doesn't work correctly, CP1 baseline broken.

**Mitigation**:
- Comprehensive tests for both CP1 and CP2 modes
- Feature flag defaults to `false` (CP1 baseline)
- Can disable feature flags if issues found

### Risk 2: Performance Regression

**Risk**: Feature flag check adds overhead, performance degrades.

**Mitigation**:
- Feature flag check is fast (environment variable or config lookup)
- Performance benchmarks before and after
- Can disable feature flags if performance issues

### Risk 3: Configuration Errors

**Risk**: Incorrect configuration causes unexpected behavior.

**Mitigation**:
- Default configuration values (safe defaults)
- Configuration validation
- Documentation with examples

### Risk 4: Integration Issues

**Risk**: Router doesn't handle queue rejection correctly.

**Mitigation**:
- Backward compatible (Router can ignore `overload_status`)
- Router already handles `status: "rejected"` in CP1
- Integration tests with Router

---

## References

### Primary Documents
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` - Complete backlog with all tickets and priorities
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/CP2_WORKER_RETRY_DESIGN.md` - Retry policy design
- `docs/archive/dev/CP2_WORKER_BACKPRESSURE_DESIGN.md` - Backpressure protocol design

### Implementation Files
- `apps/caf/processor/src/worker_actor.cpp` - Retry logic implementation
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP block implementation
- `apps/caf/processor/src/blocks/fs_block.cpp` - FS block implementation
- `apps/caf/processor/src/runtime/actor_pools.hpp` - Queue management implementation

### Contracts
- `apps/otp/router/docs/API_CONTRACTS.md` - ExecResult and ExecAssignmentAck contracts
- `apps/otp/router/docs/NATS_SUBJECTS.md` - NATS subjects and message flow

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Worker Reliability Wave 1 specification
- Wave 1 tickets defined (9 tickets, P1 Critical)
- Goal, risks, and dependencies documented
- Implementation order and acceptance criteria specified

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Wave 1 Specification Ready for Implementation

