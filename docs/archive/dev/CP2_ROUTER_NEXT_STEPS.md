# CP2 Router Next Steps

## Purpose

This document defines the remaining tasks for Circuit Breaker and Router Observability after the initial implementation is complete.

## Status

📋 **PLANNED** - Tasks identified, ready for implementation when needed

## Date

**2025-01-27**

## Summary

After completing Circuit Breaker foundation, integration, and Router OTel spans, **4 focused tasks** remain:

1. **CB Provider Callbacks Integration** (High Priority) - Complete CB cycle with real provider outcomes
2. **Router Observability Expansion** - ✅ COMPLETE (no further action needed)
3. **CB-OPT-1: Error-Rate Window & Timeout** (High Priority) - Production readiness optimization
4. **CB-OPT-2: Enhanced Metrics/Logging** (High Priority) - Production observability

## Completed Work

### Circuit Breaker (CP2 / wrk-1) ✅ COMPLETE

- ✅ **Foundation**: `router_circuit_breaker.erl`, `#policy{}` field, parsing, schema+fixtures
- ✅ **Integration**: CB check in `router_decider`, initialization in `router_policy_applier`, integration tests, metrics, docs
- ✅ **Documentation/Observability**: `ROUTING_POLICY.md`, `CIRCUIT_BREAKER_DESIGN.md`, `CIRCUIT_BREAKER_OBSERVABILITY.md`, alerts in `PROMETHEUS_ALERTS.md`, `CP2_CHECKLIST.md` updated
- ✅ **Optimization Priorities**: CB-OPT-1/2 identified as high-priority, general perf optimizations deferred

### Router Observability (CP2 / wrk-3) ✅ COMPLETE

- ✅ **OTel Spans**: 3 key spans implemented:
  - `beamline.router.route` (in `router_core.erl`)
  - `beamline.router.policy.apply` (in `router_policy_applier.erl`)
  - `beamline.router.provider.select` (in `router_decider.erl`)
- ✅ **Trace Context**: Context extraction and propagation, attributes (tenant_id, policy_id, etc.), status handling
- ✅ **Tests**: Updated `router_observability_SUITE.erl`
- ✅ **Documentation**: Synced with `CP2_OBSERVABILITY_BACKLOG.md` and `CP2_CHECKLIST.md`

## Remaining Tasks

### Task 1: CP2 / wrk-1 - CB Provider Callbacks Integration

**Task ID**: `cp2-cb-provider-callbacks-wrk1`

**Priority**: **High**

**Status**: 📅 Planned

**Description**: Complete Circuit Breaker integration by recording success/failure outcomes from actual provider calls.

**Current State**:
- ✅ `router_circuit_breaker:record_success/2` and `record_failure/2` implemented
- ✅ CB check before provider selection (fail-fast when open)
- ❌ **Missing**: Recording success/failure after actual provider calls

**What to Do**:

1. **In `router_core.erl` or `router_decide_consumer.erl`**:
   - After successful provider call → `router_circuit_breaker:record_success(TenantId, ProviderId)`
   - After provider error (timeout, 5xx, connection_error, provider_unavailable) → `router_circuit_breaker:record_failure(TenantId, ProviderId)`
   - **Do NOT record**: 4xx, validation_error, rate_limit_exceeded (per design doc)

2. **Add Integration Tests**:
   - Test: N consecutive failures → CB opens
   - Test: After series of successes → CB returns to Closed (meets thresholds)
   - Test: Error type filtering (only CB-relevant errors recorded)

**Files to Modify**:
- `apps/otp/router/src/router_core.erl` (if provider calls happen here)
- `apps/otp/router/src/router_decide_consumer.erl` (if provider calls happen here)
- `apps/otp/router/test/router_circuit_breaker_integration_SUITE.erl` (add provider callback tests)

**Success Criteria**:
- ✅ Success/failure recorded after actual provider calls
- ✅ CB state transitions based on real outcomes
- ✅ Error type filtering works correctly
- ✅ Integration tests verify full CB cycle

**Estimated Effort**: 1-2 days

**When to Implement**: Before production deployment (if CB is critical path)

**Reference**: `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` (error types section)

---

### Task 2: CP2 / wrk-3 - Router Observability Expansion

**Task ID**: `cp2-router-obs-expansion-wrk3`

**Status**: ✅ **COMPLETE**

**Note**: No further action needed. OTel spans implementation is complete. Further observability work is described in Wave-1/Wave-2 docs.

---

### Task 3: CP2 / wrk-1 - CB-OPT-1: Error-Rate Window & Timeout Transitions

**Task ID**: `cp2-cb-opt1-error-window-wrk1`

**Priority**: **High** (Production Readiness)

**Status**: 📅 Planned

**Description**: Fine-tune error-rate window calculation and timeout-based state transitions for production readiness.

**What to Do**:

1. **Sliding Window for Error-Rate**:
   - Implement time-based sliding window (e.g., last 60 seconds)
   - Exclude old failures outside window
   - More accurate error-rate calculation

2. **Configurable Window Size**:
   - Allow per-provider or per-policy window configuration
   - Support different window sizes for different provider types
   - Default: 60 seconds (backward compatible)

3. **Adaptive Timeout Transitions**:
   - Shorter timeout for transient failures (e.g., 30 seconds)
   - Longer timeout for persistent failures (e.g., 120 seconds)
   - Exponential backoff for repeated failures

4. **Timeout-Based Transition Optimization**:
   - Check timeout transitions more efficiently (not on every request)
   - Use timer-based checks for timeout transitions
   - Reduce gen_server call overhead

**Files to Modify**:
- `apps/otp/router/src/router_circuit_breaker.erl` (sliding window, adaptive timeout)
- `apps/otp/router/src/router_policy_store.erl` (parse window/timeout config)
- `apps/otp/router/docs/schemas/policy.schema.json` (add window/timeout fields)
- `docs/ROUTING_POLICY.md` (document new options)

**Test Cases to Add**:
- Sliding window accuracy tests
- Adaptive timeout transition tests
- Configurable window size tests
- Timeout optimization performance tests

**Success Criteria**:
- ✅ Sliding window accurately reflects recent error rate
- ✅ Adaptive timeout reduces recovery time for transient failures
- ✅ Configuration options work correctly
- ✅ No regression in existing CB behavior
- ✅ Performance impact < 5% (timeout checks)

**Estimated Effort**: 2-3 days

**When to Implement**: Before production deployment (if CB is critical path)

**Reference**: `docs/archive/dev/CIRCUIT_BREAKER_OPTIMIZATION_PRIORITIES.md` (CB-OPT-1 section)

---

### Task 4: CP2 / wrk-1 - CB-OPT-2: Enhanced Metrics/Logging

**Task ID**: `cp2-cb-opt2-metrics-logging-wrk1`

**Priority**: **High** (Production Observability)

**Status**: 📅 Planned

**Description**: Add additional metrics and structured logging for complex production scenarios to enable better observability and debugging.

**What to Do**:

1. **Error-Rate Metrics**:
   - `router_circuit_breaker_error_rate` (gauge): Current error rate per provider
   - `router_circuit_breaker_window_requests_total` (counter): Requests in current window
   - `router_circuit_breaker_window_failures_total` (counter): Failures in current window

2. **Timeout Tracking Metrics**:
   - `router_circuit_breaker_timeout_remaining_ms` (gauge): Time until next timeout transition
   - `router_circuit_breaker_timeout_transitions_total` (counter): Timeout-based transitions

3. **Enhanced Logging**:
   - Log error-rate value when CB opens/closes
   - Log window statistics (requests, failures, window size)
   - Log timeout remaining when CB is open
   - Log failure patterns (error types, timing)

4. **Debug Context in Logs**:
   - Include full CB state in transition logs
   - Include configuration values (thresholds, timeouts, windows)
   - Include recent failure history (last N failures)

**Files to Modify**:
- `apps/otp/router/src/router_circuit_breaker.erl` (enhanced logging)
- `apps/otp/router/src/router_metrics.erl` (new metrics)
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` (alert rules)
- `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` (documentation)

**Test Cases to Add**:
- Metrics export tests
- Enhanced logging format tests
- Debug context validation tests

**Success Criteria**:
- ✅ Error-rate metrics exported correctly
- ✅ Timeout tracking metrics accurate
- ✅ Enhanced logs include all required context
- ✅ No performance impact from additional logging (< 1% overhead)
- ✅ Alert rules work correctly

**Estimated Effort**: 1-2 days

**When to Implement**: Before production deployment (if CB observability is critical) or if production debugging shows need for more context

**Reference**: `docs/archive/dev/CIRCUIT_BREAKER_OPTIMIZATION_PRIORITIES.md` (CB-OPT-2 section)

---

## Implementation Order

### Immediate (Before Production)

1. **Task 1**: CB Provider Callbacks Integration
   - **Rationale**: Completes CB cycle with real outcomes
   - **Impact**: CB actually works end-to-end

2. **Task 3**: CB-OPT-1 (Error-Rate Window & Timeout)
   - **Rationale**: Production-ready CB behavior
   - **Impact**: Better accuracy and recovery time

3. **Task 4**: CB-OPT-2 (Enhanced Metrics/Logging)
   - **Rationale**: Production observability
   - **Impact**: Better debugging and monitoring

### Deferred (Wait for Production Signals)

- **Task 2**: Router Observability - ✅ Already complete, no further action

## Priority Summary

### High Priority (Before Production)

1. **Task 1**: CB Provider Callbacks Integration (1-2 days)
2. **Task 3**: CB-OPT-1 Error-Rate Window & Timeout (2-3 days)
3. **Task 4**: CB-OPT-2 Enhanced Metrics/Logging (1-2 days)

### Complete

- **Task 2**: Router Observability Expansion - ✅ COMPLETE

## References

- `docs/CP2_CHECKLIST.md` - Overall CP2 progress
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Circuit Breaker design
- `docs/archive/dev/CIRCUIT_BREAKER_OPTIMIZATION_PRIORITIES.md` - CB optimization priorities
- `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` - CB observability specification
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - Observability backlog

## Change History

**v1.0 (2025-01-27)**:
- Initial next steps document created
- 4 tasks identified (1 high-priority integration, 1 complete, 2 production-readiness optimizations)
- Implementation order and rationale provided

