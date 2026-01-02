# Circuit Breaker Optimization Priorities

## Purpose

This document prioritizes Circuit Breaker (CB) optimization tasks based on production readiness and real-world usage patterns. Focus is on **high-value, low-noise** optimizations that improve CB behavior in production scenarios.

## Status

✅ **PRIORITIZED** - High-priority CB optimizations identified, ready for implementation when needed

**Current Focus**: **High-value, low-noise** optimizations for Circuit Breaker production readiness

## Date

**2025-01-27**

## Summary

Based on current Circuit Breaker implementation and production readiness requirements, identified **2 High Priority** optimizations for CB path:

1. **CB-OPT-1**: Fine-tune error-rate window and timeout transitions (High Priority, 2-3 days)
2. **CB-OPT-2**: Additional metrics/logging for complex production cases (High Priority, 1-2 days)

**Deferred** (Low/Medium Priority - Wait for Production Signals):
- **Parallel load generation** (OPT-1, OPT-2 from POLICY_PERFORMANCE_OPTIMIZATION_TASKS.md)
  - **Rationale**: Current performance meets targets, no need for concurrent load generation yet
  - **When**: If true throughput testing needed for 5k/10k QPS
- **ETS micro-optimizations** (OPT-3, OPT-4, OPT-5, OPT-6 from POLICY_PERFORMANCE_OPTIMIZATION_TASKS.md)
  - **Rationale**: Current performance meets targets, no bottlenecks observed
  - **When**: If performance degrades with 10k+ policies or memory becomes concern

**Decision**: Focus only on **CB-specific optimizations** that improve production behavior. General performance optimizations can wait until real production signals indicate they are needed.

## High Priority Optimizations

### CB-OPT-1: Fine-Tune Error-Rate Window and Timeout Transitions

**Priority**: **High**

**Status**: 📅 Planned

**Description**: 
Fine-tune Circuit Breaker error-rate window calculation and timeout-based state transitions to improve responsiveness and accuracy in production scenarios.

**Current Implementation**:
- Error-rate window: Fixed 60-second window (`DEFAULT_ERROR_RATE_WINDOW_SECONDS = 60`)
- Error-rate calculation: Simple `total_failures / total_requests` over window
- Timeout transitions: Fixed `timeout_ms` (default 60000ms) for `open → half_open`
- Window management: Simple counter-based (no sliding window)

**Production Issues**:
- **Fixed window may miss rapid recovery**: 60-second window may be too long for fast-recovering providers
- **No sliding window**: Fixed window may include old failures that are no longer relevant
- **Timeout transitions may be too slow**: 60-second timeout may delay recovery for transient issues
- **No adaptive timeout**: Timeout doesn't adapt based on provider behavior patterns

**Target Improvements**:
1. **Sliding window for error-rate**:
   - Implement time-based sliding window (e.g., last 60 seconds)
   - Exclude old failures outside window
   - More accurate error-rate calculation

2. **Configurable window size**:
   - Allow per-provider or per-policy window configuration
   - Support different window sizes for different provider types
   - Default: 60 seconds (backward compatible)

3. **Adaptive timeout transitions**:
   - Shorter timeout for transient failures (e.g., 30 seconds)
   - Longer timeout for persistent failures (e.g., 120 seconds)
   - Exponential backoff for repeated failures

4. **Timeout-based transition optimization**:
   - Check timeout transitions more efficiently (not on every request)
   - Use timer-based checks for timeout transitions
   - Reduce gen_server call overhead

**Implementation Tasks**:
1. Implement sliding window for error-rate calculation
2. Add configurable window size to CB configuration
3. Implement adaptive timeout logic
4. Optimize timeout transition checks (timer-based)
5. Add configuration options to policy DSL
6. Update unit and integration tests
7. Document new configuration options

**Estimated Impact**: 
- More accurate error-rate detection (sliding window)
- Faster recovery for transient failures (adaptive timeout)
- Better production behavior (configurable per provider/policy)

**Dependencies**: None

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
- Sliding window accurately reflects recent error rate
- Adaptive timeout reduces recovery time for transient failures
- Configuration options work correctly
- No regression in existing CB behavior
- Performance impact < 5% (timeout checks)

**Estimated Effort**: 2-3 days

**When to Implement**: 
- Before production deployment (if CB is critical path)
- If production signals show slow recovery or inaccurate error-rate detection

---

### CB-OPT-2: Additional Metrics/Logging for Complex Production Cases

**Priority**: **High**

**Status**: 📅 Planned

**Description**: 
Add additional metrics and structured logging for complex production scenarios to enable better observability and debugging of Circuit Breaker behavior.

**Current Implementation**:
- Basic metrics: `router_circuit_breaker_events_total`, `router_circuit_breaker_state_transitions_total`
- Basic logging: State transition events (opened, closed, half-opened)
- Limited context: Basic tenant_id, provider_id in logs

**Production Gaps**:
- **No error-rate visibility**: Cannot see current error-rate value in metrics/logs
- **No window statistics**: Cannot see window size, request count, failure count
- **No timeout tracking**: Cannot see when timeout will trigger next transition
- **No failure pattern analysis**: Cannot see failure types, timing patterns
- **Limited debugging context**: Hard to debug why CB opened/closed in production

**Target Improvements**:
1. **Error-rate metrics**:
   - `router_circuit_breaker_error_rate` (gauge): Current error rate per provider
   - `router_circuit_breaker_window_requests_total` (counter): Requests in current window
   - `router_circuit_breaker_window_failures_total` (counter): Failures in current window

2. **Timeout tracking metrics**:
   - `router_circuit_breaker_timeout_remaining_ms` (gauge): Time until next timeout transition
   - `router_circuit_breaker_timeout_transitions_total` (counter): Timeout-based transitions

3. **Enhanced logging**:
   - Log error-rate value when CB opens/closes
   - Log window statistics (requests, failures, window size)
   - Log timeout remaining when CB is open
   - Log failure patterns (error types, timing)

4. **Debug context in logs**:
   - Include full CB state in transition logs
   - Include configuration values (thresholds, timeouts, windows)
   - Include recent failure history (last N failures)

**Implementation Tasks**:
1. Add error-rate metrics to `router_metrics.erl`
2. Add timeout tracking metrics
3. Enhance logging in `router_circuit_breaker.erl`
4. Add debug context to transition logs
5. Update observability documentation
6. Add Prometheus alert rules for new metrics

**Estimated Impact**: 
- Better visibility into CB behavior in production
- Easier debugging of CB issues
- Better alerting on CB state changes
- Data for optimizing CB configuration

**Dependencies**: None

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
- Error-rate metrics exported correctly
- Timeout tracking metrics accurate
- Enhanced logs include all required context
- No performance impact from additional logging (< 1% overhead)
- Alert rules work correctly

**Estimated Effort**: 1-2 days

**When to Implement**: 
- Before production deployment (if CB observability is critical)
- If production debugging shows need for more context

---

## Deferred Optimizations (Low/Medium Priority)

### From POLICY_PERFORMANCE_OPTIMIZATION_TASKS.md

The following optimizations are **explicitly deferred** until real production signals indicate they are needed. **No action required** until production monitoring shows bottlenecks.

#### Load Generation Optimizations (Medium Priority)

1. **OPT-1**: Concurrent Load Generation for High QPS
   - **When**: If true throughput testing needed for 5k/10k QPS
   - **Status**: Current sequential generation sufficient for 1k QPS testing
   - **Rationale**: 1k QPS tests pass, higher QPS testing can wait

2. **OPT-2**: Concurrent Workers for Load Generation
   - **When**: If combined with OPT-1
   - **Status**: Not needed until OPT-1 is implemented
   - **Rationale**: Depends on concurrent load generation

#### ETS and Parsing Optimizations (Low Priority)

3. **OPT-3**: ETS Table Structure Optimization
   - **When**: If performance degrades with 10k+ policies
   - **Status**: Current performance meets targets (tested up to 5k policies)
   - **Rationale**: No performance degradation observed, premature optimization

4. **OPT-4**: ETS Table Compression
   - **When**: If memory usage becomes concern
   - **Status**: Current memory usage is efficient
   - **Rationale**: No memory pressure signals

5. **OPT-5**: Policy Parsing Cache
   - **When**: If parsing becomes bottleneck
   - **Status**: No parsing bottleneck observed
   - **Rationale**: Parsing is fast, caching would add complexity without benefit

6. **OPT-6**: Policy Validation Optimization
   - **When**: If validation becomes slow
   - **Status**: No validation bottleneck observed
   - **Rationale**: Validation is fast, optimization not needed

**Decision**: These optimizations are **low-noise** - they don't add value until production signals indicate bottlenecks. Focus on CB-specific optimizations instead.

## Implementation Order

### Immediate (High Priority - Before Production)

1. **CB-OPT-1**: Fine-tune error-rate window and timeout transitions
   - **Rationale**: Improves CB accuracy and recovery time
   - **Impact**: Better production behavior

2. **CB-OPT-2**: Additional metrics/logging for complex production cases
   - **Rationale**: Enables production debugging and observability
   - **Impact**: Better visibility into CB behavior

### Deferred (Low/Medium Priority - Wait for Signals)

- **OPT-1, OPT-2**: Concurrent load generation (wait for load testing requirements)
- **OPT-3, OPT-4, OPT-5, OPT-6**: ETS and parsing optimizations (wait for performance signals)

## Priority Summary

### High Priority (Implement When CB is Production-Critical)

**Focus**: CB-specific optimizations that improve production behavior

1. **CB-OPT-1**: Fine-tune error-rate window and timeout transitions
   - **Effort**: 2-3 days
   - **Impact**: Better CB accuracy and recovery time
   - **When**: Before production deployment (if CB is critical path)
   - **Key Improvements**: Sliding window, adaptive timeout, configurable parameters

2. **CB-OPT-2**: Additional metrics/logging for complex production cases
   - **Effort**: 1-2 days
   - **Impact**: Better observability and debugging
   - **When**: Before production deployment (if CB observability is critical)
   - **Key Improvements**: Error-rate metrics, timeout tracking, enhanced logging context

### Low/Medium Priority (Explicitly Deferred - Wait for Signals)

**Focus**: General performance optimizations that are not needed yet

- **OPT-1, OPT-2**: Concurrent load generation (wait for load testing requirements)
- **OPT-3, OPT-4, OPT-5, OPT-6**: ETS and parsing optimizations (wait for performance signals)
- **Rationale**: Current performance meets all targets, no production bottlenecks observed
- **Decision**: **Low-noise approach** - don't optimize until signals indicate need

## References

- `docs/archive/dev/POLICY_PERFORMANCE_OPTIMIZATION_TASKS.md` - General performance optimizations
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Circuit Breaker design and implementation
- `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` - Circuit Breaker observability specification
- `apps/otp/router/src/router_circuit_breaker.erl` - Circuit Breaker implementation

## Change History

**v1.0 (2025-01-27)**:
- Initial CB optimization priorities defined
- 2 High Priority optimizations identified (CB-OPT-1, CB-OPT-2)
- Deferred general performance optimizations (OPT-1 through OPT-6)
- Implementation order and rationale provided

