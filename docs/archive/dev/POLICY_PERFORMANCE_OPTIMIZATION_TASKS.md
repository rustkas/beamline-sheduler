# Policy Performance Optimization Tasks

## Purpose

This document defines performance optimization tasks identified from load test results.

## Status

📅 **DEFERRED** - Optimization tasks defined, **explicitly deferred** until production signals indicate need

**Current Decision**: Focus on **CB-specific optimizations** (see `CIRCUIT_BREAKER_OPTIMIZATION_PRIORITIES.md`). General performance optimizations (this document) are **low-priority** and can wait until production monitoring shows bottlenecks.

## Date

**2025-01-27**

## Summary

Based on load test results for Scenarios 1 and 2, identified 6 optimization opportunities:
- **2 Medium Priority**: Concurrent load generation for high QPS
- **4 Low Priority**: ETS optimizations, parsing cache, validation optimization

**Current Status**: **All optimizations DEFERRED** - Current performance meets all targets. These optimizations are **optional improvements** that can wait until production signals indicate bottlenecks.

**See Also**: `docs/archive/dev/CIRCUIT_BREAKER_OPTIMIZATION_PRIORITIES.md` for **High Priority** CB-specific optimizations that are ready for implementation.

## Optimization Tasks

### OPT-1: Concurrent Load Generation for High QPS

**Priority**: Medium

**Status**: 📅 Planned

**Description**: 
Implement concurrent load generation to achieve exact target QPS for 5k/10k scenarios. Current sequential generation cannot achieve exact target QPS for high loads.

**Current Behavior**:
- Sequential request generation with interval control
- System processes requests faster than interval allows
- Cannot generate concurrent requests at exact target QPS
- **1k QPS**: Can achieve 50-120% of target (acceptable)
- **5k QPS**: Achieves ~8-10% of target (sequential limitation)
- **10k QPS**: Achieves ~2-5% of target (sequential limitation)

**Target Behavior**:
- Concurrent request generation using worker pool
- Distribute target QPS across workers
- Achieve exact target QPS (±5%) for 5k/10k scenarios
- Maintain latency validation (primary metric)

**Implementation Tasks**:
1. Create concurrent load generation function
2. Use `spawn_opt/3` or similar for concurrent workers
3. Implement rate limiting per worker
4. Update test cases to use concurrent generation
5. Validate throughput matches target QPS (±5%)

**Estimated Impact**: 
- Enable true throughput testing for 5k/10k QPS scenarios
- Validate Router can handle high concurrent load

**Dependencies**: None

**Files to Modify**:
- `apps/otp/router/test/router_policy_applier_load_SUITE.erl`

**Test Cases to Update**:
- `test_load_5k_qps/1`
- `test_load_10k_qps/1`

**Success Criteria**:
- Actual QPS within ±5% of target for 5k/10k scenarios
- Latency targets still met (P95 < 10ms, P99 < 50ms)
- No increase in error rate

**Estimated Effort**: 1-2 days

---

### OPT-2: Concurrent Workers for Load Generation

**Priority**: Medium

**Status**: 📅 Planned

**Description**: 
Use concurrent workers instead of sequential generation for high QPS. This is a refinement of OPT-1.

**Implementation Tasks**:
1. Implement worker pool for load generation
2. Distribute target QPS across workers (e.g., 10 workers for 5k QPS = 500 QPS per worker)
3. Collect results from all workers
4. Aggregate statistics (latency, throughput, errors)
5. Handle worker failures gracefully

**Estimated Impact**: 
- Achieve exact target QPS for 5k/10k scenarios
- Better resource utilization
- More realistic load testing

**Dependencies**: OPT-1 (can be combined)

**Files to Modify**:
- `apps/otp/router/test/router_policy_applier_load_SUITE.erl`

**Success Criteria**:
- Worker pool successfully generates target QPS
- Results aggregated correctly
- No worker leaks or failures

**Estimated Effort**: 1 day (if combined with OPT-1)

---

### OPT-3: ETS Table Structure Optimization

**Priority**: Low

**Status**: 📅 Planned

**Description**: 
Optimize ETS table structure if performance degrades with larger stores (10k+ policies).

**Current Performance**:
- Lookup latency: < 5ms P95 (excellent)
- Load time: < 1s per 100 policies (meets target)
- Memory: < 1MB per policy (efficient)
- **Verified up to**: 5000 policies

**Potential Issues**:
- Performance may degrade with 10k+ policies
- ETS table type (set vs ordered_set) may not be optimal
- Key structure may not be optimal for access patterns

**Implementation Tasks**:
1. Analyze ETS table access patterns
2. Profile lookup operations for 10k+ policies
3. Consider table type (set vs ordered_set vs bag)
4. Optimize key structure (if needed)
5. Measure impact on lookup latency

**Estimated Impact**: 
- Potential improvement in lookup latency for very large stores (10k+ policies)
- Better memory efficiency (if structure optimized)

**Dependencies**: None (can be done independently)

**Files to Modify**:
- `apps/otp/router/src/router_policy_store.erl`

**Success Criteria**:
- Lookup latency remains < 5ms P95 for 10k+ policies
- Memory usage remains < 1MB per policy
- No regression in existing performance

**Estimated Effort**: 2-3 days

**When to Implement**: 
- If performance degrades with 10k+ policies
- If memory usage becomes concern

---

### OPT-4: ETS Table Compression

**Priority**: Low

**Status**: 📅 Planned

**Description**: 
Add ETS table compression if memory usage becomes concern.

**Current Performance**:
- Memory: < 1MB per policy (efficient)
- **Verified up to**: 5000 policies (~5GB total)

**Potential Issues**:
- Memory usage may become concern with 10k+ policies
- Compression may reduce memory usage

**Implementation Tasks**:
1. Enable ETS table compression (`{compressed, true}`)
2. Measure memory reduction
3. Measure performance impact (if any)
4. Document trade-offs (memory vs CPU)
5. Make compression configurable

**Estimated Impact**: 
- Reduce memory usage (if needed)
- Potential CPU overhead (compression/decompression)

**Dependencies**: None

**Files to Modify**:
- `apps/otp/router/src/router_policy_store.erl`

**Success Criteria**:
- Memory usage reduced (if compression effective)
- Performance impact acceptable (< 10% latency increase)
- Configurable (can be enabled/disabled)

**Estimated Effort**: 1 day

**When to Implement**: 
- If memory usage becomes concern (10k+ policies)
- If memory constraints are tight

---

### OPT-5: Policy Parsing Cache

**Priority**: Low

**Status**: 📅 Planned

**Description**: 
Cache parsed policies if parsing becomes bottleneck.

**Current Performance**:
- Load time: < 1s per 100 policies (meets target)
- No significant parsing overhead observed

**Potential Issues**:
- Parsing may become slow with complex policies
- Repeated parsing of same policy (if policy reloaded)

**Implementation Tasks**:
1. Implement parsing cache (ETS table or process dictionary)
2. Cache parsed policies by `{tenant_id, policy_id}`
3. Invalidate cache on policy updates
4. Measure performance impact
5. Handle cache size limits (LRU eviction if needed)

**Estimated Impact**: 
- Reduce parsing overhead (if parsing becomes slow)
- Faster policy reloads (if policies reloaded frequently)

**Dependencies**: None

**Files to Modify**:
- `apps/otp/router/src/router_policy_store.erl`

**Success Criteria**:
- Parsing overhead reduced (if applicable)
- Cache invalidation works correctly
- No memory leaks from cache

**Estimated Effort**: 2-3 days

**When to Implement**: 
- If parsing becomes bottleneck
- If policies are reloaded frequently

---

### OPT-6: Policy Validation Optimization

**Priority**: Low

**Status**: 📅 Planned

**Description**: 
Optimize policy validation if validation becomes slow.

**Current Performance**:
- Load time: < 1s per 100 policies (meets target)
- No significant validation overhead observed

**Potential Issues**:
- Validation may become slow with complex policies
- Validation may be redundant (if policy already validated)

**Implementation Tasks**:
1. Profile policy validation code
2. Identify slow validation steps
3. Optimize validation logic (early exits, caching)
4. Measure performance impact
5. Ensure validation correctness (no regressions)

**Estimated Impact**: 
- Reduce validation overhead (if validation becomes slow)
- Faster policy loading (if validation is bottleneck)

**Dependencies**: None

**Files to Modify**:
- `apps/otp/router/src/router_policy_store.erl`
- `apps/otp/router/src/router_policy_validator.erl`

**Success Criteria**:
- Validation overhead reduced (if applicable)
- Validation correctness maintained (no regressions)
- No increase in invalid policies accepted

**Estimated Effort**: 2-3 days

**When to Implement**: 
- If validation becomes bottleneck
- If validation is slow for complex policies

## Priority Summary

### Medium Priority (Implement if needed)

1. **OPT-1**: Concurrent Load Generation for High QPS
   - **When**: If true throughput testing needed for 5k/10k QPS
   - **Impact**: Enable accurate throughput validation

2. **OPT-2**: Concurrent Workers for Load Generation
   - **When**: If combined with OPT-1
   - **Impact**: Better resource utilization

### Low Priority (Implement only if performance degrades)

3. **OPT-3**: ETS Table Structure Optimization
   - **When**: If performance degrades with 10k+ policies
   - **Impact**: Maintain performance at scale

4. **OPT-4**: ETS Table Compression
   - **When**: If memory usage becomes concern
   - **Impact**: Reduce memory usage

5. **OPT-5**: Policy Parsing Cache
   - **When**: If parsing becomes bottleneck
   - **Impact**: Reduce parsing overhead

6. **OPT-6**: Policy Validation Optimization
   - **When**: If validation becomes slow
   - **Impact**: Reduce validation overhead

## Implementation Order

### Immediate (Not Required)

**None** - Current performance meets all targets. Optimizations are optional improvements.

**Decision**: **Low-noise approach** - Don't optimize until production signals indicate bottlenecks.

### If Needed (Based on Future Requirements / Production Signals)

**Wait for production monitoring to indicate need before implementing:**

1. **OPT-1 + OPT-2**: Concurrent load generation
   - **Trigger**: If true throughput testing needed for 5k/10k QPS
   - **Current**: 1k QPS testing sufficient, higher QPS can wait

2. **OPT-3**: ETS optimization
   - **Trigger**: If performance degrades with 10k+ policies
   - **Current**: Performance meets targets up to 5k policies

3. **OPT-4**: ETS compression
   - **Trigger**: If memory becomes concern
   - **Current**: Memory usage is efficient

4. **OPT-5**: Parsing cache
   - **Trigger**: If parsing becomes bottleneck
   - **Current**: No parsing bottleneck observed

5. **OPT-6**: Validation optimization
   - **Trigger**: If validation becomes slow
   - **Current**: No validation bottleneck observed

**Note**: For **CB-specific optimizations** that are production-ready, see `docs/archive/dev/CIRCUIT_BREAKER_OPTIMIZATION_PRIORITIES.md` (CB-OPT-1, CB-OPT-2).

## References

- `docs/archive/dev/POLICY_PERFORMANCE_PLAN.md` - Performance testing plan
- `docs/archive/dev/POLICY_PERFORMANCE_IMPLEMENTATION_PLAN.md` - Implementation plan with results
- `docs/archive/dev/POLICY_APPLIER_LOAD_TEST_RESULTS.md` - Scenario 1 test results
- `docs/archive/dev/POLICY_STORE_LOAD_TEST_RESULTS.md` - Scenario 2 test results

## Change History

**v1.0 (2025-01-27)**:
- Initial optimization tasks defined
- 6 optimization opportunities identified
- Priority classification (Medium: 2, Low: 4)
- Implementation guidance provided

