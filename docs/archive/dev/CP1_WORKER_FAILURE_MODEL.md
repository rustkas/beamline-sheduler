# CP1 Worker Failure Model

**Date**: 2025-01-27  
**Checkpoint**: CP1-LC (Baseline Worker)  
**Purpose**: Explicit failure model specification for CP1 Worker to set stakeholder expectations

---

## Executive Summary

This document provides a **gap analysis** of Worker reliability implementation against CP1 requirements, explicitly stating:

- ✅ **Guaranteed failure scenarios** supported in CP1
- ❌ **Not guaranteed** in CP1 (deferred to CP2+)
- ⚠️ **Partial support** with known limitations

**Key Principle**: CP1 focuses on **stability and predictability** over feature completeness. Some failure handling is simplified or stubbed.

---

## 1. Failure Scenarios: Guaranteed Support (CP1)

### 1.1. Status Reporting

**✅ GUARANTEED**: All block executions return one of four status codes:

- `success` - Step completed successfully
- `error` - Step failed with error details
- `timeout` - Step exceeded configured timeout
- `cancelled` - Step was cancelled (by user or system)

**Implementation**: `StepResult` contract with `StepStatus` enum (`apps/caf/processor/include/beamline/worker/core.hpp`)

**Reference**: `apps/caf/processor/docs/ARCHITECTURE_ROLE.md#43-stepresult-contract-cp1-invariant`

### 1.2. HTTP Block Failures

**✅ GUARANTEED**:
- HTTP request timeout enforcement (`CURLOPT_TIMEOUT_MS`)
- Network error detection (connection refused, DNS failure)
- HTTP error status codes (4xx, 5xx) reported as `error` status
- Basic retry logic at executor actor level

**Implementation**: `apps/caf/processor/src/blocks/http_block.cpp`

**Limitations** (see Section 3):
- ⚠️ Connection timeout not fully configured (`CURLOPT_CONNECTTIMEOUT_MS` missing)
- ⚠️ No cancellation during in-flight HTTP requests
- ⚠️ Retries all errors (doesn't distinguish retryable vs non-retryable)

### 1.3. Filesystem Block Failures

**✅ GUARANTEED**:
- File operation error detection (permission denied, file not found)
- Path validation (security checks)
- Basic error reporting with `error` status

**Implementation**: `apps/caf/processor/src/blocks/fs_block.cpp`

**Limitations** (see Section 3):
- ❌ **No timeout enforcement** for FS operations (can block indefinitely)
- ❌ **No cancellation support** for long-running file operations
- ⚠️ Generic error classification (doesn't distinguish transient vs permanent)

### 1.4. Retry Logic (Basic)

**✅ GUARANTEED**:
- Configurable retry count (`StepRequest.retry_count`)
- Retry attempts tracked in `StepResult.retries_used`
- Basic backoff delay between retries

**Implementation**: `apps/caf/processor/src/worker_actor.cpp::execute_with_retry()`

**Limitations** (see Section 3):
- ⚠️ **Fixed backoff**: `100 * (attempt + 1)` ms (not exponential)
- ❌ **No jitter** (can cause thundering herd)
- ⚠️ **Retries all errors** (doesn't filter non-retryable errors)
- ❌ **No cancellation during retry wait**
- ❌ **No total timeout check** across all retries

### 1.5. Backpressure (Basic)

**✅ GUARANTEED**:
- Concurrent execution limits (`max_concurrency_` per resource pool)
- Queue-based task submission when at capacity
- Load tracking (`current_load_`)

**Implementation**: `apps/caf/processor/src/runtime/actor_pools.hpp`

**Limitations** (see Section 3):
- ❌ **Unbounded queue growth** (no `max_queue_size` limit)
- ⚠️ **Race condition** in load counting (not atomic)
- ❌ **No queue depth monitoring** or rejection when overloaded

---

## 2. Failure Scenarios: NOT Guaranteed in CP1

### 2.1. Advanced Retry Policies

**❌ NOT GUARANTEED**:
- Exponential backoff (currently fixed: `100 * (attempt + 1)` ms)
- Jitter to prevent thundering herd
- Per-error-type retry policies (retry network errors, not 4xx HTTP)
- Retry budget/timeout across all attempts
- Circuit breaker patterns

**Deferred to**: CP2+ (advanced retry policies)

### 2.2. Cancellation Support

**❌ NOT GUARANTEED**:
- Cancellation during in-flight HTTP requests
- Cancellation during FS operations
- Cancellation during retry wait periods
- Graceful cancellation with cleanup

**Current State**: `BaseBlockExecutor::cancel()` is a no-op

**Deferred to**: CP2+ (full cancellation support)

### 2.3. Timeout Enforcement (Complete)

**❌ NOT GUARANTEED**:
- FS operation timeouts (operations can block indefinitely)
- Connection timeout for HTTP (only total timeout configured)
- Total timeout across all retry attempts

**Current State**:
- HTTP: `CURLOPT_TIMEOUT_MS` set, but `CURLOPT_CONNECTTIMEOUT_MS` missing
- FS: No timeout mechanism

**Deferred to**: CP2+ (complete timeout enforcement)

### 2.4. Error Classification

**❌ NOT GUARANTEED**:
- Automatic distinction between retryable vs non-retryable errors
- Error code-based retry decisions
- Transient vs permanent error classification

**Current State**: All errors retried regardless of type

**Deferred to**: CP2+ (error classification and smart retry)

### 2.5. Queue Management

**❌ NOT GUARANTEED**:
- Bounded queue size limits
- Queue rejection when overloaded
- Queue depth monitoring and metrics
- Task priority scheduling

**Current State**: Unbounded queue growth risk

**Deferred to**: CP2+ (advanced queue management)

### 2.6. Circuit Breaker

**❌ NOT GUARANTEED**:
- Circuit breaker patterns for external services
- Automatic service degradation
- Health-based routing

**Deferred to**: CP2+ (resilience patterns)

---

## 3. Partial Support: Known Limitations

### 3.1. Retry Logic

**Current Implementation** (`worker_actor.cpp:220-241`):

```cpp
caf::expected<StepResult> ExecutorActorState::execute_with_retry(const StepRequest& req) {
    StepResult final_result;
    
    for (int attempt = 0; attempt <= req.retry_count; attempt++) {
        auto result = execute_single_attempt(req);
        if (result) {
            final_result = *result;
            final_result.retries_used = attempt;
            
            if (final_result.status == StepStatus::ok) {
                return final_result;
            }
        }
        
        // Fixed backoff: 100ms, 200ms, 300ms, ...
        if (attempt < req.retry_count) {
            std::this_thread::sleep_for(std::chrono::milliseconds(100 * (attempt + 1)));
        }
    }
    
    return final_result;
}
```

**Limitations**:
1. ⚠️ **Fixed backoff**: Should be exponential (`base * 2^attempt`)
2. ❌ **No jitter**: Can cause synchronized retries (thundering herd)
3. ⚠️ **Retries all errors**: Should only retry transient errors (network, timeout)
4. ❌ **No cancellation check**: Cannot cancel during retry wait
5. ❌ **No total timeout**: Doesn't check if total time across retries exceeds `req.timeout_ms`

**Reference**: `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md#4-executor-actor-retry-logic`

### 3.2. HTTP Block Timeout

**Current Implementation** (`http_block.cpp`):

- ✅ `CURLOPT_TIMEOUT_MS` set (total timeout)
- ❌ `CURLOPT_CONNECTTIMEOUT_MS` missing (connection timeout)

**Impact**: Connection attempts can hang indefinitely if DNS/connection fails

**Reference**: `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md#2-http-block`

### 3.3. FS Block Timeout

**Current Implementation** (`fs_block.cpp`):

- ❌ No timeout mechanism for file I/O operations
- ❌ Operations can block indefinitely on slow disks

**Impact**: Thread blocking risk for large file operations

**Reference**: `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md#1-filesystem-block`

### 3.4. Backpressure

**Current Implementation** (`actor_pools.hpp`):

- ✅ `max_concurrency_` limits concurrent execution
- ✅ Queue-based task submission
- ❌ Unbounded queue growth (no `max_queue_size`)
- ⚠️ Race condition in `current_load_` (not atomic)

**Impact**: Memory exhaustion risk under high load

**Reference**: `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md#3-runtime-management`

---

## 4. Gap Analysis: Requirements vs Implementation

### 4.1. ARCHITECTURE_ROLE.md Requirements

**Requirement**: "Retry Logic: Configurable retry attempts with exponential backoff"

**Status**: ⚠️ **PARTIAL**
- ✅ Retry attempts configurable
- ❌ Backoff is fixed (not exponential)
- ❌ No jitter

**Requirement**: "Timeout Enforcement: All operations have configurable timeouts"

**Status**: ⚠️ **PARTIAL**
- ✅ HTTP total timeout configured
- ❌ HTTP connection timeout missing
- ❌ FS operations have no timeout

**Requirement**: "Status Reporting: Clear status codes (success/error/timeout/cancelled)"

**Status**: ✅ **COMPLETE**
- All four status codes supported
- StepResult contract enforced

### 4.2. BLOCKS_RELIABILITY_REVIEW.md Findings

**Critical Issues** (High Priority):
1. ❌ Unbounded queue growth → **NOT FIXED in CP1**
2. ❌ No FS operation timeouts → **NOT FIXED in CP1**
3. ❌ No cancellation for FS/HTTP → **NOT FIXED in CP1**
4. ⚠️ Race condition in backpressure → **NOT FIXED in CP1**

**Medium Priority**:
5. ⚠️ Incomplete HTTP timeout → **NOT FIXED in CP1**
6. ⚠️ No error classification → **NOT FIXED in CP1**
7. ⚠️ Retry all errors → **NOT FIXED in CP1**
8. ⚠️ Fixed backoff → **NOT FIXED in CP1**

**Conclusion**: CP1 implements **basic** failure handling, but **advanced** reliability features are deferred to CP2+.

### 4.3. CP1 Acceptance Criteria

**From** `docs/archive/dev/CP1_ACCEPTANCE_REPORT.md`:

- ✅ Worker handles errors gracefully (no crashes)
- ✅ Status codes are consistent and reliable
- ✅ Basic block execution (HTTP, FS, SQL) with correct status

**Not Required for CP1**:
- ❌ Advanced retry policies
- ❌ Full cancellation support
- ❌ Complete timeout enforcement
- ❌ Error classification

---

## 5. Failure Model Summary

### 5.1. Guaranteed in CP1

| Failure Scenario | Support Level | Notes |
|----------------|---------------|-------|
| HTTP timeout | ✅ Basic | Total timeout only, connection timeout missing |
| HTTP network errors | ✅ Complete | Detected and reported |
| FS errors | ✅ Basic | Detected but no timeout enforcement |
| Status reporting | ✅ Complete | All four status codes supported |
| Basic retry | ✅ Basic | Fixed backoff, retries all errors |
| Backpressure | ✅ Basic | Concurrent limits, but unbounded queue |

### 5.2. NOT Guaranteed in CP1

| Failure Scenario | Status | Deferred To |
|-----------------|--------|-------------|
| Exponential backoff | ❌ | CP2+ |
| Retry jitter | ❌ | CP2+ |
| Error classification | ❌ | CP2+ |
| Cancellation support | ❌ | CP2+ |
| FS operation timeouts | ❌ | CP2+ |
| HTTP connection timeout | ❌ | CP2+ |
| Total timeout across retries | ❌ | CP2+ |
| Bounded queue | ❌ | CP2+ |
| Circuit breaker | ❌ | CP2+ |

### 5.3. Stakeholder Expectations

**CP1 Worker provides**:
- ✅ Stable execution (no crashes)
- ✅ Predictable status codes
- ✅ Basic retry logic (fixed backoff)
- ✅ Basic timeout enforcement (HTTP total timeout)
- ✅ Basic backpressure (concurrent limits)

**CP1 Worker does NOT provide**:
- ❌ Advanced retry policies (exponential backoff, jitter)
- ❌ Smart error classification (retryable vs non-retryable)
- ❌ Full cancellation support
- ❌ Complete timeout enforcement (FS, connection timeout)
- ❌ Bounded queue management
- ❌ Circuit breaker patterns

**For production use**:
- CP1 is suitable for **development and testing**
- CP1 is **NOT suitable** for production workloads requiring:
  - High reliability guarantees
  - Advanced retry policies
  - Full cancellation support
  - Complete timeout enforcement

**CP2+ will add**:
- Advanced retry policies
- Full cancellation support
- Complete timeout enforcement
- Error classification
- Bounded queue management
- Circuit breaker patterns

---

## 6. Recommendations

### 6.1. For CP1 Users

1. **Set realistic expectations**: CP1 provides basic failure handling, not production-grade reliability
2. **Monitor queue depth**: Watch for unbounded queue growth under load
3. **Use HTTP timeouts carefully**: Connection timeout not enforced
4. **Avoid long-running FS operations**: No timeout enforcement
5. **Expect fixed backoff**: Retries use linear backoff, not exponential

### 6.2. For CP2 Planning

**Priority 1** (Critical):
1. Add bounded queue size limits
2. Implement FS operation timeouts
3. Add HTTP connection timeout
4. Fix race condition in backpressure

**Priority 2** (Important):
5. Implement exponential backoff with jitter
6. Add error classification (retryable vs non-retryable)
7. Implement cancellation support
8. Add total timeout check across retries

**Priority 3** (Enhancements):
9. Add circuit breaker patterns
10. Add queue depth monitoring
11. Add task priority scheduling

---

## 7. References

### 7.1. Architecture Documents

- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md` - Worker role and CP1 requirements
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md#43-stepresult-contract-cp1-invariant` - StepResult contract
- `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md` - Detailed reliability review

### 7.2. Implementation Files

- `apps/caf/processor/src/worker_actor.cpp` - Retry logic implementation
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP block implementation
- `apps/caf/processor/src/blocks/fs_block.cpp` - FS block implementation
- `apps/caf/processor/include/beamline/worker/core.hpp` - StepResult type definition
- `apps/caf/processor/src/runtime/actor_pools.hpp` - Backpressure implementation

### 7.3. Acceptance Documents

- `docs/archive/dev/CP1_ACCEPTANCE_REPORT.md` - CP1 acceptance criteria
- `apps/otp/router/docs/API_CONTRACTS.md` - ExecResult contract specification

---

## 8. Change History

**v1.0 (2025-01-27)**:
- Initial gap analysis document
- Explicit failure model specification for CP1
- Gap analysis against ARCHITECTURE_ROLE.md and BLOCKS_RELIABILITY_REVIEW.md

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP1-LC  
**Status**: Gap Analysis Complete

