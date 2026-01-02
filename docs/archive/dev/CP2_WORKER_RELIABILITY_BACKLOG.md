# CP2 Worker Reliability Backlog

**Version**: 2.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-3 (Worker Reliability)  
**Status**: 📋 **EXECUTABLE BACKLOG WITH PRIORITIES** (CP2)

---

## Executive Summary

This document breaks down the high-level CP2 Worker Reliability Plan (`CP2_WORKER_RELIABILITY_PLAN.md`) into atomic, sprint-ready tickets. Each ticket is:

- **Atomic**: 0.5–2 days of work
- **Scoped**: Clear deliverables (code/tests/docs)
- **Gated**: Feature flags to protect CP1 baseline
- **Testable**: Acceptance criteria defined

**Foundation**: CP1 Worker provides basic failure handling (status reporting, basic retry, basic timeout). CP2 extends this with production-grade reliability features.

**References**:
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/CP1_WORKER_FAILURE_MODEL.md` - CP1 failure model and gap analysis
- `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md` - Detailed reliability review

---

## Feature Gate Strategy

### CP1 Baseline Protection

**CRITICAL**: All CP2 features must be gated behind feature flags to prevent breaking CP1 baseline.

**Feature Flag Pattern**:
```cpp
// Feature flag check
bool is_cp2_feature_enabled(const std::string& feature_name) {
    // Check environment variable or config
    auto env_value = std::getenv(("CP2_" + feature_name + "_ENABLED").c_str());
    if (env_value && std::string(env_value) == "true") {
        return true;
    }
    // Default: CP2 features disabled (CP1 baseline)
    return false;
}

// Usage in code
if (is_cp2_feature_enabled("ADVANCED_RETRY")) {
    // Use exponential backoff with jitter
    return exponential_backoff_with_jitter(attempt);
} else {
    // CP1 baseline: fixed backoff
    return fixed_backoff(attempt);
}
```

**Feature Flags**:
- `CP2_ADVANCED_RETRY_ENABLED` - Advanced retry policies (CP2.1)
- `CP2_COMPLETE_TIMEOUT_ENABLED` - Complete timeout enforcement (CP2.2)
- `CP2_CANCELLATION_ENABLED` - Cancellation support (CP2.3)
- `CP2_QUEUE_MANAGEMENT_ENABLED` - Bounded queue management (CP2.4)
- `CP2_CIRCUIT_BREAKER_ENABLED` - Circuit breaker patterns (CP2.5)

**Default**: All CP2 feature flags are `false` (CP1 baseline mode).

---

## CP2.1: Advanced Retry Policies

**Total Estimate**: 4 days  
**Priority**: High  
**Feature Flag**: `CP2_ADVANCED_RETRY_ENABLED`  
**Wave**: Wave 1 (P1 Critical)

### Ticket W3-1.1: Exponential Backoff Implementation

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Fixed backoff causes thundering herd, wastes resources on retries, poor user experience  
**Wave**: Wave 1  
**Dependencies**: None

**Scope**:
- Replace fixed backoff (`100 * (attempt + 1)` ms) with exponential backoff (`base * 2^attempt`)
- Configurable base delay (default: 100ms)
- Maximum backoff cap (default: 30s)
- Feature flag: `CP2_ADVANCED_RETRY_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/include/beamline/worker/retry_policy.hpp` - Retry policy interface
  - `apps/caf/processor/src/retry_policy.cpp` - Exponential backoff implementation
  - `apps/caf/processor/src/worker_actor.cpp` - Updated retry logic with feature flag check
- **Tests**: 
  - `apps/caf/processor/tests/test_retry_policy.cpp` - Exponential backoff unit tests
  - `apps/caf/processor/tests/test_retry_policy_integration.cpp` - Integration tests
- **Docs**: 
  - `docs/archive/dev/WORKER_RETRY_POLICY_IMPLEMENTATION.md` - Implementation notes

**Feature Gate**:
```cpp
// In worker_actor.cpp::execute_with_retry()
if (is_cp2_feature_enabled("ADVANCED_RETRY")) {
    int backoff_ms = exponential_backoff(attempt, base_delay_ms, max_backoff_ms);
} else {
    // CP1 baseline: fixed backoff
    int backoff_ms = 100 * (attempt + 1);
}
```

**Acceptance Criteria**:
- ✅ Exponential backoff implemented (`base * 2^attempt`)
- ✅ Configurable base delay (default: 100ms)
- ✅ Maximum backoff cap enforced (default: 30s)
- ✅ Feature flag gates CP2 behavior (CP1 baseline preserved)
- ✅ Unit tests pass (exponential backoff calculation)
- ✅ Integration tests pass (retry behavior with exponential backoff)

---

### Ticket W3-1.2: Jitter Addition

**Estimate**: 0.5 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without jitter, synchronized retries can overwhelm services, but exponential backoff alone provides significant improvement  
**Wave**: Wave 2  
**Dependencies**: W3-1.1 (Exponential Backoff)

**Scope**:
- Add random jitter to prevent thundering herd
- Full jitter: `random(0, base * 2^attempt)`
- Equal jitter: `base * 2^attempt + random(0, base * 2^attempt)`
- Feature flag: `CP2_ADVANCED_RETRY_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/retry_policy.cpp` - Jitter calculation functions
  - `apps/caf/processor/src/worker_actor.cpp` - Jitter integration
- **Tests**: 
  - `apps/caf/processor/tests/test_retry_policy.cpp` - Jitter unit tests
- **Docs**: 
  - `docs/archive/dev/WORKER_RETRY_POLICY_IMPLEMENTATION.md` - Jitter implementation notes

**Feature Gate**:
```cpp
// In retry_policy.cpp
if (is_cp2_feature_enabled("ADVANCED_RETRY")) {
    int jitter = calculate_jitter(backoff_ms, jitter_type);
    return backoff_ms + jitter;
} else {
    // CP1 baseline: no jitter
    return backoff_ms;
}
```

**Acceptance Criteria**:
- ✅ Full jitter implemented
- ✅ Equal jitter implemented
- ✅ Jitter prevents synchronized retries
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (jitter distribution)

---

### Ticket W3-1.3: Error Classification

**Estimate**: 1.5 days  
**Priority**: **P1 (Critical)**  
**Business Risk**: Retrying non-retryable errors (4xx HTTP, permission denied) wastes resources, increases latency, provides no benefit  
**Wave**: Wave 1  
**Dependencies**: W3-1.1 (Exponential Backoff)

**Scope**:
- Classify errors as retryable vs non-retryable
- Retryable: Network errors, timeouts, 5xx HTTP errors
- Non-retryable: 4xx HTTP errors (except 429), permission denied, invalid input
- Error code-based retry decisions
- Feature flag: `CP2_ADVANCED_RETRY_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/include/beamline/worker/error_classifier.hpp` - Error classification interface
  - `apps/caf/processor/src/error_classifier.cpp` - Error classification logic
  - `apps/caf/processor/src/worker_actor.cpp` - Error classification integration
  - `apps/caf/processor/src/blocks/http_block.cpp` - HTTP error classification
  - `apps/caf/processor/src/blocks/fs_block.cpp` - FS error classification
- **Tests**: 
  - `apps/caf/processor/tests/test_error_classifier.cpp` - Error classification unit tests
  - `apps/caf/processor/tests/test_retry_policy.cpp` - Retry decision tests
- **Docs**: 
  - `docs/archive/dev/WORKER_RETRY_POLICY_IMPLEMENTATION.md` - Error classification rules

**Feature Gate**:
```cpp
// In worker_actor.cpp::execute_with_retry()
if (is_cp2_feature_enabled("ADVANCED_RETRY")) {
    if (!is_retryable_error(result.error_code)) {
        // Don't retry non-retryable errors
        return result;
    }
} else {
    // CP1 baseline: retry all errors
}
```

**Acceptance Criteria**:
- ✅ Error classification implemented (retryable vs non-retryable)
- ✅ Network errors classified as retryable
- ✅ 5xx HTTP errors classified as retryable
- ✅ 4xx HTTP errors (except 429) classified as non-retryable
- ✅ Permission denied classified as non-retryable
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (error classification)
- ✅ Integration tests pass (retry decisions)

---

### Ticket W3-1.4: Retry Budget Management

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without total timeout check, retries can exceed user's timeout expectations, causing poor user experience  
**Wave**: Wave 1  
**Dependencies**: W3-1.1 (Exponential Backoff), W3-1.3 (Error Classification)

**Scope**:
- Total timeout check across all retry attempts
- Retry budget: Maximum time allowed for all retries
- Early termination if retry budget exhausted
- Feature flag: `CP2_ADVANCED_RETRY_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/worker_actor.cpp` - Retry budget tracking
  - `apps/caf/processor/src/retry_policy.cpp` - Retry budget calculation
- **Tests**: 
  - `apps/caf/processor/tests/test_retry_policy.cpp` - Retry budget tests
  - `apps/caf/processor/tests/test_retry_policy_integration.cpp` - Integration tests
- **Docs**: 
  - `docs/archive/dev/WORKER_RETRY_POLICY_IMPLEMENTATION.md` - Retry budget implementation

**Feature Gate**:
```cpp
// In worker_actor.cpp::execute_with_retry()
if (is_cp2_feature_enabled("ADVANCED_RETRY")) {
    auto elapsed = std::chrono::steady_clock::now() - start_time;
    if (elapsed > std::chrono::milliseconds(req.timeout_ms)) {
        // Retry budget exhausted
        return timeout_result();
    }
} else {
    // CP1 baseline: no total timeout check
}
```

**Acceptance Criteria**:
- ✅ Total timeout across retries enforced
- ✅ Retry budget calculated correctly
- ✅ Early termination on budget exhaustion
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (retry budget calculation)
- ✅ Integration tests pass (timeout enforcement)

---

## CP2.2: Complete Timeout Enforcement

**Total Estimate**: 3 days  
**Priority**: High  
**Feature Flag**: `CP2_COMPLETE_TIMEOUT_ENABLED`  
**Wave**: Wave 1 (P1 Critical)

### Ticket W3-2.1: FS Operation Timeouts

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: FS operations can block indefinitely on slow disks, causing thread exhaustion and system unresponsiveness  
**Wave**: Wave 1  
**Dependencies**: None

**Scope**:
- Add timeout wrapper for file I/O operations
- Use thread-based timeout mechanism
- Timeout configuration per operation type (read, write, delete)
- Feature flag: `CP2_COMPLETE_TIMEOUT_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/include/beamline/worker/timeout_wrapper.hpp` - Timeout wrapper interface
  - `apps/caf/processor/src/timeout_wrapper.cpp` - Thread-based timeout implementation
  - `apps/caf/processor/src/blocks/fs_block.cpp` - FS timeout integration
- **Tests**: 
  - `apps/caf/processor/tests/test_timeout_enforcement.cpp` - FS timeout tests
  - `apps/caf/processor/tests/test_fs_block.cpp` - FS block timeout tests
- **Docs**: 
  - `docs/archive/dev/WORKER_TIMEOUT_ENFORCEMENT_IMPLEMENTATION.md` - FS timeout implementation

**Feature Gate**:
```cpp
// In fs_block.cpp::execute()
if (is_cp2_feature_enabled("COMPLETE_TIMEOUT")) {
    return execute_with_timeout(operation, timeout_ms);
} else {
    // CP1 baseline: no timeout enforcement
    return operation();
}
```

**Acceptance Criteria**:
- ✅ FS operations have timeout enforcement
- ✅ Thread-based timeout mechanism working
- ✅ Timeout configurable per operation type
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (timeout enforcement)
- ✅ Integration tests pass (FS timeout scenarios)

---

### Ticket W3-2.2: HTTP Connection Timeout

**Estimate**: 0.5 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Connection attempts can hang indefinitely if DNS/connection fails, blocking threads and causing timeouts  
**Wave**: Wave 1  
**Dependencies**: None

**Scope**:
- Add `CURLOPT_CONNECTTIMEOUT_MS` configuration
- Separate connection timeout from total timeout
- Default: 5s connection timeout, 30s total timeout
- Feature flag: `CP2_COMPLETE_TIMEOUT_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/blocks/http_block.cpp` - HTTP connection timeout
- **Tests**: 
  - `apps/caf/processor/tests/test_timeout_enforcement.cpp` - HTTP connection timeout tests
  - `apps/caf/processor/tests/test_http_block.cpp` - HTTP block timeout tests
- **Docs**: 
  - `docs/archive/dev/WORKER_TIMEOUT_ENFORCEMENT_IMPLEMENTATION.md` - HTTP timeout implementation

**Feature Gate**:
```cpp
// In http_block.cpp::perform_http_request()
if (is_cp2_feature_enabled("COMPLETE_TIMEOUT")) {
    curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT_MS, connection_timeout_ms);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, total_timeout_ms);
} else {
    // CP1 baseline: only total timeout
    curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, total_timeout_ms);
}
```

**Acceptance Criteria**:
- ✅ HTTP connection timeout configured
- ✅ Connection timeout separate from total timeout
- ✅ Default values: 5s connection, 30s total
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (connection timeout)
- ✅ Integration tests pass (HTTP timeout scenarios)

---

### Ticket W3-2.3: Total Timeout Across Retries

**Estimate**: 1.5 days  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without total timeout enforcement, retries can exceed user expectations, causing poor user experience and resource waste  
**Wave**: Wave 1  
**Dependencies**: W3-1.4 (Retry Budget Management), W3-2.1 (FS Timeouts), W3-2.2 (HTTP Connection Timeout)

**Scope**:
- Track total time across all retry attempts
- Enforce `StepRequest.timeout_ms` as total budget
- Early termination if total timeout exceeded
- Feature flag: `CP2_COMPLETE_TIMEOUT_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/worker_actor.cpp` - Total timeout tracking
  - `apps/caf/processor/src/retry_policy.cpp` - Timeout budget calculation
- **Tests**: 
  - `apps/caf/processor/tests/test_timeout_enforcement.cpp` - Total timeout tests
  - `apps/caf/processor/tests/test_retry_policy.cpp` - Retry timeout tests
- **Docs**: 
  - `docs/archive/dev/WORKER_TIMEOUT_ENFORCEMENT_IMPLEMENTATION.md` - Total timeout implementation

**Feature Gate**:
```cpp
// In worker_actor.cpp::execute_with_retry()
if (is_cp2_feature_enabled("COMPLETE_TIMEOUT")) {
    auto elapsed = std::chrono::steady_clock::now() - start_time;
    if (elapsed > std::chrono::milliseconds(req.timeout_ms)) {
        return timeout_result();
    }
} else {
    // CP1 baseline: no total timeout check
}
```

**Acceptance Criteria**:
- ✅ Total timeout across retries enforced
- ✅ `StepRequest.timeout_ms` used as total budget
- ✅ Early termination on timeout exceeded
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (total timeout calculation)
- ✅ Integration tests pass (timeout enforcement across retries)

---

## CP2.3: Cancellation Support

**Total Estimate**: 5 days  
**Priority**: High  
**Feature Flag**: `CP2_CANCELLATION_ENABLED`  
**Wave**: Wave 2 (P2 Important)

### Ticket W3-3.1: Cancellation Token Implementation

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without cancellation, long-running operations cannot be stopped, wasting resources, but timeouts provide basic protection  
**Wave**: Wave 2  
**Dependencies**: None

**Scope**:
- Add cancellation token to `BlockContext`
- Thread-safe cancellation flag
- Cancellation propagation through operation chain
- Feature flag: `CP2_CANCELLATION_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/include/beamline/worker/cancellation_token.hpp` - Cancellation token interface
  - `apps/caf/processor/src/cancellation_token.cpp` - Cancellation token implementation
  - `apps/caf/processor/include/beamline/worker/core.hpp` - BlockContext update
- **Tests**: 
  - `apps/caf/processor/tests/test_cancellation.cpp` - Cancellation token tests
- **Docs**: 
  - `docs/archive/dev/WORKER_CANCELLATION_IMPLEMENTATION.md` - Cancellation token implementation

**Feature Gate**:
```cpp
// In core.hpp
struct BlockContext {
    // ... existing fields ...
    #ifdef CP2_CANCELLATION_ENABLED
    std::shared_ptr<CancellationToken> cancellation_token;
    #endif
};
```

**Acceptance Criteria**:
- ✅ Cancellation token implemented
- ✅ Thread-safe cancellation flag
- ✅ Cancellation propagation working
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (cancellation token)

---

### Ticket W3-3.2: HTTP Cancellation

**Estimate**: 1.5 days  
**Priority**: **P2 (Important)**  
**Business Risk**: Cannot cancel in-flight HTTP requests, but timeouts provide basic protection  
**Wave**: Wave 2  
**Dependencies**: W3-3.1 (Cancellation Token)

**Scope**:
- Cancel in-flight HTTP requests using `CURLOPT_XFERINFOFUNCTION`
- Check cancellation token during HTTP transfer
- Graceful cleanup of HTTP resources
- Feature flag: `CP2_CANCELLATION_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/blocks/http_block.cpp` - HTTP cancellation
- **Tests**: 
  - `apps/caf/processor/tests/test_cancellation.cpp` - HTTP cancellation tests
  - `apps/caf/processor/tests/test_http_block.cpp` - HTTP block cancellation tests
- **Docs**: 
  - `docs/archive/dev/WORKER_CANCELLATION_IMPLEMENTATION.md` - HTTP cancellation implementation

**Feature Gate**:
```cpp
// In http_block.cpp::perform_http_request()
if (is_cp2_feature_enabled("CANCELLATION")) {
    // Set up cancellation callback
    curl_easy_setopt(curl, CURLOPT_XFERINFOFUNCTION, cancellation_callback);
    // Check cancellation during transfer
    if (ctx.cancellation_token && ctx.cancellation_token->is_cancelled()) {
        curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, 1L); // Force timeout
    }
} else {
    // CP1 baseline: no cancellation
}
```

**Acceptance Criteria**:
- ✅ HTTP cancellation working
- ✅ Cancellation during HTTP transfer
- ✅ Graceful cleanup of HTTP resources
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (HTTP cancellation)
- ✅ Integration tests pass (HTTP cancellation scenarios)

---

### Ticket W3-3.3: FS Cancellation

**Estimate**: 1.5 days  
**Priority**: **P2 (Important)**  
**Business Risk**: Cannot cancel long-running FS operations, but FS timeouts provide basic protection  
**Wave**: Wave 2  
**Dependencies**: W3-3.1 (Cancellation Token), W3-2.1 (FS Timeouts)

**Scope**:
- Cancel long-running file operations
- Thread-based cancellation for blocking I/O
- Resource cleanup on cancellation
- Feature flag: `CP2_CANCELLATION_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/blocks/fs_block.cpp` - FS cancellation
  - `apps/caf/processor/src/timeout_wrapper.cpp` - Cancellation integration
- **Tests**: 
  - `apps/caf/processor/tests/test_cancellation.cpp` - FS cancellation tests
  - `apps/caf/processor/tests/test_fs_block.cpp` - FS block cancellation tests
- **Docs**: 
  - `docs/archive/dev/WORKER_CANCELLATION_IMPLEMENTATION.md` - FS cancellation implementation

**Feature Gate**:
```cpp
// In fs_block.cpp::execute()
if (is_cp2_feature_enabled("CANCELLATION")) {
    return execute_with_cancellation(operation, ctx.cancellation_token);
} else {
    // CP1 baseline: no cancellation
    return operation();
}
```

**Acceptance Criteria**:
- ✅ FS cancellation working
- ✅ Thread-based cancellation for blocking I/O
- ✅ Resource cleanup on cancellation
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (FS cancellation)
- ✅ Integration tests pass (FS cancellation scenarios)

---

### Ticket W3-3.4: Retry Wait Cancellation

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Cannot cancel during retry wait, but retry budget management provides basic protection  
**Wave**: Wave 2  
**Dependencies**: W3-3.1 (Cancellation Token), W3-1.1 (Exponential Backoff)

**Scope**:
- Check cancellation token during retry wait periods
- Early termination of retry loop on cancellation
- Cleanup of retry state
- Feature flag: `CP2_CANCELLATION_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/worker_actor.cpp` - Retry cancellation
  - `apps/caf/processor/src/retry_policy.cpp` - Cancellation check in backoff
- **Tests**: 
  - `apps/caf/processor/tests/test_cancellation.cpp` - Retry cancellation tests
  - `apps/caf/processor/tests/test_retry_policy.cpp` - Retry wait cancellation tests
- **Docs**: 
  - `docs/archive/dev/WORKER_CANCELLATION_IMPLEMENTATION.md` - Retry cancellation implementation

**Feature Gate**:
```cpp
// In worker_actor.cpp::execute_with_retry()
if (is_cp2_feature_enabled("CANCELLATION")) {
    // During retry wait
    while (wait_time_remaining > 0) {
        if (ctx.cancellation_token && ctx.cancellation_token->is_cancelled()) {
            return cancelled_result();
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
        wait_time_remaining -= 10;
    }
} else {
    // CP1 baseline: no cancellation during wait
    std::this_thread::sleep_for(std::chrono::milliseconds(backoff_ms));
}
```

**Acceptance Criteria**:
- ✅ Retry wait cancellation working
- ✅ Early termination on cancellation
- ✅ Cleanup of retry state
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (retry cancellation)
- ✅ Integration tests pass (retry wait cancellation scenarios)

---

## CP2.4: Queue Management

**Total Estimate**: 3 days  
**Priority**: High  
**Feature Flag**: `CP2_QUEUE_MANAGEMENT_ENABLED`  
**Wave**: Wave 1 (P1 Critical)

### Ticket W3-4.1: Bounded Queue Implementation

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Unbounded queue growth can cause memory exhaustion, system crashes, and production outages  
**Wave**: Wave 1  
**Dependencies**: None

**Scope**:
- Add `max_queue_size` configuration per resource pool
- Queue rejection when queue is full
- Rejection policy: `reject_new` (default) or `drop_oldest`
- Feature flag: `CP2_QUEUE_MANAGEMENT_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/runtime/actor_pools.hpp` - Bounded queue implementation
  - `apps/caf/processor/src/runtime/actor_pools.cpp` - Queue size limits
- **Tests**: 
  - `apps/caf/processor/tests/test_queue_management.cpp` - Bounded queue tests
- **Docs**: 
  - `docs/archive/dev/WORKER_QUEUE_MANAGEMENT_IMPLEMENTATION.md` - Bounded queue implementation

**Feature Gate**:
```cpp
// In actor_pools.hpp::submit()
if (is_cp2_feature_enabled("QUEUE_MANAGEMENT")) {
    if (q_.size() >= max_queue_size_) {
        return false; // Reject when full
    }
} else {
    // CP1 baseline: unbounded queue
}
```

**Acceptance Criteria**:
- ✅ Bounded queue size limits implemented
- ✅ Queue rejection when full
- ✅ Rejection policy configurable
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (bounded queue)
- ✅ Integration tests pass (queue rejection)

---

### Ticket W3-4.2: Queue Depth Monitoring

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without monitoring, cannot detect queue growth before memory exhaustion, preventing proactive response  
**Wave**: Wave 1  
**Dependencies**: W3-4.1 (Bounded Queue)

**Scope**:
- Track queue depth per resource pool
- Expose queue depth metrics
- Queue depth alerts (configurable thresholds)
- Feature flag: `CP2_QUEUE_MANAGEMENT_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/runtime/queue_manager.hpp` - Queue management interface
  - `apps/caf/processor/src/runtime/queue_manager.cpp` - Queue depth tracking
  - `apps/caf/processor/src/runtime/actor_pools.cpp` - Metrics exposure
- **Tests**: 
  - `apps/caf/processor/tests/test_queue_management.cpp` - Queue depth monitoring tests
- **Docs**: 
  - `docs/archive/dev/WORKER_QUEUE_MANAGEMENT_IMPLEMENTATION.md` - Queue depth monitoring

**Feature Gate**:
```cpp
// In queue_manager.cpp
if (is_cp2_feature_enabled("QUEUE_MANAGEMENT")) {
    metrics.set_queue_depth(pool_id, queue_depth);
} else {
    // CP1 baseline: no queue depth monitoring
}
```

**Acceptance Criteria**:
- ✅ Queue depth monitoring working
- ✅ Queue depth metrics exposed
- ✅ Configurable thresholds
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (queue depth tracking)
- ✅ Integration tests pass (metrics exposure)

---

### Ticket W3-4.3: Queue Rejection Handling

**Estimate**: 1 day  
**Priority**: **P1 (Critical)**  
**Business Risk**: Without rejection handling, queue can still grow if bounded queue check fails, and Router needs feedback for backpressure  
**Wave**: Wave 1  
**Dependencies**: W3-4.1 (Bounded Queue), W3-4.2 (Queue Depth Monitoring)

**Scope**:
- Return `ExecAssignmentAck` with `status: "rejected"` when queue full
- Include rejection reason in ACK message
- Metrics for queue rejections
- Feature flag: `CP2_QUEUE_MANAGEMENT_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/worker_actor.cpp` - Queue rejection handling
  - `apps/caf/processor/src/runtime/actor_pools.cpp` - Rejection metrics
- **Tests**: 
  - `apps/caf/processor/tests/test_queue_management.cpp` - Queue rejection tests
  - `apps/caf/processor/tests/test_worker_actor.cpp` - ACK rejection tests
- **Docs**: 
  - `docs/archive/dev/WORKER_QUEUE_MANAGEMENT_IMPLEMENTATION.md` - Queue rejection handling

**Feature Gate**:
```cpp
// In worker_actor.cpp::handle_exec_assignment()
if (is_cp2_feature_enabled("QUEUE_MANAGEMENT")) {
    if (!pool.submit(task)) {
        // Queue full, reject assignment
        return ExecAssignmentAck{status: "rejected", reason: "queue_full"};
    }
} else {
    // CP1 baseline: always accept (unbounded queue)
    pool.submit(task);
}
```

**Acceptance Criteria**:
- ✅ Queue rejection handling working
- ✅ `ExecAssignmentAck` with `status: "rejected"` returned
- ✅ Rejection reason included
- ✅ Metrics for queue rejections
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (queue rejection)
- ✅ Integration tests pass (ACK rejection)

---

## CP2.5: Circuit Breaker Patterns

**Total Estimate**: 4 days  
**Priority**: Medium  
**Feature Flag**: `CP2_CIRCUIT_BREAKER_ENABLED`  
**Wave**: Wave 2 (P2 Important)

### Ticket W3-5.1: Circuit Breaker Implementation

**Estimate**: 2 days  
**Priority**: **P2 (Important)**  
**Business Risk**: Without circuit breaker, failing services can cause cascading failures, but retry policies and timeouts provide basic protection  
**Wave**: Wave 2  
**Dependencies**: None

**Scope**:
- Circuit states: `closed`, `open`, `half_open`
- Failure threshold: Open circuit after N consecutive failures
- Recovery timeout: Attempt recovery after timeout period
- Success threshold: Close circuit after M consecutive successes
- Feature flag: `CP2_CIRCUIT_BREAKER_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/include/beamline/worker/circuit_breaker.hpp` - Circuit breaker interface
  - `apps/caf/processor/src/circuit_breaker.cpp` - Circuit breaker implementation
- **Tests**: 
  - `apps/caf/processor/tests/test_circuit_breaker.cpp` - Circuit breaker tests
- **Docs**: 
  - `docs/archive/dev/WORKER_CIRCUIT_BREAKER_IMPLEMENTATION.md` - Circuit breaker implementation

**Feature Gate**:
```cpp
// In circuit_breaker.hpp
class CircuitBreaker {
    bool is_open() const {
        if (!is_cp2_feature_enabled("CIRCUIT_BREAKER")) {
            return false; // CP1 baseline: always closed
        }
        return state_ == CircuitState::open;
    }
};
```

**Acceptance Criteria**:
- ✅ Circuit breaker implemented
- ✅ Circuit states working (closed, open, half_open)
- ✅ Failure threshold configurable
- ✅ Recovery timeout configurable
- ✅ Success threshold configurable
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (circuit breaker logic)
- ✅ Integration tests pass (circuit breaker scenarios)

---

### Ticket W3-5.2: HTTP Circuit Breaker

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Failing HTTP endpoints can cause cascading failures, but retry policies provide basic protection  
**Wave**: Wave 2  
**Dependencies**: W3-5.1 (Circuit Breaker Implementation)

**Scope**:
- Per-endpoint circuit breaker
- Failure detection: 5xx errors, timeouts
- Automatic service degradation
- Feature flag: `CP2_CIRCUIT_BREAKER_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/blocks/http_block.cpp` - HTTP circuit breaker integration
  - `apps/caf/processor/src/circuit_breaker.cpp` - Per-endpoint circuit breakers
- **Tests**: 
  - `apps/caf/processor/tests/test_circuit_breaker.cpp` - HTTP circuit breaker tests
  - `apps/caf/processor/tests/test_http_block.cpp` - HTTP block circuit breaker tests
- **Docs**: 
  - `docs/archive/dev/WORKER_CIRCUIT_BREAKER_IMPLEMENTATION.md` - HTTP circuit breaker implementation

**Feature Gate**:
```cpp
// In http_block.cpp::perform_http_request()
if (is_cp2_feature_enabled("CIRCUIT_BREAKER")) {
    auto breaker = get_circuit_breaker(endpoint);
    if (breaker->is_open()) {
        return error_result("circuit_breaker_open");
    }
    // ... perform request ...
    if (is_failure(result)) {
        breaker->record_failure();
    } else {
        breaker->record_success();
    }
} else {
    // CP1 baseline: no circuit breaker
    return perform_http_request(...);
}
```

**Acceptance Criteria**:
- ✅ HTTP circuit breaker working
- ✅ Per-endpoint circuit breakers
- ✅ Failure detection (5xx, timeouts)
- ✅ Automatic service degradation
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (HTTP circuit breaker)
- ✅ Integration tests pass (HTTP circuit breaker scenarios)

---

### Ticket W3-5.3: FS Circuit Breaker

**Estimate**: 1 day  
**Priority**: **P3 (Nice to Have)**  
**Business Risk**: FS failures are less common and less critical than HTTP failures, FS timeouts provide basic protection  
**Wave**: Wave 2  
**Dependencies**: W3-5.1 (Circuit Breaker Implementation)

**Scope**:
- Per-path circuit breaker (optional)
- Failure detection: Permission denied, disk full
- Automatic fallback to alternative storage
- Feature flag: `CP2_CIRCUIT_BREAKER_ENABLED`

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/src/blocks/fs_block.cpp` - FS circuit breaker integration
  - `apps/caf/processor/src/circuit_breaker.cpp` - Per-path circuit breakers
- **Tests**: 
  - `apps/caf/processor/tests/test_circuit_breaker.cpp` - FS circuit breaker tests
  - `apps/caf/processor/tests/test_fs_block.cpp` - FS block circuit breaker tests
- **Docs**: 
  - `docs/archive/dev/WORKER_CIRCUIT_BREAKER_IMPLEMENTATION.md` - FS circuit breaker implementation

**Feature Gate**:
```cpp
// In fs_block.cpp::execute()
if (is_cp2_feature_enabled("CIRCUIT_BREAKER")) {
    auto breaker = get_circuit_breaker(path);
    if (breaker->is_open()) {
        return error_result("circuit_breaker_open");
    }
    // ... perform operation ...
    if (is_failure(result)) {
        breaker->record_failure();
    } else {
        breaker->record_success();
    }
} else {
    // CP1 baseline: no circuit breaker
    return execute(...);
}
```

**Acceptance Criteria**:
- ✅ FS circuit breaker working (optional)
- ✅ Per-path circuit breakers
- ✅ Failure detection (permission denied, disk full)
- ✅ Automatic fallback (if configured)
- ✅ Feature flag gates CP2 behavior
- ✅ Unit tests pass (FS circuit breaker)
- ✅ Integration tests pass (FS circuit breaker scenarios)

---

## CP2.6: Load Testing & Chaos Engineering

**Total Estimate**: 5 days  
**Priority**: High  
**Feature Flag**: N/A (Testing infrastructure, not runtime feature)  
**Wave**: Wave 2 (P2 Important)

### Ticket W3-6.1: Load Testing Infrastructure

**Estimate**: 2 days  
**Priority**: **P2 (Important)**  
**Business Risk**: Without load testing, cannot validate reliability under production load, but basic testing provides initial confidence  
**Wave**: Wave 2  
**Dependencies**: All CP2.1–CP2.5 tickets (for comprehensive testing)

**Scope**:
- High throughput tests (100-500 tasks/sec)
- Large payload tests (10-100 MB)
- Mixed workload tests
- Performance metrics collection

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/tests/test_load_performance.cpp` - Load tests
  - `scripts/run_worker_load_tests.sh` - Load test script
- **Tests**: 
  - Load test scenarios (throughput, latency, memory)
- **Docs**: 
  - `docs/archive/dev/WORKER_LOAD_TESTS_REPORT.md` - Load test report

**Acceptance Criteria**:
- ✅ Throughput ≥ 200 tasks/sec (CP2 baseline)
- ✅ p95 latency < 1s for small payloads
- ✅ p95 latency < 5s for large payloads (100 MB)
- ✅ Performance metrics collected
- ✅ Load test report generated

---

### Ticket W3-6.2: Chaos Engineering Infrastructure

**Estimate**: 2 days  
**Priority**: **P2 (Important)**  
**Business Risk**: Without chaos testing, cannot validate resilience under failure scenarios, but basic testing provides initial confidence  
**Wave**: Wave 2  
**Dependencies**: All CP2.1–CP2.5 tickets (for comprehensive testing)

**Scope**:
- Latency injection for FS/HTTP operations
- Failure injection (network errors, disk errors)
- Recovery testing after failures
- Stability verification

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/tests/test_chaos_fs_http.cpp` - Chaos tests
  - `scripts/run_worker_chaos_tests.sh` - Chaos test script
- **Tests**: 
  - Chaos test scenarios (latency injection, failure injection)
- **Docs**: 
  - `docs/archive/dev/WORKER_CHAOS_TESTS_REPORT.md` - Chaos test report

**Acceptance Criteria**:
- ✅ Latency injection working
- ✅ Failure injection working
- ✅ Recovery testing completed
- ✅ Worker doesn't crash under chaos scenarios
- ✅ Chaos test report generated

---

### Ticket W3-6.3: Graceful Degradation Testing

**Estimate**: 1 day  
**Priority**: **P2 (Important)**  
**Business Risk**: Without degradation testing, cannot validate graceful handling of overload, but bounded queue provides basic protection  
**Wave**: Wave 2  
**Dependencies**: W3-4.1 (Bounded Queue), W3-6.1 (Load Testing), W3-6.2 (Chaos Engineering)

**Scope**:
- Queue overflow scenarios
- Resource exhaustion scenarios
- Backpressure propagation
- Degradation under load

**Artifacts**:
- **Code**: 
  - `apps/caf/processor/tests/test_backpressure.cpp` - Backpressure tests
  - `apps/caf/processor/tests/test_graceful_degradation.cpp` - Degradation tests
- **Tests**: 
  - Degradation test scenarios (queue overflow, resource exhaustion)
- **Docs**: 
  - `docs/archive/dev/WORKER_LOAD_TESTS_REPORT.md` - Degradation test results

**Acceptance Criteria**:
- ✅ Graceful degradation working
- ✅ Backpressure working correctly
- ✅ Queue overflow handled gracefully
- ✅ Resource exhaustion handled gracefully
- ✅ Degradation test report generated

---

## Ticket Dependencies Graph

```
CP2.1: Advanced Retry Policies
├─ W3-1.1: Exponential Backoff (1d) [No deps]
├─ W3-1.2: Jitter Addition (0.5d) [Depends: W3-1.1]
├─ W3-1.3: Error Classification (1.5d) [Depends: W3-1.1]
└─ W3-1.4: Retry Budget Management (1d) [Depends: W3-1.1, W3-1.3]

CP2.2: Complete Timeout Enforcement
├─ W3-2.1: FS Operation Timeouts (1d) [No deps]
├─ W3-2.2: HTTP Connection Timeout (0.5d) [No deps]
└─ W3-2.3: Total Timeout Across Retries (1.5d) [Depends: W3-1.4, W3-2.1, W3-2.2]

CP2.3: Cancellation Support
├─ W3-3.1: Cancellation Token (1d) [No deps]
├─ W3-3.2: HTTP Cancellation (1.5d) [Depends: W3-3.1]
├─ W3-3.3: FS Cancellation (1.5d) [Depends: W3-3.1, W3-2.1]
└─ W3-3.4: Retry Wait Cancellation (1d) [Depends: W3-3.1, W3-1.1]

CP2.4: Queue Management
├─ W3-4.1: Bounded Queue (1d) [No deps]
├─ W3-4.2: Queue Depth Monitoring (1d) [Depends: W3-4.1]
└─ W3-4.3: Queue Rejection Handling (1d) [Depends: W3-4.1, W3-4.2]

CP2.5: Circuit Breaker Patterns
├─ W3-5.1: Circuit Breaker Implementation (2d) [No deps]
├─ W3-5.2: HTTP Circuit Breaker (1d) [Depends: W3-5.1]
└─ W3-5.3: FS Circuit Breaker (1d) [Depends: W3-5.1]

CP2.6: Load Testing & Chaos Engineering
├─ W3-6.1: Load Testing Infrastructure (2d) [Depends: All CP2.1–CP2.5]
├─ W3-6.2: Chaos Engineering Infrastructure (2d) [Depends: All CP2.1–CP2.5]
└─ W3-6.3: Graceful Degradation Testing (1d) [Depends: W3-4.1, W3-6.1, W3-6.2]
```

---

## Feature Flag Summary

| Feature Flag | Default | CP2 Task | Purpose |
|--------------|---------|----------|---------|
| `CP2_ADVANCED_RETRY_ENABLED` | `false` | CP2.1 | Advanced retry policies (exponential backoff, jitter, error classification) |
| `CP2_COMPLETE_TIMEOUT_ENABLED` | `false` | CP2.2 | Complete timeout enforcement (FS, HTTP connection, total timeout) |
| `CP2_CANCELLATION_ENABLED` | `false` | CP2.3 | Cancellation support (HTTP, FS, retry wait) |
| `CP2_QUEUE_MANAGEMENT_ENABLED` | `false` | CP2.4 | Bounded queue management (queue limits, rejection policies) |
| `CP2_CIRCUIT_BREAKER_ENABLED` | `false` | CP2.5 | Circuit breaker patterns (HTTP, FS) |

**CRITICAL**: All feature flags default to `false` to preserve CP1 baseline behavior.

---

## Priority Classification

### P1 (Critical) - Production Blockers

**Business Risks**:
- **Memory Exhaustion**: Unbounded queue growth can cause system crashes
- **Thread Exhaustion**: FS operations without timeouts can block indefinitely
- **Resource Waste**: Retrying non-retryable errors wastes resources and increases latency
- **Poor User Experience**: Missing timeout enforcement causes unpredictable behavior

**Tickets**:
- W3-1.1: Exponential Backoff Implementation
- W3-1.3: Error Classification
- W3-1.4: Retry Budget Management
- W3-2.1: FS Operation Timeouts
- W3-2.2: HTTP Connection Timeout
- W3-2.3: Total Timeout Across Retries
- W3-4.1: Bounded Queue Implementation
- W3-4.2: Queue Depth Monitoring
- W3-4.3: Queue Rejection Handling

**Total Estimate**: ~9.5 days

### P2 (Important) - Production Quality

**Business Risks**:
- **Resource Waste**: Cannot cancel long-running operations
- **Cascading Failures**: Without circuit breaker, failing services can cause system-wide issues
- **Validation Gaps**: Without load/chaos testing, cannot validate production readiness

**Tickets**:
- W3-1.2: Jitter Addition
- W3-3.1: Cancellation Token Implementation
- W3-3.2: HTTP Cancellation
- W3-3.3: FS Cancellation
- W3-3.4: Retry Wait Cancellation
- W3-5.1: Circuit Breaker Implementation
- W3-5.2: HTTP Circuit Breaker
- W3-6.1: Load Testing Infrastructure
- W3-6.2: Chaos Engineering Infrastructure
- W3-6.3: Graceful Degradation Testing

**Total Estimate**: ~13.5 days

### P3 (Nice to Have) - Enhancements

**Business Risks**:
- **Edge Cases**: FS circuit breaker is less critical than HTTP circuit breaker

**Tickets**:
- W3-5.3: FS Circuit Breaker

**Total Estimate**: ~1 day

---

## Wave Planning

**For detailed Wave 1 specification**, see `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1.md`.

### Wave 1: Critical Reliability (P1) - Production Blockers

**Goal**: Address critical production blockers that can cause system crashes, memory exhaustion, and poor user experience.

**Scope**:
- Advanced Retry Policies (exponential backoff, error classification, retry budget)
- Complete Timeout Enforcement (FS, HTTP connection, total timeout)
- Bounded Queue Management (queue limits, monitoring, rejection)

**Tickets**:
1. W3-1.1: Exponential Backoff Implementation (1d)
2. W3-1.3: Error Classification (1.5d)
3. W3-1.4: Retry Budget Management (1d)
4. W3-2.1: FS Operation Timeouts (1d)
5. W3-2.2: HTTP Connection Timeout (0.5d)
6. W3-2.3: Total Timeout Across Retries (1.5d)
7. W3-4.1: Bounded Queue Implementation (1d)
8. W3-4.2: Queue Depth Monitoring (1d)
9. W3-4.3: Queue Rejection Handling (1d)

**Total Estimate**: ~9.5 days (~2 weeks)

**Business Value**:
- ✅ Prevents memory exhaustion (bounded queue)
- ✅ Prevents thread exhaustion (FS timeouts)
- ✅ Reduces resource waste (error classification)
- ✅ Improves user experience (timeout enforcement)
- ✅ Enables production deployment

### Wave 2: Production Quality (P2/P3) - Enhancements

**Goal**: Add production-quality features for cancellation, circuit breakers, and comprehensive testing.

**Scope**:
- Jitter Addition (retry improvements)
- Cancellation Support (HTTP, FS, retry wait)
- Circuit Breaker Patterns (HTTP, FS)
- Load Testing & Chaos Engineering

**Tickets**:
1. W3-1.2: Jitter Addition (0.5d)
2. W3-3.1: Cancellation Token Implementation (1d)
3. W3-3.2: HTTP Cancellation (1.5d)
4. W3-3.3: FS Cancellation (1.5d)
5. W3-3.4: Retry Wait Cancellation (1d)
6. W3-5.1: Circuit Breaker Implementation (2d)
7. W3-5.2: HTTP Circuit Breaker (1d)
8. W3-5.3: FS Circuit Breaker (1d) [P3]
9. W3-6.1: Load Testing Infrastructure (2d)
10. W3-6.2: Chaos Engineering Infrastructure (2d)
11. W3-6.3: Graceful Degradation Testing (1d)

**Total Estimate**: ~14.5 days (~3 weeks)

**Business Value**:
- ✅ Improves retry efficiency (jitter)
- ✅ Enables operation cancellation
- ✅ Prevents cascading failures (circuit breaker)
- ✅ Validates production readiness (load/chaos testing)

---

## Implementation Order Recommendation

### Wave 1: Critical Reliability (Week 1-2) [P1]
1. **W3-1.1**: Exponential Backoff (1d) [P1]
2. **W3-1.3**: Error Classification (1.5d) [P1]
3. **W3-1.4**: Retry Budget Management (1d) [P1]
4. **W3-2.1**: FS Operation Timeouts (1d) [P1]
5. **W3-2.2**: HTTP Connection Timeout (0.5d) [P1]
6. **W3-2.3**: Total Timeout Across Retries (1.5d) [P1]
7. **W3-4.1**: Bounded Queue (1d) [P1]
8. **W3-4.2**: Queue Depth Monitoring (1d) [P1]
9. **W3-4.3**: Queue Rejection Handling (1d) [P1]

**Total**: ~9.5 days (~2 weeks)

### Wave 2: Production Quality (Week 3-5) [P2/P3]
1. **W3-1.2**: Jitter Addition (0.5d) [P2]
2. **W3-3.1**: Cancellation Token (1d) [P2]
3. **W3-3.2**: HTTP Cancellation (1.5d) [P2]
4. **W3-3.3**: FS Cancellation (1.5d) [P2]
5. **W3-3.4**: Retry Wait Cancellation (1d) [P2]
6. **W3-5.1**: Circuit Breaker Implementation (2d) [P2]
7. **W3-5.2**: HTTP Circuit Breaker (1d) [P2]
8. **W3-5.3**: FS Circuit Breaker (1d) [P3]
9. **W3-6.1**: Load Testing Infrastructure (2d) [P2]
10. **W3-6.2**: Chaos Engineering Infrastructure (2d) [P2]
11. **W3-6.3**: Graceful Degradation Testing (1d) [P2]

**Total**: ~14.5 days (~3 weeks)

**Grand Total**: ~24 days (~5 weeks)

---

## Acceptance Criteria Summary

### CP2.1: Advanced Retry Policies
- ✅ Exponential backoff implemented
- ✅ Jitter added
- ✅ Error classification working
- ✅ Retry budget management working
- ✅ All features gated behind `CP2_ADVANCED_RETRY_ENABLED`

### CP2.2: Complete Timeout Enforcement
- ✅ FS operations have timeout enforcement
- ✅ HTTP connection timeout configured
- ✅ Total timeout across retries enforced
- ✅ All features gated behind `CP2_COMPLETE_TIMEOUT_ENABLED`

### CP2.3: Cancellation Support
- ✅ Cancellation token implemented
- ✅ HTTP cancellation working
- ✅ FS cancellation working
- ✅ Retry wait cancellation working
- ✅ All features gated behind `CP2_CANCELLATION_ENABLED`

### CP2.4: Queue Management
- ✅ Bounded queue size limits implemented
- ✅ Queue rejection when full
- ✅ Queue depth monitoring working
- ✅ All features gated behind `CP2_QUEUE_MANAGEMENT_ENABLED`

### CP2.5: Circuit Breaker Patterns
- ✅ Circuit breaker implemented
- ✅ HTTP circuit breaker working
- ✅ FS circuit breaker working (optional)
- ✅ All features gated behind `CP2_CIRCUIT_BREAKER_ENABLED`

### CP2.6: Load Testing & Chaos Engineering
- ✅ Throughput ≥ 200 tasks/sec
- ✅ p95 latency < 1s (small payloads)
- ✅ p95 latency < 5s (large payloads)
- ✅ Worker doesn't crash under chaos scenarios
- ✅ Graceful degradation working

---

## References

### Input Documents
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/CP1_WORKER_FAILURE_MODEL.md` - CP1 failure model and gap analysis
- `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md` - Detailed reliability review

### Implementation Files
- `apps/caf/processor/src/worker_actor.cpp` - Retry logic implementation
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP block implementation
- `apps/caf/processor/src/blocks/fs_block.cpp` - FS block implementation
- `apps/caf/processor/src/runtime/actor_pools.hpp` - Backpressure implementation

### Architecture Documents
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md` - Worker architecture and requirements
- `apps/caf/processor/docs/CP1_WORKER_CORE_PROFILE.md` - CP1 core profile

---

## Change History

**v2.0 (2025-01-27)**:
- Added priority classification (P1/P2/P3) with business risk justification
- Added Wave planning (Wave 1: P1 Critical, Wave 2: P2/P3 Enhancements)
- Updated implementation order recommendation to align with waves
- All tickets marked with priority and wave assignment

**v1.0 (2025-01-27)**:
- Initial CP2 Worker Reliability Backlog
- Breakdown of CP2.1–CP2.6 into atomic tickets
- Feature flag strategy defined
- Implementation order recommended

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Executable Backlog with Priorities Ready

