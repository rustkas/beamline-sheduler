# CP2 Worker Reliability Plan

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-3 (Worker Reliability)  
**Status**: 📋 **PLANNING DOCUMENT** (CP2)

---

## Executive Summary

This document defines the **CP2 Worker Reliability Plan** for wrk-3, building upon CP1 baseline reliability and addressing gaps identified in the CP1 failure model. The plan focuses on:

1. **Advanced Retry Policies**: Exponential backoff, jitter, error classification
2. **Complete Timeout Enforcement**: FS operations, connection timeouts, total timeout across retries
3. **Cancellation Support**: Full cancellation for in-flight operations
4. **Queue Management**: Bounded queues, rejection policies, monitoring
5. **Circuit Breaker Patterns**: Resilience patterns for external services
6. **Load Testing & Chaos Engineering**: Verification under high load and failure scenarios

**Foundation**: CP1 Worker provides basic failure handling (status reporting, basic retry, basic timeout). CP2 extends this with production-grade reliability features.

**Reference**: `docs/archive/dev/CP1_WORKER_FAILURE_MODEL.md` - CP1 failure model and gap analysis

---

## CP1 Baseline (Complete)

### CP1 Guaranteed Features

**Status Reporting**:
- ✅ All block executions return one of four status codes: `success`, `error`, `timeout`, `cancelled`
- ✅ StepResult contract with `StepStatus` enum

**HTTP Block Failures**:
- ✅ HTTP request timeout enforcement (`CURLOPT_TIMEOUT_MS`)
- ✅ Network error detection (connection refused, DNS failure)
- ✅ HTTP error status codes (4xx, 5xx) reported as `error` status
- ✅ Basic retry logic at executor actor level

**Filesystem Block Failures**:
- ✅ File operation error detection (permission denied, file not found)
- ✅ Path validation (security checks)
- ✅ Basic error reporting with `error` status

**Retry Logic (Basic)**:
- ✅ Configurable retry count (`StepRequest.retry_count`)
- ✅ Retry attempts tracked in `StepResult.retries_used`
- ✅ Basic backoff delay between retries

**Backpressure (Basic)**:
- ✅ Concurrent execution limits (`max_concurrency_` per resource pool)
- ✅ Queue-based task submission when at capacity
- ✅ Load tracking (`current_load_`)

### CP1 Limitations (Addressed in CP2)

**Retry Logic**:
- ❌ Fixed backoff (not exponential)
- ❌ No jitter (can cause thundering herd)
- ❌ Retries all errors (doesn't distinguish retryable vs non-retryable)
- ❌ No cancellation during retry wait
- ❌ No total timeout check across all retries

**Timeout Enforcement**:
- ❌ FS operation timeouts missing (operations can block indefinitely)
- ❌ HTTP connection timeout missing (only total timeout configured)
- ❌ Total timeout across all retry attempts missing

**Cancellation Support**:
- ❌ No cancellation during in-flight HTTP requests
- ❌ No cancellation during FS operations
- ❌ No cancellation during retry wait periods

**Queue Management**:
- ❌ Unbounded queue growth (no `max_queue_size` limit)
- ❌ No queue rejection when overloaded
- ❌ No queue depth monitoring

**Error Classification**:
- ❌ No automatic distinction between retryable vs non-retryable errors
- ❌ No error code-based retry decisions

---

## CP2 Reliability Tasks

### Task CP2.1: Advanced Retry Policies

**Worker**: wrk-3  
**Priority**: High  
**Estimate**: 4 days

**Objective**: Implement exponential backoff with jitter, error classification, and retry budget management.

**Subtasks**:

1. **Exponential Backoff Implementation**:
   - Replace fixed backoff (`100 * (attempt + 1)` ms) with exponential backoff (`base * 2^attempt`)
   - Configurable base delay (default: 100ms)
   - Maximum backoff cap (default: 30s)

2. **Jitter Addition**:
   - Add random jitter to prevent thundering herd
   - Full jitter: `random(0, base * 2^attempt)`
   - Equal jitter: `base * 2^attempt + random(0, base * 2^attempt)`

3. **Error Classification**:
   - Classify errors as retryable vs non-retryable
   - Retryable: Network errors, timeouts, 5xx HTTP errors
   - Non-retryable: 4xx HTTP errors (except 429), permission denied, invalid input
   - Error code-based retry decisions

4. **Retry Budget Management**:
   - Total timeout check across all retry attempts
   - Retry budget: Maximum time allowed for all retries
   - Early termination if retry budget exhausted

**Artifacts**:
- `apps/caf/processor/include/beamline/worker/retry_policy.hpp` - Retry policy interface
- `apps/caf/processor/src/worker_actor.cpp` - Updated retry logic
- `apps/caf/processor/tests/test_retry_policy.cpp` - Retry policy tests
- `docs/archive/dev/WORKER_RETRY_POLICY_IMPLEMENTATION.md` - Implementation report

**Acceptance Criteria**:
- ✅ Exponential backoff implemented (configurable base, max cap)
- ✅ Jitter added to prevent thundering herd
- ✅ Error classification working (retryable vs non-retryable)
- ✅ Retry budget management working (total timeout across retries)
- ✅ Unit tests for retry policy
- ✅ Integration tests for retry scenarios

**Reference**: `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md#4-executor-actor-retry-logic`

---

### Task CP2.2: Complete Timeout Enforcement

**Worker**: wrk-3  
**Priority**: High  
**Estimate**: 3 days

**Objective**: Implement complete timeout enforcement for all operations (FS, HTTP connection, total timeout across retries).

**Subtasks**:

1. **FS Operation Timeouts**:
   - Add timeout wrapper for file I/O operations
   - Use thread-based timeout mechanism
   - Timeout configuration per operation type (read, write, delete)

2. **HTTP Connection Timeout**:
   - Add `CURLOPT_CONNECTTIMEOUT_MS` configuration
   - Separate connection timeout from total timeout
   - Default: 5s connection timeout, 30s total timeout

3. **Total Timeout Across Retries**:
   - Track total time across all retry attempts
   - Enforce `StepRequest.timeout_ms` as total budget
   - Early termination if total timeout exceeded

**Artifacts**:
- `apps/caf/processor/src/blocks/fs_block.cpp` - FS timeout implementation
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP connection timeout
- `apps/caf/processor/src/worker_actor.cpp` - Total timeout tracking
- `apps/caf/processor/tests/test_timeout_enforcement.cpp` - Timeout tests
- `docs/archive/dev/WORKER_TIMEOUT_ENFORCEMENT_IMPLEMENTATION.md` - Implementation report

**Acceptance Criteria**:
- ✅ FS operations have timeout enforcement
- ✅ HTTP connection timeout configured
- ✅ Total timeout across retries enforced
- ✅ Unit tests for timeout scenarios
- ✅ Integration tests for timeout handling

**Reference**: `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md#1-filesystem-block`, `#2-http-block`

---

### Task CP2.3: Cancellation Support

**Worker**: wrk-3  
**Priority**: High  
**Estimate**: 5 days

**Objective**: Implement full cancellation support for in-flight operations (HTTP, FS, retry wait).

**Subtasks**:

1. **Cancellation Token Implementation**:
   - Add cancellation token to `BlockContext`
   - Thread-safe cancellation flag
   - Cancellation propagation through operation chain

2. **HTTP Cancellation**:
   - Cancel in-flight HTTP requests using `CURLOPT_XFERINFOFUNCTION`
   - Check cancellation token during HTTP transfer
   - Graceful cleanup of HTTP resources

3. **FS Cancellation**:
   - Cancel long-running file operations
   - Thread-based cancellation for blocking I/O
   - Resource cleanup on cancellation

4. **Retry Wait Cancellation**:
   - Check cancellation token during retry wait periods
   - Early termination of retry loop on cancellation
   - Cleanup of retry state

**Artifacts**:
- `apps/caf/processor/include/beamline/worker/cancellation_token.hpp` - Cancellation token
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP cancellation
- `apps/caf/processor/src/blocks/fs_block.cpp` - FS cancellation
- `apps/caf/processor/src/worker_actor.cpp` - Retry cancellation
- `apps/caf/processor/tests/test_cancellation.cpp` - Cancellation tests
- `docs/archive/dev/WORKER_CANCELLATION_IMPLEMENTATION.md` - Implementation report

**Acceptance Criteria**:
- ✅ Cancellation token implemented
- ✅ HTTP cancellation working
- ✅ FS cancellation working
- ✅ Retry wait cancellation working
- ✅ Unit tests for cancellation scenarios
- ✅ Integration tests for cancellation handling

**Reference**: `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md#1-filesystem-block`, `#2-http-block`

---

### Task CP2.4: Queue Management

**Worker**: wrk-3  
**Priority**: High  
**Estimate**: 3 days

**Objective**: Implement bounded queues, rejection policies, and queue depth monitoring.

**Subtasks**:

1. **Bounded Queue Implementation**:
   - Add `max_queue_size` configuration per resource pool
   - Queue rejection when queue is full
   - Rejection policy: `reject_new` (default) or `drop_oldest`

2. **Queue Depth Monitoring**:
   - Track queue depth per resource pool
   - Expose queue depth metrics
   - Queue depth alerts (configurable thresholds)

3. **Queue Rejection Handling**:
   - Return `ExecAssignmentAck` with `status: "rejected"` when queue full
   - Include rejection reason in ACK message
   - Metrics for queue rejections

**Artifacts**:
- `apps/caf/processor/src/runtime/actor_pools.hpp` - Bounded queue implementation
- `apps/caf/processor/src/runtime/queue_manager.hpp` - Queue management
- `apps/caf/processor/tests/test_queue_management.cpp` - Queue tests
- `docs/archive/dev/WORKER_QUEUE_MANAGEMENT_IMPLEMENTATION.md` - Implementation report

**Acceptance Criteria**:
- ✅ Bounded queue size limits implemented
- ✅ Queue rejection when full
- ✅ Queue depth monitoring working
- ✅ Queue depth metrics exposed
- ✅ Unit tests for queue management
- ✅ Integration tests for queue rejection

**Reference**: `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md#3-runtime-management`

---

### Task CP2.5: Circuit Breaker Patterns

**Worker**: wrk-3  
**Priority**: Medium  
**Estimate**: 4 days

**Objective**: Implement circuit breaker patterns for external services (HTTP endpoints, file systems).

**Subtasks**:

1. **Circuit Breaker Implementation**:
   - Circuit states: `closed`, `open`, `half_open`
   - Failure threshold: Open circuit after N consecutive failures
   - Recovery timeout: Attempt recovery after timeout period
   - Success threshold: Close circuit after M consecutive successes

2. **HTTP Circuit Breaker**:
   - Per-endpoint circuit breaker
   - Failure detection: 5xx errors, timeouts
   - Automatic service degradation

3. **FS Circuit Breaker**:
   - Per-path circuit breaker (optional)
   - Failure detection: Permission denied, disk full
   - Automatic fallback to alternative storage

**Artifacts**:
- `apps/caf/processor/include/beamline/worker/circuit_breaker.hpp` - Circuit breaker interface
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP circuit breaker
- `apps/caf/processor/src/blocks/fs_block.cpp` - FS circuit breaker
- `apps/caf/processor/tests/test_circuit_breaker.cpp` - Circuit breaker tests
- `docs/archive/dev/WORKER_CIRCUIT_BREAKER_IMPLEMENTATION.md` - Implementation report

**Acceptance Criteria**:
- ✅ Circuit breaker implemented
- ✅ HTTP circuit breaker working
- ✅ FS circuit breaker working (optional)
- ✅ Unit tests for circuit breaker
- ✅ Integration tests for circuit breaker scenarios

---

### Task CP2.6: Load Testing & Chaos Engineering

**Worker**: wrk-3  
**Priority**: High  
**Estimate**: 5 days

**Objective**: Verify Worker reliability under high load and failure scenarios.

**Subtasks**:

1. **Load Testing**:
   - High throughput tests (100-500 tasks/sec)
   - Large payload tests (10-100 MB)
   - Mixed workload tests
   - Performance metrics collection

2. **Chaos Engineering**:
   - Latency injection for FS/HTTP operations
   - Failure injection (network errors, disk errors)
   - Recovery testing after failures
   - Stability verification

3. **Graceful Degradation Testing**:
   - Queue overflow scenarios
   - Resource exhaustion scenarios
   - Backpressure propagation
   - Degradation under load

**Artifacts**:
- `apps/caf/processor/tests/test_load_performance.cpp` - Load tests
- `apps/caf/processor/tests/test_chaos_fs_http.cpp` - Chaos tests
- `apps/caf/processor/tests/test_backpressure.cpp` - Backpressure tests
- `apps/caf/processor/tests/test_graceful_degradation.cpp` - Degradation tests
- `scripts/run_worker_load_tests.sh` - Load test script
- `scripts/run_worker_chaos_tests.sh` - Chaos test script
- `docs/archive/dev/WORKER_LOAD_TESTS_REPORT.md` - Load test report
- `docs/archive/dev/WORKER_CHAOS_TESTS_REPORT.md` - Chaos test report

**Acceptance Criteria**:
- ✅ Throughput ≥ 200 tasks/sec (CP2 baseline)
- ✅ p95 latency < 1s for small payloads
- ✅ p95 latency < 5s for large payloads (100 MB)
- ✅ Worker doesn't crash under chaos scenarios
- ✅ Graceful degradation working
- ✅ Backpressure working correctly

**Reference**: `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` (existing plan, to be updated)

---

## CP2 Acceptance Criteria

### Functional Requirements

- ✅ Advanced retry policies implemented (exponential backoff, jitter, error classification)
- ✅ Complete timeout enforcement (FS, HTTP connection, total timeout)
- ✅ Full cancellation support (HTTP, FS, retry wait)
- ✅ Bounded queue management (queue limits, rejection policies)
- ✅ Circuit breaker patterns implemented (HTTP, FS)
- ✅ Load testing and chaos engineering completed

### Non-Functional Requirements

- ✅ **Stability**: Worker runs without crashes under high load and failures
- ✅ **Predictability**: Retry policies are consistent and reliable
- ✅ **Observability**: Queue depth, retry metrics, circuit breaker state exposed
- ✅ **Performance**: Throughput ≥ 200 tasks/sec, p95 latency < 1s (small payloads)

### Test Coverage

- ✅ Retry policy tests (exponential backoff, jitter, error classification)
- ✅ Timeout enforcement tests (FS, HTTP, total timeout)
- ✅ Cancellation tests (HTTP, FS, retry wait)
- ✅ Queue management tests (bounded queues, rejection)
- ✅ Circuit breaker tests (HTTP, FS)
- ✅ Load tests (throughput, latency, memory)
- ✅ Chaos tests (latency injection, failure injection)

---

## Dependencies

### Internal

- ✅ CP1 completed (contracts, observability, basic tests)
- ✅ `test_block_executor.cpp` - Existing tests
- ✅ `BLOCKS_RELIABILITY_REVIEW.md` - Identified issues
- ✅ Contracts (StepResult → ExecResult)

### External

- Router must support backpressure signals (for CP2.6)
- NATS must be available for integration tests

---

## Risks and Mitigations

### Risk 1: Performance Degradation

**Risk**: Advanced retry policies and cancellation may add overhead.

**Mitigation**: 
- Performance benchmarks before and after implementation
- Profiling to identify bottlenecks
- Optimize hot paths

### Risk 2: Complexity Increase

**Risk**: Additional reliability features increase code complexity.

**Mitigation**:
- Clear separation of concerns (retry policy, cancellation token, circuit breaker)
- Comprehensive unit tests
- Code reviews focused on maintainability

### Risk 3: Integration Issues

**Risk**: Circuit breaker and cancellation may affect Router integration.

**Mitigation**:
- Integration tests with Router
- Backward compatibility verification
- Coordination with Router team

---

## References

### CP1 Documentation
- `docs/archive/dev/CP1_WORKER_FAILURE_MODEL.md` - CP1 failure model and gap analysis
- `docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md` - Detailed reliability review
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md` - Worker architecture and requirements

### Implementation Files
- `apps/caf/processor/src/worker_actor.cpp` - Retry logic implementation
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP block implementation
- `apps/caf/processor/src/blocks/fs_block.cpp` - FS block implementation
- `apps/caf/processor/src/runtime/actor_pools.hpp` - Backpressure implementation

### Acceptance Documents
- `docs/archive/dev/CP1_ACCEPTANCE_REPORT.md` - CP1 acceptance criteria
- `apps/otp/router/docs/API_CONTRACTS.md` - ExecResult contract specification

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Worker Reliability Plan
- Advanced retry policies specification
- Complete timeout enforcement specification
- Cancellation support specification
- Queue management specification
- Circuit breaker patterns specification
- Load testing and chaos engineering specification

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Planning Document
