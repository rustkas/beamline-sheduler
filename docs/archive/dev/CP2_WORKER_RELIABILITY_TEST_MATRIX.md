# CP2 Worker Reliability Test Matrix

**Version**: 1.1  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-3 (Worker Reliability)  
**Status**: 📋 **TEST MATRIX** (CP2)

---

## Executive Summary

This document defines the **formal test matrix** for CP2 Worker reliability testing (load testing and chaos engineering). The matrix maps test scenarios to input parameters, expected results, metrics, and error code/status mappings.

**Reference**: `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - CP2 Worker Reliability Plan (Task CP2.6)

---

## Test Matrix Structure

Each test scenario is defined with:
- **Scenario ID**: Unique identifier (e.g., `LOAD-001`, `CHAOS-001`)
- **Scenario Name**: Human-readable description
- **Input Parameters**: Configuration and test data
- **Expected Result**: Expected StepStatus and ErrorCode
- **Metrics**: Performance and reliability metrics to collect
- **StepStatus Mapping**: Expected `StepStatus` enum value
- **ErrorCode Mapping**: Expected `ErrorCode` enum value (if applicable)
- **Test Target / Ticket**: Test binary/suite and backlog ticket implementing this scenario

---

## StepStatus Reference

**StepStatus Enum** (from `apps/caf/processor/include/beamline/worker/core.hpp`):
- `StepStatus::ok` → `ExecResult.status = "success"`
- `StepStatus::error` → `ExecResult.status = "error"`
- `StepStatus::timeout` → `ExecResult.status = "timeout"`
- `StepStatus::cancelled` → `ExecResult.status = "cancelled"`

---

## ErrorCode Reference

**ErrorCode Enum** (from `apps/caf/processor/include/beamline/worker/core.hpp`):

### 1xxx: Input Validation Errors
- `1001`: `ErrorCode::invalid_input`
- `1002`: `ErrorCode::missing_required_field`
- `1003`: `ErrorCode::invalid_format`

### 2xxx: Execution Errors
- `2001`: `ErrorCode::execution_failed`
- `2002`: `ErrorCode::resource_unavailable`
- `2003`: `ErrorCode::permission_denied`
- `2004`: `ErrorCode::quota_exceeded`

### 3xxx: Network Errors
- `3001`: `ErrorCode::network_error`
- `3002`: `ErrorCode::connection_timeout`
- `3003`: `ErrorCode::http_error`

### 4xxx: System Errors
- `4001`: `ErrorCode::internal_error`
- `4002`: `ErrorCode::system_overload`

### 5xxx: Cancellation
- `5001`: `ErrorCode::cancelled_by_user`
- `5002`: `ErrorCode::cancelled_by_timeout`

---

## Load Testing Scenarios

### LOAD-001: High Throughput Baseline

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `LOAD-001` |
| **Scenario Name** | High Throughput Baseline |
| **Input Parameters** | - Throughput: 200 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Retry count: 0<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (100% success rate)<br>- ErrorCode: `none`<br>- Throughput: ≥ 200 tasks/sec<br>- p95 latency: < 1s<br>- p99 latency: < 2s |
| **Metrics** | - Throughput (tasks/sec)<br>- Latency distribution (p50, p95, p99, max)<br>- Success rate (%)<br>- Memory usage (MB)<br>- CPU usage (%)<br>- Queue depth |
| **StepStatus Mapping** | `StepStatus::ok` |
| **ErrorCode Mapping** | `ErrorCode::none` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_load_performance.cpp`<br>Ticket: **W3-6.1** (Load Testing Infrastructure) |

---

### LOAD-002: High Throughput Stress

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `LOAD-002` |
| **Scenario Name** | High Throughput Stress |
| **Input Parameters** | - Throughput: 500 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Retry count: 0<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (≥ 95% success rate)<br>- ErrorCode: `none` (success) or `4002` (system_overload) for failures<br>- Throughput: ≥ 400 tasks/sec sustained<br>- p95 latency: < 2s<br>- Worker doesn't crash |
| **Metrics** | - Throughput (tasks/sec)<br>- Latency distribution (p50, p95, p99, max)<br>- Success rate (%)<br>- Error rate (%)<br>- Memory usage (MB)<br>- CPU usage (%)<br>- Queue depth<br>- Rejection count |
| **StepStatus Mapping** | `StepStatus::ok` (success) or `StepStatus::error` (overload) |
| **ErrorCode Mapping** | `ErrorCode::none` (success) or `ErrorCode::system_overload` (overload) |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_load_performance.cpp`<br>Ticket: **W3-6.1** (Load Testing Infrastructure) |

---

### LOAD-003: Large Payload Baseline

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `LOAD-003` |
| **Scenario Name** | Large Payload Baseline |
| **Input Parameters** | - Throughput: 10 tasks/sec<br>- Payload size: 10 MB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Retry count: 0<br>- Timeout: 60s |
| **Expected Result** | - StepStatus: `ok` (100% success rate)<br>- ErrorCode: `none`<br>- p95 latency: < 5s<br>- p99 latency: < 10s |
| **Metrics** | - Latency distribution (p50, p95, p99, max)<br>- Success rate (%)<br>- Memory usage (MB)<br>- Network throughput (MB/s) |
| **StepStatus Mapping** | `StepStatus::ok` |
| **ErrorCode Mapping** | `ErrorCode::none` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_load_performance.cpp`<br>Ticket: **W3-6.1** (Load Testing Infrastructure) |

---

### LOAD-004: Large Payload Stress

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `LOAD-004` |
| **Scenario Name** | Large Payload Stress |
| **Input Parameters** | - Throughput: 50 tasks/sec<br>- Payload size: 100 MB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Retry count: 0<br>- Timeout: 120s |
| **Expected Result** | - StepStatus: `ok` (≥ 90% success rate)<br>- ErrorCode: `none` (success) or `2002` (resource_unavailable) for failures<br>- p95 latency: < 30s<br>- Worker doesn't crash |
| **Metrics** | - Latency distribution (p50, p95, p99, max)<br>- Success rate (%)<br>- Error rate (%)<br>- Memory usage (MB)<br>- Network throughput (MB/s) |
| **StepStatus Mapping** | `StepStatus::ok` (success) or `StepStatus::error` (resource unavailable) |
| **ErrorCode Mapping** | `ErrorCode::none` (success) or `ErrorCode::resource_unavailable` (failure) |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_load_performance.cpp`<br>Ticket: **W3-6.1** (Load Testing Infrastructure) |

---

### LOAD-005: Mixed Workload

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `LOAD-005` |
| **Scenario Name** | Mixed Workload |
| **Input Parameters** | - Throughput: 100 tasks/sec<br>- Payload sizes: 1 KB (50%), 10 KB (30%), 1 MB (20%)<br>- Duration: 120 seconds<br>- Block types: HTTP (70%), FS (30%)<br>- Retry count: 2<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (≥ 95% success rate)<br>- ErrorCode: `none` (success)<br>- Throughput: ≥ 90 tasks/sec sustained<br>- p95 latency: < 2s |
| **Metrics** | - Throughput (tasks/sec)<br>- Latency distribution (p50, p95, p99, max)<br>- Success rate (%)<br>- Memory usage (MB)<br>- CPU usage (%)<br>- Queue depth |
| **StepStatus Mapping** | `StepStatus::ok` |
| **ErrorCode Mapping** | `ErrorCode::none` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_load_performance.cpp`<br>Ticket: **W3-6.1** (Load Testing Infrastructure) |

---

### LOAD-006: Queue Overflow

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `LOAD-006` |
| **Scenario Name** | Queue Overflow |
| **Input Parameters** | - Throughput: 1000 tasks/sec (burst)<br>- Payload size: 1 KB<br>- Duration: 10 seconds<br>- Block type: HTTP<br>- Max queue size: 100<br>- Retry count: 0<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `error` (for rejected tasks)<br>- ErrorCode: `4002` (system_overload)<br>- Queue rejections: > 0<br>- Rejection rate: < 10% (graceful degradation)<br>- Worker doesn't crash |
| **Metrics** | - Queue depth (max, avg)<br>- Rejection count<br>- Rejection rate (%)<br>- Success rate (%)<br>- Memory usage (MB) |
| **StepStatus Mapping** | `StepStatus::error` (for rejected tasks) |
| **ErrorCode Mapping** | `ErrorCode::system_overload` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_load_performance.cpp`<br>Ticket: **W3-6.1** (Load Testing Infrastructure) |

---

### LOAD-007: Resource Exhaustion

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `LOAD-007` |
| **Scenario Name** | Resource Exhaustion |
| **Input Parameters** | - Throughput: 200 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 300 seconds<br>- Block type: HTTP<br>- Max concurrency: 10<br>- Retry count: 3<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (≥ 90% success rate) or `error` (resource unavailable)<br>- ErrorCode: `none` (success) or `2002` (resource_unavailable)<br>- Queue depth: Bounded by max_queue_size<br>- Worker doesn't crash |
| **Metrics** | - Queue depth (max, avg)<br>- Active tasks (max, avg)<br>- Success rate (%)<br>- Error rate (%)<br>- Memory usage (MB)<br>- CPU usage (%) |
| **StepStatus Mapping** | `StepStatus::ok` (success) or `StepStatus::error` (resource unavailable) |
| **ErrorCode Mapping** | `ErrorCode::none` (success) or `ErrorCode::resource_unavailable` (failure) |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_load_performance.cpp`<br>Ticket: **W3-6.1** (Load Testing Infrastructure) |

---

## Chaos Engineering Scenarios

### CHAOS-001: Latency Injection - HTTP

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `CHAOS-001` |
| **Scenario Name** | Latency Injection - HTTP |
| **Input Parameters** | - Throughput: 50 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Latency injection: 500ms (50% of requests)<br>- Retry count: 2<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (≥ 90% success rate) or `timeout` (if timeout exceeded)<br>- ErrorCode: `none` (success) or `5002` (cancelled_by_timeout)<br>- Retry attempts: 0-2 per task<br>- p95 latency: < 2s (with retries) |
| **Metrics** | - Latency distribution (p50, p95, p99, max)<br>- Success rate (%)<br>- Timeout rate (%)<br>- Retry count (avg, max)<br>- Retry success rate (%) |
| **StepStatus Mapping** | `StepStatus::ok` (success) or `StepStatus::timeout` (timeout) |
| **ErrorCode Mapping** | `ErrorCode::none` (success) or `ErrorCode::cancelled_by_timeout` (timeout) |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_chaos_fs_http.cpp`<br>Ticket: **W3-6.2** (Chaos Engineering Infrastructure) |

---

### CHAOS-002: Latency Injection - FS

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `CHAOS-002` |
| **Scenario Name** | Latency Injection - FS |
| **Input Parameters** | - Throughput: 20 tasks/sec<br>- Payload size: 1 MB<br>- Duration: 60 seconds<br>- Block type: FS (write)<br>- Latency injection: 1s (30% of operations)<br>- Retry count: 2<br>- Timeout: 5s |
| **Expected Result** | - StepStatus: `ok` (≥ 85% success rate) or `timeout` (if timeout exceeded)<br>- ErrorCode: `none` (success) or `5002` (cancelled_by_timeout)<br>- Retry attempts: 0-2 per task |
| **Metrics** | - Latency distribution (p50, p95, p99, max)<br>- Success rate (%)<br>- Timeout rate (%)<br>- Retry count (avg, max) |
| **StepStatus Mapping** | `StepStatus::ok` (success) or `StepStatus::timeout` (timeout) |
| **ErrorCode Mapping** | `ErrorCode::none` (success) or `ErrorCode::cancelled_by_timeout` (timeout) |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_chaos_fs_http.cpp`<br>Ticket: **W3-6.2** (Chaos Engineering Infrastructure) |

---

### CHAOS-003: Network Failure Injection

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `CHAOS-003` |
| **Scenario Name** | Network Failure Injection |
| **Input Parameters** | - Throughput: 50 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Failure injection: Connection refused (10% of requests)<br>- Retry count: 3<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `error` (for network failures) or `ok` (after retry)<br>- ErrorCode: `3001` (network_error) or `none` (success after retry)<br>- Retry attempts: 1-3 per failed task<br>- Success rate: ≥ 85% (with retries) |
| **Metrics** | - Success rate (%)<br>- Error rate (%)<br>- Retry count (avg, max)<br>- Retry success rate (%)<br>- Network error count |
| **StepStatus Mapping** | `StepStatus::ok` (success after retry) or `StepStatus::error` (network failure) |
| **ErrorCode Mapping** | `ErrorCode::none` (success) or `ErrorCode::network_error` (network failure) |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_chaos_fs_http.cpp`<br>Ticket: **W3-6.2** (Chaos Engineering Infrastructure) |

---

### CHAOS-004: HTTP Error Injection (5xx)

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `CHAOS-004` |
| **Scenario Name** | HTTP Error Injection (5xx) |
| **Input Parameters** | - Throughput: 50 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Failure injection: HTTP 500 (15% of requests)<br>- Retry count: 3<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (after retry) or `error` (if all retries fail)<br>- ErrorCode: `none` (success) or `3003` (http_error)<br>- Retry attempts: 1-3 per failed task<br>- Success rate: ≥ 80% (with retries) |
| **Metrics** | - Success rate (%)<br>- Error rate (%)<br>- Retry count (avg, max)<br>- Retry success rate (%)<br>- HTTP 5xx error count |
| **StepStatus Mapping** | `StepStatus::ok` (success after retry) or `StepStatus::error` (HTTP error) |
| **ErrorCode Mapping** | `ErrorCode::none` (success) or `ErrorCode::http_error` (HTTP error) |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_chaos_fs_http.cpp`<br>Ticket: **W3-6.2** (Chaos Engineering Infrastructure) |

---

### CHAOS-005: HTTP Error Injection (4xx)

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `CHAOS-005` |
| **Scenario Name** | HTTP Error Injection (4xx) |
| **Input Parameters** | - Throughput: 50 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Failure injection: HTTP 400 (10% of requests)<br>- Retry count: 3<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `error` (4xx errors are non-retryable)<br>- ErrorCode: `3003` (http_error)<br>- Retry attempts: 0 (4xx errors should not be retried)<br>- Success rate: ≥ 90% |
| **Metrics** | - Success rate (%)<br>- Error rate (%)<br>- Retry count (avg, max)<br>- HTTP 4xx error count |
| **StepStatus Mapping** | `StepStatus::error` |
| **ErrorCode Mapping** | `ErrorCode::http_error` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_chaos_fs_http.cpp`<br>Ticket: **W3-6.2** (Chaos Engineering Infrastructure) |

---

### CHAOS-006: Disk Error Injection

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `CHAOS-006` |
| **Scenario Name** | Disk Error Injection |
| **Input Parameters** | - Throughput: 20 tasks/sec<br>- Payload size: 1 MB<br>- Duration: 60 seconds<br>- Block type: FS (write)<br>- Failure injection: Permission denied (5% of operations)<br>- Retry count: 2<br>- Timeout: 5s |
| **Expected Result** | - StepStatus: `error` (permission denied is non-retryable)<br>- ErrorCode: `2003` (permission_denied)<br>- Retry attempts: 0 (permission errors should not be retried)<br>- Success rate: ≥ 95% |
| **Metrics** | - Success rate (%)<br>- Error rate (%)<br>- Retry count (avg, max)<br>- Permission denied count |
| **StepStatus Mapping** | `StepStatus::error` |
| **ErrorCode Mapping** | `ErrorCode::permission_denied` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_chaos_fs_http.cpp`<br>Ticket: **W3-6.2** (Chaos Engineering Infrastructure) |

---

### CHAOS-007: Connection Timeout Injection

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `CHAOS-007` |
| **Scenario Name** | Connection Timeout Injection |
| **Input Parameters** | - Throughput: 50 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Failure injection: Connection timeout (20% of requests)<br>- Connection timeout: 5s<br>- Retry count: 3<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (after retry) or `timeout` (if timeout exceeded)<br>- ErrorCode: `none` (success) or `3002` (connection_timeout) or `5002` (cancelled_by_timeout)<br>- Retry attempts: 1-3 per failed task<br>- Success rate: ≥ 75% (with retries) |
| **Metrics** | - Success rate (%)<br>- Timeout rate (%)<br>- Retry count (avg, max)<br>- Retry success rate (%)<br>- Connection timeout count |
| **StepStatus Mapping** | `StepStatus::ok` (success) or `StepStatus::timeout` (timeout) |
| **ErrorCode Mapping** | `ErrorCode::none` (success) or `ErrorCode::connection_timeout` (connection timeout) or `ErrorCode::cancelled_by_timeout` (total timeout) |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_chaos_fs_http.cpp`<br>Ticket: **W3-6.2** (Chaos Engineering Infrastructure) |

---

### CHAOS-008: Recovery Testing

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `CHAOS-008` |
| **Scenario Name** | Recovery Testing |
| **Input Parameters** | - Throughput: 50 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 120 seconds<br>- Block type: HTTP<br>- Failure injection: Network failure (0-60s), then recovery (60-120s)<br>- Retry count: 3<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (after recovery)<br>- ErrorCode: `none` (success)<br>- Success rate: ≥ 95% (after recovery period)<br>- Recovery time: < 10s<br>- Worker doesn't crash |
| **Metrics** | - Success rate by time period (%)<br>- Recovery time (seconds)<br>- Error rate by time period (%)<br>- Retry count (avg, max) |
| **StepStatus Mapping** | `StepStatus::ok` (after recovery) |
| **ErrorCode Mapping** | `ErrorCode::none` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_chaos_fs_http.cpp`<br>Ticket: **W3-6.2** (Chaos Engineering Infrastructure) |

---

### CHAOS-009: Circuit Breaker Activation

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `CHAOS-009` |
| **Scenario Name** | Circuit Breaker Activation |
| **Input Parameters** | - Throughput: 50 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 120 seconds<br>- Block type: HTTP<br>- Failure injection: HTTP 500 (80% of requests, first 30s)<br>- Circuit breaker: Failure threshold = 5, Recovery timeout = 10s<br>- Retry count: 3<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `error` (circuit open) or `ok` (circuit closed/recovered)<br>- ErrorCode: `3003` (http_error) or `none` (success)<br>- Circuit breaker state: `open` → `half_open` → `closed`<br>- Success rate: ≥ 80% (after recovery) |
| **Metrics** | - Circuit breaker state transitions<br>- Success rate by time period (%)<br>- Error rate by time period (%)<br>- Circuit open duration (seconds) |
| **StepStatus Mapping** | `StepStatus::error` (circuit open) or `StepStatus::ok` (circuit closed) |
| **ErrorCode Mapping** | `ErrorCode::http_error` (circuit open) or `ErrorCode::none` (success) |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_chaos_fs_http.cpp`<br>Ticket: **W3-6.2** (Chaos Engineering Infrastructure) |

---

## Graceful Degradation Scenarios

### DEGRADE-001: Queue Overflow with Rejection

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `DEGRADE-001` |
| **Scenario Name** | Queue Overflow with Rejection |
| **Input Parameters** | - Throughput: 1000 tasks/sec (burst)<br>- Payload size: 1 KB<br>- Duration: 10 seconds<br>- Block type: HTTP<br>- Max queue size: 100<br>- Rejection policy: `reject_new`<br>- Retry count: 0<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `error` (for rejected tasks)<br>- ErrorCode: `4002` (system_overload)<br>- Queue rejections: > 0<br>- Rejection rate: < 10%<br>- Worker doesn't crash<br>- Accepted tasks: ≥ 90% success rate |
| **Metrics** | - Queue depth (max, avg)<br>- Rejection count<br>- Rejection rate (%)<br>- Success rate for accepted tasks (%)<br>- Memory usage (MB) |
| **StepStatus Mapping** | `StepStatus::error` (for rejected tasks) |
| **ErrorCode Mapping** | `ErrorCode::system_overload` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_graceful_degradation.cpp`<br>Ticket: **W3-6.3** (Graceful Degradation Testing) |

---

### DEGRADE-002: Backpressure Propagation

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `DEGRADE-002` |
| **Scenario Name** | Backpressure Propagation |
| **Input Parameters** | - Throughput: 200 tasks/sec<br>- Payload size: 1 KB<br>- Duration: 60 seconds<br>- Block type: HTTP<br>- Max concurrency: 10<br>- Backpressure signal: Enabled<br>- Retry count: 0<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (≥ 90% success rate)<br>- ErrorCode: `none` (success)<br>- Queue depth: Bounded by max_queue_size<br>- Backpressure signals: Sent to Router<br>- Worker doesn't crash |
| **Metrics** | - Queue depth (max, avg)<br>- Active tasks (max, avg)<br>- Backpressure signal count<br>- Success rate (%)<br>- Memory usage (MB) |
| **StepStatus Mapping** | `StepStatus::ok` |
| **ErrorCode Mapping** | `ErrorCode::none` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_backpressure.cpp`<br>Ticket: **W3-6.3** (Graceful Degradation Testing) |

---

### DEGRADE-003: Resource Exhaustion Recovery

| **Field** | **Value** |
|-----------|-----------|
| **Scenario ID** | `DEGRADE-003` |
| **Scenario Name** | Resource Exhaustion Recovery |
| **Input Parameters** | - Throughput: 200 tasks/sec (0-30s), 50 tasks/sec (30-90s)<br>- Payload size: 1 KB<br>- Duration: 90 seconds<br>- Block type: HTTP<br>- Max concurrency: 10<br>- Retry count: 0<br>- Timeout: 30s |
| **Expected Result** | - StepStatus: `ok` (≥ 95% success rate after recovery)<br>- ErrorCode: `none` (success)<br>- Queue depth: Returns to normal after load reduction<br>- Recovery time: < 10s<br>- Worker doesn't crash |
| **Metrics** | - Queue depth by time period (max, avg)<br>- Success rate by time period (%)<br>- Recovery time (seconds)<br>- Memory usage (MB) |
| **StepStatus Mapping** | `StepStatus::ok` |
| **ErrorCode Mapping** | `ErrorCode::none` |
| **Test Target / Ticket** | `apps/caf/processor/tests/test_graceful_degradation.cpp`<br>Ticket: **W3-6.3** (Graceful Degradation Testing) |

---

## Test Execution Summary

### Load Testing (7 scenarios)
- `LOAD-001`: High Throughput Baseline
- `LOAD-002`: High Throughput Stress
- `LOAD-003`: Large Payload Baseline
- `LOAD-004`: Large Payload Stress
- `LOAD-005`: Mixed Workload
- `LOAD-006`: Queue Overflow
- `LOAD-007`: Resource Exhaustion

### Chaos Engineering (9 scenarios)
- `CHAOS-001`: Latency Injection - HTTP
- `CHAOS-002`: Latency Injection - FS
- `CHAOS-003`: Network Failure Injection
- `CHAOS-004`: HTTP Error Injection (5xx)
- `CHAOS-005`: HTTP Error Injection (4xx)
- `CHAOS-006`: Disk Error Injection
- `CHAOS-007`: Connection Timeout Injection
- `CHAOS-008`: Recovery Testing
- `CHAOS-009`: Circuit Breaker Activation

### Graceful Degradation (3 scenarios)
- `DEGRADE-001`: Queue Overflow with Rejection
- `DEGRADE-002`: Backpressure Propagation
- `DEGRADE-003`: Resource Exhaustion Recovery

**Total**: 19 test scenarios

---

## Acceptance Criteria

### Performance Requirements
- ✅ Throughput: ≥ 200 tasks/sec (CP2 baseline)
- ✅ p95 latency: < 1s (small payloads, 1 KB)
- ✅ p95 latency: < 5s (large payloads, 100 MB)
- ✅ Success rate: ≥ 95% (baseline scenarios)
- ✅ Success rate: ≥ 90% (stress scenarios)

### Reliability Requirements
- ✅ Worker doesn't crash under any scenario
- ✅ Graceful degradation under overload
- ✅ Recovery after failures within 10s
- ✅ Queue depth bounded by max_queue_size
- ✅ Backpressure signals sent correctly

### Error Handling Requirements
- ✅ Retryable errors: Network errors, timeouts, 5xx HTTP errors
- ✅ Non-retryable errors: 4xx HTTP errors (except 429), permission denied
- ✅ Error codes correctly mapped to StepStatus
- ✅ Cancellation support working (HTTP, FS, retry wait)

---

## Test Implementation

### Test Files and Ticket Mapping

| **Test File** | **Scenarios** | **Backlog Ticket** | **Description** |
|---------------|---------------|---------------------|-----------------|
| `apps/caf/processor/tests/test_load_performance.cpp` | `LOAD-001` through `LOAD-007` (7 scenarios) | **W3-6.1** (Load Testing Infrastructure) | High throughput, large payload, mixed workload, queue overflow, resource exhaustion tests |
| `apps/caf/processor/tests/test_chaos_fs_http.cpp` | `CHAOS-001` through `CHAOS-009` (9 scenarios) | **W3-6.2** (Chaos Engineering Infrastructure) | Latency injection, failure injection, recovery testing, circuit breaker activation |
| `apps/caf/processor/tests/test_backpressure.cpp` | `DEGRADE-002` (1 scenario) | **W3-6.3** (Graceful Degradation Testing) | Backpressure propagation tests |
| `apps/caf/processor/tests/test_graceful_degradation.cpp` | `DEGRADE-001`, `DEGRADE-003` (2 scenarios) | **W3-6.3** (Graceful Degradation Testing) | Queue overflow with rejection, resource exhaustion recovery |

### Scenario-to-Ticket Summary

**W3-6.1: Load Testing Infrastructure** (7 scenarios):
- `LOAD-001`: High Throughput Baseline
- `LOAD-002`: High Throughput Stress
- `LOAD-003`: Large Payload Baseline
- `LOAD-004`: Large Payload Stress
- `LOAD-005`: Mixed Workload
- `LOAD-006`: Queue Overflow
- `LOAD-007`: Resource Exhaustion

**W3-6.2: Chaos Engineering Infrastructure** (9 scenarios):
- `CHAOS-001`: Latency Injection - HTTP
- `CHAOS-002`: Latency Injection - FS
- `CHAOS-003`: Network Failure Injection
- `CHAOS-004`: HTTP Error Injection (5xx)
- `CHAOS-005`: HTTP Error Injection (4xx)
- `CHAOS-006`: Disk Error Injection
- `CHAOS-007`: Connection Timeout Injection
- `CHAOS-008`: Recovery Testing
- `CHAOS-009`: Circuit Breaker Activation

**W3-6.3: Graceful Degradation Testing** (3 scenarios):
- `DEGRADE-001`: Queue Overflow with Rejection → `test_graceful_degradation.cpp`
- `DEGRADE-002`: Backpressure Propagation → `test_backpressure.cpp`
- `DEGRADE-003`: Resource Exhaustion Recovery → `test_graceful_degradation.cpp`

### Test Scripts
- `scripts/run_worker_load_tests.sh` - Load test script (executes `test_load_performance.cpp`)
- `scripts/run_worker_chaos_tests.sh` - Chaos test script (executes `test_chaos_fs_http.cpp`)
- `scripts/run_worker_degradation_tests.sh` - Degradation test script (executes `test_backpressure.cpp` and `test_graceful_degradation.cpp`)

### Test Reports
- `docs/archive/dev/WORKER_LOAD_TESTS_REPORT.md` - Load test report (W3-6.1)
- `docs/archive/dev/WORKER_CHAOS_TESTS_REPORT.md` - Chaos test report (W3-6.2)
- `docs/archive/dev/WORKER_DEGRADATION_TESTS_REPORT.md` - Degradation test report (W3-6.3)

---

## References

### CP2 Documentation
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - CP2 Worker Reliability Plan
- `docs/archive/dev/CP1_WORKER_FAILURE_MODEL.md` - CP1 failure model and gap analysis

### Implementation Files
- `apps/caf/processor/include/beamline/worker/core.hpp` - StepStatus, ErrorCode definitions
- `apps/caf/processor/include/beamline/worker/result_converter.hpp` - Result conversion
- `apps/caf/processor/src/worker_actor.cpp` - Retry logic implementation
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP block implementation
- `apps/caf/processor/src/blocks/fs_block.cpp` - FS block implementation

### Router Test Examples
- `apps/otp/router/test/router_intake_chaos_SUITE.erl` - Router chaos tests
- `apps/otp/router/test/router_extensions_pipeline_load_SUITE.erl` - Router load tests

---

## Change History

**v1.1 (2025-01-27)**:
- Added **Test Target / Ticket** column to all test scenarios
- Added test file and ticket mapping table
- Added scenario-to-ticket summary section
- Updated Test Implementation section with detailed mapping

**v1.0 (2025-01-27)**:
- Initial CP2 Worker Reliability Test Matrix
- 19 test scenarios defined (7 load, 9 chaos, 3 degradation)
- StepStatus and ErrorCode mappings documented
- Acceptance criteria specified

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Test Matrix Document

