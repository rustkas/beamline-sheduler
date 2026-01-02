# CP2 Worker Retry v2 PoC Plan

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-3 (Worker Reliability)  
**Status**: 📋 **POC PLAN** (CP2)

---

## Executive Summary

This document defines a **minimal PoC (Proof of Concept)** for CP2 retry v2 implementation, focusing on core features that can be implemented without touching all blocks. The PoC validates the retry v2 design and feature gate mechanism before full implementation.

**Goal**: Validate retry v2 design with minimal scope (exponential backoff + basic error classification) under `worker.retries.v2.enabled` feature flag.

**Reference**: `docs/archive/dev/CP2_WORKER_RETRY_DESIGN.md` - Full retry v2 design

---

## PoC Scope

### In Scope (PoC)

**Core Features**:
1. **Exponential Backoff**: Replace fixed backoff with exponential backoff (`base * 2^attempt`)
2. **Basic Error Classification**: Classify network errors as retryable, validation errors as non-retryable
3. **Feature Gate**: `worker.retries.v2.enabled` feature flag implementation
4. **HTTP Block Only**: Focus on HTTP block for PoC (most common, highest impact)

**Why This Scope**:
- **Minimal Implementation**: Only HTTP block needs changes (no FS, SQL blocks)
- **High Impact**: HTTP errors are most common, exponential backoff provides immediate value
- **Low Risk**: Feature flag protects CP1 baseline, can be disabled if issues found
- **Fast Validation**: Can validate design in 1-2 days instead of full 4-day implementation

### Out of Scope (PoC)

**Deferred to Full Implementation**:
- ❌ Jitter (can be added later, exponential backoff alone provides value)
- ❌ HTTP status code parsing (complex, can use ErrorCode classification first)
- ❌ Retry budget management (can be added after PoC validation)
- ❌ FS block retry policies (can be added after PoC validation)
- ❌ SQL block retry policies (can be added after PoC validation)
- ❌ Per-block configuration (can use global defaults for PoC)
- ❌ Per-category configuration (can use simple ErrorCode mapping for PoC)

**Rationale**: These features add complexity but don't block PoC validation. Can be added incrementally after PoC proves the design.

---

## PoC Requirements

### Requirement 1: Exponential Backoff

**Scope**: Replace fixed backoff with exponential backoff for HTTP block retries.

**Implementation**:
- Formula: `backoff_ms = min(base_delay_ms * (2 ^ attempt), max_delay_ms)`
- Defaults: `base_delay_ms = 100ms`, `max_delay_ms = 30000ms` (30s)
- Configurable via environment variable or config file (optional for PoC)

**Acceptance**:
- ✅ Exponential backoff calculation correct
- ✅ Backoff capped at max_delay_ms
- ✅ Feature flag gates behavior (CP1 baseline preserved when disabled)

**Test Cases**:
- Attempt 0: `backoff = min(100 * 2^0, 30000) = 100ms`
- Attempt 1: `backoff = min(100 * 2^1, 30000) = 200ms`
- Attempt 2: `backoff = min(100 * 2^2, 30000) = 400ms`
- Attempt 8: `backoff = min(100 * 2^8, 30000) = 25600ms`
- Attempt 9: `backoff = min(100 * 2^9, 30000) = 30000ms` (capped)

### Requirement 2: Basic Error Classification

**Scope**: Classify errors as retryable vs non-retryable for HTTP block.

**Classification Rules** (Simplified for PoC):

| ErrorCode | Retryable | Reason |
|-----------|-----------|--------|
| `network_error` (3001) | ✅ Yes | Transient network failures |
| `connection_timeout` (3002) | ✅ Yes | Connection timeout (may be transient) |
| `http_error` (3003) | ⚠️ **Conditional** | Requires HTTP status code (deferred to full implementation) |
| `resource_unavailable` (2002) | ✅ Yes | Resource temporarily unavailable |
| `system_overload` (4002) | ✅ Yes | System overload (may recover) |
| `invalid_input` (1001) | ❌ No | Client error, retry won't help |
| `missing_required_field` (1002) | ❌ No | Client error, retry won't help |
| `invalid_format` (1003) | ❌ No | Client error, retry won't help |
| `permission_denied` (2003) | ❌ No | Permission denied (permanent) |
| `execution_failed` (2001) | ❌ No | Generic failure (default: no retry) |
| `internal_error` (4001) | ❌ No | Internal error (default: no retry) |

**HTTP Error (3003) Handling**:
- **PoC**: Treat `http_error` as **retryable** (conservative approach)
- **Full Implementation**: Parse HTTP status code from metadata/error message
- **Rationale**: HTTP status code parsing requires changes to HTTP block error reporting, can be added after PoC

**Acceptance**:
- ✅ Network errors classified as retryable
- ✅ Validation errors classified as non-retryable
- ✅ HTTP errors treated as retryable (PoC simplification)
- ✅ Feature flag gates behavior

**Test Cases**:
- `network_error` → Retry with exponential backoff
- `connection_timeout` → Retry with exponential backoff
- `http_error` → Retry with exponential backoff (PoC simplification)
- `invalid_input` → No retry
- `permission_denied` → No retry

### Requirement 3: Feature Gate

**Scope**: Implement `worker.retries.v2.enabled` feature flag.

**Implementation**:
- Environment variable: `WORKER_RETRIES_V2_ENABLED=true`
- Config file: `worker.retries.v2.enabled = true`
- Default: `false` (CP1 baseline)

**Behavior**:
- **When `false` (CP1 mode)**: Fixed backoff, retry all errors
- **When `true` (CP2 PoC mode)**: Exponential backoff, error classification

**Acceptance**:
- ✅ Feature flag check implemented
- ✅ CP1 baseline preserved when disabled
- ✅ CP2 PoC features enabled when enabled
- ✅ Can toggle feature flag without code changes

---

## PoC Implementation

### Code Changes

**Files to Modify**:

1. **`apps/caf/processor/include/beamline/worker/retry_policy.hpp`** (New):
   - `RetryPolicy` interface
   - `calculate_exponential_backoff()` function
   - `is_retryable_error()` function

2. **`apps/caf/processor/src/retry_policy.cpp`** (New):
   - Exponential backoff implementation
   - Error classification logic (simplified)

3. **`apps/caf/processor/src/worker_actor.cpp`** (Modify):
   - Add feature flag check
   - Use exponential backoff when enabled
   - Use error classification when enabled

**Minimal Changes**:
- Only HTTP block retry logic affected (no FS, SQL blocks)
- Feature flag check in `execute_with_retry()` method
- Error classification only for ErrorCode enum (no HTTP status parsing)

### Configuration

**Minimal Configuration** (PoC):

```json
{
  "worker": {
    "retries": {
      "v2": {
        "enabled": false,
        "default_policy": {
          "base_delay_ms": 100,
          "max_delay_ms": 30000
        }
      }
    }
  }
}
```

**Environment Variable** (Alternative):

```bash
export WORKER_RETRIES_V2_ENABLED=true
```

### Feature Gate Implementation

```cpp
bool is_retry_v2_enabled() {
    // Check environment variable
    auto env_value = std::getenv("WORKER_RETRIES_V2_ENABLED");
    if (env_value && std::string(env_value) == "true") {
        return true;
    }
    
    // Check config file
    auto config = load_worker_config();
    if (config.worker.retries.v2.enabled) {
        return true;
    }
    
    // Default: CP1 baseline
    return false;
}

caf::expected<StepResult> ExecutorActorState::execute_with_retry(const StepRequest& req) {
    if (is_retry_v2_enabled()) {
        // CP2 PoC: Exponential backoff + error classification
        return execute_with_retry_v2_poc(req);
    } else {
        // CP1 baseline: Fixed backoff, retry all errors
        return execute_with_retry_cp1(req);
    }
}

caf::expected<StepResult> execute_with_retry_v2_poc(const StepRequest& req) {
    StepResult final_result;
    auto start_time = std::chrono::steady_clock::now();
    
    for (int attempt = 0; attempt <= req.retry_count; attempt++) {
        auto result = execute_single_attempt(req);
        if (result) {
            final_result = *result;
            final_result.retries_used = attempt;
            
            if (final_result.status == StepStatus::ok) {
                return final_result;
            }
            
            // PoC: Basic error classification (no HTTP status parsing)
            if (!is_retryable_error_poc(final_result.error_code)) {
                // Don't retry non-retryable errors
                return final_result;
            }
        }
        
        // PoC: Exponential backoff (no jitter)
        if (attempt < req.retry_count) {
            int backoff_ms = calculate_exponential_backoff_poc(attempt, 100, 30000);
            std::this_thread::sleep_for(std::chrono::milliseconds(backoff_ms));
        }
    }
    
    return final_result;
}

bool is_retryable_error_poc(ErrorCode code) {
    switch (code) {
        // Retryable errors
        case ErrorCode::network_error:
        case ErrorCode::connection_timeout:
        case ErrorCode::http_error:  // PoC: Treat all HTTP errors as retryable
        case ErrorCode::resource_unavailable:
        case ErrorCode::system_overload:
            return true;
        
        // Non-retryable errors
        case ErrorCode::invalid_input:
        case ErrorCode::missing_required_field:
        case ErrorCode::invalid_format:
        case ErrorCode::execution_failed:
        case ErrorCode::permission_denied:
        case ErrorCode::quota_exceeded:
        case ErrorCode::internal_error:
        case ErrorCode::cancelled_by_user:
        case ErrorCode::cancelled_by_timeout:
            return false;
        
        default:
            return false;  // Conservative: don't retry unknown errors
    }
}

int calculate_exponential_backoff_poc(int attempt, int base_delay_ms, int max_delay_ms) {
    int backoff = base_delay_ms * (1 << attempt);  // 2^attempt
    return std::min(backoff, max_delay_ms);
}
```

---

## PoC Test Cases

### Test 1: Exponential Backoff Calculation

**Test**: Verify exponential backoff calculation.

**Steps**:
1. Enable feature flag: `WORKER_RETRIES_V2_ENABLED=true`
2. Execute HTTP block with `retry_count=3`
3. Inject network error on first attempt
4. Measure backoff delays between retries

**Expected**:
- Attempt 0 → Attempt 1: ~100ms delay
- Attempt 1 → Attempt 2: ~200ms delay
- Attempt 2 → Attempt 3: ~400ms delay

**Test File**: `apps/caf/processor/tests/test_retry_policy_poc.cpp`

### Test 2: Error Classification (Retryable)

**Test**: Verify retryable errors are retried with exponential backoff.

**Steps**:
1. Enable feature flag
2. Execute HTTP block with `retry_count=3`
3. Inject `network_error` on first attempt
4. Verify retry occurs with exponential backoff

**Expected**:
- Error classified as retryable
- Retry occurs with exponential backoff
- Final result shows `retries_used > 0`

**Test File**: `apps/caf/processor/tests/test_retry_policy_poc.cpp`

### Test 3: Error Classification (Non-Retryable)

**Test**: Verify non-retryable errors are not retried.

**Steps**:
1. Enable feature flag
2. Execute HTTP block with `retry_count=3`
3. Inject `invalid_input` error
4. Verify no retry occurs

**Expected**:
- Error classified as non-retryable
- No retry occurs (only one attempt)
- Final result shows `retries_used = 0`

**Test File**: `apps/caf/processor/tests/test_retry_policy_poc.cpp`

### Test 4: Feature Flag Disabled (CP1 Baseline)

**Test**: Verify CP1 baseline behavior when feature flag disabled.

**Steps**:
1. Disable feature flag: `WORKER_RETRIES_V2_ENABLED=false` (or unset)
2. Execute HTTP block with `retry_count=3`
3. Inject network error on first attempt
4. Measure backoff delays

**Expected**:
- Fixed backoff: 100ms, 200ms, 300ms
- All errors retried (no classification)
- CP1 baseline behavior preserved

**Test File**: `apps/caf/processor/tests/test_retry_policy_poc.cpp`

### Test 5: Feature Flag Toggle

**Test**: Verify feature flag can be toggled without code changes.

**Steps**:
1. Run test with feature flag disabled → Verify CP1 behavior
2. Run same test with feature flag enabled → Verify CP2 PoC behavior
3. Verify no crashes or errors when toggling

**Expected**:
- Feature flag toggle works correctly
- No crashes or errors
- Behavior changes as expected

**Test File**: `apps/caf/processor/tests/test_retry_policy_poc.cpp`

---

## PoC Success Criteria

### Functional Criteria

- ✅ Exponential backoff calculation correct (verified in tests)
- ✅ Error classification working (retryable vs non-retryable)
- ✅ Feature flag gates behavior correctly
- ✅ CP1 baseline preserved when feature flag disabled
- ✅ HTTP block retry behavior improved (exponential backoff, smart retry)

### Non-Functional Criteria

- ✅ No performance regression (feature flag check is fast)
- ✅ No crashes or errors when toggling feature flag
- ✅ Code changes are minimal (only HTTP block affected)
- ✅ Tests pass (all PoC test cases)

### Validation Criteria

**PoC is successful if**:
1. Exponential backoff works correctly (tested)
2. Error classification works correctly (tested)
3. Feature flag works correctly (tested)
4. CP1 baseline preserved (tested)
5. Can proceed to full implementation with confidence

**PoC fails if**:
1. Exponential backoff calculation incorrect
2. Error classification incorrect
3. Feature flag doesn't work
4. CP1 baseline broken
5. Performance regression detected

---

## PoC Test Matrix

| Test ID | Test Name | Feature Flag | Error Type | Expected Behavior | Priority |
|---------|-----------|--------------|------------|-------------------|----------|
| POC-001 | Exponential Backoff | Enabled | `network_error` | Retry with 100ms, 200ms, 400ms delays | P1 |
| POC-002 | Retryable Error | Enabled | `network_error` | Retry with exponential backoff | P1 |
| POC-003 | Non-Retryable Error | Enabled | `invalid_input` | No retry | P1 |
| POC-004 | CP1 Baseline | Disabled | `network_error` | Fixed backoff, retry all | P1 |
| POC-005 | Feature Flag Toggle | Toggle | `network_error` | Behavior changes correctly | P1 |
| POC-006 | HTTP Error (PoC) | Enabled | `http_error` | Retry (PoC simplification) | P2 |
| POC-007 | Connection Timeout | Enabled | `connection_timeout` | Retry with exponential backoff | P2 |
| POC-008 | Permission Denied | Enabled | `permission_denied` | No retry | P2 |

---

## PoC Deliverables

### Code

- `apps/caf/processor/include/beamline/worker/retry_policy.hpp` - Retry policy interface
- `apps/caf/processor/src/retry_policy.cpp` - Exponential backoff + error classification (PoC)
- `apps/caf/processor/src/worker_actor.cpp` - Feature flag integration

### Tests

- `apps/caf/processor/tests/test_retry_policy_poc.cpp` - PoC test cases
- All PoC test cases pass

### Documentation

- `docs/archive/dev/WORKER_RETRY_POC_REPORT.md` - PoC results and validation
- Feature flag usage instructions

---

## PoC Timeline

**Estimate**: 1.5 days

**Day 1**:
- Morning: Implement exponential backoff + feature flag
- Afternoon: Implement basic error classification
- End of day: Unit tests pass

**Day 2**:
- Morning: Integration tests with HTTP block
- Afternoon: PoC validation and report
- End of day: PoC complete, ready for full implementation

---

## Post-PoC: Full Implementation

**After PoC Success**:

1. **Add Jitter** (W3-1.2): Add jitter to exponential backoff
2. **HTTP Status Parsing** (W3-1.3): Parse HTTP status codes for `http_error` classification
3. **Retry Budget** (W3-1.4): Add total timeout across retries
4. **FS Block Support**: Extend retry policies to FS block
5. **SQL Block Support**: Extend retry policies to SQL block
6. **Configuration**: Add per-block and per-category configuration

**PoC → Full Implementation Path**:
- PoC validates design and feature gate mechanism
- Full implementation extends PoC to all blocks and features
- Feature flag remains: `worker.retries.v2.enabled`

---

## References

### Input Documents
- `docs/archive/dev/CP2_WORKER_RETRY_DESIGN.md` - Full retry v2 design
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` - Executable backlog with priorities

### Implementation Files
- `apps/caf/processor/src/worker_actor.cpp` - Retry logic implementation
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP block implementation

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Worker Retry v2 PoC Plan
- PoC scope defined (exponential backoff + basic error classification)
- Test cases specified
- Success criteria defined

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: PoC Plan Ready for Implementation

