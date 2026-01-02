# CP2 Worker Retry Policy Design

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-3 (Worker Reliability)  
**Status**: 📋 **DESIGN DOCUMENT** (CP2)

---

## Executive Summary

This document designs the CP2 retry policy system for Worker, including:

1. **ErrorCode → Retry Policy Mapping**: Classification of errors as retryable vs non-retryable
2. **Configuration Format**: Per-block and per-category retry policy configuration
3. **Feature Gate Strategy**: `worker.retries.v2.enabled` feature flag to protect CP1 baseline

**Foundation**: CP1 Worker provides basic retry logic (fixed backoff, retries all errors). CP2 extends this with exponential backoff, jitter, and smart error classification.

**References**:
- `docs/archive/dev/CP1_WORKER_FAILURE_MODEL.md` - CP1 failure model and gap analysis
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` - Executable backlog

---

## 1. ErrorCode → Retry Policy Mapping

### 1.1. ErrorCode Categories

**Current ErrorCode Enum** (`apps/caf/processor/include/beamline/worker/core.hpp`):

```cpp
enum class ErrorCode {
    none = 0,
    // Validation errors (1xxx)
    invalid_input = 1001,
    missing_required_field = 1002,
    invalid_format = 1003,
    // Execution errors (2xxx)
    execution_failed = 2001,
    resource_unavailable = 2002,
    permission_denied = 2003,
    quota_exceeded = 2004,
    // Network errors (3xxx)
    network_error = 3001,
    connection_timeout = 3002,
    http_error = 3003,
    // System errors (4xxx)
    internal_error = 4001,
    system_overload = 4002,
    // Cancellation (5xxx)
    cancelled_by_user = 5001,
    cancelled_by_timeout = 5002
};
```

### 1.2. Retryability Classification

**Design Principle**: Errors are classified as **retryable** (transient, may succeed on retry) or **non-retryable** (permanent, retry will not help).

#### Retryable Errors (Default: Retry)

| ErrorCode | Category | Reason | Retry Policy |
|-----------|----------|--------|--------------|
| `network_error` (3001) | Network | Transient network failures | Retry with exponential backoff |
| `connection_timeout` (3002) | Network | Connection timeout (may be transient) | Retry with exponential backoff |
| `http_error` (3003) | Network | HTTP error (requires HTTP status code check) | Conditional (see HTTP Status Code Mapping) |
| `resource_unavailable` (2002) | Execution | Resource temporarily unavailable | Retry with exponential backoff |
| `system_overload` (4002) | System | System overload (may recover) | Retry with exponential backoff |

#### Non-Retryable Errors (Default: No Retry)

| ErrorCode | Category | Reason | Retry Policy |
|-----------|----------|--------|--------------|
| `invalid_input` (1001) | Validation | Client error, retry won't help | No retry |
| `missing_required_field` (1002) | Validation | Client error, retry won't help | No retry |
| `invalid_format` (1003) | Validation | Client error, retry won't help | No retry |
| `execution_failed` (2001) | Execution | Generic execution failure (ambiguous) | **Configurable** (default: no retry) |
| `permission_denied` (2003) | Execution | Permission denied (permanent) | No retry |
| `quota_exceeded` (2004) | Execution | Quota exceeded (may be temporary, but requires quota reset) | **Configurable** (default: no retry) |
| `internal_error` (4001) | System | Internal error (may indicate bug) | **Configurable** (default: no retry) |
| `cancelled_by_user` (5001) | Cancellation | User cancellation | No retry |
| `cancelled_by_timeout` (5002) | Cancellation | Timeout cancellation | No retry |

#### Special Cases

**HTTP Error (3003)**: Requires HTTP status code inspection:

| HTTP Status Code | Retryable | Reason |
|------------------|-----------|--------|
| 400 Bad Request | ❌ No | Client error |
| 401 Unauthorized | ❌ No | Authentication error (permanent) |
| 403 Forbidden | ❌ No | Permission denied (permanent) |
| 404 Not Found | ❌ No | Resource not found (permanent) |
| 408 Request Timeout | ✅ Yes | Transient timeout |
| 429 Too Many Requests | ✅ Yes | Rate limit (retry with backoff) |
| 500 Internal Server Error | ✅ Yes | Server error (transient) |
| 502 Bad Gateway | ✅ Yes | Gateway error (transient) |
| 503 Service Unavailable | ✅ Yes | Service unavailable (transient) |
| 504 Gateway Timeout | ✅ Yes | Gateway timeout (transient) |
| Other 4xx | ❌ No | Client errors (non-retryable) |
| Other 5xx | ✅ Yes | Server errors (retryable) |

**Note**: HTTP status code must be extracted from `StepResult` metadata or error message for proper classification.

### 1.3. ErrorCode → Retry Policy Mapping Table

**Default Retry Policy Configuration**:

```cpp
struct RetryPolicyConfig {
    bool is_retryable(ErrorCode code) const {
        switch (code) {
            // Validation errors (1xxx) - Non-retryable
            case ErrorCode::invalid_input:
            case ErrorCode::missing_required_field:
            case ErrorCode::invalid_format:
                return false;
            
            // Execution errors (2xxx) - Conditional
            case ErrorCode::execution_failed:
                return config_.retry_execution_failed; // Default: false
            case ErrorCode::resource_unavailable:
                return true; // Retryable
            case ErrorCode::permission_denied:
                return false; // Non-retryable
            case ErrorCode::quota_exceeded:
                return config_.retry_quota_exceeded; // Default: false
            
            // Network errors (3xxx) - Retryable
            case ErrorCode::network_error:
            case ErrorCode::connection_timeout:
                return true; // Retryable
            case ErrorCode::http_error:
                return check_http_status_retryable(result); // Conditional (see HTTP mapping)
            
            // System errors (4xxx) - Conditional
            case ErrorCode::internal_error:
                return config_.retry_internal_error; // Default: false
            case ErrorCode::system_overload:
                return true; // Retryable
            
            // Cancellation (5xxx) - Non-retryable
            case ErrorCode::cancelled_by_user:
            case ErrorCode::cancelled_by_timeout:
                return false; // Non-retryable
            
            default:
                return false; // Conservative: don't retry unknown errors
        }
    }
};
```

---

## 2. Configuration Format

### 2.1. Configuration Hierarchy

**Design Principle**: Configuration follows a hierarchy: **Global → Category → Block → Request-level**.

1. **Global Default**: System-wide default retry policy
2. **Category-Level**: Per-error-category configuration (validation, execution, network, system)
3. **Block-Level**: Per-block-type configuration (http, fs, sql, etc.)
4. **Request-Level**: Per-request override (via `StepRequest`)

### 2.2. Configuration Structure

**JSON Configuration Format**:

```json
{
  "worker": {
    "retries": {
      "v2": {
        "enabled": false,
        "default_policy": {
          "max_attempts": 3,
          "base_delay_ms": 100,
          "max_delay_ms": 30000,
          "jitter_type": "full",
          "exponential_base": 2.0
        },
        "error_classification": {
          "validation_errors": {
            "retryable": false,
            "max_attempts": 0
          },
          "execution_errors": {
            "retryable": {
              "resource_unavailable": true,
              "execution_failed": false,
              "permission_denied": false,
              "quota_exceeded": false
            },
            "max_attempts": 3
          },
          "network_errors": {
            "retryable": {
              "network_error": true,
              "connection_timeout": true,
              "http_error": "conditional"
            },
            "max_attempts": 5,
            "http_status_mapping": {
              "4xx": {
                "retryable": false,
                "exceptions": {
                  "408": true,
                  "429": true
                }
              },
              "5xx": {
                "retryable": true
              }
            }
          },
          "system_errors": {
            "retryable": {
              "internal_error": false,
              "system_overload": true
            },
            "max_attempts": 3
          },
          "cancellation_errors": {
            "retryable": false,
            "max_attempts": 0
          }
        },
        "block_policies": {
          "http": {
            "max_attempts": 5,
            "base_delay_ms": 200,
            "max_delay_ms": 10000,
            "jitter_type": "equal",
            "error_overrides": {
              "429": {
                "max_attempts": 3,
                "base_delay_ms": 1000,
                "max_delay_ms": 60000
              }
            }
          },
          "fs": {
            "max_attempts": 3,
            "base_delay_ms": 100,
            "max_delay_ms": 5000,
            "jitter_type": "full"
          },
          "sql": {
            "max_attempts": 3,
            "base_delay_ms": 150,
            "max_delay_ms": 10000,
            "jitter_type": "full"
          }
        }
      }
    }
  }
}
```

### 2.3. Configuration Levels

#### Level 1: Global Default

**Purpose**: System-wide default retry policy when no block-specific or category-specific configuration exists.

**Fields**:
- `max_attempts`: Maximum retry attempts (default: 3)
- `base_delay_ms`: Base delay for exponential backoff (default: 100ms)
- `max_delay_ms`: Maximum backoff cap (default: 30000ms = 30s)
- `jitter_type`: Jitter algorithm (`"full"` or `"equal"`, default: `"full"`)
- `exponential_base`: Exponential base (default: 2.0)

#### Level 2: Category-Level Configuration

**Purpose**: Per-error-category retry policy (validation, execution, network, system, cancellation).

**Fields**:
- `retryable`: Boolean or object mapping ErrorCode → boolean
- `max_attempts`: Override max attempts for this category
- `http_status_mapping`: HTTP status code → retryable mapping (for network/http errors)

**Example**:
```json
"network_errors": {
  "retryable": {
    "network_error": true,
    "connection_timeout": true,
    "http_error": "conditional"
  },
  "max_attempts": 5,
  "http_status_mapping": {
    "4xx": {"retryable": false, "exceptions": {"408": true, "429": true}},
    "5xx": {"retryable": true}
  }
}
```

#### Level 3: Block-Level Configuration

**Purpose**: Per-block-type retry policy (http, fs, sql, etc.).

**Fields**:
- `max_attempts`: Override max attempts for this block type
- `base_delay_ms`: Override base delay for this block type
- `max_delay_ms`: Override max delay for this block type
- `jitter_type`: Override jitter type for this block type
- `error_overrides`: Per-error-code or per-HTTP-status overrides

**Example**:
```json
"http": {
  "max_attempts": 5,
  "base_delay_ms": 200,
  "max_delay_ms": 10000,
  "jitter_type": "equal",
  "error_overrides": {
    "429": {
      "max_attempts": 3,
      "base_delay_ms": 1000,
      "max_delay_ms": 60000
    }
  }
}
```

#### Level 4: Request-Level Override

**Purpose**: Per-request override via `StepRequest` (existing CP1 contract).

**Fields** (from `StepRequest`):
- `retry_count`: Override max attempts for this request
- `timeout_ms`: Total timeout budget across all retries

**Note**: Request-level overrides respect category-level and block-level `retryable` classification.

### 2.4. Configuration Resolution

**Resolution Order** (highest priority first):

1. **Request-Level**: `StepRequest.retry_count` (if provided)
2. **Block-Level**: Block-specific policy (if exists)
3. **Category-Level**: Error category policy (if exists)
4. **Global Default**: System-wide default policy

**Example Resolution**:

```
Request: HTTP block, ErrorCode::http_error, HTTP 429, StepRequest.retry_count=2

Resolution:
1. Check request-level: retry_count=2 (override)
2. Check block-level (http): max_attempts=5, error_overrides["429"]={max_attempts: 3, base_delay_ms: 1000}
   → Use error_overrides["429"] (max_attempts=3, but request says 2, so use 2)
3. Check category-level (network_errors): retryable=true (HTTP 429 is exception)
4. Check global default: base_delay_ms=100, max_delay_ms=30000

Final Policy:
- max_attempts: 2 (from request)
- base_delay_ms: 1000 (from block-level error_overrides["429"])
- max_delay_ms: 60000 (from block-level error_overrides["429"])
- retryable: true (from category-level HTTP 429 exception)
- jitter_type: "equal" (from block-level)
```

---

## 3. Feature Gate Strategy

### 3.1. Feature Flag: `worker.retries.v2.enabled`

**Purpose**: Gate CP2 retry policy features to protect CP1 baseline.

**Default**: `false` (CP1 baseline mode)

**Behavior**:
- **When `false` (CP1 mode)**:
  - Fixed backoff: `100 * (attempt + 1)` ms
  - No jitter
  - Retries all errors (no error classification)
  - No retry budget management
  - No exponential backoff

- **When `true` (CP2 mode)**:
  - Exponential backoff: `base * 2^attempt`
  - Jitter (full or equal)
  - Error classification (retryable vs non-retryable)
  - Retry budget management (total timeout across retries)
  - Configurable per-block and per-category policies

### 3.2. Feature Gate Implementation

**Configuration Check**:

```cpp
bool is_cp2_retry_enabled() {
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
```

**Usage in Retry Logic**:

```cpp
caf::expected<StepResult> ExecutorActorState::execute_with_retry(const StepRequest& req) {
    if (is_cp2_retry_enabled()) {
        // CP2: Advanced retry policy
        return execute_with_cp2_retry(req);
    } else {
        // CP1: Basic retry policy
        return execute_with_cp1_retry(req);
    }
}

caf::expected<StepResult> execute_with_cp2_retry(const StepRequest& req) {
    // Get retry policy from configuration
    auto policy = get_retry_policy(req.type, req.error_code);
    
    // Check if error is retryable
    if (!policy.is_retryable(req.error_code)) {
        return result; // Don't retry
    }
    
    // Exponential backoff with jitter
    int backoff_ms = calculate_exponential_backoff_with_jitter(
        attempt, policy.base_delay_ms, policy.max_delay_ms, policy.jitter_type
    );
    
    // Check retry budget
    if (elapsed_time > req.timeout_ms) {
        return timeout_result();
    }
    
    // ... retry logic ...
}

caf::expected<StepResult> execute_with_cp1_retry(const StepRequest& req) {
    // CP1 baseline: fixed backoff, retry all errors
    for (int attempt = 0; attempt <= req.retry_count; attempt++) {
        auto result = execute_single_attempt(req);
        if (result && result->status == StepStatus::ok) {
            return result;
        }
        
        // Fixed backoff: 100ms, 200ms, 300ms, ...
        if (attempt < req.retry_count) {
            std::this_thread::sleep_for(std::chrono::milliseconds(100 * (attempt + 1)));
        }
    }
    return final_result;
}
```

### 3.3. Feature Gate Testing

**Test Scenarios**:

1. **CP1 Mode (feature flag disabled)**:
   - Verify fixed backoff: `100 * (attempt + 1)` ms
   - Verify no jitter
   - Verify all errors retried (no classification)
   - Verify no retry budget check

2. **CP2 Mode (feature flag enabled)**:
   - Verify exponential backoff: `base * 2^attempt`
   - Verify jitter applied
   - Verify error classification (retryable vs non-retryable)
   - Verify retry budget check

3. **Mixed Mode (feature flag enabled, but config missing)**:
   - Fallback to CP1 baseline
   - Log warning about missing CP2 config

---

## 4. Exponential Backoff and Jitter

### 4.1. Exponential Backoff Formula

**CP2 Formula**:
```
backoff_ms = min(base_delay_ms * (exponential_base ^ attempt), max_delay_ms)
```

**Example** (base=100ms, exponential_base=2.0, max=30000ms):
- Attempt 0: `min(100 * 2^0, 30000) = 100ms`
- Attempt 1: `min(100 * 2^1, 30000) = 200ms`
- Attempt 2: `min(100 * 2^2, 30000) = 400ms`
- Attempt 3: `min(100 * 2^3, 30000) = 800ms`
- Attempt 4: `min(100 * 2^4, 30000) = 1600ms`
- Attempt 5: `min(100 * 2^5, 30000) = 3200ms`
- ...
- Attempt 8: `min(100 * 2^8, 30000) = 25600ms`
- Attempt 9: `min(100 * 2^9, 30000) = 30000ms` (capped)

### 4.2. Jitter Types

#### Full Jitter

**Formula**:
```
jitter_ms = random(0, backoff_ms)
final_delay_ms = jitter_ms
```

**Example** (backoff=800ms):
- Random jitter: `0..800ms`
- Final delay: `random(0, 800)ms`

**Use Case**: Reduces thundering herd, spreads retries evenly.

#### Equal Jitter

**Formula**:
```
jitter_ms = random(0, backoff_ms / 2)
final_delay_ms = backoff_ms / 2 + jitter_ms
```

**Example** (backoff=800ms):
- Base: `800 / 2 = 400ms`
- Jitter: `random(0, 400)ms`
- Final delay: `400 + random(0, 400) = 400..800ms`

**Use Case**: Guarantees minimum delay while reducing thundering herd.

### 4.3. Jitter Selection

**Default**: Full jitter (better for distributed systems)

**Block-Specific Overrides**:
- HTTP blocks: Equal jitter (guarantees minimum delay for rate-limited APIs)
- FS blocks: Full jitter (better for local I/O)
- SQL blocks: Full jitter (better for database connections)

---

## 5. Retry Budget Management

### 5.1. Total Timeout Across Retries

**Design Principle**: `StepRequest.timeout_ms` is the **total budget** for all retry attempts, not per-attempt.

**Calculation**:
```
total_elapsed = attempt_execution_time + retry_wait_time + ... + current_attempt_execution_time

if (total_elapsed > StepRequest.timeout_ms) {
    return timeout_result();
}
```

**Example**:
- `StepRequest.timeout_ms = 5000ms` (5 seconds total budget)
- Attempt 0: 1000ms execution + 100ms wait = 1100ms elapsed
- Attempt 1: 800ms execution + 200ms wait = 2100ms elapsed (total: 3200ms)
- Attempt 2: 1200ms execution + 400ms wait = 1600ms elapsed (total: 4800ms)
- Attempt 3: Would exceed 5000ms → Return timeout

### 5.2. Early Termination

**Conditions for Early Termination**:

1. **Retry Budget Exhausted**: `total_elapsed > StepRequest.timeout_ms`
2. **Non-Retryable Error**: Error classification says don't retry
3. **Cancellation**: Cancellation token set
4. **Max Attempts Reached**: `attempt >= max_attempts`

---

## 6. HTTP Status Code Mapping

### 6.1. HTTP Error Classification

**Current State**: `ErrorCode::http_error` (3003) is used for all HTTP errors, but HTTP status code is not extracted.

**CP2 Enhancement**: Extract HTTP status code from `StepResult` metadata or error message.

**Proposed Metadata Structure**:

```cpp
struct StepResult {
    // ... existing fields ...
    std::unordered_map<std::string, std::string> metadata; // Includes HTTP status code
};

// In http_block.cpp
if (http_result.status_code >= 400) {
    auto result = StepResult::error_result(
        ErrorCode::http_error,
        "HTTP request failed with status: " + std::to_string(http_result.status_code),
        metadata,
        latency_ms
    );
    // Add HTTP status code to metadata
    result.metadata["http_status_code"] = std::to_string(http_result.status_code);
    return result;
}
```

### 6.2. HTTP Status Code → Retryable Mapping

**Default Mapping** (from Section 1.2):

| HTTP Status | Retryable | Special Handling |
|-------------|-----------|------------------|
| 400-407, 410-427 | ❌ No | Client errors |
| 408 | ✅ Yes | Request timeout (transient) |
| 429 | ✅ Yes | Rate limit (use longer backoff) |
| 500-504 | ✅ Yes | Server errors (transient) |
| 505+ | ✅ Yes | Server errors (transient) |

**Rate Limit (429) Special Handling**:

```json
"error_overrides": {
  "429": {
    "max_attempts": 3,
    "base_delay_ms": 1000,
    "max_delay_ms": 60000,
    "jitter_type": "equal"
  }
}
```

---

## 7. Configuration Examples

### 7.1. Minimal Configuration (CP2 Defaults)

```json
{
  "worker": {
    "retries": {
      "v2": {
        "enabled": true
      }
    }
  }
}
```

**Uses**: Global defaults (max_attempts=3, base_delay_ms=100, max_delay_ms=30000, jitter_type="full")

### 7.2. HTTP-Specific Configuration

```json
{
  "worker": {
    "retries": {
      "v2": {
        "enabled": true,
        "block_policies": {
          "http": {
            "max_attempts": 5,
            "base_delay_ms": 200,
            "max_delay_ms": 10000,
            "jitter_type": "equal",
            "error_overrides": {
              "429": {
                "max_attempts": 3,
                "base_delay_ms": 1000,
                "max_delay_ms": 60000
              },
              "503": {
                "max_attempts": 10,
                "base_delay_ms": 500,
                "max_delay_ms": 30000
              }
            }
          }
        }
      }
    }
  }
}
```

### 7.3. Category-Specific Configuration

```json
{
  "worker": {
    "retries": {
      "v2": {
        "enabled": true,
        "error_classification": {
          "execution_errors": {
            "retryable": {
              "execution_failed": true,
              "quota_exceeded": true
            },
            "max_attempts": 2
          }
        }
      }
    }
  }
}
```

---

## 8. Implementation Notes

### 8.1. Backward Compatibility

**CP1 Compatibility**:
- When `worker.retries.v2.enabled = false`, use CP1 baseline behavior
- CP1 behavior: Fixed backoff, retry all errors, no jitter, no error classification

**Migration Path**:
1. Enable feature flag: `worker.retries.v2.enabled = true`
2. Configure retry policies (optional, uses defaults)
3. Test with CP2 retry policies
4. Monitor retry behavior and adjust configuration

### 8.2. Performance Considerations

**Jitter Calculation**:
- Use fast random number generator (e.g., `std::mt19937`)
- Cache random generator per thread (thread-local)

**Configuration Lookup**:
- Cache resolved retry policies per block type
- Use hash map for O(1) lookup

### 8.3. Observability

**Metrics to Expose**:
- Retry attempts per error code
- Retry attempts per block type
- Retry success rate
- Average backoff delay
- Retry budget exhaustion count

**Logging**:
- Log retry decisions (retryable vs non-retryable)
- Log backoff delay calculation
- Log retry budget exhaustion

---

## 9. Acceptance Criteria

### 9.1. Error Classification

- ✅ ErrorCode → retryable mapping implemented
- ✅ HTTP status code → retryable mapping implemented
- ✅ Configurable per-error-code retryability
- ✅ Default retryability rules match design

### 9.2. Configuration

- ✅ Global default configuration working
- ✅ Category-level configuration working
- ✅ Block-level configuration working
- ✅ Request-level override working
- ✅ Configuration resolution order correct

### 9.3. Feature Gate

- ✅ Feature flag `worker.retries.v2.enabled` working
- ✅ CP1 baseline preserved when feature flag disabled
- ✅ CP2 features enabled when feature flag enabled
- ✅ Fallback to CP1 when config missing

### 9.4. Exponential Backoff and Jitter

- ✅ Exponential backoff calculation correct
- ✅ Full jitter implementation working
- ✅ Equal jitter implementation working
- ✅ Jitter type configurable per block

### 9.5. Retry Budget

- ✅ Total timeout across retries enforced
- ✅ Early termination on budget exhaustion
- ✅ Retry budget calculation correct

---

## 10. PoC Plan

**For minimal PoC implementation**, see `docs/archive/dev/CP2_WORKER_RETRY_POC_PLAN.md`.

**PoC Scope**:
- Exponential backoff (no jitter)
- Basic error classification (ErrorCode-based, no HTTP status parsing)
- HTTP block only (no FS, SQL blocks)
- Feature flag: `worker.retries.v2.enabled`

**PoC Timeline**: 1.5 days

**PoC Success Criteria**: Exponential backoff works, error classification works, feature flag works, CP1 baseline preserved.

---

## 11. References

### Input Documents
- `docs/archive/dev/CP1_WORKER_FAILURE_MODEL.md` - CP1 failure model and gap analysis
- `docs/archive/dev/CP2_WORKER_RELIABILITY_PLAN.md` - High-level CP2 plan
- `docs/archive/dev/CP2_WORKER_RELIABILITY_BACKLOG.md` - Executable backlog with priorities
- `docs/archive/dev/CP2_WORKER_RETRY_POC_PLAN.md` - PoC implementation plan

### Implementation Files
- `apps/caf/processor/include/beamline/worker/core.hpp` - ErrorCode enum definition
- `apps/caf/processor/src/worker_actor.cpp` - Retry logic implementation
- `apps/caf/processor/src/blocks/http_block.cpp` - HTTP block implementation

### Related Documents
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md` - Worker architecture and requirements
- `apps/otp/router/docs/API_CONTRACTS.md` - ExecResult contract specification

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Worker Retry Policy Design
- ErrorCode → retry-policy mapping defined
- Configuration format specified (global, category, block, request levels)
- Feature gate strategy defined (`worker.retries.v2.enabled`)
- Exponential backoff and jitter formulas specified
- Retry budget management designed

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Design Document Ready for Implementation

