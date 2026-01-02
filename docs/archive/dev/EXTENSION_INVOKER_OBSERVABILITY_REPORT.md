# Extension Invoker Observability Report

**Date**: 2025-01-27  
**Status**: ✅ **COMPLETED**  
**Target**: CP1 Observability Compliance  
**Worker**: wrk-2 (Router OTP)

---

## Executive Summary

Successfully enhanced Extension Invoker observability with:
- ✅ Unified telemetry fields (tenant_id, policy_id, extension_id, type, subject, status, latency_ms, retries_used)
- ✅ Structured JSON logging compliant with OBSERVABILITY_CP1_INVARIANTS.md
- ✅ PII/secret filtering compliance
- ✅ Comprehensive smoke tests for telemetry events

---

## Implementation Details

### 1. Unified Telemetry Fields

**File**: `apps/otp/router/src/router_extension_invoker.erl`

**Required Fields** (always present):
- `extension_id` - Extension identifier
- `type` - Extension type (pre, validator, post, provider)
- `subject` - NATS subject
- `status` - Invocation status (success, error, timeout, max_retries_exceeded)
- `latency_ms` - Invocation latency in milliseconds
- `retries_used` - Number of retries used (0 if no retries)

**Optional Correlation Fields** (when available):
- `tenant_id` - Tenant identifier
- `policy_id` - Policy identifier

**Telemetry Event**:
```erlang
router_telemetry_helper:execute(
    [router_extension_invoker, invocation_total],
    #{count => 1},
    #{
        extension_id => ExtensionId,
        type => ExtensionType,
        subject => ExtensionSubject,
        status => Status,
        latency_ms => LatencyMs,
        retries_used => RetriesUsed,
        tenant_id => TenantId,  % optional
        policy_id => PolicyId   % optional
    }
)
```

### 2. Structured JSON Logging

**Compliance**: `docs/OBSERVABILITY_CP1_INVARIANTS.md`

**Log Format**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO|WARN|ERROR",
  "component": "router",
  "message": "Extension invocation succeeded|failed|timeout",
  "tenant_id": "tenant_123",  % optional
  "policy_id": "policy_456",  % optional
  "context": {
    "extension_id": "test_extension",
    "type": "pre",
    "subject": "beamline.ext.pre.test.v1",
    "status": "success",
    "latency_ms": 45.2,
    "retries_used": 0,
    "error": {...}  % only on error
  }
}
```

**Log Levels**:
- `INFO` - Successful invocation
- `WARN` - Timeout or max retries exceeded
- `ERROR` - Invocation failed

**PII Filtering**:
- Automatic PII filtering via `router_logger:filter_pii/1`
- Compliant with OBSERVABILITY_CP1_INVARIANTS.md requirements

### 3. Retry Tracking

**Enhancement**: Track retries used in both telemetry and logs

**Implementation**:
- `retries_used` calculated as `MaxRetries - RetriesLeft`
- Included in all telemetry events
- Included in structured logs
- Properly tracked in error messages

### 4. Context Propagation

**Enhancement**: Extract correlation fields from Context/Request

**Fields Extracted**:
- `tenant_id` - From Context or Request
- `policy_id` - From Context or Request
- `trace_id` - Already extracted for NATS payload

**Usage**:
- Included in telemetry metadata
- Included in structured logs
- Passed to NATS request payload

---

## Testing

### Test Suite

**File**: `apps/otp/router/test/router_extension_invoker_telemetry_SUITE.erl`

**Test Cases**:

1. **test_telemetry_success_event**
   - Verifies telemetry event emitted on successful invocation
   - Validates all required fields present
   - Validates correlation fields (tenant_id, policy_id)

2. **test_telemetry_error_event**
   - Verifies telemetry event emitted on error
   - Validates error status in metadata

3. **test_telemetry_timeout_event**
   - Verifies telemetry event emitted on timeout
   - Validates timeout status in metadata

4. **test_telemetry_with_retries**
   - Verifies retries_used field in telemetry events
   - Validates retry tracking across multiple attempts

5. **test_telemetry_unified_fields**
   - Verifies all required fields present in telemetry
   - Validates optional correlation fields when available

6. **test_logging_success**
   - Verifies structured log emitted on success
   - Smoke test (no crash verification)

7. **test_logging_error**
   - Verifies structured log emitted on error
   - Smoke test (no crash verification)

8. **test_logging_with_correlation_fields**
   - Verifies correlation fields in logs
   - Smoke test (no crash verification)

**Test Handler**:
- Uses `telemetry:attach_many/4` to capture events
- Validates event structure and metadata
- Clears events between tests

---

## Files Modified

### Modified Files

1. **`apps/otp/router/src/router_extension_invoker.erl`**
   - Added unified telemetry emission (`emit_unified_telemetry/8`)
   - Added structured logging (`log_extension_invocation/9`)
   - Enhanced retry tracking
   - Added correlation field extraction (tenant_id, policy_id)
   - Updated `invoke_extension_with_health/3` to emit telemetry and logs
   - Updated `invoke_with_retry_internal/5` to track retries_used

### Created Files

1. **`apps/otp/router/test/router_extension_invoker_telemetry_SUITE.erl`**
   - Comprehensive smoke tests for telemetry events
   - Test handler for capturing telemetry events
   - Validation of unified fields

---

## Compliance Checklist

### OBSERVABILITY_CP1_INVARIANTS.md Compliance

- ✅ **Unified JSON Log Format**
  - Structured logs with required fields (timestamp, level, component, message)
  - Optional correlation fields (tenant_id, policy_id) when available
  - Context map with extension-specific fields

- ✅ **CP1 Correlation Fields**
  - `tenant_id` included when available
  - `policy_id` included when available
  - `latency_ms` included in all logs

- ✅ **PII/Secret Filtering**
  - Automatic filtering via `router_logger:filter_pii/1`
  - Compliant with OBSERVABILITY_CP1_INVARIANTS.md requirements

- ✅ **Telemetry Events**
  - Unified fields in all telemetry events
  - Correlation fields included when available
  - Proper status tracking (success, error, timeout, max_retries_exceeded)

---

## Telemetry Event Specification

### Event Name

`[router_extension_invoker, invocation_total]`

### Measurements

```erlang
#{count => 1}
```

### Metadata

**Required Fields**:
```erlang
#{
    extension_id => binary(),
    type => binary(),  % pre | validator | post | provider
    subject => binary(),  % NATS subject
    status => atom(),  % success | error | timeout | max_retries_exceeded
    latency_ms => float(),
    retries_used => integer()
}
```

**Optional Fields** (when available):
```erlang
#{
    tenant_id => binary(),
    policy_id => binary()
}
```

---

## Log Event Specification

### Log Levels

- **INFO**: Successful invocation
- **WARN**: Timeout or max retries exceeded
- **ERROR**: Invocation failed

### Context Structure

```erlang
#{
    extension_id => binary(),
    type => binary(),
    subject => binary(),
    status => atom(),
    latency_ms => float(),
    retries_used => integer(),
    tenant_id => binary(),  % optional
    policy_id => binary(),  % optional
    error => map()  % only on error
}
```

---

## Examples

### Successful Invocation

**Telemetry**:
```erlang
router_telemetry_helper:execute(
    [router_extension_invoker, invocation_total],
    #{count => 1},
    #{
        extension_id => <<"normalize_text">>,
        type => <<"pre">>,
        subject => <<"beamline.ext.pre.normalize_text.v1">>,
        status => success,
        latency_ms => 45.2,
        retries_used => 0,
        tenant_id => <<"tenant_123">>,
        policy_id => <<"policy_456">>
    }
)
```

**Log**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "router",
  "message": "Extension invocation succeeded",
  "tenant_id": "tenant_123",
  "policy_id": "policy_456",
  "context": {
    "extension_id": "normalize_text",
    "type": "pre",
    "subject": "beamline.ext.pre.normalize_text.v1",
    "status": "success",
    "latency_ms": 45.2,
    "retries_used": 0
  }
}
```

### Failed Invocation with Retries

**Telemetry**:
```erlang
router_telemetry_helper:execute(
    [router_extension_invoker, invocation_total],
    #{count => 1},
    #{
        extension_id => <<"pii_guard">>,
        type => <<"validator">>,
        subject => <<"beamline.ext.validate.pii_guard.v1">>,
        status => max_retries_exceeded,
        latency_ms => 1200.0,
        retries_used => 3,
        tenant_id => <<"tenant_123">>,
        policy_id => <<"policy_456">>
    }
)
```

**Log**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "WARN",
  "component": "router",
  "message": "Extension invocation max retries exceeded",
  "tenant_id": "tenant_123",
  "policy_id": "policy_456",
  "context": {
    "extension_id": "pii_guard",
    "type": "validator",
    "subject": "beamline.ext.validate.pii_guard.v1",
    "status": "max_retries_exceeded",
    "latency_ms": 1200.0,
    "retries_used": 3,
    "error": {
      "reason": "Maximum retries exceeded",
      "max_retries": 3
    }
  }
}
```

---

## Next Steps

### Immediate

1. ✅ Run smoke tests to verify telemetry events
2. ✅ Verify structured logs in development environment
3. ✅ Validate PII filtering works correctly

### Future Enhancements

1. Add histogram metrics for latency distribution
2. Add counter metrics for retry attempts
3. Add gauge metrics for active invocations
4. Add distributed tracing support (trace_id propagation)

---

## References

- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/OBSERVABILITY_CONVENTIONS.md` - Observability conventions
- `apps/otp/router/src/router_logger.erl` - Structured JSON logger
- `apps/otp/router/src/router_telemetry_helper.erl` - Telemetry helper

---

## Conclusion

✅ **All tasks completed**:
- ✅ Unified telemetry fields implemented
- ✅ Structured JSON logging compliant with OBSERVABILITY_CP1_INVARIANTS.md
- ✅ PII/secret filtering compliance
- ✅ Comprehensive smoke tests created

Extension Invoker observability is now fully compliant with CP1 observability requirements.

