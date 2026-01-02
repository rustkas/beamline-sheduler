# Gateway Observability Improvements Plan

**Date**: 2025-01-27  
**Component**: C-Gateway (`apps/c-gateway/`)  
**Status**: ✅ **COMPLETED**  
**Priority**: High (CP1 Compliance)

## Executive Summary

Gateway currently has basic observability implementation, but needs improvements to fully comply with CP1 observability invariants. This plan outlines required changes to ensure Gateway meets all CP1 requirements for structured JSON logging, CP1 correlation fields, and proper log format.

---

## Current State Analysis

### ✅ What's Already Implemented

1. **Basic Logging**:
   - `log_error()` function exists
   - `log_info()` function exists
   - JSON format output to stderr

2. **CP1 Fields Extraction**:
   - ✅ `tenant_id` - extracted from headers, stored in context, logged in `log_info()`
   - ✅ `trace_id` - extracted from headers, stored in context, logged in both functions
   - ✅ `request_id` - extracted from body, stored in context, logged in both functions
   - ⚠️ `run_id` - extracted from HTTP body in `build_route_request_json()`, but **NOT stored in context** and **NOT logged**

3. **Health Endpoint**:
   - ✅ `GET /_health` endpoint implemented on port 3000

4. **OpenTelemetry Tracing**:
   - ✅ OpenTelemetry integration exists
   - ✅ Trace context extraction from `traceparent` header

### ❌ What's Missing or Incorrect

1. **Log Format Issues**:
   - ❌ Missing `timestamp` field (ISO 8601 format required)
   - ❌ Missing `message` field in `log_info()` (required by CP1)
   - ❌ Component name is `"c-gateway"` instead of `"gateway"` (should match CP1 spec)
   - ❌ `run_id` not included in logs (required when run context available)

2. **CP1 Fields Issues**:
   - ❌ `run_id` extracted from HTTP body but not stored in `request_context_t`
   - ❌ `run_id` not included in `log_error()` or `log_info()` output
   - ❌ `tenant_id` missing in `log_error()` (only in `log_info()`)

3. **Log Level Issues**:
   - ⚠️ Only `log_error()` and `log_info()` exist
   - ⚠️ Missing `log_warn()` and `log_debug()` functions

4. **PII/Secret Filtering**:
   - ⚠️ No explicit PII filtering in logs (should filter sensitive fields)

---

## Required Improvements

### 1. Fix Log Format to Match CP1 Requirements ✅ **HIGH PRIORITY**

**Current Format** (`log_info`):
```json
{
  "level": "info",
  "component": "c-gateway",
  "stage": "http_request",
  "method": "POST",
  "path": "/api/v1/messages",
  "status_code": 200,
  "latency_ms": 120,
  "tenant_id": "tenant_123",
  "request_id": "req_123",
  "trace_id": "trace_456"
}
```

**Required CP1 Format**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "gateway",
  "message": "Request processed successfully",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "latency_ms": 120,
  "context": {
    "method": "POST",
    "path": "/api/v1/messages",
    "status_code": 200
  }
}
```

**Changes Needed**:
1. Add `timestamp` field (ISO 8601 format with microseconds)
2. Change `level` to uppercase (`"INFO"` instead of `"info"`)
3. Change `component` to `"gateway"` (remove `"c-"` prefix)
4. Add `message` field (human-readable description)
5. Add `run_id` field (when available)
6. Move technical details to `context` object (method, path, status_code, stage)

### 2. Add `run_id` to Request Context ✅ **HIGH PRIORITY**

**Current State**:
- `run_id` is extracted from HTTP body in `build_route_request_json()` (line 973-978)
- But it's NOT stored in `request_context_t` structure
- Therefore, it cannot be logged

**Required Changes**:
1. Add `char run_id[64]` field to `request_context_t` structure
2. Extract `run_id` from HTTP body and store in context (similar to `request_id`)
3. Include `run_id` in all log functions when available

**Code Changes**:
```c
typedef struct {
    char request_id[64];
    char trace_id[64];
    char tenant_id[64];
    char run_id[64];  // NEW: CP1 field
    otel_span_t *otel_span;
} request_context_t;
```

### 3. Update `log_error()` Function ✅ **HIGH PRIORITY**

**Current Issues**:
- Missing `timestamp` field
- Missing `message` field
- Missing `tenant_id` field
- Missing `run_id` field
- Component name is `"c-gateway"` instead of `"gateway"`
- Level is lowercase `"error"` instead of uppercase `"ERROR"`

**Required Changes**:
1. Add ISO 8601 timestamp generation
2. Add `message` parameter (human-readable error description)
3. Include `tenant_id` from context
4. Include `run_id` from context (when available)
5. Change component to `"gateway"`
6. Change level to `"ERROR"`
7. Move technical details to `context` object

### 4. Update `log_info()` Function ✅ **HIGH PRIORITY**

**Current Issues**:
- Missing `timestamp` field
- Missing `message` field
- Missing `run_id` field
- Component name is `"c-gateway"` instead of `"gateway"`
- Level is lowercase `"info"` instead of uppercase `"INFO"`
- Technical details (method, path, status_code) should be in `context` object

**Required Changes**:
1. Add ISO 8601 timestamp generation
2. Add `message` parameter (human-readable description)
3. Include `run_id` from context (when available)
4. Change component to `"gateway"`
5. Change level to `"INFO"`
6. Move technical details to `context` object

### 5. Add `log_warn()` and `log_debug()` Functions ✅ **MEDIUM PRIORITY**

**Current State**:
- Only `log_error()` and `log_info()` exist
- CP1 requires support for all log levels: ERROR, WARN, INFO, DEBUG

**Required Changes**:
1. Create `log_warn()` function (similar to `log_info()`)
2. Create `log_debug()` function (similar to `log_info()`)
3. Use appropriate log levels in code

### 6. Extract `run_id` from HTTP Body and Store in Context ✅ **HIGH PRIORITY**

**Current State**:
- `run_id` is extracted in `build_route_request_json()` but not stored in context
- Need to extract it earlier (in `validate_decide_request()` or `handle_client()`) and store in context

**Required Changes**:
1. Extract `run_id` from HTTP body in `validate_decide_request()` or similar function
2. Store `run_id` in `request_context_t`
3. Use stored `run_id` in logs

**Code Location**:
- Option 1: Extract in `validate_decide_request()` (if `run_id` is in request body)
- Option 2: Extract in `handle_client()` before calling `build_route_request_json()`

### 7. Add PII/Secret Filtering ✅ **MEDIUM PRIORITY**

**Current State**:
- No explicit PII filtering in logs
- Sensitive fields (API keys, passwords, tokens) may be logged

**Required Changes**:
1. Add PII filtering function
2. Filter sensitive fields before logging:
   - `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`
   - `authorization`, `credit_card`, `ssn`, `email`, `phone`
3. Replace filtered values with `"[REDACTED]"`

### 8. ISO 8601 Timestamp Generation ✅ **HIGH PRIORITY**

**Current State**:
- No timestamp in logs

**Required Changes**:
1. Create helper function to generate ISO 8601 timestamp with microseconds
2. Use `gettimeofday()` for microsecond precision
3. Format: `"2025-01-27T12:00:00.123456Z"`

**Example Implementation**:
```c
static void get_iso8601_timestamp(char *buf, size_t buf_size) {
    struct timeval tv;
    struct tm *tm_info;
    gettimeofday(&tv, NULL);
    tm_info = gmtime(&tv.tv_sec);
    snprintf(buf, buf_size, "%04d-%02d-%02dT%02d:%02d:%02d.%06ldZ",
             tm_info->tm_year + 1900, tm_info->tm_mon + 1, tm_info->tm_mday,
             tm_info->tm_hour, tm_info->tm_min, tm_info->tm_sec,
             (long)tv.tv_usec);
}
```

---

## Implementation Plan

### Phase 1: Critical Fixes (CP1 Compliance) ✅ **HIGH PRIORITY**

**Estimated Effort**: 1-2 days

1. ✅ Add `run_id` field to `request_context_t` structure
2. ✅ Extract `run_id` from HTTP body and store in context
3. ✅ Add ISO 8601 timestamp generation function
4. ✅ Update `log_error()` to match CP1 format:
   - Add timestamp
   - Add message parameter
   - Add tenant_id
   - Add run_id (when available)
   - Fix component name and level
5. ✅ Update `log_info()` to match CP1 format:
   - Add timestamp
   - Add message parameter
   - Add run_id (when available)
   - Fix component name and level
   - Move technical details to context

### Phase 2: Additional Log Levels ✅ **MEDIUM PRIORITY**

**Estimated Effort**: 0.5 days

1. ✅ Create `log_warn()` function
2. ✅ Create `log_debug()` function
3. ✅ Update code to use appropriate log levels

### Phase 3: PII Filtering ✅ **MEDIUM PRIORITY**

**Estimated Effort**: 0.5 days

1. ✅ Create PII filtering function
2. ✅ Apply filtering to all log outputs
3. ✅ Test with sensitive data

---

## Code Changes Summary

### File: `apps/c-gateway/src/http_server.c`

**Changes**:
1. Update `request_context_t` structure (add `run_id` field)
2. Add `get_iso8601_timestamp()` helper function
3. Update `log_error()` function signature and implementation
4. Update `log_info()` function signature and implementation
5. Add `log_warn()` function
6. Add `log_debug()` function
7. Extract `run_id` from HTTP body and store in context
8. Update all `log_error()` and `log_info()` calls to include `message` parameter

**Estimated Lines Changed**: ~200-300 lines

---

## Testing Requirements

### Unit Tests

1. ✅ Test ISO 8601 timestamp format
2. ✅ Test `run_id` extraction and storage
3. ✅ Test log format compliance (JSON schema validation)
4. ✅ Test CP1 fields presence in logs
5. ✅ Test PII filtering

### Integration Tests

1. ✅ Test end-to-end log format with CP1 fields
2. ✅ Test log format validation script
3. ✅ Test health endpoint

### Validation

1. ✅ Run `scripts/observability/validate_observability_e2e.sh`
2. ✅ Verify all CP1 fields present in logs when available
3. ✅ Verify log format matches CP1 requirements

---

## Acceptance Criteria

### CP1 Compliance ✅

1. ✅ All logs include `timestamp` (ISO 8601 format)
2. ✅ All logs include `level` (uppercase: ERROR, WARN, INFO, DEBUG)
3. ✅ All logs include `component` (`"gateway"`)
4. ✅ All logs include `message` (human-readable)
5. ✅ All logs include CP1 fields when available:
   - `tenant_id` (when tenant context available)
   - `run_id` (when run context available)
   - `trace_id` (when trace context available)
6. ✅ Technical details moved to `context` object
7. ✅ PII/secret filtering implemented

### Validation ✅

1. ✅ `scripts/observability/validate_observability_e2e.sh` passes
2. ✅ Log format validation passes
3. ✅ CP1 fields validation passes

---

## References

- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants specification
- `docs/OBSERVABILITY.md` - General observability requirements
- `docs/OBSERVABILITY_CONVENTIONS.md` - Logging conventions
- `config/observability/logging.json` - Log format schema
- `scripts/observability/validate_observability_e2e.sh` - E2E validation script

---

## Priority Summary

**High Priority** (CP1 Compliance):
1. Fix log format (timestamp, message, component, level)
2. Add `run_id` to context and logs
3. Update `log_error()` and `log_info()` functions

**Medium Priority** (Completeness):
4. Add `log_warn()` and `log_debug()` functions
5. Add PII/secret filtering

**Estimated Total Effort**: 2-3 days

---

## Next Steps

1. Review and approve this plan
2. Implement Phase 1 (Critical Fixes)
3. Test and validate changes
4. Implement Phase 2 (Additional Log Levels)
5. Implement Phase 3 (PII Filtering)
6. Final validation and documentation update

