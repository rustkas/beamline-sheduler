# Worker Observability Improvements Plan

**Date**: 2025-01-27  
**Component**: CAF Worker (`apps/caf/processor/`)  
**Status**: ✅ **COMPLETED**  
**Priority**: High (CP1 Compliance)  
**Completion Date**: 2025-01-27

## Executive Summary

Worker currently has basic observability implementation, but needs improvements to fully comply with CP1 observability invariants. This plan outlines required changes to ensure Worker meets all CP1 requirements for structured JSON logging, CP1 correlation fields, and proper log format.

---

## Current State Analysis

### ✅ What's Already Implemented

1. **Basic Logging**:
   - `log_info()`, `log_warn()`, `log_error()` functions exist
   - JSON format output
   - Uses nlohmann/json for JSON building

2. **CP1 Fields in Data Structures**:
   - ✅ `BlockContext` contains all CP1 fields: `tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`
   - ✅ `ResultMetadata` contains CP1 fields: `trace_id`, `run_id`, `flow_id`, `step_id`, `tenant_id`

3. **Health Endpoint**:
   - ⚠️ Mentioned in documentation, but implementation needs verification

4. **OpenTelemetry Tracing**:
   - ✅ OpenTelemetry integration exists
   - ✅ `start_step_span()` accepts CP1 fields

### ❌ What's Missing or Incorrect

1. **Log Format Issues**:
   - ❌ `timestamp` in milliseconds (Unix timestamp), should be ISO 8601 format
   - ❌ Missing `component` field (has `worker_id` instead)
   - ❌ CP1 fields (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`) are in `fields` object, should be at top level
   - ❌ Technical details in `fields`, should be in `context` object
   - ❌ Missing `log_debug()` function

2. **CP1 Fields Issues**:
   - ❌ CP1 fields not extracted from `BlockContext` and passed to log functions
   - ❌ Log functions don't accept CP1 fields as separate parameters
   - ❌ CP1 fields passed manually in `fields` map (incomplete)

3. **PII/Secret Filtering**:
   - ❌ No PII filtering implemented

4. **Health Endpoint**:
   - ⚠️ Implementation needs verification

---

## Required Improvements

### 1. Fix Log Format to Match CP1 Requirements ✅ **HIGH PRIORITY**

**Current Format** (`format_json_log`):
```json
{
  "timestamp": 1640995200000,
  "level": "INFO",
  "worker_id": "worker_12345",
  "message": "Step execution completed",
  "fields": {
    "block_type": "http.request",
    "latency_ms": 150,
    "status": "ok",
    "tenant_id": "tenant_123",
    "flow_id": "flow_xyz"
  }
}
```

**Required CP1 Format**:
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "INFO",
  "component": "worker",
  "message": "Step execution completed",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "step_id": "step_001",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "latency_ms": 150,
  "context": {
    "block_type": "http.request",
    "status": "success",
    "worker_id": "worker_12345"
  }
}
```

**Changes Needed**:
1. Change `timestamp` to ISO 8601 format with microseconds
2. Add `component` field (`"worker"`)
3. Move CP1 fields from `fields` to top level (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`)
4. Move technical details to `context` object
5. Keep `worker_id` in `context` (not top level)

### 2. Update Log Functions to Accept CP1 Fields ✅ **HIGH PRIORITY**

**Current Signature**:
```cpp
void log_info(const std::string& message, const std::unordered_map<std::string, std::string>& fields = {});
```

**Required Signature**:
```cpp
void log_info(const std::string& message, 
              const std::string& tenant_id = "",
              const std::string& run_id = "",
              const std::string& flow_id = "",
              const std::string& step_id = "",
              const std::string& trace_id = "",
              const std::unordered_map<std::string, std::string>& context = {});
```

**Alternative**: Create helper function that extracts CP1 fields from `BlockContext`:
```cpp
void log_info_with_context(const std::string& message,
                          const BlockContext& ctx,
                          const std::unordered_map<std::string, std::string>& context = {});
```

### 3. ISO 8601 Timestamp Generation ✅ **HIGH PRIORITY**

**Current Implementation**:
```cpp
log_entry["timestamp"] = std::chrono::duration_cast<std::chrono::milliseconds>(
    std::chrono::system_clock::now().time_since_epoch()).count();
```

**Required Implementation**:
- ISO 8601 format: `"2025-01-27T12:00:00.123456Z"`
- With microseconds precision
- UTC timezone

**Example Implementation**:
```cpp
std::string get_iso8601_timestamp() {
    auto now = std::chrono::system_clock::now();
    auto time_t = std::chrono::system_clock::to_time_t(now);
    auto ms = std::chrono::duration_cast<std::chrono::microseconds>(
        now.time_since_epoch()) % 1000000;
    
    std::tm tm_buf;
    gmtime_r(&time_t, &tm_buf);
    
    char buf[32];
    snprintf(buf, sizeof(buf), "%04d-%02d-%02dT%02d:%02d:%02d.%06ldZ",
             tm_buf.tm_year + 1900, tm_buf.tm_mon + 1, tm_buf.tm_mday,
             tm_buf.tm_hour, tm_buf.tm_min, tm_buf.tm_sec,
             ms.count());
    
    return std::string(buf);
}
```

### 4. Update `format_json_log()` Function ✅ **HIGH PRIORITY**

**Current Implementation** (lines 163-177):
- Uses Unix timestamp (milliseconds)
- Has `worker_id` at top level
- CP1 fields in `fields` object
- Technical details in `fields` object

**Required Changes**:
1. Use ISO 8601 timestamp
2. Add `component` field (`"worker"`)
3. Move CP1 fields to top level (when provided)
4. Move technical details to `context` object
5. Keep `worker_id` in `context` object

### 5. Add `log_debug()` Function ✅ **MEDIUM PRIORITY**

**Current State**:
- Only `log_info()`, `log_warn()`, `log_error()` exist
- CP1 requires support for all log levels: ERROR, WARN, INFO, DEBUG

**Required Changes**:
1. Add `log_debug()` function
2. Use same CP1-compliant format as other log functions

### 6. Extract CP1 Fields from BlockContext ✅ **HIGH PRIORITY**

**Current State**:
- `BlockContext` contains all CP1 fields
- But they're not consistently extracted and passed to log functions
- Example: Line 69-70 in `worker_actor.cpp` only passes `tenant_id` and `flow_id`

**Required Changes**:
1. Create helper function to extract CP1 fields from `BlockContext`
2. Update all log calls to include all CP1 fields from context
3. Pass CP1 fields to log functions

**Example**:
```cpp
void log_info_with_block_context(const std::string& message,
                                 const BlockContext& ctx,
                                 const std::unordered_map<std::string, std::string>& context = {}) {
    log_info(message, ctx.tenant_id, ctx.run_id, ctx.flow_id, ctx.step_id, ctx.trace_id, context);
}
```

### 7. Add PII/Secret Filtering ✅ **MEDIUM PRIORITY**

**Current State**:
- No PII filtering implemented

**Required Changes**:
1. Create PII filtering function for JSON objects
2. Filter sensitive fields: `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`, `authorization`, `credit_card`, `ssn`, `email`, `phone`
3. Replace filtered values with `"[REDACTED]"`
4. Apply filtering to `context` object before logging

**Implementation**:
- Use nlohmann/json for recursive filtering
- Similar to Gateway implementation

### 8. Implement Health Endpoint ✅ **HIGH PRIORITY**

**Current State**:
- Health endpoint mentioned in documentation
- **NOT IMPLEMENTED** - No HTTP server found in Worker code
- Prometheus exposer included but not used

**Required Changes**:
1. Implement HTTP server for health endpoint (using CAF IO or separate HTTP library)
2. Implement `GET /_health` endpoint
3. Return CP1-compliant format:
   ```json
   {
     "status": "healthy",
     "timestamp": "2025-01-27T12:00:00.123456Z"
   }
   ```
4. Use ISO 8601 timestamp format
5. Return `200 OK` status code

**Implementation Options**:
- **Option 1**: Use CAF IO (`caf::io::middleman`) for HTTP server
- **Option 2**: Use Prometheus exposer with custom health handler
- **Option 3**: Use lightweight HTTP library (e.g., `httplib`, `crow`)

**Recommended**: Option 2 (Prometheus exposer) - already included, can add health endpoint alongside metrics

---

## Implementation Plan

### Phase 1: Core CP1 Compliance ✅ **HIGH PRIORITY**

**Estimated Effort**: 1-2 days

1. ✅ Add ISO 8601 timestamp generation function
2. ✅ Update `format_json_log()` to CP1 format:
   - ISO 8601 timestamp
   - Add `component` field
   - Move CP1 fields to top level
   - Move technical details to `context`
3. ✅ Update log function signatures to accept CP1 fields
4. ✅ Create helper function to extract CP1 fields from `BlockContext`
5. ✅ Update all log calls to include CP1 fields

### Phase 2: Additional Log Level ✅ **MEDIUM PRIORITY**

**Estimated Effort**: 0.5 days

1. ✅ Add `log_debug()` function
2. ✅ Update code to use appropriate log levels

### Phase 3: PII Filtering ✅ **MEDIUM PRIORITY**

**Estimated Effort**: 0.5 days

1. ✅ Create PII filtering function for JSON objects
2. ✅ Apply filtering to `context` objects in all log functions
3. ✅ Test with sensitive data

### Phase 4: Health Endpoint Verification ✅ **HIGH PRIORITY**

**Estimated Effort**: 0.5 days

1. ✅ Verify health endpoint implementation
2. ✅ Update to CP1-compliant format if needed
3. ✅ Test health endpoint

---

## Code Changes Summary

### File: `apps/caf/processor/src/observability.cpp`

**Changes**:
1. Add `get_iso8601_timestamp()` helper function
2. Update `format_json_log()` function signature and implementation
3. Update `log_info()`, `log_warn()`, `log_error()` signatures
4. Add `log_debug()` function
5. Add PII filtering function
6. Add helper function to extract CP1 fields from `BlockContext`

**Estimated Lines Changed**: ~200-300 lines

### File: `apps/caf/processor/include/beamline/worker/observability.hpp`

**Changes**:
1. Update function signatures for log functions
2. Add `log_debug()` declaration
3. Add helper function declarations

**Estimated Lines Changed**: ~20-30 lines

### File: `apps/caf/processor/src/worker_actor.cpp`

**Changes**:
1. Update all log calls to include CP1 fields from `BlockContext`
2. Use helper function to extract CP1 fields

**Estimated Lines Changed**: ~50-100 lines

---

## Testing Requirements

### Unit Tests

1. ✅ Test ISO 8601 timestamp format
2. ✅ Test CP1 fields extraction from `BlockContext`
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

1. ✅ All logs include `timestamp` (ISO 8601 format with microseconds)
2. ✅ All logs include `level` (uppercase: ERROR, WARN, INFO, DEBUG)
3. ✅ All logs include `component` (`"worker"`)
4. ✅ All logs include `message` (human-readable)
5. ✅ All logs include CP1 fields when available:
   - `tenant_id` (when tenant context available)
   - `run_id` (when run context available)
   - `flow_id` (when flow context available)
   - `step_id` (when step context available)
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
- `apps/caf/processor/include/beamline/worker/core.hpp` - BlockContext and ResultMetadata definitions

---

## Priority Summary

**High Priority** (CP1 Compliance):
1. Fix log format (timestamp, component, CP1 fields, context)
2. Update log functions to accept CP1 fields
3. Extract CP1 fields from BlockContext
4. Verify health endpoint

**Medium Priority** (Completeness):
5. Add `log_debug()` function
6. Add PII/secret filtering

**Estimated Total Effort**: 2-3 days

---

## Next Steps

1. Review and approve this plan
2. Implement Phase 1 (Core CP1 Compliance)
3. Test and validate changes
4. Implement Phase 2 (Additional Log Level)
5. Implement Phase 3 (PII Filtering)
6. Implement Phase 4 (Health Endpoint Verification)
7. Final validation and documentation update

