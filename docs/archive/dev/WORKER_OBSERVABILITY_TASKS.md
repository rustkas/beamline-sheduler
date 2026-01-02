# Worker Observability - Task List

**Date**: 2025-01-27  
**Component**: Worker CAF (`apps/caf/processor/`)  
**Status**: 📋 **TASKS**  
**Priority**: Medium (Testing, Documentation, Validation)

---

## Task List

### ✅ Task 1: Create Observability Unit Tests - **MEDIUM**

**Priority**: 🟡 **MEDIUM** (Testing)  
**Estimated Time**: 1-2 hours  
**Status**: ✅ **COMPLETED** (2025-01-27)

#### Description

Create unit tests for Worker observability features to verify:
- Log format compliance (CP1 format)
- CP1 fields extraction and formatting
- PII filtering functionality
- Timestamp format (ISO 8601 with microseconds)
- Health endpoint response format

#### Required Tests

1. **Log Format Tests**:
   - Test `format_json_log()` returns CP1-compliant JSON
   - Test required fields (`timestamp`, `level`, `component`, `message`)
   - Test CP1 fields at top level (`tenant_id`, `run_id`, `flow_id`, `step_id`, `trace_id`)
   - Test `context` object structure
   - Test ISO 8601 timestamp format (6 digits for microseconds)

2. **CP1 Fields Tests**:
   - Test CP1 fields extraction from `BlockContext`
   - Test `log_*_with_context()` helper functions
   - Test CP1 fields are only added when available (not empty)

3. **PII Filtering Tests**:
   - Test `filter_pii_recursive()` filters sensitive fields
   - Test case-insensitive field name matching
   - Test recursive filtering in nested objects and arrays
   - Test filtered values are replaced with `"[REDACTED]"`

4. **Timestamp Tests**:
   - Test `get_iso8601_timestamp()` returns ISO 8601 format
   - Test timestamp has 6 digits for microseconds
   - Test timestamp is in UTC (ends with `Z`)

5. **Health Endpoint Tests**:
   - Test `get_health_response()` returns CP1-compliant JSON
   - Test response contains `status: "healthy"`
   - Test response contains ISO 8601 `timestamp`

#### Test File Structure

**File**: `apps/caf/processor/tests/test_observability.cpp`

**Test Structure**:
```cpp
#include <gtest/gtest.h>
#include "beamline/worker/observability.hpp"
#include "beamline/worker/core.hpp"
#include <nlohmann/json.hpp>

using json = nlohmann::json;

class ObservabilityTest : public ::testing::Test {
protected:
    void SetUp() override {
        observability_ = std::make_unique<beamline::worker::Observability>("test_worker");
    }
    
    std::unique_ptr<beamline::worker::Observability> observability_;
};

TEST_F(ObservabilityTest, LogFormatCompliance) {
    // Test log format
}

TEST_F(ObservabilityTest, CP1FieldsAtTopLevel) {
    // Test CP1 fields
}

TEST_F(ObservabilityTest, PIIFiltering) {
    // Test PII filtering
}

TEST_F(ObservabilityTest, TimestampFormat) {
    // Test timestamp format
}

TEST_F(ObservabilityTest, HealthEndpointResponse) {
    // Test health endpoint
}
```

#### Acceptance Criteria

- ✅ All tests pass
- ✅ Tests verify CP1 compliance
- ✅ Tests cover all observability features
- ✅ Tests are integrated into CMake build system

#### Files to Create

- `apps/caf/processor/tests/test_observability.cpp` - Unit tests for observability

---

### ✅ Task 2: Create Health Endpoint Integration Test - **MEDIUM**

**Priority**: 🟡 **MEDIUM** (Testing)  
**Estimated Time**: 30-60 minutes  
**Status**: ✅ **COMPLETED** (2025-01-27)

#### Description

Create integration test for Worker health endpoint that:
- Starts Worker with health endpoint
- Makes HTTP request to `GET /_health`
- Validates response format (CP1 compliant)
- Validates HTTP status code (200 OK)
- Validates JSON response structure

#### Test File Structure

**File**: `apps/caf/processor/tests/test_health_endpoint.cpp`

**Test Structure**:
```cpp
#include <gtest/gtest.h>
#include <curl/curl.h>
#include <nlohmann/json.hpp>
#include "beamline/worker/observability.hpp"

class HealthEndpointTest : public ::testing::Test {
protected:
    void SetUp() override {
        // Start health endpoint on test port
    }
    
    void TearDown() override {
        // Stop health endpoint
    }
};

TEST_F(HealthEndpointTest, HealthEndpointReturns200) {
    // Test HTTP status code
}

TEST_F(HealthEndpointTest, HealthEndpointReturnsCP1Format) {
    // Test JSON response format
}

TEST_F(HealthEndpointTest, HealthEndpointHasTimestamp) {
    // Test timestamp in response
}
```

#### Acceptance Criteria

- ✅ Test starts Worker health endpoint
- ✅ Test makes HTTP request to `/_health`
- ✅ Test validates 200 OK response
- ✅ Test validates CP1-compliant JSON format
- ✅ Test validates ISO 8601 timestamp

#### Files to Create

- `apps/caf/processor/tests/test_health_endpoint.cpp` - Health endpoint integration test

---

### ✅ Task 3: Create Test Script for Worker Observability - **MEDIUM**

**Priority**: 🟡 **MEDIUM** (Testing)  
**Estimated Time**: 30-60 minutes  
**Status**: ✅ **DONE**

#### Description

Create test script similar to `test_gateway_observability.sh` for Worker that:
- Tests health endpoint with real HTTP requests
- Validates log format (if logs are accessible)
- Validates CP1 fields in logs
- Validates health endpoint format

#### Script Structure

**File**: `scripts/observability/test_worker_observability.sh`

**Features**:
- Test health endpoint (`GET /_health`)
- Validate JSON response format
- Validate CP1-compliant health endpoint format
- Check log format (if logs accessible)
- Error handling and reporting

#### Acceptance Criteria

- ✅ Script tests health endpoint
- ✅ Script validates CP1-compliant format
- ✅ Script provides clear error messages
- ✅ Script is executable and documented

#### Files to Create

- `scripts/observability/test_worker_observability.sh` - Worker observability test script
- `docs/archive/dev/WORKER_OBSERVABILITY_TEST.md` - Documentation for test script

---

### ✅ Task 4: Update Validation Scripts for Worker - **MEDIUM**

**Priority**: 🟡 **MEDIUM** (Validation)  
**Estimated Time**: 30-60 minutes  
**Status**: ✅ **DONE**

#### Description

Update validation scripts to properly validate Worker observability:
- Update `validate_observability_e2e.sh` to check Worker health endpoint port (9091, not 8080)
- Add Worker log format validation (if logs accessible)
- Add Worker CP1 fields validation
- Update error messages for Worker-specific issues

#### Current Issues

**File**: `scripts/observability/validate_observability_e2e.sh`

**Line 456**: Currently checks `http://localhost:8080/_health` but Worker uses port **9091** (or configurable).

**Required Changes**:
1. Update Worker health endpoint check to use correct port (9091)
2. Add Worker log format validation
3. Add Worker CP1 fields validation
4. Update error messages

#### Acceptance Criteria

- ✅ Validation script checks correct Worker port (9091)
- ✅ Validation script validates Worker log format
- ✅ Validation script validates Worker CP1 fields
- ✅ Validation script provides clear error messages

#### Files to Modify

- `scripts/observability/validate_observability_e2e.sh` - Update Worker validation

---

### ✅ Task 5: Create Worker Observability Documentation - **LOW**

**Priority**: 🟢 **LOW** (Documentation)  
**Estimated Time**: 30-60 minutes  
**Status**: ✅ **COMPLETED** (2025-01-27)

#### Description

Create comprehensive documentation for Worker observability:
- Log format specification
- CP1 fields usage
- Health endpoint documentation
- Testing guide
- Troubleshooting guide

#### Documentation Structure

**File**: `apps/caf/processor/docs/OBSERVABILITY.md`

**Sections**:
1. **Structured JSON Logging**
   - Log format specification
   - Required fields
   - CP1 correlation fields
   - Context object
   - PII filtering

2. **Health Endpoint**
   - Endpoint specification
   - Response format
   - Testing guide

3. **Usage Examples**
   - Basic logging
   - Logging with CP1 fields
   - Logging with BlockContext
   - Error logging

4. **Testing**
   - Unit tests
   - Integration tests
   - E2E validation

5. **Troubleshooting**
   - Common issues
   - Debugging tips

#### Acceptance Criteria

- ✅ Documentation covers all observability features
- ✅ Documentation includes examples
- ✅ Documentation is clear and comprehensive
- ✅ Documentation matches actual implementation

#### Files to Create

- `apps/caf/processor/docs/OBSERVABILITY.md` - Worker observability documentation

---

### ✅ Task 6: Verify Health Endpoint Port Configuration - **LOW**

**Priority**: 🟢 **LOW** (Configuration)  
**Estimated Time**: 15-30 minutes  
**Status**: ✅ **DONE**

#### Description

Verify and document Worker health endpoint port configuration:
- Check if port is configurable
- Document default port (9091)
- Document how to configure port
- Update validation scripts if needed

#### Current Implementation

**File**: `apps/caf/processor/src/main.cpp` (lines 49-61)

**Current Behavior**:
- Default port: 9091 (Prometheus port + 1)
- Port is derived from `prometheus_endpoint` config
- Health endpoint address: `0.0.0.0` (all interfaces)

#### Required Actions

1. **Verify Configuration**:
   - Check if port can be configured via environment variable
   - Check if port can be configured via config file
   - Document configuration options

2. **Update Documentation**:
   - Document default port (9091)
   - Document configuration options
   - Update `OBSERVABILITY_HEALTH_ENDPOINTS.md` if needed

#### Acceptance Criteria

- ✅ Port configuration is documented
- ✅ Default port is clearly stated
- ✅ Configuration options are documented
- ✅ Validation scripts use correct port

#### Files to Modify

- `apps/caf/processor/docs/OBSERVABILITY.md` (when created)
- `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md` - Update Worker port information

---

## Summary

### Task Summary

| Task | Priority | Status | Estimated Time |
|------|----------|--------|----------------|
| Task 1: Create Observability Unit Tests | 🟡 MEDIUM | ✅ COMPLETED | 1-2 hours |
| Task 2: Create Health Endpoint Integration Test | 🟡 MEDIUM | ✅ COMPLETED | 30-60 min |
| Task 3: Create Test Script for Worker Observability | 🟡 MEDIUM | ✅ DONE | 30-60 min |
| Task 4: Update Validation Scripts for Worker | 🟡 MEDIUM | ✅ DONE | 30-60 min |
| Task 5: Create Worker Observability Documentation | 🟢 LOW | ✅ COMPLETED | 30-60 min |
| Task 6: Verify Health Endpoint Port Configuration | 🟢 LOW | ✅ DONE | 15-30 min |

**Total Estimated Time**: 3-5 hours

### Current CP1 Compliance Status

| Requirement | Status | Notes |
|------------|--------|-------|
| Structured JSON logs | ✅ | CP1-compliant format implemented |
| ISO 8601 timestamps | ✅ | Microseconds (6 digits) |
| CP1 correlation fields | ✅ | At top level |
| PII filtering | ✅ | Recursive filtering implemented |
| All log levels | ✅ | ERROR, WARN, INFO, DEBUG |
| Health endpoint | ✅ | HTTP `/_health` implemented |
| **Unit tests** | ✅ | **Complete** |
| **Integration tests** | ✅ | **Complete** |
| **Test scripts** | ✅ | **Complete** |
| **Documentation** | ✅ | **Complete** |
| **Validation scripts** | ✅ | **Updated** (port 9091) |

### After Completion

| Requirement | Status | Notes |
|------------|--------|-------|
| Structured JSON logs | ✅ | CP1-compliant format |
| ISO 8601 timestamps | ✅ | Microseconds (6 digits) |
| CP1 correlation fields | ✅ | At top level |
| PII filtering | ✅ | Recursive filtering |
| All log levels | ✅ | ERROR, WARN, INFO, DEBUG |
| Health endpoint | ✅ | HTTP `/_health` |
| **Unit tests** | ✅ | **Complete** |
| **Integration tests** | ✅ | **Complete** |
| **Test scripts** | ✅ | **Complete** |
| **Documentation** | ✅ | **Complete** |
| **Validation scripts** | ✅ | **Updated** |

---

## Implementation Order

1. ✅ **Task 6** - Verify health endpoint port (quick check)
2. ✅ **Task 4** - Update validation scripts (fix port issue)
3. ✅ **Task 1** - Create unit tests (foundation for testing)
4. ✅ **Task 2** - Create integration tests (health endpoint)
5. ✅ **Task 3** - Create test script (E2E testing)
6. ✅ **Task 5** - Create documentation (finalize)

---

## References

- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants specification
- `docs/OBSERVABILITY.md` - General observability requirements
- `docs/archive/dev/WORKER_OBSERVABILITY_COMPLETION_REPORT.md` - Worker observability completion report
- `apps/caf/processor/src/observability.cpp` - Observability implementation
- `apps/caf/processor/include/beamline/worker/observability.hpp` - Observability header
- `scripts/observability/validate_observability_e2e.sh` - E2E validation script
- `scripts/observability/test_gateway_observability.sh` - Gateway test script (reference)

