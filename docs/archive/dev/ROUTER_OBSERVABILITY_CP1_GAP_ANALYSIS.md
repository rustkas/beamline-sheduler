# Router Observability CP1 Gap Analysis

**Date**: 2025-01-27  
**Status**: Analysis Complete  
**Version**: 1.0

## Purpose

This document identifies gaps between CP1 observability requirements and current Router implementation, mapping requirements to code and tests.

---

## CP1 Observability Requirements Matrix

### Logging Requirements

| CP1 Requirement | Code Implementation | Test Coverage | Gap Status |
|----------------|-------------------|---------------|------------|
| **Structured JSON Format** | ✅ `router_logger.erl:72-89` | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **Required Fields (timestamp, level, component, message)** | ✅ `router_logger.erl:92-130` | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **Optional Fields (trace_id, tenant_id, context)** | ✅ `router_logger.erl:98-128` | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **PII/Secret Filtering** | ✅ `router_logger.erl:148-190` | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **Log Levels (ERROR, WARN, INFO, DEBUG)** | ✅ `router_logger.erl:202-208` | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |

### Health Endpoint Requirements

| CP1 Requirement | Code Implementation | Test Coverage | Gap Status |
|----------------|-------------------|---------------|------------|
| **Health Endpoint (gRPC on port 9000)** | ✅ `router_grpc_sup.erl` (gRPC health service) | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **Health Response Format (status, timestamp)** | ✅ gRPC health protocol | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |

### Logging Scenarios

| Scenario | Code Implementation | Test Coverage | Gap Status |
|----------|-------------------|---------------|------------|
| **NATS Errors: Logged with context** | ✅ `router_nats_subscriber.erl:81-85, 98-108` | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **Routing Errors: Logged with context** | ✅ `router_core.erl:116-148` | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **Invalid Payloads: Logged with validation error** | ✅ `router_nats_subscriber.erl:111-118` | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |
| **Internal Errors: Logged with error details** | ✅ `router_core.erl:116-148` | ⚠️ Partial | ⚠️ **GAP: Needs explicit test** |

---

## Detailed Gap Analysis

### Gap 1: Log Format Validation Test

**Requirement**: All Router logs must use structured JSON format with required fields

**Current State**:
- `router_logger.erl:72-89` - Structured JSON logging implemented
- `router_logger.erl:92-130` - Required fields (timestamp, level, component, message) implemented
- ⚠️ No explicit test for log format validation

**Gap**:
- ✅ Log format implementation exists
- ❌ No explicit test for log format validation
- ❌ No test for required fields presence

**Priority**: **HIGH**

**Action Required**:
- Create `router_observability_SUITE.erl` (or add to existing test suite)
- Add test for log format validation (JSON structure)
- Add test for required fields presence
- Add test for optional fields (when available)

---

### Gap 2: PII/Secret Filtering Test

**Requirement**: All sensitive data must be filtered before logging

**Current State**:
- `router_logger.erl:148-190` - PII filtering implemented
- `router_logger.erl:35-42` - PII fields list defined
- ⚠️ No explicit test for PII filtering

**Gap**:
- ✅ PII filtering implementation exists
- ❌ No explicit test for PII filtering
- ❌ No test for secret pattern detection

**Priority**: **HIGH**

**Action Required**:
- Add test in `router_observability_SUITE.erl` for PII filtering
- Test all PII fields are filtered
- Test secret pattern detection in values
- Test recursive filtering in nested maps

---

### Gap 3: Log Level Validation Test

**Requirement**: Correct log levels must be used (ERROR, WARN, INFO, DEBUG)

**Current State**:
- `router_logger.erl:202-208` - Log level conversion implemented
- ⚠️ No explicit test for log level validation

**Gap**:
- ✅ Log level implementation exists
- ❌ No explicit test for log level validation
- ❌ No test for log level usage correctness

**Priority**: **MEDIUM**

**Action Required**:
- Add test in `router_observability_SUITE.erl` for log level validation
- Test all log levels are correctly used
- Test log level conversion to binary

---

### Gap 4: Health Endpoint Test

**Requirement**: Health endpoint must return correct format (gRPC health protocol)

**Current State**:
- `router_grpc_sup.erl` - gRPC health service configured
- Port 9000 (default, configurable)
- ⚠️ No explicit test for health endpoint

**Gap**:
- ✅ Health endpoint implementation exists
- ❌ No explicit test for health endpoint
- ❌ No test for health response format

**Priority**: **MEDIUM**

**Action Required**:
- Add test in `router_observability_SUITE.erl` for health endpoint
- Test health endpoint availability
- Test health response format (gRPC health protocol)
- Test health status (SERVING/UNKNOWN/NOT_SERVING)

---

### Gap 5: Logging Scenarios Test

**Requirement**: All key CP1 scenarios must be logged correctly

**Current State**:
- NATS errors logged in `router_nats_subscriber.erl:81-85, 98-108`
- Routing errors logged in `router_core.erl:116-148`
- Invalid payloads logged in `router_nats_subscriber.erl:111-118`
- ⚠️ No explicit test for logging scenarios

**Gap**:
- ✅ Logging implementation exists for all scenarios
- ❌ No explicit test for logging scenarios
- ❌ No test for log context completeness

**Priority**: **MEDIUM**

**Action Required**:
- Add test in `router_observability_SUITE.erl` for logging scenarios
- Test NATS error logging
- Test routing error logging
- Test invalid payload logging
- Test internal error logging
- Verify log context completeness

---

## Test Coverage Gaps

### Missing Tests

1. **Log Format Validation**:
   - ❌ No test for JSON structure validation
   - ❌ No test for required fields presence
   - ❌ No test for optional fields (when available)

2. **PII/Secret Filtering**:
   - ❌ No test for PII field filtering
   - ❌ No test for secret pattern detection
   - ❌ No test for recursive filtering

3. **Log Level Validation**:
   - ❌ No test for log level correctness
   - ❌ No test for log level conversion

4. **Health Endpoint**:
   - ❌ No test for health endpoint availability
   - ❌ No test for health response format

5. **Logging Scenarios**:
   - ❌ No test for NATS error logging
   - ❌ No test for routing error logging
   - ❌ No test for invalid payload logging
   - ❌ No test for internal error logging

---

## Implementation Priority

### High Priority (Blocking CP1)

1. **Gap 1: Log Format Validation Test** - Add log format validation test
2. **Gap 2: PII/Secret Filtering Test** - Add PII filtering test

### Medium Priority (Non-blocking)

3. **Gap 3: Log Level Validation Test** - Add log level validation test
4. **Gap 4: Health Endpoint Test** - Add health endpoint test
5. **Gap 5: Logging Scenarios Test** - Add logging scenarios test

---

## Code References

### Logging Implementation

- **Module**: `router_logger.erl`
  - **Function**: `log/3` - Structured JSON logging
  - **Location**: `apps/otp/router/src/router_logger.erl:72-89`
  - **Test**: ⚠️ No explicit test suite

- **Module**: `router_logger.erl`
  - **Function**: `build_log_entry/3` - Build log entry with required fields
  - **Location**: `apps/otp/router/src/router_logger.erl:92-130`
  - **Test**: ⚠️ No explicit test suite

- **Module**: `router_logger.erl`
  - **Function**: `filter_pii/1` - PII filtering
  - **Location**: `apps/otp/router/src/router_logger.erl:148-190`
  - **Test**: ⚠️ No explicit test suite

### Health Endpoint

- **Module**: `router_grpc_sup.erl`
  - **Function**: gRPC health service configuration
  - **Location**: `apps/otp/router/src/router_grpc_sup.erl`
  - **Test**: ⚠️ No explicit test suite

### Logging Scenarios

- **Module**: `router_nats_subscriber.erl`
  - **Function**: `handle_nats_message/2` - NATS error logging
  - **Location**: `apps/otp/router/src/router_nats_subscriber.erl:81-85, 98-108`
  - **Test**: ⚠️ No explicit test suite

- **Module**: `router_core.erl`
  - **Function**: `route/2` - Routing error logging
  - **Location**: `apps/otp/router/src/router_core.erl:116-148`
  - **Test**: ⚠️ No explicit test suite

---

## Test Suite Coverage

### Existing Test Suites

1. **router_secrets_logging_SUITE.erl** (if exists):
   - ⚠️ May have PII filtering tests
   - Status: Needs verification

2. **router_state_observability_SUITE.erl** (if exists):
   - ⚠️ May have observability tests
   - Status: Needs verification

### Missing Test Suite

3. **router_observability_SUITE.erl**:
   - ❌ Does not exist
   - **Action Required**: Create new test suite

---

## Recommendations

### Immediate Actions (Before CP1 Acceptance)

1. **Create Observability Test Suite**:
   - Create `router_observability_SUITE.erl`
   - Add log format validation tests
   - Add PII filtering tests

2. **Add Log Format Validation**:
   - Test JSON structure
   - Test required fields presence
   - Test optional fields (when available)

3. **Add PII Filtering Tests**:
   - Test all PII fields are filtered
   - Test secret pattern detection
   - Test recursive filtering

### Short-term Actions (CP1 Enhancement)

4. **Add Log Level Validation**:
   - Test log level correctness
   - Test log level conversion

5. **Add Health Endpoint Test**:
   - Test health endpoint availability
   - Test health response format

6. **Add Logging Scenarios Tests**:
   - Test NATS error logging
   - Test routing error logging
   - Test invalid payload logging
   - Test internal error logging

---

## References

- **Observability Conventions**: `docs/OBSERVABILITY_CONVENTIONS.md`
- **Observability**: `docs/OBSERVABILITY.md`
- **Router Logger**: `apps/otp/router/src/router_logger.erl`
- **CP1 Acceptance Report**: `docs/CP1_ACCEPTANCE_REPORT.md`
- **Router CP1 Report**: `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md`

