# Router Observability - Task List

**Date**: 2025-01-27  
**Component**: Router (`apps/otp/router/`)  
**Status**: 📋 **TASKS**  
**Priority**: Medium (Testing, Documentation, Validation)

---

## Current Status

### ✅ Completed

- ✅ CP1-compliant structured JSON logging
- ✅ ISO 8601 timestamps with **microseconds** (6 digits)
- ✅ CP1 correlation fields at **top level**
- ✅ PII/secret filtering
- ✅ All log levels (ERROR, WARN, INFO, DEBUG)
- ✅ gRPC health check (documented)
- ✅ Unit tests (`router_observability_SUITE.erl`)
- ✅ Documentation (`apps/otp/router/docs/OBSERVABILITY.md`)

### ❌ Missing

- ❌ **Test script** for Router observability (E2E testing with real gRPC requests)
- ❌ **Integration test** for gRPC health endpoint (full test with grpc_health_probe or grpcurl)
- ❌ **Documentation** for test script usage

---

## Task List

### ✅ Task 1: Create Test Script for Router Observability - **MEDIUM**

**Priority**: 🟡 **MEDIUM** (Testing)  
**Estimated Time**: 1-2 hours  
**Status**: ✅ **DONE** (2025-01-27)

#### Description

Create a test script for Router observability similar to `test_gateway_observability.sh` and `test_worker_observability.sh`, but adapted for Router's gRPC health check.

**Requirements**:
- Test gRPC health endpoint using `grpc_health_probe` or `grpcurl`
- Validate health check response format
- Test log format (if logs are accessible)
- Validate CP1 fields in logs (if logs are accessible)
- Error handling and reporting
- Exit codes: 0 = passed, 1 = failed, 2 = service not running

**Script Location**: `scripts/observability/test_router_observability.sh`

**Dependencies**:
- `grpc_health_probe` or `grpcurl` (for gRPC health checks)
- `jq` (for JSON parsing, if testing logs)
- Router running on port 9000 (default)

**Example Usage**:
```bash
# Basic usage (port 9000 by default)
bash scripts/observability/test_router_observability.sh

# With custom port
ROUTER_PORT=9000 bash scripts/observability/test_router_observability.sh
```

**Test Scenarios**:
1. ✅ Health endpoint check via `grpc_health_probe`
2. ✅ Health endpoint check via `grpcurl` (if available)
3. ✅ Validate health response (SERVING status)
4. ⚠️ Log format validation (if logs accessible)
5. ⚠️ CP1 fields validation (if logs accessible)

**Note**: Router uses gRPC (not HTTP), so the script will be different from Gateway/Worker scripts.

---

### ✅ Task 2: Create Integration Test for gRPC Health Endpoint - **MEDIUM**

**Priority**: 🟡 **MEDIUM** (Testing)  
**Estimated Time**: 1-2 hours  
**Status**: ✅ **DONE** (2025-01-27)

#### Description

Create an integration test for Router's gRPC health endpoint that:
- Tests health endpoint startup
- Tests health endpoint response format
- Tests health endpoint with real gRPC requests (if possible)
- Validates CP1 compliance (if applicable)

**Test File**: `apps/otp/router/test/router_health_integration_SUITE.erl`

**Requirements**:
- Test gRPC health service availability
- Test health check response format
- Test health status values (SERVING, UNKNOWN, NOT_SERVING)
- Test health endpoint configuration

**Current State**:
- Basic test exists in `router_observability_SUITE.erl:test_health_endpoint/1`
- Only checks configuration, not actual health check

**Enhancement Needed**:
- Add actual gRPC health check test (if grpcbox_health_service is accessible in tests)
- Add health status validation
- Add error handling tests

**Note**: May require mocking gRPC client or using actual gRPC client library in tests.

---

### ✅ Task 3: Create Documentation for Router Observability Test Script - **LOW**

**Priority**: 🟢 **LOW** (Documentation)  
**Estimated Time**: 30-60 minutes  
**Status**: ✅ **DONE** (2025-01-27)

#### Description

Create documentation for the Router observability test script, similar to `docs/archive/dev/GATEWAY_OBSERVABILITY_TEST.md` and `docs/archive/dev/WORKER_OBSERVABILITY_TEST.md`.

**Documentation File**: `docs/archive/dev/ROUTER_OBSERVABILITY_TEST.md`

**Content**:
- Script description and purpose
- Prerequisites (grpc_health_probe, grpcurl, Router running)
- Usage instructions
- Test scenarios
- Expected results
- Troubleshooting guide
- Integration with CI/CD

**Sections**:
1. Overview
2. Prerequisites
3. Usage
4. Test Scenarios
5. Expected Results
6. Troubleshooting
7. CI/CD Integration

---

### ✅ Task 4: Verify Router Observability Documentation Completeness - **LOW**

**Priority**: 🟢 **LOW** (Documentation)  
**Estimated Time**: 30-60 minutes  
**Status**: ❌ **NOT DONE**

#### Description

Verify that `apps/otp/router/docs/OBSERVABILITY.md` is complete and includes:
- All CP1 requirements
- Complete usage examples
- Health endpoint documentation (gRPC)
- Testing guide
- Troubleshooting section
- References to test scripts

**Checklist**:
- ✅ Log format specification
- ✅ CP1 fields documentation
- ✅ PII filtering documentation
- ✅ Health endpoint documentation (gRPC)
- ⚠️ Testing guide (may need update with test script)
- ⚠️ Troubleshooting section
- ⚠️ References to test scripts

**Action Items**:
- Review documentation completeness
- Add missing sections if needed
- Update references to test scripts
- Add troubleshooting guide

---

## Summary

| Task | Priority | Status | Estimated Time |
|------|----------|--------|----------------|
| Task 1: Create Test Script for Router Observability | 🟡 MEDIUM | ✅ DONE | 1-2 hours |
| Task 2: Create Integration Test for gRPC Health Endpoint | 🟡 MEDIUM | ✅ DONE | 1-2 hours |
| Task 3: Create Documentation for Test Script | 🟢 LOW | ✅ DONE | 30-60 min |
| Task 4: Verify Documentation Completeness | 🟢 LOW | ❌ NOT DONE | 30-60 min |

**Total Estimated Time**: 3-5 hours

---

## Comparison with Gateway and Worker

| Feature | Gateway | Worker | Router |
|---------|---------|--------|--------|
| **Unit tests** | ❌ | ✅ | ✅ |
| **Integration tests** | ❌ | ✅ | ⚠️ (basic only) |
| **Test scripts** | ✅ | ✅ | ❌ |
| **Test script documentation** | ✅ | ✅ | ❌ |
| **Documentation** | ✅ | ✅ | ✅ |

**Router Gaps**:
- ✅ Test script for E2E validation (DONE)
- ✅ Full integration test for health endpoint (DONE)
- ✅ Test script documentation (DONE)

---

## References

- `apps/otp/router/docs/OBSERVABILITY.md` - Router observability documentation
- `apps/otp/router/test/router_observability_SUITE.erl` - Unit tests
- `scripts/observability/test_gateway_observability.sh` - Gateway test script (reference)
- `scripts/observability/test_worker_observability.sh` - Worker test script (reference)
- `docs/archive/dev/GATEWAY_OBSERVABILITY_TEST.md` - Gateway test script documentation (reference)
- `docs/archive/dev/WORKER_OBSERVABILITY_TEST.md` - Worker test script documentation (reference)
- `scripts/observability/validate_observability_e2e.sh` - E2E validation script
