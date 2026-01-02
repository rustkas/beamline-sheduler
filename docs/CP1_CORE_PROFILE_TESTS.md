# CP1 Core Profile: Tests

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Purpose**: Define CP1 core test requirements for Router, Gateway, and Worker components  
**Status**: ✅ **ACTIVE**

---

## Overview

This document defines the **minimal mandatory tests** for CP1. It explicitly separates CP1 required tests from CP2+ enhancements (performance tests, load tests, stress tests) to simplify CP1 validation and prevent CP2+ elements from becoming de facto mandatory.

**Key Principle**: CP1 tests are **minimal and focused** on core functionality, contracts, observability, and basic integration. Advanced tests (performance, load, stress, edge cases) are explicitly **CP2+**.

---

## CP1 Core Tests (Mandatory)

### 1. Contract Tests

**Required**:
- ✅ `test_worker_router_contract.cpp` - StepResult → ExecResult conversion tests
  - Status mapping tests (success, error, timeout, cancelled)
  - Error code mapping tests (1xxx-5xxx)
  - Metadata preservation tests (correlation IDs)
- ✅ `router_worker_contract_SUITE.erl` - Router-side ExecResult processing tests
  - ExecResult validation tests
  - Correlation fields preservation tests

**Test Files**:
- `apps/caf/processor/tests/test_worker_router_contract.cpp` - Worker-side contract tests
- `apps/otp/router/test/router_worker_contract_SUITE.erl` - Router-side contract tests

**Reference**: `docs/archive/dev/WORKER_ROUTER_CONTRACT_TESTS.md`

---

### 2. Core Functionality Tests

#### Router (Erlang/OTP)

**Required**:
- ✅ Assignment validation tests: Invalid `ExecAssignment` rejection
- ✅ Routing decision tests: Provider selection logic
- ✅ CP1 fields propagation tests: Correlation fields through flow
- ✅ Error handling tests: Graceful error handling (no crashes)

**Test Files**:
- `apps/otp/router/test/router_cp1_fields_integration_SUITE.erl` - CP1 fields integration tests
- `apps/otp/router/test/router_observability_SUITE.erl` - Observability tests

**Test Groups**:
- `cp1_fields_validation_tests`: 6 tests for validation of required CP1 fields
- `cp1_fields_propagation_tests`: 5 tests for end-to-end propagation
- `cp1_fields_error_tests`: 1 test for error handling

#### Gateway (C-Gateway)

**Required**:
- ✅ Request validation tests: Invalid request rejection
- ✅ Routing request tests: Request forwarding to Router
- ✅ Error handling tests: Graceful error handling (no crashes)

**Test Files**:
- `apps/c-gateway/tests/test_observability.c` - Observability tests (includes request validation)

#### Worker (CAF/C++)

**Required**:
- ✅ Assignment validation tests: Invalid `ExecAssignment` rejection
- ✅ Block execution tests: HTTP, FS, SQL blocks (basic happy path)
- ✅ Status reporting tests: All status codes (success, error, timeout, cancelled)
- ✅ Error handling tests: Graceful error handling (no crashes)

**Test Files**:
- `apps/caf/processor/tests/test_core.cpp` - Core data structures and contract tests
- `apps/caf/processor/tests/test_block_executor.cpp` - Block execution tests (basic)

---

### 3. Observability Tests

#### Router (Erlang/OTP)

**Required**:
- ✅ Log format tests: Structured JSON format validation
- ✅ CP1 fields tests: Correlation fields extraction and logging
- ✅ PII filtering tests: Secret filtering validation
- ✅ Health endpoint tests: gRPC health endpoint availability and format

**Test Files**:
- `apps/otp/router/test/router_observability_SUITE.erl` - Observability unit tests
- `apps/otp/router/test/router_health_integration_SUITE.erl` - Health endpoint integration tests

**CP1 Core Test Groups**:
- `log_format_tests` - JSON log format validation (6 tests)
- `pii_filtering_tests` - PII/secret filtering validation (2 tests)
- `health_endpoint_tests` - Health endpoint validation (1 test)
- `logging_scenarios_tests` - Error logging scenarios (4 tests)

**Total**: 12 unit tests + 6 integration tests = 18 tests

#### Gateway (C-Gateway)

**Required**:
- ✅ Log format tests: Structured JSON format validation
- ✅ CP1 fields tests: Correlation fields extraction and logging
- ✅ PII filtering tests: Secret filtering validation
- ✅ Health endpoint tests: HTTP endpoint availability and format

**Test Files**:
- `apps/c-gateway/tests/test_observability.c` - Observability unit tests (11 tests)
- `apps/c-gateway/tests/test_health_endpoint.c` - Health endpoint integration tests (10 tests)

**CP1 Core Tests**:
- `test_log_format_json_structure` - Validates CP1-compliant JSON log structure
- `test_log_required_fields` - Validates required fields (timestamp, level, component, message)
- `test_cp1_fields_at_top_level` - Validates CP1 fields are at top level
- `test_iso8601_timestamp_format` - Validates ISO 8601 timestamp format with microseconds
- `test_all_log_levels` - Validates all log levels (ERROR, WARN, INFO, DEBUG)
- `test_pii_filtering_sensitive_fields` - Validates PII filtering
- `test_log_context_object` - Validates context object structure
- `test_error_logging_format` - Validates ERROR level logging
- `test_warn_logging_format` - Validates WARN level logging
- `test_debug_logging_format` - Validates DEBUG level logging

**Total**: 11 unit tests + 10 integration tests = 21 tests

#### Worker (CAF/C++)

**Required**:
- ✅ Log format tests: Structured JSON format validation
- ✅ CP1 fields tests: Correlation fields extraction and logging
- ✅ PII filtering tests: Secret filtering validation
- ✅ Health endpoint tests: HTTP endpoint availability and format

**Test Files**:
- `apps/caf/processor/tests/test_observability.cpp` - Observability unit tests (8 tests)
- `apps/caf/processor/tests/test_health_endpoint.cpp` - Health endpoint integration tests (4 tests)

**CP1 Core Tests**:
- `test_timestamp_format()` - Verifies ISO 8601 timestamp format with microseconds
- `test_log_format_compliance()` - Verifies CP1-compliant log format
- `test_cp1_fields_at_top_level()` - Verifies CP1 fields at top level
- `test_cp1_fields_with_context()` - Verifies CP1 fields extraction from BlockContext
- `test_pii_filtering()` - Verifies PII filtering functionality
- `test_all_log_levels()` - Verifies all log levels (ERROR, WARN, INFO, DEBUG)
- `test_health_endpoint_response()` - Verifies health endpoint response format
- `test_context_object_structure()` - Verifies context object structure

**Total**: 8 unit tests + 4 integration tests = 12 tests

---

### 4. Integration Tests

#### Router (Erlang/OTP)

**Required**:
- ✅ NATS integration tests: Subscribe to assignments, publish results
- ✅ Router ↔ Worker contract tests: End-to-end contract verification
- ✅ CP1 fields propagation tests: End-to-end propagation through flow

**Test Files**:
- `apps/otp/router/test/router_cp1_fields_integration_SUITE.erl` - CP1 fields integration tests
- `apps/otp/router/test/router_health_integration_SUITE.erl` - Health endpoint integration tests

#### Gateway (C-Gateway)

**Required**:
- ✅ HTTP endpoint tests: Request/response handling
- ✅ Router integration tests: Request forwarding to Router

**Test Files**:
- `apps/c-gateway/tests/test_health_endpoint.c` - Health endpoint integration tests

#### Worker (CAF/C++)

**Required**:
- ✅ NATS integration tests: Subscribe to assignments, publish results
- ✅ Router ↔ Worker contract tests: End-to-end contract verification

**Test Files**:
- `apps/caf/processor/tests/test_health_endpoint.cpp` - Health endpoint integration tests

---

### 5. E2E Test Scripts

**Required**:
- ✅ `test_router_observability.sh` - Router observability E2E tests
- ✅ `test_gateway_observability.sh` - Gateway observability E2E tests
- ✅ `test_worker_observability.sh` - Worker observability E2E tests
- ✅ `validate_observability_e2e.sh` - Cross-component E2E validation

**Location**: `scripts/observability/`

**Purpose**: Tests observability with real requests (gRPC for Router, HTTP for Gateway/Worker)

**Exit Codes**:
- `0` - All tests passed
- `1` - Tests failed
- `2` - Service not running (skip)

---

## CP1 Test Summary

### CP1 Core Tests (Mandatory)

| Component | Unit Tests | Integration Tests | E2E Scripts | Total |
|-----------|-----------|-------------------|--------------|-------|
| **Router** | 12 tests | 6 tests | 1 script | 18+ tests |
| **Gateway** | 11 tests | 10 tests | 1 script | 21+ tests |
| **Worker** | 8 tests | 4 tests | 1 script | 12+ tests |
| **General** | - | - | 2 scripts | 2 scripts |
| **Total** | **31 tests** | **20 tests** | **5 scripts** | **51+ tests** |

### Test Execution

**Unified CP1 Test Profile**:
```bash
# Run all CP1 core tests
bash scripts/observability/run_cp1_profile.sh

# Run CP1 core tests for specific component
bash scripts/observability/run_cp1_profile.sh --component router
bash scripts/observability/run_cp1_profile.sh --component gateway
bash scripts/observability/run_cp1_profile.sh --component worker

# Skip E2E tests (unit/integration only)
bash scripts/observability/run_cp1_profile.sh --skip-e2e
```

**Component-Specific Tests**:

**Router**:
```bash
cd apps/otp/router
rebar3 ct --suite test/router_observability_SUITE --group log_format_tests --group pii_filtering_tests --group health_endpoint_tests
rebar3 ct --suite test/router_health_integration_SUITE
bash scripts/observability/test_router_observability.sh
```

**Gateway**:
```bash
cd apps/c-gateway
make test-observability
make test-health
bash scripts/observability/test_gateway_observability.sh
```

**Worker**:
```bash
cd apps/caf/processor/build
cmake ..
make test_observability
make test_health_endpoint
./tests/test_observability
./tests/test_health_endpoint
bash scripts/observability/test_worker_observability.sh
```

---

## CP2+ Optional/Enhancement Tests

### Performance Tests (CP2+)

**CP2+**:
- 📋 Load tests: Throughput validation (≥500 tasks/s for CP3)
- 📋 Performance tests: Latency benchmarks
- 📋 Memory usage tests: Resource consumption estimation
- 📋 Concurrent logging tests: Concurrent logging performance

**Excluded from CP1**:
- ❌ Load tests (performance, throughput)
- ❌ Performance tests (latency benchmarks)
- ❌ Memory usage tests (resource consumption)
- ❌ Concurrent logging tests (performance)

### Stress Tests (CP2+)

**CP2+**:
- 📋 Stress tests: Resource exhaustion scenarios
- 📋 Edge case tests: Very large inputs, extreme timeouts
- 📋 Fault injection tests: Network failures, service unavailability

**Excluded from CP1**:
- ❌ Stress tests (resource exhaustion)
- ❌ Edge case tests (very large inputs, extreme timeouts)
- ❌ Fault injection tests (network failures)

### Advanced Tests (CP2+)

**CP2+**:
- 📋 Property-based tests: Generate valid values and verify contracts
- 📋 Fuzz testing: Random values to test robustness
- 📋 Advanced integration tests: Complex scenarios, multi-component flows

**Excluded from CP1**:
- ❌ Property-based tests
- ❌ Fuzz testing
- ❌ Advanced integration tests (beyond basic Router ↔ Worker)

---

## CP1 Acceptance Criteria

### Functional Requirements

- ✅ All contract tests pass (StepResult → ExecResult)
- ✅ All core functionality tests pass (assignment, blocks, status)
- ✅ All observability tests pass (logs, CP1 fields, health endpoint)
- ✅ All integration tests pass (NATS, Router ↔ Worker)
- ✅ All E2E test scripts pass (when services running)

### Non-Functional Requirements

- ✅ **Stability**: Tests are consistent and reliable
- ✅ **Predictability**: Test results are deterministic
- ✅ **Observability**: Tests verify observability requirements
- ✅ **Contract Compliance**: Tests verify contract compliance

### Test Coverage

- ✅ Contract tests (StepResult → ExecResult)
- ✅ Core functionality tests (assignment, blocks, status)
- ✅ Observability tests (logs, CP1 fields, health endpoint)
- ✅ Integration tests (NATS, Router ↔ Worker)
- ✅ E2E tests (real requests)

---

## CI/CD Integration

### GitHub Actions

```yaml
- name: Run CP1 Observability Tests
  run: bash scripts/observability/run_cp1_profile.sh
```

### GitLab CI

```yaml
cp1_observability_tests:
  script:
    - bash scripts/observability/run_cp1_profile.sh
```

### Drone CI

```yaml
- name: cp1-observability-tests
  image: alpine:latest
  commands:
    - apk add bash
    - bash scripts/observability/run_cp1_profile.sh
```

---

## References

### CP1 Documentation
- `docs/CP1_ACCEPTANCE_REPORT.md` - CP1 acceptance criteria and verification (see "Testing & Quality" section)
- `docs/OBSERVABILITY_CP1_TEST_PROFILE.md` - CP1 observability test profile
- `docs/CP1_CORE_PROFILE_CONTRACTS.md` - CP1 contracts profile
- `docs/CP1_CORE_PROFILE_OBSERVABILITY.md` - CP1 observability profile
- `docs/archive/dev/WORKER_ROUTER_CONTRACT_TESTS.md` - Contract tests documentation

### CP2 Planning Documents
- `apps/caf/processor/docs/CP1_WORKER_CORE_PROFILE.md` - CP1 Worker core profile (CP1 vs CP2+ classification)
- `docs/archive/dev/OBSERVABILITY_CP1_COMPLETION_REPORT.md` - CP1 observability completion report

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP1 core tests profile definition
- Contract tests specification
- Core functionality tests specification
- Observability tests specification
- Integration tests specification
- E2E test scripts specification
- CP1 vs CP2+ separation

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP1-LC  
**Status**: Core Profile Definition

