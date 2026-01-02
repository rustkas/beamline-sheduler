# CP1 Observability Test Profile

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Purpose**: Define unified CP1 observability test profile for Router/Gateway/Worker components

## Overview

This document defines the **CP1 Observability Test Profile** - a unified set of tests and scripts that validate CP1 observability compliance across all key components (Router, Gateway, Worker). This profile can be invoked as a single "package" from scripts and CI/CD pipelines.

## Profile Structure

The CP1 test profile consists of:

1. **CP1 Core Tests** - Mandatory tests for CP1 compliance
2. **CP2+ Tests** - Optional tests that can be disabled for CP1 validation
3. **E2E Test Scripts** - End-to-end validation scripts
4. **Validation Scripts** - General observability validation

## CP1 Core Tests

### Router (Erlang/OTP)

**Component**: `apps/otp/router/`

#### Unit Tests (CP1 Core)

**Test Suite**: `router_observability_SUITE.erl`

**Location**: `apps/otp/router/test/router_observability_SUITE.erl`

**CP1 Core Test Groups**:
- `log_format_tests` - JSON log format validation
  - `test_log_format_json` - Validates JSON log format
  - `test_log_required_fields` - Validates required fields (timestamp, level, component, message)
  - `test_log_optional_fields` - Validates optional fields (context, CP1 fields)
  - `test_correlation_fields` - Validates CP1 correlation fields at top level
  - `test_error_code_and_latency` - Validates error_code and latency_ms fields
  - `test_log_levels` - Validates all log levels (ERROR, WARN, INFO, DEBUG)
- `pii_filtering_tests` - PII/secret filtering validation
  - `test_pii_filtering` - Validates PII filtering
  - `test_secret_pattern_detection` - Validates secret pattern detection
- `health_endpoint_tests` - Health endpoint validation
  - `test_health_endpoint` - Validates gRPC health endpoint configuration
- `logging_scenarios_tests` - Error logging scenarios
  - `test_nats_error_logging` - Validates NATS error logging
  - `test_routing_error_logging` - Validates routing error logging
  - `test_invalid_payload_logging` - Validates invalid payload logging
  - `test_internal_error_logging` - Validates internal error logging

**Run Command**:
```bash
cd apps/otp/router
rebar3 ct --suite test/router_observability_SUITE --group log_format_tests --group pii_filtering_tests --group health_endpoint_tests --group logging_scenarios_tests
```

#### Integration Tests (CP1 Core)

**Test Suite**: `router_health_integration_SUITE.erl`

**Location**: `apps/otp/router/test/router_health_integration_SUITE.erl`

**CP1 Core Tests**:
- `test_health_check_available` - Verifies gRPC health service is available
- `test_health_check_response_format` - Validates health response format
- `test_health_check_serving_status` - Validates SERVING status
- `test_health_check_empty_service` - Tests empty service name
- `test_health_check_specific_service` - Tests specific service name
- `test_health_check_error_handling` - Tests error handling

**Run Command**:
```bash
cd apps/otp/router
rebar3 ct --suite test/router_health_integration_SUITE
```

#### E2E Test Script (CP1 Core)

**Script**: `scripts/observability/test_router_observability.sh`

**Purpose**: Tests Router observability with real gRPC health check requests

**Tests**:
- gRPC health check via `grpc_health_probe` (preferred)
- gRPC health check via `grpcurl` (alternative)
- Health status values validation

**Run Command**:
```bash
bash scripts/observability/test_router_observability.sh
```

**Exit Codes**:
- `0` - All tests passed
- `1` - Tests failed
- `2` - Service not running (skip)

---

### Gateway (C-Gateway)

**Component**: `apps/c-gateway/`

#### Unit Tests (CP1 Core)

**Test Suite**: `test_observability.c`

**Location**: `apps/c-gateway/tests/test_observability.c`

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

**Run Command**:
```bash
cd apps/c-gateway
make test-observability
# Or using CMake:
cd build
make c-gateway-observability-test
./c-gateway-observability-test
```

#### Integration Tests (CP1 Core)

**Test Suite**: `test_health_endpoint.c`

**Location**: `apps/c-gateway/tests/test_health_endpoint.c`

**CP1 Core Tests**:
- `test_health_endpoint_returns_200` - Validates HTTP 200 OK response
- `test_health_endpoint_json_format` - Validates JSON response format
- `test_health_endpoint_status_field` - Validates `status` field (must be "healthy")
- `test_health_endpoint_timestamp_field` - Validates `timestamp` field (ISO 8601)
- `test_health_endpoint_required_fields` - Validates required fields presence
- `test_health_endpoint_content_type` - Validates Content-Type header
- `test_health_endpoint_error_handling` - Tests error handling
- `test_health_endpoint_concurrent_requests` - Tests concurrent requests
- `test_health_endpoint_timeout_handling` - Tests timeout handling
- `test_health_endpoint_invalid_methods` - Tests invalid HTTP methods

**Run Command**:
```bash
cd apps/c-gateway
make test-health
# Or using CMake:
cd build
make c-gateway-health-test
./c-gateway-health-test
```

#### E2E Test Script (CP1 Core)

**Script**: `scripts/observability/test_gateway_observability.sh`

**Purpose**: Tests Gateway observability with real HTTP requests

**Tests**:
- Health endpoint (`GET /health` or `GET /_health`) - CP1 format validation
- Health endpoint alternative (`GET /_health`) - CP1 format validation
- Metrics endpoint (`GET /metrics`) - Optional
- Metrics JSON endpoint (`GET /_metrics`) - Optional
- POST `/api/v1/routes/decide` - Happy path with observability
- POST `/api/v1/messages` - Happy path with observability
- Validation error (400 Bad Request)
- Missing tenant ID header (400 Bad Request)

**Run Command**:
```bash
bash scripts/observability/test_gateway_observability.sh
```

**Exit Codes**:
- `0` - All tests passed
- `1` - Tests failed
- `2` - Service not running (skip)

---

### Worker (CAF/C++)

**Component**: `apps/caf/processor/`

#### Unit Tests (CP1 Core)

**Test Suite**: `test_observability.cpp`

**Location**: `apps/caf/processor/tests/test_observability.cpp`

**CP1 Core Tests**:
- `test_timestamp_format()` - Verifies ISO 8601 timestamp format with microseconds
- `test_log_format_compliance()` - Verifies CP1-compliant log format
- `test_cp1_fields_at_top_level()` - Verifies CP1 fields at top level
- `test_cp1_fields_with_context()` - Verifies CP1 fields extraction from BlockContext
- `test_pii_filtering()` - Verifies PII filtering functionality
- `test_all_log_levels()` - Verifies all log levels (ERROR, WARN, INFO, DEBUG)
- `test_health_endpoint_response()` - Verifies health endpoint response format
- `test_context_object_structure()` - Verifies context object structure

**Run Command**:
```bash
cd apps/caf/processor
mkdir -p build
cd build
cmake ..
make test_observability
./test_observability
```

#### Integration Tests (CP1 Core)

**Test Suite**: `test_health_endpoint.cpp`

**Location**: `apps/caf/processor/tests/test_health_endpoint.cpp`

**CP1 Core Tests**:
- `test_health_endpoint_starts()` - Verifies health endpoint can start
- `test_health_endpoint_cp1_format()` - Verifies CP1-compliant response format
- `test_health_endpoint_stops()` - Verifies health endpoint can stop gracefully
- `test_health_endpoint_returns_200()` - Verifies HTTP 200 OK response

**Run Command**:
```bash
cd apps/caf/processor
mkdir -p build
cd build
cmake ..
make test_health_endpoint
./test_health_endpoint
```

#### E2E Test Script (CP1 Core)

**Script**: `scripts/observability/test_worker_observability.sh`

**Purpose**: Tests Worker observability with real HTTP requests

**Tests**:
- Health endpoint (`GET /_health`) - CP1 format validation
- HTTP status code validation (200 OK)
- Response Content-Type validation (application/json)

**Run Command**:
```bash
bash scripts/observability/test_worker_observability.sh
```

**Exit Codes**:
- `0` - All tests passed
- `1` - Tests failed
- `2` - Service not running (skip)

---

## CP2+ Tests (Optional, Can Be Disabled)

### Router (Erlang/OTP)

#### Performance Tests (CP2+)

**Test Suite**: `router_observability_performance_SUITE.erl`

**Location**: `apps/otp/router/test/router_observability_performance_SUITE.erl`

**CP2+ Tests** (can be disabled for CP1):
- `test_log_generation_throughput` - Logging throughput (logs/second)
- `test_pii_filtering_performance` - PII filtering latency
- `test_json_serialization_performance` - JSON serialization performance
- `test_memory_usage_during_logging` - Memory usage estimation
- `test_concurrent_logging_performance` - Concurrent logging performance

**Run Command** (optional):
```bash
cd apps/otp/router
rebar3 ct --suite test/router_observability_performance_SUITE
```

#### Edge Case Tests (CP2+)

**Test Suite**: `router_observability_SUITE.erl` (edge_case_tests group)

**CP2+ Tests** (can be disabled for CP1):
- `test_very_long_message` - Very long log messages (100KB)
- `test_very_long_cp1_fields` - Very long CP1 field values (1000 chars)
- `test_empty_null_cp1_fields` - Empty/null CP1 fields
- `test_special_characters` - Special characters (JSON escaping)
- `test_very_large_context` - Very large context objects (1000 fields)
- `test_invalid_json_in_context` - Invalid JSON in context
- `test_concurrent_logging` - Concurrent logging scenarios

**Run Command** (optional):
```bash
cd apps/c-gateway
rebar3 ct --suite test/router_observability_SUITE --group edge_case_tests
```

---

### Gateway (C-Gateway)

#### Performance Tests (CP2+)

**Test Suite**: Performance tests (if available)

**CP2+ Tests** (can be disabled for CP1):
- Performance benchmarking tests
- Load testing scenarios

**Run Command** (optional):
```bash
cd apps/c-gateway
make test-performance
```

---

### Worker (CAF/C++)

#### Performance Tests (CP2+)

**Test Suite**: `test_observability_performance.cpp`

**Location**: `apps/caf/processor/tests/test_observability_performance.cpp`

**CP2+ Tests** (can be disabled for CP1):
- `test_logging_throughput()` - Logging throughput (logs/second)
- `test_pii_filtering_latency()` - PII filtering latency (microseconds per log entry)
- `test_json_serialization_performance()` - JSON serialization performance
- `test_memory_usage_estimation()` - Memory usage estimation
- `test_concurrent_logging_performance()` - Concurrent logging performance

**Run Command** (optional):
```bash
cd apps/caf/processor
mkdir -p build
cd build
cmake ..
make test_observability_performance
./test_observability_performance
```

#### Edge Case Tests (CP2+)

**Test Suite**: `test_observability.cpp` (edge case tests)

**CP2+ Tests** (can be disabled for CP1):
- `test_very_long_message()` - Very long log messages (100KB)
- `test_very_long_cp1_fields()` - Very long CP1 field values (1000 characters)
- `test_empty_cp1_fields()` - Empty/null CP1 fields
- `test_special_characters()` - Special characters in log messages (JSON escaping)
- `test_very_large_context()` - Very large context objects (1000 fields)
- `test_invalid_json_in_context()` - Invalid JSON in context

**Run Command** (optional):
```bash
cd apps/caf/processor
mkdir -p build
cd build
cmake ..
make test_observability
./test_observability --gtest_filter="*EdgeCase*"
```

---

## General Validation Scripts

### Observability Validation (CP1 Core)

**Script**: `scripts/observability/validate_observability.sh`

**Purpose**: Validates JSON log format and `/_health` endpoint availability

**Checks**:
1. Logging configuration validation (`config/observability/logging.json`)
2. JSON log format validation
3. CP1 cross-cutting invariants validation
4. Health endpoint availability and format (optional - services may not be running)
5. Secret detection in logs

**Run Command**:
```bash
bash scripts/observability/validate_observability.sh
```

**Exit Codes**:
- `0` - Success (may have warnings)
- `2` - External endpoints unavailable (services not running - expected in local validation)
- `3` - Local configs missing or invalid

**PowerShell Alternative**: `scripts/observability/validate_observability.ps1`

### E2E Observability Validation (CP1 Core)

**Script**: `scripts/observability/validate_observability_e2e.sh`

**Purpose**: End-to-end observability validation across all components

**Checks**:
- Health endpoint availability (all components)
- Log format compliance (sample logs)
- Correlation field presence (when context available)
- PII filtering (no secrets in logs)

**Run Command**:
```bash
bash scripts/observability/validate_observability_e2e.sh
```

---

## Unified CP1 Test Profile

### Wrapper Script

**Script**: `scripts/observability/run_cp1_profile.sh`

**Purpose**: Run all CP1 core tests as a single "package"

**Usage**:
```bash
# Run all CP1 core tests
bash scripts/observability/run_cp1_profile.sh

# Run CP1 core tests for specific component
bash scripts/observability/run_cp1_profile.sh --component router
bash scripts/observability/run_cp1_profile.sh --component gateway
bash scripts/observability/run_cp1_profile.sh --component worker

# Skip E2E tests (unit/integration only)
bash scripts/observability/run_cp1_profile.sh --skip-e2e

# Include CP2+ tests (optional)
bash scripts/observability/run_cp1_profile.sh --include-cp2
```

**Exit Codes**:
- `0` - All CP1 core tests passed
- `1` - One or more CP1 core tests failed
- `2` - Services not running (E2E tests skipped)

### Test Execution Order

1. **General Validation** (`validate_observability.sh`)
   - Validates configuration and schema
   - Checks for secrets in configs

2. **Router CP1 Core Tests**
   - Unit tests (`router_observability_SUITE.erl` - CP1 groups)
   - Integration tests (`router_health_integration_SUITE.erl`)
   - E2E test script (`test_router_observability.sh`)

3. **Gateway CP1 Core Tests**
   - Unit tests (`test_observability.c`)
   - Integration tests (`test_health_endpoint.c`)
   - E2E test script (`test_gateway_observability.sh`)

4. **Worker CP1 Core Tests**
   - Unit tests (`test_observability.cpp` - CP1 core)
   - Integration tests (`test_health_endpoint.cpp`)
   - E2E test script (`test_worker_observability.sh`)

5. **E2E Validation** (`validate_observability_e2e.sh`)
   - Cross-component validation

### CI/CD Integration

**GitHub Actions**:
```yaml
- name: Run CP1 Observability Tests
  run: bash scripts/observability/run_cp1_profile.sh
```

**GitLab CI**:
```yaml
cp1_observability_tests:
  script:
    - bash scripts/observability/run_cp1_profile.sh
```

**Drone CI**:
```yaml
- name: cp1-observability-tests
  image: alpine:latest
  commands:
    - apk add bash
    - bash scripts/observability/run_cp1_profile.sh
```

---

## Test Summary

### CP1 Core Tests (Mandatory)

| Component | Unit Tests | Integration Tests | E2E Scripts | Total |
|-----------|-----------|-------------------|--------------|-------|
| **Router** | 12 tests | 6 tests | 1 script | 18+ tests |
| **Gateway** | 11 tests | 10 tests | 1 script | 21+ tests |
| **Worker** | 8 tests | 4 tests | 1 script | 12+ tests |
| **General** | - | - | 2 scripts | 2 scripts |
| **Total** | **31 tests** | **20 tests** | **5 scripts** | **51+ tests** |

### CP2+ Tests (Optional)

| Component | Performance Tests | Edge Case Tests | Total |
|-----------|-------------------|-----------------|-------|
| **Router** | 5 tests | 7 tests | 12 tests |
| **Gateway** | TBD | - | TBD |
| **Worker** | 5 tests | 6 tests | 11 tests |
| **Total** | **10+ tests** | **13 tests** | **23+ tests** |

---

## CP2 Extensions

**⚠️ IMPORTANT**: The following CP2 observability tests are **NOT part of the CP1 test profile**. They are documented here for reference and future implementation.

**CP1 Profile Scope**: This document (`OBSERVABILITY_CP1_TEST_PROFILE.md`) and the script `scripts/observability/run_cp1_profile.sh` define **CP1 Baseline tests**. CP2 Advanced Observability extensions are outside the scope of CP1 validation.

### CP2 Observability Test Profile

**Reference Document**: `docs/OBSERVABILITY_CP2_TEST_PROFILE.md` (draft)

**CP2 Features** (not in CP1):
- **CP2.1**: Prometheus Metrics Export
  - `/metrics` endpoint validation
  - Metrics format validation (Prometheus text format)
  - Metrics collection tests (counters, histograms, gauges)
  - CP1 correlation fields as labels validation
- **CP2.2**: OpenTelemetry Distributed Tracing
  - OTLP trace export validation
  - Trace context propagation tests
  - Span creation and correlation tests
- **CP2.3**: Grafana Dashboards
  - Dashboard configuration validation
  - Query validation tests
- **CP2.4**: Alerting Rules
  - Alertmanager rule validation
  - Alert condition tests

**CP2 Test Profile Script**: `scripts/observability/run_cp2_profile.sh` (planned)

**CP2 Documentation**:
- `docs/archive/dev/OBSERVABILITY_CP2_EXTENSION_PLAN.md` - CP2 extension plan
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - Prometheus metrics specification
- `docs/archive/dev/CP2_OBSERVABILITY_PLAN.md` - CP2 observability plan
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - CP2 observability backlog

**Note**: CP2 tests will be implemented in a separate test profile that extends CP1 tests. CP1 profile remains unchanged and focused on CP1 core requirements only.

---

## References

- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/OBSERVABILITY.md` - General observability requirements
- `config/observability/logging.json` - Log format schema
- `scripts/observability/run_cp1_profile.sh` - Unified CP1 test profile script
- `docs/OBSERVABILITY_CP2_TEST_PROFILE.md` - CP2 observability test profile (draft)

---

## Change History

**v1.1 (2025-01-27)**:
- Added CP2 Extensions section with references to CP2 test profile
- Explicitly documented that CP2 tests are NOT part of CP1 profile
- Added links to CP2 planning documents

**v1.0 (2025-01-27)**:
- Initial CP1 observability test profile definition
- CP1 core tests identification
- CP2+ tests identification
- Unified wrapper script specification

