# CP1 Worker + Observability Readiness Checklist

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Purpose**: Quick readiness checklist for CP1 Worker and Observability validation (PR/review)  
**Status**: ✅ **ACTIVE**  
**Assignment**: Joint for `wrk-3` (Worker) and `wrk-obs1` (Observability)

---

## Overview

This checklist consolidates **critical CP1 requirements** for Worker and Observability from:
- `docs/CP1_CORE_PROFILE_CONTRACTS.md` - Contract requirements
- `docs/CP1_CORE_PROFILE_TESTS.md` - Test requirements
- `docs/CP1_CORE_PROFILE_OBSERVABILITY.md` - Observability requirements
- `apps/caf/processor/docs/CP1_WORKER_CORE_PROFILE.md` - Worker-specific requirements
- `docs/archive/dev/OBSERVABILITY_CP1_CORE_PROFILE.md` - Observability-specific requirements

**Usage**: Use this checklist during PR review to quickly verify CP1 readiness for Worker and Observability.

---

## Quick Checklist

| Category | Requirement | Status | Notes |
|----------|-------------|-------|-------|
| **CONTRACTS** | | | |
| Contracts-1 | `StepResult` type with required fields (`status`, `error_code`, `metadata`) | ⬜ | `apps/caf/processor/include/beamline/worker/core.hpp` |
| Contracts-2 | Status mapping: `StepStatus::ok` → `ExecResult.status = "success"` | ⬜ | All statuses: ok→success, error→error, timeout→timeout, cancelled→cancelled |
| Contracts-3 | Error code mapping: `ErrorCode` (1xxx-5xxx) → `ExecResult.error_code` (string) | ⬜ | `ResultConverter::to_exec_result_json()` |
| Contracts-4 | Metadata preservation: `ResultMetadata` → `ExecResult` correlation fields | ⬜ | trace_id, run_id, tenant_id preserved |
| Contracts-5 | `ExecAssignment` validation before accepting | ⬜ | Invalid → `ExecAssignmentAck` with `status: "rejected"` |
| Contracts-6 | `ExecAssignmentAck` published to `caf.exec.assign.v1.ack` | ⬜ | Status: accepted/rejected/error |
| Contracts-7 | `ExecResult` published to `caf.exec.result.v1` | ⬜ | Required fields: version, assignment_id, request_id, status, provider_id, job, timestamp |
| **TESTS** | | | |
| Tests-1 | `test_worker_router_contract.cpp` exists and passes | ⬜ | Status mapping, error code mapping, metadata preservation |
| Tests-2 | `router_worker_contract_SUITE.erl` exists and passes | ⬜ | Router-side ExecResult processing |
| Tests-3 | Assignment validation tests pass | ⬜ | Invalid `ExecAssignment` rejection |
| Tests-4 | Block execution tests pass (HTTP, FS, SQL - basic happy path) | ⬜ | `test_block_executor.cpp` |
| Tests-5 | Status reporting tests pass (all status codes) | ⬜ | success, error, timeout, cancelled |
| Tests-6 | Observability unit tests pass (`test_observability.cpp`) | ⬜ | Log format, CP1 fields, PII filtering |
| Tests-7 | Health endpoint tests pass (`test_health_endpoint.cpp`) | ⬜ | HTTP endpoint availability and format |
| Tests-8 | NATS integration tests pass | ⬜ | Subscribe to assignments, publish results |
| **OBSERVABILITY** | | | |
| Observability-1 | Structured JSON logs with required fields | ⬜ | timestamp, level, component, message |
| Observability-2 | CP1 correlation fields at top level (when context available) | ⬜ | tenant_id, run_id, flow_id, step_id, trace_id |
| Observability-3 | PII/secret filtering working | ⬜ | password, api_key, secret, token, etc. → `[REDACTED]` |
| Observability-4 | Health endpoint: `GET /_health` returns `200 OK` | ⬜ | Port 9091, JSON format: `{"status": "healthy", "timestamp": "..."}` |
| Observability-5 | Log format validation script passes | ⬜ | `scripts/observability/validate_observability.sh` |
| Observability-6 | ISO 8601 timestamp format with microseconds | ⬜ | Format: `2025-01-27T12:00:00.123456Z` |
| Observability-7 | All log levels supported (ERROR, WARN, INFO, DEBUG) | ⬜ | All levels produce valid JSON logs |
| Observability-8 | No secrets detected in logs | ⬜ | Validation script checks for secrets |

---

## Detailed Requirements

### 1. Contracts (StepResult/ExecResult)

#### StepResult Contract (CP1 Invariant)

**Required**:
- ✅ `StepResult` type defined in `apps/caf/processor/include/beamline/worker/core.hpp`
- ✅ Required fields: `status` (StepStatus), `error_code` (ErrorCode), `metadata` (ResultMetadata)
- ✅ Status mapping: `StepStatus::ok` → `ExecResult.status = "success"`
- ✅ Error code mapping: `ErrorCode` (1xxx-5xxx) → `ExecResult.error_code` (string format)
- ✅ Metadata preservation: `ResultMetadata` → `ExecResult` correlation fields (trace_id, run_id, tenant_id)
- ✅ Conversion: `ResultConverter::to_exec_result_json()` for NATS publishing

**Validation**:
- Contract tests: `test_worker_router_contract.cpp` must pass
- Router-side tests: `router_worker_contract_SUITE.erl` must pass

**Reference**: `docs/CP1_CORE_PROFILE_CONTRACTS.md#3-stepresult-contract-cp1-invariant`

#### ExecAssignment/ExecResult Contracts

**Required**:
- ✅ `ExecAssignment` validation before accepting
- ✅ Invalid `ExecAssignment` → `ExecAssignmentAck` with `status: "rejected"`
- ✅ `ExecAssignmentAck` published to `caf.exec.assign.v1.ack`
- ✅ `ExecResult` published to `caf.exec.result.v1` with required fields

**Required Fields (ExecResult)**:
- `version`: `"1"` (string)
- `assignment_id`: UUID string
- `request_id`: UUID string
- `status`: `"success"` | `"error"` | `"timeout"` | `"cancelled"`
- `provider_id`: String
- `job`: Object with `type`
- `timestamp`: Integer (milliseconds)

**Reference**: `docs/CP1_CORE_PROFILE_CONTRACTS.md#2-router-worker-contracts`

---

### 2. Tests (Minimal)

#### Contract Tests

**Required**:
- ✅ `test_worker_router_contract.cpp` - StepResult → ExecResult conversion tests
  - Status mapping tests (success, error, timeout, cancelled)
  - Error code mapping tests (1xxx-5xxx)
  - Metadata preservation tests (correlation IDs)
- ✅ `router_worker_contract_SUITE.erl` - Router-side ExecResult processing tests
  - ExecResult validation tests
  - Correlation fields preservation tests

**Location**: 
- `apps/caf/processor/tests/test_worker_router_contract.cpp`
- `apps/otp/router/test/router_worker_contract_SUITE.erl`

**Reference**: `docs/CP1_CORE_PROFILE_TESTS.md#1-contract-tests`

#### Core Functionality Tests

**Required**:
- ✅ Assignment validation tests: Invalid `ExecAssignment` rejection
- ✅ Block execution tests: HTTP, FS, SQL blocks (basic happy path)
- ✅ Status reporting tests: All status codes (success, error, timeout, cancelled)
- ✅ Error handling tests: Graceful error handling (no crashes)

**Test Files**:
- `apps/caf/processor/tests/test_core.cpp` - Core data structures and contract tests
- `apps/caf/processor/tests/test_block_executor.cpp` - Block execution tests (basic)

**Reference**: `docs/CP1_CORE_PROFILE_TESTS.md#2-core-functionality-tests`

#### Observability Tests

**Required**:
- ✅ Log format tests: Structured JSON format validation
- ✅ CP1 fields tests: Correlation fields extraction and logging
- ✅ PII filtering tests: Secret filtering validation
- ✅ Health endpoint tests: HTTP endpoint availability and format

**Test Files**:
- `apps/caf/processor/tests/test_observability.cpp` - Observability unit tests (8 tests)
- `apps/caf/processor/tests/test_health_endpoint.cpp` - Health endpoint integration tests (4 tests)

**Total**: 8 unit tests + 4 integration tests = 12 tests

**Reference**: `docs/CP1_CORE_PROFILE_TESTS.md#3-observability-tests`

#### Integration Tests

**Required**:
- ✅ NATS integration tests: Subscribe to assignments, publish results
- ✅ Router ↔ Worker contract tests: End-to-end contract verification

**Reference**: `docs/CP1_CORE_PROFILE_TESTS.md#4-integration-tests`

---

### 3. Observability (Logs/Health/PII)

#### Structured JSON Logging

**Required Fields** (always present):
- `timestamp`: ISO 8601, UTC, microseconds precision (e.g., `2025-01-27T12:00:00.123456Z`)
- `level`: `ERROR` | `WARN` | `INFO` | `DEBUG`
- `component`: `"worker"`
- `message`: Human-readable text

**Schema**: `config/observability/logging.json`

**Validation**: `scripts/observability/validate_observability.sh`

**Reference**: `docs/CP1_CORE_PROFILE_OBSERVABILITY.md#1-structured-json-logging`

#### CP1 Correlation Fields

**Required Fields** (when context available):
- `tenant_id`: Tenant context available
- `run_id`: Run context available (**CP1 observability invariant**)
- `flow_id`: Flow context available
- `step_id`: Step context available
- `trace_id`: Trace context available

**CP1 Format**: CP1 fields must be at **top level** in JSON logs (not in `correlation` object).

**Example**:
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
    "status": "success"
  }
}
```

**Reference**: `docs/CP1_CORE_PROFILE_OBSERVABILITY.md#2-cp1-correlation-fields`

#### Health Endpoint

**Required**:
- ✅ HTTP endpoint: `GET /_health`
- ✅ Port: `9091`
- ✅ Status code: `200 OK`
- ✅ Response format: `{"status": "healthy", "timestamp": "ISO-8601"}`

**Verification**:
```bash
curl http://localhost:9091/_health
# Expected: {"status": "healthy", "timestamp": "2025-01-27T12:00:00.123456Z"}
```

**Reference**: `docs/CP1_CORE_PROFILE_OBSERVABILITY.md#3-health-endpoints`

#### PII/Secret Filtering

**Required**:
- ✅ Automatic filtering of sensitive data before logging
- ✅ Filtered fields: `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`, `authorization`, `credit_card`, `ssn`, `email`, `phone`
- ✅ Replacement: `[REDACTED]`

**Validation**: `scripts/observability/validate_observability.sh` checks for secrets in logs.

**Reference**: `docs/CP1_CORE_PROFILE_OBSERVABILITY.md#4-piisecret-filtering`

---

## Validation Commands

### Local Validation

**Run all CP1 tests**:
```bash
cd apps/caf/processor/build
cmake ..
make test_worker_router_contract
make test_core
make test_block_executor
make test_observability
make test_health_endpoint
./tests/test_worker_router_contract
./tests/test_core
./tests/test_block_executor
./tests/test_observability
./tests/test_health_endpoint
```

**Run observability validation**:
```bash
bash scripts/observability/validate_observability.sh
# Exit codes: 0 (success), 2 (external unavailable), 3 (local errors)
```

**Run E2E observability tests**:
```bash
bash scripts/observability/test_worker_observability.sh
# Exit codes: 0 (success), 1 (failed), 2 (service not running)
```

### Router-Side Contract Tests

**Run Router contract tests**:
```bash
cd apps/otp/router
rebar3 ct --suite test/router_worker_contract_SUITE
```

---

## CP1 vs CP2+ Separation

### CP1 Required (This Checklist)

- ✅ StepResult/ExecResult contracts
- ✅ Basic contract tests
- ✅ Core functionality tests (happy path)
- ✅ Observability tests (log format, CP1 fields, health endpoint)
- ✅ Structured JSON logging
- ✅ CP1 correlation fields
- ✅ Health endpoint (basic)
- ✅ PII/secret filtering

### CP2+ Excluded (Not Required for CP1)

- ❌ Prometheus metrics endpoint (`/metrics`)
- ❌ OpenTelemetry tracing integration
- ❌ Performance tests (latency benchmarks)
- ❌ Load tests (throughput validation)
- ❌ Stress tests (resource exhaustion)
- ❌ Edge case tests (very large inputs, extreme timeouts)
- ❌ Fault injection tests (network failures)
- ❌ Grafana dashboards
- ❌ Advanced health checks (database, external services)

**Reference**: `apps/caf/processor/docs/CP1_WORKER_CORE_PROFILE.md#cp2-optionalenhancement-features`

---

## Acceptance Criteria

### Functional Requirements

- ✅ Worker subscribes to `caf.exec.assign.v1` and receives assignments
- ✅ Worker validates `ExecAssignment` and publishes ACK
- ✅ Worker executes basic blocks (HTTP, FS, SQL) with correct status
- ✅ Worker publishes `ExecResult` with accurate status codes
- ✅ Worker handles errors gracefully (no crashes)
- ✅ Worker health endpoint returns `200 OK`
- ✅ Worker produces structured JSON logs with CP1 correlation fields
- ✅ Worker filters PII/secrets before logging

### Non-Functional Requirements

- ✅ **Stability**: Worker runs without crashes for extended periods
- ✅ **Predictability**: Status codes are consistent and reliable
- ✅ **Observability**: Structured JSON logs with CP1 correlation fields
- ✅ **Contract Compliance**: Follows NATS contracts exactly
- ✅ **Security**: PII/secrets are filtered before logging

### Test Coverage

- ✅ Contract tests (StepResult → ExecResult)
- ✅ Core functionality tests (assignment, blocks, status)
- ✅ Observability tests (logs, CP1 fields, health endpoint)
- ✅ Integration tests (NATS, Router ↔ Worker)

---

## Quick Reference

### Test Files

| Test File | Purpose | Location |
|-----------|---------|----------|
| `test_worker_router_contract.cpp` | StepResult → ExecResult conversion | `apps/caf/processor/tests/` |
| `router_worker_contract_SUITE.erl` | Router-side ExecResult processing | `apps/otp/router/test/` |
| `test_core.cpp` | Core data structures | `apps/caf/processor/tests/` |
| `test_block_executor.cpp` | Block execution (basic) | `apps/caf/processor/tests/` |
| `test_observability.cpp` | Log format, CP1 fields, PII filtering | `apps/caf/processor/tests/` |
| `test_health_endpoint.cpp` | Health endpoint | `apps/caf/processor/tests/` |

### Validation Scripts

| Script | Purpose | Exit Codes |
|--------|---------|------------|
| `scripts/observability/validate_observability.sh` | Log format, CP1 fields, health endpoint | 0 (success), 2 (external unavailable), 3 (local errors) |
| `scripts/observability/test_worker_observability.sh` | E2E observability tests | 0 (success), 1 (failed), 2 (service not running) |

### Key Files

| File | Purpose |
|------|---------|
| `apps/caf/processor/include/beamline/worker/core.hpp` | StepResult type definition |
| `apps/caf/processor/include/beamline/worker/result_converter.hpp` | ResultConverter implementation |
| `config/observability/logging.json` | Log format schema |
| `docs/CP1_CORE_PROFILE_CONTRACTS.md` | Contract requirements |
| `docs/CP1_CORE_PROFILE_TESTS.md` | Test requirements |
| `docs/CP1_CORE_PROFILE_OBSERVABILITY.md` | Observability requirements |

---

## References

### CP1 Documentation
- `docs/CP1_CORE_PROFILE_CONTRACTS.md` - CP1 contracts profile
- `docs/CP1_CORE_PROFILE_TESTS.md` - CP1 tests profile
- `docs/CP1_CORE_PROFILE_OBSERVABILITY.md` - CP1 observability profile
- `apps/caf/processor/docs/CP1_WORKER_CORE_PROFILE.md` - Worker CP1 core profile
- `docs/archive/dev/OBSERVABILITY_CP1_CORE_PROFILE.md` - Observability CP1 core profile
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- `docs/archive/dev/WORKER_ROUTER_CONTRACT_TESTS.md` - Contract tests documentation

### Component Documentation
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md` - Worker architectural role
- `apps/caf/processor/docs/OBSERVABILITY.md` - Worker observability
- `docs/API_CONTRACTS.md` - API contracts specification

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP1 Worker + Observability readiness checklist
- Consolidated critical requirements from all CP1 core profiles
- Quick checklist table for PR/review usage
- Detailed requirements with references
- Validation commands and quick reference

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP1-LC  
**Status**: Readiness Checklist

