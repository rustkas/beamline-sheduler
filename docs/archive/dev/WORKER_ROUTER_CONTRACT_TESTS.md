# Worker ↔ Router Contract Integration Tests

**Date**: 2025-01-27  
**Status**: ✅ Created  
**Purpose**: Integration tests verifying StepResult → ExecResult contract between CAF Worker and Router

## Overview

These tests verify the contract between CAF Worker and Router as defined in:
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md#43-stepresult-contract-cp1-invariant`
- `docs/API_CONTRACTS.md` (ExecResult contract)

## Test Files

### 1. C++ Tests (Worker Side)

**File**: `apps/caf/processor/tests/test_worker_router_contract.cpp`

**Purpose**: Tests StepResult → ExecResult conversion on Worker side

**Test Cases**:
- `test_stepresult_to_execresult_success` - Success status conversion
- `test_stepresult_to_execresult_error` - Error status with error_code conversion
- `test_stepresult_to_execresult_timeout` - Timeout status conversion
- `test_stepresult_to_execresult_cancelled` - Cancelled status conversion
- `test_stepresult_metadata_preservation` - Correlation fields (trace_id, tenant_id) preservation
- `test_stepresult_error_code_mapping` - ErrorCode (1xxx-5xxx) → string mapping
- `test_stepresult_validation` - StepResult validation before conversion
- `test_stepresult_status_mapping` - StepStatus → ExecResult.status mapping

**Build**: Added to `apps/caf/processor/tests/CMakeLists.txt`

**Run**:
```bash
cd apps/caf/processor/build
cmake ..
make test_worker_router_contract
./tests/test_worker_router_contract
```

### 2. Erlang Tests (Router Side)

**File**: `apps/otp/router/test/router_worker_contract_SUITE.erl`

**Purpose**: Tests ExecResult processing on Router side

**Test Cases**:
- `test_execresult_success_with_full_metadata` - Success status with full correlation fields
- `test_execresult_error_with_error_code` - Error status with error_code field
- `test_execresult_timeout` - Timeout status processing
- `test_execresult_cancelled` - Cancelled status processing
- `test_execresult_metadata_preservation` - Correlation fields preservation in usage events
- `test_execresult_error_code_mapping` - Various error codes processing
- `test_execresult_missing_correlation_fields` - Handling of missing correlation fields

**Run**:
```bash
cd apps/otp/router
rebar3 ct --suite router_worker_contract_SUITE
```

## Contract Verification

### Status Mapping

| StepResult.status | ExecResult.status | Test Coverage |
|-------------------|-------------------|---------------|
| `StepStatus::ok` | `"success"` | ✅ C++ and Erlang |
| `StepStatus::error` | `"error"` | ✅ C++ and Erlang |
| `StepStatus::timeout` | `"timeout"` | ✅ C++ and Erlang |
| `StepStatus::cancelled` | `"cancelled"` | ✅ C++ and Erlang |

### Error Code Mapping

| ErrorCode | ExecResult.error_code | Test Coverage |
|-----------|----------------------|---------------|
| `ErrorCode::network_error` | `"NETWORK_ERROR"` | ✅ C++ and Erlang |
| `ErrorCode::connection_timeout` | `"CONNECTION_TIMEOUT"` | ✅ C++ and Erlang |
| `ErrorCode::execution_failed` | `"EXECUTION_FAILED"` | ✅ C++ and Erlang |
| `ErrorCode::invalid_input` | `"INVALID_INPUT"` | ✅ C++ and Erlang |
| `ErrorCode::internal_error` | `"INTERNAL_ERROR"` | ✅ C++ and Erlang |
| `ErrorCode::system_overload` | `"SYSTEM_OVERLOAD"` | ✅ C++ and Erlang |
| `ErrorCode::cancelled_by_user` | `"CANCELLED_BY_USER"` | ✅ C++ and Erlang |
| `ErrorCode::cancelled_by_timeout` | `"CANCELLED_BY_TIMEOUT"` | ✅ C++ and Erlang |

### Metadata Preservation

| StepResult.metadata | ExecResult Field | Test Coverage |
|---------------------|------------------|---------------|
| `trace_id` | `trace_id` | ✅ C++ and Erlang |
| `run_id` | `run_id` | ✅ C++ and Erlang (CP1 observability invariant) |
| `tenant_id` | `tenant_id` | ✅ C++ and Erlang |
| `flow_id` | (not in ExecResult) | ✅ Documented |
| `step_id` | (not in ExecResult) | ✅ Documented |

## Test Execution

### Run All Contract Tests

**C++ Tests**:
```bash
cd apps/caf/processor/build
cmake ..
make test_worker_router_contract
./tests/test_worker_router_contract
```

**Erlang Tests**:
```bash
cd apps/otp/router
rebar3 ct --suite router_worker_contract_SUITE
```

### Run Specific Test

**C++**:
```bash
# Test is a single executable, all tests run together
./tests/test_worker_router_contract
```

**Erlang**:
```bash
# Run specific test case
rebar3 ct --suite router_worker_contract_SUITE --case test_execresult_success_with_full_metadata
```

## Expected Results

### C++ Tests

All tests should pass with output:
```
=========================================
Worker ↔ Router Contract Integration Tests
=========================================

Testing StepResult status mapping...
✓ StepResult status mapping test passed
Testing StepResult → ExecResult: success status...
✓ StepResult → ExecResult: success status test passed
...
=========================================
✓ All Worker ↔ Router contract tests passed!
=========================================
```

### Erlang Tests

All tests should pass with Common Test output:
```
TEST INFO: Starting test suite router_worker_contract_SUITE
...
TEST INFO: Test suite router_worker_contract_SUITE completed successfully
```

## Integration with CI/CD

These tests should be included in CI/CD pipelines:

1. **C++ Tests**: Run as part of CAF Worker build/test pipeline
2. **Erlang Tests**: Run as part of Router test suite

**CI Integration**:
- Add `test_worker_router_contract` to CAF Worker test matrix
- Add `router_worker_contract_SUITE` to Router test matrix
- Both test suites should run on every PR

## Contract Documentation

**References**:
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md#43-stepresult-contract-cp1-invariant` - StepResult contract definition
- `docs/API_CONTRACTS.md` - ExecResult contract definition
- `apps/caf/processor/include/beamline/worker/result_converter.hpp` - ResultConverter implementation
- `apps/caf/processor/include/beamline/worker/core.hpp` - StepResult type definition

## Future Enhancements

1. **E2E Integration Tests**: Full flow from Router → Worker → Router via NATS
2. **Performance Tests**: Measure conversion overhead
3. **Fuzz Testing**: Random StepResult values to test robustness
4. **Property-Based Tests**: Generate valid StepResult values and verify conversion

---

**Last Updated**: 2025-01-27  
**Status**: ✅ Tests Created

