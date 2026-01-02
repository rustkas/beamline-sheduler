# CP1 Core Profile: Contracts

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Purpose**: Define CP1 core contracts between Router, Gateway, and Worker components  
**Status**: ✅ **ACTIVE**

---

## Overview

This document defines the **minimal mandatory contracts** for CP1. It explicitly separates CP1 required contracts from CP2+ enhancements to simplify CP1 validation and prevent CP2+ elements from becoming de facto mandatory.

**Key Principle**: CP1 contracts are **minimal and focused** on core message formats, validation rules, and status mappings. Advanced features (idempotency, advanced retry, complex metadata) are explicitly **CP2+**.

---

## CP1 Core Contracts (Mandatory)

### 1. Router ↔ Gateway Contracts

#### DecideRequest (Gateway → Router)

**Source of Truth**: `docs/API_CONTRACTS.md`

**Required Fields**:
- `version`: Must be `"1"` (string)
- `tenant_id`: String (required)
- `request_id`: UUID string (required)
- `task`: Object with `type` and `payload` (required)
  - `type`: `"chat"` | `"completion"` | `"embedding"`
  - `payload`: Object (task-specific)

**Optional Fields**:
- `trace_id`: String (recommended for distributed tracing)
- `policy_id`: String
- `constraints`: Object
- `metadata`: Object
- `push_assignment`: Boolean (triggers ExecAssignment publication)
- `assignment_subject`: String (default: `caf.exec.assign.v1`)
- `context`: Object with `session_id?`, `user_id?`

**Payload Schemas**:
- `chat`: `{ text: string, role?: "user"|"system"|"assistant", metadata?: object }`
- `completion`: `{ prompt: string, max_tokens?: number, temperature?: number }`
- `embedding`: `{ input: string | string[], metadata?: object }`

**Validation**:
- Router validates all incoming requests at runtime
- Missing required fields → `ErrorResponse` with `code: "invalid_request"`
- Proto contract fields are optional (backward compatibility)
- Runtime validation enforces required fields

#### DecideResponse (Router → Gateway)

**Required Fields**:
- `ok`: Boolean (required)
- `decision`: Object (required when `ok: true`)
  - `provider_id`: String (required)
  - `priority`: Number (required)
  - `reason`: String (required)
  - `expected_latency_ms?`: Number (optional)
  - `expected_cost?`: Number (optional)
  - `policy_id?`: String (optional)
  - `metadata?`: Object (optional)
- `context`: Object with `request_id` (required), `trace_id?` (optional)

**Error Response**:
- `ok`: `false`
- `error`: Object with `code`, `message`, `details?`
  - `code`: `"unauthorized"` | `"invalid_request"` | `"policy_not_found"` | `"denied"` | `"decision_failed"` | `"internal"`
- `context`: Object with `request_id`, `trace_id?`

---

### 2. Router ↔ Worker Contracts

#### ExecAssignment (Router → Worker)

**Source of Truth**: `docs/API_CONTRACTS.md`

**Required Fields**:
- `version`: Must be `"1"` (string)
- `assignment_id`: UUID string (required)
- `request_id`: UUID string (required)
- `tenant_id`: String (required, non-empty)
- `executor`: Object (required)
  - `provider_id`: String (required, must be supported)
  - `channel`: `"nats"` | `"grpc"` (required)
  - `endpoint?`: String (optional)
- `job`: Object (required)
  - `type`: String (required, must be supported)
  - `payload_ref?`: URI string (optional)
  - `payload?`: Object (optional, null if using `payload_ref`)

**Optional Fields**:
- `options`: Object with `priority?`, `deadline_ms?`, `retry?`
- `correlation`: Object with `trace_id?`
- `decision`: Object with provider selection metadata
- `metadata`: Object (any map)

**Validation**:
- Worker validates `ExecAssignment` before accepting
- Invalid `ExecAssignment` → Publish `ExecAssignmentAck` with `status: "rejected"`
- Missing required fields → Reject with reason

#### ExecAssignmentAck (Worker → Router)

**Required Fields**:
- `version`: Must be `"1"` (string)
- `assignment_id`: UUID string (required, matches `ExecAssignment.assignment_id`)
- `status`: `"accepted"` | `"rejected"` | `"error"` (string, required)
- `message?`: String (optional, status message)

**Optional Fields**:
- `reason?`: String (optional, rejection reason)
- `correlation?`: Object with `trace_id?`
- `tenant_id?`: String (optional)
- `metadata?`: Object (optional)

**Router Guarantees**:
- `ExecAssignmentAck` **only** processed if received on `caf.exec.assign.v1.ack` subject
- `assignment_id` **always** matches `ExecAssignment.assignment_id`
- `status` **always** one of: `"accepted"`, `"rejected"`, `"error"`

#### ExecResult (Worker → Router)

**Source of Truth**: `docs/API_CONTRACTS.md`

**CP1 StepResult Contract**: CAF Worker internally uses a unified `StepResult` type (C++ struct) for all block executions. This type is converted to `ExecResult` JSON format via `ResultConverter::to_exec_result_json()` before publishing to NATS.

**StepResult → ExecResult Mapping**:
- `StepStatus::ok` → `ExecResult.status = "success"`
- `StepStatus::error` → `ExecResult.status = "error"`
- `StepStatus::timeout` → `ExecResult.status = "timeout"`
- `StepStatus::cancelled` → `ExecResult.status = "cancelled"`
- `ErrorCode` (1xxx-5xxx) → `ExecResult.error_code` (string format)
- `ResultMetadata` (trace_id, run_id, flow_id, step_id, tenant_id) → `ExecResult` correlation fields

**Required Fields**:
- `version`: Must be `"1"` (string)
- `assignment_id`: UUID string (required)
- `request_id`: UUID string (required)
- `status`: `"success"` | `"error"` | `"timeout"` | `"cancelled"` (string, required)
- `provider_id`: String (required)
- `job`: Object with `type` (required)
- `timestamp`: Integer (milliseconds, required)

**Optional Fields**:
- `latency_ms`: Number (optional)
- `cost`: Number (optional)
- `trace_id`: String (optional, from `ResultMetadata`)
- `tenant_id`: String (optional, from `ResultMetadata`)
- `run_id`: String (optional, CP1 observability invariant)
- `msg_id`: String (optional)

**Headers (JetStream, priority over payload)**:
- `trace_id` (optional): Trace identifier
- `tenant_id` (optional): Tenant identifier
- `version` (optional): Message version
- `Nats-Msg-Id`: Message ID for ACK/NAK operations
- `traceparent` (optional): W3C trace context, used for OTel propagation

**Validation**:
- At least one of `assignment_id` or `request_id` must be present
- `status` is required and must be one of: `"success"`, `"error"`, `"timeout"`, `"cancelled"`
- `provider_id` is required
- `timestamp` is in milliseconds (integer)
- Headers have priority over payload for `trace_id`, `tenant_id`, `version`

---

### 3. StepResult Contract (CP1 Invariant)

**CRITICAL**: All block executions in CP1 must return a unified `StepResult` type with complete metadata. This contract ensures predictable and reliable integration with Router.

**Contract Definition** (C++ struct):

```cpp
struct StepResult {
    StepStatus status;                    // Required: ok | error | timeout | cancelled
    ErrorCode error_code;                  // Required: Machine-readable error code
    ResultMetadata metadata;               // Required: Complete correlation metadata
    std::unordered_map<std::string, std::string> outputs;  // Optional: Execution outputs
    std::string error_message;            // Optional: Human-readable error message
    int64_t latency_ms;                   // Optional: Execution latency
    int32_t retries_used;                 // Optional: Number of retries attempted
};
```

**Required Fields**:
- `status`: Must be one of `ok`, `error`, `timeout`, `cancelled`
- `error_code`: Must be set (use `ErrorCode::none` for success)
- `metadata`: Must contain complete correlation IDs (trace_id, flow_id, step_id, tenant_id, run_id)

**Status Mapping** (StepResult → ExecResult):
- `StepStatus::ok` → `ExecResult.status = "success"`
- `StepStatus::error` → `ExecResult.status = "error"`
- `StepStatus::timeout` → `ExecResult.status = "timeout"`
- `StepStatus::cancelled` → `ExecResult.status = "cancelled"`

**Error Code Mapping**:
- `ErrorCode` (1xxx-5xxx) → `ExecResult.error_code` (string format)
- Examples: `ErrorCode::network_error` → `"NETWORK_ERROR"`

**Metadata Preservation**:
- `ResultMetadata.trace_id` → `ExecResult.trace_id`
- `ResultMetadata.run_id` → `ExecResult.run_id` (CP1 observability invariant)
- `ResultMetadata.tenant_id` → `ExecResult.tenant_id`
- `ResultMetadata.flow_id` → (not in ExecResult, but preserved in Worker logs)
- `ResultMetadata.step_id` → (not in ExecResult, but preserved in Worker logs)

**Implementation**:
- StepResult type: `apps/caf/processor/include/beamline/worker/core.hpp`
- ResultConverter: `apps/caf/processor/include/beamline/worker/result_converter.hpp`
- Conversion: `ResultConverter::to_exec_result_json()` for NATS publishing

**Reference**: `apps/caf/processor/docs/ARCHITECTURE_ROLE.md#43-stepresult-contract-cp1-invariant`

---

## CP1 Contract Validation

### Runtime Validation

**Router**:
- Validates all incoming requests at runtime
- Missing required fields → `ErrorResponse` with `code: "invalid_request"`
- Contract violations logged with `contract_violation: true`
- Metrics emitted: `router_nats_contract_violations_total`

**Worker**:
- Validates `ExecAssignment` before accepting
- Invalid `ExecAssignment` → Publish `ExecAssignmentAck` with `status: "rejected"`
- Missing required fields → Reject with reason

### Contract Tests

**Required**:
- ✅ `test_worker_router_contract.cpp` - StepResult → ExecResult conversion tests
  - Status mapping tests (success, error, timeout, cancelled)
  - Error code mapping tests (1xxx-5xxx)
  - Metadata preservation tests (correlation IDs)
- ✅ `router_worker_contract_SUITE.erl` - Router-side ExecResult processing tests
  - ExecResult validation tests
  - Correlation fields preservation tests

**Reference**: `docs/archive/dev/WORKER_ROUTER_CONTRACT_TESTS.md`

---

## CP2+ Optional/Enhancement Contracts

### Advanced Features (CP2+)

**CP2+**:
- 📋 Advanced idempotency (composite signatures, TTL stores)
- 📋 Complex retry policies (circuit breakers, exponential backoff with jitter)
- 📋 Advanced metadata (user_id, session_id, custom fields)
- 📋 Proto message definitions (currently JSON only)
- 📋 JetStream integration (durable subscriptions, ACK/NAK)
- 📋 Advanced error handling (retryable flags, error categories)

**Excluded from CP1**:
- ❌ Advanced idempotency beyond basic `assignment_id` uniqueness
- ❌ Complex retry policies (simple retry is acceptable)
- ❌ Advanced metadata beyond correlation IDs
- ❌ Proto message definitions (JSON contracts are sufficient for CP1)

---

## CP1 Acceptance Criteria

### Functional Requirements

- ✅ Router validates all incoming requests (DecideRequest, ExecResult)
- ✅ Worker validates ExecAssignment before accepting
- ✅ Worker publishes ExecAssignmentAck with correct status
- ✅ Worker converts StepResult to ExecResult correctly
- ✅ Router processes ExecResult with correct status mapping
- ✅ Correlation fields preserved through all contracts

### Non-Functional Requirements

- ✅ **Stability**: Contracts are consistent and reliable
- ✅ **Predictability**: Status codes are consistent across components
- ✅ **Observability**: Correlation fields available for tracing
- ✅ **Contract Compliance**: All components follow contracts exactly

### Test Coverage

- ✅ Contract tests (StepResult → ExecResult)
- ✅ Validation tests (required fields, status codes)
- ✅ Metadata preservation tests
- ✅ Integration tests (Router ↔ Worker)

---

## References

### CP1 Documentation
- `docs/CP1_ACCEPTANCE_REPORT.md` - CP1 acceptance criteria and verification (see "API Compatibility" section)
- `docs/API_CONTRACTS.md` - Complete API contracts specification
- `apps/caf/processor/docs/ARCHITECTURE_ROLE.md#43-stepresult-contract-cp1-invariant` - StepResult contract definition
- `docs/archive/dev/WORKER_ROUTER_CONTRACT_TESTS.md` - Contract tests documentation
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto vs NATS JSON format mapping

### CP2 Planning Documents
- `docs/archive/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md` - CP2 Proto definitions planning
- `docs/archive/dev/CP2_WORKER_ASSIGNMENTS_DETAILED.md` - CP2 Worker enhancements planning

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP1 core contracts profile definition
- StepResult contract specification
- ExecAssignment/ExecResult contracts
- DecideRequest/DecideResponse contracts
- CP1 vs CP2+ separation

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP1-LC  
**Status**: Core Profile Definition

