# Router CP1 Complete Plan: A → B → C Vectors

**Date**: 2025-01-27  
**Status**: Ready for Execution  
**Version**: 1.0

## Purpose

This document provides a sequential, step-by-step plan to complete CP1 requirements across three vectors:
- **A. Router** (behavior/errors/idempotency)
- **B. Gateway ↔ Router** (CP1-smoke / NATS integration)
- **C. Observability baseline** for Router (CP1-minimum logging)

Each vector is divided into: **Analysis → Implementation → Documentation/Integration**.

---

## Overall Sequence

```
A. Router (behavior/errors/idempotency)
    ↓
B. Gateway ↔ Router (CP1-smoke / NATS integration)
    ↓
C. Observability baseline for Router (CP1-minimum logging)
```

**Execution Order**: Sequential (A → B → C)  
**Estimated Time**: 5-7 days total  
**Dependencies**: Each vector builds on the previous one

---

## A. Router — Behavior, Errors, Idempotency (CP1)

### A1. Analysis of Current State (CP1 for Router)

**Goal**: Understand CP1 requirements and current implementation gaps

#### Step A1.1: Read CP1 Requirements Documents

**Files to Read**:
- `docs/CP1_ACCEPTANCE_REPORT.md` - Main CP1 acceptance report
- `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md` - Router-specific CP1 report
- `../../../apps/otp/router/docs/archive/dev_reports/CP1_ACCEPTANCE_SUMMARY.md` - Router CP1 summary
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` - CP1 boundaries and contracts

**Tasks**:
- [ ] Extract CP1 behavioral invariants:
  - Retry policy requirements
  - Error handling requirements
  - Idempotency guarantees (if any in CP1)
  - Routing rules and decision logic
- [ ] Identify explicitly uncovered test cases
- [ ] Document findings in analysis table

**Output**: Analysis table: **"CP1 Requirement → Code Implementation → Test Coverage → Gap Status"**

#### Step A1.2: Review Router Code and Tests

**Code to Review**:
- `apps/otp/router/src/router_nats_subscriber.erl` - NATS publish/subscribe
- `apps/otp/router/src/router_core.erl` - Routing decision logic
- `apps/otp/router/src/router_decider.erl` - Decision engine
- `apps/otp/router/src/router_error.erl` - Error handling (if exists)
- `apps/otp/router/src/router_idempotency.erl` - Idempotency layer (if exists, check if CP1 or CP2+)

**Test Suites to Review**:
- `apps/otp/router/test/router_core_SUITE.erl` - Core routing tests
- `apps/otp/router/test/router_decider_SUITE.erl` - Decision engine tests
- `apps/otp/router/test/router_error_SUITE.erl` - Error handling tests
- `apps/otp/router/test/router_cp1_minimal_mode_SUITE.erl` - CP1 minimal mode tests
- `apps/otp/router/test/router_e2e_smoke_SUITE.erl` - E2E smoke tests

**E2E/Contract Tests**:
- `tests/integration/*` - Integration tests involving Router
- `tests/e2e/*` - End-to-end tests

**Tasks**:
- [ ] Map each CP1 requirement to code modules/functions
- [ ] Map each CP1 requirement to test suites/cases
- [ ] Identify gaps: requirements without tests
- [ ] Identify gaps: code without CP1 requirements

**Output**: Gap analysis table with specific file paths and line numbers

#### Step A1.3: Create Gap Analysis Document

**File to Create**: `docs/archive/dev/ROUTER_CP1_GAP_ANALYSIS.md`

**Content**:
- CP1 requirements matrix
- Code implementation mapping
- Test coverage mapping
- Gap identification (requirements without tests, code without requirements)
- Priority classification (critical, high, medium, low)

**Acceptance Criteria**:
- ✅ All CP1 requirements from acceptance reports are listed
- ✅ Each requirement has code location (or "NOT IMPLEMENTED")
- ✅ Each requirement has test location (or "NOT TESTED")
- ✅ Gaps are clearly identified with priority

---

### A2. Implementation of Missing Pieces

**Goal**: Implement missing CP1 behavior, error handling, and idempotency guarantees

#### Step A2.1: Behavior and Error Handling

**Scenarios to Implement/Verify**:

1. **NATS Subject Not Available**:
   - [ ] Behavior: Router returns error response
   - [ ] Error code: `internal` or `service_unavailable`
   - [ ] Logging: Error logged with context
   - [ ] Test: `router_error_SUITE.erl` - `nats_subject_unavailable_test/0`

2. **NATS Unavailable**:
   - [ ] Behavior: Router handles connection failure gracefully
   - [ ] Error code: `service_unavailable`
   - [ ] Logging: Connection error logged
   - [ ] Test: `router_error_SUITE.erl` - `nats_unavailable_test/0`

3. **Invalid Payload (Schema Error)**:
   - [ ] Behavior: Router validates payload against schema
   - [ ] Error code: `invalid_request`
   - [ ] Error details: Field name and validation failure type
   - [ ] Logging: Validation error logged
   - [ ] Test: `router_error_SUITE.erl` - `invalid_payload_test/0`

4. **Internal Router Error**:
   - [ ] Behavior: Router catches internal errors
   - [ ] Error code: `internal`
   - [ ] Logging: Error logged with stack trace (if available)
   - [ ] Test: `router_error_SUITE.erl` - `internal_error_test/0`

5. **No Route Found**:
   - [ ] Behavior: Router returns error when no route available
   - [ ] Error code: `policy_not_found` or `decision_failed`
   - [ ] Logging: Routing failure logged
   - [ ] Test: `router_core_SUITE.erl` - `no_route_found_test/0`

**Implementation Tasks**:
- [ ] Review error handling in `router_nats_subscriber.erl`
- [ ] Review error handling in `router_core.erl`
- [ ] Add/update error handling for each scenario
- [ ] Ensure error responses match CP1 contract (see `docs/API_CONTRACTS.md`)

**Test Tasks**:
- [ ] Add/update tests in `router_error_SUITE.erl`
- [ ] Add/update tests in `router_core_SUITE.erl`
- [ ] Verify all error scenarios are covered

**Acceptance Criteria**:
- ✅ All error scenarios have predictable, documented behavior
- ✅ Error responses match CP1 contract format
- ✅ All error scenarios have tests
- ✅ Error logging is consistent and informative

#### Step A2.2: Idempotency (CP1 Scope)

**CP1 vs CP2+ Separation**:

**CP1 Idempotency** (if required):
- [ ] Identify CP1 idempotency requirements from acceptance reports
- [ ] Separate from CP2+ idempotency features
- [ ] Document CP1 scope vs CP2+ scope

**CP1 Implementation** (if required):
- [ ] Implement minimal idempotency guarantees (e.g., ETS-based deduplication by key)
- [ ] Ensure no CP2+ features are included
- [ ] Add tests in `router_core_SUITE.erl` or new `router_idempotency_cp1_SUITE.erl`

**If Idempotency is CP2+ Only**:
- [ ] Document that idempotency is deferred to CP2+
- [ ] Ensure no CP1 promises include idempotency
- [ ] Update CP1 acceptance reports if needed

**Acceptance Criteria**:
- ✅ CP1 idempotency scope is clearly defined
- ✅ CP1 implementation (if any) is separate from CP2+
- ✅ CP1 idempotency is tested (if required)
- ✅ CP2+ idempotency is not included in CP1

#### Step A2.3: Test Coverage Expansion

**Test Suites to Expand**:

1. **router_core_SUITE.erl**:
   - [ ] Positive scenarios: Successful routing
   - [ ] Negative scenarios: NATS down, invalid payload, duplicates
   - [ ] Edge cases: Empty policies, missing providers

2. **router_error_SUITE.erl**:
   - [ ] All error scenarios from A2.1
   - [ ] Error code mapping verification
   - [ ] Error response format validation

3. **router_e2e_smoke_SUITE.erl**:
   - [ ] End-to-end success path
   - [ ] End-to-end error paths
   - [ ] Integration with NATS

**E2E Tests** (if needed):
- [ ] Add tests in `tests/e2e/router/` (if directory exists)
- [ ] Verify Router behavior in full stack context

**Acceptance Criteria**:
- ✅ All CP1 behavioral requirements have tests
- ✅ All error scenarios have tests
- ✅ Test coverage for Router core functionality ≥ 95%
- ✅ All tests pass

---

### A3. Documentation and Integration

**Goal**: Document Router CP1 behavior and integrate into CP1 validation

#### Step A3.1: Update CP1 Reports

**Files to Update**:
- `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md`
- `docs/CP1_ACCEPTANCE_REPORT.md` (Router section)

**Content to Add**:
- [ ] Exact module/function references for each CP1 requirement
- [ ] Test suite/case references for each CP1 requirement
- [ ] Error handling behavior documentation
- [ ] Idempotency scope (CP1 vs CP2+)

**Format**:
```markdown
### Router Behavior: Error Handling

**Requirement**: Router handles NATS unavailability gracefully

**Implementation**:
- Module: `router_nats_subscriber.erl`
- Function: `handle_nats_error/2` (line 123)
- Error Code: `service_unavailable`

**Test Coverage**:
- Suite: `router_error_SUITE.erl`
- Test: `nats_unavailable_test/0` (line 45)
```

#### Step A3.2: Update/Verify Validation Scripts

**Scripts to Check/Update**:
- `scripts/check_cp1_contracts.sh` - CP1 contract validation
- `scripts/run_checks.sh` - Main validation script
- `apps/otp/router/scripts/test_cp1_smoke.sh` - CP1 smoke tests

**Tasks**:
- [ ] Verify `check_cp1_contracts.sh` runs Router tests
- [ ] Verify `test_cp1_smoke.sh` includes all CP1 test suites
- [ ] Ensure Router tests are called from `run_checks.sh`
- [ ] Add Router-specific validation if needed

**Acceptance Criteria**:
- ✅ Router CP1 tests are integrated into validation pipeline
- ✅ `check_cp1_contracts.sh` validates Router behavior
- ✅ All validation scripts pass

---

## B. Gateway ↔ Router — CP1 Integration via NATS

### B1. Analysis of Current Integration

**Goal**: Understand Gateway↔Router integration requirements and current state

#### Step B1.1: Read Integration Specifications

**Files to Read**:
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto/NATS mapping (already updated in CP1.1/CP1.2)
- `docs/CP1_ACCEPTANCE_REPORT.md` - Gateway↔Router section
- `docs/API_CONTRACTS.md` - API contracts (already updated in CP1.1/CP1.2)
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` - Module boundaries

**Tasks**:
- [ ] Extract CP1 REST/NATS scenarios:
  - Success paths (main routing flows)
  - Negative scenarios (invalid body, no route, Router errors)
- [ ] Document expected behavior for each scenario
- [ ] Identify integration points (Gateway → NATS → Router)

**Output**: Integration scenarios matrix

#### Step B1.2: Review Smoke/E2E Tests

**Scripts to Review**:
- `scripts/gateway_router_contract_smoke.sh` - Gateway↔Router smoke tests
- `scripts/validate_all_projects.sh` - Main validation script

**E2E Tests to Review**:
- `tests/integration/gateway_router/*` - Gateway↔Router integration tests
- `tests/e2e/gateway_router/*` - End-to-end Gateway↔Router tests

**Tasks**:
- [ ] Review `gateway_router_contract_smoke.sh`:
  - What scenarios are covered?
  - What scenarios are missing?
  - Are all CP1 scenarios included?
- [ ] Review integration/E2E tests:
  - What Gateway↔Router flows are tested?
  - What flows are missing?
- [ ] Create matrix: **"CP1 Scenario ↔ Smoke/E2E Test → Coverage Status"**

**Output**: Test coverage matrix with gaps identified

#### Step B1.3: Create Integration Gap Analysis

**File to Create**: `docs/archive/dev/GATEWAY_ROUTER_CP1_GAP_ANALYSIS.md`

**Content**:
- CP1 integration scenarios matrix
- Smoke test coverage mapping
- E2E test coverage mapping
- Gap identification (scenarios without tests)
- Priority classification

**Acceptance Criteria**:
- ✅ All CP1 Gateway↔Router scenarios are listed
- ✅ Each scenario has test location (or "NOT TESTED")
- ✅ Gaps are clearly identified with priority

---

### B2. Strengthen CP1 Smoke/E2E

**Goal**: Ensure complete CP1 smoke/E2E test coverage

#### Step B2.1: Enhance Smoke Test Suite

**Script to Update**: `scripts/gateway_router_contract_smoke.sh`

**Scenarios to Add/Verify**:

1. **Success Paths**:
   - [ ] Basic routing: Gateway → NATS → Router → Response
   - [ ] Policy-based routing: Different policies
   - [ ] Sticky sessions: Session affinity (if CP1)

2. **Error Scenarios**:
   - [ ] Bad request (REST → NATS): Invalid JSON body
   - [ ] Router errors: Router returns error → Gateway propagates
   - [ ] NATS unavailable: Gateway handles NATS connection failure
   - [ ] Timeout: Router timeout → Gateway returns timeout error

**Implementation Tasks**:
- [ ] Review current `gateway_router_contract_smoke.sh`
- [ ] Add missing scenarios
- [ ] Ensure all scenarios are easily runnable locally
- [ ] Add clear output (pass/fail per scenario)

**Acceptance Criteria**:
- ✅ All CP1 success paths are covered
- ✅ All CP1 error scenarios are covered
- ✅ Script runs locally without issues
- ✅ Script provides clear pass/fail output

#### Step B2.2: Enhance E2E Tests

**Tests to Add/Update**:
- `tests/integration/gateway_router/*` - Integration tests
- `tests/e2e/gateway_router/*` - End-to-end tests

**Scenarios to Add/Verify**:
- [ ] REST DTO → NATS message mapping correctness
- [ ] NATS message → Router behavior correctness
- [ ] Router response → NATS → Gateway response mapping
- [ ] Negative tests: Invalid payloads, headers
- [ ] Error propagation: Router errors → Gateway errors

**Implementation Tasks**:
- [ ] Review existing E2E tests
- [ ] Add missing scenarios
- [ ] Ensure tests use real NATS (or mock NATS correctly)
- [ ] Verify tests are deterministic and repeatable

**Acceptance Criteria**:
- ✅ All CP1 Gateway↔Router flows are tested
- ✅ Error propagation is tested
- ✅ Tests are deterministic and repeatable
- ✅ All tests pass

---

### B3. Integration into CP1 Process

**Goal**: Integrate Gateway↔Router smoke/E2E into CP1 validation pipeline

#### Step B3.1: Automation Integration

**Scripts to Update**:
- `scripts/validate_all_projects.sh` - Main validation script
- `scripts/run_checks.sh` - Check runner
- `.github/workflows/validate.yml.template` - CI/CD workflow

**Tasks**:
- [ ] Ensure `gateway_router_contract_smoke.sh` is called from `validate_all_projects.sh`
- [ ] Ensure smoke script failure blocks CP1 acceptance
- [ ] Add Gateway↔Router E2E tests to CI/CD pipeline
- [ ] Verify exit codes are correct (0 = success, non-zero = failure)

**Acceptance Criteria**:
- ✅ Smoke scripts are called from validation pipeline
- ✅ Smoke script failure blocks CP1
- ✅ CI/CD includes Gateway↔Router tests

#### Step B3.2: Documentation Updates

**Files to Update**:
- `docs/CP1_ACCEPTANCE_REPORT.md` - Add Gateway↔Router CP1-smoke section
- `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md` - Add integration section

**Content to Add**:
- [ ] "Gateway↔Router CP1-smoke" section:
  - List of scenarios covered
  - Links to smoke scripts
  - Links to E2E tests
  - Expected behavior for each scenario

**Acceptance Criteria**:
- ✅ CP1 reports include Gateway↔Router smoke section
- ✅ All scenarios are documented
- ✅ Links to scripts/tests are correct

---

## C. Observability Baseline for Router (CP1)

### C1. Analysis of Current Observability

**Goal**: Understand CP1 observability requirements and current implementation

#### Step C1.1: Read Observability Documents

**Files to Read**:
- `docs/OBSERVABILITY.md` - Main observability specification
- `docs/OBSERVABILITY_CONVENTIONS.md` - Logging conventions
- `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md` - Router CP1 observability requirements

**Tasks**:
- [ ] Extract CP1 observability requirements:
  - Required log fields for Router
  - Log levels (ERROR, WARN, INFO, DEBUG)
  - Health endpoint requirements (if Router has direct endpoint)
  - Structured JSON logging format
- [ ] Identify PII/secret filtering requirements
- [ ] Document CP1 scope vs CP2+ observability (Prometheus, Grafana, etc.)

**Output**: CP1 observability requirements list

#### Step C1.2: Review Router Logging Code

**Code to Review**:
- `apps/otp/router/src/router_logger.erl` - Logging module (if exists)
- `apps/otp/router/src/*.erl` - Modules that generate logs (grep for logging calls)
- `apps/otp/router/src/router_nats_subscriber.erl` - NATS logging
- `apps/otp/router/src/router_core.erl` - Core routing logging

**Tasks**:
- [ ] Find all logging calls in Router code
- [ ] Identify log format (JSON, plain text, etc.)
- [ ] Check log fields present (trace_id, tenant_id, subject, error, severity, etc.)
- [ ] Compare with `OBSERVABILITY_CONVENTIONS.md`:
  - What fields are present?
  - What fields are missing?
  - Are secrets/PII filtered?

**Output**: Logging implementation analysis

#### Step C1.3: Review Health Endpoint (if applicable)

**Code to Review**:
- `apps/otp/router/src/router_health.erl` - Health endpoint (if exists)
- `apps/otp/router/src/router_sup.erl` - Supervisor (may expose health)
- gRPC health service (if Router exposes gRPC health)

**Tasks**:
- [ ] Identify Router health endpoint (if any)
- [ ] Check health endpoint format (HTTP, gRPC)
- [ ] Verify health endpoint matches CP1 requirements
- [ ] Document health endpoint location and format

**Output**: Health endpoint analysis

#### Step C1.4: Create Observability Gap Analysis

**File to Create**: `docs/archive/dev/ROUTER_OBSERVABILITY_CP1_GAP_ANALYSIS.md`

**Content**:
- CP1 observability requirements
- Current logging implementation
- Gap identification (missing fields, missing filtering, etc.)
- Priority classification

**Acceptance Criteria**:
- ✅ All CP1 observability requirements are listed
- ✅ Current implementation is documented
- ✅ Gaps are clearly identified with priority

---

### C2. Enhance Observability for CP1

**Goal**: Align Router observability with CP1 requirements

#### Step C2.1: Log Format Alignment

**Requirements**:
- [ ] All Router logs use structured JSON format
- [ ] Required fields present:
  - `timestamp` (ISO 8601, UTC)
  - `level` (ERROR, WARN, INFO, DEBUG)
  - `component` ("router")
  - `message` (human-readable)
- [ ] Optional fields (when available):
  - `context` (structured JSON object)
  - `tenant_id` (when available)
  - `trace_id` (when available)
  - `error` (for ERROR level)

**Implementation Tasks**:
- [ ] Review/update logging module (or create if missing)
- [ ] Ensure all log calls use structured JSON format
- [ ] Add required fields to all log calls
- [ ] Verify log format matches `OBSERVABILITY_CONVENTIONS.md`

**Acceptance Criteria**:
- ✅ All Router logs are structured JSON
- ✅ Required fields are present in all logs
- ✅ Log format matches conventions

#### Step C2.2: PII/Secret Filtering

**Requirements**:
- [ ] All sensitive data is filtered before logging
- [ ] Filtered fields: `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`, `authorization`, `credit_card`, `ssn`, `email`, `phone`
- [ ] Replacement: `[REDACTED]` or `[MASKED]`

**Implementation Tasks**:
- [ ] Review all log calls for sensitive data
- [ ] Add filtering function (if not exists)
- [ ] Apply filtering to all log calls
- [ ] Verify no secrets appear in logs

**Acceptance Criteria**:
- ✅ All sensitive fields are filtered
- ✅ No secrets appear in logs
- ✅ Filtering is consistent across all modules

#### Step C2.3: Logging Scenarios

**Scenarios to Verify**:
- [ ] NATS errors: Logged with context (subject, error type)
- [ ] Routing errors: Logged with context (tenant_id, policy_id, error)
- [ ] Invalid payloads: Logged with validation error details
- [ ] Internal errors: Logged with error details (no stack traces in production)

**Implementation Tasks**:
- [ ] Review error logging in Router modules
- [ ] Ensure all error scenarios are logged
- [ ] Verify logs allow "replaying" problems from log chain

**Acceptance Criteria**:
- ✅ All key CP1 scenarios are logged
- ✅ Logs contain sufficient context for debugging
- ✅ Log format is consistent across scenarios

#### Step C2.4: Health Endpoint (if applicable)

**If Router has health endpoint**:
- [ ] Verify health endpoint format matches CP1 requirements
- [ ] Ensure health endpoint returns JSON with `status` and `timestamp`
- [ ] Add health endpoint tests

**If Router does not have health endpoint**:
- [ ] Document that Router health is checked via NATS/gRPC (if applicable)
- [ ] Or document that health endpoint is deferred to CP2+

**Acceptance Criteria**:
- ✅ Health endpoint (if exists) matches CP1 requirements
- ✅ Health endpoint is tested
- ✅ Health endpoint status is documented

---

### C3. Observability Testing and Integration

**Goal**: Test observability implementation and integrate into CP1 validation

#### Step C3.1: Observability Tests

**Tests to Add/Update**:
- `apps/otp/router/test/router_observability_SUITE.erl` - New or existing observability test suite

**Test Scenarios**:
- [ ] Log format validation: All logs are valid JSON
- [ ] Required fields validation: All logs contain required fields
- [ ] Secret filtering validation: No secrets in logs
- [ ] Log level validation: Correct log levels used
- [ ] Health endpoint validation (if applicable)

**Implementation Tasks**:
- [ ] Create/update `router_observability_SUITE.erl`
- [ ] Add log format validation tests
- [ ] Add secret filtering tests
- [ ] Add health endpoint tests (if applicable)

**Optional: Log Validator Script**:
- [ ] Create `tests/utils/log_conformance_validator.py` (or similar)
- [ ] Script validates log format, fields, and secret filtering
- [ ] Script can be run standalone or in tests

**Acceptance Criteria**:
- ✅ Log format is validated by tests
- ✅ Required fields are validated
- ✅ Secret filtering is validated
- ✅ All observability tests pass

#### Step C3.2: Integration into Validation Pipeline

**Scripts to Update**:
- `scripts/run_checks.sh` - Main validation script
- `scripts/validate_observability.sh` - Observability validation (if exists, or create)
- `.github/workflows/validate.yml.template` - CI/CD workflow

**Tasks**:
- [ ] Add observability validation to `run_checks.sh`
- [ ] Create/update `validate_observability.sh`:
  - Validates log format
  - Validates required fields
  - Validates secret filtering
- [ ] Integrate into CI/CD pipeline
- [ ] Ensure observability validation failure blocks CP1

**Acceptance Criteria**:
- ✅ Observability validation is in validation pipeline
- ✅ Validation script checks log format, fields, and filtering
- ✅ Validation failure blocks CP1 acceptance

#### Step C3.3: Documentation Updates

**Files to Update**:
- `docs/OBSERVABILITY.md` - Add Router CP1 observability section
- `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md` - Add observability section
- `docs/CP1_ACCEPTANCE_REPORT.md` - Add Router observability section

**Content to Add**:
- [ ] "CP1 Observability Router" section:
  - Log format specification
  - Required fields list
  - PII/secret filtering rules
  - Health endpoint (if applicable)
  - Links to tests and validation scripts

**Acceptance Criteria**:
- ✅ CP1 reports include Router observability section
- ✅ Log format is documented
- ✅ Filtering rules are documented
- ✅ Links to tests/scripts are correct

---

## Execution Checklist

### Phase A: Router (Behavior/Errors/Idempotency)

- [ ] **A1.1**: Read CP1 requirements documents
- [ ] **A1.2**: Review Router code and tests
- [ ] **A1.3**: Create gap analysis document
- [ ] **A2.1**: Implement/verify behavior and error handling
- [ ] **A2.2**: Implement/verify CP1 idempotency (if required)
- [ ] **A2.3**: Expand test coverage
- [ ] **A3.1**: Update CP1 reports with implementation references
- [ ] **A3.2**: Update/verify validation scripts

### Phase B: Gateway ↔ Router (CP1-smoke / NATS Integration)

- [ ] **B1.1**: Read integration specifications
- [ ] **B1.2**: Review smoke/E2E tests
- [ ] **B1.3**: Create integration gap analysis
- [ ] **B2.1**: Enhance smoke test suite
- [ ] **B2.2**: Enhance E2E tests
- [ ] **B3.1**: Integrate into validation pipeline
- [ ] **B3.2**: Update documentation

### Phase C: Observability Baseline (CP1)

- [ ] **C1.1**: Read observability documents
- [ ] **C1.2**: Review Router logging code
- [ ] **C1.3**: Review health endpoint (if applicable)
- [ ] **C1.4**: Create observability gap analysis
- [ ] **C2.1**: Align log format with CP1 requirements
- [ ] **C2.2**: Implement PII/secret filtering
- [ ] **C2.3**: Verify logging scenarios
- [ ] **C2.4**: Verify/implement health endpoint (if applicable)
- [ ] **C3.1**: Add observability tests
- [ ] **C3.2**: Integrate into validation pipeline
- [ ] **C3.3**: Update documentation

---

## Deliverables

### Analysis Documents

1. ✅ `docs/archive/dev/ROUTER_CP1_GAP_ANALYSIS.md` - Router CP1 gap analysis
2. ✅ `docs/archive/dev/GATEWAY_ROUTER_CP1_GAP_ANALYSIS.md` - Gateway↔Router integration gap analysis
3. ✅ `docs/archive/dev/ROUTER_OBSERVABILITY_CP1_GAP_ANALYSIS.md` - Router observability gap analysis

### Implementation

1. ✅ Router error handling enhancements (if needed)
2. ✅ Router CP1 idempotency (if required)
3. ✅ Test coverage expansion
4. ✅ Gateway↔Router smoke test enhancements
5. ✅ Gateway↔Router E2E test enhancements
6. ✅ Router observability alignment
7. ✅ Observability tests

### Documentation

1. ✅ Updated CP1 acceptance reports with implementation references
2. ✅ Updated validation scripts integration
3. ✅ Updated observability documentation

### Validation

1. ✅ All CP1 tests pass
2. ✅ All smoke tests pass
3. ✅ All observability validation passes
4. ✅ CP1 validation pipeline passes

---

## Success Criteria

### Phase A: Router

- ✅ All CP1 behavioral requirements are implemented
- ✅ All error scenarios are handled correctly
- ✅ CP1 idempotency scope is clear and implemented (if required)
- ✅ Test coverage ≥ 95% for Router core functionality
- ✅ All tests pass

### Phase B: Gateway ↔ Router

- ✅ All CP1 integration scenarios are covered by smoke/E2E tests
- ✅ Smoke tests run locally without issues
- ✅ Smoke tests are integrated into validation pipeline
- ✅ All smoke/E2E tests pass

### Phase C: Observability

- ✅ All Router logs use structured JSON format
- ✅ Required fields are present in all logs
- ✅ PII/secrets are filtered
- ✅ Observability validation passes
- ✅ Observability is documented

---

## Estimated Timeline

**Phase A (Router)**: 2-3 days
- A1 (Analysis): 0.5 days
- A2 (Implementation): 1-1.5 days
- A3 (Documentation): 0.5 days

**Phase B (Gateway ↔ Router)**: 1-2 days
- B1 (Analysis): 0.5 days
- B2 (Implementation): 0.5-1 day
- B3 (Integration): 0.5 days

**Phase C (Observability)**: 1-2 days
- C1 (Analysis): 0.5 days
- C2 (Implementation): 0.5-1 day
- C3 (Testing/Integration): 0.5 days

**Total**: 5-7 days

---

## Dependencies

**Phase A → Phase B**:
- Router behavior must be stable before Gateway↔Router integration testing

**Phase B → Phase C**:
- Integration must be stable before observability validation

**All Phases → CP1 Acceptance**:
- All phases must be complete before CP1 acceptance

---

## References

- **CP1 Acceptance Report**: `docs/CP1_ACCEPTANCE_REPORT.md`
- **Router CP1 Report**: `apps/otp/router/docs/archive/dev/CP1_ACCEPTANCE_REPORT.md`
- **CP1 Boundaries**: `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md`
- **Observability Conventions**: `docs/OBSERVABILITY_CONVENTIONS.md`
- **Proto/NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- **API Contracts**: `docs/API_CONTRACTS.md`
- **CP1 Testing Guide**: `apps/otp/router/docs/CP1_TESTING.md`

---

## Notes

- **Sequential Execution**: Phases must be executed in order (A → B → C)
- **No Skipping**: Each phase builds on the previous one
- **Documentation First**: Analysis documents should be created before implementation
- **Test-Driven**: Tests should be added/updated alongside implementation
- **Validation Integration**: All validation should be integrated into pipeline before completion

