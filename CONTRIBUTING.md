# Contributing to Beamline Constructor

Thank you for contributing to Beamline Constructor! This guide will help you understand our development process and ensure your contributions align with project standards.

## Table of Contents

- [Development Process](#development-process)
- [JetStream/NATS/OBS Changes](#jetstreamnatsob-changes)
- [Pull Request Process](#pull-request-process)
- [Code Standards](#code-standards)
- [Testing Requirements](#testing-requirements)

---

## Development Process

### Checkpoints (CP)

Beamline Constructor uses a checkpoint-based development model. Each checkpoint represents a milestone with specific validation gates.

**Current Checkpoint**: CP1-LC (Operational Readiness)

**Key Documents**:
- `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md` - Project vision and architecture
- `docs/OPERATIONS_GUIDE_RU.md` - Operational routines and daily workflows
- `docs/dev/PR_CHECKLIST.md` - PR validation checklist

### Pre-PR Checklist

Before submitting a PR, ensure:

1. **Local Validation**: Run `bash scripts/dry_run_ci.sh all`
2. **Schema Validation**: Run `bash scripts/check_schema_changes.sh` (if proto/schema changes)
3. **State Validation**: Run `bash scripts/validate_state.sh` (if `.trae/state.json` changes)
4. **Link Check**: Run `bash scripts/check_links.sh` (if documentation changes)
5. **Security Scan**: Run `bash scripts/check_secret_leaks.sh` and `bash scripts/check_hmac_masking.sh`

---

## JetStream/NATS/OBS Changes

**CRITICAL**: Any changes to JetStream, NATS, or Observability (OBS) components **MUST** update the formal coverage documentation.

### What Requires Updates

**Changes to**:
- JetStream message handling (`apps/otp/router/src/router_jetstream.erl`)
- NATS operations (`apps/otp/router/src/router_nats.erl`)
- Metrics emission (`apps/otp/router/src/router_metrics.erl`)
- Alert rules (`apps/otp/router/docs/observability/router-alert-rules.yaml`)
- Dashboard panels (`docs/OBSERVABILITY_ROUTER_DASHBOARD.md`)
- Fault injection tests (`apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl`)
- Test scenarios (`apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`)

### Required Updates

#### 1. Coverage Matrix

**File**: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md`

**When to Update**:
- Adding new JetStream/NATS scenarios
- Adding new metrics
- Adding new alerts
- Adding new dashboard panels
- Changing test coverage status

**What to Update**:
- Add new scenario row with:
  - Scenario ID (S1-S3, JS-XXX, NATS-XXX, PERF-XXX)
  - Tests (unit, integration, performance, fault injection)
  - Metrics (with labels)
  - Alerts (with scenario IDs)
  - Dashboards (with panel references)
  - Coverage status (covered/partial/none)
  - Comments and future work

**Example**:
```markdown
| **NATS-009** | New NATS Operation | ✅ Covered<br/>- `test_new_operation/1` | ✅ Covered<br/>- `router_nats_new_operation_total` | ✅ Covered<br/>- `RouterNatsNewOperationHigh` | ✅ Covered<br/>- **Panel**: "New Operation Rate" | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Complete**: Full coverage |
```

#### 2. Fault Injection Test Scenarios

**File**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`

**When to Update**:
- Adding new fault injection scenarios (S4, S5, etc.)
- Changing existing scenario behavior
- Adding new test implementations
- Updating observability requirements

**What to Update**:
- Add new scenario section with:
  - **Scenario ID**: `S4` (or next available ID)
  - **Status**: ✅ Implemented / ⚠️ Partial / ❌ Planned
  - **Coverage Matrix**: Link to coverage matrix
  - **Purpose**: What the scenario verifies
  - **Fault Injection**: Type, level, pattern, input data
  - **Expected System Behavior**: What should happen
  - **Expected Observability (OBS)**:
    - Metrics (with labels)
    - Alerts (with scenario IDs and file references)
    - Dashboard panels (with section references)
  - **Test Implementations**: List of tests with file paths and line numbers

**Example**:
```markdown
### Scenario 4: New Fault Type

**Scenario ID**: `S4`  
**Status**: ✅ **Implemented**  
**Coverage Matrix**: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md#s4-new-fault-type`

**Purpose**: Verify Router handles new fault type gracefully.

**Fault Injection**:
- **Type**: New fault type
- **Level**: Application layer
- **Pattern**: Fault pattern description
- **Input Data**: Test data description

**Expected System Behavior**:
- System behavior description

**Expected Observability (OBS)**:
- **Metrics**:
  - `router_new_metric_total` (with labels: `reason`, `source`)
- **Alerts**:
  - `RouterNewMetricHigh` (Scenario ID: NEW-001) - `apps/otp/router/docs/observability/router-alert-rules.yaml:XXX-YYY`
- **Dashboard Panels**:
  - "New Metric Rate" (Section 4.X, Panel Y) - `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`

**Test Implementations**:
1. **E2E Test**: `test_new_fault_type/1`
   - **File**: `apps/otp/router/test/router_jetstream_e2e_SUITE.erl`
   - **Lines**: XXX-YYY
```

#### 3. Alert Rules Scenario IDs

**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml`

**When to Update**:
- Adding new alerts
- Modifying existing alerts
- Changing alert thresholds or conditions

**What to Update**:
- Add scenario ID comment at the top of alert definition:
  ```yaml
  # Scenario ID: S1 (Intermittent ACK/NAK Errors), NATS-004 (NATS ACK Failures)
  # Coverage Analysis: apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md#...
  # Coverage Matrix: apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md#...
  # Fault Injection: apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md#...
  # Dashboard: docs/OBSERVABILITY_ROUTER_DASHBOARD.md#...
  ```
- Add scenario IDs to alert description:
  ```yaml
  annotations:
    description: |
      Alert description...
      Related Scenarios: S1 (Intermittent ACK/NAK Errors), NATS-004 (NATS ACK Failures)
      Coverage Matrix: apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md#nats-004-nats-ack-failures
  ```

#### 4. Dashboard Panel Scenario IDs

**File**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`

**When to Update**:
- Adding new dashboard panels
- Modifying existing panel queries
- Changing panel descriptions

**What to Update**:
- Add scenario IDs to panel description:
  ```markdown
  - **Panel Name**

    **Scenario IDs**: S2 (Processing Delays → Redelivery Growth), JS-001 (High Redelivery Rate)  
    **Coverage Matrix**: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md#s2-processing-delays--redelivery-growth`  
    **Alert**: `RouterJetStreamHighRedeliveryRate` (Scenario ID: JS-001)

    - Query:  
      ```promql
      sum by (assignment_id) (rate(router_jetstream_redelivery_total[5m]))
      ```
  ```

### Validation

**Before submitting PR**, run:

```bash
# Validate metrics labels and coverage
bash apps/otp/router/scripts/validate_metrics_labels.sh

# This checks:
# 1. Helper function exports
# 2. Metric emission with labels
# 3. Dashboard PromQL queries
# 4. Test files exist
# 5. Documentation references
# 6. Scenario-to-test mapping (S1, S2, S3)
# 7. Fault injection test documentation OBS links
```

**Expected Result**: All checks pass (may have warnings for future work items)

### CI Integration

The validation is automatically run in CI via `.github/workflows/router-observability-validation.yml`:

- **Trigger**: On push/PR to observability-related files
- **Checks**:
  - Metrics labels validation
  - Alert rules scenario IDs
  - Coverage matrix completeness
  - Alert rules YAML syntax

**If CI fails**: Fix the issues before requesting review.

---

## Pull Request Process

### PR Checklist

See `docs/dev/PR_CHECKLIST.md` for complete PR checklist.

**Key Requirements**:
1. ✅ All tests pass locally
2. ✅ Documentation updated (if applicable)
3. ✅ Links validated (`bash scripts/check_links.sh`)
4. ✅ Secrets masked (`bash scripts/check_hmac_masking.sh`)
5. ✅ **JetStream/NATS/OBS changes**: Coverage matrix, scenarios, and scenario IDs updated

### PR Description Template

When creating a PR, include:

1. **Summary**: Brief description of changes
2. **Changes**: List of modified files
3. **Testing**: How changes were tested
4. **Documentation**: Documentation updates (if applicable)
5. **JetStream/NATS/OBS Updates** (if applicable):
   - Coverage matrix updated: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md`
   - Scenarios updated: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
   - Alert scenario IDs updated: `apps/otp/router/docs/observability/router-alert-rules.yaml`
   - Dashboard scenario IDs updated: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`
   - Validation passed: `bash apps/otp/router/scripts/validate_metrics_labels.sh`

---

## Code Standards

### Erlang/OTP

- Follow Erlang/OTP coding standards
- Use `dialyzer` for type checking
- Export functions for testing when needed
- Document complex logic with comments

### Testing

- Unit tests for helper functions
- Integration tests for end-to-end scenarios
- Performance tests for critical paths
- Fault injection tests for resilience scenarios

### Documentation

- All code comments in English
- All documentation in English
- Chat communication in Russian (per project rules)
- Update related documentation when changing code

---

## Testing Requirements

### Test Coverage

- **Unit Tests**: Helper functions, utility modules
- **Integration Tests**: Component interactions, end-to-end flows
- **Performance Tests**: Latency, throughput, resource usage
- **Fault Injection Tests**: Resilience, error handling, recovery

### Running Tests

```bash
# Run all Router tests
cd apps/otp/router
rebar3 ct

# Run specific test suite
rebar3 ct --suite router_jetstream_fault_injection_SUITE

# Run specific test
rebar3 ct --suite router_jetstream_e2e_SUITE --case test_intermittent_ack_failure_recovery
```

### Test Documentation

- Link tests to scenarios in `JETSTREAM_FAULT_INJECTION_TESTS.md`
- Update coverage matrix when adding tests
- Document test purpose and expected behavior

---

## References

### Core Documentation

- **Coverage Matrix**: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md`
- **Fault Injection Tests**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- **Testing Guide**: `apps/otp/router/docs/dev/METRICS_LABELS_TESTING_GUIDE.md`
- **OBS Coverage Analysis**: `apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md`
- **Alert Rules**: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- **Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`

### Development Guides

- **Operations Guide**: `docs/OPERATIONS_GUIDE_RU.md`
- **PR Checklist**: `docs/dev/PR_CHECKLIST.md`
- **Architecture**: `docs/BEAMLINE_VISION_AND_ARCHITECTURE.md`

### Validation Scripts

- **Metrics Labels Validation**: `apps/otp/router/scripts/validate_metrics_labels.sh`
- **Schema Changes**: `scripts/check_schema_changes.sh`
- **State Validation**: `scripts/validate_state.sh`
- **Link Check**: `scripts/check_links.sh`

---

**Last Updated**: 2025-01-27  
**Maintained by**: WORKER wrk-9 (Documentation & Developer Experience)

