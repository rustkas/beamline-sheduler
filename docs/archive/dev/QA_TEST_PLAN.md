# QA Test Plan

Test coverage and behavior requirements for local development and CI dry-run.

## Coverage
- Unit tests: business logic, utilities
- Integration tests: Gateway  Router, NATS subjects, gRPC interactions
- E2E tests: UI through Gateway to services

## Mock Mode
- Deterministic responses for providers
- No outbound calls to real services
- Enable via config/env in test profiles

## Exceptions
- Secret detection and PII checks run even in mock mode
- Validators mask sensitive output

## Reports
- Test reports saved in `reports/tests/`
- Coverage summary in `reports/tests/coverage.txt`
- Generated via `scripts/run_tests.ps1` / `.sh`

## Dry-run CI
- Integrates tests with local gates
- Fails PR if coverage below threshold or gates fail

## R10 E2E Test Suite

### Overview

R10 E2E tests (`router_publish_failure_e2e_SUITE`) validate retry logic and circuit breaker behavior under various failure scenarios.

### Test Scenarios

1. **Mass Failure → Breaker Open**: Validates circuit breaker opens after consecutive failures
2. **Recovery After Failure**: Validates circuit breaker transitions from open → half_open → closed
3. **Latency-Based Trigger**: Validates circuit breaker opens due to latency threshold
4. **Error Rate / Partial Failure**: Validates circuit breaker opens due to error rate threshold
5. **Thundering Herd Recovery**: Validates recovery behavior under concurrent load
6. **Deadline vs SLA**: Validates latency compliance under retry scenarios

### Test Configuration

**CI Profiles** (defined in `test/ct.config`):
- **`ci` profile** (default): 10 clients × 20 requests = 200 publishes (fast, CI-friendly)
- **`heavy` profile**: 50 clients × 100 requests = 5000 publishes (comprehensive, nightly)

**Canonical Commands**:

```bash
# Unit tests (circuit breaker logic)
rebar3 ct --suite test/router_circuit_breaker_SUITE

# E2E tests - CI profile (default)
rebar3 ct --suite test/router_publish_failure_e2e_SUITE

# E2E tests - Heavy profile (nightly)
R10_PROFILE=heavy rebar3 ct --suite test/router_publish_failure_e2e_SUITE

# All router tests
rebar3 ct --dir apps/otp/router/test
```

**Note**: Combined execution of both suites in a single command is not currently standardized. Use separate commands or `--dir` approach.

### Metric Access Layer

**CRITICAL**: All R10 metric reading must use `router_r10_metrics` module. Tests should **never** access ETS directly.

**Key Functions** (from `router_r10_metrics`):
- `get_metric_value/2` - Read any metric value by name and labels
- `get_latest_trigger_reason/2` - Get latest trigger reason for tenant/provider
- `wait_for_trigger_reason/4` - Wait for trigger reason to appear with timeout (recommended)
- `assert_trigger_reason_in/3` - Assert trigger reason is in allowed list (instant check)
- `get_publish_attempts_total/0` - Get total publish attempts
- `get_publish_errors_total/0` - Get total publish errors
- `dump_metrics/0` - Dump all metrics for debugging

**Trigger Reason Checks** (P2.2):
- **Recommended**: Use `router_r10_metrics:wait_for_trigger_reason/4` for waiting with timeout
- **Alternative**: Use `router_r10_metrics:assert_trigger_reason_in/3` for instant checks (after state transition)
- Accept multiple valid reasons (e.g., `failure_threshold_exceeded` OR `error_rate_threshold_exceeded`)
- All trigger reason values use constants from `router_r10_metrics:trigger_reason_*()`

**Example (Recommended Pattern)**:
```erlang
%% Wait for trigger reason with timeout (P2.2)
case router_r10_metrics:wait_for_trigger_reason(TenantId, ProviderId, [
    router_r10_metrics:trigger_reason_failure_threshold(),
    router_r10_metrics:trigger_reason_error_rate()
], 3000) of
    ok -> ok;
    {error, Error} ->
        _ = router_test_utils:dump_metrics(),
        ct:fail(Error)
end.
```

**Helper Module Separation** (P2.1):
- **`router_test_utils`**: Lifecycle, waiters (start_router_app, ensure_circuit_breaker_alive, reset_circuit_breaker, wait_for_breaker_state)
- **`router_r10_metrics`**: All metric reading and assertions (single source of truth)

### Test Independence

Each E2E scenario uses **unique tenant/provider IDs** to ensure complete independence:
- `scenario_mass_failure_opens_breaker`: `tenant_r10_s1` / `provider_r10_s1`
- `scenario_recovery_after_failure`: `tenant_r10_s2` / `provider_r10_s2`
- `scenario_latency_based_trigger`: `tenant_r10_s3` / `provider_r10_s3`
- etc.

### Timeouts

- **State transitions**: 3-5 seconds
- **Metric checks**: 2-3 seconds
- **Order**: Always check state first, then metrics

## Next Steps
- Add property-based tests for Router decisions
- Expand coverage for SSE and sticky sessions
- Include error normalization tests in Gateway
