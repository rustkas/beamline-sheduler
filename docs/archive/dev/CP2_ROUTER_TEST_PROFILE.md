# CP2 Router Test Profile

**Purpose**: Minimal test profile for CP2 Router regression testing  
**Target**: Developers and CI pipelines  
**Time**: ~10-15 minutes (full suite)

## Overview

This document defines the minimal set of test suites that must pass for CP2 Router features. These tests verify that all CP2 features (JetStream, Idempotency, ACL, Circuit Breaker, Observability) are working correctly.

## Test Suites (Must-Run for CP2)

| Test Suite | Purpose | CP2 Features Verified | Must-Run |
|------------|---------|----------------------|----------|
| `router_jetstream_SUITE.erl` | JetStream durability, redelivery, DLQ | JetStream ACK/NAK, MaxDeliver, backoff, DLQ | ✅ **Yes** |
| `router_idem_SUITE.erl` | Idempotency cache, TTL, double-execution prevention | Idempotency hits/misses, zero double-execution, TTL expiration | ✅ **Yes** |
| `router_circuit_breaker_SUITE.erl` | Circuit Breaker state machine | CB state transitions (Closed/Open/Half-Open), failure threshold | ✅ **Yes** |
| `router_circuit_breaker_integration_SUITE.erl` | Circuit Breaker integration with Router | CB integration with policy applier, fallback chains | ✅ **Yes** |
| `router_cp2_features_e2e_SUITE.erl` | **E2E integration of all CP2 features** | JetStream + Idempotency + ACL + CB working together | ✅ **Yes** |
| `router_observability_SUITE.erl` | Observability (logging, OTel spans) | JSON log format, CP1 correlation fields, OTel spans | ✅ **Yes** |
| `router_metrics_dump_SUITE.erl` | Prometheus metrics export | Metrics format, HELP/TYPE headers, metric values | ✅ **Yes** |

## Quick Reference

### Run All CP2 Router Tests

```bash
cd apps/otp/router

# Run all CP2 test suites
rebar3 ct --suite test/router_jetstream_SUITE \
          --suite test/router_idem_SUITE \
          --suite test/router_circuit_breaker_SUITE \
          --suite test/router_circuit_breaker_integration_SUITE \
          --suite test/router_cp2_features_e2e_SUITE \
          --suite test/router_observability_SUITE \
          --suite test/router_metrics_dump_SUITE
```

### Run Individual Test Suite

```bash
# JetStream tests
rebar3 ct --suite test/router_jetstream_SUITE

# Idempotency tests
rebar3 ct --suite test/router_idem_SUITE

# Circuit Breaker tests
rebar3 ct --suite test/router_circuit_breaker_SUITE

# E2E integration tests (all CP2 features)
rebar3 ct --suite test/router_cp2_features_e2e_SUITE
```

## Test Coverage Summary

### JetStream (`router_jetstream_SUITE.erl`)

**Tests**:
- Durable subscription creation and reconnection
- ACK/NAK semantics
- MaxDeliver enforcement
- Backoff schedule application
- DLQ policy (messages sent to DLQ after MaxDeliver)
- DLQ payload contains context (trace_id, tenant_id, error_code)

**Metrics Verified**:
- `router_jetstream_ack_total`
- `router_redelivery_total`
- `router_dlq_total`

### Idempotency (`router_idem_SUITE.erl`)

**Tests**:
- Basic duplicate check (`is_dup/1`)
- Remember and evict (`remember/2`, `evict/1`)
- TTL expiration
- Atomic check-and-mark (`check_and_mark/2`, `check_and_mark/3`)
- **Zero double-execution** (concurrent check_and_mark calls)
- p95 duplicate handling latency
- Keys propagation (Router → Gateway)

**Metrics Verified**:
- `router_idem_hits_total`
- `router_idem_miss_total`

### Circuit Breaker (`router_circuit_breaker_SUITE.erl` + `router_circuit_breaker_integration_SUITE.erl`)

**Tests**:
- State transitions (Closed → Open → Half-Open)
- Failure threshold enforcement
- Timeout handling
- Integration with policy applier
- Fallback chains with CB

**Metrics Verified**:
- `router_circuit_breaker_events_total`
- `router_circuit_breaker_state_transitions_total`

### E2E Integration (`router_cp2_features_e2e_SUITE.erl`)

**Tests**:
- Full flow with all CP2 features (happy path)
- JetStream redelivery with MaxDeliver and DLQ
- ACL deny with proper logging and metrics
- Circuit Breaker state transitions
- Metrics verification for all CP2 features

**Verifications**:
- ✅ No double-execution (idempotency)
- ✅ Redelivery doesn't exceed MaxDeliver and leads to DLQ
- ✅ ACL rejections are logged and counted
- ✅ CB blocks further calls after threshold
- ✅ All CP2 feature metrics are emitted correctly

### Observability (`router_observability_SUITE.erl`)

**Tests**:
- JSON log format validation
- CP1 correlation fields (trace_id, run_id, flow_id, step_id, tenant_id)
- OTel spans for decide and result handling paths
- Trace propagation (headers priority over payload)
- PII filtering

### Metrics (`router_metrics_dump_SUITE.erl`)

**Tests**:
- Prometheus metrics format (text format)
- HELP and TYPE headers
- Metric values correctness
- Metrics endpoint accessibility (`/metrics`)

## CI Integration

### GitHub Actions

The CP2 Router test profile is integrated into:
- `.github/workflows/ci.yml` - Runs all test suites on every PR
- `.github/workflows/router-full-test-suite.yml` - Full test suite for Router

### Local Development

Before committing CP2 Router changes, run:

```bash
cd apps/otp/router
rebar3 ct --suite test/router_cp2_features_e2e_SUITE
```

This single E2E suite verifies all CP2 features working together.

## Success Criteria

All test suites must pass with:
- ✅ Exit code: 0
- ✅ No test failures
- ✅ All metrics verified
- ✅ All CP2 features working together (E2E suite)

## Troubleshooting

### Tests Fail

1. **Check Router is running**: `whereis(router_jetstream)` should return PID
2. **Check ETS tables**: `ets:info(router_idem)` should return table info
3. **Check NATS connection**: Verify NATS is accessible (if using real NATS)
4. **Check logs**: Review `ct_logs/` directory for detailed error messages

### Metrics Not Found

1. **Check metrics endpoint**: `curl http://localhost:9001/metrics`
2. **Verify metrics are registered**: Check `router_prometheus.erl` for metric definitions
3. **Check metrics initialization**: Verify `router_metrics:init()` was called

## References

- `docs/CP2_CHECKLIST.md` - Complete CP2 checklist with implementation status
- `apps/otp/router/test/router_cp2_features_e2e_SUITE.erl` - E2E integration test suite
- `docs/archive/dev/CP2_ROUTER_SANITY_RUN.md` - Manual sanity run guide
- `docs/archive/dev/CP2_ROUTER_COMPLETION_SUMMARY.md` - CP2 Router completion summary

