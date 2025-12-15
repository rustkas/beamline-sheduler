# Rate Limiting Load Tests

**Purpose**: Load testing for Router and Gateway rate limiting functionality

**Reference**: `docs/dev/RATE_LIMITING_LOAD_TEST_PLAN.md`

## Overview

This directory contains load test scripts for validating rate limiting performance, latency, and behavior under various load scenarios.

## Test Scenarios

1. **Single Tenant, Under Limit**: 50 req/s (limit: 100 req/s)
2. **Single Tenant, At Limit**: 100 req/s (limit: 100 req/s)
3. **Single Tenant, Over Limit**: 200 req/s (limit: 100 req/s)
4. **Multiple Tenants**: 10 tenants, 50 req/s each
5. **Traffic Spikes**: Sudden spikes testing burst capacity
6. **Long-Running Exceed**: 5 minutes of sustained over-limit traffic
7. **Gateway + Router Combined**: E2E test with both layers active

## Prerequisites

1. **Gateway Running**:
   ```bash
   cd apps/c-gateway
   ./build/gateway
   ```

2. **Router Running**:
   ```bash
   cd apps/otp/router
   rebar3 shell
   ```

3. **Redis Running** (for Gateway distributed rate limiting):
   ```bash
   docker run -d -p 6379:6379 redis:7-alpine
   ```

4. **Environment Variables**:
   ```bash
   export GATEWAY_URL=http://localhost:8080
   export ROUTER_METRICS_URL=http://localhost:9001/metrics
   export GATEWAY_METRICS_URL=http://localhost:8080/_metrics
   ```

## Running Tests

### Install Dependencies

```bash
cd tests/load/rate-limiting
npm install
```

### Run All Scenarios

```bash
npm run test -- --all
```

### Run Single Scenario

```bash
npm run test -- --scenario scenario1
npm run test -- --scenario scenario3
```

### Run with Custom Config

```bash
npm run test -- --scenario scenario3 --request-rate 200 --duration 300
```

## Test Results

Results are saved to `reports/load-tests/rate-limiting/rate-limiting-load-test-results-{timestamp}.json`

**Format**:
```json
{
  "scenario": "scenario1_single_tenant_under_limit",
  "config": { ... },
  "totalRequests": 3000,
  "allowedRequests": 3000,
  "rejectedRequests": 0,
  "successRate": 1.0,
  "latency": {
    "p50": 1.2,
    "p95": 3.5,
    "p99": 7.8
  },
  "metrics": {
    "routerAllowed": 3000,
    "routerExceeded": 0,
    "gatewayAllowed": 3000,
    "gatewayExceeded": 0
  }
}
```

## Metrics Collection

The test script automatically collects metrics from:
- Router: `http://localhost:9001/metrics`
- Gateway: `http://localhost:8080/_metrics`

Metrics are collected before and after the test to calculate differences.

## Artifacts

The test generates the following artifacts:
- Test results JSON file
- Metrics snapshots (before/after)
- Latency statistics (P50, P95, P99)

## Notes

- This is a **skeleton implementation** - full automation requires additional work
- Some scenarios (e.g., traffic spikes, multiple tenants) require more sophisticated load generation
- Metrics collection requires Prometheus-compatible endpoints
- Results directory is created automatically if it doesn't exist

## Reference

- `docs/dev/RATE_LIMITING_LOAD_TEST_PLAN.md` - Complete test plan
- `docs/dev/RATE_LIMITING_OBSERVABILITY_SPEC.md` - Metrics specification

