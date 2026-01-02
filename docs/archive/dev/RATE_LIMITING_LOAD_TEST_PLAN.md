# Rate Limiting Load Test Plan

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: 📋 **PLAN** (Not yet implemented)  
**Component**: Router + Gateway

## Purpose

This document defines the plan for load testing rate limiting functionality to validate performance, latency, and behavior under various load scenarios.

## Goals and SLO

### Performance Goals

**Latency Under Limit**:
- **P50 Latency**: < 1ms (rate limit check overhead)
- **P95 Latency**: < 5ms (rate limit check overhead)
- **P99 Latency**: < 10ms (rate limit check overhead)

**Throughput**:
- **Router Rate Limit Store**: > 10,000 checks/second
- **Gateway Rate Limit Store**: > 10,000 checks/second
- **Combined (Gateway + Router)**: > 5,000 requests/second

**Behavior Under Constant Exceed**:
- **No Degradation**: Rate limit check latency should not degrade when limits are constantly exceeded
- **Predictable Rejection**: All requests should be rejected consistently (no false positives)
- **Memory Stability**: No memory leaks or unbounded growth

### SLO Targets

**Success Rate**:
- **Under Limit**: 100% success rate (all requests allowed)
- **Over Limit**: 100% rejection rate (all requests rejected correctly)

**Latency Impact**:
- **P95 Impact**: Rate limiting should add < 5ms to P95 latency
- **P99 Impact**: Rate limiting should add < 10ms to P99 latency

**Resource Usage**:
- **CPU**: Rate limiting should use < 5% CPU under normal load
- **Memory**: Rate limiting should use < 100MB memory per Router instance
- **ETS Table Size**: Should not grow unbounded (cleanup working)

## Test Scenarios

### Scenario 1: Single Tenant, Under Limit

**Description**: Single tenant making requests within rate limit.

**Configuration**:
- **Tenant**: 1 tenant
- **Policy**: 1 policy with rate limit: 100 req/s, burst 50
- **Request Rate**: 50 req/s (under limit)
- **Duration**: 60 seconds
- **Total Requests**: 3,000

**Expected Results**:
- ✅ All requests allowed (100% success rate)
- ✅ P95 latency < 5ms
- ✅ No rate limit errors
- ✅ Metrics: `router_rate_limit_allowed_total` = 3,000

**Metrics to Collect**:
- `router_rate_limit_allowed_total{scope="policy", tenant_id="..."}`
- `router_rate_limit_exceeded_total{scope="policy", tenant_id="..."}` (should be 0)
- Request latency (P50, P95, P99)
- CPU usage
- Memory usage
- ETS table size

### Scenario 2: Single Tenant, At Limit

**Description**: Single tenant making requests exactly at rate limit.

**Configuration**:
- **Tenant**: 1 tenant
- **Policy**: 1 policy with rate limit: 100 req/s, burst 50
- **Request Rate**: 100 req/s (at limit)
- **Duration**: 60 seconds
- **Total Requests**: 6,000

**Expected Results**:
- ✅ All requests allowed (100% success rate, sustained rate)
- ✅ P95 latency < 5ms
- ✅ No rate limit errors
- ✅ Metrics: `router_rate_limit_allowed_total` = 6,000

**Metrics to Collect**:
- Same as Scenario 1
- Token bucket refill rate
- Burst capacity utilization

### Scenario 3: Single Tenant, Over Limit (Constant Exceed)

**Description**: Single tenant making requests constantly exceeding rate limit.

**Configuration**:
- **Tenant**: 1 tenant
- **Policy**: 1 policy with rate limit: 100 req/s, burst 50
- **Request Rate**: 200 req/s (2x limit)
- **Duration**: 60 seconds
- **Total Requests**: 12,000

**Expected Results**:
- ✅ ~50% requests allowed (burst + sustained rate)
- ✅ ~50% requests rejected (rate limit exceeded)
- ✅ P95 latency < 10ms (rejection is fast)
- ✅ Metrics: `router_rate_limit_exceeded_total` ≈ 6,000
- ✅ No latency degradation over time

**Metrics to Collect**:
- `router_rate_limit_allowed_total` (should be ~6,000)
- `router_rate_limit_exceeded_total` (should be ~6,000)
- Request latency (P50, P95, P99)
- Rejection latency (should be < 1ms)
- Memory stability (no leaks)

### Scenario 4: Multiple Tenants, Independent Limits

**Description**: Multiple tenants with independent rate limits.

**Configuration**:
- **Tenants**: 10 tenants
- **Policies**: 10 policies (1 per tenant), each with rate limit: 100 req/s, burst 50
- **Request Rate**: 50 req/s per tenant (500 req/s total, under limit)
- **Duration**: 60 seconds
- **Total Requests**: 30,000 (3,000 per tenant)

**Expected Results**:
- ✅ All requests allowed (100% success rate)
- ✅ P95 latency < 5ms
- ✅ No cross-tenant interference
- ✅ Metrics: `router_rate_limit_allowed_total` = 30,000 (10 tenants × 3,000)

**Metrics to Collect**:
- `router_rate_limit_allowed_total{scope="policy", tenant_id="tenant_1"}` through `tenant_10`
- Request latency per tenant
- ETS table size (should be ~10 buckets)
- Scope isolation verification

### Scenario 5: Traffic Spikes (Burst Handling)

**Description**: Sudden traffic spikes testing burst capacity.

**Configuration**:
- **Tenant**: 1 tenant
- **Policy**: 1 policy with rate limit: 100 req/s, burst 50
- **Request Pattern**: 
  - Baseline: 50 req/s for 30 seconds
  - Spike: 200 req/s for 5 seconds (4x baseline)
  - Baseline: 50 req/s for 25 seconds
- **Total Requests**: ~4,000

**Expected Results**:
- ✅ Baseline requests: 100% allowed
- ✅ Spike requests: ~75% allowed (burst + sustained rate)
- ✅ Spike requests: ~25% rejected (after burst exhausted)
- ✅ Recovery: Back to 100% allowed after spike
- ✅ P95 latency < 10ms during spike

**Metrics to Collect**:
- Request success rate over time
- Burst capacity utilization
- Token refill rate
- Recovery time (time to return to 100% success rate)

### Scenario 6: Long-Running Exceed (Sustained Over-Limit)

**Description**: Long-running traffic constantly exceeding rate limit.

**Configuration**:
- **Tenant**: 1 tenant
- **Policy**: 1 policy with rate limit: 100 req/s, burst 50
- **Request Rate**: 200 req/s (2x limit)
- **Duration**: 300 seconds (5 minutes)
- **Total Requests**: 60,000

**Expected Results**:
- ✅ Consistent rejection rate (~50%)
- ✅ No memory leaks or unbounded growth
- ✅ No latency degradation over time
- ✅ ETS table size stable (cleanup working)
- ✅ Metrics: `router_rate_limit_exceeded_total` ≈ 30,000

**Metrics to Collect**:
- Memory usage over time (should be stable)
- ETS table size over time (should be stable)
- CPU usage over time (should be stable)
- Request latency over time (should be stable)
- Cleanup effectiveness (expired buckets removed)

### Scenario 7: Gateway + Router Combined (E2E)

**Description**: Full E2E test with both Gateway and Router rate limiting active.

**Configuration**:
- **Gateway Rate Limit**: 1000 req/min (endpoint-level)
- **Router Rate Limit**: 100 req/s, burst 50 (policy-level)
- **Request Rate**: 150 req/s (over Router limit, under Gateway limit)
- **Duration**: 60 seconds
- **Total Requests**: 9,000

**Expected Results**:
- ✅ Gateway allows all requests (under Gateway limit)
- ✅ Router rejects ~33% requests (over Router limit)
- ✅ No double-dropping
- ✅ Clear error format (Router error, not Gateway error)
- ✅ Metrics: `gateway_rate_limit_allowed_total` = 9,000
- ✅ Metrics: `router_rate_limit_exceeded_total` ≈ 3,000

**Metrics to Collect**:
- `gateway_rate_limit_allowed_total{endpoint="/api/v1/routes/decide"}`
- `gateway_rate_limit_exceeded_total{endpoint="/api/v1/routes/decide"}` (should be 0)
- `router_rate_limit_allowed_total{scope="policy"}`
- `router_rate_limit_exceeded_total{scope="policy"}`
- Request latency (Gateway + Router combined)
- Error format verification (no double-dropping)

## Load Test Implementation

### Test Infrastructure

**Location**: `tests/load/rate-limiting/`

**Test Files**:
- `rate-limiting-load-test.ts` - Main load test script (TypeScript)
- `rate-limiting-load-test.erl` - Erlang load test suite (alternative)

**Dependencies**:
- Gateway (C-Gateway) running on `http://localhost:8081` (or `GATEWAY_URL`)
- Router running and accessible via Gateway
- Redis running (for Gateway distributed rate limiting)
- Prometheus (optional, for metrics collection)

### Test Script Structure

**Skeleton**: `tests/load/rate-limiting/rate-limiting-load-test.ts`

```typescript
/**
 * Rate Limiting Load Test
 * 
 * Tests rate limiting performance under various load scenarios.
 */

import axios from 'axios';

interface LoadTestConfig {
  scenario: string;
  tenantId: string;
  policyId: string;
  requestRate: number; // requests per second
  duration: number; // seconds
  gatewayUrl: string;
}

interface LoadTestResult {
  scenario: string;
  totalRequests: number;
  allowedRequests: number;
  rejectedRequests: number;
  successRate: number;
  latency: {
    p50: number;
    p95: number;
    p99: number;
  };
  metrics: {
    routerAllowed: number;
    routerExceeded: number;
    gatewayAllowed: number;
    gatewayExceeded: number;
  };
}

async function runLoadTest(config: LoadTestConfig): Promise<LoadTestResult> {
  // Implementation skeleton
  // 1. Generate requests at specified rate
  // 2. Collect responses (allowed/rejected)
  // 3. Measure latency
  // 4. Collect metrics
  // 5. Return results
}

// Scenario implementations
async function scenario1_SingleTenantUnderLimit() { /* ... */ }
async function scenario2_SingleTenantAtLimit() { /* ... */ }
async function scenario3_SingleTenantOverLimit() { /* ... */ }
async function scenario4_MultipleTenants() { /* ... */ }
async function scenario5_TrafficSpikes() { /* ... */ }
async function scenario6_LongRunningExceed() { /* ... */ }
async function scenario7_GatewayRouterCombined() { /* ... */ }

// Main execution
async function main() {
  const results = await Promise.all([
    scenario1_SingleTenantUnderLimit(),
    scenario2_SingleTenantAtLimit(),
    scenario3_SingleTenantOverLimit(),
    scenario4_MultipleTenants(),
    scenario5_TrafficSpikes(),
    scenario6_LongRunningExceed(),
    scenario7_GatewayRouterCombined(),
  ]);
  
  // Save results
  saveResults(results, 'rate-limiting-load-test-results.json');
}

main().catch(console.error);
```

## Metrics Collection

### Required Metrics

**Router Metrics** (from Prometheus `/metrics` endpoint):
- `router_rate_limit_allowed_total{scope, tenant_id}`
- `router_rate_limit_exceeded_total{scope, tenant_id}`
- `router_rate_limit_current{scope, tenant_id}` (if available)
- `router_rate_limit_limit{scope, tenant_id}` (if available)
- `router_rate_limit_burst{scope, tenant_id}` (if available)

**Gateway Metrics** (from Prometheus `/metrics` endpoint):
- `gateway_rate_limit_hits_total{endpoint, tenant_id}`
- `gateway_rate_limit_allowed_total{endpoint, tenant_id}`
- `gateway_rate_limit_exceeded_total{endpoint, tenant_id}`
- `gateway_rate_limit_redis_errors_total{endpoint}` (if Redis enabled)

**System Metrics**:
- Request latency (P50, P95, P99)
- CPU usage
- Memory usage
- ETS table size (Router)
- Redis memory usage (Gateway, if Redis enabled)

### Metrics Collection Methods

**Method 1: Prometheus Scraping**
- Scrape metrics during load test
- Export to Prometheus format
- Analyze with Grafana

**Method 2: Direct HTTP Queries**
- Query `/metrics` endpoint before/after test
- Calculate differences
- Store in test results

**Method 3: Telemetry Events**
- Subscribe to telemetry events during test
- Aggregate metrics in real-time
- Store in test results

## Artifacts to Save

### 1. Prometheus Metrics Dump

**Format**: Prometheus text format  
**Location**: `reports/load-tests/rate-limiting/{scenario}-metrics.prom`  
**Content**: All metrics at test start and end

**Collection**:
```bash
# Before test
curl http://router:9001/metrics > reports/load-tests/rate-limiting/scenario1-metrics-before.prom

# After test
curl http://router:9001/metrics > reports/load-tests/rate-limiting/scenario1-metrics-after.prom
```

### 2. Log Slices

**Format**: JSON logs  
**Location**: `reports/load-tests/rate-limiting/{scenario}-logs.jsonl`  
**Content**: Rate limiting logs during test period

**Collection**:
```bash
# Collect logs during test (10 seconds before to 10 seconds after)
journalctl -u router --since "10 seconds ago" --until "10 seconds from now" \
  | jq 'select(.module == "router_rate_limit_store")' \
  > reports/load-tests/rate-limiting/scenario1-logs.jsonl
```

### 3. Test Results

**Format**: JSON  
**Location**: `reports/load-tests/rate-limiting/{scenario}-results.json`  
**Content**: Test execution results, metrics, latency statistics

**Structure**:
```json
{
  "scenario": "scenario1_single_tenant_under_limit",
  "config": {
    "tenantId": "test-tenant-1",
    "policyId": "default",
    "requestRate": 50,
    "duration": 60
  },
  "results": {
    "totalRequests": 3000,
    "allowedRequests": 3000,
    "rejectedRequests": 0,
    "successRate": 1.0,
    "latency": {
      "p50": 1.2,
      "p95": 3.5,
      "p99": 7.8
    }
  },
  "metrics": {
    "routerAllowed": 3000,
    "routerExceeded": 0,
    "gatewayAllowed": 3000,
    "gatewayExceeded": 0
  },
  "system": {
    "cpuUsage": 2.5,
    "memoryUsage": 85.3,
    "etsTableSize": 1
  }
}
```

### 4. Performance Graphs

**Format**: PNG/SVG  
**Location**: `reports/load-tests/rate-limiting/{scenario}-graphs/`  
**Content**: Latency over time, success rate over time, metrics over time

**Generation**: Use Grafana or custom plotting scripts

## Test Execution

### Prerequisites

1. **Gateway Running**:
   ```bash
   # Start Gateway
   cd apps/c-gateway
   ./build/gateway
   ```

2. **Router Running**:
   ```bash
   # Start Router
   cd apps/otp/router
   rebar3 shell
   ```

3. **Redis Running** (for Gateway distributed rate limiting):
   ```bash
   docker run -d -p 6379:6379 redis:7-alpine
   ```

4. **Test Environment**:
   ```bash
   export GATEWAY_URL=http://localhost:8081
   export ROUTER_METRICS_URL=http://localhost:9001/metrics
   export GATEWAY_METRICS_URL=http://localhost:8081/_metrics
   ```

### Running Tests

**Single Scenario**:
```bash
cd tests/load/rate-limiting
npm install
npm run test -- --scenario scenario1
```

**All Scenarios**:
```bash
cd tests/load/rate-limiting
npm run test -- --all
```

**With Custom Config**:
```bash
cd tests/load/rate-limiting
npm run test -- --scenario scenario3 --request-rate 200 --duration 300
```

## Success Criteria

### Performance Criteria

- ✅ **Latency**: P95 < 5ms (under limit), P95 < 10ms (over limit)
- ✅ **Throughput**: > 10,000 checks/second (Router), > 10,000 checks/second (Gateway)
- ✅ **Success Rate**: 100% under limit, ~50% over limit (expected)
- ✅ **Memory**: No leaks, stable memory usage
- ✅ **CPU**: < 5% CPU usage under normal load

### Correctness Criteria

- ✅ **No False Positives**: All requests under limit should be allowed
- ✅ **No False Negatives**: All requests over limit should be rejected
- ✅ **No Double-Dropping**: Requests should not be blocked by both Gateway and Router
- ✅ **Scope Isolation**: Multiple tenants should not interfere with each other
- ✅ **Burst Handling**: Burst capacity should work correctly

## References

- `docs/archive/dev/RATE_LIMITING_OBSERVABILITY_SPEC.md` - Metrics specification
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - General metrics specification
- `tests/load/README.md` - Existing load test infrastructure
- `docs/archive/dev/POLICY_PERFORMANCE_PLAN.md` - Policy performance plan (reference)

