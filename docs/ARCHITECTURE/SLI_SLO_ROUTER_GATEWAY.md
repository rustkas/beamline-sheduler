# Service Level Indicators (SLI) and Service Level Objectives (SLO) for Router Intake and Gateway

**Date**: 2025-11-26  
**Status**: 📋 **Production Readiness Specification**  
**Purpose**: Define formal SLO/SLI for Router intake and Gateway for CP3+/Release readiness  
**Related**: `docs/ARCHITECTURE/router-intake-backpressure-policy.md`, `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`

## Executive Summary

This document defines formal Service Level Indicators (SLI) and Service Level Objectives (SLO) for Router intake and Gateway components. These SLO/SLI are used for:
- **Production Readiness**: Formal acceptance criteria for release
- **Pre-Release Gates**: Automated checks before release
- **Monitoring**: Continuous monitoring of service health
- **Error Budget Management**: Tracking and managing error budgets

## SLI Definitions

### Router Intake SLI

#### SLI 1: Intake Success Rate

**Definition**: Percentage of messages that reach business logic without rejection.

**Formula**:
```
SLI_intake_success = (messages_processed_successfully) / (total_messages_received)
```

**Metrics**:
- Numerator: `router_intake_messages_total{status="ok"}`
- Denominator: `router_intake_messages_total` (all statuses)

**Prometheus Query**:
```promql
# Intake success rate (last 1 hour)
sum(rate(router_intake_messages_total{status="ok"}[1h])) 
/ 
sum(rate(router_intake_messages_total[1h]))
```

**Measurement Window**: Rolling 1-hour window (configurable: 5m, 1h, 24h, 30d)

**Labels**:
- `subject`: NATS subject (e.g., `beamline.router.v1.decide`)
- `tenant_id`: Tenant ID (optional, for per-tenant SLI)

#### SLI 2: Intake Error Rate (4xx/5xx/429)

**Definition**: Percentage of messages that fail validation or processing.

**Formula**:
```
SLI_intake_error = (messages_failed) / (total_messages_received)
```

**Metrics**:
- Numerator: `router_intake_messages_total{status="failed"}`
- Denominator: `router_intake_messages_total`

**Error Categories**:
- **4xx Errors**: Validation errors (`SCHEMA_VALIDATION_FAILED`, `VERSION_UNSUPPORTED`, `CORRELATION_FIELDS_INVALID`, `TENANT_FORBIDDEN`)
- **5xx Errors**: Internal errors (`INTERNAL_VALIDATION_ERROR`, processing failures)
- **429 Errors**: Rate limiting (Gateway layer, not Router intake)

**Prometheus Query**:
```promql
# Intake error rate (last 1 hour)
sum(rate(router_intake_messages_total{status="failed"}[1h])) 
/ 
sum(rate(router_intake_messages_total[1h]))
```

**Error Budget**:
- **Error Budget**: 1 - SLO (e.g., if SLO = 99.9%, error budget = 0.1%)
- **Error Budget Consumption**: Track error rate over time
- **Error Budget Exhaustion**: Alert when error budget consumed

#### SLI 3: Intake Processing Latency

**Definition**: End-to-end latency from message receipt to business logic processing.

**Formula**:
```
SLI_intake_latency_pXX = percentile(processing_latency, XX)
```

**Metrics**:
- `router_intake_processing_latency_seconds` (histogram) - Processing latency per message
- `router_intake_processing_latency_p50` (gauge) - 50th percentile latency
- `router_intake_processing_latency_p95` (gauge) - 95th percentile latency
- `router_intake_processing_latency_p99` (gauge) - 99th percentile latency

**Prometheus Query**:
```promql
# P95 latency (last 1 hour)
histogram_quantile(0.95, 
  sum(rate(router_intake_processing_latency_seconds_bucket[1h])) by (le, subject)
)

# P99 latency (last 1 hour)
histogram_quantile(0.99, 
  sum(rate(router_intake_processing_latency_seconds_bucket[1h])) by (le, subject)
)
```

**Measurement Window**: Rolling 1-hour window

**Labels**:
- `subject`: NATS subject
- `status`: Message status (`ok`, `failed`)

#### SLI 4: DLQ Publication Success Rate

**Definition**: Percentage of failed messages successfully published to DLQ.

**Formula**:
```
SLI_dlq_success = (dlq_messages_published) / (dlq_messages_attempted)
```

**Metrics**:
- Numerator: `router_intake_dlq_messages_total`
- Denominator: `router_intake_dlq_messages_total + router_intake_dlq_publish_failed_total`

**Prometheus Query**:
```promql
# DLQ publication success rate (last 1 hour)
sum(rate(router_intake_dlq_messages_total[1h])) 
/ 
(sum(rate(router_intake_dlq_messages_total[1h])) + sum(rate(router_intake_dlq_publish_failed_total[1h])))
```

**Target**: 100% (all failed messages must be published to DLQ)

### Gateway SLI

#### SLI 5: Gateway HTTP Success Rate

**Definition**: Percentage of HTTP requests that return 2xx status codes.

**Formula**:
```
SLI_gateway_success = (http_requests_2xx) / (total_http_requests)
```

**Metrics**:
- Numerator: `gateway_http_requests_total{status=~"2.."}`
- Denominator: `gateway_http_requests_total`

**Prometheus Query**:
```promql
# Gateway HTTP success rate (last 1 hour)
sum(rate(gateway_http_requests_total{status=~"2.."}[1h])) 
/ 
sum(rate(gateway_http_requests_total[1h]))
```

**Labels**:
- `method`: HTTP method (GET, POST, etc.)
- `path`: HTTP path (e.g., `/api/v1/routes/decide`)
- `status`: HTTP status code

#### SLI 6: Gateway Error Rate (4xx/5xx/429)

**Definition**: Percentage of HTTP requests that return error status codes.

**Formula**:
```
SLI_gateway_error = (http_requests_4xx_5xx_429) / (total_http_requests)
```

**Error Categories**:
- **4xx Errors**: Client errors (400, 401, 404, etc.)
- **5xx Errors**: Server errors (500, 502, 503, etc.)
- **429 Errors**: Rate limiting (Too Many Requests)

**Prometheus Query**:
```promql
# Gateway error rate (last 1 hour)
sum(rate(gateway_http_requests_total{status=~"4..|5.."}[1h])) 
/ 
sum(rate(gateway_http_requests_total[1h]))

# Gateway rate limiting (429) rate
sum(rate(gateway_http_requests_total{status="429"}[1h])) 
/ 
sum(rate(gateway_http_requests_total[1h]))
```

**Error Budget**:
- **Error Budget**: 1 - SLO (e.g., if SLO = 99.5%, error budget = 0.5%)
- **429 Errors**: Separate budget for rate limiting (acceptable under load)

#### SLI 7: Gateway End-to-End Latency (HTTP → NATS → Router → HTTP)

**Definition**: End-to-end latency from HTTP request to HTTP response.

**Formula**:
```
SLI_gateway_latency_pXX = percentile(http_request_duration, XX)
```

**Metrics**:
- `gateway_http_request_duration_seconds` (histogram) - HTTP request latency
- `gateway_http_request_duration_p50` (gauge) - 50th percentile latency
- `gateway_http_request_duration_p95` (gauge) - 95th percentile latency
- `gateway_http_request_duration_p99` (gauge) - 99th percentile latency

**Prometheus Query**:
```promql
# Gateway P95 latency (last 1 hour)
histogram_quantile(0.95, 
  sum(rate(gateway_http_request_duration_seconds_bucket[1h])) by (le, method, path)
)

# Gateway P99 latency (last 1 hour)
histogram_quantile(0.99, 
  sum(rate(gateway_http_request_duration_seconds_bucket[1h])) by (le, method, path)
)
```

**Measurement Window**: Rolling 1-hour window

**Labels**:
- `method`: HTTP method
- `path`: HTTP path
- `status`: HTTP status code

#### SLI 8: Gateway Rate Limiting Hit Rate

**Definition**: Percentage of requests that hit rate limits (429 responses).

**Formula**:
```
SLI_gateway_rate_limit = (rate_limit_exceeded) / (total_rate_limit_checks)
```

**Metrics**:
- Numerator: `gateway_rate_limit_exceeded_total`
- Denominator: `gateway_rate_limit_hits_total`

**Prometheus Query**:
```promql
# Gateway rate limit hit rate (last 1 hour)
sum(rate(gateway_rate_limit_exceeded_total[1h])) 
/ 
sum(rate(gateway_rate_limit_hits_total[1h]))
```

**Note**: This SLI measures rate limiting effectiveness, not service health. High rate limit hit rate may indicate:
- Legitimate traffic spike (expected)
- Abuse/attack (investigate)
- Too strict limits (adjust configuration)

## SLO Definitions

### Router Intake SLO

#### SLO 1: Intake Success Rate

**Target**: 99.9% of messages processed successfully

**SLO Definition**:
```
SLI_intake_success >= 0.999 (99.9%)
```

**Measurement Window**: Rolling 30-day window

**Error Budget**: 0.1% (1 message per 1000 can fail)

**Alert Thresholds**:
- **Warning**: SLI < 0.999 for 1 hour
- **Critical**: SLI < 0.995 for 15 minutes

**Prometheus Alert**:
```yaml
- alert: RouterIntakeSuccessRateLow
  expr: |
    (sum(rate(router_intake_messages_total{status="ok"}[1h])) 
     / 
     sum(rate(router_intake_messages_total[1h]))) < 0.999
  for: 1h
  labels:
    severity: warning
    service: router
  annotations:
    summary: "Router intake success rate below SLO"
    description: "Intake success rate is {{ $value | humanizePercentage }}, target is 99.9%"
```

#### SLO 2: Intake Error Rate

**Target**: < 0.1% error rate (4xx/5xx)

**SLO Definition**:
```
SLI_intake_error < 0.001 (0.1%)
```

**Measurement Window**: Rolling 30-day window

**Error Budget**: 0.1% (1 error per 1000 messages)

**Alert Thresholds**:
- **Warning**: Error rate > 0.1% for 1 hour
- **Critical**: Error rate > 0.5% for 15 minutes

**Prometheus Alert**:
```yaml
- alert: RouterIntakeErrorRateHigh
  expr: |
    (sum(rate(router_intake_messages_total{status="failed"}[1h])) 
     / 
     sum(rate(router_intake_messages_total[1h]))) > 0.001
  for: 1h
  labels:
    severity: warning
    service: router
  annotations:
    summary: "Router intake error rate above SLO"
    description: "Intake error rate is {{ $value | humanizePercentage }}, target is < 0.1%"
```

#### SLO 3: Intake Processing Latency

**Target**: P95 latency < 500ms for 99% of messages

**SLO Definition**:
```
P95(processing_latency) < 500ms for 99% of time
```

**Measurement Window**: Rolling 1-hour window (evaluated every 5 minutes)

**Alert Thresholds**:
- **Warning**: P95 latency > 500ms for 5 minutes
- **Critical**: P95 latency > 2000ms for 1 minute

**Prometheus Alert**:
```yaml
- alert: RouterIntakeLatencyHigh
  expr: |
    histogram_quantile(0.95, 
      sum(rate(router_intake_processing_latency_seconds_bucket[5m])) by (le, subject)
    ) > 0.5
  for: 5m
  labels:
    severity: warning
    service: router
  annotations:
    summary: "Router intake P95 latency above SLO"
    description: "P95 latency is {{ $value | humanizeDuration }}, target is < 500ms"
```

**Additional Targets**:
- **P50**: < 100ms for 99% of time
- **P99**: < 2000ms for 99% of time

#### SLO 4: DLQ Publication Success Rate

**Target**: 100% of failed messages published to DLQ

**SLO Definition**:
```
SLI_dlq_success = 1.0 (100%)
```

**Measurement Window**: Rolling 30-day window

**Error Budget**: 0% (zero tolerance for DLQ publication failures)

**Alert Thresholds**:
- **Critical**: DLQ publication failure > 0 for any duration

**Prometheus Alert**:
```yaml
- alert: RouterIntakeDLQPublicationFailed
  expr: |
    sum(rate(router_intake_dlq_publish_failed_total[5m])) > 0
  for: 1m
  labels:
    severity: critical
    service: router
  annotations:
    summary: "Router intake DLQ publication failures detected"
    description: "{{ $value }} DLQ publication failures in last 5 minutes"
```

### Gateway SLO

#### SLO 5: Gateway HTTP Success Rate

**Target**: 99.5% of HTTP requests return 2xx

**SLO Definition**:
```
SLI_gateway_success >= 0.995 (99.5%)
```

**Measurement Window**: Rolling 30-day window

**Error Budget**: 0.5% (5 errors per 1000 requests)

**Alert Thresholds**:
- **Warning**: Success rate < 99.5% for 1 hour
- **Critical**: Success rate < 99.0% for 15 minutes

**Prometheus Alert**:
```yaml
- alert: GatewayHTTPSuccessRateLow
  expr: |
    (sum(rate(gateway_http_requests_total{status=~"2.."}[1h])) 
     / 
     sum(rate(gateway_http_requests_total[1h]))) < 0.995
  for: 1h
  labels:
    severity: warning
    service: gateway
  annotations:
    summary: "Gateway HTTP success rate below SLO"
    description: "HTTP success rate is {{ $value | humanizePercentage }}, target is 99.5%"
```

#### SLO 6: Gateway Error Rate

**Target**: < 0.5% error rate (4xx/5xx, excluding 429)

**SLO Definition**:
```
SLI_gateway_error < 0.005 (0.5%)
```

**Measurement Window**: Rolling 30-day window

**Error Budget**: 0.5% (5 errors per 1000 requests)

**Note**: 429 (rate limiting) errors are excluded from error budget (expected under load)

**Alert Thresholds**:
- **Warning**: Error rate > 0.5% for 1 hour
- **Critical**: Error rate > 1.0% for 15 minutes

**Prometheus Alert**:
```yaml
- alert: GatewayHTTPErrorRateHigh
  expr: |
    (sum(rate(gateway_http_requests_total{status=~"4..|5..",status!="429"}[1h])) 
     / 
     sum(rate(gateway_http_requests_total[1h]))) > 0.005
  for: 1h
  labels:
    severity: warning
    service: gateway
  annotations:
    summary: "Gateway HTTP error rate above SLO"
    description: "HTTP error rate is {{ $value | humanizePercentage }}, target is < 0.5%"
```

#### SLO 7: Gateway End-to-End Latency

**Target**: P95 latency < 1000ms for 99% of requests

**SLO Definition**:
```
P95(http_request_duration) < 1000ms for 99% of time
```

**Measurement Window**: Rolling 1-hour window (evaluated every 5 minutes)

**Alert Thresholds**:
- **Warning**: P95 latency > 1000ms for 5 minutes
- **Critical**: P95 latency > 5000ms for 1 minute

**Prometheus Alert**:
```yaml
- alert: GatewayHTTPLatencyHigh
  expr: |
    histogram_quantile(0.95, 
      sum(rate(gateway_http_request_duration_seconds_bucket[5m])) by (le, method, path)
    ) > 1.0
  for: 5m
  labels:
    severity: warning
    service: gateway
  annotations:
    summary: "Gateway HTTP P95 latency above SLO"
    description: "P95 latency is {{ $value | humanizeDuration }}, target is < 1000ms"
```

**Additional Targets**:
- **P50**: < 200ms for 99% of time
- **P99**: < 5000ms for 99% of time

#### SLO 8: Gateway Rate Limiting

**Target**: Rate limiting hit rate < 5% under normal load

**SLO Definition**:
```
SLI_gateway_rate_limit < 0.05 (5%)
```

**Measurement Window**: Rolling 1-hour window

**Note**: This SLO measures rate limiting effectiveness, not service health. High rate limit hit rate may indicate:
- Legitimate traffic spike (expected, adjust limits)
- Abuse/attack (investigate)
- Too strict limits (adjust configuration)

**Alert Thresholds**:
- **Warning**: Rate limit hit rate > 5% for 1 hour
- **Info**: Rate limit hit rate > 10% for 15 minutes (investigate)

**Prometheus Alert**:
```yaml
- alert: GatewayRateLimitHitRateHigh
  expr: |
    (sum(rate(gateway_rate_limit_exceeded_total[1h])) 
     / 
     sum(rate(gateway_rate_limit_hits_total[1h]))) > 0.05
  for: 1h
  labels:
    severity: warning
    service: gateway
  annotations:
    summary: "Gateway rate limit hit rate above threshold"
    description: "Rate limit hit rate is {{ $value | humanizePercentage }}, investigate traffic patterns"
```

## SLO Summary Table

| SLI | Component | Target | Measurement Window | Error Budget |
|-----|-----------|--------|-------------------|--------------|
| **Intake Success Rate** | Router | 99.9% | 30 days | 0.1% |
| **Intake Error Rate** | Router | < 0.1% | 30 days | 0.1% |
| **Intake Latency (P95)** | Router | < 500ms | 1 hour | 1% of time |
| **DLQ Publication Success** | Router | 100% | 30 days | 0% |
| **HTTP Success Rate** | Gateway | 99.5% | 30 days | 0.5% |
| **HTTP Error Rate** | Gateway | < 0.5% | 30 days | 0.5% |
| **HTTP Latency (P95)** | Gateway | < 1000ms | 1 hour | 1% of time |
| **Rate Limit Hit Rate** | Gateway | < 5% | 1 hour | N/A (info only) |

## Test Coverage for SLO/SLI

### Existing Tests

#### Load Tests

**Test Suite**: `router_intake_e2e_SUITE.erl` (load_tests group)

**Tests**:
- `test_load_decide_success_flood` - High-volume success flood
- `test_load_decide_error_flood` - High-volume error flood
- `test_load_decide_mixed_stream` - Mixed success/error stream
- `test_load_decide_idempotency_stress` - Idempotency stress test

**SLO Coverage**:
- ✅ **Intake Success Rate**: Verified in `test_load_decide_success_flood`
- ✅ **Intake Error Rate**: Verified in `test_load_decide_error_flood`
- ✅ **Intake Latency**: Latency tracked in load tests
- ✅ **DLQ Publication**: Verified in error flood tests

**Metrics Verified**:
- `router_intake_messages_total{status="ok"}` ≈ N
- `router_intake_messages_total{status="failed"}` ≈ 0 (for success flood)
- `router_intake_validation_errors_total` = 0 (for success flood)
- `router_intake_dlq_messages_total` = N (for error flood)

#### Chaos Tests

**Test Suite**: `router_intake_chaos_SUITE.erl`

**Tests**:
- `test_chaos_nats_connection_loss` - NATS connection loss
- `test_chaos_nats_restart` - NATS server restart
- `test_chaos_nats_flapping` - NATS flapping (frequent restarts)
- `test_chaos_nats_partial_failure` - Partial NATS failure
- `test_chaos_nats_recovery` - NATS recovery after failure

**SLO Coverage**:
- ✅ **Intake Success Rate**: Verified during chaos scenarios
- ✅ **Intake Error Rate**: Verified during chaos scenarios
- ✅ **DLQ Publication**: Verified during NATS failures

**Metrics Verified**:
- `router_nats_connection_errors_total` - Connection errors
- `router_nats_reconnect_total` - Reconnection events
- `router_intake_dlq_publish_failed_total` - DLQ failures during NATS outages

#### E2E Tests

**Test Suite**: `router_intake_e2e_SUITE.erl` (e2e_tests group)

**Tests**:
- `test_e2e_decide_validation_success` - Successful validation
- `test_e2e_decide_validation_schema_error` - Schema validation errors
- `test_e2e_decide_validation_version_error` - Version validation errors
- `test_e2e_decide_validation_correlation_error` - Correlation field errors
- `test_e2e_decide_validation_tenant_error` - Tenant validation errors
- `test_e2e_decide_validation_dlq_publication` - DLQ publication
- `test_e2e_decide_validation_audit_logging` - Audit logging
- `test_e2e_decide_validation_metrics` - Metrics verification

**SLO Coverage**:
- ✅ **Intake Success Rate**: Verified in success tests
- ✅ **Intake Error Rate**: Verified in error tests
- ✅ **DLQ Publication**: Verified in DLQ tests

#### Overload Tests

**Test Suite**: `router_intake_overload_SUITE.erl`

**Tests**:
- `test_overload_jetstream_backlog` - JetStream backlog overload
- `test_overload_processing_latency` - Processing latency overload
- `test_overload_inflight_messages` - In-flight messages overload
- `test_overload_combined` - Combined overload scenarios
- `test_overload_recovery` - Recovery after overload

**SLO Coverage**:
- ✅ **Intake Latency**: Verified in latency overload tests
- ✅ **Queue Depth**: Verified in backlog overload tests
- ✅ **Backpressure**: Verified in combined overload tests

### Gateway Tests

#### Rate Limiting Tests

**Test Suite**: `tests/integration/gateway-rate-limiting.test.ts`

**Tests**:
- Under-limit requests
- At-limit requests
- Over-limit requests (429)
- Window reset
- Multi-tenant isolation

**SLO Coverage**:
- ✅ **Rate Limit Hit Rate**: Verified in rate limiting tests
- ✅ **HTTP Success Rate**: Verified in under-limit tests
- ✅ **HTTP Error Rate**: Verified in over-limit tests

#### Integration Tests

**Test Suite**: `tests/integration/gateway-router-error-handling.test.ts`

**Tests**:
- Router intake errors → HTTP 4xx/5xx
- Gateway rate limiting → HTTP 429
- Error response format
- Error code mapping

**SLO Coverage**:
- ✅ **HTTP Success Rate**: Verified in success tests
- ✅ **HTTP Error Rate**: Verified in error tests
- ✅ **HTTP Latency**: Latency tracked in integration tests

## Pre-Release Gates

**CI/CD Integration**: SLO verification gates are automatically run in GitHub Actions workflow (`.github/workflows/slo-verification.yml`).

**Modes**:
- **Advisory Mode** (default for PRs): Warning if gates fail, but does not block merge
- **Blocking Mode** (for releases): Blocks release if gates fail

**See**: `docs/RELEASE_PROCESS.md` for complete release process and SLO gate requirements.

### Gate 1: SLO Verification Tests

**Purpose**: Verify SLO targets are met before release

**Tests**:
1. **Load Test Suite** (`router_intake_e2e_SUITE.erl` - load_tests):
   - Run `test_load_decide_success_flood` with 10,000 messages
   - Verify: Success rate ≥ 99.9%
   - Verify: Error rate < 0.1%
   - Verify: P95 latency < 500ms
   - Verify: DLQ publication success = 100%

2. **Chaos Test Suite** (`router_intake_chaos_SUITE.erl`):
   - Run all chaos tests
   - Verify: Success rate ≥ 99.9% (excluding NATS outages)
   - Verify: DLQ publication success = 100% (when NATS available)

3. **Overload Test Suite** (`router_intake_overload_SUITE.erl`):
   - Run all overload tests
   - Verify: Backpressure triggers correctly
   - Verify: Latency recovers after overload

**Script**: `scripts/run_router_slo_verification.sh`

**Exit Codes**:
- `0`: All SLO targets met
- `1`: SLO targets not met (block release)
- `2`: Tests failed (block release)

### Gate 2: Metrics Verification

**Purpose**: Verify metrics are correctly emitted and queryable

**Checks**:
1. **Router Metrics**:
   ```promql
   # Verify metrics exist
   router_intake_messages_total
   router_intake_processing_latency_seconds
   router_intake_dlq_messages_total
   ```

2. **Gateway Metrics**:
   ```promql
   # Verify metrics exist
   gateway_http_requests_total
   gateway_http_request_duration_seconds
   gateway_rate_limit_exceeded_total
   ```

3. **SLI Queries**:
   - Run all SLI Prometheus queries
   - Verify queries return valid results
   - Verify SLI values are within expected ranges

**Script**: `scripts/verify_slo_metrics.sh`

**Exit Codes**:
- `0`: All metrics verified
- `1`: Metrics missing or invalid (block release)
- `2`: SLI queries failed (block release)

### Gate 3: Alert Rules Verification

**Purpose**: Verify Prometheus alert rules are correctly configured

**Checks**:
1. **Alert Rules Syntax**:
   - Validate Prometheus alert rule YAML syntax
   - Verify all alert expressions are valid PromQL

2. **Alert Thresholds**:
   - Verify alert thresholds match SLO targets
   - Verify alert labels and annotations are correct

3. **Alert Testing**:
   - Test alert firing (if possible in test environment)
   - Verify alert notifications work

**Script**: `scripts/verify_slo_alerts.sh`

**Exit Codes**:
- `0`: All alerts verified
- `1`: Alert rules invalid (block release)
- `2`: Alert thresholds incorrect (block release)

### Gate 4: Documentation Verification

**Purpose**: Verify SLO/SLI documentation is complete and accurate

**Checks**:
1. **SLO/SLI Definitions**:
   - Verify all SLI formulas are correct
   - Verify all SLO targets are defined
   - Verify measurement windows are specified

2. **Test Coverage**:
   - Verify test coverage for each SLO is documented
   - Verify pre-release gates are documented

3. **Metrics Catalog**:
   - Verify all metrics used in SLI are in metrics catalog
   - Verify metric labels match SLI queries

**Script**: `scripts/verify_slo_docs.sh`

**Exit Codes**:
- `0`: Documentation complete
- `1`: Documentation incomplete (block release)
- `2`: Documentation inaccurate (block release)

## When SLO Gate Should Be Green

### Requirements for SLO Gate to Pass

**All gates must pass** for SLO verification to be green:

1. **Gate 1: SLO Verification Tests** ✅
   - Load tests pass (10,000 messages, success rate ≥ 99.9%)
   - Chaos tests pass (success rate ≥ 99.9% excluding NATS outages)
   - Overload tests pass (backpressure triggers correctly)

2. **Gate 2: Metrics Verification** ✅
   - All Router metrics exist and are queryable
   - All Gateway metrics exist and are queryable (if applicable)
   - All SLI queries return valid results

3. **Gate 3: Alert Rules Verification** ✅
   - Alert rules syntax valid (Prometheus YAML)
   - Alert thresholds match SLO targets
   - Alert labels and annotations correct

4. **Gate 4: Documentation Verification** ✅
   - SLO/SLI definitions complete
   - Test coverage documented
   - Pre-release gates documented

### CI/CD Integration

**GitHub Actions Workflow**: `.github/workflows/slo-verification.yml`

**Modes**:
- **Advisory Mode** (default for PRs): Warning if gates fail, but does not block merge
- **Blocking Mode** (for releases): Blocks release if gates fail

**Triggers**:
- Pull requests (advisory mode)
- Pushes to main/develop (blocking mode)
- Manual dispatch (configurable mode)

**Artifacts**:
- SLO verification summary JSON (`slo-verification-results/summary.json`)
- Test logs and results
- Uploaded to GitHub Actions artifacts (30-day retention)

### What to Do If SLO Gate Is Red

**Step 1: Review SLO Verification Summary**

Check GitHub Actions artifacts for detailed results:
```bash
# Download artifacts from GitHub Actions
# View summary.json for gate results
cat slo-verification-results/summary.json
```

**Step 2: Identify Failed Gates**

Determine which gates failed:
- **Tests Failed**: Check test logs for specific failures
- **Metrics Missing**: Check Prometheus availability and metric names
- **Alerts Invalid**: Check alert rule syntax and thresholds
- **Docs Incomplete**: Check SLO/SLI documentation completeness

**Step 3: Fix Issues**

**If Tests Failed**:
1. Review test failures in logs
2. Check SLO targets (may need adjustment)
3. Fix test configuration or implementation
4. Re-run SLO verification workflow

**If Metrics Missing**:
1. Verify Prometheus is running and accessible
2. Check metric names match SLI queries
3. Verify metrics are emitted during tests
4. Add missing metrics if needed

**If Alerts Invalid**:
1. Fix alert rule YAML syntax
2. Adjust thresholds to match SLO targets
3. Verify alert labels and annotations
4. Test alert rules with promtool

**If Docs Incomplete**:
1. Complete SLO/SLI definitions
2. Document test coverage for each SLO
3. Update pre-release gates documentation
4. Verify metrics catalog includes all SLI metrics

**Step 4: Re-run Verification**

After fixes, re-run SLO verification:
```bash
# Manual re-run via GitHub Actions
# Or locally:
bash scripts/run_router_slo_verification.sh
bash scripts/verify_slo_metrics.sh
bash scripts/verify_slo_alerts.sh
bash scripts/verify_slo_docs.sh
```

**Step 5: Consume Error Budget (If Applicable)**

If SLO targets cannot be met, document error budget consumption:

**Error Budget Calculation**:
```
Error Budget = (1 - SLO) × Measurement Window
Error Budget Consumed = (1 - SLI) × Measurement Window
```

**Example**:
- SLO: 99.9% success rate (30-day window)
- SLI: 99.8% success rate (30-day window)
- Error Budget: 0.1% × 30 days = 43.2 minutes
- Error Budget Consumed: 0.2% × 30 days = 86.4 minutes
- **Result**: Error budget exceeded, document in release notes

**Step 6: Postpone Release (If Blocking)**

If SLO gates fail in blocking mode:
1. **Do not merge/release** until all gates pass
2. **Document issues** in release notes or PR comments
3. **Plan remediation** for next release cycle
4. **Consider emergency release** only if critical security fix (with documented error budget consumption)

**Emergency Release Exception**:
- For critical security fixes, emergency release may proceed with:
  - Documented error budget consumption
  - Plan for SLO remediation in next release
  - Approval from release manager

## Acceptance Criteria for Release

### Router Intake

**Must Pass**:
- ✅ **SLO 1**: Intake success rate ≥ 99.9% (30-day window)
- ✅ **SLO 2**: Intake error rate < 0.1% (30-day window)
- ✅ **SLO 3**: P95 latency < 500ms for 99% of time (1-hour window)
- ✅ **SLO 4**: DLQ publication success = 100% (30-day window)

**Test Verification**:
- ✅ Load tests pass (10,000 messages, success rate ≥ 99.9%)
- ✅ Chaos tests pass (success rate ≥ 99.9% excluding NATS outages)
- ✅ Overload tests pass (backpressure triggers correctly)
- ✅ E2E tests pass (all validation scenarios)

**Metrics Verification**:
- ✅ All SLI metrics exist and are queryable
- ✅ SLI queries return valid results
- ✅ SLI values within expected ranges

### Gateway

**Must Pass**:
- ✅ **SLO 5**: HTTP success rate ≥ 99.5% (30-day window)
- ✅ **SLO 6**: HTTP error rate < 0.5% (30-day window, excluding 429)
- ✅ **SLO 7**: P95 latency < 1000ms for 99% of time (1-hour window)
- ✅ **SLO 8**: Rate limit hit rate < 5% under normal load (1-hour window)

**Test Verification**:
- ✅ Rate limiting tests pass (under-limit, at-limit, over-limit)
- ✅ Integration tests pass (error handling, latency)
- ✅ Abuse tests pass (abuse scenarios detected)

**Metrics Verification**:
- ✅ All SLI metrics exist and are queryable
- ✅ SLI queries return valid results
- ✅ SLI values within expected ranges

## Error Budget Management

### Error Budget Calculation

**Formula**:
```
Error Budget = (1 - SLO) × Measurement Window
```

**Example** (Intake Success Rate SLO = 99.9%, 30-day window):
```
Error Budget = (1 - 0.999) × 30 days = 0.001 × 30 = 0.03 days = 43.2 minutes
```

**Error Budget Consumption**:
```
Error Budget Consumed = (1 - SLI) × Measurement Window
```

**Example** (SLI = 99.8%, 30-day window):
```
Error Budget Consumed = (1 - 0.998) × 30 days = 0.002 × 30 = 0.06 days = 86.4 minutes
```

### Error Budget Alerts

**Warning**: Error budget 50% consumed
```yaml
- alert: RouterIntakeErrorBudgetWarning
  expr: |
    ((1 - (sum(rate(router_intake_messages_total{status="ok"}[30d])) 
           / sum(rate(router_intake_messages_total[30d])))) 
     * 30 * 24 * 60) > (0.001 * 30 * 24 * 60 * 0.5)
  for: 1h
  labels:
    severity: warning
    service: router
  annotations:
    summary: "Router intake error budget 50% consumed"
    description: "Error budget consumption: {{ $value | humanizeDuration }}"
```

**Critical**: Error budget 80% consumed
```yaml
- alert: RouterIntakeErrorBudgetCritical
  expr: |
    ((1 - (sum(rate(router_intake_messages_total{status="ok"}[30d])) 
           / sum(rate(router_intake_messages_total[30d])))) 
     * 30 * 24 * 60) > (0.001 * 30 * 24 * 60 * 0.8)
  for: 1h
  labels:
    severity: critical
    service: router
  annotations:
    summary: "Router intake error budget 80% consumed"
    description: "Error budget consumption: {{ $value | humanizeDuration }}"
```

## Monitoring Dashboard

### Router Intake Dashboard

**Panels**:
1. **Intake Success Rate** (SLI 1):
   - Query: `sum(rate(router_intake_messages_total{status="ok"}[1h])) / sum(rate(router_intake_messages_total[1h]))`
   - Target: 99.9% (red line)
   - Warning: 99.5% (yellow line)

2. **Intake Error Rate** (SLI 2):
   - Query: `sum(rate(router_intake_messages_total{status="failed"}[1h])) / sum(rate(router_intake_messages_total[1h]))`
   - Target: < 0.1% (green line)
   - Warning: > 0.1% (yellow line)
   - Critical: > 0.5% (red line)

3. **Intake Latency (P95)** (SLI 3):
   - Query: `histogram_quantile(0.95, sum(rate(router_intake_processing_latency_seconds_bucket[1h])) by (le, subject))`
   - Target: < 500ms (green line)
   - Warning: > 500ms (yellow line)
   - Critical: > 2000ms (red line)

4. **DLQ Publication Success Rate** (SLI 4):
   - Query: `sum(rate(router_intake_dlq_messages_total[1h])) / (sum(rate(router_intake_dlq_messages_total[1h])) + sum(rate(router_intake_dlq_publish_failed_total[1h])))`
   - Target: 100% (green line)
   - Critical: < 100% (red line)

5. **Error Budget Consumption**:
   - Query: `((1 - (sum(rate(router_intake_messages_total{status="ok"}[30d])) / sum(rate(router_intake_messages_total[30d])))) * 30 * 24 * 60)`
   - Warning: 50% consumed (yellow line)
   - Critical: 80% consumed (red line)

### Gateway Dashboard

**Panels**:
1. **HTTP Success Rate** (SLI 5):
   - Query: `sum(rate(gateway_http_requests_total{status=~"2.."}[1h])) / sum(rate(gateway_http_requests_total[1h]))`
   - Target: 99.5% (red line)
   - Warning: 99.0% (yellow line)

2. **HTTP Error Rate** (SLI 6):
   - Query: `sum(rate(gateway_http_requests_total{status=~"4..|5..",status!="429"}[1h])) / sum(rate(gateway_http_requests_total[1h]))`
   - Target: < 0.5% (green line)
   - Warning: > 0.5% (yellow line)
   - Critical: > 1.0% (red line)

3. **HTTP Latency (P95)** (SLI 7):
   - Query: `histogram_quantile(0.95, sum(rate(gateway_http_request_duration_seconds_bucket[1h])) by (le, method, path))`
   - Target: < 1000ms (green line)
   - Warning: > 1000ms (yellow line)
   - Critical: > 5000ms (red line)

4. **Rate Limit Hit Rate** (SLI 8):
   - Query: `sum(rate(gateway_rate_limit_exceeded_total[1h])) / sum(rate(gateway_rate_limit_hits_total[1h]))`
   - Target: < 5% (green line)
   - Warning: > 5% (yellow line)
   - Info: > 10% (blue line)

## References

- **Backpressure Policy**: `docs/ARCHITECTURE/router-intake-backpressure-policy.md`
- **Metrics Monitoring Guide**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`
- **Metrics Catalog**: `config/observability/metrics.catalog.yaml`
- **Prometheus Alerts**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md`
- **Router Intake Runbook**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
- **Gateway Rate Limiting Runbook**: `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`

