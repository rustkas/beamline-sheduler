# CP2 Observability Wave 1 Dry-Run Plan

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Worker**: wrk-obs1 (Observability CP2)  
**Status**: 📋 **DRY-RUN PLAN** (CP2)

---

## Executive Summary

This document describes a **dry-run scenario** for Wave 1 Observability (Prometheus metrics) implementation. The dry-run allows "rehearsing" Wave 1 at the infrastructure and configuration level **without code changes**, validating readiness and identifying potential issues before actual development begins.

**Key Principle**: Dry-run validates that Prometheus infrastructure, metrics endpoints, and validation scripts are ready for Wave 1 implementation.

**Reference**: `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md` - Wave 1 specification

**CI Integration**: `docs/archive/dev/CP2_WAVE1_CI_INTEGRATION_PLAN.md` - CI integration plan for dry-run steps

---

## Dry-Run Objectives

### Goals

1. **Validate Prometheus Infrastructure**: Verify that Prometheus can be started from Docker Compose and is accessible
2. **Validate Metrics Endpoints**: Verify that metrics endpoints are accessible (even if empty) on correct ports
3. **Validate Scrape Configuration**: Verify that Prometheus can scrape metrics from all components
4. **Validate CP1 Profile**: Verify that CP1 profile tests can run and generate metrics
5. **Validate Metrics Validation Script**: Verify that metrics validation script can check for required time-series
6. **Identify Blockers**: Identify any missing infrastructure or configuration issues

### Success Criteria

- ✅ Prometheus starts successfully from Docker Compose
- ✅ All metrics endpoints are accessible (Router: 9001, Gateway: 3001, Worker: 9092)
- ✅ Prometheus can scrape metrics from all components
- ✅ CP1 profile tests run successfully
- ✅ Metrics validation script can check for required time-series
- ✅ No blockers identified for Wave 1 implementation

---

## Infrastructure Setup

### 1. Prometheus from Docker Compose

#### 1.1. Start Prometheus

**Location**: `tools/observability/docker-compose.observability.yml`

**Start Command**:
```bash
cd tools/observability
docker-compose -f docker-compose.observability.yml up -d prometheus
```

**Expected Output**:
```
Creating beamline-prometheus ... done
```

**Verify Prometheus is Running**:
```bash
docker ps | grep prometheus
# Expected: beamline-prometheus container running
```

**Check Prometheus Health**:
```bash
curl -s http://localhost:9090/-/healthy
# Expected: Prometheus is Healthy.
```

**Check Prometheus UI**:
```bash
# Open in browser: http://localhost:9090
# Or check via curl:
curl -s http://localhost:9090/api/v1/status/config | jq .
# Expected: Prometheus configuration JSON
```

**Dry-Run Validation**:
- ✅ Docker Compose file exists
- ✅ Prometheus container starts successfully
- ✅ Prometheus health endpoint returns "Prometheus is Healthy."
- ✅ Prometheus UI is accessible on port 9090
- ✅ Prometheus API is accessible

#### 1.2. Verify Prometheus Configuration

**Configuration File**: `tools/observability/prometheus.yml`

**Check Configuration**:
```bash
cd tools/observability
cat prometheus.yml
```

**Expected Configuration** (for Wave 1):
```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s
  external_labels:
    cluster: 'local'
    environment: 'development'

scrape_configs:
  # Router (Erlang/OTP) - Port 9001
  - job_name: 'router'
    static_configs:
      - targets: ['host.docker.internal:9001']
        labels:
          component: 'router'
          language: 'erlang'
    metrics_path: '/metrics'

  # Gateway (NestJS) - Port 3001
  - job_name: 'gateway'
    static_configs:
      - targets: ['host.docker.internal:3001']
        labels:
          component: 'gateway'
          language: 'typescript'
    metrics_path: '/metrics'

  # Worker (C++ CAF) - Port 9092
  - job_name: 'worker'
    static_configs:
      - targets: ['host.docker.internal:9092']
        labels:
          component: 'worker'
          language: 'cpp'
    metrics_path: '/metrics'

  # Prometheus itself
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']
```

**Dry-Run Validation**:
- ✅ Prometheus configuration file exists
- ✅ Configuration includes Router (port 9001), Gateway (port 3001), Worker (port 9092)
- ✅ Configuration uses correct metrics paths (`/metrics`)
- ✅ Configuration uses correct host (`host.docker.internal` for Docker)

**Reload Prometheus Configuration** (if needed):
```bash
# Reload configuration via API
curl -X POST http://localhost:9090/-/reload
# Expected: Configuration reloaded
```

---

### 2. Metrics Endpoints Verification

#### 2.1. Router Metrics Endpoint

**Endpoint**: `http://localhost:9001/metrics`

**Check Endpoint Accessibility**:
```bash
curl -s http://localhost:9001/metrics
# Expected: Prometheus metrics in text format (may be empty or minimal without implementation)
```

**Expected Response** (without implementation):
```
# Empty or minimal metrics (e.g., only Go/Erlang runtime metrics)
# Or 404/connection refused if endpoint not implemented
```

**Expected Response** (with Wave 1 implementation):
```
# HELP router_decisions_total Total routing decisions
# TYPE router_decisions_total counter
router_decisions_total{policy_id="default",provider="openai",decision_reason="cost",tenant_id="tenant_123",run_id="run_abc"} 42

# HELP router_decision_duration_seconds Routing decision duration
# TYPE router_decision_duration_seconds histogram
router_decision_duration_seconds_bucket{policy_id="default",provider="openai",tenant_id="tenant_123",run_id="run_abc",le="0.1"} 10
router_decision_duration_seconds_bucket{policy_id="default",provider="openai",tenant_id="tenant_123",run_id="run_abc",le="0.5"} 35
router_decision_duration_seconds_bucket{policy_id="default",provider="openai",tenant_id="tenant_123",run_id="run_abc",le="1.0"} 40
router_decision_duration_seconds_bucket{policy_id="default",provider="openai",tenant_id="tenant_123",run_id="run_abc",le="+Inf"} 42
router_decision_duration_seconds_sum{policy_id="default",provider="openai",tenant_id="tenant_123",run_id="run_abc"} 8.5
router_decision_duration_seconds_count{policy_id="default",provider="openai",tenant_id="tenant_123",run_id="run_abc"} 42
```

**Dry-Run Validation**:
- ✅ Endpoint is accessible (even if returns 404/empty)
- ✅ Port 9001 is not in use by other services
- ✅ Endpoint path is `/metrics`
- ✅ Response format is Prometheus text format (if implemented)

#### 2.2. Gateway Metrics Endpoint

**Endpoint**: `http://localhost:3001/metrics`

**Check Endpoint Accessibility**:
```bash
curl -s http://localhost:3001/metrics
# Expected: Prometheus metrics in text format (may be empty or minimal without implementation)
```

**Expected Response** (without implementation):
```
# Empty or minimal metrics (e.g., only Node.js runtime metrics)
# Or 404/connection refused if endpoint not implemented
```

**Expected Response** (with Wave 1 implementation):
```
# HELP gateway_http_requests_total Total HTTP requests
# TYPE gateway_http_requests_total counter
gateway_http_requests_total{method="POST",route="/api/v1/messages",status_code="200",tenant_id="tenant_123"} 100

# HELP gateway_http_request_duration_seconds HTTP request duration
# TYPE gateway_http_request_duration_seconds histogram
gateway_http_request_duration_seconds_bucket{method="POST",route="/api/v1/messages",status_code="200",tenant_id="tenant_123",le="0.1"} 50
gateway_http_request_duration_seconds_bucket{method="POST",route="/api/v1/messages",status_code="200",tenant_id="tenant_123",le="0.5"} 90
gateway_http_request_duration_seconds_bucket{method="POST",route="/api/v1/messages",status_code="200",tenant_id="tenant_123",le="1.0"} 100
gateway_http_request_duration_seconds_bucket{method="POST",route="/api/v1/messages",status_code="200",tenant_id="tenant_123",le="+Inf"} 100
gateway_http_request_duration_seconds_sum{method="POST",route="/api/v1/messages",status_code="200",tenant_id="tenant_123"} 25.5
gateway_http_request_duration_seconds_count{method="POST",route="/api/v1/messages",status_code="200",tenant_id="tenant_123"} 100
```

**Dry-Run Validation**:
- ✅ Endpoint is accessible (even if returns 404/empty)
- ✅ Port 3001 is not in use by other services (Gateway API is on 3000)
- ✅ Endpoint path is `/metrics`
- ✅ Response format is Prometheus text format (if implemented)

#### 2.3. Worker Metrics Endpoint

**Endpoint**: `http://localhost:9092/metrics`

**Check Endpoint Accessibility**:
```bash
curl -s http://localhost:9092/metrics
# Expected: Prometheus metrics in text format (may be empty or minimal without implementation)
```

**Expected Response** (without implementation):
```
# Empty or minimal metrics (e.g., only C++ runtime metrics)
# Or 404/connection refused if endpoint not implemented
```

**Expected Response** (with Wave 1 implementation):
```
# HELP worker_step_executions_total Total step executions
# TYPE worker_step_executions_total counter
worker_step_executions_total{step_type="http",execution_status="success",tenant_id="tenant_123",run_id="run_abc",flow_id="flow_xyz",step_id="step_001"} 50

# HELP worker_step_execution_duration_seconds Step execution duration
# TYPE worker_step_execution_duration_seconds histogram
worker_step_execution_duration_seconds_bucket{step_type="http",execution_status="success",tenant_id="tenant_123",run_id="run_abc",flow_id="flow_xyz",step_id="step_001",le="0.1"} 20
worker_step_execution_duration_seconds_bucket{step_type="http",execution_status="success",tenant_id="tenant_123",run_id="run_abc",flow_id="flow_xyz",step_id="step_001",le="0.5"} 45
worker_step_execution_duration_seconds_bucket{step_type="http",execution_status="success",tenant_id="tenant_123",run_id="run_abc",flow_id="flow_xyz",step_id="step_001",le="1.0"} 50
worker_step_execution_duration_seconds_bucket{step_type="http",execution_status="success",tenant_id="tenant_123",run_id="run_abc",flow_id="flow_xyz",step_id="step_001",le="+Inf"} 50
worker_step_execution_duration_seconds_sum{step_type="http",execution_status="success",tenant_id="tenant_123",run_id="run_abc",flow_id="flow_xyz",step_id="step_001"} 12.5
worker_step_execution_duration_seconds_count{step_type="http",execution_status="success",tenant_id="tenant_123",run_id="run_abc",flow_id="flow_xyz",step_id="step_001"} 50
```

**Dry-Run Validation**:
- ✅ Endpoint is accessible (even if returns 404/empty)
- ✅ Port 9092 is not in use by other services (Worker health is on 9091)
- ✅ Endpoint path is `/metrics`
- ✅ Response format is Prometheus text format (if implemented)

---

### 3. Prometheus Scrape Verification

#### 3.1. Check Prometheus Targets

**Check Scrape Targets**:
```bash
curl -s http://localhost:9090/api/v1/targets | jq '.data.activeTargets[] | {job: .labels.job, health: .health, lastError: .lastError}'
```

**Expected Output** (without implementation):
```json
{
  "job": "router",
  "health": "down",
  "lastError": "connection refused"
}
{
  "job": "gateway",
  "health": "down",
  "lastError": "connection refused"
}
{
  "job": "worker",
  "health": "down",
  "lastError": "connection refused"
}
{
  "job": "prometheus",
  "health": "up",
  "lastError": ""
}
```

**Expected Output** (with Wave 1 implementation):
```json
{
  "job": "router",
  "health": "up",
  "lastError": ""
}
{
  "job": "gateway",
  "health": "up",
  "lastError": ""
}
{
  "job": "worker",
  "health": "up",
  "lastError": ""
}
{
  "job": "prometheus",
  "health": "up",
  "lastError": ""
}
```

**Dry-Run Validation**:
- ✅ Prometheus can query targets API
- ✅ All targets are configured (router, gateway, worker, prometheus)
- ⚠️ Targets may be "down" without implementation (expected)
- ✅ Prometheus target is "up" (self-monitoring works)

#### 3.2. Check Prometheus Scrape Status

**Check Scrape Status**:
```bash
curl -s http://localhost:9090/api/v1/status/config | jq '.data.yaml' | grep -A 20 'scrape_configs'
```

**Expected Output**:
```yaml
scrape_configs:
  - job_name: router
    static_configs:
      - targets:
          - host.docker.internal:9001
  - job_name: gateway
    static_configs:
      - targets:
          - host.docker.internal:3001
  - job_name: worker
    static_configs:
      - targets:
          - host.docker.internal:9092
```

**Dry-Run Validation**:
- ✅ Prometheus configuration is loaded correctly
- ✅ All scrape targets are configured
- ✅ Ports match Wave 1 specification (Router: 9001, Gateway: 3001, Worker: 9092)

---

## CP1 Profile Execution

### 1. Run CP1 Profile Tests

**Script**: `scripts/observability/run_cp1_profile.sh`

**Purpose**: Run CP1 profile tests to generate metrics (if metrics are implemented)

**Run Command**:
```bash
cd scripts/observability
bash run_cp1_profile.sh
```

**Expected Output** (without implementation):
```
[INFO] Running CP1 profile tests...
[INFO] Starting Router...
[INFO] Starting Gateway...
[INFO] Starting Worker...
[INFO] Running test scenarios...
[INFO] Tests completed
[WARN] Metrics endpoints may not be available (implementation pending)
```

**Expected Output** (with Wave 1 implementation):
```
[INFO] Running CP1 profile tests...
[INFO] Starting Router...
[INFO] Starting Gateway...
[INFO] Starting Worker...
[INFO] Running test scenarios...
[INFO] Tests completed
[INFO] Metrics endpoints available:
  - Router: http://localhost:9001/metrics
  - Gateway: http://localhost:3001/metrics
  - Worker: http://localhost:9092/metrics
```

**Dry-Run Validation**:
- ✅ CP1 profile script exists and can be run
- ✅ Script starts all components (Router, Gateway, Worker)
- ✅ Script runs test scenarios
- ✅ Script completes successfully
- ⚠️ Metrics may not be available without implementation (expected)

---

### 2. Metrics Validation Script

**Script**: `scripts/observability/validate_metrics.sh` (to be created)

**Purpose**: Validate that required metrics are exported and scraped by Prometheus

**Script Content** (example):
```bash
#!/bin/bash
# Validate Prometheus metrics for Wave 1

set -euo pipefail

PROMETHEUS_URL="${PROMETHEUS_URL:-http://localhost:9090}"
REQUIRED_METRICS=(
  "router_decisions_total"
  "router_decision_duration_seconds"
  "router_provider_selections_total"
  "router_errors_total"
  "gateway_http_requests_total"
  "gateway_http_request_duration_seconds"
  "gateway_rate_limit_hits_total"
  "gateway_idempotency_hits_total"
  "worker_step_executions_total"
  "worker_step_execution_duration_seconds"
  "worker_step_errors_total"
  "worker_queue_depth"
)

echo "[INFO] Validating Prometheus metrics..."
echo "[INFO] Prometheus URL: $PROMETHEUS_URL"

# Check Prometheus is accessible
if ! curl -s "$PROMETHEUS_URL/-/healthy" > /dev/null; then
  echo "[ERROR] Prometheus is not accessible at $PROMETHEUS_URL"
  exit 1
fi

# Check each required metric
MISSING_METRICS=()
for metric in "${REQUIRED_METRICS[@]}"; do
  QUERY="count($metric)"
  RESULT=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=$QUERY" | jq -r '.data.result[0].value[1]')
  
  if [ "$RESULT" = "null" ] || [ "$RESULT" = "0" ]; then
    echo "[WARN] Metric not found or empty: $metric"
    MISSING_METRICS+=("$metric")
  else
    echo "[OK] Metric found: $metric (series count: $RESULT)"
  fi
done

# Report results
if [ ${#MISSING_METRICS[@]} -eq 0 ]; then
  echo "[OK] All required metrics are present"
  exit 0
else
  echo "[WARN] Missing metrics: ${MISSING_METRICS[*]}"
  echo "[INFO] This is expected if Wave 1 implementation is not complete"
  exit 0  # Don't fail dry-run if metrics are missing
fi
```

**Run Command**:
```bash
cd scripts/observability
bash validate_metrics.sh
```

**Expected Output** (without implementation):
```
[INFO] Validating Prometheus metrics...
[INFO] Prometheus URL: http://localhost:9090
[WARN] Metric not found or empty: router_decisions_total
[WARN] Metric not found or empty: router_decision_duration_seconds
...
[WARN] Missing metrics: router_decisions_total router_decision_duration_seconds ...
[INFO] This is expected if Wave 1 implementation is not complete
```

**Expected Output** (with Wave 1 implementation):
```
[INFO] Validating Prometheus metrics...
[INFO] Prometheus URL: http://localhost:9090
[OK] Metric found: router_decisions_total (series count: 42)
[OK] Metric found: router_decision_duration_seconds (series count: 42)
...
[OK] All required metrics are present
```

**Dry-Run Validation**:
- ✅ Validation script can be created
- ✅ Script can query Prometheus API
- ✅ Script checks for required metrics
- ✅ Script provides clear output
- ⚠️ Script may report missing metrics without implementation (expected)

---

## Required Time-Series (Wave 1)

### 1. Router Metrics (Required)

**Time-Series That Must Appear** (when Wave 1 is implemented):

1. **`router_decisions_total`** (Counter)
   - Labels: `policy_id`, `provider`, `decision_reason`, `tenant_id`, `run_id`
   - Purpose: Track total routing decisions

2. **`router_decision_duration_seconds`** (Histogram)
   - Labels: `policy_id`, `provider`, `tenant_id`, `run_id`
   - Purpose: Track routing decision latency (p50/p95/p99)

3. **`router_provider_selections_total`** (Counter)
   - Labels: `provider`, `policy_id`, `tenant_id`, `run_id`
   - Purpose: Track provider selection patterns

4. **`router_errors_total`** (Counter)
   - Labels: `error_code`, `tenant_id`, `run_id`
   - Purpose: Track routing errors

**Dry-Run Validation**:
- ✅ Metric names are defined
- ✅ Metric labels are specified
- ✅ Metric types are correct (Counter/Histogram)
- ✅ Metrics can be queried (even if empty)

**Prometheus Query Examples**:
```promql
# Total routing decisions
sum(rate(router_decisions_total[5m]))

# Routing decision latency (p95)
histogram_quantile(0.95, rate(router_decision_duration_seconds_bucket[5m]))

# Provider selection distribution
sum by (provider) (router_provider_selections_total)

# Error rate
sum(rate(router_errors_total[5m]))
```

---

### 2. Gateway Metrics (Required)

**Time-Series That Must Appear** (when Wave 1 is implemented):

1. **`gateway_http_requests_total`** (Counter)
   - Labels: `method`, `route`, `status_code`, `tenant_id`
   - Purpose: Track total HTTP requests

2. **`gateway_http_request_duration_seconds`** (Histogram)
   - Labels: `method`, `route`, `status_code`, `tenant_id`
   - Purpose: Track HTTP request latency (p50/p95/p99)

3. **`gateway_rate_limit_hits_total`** (Counter)
   - Labels: `tenant_id`, `endpoint`
   - Purpose: Track rate limit hits

4. **`gateway_idempotency_hits_total`** (Counter)
   - Labels: None
   - Purpose: Track idempotency cache hits

**Dry-Run Validation**:
- ✅ Metric names are defined
- ✅ Metric labels are specified
- ✅ Metric types are correct (Counter/Histogram)
- ✅ Metrics can be queried (even if empty)

**Prometheus Query Examples**:
```promql
# Total HTTP requests
sum(rate(gateway_http_requests_total[5m]))

# HTTP request latency (p95)
histogram_quantile(0.95, rate(gateway_http_request_duration_seconds_bucket[5m]))

# Rate limit hit rate
sum(rate(gateway_rate_limit_hits_total[5m]))

# Idempotency hit rate
sum(rate(gateway_idempotency_hits_total[5m]))
```

---

### 3. Worker Metrics (Required)

**Time-Series That Must Appear** (when Wave 1 is implemented):

1. **`worker_step_executions_total`** (Counter)
   - Labels: `step_type`, `execution_status`, `tenant_id`, `run_id`, `flow_id`, `step_id`
   - Purpose: Track total step executions

2. **`worker_step_execution_duration_seconds`** (Histogram)
   - Labels: `step_type`, `execution_status`, `tenant_id`, `run_id`, `flow_id`, `step_id`
   - Purpose: Track step execution latency (p50/p95/p99)

3. **`worker_step_errors_total`** (Counter)
   - Labels: `step_type`, `error_code`, `tenant_id`, `run_id`, `flow_id`, `step_id`
   - Purpose: Track step execution errors

4. **`worker_queue_depth`** (Gauge)
   - Labels: `resource_pool`
   - Purpose: Track queue depth per resource pool

**Dry-Run Validation**:
- ✅ Metric names are defined
- ✅ Metric labels are specified
- ✅ Metric types are correct (Counter/Histogram/Gauge)
- ✅ Metrics can be queried (even if empty)

**Prometheus Query Examples**:
```promql
# Total step executions
sum(rate(worker_step_executions_total[5m]))

# Step execution latency (p95)
histogram_quantile(0.95, rate(worker_step_execution_duration_seconds_bucket[5m]))

# Error rate
sum(rate(worker_step_errors_total[5m]))

# Queue depth
sum(worker_queue_depth)
```

---

## Alerts (Wave 2, Not Wave 1)

### Alerts That Are NOT Included in Wave 1

**Wave 1 Focus**: Metrics export only. Alerting is deferred to Wave 2.

**Alerts Deferred to Wave 2**:
- ❌ High error rate alerts
- ❌ High latency alerts
- ❌ Queue depth alerts
- ❌ Component health alerts
- ❌ Rate limit alerts

**Rationale**:
- Wave 1 focuses on metrics export infrastructure
- Alerting requires Alertmanager setup (Wave 2)
- Alert rules require metrics to be stable first (Wave 1)

**Dry-Run Validation**:
- ✅ No alert rules are configured in Prometheus (Wave 1)
- ✅ Alertmanager is not started (Wave 2)
- ✅ Alert configuration files are not present (Wave 2)

---

## Dry-Run Execution Plan

### Phase 1: Infrastructure Setup (Day 1, Morning)

**Goal**: Set up Prometheus infrastructure and verify accessibility

**Steps**:
1. **Start Prometheus**:
   ```bash
   cd tools/observability
   docker-compose -f docker-compose.observability.yml up -d prometheus
   ```

2. **Verify Prometheus Health**:
   ```bash
   curl -s http://localhost:9090/-/healthy
   # Expected: Prometheus is Healthy.
   ```

3. **Check Prometheus UI**:
   ```bash
   # Open in browser: http://localhost:9090
   # Or check via API:
   curl -s http://localhost:9090/api/v1/status/config | jq .
   ```

4. **Verify Prometheus Configuration**:
   ```bash
   cat prometheus.yml
   # Verify Router (9001), Gateway (3001), Worker (9092) are configured
   ```

**Success Criteria**:
- ✅ Prometheus starts successfully
- ✅ Prometheus health endpoint returns "Prometheus is Healthy."
- ✅ Prometheus UI is accessible
- ✅ Prometheus configuration includes all components

---

### Phase 2: Metrics Endpoints Verification (Day 1, Afternoon)

**Goal**: Verify that metrics endpoints are accessible (even if empty)

**Steps**:
1. **Check Router Metrics Endpoint**:
   ```bash
   curl -s http://localhost:9001/metrics
   # Expected: Empty or minimal metrics (may be 404/connection refused)
   ```

2. **Check Gateway Metrics Endpoint**:
   ```bash
   curl -s http://localhost:3001/metrics
   # Expected: Empty or minimal metrics (may be 404/connection refused)
   ```

3. **Check Worker Metrics Endpoint**:
   ```bash
   curl -s http://localhost:9092/metrics
   # Expected: Empty or minimal metrics (may be 404/connection refused)
   ```

4. **Verify Ports Are Not in Use**:
   ```bash
   # Check if ports are available
   netstat -tuln | grep -E ':(9001|3001|9092)'
   # Expected: Ports are available (or in use by components)
   ```

**Success Criteria**:
- ✅ All metrics endpoints are accessible (even if empty/404)
- ✅ Ports are not in conflict with other services
- ✅ Endpoint paths are `/metrics`

---

### Phase 3: Prometheus Scrape Verification (Day 1, Afternoon)

**Goal**: Verify that Prometheus can scrape metrics from all components

**Steps**:
1. **Check Prometheus Targets**:
   ```bash
   curl -s http://localhost:9090/api/v1/targets | jq '.data.activeTargets[] | {job: .labels.job, health: .health}'
   # Expected: All targets configured (may be "down" without implementation)
   ```

2. **Check Scrape Status**:
   ```bash
   curl -s http://localhost:9090/api/v1/status/config | jq '.data.yaml' | grep -A 20 'scrape_configs'
   # Expected: All scrape targets configured correctly
   ```

3. **Query Metrics** (even if empty):
   ```bash
   # Query for router metrics (may return empty)
   curl -s "http://localhost:9090/api/v1/query?query=router_decisions_total" | jq .
   # Expected: Query succeeds (may return empty result)
   ```

**Success Criteria**:
- ✅ Prometheus can query targets API
- ✅ All targets are configured (router, gateway, worker)
- ✅ Prometheus can query metrics (even if empty)
- ⚠️ Targets may be "down" without implementation (expected)

---

### Phase 4: CP1 Profile Execution (Day 2, Morning)

**Goal**: Run CP1 profile tests and verify metrics generation

**Steps**:
1. **Run CP1 Profile Script**:
   ```bash
   cd scripts/observability
   bash run_cp1_profile.sh
   # Expected: Tests run successfully
   ```

2. **Check Metrics After Tests**:
   ```bash
   # Wait for Prometheus to scrape (15s interval)
   sleep 20
   
   # Check if metrics are available
   curl -s "http://localhost:9090/api/v1/query?query=router_decisions_total" | jq .
   # Expected: Metrics may be empty without implementation
   ```

3. **Verify Components Are Running**:
   ```bash
   # Check if components are accessible
   curl -s http://localhost:9000/-/health  # Router health (gRPC)
   curl -s http://localhost:3000/_health   # Gateway health
   curl -s http://localhost:9091/_health   # Worker health
   ```

**Success Criteria**:
- ✅ CP1 profile script runs successfully
- ✅ Components are accessible
- ✅ Metrics endpoints are accessible (even if empty)
- ⚠️ Metrics may be empty without implementation (expected)

---

### Phase 5: Metrics Validation Script (Day 2, Afternoon)

**Goal**: Create and run metrics validation script

**Steps**:
1. **Create Validation Script**:
   ```bash
   cd scripts/observability
   # Create validate_metrics.sh (see script content above)
   chmod +x validate_metrics.sh
   ```

2. **Run Validation Script**:
   ```bash
   bash validate_metrics.sh
   # Expected: Script runs, reports missing metrics (expected without implementation)
   ```

3. **Verify Script Logic**:
   ```bash
   # Test individual metric queries
   curl -s "http://localhost:9090/api/v1/query?query=router_decisions_total" | jq .
   curl -s "http://localhost:9090/api/v1/query?query=gateway_http_requests_total" | jq .
   curl -s "http://localhost:9090/api/v1/query?query=worker_step_executions_total" | jq .
   ```

**Success Criteria**:
- ✅ Validation script can be created
- ✅ Script can query Prometheus API
- ✅ Script checks for required metrics
- ✅ Script provides clear output
- ⚠️ Script may report missing metrics without implementation (expected)

---

### Phase 6: End-to-End Dry-Run (Day 2, Afternoon)

**Goal**: Execute complete dry-run scenario

**Steps**:
1. **Full Infrastructure Check**:
   ```bash
   # Check Prometheus
   curl -s http://localhost:9090/-/healthy
   
   # Check all metrics endpoints
   curl -s http://localhost:9001/metrics | head -5
   curl -s http://localhost:3001/metrics | head -5
   curl -s http://localhost:9092/metrics | head -5
   
   # Check Prometheus targets
   curl -s http://localhost:9090/api/v1/targets | jq '.data.activeTargets[] | {job: .labels.job, health: .health}'
   ```

2. **Run CP1 Profile**:
   ```bash
   cd scripts/observability
   bash run_cp1_profile.sh
   ```

3. **Run Metrics Validation**:
   ```bash
   bash validate_metrics.sh
   ```

4. **Verify No Alerts Configured**:
   ```bash
   # Check if alert rules exist (should not exist in Wave 1)
   curl -s http://localhost:9090/api/v1/rules | jq '.data.groups'
   # Expected: Empty or no alert rules
   ```

**Success Criteria**:
- ✅ All infrastructure checks pass
- ✅ CP1 profile runs successfully
- ✅ Metrics validation script runs successfully
- ✅ No alert rules configured (Wave 1)
- ✅ No blockers identified

---

## Dry-Run Checklist

### Infrastructure Readiness

- ✅ Prometheus Docker Compose file exists
- ✅ Prometheus starts successfully
- ✅ Prometheus health endpoint returns "Prometheus is Healthy."
- ✅ Prometheus UI is accessible on port 9090
- ✅ Prometheus configuration includes Router (9001), Gateway (3001), Worker (9092)

### Metrics Endpoints Readiness

- ✅ Router metrics endpoint is accessible (port 9001, path `/metrics`)
- ✅ Gateway metrics endpoint is accessible (port 3001, path `/metrics`)
- ✅ Worker metrics endpoint is accessible (port 9092, path `/metrics`)
- ✅ Ports are not in conflict with other services
- ✅ Endpoints return Prometheus text format (if implemented)

### Prometheus Scrape Readiness

- ✅ Prometheus can query targets API
- ✅ All targets are configured (router, gateway, worker)
- ✅ Prometheus can query metrics (even if empty)
- ✅ Scrape configuration is correct

### CP1 Profile Readiness

- ✅ CP1 profile script exists and can be run
- ✅ Script starts all components
- ✅ Script runs test scenarios
- ✅ Script completes successfully

### Metrics Validation Readiness

- ✅ Metrics validation script can be created
- ✅ Script can query Prometheus API
- ✅ Script checks for required metrics
- ✅ Script provides clear output

### Overall Readiness

- ✅ All infrastructure checks pass
- ✅ All metrics endpoints are accessible
- ✅ Prometheus can scrape metrics
- ✅ CP1 profile runs successfully
- ✅ Metrics validation script works
- ✅ No alert rules configured (Wave 1)
- ✅ No blockers identified
- ✅ Ready for Wave 1 implementation

---

## Expected Dry-Run Results

### Successful Dry-Run

**Infrastructure**:
- ✅ Prometheus starts successfully
- ✅ Prometheus health endpoint returns "Prometheus is Healthy."
- ✅ Prometheus UI is accessible
- ✅ Prometheus configuration is correct

**Metrics Endpoints**:
- ✅ All metrics endpoints are accessible (even if empty/404)
- ✅ Ports are not in conflict
- ✅ Endpoint paths are `/metrics`

**Prometheus Scrape**:
- ✅ Prometheus can query targets API
- ✅ All targets are configured
- ✅ Prometheus can query metrics (even if empty)

**CP1 Profile**:
- ✅ CP1 profile script runs successfully
- ✅ Components are accessible

**Metrics Validation**:
- ✅ Validation script works
- ⚠️ Script may report missing metrics (expected without implementation)

**Outcome**: ✅ **Ready for Wave 1 implementation**

---

### Dry-Run with Issues

**If Issues Found**:

1. **Prometheus Issues**:
   - Fix Docker Compose configuration
   - Fix Prometheus configuration
   - Re-start Prometheus

2. **Metrics Endpoint Issues**:
   - Fix port conflicts
   - Fix endpoint paths
   - Re-check endpoint accessibility

3. **Scrape Issues**:
   - Fix Prometheus scrape configuration
   - Fix host/port settings
   - Re-check scrape targets

4. **CP1 Profile Issues**:
   - Fix component startup
   - Fix test scenarios
   - Re-run CP1 profile

5. **Validation Script Issues**:
   - Fix script logic
   - Fix Prometheus queries
   - Re-run validation script

**Outcome**: ⚠️ **Issues identified and resolved before implementation**

---

## Dry-Run Report Template

### Dry-Run Report

**Date**: YYYY-MM-DD  
**Worker**: wrk-obs1  
**Wave**: Wave 1 Observability (Prometheus Metrics)

**Infrastructure Validation**:
- [ ] Prometheus starts successfully: PASS / FAIL
- [ ] Prometheus health endpoint: PASS / FAIL
- [ ] Prometheus UI accessible: PASS / FAIL
- [ ] Prometheus configuration correct: PASS / FAIL

**Metrics Endpoints Validation**:
- [ ] Router metrics endpoint (9001): PASS / FAIL
- [ ] Gateway metrics endpoint (3001): PASS / FAIL
- [ ] Worker metrics endpoint (9092): PASS / FAIL
- [ ] Ports not in conflict: PASS / FAIL

**Prometheus Scrape Validation**:
- [ ] Prometheus targets API: PASS / FAIL
- [ ] All targets configured: PASS / FAIL
- [ ] Prometheus can query metrics: PASS / FAIL

**CP1 Profile Validation**:
- [ ] CP1 profile script runs: PASS / FAIL
- [ ] Components accessible: PASS / FAIL

**Metrics Validation Script**:
- [ ] Script can query Prometheus: PASS / FAIL
- [ ] Script checks required metrics: PASS / FAIL
- [ ] Script provides clear output: PASS / FAIL

**Overall Readiness**:
- [ ] Ready for implementation: YES / NO
- [ ] Blockers identified: YES / NO
- [ ] Blockers resolved: YES / NO

**Notes**:
- [Any issues found, resolutions, or recommendations]

---

## CI Integration

### CI Dry-Run Script

**Script**: `scripts/observability/dry_run_ci.sh` (to be created)

**Purpose**: Automated dry-run for CI pipelines

**Script Content** (example):
```bash
#!/bin/bash
# CI dry-run script for Wave 1 Observability

set -euo pipefail

echo "[INFO] Starting Wave 1 Observability dry-run..."

# Start Prometheus
echo "[INFO] Starting Prometheus..."
cd tools/observability
docker-compose -f docker-compose.observability.yml up -d prometheus
sleep 5

# Check Prometheus health
echo "[INFO] Checking Prometheus health..."
if ! curl -s http://localhost:9090/-/healthy | grep -q "Prometheus is Healthy"; then
  echo "[ERROR] Prometheus is not healthy"
  exit 1
fi

# Check metrics endpoints (may be empty)
echo "[INFO] Checking metrics endpoints..."
curl -s http://localhost:9001/metrics > /dev/null || echo "[WARN] Router metrics endpoint not accessible"
curl -s http://localhost:3001/metrics > /dev/null || echo "[WARN] Gateway metrics endpoint not accessible"
curl -s http://localhost:9092/metrics > /dev/null || echo "[WARN] Worker metrics endpoint not accessible"

# Check Prometheus targets
echo "[INFO] Checking Prometheus targets..."
TARGETS=$(curl -s http://localhost:9090/api/v1/targets | jq '.data.activeTargets | length')
echo "[INFO] Prometheus targets configured: $TARGETS"

# Run metrics validation (may report missing metrics)
echo "[INFO] Running metrics validation..."
cd ../../scripts/observability
bash validate_metrics.sh || echo "[WARN] Metrics validation reported missing metrics (expected without implementation)"

echo "[INFO] Dry-run completed successfully"
```

**CI Integration**:
- Add to `.github/workflows/validate.yml` or similar
- Run before Wave 1 implementation
- Don't fail if metrics are missing (expected without implementation)

---

## References

### Primary Documents
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md` - Wave 1 specification
- `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - Complete backlog
- `docs/archive/dev/CP2_WORKER_OBSERVABILITY_READINESS.md` - Readiness criteria

### Infrastructure Files
- `tools/observability/docker-compose.observability.yml` - Docker Compose configuration
- `tools/observability/prometheus.yml` - Prometheus configuration

### Scripts
- `scripts/observability/run_cp1_profile.sh` - CP1 profile test script
- `scripts/observability/validate_metrics.sh` - Metrics validation script (to be created)

### Metrics Specification
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - Metrics specification
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md#core-metrics-wave-1` - Wave 1 core metrics

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Observability Wave 1 Dry-Run Plan
- Prometheus infrastructure setup procedures
- Metrics endpoints verification
- Prometheus scrape verification
- CP1 profile execution
- Metrics validation script
- Required time-series specification
- Alerts exclusion (Wave 2)

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: Dry-Run Plan Ready for Execution

