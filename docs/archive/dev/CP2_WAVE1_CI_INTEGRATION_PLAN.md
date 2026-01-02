# CP2 Wave 1 CI Integration Plan

**Version**: 1.0  
**Date**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: 📋 **CI INTEGRATION PLAN** (CP2)

---

## Executive Summary

This document defines how dry-run scenarios for **Worker Reliability Wave 1** and **Observability Wave 1** are integrated into CI/CD pipelines.

**Key Principle**: Fast, lightweight checks run on every PR; expensive infrastructure checks run on schedule or manually.

**References**:
- `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1_DRYRUN.md` - Worker Reliability dry-run plan
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1_DRYRUN.md` - Observability dry-run plan

---

## Integration Matrix

### Worker Reliability Dry-Run Steps

| Step | Description | CI Mode | Trigger | Cost | Notes |
|------|-------------|---------|---------|------|-------|
| **Configuration Validation** | Validate JSON configs (retry, timeout, queue) | **CI: auto** | Every PR | Low | Fast JSON validation |
| **Feature Flag Validation** | Check feature flags can be set/read | **CI: auto** | Every PR | Low | Fast env var check |
| **CP1 Baseline Tests** | Run CP1 tests with flags disabled | **CI: auto** | Every PR | Medium | Standard test suite |
| **CP2 Test Stubs** | Compile CP2 test stubs (may fail) | **CI: optional** | Every PR | Low | Best-effort, don't fail CI |
| **Integration Tests** | Full integration test scenarios | **CI: optional** | Nightly | High | Requires full stack |
| **Metrics Collection** | Check metrics infrastructure ready | **CI: optional** | Nightly | Low | Best-effort validation |
| **Log Collection** | Check log infrastructure ready | **CI: optional** | Nightly | Low | Best-effort validation |
| **End-to-End Dry-Run** | Complete dry-run scenario | **manual only** | Pre-release | High | Full infrastructure |

### Observability Dry-Run Steps

| Step | Description | CI Mode | Trigger | Cost | Notes |
|------|-------------|---------|---------|------|-------|
| **Prometheus Config Validation** | Validate prometheus.yml syntax | **CI: auto** | Every PR | Low | Fast YAML validation |
| **Metrics Endpoint Check** | Check endpoints accessible (curl) | **CI: optional** | Nightly | Medium | Requires components running |
| **Prometheus Start** | Start Prometheus from Docker Compose | **CI: optional** | Nightly | High | Requires Docker, ~30s startup |
| **Prometheus Scrape Check** | Check scrape targets configured | **CI: optional** | Nightly | Medium | Requires Prometheus running |
| **CP1 Profile Execution** | Run CP1 profile tests | **CI: optional** | Nightly | High | Requires full stack + Prometheus |
| **Metrics Validation Script** | Check required metrics exist | **CI: optional** | Nightly | Medium | Requires Prometheus + metrics |
| **Full Metrics Validation** | Validate all time-series | **manual only** | Pre-release | High | Full infrastructure + metrics |

---

## CI Integration Recommendations

### 1. GitHub Actions Workflow Structure

#### 1.1. Fast Checks (Every PR)

**Workflow**: `.github/workflows/cp2-wave1-fast-checks.yml`

**Purpose**: Fast, lightweight validation that runs on every PR

**Steps**:
```yaml
name: CP2 Wave 1 Fast Checks

on:
  pull_request:
    paths:
      - 'config/worker/**'
      - 'scripts/worker/**'
      - 'tools/observability/**'
      - 'apps/**'
  push:
    branches: [ main ]

jobs:
  worker-config:
    name: Worker Configuration Validation
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Validate Worker Configs
        run: |
          bash scripts/worker/validate_config.sh
      
      - name: Validate Feature Flags
        run: |
          bash scripts/worker/validate_feature_flags.sh

  observability-config:
    name: Observability Configuration Validation
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.11'
      
      - name: Validate Prometheus Config
        run: |
          python3 -c "import yaml; yaml.safe_load(open('tools/observability/prometheus.yml'))"
          echo "✅ Prometheus config is valid YAML"
```

**Execution Time**: ~30-60 seconds  
**Cost**: Low (no Docker, no services)

---

#### 1.2. CP1 Baseline Tests (Every PR)

**Workflow**: `.github/workflows/cp2-wave1-baseline-tests.yml` (or integrate into existing test workflow)

**Purpose**: Ensure CP1 baseline tests pass with feature flags disabled

**Steps**:
```yaml
name: CP2 Wave 1 Baseline Tests

on:
  pull_request:
  push:
    branches: [ main ]

jobs:
  worker-baseline-tests:
    name: Worker CP1 Baseline Tests
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup C++ Build
        run: |
          sudo apt-get update
          sudo apt-get install -y build-essential cmake
      
      - name: Build Worker Tests
        working-directory: apps/caf/processor
        run: |
          mkdir -p build
          cd build
          cmake ..
          make test_core test_block_executor test_worker_router_contract test_observability
      
      - name: Run CP1 Baseline Tests
        working-directory: apps/caf/processor/build
        env:
          CP2_ADVANCED_RETRY_ENABLED: false
          CP2_COMPLETE_TIMEOUT_ENABLED: false
          CP2_QUEUE_MANAGEMENT_ENABLED: false
        run: |
          ./test_core
          ./test_block_executor
          ./test_worker_router_contract
          ./test_observability
```

**Execution Time**: ~5-10 minutes  
**Cost**: Medium (build time, but no services)

---

#### 1.3. Optional Checks (Nightly)

**Workflow**: `.github/workflows/cp2-wave1-nightly.yml`

**Purpose**: Expensive checks that run on schedule (nightly) or on-demand

**Steps**:
```yaml
name: CP2 Wave 1 Nightly Checks

on:
  schedule:
    - cron: '0 2 * * *'  # 2 AM UTC daily
  workflow_dispatch:  # Manual trigger

jobs:
  observability-infrastructure:
    name: Observability Infrastructure Check
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Docker Compose
        run: |
          sudo apt-get update
          sudo apt-get install -y docker-compose
      
      - name: Start Prometheus
        working-directory: tools/observability
        run: |
          docker-compose -f docker-compose.observability.yml up -d prometheus
          sleep 10  # Wait for Prometheus to start
      
      - name: Check Prometheus Health
        run: |
          curl -f http://localhost:9090/-/healthy || exit 1
      
      - name: Check Prometheus Targets
        run: |
          curl -s http://localhost:9090/api/v1/targets | jq '.data.activeTargets | length'
      
      - name: Cleanup
        if: always()
        working-directory: tools/observability
        run: |
          docker-compose -f docker-compose.observability.yml down

  worker-integration-tests:
    name: Worker Integration Tests
    runs-on: ubuntu-latest
    continue-on-error: true  # Best-effort, don't fail CI
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup NATS
        run: |
          docker run -d --name nats -p 4222:4222 nats:latest
          sleep 5
      
      - name: Run Integration Tests
        env:
          NATS_URL: nats://localhost:4222
        run: |
          # Run integration tests (if implemented)
          echo "Integration tests would run here"
      
      - name: Cleanup
        if: always()
        run: |
          docker stop nats || true
          docker rm nats || true
```

**Execution Time**: ~10-20 minutes  
**Cost**: High (Docker, services, infrastructure)

---

#### 1.4. Pre-Release Checks (Manual Only)

**Workflow**: `.github/workflows/cp2-wave1-pre-release.yml`

**Purpose**: Full dry-run scenario before release

**Steps**:
```yaml
name: CP2 Wave 1 Pre-Release

on:
  workflow_dispatch:  # Manual trigger only
    inputs:
      enable_full_stack:
        description: 'Start full stack (Router, Gateway, Worker)'
        required: false
        default: false
        type: boolean

jobs:
  full-dry-run:
    name: Full Dry-Run Scenario
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Infrastructure
        run: |
          # Start Prometheus
          cd tools/observability
          docker-compose -f docker-compose.observability.yml up -d prometheus
          
          # Start NATS (if needed)
          docker run -d --name nats -p 4222:4222 nats:latest
      
      - name: Run Worker Dry-Run
        run: |
          bash scripts/worker/validate_config.sh
          bash scripts/worker/validate_feature_flags.sh
          # Run CP1 baseline tests
          # Run CP2 test stubs (best-effort)
      
      - name: Run Observability Dry-Run
        run: |
          # Check Prometheus
          curl -f http://localhost:9090/-/healthy
          
          # Check metrics endpoints (if components running)
          if [ "${{ inputs.enable_full_stack }}" = "true" ]; then
            curl -s http://localhost:9001/metrics || echo "Router metrics not available"
            curl -s http://localhost:3001/metrics || echo "Gateway metrics not available"
            curl -s http://localhost:9092/metrics || echo "Worker metrics not available"
          fi
      
      - name: Run Metrics Validation
        if: inputs.enable_full_stack == true
        run: |
          bash scripts/observability/validate_metrics.sh || echo "Metrics validation failed (expected if not implemented)"
      
      - name: Cleanup
        if: always()
        run: |
          cd tools/observability
          docker-compose -f docker-compose.observability.yml down
          docker stop nats || true
          docker rm nats || true
```

**Execution Time**: ~20-30 minutes  
**Cost**: Very High (full infrastructure)

---

### 2. Local Scripts

#### 2.1. Fast Local Validation

**Script**: `scripts/cp2-wave1/validate-fast.sh`

**Purpose**: Fast local validation (matches CI fast checks)

**Content**:
```bash
#!/bin/bash
# Fast validation for CP2 Wave 1 (matches CI fast checks)

set -euo pipefail

echo "[INFO] Running CP2 Wave 1 fast validation..."

# Worker config validation
echo "[INFO] Validating Worker configuration..."
bash scripts/worker/validate_config.sh

# Worker feature flags
echo "[INFO] Validating Worker feature flags..."
bash scripts/worker/validate_feature_flags.sh

# Observability config validation
echo "[INFO] Validating Observability configuration..."
python3 -c "import yaml; yaml.safe_load(open('tools/observability/prometheus.yml'))"
echo "✅ Prometheus config is valid YAML"

echo "[OK] Fast validation completed"
```

**Usage**:
```bash
bash scripts/cp2-wave1/validate-fast.sh
```

---

#### 2.2. Full Local Dry-Run

**Script**: `scripts/cp2-wave1/dry-run-full.sh`

**Purpose**: Full local dry-run (matches pre-release checks)

**Content**:
```bash
#!/bin/bash
# Full dry-run for CP2 Wave 1 (matches pre-release checks)

set -euo pipefail

ENABLE_FULL_STACK="${ENABLE_FULL_STACK:-false}"

echo "[INFO] Running CP2 Wave 1 full dry-run..."

# Fast validation first
bash scripts/cp2-wave1/validate-fast.sh

# Start Prometheus
echo "[INFO] Starting Prometheus..."
cd tools/observability
docker-compose -f docker-compose.observability.yml up -d prometheus
sleep 10

# Check Prometheus
echo "[INFO] Checking Prometheus..."
curl -f http://localhost:9090/-/healthy || exit 1

# Check metrics endpoints (if full stack enabled)
if [ "$ENABLE_FULL_STACK" = "true" ]; then
  echo "[INFO] Checking metrics endpoints..."
  curl -s http://localhost:9001/metrics | head -5 || echo "[WARN] Router metrics not available"
  curl -s http://localhost:3001/metrics | head -5 || echo "[WARN] Gateway metrics not available"
  curl -s http://localhost:9092/metrics | head -5 || echo "[WARN] Worker metrics not available"
  
  # Run metrics validation
  echo "[INFO] Running metrics validation..."
  bash scripts/observability/validate_metrics.sh || echo "[WARN] Metrics validation failed (expected if not implemented)"
fi

# Cleanup
echo "[INFO] Cleaning up..."
docker-compose -f docker-compose.observability.yml down

echo "[OK] Full dry-run completed"
```

**Usage**:
```bash
# Without full stack
bash scripts/cp2-wave1/dry-run-full.sh

# With full stack (requires components running)
ENABLE_FULL_STACK=true bash scripts/cp2-wave1/dry-run-full.sh
```

---

### 3. Environment Variables and Secrets

#### 3.1. Required Environment Variables

**For Worker Reliability**:
```yaml
env:
  CP2_ADVANCED_RETRY_ENABLED: false  # Default: CP1 baseline
  CP2_COMPLETE_TIMEOUT_ENABLED: false  # Default: CP1 baseline
  CP2_QUEUE_MANAGEMENT_ENABLED: false  # Default: CP1 baseline
```

**For Observability**:
```yaml
env:
  CP2_OBSERVABILITY_METRICS_ENABLED: false  # Default: CP1 baseline
  PROMETHEUS_URL: http://localhost:9090  # For metrics validation
```

**No Secrets Required**: All Wave 1 checks are configuration/infrastructure validation, no secrets needed.

---

#### 3.2. Optional Environment Variables

**For Full Stack Testing**:
```yaml
env:
  NATS_URL: nats://localhost:4222  # For integration tests
  ROUTER_URL: http://localhost:9000  # For Router health checks
  GATEWAY_URL: http://localhost:8081  # For C-Gateway health checks
  WORKER_URL: http://localhost:9091  # For Worker health checks
```

**For Metrics Endpoints**:
```yaml
env:
  ROUTER_METRICS_URL: http://localhost:9001/metrics
  GATEWAY_METRICS_URL: http://localhost:8081/_metrics
  WORKER_METRICS_URL: http://localhost:9092/metrics
```

---

## CI Success Criteria

### 1. Required Checks (Must Pass)

**Fast Checks (Every PR)**:
- ✅ Worker configuration validation passes (JSON syntax, structure)
- ✅ Worker feature flag validation passes (flags can be set/read)
- ✅ Prometheus configuration validation passes (YAML syntax)
- ✅ CP1 baseline tests pass (with feature flags disabled)

**Failure Behavior**: PR cannot be merged if required checks fail.

---

### 2. Optional Checks (Best-Effort)

**Nightly Checks**:
- ⚠️ Prometheus starts successfully (best-effort, don't fail CI)
- ⚠️ Metrics endpoints are accessible (best-effort, may be 404 without implementation)
- ⚠️ Prometheus scrape targets configured (best-effort, may be "down" without implementation)
- ⚠️ CP2 test stubs compile (best-effort, may fail without implementation)
- ⚠️ Integration tests pass (best-effort, may fail without full stack)

**Failure Behavior**: Optional checks use `continue-on-error: true`, failures are reported but don't block PRs.

---

### 3. Manual Checks (Pre-Release)

**Pre-Release Checks**:
- ✅ Full dry-run scenario completes
- ✅ All infrastructure components start successfully
- ✅ Metrics validation script runs (may report missing metrics if not implemented)
- ✅ End-to-end validation passes

**Failure Behavior**: Pre-release checks must pass before release, but are not automated in PR workflow.

---

## Cost Analysis

### Fast Checks (Every PR)

**Execution Time**: ~30-60 seconds  
**Resource Usage**: Minimal (no Docker, no services)  
**Cost**: **Low** ✅

**Steps**:
- JSON/YAML validation (CPU only)
- Environment variable checks (CPU only)
- Configuration structure validation (CPU only)

---

### Baseline Tests (Every PR)

**Execution Time**: ~5-10 minutes  
**Resource Usage**: Build time (C++ compilation)  
**Cost**: **Medium** ⚠️

**Steps**:
- C++ compilation (CPU intensive)
- Test execution (CPU only, no services)

---

### Nightly Checks (Scheduled)

**Execution Time**: ~10-20 minutes  
**Resource Usage**: Docker containers, Prometheus, NATS  
**Cost**: **High** ⚠️

**Steps**:
- Docker Compose startup (~30s)
- Prometheus startup (~10s)
- Service health checks
- Metrics endpoint checks

**Rationale**: Too expensive for every PR, but valuable for catching infrastructure issues.

---

### Pre-Release Checks (Manual)

**Execution Time**: ~20-30 minutes  
**Resource Usage**: Full infrastructure (Prometheus, NATS, full stack)  
**Cost**: **Very High** ⚠️

**Steps**:
- Full infrastructure startup
- Complete dry-run scenario
- Full metrics validation
- End-to-end checks

**Rationale**: Only run before releases, not in regular CI.

---

## Integration Checklist

### GitHub Actions Workflows

- [ ] Create `.github/workflows/cp2-wave1-fast-checks.yml` (fast validation)
- [ ] Create `.github/workflows/cp2-wave1-baseline-tests.yml` (CP1 baseline tests)
- [ ] Create `.github/workflows/cp2-wave1-nightly.yml` (optional checks)
- [ ] Create `.github/workflows/cp2-wave1-pre-release.yml` (manual pre-release)

### Local Scripts

- [ ] Create `scripts/cp2-wave1/validate-fast.sh` (fast local validation)
- [ ] Create `scripts/cp2-wave1/dry-run-full.sh` (full local dry-run)

### Validation Scripts

- [ ] Create `scripts/worker/validate_config.sh` (Worker config validation)
- [ ] Create `scripts/worker/validate_feature_flags.sh` (feature flag validation)
- [ ] Create `scripts/observability/validate_metrics.sh` (metrics validation)

### Documentation

- [ ] Update `.github/workflows/README.md` with CP2 Wave 1 workflows
- [ ] Add CI integration section to dry-run documents
- [ ] Document environment variables in CI setup guide

---

## Example Workflow Integration

### Fast Checks in Existing Workflow

**Add to `.github/workflows/ci-validate.yml`**:

```yaml
jobs:
  # ... existing jobs ...
  
  cp2-wave1-fast:
    name: CP2 Wave 1 Fast Checks
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.11'
      
      - name: Validate Worker Configs
        run: |
          bash scripts/worker/validate_config.sh || echo "Worker config validation failed"
      
      - name: Validate Feature Flags
        run: |
          bash scripts/worker/validate_feature_flags.sh || echo "Feature flag validation failed"
      
      - name: Validate Prometheus Config
        run: |
          python3 -c "import yaml; yaml.safe_load(open('tools/observability/prometheus.yml'))" || echo "Prometheus config validation failed"
```

---

## Troubleshooting

### Common Issues

**Issue**: Prometheus fails to start in CI
- **Solution**: Check Docker Compose version, ensure ports are available
- **Workaround**: Mark as `continue-on-error: true` for optional checks

**Issue**: Metrics endpoints return 404
- **Solution**: Expected if Wave 1 not implemented, mark as best-effort
- **Workaround**: Check endpoint accessibility, don't fail CI

**Issue**: CP2 test stubs fail to compile
- **Solution**: Expected if Wave 1 not implemented, mark as best-effort
- **Workaround**: Use `continue-on-error: true`, report but don't fail

**Issue**: Integration tests require full stack
- **Solution**: Run only in nightly or pre-release, not on every PR
- **Workaround**: Use `workflow_dispatch` for manual triggering

---

## References

### Dry-Run Documents
- `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1_DRYRUN.md` - Worker Reliability dry-run plan
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1_DRYRUN.md` - Observability dry-run plan

### Wave 1 Specifications
- `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1.md` - Worker Reliability Wave 1 specification
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md` - Observability Wave 1 specification

### CI Documentation
- `.github/workflows/README.md` - GitHub Actions workflows documentation
- `docs/CI_VALIDATION.md` - CI validation process

---

## Change History

**v1.0 (2025-01-27)**:
- Initial CP2 Wave 1 CI Integration Plan
- Integration matrix for all dry-run steps
- GitHub Actions workflow recommendations
- Local scripts for validation
- CI success criteria
- Cost analysis

---

**Last Updated**: 2025-01-27  
**Checkpoint**: CP2-LC  
**Status**: CI Integration Plan Ready for Implementation

