# Router ↔ DevState E2E Scenarios

This document describes end-to-end scenarios for testing Router's interaction with DevState, including normal CP transitions, DevState degradation, and recovery procedures.

**Purpose**: Enable QA/Dev/On-call teams to manually verify Router behavior in various DevState conditions.

**Note**: These scenarios are for **manual testing**. Automated tests are available in `apps/otp/router/test/router_state_observability_SUITE.erl`.

## Prerequisites

- DevState running: `make devstate-up`
- Router application compiled and ready to start
- Access to Router logs (structured JSON)
- Access to Router metrics (via Telemetry bridge or Prometheus)

**Environment Setup**:
```bash
# Start DevState
make devstate-up

# Verify DevState is running
curl -fsS http://localhost:3080/health

# Verify HMAC chain
bash devstate/scripts/devstate_verify.sh
```

## Scenario 1: Normal CP Transition (CP1 → CP2)

**Purpose**: Verify Router correctly loads new CP from DevState and applies feature gates.

### Preconditions

- DevState running (`make devstate-up`)
- Router not running (or stopped)
- Current CP is `CP1-LC` (or any valid CP)

### Steps

1. **Set CP to CP2-LC via DevState**:
   ```bash
   make devstate-set-cp CP=CP2-LC
   ```

2. **Verify state file updated**:
   ```bash
   cat .trae/state.json | jq '.current_cp'
   # Expected: "CP2-LC"
   ```

3. **Start Router**:
   ```bash
   cd apps/otp/router
   rebar3 shell
   # Or: make run (if Makefile target exists)
   ```

4. **Verify Router loaded CP2**:
   - Check Router logs for successful state load (no fallback messages)
   - Verify CP2+ features are enabled (if configured)

### Expected Results

#### Logs

**No fallback messages** (Router successfully loaded CP2):
- No structured log entries with `"cp_fallback": "CP1-baseline"`
- No error messages about DevState/CP state failure

**Example of successful log** (if Router logs state load):
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "INFO",
  "component": "router",
  "message": "Router started",
  "context": {
    "current_cp": "CP2-LC",
    "no_drift": true
  }
}
```

#### Metrics

**No error metrics**:
- `router_cp_state_errors_total{reason="*"}` = 0 (no increments)
- `router_cp_state_fallback_total{cp_fallback="CP1-baseline"}` = 0 (no increments)

#### State Verification

- `.trae/state.json` contains `"current_cp": "CP2-LC"`
- `.trae/state.json` contains `"no_drift": true`
- Router processes requests normally (no degradation)

### Rollback (CP2 → CP1)

To rollback to CP1:
```bash
make devstate-set-cp CP=CP1-LC
# Restart Router to apply CP1
```

### References

- **CP Transitions**: `devstate/docs/USAGE.md#cp-transitions-for-router-quick-how-to`
- **CP vs Features**: `docs/ADR/ADR-015-router-devstate-integration.md#cp-vs-features-code-level-gates`
- **CP1 Baseline**: `docs/CP1_BASELINE.md`

---

## Scenario 2: DevState Not Available / Missing State File

**Purpose**: Verify Router falls back to CP1 baseline when state file is missing.

**Automated**: Yes - `./scripts/devstate_router_fallback_smoke.sh --scenario missing_state`

### Preconditions

- DevState running (for initial setup)
- Router not running

### Steps

1. **Create valid state file** (for initial setup):
   ```bash
   make devstate-set-cp CP=CP2-LC
   ```

2. **Stop DevState** (simulate DevState outage):
   ```bash
   make devstate-down
   ```

3. **Remove state file** (simulate missing state):
   ```bash
   # WARNING: Only for local testing! Never do this in production.
   mv .trae/state.json .trae/state.json.backup
   ```

4. **Start Router**:
   ```bash
   cd apps/otp/router
   rebar3 shell
   ```

5. **Observe Router behavior**:
   - Router should start successfully
   - Router should operate in CP1 baseline mode
   - Check logs and metrics

### Expected Results

#### Logs

**Structured fallback log**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "ERROR",
  "component": "router",
  "message": "DevState/CP state failure",
  "context": {
    "cp_fallback": "CP1-baseline",
    "failure": "missing_state",
    "reason": "missing_state"
  }
}
```

**Key fields**:
- `"cp_fallback": "CP1-baseline"` - indicates fallback occurred
- `"failure": "missing_state"` - reason for fallback
- `"level": "ERROR"` - error level (but Router continues operating)

#### Metrics

**Error and fallback metrics incremented**:
- `router_cp_state_errors_total{reason="missing_state"}` > 0
- `router_cp_state_fallback_total{cp_fallback="CP1-baseline"}` > 0

#### Router Behavior

- Router starts successfully (no crash)
- Router operates in CP1 baseline mode:
  - CP1 mandatory components started (see `docs/CP1_BASELINE.md`)
  - CP2+ features disabled (idempotency, tracing, admin gRPC, etc.)
- Router processes requests normally (degraded but functional)

### Recovery

To recover from missing state:

1. **Restore state file**:
   ```bash
   # Restore from backup
   mv .trae/state.json.backup .trae/state.json
   
   # Or: Re-export from DevState (if DevState is running)
   make devstate-up
   bash devstate/scripts/devstate_export.sh
   ```

2. **Restart Router** to load restored state

3. **Verify recovery**:
   - No fallback logs
   - Metrics return to 0 (or stop incrementing)
   - Router operates in normal mode (not CP1 baseline)

### References

- **Fallback Behavior**: `docs/ADR/ADR-015-router-devstate-integration.md#fallback-and-observability`
- **CP1 Baseline**: `docs/CP1_BASELINE.md`
- **Observability Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`

---

## Scenario 3: Invalid JSON in State File

**Purpose**: Verify Router handles corrupted state file gracefully.

**Automated**: Yes - `./scripts/devstate_router_fallback_smoke.sh --scenario invalid_json`

### Preconditions

- DevState running
- Router not running

### Steps

1. **Create valid state file**:
   ```bash
   make devstate-set-cp CP=CP2-LC
   ```

2. **Corrupt state file** (simulate JSON corruption):
   ```bash
   # WARNING: Only for local testing! Never do this in production.
   echo '{"current_cp":"CP2-LC","no_drift":true' > .trae/state.json
   # Note: Missing closing brace - invalid JSON
   ```

3. **Start Router**:
   ```bash
   cd apps/otp/router
   rebar3 shell
   ```

4. **Observe Router behavior**:
   - Router should start successfully
   - Router should fall back to CP1 baseline
   - Check logs and metrics

### Expected Results

#### Logs

**Structured fallback log**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "ERROR",
  "component": "router",
  "message": "DevState/CP state failure",
  "context": {
    "cp_fallback": "CP1-baseline",
    "failure": "decode_error",
    "reason": "decode_error"
  }
}
```

**Key fields**:
- `"failure": "decode_error"` - indicates JSON parsing failed
- `"cp_fallback": "CP1-baseline"` - fallback occurred

#### Metrics

**Error and fallback metrics incremented**:
- `router_cp_state_errors_total{reason="decode_error"}` > 0
- `router_cp_state_fallback_total{cp_fallback="CP1-baseline"}` > 0

#### Router Behavior

- Router starts successfully (no crash)
- Router operates in CP1 baseline mode
- Router processes requests normally (degraded but functional)

### Recovery

To recover from invalid JSON:

1. **Restore valid state file**:
   ```bash
   # Re-export from DevState
   make devstate-up
   bash devstate/scripts/devstate_export.sh
   ```

2. **Restart Router** to load restored state

3. **Verify recovery**:
   - No fallback logs
   - Metrics return to 0
   - Router operates in normal mode (CP2-LC if set)

### References

- **Fallback Behavior**: `docs/ADR/ADR-015-router-devstate-integration.md#fallback-and-observability`
- **Observability Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`

---

## Scenario 4: no_drift=false (Drift Detected)

**Purpose**: Verify Router falls back to CP1 baseline when `no_drift=false`.

**Automated**: Yes - `./scripts/devstate_router_fallback_smoke.sh --scenario no_drift_false`

### Preconditions

- DevState running
- Router not running

### Steps

1. **Set CP to CP2-LC**:
   ```bash
   make devstate-set-cp CP=CP2-LC
   ```

2. **Manually set no_drift=false** (simulate drift detection):
   ```bash
   # WARNING: Only for local testing! This simulates drift detection.
   # In production, DevState sets no_drift=false when drift is detected.
   cat .trae/state.json | jq '. + {no_drift: false}' > .trae/state.json.tmp
   mv .trae/state.json.tmp .trae/state.json
   ```

3. **Start Router**:
   ```bash
   cd apps/otp/router
   rebar3 shell
   ```

4. **Observe Router behavior**:
   - Router should start successfully
   - Router should fall back to CP1 baseline
   - Check logs and metrics

### Expected Results

#### Logs

**Structured fallback log**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "ERROR",
  "component": "router",
  "message": "DevState/CP state failure",
  "context": {
    "cp_fallback": "CP1-baseline",
    "failure": "no_drift_violation",
    "no_drift": false,
    "current_cp": "CP2-LC"
  }
}
```

**Key fields**:
- `"failure": "no_drift_violation"` - indicates no_drift=false detected
- `"cp_fallback": "CP1-baseline"` - fallback occurred
- `"no_drift": false` - shows the problematic value

#### Metrics

**Error and fallback metrics incremented**:
- `router_cp_state_errors_total{reason="no_drift_violation"}` > 0
- `router_cp_state_fallback_total{cp_fallback="CP1-baseline"}` > 0

#### Router Behavior

- Router starts successfully (no crash)
- Router operates in CP1 baseline mode (ignores CP2-LC due to drift)
- Router processes requests normally (degraded but functional)

### Recovery

To recover from drift:

1. **Fix drift via DevState**:
   ```bash
   # DevState should detect and fix drift, then set no_drift=true
   # For manual testing, restore via DevState:
   make devstate-set-cp CP=CP2-LC
   # DevState will set no_drift=true automatically
   ```

2. **Restart Router** to load restored state

3. **Verify recovery**:
   - No fallback logs
   - Metrics return to 0
   - Router operates in normal mode (CP2-LC if set)

### References

- **No-Drift Policy**: `docs/ADR/ADR-015-router-devstate-integration.md#decision`
- **Fallback Behavior**: `docs/ADR/ADR-015-router-devstate-integration.md#fallback-and-observability`
- **Observability Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`

---

## Scenario 5: Invalid Schema (Missing Required Fields)

**Purpose**: Verify Router handles state file with invalid schema gracefully.

### Preconditions

- DevState running
- Router not running

### Steps

1. **Create valid state file**:
   ```bash
   make devstate-set-cp CP=CP2-LC
   ```

2. **Corrupt schema** (remove required fields):
   ```bash
   # WARNING: Only for local testing! Never do this in production.
   echo '{"some_field":"value"}' > .trae/state.json
   # Note: Missing "current_cp" and "no_drift" - invalid schema
   ```

3. **Start Router**:
   ```bash
   cd apps/otp/router
   rebar3 shell
   ```

4. **Observe Router behavior**:
   - Router should start successfully
   - Router should fall back to CP1 baseline
   - Check logs and metrics

### Expected Results

#### Logs

**Structured fallback log**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "ERROR",
  "component": "router",
  "message": "DevState/CP state failure",
  "context": {
    "cp_fallback": "CP1-baseline",
    "failure": "invalid_schema"
  }
}
```

**Key fields**:
- `"failure": "invalid_schema"` - indicates schema validation failed
- `"cp_fallback": "CP1-baseline"` - fallback occurred

#### Metrics

**Error and fallback metrics incremented**:
- `router_cp_state_errors_total{reason="invalid_schema"}` > 0
- `router_cp_state_fallback_total{cp_fallback="CP1-baseline"}` > 0

#### Router Behavior

- Router starts successfully (no crash)
- Router operates in CP1 baseline mode
- Router processes requests normally (degraded but functional)

### Recovery

To recover from invalid schema:

1. **Restore valid state file**:
   ```bash
   # Re-export from DevState
   make devstate-set-cp CP=CP2-LC
   ```

2. **Restart Router** to load restored state

3. **Verify recovery** (same as Scenario 2)

### References

- **State Schema**: `docs/STATE.schema.json`
- **Fallback Behavior**: `docs/ADR/ADR-015-router-devstate-integration.md#fallback-and-observability`

---

## Verification Checklist

For each scenario, verify:

- [ ] **Router starts successfully** (no crash)
- [ ] **Logs contain expected structured JSON** (with `cp_fallback` for fallback scenarios)
- [ ] **Metrics are incremented** (for error/fallback scenarios)
- [ ] **Router processes requests** (even in fallback mode)
- [ ] **CP1 baseline components are active** (see `docs/CP1_BASELINE.md`)
- [ ] **CP2+ features are disabled** (in fallback mode)

## How to Check Logs

### Structured JSON Logs

Router emits structured JSON logs. To view logs:

```bash
# If using rebar3 shell
# Logs appear in console output

# If using file-based logging
tail -f /path/to/router.log | jq '.'
```

**Key log fields to check**:
- `"level"`: `ERROR` for fallback scenarios
- `"message"`: `"DevState/CP state failure"` for fallback
- `"context.cp_fallback"`: `"CP1-baseline"` when fallback occurs
- `"context.failure"`: Reason (`missing_state`, `decode_error`, `no_drift_violation`, `invalid_schema`)

### Filtering Logs

```bash
# Filter for fallback logs
tail -f router.log | jq 'select(.context.cp_fallback == "CP1-baseline")'

# Filter for specific error reason
tail -f router.log | jq 'select(.context.failure == "missing_state")'
```

## How to Check Metrics

### Via Telemetry Bridge

If Router metrics are exported via Telemetry → Prometheus bridge:

```bash
# Query Prometheus
curl 'http://localhost:9090/api/v1/query?query=router_cp_state_errors_total'

# Or use PromQL in Grafana
sum(rate(router_cp_state_errors_total[5m])) by (reason)
```

### Via Erlang Console

If Router is running in Erlang shell:

```erlang
% Check if telemetry events are emitted
% (requires telemetry_poller or similar bridge)
```

### Expected Metric Values

**Normal operation** (no fallback):
- `router_cp_state_errors_total{reason="*"}` = 0
- `router_cp_state_fallback_total{cp_fallback="CP1-baseline"}` = 0

**Fallback operation**:
- `router_cp_state_errors_total{reason="<reason>"}` > 0
- `router_cp_state_fallback_total{cp_fallback="CP1-baseline"}` > 0

## Troubleshooting

### Router Doesn't Start

**Symptom**: Router crashes on startup

**Possible causes**:
- Missing dependencies (NATS, gRPC)
- Port conflicts
- Configuration errors

**Check**:
- Router logs for crash reason
- Dependencies: `rebar3 deps`
- Port availability: `netstat -tuln | grep 9000` (gRPC port)

### Router Starts But No Logs

**Symptom**: Router starts but no structured logs appear

**Possible causes**:
- Logger not configured
- Log level too high

**Check**:
- Logger configuration in `apps/otp/router/src/beamline_router.app.src`
- Log level: `application:get_env(beamline_router, log_level, info)`

### Metrics Not Incrementing

**Symptom**: Router falls back but metrics don't increment

**Possible causes**:
- Telemetry disabled
- Metrics bridge not configured

**Check**:
- Telemetry enabled: `application:get_env(beamline_router, telemetry_enabled, true)`
- Telemetry bridge configuration (if using Prometheus exporter)

## References

- **DevState Integration ADR**: `docs/ADR/ADR-015-router-devstate-integration.md`
- **DevState Usage**: `devstate/docs/USAGE.md`
- **CP Transitions**: `devstate/docs/USAGE.md#cp-transitions-for-router-quick-how-to`
- **Router Quick Start**: `docs/archive/dev/ROUTER_DEV_QUICKSTART.md`
- **CP1 Baseline**: `docs/CP1_BASELINE.md`
- **Observability Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`
- **State Schema**: `docs/STATE.schema.json`

## Semi-Automated Smoke Tests

Semi-automated smoke test scripts are available for multiple scenarios:

**Script**: `scripts/devstate_router_fallback_smoke.sh`

**Usage**:
```bash
# Show help and available options
./scripts/devstate_router_fallback_smoke.sh --help

# Run default scenario (missing_state)
./scripts/devstate_router_fallback_smoke.sh

# Run specific scenario
./scripts/devstate_router_fallback_smoke.sh --scenario missing_state
./scripts/devstate_router_fallback_smoke.sh --scenario invalid_json
./scripts/devstate_router_fallback_smoke.sh --scenario no_drift_false

# Run all scenarios sequentially
./scripts/devstate_router_fallback_smoke.sh --scenario all
```

**Note**: Use `--help` to see detailed usage information, available scenarios, exit codes, and troubleshooting tips. The script is self-documenting and provides clear error messages for common issues (exit codes 2 and 3).

**Available Scenarios**:

1. **`missing_state`** (default) - Scenario 2: Missing State File
   - Creates valid state file
   - Removes state file (simulates missing state)
   - Verifies Router detects missing state and falls back
   - Restores state file
   - Verifies Router recovers (no fallback)

2. **`invalid_json`** - Scenario 3: Invalid JSON in State File
   - Creates valid state file
   - Corrupts state file with invalid JSON (missing closing brace)
   - Verifies Router detects decode_error and falls back
   - Restores valid state file
   - Verifies Router recovers (no fallback)

3. **`no_drift_false`** - Scenario 4: no_drift=false (Drift Detected)
   - Creates valid state file (CP2-LC)
   - Sets `no_drift=false` (simulates drift detection)
   - Verifies Router detects no_drift_violation and falls back
   - Restores valid state file
   - Verifies Router recovers (no fallback)

**What each scenario does**:
1. Checks prerequisites (DevState running, Router compiled)
2. Backs up existing state file (if present)
3. Sets up valid state file
4. Applies scenario-specific corruption/modification
5. Verifies Router detects expected failure and falls back
6. Restores valid state file
7. Verifies Router recovers (no fallback)
8. Cleans up (restores original state file from backup)

**Exit codes**:
- `0` - Smoke test passed
- `1` - Smoke test failed
- `2` - Prerequisites not met
- `3` - Invalid scenario specified

**CLI Shortcuts**:

| Scenario | CLI Command |
|----------|-------------|
| Scenario 2: Missing State | `./scripts/devstate_router_fallback_smoke.sh --scenario missing_state` |
| Scenario 3: Invalid JSON | `./scripts/devstate_router_fallback_smoke.sh --scenario invalid_json` |
| Scenario 4: no_drift=false | `./scripts/devstate_router_fallback_smoke.sh --scenario no_drift_false` |
| All scenarios | `./scripts/devstate_router_fallback_smoke.sh --scenario all` |

**Note**: This script is for **local testing only**. It modifies `.trae/state.json` temporarily.

## CI Smoke Execution

### Automated CI Smoke Test

The DevState ↔ Router fallback smoke test is integrated into the CI pipeline to provide periodic health checks.

**CI Integration**:
- **Location**: `.github/workflows/ci.yml`
- **Script**: `scripts/devstate_router_fallback_smoke.sh`
- **Execution**: Runs as a separate step in the CI pipeline
- **Failure Behavior**: CI job fails if smoke test fails (non-zero exit code)

**CI Configuration**:
```yaml
- name: DevState ↔ Router Fallback Smoke Test
  working-directory: .
  run: bash scripts/devstate_router_fallback_smoke.sh
  continue-on-error: false
  env:
    DATABASE_URL: ${{ secrets.DATABASE_URL || 'postgresql://devstate:dev_password@localhost:55432/devstate' }}
    HMAC_SECRET: ${{ secrets.HMAC_SECRET || 'dev-secret-not-for-prod' }}
    DEVSTATE_HTTP_PORT: 3080
```

**Coverage**:
The CI smoke test covers the following scenarios from this document:
- DevState service health check
- Router fallback to CP1 baseline on state errors (Scenario 2: missing_state)
- HMAC chain verification
- State export/import validation

**Extended Scenarios** (optional):
To run additional scenarios in CI, modify the CI step to use `--scenario all`:
```yaml
- name: DevState ↔ Router Fallback Smoke Test (Extended)
  working-directory: .
  run: bash scripts/devstate_router_fallback_smoke.sh --scenario all
  continue-on-error: false
  env:
    DATABASE_URL: ${{ secrets.DATABASE_URL || 'postgresql://devstate:dev_password@localhost:55432/devstate' }}
    HMAC_SECRET: ${{ secrets.HMAC_SECRET || 'dev-secret-not-for-prod' }}
    DEVSTATE_HTTP_PORT: 3080
  if: always()  # Run even if previous steps failed
```

**Available Extended Scenarios**:
- Scenario 2: Missing State File (`missing_state`) - default
- Scenario 3: Invalid JSON (`invalid_json`)
- Scenario 4: no_drift=false (`no_drift_false`)

**Limitations**:
- CI smoke test uses test/development configuration (not production)
- Requires DevState service to be available in CI environment
- May be skipped in "fast" CI runs (only runs in full/extended pipelines)

**Local Execution**:
For local testing before pushing:
```bash
# Run smoke test locally
bash scripts/devstate_router_fallback_smoke.sh
```

## Automated Tests

For automated testing, see:
- `apps/otp/router/test/router_state_observability_SUITE.erl` - Automated test suite covering these scenarios

## Runbook: Router CP State Alerts

### Alert: RouterCPStateErrors

**When this alert fires**: Router has detected CP state errors (rate > 0 errors/sec for 5 minutes).

**Steps**:
1. **Check DevState service health**:
   ```bash
   curl -fsS http://localhost:3080/health
   # Expected: {"status":"ok"}
   ```

2. **Verify HMAC chain**:
   ```bash
   bash devstate/scripts/devstate_verify.sh
   # Expected: [OK] HMAC chain verification passed
   ```

3. **Check Router logs** for specific error reason:
   ```bash
   grep "cp_state_errors" router.log | tail -20
   # Look for: reason field in error logs
   ```

4. **If DevState is down**: Start DevState service:
   ```bash
   make devstate-up
   ```

5. **If HMAC chain broken**: Follow recovery procedures in ADR-015.

**Related Documentation**:
- `docs/ADR/ADR-015-router-devstate-integration.md` - DevState integration details
- `docs/archive/dev/ROUTER_DEVSTATE_E2E_SCENARIOS.md#scenario-2-devstate-degradation-missing-state-file` - Degradation scenario

### Alert: RouterCPFallback

**When this alert fires**: Router is falling back to CP1 baseline (rate > 0 fallbacks/sec for 1 minute).

**Steps**:
1. **Verify Router is in CP1 mode**:
   ```bash
   # Check Router logs for CP fallback message
   grep "cp_fallback" router.log | tail -10
   # Expected: "Falling back to CP1 baseline"
   ```

2. **Check DevState state file**:
   ```bash
   cat .trae/state.json | jq '.current_cp, .no_drift'
   # Verify current_cp is valid and no_drift is true
   ```

3. **If state file missing**: Follow Scenario 2 recovery:
   ```bash
   ./scripts/devstate_router_fallback_smoke.sh --scenario missing_state
   ```

4. **If state file invalid**: Export from DevState:
   ```bash
   bash devstate/scripts/devstate_export.sh
   ```

5. **Verify CP2+ features are disabled**:
   - Check Router logs: CP2+ features should not start
   - Verify `router_state:is_cp2_plus_allowed()` returns `false`

**Related Documentation**:
- `docs/ADR/ADR-015-router-devstate-integration.md#cp1-minimal-mode-enforcement` - CP1 enforcement
- `docs/archive/dev/ROUTER_DEVSTATE_E2E_SCENARIOS.md#scenario-2-devstate-degradation-missing-state-file` - Recovery steps

