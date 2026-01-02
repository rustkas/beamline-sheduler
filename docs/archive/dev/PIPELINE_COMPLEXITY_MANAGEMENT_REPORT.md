# Pipeline Complexity Management Implementation Report

**Version**: CP2-LC  
**Date**: 2025-01-27  
**Status**: ✅ Complete  
**Workers**: wrk-2 (Router OTP) + wrk-5 (UI-Web)

## Summary

Implemented pipeline complexity management with soft limits, warnings, and UI visualization to help users understand and optimize their extension pipelines based on performance recommendations from PERF_REPORT.

## Tasks Completed

### ✅ Router (wrk-2) - Complexity Calculation & Warnings

**1. Pipeline Complexity Calculation**:
- ✅ Added `calculate_pipeline_complexity/3` function in `router_decider.erl`
- Calculates complexity score (0-100) based on:
  - Total extension count
  - Per-type extension counts (pre/validators/post)
  - Estimated latency (30ms per extension)
- Determines complexity level: `low`, `medium`, `high`, `very_high`

**2. Soft Limits & Recommendations**:
- ✅ Recommended limits from PERF_REPORT:
  - Maximum total: 4 extensions
  - Maximum pre: 2 extensions
  - Maximum validators: 2 extensions
  - Maximum post: 2 extensions
- ✅ Generates warnings when limits exceeded
- ✅ Generates recommendations for optimization

**3. Logging & Metrics**:
- ✅ Added `log_pipeline_complexity/3` function
- Logs warnings for `high` complexity
- Logs errors for `very_high` complexity
- Emits telemetry metrics: `[router_pipeline, complexity]`
- Logs recommendations when available

**4. Complexity Endpoint**:
- ✅ Added `get_pipeline_complexity/2` function in `router_decider.erl`
- ✅ Added `get_pipeline_complexity/2` RPC method in `router_admin_grpc.erl`
- Returns complexity assessment for a policy

**Files Modified**:
- `apps/otp/router/src/router_decider.erl`:
  - Added `calculate_pipeline_complexity/3`
  - Added `get_pipeline_complexity/2`
  - Added `log_pipeline_complexity/3`
  - Added complexity calculation in `decide/3`
  - Added helper functions: `calculate_complexity_score/4`, `generate_complexity_warnings/8`, `generate_complexity_recommendations/7`
- `apps/otp/router/src/router_admin_grpc.erl`:
  - Added `get_pipeline_complexity/2` RPC method

### ✅ UI-Web (wrk-5) - Complexity Visualization

**1. Complexity Assessment Display**:
- ✅ Added `pipeline_complexity` assign in LiveView
- ✅ Added `load_pipeline_complexity/1` function
- ✅ Added `render_complexity_assessment/1` function
- Displays:
  - Complexity score and level (with color coding)
  - Total extensions count
  - Estimated latency
  - Recommended limits
  - Warnings (yellow box)
  - Recommendations (blue box)

**2. Visual Indicators**:
- ✅ Color-coded complexity levels:
  - `low`: Green
  - `medium`: Yellow
  - `high`: Orange
  - `very_high`: Red
- ✅ Warning boxes for exceeded limits
- ✅ Recommendation boxes for optimization suggestions

**3. Mock Endpoint**:
- ✅ Added `GET /api/v1/policies/:tenant_id/:policy_id/complexity` in `mock_gateway.ex`

**Files Modified**:
- `apps/ui_web/lib/ui_web_web/live/extensions_pipeline_live.ex`:
  - Added complexity assessment section
  - Added complexity loading and rendering
- `apps/ui_web/test/support/mock_gateway.ex`:
  - Added mock complexity endpoint

### ✅ Documentation

**Updated**:
- ✅ `docs/ARCHITECTURE/api-registry.md`:
  - Added `GET /api/v1/policies/:tenant_id/:policy_id/complexity` endpoint
  - Documented request/response DTOs
  - Documented complexity levels

**Created**:
- ✅ `docs/archive/dev/PIPELINE_COMPLEXITY_MANAGEMENT_REPORT.md` - This report

## Complexity Calculation Details

### Score Calculation (0-100)

**Base Score (0-60 points)**:
- Based on total extension count vs recommended maximum (4)
- Formula: `(count / 4) * 60` (capped at 60)

**Penalties**:
- Per-type limit exceeded: +5 points each (max 15)
- Total count exceeded: +4 points per extension over limit (max 20)

**Final Score**: `min(100, BaseScore + Penalties)`

### Complexity Levels

- **Low** (0-29): Simple pipeline, optimal performance
- **Medium** (30-59): Acceptable complexity, minor optimizations possible
- **High** (60-79): Complex pipeline, may cause latency issues
- **Very High** (80-100): Very complex pipeline, significant performance impact

### Estimated Latency

Based on PERF_REPORT findings:
- **Formula**: `TotalExtensions * 30ms`
- Accounts for sequential execution and NATS overhead
- P95 target: < 150ms for 3 extensions

## Warnings & Recommendations

### Warnings Generated

1. **Total Count Exceeded**:
   - Trigger: Total extensions > 4
   - Message: "Pipeline has X extensions, recommended maximum is 4. This may cause high latency."

2. **Pre-processors Exceeded**:
   - Trigger: Pre count > 2
   - Message: "Pre-processors count (X) exceeds recommended limit (2). Consider reducing or parallelizing."

3. **Validators Exceeded**:
   - Trigger: Validators count > 2
   - Message: "Validators count (X) exceeds recommended limit (2). This may slow down request processing."

4. **Post-processors Exceeded**:
   - Trigger: Post count > 2
   - Message: "Post-processors count (X) exceeds recommended limit (2). Consider reducing or parallelizing."

5. **Latency Exceeded**:
   - Trigger: Estimated latency > 150ms
   - Message: "Estimated pipeline latency (Xms) exceeds recommended P95 target (150ms). Consider optimizing."

### Recommendations Generated

1. **Reduce Total Extensions**:
   - "Consider reducing total extensions to 4 or fewer for optimal performance"

2. **Reduce Pre-processors**:
   - "Reduce pre-processors to 2 or fewer, or consider parallel execution"

3. **Reduce Validators**:
   - "Reduce validators to 2 or fewer for faster validation"

4. **Reduce Post-processors**:
   - "Reduce post-processors to 2 or fewer, or consider parallel execution"

## Logging & Metrics

### Logging

**Warning Level** (complexity = `high`):
```json
{
  "level": "WARNING",
  "message": "Pipeline complexity is high",
  "policy_id": "default",
  "tenant_id": "tenant_dev",
  "total_extensions": 5,
  "estimated_latency_ms": 150,
  "complexity_score": 65,
  "warnings": [...]
}
```

**Error Level** (complexity = `very_high`):
```json
{
  "level": "ERROR",
  "message": "Pipeline complexity is very high",
  "policy_id": "default",
  "tenant_id": "tenant_dev",
  "total_extensions": 6,
  "estimated_latency_ms": 180,
  "complexity_score": 85,
  "warnings": [...]
}
```

**Info Level** (recommendations available):
```json
{
  "level": "INFO",
  "message": "Pipeline complexity recommendations",
  "policy_id": "default",
  "tenant_id": "tenant_dev",
  "recommendations": [...]
}
```

### Telemetry Metrics

**Event**: `[router_pipeline, complexity]`

**Measurements**:
- `complexity_score`: Complexity score (0-100)
- `total_extensions`: Total extension count
- `estimated_latency_ms`: Estimated latency in milliseconds

**Metadata**:
- `policy_id`: Policy identifier
- `tenant_id`: Tenant identifier
- `complexity_level`: Complexity level (low/medium/high/very_high)

## UI Features

### Complexity Assessment Section

**Displays**:
1. **Complexity Score**: Score (0-100) with color-coded level badge
2. **Total Extensions**: Count of all extensions
3. **Estimated Latency**: Estimated latency in milliseconds
4. **Recommended Limits**: Maximum recommended counts per type
5. **Warnings**: Yellow warning box with exceeded limits
6. **Recommendations**: Blue recommendation box with optimization suggestions

**Visual Design**:
- Color-coded complexity levels for quick assessment
- Clear warnings for exceeded limits
- Actionable recommendations for optimization
- Positioned at top of page for visibility

## Acceptance Criteria

### ✅ Router

- ✅ Soft limits implemented (recommended max: 4 total, 2 per type)
- ✅ Warnings generated when limits exceeded
- ✅ Logging at appropriate levels (warning/error/info)
- ✅ Telemetry metrics emitted
- ✅ Complexity endpoint available

### ✅ UI-Web

- ✅ Complexity assessment displayed on inspector page
- ✅ Visual indicators (color-coded levels)
- ✅ Warnings shown when limits exceeded
- ✅ Recommendations displayed for optimization
- ✅ Mock endpoint available for testing

### ✅ Documentation

- ✅ API endpoint documented in `api-registry.md`
- ✅ Implementation report created

## Files Created/Modified

### Created
- `docs/archive/dev/PIPELINE_COMPLEXITY_MANAGEMENT_REPORT.md` - This report

### Modified
- `apps/otp/router/src/router_decider.erl`:
  - Added complexity calculation functions
  - Added logging function
  - Integrated complexity check in `decide/3`
- `apps/otp/router/src/router_admin_grpc.erl`:
  - Added `get_pipeline_complexity/2` RPC method
- `apps/ui_web/lib/ui_web_web/live/extensions_pipeline_live.ex`:
  - Added complexity assessment display
  - Added complexity loading
- `apps/ui_web/test/support/mock_gateway.ex`:
  - Added mock complexity endpoint
- `docs/ARCHITECTURE/api-registry.md`:
  - Added complexity endpoint documentation

## Next Steps

1. **Gateway Integration**: Add REST endpoint `/api/v1/policies/:tenant_id/:policy_id/complexity` in Gateway
2. **Production API**: Connect UI to real Router API (via Gateway)
3. **Enhanced Metrics**: Add Prometheus metrics for complexity tracking
4. **Alerting**: Add alert rules for very_high complexity policies

## References

- `docs/archive/dev/EXTENSIONS_PIPELINE_PERF_REPORT.md` - Performance report with recommendations
- `apps/otp/router/src/router_decider.erl` - Complexity calculation
- `apps/ui_web/lib/ui_web_web/live/extensions_pipeline_live.ex` - UI implementation
- `docs/ARCHITECTURE/api-registry.md` - API documentation

