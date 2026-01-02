# Extensions Pipeline Parallelization - Experimental Plan

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: 📋 **EXPERIMENTAL PLAN** (CP3)  
**Control Point**: CP3  
**WORKER**: `wrk-2` (Router OTP) + `wrk-4` (Docs/Architecture)  
**Related ADR**: `docs/ADR/ADR-024-extensions-pipeline-parallelization.md`

---

## Executive Summary

This document defines a minimal experimental plan for safely parallelizing a subset of the Extensions Pipeline. The experiment focuses on **independent pre-processors without side effects** as the safest starting point, with clear metrics, observability requirements, and a rollout strategy for CP3.

**Goal**: Validate that parallel execution of independent extensions can reduce latency by 50-70% without introducing race conditions or breaking existing functionality.

---

## 1. Experimental Scope

### 1.1. Safe Subset for Initial Experiment

**Target**: **Independent Pre-processors Without Side Effects**

**Rationale**:
- ✅ **Lowest Risk**: Pre-processors operate on original message (no dependencies on previous results)
- ✅ **Clear Isolation**: Each pre-processor reads original message independently
- ✅ **Easy Validation**: Results can be compared with sequential execution
- ✅ **Minimal Impact**: Pre-processors are first in pipeline (failures don't cascade)

**Example Safe Extensions**:
- `normalize_text` - Text normalization (reads original message, no side effects)
- `detect_language` - Language detection (reads original message, no side effects)
- `extract_metadata` - Metadata extraction (reads original message, no side effects)

**Excluded from Initial Experiment**:
- ❌ **Dependent Extensions**: Extensions that depend on results from other extensions
  - Example: `enrich_context` depends on `normalize_text` output
- ❌ **Extensions with Side Effects**: Extensions that modify external state
  - Example: Extensions that call external APIs (billing, logging, metrics)
- ❌ **Validators**: All validators (experiment with pre-processors first)
- ❌ **Post-processors**: All post-processors (experiment with pre-processors first)

### 1.2. Success Criteria

**Primary Metrics**:
- ✅ **Latency Reduction**: 50-70% reduction in total pre-processor latency
  - Example: 3 extensions × 30ms each = 90ms sequential → 30-45ms parallel
- ✅ **No Functional Regressions**: Results identical to sequential execution
- ✅ **No Race Conditions**: No observable race conditions in logs/traces
- ✅ **Error Handling**: Errors handled correctly (fail-closed for required, fail-open for optional)

**Secondary Metrics**:
- ✅ **Throughput**: 2-3x increase in requests/second with parallel pre-processors
- ✅ **Resource Usage**: CPU/memory usage acceptable (no excessive process spawning)
- ✅ **Observability**: Traces/logs remain clear and debuggable

---

## 2. Extension Requirements

### 2.1. Side-Effect-Free Declaration

**New Extension Metadata Field**: `side_effect_free`

**Format** (in Extension Registry):
```json
{
  "id": "normalize_text",
  "type": "pre",
  "subject": "beamline.ext.pre.normalize_text.v1",
  "timeout_ms": 100,
  "retry": 0,
  "enabled": true,
  "metadata": {
    "side_effect_free": true,
    "description": "Text normalization - no external side effects"
  }
}
```

**Database Schema** (PostgreSQL):
```sql
ALTER TABLE extensions ADD COLUMN IF NOT EXISTS side_effect_free BOOLEAN DEFAULT false;
ALTER TABLE extension_versions ADD COLUMN IF NOT EXISTS side_effect_free BOOLEAN DEFAULT false;
```

**Validation Rules**:
- ✅ **`side_effect_free: true`**: Extension declares no external side effects
  - No external API calls (except read-only)
  - No database writes
  - No external logging/metrics (only Router telemetry)
  - No state modification outside Router context
- ⚠️ **`side_effect_free: false` or missing**: Extension may have side effects
  - **Default**: `false` (conservative, sequential execution)
  - **Parallel Execution**: Only allowed if `side_effect_free: true`

**Documentation Requirements**:
- Extension authors must document side effects in extension metadata
- Extension registry UI should show `side_effect_free` status
- Extension developer guide must explain side-effect-free requirements

### 2.2. Dependency Declaration

**New Policy Field**: `depends_on`

**Format** (in Routing Policy):
```json
{
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "depends_on": [],
      "config": {}
    },
    {
      "id": "detect_language",
      "mode": "required",
      "depends_on": [],
      "config": {}
    },
    {
      "id": "enrich_context",
      "mode": "required",
      "depends_on": ["normalize_text"],
      "config": {}
    }
  ]
}
```

**Validation Rules**:
- ✅ **`depends_on: []`**: Extension is independent (can run in parallel)
- ⚠️ **`depends_on: ["ext_id"]`**: Extension depends on another (must run sequentially)
- ❌ **Cyclic Dependencies**: Not allowed (validation error)
- ❌ **Missing Dependencies**: Extension ID in `depends_on` must exist in same pipeline stage

**Default Behavior**:
- **If `depends_on` is missing**: Assume sequential execution (conservative)
- **If `depends_on: []`**: Can run in parallel (if `side_effect_free: true`)

### 2.3. Extension Developer Documentation

**New Section in `docs/EXTENSIONS_DEVELOPER_GUIDE.md`**:

```markdown
## Side-Effect-Free Extensions

To enable parallel execution, extensions must declare `side_effect_free: true` in their registry metadata.

**Requirements for Side-Effect-Free Extensions**:
- ✅ No external API calls (except read-only lookups)
- ✅ No database writes
- ✅ No external logging/metrics (only Router telemetry)
- ✅ No state modification outside Router context
- ✅ Idempotent operations (same input = same output)

**Example Side-Effect-Free Extension**:
- `normalize_text`: Text normalization (pure function)
- `detect_language`: Language detection (pure function)
- `extract_metadata`: Metadata extraction (pure function)

**Example Extension with Side Effects**:
- `billing_tracker`: Calls billing API (has side effects)
- `audit_logger`: Writes to audit log (has side effects)
- `rate_limiter`: Updates rate limit counters (has side effects)
```

---

## 3. Observability Requirements

### 3.1. Metrics

**New Prometheus Metrics** (CP2 observability spec):

```prometheus
# Extension parallel execution metrics
router_extension_parallel_group_total{stage="pre", group_id="<hash>", policy_id="<id>"} <count>
router_extension_parallel_group_duration_seconds{stage="pre", group_id="<hash>", policy_id="<id>"} <duration>
router_extension_parallel_group_size{stage="pre", group_id="<hash>", policy_id="<id>"} <size>
router_extension_parallel_execution_errors_total{stage="pre", group_id="<hash>", policy_id="<id>", error_type="<type>"} <count>
```

**Existing Metrics** (enhanced):
- `router_extension_invocations_total` - Add label `execution_mode="parallel|sequential"`
- `router_extension_invocation_duration_seconds` - Add label `execution_mode="parallel|sequential"`

### 3.2. Traces

**OpenTelemetry Spans** (CP2 tracing spec):

```
router.extension.parallel_group
├── extension_id: "normalize_text"
│   ├── execution_mode: "parallel"
│   ├── group_id: "<hash>"
│   └── latency_ms: 30
├── extension_id: "detect_language"
│   ├── execution_mode: "parallel"
│   ├── group_id: "<hash>"
│   └── latency_ms: 25
└── total_latency_ms: 35  # Max of parallel executions
```

**Span Attributes**:
- `extension.execution_mode`: `"parallel"` or `"sequential"`
- `extension.parallel_group_id`: Hash of extension IDs in parallel group
- `extension.parallel_group_size`: Number of extensions in parallel group
- `extension.dependency_level`: Dependency level (0 = independent, 1+ = depends on level N-1)

### 3.3. Logs

**Structured JSON Logs**:

```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "INFO",
  "component": "router",
  "message": "Parallel pre-processor group executed",
  "context": {
    "policy_id": "policy-123",
    "tenant_id": "tenant-456",
    "stage": "pre",
    "execution_mode": "parallel",
    "parallel_group_id": "abc123",
    "parallel_group_size": 3,
    "extensions": ["normalize_text", "detect_language", "extract_metadata"],
    "total_latency_ms": 35,
    "max_extension_latency_ms": 30,
    "sequential_latency_estimate_ms": 90
  }
}
```

**Error Logs**:
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "ERROR",
  "component": "router",
  "message": "Parallel pre-processor group failed",
  "context": {
    "policy_id": "policy-123",
    "tenant_id": "tenant-456",
    "stage": "pre",
    "execution_mode": "parallel",
    "parallel_group_id": "abc123",
    "failed_extensions": ["normalize_text"],
    "error_type": "timeout",
    "total_latency_ms": 35
  }
}
```

---

## 4. Rollout Plan (CP3)

### 4.1. Feature Flags

**Application Config** (`config/*.config`):
```erlang
{beamline_router, [
    {extension_pipeline, [
        {parallel_execution_enabled, false},  % Master switch
        {parallel_pre_processors_enabled, false},  % Pre-processors parallel execution
        {parallel_validators_enabled, false},  % Validators parallel execution (future)
        {parallel_post_processors_enabled, false},  % Post-processors parallel execution (future)
        {parallel_execution_tenant_allowlist, []},  % Tenant allowlist (empty = all tenants)
        {parallel_execution_policy_allowlist, []},  % Policy allowlist (empty = all policies)
        {parallel_execution_max_group_size, 5},  % Maximum extensions in parallel group
        {parallel_execution_timeout_multiplier, 1.5}  % Timeout multiplier for parallel groups
    ]}
]}.
```

**Environment Variables**:
```bash
EXTENSION_PIPELINE_PARALLEL_EXECUTION_ENABLED=false
EXTENSION_PIPELINE_PARALLEL_PRE_PROCESSORS_ENABLED=false
EXTENSION_PIPELINE_PARALLEL_EXECUTION_TENANT_ALLOWLIST=tenant-1,tenant-2
EXTENSION_PIPELINE_PARALLEL_EXECUTION_POLICY_ALLOWLIST=policy-1,policy-2
EXTENSION_PIPELINE_PARALLEL_EXECUTION_MAX_GROUP_SIZE=5
```

**Runtime Control** (via Admin API):
```erlang
%% Enable parallel execution for specific tenant
router_admin:set_parallel_execution_tenant("tenant-1", true).

%% Disable parallel execution for specific tenant
router_admin:set_parallel_execution_tenant("tenant-1", false).

%% Get parallel execution status
router_admin:get_parallel_execution_status().
%% Returns: #{enabled => true, tenants => [...], policies => [...]}
```

### 4.2. Rollout Phases

#### Phase 1: Development/Testing (CP3 Pre-Release)

**Duration**: 2-3 weeks

**Actions**:
1. ✅ Implement parallel execution framework (dependency graph, parallel spawn)
2. ✅ Add `side_effect_free` metadata field to Extension Registry
3. ✅ Add `depends_on` field to policy schema
4. ✅ Implement feature flags (config + runtime)
5. ✅ Add observability (metrics, traces, logs)
6. ✅ Unit tests for parallel execution
7. ✅ Integration tests with 2-3 independent pre-processors
8. ✅ Chaos tests (simulate failures, timeouts, race conditions)

**Success Criteria**:
- ✅ All tests pass
- ✅ No functional regressions
- ✅ Observability working correctly
- ✅ Feature flags working correctly

#### Phase 2: Internal Testing (CP3 Pre-Release)

**Duration**: 1-2 weeks

**Actions**:
1. ✅ Enable parallel execution for internal test tenant
2. ✅ Monitor metrics and logs for 1 week
3. ✅ Compare results with sequential execution (A/B test)
4. ✅ Validate latency reduction (50-70%)
5. ✅ Validate no race conditions or functional regressions

**Success Criteria**:
- ✅ Latency reduction achieved (50-70%)
- ✅ No race conditions observed
- ✅ No functional regressions
- ✅ Observability clear and debuggable

#### Phase 3: Canary Rollout (CP3)

**Duration**: 2-3 weeks

**Actions**:
1. ✅ Enable parallel execution for 1-2 production tenants (low traffic)
2. ✅ Monitor metrics and logs for 1 week
3. ✅ Compare results with sequential execution (A/B test)
4. ✅ Validate latency reduction and error rates
5. ✅ Gradually expand to 5-10 tenants

**Success Criteria**:
- ✅ Latency reduction achieved (50-70%)
- ✅ Error rates unchanged or improved
- ✅ No customer complaints
- ✅ Observability clear and debuggable

#### Phase 4: Full Rollout (CP3+)

**Duration**: 2-4 weeks

**Actions**:
1. ✅ Enable parallel execution for all tenants (via feature flag)
2. ✅ Monitor metrics and logs for 2 weeks
3. ✅ Validate latency reduction and error rates
4. ✅ Document lessons learned

**Success Criteria**:
- ✅ Latency reduction achieved (50-70%)
- ✅ Error rates unchanged or improved
- ✅ No customer complaints
- ✅ Full observability coverage

### 4.3. Rollback Strategy

**Automatic Rollback Triggers**:
- ❌ **Error Rate Increase**: Error rate increases by > 10% (compared to sequential)
- ❌ **Latency Degradation**: P95 latency increases by > 20% (compared to sequential)
- ❌ **Race Condition Detected**: Any race condition observed in logs/traces
- ❌ **Functional Regression**: Any functional regression reported

**Rollback Procedure**:
1. **Immediate**: Disable parallel execution via feature flag
   ```erlang
   application:set_env(beamline_router, extension_pipeline, [
       {parallel_execution_enabled, false}
   ]).
   ```
2. **Per-Tenant Rollback**: Disable for specific tenant
   ```erlang
   router_admin:set_parallel_execution_tenant("tenant-1", false).
   ```
3. **Per-Policy Rollback**: Remove from policy allowlist
4. **Full Rollback**: Set `parallel_execution_enabled = false` in config

**Rollback Verification**:
- ✅ Verify sequential execution restored (check logs/metrics)
- ✅ Verify error rates return to baseline
- ✅ Verify latency returns to baseline
- ✅ Document rollback reason and lessons learned

### 4.4. Monitoring and Alerting

**Key Metrics to Monitor**:
- `router_extension_parallel_group_duration_seconds` (P50, P95, P99)
- `router_extension_parallel_execution_errors_total` (by error_type)
- `router_extension_invocations_total{execution_mode="parallel"}` (success rate)
- Comparison: Parallel vs Sequential latency (side-by-side)

**Alert Rules** (Prometheus):
```yaml
- alert: ExtensionParallelExecutionHighErrorRate
  expr: rate(router_extension_parallel_execution_errors_total[5m]) > 0.1
  for: 5m
  annotations:
    summary: "High error rate in parallel extension execution"

- alert: ExtensionParallelExecutionLatencyDegradation
  expr: |
    (
      histogram_quantile(0.95, router_extension_parallel_group_duration_seconds) /
      histogram_quantile(0.95, router_extension_sequential_group_duration_seconds)
    ) > 1.2
  for: 10m
  annotations:
    summary: "Parallel execution latency degraded compared to sequential"
```

---

## 5. Implementation Details

### 5.1. Dependency Graph Builder

**Module**: `router_extension_dependency_graph.erl`

**Functions**:
```erlang
-spec build_dependency_graph([map()]) -> {ok, dependency_graph()} | {error, term()}.
build_dependency_graph(Extensions) ->
    %% Parse depends_on declarations
    %% Build dependency graph
    %% Validate graph (no cycles, all dependencies exist)
    %% Topological sort for execution order
    %% Return dependency levels (0 = independent, 1+ = depends on level N-1)
    ...
```

**Dependency Graph Structure**:
```erlang
-type dependency_graph() :: #{
    level_0 => [extension_id()],  % Independent extensions (can run in parallel)
    level_1 => [extension_id()],  % Depends on level_0 (run after level_0)
    level_2 => [extension_id()],  % Depends on level_1 (run after level_1)
    ...
}.
```

### 5.2. Parallel Execution Framework

**Module**: `router_extension_parallel_executor.erl`

**Functions**:
```erlang
-spec execute_parallel_group([extension_id()], map(), map()) -> 
    {ok, [map()], map()} | {error, term()}.
execute_parallel_group(ExtensionIds, Request, Context) ->
    %% Spawn processes for each extension
    %% Wait for all to complete (or first failure for required extensions)
    %% Merge results (handle conflicts)
    %% Return merged payload and context
    ...
```

**Implementation**:
- Use `spawn_link` for each extension (linked to parent process)
- Use `receive` with timeout for each extension
- Aggregate results (merge payload and context)
- Handle errors (fail-closed for required, fail-open for optional)

### 5.3. Context Merge Logic

**Module**: `router_extension_context_merger.erl`

**Functions**:
```erlang
-spec merge_parallel_results([map()], map()) -> {ok, map(), map()}.
merge_parallel_results(Results, OriginalContext) ->
    %% Merge payloads (handle conflicts - last wins or merge strategy)
    %% Merge metadata (handle conflicts - last wins or merge strategy)
    %% Preserve execution order in logs/traces
    %% Return merged payload and context
    ...
```

**Conflict Resolution**:
- **Payload Conflicts**: Last extension wins (deterministic order)
- **Metadata Conflicts**: Last extension wins (deterministic order)
- **Log Conflicts**: All extensions logged (preserve execution order)

---

## 6. Testing Strategy

### 6.1. Unit Tests

**Test Suite**: `router_extension_parallel_executor_SUITE.erl`

**Test Cases**:
- ✅ Execute 2 independent pre-processors in parallel
- ✅ Execute 3 independent pre-processors in parallel
- ✅ Handle timeout in parallel group (fail-closed for required)
- ✅ Handle timeout in parallel group (fail-open for optional)
- ✅ Handle error in parallel group (fail-closed for required)
- ✅ Handle error in parallel group (fail-open for optional)
- ✅ Context merge (no conflicts)
- ✅ Context merge (with conflicts - last wins)
- ✅ Dependency graph building (no cycles)
- ✅ Dependency graph building (with cycles - error)
- ✅ Dependency graph building (missing dependencies - error)

### 6.2. Integration Tests

**Test Suite**: `router_extensions_pipeline_parallel_SUITE.erl`

**Test Cases**:
- ✅ Full pipeline with 2 parallel pre-processors (sequential validators/post)
- ✅ Full pipeline with 3 parallel pre-processors (sequential validators/post)
- ✅ Full pipeline with mixed parallel/sequential pre-processors
- ✅ Compare results with sequential execution (A/B test)
- ✅ Error handling (required extension fails)
- ✅ Error handling (optional extension fails)
- ✅ Observability (metrics, traces, logs)

### 6.3. Chaos Tests

**Test Suite**: `router_extensions_chaos_parallel_SUITE.erl`

**Test Cases**:
- ✅ Simulate NATS failure during parallel execution
- ✅ Simulate extension timeout during parallel execution
- ✅ Simulate extension error during parallel execution
- ✅ Simulate race condition (if possible)
- ✅ Simulate high load with parallel execution

---

## 7. Documentation Updates

### 7.1. Extension Developer Guide

**File**: `docs/EXTENSIONS_DEVELOPER_GUIDE.md`

**New Section**: "Side-Effect-Free Extensions for Parallel Execution"

### 7.2. Routing Policy Documentation

**File**: `docs/ROUTING_POLICY.md`

**New Section**: "Parallel Execution with `depends_on`"

### 7.3. Operations Runbook

**File**: `apps/otp/router/docs/EXTENSIONS_RUNBOOK.md`

**New Section**: "Parallel Execution Monitoring and Rollback"

### 7.4. ADR Update

**File**: `docs/ADR/ADR-024-extensions-pipeline-parallelization.md`

**Update Status**: `proposed` → `accepted` (after experiment validation)

---

## 8. Acceptance Criteria

### 8.1. Code Implementation

- ✅ Parallel execution framework implemented
- ✅ Dependency graph builder implemented
- ✅ Context merge logic implemented
- ✅ Feature flags implemented
- ✅ Observability (metrics, traces, logs) implemented
- ✅ Unit tests pass
- ✅ Integration tests pass
- ✅ Chaos tests pass

### 8.2. Documentation

- ✅ Extension developer guide updated
- ✅ Routing policy documentation updated
- ✅ Operations runbook updated
- ✅ ADR updated (status: accepted)

### 8.3. Rollout

- ✅ Phase 1 complete (development/testing)
- ✅ Phase 2 complete (internal testing)
- ✅ Phase 3 complete (canary rollout)
- ✅ Phase 4 complete (full rollout)

### 8.4. Success Metrics

- ✅ Latency reduction: 50-70%
- ✅ No functional regressions
- ✅ No race conditions
- ✅ Error rates unchanged or improved
- ✅ Observability clear and debuggable

---

## 9. Risks and Mitigations

### 9.1. Race Conditions

**Risk**: Parallel execution may cause race conditions if extensions have side effects.

**Mitigation**:
- ✅ Require `side_effect_free: true` declaration
- ✅ Validate side-effect-free status in registry
- ✅ Document side-effect-free requirements clearly
- ✅ Test with chaos scenarios

### 9.2. Context Merge Conflicts

**Risk**: Multiple extensions may modify same metadata keys, causing conflicts.

**Mitigation**:
- ✅ Last extension wins (deterministic order)
- ✅ Log conflicts for debugging
- ✅ Document merge strategy clearly
- ✅ Test with conflict scenarios

### 9.3. Observability Complexity

**Risk**: Parallel execution makes logs/traces harder to debug.

**Mitigation**:
- ✅ Enhanced tracing with execution graph
- ✅ Structured logging with parallel group IDs
- ✅ Correlation IDs for parallel executions
- ✅ Clear documentation of observability enhancements

### 9.4. Rollback Complexity

**Risk**: Rollback may be complex if issues are discovered in production.

**Mitigation**:
- ✅ Feature flags for immediate rollback
- ✅ Per-tenant rollback capability
- ✅ Per-policy rollback capability
- ✅ Clear rollback procedures documented

---

## 10. Timeline

### CP3 Pre-Release (Weeks 1-6)

- **Week 1-2**: Implementation (parallel execution framework, dependency graph, context merge)
- **Week 3**: Testing (unit, integration, chaos)
- **Week 4**: Internal testing (1-2 test tenants)
- **Week 5**: Canary rollout (5-10 production tenants)
- **Week 6**: Full rollout (all tenants)

### CP3 (Weeks 7-12)

- **Week 7-8**: Monitor and optimize
- **Week 9-10**: Expand to validators (if pre-processors successful)
- **Week 11-12**: Expand to post-processors (if validators successful)

---

## 11. References

- `docs/ADR/ADR-024-extensions-pipeline-parallelization.md` - ADR with detailed analysis
- `docs/archive/dev/EXTENSIONS_PIPELINE_PERF_REPORT.md` - Performance report with parallelization recommendation
- `docs/EXTENSIONS_API.md` - Extensions API contract
- `apps/otp/router/src/router_decider.erl` - Current pipeline implementation
- `docs/archive/dev/EXTENSIONS_CHAOS_RESILIENCE_REPORT.md` - Chaos/resilience testing
- `docs/archive/dev/OBSERVABILITY_TRACING_SPEC_CP2.md` - CP2 tracing specification
- `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - CP2 metrics specification

---

## 12. Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy
- ✅ Respects security constraints
- ✅ Integrates with STATE/HISTORY (no changes needed)

---

## Change History

**v1.0 (2025-01-27)**:
- Initial experimental plan
- Safe subset: independent pre-processors without side effects
- Observability requirements defined
- Rollout plan for CP3
- Feature flags and rollback strategy

---

**WORKER**: `wrk-2` (Router OTP) + `wrk-4` (Docs)  
**Control Point**: CP3  
**Status**: 📋 **EXPERIMENTAL PLAN**

