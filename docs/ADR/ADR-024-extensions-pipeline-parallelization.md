---
version: 1.0
status: proposed
date: 2025-01-27
deciders:
  - wrk-2: Router OTP (Architecture/Tech Lead)
  - wrk-4: Architecture/Docs
related_adrs:
  - ADR-023: Remove provider, use extensions
supersedes: []
superseded_by: []
---

# ADR-024: Extensions Pipeline Parallelization Strategy

## Status

**proposed**

## Context

### Current Situation

The Extensions Pipeline executes **strictly sequentially**:

```
Request → pre[0] → pre[1] → ... → validators[0] → validators[1] → ... → 
provider → post[0] → post[1] → ... → Response
```

**Current Implementation** (`router_decider.erl`):
- `execute_pre_processors/3`: Executes pre-processors one by one, passing result to next
- `execute_validators/3`: Executes validators one by one, stops on first rejection
- `execute_post_processors/3`: Executes post-processors one by one, passing result to next

**Performance Impact** (from `EXTENSIONS_PIPELINE_PERF_REPORT.md`):
- **Sequential Execution**: Total latency = sum of all extension latencies
- **Example**: 3 extensions with 30ms each = 90ms total latency
- **Scales linearly**: More extensions = proportionally higher latency
- **Bottleneck**: Sequential execution is identified as main performance bottleneck

### Problem Statement

**Performance Requirement**:
- Target SLO: P95 < 150ms for 3 extensions
- Current: Sequential execution may exceed SLO with multiple extensions
- Recommendation from PERF_REPORT: "Consider parallel execution for independent extensions"

**Constraints**:
- Extensions may have **dependencies** (order matters)
- Extensions may have **side effects** (modify shared context)
- Extensions may have **observability requirements** (linear execution is easier to debug)
- Extensions communicate via **NATS** (network latency adds overhead)

### Stakeholders Affected

- **wrk-2 (Router OTP)**: Implementation complexity
- **wrk-4 (Architecture/Docs)**: Architecture decisions
- **Extension Developers**: Need to understand execution model
- **SRE/Ops**: Need to debug and monitor pipeline behavior
- **End Users**: Latency and reliability

### Technical Context

**Current Pipeline Flow**:
1. **Pre-processors**: Modify/enrich incoming message
   - Each pre-processor receives message from previous
   - Result is passed to next pre-processor
   - Context is merged: `maps:merge(Context, NewMetadata)`

2. **Validators**: Decide whether processing can continue
   - Execute sequentially until first rejection
   - Early exit on `status = "reject"` with `on_fail = "block"`
   - All validators must pass (or be ignored) to continue

3. **Provider Selection**: Choose provider based on weights/fallback
   - Not parallelizable (single provider per request)

4. **Post-processors**: Modify provider response
   - Each post-processor receives response from previous
   - Result is passed to next post-processor
   - Context is merged similarly to pre-processors

**Observability**:
- Current: Linear execution = clear trace/log order
- Each extension call emits telemetry events
- Logs show sequential execution: `pre[0] → pre[1] → validator[0] → ...`

**Error Handling**:
- Current: Fail-fast on required extensions
- Fail-open on optional extensions
- Error propagation is straightforward (sequential)

---

## Decision

**Status**: **PROPOSED** (not yet implemented)

This ADR proposes **two alternative approaches** for pipeline parallelization:

### Option A: Strictly Sequential Pipeline (Status Quo + Optimizations)

**Decision**: Keep strictly sequential execution, optimize through:
- Aggressive caching (reduce extension call overhead)
- Timeout tuning (reduce wait time for slow extensions)
- Circuit breaker improvements (fail-fast on known failures)
- Extension registry optimization (already fast, maintain)

**Rationale**:
- **Simplicity**: No architectural changes needed
- **Predictability**: Deterministic execution order
- **Debuggability**: Linear execution is easier to trace
- **Side Effects**: No risk of race conditions or order-dependent bugs
- **Observability**: Clear trace/log order

**Implementation Approach**:
- No code changes to pipeline execution
- Focus on:
  - Extension registry caching (already optimized)
  - Circuit breaker tuning (lower thresholds, faster recovery)
  - Timeout optimization (adaptive timeouts per extension type)
  - Health-based routing (skip unhealthy extensions faster)

**Dependencies**:
- Circuit breaker improvements (CP3)
- Health monitoring improvements (CP3)
- Adaptive timeout configuration

### Option B: Parallel Extension Groups with Strict Constraints

**Decision**: Allow parallel execution within groups (pre/validators/post) with strict constraints:
- **Pre-processors**: Can run in parallel if declared independent
- **Validators**: Can run in parallel (all must pass)
- **Post-processors**: Can run in parallel if declared independent
- **Constraints**: Explicit dependency declaration required

**Rationale**:
- **Performance**: Latency reduction (50-70% for multiple extensions)
- **Throughput**: 2-3x increase for multiple extensions
- **Flexibility**: Supports both sequential and parallel execution

**Implementation Approach**:
1. **Dependency Declaration**:
   - Add `depends_on` field to extension policy config
   - Example: `{"id": "normalize_text", "depends_on": []}` (independent)
   - Example: `{"id": "enrich_context", "depends_on": ["normalize_text"]}` (depends on normalize_text)

2. **Parallel Execution**:
   - Build dependency graph from `depends_on` declarations
   - Execute extensions in parallel within same dependency level
   - Wait for all dependencies before executing next level

3. **Context Merging**:
   - Merge results from parallel extensions deterministically
   - Handle conflicts (same metadata key from multiple extensions)
   - Preserve execution order in logs/traces

4. **Error Handling**:
   - If any required extension fails, fail entire group
   - If any optional extension fails, continue with successful ones
   - Aggregate errors from parallel executions

**Dependencies**:
- Dependency graph implementation
- Parallel execution framework (Erlang processes/spawn)
- Context merge conflict resolution
- Enhanced observability (parallel execution traces)

---

## Consequences

### Option A: Strictly Sequential (Status Quo + Optimizations)

#### Positive

- ✅ **Simplicity**: No architectural changes
- ✅ **Predictability**: Deterministic execution order
- ✅ **Debuggability**: Linear execution is easier to trace
- ✅ **No Side Effects**: No risk of race conditions
- ✅ **Observability**: Clear trace/log order
- ✅ **Lower Risk**: No new failure modes
- ✅ **Faster Implementation**: Can be done incrementally

#### Negative

- ❌ **Latency**: Still scales linearly with number of extensions
- ❌ **Performance Limit**: Cannot achieve < 50ms for 3 extensions (each 30ms)
- ❌ **Throughput**: Limited by sequential execution
- ❌ **Scalability**: Adding more extensions increases latency proportionally

#### Neutral

- **Optimization Focus**: Caching, timeouts, circuit breakers
- **SLO Compliance**: May require limiting extensions to 2-3 per pipeline
- **Future Migration**: Can migrate to Option B later if needed

### Option B: Parallel Extension Groups

#### Positive

- ✅ **Performance**: 50-70% latency reduction for multiple extensions
- ✅ **Throughput**: 2-3x increase for multiple extensions
- ✅ **Scalability**: Can support more extensions in pipeline
- ✅ **SLO Compliance**: Can achieve < 50ms for 3 extensions (parallel)

#### Negative

- ❌ **Complexity**: Dependency graph, parallel execution, context merging
- ❌ **Debuggability**: Harder to trace parallel execution
- ❌ **Side Effects**: Risk of race conditions if dependencies not declared
- ❌ **Observability**: Need enhanced tracing for parallel execution
- ❌ **Error Handling**: More complex (aggregate errors from parallel executions)
- ❌ **Implementation Time**: Significant development effort

#### Neutral

- **Dependency Declaration**: Requires extension authors to declare dependencies
- **Backward Compatibility**: Sequential execution still supported (default)
- **Migration Path**: Can enable parallelization per-policy or per-extension

---

## Alternatives Considered

### Alternative 1: Strictly Sequential Pipeline (Status Quo + Optimizations)

**Description**: Keep sequential execution, optimize through caching, timeouts, circuit breakers.

**Pros**:
- No architectural changes
- Predictable execution order
- Easy to debug and trace
- No risk of race conditions
- Faster to implement

**Cons**:
- Latency scales linearly
- Cannot achieve < 50ms for 3 extensions
- Limited throughput improvement

**Why not chosen**: **PROPOSED AS OPTION A** (recommended for CP2-LC)

**Implementation Notes**:
- Focus on circuit breaker tuning (lower thresholds)
- Adaptive timeout configuration
- Health-based routing improvements
- Extension registry optimization (already fast)

### Alternative 2: Parallel Extension Groups with Strict Constraints

**Description**: Allow parallel execution within groups (pre/validators/post) with explicit dependency declaration.

**Pros**:
- 50-70% latency reduction
- 2-3x throughput increase
- Can support more extensions
- Backward compatible (sequential default)

**Cons**:
- High implementation complexity
- Requires dependency declaration
- Harder to debug
- Risk of race conditions

**Why not chosen**: **PROPOSED AS OPTION B** (future consideration for CP3+)

**Implementation Notes**:
- Dependency graph from `depends_on` field
- Parallel execution with Erlang processes
- Context merge conflict resolution
- Enhanced observability

### Alternative 3: Hybrid Approach (Sequential by Default, Parallel Opt-In)

**Description**: Sequential by default, allow parallel execution via policy flag.

**Pros**:
- Backward compatible
- Opt-in parallelization
- Gradual migration path

**Cons**:
- Two execution modes to maintain
- Complexity in supporting both
- May confuse users

**Why not chosen**: Adds complexity without clear benefit over Option B

### Alternative 4: Full Parallelization (All Extensions in Parallel)

**Description**: Execute all extensions (pre, validators, post) in parallel.

**Pros**:
- Maximum performance gain
- Simplest parallel model

**Cons**:
- Ignores dependencies (pre must run before validators)
- Context conflicts (multiple extensions modify same fields)
- Invalid for current pipeline semantics

**Why not chosen**: Violates pipeline semantics (pre → validators → provider → post)

---

## Detailed Analysis

### What Can Be Parallelized?

#### Pre-processors

**Current**: Sequential execution, each receives result from previous.

**Parallelization Potential**:
- ✅ **Independent pre-processors**: Can run in parallel
  - Example: `normalize_text` and `detect_language` (both read original message)
- ❌ **Dependent pre-processors**: Must run sequentially
  - Example: `normalize_text` → `enrich_context` (enrich depends on normalized text)

**Risks**:
- **Context Merging**: Multiple extensions modify same metadata keys
- **Order Dependencies**: Some extensions may depend on others' results
- **Side Effects**: Extensions may have external side effects (logging, metrics)

**Solution**: Explicit `depends_on` declaration in policy.

#### Validators

**Current**: Sequential execution, early exit on first rejection.

**Parallelization Potential**:
- ✅ **All validators**: Can run in parallel (all must pass)
  - Example: `pii_guard` and `content_filter` (both check different aspects)
- ⚠️ **Early Exit**: If any validator rejects, stop all others (fail-fast)

**Risks**:
- **Early Exit**: Need to cancel in-flight validators when one rejects
- **Error Aggregation**: Multiple validators may reject simultaneously
- **Order Independence**: Validators should be order-independent (current design)

**Solution**: Parallel execution with cancellation on first rejection.

#### Post-processors

**Current**: Sequential execution, each receives result from previous.

**Parallelization Potential**:
- ✅ **Independent post-processors**: Can run in parallel
  - Example: `mask_pii` and `format_response` (both read provider response)
- ❌ **Dependent post-processors**: Must run sequentially
  - Example: `mask_pii` → `add_metadata` (add_metadata depends on masked response)

**Risks**:
- **Context Merging**: Multiple extensions modify same response fields
- **Order Dependencies**: Some extensions may depend on others' results
- **Side Effects**: Extensions may have external side effects

**Solution**: Explicit `depends_on` declaration in policy.

### Risks: Order, Side Effects, Logging

#### Order Dependencies

**Problem**: Extensions may depend on execution order.

**Examples**:
- `normalize_text` must run before `enrich_context` (enrich uses normalized text)
- `mask_pii` must run before `add_metadata` (metadata uses masked response)

**Solution (Option B)**:
- Explicit `depends_on` declaration in policy
- Dependency graph validation
- Sequential execution for dependent extensions

**Mitigation (Option A)**:
- Document recommended order in policy
- Validation warnings for suspicious orders

#### Side Effects

**Problem**: Extensions may have side effects (external API calls, logging, metrics).

**Examples**:
- Extension calls external API (rate limiting, billing)
- Extension logs to external system
- Extension updates external database

**Risks**:
- **Race Conditions**: Parallel execution may cause race conditions
- **Duplicate Side Effects**: Same side effect executed multiple times
- **Order-Dependent Side Effects**: Side effects may depend on execution order

**Solution (Option B)**:
- Document side effects in extension metadata
- Warn about potential race conditions
- Require idempotency for parallelizable extensions

**Mitigation (Option A)**:
- Sequential execution prevents race conditions
- Deterministic order prevents duplicate side effects

#### Logging and Observability

**Problem**: Parallel execution makes logging/tracing more complex.

**Current (Sequential)**:
```
[INFO] Pre-processor normalize_text started
[INFO] Pre-processor normalize_text completed (10ms)
[INFO] Pre-processor enrich_context started
[INFO] Pre-processor enrich_context completed (15ms)
```

**Parallel (Option B)**:
```
[INFO] Pre-processor normalize_text started
[INFO] Pre-processor enrich_context started
[INFO] Pre-processor normalize_text completed (10ms)
[INFO] Pre-processor enrich_context completed (15ms)
```

**Risks**:
- **Trace Order**: Logs may appear out of order
- **Correlation**: Harder to correlate logs with execution
- **Debugging**: Harder to debug parallel execution

**Solution (Option B)**:
- Enhanced tracing with execution graph
- Structured logging with execution timestamps
- Trace correlation IDs for parallel executions

**Mitigation (Option A)**:
- Sequential execution = clear log order
- Easy to correlate logs with execution

### Influence on Observability and Debugging

#### Current (Sequential)

**Observability**:
- ✅ Clear execution order in logs
- ✅ Easy to trace request flow
- ✅ Simple telemetry (one extension at a time)
- ✅ Deterministic trace IDs

**Debugging**:
- ✅ Easy to reproduce issues (deterministic order)
- ✅ Clear error attribution (which extension failed)
- ✅ Simple context inspection (linear context evolution)

#### Option B (Parallel)

**Observability**:
- ⚠️ Execution order not clear in logs (interleaved)
- ⚠️ Need execution graph for tracing
- ⚠️ Complex telemetry (multiple extensions simultaneously)
- ⚠️ Need correlation IDs for parallel executions

**Debugging**:
- ⚠️ Harder to reproduce (non-deterministic order)
- ⚠️ Error attribution more complex (which extension failed first?)
- ⚠️ Context inspection more complex (merged from multiple extensions)

**Required Enhancements**:
- Execution graph visualization
- Enhanced trace correlation
- Parallel execution metrics
- Context merge conflict detection

---

## Implementation Notes

### Option A: Strictly Sequential (Recommended for CP2-LC)

**No Code Changes Required** (to pipeline execution).

**Optimizations**:

1. **Circuit Breaker Tuning**:
   - Lower error rate threshold: 30% (from 50%)
   - Lower failure count threshold: 3 (from 5)
   - Faster recovery: 30-60 seconds (from 60+)

2. **Timeout Optimization**:
   - Adaptive timeouts per extension type:
     - Pre/post: 100-200ms (fast extensions)
     - Validators: 200-500ms (may need external calls)
     - Providers: 5000ms+ (slow external APIs)
   - Per-extension timeout configuration in registry

3. **Health-Based Routing**:
   - Skip unhealthy extensions faster (circuit breaker)
   - Prefer healthy instances (if load balancing enabled)
   - Fail-fast on known failures

4. **Caching** (if applicable):
   - Cache extension registry lookups (already optimized)
   - Cache extension responses (if idempotent, future enhancement)

**Dependencies**:
- Circuit breaker improvements (CP3)
- Health monitoring improvements (CP3)
- Adaptive timeout configuration (CP2+)

**Migration Path**:
- Incremental improvements
- No breaking changes
- Can be done in CP2-LC timeframe

### Option B: Parallel Extension Groups (Future CP3+)

**Code Changes Required**:

1. **Policy Schema Extension**:
   ```json
   {
     "pre": [
       {
         "id": "normalize_text",
         "depends_on": [],  // Independent
         "mode": "required"
       },
       {
         "id": "enrich_context",
         "depends_on": ["normalize_text"],  // Depends on normalize_text
         "mode": "required"
       }
     ]
   }
   ```

2. **Dependency Graph Builder**:
   - Parse `depends_on` declarations
   - Build dependency graph
   - Validate graph (no cycles, all dependencies exist)
   - Topological sort for execution order

3. **Parallel Execution Framework**:
   - Execute extensions in parallel within same dependency level
   - Use Erlang processes (`spawn`, `spawn_link`)
   - Wait for all dependencies before next level
   - Handle errors (cancel in-flight extensions on failure)

4. **Context Merge Logic**:
   - Merge results from parallel extensions
   - Handle conflicts (same metadata key from multiple extensions)
   - Preserve execution order in logs/traces

5. **Enhanced Observability**:
   - Execution graph in traces
   - Parallel execution metrics
   - Context merge conflict detection

**Dependencies**:
- Policy schema extension
- Dependency graph implementation
- Parallel execution framework
- Enhanced observability

**Migration Path**:
- Sequential by default (backward compatible)
- Opt-in parallelization via policy flag
- Gradual migration per-policy

---

## Recommendations

### Immediate (CP2-LC)

**Recommendation**: **Option A (Strictly Sequential + Optimizations)**

**Rationale**:
- Lower risk (no architectural changes)
- Faster implementation (incremental improvements)
- Sufficient for CP2-LC requirements (3-4 extensions max)
- Can achieve SLO with optimizations (circuit breaker, timeouts)

**Actions**:
1. ✅ Implement circuit breaker tuning (lower thresholds)
2. ✅ Implement adaptive timeout configuration
3. ✅ Improve health-based routing
4. ✅ Document recommended extension limits (3-4 per pipeline)

### Future (CP3+)

**Recommendation**: **Option B (Parallel Extension Groups)**

**Rationale**:
- Significant performance improvement (50-70% latency reduction)
- Enables more extensions in pipeline
- Better scalability

**Prerequisites**:
- Dependency declaration mechanism
- Parallel execution framework
- Enhanced observability
- Thorough testing (chaos tests for parallel execution)

**Actions**:
1. Design dependency declaration format
2. Implement dependency graph builder
3. Implement parallel execution framework
4. Add enhanced observability
5. Test thoroughly (chaos tests)

---

## References

- `docs/archive/dev/EXTENSIONS_PIPELINE_PERF_REPORT.md` - Performance report with parallelization recommendation
- `docs/EXTENSIONS_API.md` - Extensions API contract
- `apps/otp/router/src/router_decider.erl` - Current pipeline implementation
- `docs/archive/dev/EXTENSIONS_CHAOS_RESILIENCE_REPORT.md` - Chaos/resilience testing
- `docs/archive/dev/PIPELINE_COMPLEXITY_MANAGEMENT_REPORT.md` - Pipeline complexity management

---

## Compliance

- ✅ Aligns with `.trae/manifest.json`
- ✅ Follows compatibility policy
- ✅ Respects security constraints
- ✅ Integrates with STATE/HISTORY (no changes needed)

---

## Change History

**v1.0 (2025-01-27)**:
- Initial ADR proposal
- Two options: Sequential (Option A) and Parallel (Option B)
- Detailed analysis of risks and benefits
- Recommendation: Option A for CP2-LC, Option B for CP3+

