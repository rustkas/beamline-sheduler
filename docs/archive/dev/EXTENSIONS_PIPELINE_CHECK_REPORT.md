# Extensions Pipeline Check Report

**Date**: 2025-01-27  
**Step**: 4.1 - Extensions Pipeline Verification  
**Status**: ✅ **IMPLEMENTED** (CP2-LC, see Follow-up section)

---

## Objective

Verify that Router can build pipeline `pre → validators → provider → post` from Policy Store / Registry data.

---

## Current State Analysis

**⚠️ NOTE**: This section reflects the state **before implementation** (pre-CP2-LC). For current implementation status, see [Conclusion](#conclusion) and [Follow-up](#follow-up-pipeline-implementation-cp2-lc) sections below.

### 1. Policy Structure

**Current Policy Record** (`apps/otp/router/include/beamline_router.hrl`):

```erlang
-record(policy, {
    tenant_id :: tenant_id(),
    policy_id :: policy_id(),
    version = <<"1.0">> :: binary(),
    defaults = #{} :: map(),
    escalate_on = [] :: list(),
    weights = #{} :: map(),  % Provider weights (0.0-1.0)
    fallback = undefined :: map() | undefined,  % Legacy: single fallback object
    fallbacks = [] :: list(),  % New: array of fallback rules with when/retry/to
    sticky = undefined :: map() | undefined,
    metadata = #{} :: map()
}).
```

**Missing Fields**:
- ❌ `pre` - pre-processor extensions list
- ❌ `validators` - validator extensions list
- ❌ `post` - post-processor extensions list

### 2. Extension Registry

**Status**: ❌ **NOT IMPLEMENTED**

**Expected** (from `docs/EXTENSIONS_API.md`):
- Extension Registry should store extension metadata:
  - `id` - logical extension ID
  - `type` - `pre`, `validator`, `post`, or `provider`
  - `subject` - NATS subject (e.g., `beamline.ext.pre.normalize_text.v1`)
  - `timeout_ms` - timeout configuration
  - `retry` - retry count

**Current State**:
- No Extension Registry module (`router_extension_registry.erl` does not exist)
- No PostgreSQL schema for extensions
- No Mnesia cache layer for extensions

### 3. Pipeline Execution

**Status**: ❌ **NOT IMPLEMENTED**

**Expected Pipeline Flow** (from `docs/EXTENSIONS_API.md`):
```
Request → Pre-processors → Validators → Provider → Post-processors → Response
```

**Current Router Flow** (`apps/otp/router/src/router_core.erl`):
```erlang
route(RouteRequest, Context) ->
    %% Load policy
    case router_policy:load_policy(TenantId, FinalPolicyId) of
        {ok, Policy} ->
            %% Make decision (only provider selection)
            router_decider:decide(RouteRequest, Policy, SafeContext);
        ...
    end.
```

**Current Decision Flow** (`apps/otp/router/src/router_decider.erl`):
- ✅ Sticky session check
- ✅ Weighted provider selection
- ✅ Fallback provider selection
- ❌ **NO** pre-processor execution
- ❌ **NO** validator execution
- ❌ **NO** post-processor execution

### 4. Policy Store Integration

**Status**: ✅ **PARTIALLY IMPLEMENTED**

**Current Implementation**:
- ✅ Policy Store exists (`router_policy_store.erl`)
- ✅ Policies loaded from fixtures (CP1) or database (CP2+)
- ✅ Policy parsing supports legacy and JSON-DSL formats
- ❌ Policy structure does not include `pre`, `validators`, `post` fields

**Policy Parsing** (`apps/otp/router/src/router_policy_store.erl`):
```erlang
parse_policy_map(TenantId, PolicyId, PolicyMap) ->
    #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = maps:get(<<"version">>, PolicyMap, <<"1.0">>),
        defaults = maps:get(<<"defaults">>, PolicyMap, #{}),
        escalate_on = maps:get(<<"escalate_on">>, PolicyMap, []),
        weights = Weights,
        fallback = Fallback,
        fallbacks = Fallbacks,
        sticky = Sticky,
        metadata = maps:get(<<"metadata">>, PolicyMap, #{})
    }.
```

**Missing**: No parsing of `pre`, `validators`, `post` fields from PolicyMap.

---

## Expected Policy Format (from Documentation)

According to `docs/EXTENSIONS_API.md`, Policy should support:

```json
{
  "policy_id": "support_en",
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "config": {
        "lowercase": true
      }
    }
  ],
  "validators": [
    {
      "id": "pii_guard",
      "on_fail": "block"
    }
  ],
  "providers": [
    "openai:gpt-4.1-mini",
    "my_crm_summarizer"
  ],
  "post": [
    {
      "id": "mask_pii",
      "mode": "required",
      "config": {
        "mask_email": true
      }
    }
  ]
}
```

**Current Policy Format** (CP1):
- Only supports `weights`, `fallback`, `fallbacks`, `sticky`
- Does not support `pre`, `validators`, `post` arrays

---

## Implementation Plan

According to `docs/archive/dev/CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md`, pipeline functionality is planned for **CP2-LC**:

### Phase 1: Extension Registry (Week 1, Days 1-3)
- [ ] PostgreSQL schema for extensions
- [ ] Mnesia cache layer (`router_extension_registry.erl`)
- [ ] Admin API for CRUD operations

### Phase 2: Router Integration (Week 1, Days 4-5)
- [ ] Extension invoker (NATS request-reply)
- [ ] Pipeline execution in `router_decider.erl`
- [ ] Policy schema update to include `pre`, `validators`, `post`

### Phase 3: Reference Implementations (Week 2)
- [ ] Mock Provider (Erlang)
- [ ] OpenAI Provider (Go)

---

## Verification Results

### ✅ What Works

1. **Policy Store**: ✅ Functional
   - Policies can be loaded from fixtures or database
   - Policy parsing supports legacy and JSON-DSL formats
   - Policy lookup by tenant_id and policy_id works

2. **Provider Selection**: ✅ Functional
   - Weighted distribution
   - Sticky sessions
   - Fallback providers

### ❌ What's Missing

1. **Extension Registry**: ❌ Not implemented
   - No database schema
   - No cache layer
   - No lookup API

2. **Policy Structure**: ❌ Missing fields
   - No `pre` field in `#policy{}` record
   - No `validators` field in `#policy{}` record
   - No `post` field in `#policy{}` record

3. **Pipeline Execution**: ❌ Not implemented
   - No pre-processor execution
   - No validator execution
   - No post-processor execution
   - No NATS request-reply to extensions

4. **Extension Invocation**: ❌ Not implemented
   - No `router_extension_invoker.erl` module
   - No NATS subject lookup from Extension Registry
   - No timeout/retry handling for extensions

---

## Conclusion

**Status**: ✅ **IMPLEMENTED** (CP2-LC)

Router **can** now build pipeline `pre → validators → provider → post` from Policy Store / Registry.

**Implementation Status**:
1. ✅ Extension Registry implemented (`router_extension_registry.erl`)
2. ✅ Policy structure includes `pre`, `validators`, `post` fields
3. ✅ Pipeline execution logic implemented (`router_decider.erl`)
4. ✅ Extension invocation mechanism implemented (`router_extension_invoker.erl`)

---

## Follow-up: Pipeline Implementation (CP2-LC)

**Status**: ✅ **COMPLETED**

The Extensions Pipeline was successfully implemented in CP2-LC. See detailed reports:

- **`docs/archive/dev/EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md`** - Initial implementation (Steps 4.2, 4.3)
- **`docs/archive/dev/EXTENSIONS_PIPELINE_ENHANCEMENT_REPORT.md`** - Post-processors and error unification
- **`docs/archive/dev/EXTENSIONS_PIPELINE_TESTS_ENHANCEMENT_REPORT.md`** - Comprehensive test coverage
- **`docs/archive/dev/EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md`** - Production-ready Extension Registry (PostgreSQL + dual-mode)

**Key Features Implemented**:
- ✅ Extension Registry (ETS-based CP1, PostgreSQL CP2-LC)
- ✅ Pipeline execution: `pre → validators → provider → post`
- ✅ Error handling: `required`/`optional` modes for pre/post, `block`/`warn`/`ignore` for validators
- ✅ Timeout and retry mechanisms
- ✅ Unified error format: `{error, {Reason, Metadata}}`
- ✅ Context propagation throughout pipeline
- ✅ Comprehensive integration tests
- ✅ Production-ready database integration (CP2-LC)
- ✅ Circuit breaker and health monitoring (CP3, ready for enable)

**Current State**:
- Pipeline fully functional
- Extension Registry supports dual-mode (database + fixtures)
- All error handling modes implemented
- Comprehensive test coverage

---

## References

- `docs/EXTENSIONS_API.md` - Extensions API specification
- `docs/archive/dev/CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md` - Implementation plan
- `apps/otp/router/include/beamline_router.hrl` - Policy record definition
- `apps/otp/router/src/router_policy_store.erl` - Policy Store implementation
- `apps/otp/router/src/router_decider.erl` - Decision engine (current implementation)

