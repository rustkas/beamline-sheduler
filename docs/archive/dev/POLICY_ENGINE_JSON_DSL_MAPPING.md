# Policy Engine JSON-DSL Mapping Report

## Purpose

Сопоставление текущей реализации policy-движка с JSON-DSL спецификацией из `docs/ROUTING_POLICY.md`.

## Scope

Проверка соответствия реализации:
- **Weights** (веса провайдеров)
- **Sticky** (sticky sessions)
- **Fallback** (fallback правила)
- **Feature Flags** (флаги функций)

## Current Implementation

### Modules

- `apps/otp/router/src/router_decider.erl` - основной движок принятия решений
- `apps/otp/router/src/router_policy_store.erl` - хранилище политик
- `apps/otp/router/src/router_sticky_store.erl` - хранилище sticky сессий
- `apps/otp/router/src/router_policy_static.erl` - статический провайдер политик

### Current Policy Structure

```erlang
#policy{
    tenant_id = binary(),
    policy_id = binary(),
    version = binary(),
    defaults = #{<<"provider">> => binary()},
    weights = #{binary() => float()},  % 0.0-1.0
    fallback = #{<<"provider">> => binary(), <<"conditions">> => [binary()]},
    sticky = #{<<"enabled">> => boolean(), <<"ttl_seconds">> => integer()},
    metadata = #{}
}
```

### Current Fixture Format

```json
{
  "version": "1.0",
  "defaults": {
    "provider": "openai"
  },
  "weights": {
    "openai": 0.7,
    "anthropic": 0.3
  },
  "fallback": {
    "provider": "local_llm",
    "conditions": ["all_providers_failed", "timeout"]
  },
  "sticky": {
    "enabled": true,
    "ttl_seconds": 3600
  }
}
```

## JSON-DSL Specification (from ROUTING_POLICY.md)

### Expected Structure

```json
{
  "version": "1.0",
  "providers": [
    {"name": "provider_a", "weight": 70},
    {"name": "provider_b", "weight": 30}
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx"]},
      "retry": 2,
      "to": "provider_b"
    }
  ],
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  }
}
```

## Mapping Analysis

### 1. Weights (Веса)

#### JSON-DSL Specification

- **Format**: `providers` array with `name` and `weight` (0-100 integer)
- **Conversion**: `weight` (0-100) → `AdminProvider.weight` (0.0-1.0, divide by 100)
- **Requirement**: Weights **must** sum to 100 for deterministic distribution

#### Current Implementation

**Status**: ⚠️ **PARTIAL MISMATCH**

**Issues**:

1. **Format Mismatch**:
   - JSON-DSL: `providers` array with `name` and `weight` (0-100)
   - Current: `weights` map with provider names as keys and float values (0.0-1.0)

2. **Weight Range**:
   - JSON-DSL: 0-100 (integer)
   - Current: 0.0-1.0 (float) - stored directly as float

3. **Structure**:
   - JSON-DSL: `{"providers": [{"name": "openai", "weight": 70}, ...]}`
   - Current: `{"weights": {"openai": 0.7, ...}}`

**Code Reference**:

```118:133:apps/otp/router/src/router_decider.erl
apply_weights(Weights) when map_size(Weights) =:= 0 ->
    {error, no_providers};
apply_weights(Weights) ->
    %% Calculate cumulative weights
    Providers = maps:keys(Weights),
    TotalWeight = lists:sum([maps:get(P, Weights) || P <- Providers]),
    
    if
        TotalWeight =< 0.0 ->
            {error, no_providers};
        true ->
            %% Generate random value
            Random = rand:uniform() * TotalWeight,
            select_provider_by_weight(Providers, Weights, Random, 0.0)
    end.
```

**Parse Reference**:

```798:809:apps/otp/router/src/router_policy_store.erl
parse_policy_map(TenantId, PolicyId, PolicyMap) ->
    #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = maps:get(<<"version">>, PolicyMap, <<"1.0">>),
        defaults = maps:get(<<"defaults">>, PolicyMap, #{}),
        escalate_on = maps:get(<<"escalate_on">>, PolicyMap, []),
        weights = maps:get(<<"weights">>, PolicyMap, #{}),
        fallback = maps:get(<<"fallback">>, PolicyMap, undefined),
        sticky = maps:get(<<"sticky">>, PolicyMap, undefined),
        metadata = maps:get(<<"metadata">>, PolicyMap, #{})
    }.
```

**Required Changes**:

1. Add parsing for `providers` array format
2. Convert `weight` (0-100) → float (0.0-1.0) during parsing
3. Validate that weights sum to 100 (or normalize)
4. Support both formats for backward compatibility (if needed)

### 2. Sticky Sessions

#### JSON-DSL Specification

- **Format**: `sticky` object with `enabled`, `session_key`, `ttl`
- **Fields**:
  - `enabled`: boolean
  - `session_key`: string (e.g., "user_id", "message_id")
  - `ttl`: string duration (e.g., "10m", "5m")

#### Current Implementation

**Status**: ⚠️ **PARTIAL MISMATCH**

**Issues**:

1. **TTL Format**:
   - JSON-DSL: `"ttl": "10m"` (string duration)
   - Current: `"ttl_seconds": 3600` (integer seconds)

2. **Session Key**:
   - JSON-DSL: `"session_key": "user_id"` (explicit field)
   - Current: Uses `session_key` from sticky map, but defaults to `"session_id"` if not present

**Code Reference**:

```89:116:apps/otp/router/src/router_decider.erl
check_sticky(undefined, _Context) ->
    {error, not_found};
check_sticky(Sticky, Context) when not(is_map(Sticky)) ->
    {error, not_found};
check_sticky(Sticky, Context) ->
    case maps:get(<<"enabled">>, Sticky, false) of
        true ->
            %% Get tenant_id and session_key from context
            TenantId = maps:get(<<"tenant_id">>, Context, undefined),
            SessionKeyName = maps:get(<<"session_key">>, Sticky, <<"session_id">>),
            SessionValue = maps:get(SessionKeyName, Context, undefined),
            
            case TenantId =:= undefined orelse SessionValue =:= undefined of
                true ->
                    {error, not_found};
                false ->
                    %% Lookup provider from sticky store
                    case router_sticky_store:get_provider(TenantId, SessionValue) of
                        {ok, ProviderId} ->
                            {ok, ProviderId};
                        {error, not_found} ->
                            {error, not_found}
                    end
            end;
        false ->
            {error, not_found}
    end.
```

**Required Changes**:

1. Parse `ttl` string duration ("10m", "5m") → seconds (integer)
2. Store `session_key` explicitly in sticky configuration
3. Update `router_sticky_store` to use TTL from policy (if not already done)

### 3. Fallback

#### JSON-DSL Specification

- **Format**: `fallbacks` array with objects containing:
  - `when`: condition object (e.g., `{"status": ["timeout", "5xx"]}`)
  - `retry`: integer (number of retries)
  - `to`: provider name (string)

- **Conversion**: `fallbacks[].when` → `AdminRule.match` (match expression)
- **Example**: `{"status": ["timeout", "5xx"]}` → `"status:timeout|5xx"`

#### Current Implementation

**Status**: ❌ **MAJOR MISMATCH**

**Issues**:

1. **Structure**:
   - JSON-DSL: `fallbacks` array
   - Current: `fallback` single object

2. **Condition Format**:
   - JSON-DSL: `when: {"status": ["timeout", "5xx"]}`
   - Current: `conditions: ["all_providers_failed", "timeout"]` (array of strings)

3. **Retry Count**:
   - JSON-DSL: `retry: 2` (explicit retry count)
   - Current: Not stored in policy

4. **Provider Field**:
   - JSON-DSL: `to: "provider_b"`
   - Current: `provider: "local_llm"`

5. **Multiple Fallbacks**:
   - JSON-DSL: Supports multiple fallback rules (array)
   - Current: Only single fallback rule

**Code Reference**:

```148:160:apps/otp/router/src/router_decider.erl
check_fallback(undefined) ->
    {error, no_fallback};
check_fallback(Fallback) when not(is_map(Fallback)) ->
    {error, no_fallback};
check_fallback(Fallback) ->
    Provider = maps:get(<<"provider">>, Fallback, undefined),
    case Provider of
        undefined ->
            {error, no_fallback};
        _ ->
            {ok, Provider}
    end.
```

**Current Fixture**:

```10:13:apps/otp/router/priv/fixtures/policies/default_tenant/default.json
  "fallback": {
    "provider": "local_llm",
    "conditions": ["all_providers_failed", "timeout"]
  },
```

**Required Changes**:

1. **Parse `fallbacks` array** instead of single `fallback` object
2. **Convert `when` conditions** to match expressions:
   - `{"status": ["timeout", "5xx"]}` → `"status:timeout|5xx"`
   - `{"message_type": "chat"}` → `"message_type:chat"`
3. **Store `retry` count** in policy metadata or internal state
4. **Support multiple fallback rules** with ordered evaluation
5. **Update `check_fallback`** to evaluate conditions and retry logic

### 4. Feature Flags

#### JSON-DSL Specification

**Status**: ❌ **NOT SPECIFIED**

**Analysis**:

- `docs/ROUTING_POLICY.md` does **not** mention feature flags in policy JSON-DSL
- Feature flags are mentioned in other contexts (CP2 features, app.src), but not in routing policy

**Current Implementation**:

- Feature flags are managed at application level (`beamline_router.app.src`)
- Not part of policy JSON-DSL structure

**Conclusion**:

Feature flags are **out of scope** for routing policy JSON-DSL. They are managed separately at application/CP level.

## Summary

### Compliance Status

| Feature | Status | Issues |
|---------|--------|--------|
| **Weights** | ⚠️ Partial | Format mismatch (array vs map), weight range (0-100 vs 0.0-1.0) |
| **Sticky** | ⚠️ Partial | TTL format (string vs integer), session_key handling |
| **Fallback** | ❌ Major | Structure (array vs single), condition format, retry count missing |
| **Feature Flags** | ✅ N/A | Not part of JSON-DSL specification |

### Priority Fixes

1. **HIGH**: Fallback structure and condition parsing
   - Parse `fallbacks` array
   - Convert `when` conditions to match expressions
   - Store and use `retry` count

2. **MEDIUM**: Weights format
   - Support `providers` array format
   - Convert 0-100 → 0.0-1.0 during parsing
   - Validate weight sum = 100

3. **LOW**: Sticky TTL format
   - Parse string duration ("10m") → seconds
   - Ensure `session_key` is properly stored

## Recommendations

### Immediate Actions

1. **Update `parse_policy_map/3`** in `router_policy_store.erl`:
   - Parse `providers` array → convert to weights map (0.0-1.0)
   - Parse `fallbacks` array → convert to internal format
   - Parse `sticky.ttl` string → seconds

2. **Update `router_decider.erl`**:
   - Implement fallback condition evaluation
   - Support multiple fallback rules with ordered evaluation
   - Use retry count from policy

3. **Update Schema** (`apps/otp/router/docs/schemas/policy.schema.json`):
   - Add `providers` array structure
   - Add `fallbacks` array structure
   - Update `sticky` to include `ttl` as string

4. **Backward Compatibility**:
   - Support both old format (`weights` map) and new format (`providers` array)
   - Support both old format (`fallback` object) and new format (`fallbacks` array)
   - Migrate fixtures to new format

### Testing

1. Add tests for JSON-DSL parsing
2. Add tests for fallback condition evaluation
3. Add tests for weight conversion (0-100 → 0.0-1.0)
4. Add tests for TTL string parsing

## References

- `docs/ROUTING_POLICY.md` - JSON-DSL specification
- `apps/otp/router/src/router_decider.erl` - Decision engine
- `apps/otp/router/src/router_policy_store.erl` - Policy storage
- `apps/otp/router/src/router_sticky_store.erl` - Sticky session storage
- `apps/otp/router/docs/schemas/policy.schema.json` - Policy schema

