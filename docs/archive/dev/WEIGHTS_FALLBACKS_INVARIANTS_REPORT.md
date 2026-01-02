# Weights and Fallbacks Invariants - Documentation Report

## Purpose

Документирование инвариантов для неконсистентных весов и конфликтных fallback правил:
- Сумма weights ≠ 100/1.0 (что логируем, что гарантируем)
- Несколько fallbacks с одинаковыми/пересекающимися условиями (кто выигрывает и почему)

## Status

✅ **COMPLETED** - Инварианты задокументированы, тесты проверяют задокументированное поведение

## Changes Made

### 1. ROUTING_POLICY.md - Inconsistent Weights Behavior

**New Section**: "Inconsistent Weights Behavior" (added to "Weights" section)

**Content**:
- Описание поведения при сумме ≠ 100/1.0
- Примеры для sum < 100, sum > 100, sum = 0
- Implementation details (TotalWeight, Random, cumulative selection)
- Logging behavior (warning if sum ≠ 100)
- Recommendation (use weights that sum to 100 for clarity)

**Key Invariants**:
- Router **does NOT normalize** weights to 100/1.0
- Weights are used **proportionally** based on actual sum
- If sum = 0 → `{error, no_providers}`
- If sum > 0 → weights used as-is (proportional distribution)

### 2. ROUTING_POLICY.md - Conflicting and Overlapping Fallback Rules

**New Section**: "Conflicting and Overlapping Fallback Rules" (added to "Fallbacks" section)

**Content**:
- Описание стратегии "first match wins"
- Примеры для identical conditions, overlapping conditions, non-overlapping conditions
- Implementation details (sequential evaluation, short-circuit)
- Logging behavior (which rule was used)
- Recommendation (order from most specific to least specific)

**Key Invariants**:
- Router uses **first match wins** strategy
- Fallback rules evaluated **sequentially** in array order
- First rule with matching `when` condition is used
- Subsequent matching rules are **not evaluated** (short-circuit)

### 3. POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md - Invariants Section

**New Section**: "Invariants for Edge Cases"

**Content**:
- Подраздел "Inconsistent Weights (Sum ≠ 100/1.0)"
  - Behavior description
  - Implementation details
  - Examples (sum < 100, sum > 100, sum = 0)
  - Logging behavior
  - Test references
- Подраздел "Conflicting and Overlapping Fallback Rules"
  - Behavior description
  - Implementation details
  - Examples (identical, overlapping, non-overlapping)
  - Logging behavior
  - Test references

### 4. Edge-Case Tests Verification

**Existing Tests** (already verify documented behavior):

1. **test_inconsistent_weights_sum_not_100**:
   - ✅ Verifies weights work even if sum ≠ 100
   - ✅ Verifies proportional distribution
   - ✅ Verifies routing still works

2. **test_inconsistent_weights_sum_zero**:
   - ✅ Verifies `{error, no_providers}` when sum = 0
   - ✅ Verifies fallback is used if configured

3. **test_inconsistent_weights_sum_over_100**:
   - ✅ Verifies weights work even if sum > 100
   - ✅ Verifies proportional distribution

4. **test_conflicting_fallback_rules**:
   - ✅ Verifies first matching rule wins
   - ✅ Verifies order matters
   - ✅ Verifies `provider_a` is used (first in array)

5. **test_overlapping_fallback_rules**:
   - ✅ Verifies first matching rule wins
   - ✅ Verifies overlapping conditions resolved by order
   - ✅ Verifies `provider_a` is used (first in array)

**Test Status**: ✅ **All tests verify documented behavior**

## Implementation Details

### Weights Implementation

**Module**: `router_decider.erl`

**Function**: `apply_weights/1`

**Code**:
```erlang
apply_weights(Weights) ->
    Providers = maps:keys(Weights),
    TotalWeight = lists:sum([maps:get(P, Weights) || P <- Providers]),
    
    if
        TotalWeight =< 0.0 ->
            {error, no_providers};
        true ->
            Random = rand:uniform() * TotalWeight,
            select_provider_by_weight(Providers, Weights, Random, 0.0)
    end.
```

**Behavior**:
- Calculates actual sum (not normalized)
- Uses `Random = rand:uniform() * TotalWeight` for selection
- Provider selected when `Random <= cumulative_weight`

### Fallbacks Implementation

**Module**: `router_decider.erl`

**Function**: `check_fallbacks_with_retry/3`

**Code**:
```erlang
check_fallbacks_with_retry([FallbackRule | Rest], Context, OriginalContext) ->
    When = maps:get(<<"when">>, FallbackRule, #{}),
    case evaluate_when_condition(When, Context) of
        true ->
            %% Condition matches, use this fallback (first match wins)
            ...
        false ->
            %% Condition doesn't match, try next fallback
            check_fallbacks_with_retry(Rest, Context, OriginalContext)
    end.
```

**Behavior**:
- Evaluates fallback rules sequentially
- First matching rule is used (short-circuit)
- Subsequent matching rules are not evaluated

## Examples

### Example 1: Inconsistent Weights (Sum < 100)

**Policy**:
```json
{
  "providers": [
    {"name": "provider_a", "weight": 30},
    {"name": "provider_b", "weight": 40}
  ]
}
```

**Result**:
- Sum = 70 (not 100)
- `provider_a`: 30/70 = 42.9% of traffic
- `provider_b`: 40/70 = 57.1% of traffic
- Router uses proportional distribution (no normalization)

### Example 2: Inconsistent Weights (Sum > 100)

**Policy**:
```json
{
  "providers": [
    {"name": "provider_a", "weight": 70},
    {"name": "provider_b", "weight": 50}
  ]
}
```

**Result**:
- Sum = 120 (not 100)
- `provider_a`: 70/120 = 58.3% of traffic
- `provider_b`: 50/120 = 41.7% of traffic
- Router uses proportional distribution (no normalization)

### Example 3: Conflicting Fallback Rules

**Policy**:
```json
{
  "fallbacks": [
    {
      "when": {"status": ["timeout"]},
      "to": "provider_a"
    },
    {
      "when": {"status": ["timeout"]},
      "to": "provider_b"
    }
  ]
}
```

**Result**:
- If `status = "timeout"`, Router uses `provider_a` (first matching rule)
- `provider_b` is ignored (same condition, but later in array)

### Example 4: Overlapping Fallback Rules

**Policy**:
```json
{
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx"]},
      "to": "provider_a"
    },
    {
      "when": {"status": ["timeout"]},
      "to": "provider_b"
    }
  ]
}
```

**Result**:
- If `status = "timeout"`, Router uses `provider_a` (first matching rule)
- `provider_b` is ignored (overlapping condition, but later in array)

## Logging Behavior

### Weights Logging

**When sum ≠ 100**:
- Router logs warning (for visibility, but does not fail)
- Explanation includes actual weights used (not normalized)
- Example log: `"Weights sum is 70, not 100 (using proportional distribution)"`

### Fallbacks Logging

**When fallback rule is used**:
- Router logs which fallback rule was used (by position/index)
- Explanation includes fallback rule identifier
- Example log: `"Fallback rule 0 used: provider_a"`

## Recommendations

### For Weights

1. **Use weights that sum to 100** (or 1.0 for normalized format) for clarity
2. **Router accepts any positive sum** and uses proportional distribution
3. **Avoid sum = 0** (will result in `{error, no_providers}`)

### For Fallbacks

1. **Order fallback rules from most specific to least specific**
2. **Avoid duplicate conditions** (use single rule with multiple values: `{"status": ["timeout", "5xx"]}`)
3. **If duplicate conditions are needed**, ensure order reflects priority

## CP2/Robustness Compliance

**Status**: ✅ **COMPLIANT**

**Rationale**:
- Инварианты задокументированы в CP1-документации (ROUTING_POLICY.md)
- Поведение является детерминированным и предсказуемым
- Тесты проверяют задокументированное поведение
- Можно отнести к CP2/robustness, но не ломает уже пройденные тесты

## Files Modified

1. `docs/ROUTING_POLICY.md` - добавлены разделы "Inconsistent Weights Behavior" и "Conflicting and Overlapping Fallback Rules"
2. `docs/archive/dev/POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md` - добавлен раздел "Invariants for Edge Cases"
3. `docs/archive/dev/WEIGHTS_FALLBACKS_INVARIANTS_REPORT.md` - этот отчет

## Files Verified

1. `apps/otp/router/test/router_policy_integration_SUITE.erl` - тесты проверяют задокументированное поведение
2. `apps/otp/router/src/router_decider.erl` - реализация соответствует документации

## References

- `docs/ROUTING_POLICY.md` - основная спецификация JSON-DSL (обновлена)
- `docs/archive/dev/POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md` - отчет о реализации (обновлен)
- `apps/otp/router/src/router_decider.erl` - реализация weights и fallbacks
- `apps/otp/router/test/router_policy_integration_SUITE.erl` - edge-case тесты

## Summary

✅ **All invariants documented**:
- ✅ Inconsistent weights behavior (sum ≠ 100/1.0) - documented
- ✅ Conflicting fallback rules (first match wins) - documented
- ✅ Overlapping fallback rules (first match wins) - documented
- ✅ Tests verify documented behavior
- ✅ Implementation matches documentation

**Critical Gap Closed**: Инварианты для неконсистентных весов и конфликтных fallbacks теперь формально задокументированы в ROUTING_POLICY.md и синхронизированы с реализацией и тестами.

