# Retry + Backoff для Fallbacks - Implementation Report

## Purpose

Реализация retry и backoff для fallbacks согласно спецификации в `ROUTING_POLICY.md`:
- Согласование семантики retry и backoff
- Доработка router_decider для учета retry-count/backoff
- Доработка router_policy_applier для explanation с retry/attempts
- Обновление всех тестов

## Status

🔄 **IN PROGRESS** - Базовая реализация завершена, требуется обновление тестов

## Changes Made

### 1. ROUTING_POLICY.md - Updated Specification

**Added Sections**:
- **Retry Semantics**: Описание retry count (0-10, default: 1)
- **Backoff Semantics**: Описание backoff стратегий (exponential, linear, fixed)
- **Examples**: Примеры использования retry/backoff

**Key Points**:
- Retry count: число попыток перед применением fallback
- Backoff strategies: exponential (default), linear, fixed
- Jitter: опциональный случайный компонент (0-10% от delay)

### 2. router_decider.erl - Retry/Backoff Logic

**New Functions**:
- `check_fallbacks_with_retry/3` - проверка fallbacks с учетом retry/backoff
- `get_fallback_rule_id/1` - генерация уникального ID для fallback rule
- `calculate_backoff/2` - расчет backoff delay

**Changes**:
- Обновлен `execute_provider_selection/5` для использования `check_fallbacks_with_retry/3`
- Добавлена поддержка retry attempts tracking в контексте
- Добавлена поддержка backoff конфигурации

**Retry Logic**:
```erlang
%% Returns:
%%   {ok, FallbackProvider, RetryInfo} - fallback selected after retry exhausted
%%   {retry, ProviderId, RetryInfo} - retry current provider (retry not exhausted)
%%   {error, no_fallback} - no matching fallback rule
```

**Backoff Calculation**:
- Exponential (default): `delay = base_ms * 2^(attempt - 1) + jitter`
- Linear: `delay = base_ms * attempt + jitter`
- Fixed: `delay = base_ms + jitter`

### 3. router_policy_applier.erl - Explanation Updates

**Changes**:
- Обновлен `build_explanation_steps/5` для отображения retry/attempts
- Добавлена поддержка retry info в explanation steps
- Добавлена поддержка backoff info в explanation steps

**Explanation Format**:
```erlang
#{
    reason => <<"fallback">> | <<"retry">>,
    steps => [
        <<"3. Applied fallback rule after 2/3 retry attempts exhausted">>,
        %% OR
        <<"3. Retry attempt 1/3 with backoff 100ms">>
    ],
    context => #{
        <<"retry_attempts_used">> => 2,
        <<"retry_max">> => 3,
        <<"backoff_ms">> => 100
    }
}
```

## Implementation Details

### Retry Tracking

**Context Keys**:
- `retry_attempts_<fallback_rule_id>` - текущее количество попыток для fallback rule
- `current_provider` - текущий провайдер (для retry)
- `status` - статус ошибки (timeout, 5xx, etc.)

**Flow**:
1. Provider selected (weighted/sticky)
2. Request fails with error (timeout, 5xx, etc.)
3. Context updated with `current_provider` and `status`
4. `check_fallbacks_with_retry/3` checks retry count
5. If retry not exhausted: return `{retry, ProviderId, RetryInfo}`
6. If retry exhausted: return `{ok, FallbackProvider, RetryInfo}`

### Backoff Strategies

**Exponential (Default)**:
```erlang
BaseMs = 100,
Delay = trunc(BaseMs * math:pow(2, Attempt - 1)),
Jitter = rand:uniform(trunc(Delay * 0.1)) - 1,
FinalDelay = Delay + Jitter
```

**Linear**:
```erlang
Delay = BaseMs * Attempt,
Jitter = rand:uniform(trunc(Delay * 0.1)) - 1,
FinalDelay = Delay + Jitter
```

**Fixed**:
```erlang
Delay = BaseMs,
Jitter = rand:uniform(trunc(Delay * 0.1)) - 1,
FinalDelay = Delay + Jitter
```

## Integration with router_core

**Note**: Для полной работы retry логики требуется обновление `router_core.erl`:

1. При ошибке провайдера обновить контекст:
   ```erlang
   UpdatedContext = maps:merge(Context, #{
       <<"current_provider">> => ProviderId,
       <<"status">> => ErrorStatus  %% timeout, 5xx, etc.
   })
   ```

2. Повторно вызвать `router_decider:decide/3` с обновленным контекстом

3. Обработать результат `{retry, ProviderId, RetryInfo}`:
   - Применить backoff delay
   - Повторить запрос к провайдеру
   - Обновить retry attempts в контексте

**Current State**: Базовая логика реализована, но требует интеграции в router_core для полной работы.

## Schema Updates

**policy.schema.json**:
- ✅ `fallbacks[].retry` - уже поддерживается (integer, 0-10, default: 1)
- ⚠️ `fallbacks[].backoff` - **TODO**: добавить в schema

**Backoff Schema** (to be added):
```json
{
  "backoff": {
    "type": "object",
    "properties": {
      "strategy": {
        "type": "string",
        "enum": ["exponential", "linear", "fixed"],
        "default": "exponential"
      },
      "base_ms": {
        "type": "integer",
        "minimum": 1,
        "maximum": 10000,
        "default": 100
      },
      "max_ms": {
        "type": "integer",
        "minimum": 1,
        "maximum": 60000,
        "default": 5000
      },
      "jitter": {
        "type": "boolean",
        "default": true
      }
    },
    "additionalProperties": false
  }
}
```

## Testing Status

### Unit Tests

**Status**: ⚠️ **TODO** - требуется обновление

**Files to Update**:
- `router_policy_applier_dsl_SUITE.erl` - добавить тесты для retry/backoff
- `router_decider_SUITE.erl` - добавить тесты для retry/backoff логики

**Test Cases Needed**:
1. Retry count tracking
2. Retry exhaustion
3. Backoff calculation (exponential, linear, fixed)
4. Backoff with jitter
5. Backoff max_ms capping
6. Explanation with retry info

### Integration Tests

**Status**: ⚠️ **TODO** - требуется обновление

**Files to Update**:
- `router_policy_integration_SUITE.erl` - добавить тесты для retry/backoff

**Test Cases Needed**:
1. Retry flow with real JSON policies
2. Backoff delay application
3. Retry exhaustion scenarios
4. Explanation format with retry

### Property Tests

**Status**: ⚠️ **TODO** - требуется обновление

**Files to Update**:
- `router_policy_structure_prop_SUITE.erl` - добавить генераторы для backoff

**Test Cases Needed**:
1. Random backoff configurations
2. Retry count edge cases (0, 10, negative)
3. Backoff strategy variations

## Known Limitations

1. **router_core Integration**: Retry логика требует обновления router_core для полной работы
2. **Backoff Schema**: Backoff конфигурация еще не добавлена в policy.schema.json
3. **Context Management**: Retry attempts tracking требует правильной передачи контекста между вызовами

## Next Steps

1. ✅ Обновить ROUTING_POLICY.md с описанием retry/backoff
2. ✅ Реализовать retry/backoff логику в router_decider
3. ✅ Обновить router_policy_applier для explanation
4. ⚠️ Добавить backoff в policy.schema.json
5. ⚠️ Обновить unit тесты
6. ⚠️ Обновить integration тесты
7. ⚠️ Обновить property тесты
8. ⚠️ Интегрировать retry логику в router_core

## References

- `docs/ROUTING_POLICY.md` - обновленная спецификация
- `apps/otp/router/src/router_decider.erl` - реализация retry/backoff
- `apps/otp/router/src/router_policy_applier.erl` - обновление explanation
- `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md` - gap analysis

