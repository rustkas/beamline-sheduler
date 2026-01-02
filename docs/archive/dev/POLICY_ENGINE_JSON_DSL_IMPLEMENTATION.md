# Policy Engine JSON-DSL Implementation Report

## Purpose

Доработка/реализация недостающих конструкций policy-движка согласно JSON-DSL спецификации из `docs/ROUTING_POLICY.md`:
- Сложные fallback-цепочки
- Sticky-routing (привязка к provider по ключу)
- Provider-weights

## Status

✅ **COMPLETED** - Все конструкции реализованы с поддержкой обратной совместимости

## Changes Summary

### 1. Provider Weights (Веса провайдеров)

**Реализовано**:
- ✅ Парсинг `providers` массива из JSON-DSL формата
- ✅ Конвертация весов 0-100 → 0.0-1.0
- ✅ Поддержка обратной совместимости с legacy форматом (`weights` map)

**Файлы**:
- `apps/otp/router/src/router_policy_store.erl`:
  - `parse_weights/1` - парсинг weights из обоих форматов
  - `convert_providers_to_weights/1` - конвертация providers массива

**Пример**:
```json
{
  "providers": [
    {"name": "openai", "weight": 70},
    {"name": "anthropic", "weight": 30}
  ]
}
```

Конвертируется в:
```erlang
#{
  <<"openai">> => 0.7,
  <<"anthropic">> => 0.3
}
```

### 2. Fallback Chains (Сложные fallback-цепочки)

**Реализовано**:
- ✅ Парсинг `fallbacks` массива из JSON-DSL формата
- ✅ Поддержка множественных fallback правил с условиями `when`
- ✅ Конвертация условий `when` в match expressions
- ✅ Оценка условий при выборе fallback провайдера
- ✅ Поддержка обратной совместимости с legacy форматом (`fallback` object)

**Файлы**:
- `apps/otp/router/include/beamline_router.hrl`:
  - Добавлено поле `fallbacks = []` в record `policy`
- `apps/otp/router/src/router_policy_store.erl`:
  - `parse_fallbacks/1` - парсинг fallbacks массива
  - `parse_fallback_rule/1` - парсинг отдельного fallback правила
  - `convert_when_to_match/1` - конвертация when условий в match expressions
  - `convert_legacy_fallback/1` - конвертация legacy формата
- `apps/otp/router/src/router_decider.erl`:
  - `check_fallbacks/2` - проверка множественных fallback правил
  - `evaluate_when_condition/2` - оценка when условий
  - `evaluate_condition/3` - оценка отдельного условия

**Пример**:
```json
{
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx"]},
      "retry": 2,
      "to": "anthropic"
    },
    {
      "when": {"status": ["rate_limited"]},
      "retry": 1,
      "to": "local_llm"
    }
  ]
}
```

**Логика**:
- Fallback правила оцениваются в порядке массива
- Первое правило с совпадающим условием `when` используется
- Условие `{"status": ["timeout", "5xx"]}` проверяет значение `status` в контексте

### 3. Sticky Routing (Привязка к provider по ключу)

**Реализовано**:
- ✅ Парсинг `sticky.ttl` строки ("10m", "5m", "1h") → секунды
- ✅ Поддержка `sticky.session_key` из JSON-DSL
- ✅ Обратная совместимость с legacy форматом (`ttl_seconds`)

**Файлы**:
- `apps/otp/router/src/router_policy_store.erl`:
  - `parse_sticky/1` - парсинг sticky конфигурации
  - `parse_ttl_duration/1` - парсинг TTL строки
  - `parse_ttl_duration_string/1` - парсинг duration формата

**Пример**:
```json
{
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  }
}
```

Конвертируется в:
```erlang
#{
  <<"enabled">> => true,
  <<"session_key">> => <<"user_id">>,
  <<"ttl_seconds">> => 600
}
```

**Поддерживаемые TTL форматы**:
- `"30s"` - 30 секунд
- `"5m"` - 5 минут
- `"1h"` - 1 час

### 4. Schema Updates

**Обновлено**:
- ✅ `apps/otp/router/docs/schemas/policy.schema.json`:
  - Добавлен `providers` array
  - Добавлен `fallbacks` array
  - Обновлен `sticky` для поддержки `ttl` строки
  - Сохранена обратная совместимость с legacy форматом

### 5. Fixtures Updates

**Обновлено**:
- ✅ `apps/otp/router/priv/fixtures/policies/default_tenant/default.json`:
  - Переведен на новый JSON-DSL формат
  - Использует `providers` array
  - Использует `fallbacks` array
  - Использует `sticky.ttl` строку

### 6. Integration Updates

**Обновлено**:
- ✅ `apps/otp/router/src/router_policy.erl`:
  - Использует `parse_policy_map/3` из `router_policy_store` для консистентного парсинга
- ✅ `apps/otp/router/src/router_policy_store.erl`:
  - Экспортирован `parse_policy_map/3` для использования в `router_policy.erl`

## Backward Compatibility

**Поддерживается**:
- ✅ Legacy формат `weights` map (0.0-1.0 или 0-100)
- ✅ Legacy формат `fallback` object
- ✅ Legacy формат `sticky.ttl_seconds` integer

**Миграция**:
- Старые фикстуры продолжают работать
- Новые фикстуры используют JSON-DSL формат
- Автоматическая конвертация legacy формата в новый формат

## Testing Recommendations

### Unit Tests

1. **Weights Parsing**:
   - Парсинг `providers` массива
   - Конвертация 0-100 → 0.0-1.0
   - Валидация суммы весов = 100

2. **Fallback Chains**:
   - Парсинг `fallbacks` массива
   - Конвертация `when` условий
   - Оценка условий в контексте
   - Порядок оценки fallback правил

3. **Sticky Routing**:
   - Парсинг `ttl` строки
   - Поддержка различных форматов (s, m, h)
   - Использование `session_key`

### Integration Tests

1. **End-to-End Policy Decision**:
   - Sticky → Weights → Fallbacks цепочка
   - Множественные fallback правила
   - Условия в контексте запроса

2. **Backward Compatibility**:
   - Legacy формат работает
   - Новый формат работает
   - Смешанный формат (частично legacy, частично новый)

## Code References

### Key Functions

1. **Weights**:
   - `parse_weights/1` - парсинг weights
   - `convert_providers_to_weights/1` - конвертация providers массива

2. **Fallbacks**:
   - `parse_fallbacks/1` - парсинг fallbacks массива
   - `parse_fallback_rule/1` - парсинг отдельного правила
   - `convert_when_to_match/1` - конвертация when условий
   - `check_fallbacks/2` - проверка fallback правил
   - `evaluate_when_condition/2` - оценка условий

3. **Sticky**:
   - `parse_sticky/1` - парсинг sticky конфигурации
   - `parse_ttl_duration/1` - парсинг TTL строки

## Files Modified

1. `apps/otp/router/include/beamline_router.hrl` - добавлено поле `fallbacks`
2. `apps/otp/router/src/router_policy_store.erl` - парсинг нового формата
3. `apps/otp/router/src/router_decider.erl` - поддержка множественных fallbacks
4. `apps/otp/router/src/router_policy.erl` - использование parse_policy_map
5. `apps/otp/router/docs/schemas/policy.schema.json` - обновлена схема
6. `apps/otp/router/priv/fixtures/policies/default_tenant/default.json` - обновлен фикстур

## Next Steps

1. **Testing**:
   - Добавить unit тесты для новых функций
   - Добавить integration тесты для fallback цепочек
   - Проверить backward compatibility

2. **Documentation**:
   - Обновить `docs/ROUTING_POLICY.md` с примерами использования
   - Добавить примеры миграции с legacy формата

3. **Validation**:
   - Добавить валидацию суммы весов = 100
   - Добавить валидацию fallback правил
   - Добавить валидацию TTL формата

## Invariants for Edge Cases

### Inconsistent Weights (Sum ≠ 100/1.0)

**Behavior**: Router **does NOT normalize** weights to 100/1.0. Instead, weights are used **proportionally** based on their actual sum.

**Implementation**:
- `TotalWeight = sum(weights)` (actual sum, not normalized)
- `Random = rand:uniform() * TotalWeight` (random value in [0, TotalWeight])
- Provider selected when `Random <= cumulative_weight`

**Examples**:
1. **Sum < 100** (e.g., 70):
   - `provider_a: 30` → 30/70 = 42.9% of traffic
   - `provider_b: 40` → 40/70 = 57.1% of traffic
   - **Result**: Works correctly, proportional distribution

2. **Sum > 100** (e.g., 120):
   - `provider_a: 70` → 70/120 = 58.3% of traffic
   - `provider_b: 50` → 50/120 = 41.7% of traffic
   - **Result**: Works correctly, proportional distribution

3. **Sum = 0**:
   - **Result**: `{error, no_providers}` (no provider can be selected)

**Logging**:
- Router logs warning if sum ≠ 100 (for visibility, but does not fail)
- Explanation includes actual weights used (not normalized)

**Tests**: `test_inconsistent_weights_sum_not_100`, `test_inconsistent_weights_sum_zero`, `test_inconsistent_weights_sum_over_100` in `router_policy_integration_SUITE.erl`

**Reference**: `docs/ROUTING_POLICY.md` - "Inconsistent Weights Behavior"

### Conflicting and Overlapping Fallback Rules

**Behavior**: Router uses **first match wins** strategy: the first fallback rule in the array that matches the condition is used, and subsequent matching rules are ignored.

**Implementation**:
- Fallback rules evaluated **sequentially** in array order
- First rule with matching `when` condition is used
- Subsequent matching rules are **not evaluated** (short-circuit)
- Retry count and backoff are tracked per fallback rule (by rule ID)

**Examples**:
1. **Identical Conditions** (conflicting):
   ```json
   {
     "fallbacks": [
       {"when": {"status": ["timeout"]}, "to": "provider_a"},  // ← Wins
       {"when": {"status": ["timeout"]}, "to": "provider_b"}   // ← Ignored
     ]
   }
   ```
   - **Result**: If `status = "timeout"`, Router uses `provider_a` (first matching rule)

2. **Overlapping Conditions** (subset/superset):
   ```json
   {
     "fallbacks": [
       {"when": {"status": ["timeout", "5xx"]}, "to": "provider_a"},  // ← Wins for "timeout"
       {"when": {"status": ["timeout"]}, "to": "provider_b"}           // ← Ignored
     ]
   }
   ```
   - **Result**: If `status = "timeout"`, Router uses `provider_a` (first matching rule)

**Logging**:
- Router logs which fallback rule was used (by position/index)
- Explanation includes fallback rule identifier

**Tests**: `test_conflicting_fallback_rules`, `test_overlapping_fallback_rules` in `router_policy_integration_SUITE.erl`

**Reference**: `docs/ROUTING_POLICY.md` - "Conflicting and Overlapping Fallback Rules"

## TODO: Remaining Gaps and Improvements

### Critical (Must Fix)

1. **Retry Logic Implementation** ❌
   - **Issue**: Retry count парсится из JSON-DSL, но **не используется** в логике принятия решений
   - **Current**: Fallback применяется сразу, без учета retry count
   - **Required**: 
     - Добавить отслеживание retry count в `router_decider:check_fallbacks/2`
     - Применять fallback только после исчерпания retry попыток
     - Хранить retry state в контексте запроса
   - **Files**: `apps/otp/router/src/router_decider.erl`
   - **Reference**: `docs/ROUTING_POLICY.md` - "Fallback provider **always** selected after retry count exhausted"

2. **Explanation Format Specification** ❌
   - **Issue**: Формат explanation не описан в `docs/ROUTING_POLICY.md`
   - **Current**: Explanation реализован в `router_policy_applier.erl`, но не специфицирован
   - **Required**:
     - Добавить раздел "Decision Explanation Format" в `docs/ROUTING_POLICY.md`
     - Описать все поля explanation (reason, provider_id, policy_id, steps, context)
     - Добавить примеры explanation для разных сценариев
   - **Files**: `docs/ROUTING_POLICY.md`

3. **Extensions in ROUTING_POLICY.md** ❌
   - **Issue**: Extensions описаны только в `docs/EXTENSIONS_API.md`, но не в `docs/ROUTING_POLICY.md`
   - **Current**: Extensions парсятся и используются, но не включены в основной JSON-DSL пример
   - **Required**:
     - Добавить раздел "Extensions Configuration" в `docs/ROUTING_POLICY.md`
     - Включить extensions в основной JSON-DSL пример
     - Описать связь между policy и Extension Registry
   - **Files**: `docs/ROUTING_POLICY.md`

### Important (Should Fix)

4. **Backoff Strategy Implementation** ⚠️
   - **Issue**: Backoff не реализован и не упоминается в спецификации
   - **Current**: Нет backoff между retry попытками
   - **Required**:
     - Добавить поле `backoff` в fallback rule JSON-DSL
     - Реализовать backoff стратегии (exponential, linear, fixed)
     - Использовать backoff между retry попытками
   - **Files**: `apps/otp/router/src/router_decider.erl`, `docs/ROUTING_POLICY.md`

5. **Metadata and Defaults Deprecation** ✅
   - **Status**: Помечены как deprecated
   - **Decision**: Поля `metadata`, `defaults`, `escalate_on` парсятся, но **не используются** в логике routing
   - **Rationale**: Эти поля не влияют на routing decisions и не являются частью публичного DSL
   - **Action**: Помечены как `deprecated: true` в `policy.schema.json`, не включены в `ROUTING_POLICY.md`
   - **Future**: Могут быть использованы в CP2 для внутреннего хранения или будущих функций
   - **Files**: `apps/otp/router/docs/schemas/policy.schema.json`

### Nice to Have (Future Enhancements)

6. **Explanation Detail Levels** 📝
   - **Enhancement**: Добавить уровни детализации explanation (minimal, detailed, verbose)
   - **Current**: Только один уровень детализации
   - **Future**: Настраиваемые уровни детализации для разных use cases

7. **Extension Fields in Policy** 📝
   - **Enhancement**: Перенос timeout_ms и retry из Extension Registry в Policy (per-policy override)
   - **Current**: timeout_ms и retry только в Registry
   - **Future**: Возможность переопределения на уровне policy

8. **Circuit Breaker and Rate Limiting in Policy** 📝
   - **Enhancement**: Добавить circuit_breaker и rate_limit в Policy DSL
   - **Current**: Не реализовано
   - **Future**: Per-policy circuit breaker и rate limiting configuration

## Gap Analysis Document

Подробный анализ расхождений между спецификацией и реализацией:
- `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md` - детальный анализ gaps

## References

- `docs/ROUTING_POLICY.md` - JSON-DSL спецификация
- `docs/EXTENSIONS_API.md` - спецификация extensions
- `docs/archive/dev/POLICY_ENGINE_JSON_DSL_MAPPING.md` - анализ несоответствий
- `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md` - детальный анализ gaps
- `apps/otp/router/src/router_decider.erl` - decision engine
- `apps/otp/router/src/router_policy_store.erl` - policy storage
- `apps/otp/router/src/router_policy_applier.erl` - policy application

