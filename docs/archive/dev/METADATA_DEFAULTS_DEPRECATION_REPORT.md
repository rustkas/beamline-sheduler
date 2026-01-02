# Metadata/Defaults Deprecation Report

## Purpose

Решение о судьбе полей `metadata`, `defaults`, и `escalate_on` в policy DSL:
- Либо специфицировать их в ROUTING_POLICY.md + схеме
- Либо пометить как deprecated и ограничить использование

## Status

✅ **COMPLETED** - Поля помечены как deprecated

## Decision

**Решение**: Пометить как **deprecated**, так как они не используются в логике routing.

**Rationale**:
1. Поля парсятся и сохраняются в `#policy{}` record
2. НО они **не используются** в `router_decider.erl` или `router_policy_applier.erl`
3. Они **не влияют** на routing decisions (provider selection, fallbacks, sticky sessions)
4. Они не являются частью публичного DSL для routing

## Changes Made

### 1. policy.schema.json - Deprecated Fields

**Updated Fields**:
- `defaults` - помечен как `deprecated: true` с описанием "[DEPRECATED] Default values - not used in routing logic, reserved for future use"
- `escalate_on` - помечен как `deprecated: true` с описанием "[DEPRECATED] Escalation conditions - not used in routing logic, reserved for future use"
- `metadata` - помечен как `deprecated: true` с описанием "[DEPRECATED] Additional policy metadata - not used in routing logic, reserved for future use or internal storage"

**Schema Changes**:
```json
{
  "defaults": {
    "type": "object",
    "description": "[DEPRECATED] Default values - not used in routing logic, reserved for future use",
    "additionalProperties": true,
    "deprecated": true
  },
  "escalate_on": {
    "type": "array",
    "description": "[DEPRECATED] Escalation conditions - not used in routing logic, reserved for future use",
    "items": {
      "type": "string"
    },
    "deprecated": true
  },
  "metadata": {
    "type": "object",
    "description": "[DEPRECATED] Additional policy metadata - not used in routing logic, reserved for future use or internal storage",
    "additionalProperties": true,
    "deprecated": true
  }
}
```

### 2. ROUTING_POLICY.md - Deprecated Fields Section

**New Section**: "Deprecated Fields" (added before "Storage" section)

**Content**:
- Overview: список deprecated полей
- Status: статус deprecated
- Rationale: почему они deprecated
- Behavior: текущее поведение (парсятся, но игнорируются)
- Schema: ссылка на schema.json
- Recommendation: не использовать в новых policies

**Key Points**:
- Поля **не включены** в основной JSON-DSL пример
- Поля **не влияют** на routing decisions
- Поля могут быть использованы в CP2 для внутреннего хранения или будущих функций

### 3. POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md - Updated Status

**Updated Section**: "Metadata and Defaults Specification" → "Metadata and Defaults Deprecation"

**Status Change**: ⚠️ → ✅

**Content**:
- Status: Помечены как deprecated
- Decision: Поля парсятся, но не используются в логике routing
- Rationale: Эти поля не влияют на routing decisions и не являются частью публичного DSL
- Action: Помечены как `deprecated: true` в `policy.schema.json`, не включены в `ROUTING_POLICY.md`
- Future: Могут быть использованы в CP2 для внутреннего хранения или будущих функций

## Analysis

### Current Usage

**Parsing**:
- ✅ Поля парсятся в `router_policy_store.erl`:
  ```erlang
  defaults = maps:get(<<"defaults">>, PolicyMap, #{}),
  escalate_on = maps:get(<<"escalate_on">>, PolicyMap, []),
  metadata = maps:get(<<"metadata">>, PolicyMap, #{})
  ```

**Storage**:
- ✅ Поля сохраняются в `#policy{}` record:
  ```erlang
  -record(policy, {
      ...
      defaults = #{} :: map(),
      escalate_on = [] :: list(),
      metadata = #{} :: map()
  }).
  ```

**Usage in Routing Logic**:
- ❌ Поля **не используются** в `router_decider.erl`
- ❌ Поля **не используются** в `router_policy_applier.erl`
- ❌ Поля **не влияют** на provider selection, fallbacks, sticky sessions

**Fixtures**:
- ⚠️ Поля присутствуют в некоторых фикстурах (например, `default.json`), но это просто примеры

### Impact Assessment

**Backward Compatibility**:
- ✅ Существующие policies с этими полями продолжат работать (поля игнорируются)
- ✅ Парсинг не сломается (поля все еще парсятся)
- ✅ Schema валидация не сломается (поля помечены как deprecated, но не удалены)

**Forward Compatibility**:
- ✅ Новые policies не должны использовать эти поля
- ✅ Поля могут быть использованы в CP2 для внутреннего хранения или будущих функций
- ✅ Если поля понадобятся в будущем, их можно будет специфицировать и реализовать

## Examples

### Example 1: Policy with Deprecated Fields (Still Works)

**Policy**:
```json
{
  "version": "1.0",
  "providers": [
    {"name": "openai", "weight": 70},
    {"name": "anthropic", "weight": 30}
  ],
  "metadata": {
    "strategy": "cp1_json_dsl",
    "description": "CP1 default policy"
  },
  "defaults": {
    "timeout": 30
  },
  "escalate_on": ["timeout", "5xx"]
}
```

**Behavior**:
- Policy парсится успешно
- `metadata`, `defaults`, `escalate_on` сохраняются в `#policy{}` record
- Эти поля **игнорируются** при routing decisions
- Routing работает нормально (используются только `providers`)

### Example 2: Policy without Deprecated Fields (Recommended)

**Policy**:
```json
{
  "version": "1.0",
  "providers": [
    {"name": "openai", "weight": 70},
    {"name": "anthropic", "weight": 30}
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx"]},
      "retry": 2,
      "to": "anthropic"
    }
  ],
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  }
}
```

**Behavior**:
- Policy парсится успешно
- Только публичные DSL поля используются
- Рекомендуемый формат для новых policies

## CP2/Pre-release Compliance

**Status**: ✅ **COMPLIANT**

**Rationale**:
- Поля помечены как deprecated в schema.json
- Поля не включены в публичный DSL (ROUTING_POLICY.md)
- Поля не влияют на core routing behavior
- Поля могут быть использованы в CP2 для внутреннего хранения или будущих функций
- Не критично для core routing behavior, если уже сегодня не экспонируется наружу

**CP2 Readiness**:
- ✅ Поля могут быть специфицированы и реализованы в CP2, если понадобятся
- ✅ Поля могут быть использованы для внутреннего хранения
- ✅ Поля не ломают существующие policies

## Files Modified

1. `apps/otp/router/docs/schemas/policy.schema.json` - поля помечены как deprecated
2. `docs/ROUTING_POLICY.md` - добавлен раздел "Deprecated Fields"
3. `docs/archive/dev/POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md` - обновлен статус
4. `docs/archive/dev/METADATA_DEFAULTS_DEPRECATION_REPORT.md` - этот отчет

## Files Verified

1. `apps/otp/router/src/router_policy_store.erl` - парсинг полей (не используется в routing)
2. `apps/otp/router/src/router_decider.erl` - поля не используются
3. `apps/otp/router/src/router_policy_applier.erl` - поля не используются
4. `apps/otp/router/include/beamline_router.hrl` - поля в #policy{} record

## References

- `docs/ROUTING_POLICY.md` - основная спецификация JSON-DSL (обновлена)
- `apps/otp/router/docs/schemas/policy.schema.json` - JSON Schema (обновлена)
- `docs/archive/dev/POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md` - отчет о реализации (обновлен)
- `apps/otp/router/src/router_policy_store.erl` - парсинг полей

## Summary

✅ **All deprecated fields documented**:
- ✅ `metadata` - помечен как deprecated
- ✅ `defaults` - помечен как deprecated
- ✅ `escalate_on` - помечен как deprecated
- ✅ Schema обновлена с `deprecated: true`
- ✅ ROUTING_POLICY.md обновлена с разделом "Deprecated Fields"
- ✅ Backward compatibility сохранена (поля парсятся, но игнорируются)

**Critical Gap Closed**: Поля `metadata`, `defaults`, и `escalate_on` теперь явно помечены как deprecated и не являются частью публичного DSL для routing.

