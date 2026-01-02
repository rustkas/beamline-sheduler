# Extensions в ROUTING_POLICY.md - Synchronization Report

## Purpose

Добавление раздела про extensions в `ROUTING_POLICY.md` и синхронизация со всеми компонентами:
- Раздел про extensions (pre/validators/post: структура, поля, mode, on_fail, config)
- Обновлённый полный пример policy JSON с extensions
- Синхронизация с EXTENSIONS_API.md (кросс-линк)
- Синхронизация с policy.schema.json
- Синхронизация с реализованной моделью в #policy{} и парсинге

## Status

✅ **COMPLETED** - Раздел добавлен, все компоненты синхронизированы

## Changes Made

### 1. ROUTING_POLICY.md - Extensions Section Added

**New Section**: "Extensions"

**Location**: После раздела "Sticky Sessions", перед "Decision Explanation Format"

**Content**:
- **Overview**: Описание extensions как отдельных NATS сервисов
- **Extension Types**: Описание трех типов (pre, validators, post)
- **Invariants**: Гарантии Router для extensions
- **Pre-processor Extensions**: Структура, поля (id, mode, config), поведение
- **Validator Extensions**: Структура, поля (id, on_fail), поведение
- **Post-processor Extensions**: Структура, поля (id, mode, config), поведение
- **Extension Registry**: Описание Registry и ссылка на EXTENSIONS_API.md
- **Implementation**: Модули и тесты

**Cross-links**:
- `docs/EXTENSIONS_API.md` - ссылка на полную документацию Extension API (2 места)

### 2. ROUTING_POLICY.md - Updated Examples

**Updated Sections**:
- **Structure** (начало документа): Добавлены extensions в базовый пример
- **Examples**: Добавлен полный пример "Complete policy with extensions"

**New Example**:
```json
{
  "version": "1.0",
  "providers": [...],
  "fallbacks": [...],
  "sticky": {...},
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "config": {...}
    }
  ],
  "validators": [
    {
      "id": "pii_guard",
      "on_fail": "block"
    }
  ],
  "post": [
    {
      "id": "mask_pii",
      "mode": "required",
      "config": {...}
    }
  ]
}
```

### 3. Synchronization Verification

#### policy.schema.json

**Status**: ✅ **SYNCHRONIZED**

**Verification**:
- ✅ `pre[]` - array of objects with `id` (required), `mode` (optional, enum: "required"|"optional"), `config` (optional, object)
- ✅ `validators[]` - array of objects with `id` (required), `on_fail` (optional, enum: "block"|"warn"|"ignore")
- ✅ `post[]` - array of objects with `id` (required), `mode` (optional, enum: "required"|"optional"), `config` (optional, object)
- ✅ Schema соответствует описанию в ROUTING_POLICY.md

**Schema Structure**:
```json
{
  "pre": {
    "type": "array",
    "items": {
      "type": "object",
      "required": ["id"],
      "properties": {
        "id": {"type": "string", "pattern": "^[a-z0-9_]+$"},
        "mode": {"type": "string", "enum": ["required", "optional"], "default": "optional"},
        "config": {"type": "object", "additionalProperties": true}
      }
    }
  },
  "validators": {
    "type": "array",
    "items": {
      "type": "object",
      "required": ["id"],
      "properties": {
        "id": {"type": "string", "pattern": "^[a-z0-9_]+$"},
        "on_fail": {"type": "string", "enum": ["block", "warn", "ignore"], "default": "block"}
      }
    }
  },
  "post": {
    "type": "array",
    "items": {
      "type": "object",
      "required": ["id"],
      "properties": {
        "id": {"type": "string", "pattern": "^[a-z0-9_]+$"},
        "mode": {"type": "string", "enum": ["required", "optional"], "default": "optional"},
        "config": {"type": "object", "additionalProperties": true}
      }
    }
  }
}
```

#### router_policy_store.erl

**Status**: ✅ **SYNCHRONIZED**

**Verification**:
- ✅ `parse_pre_extensions/1` - парсит `pre[]` array, извлекает `id`, `mode` (default: "optional"), `config`
- ✅ `parse_validator_extensions/1` - парсит `validators[]` array, извлекает `id`, `on_fail` (default: "block")
- ✅ `parse_post_extensions/1` - парсит `post[]` array, извлекает `id`, `mode` (default: "optional"), `config`
- ✅ Парсинг соответствует описанию в ROUTING_POLICY.md

**Implementation**:
```erlang
parse_pre_extensions(PreList) ->
    lists:map(fun(PreItem) ->
        #{<<"id">> := Id} = PreItem,
        Mode = maps:get(<<"mode">>, PreItem, <<"optional">>),
        Config = maps:get(<<"config">>, PreItem, #{}),
        #{id => Id, mode => Mode, config => Config}
    end, PreList).

parse_validator_extensions(ValidatorsList) ->
    lists:map(fun(ValidatorItem) ->
        #{<<"id">> := Id} = ValidatorItem,
        OnFail = maps:get(<<"on_fail">>, ValidatorItem, <<"block">>),
        #{id => Id, on_fail => OnFail}
    end, ValidatorsList).

parse_post_extensions(PostList) ->
    lists:map(fun(PostItem) ->
        #{<<"id">> := Id} = PostItem,
        Mode = maps:get(<<"mode">>, PostItem, <<"optional">>),
        Config = maps:get(<<"config">>, PostItem, #{}),
        #{id => Id, mode => Mode, config => Config}
    end, PostList).
```

#### beamline_router.hrl (#policy{} record)

**Status**: ✅ **SYNCHRONIZED**

**Verification**:
- ✅ `pre = [] :: list()` - Pre-processor extensions: `[{id, mode, config}]`
- ✅ `validators = [] :: list()` - Validator extensions: `[{id, on_fail}]`
- ✅ `post = [] :: list()` - Post-processor extensions: `[{id, mode, config}]`
- ✅ Record структура соответствует описанию в ROUTING_POLICY.md

**Record Structure**:
```erlang
-record(policy, {
    ...
    pre = [] :: list(),      % Pre-processor extensions: [{id, mode, config}]
    validators = [] :: list(), % Validator extensions: [{id, on_fail}]
    post = [] :: list(),     % Post-processor extensions: [{id, mode, config}]
    ...
}).
```

#### EXTENSIONS_API.md

**Status**: ✅ **CROSS-LINKED**

**Verification**:
- ✅ Кросс-линк добавлен в раздел "Extensions" → "Overview"
- ✅ Кросс-линк добавлен в раздел "Extensions" → "Extension Registry"
- ✅ Формат extensions в ROUTING_POLICY.md соответствует EXTENSIONS_API.md

**Cross-links**:
- `docs/EXTENSIONS_API.md` - ссылка на полную документацию Extension API

### 4. Field Specifications

#### Pre-processor Extensions (`pre[]`)

**Fields**:
- `id` (required, string) — Extension identifier (logical ID from Extension Registry)
  - Pattern: `^[a-z0-9_]+$`
  - Min length: 1, Max length: 255
- `mode` (optional, string) — Extension mode
  - Values: `"required"` | `"optional"`
  - Default: `"optional"`
- `config` (optional, object) — Per-policy extension configuration
  - Extension-specific structure
  - Additional properties allowed

**Behavior**:
- Execute **before** provider selection
- `mode: "required"` → fail if extension fails
- `mode: "optional"` → continue with original message if extension fails

#### Validator Extensions (`validators[]`)

**Fields**:
- `id` (required, string) — Extension identifier (logical ID from Extension Registry)
  - Pattern: `^[a-z0-9_]+$`
  - Min length: 1, Max length: 255
- `on_fail` (optional, string) — Behavior on validation failure
  - Values: `"block"` | `"warn"` | `"ignore"`
  - Default: `"block"`

**Behavior**:
- Execute **after** pre-processors, **before** provider selection
- `on_fail: "block"` → reject request if validation fails
- `on_fail: "warn"` → log warning, continue processing
- `on_fail: "ignore"` → ignore failure, continue silently

#### Post-processor Extensions (`post[]`)

**Fields**:
- `id` (required, string) — Extension identifier (logical ID from Extension Registry)
  - Pattern: `^[a-z0-9_]+$`
  - Min length: 1, Max length: 255
- `mode` (optional, string) — Extension mode
  - Values: `"required"` | `"optional"`
  - Default: `"optional"`
- `config` (optional, object) — Per-policy extension configuration
  - Extension-specific structure
  - Additional properties allowed

**Behavior**:
- Execute **after** provider response
- `mode: "required"` → return error if extension fails
- `mode: "optional"` → return original response if extension fails

## Examples

### Complete Policy with Extensions

**Full Example** (added to ROUTING_POLICY.md):
```json
{
  "version": "1.0",
  "providers": [
    { "name": "provider_a", "weight": 70 },
    { "name": "provider_b", "weight": 30 }
  ],
  "fallbacks": [
    {
      "when": { "status": ["timeout", "5xx"] },
      "retry": 2,
      "to": "provider_b"
    }
  ],
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  },
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "config": {
        "lowercase": true,
        "trim_whitespace": true
      }
    },
    {
      "id": "add_context",
      "mode": "optional",
      "config": {
        "include_timestamp": true
      }
    }
  ],
  "validators": [
    {
      "id": "pii_guard",
      "on_fail": "block"
    },
    {
      "id": "content_filter",
      "on_fail": "warn"
    }
  ],
  "post": [
    {
      "id": "mask_pii",
      "mode": "required",
      "config": {
        "mask_email": true,
        "mask_phone": true
      }
    }
  ]
}
```

## Synchronization Matrix

| Component | Status | Verification |
|-----------|--------|--------------|
| **ROUTING_POLICY.md** | ✅ Updated | Extensions section added, examples updated |
| **EXTENSIONS_API.md** | ✅ Cross-linked | 2 cross-links added |
| **policy.schema.json** | ✅ Synchronized | Schema matches specification |
| **router_policy_store.erl** | ✅ Synchronized | Parsing matches specification |
| **beamline_router.hrl** | ✅ Synchronized | #policy{} record matches specification |
| **Fixtures** | ✅ Valid | extensions_full.json validates against schema |

## CP1/CP2 Boundary Compliance

**Status**: ✅ **COMPLIANT**

**Rationale**:
- Extensions спецификация добавлена в CP1-документацию (ROUTING_POLICY.md)
- Спецификация является "зачатком" для CP2/Extensions
- Все компоненты синхронизированы
- Формат extensions является контрактным аспектом

**CP2 Readiness**:
- ✅ Базовая спецификация extensions в CP1-документации
- ✅ Полная спецификация в EXTENSIONS_API.md
- ✅ Реализация готова для CP2 расширений

## Files Modified

1. `docs/ROUTING_POLICY.md` - добавлен раздел "Extensions", обновлены примеры
2. `docs/archive/dev/EXTENSIONS_IN_ROUTING_POLICY_SYNC.md` - этот отчет

## Files Verified

1. `docs/EXTENSIONS_API.md` - кросс-линк добавлен
2. `apps/otp/router/docs/schemas/policy.schema.json` - синхронизирован
3. `apps/otp/router/src/router_policy_store.erl` - синхронизирован
4. `apps/otp/router/include/beamline_router.hrl` - синхронизирован
5. `apps/otp/router/priv/fixtures/policies/default_tenant/extensions_full.json` - валиден

## References

- `docs/ROUTING_POLICY.md` - основная спецификация JSON-DSL (обновлена)
- `docs/EXTENSIONS_API.md` - полная документация Extension API
- `apps/otp/router/docs/schemas/policy.schema.json` - JSON Schema для валидации
- `apps/otp/router/src/router_policy_store.erl` - парсинг extensions
- `apps/otp/router/include/beamline_router.hrl` - #policy{} record структура

## Summary

✅ **All components synchronized**:
- ✅ Extensions section added to ROUTING_POLICY.md
- ✅ Complete policy example with extensions added
- ✅ Cross-links to EXTENSIONS_API.md added
- ✅ Schema synchronized
- ✅ Implementation synchronized
- ✅ Record structure synchronized

**Critical Gap Closed**: Extensions теперь формально специфицированы в ROUTING_POLICY.md и синхронизированы со всеми компонентами.

