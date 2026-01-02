# Policy Schema and Fixtures Update Report

## Purpose

Обновление `policy.schema.json` и расширение набора JSON-фикстур для полного покрытия DSL формата:
- Проверка схемы на полноту описания DSL
- Добавление недостающих полей (extensions)
- Расширение набора фикстур для всех сценариев

## Status

✅ **COMPLETED** - Схема обновлена, фикстуры расширены

## Schema Updates

### Added: Extensions Support

**Added Fields**:
- `pre` - array of pre-processor extensions
- `validators` - array of validator extensions
- `post` - array of post-processor extensions

**Schema Structure**:
```json
{
  "pre": [{
    "id": "string (required)",
    "mode": "required | optional (default: optional)",
    "config": "object (optional)"
  }],
  "validators": [{
    "id": "string (required)",
    "on_fail": "block | warn | ignore (default: block)"
  }],
  "post": [{
    "id": "string (required)",
    "mode": "required | optional (default: optional)",
    "config": "object (optional)"
  }]
}
```

### Schema Validation

**Current Coverage**:
- ✅ `providers[]` - JSON-DSL format (0-100 weights)
- ✅ `weights{}` - legacy format (0.0-1.0 or 0-100)
- ✅ `fallbacks[]` - JSON-DSL format with when/retry/to
- ✅ `fallback{}` - legacy format
- ✅ `sticky{}` - both JSON-DSL (ttl string) and legacy (ttl_seconds)
- ✅ `pre[]` - **NEW** pre-processor extensions
- ✅ `validators[]` - **NEW** validator extensions
- ✅ `post[]` - **NEW** post-processor extensions
- ✅ `metadata{}` - additional metadata
- ✅ `defaults{}` - default values
- ✅ `escalate_on[]` - escalation conditions

**Validation Rules**:
- `anyOf`: requires either `providers` or `weights`
- All extension arrays are optional
- All extension items require `id` field
- Validators require `on_fail` enum value

## Fixtures Created

### 1. Complex Fallback Chains

**File**: `complex_fallbacks.json`

**Features**:
- Multiple fallback rules (4 rules)
- Different retry counts (1, 2, 3)
- Single and multiple condition values
- Complex when conditions (multiple fields)

**Example**:
```json
{
  "fallbacks": [
    {"when": {"status": ["timeout"]}, "retry": 3, "to": "anthropic"},
    {"when": {"status": ["5xx", "503"]}, "retry": 2, "to": "cohere"},
    {"when": {"status": ["rate_limited", "429"]}, "retry": 1, "to": "local_llm"},
    {"when": {"status": ["timeout"], "message_type": ["chat"]}, "retry": 2, "to": "anthropic"}
  ]
}
```

### 2. Sticky + Weights Together

**File**: `sticky_weights.json`

**Features**:
- Sticky routing enabled
- Weighted distribution (60/40)
- Fallback rules
- Combined behavior: sticky first, then weights, then fallback

**Example**:
```json
{
  "providers": [{"name": "openai", "weight": 60}, {"name": "anthropic", "weight": 40}],
  "sticky": {"enabled": true, "session_key": "user_id", "ttl": "15m"},
  "fallbacks": [{"when": {"status": ["timeout", "5xx"]}, "retry": 2, "to": "anthropic"}]
}
```

### 3. Extensions - Full Configuration

**File**: `extensions_full.json`

**Features**:
- All extension types: pre, validators, post
- Multiple extensions per type
- Config objects with various fields
- All on_fail modes: block, warn, ignore

**Example**:
```json
{
  "pre": [
    {"id": "normalize_text", "mode": "required", "config": {"lowercase": true}},
    {"id": "add_context", "mode": "optional", "config": {"include_timestamp": true}}
  ],
  "validators": [
    {"id": "pii_guard", "on_fail": "block"},
    {"id": "content_filter", "on_fail": "warn"},
    {"id": "rate_limit_check", "on_fail": "ignore"}
  ],
  "post": [
    {"id": "mask_pii", "mode": "required", "config": {"mask_email": true}},
    {"id": "format_response", "mode": "optional", "config": {"pretty_print": false}}
  ]
}
```

### 4. Extensions - Minimal Configuration

**File**: `extensions_minimal.json`

**Features**:
- Minimal extensions (one per type)
- No config objects (only required fields)
- Default mode values

### 5. Extensions - Individual Types

**Files**:
- `extensions_pre_only.json` - only pre-processors
- `extensions_validators_only.json` - only validators
- `extensions_post_only.json` - only post-processors

**Purpose**: Тестирование каждого типа extensions отдельно

### 6. Legacy Format

**File**: `legacy_format.json`

**Features**:
- Pure legacy format (no JSON-DSL fields)
- `weights` map (0.0-1.0)
- `fallback` object (not array)
- `ttl_seconds` (not ttl string)

**Example**:
```json
{
  "weights": {"openai": 0.7, "anthropic": 0.3},
  "fallback": {"provider": "local_llm", "conditions": ["all_providers_failed", "timeout"]},
  "sticky": {"enabled": true, "ttl_seconds": 3600}
}
```

### 7. Mixed Format

**File**: `mixed_format.json`

**Features**:
- Combination of JSON-DSL and legacy format
- `providers[]` (new) + `fallback{}` (legacy)
- `fallbacks[]` (new) + `fallback{}` (legacy)
- `ttl` string (new) + `ttl_seconds` (legacy)
- Extensions (new format)

**Purpose**: Тестирование backward compatibility и смешанного формата

## Fixtures Summary

| Fixture | Format | Features |
|---------|--------|----------|
| `default.json` | JSON-DSL | Basic: providers, fallbacks, sticky |
| `complex_fallbacks.json` | JSON-DSL | 4 fallback rules, different retry counts |
| `sticky_weights.json` | JSON-DSL | Sticky + weights + fallbacks |
| `extensions_full.json` | JSON-DSL | All extension types with configs |
| `extensions_minimal.json` | JSON-DSL | Minimal extensions (no configs) |
| `extensions_pre_only.json` | JSON-DSL | Only pre-processors |
| `extensions_validators_only.json` | JSON-DSL | Only validators |
| `extensions_post_only.json` | JSON-DSL | Only post-processors |
| `legacy_format.json` | Legacy | Pure legacy format |
| `mixed_format.json` | Mixed | JSON-DSL + legacy together |

**Total**: 10 fixtures covering all scenarios

## Schema Validation

### Required Fields

**Current**:
- `version` - always required
- `providers` OR `weights` - at least one required (via anyOf)

**Optional**:
- `fallbacks[]` - optional (new format)
- `fallback{}` - optional (legacy format)
- `sticky{}` - optional
- `pre[]` - optional
- `validators[]` - optional
- `post[]` - optional
- `metadata{}` - optional
- `defaults{}` - optional
- `escalate_on[]` - optional

### Field Constraints

**Providers**:
- `name`: pattern `^[a-z0-9_]+$`, 1-255 chars
- `weight`: integer, 0-100

**Fallbacks**:
- `when`: object with pattern properties
- `retry`: integer, 0-10, default 1
- `to`: pattern `^[a-z0-9_]+$`, 1-255 chars

**Sticky**:
- `enabled`: boolean, required
- `session_key`: string, 1-255 chars, default "session_id"
- `ttl`: pattern `^[0-9]+[smh]$` (JSON-DSL)
- `ttl_seconds`: integer, 1-86400 (legacy)

**Extensions**:
- `id`: pattern `^[a-z0-9_]+$`, 1-255 chars, required
- `mode`: enum ["required", "optional"], default "optional" (pre/post)
- `on_fail`: enum ["block", "warn", "ignore"], default "block" (validators)
- `config`: object, additionalProperties: true

## Testing Recommendations

### Schema Validation Tests

1. **Valid Policies**:
   - All fixtures should pass schema validation
   - Test with JSON schema validator

2. **Invalid Policies**:
   - Missing required fields
   - Invalid field types
   - Invalid enum values
   - Pattern mismatches

### Fixture Loading Tests

1. **Parse All Fixtures**:
   - Load each fixture via `router_policy_store:parse_policy_map/3`
   - Verify all fields parsed correctly
   - Verify backward compatibility

2. **Apply All Fixtures**:
   - Apply each policy via `router_policy_applier:apply_policy/3`
   - Verify extensions extraction
   - Verify explanation building

## Files Modified

1. `apps/otp/router/docs/schemas/policy.schema.json` - добавлены extensions (pre, validators, post)

## Files Created

1. `apps/otp/router/priv/fixtures/policies/default_tenant/complex_fallbacks.json`
2. `apps/otp/router/priv/fixtures/policies/default_tenant/sticky_weights.json`
3. `apps/otp/router/priv/fixtures/policies/default_tenant/extensions_full.json`
4. `apps/otp/router/priv/fixtures/policies/default_tenant/extensions_minimal.json`
5. `apps/otp/router/priv/fixtures/policies/default_tenant/extensions_pre_only.json`
6. `apps/otp/router/priv/fixtures/policies/default_tenant/extensions_validators_only.json`
7. `apps/otp/router/priv/fixtures/policies/default_tenant/extensions_post_only.json`
8. `apps/otp/router/priv/fixtures/policies/default_tenant/legacy_format.json`
9. `apps/otp/router/priv/fixtures/policies/default_tenant/mixed_format.json`

## Next Steps

1. **Schema Validation**:
   - Добавить тесты валидации всех фикстур против схемы
   - Проверить edge cases (пустые массивы, отсутствующие поля)

2. **Fixture Loading**:
   - Добавить тесты загрузки всех фикстур
   - Проверить парсинг всех полей

3. **Documentation**:
   - Обновить `docs/ROUTING_POLICY.md` с примерами из фикстур
   - Добавить ссылки на фикстуры в документации

## References

- `apps/otp/router/docs/schemas/policy.schema.json` - обновленная схема
- `apps/otp/router/priv/fixtures/policies/default_tenant/*.json` - все фикстуры
- `docs/ROUTING_POLICY.md` - JSON-DSL спецификация
- `docs/EXTENSIONS_API.md` - extensions спецификация

