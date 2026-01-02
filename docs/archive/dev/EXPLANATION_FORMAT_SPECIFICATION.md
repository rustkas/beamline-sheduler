# Explanation Format Specification - Implementation Report

## Purpose

Формальное описание JSON-структуры explanation в `ROUTING_POLICY.md` и синхронизация всех компонентов:
- Спецификация explanation формата
- Синхронизация router_policy_applier
- Синхронизация router_audit:log_decision/1
- Обновление всех тестов

## Status

✅ **COMPLETED** - Формальная спецификация добавлена, все компоненты синхронизированы

## Changes Made

### 1. ROUTING_POLICY.md - Formal Specification Added

**New Section**: "Decision Explanation Format"

**Content**:
- **Purpose**: Описание назначения explanation (audit trail, debugging, analytics)
- **JSON Structure**: Формальная структура с обязательными полями
- **Field Specifications**: Детальное описание каждого поля:
  - `reason` (required): "sticky" | "weighted" | "fallback" | "retry"
  - `provider_id` (required): selected provider identifier
  - `policy_id` (required): policy identifier used
  - `policy_version` (required): policy version (MAJOR.MINOR format)
  - `priority` (required): decision priority (25, 50, 100)
  - `steps` (required): array of step-by-step explanation strings
  - `context` (required): context object with tenant_id (required) and optional fields
- **Examples**: Примеры explanation для всех сценариев (sticky, weighted, fallback, retry)
- **Audit Trail Integration**: Описание использования explanation в audit trail

**Location**: `docs/ROUTING_POLICY.md` - раздел "Decision Explanation Format"

### 2. router_policy_applier.erl - Synchronized with Specification

**Changes**:
- Обновлен `build_explanation/3` для соответствия формальной спецификации
- Добавлена проверка обязательных полей
- Гарантируется наличие `tenant_id` в context (required field)
- Добавлены комментарии со ссылкой на спецификацию

**Key Updates**:
```erlang
%% Build explanation according to formal specification in ROUTING_POLICY.md
%% Required fields: reason, provider_id, policy_id, policy_version, priority, steps, context
Explanation = #{
    reason => Reason,                    % Required: "sticky" | "weighted" | "fallback" | "retry"
    provider_id => ProviderId,           % Required: selected provider ID
    policy_id => PolicyId,              % Required: policy ID used
    policy_version => Version,          % Required: policy version (e.g., "1.0")
    priority => Priority,                % Required: decision priority (25, 50, 100)
    steps => Steps,                     % Required: array of step-by-step explanation strings
    context => FinalContext             % Required: context with tenant_id (required) and optional fields
}
```

### 3. router_audit.erl - Synchronized with Specification

**Changes**:
- Обновлен `log_decision/1` для соответствия формальной спецификации
- Добавлена валидация обязательных полей
- Добавлена валидация значений (reason, priority)
- Обновлены комментарии со ссылкой на спецификацию

**Key Updates**:
```erlang
%% @doc Log routing decision explanation for audit
%% Explanation format according to ROUTING_POLICY.md "Decision Explanation Format" section
%% Required fields: reason, provider_id, policy_id, policy_version, priority, steps, context
%% 
%% Specification: docs/ROUTING_POLICY.md#decision-explanation-format
```

**Validation**:
- `reason` must be one of: "sticky", "weighted", "fallback", "retry"
- `priority` must be one of: 25, 50, 100
- `steps` must be array (list in Erlang)
- `context` must contain `tenant_id` (required field)

### 4. Test Updates

#### router_policy_applier_dsl_SUITE.erl

**Added**:
- Helper function `verify_explanation_format/1` для проверки формата explanation
- Обновлены все тесты для использования helper функции
- Проверка всех обязательных полей согласно спецификации

**Updated Tests**:
- `test_apply_policy_explanation_sticky` - проверяет формат explanation для sticky
- `test_apply_policy_explanation_weighted` - проверяет формат explanation для weighted
- `test_apply_policy_explanation_fallback` - проверяет формат explanation для fallback
- `test_apply_policy_with_providers_array` - проверяет формат explanation
- `test_apply_policy_with_fallbacks_array` - проверяет формат explanation
- `test_apply_policy_with_sticky_enabled` - проверяет формат explanation
- `test_sticky_session_expired` - проверяет формат explanation

#### router_policy_integration_SUITE.erl

**Updated**:
- `test_explanation_format` - расширен для проверки всех обязательных полей
- Добавлена проверка формата согласно спецификации

**New Checks**:
- Все обязательные поля присутствуют
- `reason` - один из валидных значений
- `provider_id` - не пустой
- `policy_id` - не пустой
- `policy_version` - формат MAJOR.MINOR
- `priority` - один из: 25, 50, 100
- `steps` - массив строк
- `context` - содержит `tenant_id`

## Specification Details

### Required Fields

**All fields are required** according to specification:

1. **reason** (string):
   - Type: `binary()` in Erlang, `string` in JSON
   - Values: `"sticky"` | `"weighted"` | `"fallback"` | `"retry"`
   - Never empty or undefined

2. **provider_id** (string):
   - Type: `binary()` in Erlang, `string` in JSON
   - Selected provider identifier
   - Never empty or undefined

3. **policy_id** (string):
   - Type: `binary()` in Erlang, `string` in JSON
   - Policy identifier used
   - Default: `"default"` if not specified

4. **policy_version** (string):
   - Type: `binary()` in Erlang, `string` in JSON
   - Format: `MAJOR.MINOR` (e.g., `"1.0"`, `"2.1"`)
   - Default: `"1.0"` if not specified

5. **priority** (integer):
   - Type: `integer()` in Erlang, `integer` in JSON
   - Values: `25` | `50` | `100`
   - Default: `50` if not specified

6. **steps** (array of strings):
   - Type: `[binary()]` in Erlang, `string[]` in JSON
   - Step-by-step explanation in chronological order
   - May be empty array `[]`

7. **context** (object):
   - Type: `map()` in Erlang, `object` in JSON
   - Required field: `tenant_id` (string)
   - Optional fields: `trace_id`, `retry_attempt`, `retry_max`, etc.
   - PII filtered before logging

### Field Validation

**Reason Validation**:
- Must be one of: `"sticky"`, `"weighted"`, `"fallback"`, `"retry"`
- Invalid values default to `"unknown"` in router_audit

**Priority Validation**:
- Must be one of: `25`, `50`, `100`
- Invalid values default to `50` in router_audit

**Steps Validation**:
- Must be array (list in Erlang)
- All elements must be binaries (strings)
- Empty array `[]` is valid

**Context Validation**:
- Must be map/object
- Must contain `tenant_id` field
- If `tenant_id` not available, set to `"unknown"`

## Examples

### Sticky Session Decision

```json
{
  "reason": "sticky",
  "provider_id": "openai",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 100,
  "steps": [
    "1. Checked sticky session: found existing provider for key session_id = abc123"
  ],
  "context": {
    "tenant_id": "tenant_1",
    "trace_id": "trace_abc123"
  }
}
```

### Weighted Distribution Decision

```json
{
  "reason": "weighted",
  "provider_id": "anthropic",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 50,
  "steps": [
    "1. Checked sticky session: no existing session found",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00"
  ],
  "context": {
    "tenant_id": "tenant_1",
    "trace_id": "trace_abc123"
  }
}
```

### Fallback After Retry Decision

```json
{
  "reason": "fallback",
  "provider_id": "cohere",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 25,
  "steps": [
    "1. Checked sticky session: no existing session found",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00",
    "3. Applied fallback rule after 2/3 retry attempts exhausted"
  ],
  "context": {
    "tenant_id": "tenant_1",
    "trace_id": "trace_abc123",
    "retry_attempts_used": 2,
    "retry_max": 3,
    "fallback_rule_id": "abc123def456",
    "fallback_applied": true
  }
}
```

### Retry Attempt Decision

```json
{
  "reason": "retry",
  "provider_id": "openai",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 50,
  "steps": [
    "1. Checked sticky session: no existing session found",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00",
    "3. Retry attempt 1/3 with backoff 100ms"
  ],
  "context": {
    "tenant_id": "tenant_1",
    "trace_id": "trace_abc123",
    "retry_attempt": 1,
    "retry_max": 3,
    "backoff_ms": 100,
    "backoff_applied": true,
    "fallback_rule_id": "abc123def456"
  }
}
```

## Implementation Synchronization

### router_policy_applier.erl

**Status**: ✅ **SYNCHRONIZED**

**Compliance**:
- ✅ Все обязательные поля присутствуют
- ✅ `tenant_id` всегда присутствует в context
- ✅ Формат соответствует спецификации
- ✅ Комментарии ссылаются на спецификацию

### router_audit.erl

**Status**: ✅ **SYNCHRONIZED**

**Compliance**:
- ✅ Валидация обязательных полей
- ✅ Валидация значений (reason, priority)
- ✅ PII filtering перед логированием
- ✅ Комментарии ссылаются на спецификацию

### Tests

**Status**: ✅ **SYNCHRONIZED**

**Compliance**:
- ✅ Helper функция `verify_explanation_format/1` проверяет все обязательные поля
- ✅ Все тесты обновлены для использования helper функции
- ✅ Проверка формата согласно спецификации

## CP1/CP2 Boundary Compliance

**Status**: ✅ **COMPLIANT**

**Rationale**:
- Формальная спецификация добавлена в `ROUTING_POLICY.md`
- Все компоненты синхронизированы с спецификацией
- Тесты проверяют соответствие спецификации
- Формат explanation является контрактным аспектом и закрыт в CP1/CP2 boundary

## Files Modified

1. `docs/ROUTING_POLICY.md` - добавлена формальная спецификация explanation
2. `apps/otp/router/src/router_policy_applier.erl` - синхронизирован с спецификацией
3. `apps/otp/router/src/router_audit.erl` - синхронизирован с спецификацией
4. `apps/otp/router/test/router_policy_applier_dsl_SUITE.erl` - обновлены тесты
5. `apps/otp/router/test/router_policy_integration_SUITE.erl` - обновлены тесты

## Files Created

1. `docs/archive/dev/EXPLANATION_FORMAT_SPECIFICATION.md` - этот отчет

## References

- `docs/ROUTING_POLICY.md` - формальная спецификация explanation формата
- `apps/otp/router/src/router_policy_applier.erl` - реализация explanation
- `apps/otp/router/src/router_audit.erl` - логирование explanation
- `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md` - gap analysis (explanation format gap закрыт)

## Summary

✅ **All components synchronized** with formal specification:
- ✅ Specification added to ROUTING_POLICY.md
- ✅ router_policy_applier synchronized
- ✅ router_audit synchronized
- ✅ All tests updated

**Critical Gap Closed**: Explanation format теперь формально специфицирован и синхронизирован во всех компонентах.

