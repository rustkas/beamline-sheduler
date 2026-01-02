# Policy DSL Gaps CP Classification

## Purpose

Классификация gaps из `POLICY_DSL_SPEC_VS_IMPL_GAP.md` по CP1/CP2 для планирования и управления прогрессом.

## CP Classification Legend

- **CP1-blocker**: Блокирует завершение CP1 (критично для CP1 acceptance)
- **CP1-nice**: Желательно для CP1, но не блокирует (улучшение качества)
- **CP2**: Для CP2-LC или CP2+ (расширенная функциональность)
- **Pre-release**: Для pre-release (качество и защита от регрессий)

## ✅ CLOSED Gaps (CP1-Complete)

### 1. Explanation Format ✅

**Status**: ✅ **CLOSED** (2025-01-27)

**CP Tag**: **CP1-blocker** → ✅ **CLOSED**

**Details**:
- Формальная спецификация добавлена в `ROUTING_POLICY.md`
- Все обязательные поля описаны (reason, provider_id, policy_id, policy_version, priority, steps, context)
- Примеры для всех сценариев (sticky, weighted, fallback, retry)
- Связь с audit trail документирована

**Reference**: `docs/ROUTING_POLICY.md` - раздел "Decision Explanation Format"

### 2. Extensions Format ✅

**Status**: ✅ **CLOSED** (2025-01-27)

**CP Tag**: **CP1-blocker** → ✅ **CLOSED**

**Details**:
- Раздел "Extensions" добавлен в `ROUTING_POLICY.md`
- Описание структуры pre/validators/post
- Описание полей (id, mode, on_fail, config)
- Кросс-линк на `EXTENSIONS_API.md`
- Примеры использования extensions

**Reference**: `docs/ROUTING_POLICY.md` - раздел "Extensions"

### 3. Retry and Backoff ✅

**Status**: ✅ **CLOSED** (2025-01-27)

**CP Tag**: **CP1-blocker** → ✅ **CLOSED**

**Details**:
- Retry логика реализована в `router_decider:check_fallbacks_with_retry/3`
- Backoff стратегии реализованы (exponential, linear, fixed)
- Спецификация обновлена в `ROUTING_POLICY.md` (разделы "Retry Semantics" и "Backoff Semantics")
- Примеры использования retry/backoff добавлены

**Reference**: 
- `docs/ROUTING_POLICY.md` - разделы "Retry Semantics" и "Backoff Semantics"
- `apps/otp/router/src/router_decider.erl` - `check_fallbacks_with_retry/3`, `calculate_backoff/3`

### 4. Metadata/Defaults/Escalate_on Deprecation ✅

**Status**: ✅ **CLOSED** (2025-01-27)

**CP Tag**: **CP1-nice** → ✅ **CLOSED**

**Details**:
- `metadata`, `defaults`, `escalate_on` помечены как deprecated в `policy.schema.json`
- Раздел "Deprecated Fields" добавлен в `ROUTING_POLICY.md`
- Поля парсятся, но не используются в routing logic

**Reference**: 
- `apps/otp/router/docs/schemas/policy.schema.json` - deprecated: true
- `docs/ROUTING_POLICY.md` - раздел "Deprecated Fields"

### 5. Policy Schema CI Validation ✅

**Status**: ✅ **CLOSED** (2025-01-27)

**CP Tag**: **Pre-release** → ✅ **CLOSED**

**Details**:
- Скрипт `scripts/check_policy_schema.sh` создан
- Интегрирован в `check_schema_changes.sh` и `run_checks.sh`
- Валидация schema и всех fixtures работает

**Reference**: `docs/archive/dev/POLICY_SCHEMA_CI_VALIDATION_REPORT.md`

## 📋 Open Gaps (By CP Classification)

### CP1-nice (Желательно для CP1, но не блокирует)

#### 1. Explanation Levels ✅ SPECIFIED (CP2 Enhancement)

**CP Tag**: **CP2** → ✅ **SPECIFIED** (2025-01-27)

**Priority**: Средний

**Description**: Уровни детализации explanation (minimal, detailed, verbose)

**Status**: ✅ **SPECIFIED** (базовая реализация существует, полная спецификация для CP2)
- Базовая поддержка `detail_level` существует в `router_policy_applier.erl`
- Три уровня: `"minimal"`, `"detailed"` (default), `"verbose"`
- **Draft-спецификация создана**: `docs/archive/dev/EXPLANATION_LEVELS_SPEC.md`
- Спецификация добавлена в `ROUTING_POLICY.md` как CP2 enhancement
- Полная реализация (policy-level configuration, environment defaults) отложена до CP2

**Impact**: 
- Управление объёмом данных в логах/audit без ломки контракта
- Minimal: ~70-130 bytes per decision
- Detailed: ~300-600 bytes per decision
- Verbose: ~800-1500 bytes per decision

**Reference**: 
- `docs/archive/dev/EXPLANATION_LEVELS_SPEC.md` - **comprehensive draft specification**
- `docs/ROUTING_POLICY.md` - раздел "Detail Levels" и "CP2 Enhancements" → "Explanation Detail Levels"
- `apps/otp/router/src/router_policy_applier.erl` - базовая реализация `build_explanation_steps/6`

### CP2 (Для CP2-LC или CP2+)

#### 1. Future Extension Fields ✅ SPECIFIED

**CP Tag**: **CP2** → ✅ **SPECIFIED** (2025-01-27)

**Priority**: Средний

**Description**: timeout_ms, retry в Policy (per-policy override, не только Registry)

**Status**: ✅ **SPECIFIED** (реализация отложена до CP2)
- Спецификация добавлена в `ROUTING_POLICY.md` (раздел "CP2 Enhancements")
- Schema обновлена в `policy.schema.json` (поля `timeout_ms`, `retry` в pre/validators/post)
- Реализация отложена до CP2-LC

**Impact**: Per-policy override для extension timeout/retry

**Reference**: 
- `docs/ROUTING_POLICY.md` - раздел "CP2 Enhancements" → "Extension Timeout and Retry Override"
- `apps/otp/router/docs/schemas/policy.schema.json` - поля `timeout_ms`, `retry` в extensions

#### 2. Circuit Breaker в Policy DSL ✅ SPECIFIED

**CP Tag**: **CP2** → ✅ **SPECIFIED** (2025-01-27)

**Priority**: Средний

**Description**: Circuit breaker configuration в Policy DSL

**Status**: ✅ **SPECIFIED** (реализация отложена до CP2)
- Спецификация добавлена в `ROUTING_POLICY.md` (раздел "CP2 Enhancements")
- Schema обновлена в `policy.schema.json` (поле `circuit_breaker`)
- Реализация отложена до CP2-LC

**Impact**: Улучшение reliability через circuit breaker

**Reference**: 
- `docs/ROUTING_POLICY.md` - раздел "CP2 Enhancements" → "Circuit Breaker Configuration"
- `apps/otp/router/docs/schemas/policy.schema.json` - поле `circuit_breaker`

#### 3. Rate Limit в Policy DSL ✅ SPECIFIED

**CP Tag**: **CP2** → ✅ **SPECIFIED** (2025-01-27)

**Priority**: Средний

**Description**: Per-policy rate limiting (сейчас только per-tenant)

**Status**: ✅ **SPECIFIED** (реализация отложена до CP2)
- Спецификация добавлена в `ROUTING_POLICY.md` (раздел "CP2 Enhancements")
- Schema обновлена в `policy.schema.json` (поле `rate_limit`)
- Реализация отложена до CP2-LC

**Impact**: Per-policy rate limiting

**Reference**: 
- `docs/ROUTING_POLICY.md` - раздел "CP2 Enhancements" → "Per-Policy Rate Limiting"
- `apps/otp/router/docs/schemas/policy.schema.json` - поле `rate_limit`

#### 4. Per-Policy Timeout ✅ SPECIFIED

**CP Tag**: **CP2** → ✅ **SPECIFIED** (2025-01-27)

**Priority**: Средний

**Description**: Timeout configuration в Policy DSL

**Status**: ✅ **SPECIFIED** (реализация отложена до CP2)
- Спецификация добавлена в `ROUTING_POLICY.md` (раздел "CP2 Enhancements")
- Schema обновлена в `policy.schema.json` (поле `timeout_ms`)
- Реализация отложена до CP2-LC

**Impact**: Per-policy timeout override

**Reference**: 
- `docs/ROUTING_POLICY.md` - раздел "CP2 Enhancements" → "Per-Policy Timeout"
- `apps/otp/router/docs/schemas/policy.schema.json` - поле `timeout_ms`

#### 5. Provider Priority (Separate from Weights) ✅ SPECIFIED

**CP Tag**: **CP2+** → ✅ **SPECIFIED** (2025-01-27)

**Priority**: Низкий

**Description**: Provider priority field (независимо от weights)

**Status**: ✅ **SPECIFIED** (реализация отложена до CP2+)
- Спецификация добавлена в `ROUTING_POLICY.md` (раздел "CP2 Enhancements")
- Schema обновлена в `policy.schema.json` (поле `priority` в providers)
- Реализация отложена до CP2+

**Impact**: Приоритет провайдеров независимо от weights

**Reference**: 
- `docs/ROUTING_POLICY.md` - раздел "CP2 Enhancements" → "Provider Priority (Separate from Weights)"
- `apps/otp/router/docs/schemas/policy.schema.json` - поле `priority` в providers

#### 6. Health Check в Policy DSL ✅ SPECIFIED

**CP Tag**: **CP2+** → ✅ **SPECIFIED** (2025-01-27)

**Priority**: Низкий

**Description**: Health check configuration в Policy DSL

**Status**: ✅ **SPECIFIED** (реализация отложена до CP2+)
- Спецификация добавлена в `ROUTING_POLICY.md` (раздел "CP2 Enhancements")
- Schema обновлена в `policy.schema.json` (поле `health_check`)
- Реализация отложена до CP2+

**Impact**: Per-policy health check configuration

**Reference**: 
- `docs/ROUTING_POLICY.md` - раздел "CP2 Enhancements" → "Health Check Configuration"
- `apps/otp/router/docs/schemas/policy.schema.json` - поле `health_check`

## Summary

### CP1 Status

**✅ All CP1-blocker gaps CLOSED**:
- Explanation Format ✅
- Extensions Format ✅
- Retry and Backoff ✅

**📋 CP1-nice gaps (moved to CP2)**:
- Explanation Levels → **CP2 Enhancement** (базовая реализация существует, полная спецификация для CP2)

### CP2 Status

**✅ CP2 Enhancements SPECIFIED (7 items)**:
1. **Explanation Detail Levels** ✅ SPECIFIED (draft-спецификация создана, базовая реализация существует, полная реализация отложена до CP2)
2. Future Extension Fields ✅ SPECIFIED (реализация отложена до CP2)
3. Circuit Breaker в Policy DSL ✅ SPECIFIED (реализация отложена до CP2)
4. Rate Limit в Policy DSL ✅ SPECIFIED (реализация отложена до CP2)
5. Per-Policy Timeout ✅ SPECIFIED (реализация отложена до CP2)
6. Provider Priority ✅ SPECIFIED (реализация отложена до CP2+)
7. Health Check в Policy DSL ✅ SPECIFIED (реализация отложена до CP2+)

**Note**: Все CP2 enhancements специфицированы в `ROUTING_POLICY.md` и `policy.schema.json`. Explanation Detail Levels имеет дополнительную draft-спецификацию в `docs/archive/dev/EXPLANATION_LEVELS_SPEC.md`. Реализация отложена до CP2-LC/CP2+ в соответствии с планом.

### Pre-release Status

**✅ Pre-release gaps CLOSED**:
- Policy Schema CI Validation ✅

## Integration with CP Documents

### CP1 Acceptance Report

**Updated**: `docs/archive/dev/CP1_ACCEPTANCE_REPORT.md`
- Добавлен раздел "Policy DSL Status (2025-01-27)"
- Перечислены все CP1-complete gaps
- Перечислены CP2 enhancements

### CP2 Router Plan

**Updated**: `docs/archive/dev/CP2_ROUTER_PLAN.md`
- Добавлен раздел "Policy DSL Enhancements (CP2)"
- Перечислены все CP2 enhancements с приоритетами

## References

- **Gap Analysis**: `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md` - полный анализ gaps
- **CP1 Acceptance**: `docs/archive/dev/CP1_ACCEPTANCE_REPORT.md` - CP1 acceptance report
- **CP2 Plan**: `docs/archive/dev/CP2_ROUTER_PLAN.md` - CP2-LC Router plan
- **Policy Schema CI**: `docs/archive/dev/POLICY_SCHEMA_CI_VALIDATION_REPORT.md` - CI validation report

