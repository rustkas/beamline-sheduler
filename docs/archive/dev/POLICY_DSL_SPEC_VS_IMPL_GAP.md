# Policy DSL Specification vs Implementation Gap Analysis

## Purpose

Явное документирование остаточных расхождений между спецификацией (`docs/ROUTING_POLICY.md`) и реализацией после доработки policy-движка.

## Scope

Анализ расхождений по:
- Формат explanation (структура, поля, уровни детализации)
- Формат extensions (имена, структура элементов, возможные будущие поля)
- Нюансы retry, backoff, возможные дополнительные поля в JSON-DSL

## 1. Explanation Format

### Specification (ROUTING_POLICY.md)

**Status**: ✅ **SPECIFIED** (Updated: 2025-01-27)

**Current State**:
- ✅ `docs/ROUTING_POLICY.md` содержит формальную спецификацию "Decision Explanation Format"
- ✅ Описание всех обязательных полей (reason, provider_id, policy_id, policy_version, priority, steps, context)
- ✅ Описание типов и возможных значений
- ✅ Примеры для всех сценариев (sticky, weighted, fallback, retry)
- ✅ Описание использования explanation в audit trail

### Implementation (router_policy_applier.erl)

**Current Format**:
```erlang
#{
    reason => binary(),           % "sticky" | "weighted" | "fallback"
    provider_id => binary(),      % Selected provider ID
    policy_id => binary(),        % Policy ID used
    policy_version => binary(),   % Policy version
    priority => integer(),        % Decision priority (50, 100, 25)
    steps => [binary()],         % Step-by-step explanation
    context => map()             % Context (tenant_id, trace_id, etc.)
}
```

**Steps Format**:
- Array of binary strings describing decision steps
- Example: `["1. Checked sticky session: no existing session found", "2. Applied weighted distribution: 2 providers, total weight: 1.00"]`

### Gap Analysis

**Missing in Specification** (CLOSED):
1. ✅ Описание структуры explanation - добавлено в ROUTING_POLICY.md
2. ✅ Описание полей explanation - добавлено в ROUTING_POLICY.md
3. ✅ Описание формата steps - добавлено в ROUTING_POLICY.md
4. ⚠️ Уровни детализации - не реализовано (nice to have)
5. ✅ Описание использования explanation в audit trail - добавлено в ROUTING_POLICY.md

**Implementation Details**:
- ✅ Explanation формируется в `router_policy_applier:build_explanation/3`
- ✅ Steps формируются в `router_policy_applier:build_explanation_steps/5`
- ✅ Explanation включает контекст решения

### Recommendations (IMPLEMENTED)

1. ✅ **Added to ROUTING_POLICY.md**:
   - ✅ Раздел "Decision Explanation Format"
   - ✅ Описание всех полей explanation
   - ✅ Примеры explanation для разных сценариев
   - ✅ Связь с audit trail

2. **Future Considerations**:
   - Уровни детализации (minimal, detailed, verbose) - не реализовано (nice to have)
   - Структурированный формат steps (не только строки) - не реализовано (nice to have)

## 2. Extensions Format

### Specification (EXTENSIONS_API.md)

**Format**:
```json
{
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "config": {"lowercase": true}
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
      "config": {"mask_email": true}
    }
  ]
}
```

**Fields**:
- `pre[]`: `{id, mode, config}`
- `validators[]`: `{id, on_fail}`
- `post[]`: `{id, mode, config}`

### Specification (ROUTING_POLICY.md)

**Status**: ✅ **SPECIFIED** (Updated: 2025-01-27)

**Current State**:
- ✅ `docs/ROUTING_POLICY.md` содержит раздел "Extensions"
- ✅ Описание структуры pre/validators/post
- ✅ Описание полей (id, mode, on_fail, config)
- ✅ Примеры использования extensions
- ✅ Кросс-линк на `docs/EXTENSIONS_API.md`

### Implementation (router_policy_store.erl, router_policy_applier.erl)

**Current Format**:
```erlang
#policy{
    pre = [{id, mode, config}],      % Pre-processor extensions
    validators = [{id, on_fail}],    % Validator extensions
    post = [{id, mode, config}]      % Post-processor extensions
}
```

**Output Format** (from router_policy_applier):
```erlang
#{
    pre => [ExtensionConfig],
    validators => [ExtensionConfig],
    post => [ExtensionConfig]
}
```

**ExtensionConfig Format**:
- Pre/Post: `#{id => binary(), mode => binary(), config => map()}`
- Validators: `#{id => binary(), on_fail => binary()}`

### Gap Analysis

**Missing in ROUTING_POLICY.md** (CLOSED):
1. ✅ Упоминание extensions в JSON-DSL структуре - добавлено
2. ✅ Описание формата pre/validators/post - добавлено
3. ✅ Связь между policy и extensions - добавлено (кросс-линк на EXTENSIONS_API.md)

**Inconsistencies**:
1. ⚠️ Extensions описаны в `EXTENSIONS_API.md`, но не в `ROUTING_POLICY.md`
2. ⚠️ Формат extensions не включен в основной JSON-DSL пример

**Future Fields** (not yet implemented):
- `timeout_ms` - per-extension timeout (currently in Registry, not in Policy)
- `retry` - per-extension retry count (currently in Registry, not in Policy)
- `circuit_breaker` - circuit breaker configuration
- `rate_limit` - per-extension rate limiting

### Recommendations (IMPLEMENTED)

1. ✅ **Added to ROUTING_POLICY.md**:
   - ✅ Раздел "Extensions"
   - ✅ Описание формата pre/validators/post
   - ✅ Примеры использования extensions в policy
   - ✅ Связь с Extension Registry (кросс-линк на EXTENSIONS_API.md)

2. **Future Considerations**:
   - Перенос timeout_ms и retry из Registry в Policy (per-policy override) - не реализовано
   - Добавление circuit_breaker и rate_limit в Policy DSL - не реализовано

## 3. Retry and Backoff

### Specification (ROUTING_POLICY.md)

**Retry**:
- ✅ Упоминается в fallback rules: `"retry": 2`
- ✅ Описано: "Fallback provider **always** selected after retry count exhausted"
- ❌ Нет описания логики использования retry
- ❌ Нет описания, где хранится retry count

**Backoff**:
- ❌ Не упоминается в спецификации
- ❌ Нет описания backoff стратегии для fallbacks

### Implementation

**Retry Parsing** (router_policy_store.erl):
```erlang
parse_fallback_rule(FallbackRule) ->
    Retry = maps:get(<<"retry">>, FallbackRule, 1),
    #{
        <<"retry">> => Retry,
        ...
    }.
```

**Retry Usage**:
- ✅ Retry парсится из JSON-DSL
- ✅ Retry хранится в fallback rule map
- ✅ Retry **используется** в `router_decider:check_fallbacks_with_retry/3` (реализовано)
- ✅ Retry **используется** в логике принятия решений (реализовано)

**Backoff**:
- ✅ Реализован в `router_decider:calculate_backoff/3`
- ✅ Поддерживаются стратегии: exponential, linear, fixed
- ✅ Backoff применяется между retry попытками

### Gap Analysis

**Status**: ✅ **CLOSED** (Updated: 2025-01-27)

**Implementation Status**:
1. ✅ **Retry реализован** - используется в `check_fallbacks_with_retry/3`
2. ✅ **Backoff реализован** - стратегии exponential, linear, fixed
3. ✅ **Спецификация обновлена** - разделы "Retry Semantics" и "Backoff Semantics" в ROUTING_POLICY.md

**CP Classification**: **CP1-blocker** → ✅ **CLOSED**

### Recommendations (IMPLEMENTED)

1. ✅ **Retry Logic Implemented**:
   - ✅ Отслеживание retry count в router_decider
   - ✅ Применение fallback только после исчерпания retry
   - ✅ Хранение retry state в контексте запроса

2. ✅ **Backoff Added**:
   - ✅ Поле `backoff` в fallback rule JSON-DSL
   - ✅ Backoff стратегии (exponential, linear, fixed)
   - ✅ Использование backoff между retry попытками

3. ✅ **Specification Updated**:
   - ✅ Логика retry описана в ROUTING_POLICY.md
   - ✅ Описание backoff стратегий добавлено
   - ✅ Примеры использования retry/backoff добавлены

## 4. Additional JSON-DSL Fields

### Potential Future Fields (Not in Specification)

**Not Mentioned in ROUTING_POLICY.md**:
1. `timeout` - per-policy timeout
2. `circuit_breaker` - circuit breaker configuration
3. `rate_limit` - per-policy rate limiting
4. `priority` - provider priority (separate from weights)
5. `health_check` - health check configuration
6. `metadata` - additional metadata (currently parsed, but not specified)

**Currently Parsed but Not Specified** (DEPRECATED):
- `metadata` - parsed in implementation, but format not specified → ✅ **DEPRECATED** (2025-01-27)
- `defaults` - parsed in implementation, but format not specified → ✅ **DEPRECATED** (2025-01-27)
- `escalate_on` - parsed in implementation, but format not specified → ✅ **DEPRECATED** (2025-01-27)

### Gap Analysis

**Status**: ✅ **CLOSED** (Updated: 2025-01-27)

**Decision**: Metadata, defaults, escalate_on помечены как deprecated в:
- `policy.schema.json` (deprecated: true)
- `ROUTING_POLICY.md` (раздел "Deprecated Fields")

**CP Classification**: **CP1-nice** → ✅ **CLOSED**

### Recommendations (IMPLEMENTED)

1. ✅ **Deprecated Fields**:
   - ✅ `metadata` помечен как deprecated в policy.schema.json
   - ✅ `defaults` помечен как deprecated в policy.schema.json
   - ✅ `escalate_on` помечен как deprecated в policy.schema.json
   - ✅ Раздел "Deprecated Fields" добавлен в ROUTING_POLICY.md

2. **Future Fields** (CP2):
   - Определить приоритет добавления новых полей (circuit_breaker, rate_limit, etc.)
   - Добавить в спецификацию после реализации в CP2

## Summary of Gaps

### CP Classification

**Legend**:
- **CP1-blocker**: Блокирует завершение CP1 (критично для CP1 acceptance)
- **CP1-nice**: Желательно для CP1, но не блокирует (улучшение качества)
- **CP2**: Для CP2-LC или CP2+ (расширенная функциональность)
- **Pre-release**: Для pre-release (качество и защита от регрессий)

### ✅ CLOSED Gaps (CP1-Complete)

1. ✅ **Explanation специфицирован** - формат описан в ROUTING_POLICY.md (CLOSED: 2025-01-27)
   - **CP Tag**: CP1-blocker → ✅ CLOSED
   - **Status**: Полностью специфицировано и реализовано

2. ✅ **Extensions в ROUTING_POLICY.md** - раздел добавлен, синхронизирован (CLOSED: 2025-01-27)
   - **CP Tag**: CP1-blocker → ✅ CLOSED
   - **Status**: Полностью специфицировано и реализовано

3. ✅ **Retry and Backoff специфицированы и реализованы** - описаны в ROUTING_POLICY.md, реализованы в router_decider.erl (CLOSED: 2025-01-27)
   - **CP Tag**: CP1-blocker → ✅ CLOSED
   - **Status**: Полностью специфицировано и реализовано (check_fallbacks_with_retry/3)

4. ✅ **Metadata/Defaults/Escalate_on депрекейтнуты** - помечены как deprecated в policy.schema.json и ROUTING_POLICY.md (CLOSED: 2025-01-27)
   - **CP Tag**: CP1-nice → ✅ CLOSED
   - **Status**: Депрекейтнуты, не используются в routing logic

### Open Gaps (By CP Classification)

#### CP1-nice (Желательно для CP1, но не блокирует)

1. 📝 **Explanation levels** - нет уровней детализации (minimal, detailed, verbose)
   - **CP Tag**: **CP1-nice**
   - **Priority**: Низкий
   - **Impact**: Улучшение качества audit trail, но не критично для CP1
   - **Recommendation**: Можно отложить до CP2, если не критично для CP1 acceptance

#### CP2 (Для CP2-LC или CP2+)

1. 📝 **Future extension fields** - timeout_ms, retry в Policy (не только Registry)
   - **CP Tag**: **CP2**
   - **Priority**: Средний
   - **Impact**: Per-policy override для extension timeout/retry
   - **Recommendation**: Добавить в CP2-LC plan как enhancement

2. 📝 **Circuit breaker в Policy DSL** - circuit breaker configuration
   - **CP Tag**: **CP2**
   - **Priority**: Средний
   - **Impact**: Улучшение reliability через circuit breaker
   - **Recommendation**: Добавить в CP2-LC plan как enhancement

3. 📝 **Rate limit в Policy DSL** - per-policy rate limiting
   - **CP Tag**: **CP2**
   - **Priority**: Средний
   - **Impact**: Per-policy rate limiting (сейчас только per-tenant)
   - **Recommendation**: Добавить в CP2-LC plan как enhancement

4. 📝 **Health check в Policy DSL** - health check configuration
   - **CP Tag**: **CP2**
   - **Priority**: Низкий
   - **Impact**: Per-policy health check configuration
   - **Recommendation**: Добавить в CP2+ plan

5. 📝 **Provider priority (separate from weights)** - provider priority field
   - **CP Tag**: **CP2**
   - **Priority**: Низкий
   - **Impact**: Приоритет провайдеров независимо от weights
   - **Recommendation**: Добавить в CP2+ plan

6. 📝 **Per-policy timeout** - timeout configuration в Policy DSL
   - **CP Tag**: **CP2**
   - **Priority**: Средний
   - **Impact**: Per-policy timeout override
   - **Recommendation**: Добавить в CP2-LC plan как enhancement

#### Pre-release (Качество и защита от регрессий)

1. ✅ **Policy schema CI validation** - валидация policy.schema.json и fixtures в CI (CLOSED: 2025-01-27)
   - **CP Tag**: **Pre-release** → ✅ CLOSED
   - **Status**: Интегрировано в check_schema_changes.sh и run_checks.sh
   - **Reference**: `docs/archive/dev/POLICY_SCHEMA_CI_VALIDATION_REPORT.md`

## References

- `docs/ROUTING_POLICY.md` - основная спецификация JSON-DSL
- `docs/EXTENSIONS_API.md` - спецификация extensions
- `docs/archive/dev/POLICY_ENGINE_JSON_DSL_MAPPING.md` - анализ несоответствий
- `docs/archive/dev/POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md` - отчет о реализации
- `apps/otp/router/src/router_policy_applier.erl` - реализация explanation
- `apps/otp/router/src/router_policy_store.erl` - парсинг JSON-DSL
- `apps/otp/router/src/router_decider.erl` - логика принятия решений

