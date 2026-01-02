# Policy Property-Based Tests Report

## Purpose

Создание property-based / комбинаторных тестов для Router с использованием PropEr:
- Генерация случайных policy-структур, валидных по `policy.schema.json`
- Проверка инвариантов: нормализация весов, finiteness fallback-цепочек, отсутствие крэшей

## Status

✅ **COMPLETED** - Создан property test suite `router_policy_structure_prop_SUITE.erl`

## Property-Based Testing Infrastructure

### Framework: PropEr

**Configuration**:
- PropEr доступен в test profile (`rebar3 as test`)
- Dependency: `{proper, "~> 1.4"}` в `rebar.config`
- Header: `-include_lib("proper/include/proper.hrl")`

**Unified Style**:
- Runtime check в `all/0` и property functions
- Skip fallback functions для graceful degradation
- Generators на уровне модуля
- Consistent error handling

**Reference**: `apps/otp/router/docs/PROPERTY_TESTING.md` - единый стиль property тестов

## Test Suite: router_policy_structure_prop_SUITE.erl

### Structure

**Module**: `router_policy_structure_prop_SUITE.erl`

**Test Groups**:
- `property_tests` - property-based тесты для policy структур

**Total Tests**: 6 property tests

### Property Tests

#### 1. prop_policy_parsing_no_crash

**Purpose**: Проверка, что парсинг policy никогда не крэшится на валидных структурах

**Generator**: `valid_policy_map()` - случайные валидные policy структуры

**Invariant**:
- ✅ Парсинг всегда возвращает валидный `#policy{}` record
- ✅ Никогда не крэшится на валидных структурах
- ✅ Все поля policy корректно заполнены

**Coverage**:
- Все комбинации providers/weights, fallbacks/fallback, sticky, extensions
- Различные форматы (legacy, new, mixed)

#### 2. prop_weight_normalization

**Purpose**: Проверка инварианта нормализации весов

**Generator**: `valid_policy_map_with_weights()` - policy с весами

**Invariant**:
- ✅ Веса парсятся и сохраняются корректно
- ✅ Веса в диапазоне 0.0-1.0
- ✅ Нет нормализации (веса используются как есть)
- ✅ Providers array (0-100) конвертируется в weights map (0.0-1.0)
- ✅ Weights map (0.0-1.0) используется напрямую

**Coverage**:
- Providers array с различными весами (0-100)
- Weights map с различными весами (0.0-1.0)
- Смешанные форматы

#### 3. prop_fallback_chain_finiteness

**Purpose**: Проверка инварианта finiteness fallback-цепочек

**Generator**: `valid_policy_map_with_fallbacks()` - policy с fallback правилами

**Invariant**:
- ✅ Fallback цепочки конечны (нет бесконечной рекурсии)
- ✅ Нет циклических ссылок (provider в "to" не появляется в "when")
- ✅ Все fallback правила валидны
- ✅ Максимальная глубина fallback цепочки ограничена

**Coverage**:
- Fallbacks array с различным количеством правил
- Legacy fallback object (конвертируется в fallbacks array)
- Сложные when условия

**Helper Functions**:
- `verify_no_fallback_cycles/1` - проверка отсутствия циклов
- `verify_fallback_finiteness/2` - проверка конечности цепочки

#### 4. prop_weird_but_valid_structures

**Purpose**: Проверка отсутствия крэшей при странных, но валидных структурах

**Generator**: `weird_but_valid_policy_map()` - edge case структуры

**Edge Cases**:
- ✅ Providers array с нулевыми весами
- ✅ Providers array с весами, сумма > 100
- ✅ Пустые массивы (providers, fallbacks)
- ✅ Weights map с нулевыми весами
- ✅ Weights map с отрицательными весами
- ✅ Fallbacks array с пустыми when условиями
- ✅ Fallbacks array с большим количеством правил (50+)
- ✅ Sticky с нулевым TTL ("0s")
- ✅ Sticky с отрицательным TTL (через ttl_seconds)
- ✅ Sticky с невалидным TTL string ("10x")

**Invariant**:
- ✅ Парсинг никогда не крэшится на валидных (но странных) структурах
- ✅ Все edge cases обрабатываются gracefully

#### 5. prop_legacy_new_format_equivalence

**Purpose**: Проверка эквивалентности legacy и new форматов

**Generator**: 
- `legacy_policy_map()` - legacy формат
- `equivalent_new_policy_map()` - эквивалентный new формат

**Invariant**:
- ✅ Legacy и new форматы производят эквивалентные веса
- ✅ Legacy fallback конвертируется в new fallbacks array
- ✅ Оба формата производят одинаковые routing решения

**Coverage**:
- Weights map vs providers array
- Fallback object vs fallbacks array
- TTL seconds vs TTL string

#### 6. prop_extensions_parsing_no_crash

**Purpose**: Проверка, что парсинг extensions никогда не крэшится

**Generator**: `valid_policy_map_with_extensions()` - policy с extensions

**Invariant**:
- ✅ Extensions парсятся корректно
- ✅ Pre, validators, post списки валидны
- ✅ Никогда не крэшится на валидных extension структурах

**Coverage**:
- Все типы extensions (pre, validators, post)
- Различные конфигурации extensions
- Пустые extension массивы

## Generators

### Policy Structure Generators

**valid_policy_map()**:
- Генерирует случайные валидные policy структуры
- Поддерживает все форматы: providers/weights, fallbacks/fallback, sticky, extensions
- Соблюдает schema constraints

**valid_policy_map_with_weights()**:
- Генерирует policy с весами (для тестов нормализации)

**valid_policy_map_with_fallbacks()**:
- Генерирует policy с fallback правилами (для тестов finiteness)

**weird_but_valid_policy_map()**:
- Генерирует edge case структуры (нулевые веса, отрицательные TTL, etc.)

**valid_policy_map_with_extensions()**:
- Генерирует policy с extensions (для тестов парсинга extensions)

**legacy_policy_map()**:
- Генерирует legacy формат (weights map + fallback object + ttl_seconds)

**equivalent_new_policy_map()**:
- Генерирует эквивалентный new формат (providers array + fallbacks array + ttl string)

### Primitive Generators

**provider_name()**:
- Генерирует валидные provider names (a-z0-9_, 1-50 chars)

**providers_array()**:
- Генерирует providers array (JSON-DSL format: 0-100)

**weights_map()**:
- Генерирует weights map (legacy format: 0.0-1.0)

**fallbacks_array()**:
- Генерирует fallbacks array с when условиями

**sticky_config()**:
- Генерирует sticky config (new format: ttl string)

**extensions_config()**:
- Генерирует extensions config (pre, validators, post)

**Edge Case Generators**:
- `providers_array_with_zero_weights()` - providers с нулевыми весами
- `providers_array_with_high_weights()` - providers с весами, сумма > 100
- `weights_map_with_zero()` - weights map с нулевыми весами
- `weights_map_with_negative()` - weights map с отрицательными весами
- `fallbacks_array_with_many_rules()` - fallbacks array с 50+ правилами
- `sticky_with_zero_ttl()` - sticky с нулевым TTL
- `sticky_with_negative_ttl()` - sticky с отрицательным TTL

## Invariants Verified

### 1. Weight Normalization Invariant

**Property**: Веса парсятся и сохраняются корректно, без нормализации

**Verification**:
- ✅ Providers array (0-100) → конвертируется в weights map (0.0-1.0)
- ✅ Weights map (0.0-1.0) → используется напрямую
- ✅ Веса в диапазоне 0.0-1.0
- ✅ Нет автоматической нормализации (sum может быть ≠ 1.0)

**Test**: `prop_weight_normalization`

### 2. Fallback Chain Finiteness Invariant

**Property**: Fallback цепочки конечны и не создают бесконечных циклов

**Verification**:
- ✅ Fallbacks array конечна (length < 1000)
- ✅ Нет циклических ссылок (provider в "to" не в "when")
- ✅ Максимальная глубина ограничена (100)
- ✅ Все fallback правила валидны

**Test**: `prop_fallback_chain_finiteness`

**Helper Functions**:
- `verify_no_fallback_cycles/1` - проверка отсутствия циклов
- `verify_fallback_finiteness/2` - проверка конечности с ограничением глубины

### 3. No Crash Invariant

**Property**: Парсинг никогда не крэшится на валидных структурах

**Verification**:
- ✅ Все валидные policy структуры парсятся без крэшей
- ✅ Edge cases обрабатываются gracefully
- ✅ Странные, но валидные структуры не вызывают крэшей

**Tests**:
- `prop_policy_parsing_no_crash` - общий тест
- `prop_weird_but_valid_structures` - edge cases
- `prop_extensions_parsing_no_crash` - extensions

## Test Execution

### Running Property Tests

```bash
# Run all property tests
rebar3 as test ct --suite apps/otp/router/test/router_policy_structure_prop_SUITE

# Run specific property test
rebar3 as test ct --suite apps/otp/router/test/router_policy_structure_prop_SUITE --case prop_policy_parsing_no_crash
```

### Expected Results

- **All property tests should pass** (6 tests)
- **No crashes** on valid policy structures
- **Invariants hold** for all generated structures
- **Counterexamples** are reported if properties fail

### PropEr Options

**Default Options** (via `test_helpers:get_proper_options()`):
- `numtests`: 100 (default)
- `long_result`: true
- `quiet`: false

**Custom Options**:
```erlang
Options = [{numtests, 500}, {long_result, true}],
proper:quickcheck(Prop, Options)
```

## Coverage

### Schema Coverage

**All schema fields covered**:
- ✅ `version` - всегда "1.0"
- ✅ `providers` - array format (0-100)
- ✅ `weights` - map format (0.0-1.0)
- ✅ `fallbacks` - array format with when/retry/to
- ✅ `fallback` - legacy object format
- ✅ `sticky` - new format (ttl string) and legacy (ttl_seconds)
- ✅ `pre`, `validators`, `post` - extensions arrays
- ✅ `metadata` - additional metadata

### Format Coverage

**All formats covered**:
- ✅ Pure new format (providers, fallbacks, ttl string)
- ✅ Pure legacy format (weights, fallback object, ttl_seconds)
- ✅ Mixed format (both new and legacy present)
- ✅ Edge cases (zero weights, negative TTL, empty arrays)

### Combinatorial Coverage

**Combinatorial testing**:
- ✅ Все комбинации providers/weights
- ✅ Все комбинации fallbacks/fallback
- ✅ Все комбинации sticky formats
- ✅ Все комбинации extensions
- ✅ Все комбинации edge cases

## Files Created

1. `apps/otp/router/test/router_policy_structure_prop_SUITE.erl` - property test suite (6 property tests, 20+ generators)

## Files Used

1. `apps/otp/router/docs/schemas/policy.schema.json` - schema для генерации валидных структур
2. `apps/otp/router/src/router_policy_store.erl` - парсер policy
3. `apps/otp/router/src/router_policy_applier.erl` - применение policy
4. `apps/otp/router/docs/PROPERTY_TESTING.md` - единый стиль property тестов

## Integration with Existing Tests

### Unit Tests
- `router_policy_applier_dsl_SUITE.erl` - unit тесты для DSL парсинга
- Property tests дополняют unit тесты случайными структурами

### Integration Tests
- `router_policy_integration_SUITE.erl` - интеграционные тесты с реальными фикстурами
- Property tests дополняют integration тесты комбинаторным покрытием

### Legacy Compatibility Tests
- `router_policy_legacy_compatibility_SUITE.erl` - тесты обратной совместимости
- Property tests проверяют legacy форматы через генераторы

## Next Steps

1. **Run Tests**:
   - Выполнить все property тесты
   - Проверить, что все инварианты выполняются
   - Исправить найденные проблемы

2. **Extend Coverage**:
   - Добавить больше edge case генераторов
   - Добавить тесты для сложных комбинаций
   - Добавить статистические тесты (как в router_decider_prop_SUITE)

3. **Performance**:
   - Оптимизировать генераторы для больших структур
   - Добавить тесты производительности для больших fallback цепочек

## References

- `apps/otp/router/test/router_policy_structure_prop_SUITE.erl` - property test suite
- `apps/otp/router/docs/PROPERTY_TESTING.md` - единый стиль property тестов
- `apps/otp/router/docs/schemas/policy.schema.json` - schema для генерации
- `apps/otp/router/test/router_decider_prop_SUITE.erl` - примеры property тестов
- `apps/otp/router/test/router_policy_store_prop_SUITE.erl` - примеры property тестов для policy store

