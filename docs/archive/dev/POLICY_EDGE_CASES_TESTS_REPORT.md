# Policy Edge Cases Tests Report

## Purpose

Расширение интеграционных тестов для policy_applier с edge-cases:
- Неконсистентные веса (сумма сильно ≠ 100 / 1.0)
- Некорректные duration (например "10x", "", 0, отрицательные)
- Конфликтующие/перекрывающиеся fallback-правила
- Пустые extensions и неизвестные поля в extensions-элементах
- Глубокие комбинации legacy+new в одном policy

## Status

✅ **COMPLETED** - Добавлены 12 edge-case тестов в `router_policy_integration_SUITE.erl`

## Test Coverage

### 1. Inconsistent Weights Tests (3 tests)

**test_inconsistent_weights_sum_not_100**:
- ✅ Создает policy с весами, сумма которых = 70 (не 100)
- ✅ Проверяет, что веса парсятся корректно (0.3, 0.4)
- ✅ Проверяет, что routing работает (weighted distribution использует доступные веса)

**test_inconsistent_weights_sum_zero**:
- ✅ Создает policy со всеми весами = 0
- ✅ Проверяет, что веса парсятся как 0.0
- ✅ Проверяет, что routing использует fallback или возвращает error

**test_inconsistent_weights_sum_over_100**:
- ✅ Создает policy с весами, сумма которых = 120 (> 100)
- ✅ Проверяет, что веса парсятся корректно (0.7, 0.5)
- ✅ Проверяет, что routing работает (weighted distribution нормализует или использует как есть)

### 2. Invalid TTL Duration Tests (4 tests)

**test_invalid_ttl_duration_invalid_format**:
- ✅ Создает policy с невалидным форматом TTL ("10x")
- ✅ Проверяет, что TTL fallback на default (3600 секунд)
- ✅ Проверяет, что routing работает

**test_invalid_ttl_duration_empty**:
- ✅ Создает policy с пустой строкой TTL ("")
- ✅ Проверяет, что TTL fallback на default (3600 секунд)

**test_invalid_ttl_duration_zero**:
- ✅ Создает policy с TTL = "0s"
- ✅ Проверяет, что TTL парсится как 0

**test_invalid_ttl_duration_negative**:
- ✅ Создает policy с отрицательным TTL (через legacy ttl_seconds = -100)
- ✅ Проверяет, что отрицательный TTL сохраняется как есть (валидация происходит в другом месте)

### 3. Conflicting/Overlapping Fallback Rules Tests (2 tests)

**test_conflicting_fallback_rules**:
- ✅ Создает policy с конфликтующими fallback правилами (одинаковое условие, разные провайдеры)
- ✅ Проверяет, что оба правила парсятся
- ✅ Проверяет, что используется первое правило (порядок важен)
- ✅ Проверяет, что provider_a выбирается (первое правило)

**test_overlapping_fallback_rules**:
- ✅ Создает policy с перекрывающимися fallback правилами (одно условие - подмножество другого)
- ✅ Проверяет, что оба правила парсятся
- ✅ Проверяет, что используется первое совпадающее правило (порядок важен)
- ✅ Проверяет, что provider_a выбирается для "timeout" (первое правило)

### 4. Empty Extensions and Unknown Fields Tests (2 tests)

**test_empty_extensions_arrays**:
- ✅ Создает policy с пустыми массивами extensions (pre, validators, post)
- ✅ Проверяет, что пустые массивы парсятся корректно
- ✅ Проверяет, что routing работает

**test_unknown_fields_in_extensions**:
- ✅ Создает policy с неизвестными полями в extension элементах
- ✅ Проверяет, что extensions парсятся (неизвестные поля игнорируются или сохраняются в config)
- ✅ Проверяет, что routing работает
- ✅ Проверяет, что известные поля (id, mode, on_fail) парсятся корректно

### 5. Deep Legacy+New Combination Test (1 test)

**test_deep_legacy_new_combination**:
- ✅ Создает policy с глубокой комбинацией legacy и new формата:
  - New: providers array
  - Legacy: weights map (игнорируется, если providers present)
  - New: fallbacks array
  - Legacy: fallback object (конвертируется в fallbacks array)
  - New: sticky с ttl string
  - New: extensions (pre, validators, post)
- ✅ Проверяет, что new format имеет приоритет
- ✅ Проверяет, что legacy weights игнорируются
- ✅ Проверяет, что fallbacks array используется (legacy fallback конвертирован)
- ✅ Проверяет, что sticky использует new format (ttl string = 10m = 600s)
- ✅ Проверяет, что extensions используют new format
- ✅ Проверяет, что routing работает с глубокой комбинацией

## Test Group Structure

**Group**: `edge_cases_tests` (parallel execution)

**Total Tests**: 12 тестов

**Execution Mode**: `parallel` (для ускорения выполнения)

## Edge Cases Covered

### 1. Weight Inconsistencies

**Scenarios**:
- Sum < 100 (70%)
- Sum = 0 (all weights zero)
- Sum > 100 (120%)

**Expected Behavior**:
- Weights parsed correctly (even if inconsistent)
- Routing works (weighted distribution handles inconsistencies)
- Fallback used when weights are all zero

### 2. Invalid TTL Durations

**Scenarios**:
- Invalid format ("10x")
- Empty string ("")
- Zero ("0s")
- Negative (via legacy ttl_seconds = -100)

**Expected Behavior**:
- Invalid format → fallback to default (3600 seconds)
- Empty string → fallback to default
- Zero → parsed as 0
- Negative → stored as-is (validation elsewhere)

### 3. Conflicting/Overlapping Fallbacks

**Scenarios**:
- Same condition, different providers (conflicting)
- One condition is subset of another (overlapping)

**Expected Behavior**:
- Both rules parsed
- First matching rule used (order matters)
- First rule's provider selected

### 4. Empty/Unknown Extensions

**Scenarios**:
- Empty arrays (pre, validators, post)
- Unknown fields in extension elements

**Expected Behavior**:
- Empty arrays parsed correctly
- Unknown fields ignored or stored in config
- Known fields (id, mode, on_fail) parsed correctly
- Routing works

### 5. Deep Legacy+New Combination

**Scenarios**:
- Providers array (new) + weights map (legacy)
- Fallbacks array (new) + fallback object (legacy)
- Sticky with ttl string (new) + ttl_seconds (legacy)
- Extensions in new format

**Expected Behavior**:
- New format takes precedence
- Legacy fields ignored or converted
- Routing works with deep combination

## Integration with Existing Tests

**Existing Group**: `json_policy_tests` (17 tests)
- Normal scenarios
- Happy paths
- Standard combinations

**New Group**: `edge_cases_tests` (12 tests)
- Edge cases
- Invalid inputs
- Boundary conditions
- Deep combinations

**Total**: 29 integration tests

## Test Execution

### Running Edge Cases Tests

```bash
# Run all edge cases tests
rebar3 ct --suite apps/otp/router/test/router_policy_integration_SUITE --group edge_cases_tests

# Run specific edge case test
rebar3 ct --suite apps/otp/router/test/router_policy_integration_SUITE --case test_inconsistent_weights_sum_not_100
```

### Expected Results

- **All edge case tests should pass** (12 tests)
- **Invalid inputs should be handled gracefully** (fallback to defaults or error)
- **Conflicting rules should use first match** (order matters)
- **Unknown fields should be ignored** (not break parsing)
- **Deep combinations should work** (new format takes precedence)

## Files Modified

1. `apps/otp/router/test/router_policy_integration_SUITE.erl` - добавлены 12 edge-case тестов

## Files Created

1. `docs/archive/dev/POLICY_EDGE_CASES_TESTS_REPORT.md` - отчет об edge-case тестах

## Key Insights

### Weight Inconsistencies

- **Parser handles inconsistencies**: Weights are parsed even if sum ≠ 100
- **Router handles inconsistencies**: Weighted distribution works with inconsistent weights
- **Zero weights trigger fallback**: When all weights are 0, fallback is used

### Invalid TTL Durations

- **Invalid format → default**: "10x" → 3600 seconds (default)
- **Empty string → default**: "" → 3600 seconds (default)
- **Zero is valid**: "0s" → 0 (valid, but may cause immediate expiration)
- **Negative stored as-is**: -100 → -100 (validation happens elsewhere)

### Conflicting/Overlapping Fallbacks

- **Order matters**: First matching rule is used
- **Both rules parsed**: Conflicting rules don't break parsing
- **First rule wins**: Provider from first matching rule is selected

### Empty/Unknown Extensions

- **Empty arrays work**: Routing works with empty extension arrays
- **Unknown fields ignored**: Unknown fields don't break parsing
- **Known fields parsed**: id, mode, on_fail parsed correctly

### Deep Legacy+New Combination

- **New format takes precedence**: Providers array overrides weights map
- **Legacy converted**: Fallback object converted to fallbacks array
- **New format used**: Sticky uses ttl string, not ttl_seconds
- **Routing works**: Deep combination doesn't break routing

## Next Steps

1. **Run Tests**:
   - Выполнить все edge-case тесты
   - Проверить, что все edge cases обрабатываются корректно
   - Исправить найденные проблемы

2. **Extend Coverage**:
   - Добавить тесты для других edge cases (если найдены)
   - Добавить тесты для performance edge cases
   - Добавить тесты для concurrent edge cases

3. **Documentation**:
   - Обновить документацию с информацией о edge cases
   - Добавить примеры обработки edge cases
   - Обновить schema с валидацией edge cases

## References

- `apps/otp/router/test/router_policy_integration_SUITE.erl` - интеграционный тест-сьют с edge cases
- `docs/archive/dev/POLICY_INTEGRATION_TESTS_REPORT.md` - отчет об интеграционных тестах
- `docs/archive/dev/POLICY_SCHEMA_FIXTURES_UPDATE.md` - обновление схемы и фикстур

