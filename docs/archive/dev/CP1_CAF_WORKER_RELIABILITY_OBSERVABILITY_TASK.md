# CP1: CAF Worker Reliability & Observability Invariants

**Task ID**: `AGENT_3_WORKER_CAF_RELIABILITY`  
**Control Point**: `CP1-LC`  
**Status**: `planned`  
**Created**: 2025-01-27  
**Updated**: 2025-01-27

## Objective

### CAF Worker Reliability

**CAF Worker** должен быть надёжным исполнителем шагов Flow, с чётким контрактом результата, корректной обработкой ошибок и предсказуемым поведением под нагрузкой.

**Key Requirements**:
- Чёткий контракт результата для каждого блока
- Корректная обработка ошибок (retry, timeout, DLQ)
- Предсказуемое поведение под нагрузкой
- Ресурсная изоляция (CPU/GPU/IO pools)
- Sandbox mode для безопасного выполнения

### Observability Invariants

**All CP1 Components** (Router, Gateway, Worker, Ingress) должны логировать события в унифицированном JSON-формате с обязательными correlation-полями и иметь health-эндпоинты, описанные и проверяемые автоматическими скриптами.

**Key Requirements**:
- Унифицированный JSON-формат логирования
- Обязательные correlation-поля (`trace_id`, `span_id`, `request_id`)
- Health-эндпоинты для всех компонентов
- Автоматическая валидация observability через скрипты
- PII-фильтрация в логах

## Scope

### CAF Worker Components

- **Core Architecture**: `apps/caf/processor/include/beamline/worker/core.hpp`
- **Block Executor**: `apps/caf/processor/src/block_executor.cpp`
- **Block Implementations**: Phase 1 blocks (HTTP, FS, SQL, Human approval)
- **Test Coverage**: `apps/caf/processor/tests/test_block_executor.cpp`, `apps/caf/processor/tests/test_core.cpp`

### Observability Components

- **Logging Configuration**: `config/observability/logging.json`
- **Validation Scripts**: `scripts/observability/validate_observability.sh`, `scripts/observability/validate_observability.ps1`
- **Documentation**: `docs/OBSERVABILITY.md`, `docs/OBSERVABILITY_CONVENTIONS.md`, `docs/OBSERVABILITY_HEALTH_ENDPOINTS.md`

## Expected Artifacts

### Documentation

1. **`apps/caf/processor/docs/ARCHITECTURE_ROLE.md`**
   - Описание роли CAF Worker в архитектуре
   - Контракт результата для блоков
   - Обработка ошибок и retry-логика
   - Ресурсная изоляция и sandbox mode

2. **`docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md`**
   - Обзор надёжности блоков
   - Анализ обработки ошибок
   - Тестирование под нагрузкой
   - Рекомендации по улучшению

3. **`docs/OBSERVABILITY.md`**
   - Общая спецификация observability для CP1
   - JSON-формат логирования
   - Correlation-поля
   - Health-эндпоинты

4. **`docs/OBSERVABILITY_CONVENTIONS.md`**
   - Конвенции логирования
   - Уровни логирования (ERROR, WARN, INFO, DEBUG)
   - PII-фильтрация
   - Примеры логов

5. **`docs/OBSERVABILITY_HEALTH_ENDPOINTS.md`**
   - Спецификация health-эндпоинтов
   - Формат ответа
   - Проверки здоровья (database, NATS, etc.)
   - Примеры для каждого компонента

### Implementation

1. **Updated Core Files**:
   - `apps/caf/processor/include/beamline/worker/core.hpp` - расширенный контракт результата
   - `apps/caf/processor/src/block_executor.cpp` - улучшенная обработка ошибок
   - Block implementations - обновлённые блоки с корректной обработкой ошибок

2. **Test Files**:
   - `apps/caf/processor/tests/test_block_executor.cpp` - расширенные тесты надёжности
   - `apps/caf/processor/tests/test_core.cpp` - тесты контракта результата

### Configuration

1. **`config/observability/logging.json`**
   - JSON Schema для формата логирования
   - Обязательные поля
   - Correlation-поля
   - PII-фильтрация правил

### Validation Scripts

1. **`scripts/observability/validate_observability.sh`**
   - Bash-скрипт для валидации observability
   - Проверка JSON-формата логов
   - Проверка health-эндпоинтов
   - Проверка correlation-полей

2. **`scripts/observability/validate_observability.ps1`**
   - PowerShell-версия скрипта валидации
   - Кросс-платформенная поддержка

## Acceptance Criteria

### CAF Worker Reliability

- ✅ Чёткий контракт результата для каждого блока
- ✅ Корректная обработка ошибок (retry, timeout, DLQ)
- ✅ Предсказуемое поведение под нагрузкой (тесты производительности)
- ✅ Ресурсная изоляция работает корректно
- ✅ Sandbox mode функционирует
- ✅ Все тесты проходят (`test_block_executor.cpp`, `test_core.cpp`)

### Observability

- ✅ Унифицированный JSON-формат логирования для всех компонентов
- ✅ Обязательные correlation-поля присутствуют в логах
- ✅ Health-эндпоинты работают для всех компонентов
- ✅ Валидация observability проходит автоматически
- ✅ PII-фильтрация работает корректно
- ✅ Документация полная и актуальная

## Dependencies

- **CP0-LC**: Completed (repo structure, state management)
- **CP1-LC**: Router observability (partial)
- **CP1-LC**: Gateway observability (partial)

## Integration with .trae/state.json

### Agent Entry (для добавления в `.trae/state.json`)

```json
{
  "id": "AGENT_3_WORKER_CAF_RELIABILITY",
  "name": "CAF Worker Reliability & Observability",
  "task": "Ensure CAF Worker reliability with clear result contracts, error handling, and unified observability for all CP1 components",
  "cp": "CP1-LC",
  "status": "planned",
  "started_at": null,
  "updated_at": "2025-01-27T00:00:00Z",
  "notes": "Focus on reliability invariants and observability standardization across CP1 components"
}
```

### History Entry (для добавления в `.trae/history.json`)

После начала работы:

```json
{
  "ts": "2025-01-27T00:00:00Z",
  "actor": "AGENT_3_WORKER_CAF_RELIABILITY",
  "action": "artifact_update",
  "cp_from": "CP1-LC",
  "cp_to": "CP1-LC",
  "state_checksum": "sha256_of_state_json_after_update",
  "hmac_prev": "previous_entry_hmac",
  "hmac": "calculated_hmac",
  "metadata": {
    "task": "CP1: CAF Worker Reliability & Observability Invariants",
    "artifacts_planned": [
      "apps/caf/processor/docs/ARCHITECTURE_ROLE.md",
      "docs/archive/dev/BLOCKS_RELIABILITY_REVIEW.md",
      "docs/OBSERVABILITY.md",
      "docs/OBSERVABILITY_CONVENTIONS.md",
      "docs/OBSERVABILITY_HEALTH_ENDPOINTS.md",
      "config/observability/logging.json",
      "scripts/observability/validate_observability.sh",
      "scripts/observability/validate_observability.ps1"
    ]
  }
}
```

### Artifact Checksums

После завершения задачи будут добавлены checksums для всех созданных артефактов:
- Documentation files
- Updated implementation files
- Configuration files
- Validation scripts

## Notes

- Задача фокусируется на надёжности CAF Worker и унификации observability для всех CP1 компонентов
- Observability включает только JSON-логирование и health-эндпоинты (MVP)
- Prometheus, Grafana, Loki отложены до Pre-Release фазы
- Все изменения должны быть покрыты тестами

## References

- `docs/CP1_ACCEPTANCE_REPORT.md` - CP1 acceptance criteria
- `docs/OBSERVABILITY.md` - Existing observability documentation
- `apps/caf/processor/README.md` - CAF Worker overview
- `.trae/manifest.json` - Schema versions and validation rules

