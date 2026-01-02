# Router DLQ Support Improvements Report

**Date**: 2025-01-27  
**Status**: ✅ **All Improvements Completed**  
**Scope**: Добавление и улучшение DLQ поддержки для Router intake validation

## Выполненные улучшения

### ✅ 1. Конфигурируемый DLQ Subject Pattern

**Модуль**: `router_intake_error_handler.erl`

**Изменения**:
- ✅ Добавлена поддержка конфигурируемого DLQ subject pattern
- ✅ По умолчанию: `{original_subject}.dlq` (например, `beamline.router.v1.decide.dlq`)
- ✅ Конфигурируемый: `dlq_subject_pattern` (например, `beamline.router.v1.intake.dlq` для unified DLQ)

**Реализация**:
```erlang
build_dlq_subject(Subject) ->
    case application:get_env(beamline_router, dlq_subject_pattern, undefined) of
        undefined ->
            %% Default: append .dlq to original subject
            <<Subject/binary, ".dlq">>;
        Pattern when is_binary(Pattern) ->
            %% Use configured pattern
            Pattern;
        Pattern when is_list(Pattern) ->
            %% Convert list to binary
            list_to_binary(Pattern);
        _ ->
            %% Fallback to default
            <<Subject/binary, ".dlq">>
    end
```

**Конфигурация**:
- `dlq_subject_pattern`: Binary или string (undefined = default pattern)
- Пример: `{dlq_subject_pattern, <<"beamline.router.v1.intake.dlq">>}`

### ✅ 2. Обработка ошибок публикации в DLQ

**Модуль**: `router_intake_error_handler.erl`

**Изменения**:
- ✅ Добавлена обработка ошибок при публикации в DLQ
- ✅ Логирование ошибок публикации (не блокирует обработку)
- ✅ DLQ publication is best-effort (errors don't fail processing)
- ✅ Метрика для DLQ failures

**Реализация**:
```erlang
case router_nats:publish_with_ack(DLQSubject, DLQJson, #{}) of
    {ok, _MsgId} ->
        %% Successfully published to DLQ
        router_logger:debug(<<"DLQ message published">>, ...),
        emit_dlq_metric(ErrorCode, Subject),
        ok;
    {error, Reason} ->
        %% Failed to publish to DLQ - log error but don't fail
        router_logger:error(<<"Failed to publish to DLQ">>, ...),
        emit_dlq_failure_metric(ErrorCode, Subject, Reason),
        ok  %% Don't fail - DLQ is best-effort
end
```

**Логирование**:
- Успешная публикация: DEBUG level
- Ошибка публикации: ERROR level с деталями
- Включает: dlq_subject, error_code, original_subject, reason, payload_hash

### ✅ 3. Метрика для DLQ Failures

**Модуль**: `router_metrics.erl`

**Добавлена метрика**:
- ✅ `router_intake_dlq_publish_failed_total` - счётчик ошибок публикации в DLQ

**Метрика включает**:
- `reason`: `<<"validation_failed">>`
- `error_code`: код ошибки валидации
- `subject`: оригинальный subject
- `failure_reason`: причина ошибки публикации

**Использование**:
- Мониторинг проблем с DLQ (NATS недоступен, subject не существует, и т.д.)
- Алерты на высокий уровень DLQ failures

### ✅ 4. Конфигурация для включения/выключения DLQ

**Модуль**: `router_intake_error_handler.erl`, `beamline_router.app.src`

**Добавлена конфигурация**:
- ✅ `dlq_enabled`: Boolean (default: `true`)
- ✅ `dlq_subject_pattern`: Binary или string (default: `undefined` = append `.dlq`)

**Реализация**:
```erlang
DLQEnabled = application:get_env(beamline_router, dlq_enabled, true),
case DLQEnabled andalso should_send_to_dlq(ErrorCode) of
    true -> send_to_dlq(...);
    false -> ok
end
```

**Конфигурация в `beamline_router.app.src`**:
```erlang
{dlq_subject_pattern, undefined},  %% DLQ subject pattern (undefined = append .dlq)
{dlq_enabled, true},  %% Enable DLQ publication (default: true)
```

### ✅ 5. Документация DLQ формата

**Файлы**:
- ✅ `docs/NATS_SUBJECTS.md` - добавлен раздел о DLQ subject
- ✅ `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - добавлена информация о DLQ

**DLQ Message Format**:
```json
{
  "original_subject": "beamline.router.v1.decide",
  "original_payload_hash": "sha256-hex-string",
  "validation_error": {
    "code": "SCHEMA_VALIDATION_FAILED",
    "message": "Schema validation failed: missing tenant_id",
    "severity": "error"
  },
  "context": {
    "tenant_id": "tenant_123",
    "run_id": "run_abc123",
    "trace_id": "trace_def456",
    "msg_id": "msg_uuid",
    "validation_stage": "schema"
  },
  "received_at": 1706367600123,
  "router_node_id": "router@node1"
}
```

**DLQ Subject Configuration**:
- Default: `{original_subject}.dlq` (e.g., `beamline.router.v1.decide.dlq`)
- Configurable: `dlq_subject_pattern` (e.g., `beamline.router.v1.intake.dlq` for unified DLQ)
- Can be disabled: `dlq_enabled: false`

## Обновлённые файлы

### Модули

1. **`router_intake_error_handler.erl`**:
   - ✅ Конфигурируемый DLQ subject pattern
   - ✅ Обработка ошибок публикации в DLQ
   - ✅ Функция `sanitize_error_for_logging/1`
   - ✅ Функция `emit_dlq_failure_metric/3`
   - ✅ Проверка `dlq_enabled` перед публикацией

2. **`router_metrics.erl`**:
   - ✅ Добавлена метрика `router_intake_dlq_publish_failed_total`

3. **`beamline_router.app.src`**:
   - ✅ Добавлена конфигурация `dlq_subject_pattern`
   - ✅ Добавлена конфигурация `dlq_enabled`

### Документация

1. **`docs/NATS_SUBJECTS.md`**:
   - ✅ Добавлен раздел `beamline.router.v1.decide.dlq`
   - ✅ Описание DLQ message format
   - ✅ Описание конфигурации DLQ subject

2. **`docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`**:
   - ✅ Добавлена информация о DLQ в раздел `beamline.router.v1.decide`

## Результаты

### Инварианты после улучшений

✅ **DLQ поддержка**:
- Конфигурируемый DLQ subject pattern (default или custom)
- Обработка ошибок публикации (best-effort, не блокирует обработку)
- Метрики для мониторинга DLQ (success и failures)
- Возможность отключения DLQ через конфигурацию

✅ **DLQ Message Format**:
- Содержит payload hash (SHA256) вместо полного payload (безопасность)
- Содержит все необходимые метаданные для отладки
- PII фильтруется из context
- Включает validation error details

✅ **Обработка ошибок**:
- DLQ publication failures логируются, но не блокируют обработку
- Метрики для мониторинга DLQ failures
- Sanitized error logging (без sensitive data)

## Технические детали

### DLQ Subject Pattern

**Default Pattern**:
- `{original_subject}.dlq`
- Пример: `beamline.router.v1.decide` → `beamline.router.v1.decide.dlq`

**Custom Pattern**:
- Конфигурируется через `dlq_subject_pattern`
- Пример: `beamline.router.v1.intake.dlq` (unified DLQ для всех intake errors)

**Fallback**:
- Если конфигурация неверная → используется default pattern

### DLQ Publication

**Best-Effort Strategy**:
- DLQ publication не блокирует обработку ошибок
- Ошибки публикации логируются, но не вызывают исключений
- Метрики для мониторинга DLQ failures

**Error Handling**:
- Успешная публикация → DEBUG log + metric
- Ошибка публикации → ERROR log + failure metric
- Всегда возвращает `ok` (не блокирует обработку)

### DLQ Message Security

**Payload Hash**:
- Вместо полного payload используется SHA256 hash
- Защита от PII/секретов в DLQ
- Можно использовать hash для проверки целостности

**PII Filtering**:
- Context фильтруется через `router_logger:filter_pii/1`
- Удаляются sensitive fields (password, api_key, token, и т.д.)

### Метрики

**DLQ Success**:
- `router_intake_dlq_messages_total` - успешные публикации в DLQ
- Labels: `reason`, `error_code`, `subject`

**DLQ Failures**:
- `router_intake_dlq_publish_failed_total` - ошибки публикации в DLQ
- Labels: `reason`, `error_code`, `subject`, `failure_reason`

## Конфигурация

### Application Environment

```erlang
{beamline_router, [
    %% DLQ Configuration
    {dlq_subject_pattern, undefined},  %% undefined = append .dlq, или custom pattern
    {dlq_enabled, true}  %% Enable/disable DLQ publication
]}
```

### Примеры конфигурации

**Default Pattern** (subject-specific DLQ):
```erlang
{dlq_subject_pattern, undefined}  %% beamline.router.v1.decide.dlq
```

**Unified DLQ** (single DLQ for all intake errors):
```erlang
{dlq_subject_pattern, <<"beamline.router.v1.intake.dlq">>}
```

**Disable DLQ**:
```erlang
{dlq_enabled, false}
```

## Следующие шаги (опционально)

1. **JetStream Stream для DLQ**:
   - Создать JetStream stream для DLQ (для durability)
   - Настроить retention policy для DLQ messages
   - Добавить DLQ consumer для обработки сообщений из DLQ

2. **DLQ Consumer**:
   - Создать `router_dlq_consumer.erl` для обработки DLQ messages
   - Реализовать replay механизм для исправленных сообщений
   - Добавить alerting на высокий уровень DLQ messages

3. **DLQ Analytics**:
   - Агрегация DLQ messages по error_code
   - Тренды по типам ошибок
   - Интеграция с monitoring/alerting системами

## Заключение

Все улучшения DLQ поддержки выполнены успешно:
- ✅ Конфигурируемый DLQ subject pattern
- ✅ Обработка ошибок публикации в DLQ
- ✅ Метрика для DLQ failures
- ✅ Конфигурация для включения/выключения DLQ
- ✅ Документация DLQ формата

Router теперь имеет полную DLQ поддержку с конфигурируемым subject pattern, обработкой ошибок, метриками и документацией.

