# Router Intake Validator Complete Implementation Report

**Date**: 2025-01-27  
**Status**: ✅ **All Tasks Completed**  
**Scope**: Полная реализация unified intake validation layer для Router

## Выполненные задачи

### ✅ 1. Создать `router_intake_validator.erl` с базовой структурой

**Модуль**: `router_intake_validator.erl`

**Основная структура**:
- ✅ Главная функция `validate_intake_message/4` - единая точка входа для валидации
- ✅ Экспортируемые функции для всех этапов валидации
- ✅ Типы сообщений: `decide | result | ack`
- ✅ Поддержка констант (SUPPORTED_VERSIONS)
- ✅ Обработка исключений (try-catch)

**Экспортируемые функции**:
```erlang
-export([validate_intake_message/4]).
-export([validate_schema/2, validate_version/3, validate_correlation_fields/2]).
-export([validate_uuid_v4/1, validate_ulid/1, validate_w3c_trace_context/1]).
-export([validate_decide_specific_fields/1, validate_tenant/2, validate_idempotency/3]).
```

**Архитектура валидации**:
1. Schema validation (protobuf/JSON decode)
2. Version validation (subject + payload + headers)
3. Correlation fields validation (run_id, flow_id, step_id, trace_id, idempotency_key)
4. Message-type specific validation (decide-specific fields, tenant, idempotency)

### ✅ 2. Реализовать валидацию схемы (protobuf decode)

**Реализация**:
- ✅ **Decide messages**: Protobuf decode через `flow_pb:decode_msg/2`
- ✅ **Result messages**: JSON decode (CP1 baseline compatibility)
- ✅ **ACK messages**: JSON decode (CP1 baseline compatibility)
- ✅ Fallback на JSON decode для decide (backward compatibility)
- ✅ Конвертация protobuf records в maps для совместимости

**Функции**:
```erlang
validate_schema(Payload, decide) ->
    %% Protobuf decode for decide messages
    RouteRequestPb = flow_pb:decode_msg(Payload, 'RouteRequest'),
    convert_route_request_to_map(RouteRequestPb);
validate_schema(Payload, result) ->
    %% JSON decode for result messages
    jsx:decode(Payload, [return_maps]);
validate_schema(Payload, ack) ->
    %% JSON decode for ack messages
    jsx:decode(Payload, [return_maps])
```

**Конвертация Protobuf → Map**:
- ✅ `convert_route_request_to_map/1` - конвертирует RouteRequest protobuf в map
- ✅ `convert_message_to_map/1` - конвертирует Message protobuf в map
- ✅ Поддержка protobuf maps (context, metadata)
- ✅ Добавление version для совместимости

### ✅ 3. Реализовать валидацию версии

**Реализация**:
- ✅ Извлечение версии из subject (regex: `\.v([0-9]+)\.`)
- ✅ Извлечение версии из payload (`version` field)
- ✅ Извлечение версии из headers (`version` header)
- ✅ Приоритет: Headers > Payload > Subject
- ✅ Проверка поддерживаемых версий (SUPPORTED_VERSIONS = [<<"1">>])

**Функция**:
```erlang
validate_version(Subject, Message, Headers) ->
    VersionFromSubject = extract_version_from_subject(Subject),
    VersionFromPayload = maps:get(<<"version">>, Message, undefined),
    VersionFromHeaders = maps:get(<<"version">>, Headers, undefined),
    
    %% Priority: Headers > Payload > Subject
    Version = case VersionFromHeaders of
        undefined -> case VersionFromPayload of
            undefined -> VersionFromSubject;
            V -> V
        end;
        V -> V
    end,
    
    %% Validate version
    case Version of
        undefined -> {error, missing_version};
        V when V =:= <<"1">> -> {ok, V};
        _ -> {error, unsupported_version}
    end
```

**Обработка ошибок**:
- `missing_version` - версия не найдена ни в subject, ни в payload, ни в headers
- `unsupported_version` - версия не поддерживается (не в SUPPORTED_VERSIONS)

### ✅ 4. Реализовать валидацию корреляционных полей

**Реализация**:
- ✅ Валидация обязательных полей (tenant_id)
- ✅ Валидация зависимостей между полями (run_id → flow_id → step_id)
- ✅ Валидация форматов полей:
  - `run_id`, `flow_id`, `step_id`: UUID v4 или ULID
  - `trace_id`: W3C Trace Context (32 hex chars) или UUID v4
  - `idempotency_key`: non-empty string, max 256 chars
- ✅ Построение correlation context для дальнейшей обработки

**Функции валидации форматов**:
```erlang
validate_uuid_v4/1  %% UUID v4 format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
validate_ulid/1      %% ULID format: 26 base32 characters
validate_w3c_trace_context/1  %% W3C Trace Context: 32 hex characters
```

**Валидация зависимостей**:
- Если `run_id` присутствует → `flow_id` и `step_id` обязательны
- Если `flow_id` присутствует → `run_id` обязателен
- Если `step_id` присутствует → `run_id` и `flow_id` обязательны

**Correlation Context**:
```erlang
#{
    <<"tenant_id">> => TenantId,
    <<"run_id">> => RunId,
    <<"flow_id">> => FlowId,
    <<"step_id">> => StepId,
    <<"idempotency_key">> => IdempotencyKey,
    <<"trace_id">> => TraceId
}
```

### ✅ 5. Интегрировать с decide consumer

**Интеграция**:
- ✅ `router_decide_consumer.erl` использует `router_intake_validator:validate_intake_message/4`
- ✅ Обработка результата валидации:
  - `{ok, ValidatedMessage}` → проверка idempotency status → обработка или пропуск дубликата
  - `{error, {ErrorCode, ErrorMessage, Context}}` → вызов `router_intake_error_handler`
- ✅ Интеграция с error handler для DLQ, audit, metrics

**Код интеграции**:
```erlang
case router_intake_validator:validate_intake_message(Subject, Payload, Headers, decide) of
    {ok, ValidatedMessage} ->
        IdempotencyStatus = maps:get(<<"idempotency_status">>, ValidatedMessage, new),
        case IdempotencyStatus of
            duplicate ->
                %% Log and ACK (no processing)
                router_logger:info(<<"Duplicate decide request">>, ...),
                ack_message_if_needed(MsgId);
            new ->
                %% Process normally
                handle_decide_request(Subject, ValidatedMessage, Headers, MsgId)
        end;
    {error, {ErrorCode, ErrorMessage, ErrorContext}} ->
        router_intake_error_handler:handle_intake_error(
            ErrorCode, ErrorMessage, Subject, Payload, Headers, MsgId, ErrorContext
        )
end
```

**Дополнительная интеграция**:
- ✅ `router_result_consumer.erl` также интегрирован
- ✅ `router_ack_consumer.erl` также интегрирован

### ✅ 6. Добавить DLQ поддержку

**Реализация**:
- ✅ DLQ публикация через `router_intake_error_handler:send_to_dlq/5`
- ✅ Конфигурируемый DLQ subject pattern (default: `{subject}.dlq`)
- ✅ DLQ message format с payload hash (SHA256) вместо полного payload
- ✅ Обработка ошибок публикации в DLQ (best-effort, не блокирует обработку)
- ✅ Метрики для DLQ (success и failures)
- ✅ Конфигурация для включения/выключения DLQ

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
    "trace_id": "trace_def456"
  },
  "received_at": 1706367600123,
  "router_node_id": "router@node1"
}
```

**Конфигурация**:
```erlang
{dlq_subject_pattern, undefined},  %% undefined = append .dlq, или custom pattern
{dlq_enabled, true}  %% Enable/disable DLQ publication
```

## Архитектура валидации

### Последовательность валидации

```
1. Schema Validation
   ├─ decide: Protobuf decode → Map
   ├─ result: JSON decode → Map
   └─ ack: JSON decode → Map

2. Version Validation
   ├─ Extract from subject (regex)
   ├─ Extract from payload
   ├─ Extract from headers
   └─ Priority: Headers > Payload > Subject

3. Correlation Fields Validation
   ├─ Required fields (tenant_id)
   ├─ Field dependencies (run_id → flow_id → step_id)
   ├─ Format validation (UUID, ULID, W3C Trace Context)
   └─ Build correlation context

4. Message-Type Specific Validation
   ├─ decide: request_id, task.type, task.payload, tenant, idempotency
   ├─ result: (delegated to router_result_consumer)
   └─ ack: (delegated to router_ack_consumer)
```

### Обработка ошибок

**Error Codes**:
- `schema_validation_failed` - ошибки декодирования protobuf/JSON
- `version_unsupported` - неподдерживаемая версия
- `correlation_fields_invalid` - проблемы с корреляционными полями
- `tenant_forbidden` - тенант не проходит ACL/allowlist
- `idempotency_violation` - конфликт идемпотентности
- `internal_validation_error` - неожиданные исключения

**Error Handling Flow**:
```
Validation Error
  ↓
router_intake_error_handler:handle_intake_error/7
  ├─ Audit logging
  ├─ Metrics emission
  ├─ DLQ publication (if enabled)
  ├─ NATS message fate (ACK/NAK)
  └─ Error response (for request-reply)
```

## Интеграция с consumer'ами

### router_decide_consumer.erl

**Использование**:
- ✅ Вызов `router_intake_validator:validate_intake_message/4` для всех decide messages
- ✅ Обработка idempotency status (duplicate → ACK без обработки)
- ✅ Обработка ошибок валидации через error handler

### router_result_consumer.erl

**Использование**:
- ✅ Вызов `router_intake_validator:validate_intake_message/4` для всех result messages
- ✅ Обработка ошибок валидации через error handler

### router_ack_consumer.erl

**Использование**:
- ✅ Вызов `router_intake_validator:validate_intake_message/4` для всех ack messages
- ✅ Обработка ошибок валидации через error handler

## Результаты

### Инварианты после реализации

✅ **Unified Validation Layer**:
- Все входящие сообщения проходят через единый слой валидации
- Гарантированная схема, версия и корреляционные поля
- Детерминированная обработка ошибок

✅ **Protobuf Support**:
- Decide messages декодируются через protobuf
- Fallback на JSON для backward compatibility
- Конвертация protobuf records в maps

✅ **Version Control**:
- Версия извлекается из subject, payload или headers
- Приоритет: Headers > Payload > Subject
- Поддержка только версии 1 (конфигурируемо)

✅ **Correlation Fields**:
- Валидация обязательных полей (tenant_id)
- Валидация зависимостей между полями
- Валидация форматов (UUID, ULID, W3C Trace Context)

✅ **DLQ Support**:
- Конфигурируемый DLQ subject pattern
- DLQ message format с payload hash
- Обработка ошибок публикации
- Метрики для мониторинга

## Созданные/обновлённые файлы

### Модули

1. **`router_intake_validator.erl`** (создан):
   - Базовая структура валидации
   - Protobuf decode для decide messages
   - JSON decode для result/ack messages
   - Валидация версии
   - Валидация корреляционных полей
   - Валидация форматов (UUID, ULID, W3C Trace Context)
   - Decide-specific validation (request_id, task, tenant, idempotency)

2. **`router_intake_error_handler.erl`** (обновлён):
   - DLQ поддержка с конфигурируемым subject pattern
   - Обработка ошибок публикации в DLQ
   - Метрики для DLQ failures

3. **`router_decide_consumer.erl`** (обновлён):
   - Интеграция с `router_intake_validator`
   - Обработка idempotency status
   - Обработка ошибок валидации

4. **`router_result_consumer.erl`** (обновлён):
   - Интеграция с `router_intake_validator`

5. **`router_ack_consumer.erl`** (обновлён):
   - Интеграция с `router_intake_validator`

6. **`router_metrics.erl`** (обновлён):
   - Метрика `router_intake_dlq_publish_failed_total`

7. **`beamline_router.app.src`** (обновлён):
   - Конфигурация `dlq_subject_pattern`
   - Конфигурация `dlq_enabled`

### Документация

1. **`docs/NATS_SUBJECTS.md`** (обновлён):
   - Раздел о DLQ subject

2. **`docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`** (обновлён):
   - Информация о DLQ

3. **`docs/archive/dev/ROUTER_DLQ_IMPROVEMENTS_REPORT.md`** (создан):
   - Отчёт об улучшениях DLQ

4. **`docs/archive/dev/ROUTER_INTAKE_VALIDATOR_COMPLETE_REPORT.md`** (создан):
   - Итоговый отчёт о полной реализации валидатора

## Технические детали

### Protobuf Decode

**Для decide messages**:
- Использует `flow_pb:decode_msg/2` для декодирования RouteRequest
- Конвертирует protobuf records в maps для совместимости
- Fallback на JSON decode для backward compatibility

**Конвертация**:
- `RouteRequest` protobuf → map с полями: message, policy_id, context
- `Message` protobuf → map с полями: message_id, tenant_id, trace_id, message_type, payload, metadata, timestamp_ms

### Version Validation

**Извлечение версии**:
- Subject: regex `\.v([0-9]+)\.` → извлекает версию из subject
- Payload: `maps:get(<<"version">>, Message, undefined)`
- Headers: `maps:get(<<"version">>, Headers, undefined)`

**Приоритет**:
1. Headers (highest priority)
2. Payload
3. Subject (lowest priority)

### Correlation Fields Validation

**Обязательные поля**:
- `tenant_id` - обязателен для всех message types

**Зависимости**:
- `run_id` → требует `flow_id` и `step_id`
- `flow_id` → требует `run_id`
- `step_id` → требует `run_id` и `flow_id`

**Форматы**:
- `run_id`, `flow_id`, `step_id`: UUID v4 или ULID
- `trace_id`: W3C Trace Context (32 hex) или UUID v4
- `idempotency_key`: non-empty string, max 256 chars

### DLQ Support

**DLQ Subject Pattern**:
- Default: `{original_subject}.dlq` (e.g., `beamline.router.v1.decide.dlq`)
- Configurable: `dlq_subject_pattern` (e.g., `beamline.router.v1.intake.dlq`)

**DLQ Message**:
- Contains payload hash (SHA256) instead of full payload
- Contains validation error details
- Contains filtered context (PII removed)
- Contains metadata (received_at, router_node_id)

**Error Handling**:
- DLQ publication is best-effort (errors don't fail processing)
- Errors are logged and metrics are emitted
- Always returns `ok` (doesn't block processing)

## Заключение

Все 6 задач выполнены успешно:
- ✅ Создан `router_intake_validator.erl` с базовой структурой
- ✅ Реализована валидация схемы (protobuf decode)
- ✅ Реализована валидация версии
- ✅ Реализована валидация корреляционных полей
- ✅ Интегрирован с decide consumer (и result/ack consumers)
- ✅ Добавлена DLQ поддержка

Router теперь имеет полный unified intake validation layer с:
- Protobuf decode для decide messages
- Version validation с приоритетами
- Correlation fields validation с форматами
- DLQ support с конфигурируемым subject pattern
- Интеграция со всеми consumer'ами
- Обработка ошибок через error handler

Все модули компилируются без ошибок, интеграция завершена, документация обновлена.

