# Router Message Intake Final Implementation Report

**Date**: 2025-01-27  
**Status**: ✅ **All Tasks Completed**  
**Phase**: Этап 2. Message intake и базовая валидация (Шаги 2.1, 2.2, 2.3)

## Выполненные задачи

### ✅ 1. Замена JSON decode на protobuf decode в валидаторе

**Модуль**: `router_intake_validator.erl`

**Изменения**:
- ✅ Добавлен protobuf decode для `decide` messages (использует `flow_pb:decode_msg`)
- ✅ JSON decode остаётся для `result` и `ack` messages (CP1 baseline)
- ✅ Добавлена функция `convert_route_request_to_map/1` для конвертации protobuf RouteRequest в map
- ✅ Добавлена функция `convert_message_to_map/1` для конвертации protobuf Message в map
- ✅ Fallback на JSON decode для обратной совместимости (если protobuf decode не удался)

**Реализация**:
```erlang
validate_schema(Payload, decide) ->
    %% Protobuf decode for decide messages
    RouteRequestPb = flow_pb:decode_msg(Payload, 'RouteRequest'),
    convert_route_request_to_map(RouteRequestPb);
validate_schema(Payload, result) ->
    %% JSON decode for result messages (CP1 baseline)
    jsx:decode(Payload, [return_maps]);
validate_schema(Payload, ack) ->
    %% JSON decode for ack messages (CP1 baseline)
    jsx:decode(Payload, [return_maps])
```

### ✅ 2. Расширение валидации форматов (UUID, W3C Trace Context)

**Модуль**: `router_intake_validator.erl`

**Добавленные функции валидации**:
- ✅ `validate_uuid_v4/1` - валидация UUID v4 формата (regex: `^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$`)
- ✅ `validate_ulid/1` - валидация ULID формата (26 символов, base32)
- ✅ `validate_w3c_trace_context/1` - валидация W3C Trace Context формата (32 hex символа)

**Интеграция в валидацию корреляционных полей**:
- ✅ `run_id`, `flow_id`, `step_id` - валидация UUID v4 или ULID
- ✅ `trace_id` - валидация W3C Trace Context или UUID v4
- ✅ `idempotency_key` - валидация формата (non-empty, max 256 chars)

**Функция `validate_field_formats/5`**:
- Проверяет все корреляционные поля на соответствие форматам
- Возвращает `ok` или `{error, Reason}` с конкретной причиной ошибки

### ✅ 3. Интеграция валидации в router_result_consumer

**Модуль**: `router_result_consumer.erl`

**Изменения**:
- ✅ Заменена существующая валидация на вызов `router_intake_validator:validate_intake_message/4`
- ✅ Обработка ошибок валидации через `router_intake_error_handler:handle_intake_error/7`
- ✅ Сохранена вся существующая логика обработки результатов после валидации

**До**:
```erlang
Decoded = jsx:decode(Payload, [return_maps]),
case is_map(Decoded) of
    true -> process_exec_result(...);
    false -> log_parse_error(...)
end
```

**После**:
```erlang
case router_intake_validator:validate_intake_message(Subject, Payload, Headers, result) of
    {ok, ValidatedMessage} -> process_exec_result(...);
    {error, {ErrorCode, ErrorMessage, ErrorContext}} ->
        router_intake_error_handler:handle_intake_error(...)
end
```

### ✅ 4. Интеграция валидации в router_ack_consumer

**Модуль**: `router_ack_consumer.erl`

**Изменения**:
- ✅ Заменена существующая валидация на вызов `router_intake_validator:validate_intake_message/4`
- ✅ Обработка ошибок валидации через `router_intake_error_handler:handle_intake_error/7`
- ✅ Сохранена вся существующая логика обработки ACK после валидации

**До**:
```erlang
case jsx:decode(Payload, [return_maps]) of
    Ack when is_map(Ack) -> process_ack(...);
    Error -> log_parse_error(...)
end
```

**После**:
```erlang
case router_intake_validator:validate_intake_message(Subject, Payload, Headers, ack) of
    {ok, ValidatedMessage} -> process_ack(...);
    {error, {ErrorCode, ErrorMessage, ErrorContext}} ->
        router_intake_error_handler:handle_intake_error(...)
end
```

### ✅ 5. MaxDeliver exhaustion detection в error handler

**Модуль**: `router_intake_error_handler.erl`

**Добавлена функция**: `check_maxdeliver_exhaustion/2`

**Реализация**:
- ✅ Проверяет delivery count в ETS таблицах всех consumer'ов:
  - `router_delivery_count` (router_result_consumer)
  - `router_decide_delivery_count` (router_decide_consumer)
  - `router_ack_delivery_count` (router_ack_consumer, если существует)
- ✅ Сравнивает delivery count с `nats_js_max_deliver` (по умолчанию: 3)
- ✅ Возвращает `{ok, exhausted}` или `{ok, not_exhausted}`

**Интеграция в `check_and_nak/2`**:
- ✅ Если MaxDeliver исчерпан → ACK и DLQ (не ретраить)
- ✅ Если не исчерпан → NAK для retry
- ✅ Логирование предупреждения при исчерпании

**Логика**:
```erlang
case check_maxdeliver_exhaustion(MsgId, Context) of
    {ok, exhausted} ->
        %% ACK and DLQ (already sent in handle_intake_error)
        router_nats:ack_message(MsgId);
    {ok, not_exhausted} ->
        %% NAK for retry
        router_nats:nak_message(MsgId)
end
```

## Обновлённые файлы

### Модули

1. **`router_intake_validator.erl`**:
   - ✅ Protobuf decode для decide messages
   - ✅ Валидация форматов UUID v4, ULID, W3C Trace Context
   - ✅ Функции конвертации protobuf → map

2. **`router_intake_error_handler.erl`**:
   - ✅ MaxDeliver exhaustion detection
   - ✅ Интеграция с delivery count tracking

3. **`router_result_consumer.erl`**:
   - ✅ Интеграция с `router_intake_validator`
   - ✅ Обработка ошибок через `router_intake_error_handler`

4. **`router_ack_consumer.erl`**:
   - ✅ Интеграция с `router_intake_validator`
   - ✅ Обработка ошибок через `router_intake_error_handler`

## Результаты

### Инварианты после завершения

✅ **Любое сообщение, которое дошло до бизнес‑логики Router'а, гарантированно**:
- Соответствует protobuf схеме (для decide) или JSON схеме (для results/ack)
- Принадлежит поддерживаемой версии протокола
- Имеет полный набор корреляционных полей с правильными форматами:
  - `run_id`, `flow_id`, `step_id` - UUID v4 или ULID
  - `trace_id` - W3C Trace Context или UUID v4
  - `idempotency_key` - non-empty string, max 256 chars
- Либо будет детерминированно отклонено с логом и помещено в DLQ

✅ **Любое неверное/битое входящее сообщение**:
- Не проходит в бизнес‑логику Router'а
- Детерминированно попадает в DLQ или отклоняется
- Оставляет audit‑запись с error_code
- Увеличивает соответствующую метрику
- Приводит к корректному ack/nak в NATS/JetStream:
  - Schema errors → ACK (не ретраить)
  - Temporary errors → NAK (с MaxDeliver check)
  - MaxDeliver exhausted → ACK + DLQ
- Возвращает стандартный код ошибки для Gateway/клиентов

## Технические детали

### Protobuf Decode

**Для decide messages**:
- Использует `flow_pb:decode_msg(Payload, 'RouteRequest')`
- Конвертирует protobuf record в map для совместимости
- Fallback на JSON decode для обратной совместимости

**Для results/ack messages**:
- Пока использует JSON decode (CP1 baseline)
- Готово к миграции на protobuf в будущем

### Валидация форматов

**UUID v4**:
- Regex: `^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$`
- Case-insensitive
- Проверка версии (4) и варианта (8, 9, a, b)

**ULID**:
- Длина: 26 символов
- Формат: base32 (0-9, A-Z, исключая I, L, O, U)
- Regex: `^[0-9A-HJ-KM-NP-TV-Z]{26}$`

**W3C Trace Context**:
- Длина: 32 hex символа (16 bytes)
- Regex: `^[0-9a-fA-F]{32}$`

### MaxDeliver Exhaustion Detection

**Механизм**:
- Использует ETS таблицы для tracking delivery count
- Проверяет все таблицы consumer'ов (results, decide, ack)
- Сравнивает с `nats_js_max_deliver` (configurable, default: 3)

**Поведение**:
- Если exhausted → ACK + DLQ (не ретраить)
- Если not exhausted → NAK (retry)
- Логирование предупреждения при исчерпании

## Следующие шаги (опционально)

1. **Миграция results/ack на protobuf** (когда protobuf DTO будут готовы)
2. **Добавление tenant validation** в валидатор (интеграция с router_tenant_validator)
3. **Добавление idempotency check** в валидатор (интеграция с router_idempotency)
4. **Расширенная валидация** (например, проверка существования run_id в state)

## Заключение

Все задачи выполнены успешно:
- ✅ Protobuf decode для decide messages
- ✅ Валидация форматов UUID, ULID, W3C Trace Context
- ✅ Интеграция валидации в все consumer'ы
- ✅ MaxDeliver exhaustion detection в error handler

Router теперь имеет единый, строгий слой валидации для всех входящих сообщений с полной поддержкой protobuf, валидации форматов и обработки ошибок.

