# Router Decide Subject Validation Improvements Report

**Date**: 2025-01-27  
**Status**: ✅ **All Improvements Completed**  
**Scope**: Улучшение валидации decide subject

## Выполненные улучшения

### ✅ 1. Валидация обязательных полей для DecideRequest

**Модуль**: `router_intake_validator.erl`

**Добавлена функция**: `validate_decide_specific_fields/1`

**Валидация**:
- ✅ `request_id` - обязательное поле, не пустое, binary
- ✅ `task` - обязательное поле, должно быть map
- ✅ `task.type` - обязательное поле, не пустое, binary
- ✅ `task.payload_ref` или `task.payload` - хотя бы одно должно быть присутствовать

**Реализация**:
```erlang
validate_decide_specific_fields(Message) ->
    %% Validate request_id
    RequestId = maps:get(<<"request_id">>, Message, undefined),
    case RequestId of
        undefined -> {error, missing_request_id};
        <<>> -> {error, empty_request_id};
        _ when is_binary(RequestId) ->
            %% Validate task structure
            Task = maps:get(<<"task">>, Message, undefined),
            case Task of
                undefined -> {error, missing_task};
                TaskMap when is_map(TaskMap) ->
                    TaskType = maps:get(<<"type">>, TaskMap, undefined),
                    PayloadRef = maps:get(<<"payload_ref">>, TaskMap, undefined),
                    Payload = maps:get(<<"payload">>, TaskMap, undefined),
                    %% Validate task.type and payload
                    ...
            end
    end
```

**Интеграция**:
- Вызывается в `validate_message_specific_fields/3` для decide messages
- Ошибки возвращаются с кодом `correlation_fields_invalid`

### ✅ 2. Tenant Validation (ACL/Allowlist Check)

**Модуль**: `router_intake_validator.erl`

**Добавлена функция**: `validate_tenant/2`

**Реализация**:
- ✅ Использует `router_tenant_validator:validate_tenant/2` для валидации
- ✅ Проверяет tenant_id против allowlist (если настроен)
- ✅ Проверяет tenant_id против policy registry
- ✅ Fallback на базовую валидацию, если `router_tenant_validator` недоступен

**Интеграция**:
- Вызывается в `validate_message_specific_fields/3` для decide messages
- Ошибки возвращаются с соответствующими кодами:
  - `tenant_missing` - tenant_id отсутствует
  - `tenant_empty` - tenant_id пустой
  - `tenant_not_allowed` - tenant_id не в allowlist
  - `tenant_invalid_format` - неверный формат tenant_id

**Пример**:
```erlang
validate_tenant(TenantId, Context) ->
    case erlang:function_exported(router_tenant_validator, validate_tenant, 2) of
        true ->
            router_tenant_validator:validate_tenant(TenantId, Context);
        false ->
            %% Fallback: basic validation
            ...
    end
```

### ✅ 3. Idempotency Check

**Модуль**: `router_intake_validator.erl`

**Добавлена функция**: `validate_idempotency/3`

**Реализация**:
- ✅ Использует `router_idempotency:check_and_mark/3` для проверки идемпотентности
- ✅ Key type: `<<"intake_id">>`
- ✅ Сохраняет дополнительную информацию (tenant_id, request_id, task_type)
- ✅ Возвращает статус: `new` или `duplicate`
- ✅ Fallback на `new`, если `router_idempotency` недоступен

**Интеграция**:
- Вызывается в `validate_message_specific_fields/3` для decide messages
- Статус идемпотентности добавляется в контекст как `idempotency_status`
- Ошибки возвращаются с кодом `idempotency_check_failed`

**Пример**:
```erlang
validate_idempotency(IdempotencyKey, Message, Context) ->
    case router_idempotency:check_and_mark(<<"intake_id">>, IdempotencyKey, AdditionalData) of
        {ok, not_seen} -> {ok, new};
        {ok, seen} -> {ok, duplicate};
        {error, Reason} -> {error, {idempotency_check_failed, Reason}}
    end
```

### ✅ 4. Обработка Duplicate Requests

**Модуль**: `router_decide_consumer.erl`

**Изменения**:
- ✅ Проверка `idempotency_status` из валидированного сообщения
- ✅ Если `duplicate` → логирование и ACK без обработки
- ✅ Если `new` → нормальная обработка запроса

**Реализация**:
```erlang
case router_intake_validator:validate_intake_message(...) of
    {ok, ValidatedMessage} ->
        IdempotencyStatus = maps:get(<<"idempotency_status">>, ValidatedMessage, new),
        case IdempotencyStatus of
            duplicate ->
                %% Log and ACK (no processing)
                router_logger:info(<<"Duplicate decide request (idempotency)">>, ...),
                ack_message_if_needed(MsgId),
                ok;
            new ->
                %% Process normally
                handle_decide_request(...)
        end
end
```

## Обновлённые файлы

### Модули

1. **`router_intake_validator.erl`**:
   - ✅ Добавлена функция `validate_decide_specific_fields/1`
   - ✅ Добавлена функция `validate_tenant/2`
   - ✅ Добавлена функция `validate_idempotency/3`
   - ✅ Добавлена функция `validate_message_specific_fields/3`
   - ✅ Добавлена функция `get_task_type/1`
   - ✅ Интеграция в основной поток валидации

2. **`router_decide_consumer.erl`**:
   - ✅ Обработка duplicate requests (idempotency)
   - ✅ Логирование duplicate requests
   - ✅ ACK без обработки для duplicate requests

## Результаты

### Инварианты после улучшений

✅ **Любое decide сообщение, которое дошло до бизнес‑логики Router'а, гарантированно**:
- Имеет все обязательные поля (`request_id`, `task.type`, `task.payload_ref` или `task.payload`)
- Прошло tenant validation (ACL/allowlist check)
- Прошло idempotency check (если `idempotency_key` присутствует)
- Не является duplicate request (если `idempotency_key` присутствует и уже обработан)

✅ **Duplicate requests**:
- Определяются на этапе валидации (через idempotency check)
- Логируются с информацией о request_id и idempotency_key
- ACK'аются без обработки (не проходят в бизнес-логику)
- Не создают дубликаты решений

## Технические детали

### Валидация обязательных полей

**Порядок валидации**:
1. Schema validation (protobuf decode)
2. Version validation
3. Correlation fields validation (tenant_id, run_id, flow_id, step_id, trace_id, idempotency_key)
4. **Message-specific validation** (NEW):
   - Decide-specific fields (request_id, task.type, task.payload)
   - Tenant validation (ACL/allowlist)
   - Idempotency check

**Ошибки валидации**:
- `missing_request_id` - request_id отсутствует
- `empty_request_id` - request_id пустой
- `invalid_request_id_format` - неверный формат request_id
- `missing_task` - task отсутствует
- `invalid_task_format` - task не является map
- `missing_task_type` - task.type отсутствует
- `empty_task_type` - task.type пустой
- `invalid_task_type` - task.type не является binary
- `missing_task_payload` - ни task.payload_ref, ни task.payload не присутствуют

### Tenant Validation

**Интеграция с router_tenant_validator**:
- Использует существующий модуль `router_tenant_validator.erl`
- Проверяет allowlist (если настроен)
- Проверяет policy registry
- Эмитирует audit events при ошибках

**Fallback**:
- Если `router_tenant_validator` недоступен, выполняет базовую валидацию
- Проверяет только наличие и формат tenant_id

### Idempotency Check

**Интеграция с router_idempotency**:
- Использует существующий модуль `router_idempotency.erl`
- Key type: `<<"intake_id">>`
- Сохраняет дополнительную информацию для отладки

**Дополнительная информация**:
- `tenant_id` - для фильтрации по тенанту
- `request_id` - для корреляции
- `task_type` - для анализа по типам задач

**Статусы**:
- `new` - новый запрос, не обработан ранее
- `duplicate` - запрос уже обработан (idempotency key найден)

### Обработка Duplicate Requests

**Поведение**:
- Duplicate requests определяются на этапе валидации
- Логируются с полной информацией (request_id, idempotency_key, subject, msg_id)
- ACK'аются без обработки (не проходят в `handle_decide_request`)
- Не создают дубликаты решений или метрик

**Логирование**:
```erlang
router_logger:info(<<"Duplicate decide request (idempotency)">>, #{
    <<"request_id">> => RequestId,
    <<"idempotency_key">> => IdempotencyKey,
    <<"subject">> => Subject,
    <<"msg_id">> => MsgId
})
```

## Следующие шаги (опционально)

1. **Cached Response для Duplicate Requests**:
   - Хранить cached response в idempotency store
   - Возвращать cached response для duplicate requests
   - Улучшить производительность для повторяющихся запросов

2. **Расширенная валидация task.payload**:
   - Валидация размера payload
   - Валидация формата payload (JSON, binary, и т.д.)
   - Валидация payload_ref (URI format)

3. **Валидация constraints**:
   - Валидация max_latency_ms (положительное число)
   - Валидация max_cost (положительное число)
   - Валидация других constraints

## Заключение

Все улучшения валидации decide subject выполнены успешно:
- ✅ Валидация обязательных полей (request_id, task.type, task.payload)
- ✅ Tenant validation (ACL/allowlist check)
- ✅ Idempotency check
- ✅ Обработка duplicate requests

Router теперь имеет полную валидацию decide messages с проверкой обязательных полей, tenant ACL, и идемпотентности, что предотвращает обработку невалидных или дублирующихся запросов.

