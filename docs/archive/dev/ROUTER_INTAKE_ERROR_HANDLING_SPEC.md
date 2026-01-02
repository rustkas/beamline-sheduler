# Router Intake Error Handling Specification: Шаг 2.3

**Date**: 2025-01-27  
**Status**: 📋 **Specification & Planning**  
**Phase**: Этап 2. Message intake и базовая валидация - Шаг 2.3

## Цель

Обеспечить детерминированную обработку **любого неверного/битого входящего сообщения**:

- ✅ Не ломает Router
- ✅ Детерминированно попадает в понятное состояние (DLQ / reject)
- ✅ Оставляет след в audit‑логе
- ✅ Увеличивает правильные метрики
- ✅ Возвращает/публикует **стандартный код ошибки**

## Инвариант после завершения

> **Любое неверное/битое входящее сообщение:**
> - не проходит в бизнес‑логику Router'а,
> - детерминированно попадает в DLQ или отклоняется,
> - оставляет audit‑запись с error_code,
> - увеличивает соответствующую метрику,
> - приводит к корректному ack/nack в NATS/JetStream,
> - возвращает стандартный код ошибки для Gateway/клиентов.

## Текущее состояние

### Существующие механизмы

**Error Codes**:
- ✅ `router_result_consumer.erl` имеет функцию `error_code_reason/1` для маппинга ошибок
- ✅ Существующие коды: `PARSE_ERROR`, `MISSING_CORRELATION_ID`, `MISSING_STATUS`, `INVALID_STATUS`, `TENANT_VALIDATION_FAILED`, `USAGE_EMIT_FAILED`
- ⚠️ Нет единого перечня кодов для intake валидации

**Audit Logging**:
- ✅ `router_audit.erl` модуль существует для audit логирования
- ✅ Используется для policy и RBAC операций
- ⚠️ Нет audit логирования для intake validation failures

**Metrics**:
- ✅ Telemetry используется для метрик
- ✅ Существующие метрики: `router_results_validation_failed_total`, `router_results_tenant_rejected_total`
- ⚠️ Нет единого блока метрик для intake валидации

**Error Handling**:
- ✅ NAK для tenant validation failures (results/ack consumers)
- ✅ MaxDeliver exhaustion detection
- ⚠️ Нет DLQ публикации для validation failures
- ⚠️ Нет единого обработчика ошибок для intake

## Классификация ошибок и коды

### Перечень доменных кодов ошибок

**Ограниченный, фиксированный набор доменных кодов ошибок** для intake валидации:

#### 1. SCHEMA_VALIDATION_FAILED

**Код**: `SCHEMA_VALIDATION_FAILED`

**Описание**: Невозможно распарсить protobuf / JSON, отсутствуют обязательные поля.

**Причины**:
- Невозможно декодировать protobuf message
- Невозможно распарсить JSON payload
- Отсутствуют обязательные поля в protobuf message
- Неверный формат обязательных полей

**Severity**: `ERROR`

**Machine-readable код**: `"SCHEMA_VALIDATION_FAILED"`

**Человеко‑читаемое сообщение**: `"Schema validation failed: {reason}"`

**Примеры**:
- `"Schema validation failed: missing tenant_id"`
- `"Schema validation failed: invalid protobuf format"`
- `"Schema validation failed: missing required field 'message'"`

#### 2. VERSION_UNSUPPORTED

**Код**: `VERSION_UNSUPPORTED`

**Описание**: Неподдерживаемая `schema_version` / subject‑версия.

**Причины**:
- `schema_version` отсутствует
- `schema_version` не поддерживается (например, `"2"` когда поддерживается только `"1"`)
- Subject содержит неподдерживаемую версию (например, `beamline.router.v2.decide`)

**Severity**: `ERROR`

**Machine-readable код**: `"VERSION_UNSUPPORTED"`

**Человеко‑читаемое сообщение**: `"Unsupported schema version: {version}, supported versions: {supported}"`

**Примеры**:
- `"Unsupported schema version: 2, supported versions: [1]"`
- `"Missing schema version, supported versions: [1]"`

#### 3. CORRELATION_FIELDS_INVALID

**Код**: `CORRELATION_FIELDS_INVALID`

**Описание**: Проблема с `run_id` / `flow_id` / `step_id` / `idempotency_key` / `trace_id`.

**Причины**:
- Отсутствует обязательное поле (CP2+)
- Неверный формат поля (не UUID/ULID, не W3C Trace Context)
- Нарушены зависимости между полями (например, `run_id` есть, но `flow_id` отсутствует)
- Пустое значение обязательного поля

**Severity**: `ERROR`

**Machine-readable код**: `"CORRELATION_FIELDS_INVALID"`

**Человеко‑читаемое сообщение**: `"Correlation fields validation failed: {reason}"`

**Примеры**:
- `"Correlation fields validation failed: missing run_id (required for CP2+)"`
- `"Correlation fields validation failed: invalid trace_id format"`
- `"Correlation fields validation failed: flow_id required when run_id present"`

#### 4. TENANT_FORBIDDEN

**Код**: `TENANT_FORBIDDEN`

**Описание**: Тенант не проходит ACL / allowlist.

**Причины**:
- `tenant_id` отсутствует
- `tenant_id` не в allowlist
- `tenant_id` не проходит ACL проверку
- `tenant_id` имеет неверный формат

**Severity**: `ERROR`

**Machine-readable код**: `"TENANT_FORBIDDEN"`

**Человеко‑читаемое сообщение**: `"Tenant validation failed: {reason}"`

**Примеры**:
- `"Tenant validation failed: tenant_id not in allowlist"`
- `"Tenant validation failed: missing tenant_id"`
- `"Tenant validation failed: ACL check failed"`

#### 5. IDEMPOTENCY_VIOLATION

**Код**: `IDEMPOTENCY_VIOLATION`

**Описание**: Повторное сообщение с уже обработанным `idempotency_key`, но с конфликтующими данными.

**Причины**:
- `idempotency_key` уже обработан, но данные сообщения отличаются от оригинального
- `idempotency_key` имеет неверный формат
- Конфликт идемпотентности (same key, different payload)

**Severity**: `WARN` (не ERROR, так как это может быть retry с теми же данными)

**Machine-readable код**: `"IDEMPOTENCY_VIOLATION"`

**Человеко‑читаемое сообщение**: `"Idempotency violation: {reason}"`

**Примеры**:
- `"Idempotency violation: duplicate request with conflicting data"`
- `"Idempotency violation: invalid idempotency_key format"`

**Примечание**: Если `idempotency_key` уже обработан с **теми же данными** → это не ошибка, а нормальный duplicate (возвращаем cached response).

#### 6. INTERNAL_VALIDATION_ERROR

**Код**: `INTERNAL_VALIDATION_ERROR`

**Описание**: Неожиданное исключение внутри слоя валидации.

**Причины**:
- Исключение при декодировании protobuf
- Исключение при валидации полей
- Исключение в модуле валидации
- Системная ошибка (ETS недоступен, и т.п.)

**Severity**: `ERROR`

**Machine-readable код**: `"INTERNAL_VALIDATION_ERROR"`

**Человеко‑читаемое сообщение**: `"Internal validation error: {reason}"`

**Примеры**:
- `"Internal validation error: protobuf decode exception"`
- `"Internal validation error: ETS table not available"`

### Реализация кодов ошибок

**Модуль**: `router_intake_error_codes.erl`

**Функции**:
```erlang
%% Определение кодов ошибок
-type error_code() :: 
    schema_validation_failed |
    version_unsupported |
    correlation_fields_invalid |
    tenant_forbidden |
    idempotency_violation |
    internal_validation_error.

%% Маппинг кода на machine-readable строку
-spec error_code_to_string(error_code()) -> binary().
error_code_to_string(schema_validation_failed) -> <<"SCHEMA_VALIDATION_FAILED">>;
error_code_to_string(version_unsupported) -> <<"VERSION_UNSUPPORTED">>;
error_code_to_string(correlation_fields_invalid) -> <<"CORRELATION_FIELDS_INVALID">>;
error_code_to_string(tenant_forbidden) -> <<"TENANT_FORBIDDEN">>;
error_code_to_string(idempotency_violation) -> <<"IDEMPOTENCY_VIOLATION">>;
error_code_to_string(internal_validation_error) -> <<"INTERNAL_VALIDATION_ERROR">>.

%% Получить severity для кода ошибки
-spec error_code_severity(error_code()) -> warn | error.
error_code_severity(idempotency_violation) -> warn;
error_code_severity(_) -> error.

%% Создать человеко‑читаемое сообщение
-spec error_code_message(error_code(), map()) -> binary().
error_code_message(schema_validation_failed, Context) ->
    Reason = maps:get(<<"reason">>, Context, <<"unknown">>),
    <<"Schema validation failed: ", Reason/binary>>;
error_code_message(version_unsupported, Context) ->
    Version = maps:get(<<"version">>, Context, <<"unknown">>),
    Supported = maps:get(<<"supported_versions">>, Context, [<<"1">>]),
    SupportedStr = string:join([binary_to_list(V) || V <- Supported], ", "),
    <<"Unsupported schema version: ", Version/binary, ", supported versions: [", (list_to_binary(SupportedStr))/binary, "]">>;
%% ... остальные коды
```

## Что делает Router при неверном сообщении

### Последовательность действий

Для любого входящего сообщения, которое **НЕ прошло слой 2.2** (валидация):

#### 1. Не пускает дальше по pipeline

- ❌ Никакой бизнес‑логики
- ❌ Никаких вызовов Workers/Extensions
- ❌ Никаких вызовов `router_core:route/2`
- ✅ Остановка обработки на уровне intake

#### 2. Пишет audit‑событие

**Модуль**: `router_audit.erl` (расширить) или новый `router_intake_audit.erl`

**Функция**:
```erlang
-spec log_intake_validation_failed(
    ErrorCode :: error_code(),
    ErrorMessage :: binary(),
    Subject :: binary(),
    Context :: map()
) -> ok.
```

**Audit запись**:
```erlang
#{
    <<"event_type">> => <<"router.intake.validation_failed">>,
    <<"error_code">> => error_code_to_string(ErrorCode),
    <<"error_message">> => ErrorMessage,
    <<"subject">> => Subject,
    <<"tenant_id">> => maps:get(<<"tenant_id">>, Context, undefined),
    <<"run_id">> => maps:get(<<"run_id">>, Context, undefined),
    <<"flow_id">> => maps:get(<<"flow_id">>, Context, undefined),
    <<"step_id">> => maps:get(<<"step_id">>, Context, undefined),
    <<"idempotency_key">> => maps:get(<<"idempotency_key">>, Context, undefined),
    <<"trace_id">> => maps:get(<<"trace_id">>, Context, undefined),
    <<"received_at">> => erlang:system_time(millisecond),
    <<"router_node_id">> => node(),
    <<"msg_id">> => maps:get(<<"msg_id">>, Context, undefined)
}
```

**Формат**:
- Структурированный JSON‑лог (согласно `OBSERVABILITY_CP1_INVARIANTS.md`)
- Опционально: отдельный audit‑stream (`beamline.audit.v1.router.intake`)

**Важно**: **Не логировать payload целиком**, особенно если он может содержать PII или секреты. Только метаданные и технические поля.

#### 3. Публикует/фиксирует результат как ошибку

**Для request-reply паттерна** (decide subject):
- Формирует стандартный error DTO
- Отправляет через reply subject (`beamline.router.v1.decide.reply`)

**Для pub-sub паттерна** (results/ack subjects):
- Публикует в DLQ subject
- Опционально: публикует в error subject для Gateway

#### 4. Инкрементирует метрики

**Метрики**:
- `router_intake_validation_errors_total{error_code="SCHEMA_VALIDATION_FAILED"}`
- `router_intake_validation_errors_total{error_code="VERSION_UNSUPPORTED"}`
- `router_intake_validation_errors_total{error_code="CORRELATION_FIELDS_INVALID"}`
- `router_intake_validation_errors_total{error_code="TENANT_FORBIDDEN"}`
- `router_intake_validation_errors_total{error_code="IDEMPOTENCY_VIOLATION"}`
- `router_intake_validation_errors_total{error_code="INTERNAL_VALIDATION_ERROR"}`

**Общие метрики**:
- `router_intake_messages_total{subject="beamline.router.v1.decide", status="failed"}`
- `router_intake_messages_total{subject="beamline.router.v1.decide", status="ok"}`

**DLQ метрики**:
- `router_intake_dlq_messages_total{reason="validation_failed", error_code="SCHEMA_VALIDATION_FAILED"}`

#### 5. Решает судьбу NATS‑сообщения

**Предпочтительный вариант: Ack + publish to DLQ**

1. Router делает `ack()` входного сообщения
2. Публикует отдельное сообщение в DLQ subject:
   - `beamline.router.v1.intake.dlq` (общий)
   - или `beamline.router.v1.decide.dlq` (специфичный для decide)
3. В DLQ‑payload кладёт:
   - `error_code`
   - метаданные сообщения (subject, tenant_id, run_id, и т.п.)
   - (опционально) усечённый payload или хэш

**Альтернативный вариант: Nack + max_deliver policy**

- Использовать только для **временных ошибок** (не для schema errors)
- Для schema errors → всегда Ack + DLQ (не ретраить)
- Nack только если есть надежда, что retry поможет (например, временная недоступность ETS)

**Не допускать вечных ретраев**:
- JetStream `max_deliver` + перевод в DLQ после исчерпания
- Schema errors → сразу в DLQ (не ретраить)

## Audit: что писать и куда

### Минимальный набор полей в audit‑записи

**Обязательные поля**:
- `event_type`: `"router.intake.validation_failed"`
- `error_code`: один из кодов выше (machine-readable)
- `error_message`: короткий текст (`"schema validation failed: missing run_id"`)
- `subject`: NATS subject входящего сообщения
- `received_at`: timestamp получения сообщения (ISO 8601 или milliseconds)
- `router_node_id`: идентификатор Router узла (node())

**Опциональные поля** (если удалось извлечь):
- `tenant_id`: tenant identifier
- `run_id`: run identifier
- `flow_id`: flow identifier
- `step_id`: step identifier
- `idempotency_key`: idempotency key
- `trace_id`: trace identifier
- `msg_id`: NATS message ID (для JetStream)

**Запрещённые поля**:
- ❌ Полный payload сообщения (может содержать PII/секреты)
- ❌ Секреты (api_key, password, token, и т.п.)
- ❌ Большие binary данные

### Формат audit‑записи

**JSON Log Format** (согласно `OBSERVABILITY_CP1_INVARIANTS.md`):
```json
{
  "timestamp": "2025-01-27T12:00:00.123456Z",
  "level": "ERROR",
  "component": "router",
  "message": "Intake validation failed",
  "event_type": "router.intake.validation_failed",
  "error_code": "SCHEMA_VALIDATION_FAILED",
  "error_message": "Schema validation failed: missing tenant_id",
  "subject": "beamline.router.v1.decide",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "trace_id": "trace_def4567890abcdef1234567890abcdef",
  "received_at": 1706367600123,
  "router_node_id": "router@node1",
  "context": {
    "msg_id": "msg_uuid",
    "payload_size": 1024,
    "validation_stage": "schema"
  }
}
```

**Audit Stream** (опционально, protobuf DTO):
- Subject: `beamline.audit.v1.router.intake`
- Protobuf message: `beamline.audit.v1.RouterIntakeValidationFailed` (нужно создать)

### Реализация audit логирования

**Модуль**: `router_intake_audit.erl` (новый) или расширить `router_audit.erl`

**Функция**:
```erlang
-spec log_intake_validation_failed(
    ErrorCode :: error_code(),
    ErrorMessage :: binary(),
    Subject :: binary(),
    Context :: map()
) -> ok.
log_intake_validation_failed(ErrorCode, ErrorMessage, Subject, Context) ->
    %% Фильтровать PII из контекста
    FilteredContext = router_logger:filter_pii(Context),
    
    %% Построить audit запись
    AuditEntry = #{
        <<"event_type">> => <<"router.intake.validation_failed">>,
        <<"error_code">> => router_intake_error_codes:error_code_to_string(ErrorCode),
        <<"error_message">> => ErrorMessage,
        <<"subject">> => Subject,
        <<"tenant_id">> => maps:get(<<"tenant_id">>, FilteredContext, undefined),
        <<"run_id">> => maps:get(<<"run_id">>, FilteredContext, undefined),
        <<"flow_id">> => maps:get(<<"flow_id">>, FilteredContext, undefined),
        <<"step_id">> => maps:get(<<"step_id">>, FilteredContext, undefined),
        <<"idempotency_key">> => maps:get(<<"idempotency_key">>, FilteredContext, undefined),
        <<"trace_id">> => maps:get(<<"trace_id">>, FilteredContext, undefined),
        <<"received_at">> => erlang:system_time(millisecond),
        <<"router_node_id">> => atom_to_binary(node(), utf8),
        <<"msg_id">> => maps:get(<<"msg_id">>, FilteredContext, undefined)
    },
    
    %% Логировать через router_logger (структурированный JSON)
    router_logger:error(<<"Intake validation failed">>, AuditEntry),
    
    %% Опционально: публиковать в audit stream
    case application:get_env(beamline_router, audit_stream_enabled, false) of
        true ->
            publish_audit_event(AuditEntry);
        false ->
            ok
    end.
```

## Метрики: что считать

### Блок метрик по intake‑валидации

**Счётчики ошибок** (по error_code):
```erlang
%% Метрика: router_intake_validation_errors_total
telemetry:execute(
    [router, intake, validation_errors_total],
    #{count => 1},
    #{
        error_code => <<"SCHEMA_VALIDATION_FAILED">>,
        subject => <<"beamline.router.v1.decide">>,
        tenant_id => <<"tenant_123">>  %% Опционально, если доступен
    }
)
```

**Общий счётчик входящих сообщений**:
```erlang
%% Метрика: router_intake_messages_total
telemetry:execute(
    [router, intake, messages_total],
    #{count => 1},
    #{
        subject => <<"beamline.router.v1.decide">>,
        status => <<"failed">>,  %% или <<"ok">>
        tenant_id => <<"tenant_123">>  %% Опционально
    }
)
```

**DLQ‑события**:
```erlang
%% Метрика: router_intake_dlq_messages_total
telemetry:execute(
    [router, intake, dlq_messages_total],
    #{count => 1},
    #{
        reason => <<"validation_failed">>,
        error_code => <<"SCHEMA_VALIDATION_FAILED">>,
        subject => <<"beamline.router.v1.decide">>
    }
)
```

### Регистрация метрик

**Модуль**: `router_metrics.erl` (расширить)

**Добавить метрики**:
```erlang
%% Intake validation metrics
{router_intake_validation_errors_total, counter},
{router_intake_messages_total, counter},
{router_intake_dlq_messages_total, counter}
```

### Использование метрик

**Быстро увидеть всплеск неверных сообщений**:
- `router_intake_validation_errors_total` по error_code
- `router_intake_messages_total{status="failed"}`

**Отладить проблемы миграции версий схем**:
- `router_intake_validation_errors_total{error_code="VERSION_UNSUPPORTED"}`

**Мониторинг DLQ**:
- `router_intake_dlq_messages_total` по reason и error_code

## Поведение относительно NATS/JetStream

### Fail-fast и детерминированное поведение

**Для schema errors (SCHEMA_VALIDATION_FAILED, VERSION_UNSUPPORTED, CORRELATION_FIELDS_INVALID)**:

1. **Ack + publish to DLQ** (предпочтительный вариант):
   ```erlang
   %% 1. Ack входное сообщение
   router_nats:ack_message(MsgId),
   
   %% 2. Публиковать в DLQ
   DLQSubject = <<"beamline.router.v1.intake.dlq">>,
   DLQMessage = build_dlq_message(Subject, Payload, ErrorCode, ErrorMessage, Context),
   router_nats:publish_with_ack(DLQSubject, jsx:encode(DLQMessage), #{}),
   
   %% 3. Audit + метрики
   router_intake_audit:log_intake_validation_failed(ErrorCode, ErrorMessage, Subject, Context),
   emit_validation_error_metric(ErrorCode, Subject, Context)
   ```

2. **Не ретраить** (schema errors не исправятся при retry)

**Для временных ошибок (INTERNAL_VALIDATION_ERROR, если это временная проблема)**:

1. **Nack + max_deliver policy**:
   ```erlang
   %% 1. Проверить MaxDeliver exhaustion
   case check_maxdeliver_exhaustion(MsgId, Context) of
       {ok, exhausted} ->
           %% MaxDeliver исчерпан → Ack + DLQ
           router_nats:ack_message(MsgId),
           send_to_dlq(Subject, Payload, ErrorCode, ErrorMessage, Context);
       {ok, not_exhausted} ->
           %% Ещё можно ретраить → NAK
           router_nats:nak_message(MsgId),
           emit_redelivery_metric(ErrorCode, Subject, Context)
   end
   ```

2. **Ограниченно использовать** (только для временных ошибок)

### DLQ Message Format

**DLQ Subject**: `beamline.router.v1.intake.dlq` (общий) или специфичный для subject

**DLQ Payload**:
```json
{
  "original_subject": "beamline.router.v1.decide",
  "original_payload_hash": "sha256_hash_of_payload",  %% Хэш вместо полного payload
  "validation_error": {
    "code": "SCHEMA_VALIDATION_FAILED",
    "message": "Schema validation failed: missing tenant_id",
    "field": "tenant_id",
    "severity": "ERROR"
  },
  "context": {
    "subject": "beamline.router.v1.decide",
    "tenant_id": "tenant_123",  %% Если удалось извлечь
    "run_id": "run_abc123",  %% Если удалось извлечь
    "trace_id": "trace_def4567890abcdef1234567890abcdef",  %% Если удалось извлечь
    "received_at": 1706367600123,
    "router_node_id": "router@node1",
    "msg_id": "msg_uuid"
  },
  "payload_preview": "truncated_first_100_chars..."  %% Опционально, усечённый preview
}
```

**Важно**: Не класть полный payload в DLQ, если он может содержать PII/секреты. Использовать хэш или усечённый preview.

## Формирование «ответа» для внешнего мира

### Стандартный error DTO

**Для request-reply паттерна** (decide subject):

**Формат** (в духе `google.rpc.Status`):
```json
{
  "ok": false,
  "error": {
    "code": "SCHEMA_VALIDATION_FAILED",
    "message": "Schema validation failed: missing tenant_id",
    "details": {
      "field": "tenant_id",
      "subject": "beamline.router.v1.decide",
      "severity": "ERROR"
    }
  },
  "context": {
    "request_id": "req_uuid",
    "trace_id": "trace_uuid"  %% Если доступен
  }
}
```

**Отправка**:
- Через reply subject (`beamline.router.v1.decide.reply`)
- Gateway может превратить это в HTTP 4xx/5xx с предсказуемым JSON

**Для pub-sub паттерна** (results/ack subjects):

**Отправка**:
- Публикация в DLQ (см. выше)
- Опционально: публикация в error subject для Gateway (`beamline.gateway.v1.error`)

### Маппинг error codes на HTTP статусы

**Gateway должен маппировать**:
- `SCHEMA_VALIDATION_FAILED` → HTTP 400 Bad Request
- `VERSION_UNSUPPORTED` → HTTP 400 Bad Request
- `CORRELATION_FIELDS_INVALID` → HTTP 400 Bad Request
- `TENANT_FORBIDDEN` → HTTP 403 Forbidden
- `IDEMPOTENCY_VIOLATION` → HTTP 409 Conflict
- `INTERNAL_VALIDATION_ERROR` → HTTP 500 Internal Server Error

## Реализация

### Единый обработчик ошибок

**Модуль**: `router_intake_error_handler.erl` (новый)

**Функции**:
```erlang
%% Основная функция обработки ошибки
-spec handle_intake_error(
    ErrorCode :: error_code(),
    ErrorMessage :: binary(),
    Subject :: binary(),
    Payload :: binary(),
    Headers :: map(),
    MsgId :: binary() | undefined,
    Context :: map()
) -> ok.

%% Отправить в DLQ
-spec send_to_dlq(
    Subject :: binary(),
    Payload :: binary(),
    ErrorCode :: error_code(),
    ErrorMessage :: binary(),
    Context :: map()
) -> ok.

%% Сформировать error response для Gateway
-spec build_error_response(
    ErrorCode :: error_code(),
    ErrorMessage :: binary(),
    Context :: map()
) -> map().

%% Решить судьбу NATS сообщения (ack/nak)
-spec handle_nats_message_fate(
    ErrorCode :: error_code(),
    MsgId :: binary() | undefined,
    Context :: map()
) -> ok.
```

### Интеграция с валидатором

**В `router_intake_validator.erl`**:
```erlang
validate_intake_message(Subject, Payload, Headers, MessageType) ->
    try
        %% Валидация схемы
        case validate_schema(Payload, MessageType) of
            {error, Reason} ->
                router_intake_error_handler:handle_intake_error(
                    schema_validation_failed,
                    <<"Schema validation failed: ", (atom_to_binary(Reason, utf8))/binary>>,
                    Subject, Payload, Headers, undefined,
                    #{validation_stage => <<"schema">>}
                ),
                {error, schema_validation_failed};
            {ok, DecodedMessage} ->
                %% Продолжить валидацию...
        end
    catch
        _:Exception ->
            router_intake_error_handler:handle_intake_error(
                internal_validation_error,
                <<"Internal validation error: ", (erlang:iolist_to_binary(io_lib:format("~p", [Exception])))/binary>>,
                Subject, Payload, Headers, undefined,
                #{exception => Exception}
            ),
            {error, internal_validation_error}
    end.
```

## Критерий «готово» для шага 2.3

### Чек-лист завершения

- [ ] **Есть закрытый перечень кодов ошибок**, и он используется в коде Router'а
- [ ] Любая ошибка валидации:
  - [ ] Не проходит в бизнес‑логику
  - [ ] Порождает audit‑запись с `error_code`
  - [ ] Увеличивает соответствующую метрику
  - [ ] Приводит к корректному `ack/nack` и, при необходимости, DLQ
- [ ] Поведение задокументировано:
  - [ ] В `PROTO_NATS_MAPPING.md` / `NATS_SUBJECTS.md`
  - [ ] Или в отдельном разделе «Error semantics for Router intake»
  - [ ] Понятно Gateway/другим компонентам

### Документация

**Обновить**:
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - добавить раздел "Error Handling"
- `docs/NATS_SUBJECTS.md` - добавить информацию о DLQ subjects и error codes
- `docs/API_CONTRACTS.md` - добавить error response format

**Создать** (если нужно):
- `docs/ROUTER_INTAKE_ERROR_SEMANTICS.md` - детальная спецификация обработки ошибок

## План реализации

### Этап 1: Создание модуля кодов ошибок

1. Создать `router_intake_error_codes.erl`
2. Определить типы и функции для кодов ошибок
3. Реализовать маппинг кодов на строки и severity

### Этап 2: Создание модуля audit логирования

1. Создать `router_intake_audit.erl` или расширить `router_audit.erl`
2. Реализовать `log_intake_validation_failed/4`
3. Интегрировать с `router_logger` для структурированного JSON

### Этап 3: Расширение метрик

1. Расширить `router_metrics.erl` с новыми метриками
2. Реализовать функции для emit метрик
3. Зарегистрировать метрики в telemetry

### Этап 4: Создание обработчика ошибок

1. Создать `router_intake_error_handler.erl`
2. Реализовать обработку ошибок (DLQ, audit, метрики, ack/nak)
3. Реализовать формирование error response

### Этап 5: Интеграция с валидатором

1. Интегрировать `router_intake_error_handler` в `router_intake_validator`
2. Обновить все consumer'ы для использования обработчика ошибок
3. Добавить обработку ошибок на всех уровнях валидации

### Этап 6: Тестирование

1. Unit-тесты для всех модулей
2. Integration-тесты для обработки ошибок
3. Тесты для DLQ публикации
4. Тесты для audit логирования
5. Тесты для метрик

### Этап 7: Документация

1. Обновить существующую документацию
2. Создать спецификацию error semantics (если нужно)
3. Обновить примеры в документации

## Приоритеты

1. **Высокий**: Создание модуля кодов ошибок
2. **Высокий**: Создание обработчика ошибок
3. **Высокий**: Интеграция с валидатором
4. **Средний**: Audit логирование
5. **Средний**: Метрики
6. **Средний**: DLQ поддержка
7. **Низкий**: Документация

## Следующие шаги

1. Создать `router_intake_error_codes.erl`
2. Создать `router_intake_error_handler.erl`
3. Расширить `router_intake_audit.erl` или `router_audit.erl`
4. Расширить метрики в `router_metrics.erl`
5. Интегрировать с `router_intake_validator.erl`
6. Написать тесты

## Ссылки

- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - Observability invariants
- `docs/archive/dev/ROUTER_MESSAGE_INTAKE_VALIDATION_SPEC.md` - Спецификация валидации (Шаг 2.2)
- `docs/archive/dev/ROUTER_MESSAGE_INTAKE_ANALYSIS.md` - Анализ текущего состояния (Шаг 2.1)
- `apps/otp/router/src/router_audit.erl` - Существующий audit модуль
- `apps/otp/router/src/router_result_consumer.erl` - Пример обработки ошибок

