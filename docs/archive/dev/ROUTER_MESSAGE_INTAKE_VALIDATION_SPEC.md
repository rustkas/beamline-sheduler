# Router Message Intake Validation Specification: Шаг 2.2

**Date**: 2025-01-27  
**Status**: 📋 **Specification & Planning**  
**Phase**: Этап 2. Message intake и базовая валидация - Шаг 2.2

## Цель

Создать **единый, жёсткий слой валидации** для всех входящих в Router сообщений, который гарантирует:

- ✅ Сообщение соответствует protobuf-схеме
- ✅ Сообщение принадлежит поддерживаемой версии протокола
- ✅ Сообщение содержит полный набор корреляционных полей
- ✅ Сообщение либо проходит валидацию, либо детерминированно отклоняется с логом и DLQ

## Инвариант после завершения

> **Любое сообщение, которое дошло до бизнес‑логики Router'а, гарантированно:**
> - соответствует protobuf‑схеме,
> - принадлежит поддерживаемой версии протокола,
> - имеет полный набор корреляционных полей (run/flow/step/idempotency/trace),
> - либо будет детерминированно отклонено с логом и помещено в DLQ.

## Текущее состояние

### Decide Subject (`beamline.router.v1.decide`)

**Текущая валидация**:
- ✅ Размер payload (max_payload_size)
- ✅ JSON парсинг (`jsx:decode/2`)
- ✅ Версия (`version: "1"`)
- ⚠️ Обязательные поля (частично - проверяется в `router_core:route/2`)
- ❌ Protobuf decode (нет - используется только JSON)
- ❌ Tenant validation на уровне intake (нет)
- ❌ Idempotency check на уровне intake (нет)
- ❌ Валидация корреляционных полей (run_id, flow_id, step_id) на уровне intake (нет)
- ❌ Валидация trace_id на уровне intake (нет)
- ❌ DLQ при ошибке валидации (нет)

**Файл**: `apps/otp/router/src/router_nats_subscriber.erl`

### Results Subject (`caf.exec.result.v1`)

**Текущая валидация**:
- ✅ JSON парсинг
- ✅ Contract validation (headers)
- ✅ Correlation ID (`assignment_id` или `request_id`)
- ✅ Status validation
- ✅ Tenant validation
- ✅ Idempotency check
- ⚠️ Protobuf decode (нет - используется только JSON)
- ⚠️ Валидация корреляционных полей (run_id, flow_id, step_id) - частично
- ⚠️ Валидация trace_id - частично (опционально)
- ⚠️ DLQ - неявно через MaxDeliver exhaustion

**Файл**: `apps/otp/router/src/router_result_consumer.erl`

### ACK Subject (`caf.exec.assign.v1.ack`)

**Текущая валидация**:
- ✅ JSON парсинг
- ✅ Assignment ID validation
- ✅ Status validation
- ✅ Tenant validation
- ✅ Idempotency check
- ⚠️ Protobuf decode (нет - используется только JSON)
- ⚠️ Валидация корреляционных полей - частично
- ⚠️ DLQ - неявно через MaxDeliver exhaustion

**Файл**: `apps/otp/router/src/router_ack_consumer.erl`

## Требования к валидации

### 1. Валидация схемы (Schema Validation)

#### 1.1. Protobuf DTO как единственный источник правды

**Требование**: Использовать protobuf DTO как единственный источник правды для всех входящих сообщений.

**Для каждого типа входного сообщения**:
- `decide` → `beamline.flow.v1.RouteRequest` (proto: `proto/beamline/flow/v1/flow.proto`)
- `result` → `beamline.worker.v1.ExecResult` (proto: нужно проверить/создать)
- `ack` → `beamline.worker.v1.ExecAssignmentAck` (proto: нужно проверить/создать)

**На intake уровне Router'а**:
1. **Декодировать сообщение строго через protobuf‑кодек**:
   - Если сообщение приходит как JSON → декодировать JSON, затем валидировать через protobuf schema
   - Если сообщение приходит как protobuf binary → декодировать напрямую через protobuf
   - Если декодирование невозможно → ошибка, DLQ, лог

2. **Проверка обязательных полей**:
   - В терминах protobuf/контракта (см. `CORE_MESSAGE_FIELDS_SPECIFICATION.md`)
   - Отсутствие обязательных полей → ошибка, DLQ, лог

3. **При ошибке**:
   - Логировать с минимальным, но достаточным контекстом (subject, tenant, ids)
   - Отправлять в DLQ subject (например, `beamline.router.v1.intake.dlq`)
   - Не пускать дальше по pipeline

**Что считается «завершить»**:
- ✅ Нет ручного JSON‑парсинга/допущений — только protobuf контракты
- ✅ Для каждого входного subject'а описано: «какой именно protobuf message я ожидаю»

#### 1.2. Маппинг Subjects → Protobuf Messages

**Decide Subject**:
- **Subject**: `beamline.router.v1.decide`
- **Protobuf Message**: `beamline.flow.v1.RouteRequest`
- **Proto File**: `proto/beamline/flow/v1/flow.proto`
- **Validation**: Проверить наличие всех обязательных полей согласно `CORE_MESSAGE_FIELDS_SPECIFICATION.md`

**Results Subject**:
- **Subject**: `caf.exec.result.v1`
- **Protobuf Message**: `beamline.worker.v1.ExecResult` (нужно проверить/создать)
- **Proto File**: `proto/beamline/worker/v1/worker.proto` (нужно проверить/создать)
- **Validation**: Проверить наличие всех обязательных полей

**ACK Subject**:
- **Subject**: `caf.exec.assign.v1.ack`
- **Protobuf Message**: `beamline.worker.v1.ExecAssignmentAck` (нужно проверить/создать)
- **Proto File**: `proto/beamline/worker/v1/worker.proto` (нужно проверить/создать)
- **Validation**: Проверить наличие всех обязательных полей

### 2. Валидация версии (Schema / Protocol Version)

#### 2.1. Версия в subject'е

**Текущее состояние**:
- ✅ Subjects версионированы: `beamline.router.v1.decide`, `caf.exec.result.v1`, `caf.exec.assign.v1.ack`
- ✅ Router подписывается только на поддерживаемые версии

**Требование**:
- Router должен явно знать, какую версию протокола он поддерживает
- По имени subject'а Router уже понимает версию схемы
- Если subject содержит неподдерживаемую версию → не подписываться или помечать как `future_version`

**Реализация**:
```erlang
%% В конфигурации Router'а
{supported_schema_versions, [<<"1">>]}  %% Текущая поддерживаемая версия

%% При подписке на subject
case extract_version_from_subject(Subject) of
    <<"1">> -> ok;  %% Поддерживаемая версия
    Version when Version > <<"1">> ->
        %% Future version - помечать для обратной совместимости/миграций
        mark_as_future_version(Subject, Version);
    _ ->
        %% Неподдерживаемая версия - не подписываться
        {error, unsupported_version}
end.
```

#### 2.2. Версия в payload

**Текущее состояние**:
- ✅ Поле `version` проверяется в decide subject
- ⚠️ Поле `version` проверяется частично в results/ack subjects

**Требование**:
- Поле `schema_version` или `version` в protobuf message
- Intake‑слой сверяет `schema_version` с поддерживаемой (например, `== 1`)
- Если `> 1` или `неизвестно`:
  - Либо мягко отклоняет с DLQ и логом
  - Либо обрабатывает только backward‑совместимые поля (а «незнакомые» игнорирует)

**Реализация**:
```erlang
validate_schema_version(Version, SupportedVersions) ->
    case Version of
        undefined ->
            {error, missing_schema_version};
        V when V =:= <<"1">> ->
            ok;  %% Поддерживаемая версия
        V when V > <<"1">> ->
            %% Future version - можно обработать только backward-совместимые поля
            {warning, future_version, V};
        _ ->
            {error, unsupported_version, V}
    end.
```

**Что считается «завершить»**:
- ✅ Для каждого типа входного сообщения однозначно определено, как Router узнаёт версию (subject и/или поле)
- ✅ Реализована логика:
  - «поддерживаемая версия» → продолжаем обработку
  - «неподдерживаемая/будущая версия» → предсказуемое поведение (DLQ/лог/метрика)
- ✅ Документация описывает, какую версию Router принимает и что произойдёт при другой версии

### 3. Валидация корреляционных полей (Correlation Fields)

#### 3.1. Идентификаторы процесса

**Поля**:
- `run_id` — идентификатор выполнения (общий контекст)
- `flow_id` — идентификатор flow/цепочки
- `step_id` — идентификатор конкретного шага (для result/ack)
- (опционально) `parent_step_id` или `root_step_id` — для сложных сценариев

**Валидация на intake**:
1. **Формат**:
   - `run_id`/`flow_id` должны быть **непустыми**, в ожидаемом формате (UUID v4 или ULID)
   - Для `result`/`ack`: проверять, что `step_id` не пустой
   - Валидация формата: UUID v4 regex или ULID format

2. **Обязательность**:
   - **CP1 Baseline**: Все поля опциональны
   - **CP2+ Multi-Step Workflows**: Все поля обязательны
   - Если `run_id` присутствует → `flow_id` и `step_id` должны быть присутствовать
   - Если `flow_id` присутствует → `run_id` должен быть присутствовать
   - Если `step_id` присутствует → `run_id` и `flow_id` должны быть присутствовать

3. **На уровне Router state** (опционально, но желательно):
   - Проверять, что есть ожидающееся состояние для этого `run_id/step_id`
   - Иначе — это либо запоздалый/лишний результат, либо ошибка протокола

**Реализация**:
```erlang
validate_process_identifiers(RunId, FlowId, StepId, RequiredForCP2) ->
    %% Проверка обязательности (CP2+)
    case RequiredForCP2 of
        true ->
            case {RunId, FlowId, StepId} of
                {undefined, _, _} -> {error, missing_run_id};
                {_, undefined, _} -> {error, missing_flow_id};
                {_, _, undefined} -> {error, missing_step_id};
                _ -> ok
            end;
        false ->
            ok  %% CP1 baseline - все опциональны
    end,
    %% Проверка зависимостей
    case {RunId, FlowId, StepId} of
        {R, undefined, _} when R =/= undefined -> {error, flow_id_required_when_run_id_present};
        {R, _, undefined} when R =/= undefined -> {error, step_id_required_when_run_id_present};
        {undefined, F, _} when F =/= undefined -> {error, run_id_required_when_flow_id_present};
        {undefined, _, S} when S =/= undefined -> {error, run_id_required_when_step_id_present};
        {_, undefined, S} when S =/= undefined -> {error, flow_id_required_when_step_id_present};
        _ -> ok
    end,
    %% Проверка формата (если присутствуют)
    validate_uuid_or_ulid(RunId),
    validate_uuid_or_ulid(FlowId),
    validate_uuid_or_ulid(StepId).
```

#### 3.2. Идемпотентность

**Поле**: `idempotency_key` или аналог

**Валидация на intake**:
1. **Присутствие и формат**:
   - Проверять, что ключ присутствует (CP2+ обязателен, CP1 опционален)
   - Проверять формат/длину (non-empty string, max 256 chars)

2. **Idempotency check**:
   - Сверяться с ETS/хранилищем идемпотентности
   - Если ключ уже обработан → либо сразу ack и лог, без повторной работы
   - Если новый → регистрировать перед передачей дальше

**Реализация**:
```erlang
validate_idempotency(IdempotencyKey, RequiredForCP2, RequestContext) ->
    %% Проверка обязательности
    case {IdempotencyKey, RequiredForCP2} of
        {undefined, true} -> {error, missing_idempotency_key};
        {undefined, false} -> ok;  %% CP1 baseline - опционально
        {Key, _} ->
            %% Проверка формата
            case validate_idempotency_key_format(Key) of
                {error, Reason} -> {error, {invalid_idempotency_key_format, Reason}};
                ok ->
                    %% Проверка идемпотентности
                    case router_idempotency:check_and_mark(<<"intake_id">>, Key, RequestContext) of
                        {ok, seen} -> {ok, duplicate};
                        {ok, not_seen} -> {ok, new};
                        {error, Reason} -> {error, {idempotency_check_failed, Reason}}
                    end
            end
    end.
```

#### 3.3. Трассировка / Observability

**Поля**:
- `trace_id` (обязателен для CP2+)
- `span_id` (может быть опциональным на этом уровне)
- возможно `parent_span_id`

**Валидация на intake**:
1. **Присутствие**:
   - `trace_id` обязателен для CP2+ (опционален для CP1)
   - При отсутствии:
     - Либо генерировать новый trace_id (но логировать, что поле не было передано)
     - Либо отклонять сообщение, если по инвариантам проекта **trace обязателен**

2. **Формат**:
   - W3C Trace Context format (16 hex chars) или UUID v4
   - Валидация формата: W3C Trace Context regex или UUID v4 format

**Реализация**:
```erlang
validate_trace_fields(TraceId, SpanId, RequiredForCP2) ->
    %% Проверка обязательности trace_id (CP2+)
    case {TraceId, RequiredForCP2} of
        {undefined, true} ->
            %% CP2+ требует trace_id - либо генерировать, либо отклонять
            case application:get_env(beamline_router, generate_trace_id_if_missing, false) of
                true ->
                    NewTraceId = generate_trace_id(),
                    router_logger:warning(<<"Generated trace_id (missing in request)">>, #{
                        <<"generated_trace_id">> => NewTraceId
                    }),
                    {ok, NewTraceId, SpanId};
                false ->
                    {error, missing_trace_id}
            end;
        {undefined, false} ->
            ok;  %% CP1 baseline - опционально
        {T, _} ->
            %% Проверка формата
            case validate_trace_id_format(T) of
                {error, Reason} -> {error, {invalid_trace_id_format, Reason}};
                ok -> {ok, T, SpanId}
            end
    end.
```

**Что считается «завершить»**:
- ✅ Для каждого protobuf DTO, приходящего на вход Router'у, явно определён набор полей: `run_id`, `flow_id`, `step_id` (где нужно), `idempotency_key`, `trace_id`
- ✅ В intake‑коде Router'а реализованы проверки: «если поле отсутствует/пустое → ошибка, DLQ/лог, счётчик метрики»
- ✅ Есть единый helper/модуль валидации, а не разрозненные `if` по коду

## Архитектура валидации

### Единый модуль валидации

**Модуль**: `router_intake_validator.erl`

**Функции**:
```erlang
%% Основная функция валидации
-spec validate_intake_message(
    Subject :: binary(),
    Payload :: binary(),
    Headers :: map(),
    MessageType :: decide | result | ack
) -> {ok, ValidatedMessage :: map()} | {error, Reason :: atom(), Context :: map()}.

%% Валидация схемы (protobuf)
-spec validate_schema(
    Payload :: binary(),
    MessageType :: decide | result | ack
) -> {ok, DecodedMessage :: map()} | {error, Reason :: atom()}.

%% Валидация версии
-spec validate_version(
    Version :: binary() | undefined,
    Subject :: binary()
) -> ok | {error, Reason :: atom()} | {warning, future_version, binary()}.

%% Валидация корреляционных полей
-spec validate_correlation_fields(
    Message :: map(),
    RequiredForCP2 :: boolean()
) -> {ok, ValidatedFields :: map()} | {error, Reason :: atom(), Field :: binary()}.

%% Валидация идентификаторов процесса
-spec validate_process_identifiers(
    RunId :: binary() | undefined,
    FlowId :: binary() | undefined,
    StepId :: binary() | undefined,
    RequiredForCP2 :: boolean()
) -> ok | {error, Reason :: atom()}.

%% Валидация идемпотентности
-spec validate_idempotency(
    IdempotencyKey :: binary() | undefined,
    RequiredForCP2 :: boolean(),
    RequestContext :: map()
) -> {ok, Status :: new | duplicate} | {error, Reason :: atom()}.

%% Валидация трассировки
-spec validate_trace_fields(
    TraceId :: binary() | undefined,
    SpanId :: binary() | undefined,
    RequiredForCP2 :: boolean()
) -> {ok, TraceId :: binary(), SpanId :: binary() | undefined} | {error, Reason :: atom()}.
```

### Интеграция с существующими consumer'ами

**Decide Consumer** (`router_decide_consumer.erl` - новый модуль):
```erlang
handle_info({nats_message, Subject, Payload, Headers, MsgId}, State) ->
    case router_intake_validator:validate_intake_message(Subject, Payload, Headers, decide) of
        {ok, ValidatedMessage} ->
            %% Передать дальше по pipeline
            process_validated_decide_request(Subject, ValidatedMessage, MsgId);
        {error, Reason, Context} ->
            %% Отправить в DLQ и залогировать
            send_to_dlq(Subject, Payload, Reason, Context),
            log_validation_error(Subject, Reason, Context),
            emit_validation_error_metric(Subject, Reason, Context),
            %% NAK message (если JetStream)
            case MsgId of
                undefined -> ok;
                _ -> router_nats:nak_message(MsgId)
            end
    end,
    {noreply, State}.
```

**Results Consumer** (`router_result_consumer.erl`):
```erlang
handle_info({nats_message, Subject, Payload, Headers, MsgId}, State) ->
    case router_intake_validator:validate_intake_message(Subject, Payload, Headers, result) of
        {ok, ValidatedMessage} ->
            %% Существующая логика обработки
            process_exec_result(ValidatedMessage, Headers, MsgId);
        {error, Reason, Context} ->
            %% Отправить в DLQ и залогировать
            send_to_dlq(Subject, Payload, Reason, Context),
            log_validation_error(Subject, Reason, Context),
            emit_validation_error_metric(Subject, Reason, Context),
            %% NAK message
            case MsgId of
                undefined -> ok;
                _ -> router_nats:nak_message(MsgId)
            end
    end,
    {noreply, State}.
```

**ACK Consumer** (`router_ack_consumer.erl`):
```erlang
handle_info({nats_message, Subject, Payload, Headers, MsgId}, State) ->
    case router_intake_validator:validate_intake_message(Subject, Payload, Headers, ack) of
        {ok, ValidatedMessage} ->
            %% Существующая логика обработки
            process_ack(ValidatedMessage, Headers, MsgId);
        {error, Reason, Context} ->
            %% Отправить в DLQ и залогировать
            send_to_dlq(Subject, Payload, Reason, Context),
            log_validation_error(Subject, Reason, Context),
            emit_validation_error_metric(Subject, Reason, Context),
            %% NAK message
            case MsgId of
                undefined -> ok;
                _ -> router_nats:nak_message(MsgId)
            end
    end,
    {noreply, State}.
```

## DLQ (Dead Letter Queue)

### DLQ Subjects

**Требование**: Все ошибки валидации должны попадать в DLQ.

**DLQ Subjects**:
- `beamline.router.v1.intake.dlq` - общий DLQ для всех ошибок валидации intake
- `beamline.router.v1.decide.dlq` - DLQ для ошибок decide subject (опционально, можно использовать общий)
- `caf.exec.result.dlq` - DLQ для ошибок result subject (опционально)
- `caf.exec.assign.ack.dlq` - DLQ для ошибок ack subject (опционально)

**DLQ Message Format**:
```json
{
  "original_subject": "beamline.router.v1.decide",
  "original_payload": {...},
  "validation_error": {
    "code": "missing_tenant_id",
    "message": "Required field tenant_id is missing",
    "field": "tenant_id"
  },
  "context": {
    "subject": "beamline.router.v1.decide",
    "timestamp": "2025-01-27T12:00:00Z",
    "msg_id": "uuid"
  }
}
```

**Реализация**:
```erlang
send_to_dlq(Subject, Payload, Reason, Context) ->
    DLQSubject = get_dlq_subject(Subject),
    DLQMessage = #{
        <<"original_subject">> => Subject,
        <<"original_payload">> => Payload,
        <<"validation_error">> => #{
            <<"code">> => atom_to_binary(Reason, utf8),
            <<"message">> => get_error_message(Reason),
            <<"field">> => maps:get(<<"field">>, Context, undefined)
        },
        <<"context">> => Context
    },
    DLQJson = jsx:encode(DLQMessage),
    router_nats:publish_with_ack(DLQSubject, DLQJson, #{}).
```

## План реализации

### Этап 1: Создание единого модуля валидации

1. Создать `router_intake_validator.erl`
2. Реализовать функции валидации:
   - `validate_schema/2` - protobuf decode и валидация
   - `validate_version/2` - валидация версии
   - `validate_correlation_fields/2` - валидация корреляционных полей
   - `validate_process_identifiers/4` - валидация run_id/flow_id/step_id
   - `validate_idempotency/3` - валидация идемпотентности
   - `validate_trace_fields/3` - валидация трассировки
3. Добавить helper функции:
   - `validate_uuid_or_ulid/1` - валидация формата UUID/ULID
   - `validate_trace_id_format/1` - валидация формата trace_id
   - `validate_idempotency_key_format/1` - валидация формата idempotency_key

### Этап 2: Интеграция с decide consumer

1. Создать `router_decide_consumer.erl` (миграция с `router_nats_subscriber.erl`)
2. Интегрировать `router_intake_validator` в decide consumer
3. Добавить DLQ поддержку
4. Обновить `beamline_router_sup.erl`

### Этап 3: Интеграция с results consumer

1. Интегрировать `router_intake_validator` в `router_result_consumer.erl`
2. Добавить DLQ поддержку
3. Улучшить валидацию корреляционных полей

### Этап 4: Интеграция с ack consumer

1. Интегрировать `router_intake_validator` в `router_ack_consumer.erl`
2. Добавить DLQ поддержку
3. Улучшить валидацию корреляционных полей

### Этап 5: Тестирование

1. Unit-тесты для `router_intake_validator`
2. Integration-тесты для всех consumer'ов
3. Тесты для DLQ
4. Тесты для всех типов ошибок валидации

### Этап 6: Документация

1. Обновить `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
2. Обновить `docs/NATS_SUBJECTS.md`
3. Обновить `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` (если нужно)
4. Создать ADR для валидации (если нужно)

## Чек-лист выполнения

### 1. Валидация схемы

- [ ] Protobuf decode для decide subject
- [ ] Protobuf decode для result subject
- [ ] Protobuf decode для ack subject
- [ ] Валидация обязательных полей через protobuf
- [ ] Нет ручного JSON-парсинга/допущений

### 2. Валидация версии

- [ ] Версия в subject'е (явно определена поддерживаемая версия)
- [ ] Версия в payload (валидация schema_version/version)
- [ ] Обработка неподдерживаемых версий (DLQ/лог/метрика)
- [ ] Обработка будущих версий (backward compatibility)
- [ ] Документация версионирования

### 3. Валидация корреляционных полей

- [ ] Валидация run_id/flow_id/step_id (формат, обязательность, зависимости)
- [ ] Валидация idempotency_key (формат, обязательность, проверка идемпотентности)
- [ ] Валидация trace_id (формат, обязательность, генерация при отсутствии)
- [ ] Единый модуль валидации (не разрозненные if)

### 4. DLQ

- [ ] DLQ subjects определены
- [ ] DLQ message format определен
- [ ] Публикация в DLQ при ошибках валидации
- [ ] Логирование ошибок валидации
- [ ] Метрики для ошибок валидации

### 5. Интеграция

- [ ] Интеграция с decide consumer
- [ ] Интеграция с results consumer
- [ ] Интеграция с ack consumer
- [ ] Все тесты проходят

### 6. Документация

- [ ] Обновлена документация
- [ ] Создан ADR (если нужен)

## Приоритеты

1. **Высокий**: Создание единого модуля валидации
2. **Высокий**: Интеграция с decide consumer (критично для прод-уровня)
3. **Средний**: Интеграция с results/ack consumer
4. **Средний**: DLQ поддержка
5. **Низкий**: Документация

## Следующие шаги

1. Создать `router_intake_validator.erl` с базовой структурой
2. Реализовать валидацию схемы (protobuf decode)
3. Реализовать валидацию версии
4. Реализовать валидацию корреляционных полей
5. Интегрировать с decide consumer
6. Добавить DLQ поддержку
7. Написать тесты

## Ссылки

- `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` - Спецификация core fields
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto to NATS mapping
- `docs/NATS_SUBJECTS.md` - NATS subjects documentation
- `docs/archive/dev/ROUTER_MESSAGE_INTAKE_ANALYSIS.md` - Анализ текущего состояния (Шаг 2.1)

