# Router Message Intake Analysis: Этап 2.1

**Date**: 2025-01-27  
**Status**: 🔄 **Analysis & Planning**  
**Phase**: Этап 2. Message intake и базовая валидация

## Цель

Формализовать и довести до прод‑уровня входной поток сообщений Router'а:
- Какие subjects используются
- Какая схема сообщений
- Как Router подписывается
- Что валидируется на входе

## Текущее состояние

### 1. Аудит текущих subjects

#### 1.1. Subject `beamline.router.v1.decide` (DecideRequest)

**Текущая реализация**:
- **Файл**: `apps/otp/router/src/router_nats_subscriber.erl`
- **Подписка**: Обычная NATS подписка (`router_nats:subscribe/3`), **НЕ JetStream**
- **Subject**: `beamline.router.v1.decide` (константа `?SUBJECT`)
- **Супервизор**: Запускается в `beamline_router_sup.erl` (базовый компонент)

**Проблемы**:
- ❌ **Нет JetStream durable consumer** - сообщения теряются при перезапуске Router'а
- ❌ **Нет queue group** - нет горизонтального масштабирования
- ❌ **Нет explicit ack policy** - нет гарантии обработки
- ❌ **Нет max_deliver/backoff** - нет защиты от зацикливания
- ❌ **Нет DLQ** - ошибки не попадают в dead-letter queue

**Что работает**:
- ✅ Версионированный subject (`beamline.router.v1.decide`)
- ✅ Базовая валидация версии (`version: "1"`)
- ✅ Парсинг JSON payload
- ✅ Валидация размера payload (max_payload_size)
- ✅ Обработка DecideRequest → RouteDecision
- ✅ Reply subject (`beamline.router.v1.decide.reply`)

#### 1.2. Subject `caf.exec.result.v1` (ExecResult)

**Текущая реализация**:
- **Файл**: `apps/otp/router/src/router_result_consumer.erl`
- **Подписка**: ✅ **JetStream durable consumer**
- **Subject**: `caf.exec.result.v1` (конфигурируемый, default: `?DEFAULT_RESULT_SUBJECT`)
- **Durable Group**: `router-results` (конфигурируемый, default: `?DEFAULT_JS_DURABLE_GROUP`)
- **Ack Policy**: `explicit`
- **Deliver Group**: `router-results-group` (конфигурируемый, для горизонтального масштабирования)
- **Mode**: `push`
- **Супервизор**: Запускается в `beamline_router_sup.erl` (базовый компонент)

**Что работает**:
- ✅ JetStream durable consumer
- ✅ Queue group для горизонтального масштабирования
- ✅ Explicit ack policy
- ✅ MaxDeliver exhaustion detection (ETS tracking)
- ✅ Базовая валидация (correlation_id, status)
- ✅ Tenant validation
- ✅ Idempotency check
- ✅ Contract validation (headers)
- ✅ Usage event emission

**Что нужно проверить**:
- ⚠️ DLQ subject (`beamline.router.v1.result.dlq` или `caf.exec.dlq.v1`)
- ⚠️ Backoff configuration (есть в коде, но нужно проверить значения)

#### 1.3. Subject `caf.exec.assign.v1.ack` (ExecAssignmentAck)

**Текущая реализация**:
- **Файл**: `apps/otp/router/src/router_ack_consumer.erl`
- **Подписка**: ✅ **JetStream durable consumer** (опционально)
- **Subject**: `caf.exec.assign.v1.ack` (конфигурируемый, default: `?DEFAULT_ACK_SUBJECT`)
- **Durable Group**: `router-acks` (конфигурируемый, default: `?DEFAULT_JS_DURABLE_GROUP`)
- **Ack Policy**: `explicit`
- **Deliver Group**: `router-acks-group` (конфигурируемый)
- **Mode**: `push`
- **Супервизор**: Запускается условно (только если `ack_enabled = true` и `cp2_plus_allowed = true`)

**Что работает**:
- ✅ JetStream durable consumer
- ✅ Queue group для горизонтального масштабирования
- ✅ Explicit ack policy
- ✅ MaxDeliver exhaustion detection (ETS tracking)
- ✅ Базовая валидация (assignment_id, status)
- ✅ Tenant validation
- ✅ Idempotency check

**Что нужно проверить**:
- ⚠️ DLQ subject (если нужен для ack)
- ⚠️ Backoff configuration

### 2. Нормализация contracts

#### 2.1. Proto-файлы

**Проверка**:
- ✅ `proto/beamline/flow/v1/flow.proto` - содержит `RouteRequest`, `RouteDecision`
- ✅ `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - документирует mapping
- ⚠️ Нужно проверить, что все DTO соответствуют документации

#### 2.2. Версионирование subjects

**Текущее состояние**:
- ✅ `beamline.router.v1.decide` - версионирован
- ✅ `caf.exec.result.v1` - версионирован
- ✅ `caf.exec.assign.v1.ack` - версионирован

**Проверка**:
- ⚠️ Нужно убедиться, что все subjects документированы в `docs/NATS_SUBJECTS.md`
- ⚠️ Нужно проверить соответствие с `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`

### 3. JetStream-конфигурация

#### 3.1. Decide subject (требует доработки)

**Текущее состояние**: Обычная подписка, **НЕ JetStream**

**Требуется**:
- ❌ Durable name: `router-decide-consumer` (новый)
- ❌ Queue group: `router-decide-group` (новый)
- ❌ Ack policy: `explicit` (новый)
- ❌ Max deliver: из конфига `nats_js_max_deliver` (default: 3)
- ❌ Backoff: из конфига `nats_js_backoff_seconds` (default: [1, 2, 4])
- ❌ DLQ subject: `beamline.router.v1.decide.dlq` (новый)

#### 3.2. Results subject (работает)

**Текущее состояние**: ✅ JetStream durable consumer

**Конфигурация**:
- ✅ Durable name: `router-results` (из `nats_js_durable_group_results`)
- ✅ Queue group: `router-results-group` (из `nats_js_deliver_group_results`)
- ✅ Ack policy: `explicit`
- ✅ Max deliver: из `nats_js_max_deliver` (default: 3)
- ✅ Backoff: из `nats_js_backoff_seconds` (default: [1, 2, 4])
- ⚠️ DLQ: нужно проверить, есть ли DLQ subject

#### 3.3. ACK subject (работает)

**Текущее состояние**: ✅ JetStream durable consumer (опционально)

**Конфигурация**:
- ✅ Durable name: `router-acks` (из `nats_js_durable_group_acks`)
- ✅ Queue group: `router-acks-group` (из `nats_js_deliver_group_acks`)
- ✅ Ack policy: `explicit`
- ✅ Max deliver: из `nats_js_max_deliver` (default: 3)
- ✅ Backoff: из `nats_js_backoff_seconds` (default: [1, 2, 4])
- ⚠️ DLQ: нужно проверить, нужен ли DLQ для ack

### 4. Базовая валидация на входе

#### 4.1. Decide subject

**Текущая валидация**:
- ✅ Размер payload (max_payload_size)
- ✅ Парсинг JSON
- ✅ Версия (`version: "1"`)
- ⚠️ Обязательные поля (частично - проверяется в `router_core:route/2`)
- ❌ Tenant validation (нет на уровне intake)
- ❌ Idempotency check (нет на уровне intake)
- ❌ Schema version validation (нет проверки `schema_version`)

**Требуется**:
- ❌ Decode protobuf (сейчас только JSON)
- ❌ Проверка обязательных полей (`tenant_id`, `message`, `message.message_type`, `message.payload`)
- ❌ Tenant validation (ACL)
- ❌ Idempotency check (`idempotency_key`)
- ❌ Schema version validation (`schema_version` или `version`)
- ❌ DLQ при ошибке валидации

#### 4.2. Results subject

**Текущая валидация**:
- ✅ Парсинг JSON
- ✅ Contract validation (headers)
- ✅ Correlation ID (`assignment_id` или `request_id`)
- ✅ Status validation (`success`, `error`, `timeout`, `cancelled`)
- ✅ Tenant validation
- ✅ Idempotency check
- ✅ MaxDeliver exhaustion detection

**Что работает хорошо**:
- ✅ Полная валидация на входе
- ✅ DLQ через MaxDeliver exhaustion (неявно)

#### 4.3. ACK subject

**Текущая валидация**:
- ✅ Парсинг JSON
- ✅ Assignment ID validation
- ✅ Status validation (`accepted`, `rejected`, `error`)
- ✅ Tenant validation
- ✅ Idempotency check
- ✅ MaxDeliver exhaustion detection

**Что работает хорошо**:
- ✅ Полная валидация на входе

## План действий

### Шаг 1: Миграция decide subject на JetStream

**Задача**: Перевести `beamline.router.v1.decide` с обычной подписки на JetStream durable consumer.

**Действия**:
1. Создать новый модуль `router_decide_consumer.erl` (аналог `router_result_consumer.erl`)
2. Использовать `router_nats:subscribe_jetstream/5` вместо `router_nats:subscribe/3`
3. Настроить durable name: `router-decide-consumer`
4. Настроить queue group: `router-decide-group`
5. Настроить ack policy: `explicit`
6. Добавить обработку `msg_id` для ACK/NAK
7. Добавить MaxDeliver exhaustion detection
8. Обновить `beamline_router_sup.erl` для запуска нового consumer'а
9. Удалить или пометить как deprecated `router_nats_subscriber.erl`

**Конфигурация**:
```erlang
%% Новые параметры конфигурации
{nats_js_durable_group_decide, <<"router-decide-consumer">>}
{nats_js_deliver_group_decide, <<"router-decide-group">>}
{decide_subject, <<"beamline.router.v1.decide">>}
{decide_dlq_subject, <<"beamline.router.v1.decide.dlq">>}
```

### Шаг 2: Улучшение валидации decide subject

**Задача**: Добавить полную валидацию на входе для decide messages.

**Действия**:
1. Добавить decode protobuf (если используется protobuf, а не только JSON)
2. Добавить проверку обязательных полей:
   - `tenant_id` (required)
   - `message` (required)
   - `message.message_type` (required)
   - `message.payload` (required)
3. Добавить tenant validation (ACL) на уровне intake
4. Добавить idempotency check (`idempotency_key`) на уровне intake
5. Добавить schema version validation
6. Добавить отправку в DLQ при ошибке валидации
7. Добавить логирование и метрики для ошибок валидации

**Валидация**:
```erlang
validate_decide_request(Request) ->
    %% 1. Обязательные поля
    case check_required_fields(Request) of
        {error, Reason} -> {error, Reason};
        ok -> ok
    end,
    %% 2. Tenant validation
    TenantId = maps:get(<<"tenant_id">>, Request),
    case router_tenant_validator:validate_tenant(TenantId, #{source => <<"DecideRequest">>}) of
        {ok, _} -> ok;
        {error, Reason, Context} -> {error, {tenant_validation_failed, Reason, Context}}
    end,
    %% 3. Idempotency check
    IdempotencyKey = maps:get(<<"idempotency_key">>, Request, undefined),
    case router_idempotency:check_and_mark(<<"decide_id">>, IdempotencyKey, Request) of
        {ok, seen} -> {error, duplicate_request};
        {ok, not_seen} -> ok;
        {error, Reason} -> {error, {idempotency_failed, Reason}}
    end,
    %% 4. Schema version
    SchemaVersion = maps:get(<<"schema_version">>, Request, maps:get(<<"version">>, Request)),
    case validate_schema_version(SchemaVersion) of
        ok -> ok;
        {error, Reason} -> {error, {invalid_schema_version, Reason}}
    end.
```

### Шаг 3: Проверка и доработка DLQ

**Задача**: Убедиться, что все ошибки попадают в DLQ.

**Действия**:
1. Проверить, есть ли DLQ subject для decide (`beamline.router.v1.decide.dlq`)
2. Проверить, есть ли DLQ subject для results (`caf.exec.dlq.v1` или `beamline.router.v1.result.dlq`)
3. Добавить публикацию в DLQ при ошибках валидации
4. Добавить публикацию в DLQ при MaxDeliver exhaustion
5. Документировать DLQ subjects в `docs/NATS_SUBJECTS.md`

### Шаг 4: Обновление документации

**Задача**: Обновить документацию с учетом изменений.

**Действия**:
1. Обновить `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`:
   - Добавить информацию о JetStream для decide subject
   - Обновить информацию о валидации
   - Добавить информацию о DLQ
2. Обновить `docs/NATS_SUBJECTS.md`:
   - Добавить информацию о JetStream конфигурации для decide
   - Добавить информацию о DLQ subjects
   - Обновить информацию о валидации
3. Обновить `apps/otp/router/docs/CONFIG.md`:
   - Добавить новые параметры конфигурации для decide consumer
   - Обновить описание валидации

### Шаг 5: Тестирование

**Задача**: Убедиться, что все работает корректно.

**Действия**:
1. Создать unit-тесты для новой валидации
2. Создать integration-тесты для JetStream decide consumer
3. Создать тесты для DLQ
4. Создать тесты для MaxDeliver exhaustion
5. Обновить существующие тесты

## Чек-лист выполнения

### 1. Аудит текущих subjects ✅

- [x] Найти все подписки Router'а
- [x] Сопоставить с документацией
- [x] Выявить проблемы

### 2. Нормализация contracts ⚠️

- [ ] Проверить версионирование subjects
- [ ] Проверить DTO в proto-файлах
- [ ] Проверить соответствие документации

### 3. JetStream-конфигурация ⚠️

- [ ] Мигрировать decide subject на JetStream
- [ ] Проверить конфигурацию results subject
- [ ] Проверить конфигурацию ack subject
- [ ] Добавить DLQ subjects

### 4. Базовая валидация на входе ⚠️

- [ ] Улучшить валидацию decide subject
- [ ] Проверить валидацию results subject
- [ ] Проверить валидацию ack subject
- [ ] Добавить DLQ при ошибках

### 5. Документация ⚠️

- [ ] Обновить `PROTO_NATS_MAPPING.md`
- [ ] Обновить `NATS_SUBJECTS.md`
- [ ] Обновить `CONFIG.md`
- [ ] Создать ADR (если нужен)

## Приоритеты

1. **Высокий**: Миграция decide subject на JetStream (критично для прод-уровня)
2. **Высокий**: Улучшение валидации decide subject (безопасность)
3. **Средний**: Проверка и доработка DLQ (надежность)
4. **Средний**: Обновление документации (поддерживаемость)
5. **Низкий**: Тестирование (качество)

## Следующие шаги

1. Создать `router_decide_consumer.erl` с JetStream подпиской
2. Добавить полную валидацию на входе
3. Добавить DLQ поддержку
4. Обновить документацию
5. Написать тесты

## Ссылки

- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto to NATS mapping
- `docs/NATS_SUBJECTS.md` - NATS subjects documentation
- `apps/otp/router/docs/CONFIG.md` - Router configuration
- `apps/otp/router/src/router_result_consumer.erl` - Reference implementation для JetStream consumer
- `apps/otp/router/src/router_ack_consumer.erl` - Reference implementation для ACK consumer

