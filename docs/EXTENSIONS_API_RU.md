# Extensions API

> **Коротко:** Расширение — это отдельный сервис, который говорит с BeamLine **только через NATS по фиксированному контракту**, а включение/выключение расширения делается **только конфигом/политикой**, без изменений кода Gateway, Router или CAF‑workers.

---

## 0. Определение

**Extension** — это *отдельный NATS‑сервис*, который:

- имеет собственный NATS subject (например, `beamline.ext.pre.normalize_text.v1`);
- принимает и возвращает сообщения по фиксированному контракту (CP‑Ext или CP2);
- регистрируется в **Extension Registry** по логическому `extension_id`;
- подключается к пайплайну через **Routing Policy**, без изменений кода ядра.

Gateway, Router и CAF‑workers остаются неизменными при добавлении новых расширений.

---

## 1. Типы расширений

Поддерживаются четыре типа расширений:

1. **Pre‑processor** — меняет/обогащает входящее сообщение *до* маршрутизации.
2. **Validator** — решает, можно ли продолжать обработку (accept / reject).
3. **Post‑processor** — меняет ответ провайдера *после* маршрутизации.
4. **Custom provider** — выступает как ещё один провайдер (LLM, RAG, CRM и т.п.).

Router знает про расширение только:

- логический `extension_id` (например, `normalize_text`);
- его тип (`pre` / `validator` / `post` / `provider`);
- NATS subject, timeout и retry (из Extension Registry);
- как трактовать результат в своём пайплайне.

---

## 2. Общий контракт CP‑Ext (pre / validator / post)

### 2.1. NATS subjects

Каждое расширение подписывается на **свой** NATS subject, например:

- `beamline.ext.pre.normalize_text.v1`
- `beamline.ext.validate.pii_guard.v1`
- `beamline.ext.post.mask_pii.v1`

Версионирование обязательно (`.v1`, `.v2`, …).

### 2.2. CP‑Ext Request (JSON over NATS)

```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "extensions": {
    "id": "normalize_text",
    "config": {
      "lowercase": true,
      "strip_emojis": true
    }
  },
  "message": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Original text",
    "metadata": {
      "channel": "telegram"
    }
  },
  "context": {
    "lang": "en"
  }
}
```

Поля:

- `trace_id` — сквозной trace для логов/трассировки.
- `tenant_id` — текущий tenant.
- `extensions.id` — логический ID расширения (совпадает с ключом в Registry).
- `extensions.config` — конфигурация для конкретного вызова (политика/tenant‑override).
- `message` — доменное сообщение, которое нужно трансформировать/валидировать.
- `context` — произвольный контекст (язык, канал, customer_id и т.п.).

### 2.3. CP‑Ext Response (pre / post)

```json
{
  "message": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "normalized text",
    "metadata": {
      "channel": "telegram",
      "normalized": "true"
    }
  },
  "context": {
    "lang": "en",
    "detected_lang": "en"
  }
}
```

Требования:

- `message` и `context` обязательны.
- Расширение может:
  - изменять `payload`;
  - добавлять/изменять `metadata`;
  - дополнять `context`.

Router подставляет обновлённые `message` и `context` в текущий пайплайн и продолжает выполнение следующего шага политики.

---

## 3. Validator‑расширения

Validator вызывается **после pre‑обработки и/или перед выбором провайдера**.

### 3.1. Request

Request — тот же **CP‑Ext Request**, что у pre‑processor.

### 3.2. Response

**Успех:**

```json
{
  "status": "ok"
}
```

**Блокировка:**

```json
{
  "status": "reject",
  "reason": "pii_detected",
  "details": {
    "field": "payload",
    "pattern": "credit_card"
  }
}
```

Семантика:

- `status = "ok"` — Router идёт дальше по пайплайну.
- `status = "reject"` — поведение определяется политикой:
  - `on_fail = "block"` — Router останавливает запрос и возвращает клиенту ошибку/специальный ответ;
  - `on_fail = "warn"` — Router логирует событие, но не блокирует;
  - `on_fail = "ignore"` — Router игнорирует результат и продолжает.

При таймауте или ошибке сети Router трактует результат как `reject` и применяет `on_fail`.

---

## 4. Post‑processor‑расширения

Post‑processor вызывается **после ответа от провайдера**, перед возвратом клиенту.

### 4.1. Request

```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "extensions": {
    "id": "mask_pii",
    "config": {
      "mask_email": true,
      "mask_phone": true
    }
  },
  "message": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Model reply text",
    "metadata": {
      "provider_id": "openai:gpt-4.1-mini"
    }
  },
  "context": {
    "lang": "en"
  }
}
```

### 4.2. Response

```json
{
  "message": {
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "Reply with [masked] PII",
    "metadata": {
      "provider_id": "openai:gpt-4.1-mini",
      "pii_masked": "true"
    }
  },
  "context": {
    "lang": "en"
  }
}
```

Router подставляет результат и возвращает его клиенту. Поведение при ошибках определяется `mode` в политике (`required` или `optional`).

---

## 5. Custom provider extensions

Custom provider — расширение‑провайдер, которое выглядит как обычный провайдер CP2, но реализовано как отдельный NATS‑сервис.

### 5.1. NATS subject

Пример subject:

- `beamline.provider.my_crm_summarizer.v1`

### 5.2. Provider Request (CP2‑style, JSON over NATS)

Упрощённый пример:

```json
{
  "trace_id": "uuid",
  "tenant_id": "tenant-123",
  "provider_id": "my_crm_summarizer",
  "prompt": "User message + context",
  "parameters": {
    "max_tokens": 512
  },
  "context": {
    "conversation_id": "conv-1",
    "customer_id": "cust-42"
  }
}
```

### 5.3. Provider Response

```json
{
  "provider_id": "my_crm_summarizer",
  "output": "Summarized answer...",
  "usage": {
    "prompt_tokens": 120,
    "completion_tokens": 80
  },
  "metadata": {
    "source": "crm"
  }
}
```

Router и CAF‑воркеры рассматривают такой провайдер наравне с другими (OpenAI, Anthropic и т.п.), выбор делается политикой по `provider_id`.

---

## 6. Extension Registry

**Extension Registry** — источник правды о расширениях для Router. Может храниться в БД или как конфиг, с кешем в Mnesia/ETS.

### 6.1. Структура записи

Пример JSON:

```json
{
  "normalize_text": {
    "type": "pre",
    "subject": "beamline.ext.pre.normalize_text.v1",
    "timeout_ms": 80,
    "retry": 0
  },
  "mask_pii": {
    "type": "post",
    "subject": "beamline.ext.post.mask_pii.v1",
    "timeout_ms": 100,
    "retry": 0
  },
  "pii_guard": {
    "type": "validator",
    "subject": "beamline.ext.validate.pii_guard.v1",
    "timeout_ms": 100,
    "retry": 0
  },
  "my_crm_summarizer": {
    "type": "provider",
    "subject": "beamline.provider.my_crm_summarizer.v1",
    "timeout_ms": 5000,
    "retry": 1
  }
}
```

Router использует реестр, чтобы:

- по логическому `id` из политики найти:
  - `type`;
  - NATS `subject`;
  - `timeout_ms` и `retry`;
- понять, **куда** послать запрос и **как** обрабатывать ошибки.

---

## 7. Routing Policy: подключение расширений

Routing Policy описывает, **какие** расширения и провайдеры используются для конкретного сценария/tenant и **в каком порядке**.

### 7.1. Пример политики

```json
{
  "policy_id": "support_en",
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "config": {
        "lowercase": true
      }
    }
  ],
  "validators": [
    {
      "id": "pii_guard",
      "on_fail": "block"
    }
  ],
  "providers": [
    "openai:gpt-4.1-mini",
    "my_crm_summarizer"
  ],
  "post": [
    {
      "id": "mask_pii",
      "mode": "required",
      "config": {
        "mask_email": true
      }
    }
  ]
}
```

Семантика:

- `pre[]` — последовательность pre‑processors:
  - `id` — логический ID (lookup в Registry);
  - `mode`:
    - `required` — при таймауте/ошибке политика считается неуспешной (Router действует по дефолтным правилам ошибки);
    - `optional` — при ошибке Router пропускает расширение и идёт дальше;
  - `config` — per‑policy конфиг, попадает в `extensions.config`.

- `validators[]` — последовательность validators:
  - `id` — логический ID;
  - `on_fail` — `block` / `warn` / `ignore`.

- `providers[]` — список `provider_id` в порядке приоритета/weights (детальный формат описывается в `ROUTING_POLICY.md`).

- `post[]` — последовательность post‑processors, с теми же правилами `mode` и `config`, что у `pre[]`.

---

## 8. Code vs Config

### 8.1. «Как код» (ядро платформы)

Меняется редко, требует релизов:

1. **Схемы CP1/CP2/CP‑Ext**
   - Protobuf/JSON типы `RouteRequest`, `RouteDecision`, `ExtRequest`, `ExtResponse`, `ProviderRequest`, `ProviderResponse`.
   - Код Router/Workers, который работает с этими типами.

2. **Механика вызова расширений в Router**
   - Пайплайн: `pre[] → validators[] → routing → provider → post[]`.
   - Адаптер `extension_id → subject/timeout → NATS request/response`.
   - Единые правила ошибок (timeouts, retries, circuit breaker).
   - Логирование и метрики.

3. **Extension Registry loader**
   - Чтение реестра из БД/конфига → Mnesia/ETS.
   - Чтение и применение Routing Policies.

4. **Gateway / Router / CAF‑workers**
   - Не меняются под конкретные клиентские сценарии, только при эволюции платформы.

### 8.2. «Как конфиг» (динамика без перезапуска ядра)

Меняется часто:

1. **Routing Policies**
   - Состав цепочки `pre/validators/post`.
   - Список провайдеров и их веса/приоритеты.
   - Поведение при ошибках (`mode`, `on_fail`).
   - Per‑policy `config` для расширений.

2. **Extension Registry**
   - Добавление нового расширения:
     - поднять NATS‑сервис;
     - добавить запись в реестр (`id → type/subject/timeout`);
     - Router после reload уже знает новый subject.

3. **Tenant‑level overrides**
   - Какой `policy` использовать для конкретного tenant/канала.
   - Включение/выключение расширений для tenant (feature‑flags).

Хранение:

- Postgres + кеш в Mnesia/ETS; или
- конфиг‑файлы с hot‑reload.

**Изменение конфига = новый бизнес‑кейс без перекомпиляции ядра.**

### 8.3. Где живёт кастомный код клиента

Кастомная логика клиента реализуется только в **отдельных NATS‑сервисах расширений** (`pre`, `validator`, `post`, `provider`):

- может быть написана на любом языке (Go, Rust, Erlang, C, Python и т.п.);
- обязана соблюдать CP‑Ext/CP2‑контракт.

Добавление нового поведения:

1. Поднять новый NATS‑сервис (подписка на свой subject).
2. Добавить запись в Extension Registry.
3. Подключить расширение в Routing Policy (по `id`).
4. Применить конфиг.

При этом:

- **Gateway** не меняется (HTTP ↔ NATS CP1);
- **Router** не меняет код (только перечитывает Registry/Policies);
- **CAF‑workers** не меняются (видят provider через CP2 как обычного).

---

## 9. Жизненный цикл расширения (для разработчика)

1. **Создать сервис расширения**
   - выбрать тип (`pre` / `validator` / `post` / `provider`);
   - реализовать обработчик CP‑Ext/CP2 Request → Response;
   - подписаться на свой NATS subject.

2. **Зарегистрировать в Extension Registry**
   - добавить запись `{id → type, subject, timeout_ms, retry}`.

3. **Подключить в Routing Policy**
   - указать `id` в `pre[]` / `validators[]` / `post[]` или в списке `providers[]`;
   - при необходимости указать режим (`mode` / `on_fail`) и `config`.

4. **Перезагрузить конфиг / обновить кеш**
   - Router перечитывает Registry и Policies;
   - новое расширение начинает участвовать в пайплайне без изменений кода ядра.