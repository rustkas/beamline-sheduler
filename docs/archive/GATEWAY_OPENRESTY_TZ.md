---
status: withdrawn
reason: gateway experiment cancelled
---

- все структуры должны совпадать с `docs/ARCHITECTURE/api-registry.md`:
  - `MessageRequest`, `RouteDecisionResponse`, `ErrorResponse`, `MessageStatusResponse`, и т.д.;
- никакого изменения формата URL/полей без обновления API Registry.

**Заголовки:**

- `X-Tenant-ID` — обязателен (иначе 400 `INVALID_REQUEST`);
- `X-Trace-ID` — опционален (при отсутствии генерируем UUID);
- `Authorization` — опционален (но должен корректно прокидываться при наличии).

**Валидация:**

- обязательные поля тела запросов проверяются;
- при ошибке — `400 Bad Request` с DTO `ErrorResponse` (`code = "INVALID_REQUEST"`).

### 4.2. Связь с Router/Ingress через NATS

Используем уже зафиксированные subjects и формат сообщений.

**Для `/api/v1/routes/decide`:**

- публикуем JSON в `beamline.router.v1.decide` (Request‑Reply);
- Request JSON строго соответствует `RouteRequest` (mapping из API Registry/прото);
- Response JSON маппится в `RouteDecisionResponse`.

**Для `/api/v1/messages`:**

- вариант **MVP** (упрощённый):
  - делегировать всё в Router (`beamline.router.v1.decide`) и вернуть его решение,  
    сохранив текущее поведение NestJS;
- расширенный вариант (CP3+/CP4):
  - использовать `beamline.ingress.v1.message` для первичного приёма сообщения;
  - затем `beamline.router.v1.decide` для маршрутизации.

**Таймауты и retries:**

- настраиваемые через конфиг (по умолчанию: timeout = 5000 ms, retries = 1).

---

## 5. Нефункциональные требования

### 5.1. Производительность и модель ресурсов

- целевой уровень: **10k RPS** на одном instance при типичном сценарии  
  (небольшие JSON‑запросы, быстрый ответ Router);
- неблокирующая модель (epoll/kqueue, libuv/аналог) вместо «один поток на соединение»;
- отсутствие тяжёлых runtime/GC (C + минимальные зависимости).

### 5.2. Надёжность

- при недоступности NATS → быстрый fail с `503 Service Unavailable` + `ErrorResponse`:
  - `code = "SERVICE_UNAVAILABLE"` или `"NATS_UNAVAILABLE"`;
- при таймауте Router → `504/503` + `ErrorResponse` с `code = "TIMEOUT"` (точное значение согласовать с API Registry);
- корректная обработка ошибок NATS‑клиента, без утечек памяти и зависаний.

### 5.3. Observability (MVP)

- JSON‑логи с полями:
  - `timestamp`, `level`, `component = "gateway"`, `message`;
  - контекст: `tenant_id`, `trace_id`, `path`, `status_code`, `latency_ms`, и др.;
- маскирование секретов/PII:
  - `api_key`, `token`, `authorization`, и др. → `[REDACTED]`;
- `GET /_health`:
  - JSON вида:
    - `status: healthy | degraded | unhealthy`;
    - `checks` с мини‑сводкой (`nats`, внутренние очереди и т.п.);
  - проверки:
    - HTTP‑loop жив;
    - NATS доступен (ping или простой publish‑check с таймаутом).

---

## 6. Конфигурация и деплой

Используем ту же философию, что в Environment Guide:  
`*.env.example` → `*.env`, переменные читаются через env/config‑файлы.

**Минимальный набор переменных:**

- `GATEWAY_PORT` — HTTP‑порт C‑Gateway;
- `NATS_URL`, `NATS_PORT`;
- `ROUTER_REQUEST_TIMEOUT_MS`;
- `LOG_LEVEL` (`INFO`, `DEBUG`, `WARN`, `ERROR`);
- `ENV` (`development` | `staging` | `production`).

**Docker/Compose:**

- в `docker-compose` C‑Gateway постепенно **заменяет** сервис `gateway` (NestJS);
- остальная инфраструктура (`postgres`, `nats`, `router`, `devstate`) не меняется;


### 6.1. Включение реального NATS‑клиента (USE_NATS_LIB)

По умолчанию C‑Gateway собирается с заглушкой `nats_client_stub` и **не требует** внешних зависимостей.

Для включения реального клиента на базе официального `nats-io/nats.c` (`libnats`):

1. **Сборка локально (без Docker):**

   - Установить `libnats` в систему (см. документацию `nats-io/nats.c`):
     - клонировать репозиторий `nats.c`;
     - `mkdir build && cd build && cmake .. && make && make install`.
   - В `apps/c-gateway` запустить:

     ```bash
     mkdir -p build
     cd build
     cmake -DUSE_NATS_LIB=ON ..
     make c-gateway
     ```

   - бинарь `c-gateway` будет собран с использованием `libnats` и реализацией `nats_client_real.c`.

2. **Сборка через Docker:**

   - Использовать Dockerfile `apps/c-gateway/Dockerfile`, который:
     - берёт `alpine` как базу;
     - собирает и устанавливает `nats-io/nats.c` внутри образа;
     - собирает C‑Gateway с `-DUSE_NATS_LIB=ON`;
     - копирует `c-gateway` и `libnats` в runtime‑образ.

   - В `docker-compose.yml` сервис `c-gateway` использует этот Dockerfile как build‑контекст.

При отсутствии `libnats` или при сборке с `USE_NATS_LIB=OFF` C‑Gateway продолжает работать на stub‑реализации и не делает реальных запросов к Router через NATS.

---

## 7. Безопасность и лицензии

- основной проект — Apache‑2.0 (см. Third‑Party Licenses Registry);
- при выборе HTTP/NATS C‑библиотек:
  - предпочтительные лицензии: MIT / BSD / Apache;
  - **запрещены** GPL/AGPL (несовместимы с текущей моделью);
- секреты (API‑ключи провайдеров, HMAC, и др.) — только через env/CI‑secrets  
  согласно `docs/SECURITY_GUIDE.md` и `docs/ENVIRONMENT.md`.

---

## 8. Тестирование и CI

Основано на существующих `scripts/run_checks.sh`, CI Readiness Report и CP‑доках.

**Минимум тестов:**

- unit‑тесты на парсинг DTO и маппинг NATS ↔ HTTP;
- интеграционные тесты:
  - подняты `nats` + stub‑Router;
  - `POST /api/v1/routes/decide` и `POST /api/v1/messages` проходят end‑to‑end;
  - `/ _health` возвращает ожидаемый JSON/статусы;
- нагрузочные тесты для оценки RPS/latency.

**Интеграция в CI:**

- `scripts/run_checks.sh` дополняется шагом для сборки/тестов C‑Gateway;
- C-Gateway рассматривается как ещё один компонент наряду с Router и ABI‑чеками.

---

## 9. JSON‑mapping HTTP DTO → NATS RouteRequest

Этот раздел фиксирует, как HTTP‑DTO из API Registry маппятся в NATS‑запросы к Router (`RouteRequest`) и обратно в `RouteDecisionResponse`.

### 9.1. Поля запроса (HTTP → RouteRequest)

| Источник            | HTTP поле / заголовок      | RouteRequest поле             |
|---------------------|----------------------------|-------------------------------|
| Header              | `X-Tenant-ID`              | `tenant_id`                   |
| Header (опц.)       | `X-Trace-ID` \/ сгенерированный UUID | `trace_id`          |
| Header (опц.)       | `Authorization`            | `auth.token` / `auth.scheme`* |
| Body                | `message_id`               | `message.message_id`          |
| Body                | `message_type`             | `message.message_type`        |
| Body                | `payload`                  | `message.payload`             |
| Body (опц.)         | `metadata` (object)        | `message.metadata`            |
| Body (опц.)         | `policy_id`                | `policy_id`                   |
| Body (опц.)         | `context` (object)         | `context`                     |

\*точное разбиение `Authorization` на поля `auth.*` определяется API Registry / proto‑схемой. Если в прото поле отсутствует, заголовок не сериализуется в `RouteRequest`.

Правила:

- `X-Tenant-ID` **обязателен** → при отсутствии C‑Gateway возвращает `400 INVALID_REQUEST`;
- если `X-Trace-ID` не передан, C‑Gateway генерирует новый `trace_id` (UUID) и использует его и в логах, и в `RouteRequest`;
- тело запроса должно соответствовать DTO `MessageRequest` / `RouteRequest` из API Registry.

### 9.2. Поля ответа (RouteDecisionResponse → HTTP)

Ответ Router в виде `RouteDecisionResponse` маппится обратно в HTTP‑ответ:

| RouteDecisionResponse поле | HTTP JSON поле        |
|----------------------------|-----------------------|
| `message_id`               | `message_id`          |
| `provider_id`              | `provider_id`         |
| `reason`                   | `reason`              |
| `priority`                 | `priority`            |
| `expected_latency_ms`      | `expected_latency_ms` |
| `expected_cost`            | `expected_cost`       |
| `currency`                 | `currency`            |
| `trace_id`                 | `trace_id`            |

Если API Registry / прото описывают дополнительные поля (например, `debug`, `features`, `extensions`), C‑Gateway прозрачно прокидывает их в HTTP‑JSON без трансформации.

### 9.3. Ошибки

При ошибках C‑Gateway формирует HTTP‑ошибку с DTO `ErrorResponse` и кодом из API Registry (примерно):

- валидация входных данных → `400 Bad Request`, `ErrorResponse{ code = "INVALID_REQUEST" }`;
- таймаут Router / NATS → `503/504`, `ErrorResponse{ code = "TIMEOUT" }`;
- недоступность NATS → `503`, `ErrorResponse{ code = "SERVICE_UNAVAILABLE" }`.

Точные значения `code` / `message` синхронизируются с API Registry; C‑Gateway не вводит собственные произвольные коды.

---

## 10. End‑to‑End пример (curl → c-gateway → NATS → Router)

Этот пример показывает полный путь запроса через весь стек.

Предположения:

- локально запущен `docker-compose` со службами: `nats`, `router`, `c-gateway`.

Запрос:

```bash
curl -X POST "http://localhost:8081/api/v1/routes/decide" \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: demo-tenant" \
  -H "X-Trace-ID: demo-trace-1" \
  -d '{
    "message_id": "m-1",
    "message_type": "chat",
    "payload": "hello",
    "metadata": { "channel": "cli" },
    "policy_id": "default",
    "context": { "lang": "en" }
  }'
```

Путь запроса:

1. **curl → c-gateway** — C‑Gateway принимает HTTP‑запрос.
2. **c-gateway → NATS** — C‑Gateway:
   - валидирует заголовки/JSON;
   - формирует `RouteRequest` по таблице из раздела 9;
   - отправляет request‑reply в `beamline.router.v1.decide` через NATS (stub или real `libnats`).
4. **Router → Extensions/Providers** — Router обрабатывает запрос, вызывает Extensions/Providers по Routing Policy.
5. **Router → c-gateway** — возвращает `RouteDecisionResponse` в ответе NATS.
6. **c-gateway → curl** — C‑Gateway маппит ответ в HTTP‑JSON и возвращает его клиенту.

Таким образом, C‑Gateway остаётся чистым HTTP↔NATS адаптером, а всё расширяемое поведение (Extensions/Providers) сконцентрировано в Router и полисах.

---

## 11. Влияние на документацию и ADR

**ADR‑008 (NestJS для Gateway API):**

- статус → `superseded` (фактический код на NestJS будет де‑комиссирован);
- добавить ссылку `superseded_by: ADR-011 (C HTTP Gateway)`.

**Новый ADR (условно ADR‑011, создаётся отдельно):**

- мотивация отказа от NestJS:
  - сложность зависимостей и runtime;
  - перфоманс и эксплуатационные риски;
- контекст:
  - сохранение API Registry и CP1/CP3 контрактов;
- решение:
  - C‑Gateway как основной HTTP‑сервис,
  - NATS + Erlang Router как backend;
- последствия:
  - единый продакшен‑стек C/Erlang/CAF;
  - упрощение runtime и CI.

**Обновления в документации:**

- `docs/ARCHITECTURE/context-maps.md` — «NestJS Gateway Container» → «C Gateway Container»;
- `docs/ARCHITECTURE/api-registry.md` — пометка, что реализация Gateway — на C,  
  при этом DTO/URL не меняются.

Все эти изменения в ADR/архитектурных файлах делаются в отдельных PR/итерациях;  
данный документ фиксирует техническое задание на C‑Gateway.
