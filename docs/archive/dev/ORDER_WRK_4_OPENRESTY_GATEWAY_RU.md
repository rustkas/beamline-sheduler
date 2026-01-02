# ORDER-WRK-4-OPENRESTY-001: Миграция Gateway с NestJS на новый HTTP Gateway

**Owner**: wrk-4 (Gateway Lead)  
**CP Phase**: CP4-INTEGRATION (Gateway & UI)  
**Related ADR**: ADR-016 (Gateway Migration from NestJS to alternative HTTP gateway)  

---

## 1. Purpose / Цель

Реализовать новый HTTP-Gateway и поэтапно заменить им текущий NestJS-Gateway, сохранив существующие HTTP-контракты и интеграцию с Erlang Router через NATS.

В конце этого ORDER:

- Новый gateway реализует ключевые эндпоинты (`/api/v1/messages`, `/api/v1/routes/decide`, `/healthz`).
- Поведение нового gateway функционально эквивалентно текущему NestJS-gateway по контрактам и семантике.
- Dev/Stage окружения используют новый gateway как основной.
- NestJS-gateway подготовлен к удалению (последний шаг отдельным ORDER).

---

## 2. Scope / Объём работ

### В scope

- Конфигурация HTTP-gateway и Lua-модули для обработки запросов.
- Реализация HTTP-эндпоинтов:
  - `POST /api/v1/messages`
  - `POST /api/v1/routes/decide`
  - `GET /healthz`
- Интеграция с NATS / Router:
  - subject `beamline.router.v1.decide` (см. `docs/NATS_SUBJECTS.md`, `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`).
- Минимальная валидация входных DTO в соответствии с `docs/ARCHITECTURE/api-registry.md`.
- Логирование и базовые метрики.
- Интеграционные тесты и нагрузочные проверки.

### Вне scope (в этом ORDER)

- Полная авторизация/JWT/JWKS (может быть вынесена в отдельный ORDER).
- SSE/WebSocket-потоки.
- Swagger/OpenAPI-документация.
- Окончательное удаление NestJS-gateway (отдельный ORDER после стабилизации).

---

## 3. Tasks / Задачи

### Task W4-OG-1 (ID: CP3-GW-001) — Создание каркаса HTTP-gateway

**Цель:**

Создать базовую структуру и конфигурацию HTTP-gateway.

**Шаги:**

1. Создать структуру каталогов и конфигурации для HTTP-gateway (nginx.conf, conf.d, Lua-модули, spec-тесты).
2. Добавить базовый `Dockerfile` для образа gateway.
3. Добавить make/скрипты для локального запуска (например, таргет для HTTP-gateway).
4. При необходимости включить HTTP-gateway в локальную docker-compose / скрипты запуска (отдельный PR).

**Definition of Done:**

- HTTP-gateway поднимается локально (`docker-compose` или отдельный `docker run`).
- `GET /healthz` возвращает `200 {"status":"ok"}`.

---

### Task W4-OG-2 (ID: CP3-GW-002) — Lua-модуль util.lua (headers/json/response)

**Цель:**

Реализовать общий модуль `beamline.util` с утилитами для работы с заголовками, телом запроса и JSON-ответами.

**Шаги:**

1. Реализовать функции:
   - `get_header(name, default)`
   - `read_body_json()`
   - `json_response(status, table)`
2. Покрыть модуль unit-тестами (busted):
   - корректное чтение заголовков;
   - корректное поведение при пустом/некорректном теле;
   - корректная упаковка JSON-ответа.

**Definition of Done:**

- Тесты для `util.lua` проходят локально.
- Ошибки валидации тела запроса и заголовков возвращаются в формате JSON.

---

### Task W4-OG-3 (ID: CP3-GW-003) — Lua-модуль router_client.lua (NATS/Router)

**Цель:**

Реализовать `beamline.router_client` для формирования запросов к Router и отправки их через NATS.

**Шаги:**

1. Реализовать функцию:
   - `build_decide_request(tenant_id, trace_id, body_table)` — формирует структуру NATS-запроса.
2. Реализовать функцию:
   - `decide(tenant_id, trace_id, body_table)` — отправляет запрос в subject `beamline.router.v1.decide`, обрабатывает ответ и ошибки.
3. Настроить Lua NATS-клиент (вариант A из ТЗ) или временно HTTP-sidecar (вариант B) — в зависимости от решения команды.
4. Покрыть модуль тестами (busted) с моками NATS-клиента.

**Definition of Done:**

- `build_decide_request` соответствует контракту Router (см. Proto/NATS mapping).
- Успешный путь: корректная сериализация/десериализация JSON.
- Ошибки NATS/Router маппятся в понятные error-строки.

---

### Task W4-OG-4 (ID: CP4-GW-001) — Lua-модуль messages.lua (`/api/v1/messages`, `/api/v1/routes/decide`)

**Цель:**

Реализовать HTTP-хендлеры для ключевых эндпоинтов на базе `beamline.util` и `beamline.router_client`.

**Шаги:**

1. Реализовать `handle()` в `beamline.messages`:
   - чтение `X-Tenant-ID` (обязательно);
   - чтение/генерация `trace_id`;
   - чтение и валидация JSON тела (обязательные поля: `message_id`, `message_type`, `payload`);
   - вызов `router_client.decide`;
   - маппинг ответа Router в DTO ответа HTTP.
2. Сконфигурировать конфигурацию gateway (например, `gateway.conf`) для маршрутов:
   - `location = /api/v1/messages { content_by_lua_file lua/beamline/messages.lua; }`
   - `location = /api/v1/routes/decide { content_by_lua_file lua/beamline/messages.lua; }`
3. Добавить unit-тесты/интеграционные тесты (по возможности с подменой `ngx`).

**Definition of Done:**

- `POST /api/v1/messages` и `POST /api/v1/routes/decide` работают локально и возвращают корректные ответы при подключённом Router.
- Ошибки валидации и недоступности Router возвращают коды/ошибки согласно ТЗ.

---

### Task W4-OG-5 (ID: CP4-GW-002) — Сравнение контрактов NestJS и нового gateway

**Цель:**

Убедиться, что новый gateway функционально эквивалентен NestJS по контрактам и статус-кодам.

**Шаги:**

1. Составить таблицу эндпоинтов/DTO/статусов на основе `docs/ARCHITECTURE/api-registry.md` и текущего NestJS.
2. Написать небольшой e2e-скрипт (bash/TS) для отправки одинаковых запросов в оба gateway (NestJS и новый HTTP Gateway) и сравнения ответов.
3. Исправить расхождения в реализации нового gateway, пока ответы не совпадут по форме и коду статуса.

**Definition of Done:**

- Для ключевых сценариев ответы NestJS и нового gateway совпадают по DTO и статусам.
- Отличия (если есть) задокументированы и согласованы.

---

### Task W4-OG-6 (ID: CP4-GW-003) — Интеграционные и нагрузочные тесты

**Цель:**

Проверить новый gateway под нагрузкой и интеграцией с Router/NATS.

**Шаги:**

1. Подготовить сценарии для k6/vegeta/locust:
   - равномерная нагрузка на `/api/v1/messages`;
   - сценарии с ошибками Router/NATS.
2. Зафиксировать целевые метрики (latency, error rate) и сравнить с NestJS.
3. Отразить результаты в коротком отчёте `docs/archive/dev/NEW_GATEWAY_BENCHMARK_RU.md` (по желанию).

**Definition of Done:**

- p95 latency Gateway (без учёта Router) укладывается в целевые значения (см. ТЗ).
- Ошибки и деградации зафиксированы и понятны.

---

### Task W4-OG-7 (ID: CP4-GW-004) — Переключение трафика и план удаления NestJS

**Цель:**

Поэтапно переключить трафик на новый HTTP Gateway и подготовить удаление NestJS-решения.

**Шаги:**

1. Dev:
   - запустить новый gateway и NestJS параллельно на разных портах;
   - переключить dev-инфраструктуру на новый gateway;
   - оставить NestJS как fallback.
2. Stage:
   - провести canary/blue-green переключение на новый gateway;
   - мониторить логи/метрики.
3. После успешной стабилизации:
   - подготовить отдельный ORDER на удаление NestJS-gateway из `apps/gateway/` и CI.

**Definition of Done:**

- Dev и Stage используют новый HTTP Gateway как основной.
- Есть отдельный ORDER/план на физическое удаление NestJS-решения.

---

## 4. Exit Criteria / Критерии завершения ORDER

ORDER-WRK-4-OPENRESTY-001 считается завершённым, когда:

1. Новый HTTP Gateway реализован и проходит unit/integration tests.
2. Новый HTTP Gateway реализует ключевые HTTP-эндпоинты и совпадает по контрактам с NestJS.
3. Нагрузочные тесты подтверждают приемлемую latency и стабильность.
4. Dev/Stage окружения используют новый HTTP Gateway как основной.
5. Создан отдельный ORDER для удаления NestJS-gateway и его CI/infra артефактов.
