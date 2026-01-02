# C-Gateway Dev HOWTO (EN / RU)

> Related: See also [GATEWAY_SSE_AND_EXTENSIONS.md](./GATEWAY_SSE_AND_EXTENSIONS.md) for realtime (SSE), Extensions API, build/run and smoke on port 8088.

## 1. English: Run local stack with C-Gateway + real NATS client

### Quick Summary (CP1)

- Build/Run (local, stub NATS):
  - `make -C apps/c-gateway`
  - `GATEWAY_PORT=8088 ./apps/c-gateway/build/c-gateway`
- Health/Metrics:
  - `GET /_health` → `{ "status": "ok" }`
  - `GET /_metrics` → JSON: `rps`, `latency.p50/p95`, `error_rate`, `nats`, `ts`
- Realtime SSE:
  - `GET /api/v1/messages/stream?tenant_id=...` → events: `message_created|updated|deleted`
  - Create via `POST /api/v1/routes/decide` (with `X-Tenant-ID`), update/delete via `PUT/DELETE /api/v1/messages/:id`
- Extensions API:
  - `POST|PUT|DELETE /api/v1/registry/blocks/:type/:version` with strict Draft‑07 subset validation


### 1.1. Prerequisites

- Docker and docker-compose available locally.
- This repository cloned locally.

### 1.2. Build images

C-Gateway image builds libnats (official `nats-io/nats.c`) from source and then builds the C-Gateway binary with `USE_NATS_LIB=ON`.

From the repo root:

```bash
# Build core services including c-gateway and router
docker compose build c-gateway router nats
```

You can also build only C-Gateway:

```bash
cd apps/c-gateway
docker build -t c-gateway-dev .
```

### 1.3. Run local stack

From the repo root:

```bash
# Start NATS, Router and C-Gateway
docker compose up -d nats router c-gateway
```

Check that services are healthy:

```bash
docker compose ps
```

### 1.4. End-to-end test (curl)

Assuming `c-gateway` is exposed on `localhost:8081`:

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

Path of the request:

1. `curl → c-gateway` — C-Gateway accepts HTTP on `:8081`.
2. `c-gateway → NATS` — C-Gateway:
   - validates headers and JSON;
   - sends request to `beamline.router.v1.decide` using `libnats`.
3. `Router → Extensions/Providers` — Router executes pipeline with extensions and providers.
4. `Router → c-gateway` — returns `RouteDecisionResponse` via NATS.
5. `c-gateway → client` — HTTP JSON response is returned to the caller.

If `libnats` is not available or `USE_NATS_LIB=OFF`, C-Gateway will fall back to the stub implementation and respond with a dummy `RouteDecisionResponse` JSON instead of talking to Router.

### 1.5. Additional endpoints

- `GET /api/v1/routes/decide/:message_id` — returns a decision for a previously processed message:
  - uses `X-Tenant-ID` header and `:message_id` from URL;
  - calls Router over NATS (subject `beamline.router.v1.get_decision`);
  - response codes:
    - `200` — decision found, JSON matches Router response;
    - `404` — decision for `message_id` not found;
    - `400` — invalid request (empty `message_id`, missing tenant_id);
    - `503/500` — Router or NATS are unavailable.

- `GET /metrics` — exposes basic text metrics:
  - `gateway_requests_total`
  - `gateway_requests_errors_total`
  - `gateway_requests_errors_4xx`
  - `gateway_requests_errors_5xx`

---

## 2. Русский: запуск локального стека с C-Gateway и реальным NATS-клиентом

### 2.1. Предварительные условия

- Установлены Docker и docker-compose.
- Репозиторий склонирован локально.

### 2.2. Сборка образов

Образ C-Gateway собирает официальный клиент `nats-io/nats.c` (libnats) из исходников, а затем собирает бинарь C-Gateway с `USE_NATS_LIB=ON`.

Из корня репозитория:

```bash
# Собрать ядро сервисов, включая c-gateway и router
docker compose build c-gateway router nats
```

Можно собрать только C-Gateway:

```bash
cd apps/c-gateway
docker build -t c-gateway-dev .
```

### 2.3. Запуск стека

Из корня репозитория:

```bash
# Запустить NATS, Router и C-Gateway
docker compose up -d nats router c-gateway
```

Проверить, что сервисы поднялись и здоровы:

```bash
docker compose ps
```

### 2.4. End-to-end проверка (curl)

Предполагаем, что `c-gateway` слушает на `localhost:8080`:

```bash
curl -X POST "http://localhost:8080/api/v1/routes/decide" \
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

1. `curl → c-gateway` — C‑Gateway принимает HTTP‑запрос на `:8080`.
2. `c-gateway → NATS` — C‑Gateway:
   - валидирует заголовки и JSON;
   - отправляет запрос в `beamline.router.v1.decide` через `libnats`.
3. `Router → Extensions/Providers` — Router выполняет пайплайн (pre/validator/post/extensions, провайдеры) по Routing Policy.
4. `Router → c-gateway` — возвращает `RouteDecisionResponse` по NATS.
5. `c-gateway → клиент` — C‑Gateway маппит ответ в HTTP‑JSON и возвращает его клиенту.

Если `libnats` недоступен или сборка идёт с `USE_NATS_LIB=OFF`, C‑Gateway использует stub‑реализацию и возвращает фиксированный JSON вместо реального вызова Router.
