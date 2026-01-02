# C-Gateway ↔ Router Integration (AS-IS)

**Purpose**: фиксирует фактическую (по коду) интеграцию между `apps/c-gateway` и `apps/otp/router`.

---

## 1. Components

### 1.1 C-Gateway

- **Role**: HTTP entrypoint (HTTP → NATS → Router)
- **Code**: `apps/c-gateway/src/http_server.c`, `apps/c-gateway/src/nats_client_real.c`

### 1.2 Router

- **Role**: decision engine + extensions pipeline + CAF assignment publisher
- **Code**: `apps/otp/router/src/router_decide_consumer.erl`, `apps/otp/router/src/router_decider.erl`, `apps/otp/router/src/router_caf_adapter.erl`

---

## 2. HTTP API (C-Gateway)

### 2.1 Health

- `GET /_health`

### 2.2 Decide

- `POST /api/v1/routes/decide`

C-Gateway:
- валидирует DTO (`validate_decide_request`)
- строит Router RouteRequest JSON (`build_route_request_json`)
- делает `nats_request_decide(route_req_json, resp_buf, ...)`

---

## 3. NATS Subjects and Patterns

### 3.1 Decide request

- **Subject**: `beamline.router.v1.decide`
- **Config** (C-Gateway): env `ROUTER_DECIDE_SUBJECT` (default `beamline.router.v1.decide`)
- **Pattern**: request/reply

Router:
- подписка через JetStream durable consumer: `router_decide_consumer.erl`
- обработка DecideRequest → `router_core:route(...)`
- ответ публикуется в subject: `beamline.router.v1.decide.reply`

### 3.2 NATS connection

C-Gateway:
- env `NATS_URL` (default `nats://nats:4222`)

---

## 4. Message formats (high-level)

Форматы подробно описаны в `docs/NATS_SUBJECTS.md`.

Минимально:
- DecideRequest: JSON с `tenant_id`, `request_id`, `task`, `policy_id`, `constraints`, `metadata`, опционально `run_id/flow_id/step_id/trace_id`.
- DecideResponse: JSON `ok: true/false` + `decision` или `error`.

---

## 5. Known gaps / risks

### 5.1 Router NATS transport is stubbed

Факт по коду: `apps/otp/router/src/router_nats.erl` содержит stub-реализации для publish/subscribe/ack/nak.
Это означает, что часть интеграции (JetStream ACK/NAK, publish_with_ack) пока не гарантирована реальным транспортом.

**Impact**:
- decide flow может работать только частично/в тестовом режиме;
- CAF assignment delivery через `router_nats:publish_with_ack/3` не будет реально гарантированной.

---

## 6. Cross-links

- `docs/NATS_SUBJECTS.md`
- `docs/TECHSPEC_CAF_NATS_TRANSPORT.md`
- `docs/TECHSPEC_STACK_INTEGRATION.md`
