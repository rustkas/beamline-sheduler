# TECHSPEC: Stack Integration (router / c-gateway / caf / ui_web / extensions)

## 0. Goal

Сделать стек реально запускаемым и взаимно согласованным, чтобы работал end-to-end поток:

`ui_web → c-gateway → (NATS) → router → (extensions / caf) → router → c-gateway → ui_web`.

Документ — список задач и критериев готовности.

---

## 1. AS-IS integration summary (facts)

### 1.1 ui_web ↔ c-gateway

- HTTP client: `apps/ui_web/lib/ui_web/services/gateway_client.ex`
- SSE bridge: `apps/ui_web/lib/ui_web/sse_bridge.ex` (`/api/v1/messages/stream`)

### 1.2 c-gateway ↔ router

- Subject: `beamline.router.v1.decide` (+ reply `.reply`)
- C-Gateway NATS client: `apps/c-gateway/src/nats_client_real.c`
- Router consumer: `apps/otp/router/src/router_decide_consumer.erl`

### 1.3 router ↔ extensions

- NATS request/reply:
  - `beamline.ext.pre.*`
  - `beamline.ext.validate.*`
  - `beamline.ext.post.*`
  - `beamline.provider.*`

### 1.4 router ↔ caf

- Router publishes assignments: `caf.exec.assign.v1`
- Router consumes results: `caf.exec.result.v1`
- CAF worker transport отсутствует (см. `docs/TECHSPEC_CAF_NATS_TRANSPORT.md`)

### 1.5 Critical risk: Router NATS transport is stubbed

По коду `apps/otp/router/src/router_nats.erl` содержит stub реализации publish/subscribe/ack/nak.
Без реального NATS/JetStream транспорта полноценная работа (гарантии доставки, ack/nak, durable consumer) невозможна.

---

## 2. Target behavior (CP: “stack runnable”)

### 2.1 Minimal user story

- UI-Web делает decide/message запросы через C-Gateway.
- C-Gateway отправляет decide в Router по NATS и получает ответ.
- Router применяет policy и extensions pipeline.
- При включённом `push_assignment` Router отправляет assignment в CAF, CAF выполняет, Router принимает result.
- UI-Web получает обновления через SSE.

---

## 3. Work items (tasks)

### 3.1 Ports & configuration consistency

- **T1**: единый контракт портов в compose/docs/ui_web.
  - external: C-Gateway `8081`, UI-Web `4000`, Router gRPC `9000`, NATS `4222`, NATS monitoring `8222`.
  - tests: MockGateway `8082`.

**Done partially**: исправления в ui_web defaults и тестах + docs updates.

### 3.2 Replace/neutralize legacy docs

- **T2**: пометить `docs/ROUTER_GATEWAY_INTEGRATION_SPEC.md` как legacy.
- **T3**: добавить актуальный `docs/CGATEWAY_ROUTER_INTEGRATION_ASIS.md`.

### 3.3 Router ↔ NATS real transport

- **T4 (P0)**: реализовать реальный NATS/JetStream транспорт в Router.

Acceptance (T4):
- publish/subscribe реально ходят в NATS (не stub)
- publish_with_ack возвращает реальный PubAck
- ack/nak работают с JetStream
- router_decide_consumer реально получает сообщения из JetStream

Notes:
- сейчас router_nats содержит stub для do_publish_internal, do_publish_with_ack_internal, do_subscribe_jetstream_internal, do_nak_message_internal.

### 3.4 CAF NATS/JetStream transport

- **T5 (P0)**: реализовать `docs/TECHSPEC_CAF_NATS_TRANSPORT.md`.

Acceptance (T5):
- CAF consumer получает `caf.exec.assign.v1`
- CAF публикует `caf.exec.result.v1`
- heartbeat идёт в `caf.worker.heartbeat.v1`

### 3.5 C-Gateway end-to-end hardening

- **T6**: убедиться, что C-Gateway использует реальную NATS реализацию (не stub) во всех окружениях сборки.
- **T7**: описать/зафиксировать reply pattern (Router отвечает в `.reply`) и таймауты.

### 3.6 Extensions lifecycle

- **T8**: минимальный способ запускать reference extensions вместе со стеком.
- **T9**: health/registry endpoints в C-Gateway должны отражать реальный статус extensions (не stub), или документировать как “planned”.

### 3.7 Verification / smoke tests

- **T10**: добавить smoke test сценарий:
  - POST `/api/v1/routes/decide` → 200 + валидный DecideResponse
  - SSE stream `/api/v1/messages/stream` соединяется
  - NATS subjects существуют, Router consumer активен
  - (опц.) CAF assignment/result roundtrip

---

## 4. Priorities

- **P0**: Router real NATS/JetStream transport (T4)
- **P0**: CAF NATS/JetStream transport (T5)
- **P1**: docs/ports consistency (T1–T3)
- **P1**: smoke tests (T10)
- **P2**: extensions lifecycle/registry hardening (T8–T9)

---

## 5. Deliverables

- Updated docs:
  - `docs/CGATEWAY_ROUTER_INTEGRATION_ASIS.md`
  - `docs/TECHSPEC_CAF_NATS_TRANSPORT.md`
  - `docs/TECHSPEC_STACK_INTEGRATION.md`

---

## 6. Definition of Done

Stack is “runnable” when:

- docker-compose up → ui-web loads
- C-Gateway health ok
- decide flow works end-to-end
- Router реально подключён к NATS (не stub)
- CAF roundtrip работает (если включён)
