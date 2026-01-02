# Обзор видения и архитектуры BeamLine (кратко)

Этот документ даёт краткое русскоязычное описание того, **что такое BeamLine** и
как текущая архитектура репозитория соотносится с целевым продуктом.

Полный источник на английском: см. **[BEAMLINE_VISION_AND_ARCHITECTURE.md](BEAMLINE_VISION_AND_ARCHITECTURE.md)**.

## Что мы строим

BeamLine — это **не BPMN‑редактор** и не просто автоматизация диаграмм.
Это **операционная система для фабрик искусственного интеллекта**:

- Процессы исполняют **живые исполнители**: AI‑агенты, API и люди.
- Система управляет **потоками данных и событий** между ними.
- Фокус не на рисовании диаграмм, а на **надёжном исполнении ран‑ов**.

Коротко:
> BeamLine — это "AI Orchestrator" / "AI Production Line":
> платформа, где процессы — это живые потоки взаимодействующих агентов,
> а не статичные схемы.

## Как это связано с текущей архитектурой

В репозитории уже реализован CP1/частично CP2‑скелет BeamLine:

- **Gateway (TypeScript)** — REST‑шлюз, который принимает внешние запросы,
  проверяет DTO и превращает их в NATS‑сообщения.
- **Router (Erlang/OTP)** — маршрутизатор, который по NATS‑subjects и
  политикам решает, какому провайдеру/сервису отдать сообщение.
- **Providers (Erlang/OTP)** — доменные сервисы, которые реализуют
  конкретную бизнес‑логику.
- **UI (Svelte)** — лёгкий фронт для smoke‑проверок и демонстрации.
- **Proto + NATS** — контракты и темы обмена, обеспечивающие совместимость.
- **.trae + DevState** — tooling среды разработки (TRAE) для состояния и истории проекта с HMAC‑цепочкой; это внешний по отношению к продукту BeamLine слой управления state/history.

Это всё уже работает как **базовый оркестратор**, через который в будущем
будут проходить ран‑ы агентов и AI‑фабрик.

## Технологическая опора (high‑level)

- **Erlang/OTP** — слой оркестрации и маршрутизации (Router, Providers).
- **CAF/C++** — тяжёлые вычислительные узлы (ASR, CV, большие модельные
  пайплайны) по спецификациям CAF‑воркеров.
- **gRPC + Protobuf** — единый ABI между Erlang‑оркестратором и C++‑воркерами.
- **NATS** — внутренняя шина событий/команд в BeamLine.

## Где смотреть детали

- Полное видение и архитектура (EN):
  - **[BEAMLINE_VISION_AND_ARCHITECTURE.md](BEAMLINE_VISION_AND_ARCHITECTURE.md)**
- Контракты и маршрутизация:
  - **[ARCHITECTURE/api-registry.md](ARCHITECTURE/api-registry.md)**
  - **[NATS_SUBJECTS.md](NATS_SUBJECTS.md)**
  - **[ARCHITECTURE/PROTO_NATS_MAPPING.md](ARCHITECTURE/PROTO_NATS_MAPPING.md)**
  - **[ROUTING_POLICY.md](ROUTING_POLICY.md)**
- Наблюдаемость и состояние:
  - **[OBSERVABILITY.md](OBSERVABILITY.md)**, **[OBSERVABILITY_CONVENTIONS.md](OBSERVABILITY_CONVENTIONS.md)**
  - **[DEVSTATE.md](DEVSTATE.md)**, **[STATE.schema.json](STATE.schema.json)**, **[HISTORY.schema.json](HISTORY.schema.json)**
