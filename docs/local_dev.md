# Локальный запуск Worker + NATS

- Требования: Docker, Docker Compose, Rust (stable)
- Запуск:
  - `docker compose up -d nats`
  - `NATS_URL=nats://127.0.0.1:4222 cargo run -p worker`
- Smoke-сценарий:
  - `./scripts/smoke_worker.sh` — поднимает NATS, запускает Worker, шлёт demo assignment (Envelope v1) и проверяет `/metrics`
- Маршруты:
  - `/_health` — базовое здоровье
  - `/readyz` — готовность (NATS connected + subscribe)
  - `/_build` — версия билда
  - `/metrics` — Prometheus метрики
