# CP1 Acceptance Summary (Short)

**Checkpoint:** CP1-LC
**Status:** Accepted
**Transition Intent:** Prepared for CP2-LC via DevState

## What’s Included in CP1

- Router (Erlang/OTP) baseline:
  - Core routing logic, error mapping, NATS integration
  - Policy enforcement (JSON-DSL), RBAC, audit trail
  - Observability baseline: structured JSON logs, health, basic metrics
- C-Gateway (C11):
  - HTTP REST entrypoint, NATS integration, DTO parsing
  - Health checks, error handling, initial smoke tests
- Contracts & Data:
  - NATS subjects registry aligned (`docs/NATS_SUBJECTS.md`)
  - SQL base schema present (`sql/000_init.sql`)
- State & Audit:
  - `.trae/state.json` set to `CP1-LC` with checksum
  - `.trae/history.json` updated with HMAC chain (DevState verify OK)

## Deferred to CP2

- Router CP2 scope:
  - Idempotency layer (ETS cache + TTL)
  - JetStream durability, redelivery control, headers propagation
  - Expanded tracing (OpenTelemetry spans), tenant ACL enforcement
- Gateway/Provider:
  - Rate limiting refinements (per-tenant, burst/window policies)
  - Extended error taxonomy and retries/backoff
- Observability Expansion:
  - Grafana dashboards and Prometheus alert rules
  - k6 load testing scripts in pipeline, SLA thresholds
- Contracts:
  - Proto sources restoration and CP2+ fields alignment

## Evidence & Pointers

- State: `.trae/state.json` (current_cp: CP1-LC)
- History: `.trae/history.json` (HMAC chain consistent)
- DevState: `/v1/devstate/verify?limit=0` → OK
- Docs:
  - `docs/NATS_SUBJECTS.md`
  - `docs/ROUTING_POLICY.md`
  - `docs/STATE.schema.json`

## Next Steps

- Execute CP2 via DevState process (locks, append history, update state)
- Track acceptance via minimal report and verify gates
 - Governance: any new features related to JetStream, Idempotency, ACL, Admin gRPC are classified as CP2 work and go to CP2 checklist from this point forward.
