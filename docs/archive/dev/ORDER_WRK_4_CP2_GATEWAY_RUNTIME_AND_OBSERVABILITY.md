# ORDER: WRK‑4 — CP2 Gateway Runtime & Observability Hardening 

- `order_id`: ORDER-WRK-4-CP2-004 
- `status`: pending 
- `owner`: wrk-4 
- `component`: apps/gateway 
- `checkpoint`: CP2-LC 
- `related_orders`: 
  - ORDER-WRK-4-CP2-003 (CP2 observability baseline) 
  - ORDER-WRK-4-AUTH-001 (Gateway↔Router auth integration) 

## 1. Purpose 

Harden the CP2 runtime behavior of the Gateway with: 

- Controlled JetStream/NATS behavior and metrics. 
- Sticky sessions that respect idempotency. 
- HTTP-level idempotency with clear tenant-aware keys and TTL. 
- End-to-end tracing and CP-phase metrics. 
- Reliable, idempotent usage tracking and RBAC. 
- Canonical documentation and CI smoke checks. 

This ORDER assumes the baseline observability work (Prometheus + OTLP export) from ORDER-WRK-4-CP2-003 is already in place. 

--- 

## 2. Current state (baseline) 

Already implemented in the workspace: 

- **JetStream / NATS:** 
  - ENV profiles: `NATS_MAX_DELIVER`, `NATS_ACK_WAIT_MS`, `NATS_BACKOFF_MS`. 
  - Consumption/latency metrics with `subject`/`tenant` labels. 
  - OTEL span `nats.consume` with attributes: `subject`, `durable`, `redelivery_count`. 
  - `NATS_SUBJECT_ALLOWLIST` + `NAK` for disallowed subjects. 

- **Sticky sessions:** 
  - ENV: `STICKY_TTL_MS`, `STICKY_SESSION_KEY`. 
  - Sticky logic wired into services/routing. 

- **Metrics cardinality control:** 
  - `METRICS_TENANT_LABEL_ALLOWLIST` introduced. 
  - All tenants outside allowlist aggregated as `tenant="other"`. 

These changes form the starting point for this ORDER. 

--- 

## 3. Tasks 

### Task W4‑A — Sticky Sessions: integration tests & metrics 

**Goal:** Prove sticky behavior and metrics are correct and do not break idempotency. 

**Scope:** 

- Add integration tests covering: 
  - **Cold start:** first request → `sticky_miss_total` increments, sticky target chosen. 
  - **Warm cache:** repeated request (same sticky key) → `sticky_hits_total` increments, same target reused. 
  - **TTL expiry:** after `STICKY_TTL_MS` elapses, next request behaves as miss and establishes a new sticky entry. 
  - **Key rotation:** changing `STICKY_SESSION_KEY` leads to a new sticky mapping. 

- In tests, assert: 
  - Effective provider selection for each scenario. 
  - Expected values of `sticky_hits_total` and `sticky_miss_total`. 

**Definition of Done:** 

- All scenarios above are covered by green integration tests. 
- A short note is added to file `CP2_READINESS_ROUTER_GATEWAY_RU.md` or a dedicated report, listing: 
  - test file(s), 
  - scenarios covered. 

--- 

### Task W4‑B — HTTP Idempotency: tenant-aware keys & E2E 

**Goal:** Ensure no duplicates across tenants and requests, with explicit idempotency semantics. 

**Scope:** 

- In HTTP layer: 
  - Define idempotency key as: 

    - `tenant_id + assignment_id` 
    - `tenant_id + ack_id` 
    - `tenant_id + usage_id` 

  - Use a shared helper (e.g. `markHttpId(...)`) that always builds and records exactly this `tenant_id + id` signature. 
  - Implement TTL-based cache for idempotency keys; TTL must be configurable and clearly documented. 

- E2E tests: 

  - **Same tenant + same id:** 
    - First request → side effect executed, key stored, `idempotency_miss_total++`. 
    - Second (duplicate) request → no repeated side effect, `idempotency_hit_total++`. 
  - **Different tenant + same id:** 
    - Treated as separate operations; no cross-tenant collision. 

**Definition of Done:** 

- Code uses tenant-aware idempotency keys consistently. 
- E2E tests for duplicates and cross-tenant separation are green. 
- `docs/API_CONTRACTS.md` (or CP2 readiness doc) contains: 
  - description of idempotency key shape (`tenant_id + id`), 
  - TTL semantics (time window of guarantees). 

--- 

### Task W4‑C — NATS Client Tracing & CP-phase Metrics 

**Goal:** Make CP2 traffic observable end-to-end (HTTP → NATS → Router) and by CP phase. 

**Scope:** 

- In NATS client / RouterClient service: 
  - Add spans: 
    - `nats.publish` 
    - `nats.request` 
  - Attach attributes: 
    - `subject` 
    - `tenant_id` 
    - `cp_phase` (when available in context) 
    - only non-sensitive, non-PII values. 

- For HTTP metrics: 
  - Stabilize metric names: 
    - `beamline_gateway_cp_requests_total` 
    - `beamline_gateway_cp_latency_seconds` 
  - Add `cp_phase` label. 

- Add at least one integration/e2e test that: 
  - Issues an HTTP request that results in NATS publish/consume. 
  - Verifies: 
    - presence of `cp_phase` in HTTP and NATS spans, 
    - `cp_phase` label in the corresponding metrics. 

**Definition of Done:** 

- Spans `nats.publish` / `nats.consume` are visible in OTLP traces with expected attributes. 
- `/metrics` exposes the CP-phase metrics with stable names and labels. 
- File CP2_READINESS_ROUTER_GATEWAY_RU.md includes a short section: 
  - “How to see CP2 traffic in metrics and traces” with metric names and key attributes. 

--- 

### Task W4‑D — Usage Tracking & RBAC 

**Goal:** Ensure usage events are idempotent, tenant-safe, and protected by RBAC. 

**Scope:** 

- Usage tracking: 

  - Ensure `beamline.usage.v1.metered` is emitted at most once per: 
    - `tenant_id + assignment_id` 
    - or `tenant_id + request_id` (depending on routing). 
  - Add integration tests: 
    - normal path (single emission), 
    - duplicate HTTP calls → no duplicate usage events. 

- RBAC: 

  - Extend test matrix to cover: 
    - usage endpoint access for allowed roles, 
    - denial for forbidden roles, 
    - behavior on invalid/unknown roles. 
  - Implement TTL-based permission cache: 
    - cache reduces repeated lookups, 
    - any error or missing entry results in **fail-closed** behavior. 

**Definition of Done:** 

- Usage integration tests are green; no duplicates observed in test scenarios. 
- RBAC tests cover all relevant allow/deny combinations, including error paths. 
- `docs/API_CONTRACTS.md` / `docs/NATS_SUBJECTS.md` briefly describe: 
  - which roles can trigger usage events, 
  - expected idempotent semantics. 

--- 

### Task W4‑E — Documentation & CI Hardening 

**Goal:** Align contracts/docs with implementation and add CI guards for observability and CP2 behavior. 

**Scope:** 

- Documentation: 

  - `docs/API_CONTRACTS.md`: 
    - examples for `traceparent`, 
    - strict schemas for relevant HTTP payloads, 
    - idempotency key (`tenant_id + id`) and TTL description. 
  - `docs/NATS_SUBJECTS.md` / `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`: 
    - ENV knobs for JetStream (ACK/retries), 
    - description of `NATS_SUBJECT_ALLOWLIST`, 
    - usage scenarios and payload outline for `beamline.usage.v1.metered`. 
  - File `CP2_READINESS_ROUTER_GATEWAY_RU.md`: 
    - table of key metrics and labels (including `cp_phase`, `tenant`, `subject`), 
    - how to interpret them for CP2 readiness / monitoring. 

- CI/CD: 

  - Add smoke checks for: 
    - `/metrics` endpoint (Prometheus scrape basic sanity). 
    - OTLP export (at least one trace successfully emitted). 
  - Ensure CI steps have clear exit codes and fail fast if: 
    - metrics endpoint is broken, 
    - OTLP pipeline is misconfigured. 

**Definition of Done:** 

- All relevant docs updated without duplicating existing canonical content; links to Vision & CP2 readiness preserved. 
- CI pipeline includes observability smoke checks and fails on regressions. 
- CP2 readiness doc reflects that Gateway observability and runtime behavior meet CP2-LC expectations. 

--- 

## 4. Exit criteria for ORDER-WRK-4-CP2-004 

This ORDER can be marked as **`status: done`** when: 

1. All tasks W4‑A … W4‑E meet their Definition of Done. 
2. CP2 readiness document (file `CP2_READINESS_ROUTER_GATEWAY_RU.md`) explicitly states: 
   - CP2 Gateway runtime and observability are **ready**, 
   - references to: 
     - sticky/idempotency behavior, 
     - NATS tracing and CP-phase metrics, 
     - usage tracking and RBAC behavior. 
3. CI pipeline includes and passes the new observability/idempotency smoke checks.