# CP2 Checklist (Detailed)

**Alignment Note**: `docs/archive/dev/CP2_CHECKLIST_ALIGNMENT_NOTE.md` - Verification of alignment with CP2 Wave 1 plans

---

## CP2 Execution Plan (Tasks → WORKER → Artifacts → Tests → ETA)

| Task                          | WORKER        | Key artifacts                                                                 | Tests                                                               | ETA (net) | Status |
| ----------------------------- | ------------- | ----------------------------------------------------------------------------- | ------------------------------------------------------------------- | --------- | ------ |
| JetStream durability & redelivery | wrk-1 (Router/OTP) | `router_jetstream.erl`, JetStream config, DLQ subjects                          | `router_jetstream_SUITE.erl` (durability, redelivery, DLQ)         | 3-5 days | ✅ COMPLETE |
| Idempotency layer             | wrk-1 (Router/OTP) | `router_idem.erl`, ETS tables, config keys                                     | `router_idem_SUITE.erl` (hits/misses/TTL)                           | 3-5 days | ✅ COMPLETE |
| ACL (tenant/roles)           | wrk-1 (Router/OTP) | `router_acl.erl`, policy config, audit log integration                         | `router_acl_SUITE.erl` (allow/deny matrix, audit events)           | 3-4 days | ✅ COMPLETE |
| Circuit Breaker (Policy DSL) | wrk-1 (Router/OTP) | `router_circuit_breaker.erl`, CB integration, metrics, docs                    | `router_circuit_breaker_SUITE.erl`, integration tests               | 3-5 days | ✅ COMPLETE |
| Admin gRPC                    | wrk-2 (Gateway/C)  | `admin_grpc.c`, RouterAdmin client, config                                     | `admin_grpc_test.c` (health, status, RBAC)                         | 3-5 days | ✅ COMPLETE (see `docs/GATEWAY_ADMIN_GRPC_STATUS.md`) |
| Headers propagation           | wrk-2 (Gateway/C)  | `router_headers.erl`, `headers.c`, docs                                        | E2E propagation tests (REST → NATS → CAF → Usage)                | 2-4 days | ✅ COMPLETE (see `docs/GATEWAY_HEADERS_PROPAGATION.md`) |
| Observability expansion       | wrk-3 (Obs/Docs)   | `router_observability.erl`, `router_prometheus.erl`, `router_metrics_http.erl`, updated logging/tracing config, dashboards         | `router_observability_SUITE.erl`, `router_metrics_dump_SUITE.erl`, OBS1/CP2 profile                  | 3-5 days | ✅ COMPLETE (CP1 baseline complete, **Minimal OTel spans done**, **Prometheus metrics implemented**, dashboards/alerts deferred) |
| CI DevState gates             | wrk-1 (Schemas/CI) | `devstate_verify.sh`, CI workflow                                              | CI pipeline checks (DevState verify, state/history artifacts)      | 1-2 days | ✅ COMPLETE (CP2) / ⚠️ DEFERRED (CP3: pre-commit/pre-push hooks) |

> ETA указаны как чистое инженерное время при наличии подготовленной среды и без параллельных блокеров.

## JetStream durability & redelivery ✅ COMPLETE

**Status**: ✅ **COMPLETE** - Implemented in `router_jetstream.erl`, tested and verified

- Acceptance criteria:
  - ✅ Durable consumer configured; ACK/NAK semantics verified
  - ✅ MaxDeliver enforced; backoff schedule applied; DLQ policy defined
  - ✅ p95 ack latency ≤ target; redelivery bounded per policy
- Implemented artifacts:
  - Router: `apps/otp/router/src/router_jetstream.erl` (MaxDeliver tracking, backoff, DLQ)
  - Tests: `apps/otp/router/test/router_jetstream_SUITE.erl` (durability, redelivery, DLQ, metrics, telemetry)
  - Metrics: `router_jetstream_ack_total`, `router_redelivery_total`, `router_dlq_total`
  - Integration: `router_nats.erl` (real NATS client integration)
  - Documentation: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` (DLQ subjects, MaxDeliver config)

### JetStream durability & redelivery - implementation details

- **Цель (wrk-1 / Router/OTP)**
  - Гарантировать, что решающие и результатные события:
    - не теряются при рестартах Router;
    - имеют ограниченное количество повторных доставок (MaxDeliver);
    - при исчерпании политики попадают в DLQ с трассируемым контекстом.

- **Основные модули и точки расширения**
  - `apps/otp/router/src/router_jetstream.erl` (новый модуль-фасад над NATS JetStream):
    - `start_link/0`  - регистрация процесса и инициализация JetStream client.
    - `subscribe_decide/1`  - обертка над `router_nats:subscribe_jetstream/5` для `beamline.router.v1.decide`.
    - `subscribe_results/1`  - подписка на `caf.exec.result.v1`.
    - `subscribe_acks/1`  - подписка на `caf.exec.assign.v1.ack` (если включено).
    - `ack/1`, `nak/2`, `term/1`  - явные операции ACK/NAK/terminal NAK с метриками.
  - Интеграция с существующими консьюмерами:
    - `router_decide_consumer.erl`:
      - заменить/перенастроить прямые вызовы `router_nats:subscribe_jetstream/5` на `router_jetstream:subscribe_decide/1`.
      - добавить учёт Retry/Redelivery в логе и метриках.
    - `router_result_consumer.erl`:
      - использовать `router_jetstream:subscribe_results/1`.
      - при ошибках в обработчике результата  - публиковать DLQ и вызывать `nak` с backoff.
    - `router_ack_consumer.erl`:
      - использовать `router_jetstream:subscribe_acks/1`.

- **DLQ и политика MaxDeliver**
  - Конфигурация (в `beamline_router.app.src`):
    - `decide_subject`, `result_subject`, `ack_subject`  - уже заданы.
    - добавить/уточнить:
      - `{js_max_deliver_decide, 5}`, `{js_max_deliver_results, 10}`.
      - `{js_backoff_decide, [1000, 5000, 15000]}` (мс).
      - `{dlq_subject_pattern, <<"~s.dlq">>}`.
  - Реализация DLQ (повторное использование `router_intake_error_handler.erl` либо новый helper в `router_jetstream.erl`):
    - `build_dlq_subject(Subject) -> binary().`
    - `publish_dlq(Subject, Payload, Headers) -> ok | {error, term()}.`

- **Метрики (экспорт из `router_metrics.erl`)**
  - Счётчики:
    - `router_jetstream_ack_total{subject="beamline.router.v1.decide"}`.
    - `router_jetstream_ack_total{subject="caf.exec.result.v1"}`.
    - `router_redelivery_total{subject=..., reason="max_deliver"|"error"}`.
    - `router_dlq_total{subject=..., source_subject=...}`.
  - Гистограммы/summary (при необходимости):
    - `router_js_ack_latency_ms` (p95/p99 в OBS2 профиле).

- **Тесты: `apps/otp/router/test/router_jetstream_SUITE.erl`**
  - **test_durable_subscription_survives_restart/1**
    - GIVEN: сообщение опубликовано до рестарта Router.
    - WHEN: Router рестартует, JetStream consumer пересоздаётся.
    - THEN: сообщение всё равно доставляется один раз; ack  - ok; счётчики ack_total увеличиваются.
  - **test_redelivery_until_ack_or_maxdeliver/1**
    - GIVEN: обработчик искусственно возвращает ошибку N раз.
    - WHEN: MaxDeliver=3, backoff=[1s,5s].
    - THEN: 
      - сообщение доставляется 3 раза;
      - после 3-й ошибки уходит в DLQ;
      - `router_redelivery_total{reason="error"} == 2`;
      - `router_dlq_total == 1`.
  - **test_dlq_payload_contains_context/1**
    - Проверяет, что DLQ-сообщение содержит:
      - `trace_id`, `tenant_id`, `original_subject`, `error_code`.
  - **test_ack_latency_within_target/1**
    - GIVEN: нормальная нагрузка (N сообщений).
    - WHEN: все обрабатываются без ошибок.
    - THEN: p95 ack latency (по метрике или замеру в тесте)  - целевому порогу из CP2 профиля.

- **Связанные артефакты и документация**
  - Обновить `docs/NATS_SUBJECTS.md` для раздела о DLQ и MaxDeliver.
  - Обновить `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`  - добавить примеры DLQ payload.
  - Добавить краткий подпункт в `docs/CP2_CHECKLIST.md` (этот документ) со ссылками на:
    - `router_jetstream.erl`, `router_jetstream_SUITE.erl`;
    - OBS2 профиль метрик/alert-правил.

## Idempotency layer ✅ COMPLETE

**Status**: ✅ **COMPLETE** - Implemented in `router_idem.erl`, tested and verified

- Acceptance criteria:
  - ✅ ETS cache with TTL; duplicate suppression by `idempotency_key`
  - ✅ Keys propagation across Router → Gateway; eviction policy tested
  - ✅ p95 duplicate handling latency ≤ target; zero double-execution in tests
- Implemented artifacts:
  - Router: `apps/otp/router/src/router_idem.erl` (ETS ordered_set, TTL cleanup, mass evict, protection against ETS growth)
  - Tests: `apps/otp/router/test/router_idem_SUITE.erl` (TTL edge cases, mass cleanup, concurrent operations, metrics, telemetry)
  - Metrics: `router_idem_hits_total`, `router_idem_miss_total`
  - Telemetry: Events for hit, miss, expired, remember, cleanup, table_size
  - Documentation: `docs/archive/dev/IDEMPOTENCY_IMPLEMENTATION_REPORT.md`

## ACL (Tenant Validation) ✅ COMPLETE

**Status**: ✅ **COMPLETE** - Implemented in `router_tenant_validator.erl`, tested and verified

- Acceptance criteria:
  - ✅ Tenant allow/deny lists; role permissions enforced
  - ✅ Audit entries created on deny; policy updates are atomic and logged
- Implemented artifacts:
  - Router: `apps/otp/router/src/router_tenant_validator.erl`
  - Tests: Integration tests (PASS)
  - Metrics: `router_results_tenant_rejected_total`, `router_acks_tenant_rejected_total`, `router_tenant_audit_total`

**Reference**: `docs/archive/dev/CP2_ROUTER_PLAN.md`

## Circuit Breaker (Policy DSL) ✅ COMPLETE

**Status**: ✅ **COMPLETE** - Implementation, optimization, and production readiness complete

**Completed**:
- ✅ State machine (Closed/Open/Half-Open) with ETS storage
- ✅ CB check before provider selection (fail-fast when open)
- ✅ Integration with retry/backoff and fallbacks
- ✅ Configuration parsing from policy JSON
- ✅ Sliding window for error-rate calculation (CP2-OPT-1)
- ✅ Optimized timeout transitions (CP2-OPT-1)
- ✅ Enhanced metrics and logging (CP2-OPT-2)
- ✅ Provider callbacks integration (`record_success/record_failure` after provider calls)
- ✅ Documentation and observability specification
- ✅ Load tests (`router_circuit_breaker_load_SUITE.erl`)
- ✅ Rollout plan (`CIRCUIT_BREAKER_ROLLOUT_PLAN.md`)
- ✅ Dashboard specification (`CIRCUIT_BREAKER_DASHBOARD_SPEC.md`)
- ✅ Production operations guide (`CIRCUIT_BREAKER_PRODUCTION_NOTES.md`)

**Implemented artifacts**:
- Router: `apps/otp/router/src/router_circuit_breaker.erl` (state machine, sliding window, optimized transitions)
- Integration: `apps/otp/router/src/router_decider.erl` (CB check)
- Integration: `apps/otp/router/src/router_policy_applier.erl` (state initialization)
- Integration: `apps/otp/router/src/router_result_consumer.erl` (provider callbacks)
- Parsing: `apps/otp/router/src/router_policy_store.erl` (configuration parsing)
- Tests: `apps/otp/router/test/router_circuit_breaker_SUITE.erl` (unit tests)
- Tests: `apps/otp/router/test/router_circuit_breaker_integration_SUITE.erl` (integration tests)
- Tests: `apps/otp/router/test/router_circuit_breaker_load_SUITE.erl` (load tests - 4 scenarios)
- Fixtures: `apps/otp/router/priv/fixtures/policies/default_tenant/circuit_breaker_*.json`
- Schema: `apps/otp/router/docs/schemas/policy.schema.json` (CB block)
- Metrics: `router_circuit_breaker_events_total`, `router_circuit_breaker_state_transitions_total`, `router_circuit_breaker_error_rate`, `router_circuit_breaker_window_requests_total`, `router_circuit_breaker_window_failures_total`, `router_circuit_breaker_timeout_transitions_total`, `router_circuit_breaker_timeout_remaining_ms`
- Documentation: `docs/ROUTING_POLICY.md`, `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md`, `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md`, `docs/archive/dev/CIRCUIT_BREAKER_ROLLOUT_PLAN.md`, `docs/archive/dev/CIRCUIT_BREAKER_DASHBOARD_SPEC.md`, `docs/archive/dev/CIRCUIT_BREAKER_LOAD_TEST_REPORT.md`, `docs/archive/dev/CIRCUIT_BREAKER_PRODUCTION_NOTES.md`
- Documentation:
  - `docs/ROUTING_POLICY.md` - CB specification (updated)
  - `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Design document (updated with implementation status)
  - `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` - Observability specification (NEW)
  - `docs/archive/dev/CP2_FIRST_FEATURE_SELECTION.md` - Feature selection and checklist
  - `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Alert rules (updated)

**Reference**: `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md`, `docs/ROUTING_POLICY.md`, `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md`, `docs/archive/dev/CIRCUIT_BREAKER_OPTIMIZATION_PRIORITIES.md`

## Rate Limiting (Policy DSL) ✅ COMPLETE

**Status**: ✅ **COMPLETE** - Implemented in `router_rate_limit_store.erl`, tested and verified

- Acceptance criteria:
  - ✅ Token bucket algorithm with burst support
  - ✅ Per-policy and per-tenant rate limiting
  - ✅ Enabled/disabled flag support
  - ✅ Metrics emission (router_rate_limit_allowed_total, router_rate_limit_exceeded_total)
  - ✅ Retry-After calculation
  - ✅ Clock skew protection (max(0, TimeElapsed))
  - ⚠️ Global scope (deferred to CP2+)
  - ⚠️ Multi-level checks (global → tenant → policy) (deferred to CP2+)
- Implemented artifacts:
  - Router: `apps/otp/router/src/router_rate_limit_store.erl` (token bucket implementation)
  - Router: `apps/otp/router/src/router_policy_store.erl` (rate limit parsing from Policy JSON)
  - Router: `apps/otp/router/src/router_policy_applier.erl` (rate limit check integration)
  - Tests: `apps/otp/router/test/router_rate_limit_store_SUITE.erl` (unit tests: allowed, exceeded, burst, refill, disabled, reset, status, concurrent, config_change, scope_isolation, error_handling, restart_behavior)
  - Tests: `apps/otp/router/test/router_rate_limit_e2e_SUITE.erl` (E2E tests)
  - Documentation: `../apps/otp/router/docs/dev/RATE_LIMIT_INVARIANTS.md` (invariants)
  - Documentation: `../apps/otp/router/docs/dev/RATE_LIMIT_IMPLEMENTATION_AUDIT.md` (implementation audit)
- Metrics: `router_rate_limit_allowed_total{scope, tenant_id}`, `router_rate_limit_exceeded_total{scope, tenant_id}`
- Integration: Rate limit check before provider selection in `router_policy_applier.erl`
- **Reference**: `docs/ROUTING_POLICY.md`, `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md`, `docs/archive/dev/RATE_LIMIT_BOUNDARIES_ROUTER_VS_GATEWAY.md`

Admin gRPC ✅ COMPLETE

**Status**: ✅ **COMPLETE** - Implemented in `admin_grpc.c`, tested and verified

- Acceptance criteria:
  - ✅ Management endpoints expose status, RBAC-protected operations
  - ✅ Health and telemetry endpoints respond with expected schemas
- Implemented artifacts:
  - Gateway: `apps/c-gateway/src/admin_grpc.c` (health, status, auth, metrics handlers)
  - Gateway: `apps/c-gateway/src/admin_grpc.h` (header file)
  - Tests: `apps/c-gateway/tests/admin_grpc_test.c` (positive and negative scenarios, deterministic tests)
  - Metrics: `gateway_admin_requests_total`, `gateway_admin_errors_total`
  - Documentation: `apps/c-gateway/docs/ADMIN_GRPC_API.md` (API contract, synchronization with Router Admin API)
  - Documentation: `docs/GATEWAY_ADMIN_GRPC_STATUS.md` - **Current implementation status** (actual vs planned)
  - Integration: NATS connection check, API key validation, role-based authorization (admin, operator, viewer)
- **Current Status**: See `docs/GATEWAY_ADMIN_GRPC_STATUS.md` for detailed implementation status, gaps, and roadmap

## Headers propagation ✅ COMPLETE

**Status**: ✅ **COMPLETE** - Headers propagation implemented and verified

- Acceptance criteria:
  - ✅ `trace_id`, `span_id`, `tenant_id` propagate Router → Gateway → Worker
  - ✅ Correlation verified in logs and traces
  - ⚠️ Cross-component span linkage (deferred to CP3/Pre-Release for full E2E tests)
- Implemented artifacts:
  - Gateway: `apps/c-gateway/src/http_server.c` (header extraction: X-Tenant-ID, X-Trace-ID, traceparent)
  - Gateway: `apps/c-gateway/src/http_server.c` (build_route_request_json: propagation to Router via JSON)
  - Router: `apps/otp/router/src/router_decide_consumer.erl` (headers handling)
  - Router: `apps/otp/router/src/router_result_consumer.erl` (headers handling)
  - Router: `apps/otp/router/src/router_ack_consumer.erl` (headers handling)
  - Router: `apps/otp/router/src/router_caf_adapter.erl` (headers propagation)
  - Router: `apps/otp/router/src/router_tracing.erl` (trace context)
  - Proto: `apps/otp/router/src/flow_pb.erl` (trace_id field)
  - Tests: Integration tests in consumer suites
  - Metrics: `ctx_missing_headers_total` (if implemented)
  - Documentation: `docs/GATEWAY_HEADERS_PROPAGATION.md` - **Current implementation status** (actual headers processed)
- **Current Status**: See `docs/GATEWAY_HEADERS_PROPAGATION.md` for detailed header extraction, propagation rules, and gaps
- **Reference**: `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` (Headers Propagation section)
- **Deferred to CP3/Pre-Release**: Full E2E propagation tests (REST → NATS → CAF → Usage)

Observability expansion ✅ COMPLETE

**Status**: ✅ **COMPLETE** - OTel spans implemented (CP2 wrk-3), Prometheus metrics implemented (Wave 1), dashboards/alerts deferred

**Router OTel Spans (COMPLETE)**:
- ✅ OTel span for routing decision (`beamline.router.route`) in `router_core.erl`
- ✅ Nested span for policy application (`beamline.router.policy.apply`) in `router_policy_applier.erl`
- ✅ Nested span for provider selection (`beamline.router.provider.select`) in `router_decider.erl`
- ✅ Trace context propagation (extraction from headers/context)
- ✅ Span attributes with CP1 correlation fields (tenant_id, policy_id, message_id)
- ✅ Span status and error handling
- ✅ Integration tests in `router_observability_SUITE.erl`

**Note**: No further action needed for Router OTel spans. Further observability work described in Wave-1/Wave-2 docs.

- Acceptance criteria:
  - ✅ **OTel spans for routing**: OTel spans added for routing decision (`beamline.router.route`)
  - ✅ **OTel nested spans**: Nested spans for policy application (`beamline.router.policy.apply`) and provider selection (`beamline.router.provider.select`)
  - ✅ **Trace context propagation**: Context extraction and propagation to nested spans
  - ✅ **CP1 correlation fields**: Span attributes include tenant_id, policy_id, message_id
  - ✅ **Integration tests**: Tests in `router_observability_SUITE.erl` verify span creation and attributes
  - ✅ **CP1 correlation attributes**: Spans include trace_id, run_id, flow_id, step_id, tenant_id, subject
  - ✅ **Trace propagation**: Trace context propagated from headers to spans (headers priority over payload)
  - ✅ **Prometheus metrics implemented**: HTTP `/metrics` endpoint on port 9001, Prometheus text format with HELP/TYPE headers
  - ✅ **Base metrics contract**: JetStream, Idempotency, ACL metrics exported
  - ⚠️ **Dashboards and alerts deferred**: Dashboards and alerts definitions prepared (deferred to release infra)
- Implemented artifacts (CP2):
  - Router: `apps/otp/router/src/router_observability.erl` (minimal scope: decide + result spans)
  - Router: `apps/otp/router/src/router_prometheus.erl` (Prometheus text format export)
  - Router: `apps/otp/router/src/router_metrics_http.erl` (HTTP `/metrics` endpoint on port 9001)
  - Tests: `apps/otp/router/test/router_observability_SUITE.erl` (otel_tests group: decide/result spans, CP1 attributes, trace propagation)
  - Tests: `apps/otp/router/test/router_metrics_dump_SUITE.erl` (Prometheus format validation, metric names, labels, invariants)
  - Integration: Uses existing `router_tracing.erl` for OpenTelemetry API
  - Documentation: `apps/otp/router/docs/OBSERVABILITY.md` (updated with CP2 Prometheus metrics section)
- **CP2 Implementation**:
  - **Included**: OTel spans for decide and result handling paths with CP1 correlation attributes
  - **Included**: Trace propagation from headers (W3C Trace Context format)
  - **Included**: Prometheus metrics export via HTTP `/metrics` endpoint (port 9001)
  - **Included**: Base metrics contract (JetStream: `router_jetstream_ack_total`, `router_redelivery_total`, `router_dlq_total`; Idempotency: `router_idem_hits_total`, `router_idem_miss_total`; ACL: `router_acl_allowed_total`, `router_acl_denied_total`)
  - **Included**: Prometheus text format with HELP and TYPE headers
  - **Excluded**: Dashboards, alerts (deferred to release infra)
- **Related Documents**:
  - `docs/archive/dev/OBSERVABILITY_METRICS_SPEC_CP2.md` - CP2 Prometheus metrics specification
  - `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md` - Wave 1 Observability (additional metrics for all components)
  - `docs/archive/dev/CP2_OBSERVABILITY_BACKLOG.md` - Complete Observability backlog
  - `apps/otp/router/docs/OBSERVABILITY.md` - Router observability documentation (updated with CP2 metrics)
  - **Note**: CP2_CHECKLIST includes Router OTel spans and Prometheus metrics. Wave 1 adds additional metrics for all components (complementary)

## CI DevState gates ✅ **COMPLETE** (CP2) / ⚠️ **DEFERRED TO CP3** (Pre-commit/pre-push hooks)

**Status**: ✅ **COMPLETE** (CP2) - All CI pipelines verified and active; Pre-commit/pre-push hooks deferred to CP3/Pre-Release

- Acceptance criteria:
  - ✅ **CI pipelines**: DevState verify runs in all target CI workflows (verified and active)
  - ✅ **CI exports artifacts**: `devstate_export.sh` exists and is used in CI
  - ✅ **HMAC chain validation**: Enforced in all CI pipelines (verified and active)
  - ⚠️ **Pre-commit/pre-push hooks**: Deferred to CP3/Pre-Release (documentation exists, manual setup required)
- Implemented artifacts (CP2 - ✅ COMPLETE):
  - Scripts: `devstate-tools/devstate/scripts/devstate_verify.sh` - Comprehensive validation (JSON schema, checksums, HMAC chain via API)
    - Validates JSON schema via `scripts/validate_state.sh`
    - Validates artifact checksums
    - Verifies HMAC chain via DevState API (`/v1/devstate/verify`)
    - Falls back to local validation if API unavailable
  - Scripts: `devstate-tools/devstate/scripts/devstate_export.sh` - State/history export
  - Scripts: `scripts/validate_state.sh` - State validation wrapper (used by devstate_verify.sh)
  - Scripts: `scripts/verify_hmac_chain.py` - HMAC chain verification (Python, legacy)
  - CI: `.github/workflows/ci.yml` - DevState verify step (✅ Active, line 104-115)
    - Uses `devstate_verify.sh` for comprehensive validation
    - Runs before merge (required step)
  - CI: `.github/workflows/state-validation.yml` - DevState sync and validation (✅ Active, line 42-46)
  - CI: `.github/workflows/release.yml` - DevState gates for releases (✅ Active, line 25-31)
  - CI: `.github/workflows/router-full-test-suite.yml` - DevState gates for router tests (✅ Active, line 108-119)
  - Rules: `.cursor/rules/agents/devstate-integration.mdc` - DevState integration rules
- **CI Verification Status** (CP2):
  - ✅ All target CI workflows include DevState verify step
  - ✅ All steps are active (not commented, not optional)
  - ✅ Main CI workflow (`.github/workflows/ci.yml`) uses `devstate_verify.sh` for comprehensive validation
  - ✅ `devstate_verify.sh` performs JSON schema validation, checksums verification, and HMAC chain verification
  - ✅ HMAC secret configured via `BEAMLINE_HMAC_SECRET` secret in GitHub Actions
  - ✅ DevState verify blocks merge on validation failures
- **Deferred to CP3/Pre-Release**:
  - ⚠️ Pre-commit hooks: Not implemented (documentation exists in `devstate-tools/devstate/docs/IDE_INTEGRATION.md`)
  - ⚠️ Pre-push hooks: Not implemented (documentation exists, manual setup required)
  - **Note**: Hooks are documented but require manual installation (not automated in CP2)
- **Reference**: `docs/archive/dev/CP2_CHECKLIST_GAP_REPORT.md` (CI DevState Gates section - updated with verification results)
- **Related Documents**:
  - `docs/archive/dev/CP2_WAVE1_CI_INTEGRATION_PLAN.md` - Wave 1 CI integration plan
  - `.cursor/rules/agents/wrk-1-schemas-manifest-ci-gates.mdc` - wrk-1 CI gates responsibility
  - `devstate-tools/devstate/docs/IDE_INTEGRATION.md` - IDE integration guide (includes pre-push hook example)
  - **Note**: DevState gates are wrk-1 (Schemas, Manifest & CI Gates) responsibility

---

## Alignment with CP2 Wave 1 Plans

This checklist focuses on **Router and Gateway features** (JetStream, Idempotency, ACL, Admin gRPC, Headers, Router Observability, CI DevState gates).

**CP2 Wave 1** focuses on **Worker Reliability and Observability** (separate scope):
- **Worker Reliability Wave 1**: CAF Worker retry/timeout/queue improvements (wrk-3 Worker Reliability)
- **Observability Wave 1**: Prometheus metrics for all components (wrk-obs1)

**Key Points**:
- ✅ **No Scope Overlap**: CP2_CHECKLIST tasks (Router/Gateway) are separate from Wave 1 tasks (Worker Reliability + Observability)
- ✅ **Complementary**: CP2_CHECKLIST and Wave 1 address different aspects
- ⚠️ **Role Clarification**: 
  - CP2_CHECKLIST "wrk-3 (Obs/Docs)" = Router observability (OTel spans) - **Router Core** responsibility
  - Wave 1 "wrk-3" = Worker Reliability (CAF Worker) - **Worker** responsibility (different worker)
  - Wave 1 "wrk-obs1" = Observability metrics (all components) - **Observability** responsibility
  - CI DevState gates should be **wrk-1** (Schemas, Manifest & CI Gates) responsibility

**Related Documents**:
- `docs/archive/dev/CP2_CHECKLIST_ALIGNMENT_NOTE.md` - Detailed alignment verification
- `docs/archive/dev/CP2_WORKER_RELIABILITY_WAVE1.md` - Worker Reliability Wave 1 specification
- `docs/archive/dev/CP2_OBSERVABILITY_WAVE1.md` - Observability Wave 1 specification
- `docs/archive/dev/CP2_WAVE1_PRODUCT_SUMMARY.md` - Wave 1 product summary
- `docs/archive/dev/CP2_WAVE1_CI_INTEGRATION_PLAN.md` - Wave 1 CI integration plan

## CP2 Features E2E Integration Test ✅ COMPLETE

**Status**: ✅ **COMPLETE** - Single source CP2 integration verification

- Acceptance criteria:
  - ✅ Tests complete flow: Gateway → Router → Worker → Usage
  - ✅ Verifies: JetStream, Idempotency, ACL, Circuit Breaker
  - ✅ No double-execution, redelivery limits, ACL enforcement, CB state transitions
- Implemented artifacts:
  - **Test Suite**: `apps/otp/router/test/router_cp2_features_e2e_SUITE.erl` ⭐ **Single source CP2 verification**
  - **Integration**: Tests all CP2 features in one E2E flow
  - **Metrics Verification**: All CP2 feature metrics verified
- **CI Integration**: 
  - Included in `.github/workflows/ci.yml` (Router CI job)
  - Included in `scripts/run_router_full_test_suite.sh` (E2E tests section)
- **Reference**: `docs/archive/dev/CP2_ACCEPTANCE_REPORT.md` - Main integration test: `router_cp2_features_e2e_SUITE.erl`

---

## How to Verify Everything at Once

### Option 1: CP2 Self-Check Script (Recommended for Local Verification)

```bash
# Quick CP2 verification (single source test)
bash scripts/cp2_self_check.sh

# Full pre-release regression profile
bash scripts/cp2_self_check.sh --pre-release
```

**This script runs the same tests as CI and provides a concise summary.**

### Option 2: Single CP2 Integration Test (Direct)

**For CP2 verification, run the single source CP2 integration test**:

```bash
cd apps/otp/router
rebar3 ct --suite router_cp2_features_e2e_SUITE
```

**This suite (`router_cp2_features_e2e_SUITE.erl`) is the single source of truth for CP2 integration verification.**

### Option 3: Full Test Suite Script

```bash
bash scripts/run_router_full_test_suite.sh --e2e-only
```

This runs all E2E tests including:
- Standard E2E tests (`router_intake_e2e_SUITE.erl`)
- Extensions E2E tests (`router_extensions_e2e_SUITE.erl`)
- **CP2 Features E2E tests** (`router_cp2_features_e2e_SUITE.erl`) ⭐

### Option 4: Pre-Release Regression Profile

For Pre-Release regression testing, use the pre-release profile:

```bash
bash scripts/run_router_full_test_suite.sh --pre-release
```

This runs:
- JetStream E2E
- Idempotency + RateLimit combo
- CP2 Features E2E ⭐
- Chaos scenario (extensions chaos if available, otherwise intake chaos)

### CI Integration

- **Main CI**: `.github/workflows/ci.yml` - Runs `router_cp2_features_e2e_SUITE` in Router CI job
- **Full Test Suite**: `.github/workflows/router-full-test-suite.yml` - Runs all E2E tests (scheduled/manual)
  - Manual trigger with `test_suite: pre-release` option for Pre-Release regression profile

**Reference**: `docs/CP2_READINESS_SUMMARY.md` - Quick reference for CP2 verification
