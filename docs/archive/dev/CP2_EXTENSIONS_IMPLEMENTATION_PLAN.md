# Custom Provider Extensions Implementation Plan (CP2+)

**Status**: ✅ **COMPLETED** (CP2-LC)  
**Target CP**: CP2-LC  
**Estimated Duration**: 2-3 weeks  
**Prerequisites**: CP1-LC complete (Router + Gateway)  
**Reference**: ADR-023, Custom Provider Extensions Guide

---

## CI/CD Integration

### Test Suite Classification

#### CP2-LC Required Tests (Main CI)

These test suites are **mandatory** for CP2-LC and run in main CI pipelines:

1. **`router_extensions_pipeline_SUITE`** - Core pipeline functionality
   - Tests: pre/validators/post execution, error handling, fail-open/fail-closed
   - Status: ✅ Required for CP2-LC
   - CI: Runs in main, CP2 validation, Drone, GitLab CI

2. **`router_extensions_e2e_SUITE`** - End-to-end integration
   - Tests: Full pipeline with real NATS, extension services
   - Status: ✅ Required for CP2-LC
   - CI: Runs in main, CP2 validation, Drone, GitLab CI

3. **`router_extensions_security_SUITE`** - Security and abuse prevention
   - Tests: Authorization, payload validation, pipeline depth limits
   - Status: ✅ Required for CP2-LC
   - CI: Runs in main, CP2 validation, Drone, GitLab CI

4. **`router_extension_invoker_telemetry_SUITE`** - Observability
   - Tests: Telemetry events, structured logging, metrics
   - Status: ✅ Required for CP2-LC
   - CI: Runs in main, CP2 validation, Drone, GitLab CI

#### Pre-Release Tests (Extended CI)

These test suites are **optional** for CP2-LC but required for Pre-Release:

1. **`router_extensions_pipeline_load_SUITE`** - Performance and load testing
   - Tests: Latency, throughput, circuit breaker behavior, load profiles
   - Status: ⚠️ Pre-Release only (not blocking for CP2-LC)
   - CI: Runs in extended/load test pipelines, Pre-Release validation
   - Rationale: Load tests are resource-intensive and not required for basic functionality

2. **`router_extensions_chaos_SUITE`** - Chaos/resilience testing
   - Tests: NATS failures, extension flapping, latency degradation, mass degradation, recovery
   - Status: ⚠️ Pre-Release only (not blocking for CP2-LC)
   - CI: Runs in extended/chaos test pipelines, Pre-Release validation
   - Rationale: Chaos tests verify resilience boundaries and recovery mechanisms

### CI Configuration

**Main CI (CP2-LC)**:
- GitHub Actions: `router-full-test-suite.yml`, `validate-cp2.yml`
- Drone CI: `router-observability-tests` pipeline
- GitLab CI: `router-observability-tests` job

**Extended CI (Pre-Release)**:
- GitHub Actions: `router-load-tests.yml`
- Drone CI: `extensions-load-tests` step (non-blocking)
- GitLab CI: Load test jobs (non-blocking)

### Documentation Sync Check

**Script**: `scripts/check_extensions_docs_sync.sh`

**Validates**:
- All extension-related documentation files exist
- Documentation links are valid
- Test suites are referenced in CI configuration

**Runs in**:
- Pre-commit hooks (optional)
- CI validation pipelines
- Pre-Release validation

---

## CI Coverage Summary

| Test Suite | CP2-LC | Pre-Release | CI Location |
|------------|--------|--------------|-------------|
| `router_extensions_pipeline_SUITE` | ✅ Required | ✅ Required | Main CI |
| `router_extensions_e2e_SUITE` | ✅ Required | ✅ Required | Main CI |
| `router_extensions_security_SUITE` | ✅ Required | ✅ Required | Main CI |
| `router_extension_invoker_telemetry_SUITE` | ✅ Required | ✅ Required | Main CI |
| `router_extensions_pipeline_load_SUITE` | ⚠️ Optional | ✅ Required | Extended CI |
| `router_extensions_chaos_SUITE` | ⚠️ Optional | ✅ Required | Extended CI |

### CI Pipeline Details

**Main CI (CP2-LC Required)**:
- **GitHub Actions**: 
  - `router-full-test-suite.yml` - Includes `router_extensions_e2e_SUITE` in E2E tests
  - `validate-cp2.yml` - Validates all CP2-LC required extension test suites
- **Drone CI**: 
  - `router-observability-tests` pipeline - Runs all CP2-LC required extension test suites
- **GitLab CI**: 
  - `router-observability-tests` job - Runs all CP2-LC required extension test suites

**Extended CI (Pre-Release)**:
- **GitHub Actions**: 
  - `router-load-tests.yml` - Includes `router_extensions_pipeline_load_SUITE` (non-blocking)
  - `router-chaos-tests.yml` - Includes `router_extensions_chaos_SUITE` (non-blocking)
- **Drone CI**: 
  - `extensions-load-tests` step - Runs `router_extensions_pipeline_load_SUITE` (non-blocking)
  - `extensions-chaos-tests` step - Runs `router_extensions_chaos_SUITE` (non-blocking)
- **GitLab CI**: 
  - Load test jobs - Run `router_extensions_pipeline_load_SUITE` (non-blocking)
  - Chaos test jobs - Run `router_extensions_chaos_SUITE` (non-blocking)

### Documentation Sync Check

**Script**: `scripts/check_extensions_docs_sync.sh`

**Validates**:
- All extension-related documentation files exist
- Documentation links are valid
- Test suites are referenced in CI configuration
- Links to all created reports in `CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md`

**Runs in**:
- `validate-cp2.yml` workflow (GitHub Actions)
- Pre-commit hooks (optional)
- Pre-Release validation

---

---

## Extension Implementation Reports

All extension-related implementation reports and documentation:

### CP2/Pre-Release Steps Mapping Matrix

**Mapping**: Extension reports → CP2/Pre-Release implementation steps

| CP2/Pre-Release Step | Description | Extension Reports | Status | CP Phase |
|---------------------|-------------|-------------------|--------|----------|
| **Step 4.1** | Extensions Pipeline Verification | `EXTENSIONS_PIPELINE_CHECK_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Step 4.2** | Core Pipeline Implementation | `EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Step 4.3** | Integration Tests | `EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md`<br>`EXTENSIONS_PIPELINE_TESTS_ENHANCEMENT_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Step 4.4** | Pipeline Enhancement & Error Format | `EXTENSIONS_PIPELINE_ENHANCEMENT_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Registry Implementation** | Extension Registry (PostgreSQL + Mnesia) | `EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **E2E Integration** | E2E with real extension services | `EXTENSIONS_E2E_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Contract E2E** | Gateway ↔ Router contract tests | `EXTENSIONS_CONTRACT_E2E_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Performance Testing** | Load/performance tests | `EXTENSIONS_PIPELINE_PERF_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Chaos/Resilience** | Chaos engineering scenarios | `EXTENSIONS_CHAOS_RESILIENCE_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **UI Implementation** | Extension inspector UI | `EXTENSIONS_PIPELINE_UI_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Production Integration** | Production-ready integration | `EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md` | 🟡 **PARTIALLY COMPLETE** | Pre-Release |
| **Pipeline Complexity** | Complexity management | `PIPELINE_COMPLEXITY_MANAGEMENT_REPORT.md`<br>`PIPELINE_COMPLEXITY_MANAGEMENT_FINAL.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **CI/CD Integration** | CI/CD pipeline integration | `EXTENSIONS_CI_INTEGRATION_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Observability** | Telemetry and metrics | `EXTENSION_INVOKER_OBSERVABILITY_REPORT.md`<br>`EXTENSIONS_CP2_OBSERVABILITY_ALIGNMENT_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |
| **Advanced Features** | Circuit breaker, versioning, load balancing | `EXTENSION_ADVANCED_FEATURES_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Routing Strategy** | Multi-tenant routing | `EXTENSION_ROUTING_STRATEGY_IMPLEMENTATION_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Developer Experience** | DX tools and templates | `EXTENSION_TEMPLATE_IMPLEMENTATION_REPORT.md`<br>`EXTENSIONS_DEVELOPER_EXPERIENCE_REPORT.md` | ✅ **COMPLETED** (Pre-Release) | Pre-Release |
| **Documentation Sync** | Documentation synchronization | `EXTENSIONS_DOCUMENTATION_SYNC_REPORT.md` | ✅ **COMPLETED** (CP2-LC) | CP2-LC |

**Legend**:
- ✅ **COMPLETED**: Implementation complete and verified
- 🟡 **PARTIALLY COMPLETE**: Implementation in progress or partially complete
- ⚠️ **PRE-RELEASE ONLY**: Not required for CP2-LC, but required for Pre-Release

**CP Phase Classification**:
- **CP2-LC**: Required for CP2-LC checkpoint
- **Pre-Release**: Required for Pre-Release, optional for CP2-LC

---

### Core Implementation Reports

#### Step 4.1: Extensions Pipeline Verification

- `docs/archive/dev/EXTENSIONS_PIPELINE_CHECK_REPORT.md` - Initial pipeline implementation check
  - **Status**: ✅ **COMPLETED** (CP2-LC)
  - **Scope**: Verification of pipeline structure, registry, and execution flow
  - **Note**: Contains historical "NOT IMPLEMENTED" statuses in analysis section (pre-implementation state)

#### Step 4.2: Core Pipeline Implementation

- `docs/archive/dev/EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md` - Core pipeline implementation
  - **Status**: ✅ **COMPLETED** (CP2-LC)
  - **Scope**: Extension Registry, Extension Invoker, Policy Structure, Pipeline Execution

#### Step 4.3: Integration Tests

- `docs/archive/dev/EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md` - Integration tests (part of Step 4.2/4.3 report)
- `docs/archive/dev/EXTENSIONS_PIPELINE_TESTS_ENHANCEMENT_REPORT.md` - Enhanced pipeline tests
  - **Status**: ✅ **COMPLETED** (CP2-LC)
  - **Scope**: Post-processor tests, negative tests, property-like tests, helper functions

#### Step 4.4: Pipeline Enhancement & Error Format

- `docs/archive/dev/EXTENSIONS_PIPELINE_ENHANCEMENT_REPORT.md` - Pipeline enhancement with post-processors and unified error format
  - **Status**: ✅ **COMPLETED** (CP2-LC)
  - **Scope**: Post-processor execution, unified error format, context propagation

#### Extension Registry Implementation

- `docs/archive/dev/EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md` - Extension Registry implementation (PostgreSQL + Mnesia)
  - **Status**: ✅ **COMPLETED** (CP2-LC)
  - **Scope**: Database integration, cache layer, dual-mode support

#### E2E Integration

- `docs/archive/dev/EXTENSIONS_E2E_IMPLEMENTATION_REPORT.md` - E2E integration with real extension services
  - **Status**: ✅ **COMPLETED** (CP2-LC)
  - **Scope**: Full E2E tests with NATS, reference extensions, Gateway → Router → Extension → Provider flow

#### Contract E2E

- `docs/archive/dev/EXTENSIONS_CONTRACT_E2E_REPORT.md` - Contract E2E tests for Gateway ↔ Router
  - **Status**: ✅ **COMPLETED** (CP2-LC)
  - **Scope**: Error mapping, HTTP status codes, error DTO validation

### Pre-Release Reports

#### Performance Testing

- `docs/archive/dev/EXTENSIONS_PIPELINE_PERF_REPORT.md` - Performance and load testing report
  - **Status**: ✅ **COMPLETED** (Pre-Release)
  - **Scope**: Latency, throughput, circuit breaker behavior, SLO/SLA recommendations

#### Chaos/Resilience Testing

- `docs/archive/dev/EXTENSIONS_CHAOS_RESILIENCE_REPORT.md` - Chaos/resilience testing report
  - **Status**: ✅ **COMPLETED** (Pre-Release)
  - **Scope**: NATS failures, extension flapping, latency degradation, recovery scenarios

#### UI Implementation

- `docs/archive/dev/EXTENSIONS_PIPELINE_UI_IMPLEMENTATION_REPORT.md` - UI implementation for extensions
  - **Status**: ✅ **COMPLETED** (Pre-Release)
  - **Scope**: Extension inspector UI, policy viewing, registry viewing, debugging tools

#### Production Integration

- `docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md` - Production integration report
  - **Status**: 🟡 **PARTIALLY COMPLETE** (Pre-Release)
  - **Scope**: Production-ready integration, real API endpoints, UI integration

#### Pipeline Complexity Management

- `docs/archive/dev/PIPELINE_COMPLEXITY_MANAGEMENT_REPORT.md` - Pipeline complexity management
- `docs/archive/dev/PIPELINE_COMPLEXITY_MANAGEMENT_FINAL.md` - Pipeline complexity management final report
  - **Status**: ✅ **COMPLETED** (Pre-Release)
  - **Scope**: Soft-limits, warnings, complexity assessment, UI hints

### CI/CD Integration

- `docs/archive/dev/EXTENSIONS_CI_INTEGRATION_REPORT.md` - CI/CD integration report
  - **Status**: ✅ **COMPLETED** (CP2-LC)
  - **Scope**: Test suite classification, CI configuration, documentation sync check

### Observability

- `docs/archive/dev/EXTENSION_INVOKER_OBSERVABILITY_REPORT.md` - Extension invoker observability
- `docs/archive/dev/EXTENSIONS_CP2_OBSERVABILITY_ALIGNMENT_REPORT.md` - CP2 observability alignment
  - **Status**: ✅ **COMPLETED** (CP2-LC)
  - **Scope**: Telemetry events, structured logging, CP2 OTEL/Prometheus alignment

### Advanced Features

- `docs/archive/dev/EXTENSION_ADVANCED_FEATURES_REPORT.md` - Advanced extension features
  - **Status**: ✅ **COMPLETED** (Pre-Release)
  - **Scope**: Circuit breaker, health monitoring, versioning, load balancing

### Architecture and API Documentation

- `docs/EXTENSIONS_API.md` - Extensions API specification
- `docs/ARCHITECTURE/EXTENSION_ROUTING_STRATEGY.md` - Extension routing strategy
- `docs/archive/dev/EXTENSION_ROUTING_STRATEGY_IMPLEMENTATION_REPORT.md` - Routing strategy implementation
  - **Status**: ✅ **COMPLETED** (Pre-Release)
  - **Scope**: Multi-tenant routing, environment-based routing, canary deployments

### Operational Guides

- `apps/otp/router/docs/EXTENSIONS_RUNBOOK.md` - Extension Registry and Extensions operations runbook
- `apps/otp/router/docs/EXTENSIONS_SECURITY_GUIDE.md` - Extensions security guide
- `apps/otp/router/docs/EXTENSIONS_E2E_GUIDE.md` - E2E testing guide

### Developer Guides

- `docs/EXTENSIONS_DEVELOPER_GUIDE.md` - Developer guide for creating extensions
- `docs/archive/dev/EXTENSION_TEMPLATE_IMPLEMENTATION_REPORT.md` - Extension template implementation
- `docs/archive/dev/EXTENSIONS_DEVELOPER_EXPERIENCE_REPORT.md` - Developer experience improvements
  - **Status**: ✅ **COMPLETED** (Pre-Release)
  - **Scope**: Extension templates, boilerplate generation, local run scripts, E2E test scripts

---

## Overview

Детальный 3-фазный план реализации Custom Provider Extensions architecture:

1. **Phase 1**: Extension Registry (PostgreSQL + Mnesia)
2. **Phase 2**: Router Integration
3. **Phase 3**: Reference Implementations

---

## Phase 1: Extension Registry (Week 1, Days 1-3)

### Task 1.1: PostgreSQL Schema

**File**: `sql/011_extensions_registry.sql`

**Таблицы:**
- `extensions` — основной реестр расширений
- `extension_audit_log` — аудит изменений
- `extension_health` — метрики здоровья

**Key Features:**
- Типы: `pre`, `validator`, `post`, `provider`
- NATS subject per extension
- Timeout и retry настройки
- JSONB config для гибкости
- Seed data для примеров

**Tasks:**
- [ ] Создать `sql/011_extensions_registry.sql`
- [ ] Добавить seed data (mock_provider, openai_gpt4, anthropic_claude)
- [ ] Тест миграции локально
- [ ] Добавить в CI pipeline
- [ ] Документировать schema

**Deliverables:**
- SQL migration файл
- Schema документация в `docs/ARCHITECTURE/extensions-registry-schema.md`

---

### Task 1.2: Mnesia Cache Layer

**File**: `apps/otp/router/src/router_extension_registry.erl`

**Функциональность:**
- Загрузка из PostgreSQL в Mnesia
- API: `lookup/1`, `lookup_by_type/1`, `reload/0`
- Периодическая синхронизация (каждую минуту)
- Cache invalidation при изменениях

**Record Structure:**
```erlang
-record(extension, {
    id, type, subject, timeout_ms, retry, 
    enabled, config, metadata
}).
```

**Tasks:**
- [ ] Создать `router_extension_registry.erl`
- [ ] Добавить в `beamline_router_sup.erl`
- [ ] Реализовать PostgreSQL query helper
- [ ] Unit tests для cache operations
- [ ] Integration tests для sync

**Deliverables:**
- `router_extension_registry.erl` module
- Unit и integration tests
- Performance benchmarks (cache lookup < 1ms)

---

### Task 1.3: Admin API для управления Extensions

**File**: `apps/otp/router/src/router_admin_extensions.erl`

**CRUD Operations:**
- `list_extensions/0` — список всех
- `get_extension/1` — детали одного
- `create_extension/1` — создание
- `update_extension/2` — обновление
- `delete_extension/1` — удаление
- `enable_extension/1` / `disable_extension/1` — вкл/выкл

**Security:**
- RBAC проверки (только admin роли)
- Audit logging в `extension_audit_log`
- Input validation

**Tasks:**
- [ ] Создать `router_admin_extensions.erl`
- [ ] Интеграция с `router_grpc_sup`
- [ ] RBAC checks
- [ ] Audit logging
- [ ] REST API wrapper (optional)
- [ ] Admin UI endpoints

**Deliverables:**
- Admin API module
- gRPC/REST endpoints
- API documentation
- Postman/curl examples

---

## Phase 2: Router Integration (Week 1, Days 4-5)

### Task 2.1: Extension Invocation Module

**File**: `apps/otp/router/src/router_extension_invoker.erl`

**Core Functions:**
- `invoke/3` — generic extension invocation
- `invoke_provider/3` — provider-specific wrapper
- `invoke_with_retry/6` — retry logic
- `record_success/2`, `record_failure/2` — health tracking

**Features:**
- NATS request-reply pattern
- Configurable timeout/retry
- Trace context propagation
- Health metrics collection
- Circuit breaker (optional)

**Tasks:**
- [ ] Создать `router_extension_invoker.erl`
- [ ] Implement NATS request-reply
- [ ] Add OpenTelemetry spans
- [ ] Prometheus metrics
- [ ] Circuit breaker pattern
- [ ] Unit tests with NATS mock
- [ ] Integration tests with real NATS

**Deliverables:**
- Extension invoker module
- Metrics dashboard config
- Performance benchmarks

---

### Task 2.2: Routing Policy Integration

**File**: Update `apps/otp/router/src/router_decider.erl`

**New Policy Actions:**
- `extension_pre` — pre-processor step
- `extension_validator` — validation step
- `select_provider` — provider selection (updated)

**Pipeline Flow:**
```
Request → Pre-processors → Validators → Provider → Response
```

**Example Policy:**
```json
{
  "steps": [
    {"action": "extension_pre", "extension_id": "normalize_text"},
    {"action": "extension_validator", "extension_id": "pii_guard"},
    {"action": "select_provider", "provider_id": "openai_gpt4"}
  ],
  "fallback": ["anthropic_claude", "mock_provider"]
}
```

**Tasks:**
- [ ] Update `router_decider.erl`
- [ ] Add `apply_extensions_pipeline/3`
- [ ] Policy schema validation
- [ ] Update `docs/ROUTING_POLICY.md`
- [ ] Integration tests (full pipeline)
- [ ] Performance benchmarks

**Deliverables:**
- Updated router_decider
- Policy examples
- Integration tests
- Documentation

---

## Phase 3: Reference Implementations (Week 2)

### Task 3.1: Mock Provider (Erlang)

**Location**: `providers/mock-provider-erlang/`

**Purpose**: Testing и development

**Features:**
- Echo request в response
- Configurable latency
- Fake usage metrics
- Trace context propagation

**Tasks:**
- [ ] Создать provider структуру
- [ ] Implement mock logic
- [ ] Dockerfile
- [ ] docker-compose integration
- [ ] Integration tests

**Deliverables:**
- Working mock provider
- Docker image
- Integration tests

---

### Task 3.2: OpenAI Provider (Go)

**Location**: `providers/openai-provider-go/`

**Purpose**: Production-ready OpenAI adapter

**Features:**
- OpenAI API client
- Environment config
- Error handling
- Rate limiting (client-side)
- Metrics

**Tasks:**
- [ ] Go project setup
- [ ] OpenAI API integration
- [ ] NATS subscription
- [ ] Error handling
- [ ] Dockerfile
- [ ] K8s manifests
- [ ] Integration tests

**Deliverables:**
- Working OpenAI provider
- Docker image
- K8s deployment configs
- Documentation

---

### Task 3.3: Anthropic Provider (Rust) — Optional

**Location**: `providers/anthropic-provider-rust/`

**Purpose**: Demonstrate multi-language support

**Features:**
- Anthropic API client
- High performance
- Safety guarantees

**Tasks:**
- [ ] Rust project setup
- [ ] Anthropic API integration
- [ ] NATS subscription
- [ ] Dockerfile
- [ ] Documentation

**Deliverables:**
- Working Anthropic provider
- Docker image
- Documentation

---

## Testing Strategy

### Unit Tests
- Extension Registry cache operations
- Extension invoker retry logic
- Policy pipeline parsing

### Integration Tests
- PostgreSQL ↔ Mnesia sync
- Router ↔ Extension NATS communication
- Full pipeline: Request → Extensions → Provider → Response

### Load Tests
- Extension Registry performance (>10K lookups/sec)
- Provider throughput (>1000 requests/sec)
- End-to-end latency (<200ms p99)

### Chaos Tests
- Extension timeout handling
- Provider failure + fallback
- NATS connection loss recovery
- Extension instance flapping
- Latency degradation
- Mass degradation of multiple extensions
- Recovery after failure

**Test Suite**: `router_extensions_chaos_SUITE`
**Report**: `docs/archive/dev/EXTENSIONS_CHAOS_RESILIENCE_REPORT.md`

---

## Monitoring & Observability

### Metrics (Prometheus)

**Extension Registry:**
- `extension_registry_size` — количество extensions
- `extension_registry_reload_duration_seconds`
- `extension_registry_reload_errors_total`

**Extension Invocation:**
- `extension_requests_total{extension_id, status}`
- `extension_request_duration_seconds{extension_id}`
- `extension_failures_total{extension_id, reason}`

**Provider Specific:**
- `provider_requests_total{provider_id, status}`
- `provider_latency_seconds{provider_id, percentile}`
- `provider_tokens_total{provider_id}`

### Logs (JSON Structured)

**Required Fields:**
- `timestamp`, `level`, `extension_id`, `trace_id`, `tenant_id`
- `latency_ms`, `status`, `error` (if failed)

### Tracing (OpenTelemetry)

**Spans:**
- `router.extension.invoke` — extension call
- `router.extension.{type}` — по типу (pre/validator/post/provider)
- `provider.{id}.api_call` — external API call

---

## Deployment

### Development (docker-compose)

```yaml
services:
  mock-provider:
    build: ./providers/mock-provider-erlang
    environment:
      - NATS_URL=nats://nats:4222
    depends_on:
      - nats
```

### Staging/Production (Kubernetes)

**Per-provider deployment:**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: openai-provider
spec:
  replicas: 3
  selector:
    matchLabels:
      app: openai-provider
  template:
    spec:
      containers:
      - name: openai-provider
        image: beamline/openai-provider:v1.0
        env:
        - name: NATS_URL
          value: "nats://nats:4222"
        - name: OPENAI_API_KEY
          valueFrom:
            secretKeyRef:
              name: openai-secret
              key: api-key
```

---

## Documentation Deliverables

1. **Architecture Documentation**
   - `docs/ARCHITECTURE/extensions-registry-schema.md`
   - `docs/ARCHITECTURE/extension-invocation-flow.md`

2. **Operational Guides**
   - `docs/archive/dev/EXTENSION_REGISTRY_OPERATIONS.md`
   - `docs/archive/dev/PROVIDER_DEVELOPMENT_GUIDE.md`

3. **API Reference**
   - Extension Registry Admin API
   - Provider Extension Contract Specification

4. **Examples**
   - Policy templates with extensions
   - Provider implementation examples (Go, Erlang, Rust)

---

## Success Criteria

### Functional
- ✅ Extension Registry operational (PostgreSQL + Mnesia)
- ✅ Router can invoke extensions via NATS
- ✅ At least 2 reference providers working (mock + OpenAI)
- ✅ Admin API для управления extensions

### Performance
- ✅ Extension lookup < 1ms (99th percentile)
- ✅ End-to-end request < 200ms (without provider latency)
- ✅ System handles >1000 req/sec

### Quality
- ✅ Test coverage > 80%
- ✅ All documentation complete
- ✅ Monitoring dashboards operational

---

## Timeline Summary

| Phase | Duration | Key Deliverables |
|-------|----------|------------------|
| Phase 1 | 3 days | Extension Registry (PostgreSQL + Mnesia + Admin API) |
| Phase 2 | 2 days | Router Integration (Invoker + Policy pipeline) |
| Phase 3 | 5 days | Reference Implementations (Mock + OpenAI + optional Anthropic) |
| **Total** | **10 days** | **Full Custom Provider Extensions support** |

---

## Risk Mitigation

### Technical Risks
- **NATS timeout handling** → Implement circuit breaker
- **PostgreSQL sync lag** → Periodic reload + cache warmup
- **Extension failures** → Fallback providers + graceful degradation

### Operational Risks
- **Provider API keys** → Kubernetes secrets + rotation policy
- **Rate limiting** → Client-side limits + monitoring
- **Cost tracking** → Usage metrics per provider

---

## Next Steps After CP2

### CP3+ Enhancements
- Extension versioning (v1 → v2 migrations)
- Hot reload extensions without Router restart
- Extension marketplace (community providers)
- Advanced routing (ML-based provider selection)

---

**Related Documents:**
- ADR-023: Remove apps/otp/provider in Favor of Custom Provider Extensions
- Custom Provider Extensions Guide
- Extensions API Documentation
- Routing Policy Specification
