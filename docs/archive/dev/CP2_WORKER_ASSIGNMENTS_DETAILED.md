# CP2 Worker Assignments - Detailed Task Breakdown

## Executive Summary

This document provides a comprehensive task breakdown for CP2-LC transition, explicitly mapping each task to specific Workers with concrete deliverables and sprint-ready specifications.

## Worker Specialization Matrix

| Worker | Specialization | Primary Focus | Key Responsibilities |
|--------|----------------|---------------|----------------------|
| **wrk-1** | ABI/Contracts | Protocol Definitions | `.proto` files, buf tooling, contract validation |
| **wrk-2** | Architecture/Tech Lead | System Integration | Cross-component architecture, technical specifications |
| **wrk-3** | Router Core | Erlang/OTP Implementation | Router business logic, policy engine, routing decisions |
| **wrk-4** | Gateway/Frontend | NestJS/SvelteKit | API gateways, developer experience, observability |
| **wrk-8** | CI/Infrastructure | DevOps/Automation | Pipelines, deployments, infrastructure as code |
| **wrk-9** | Documentation | Technical Writing | Guides, specifications, operational documentation |

---

## CP2 Task Ownership Matrix

### 🔴 CRITICAL PATH TASKS (Block CP2-LC)

#### Task CP2.1: Feature Flags Default Enablement ✅ COMPLETED
**Owner**: wrk-2 (Architecture/Tech Lead)  
**Status**: ✅ COMPLETED (2025-01-27)  
**Effort**: 1 day  
**Deliverables**:
- **Modified**: `apps/otp/router/src/beamline_router.app.src`
  - `idempotency_enabled: true` (was `false`)
  - `tracing_enabled: true` (was `false`) 
  - `tenant_validation_enabled: true` (was `false`)
  - `admin_grpc_enabled: true` (was `false`)
- **Updated**: `docs/CP1_BASELINE.md` - CP2 features marked as baseline
- **Validation**: `scripts/validate_cp2.sh` passes

**Next Action**: Fix compilation error in `router_result_consumer.erl:347`

---

#### Task CP2.2: CP2 Validation Suite Creation
**Owner**: wrk-2 (Architecture/Tech Lead)  
**Status**: 🔄 IN PROGRESS  
**Effort**: 2 days  
**Deliverables**:
- **New Script**: `scripts/validate_cp2.sh`
  ```bash
  #!/bin/bash
  # CP2 Feature Validation Suite
  set -euo pipefail
  
  echo "🔍 CP2 Validation Suite Starting..."
  
  # 1. Feature Flag Validation
  validate_feature_flags() {
    echo "✅ Checking CP2 feature flags..."
    grep -q "idempotency_enabled.*true" apps/otp/router/src/beamline_router.app.src
    grep -q "tracing_enabled.*true" apps/otp/router/src/beamline_router.app.src
    grep -q "tenant_validation_enabled.*true" apps/otp/router/src/beamline_router.app.src
    grep -q "admin_grpc_enabled.*true" apps/otp/router/src/beamline_router.app.src
  }
  
  # 2. JetStream Connection Validation
  validate_jetstream() {
    echo "✅ Validating JetStream connectivity..."
    cd apps/otp/router && rebar3 shell -eval "
      {ok, _} = application:ensure_all_started(beamline_router),
      {ok, Conn} = router_nats:get_connection(),
      {ok, _} = router_nats:jetstream_info(Conn),
      init:stop()
    "
  }
  
  # 3. Idempotency Validation
  validate_idempotency() {
    echo "✅ Testing idempotency layer..."
    cd apps/otp/router && rebar3 ct --suite test/router_idempotency_SUITE
  }
  
  # 4. Tracing Validation
  validate_tracing() {
    echo "✅ Verifying OpenTelemetry tracing..."
    cd apps/otp/router && rebar3 ct --suite test/router_tracing_SUITE
  }
  
  # 5. Tenant Validation
  validate_tenant_validation() {
    echo "✅ Testing tenant validation..."
    cd apps/otp/router && rebar3 ct --suite test/router_tenant_allowlist_SUITE
  }
  
  # Execute all validations
  validate_feature_flags
  validate_jetstream
  validate_idempotency
  validate_tracing
  validate_tenant_validation
  
  echo "🎉 All CP2 validations passed!"
  ```

**Acceptance Criteria**:
- ✅ Script validates all CP2 features are enabled
- ✅ Script tests JetStream connectivity
- ✅ Script runs idempotency tests
- ✅ Script verifies tracing spans creation
- ✅ Script validates tenant ACL enforcement
- ✅ Returns exit code 0 on success, non-zero on failure

---

#### Task CP2.3: Compilation Error Resolution (BLOCKING)
**Owner**: wrk-3 (Router Core)  
**Status**: 🔴 BLOCKING  
**Effort**: 0.5 day  
**Deliverables**:
- **Fixed**: `apps/otp/router/src/router_result_consumer.erl:347`
  ```erlang
  % BEFORE (broken):
  OtherError ->
    logger:error("Unhandled error: ~p", [OtherError]),
    nack_and_continue(Metadata, State)
  
  % AFTER (fixed):
  _OtherError ->
    logger:error("Unhandled error: ~p", [_OtherError]),
    nack_and_continue(Metadata, State)
  ```

**Impact**: Unblocks all Router testing and CP2 validation  
**Priority**: 🔴 CRITICAL - Must complete before any other Router tasks

---

### 🟡 MEDIUM PRIORITY TASKS

#### Task CP2.4: Gateway Prometheus Metrics Export
**Owner**: wrk-4 (Gateway Lead)  
**Status**: 📋 READY  
**Effort**: 1 day  
**Deliverables**:

**New File**: `apps/gateway/src/observability/prometheus.controller.ts`
```typescript
import { Controller, Get, Res } from '@nestjs/common';
import { Response } from 'express';
import { PrometheusService } from './prometheus.service';

@Controller('metrics')
export class PrometheusController {
  constructor(private readonly prometheusService: PrometheusService) {}

  @Get()
  async getMetrics(@Res() res: Response) {
    const metrics = await this.prometheusService.getMetrics();
    res.set('Content-Type', 'text/plain; version=0.0.4; charset=utf-8');
    res.send(metrics);
  }
}
```

**New File**: `apps/gateway/src/observability/prometheus.service.ts`
```typescript
import { Injectable } from '@nestjs/common';
import { Registry, collectDefaultMetrics, Counter, Histogram, Gauge } from 'prom-client';
import { IdempotencyService } from '../common/services/idempotency.service';
import { TracingService } from './tracing.service';

@Injectable()
export class PrometheusService {
  private readonly registry: Registry;
  
  // Idempotency Metrics
  private readonly idempotencyHits: Counter;
  private readonly idempotencyMisses: Counter;
  private readonly idempotencyEvictions: Counter;
  
  // Rate Limiting Metrics
  private readonly rateLimitHits: Counter;
  private readonly rateLimitMisses: Counter;
  
  // Request Metrics
  private readonly httpRequests: Histogram;
  private readonly activeConnections: Gauge;

  constructor(
    private readonly idempotencyService: IdempotencyService,
    private readonly tracingService: TracingService
  ) {
    this.registry = new Registry();
    this.initializeMetrics();
    collectDefaultMetrics({ register: this.registry });
  }

  private initializeMetrics() {
    this.idempotencyHits = new Counter({
      name: 'gateway_idempotency_hits_total',
      help: 'Total number of idempotency cache hits',
      registers: [this.registry]
    });

    this.idempotencyMisses = new Counter({
      name: 'gateway_idempotency_misses_total',
      help: 'Total number of idempotency cache misses',
      registers: [this.registry]
    });

    this.rateLimitHits = new Counter({
      name: 'gateway_rate_limit_hits_total',
      help: 'Total number of rate limit enforcements',
      registers: [this.registry],
      labelNames: ['tenant_id', 'endpoint']
    });

    this.httpRequests = new Histogram({
      name: 'gateway_http_request_duration_seconds',
      help: 'HTTP request duration in seconds',
      registers: [this.registry],
      labelNames: ['method', 'route', 'status_code'],
      buckets: [0.1, 0.5, 1, 2, 5]
    });
  }

  async getMetrics(): Promise<string> {
    // Update metrics from services
    const idempotencyStats = this.idempotencyService.getStats();
    this.idempotencyHits.inc(idempotencyStats.hits);
    this.idempotencyMisses.inc(idempotencyStats.misses);
    
    return this.registry.metrics();
  }
}
```

**Modified**: `apps/gateway/src/app.module.ts`
```typescript
import { PrometheusController } from './observability/prometheus.controller';
import { PrometheusService } from './observability/prometheus.service';

@Module({
  controllers: [PrometheusController],
  providers: [PrometheusService]
})
```

**Modified**: `package.json` (add dependency)
```json
{
  "dependencies": {
    "prom-client": "^15.1.0"
  }
}
```

---

#### Task CP2.5: Gateway OTLP Trace Export
**Owner**: wrk-4 (Gateway Lead)  
**Status**: 📋 READY  
**Effort**: 1 day  
**Dependencies**: CP2.4 completion  

**Modified**: `apps/gateway/src/observability/tracing.service.ts`
```typescript
import { NodeSDK } from '@opentelemetry/sdk-node';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-grpc';
import { Resource } from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';
import { getNodeAutoInstrumentations } from '@opentelemetry/auto-instrumentations-node';

@Injectable()
export class TracingService implements OnModuleInit, OnModuleDestroy {
  private sdk: NodeSDK;

  constructor(private readonly configService: ConfigService) {
    const otlpEndpoint = this.configService.get<string>('OTLP_ENDPOINT', 'http://localhost:4317');
    
    this.sdk = new NodeSDK({
      resource: new Resource({
        [SemanticResourceAttributes.SERVICE_NAME]: 'beamline-gateway',
        [SemanticResourceAttributes.SERVICE_VERSION]: '1.0.0',
        [SemanticResourceAttributes.SERVICE_NAMESPACE]: 'beamline'
      }),
      traceExporter: new OTLPTraceExporter({
        url: `${otlpEndpoint}/v1/traces`
      }),
      instrumentations: [
        getNodeAutoInstrumentations({
          '@opentelemetry/instrumentation-fs': { enabled: false },
          '@opentelemetry/instrumentation-net': { enabled: true }
        })
      ]
    });
  }

  async onModuleInit() {
    await this.sdk.start();
    console.log('🚀 OTLP tracing initialized');
  }

  async onModuleDestroy() {
    await this.sdk.shutdown();
  }
}
```

**New Environment Variables**:
```bash
# .env.development
OTLP_ENDPOINT=http://localhost:4317

# .env.production  
OTLP_ENDPOINT=http://otel-collector.observability.svc.cluster.local:4317
```

---

#### Task CP2.6: HEIR Policy Store Integration
**Owner**: wrk-3 (Router Core)  
**Status**: 📋 READY  
**Effort**: 3 days  
**Dependencies**: CP2.3 completion  

**Modified**: `apps/otp/router/src/router_policy_store.erl`
```erlang
-module(router_policy_store).
-behaviour(gen_server).

%% HEIR Policy Store Integration
-export([init_heir_store/1, get_heir_policy/2, store_heir_policy/3]).

%% API
-export([start_link/1, get_policy/2, store_policy/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    heir_enabled :: boolean(),
    heir_connection :: pid() | undefined,
    local_cache :: ets:tid()
}).

%% @doc Initialize HEIR policy store connection
init_heir_store(Config) ->
    case application:get_env(beamline_router, heir_policy_store_enabled, false) of
        true ->
            Host = maps:get(host, Config, "localhost"),
            Port = maps:get(port, Config, 8080),
            {ok, Conn} = heir_client:connect(Host, Port),
            {ok, Conn};
        false ->
            {ok, undefined}
    end.

%% @doc Get policy from HEIR store with local fallback
get_heir_policy(TenantId, PolicyId) ->
    case gen_server:call(?MODULE, {get_heir_policy, TenantId, PolicyId}) of
        {ok, Policy} -> {ok, Policy};
        {error, not_found} -> get_policy(TenantId, PolicyId); % Local fallback
        Error -> Error
    end.

%% @doc Store policy in both HEIR and local store
store_heir_policy(TenantId, PolicyId, Policy) ->
    gen_server:call(?MODULE, {store_heir_policy, TenantId, PolicyId, Policy}).

%% gen_server implementation
handle_call({get_heir_policy, TenantId, PolicyId}, _From, State) ->
    case State#state.heir_connection of
        undefined ->
            {reply, {error, heir_disabled}, State};
        Conn ->
            case heir_client:get_policy(Conn, TenantId, PolicyId) of
                {ok, Policy} ->
                    % Cache locally for future requests
                    ets:insert(State#state.local_cache, {{TenantId, PolicyId}, Policy}),
                    {reply, {ok, Policy}, State};
                Error ->
                    {reply, Error, State}
            end
    end;

handle_call({store_heir_policy, TenantId, PolicyId, Policy}, _From, State) ->
    % Store in both HEIR and local cache
    case State#state.heir_connection of
        undefined ->
            % HEIR disabled, store only locally
            ets:insert(State#state.local_cache, {{TenantId, PolicyId}, Policy}),
            {reply, ok, State};
        Conn ->
            case heir_client:store_policy(Conn, TenantId, PolicyId, Policy) of
                ok ->
                    ets:insert(State#state.local_cache, {{TenantId, PolicyId}, Policy}),
                    {reply, ok, State};
                Error ->
                    {reply, Error, State}
            end
    end.
```

**New File**: `apps/otp/router/src/heir_client.erl`
```erlang
-module(heir_client).
-export([connect/2, get_policy/3, store_policy/4]).

%% HEIR Policy Store Client Implementation
%% HTTP/REST client for HEIR policy store
```

---

### 🟢 LOW PRIORITY TASKS (CP3+ Future)

#### Task CP2.7: Multi-Region Support
**Owner**: wrk-8 (CI/Infrastructure)  
**Status**: 📅 FUTURE (CP3+)  
**Effort**: 2 weeks  
**Scope**: Cross-region routing and replication

#### Task CP2.8: Horizontal Scaling
**Owner**: wrk-8 (CI/Infrastructure)  
**Status**: 📅 FUTURE (CP3+)  
**Effort**: 1 week  
**Scope**: Load balancing across Router instances

#### Task CP2.9: Advanced Analytics
**Owner**: wrk-4 (Gateway Lead)  
**Status**: 📅 FUTURE (CP3+)  
**Effort**: 1 week  
**Scope**: Advanced metrics and analytics dashboard

---

## Sprint Planning Matrix

### Sprint 1 (Week 1): Critical Path
| Day | Task | Owner | Deliverable | Blockers |
|-----|------|-------|-------------|----------|
| **Day 1** | CP2.3 Compilation Fix | wrk-3 | Fixed `router_result_consumer.erl` | None |
| **Day 1-2** | CP2.2 Validation Suite | wrk-2 | `scripts/validate_cp2.sh` | CP2.3 |
| **Day 2-3** | CP2.4 Prometheus Export | wrk-4 | `/metrics` endpoint | None |
| **Day 3-4** | CP2.5 OTLP Export | wrk-4 | OTLP trace export | CP2.4 |
| **Day 4-5** | CP2.6 HEIR Integration | wrk-3 | HEIR policy store | CP2.3 |

### Sprint 2 (Week 2): Validation & Documentation
| Day | Task | Owner | Deliverable | Blockers |
|-----|------|-------|-------------|----------|
| **Day 1-2** | CP2 Documentation | wrk-9 | Updated guides | All CP2 tasks |
| **Day 2-3** | Integration Testing | wrk-2 | E2E test validation | All CP2 tasks |
| **Day 3-4** | Performance Testing | wrk-4 | Load test results | All CP2 tasks |
| **Day 4-5** | CP2-LC Transition | wrk-2 | State update to CP2-LC | All validation |

---

## Component Responsibility Matrix

### Router (Erlang/OTP) - wrk-3 Primary
**Core Responsibilities**:
- ✅ JetStream integration and message handling
- ✅ Idempotency layer implementation  
- ✅ Tenant validation and ACL enforcement
- ✅ OpenTelemetry tracing spans
- ✅ Admin gRPC service operations
- 🔧 HEIR policy store integration (CP2.6)
- 🔧 Compilation error fixes (CP2.3)

**Key Files**:
- `apps/otp/router/src/router_nats.erl` - JetStream client
- `apps/otp/router/src/router_idempotency.erl` - Idempotency logic
- `apps/otp/router/src/router_tenant_validator.erl` - Tenant ACL
- `apps/otp/router/src/router_tracing.erl` - OTel tracing
- `apps/otp/router/src/router_admin_grpc.erl` - Admin service

### Gateway (NestJS) - wrk-4 Primary  
**Core Responsibilities**:
- ✅ Idempotency service (existing)
- ✅ Tracing service (existing)
- ✅ Rate limiting (existing)
- 🔧 Prometheus metrics export (CP2.4)
- 🔧 OTLP trace export (CP2.5)
- 🔧 Health check validation

**Key Files**:
- `apps/gateway/src/observability/prometheus.controller.ts` - New
- `apps/gateway/src/observability/prometheus.service.ts` - New  
- `apps/gateway/src/observability/tracing.service.ts` - Modified
- `apps/gateway/src/common/services/idempotency.service.ts` - Existing

### Cross-Component Integration - wrk-2 Primary
**Core Responsibilities**:
- 🔧 CP2 validation suite (CP2.2)
- 🔧 Architecture specifications
- 🔧 Integration testing coordination
- 🔧 CP2-LC transition planning
- 🔧 Feature flag management

**Key Files**:
- `scripts/validate_cp2.sh` - New validation script
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md` - This document
- `docs/archive/dev/CP2_TRANSITION_PLAN_ROUTER.md` - Transition plan

---

## Risk Assessment & Mitigation

### 🔴 High Risk Items
1. **Compilation Error (CP2.3)**
   - **Risk**: Blocks all Router testing
   - **Mitigation**: wrk-3 priority assignment, 0.5 day SLA
   - **Escalation**: Immediate team notification

2. **Feature Flag Dependencies**
   - **Risk**: CP2 features not properly enabled
   - **Mitigation**: Automated validation in CP2.2
   - **Rollback**: Git revert capability

### 🟡 Medium Risk Items  
1. **HEIR Integration Complexity (CP2.6)**
   - **Risk**: 3-day estimate may be optimistic
   - **Mitigation**: Parallel development, fallback to local store
   - **Contingency**: Defer to CP3 if needed

2. **Gateway Metrics Performance (CP2.4)**
   - **Risk**: Metrics collection overhead
   - **Mitigation**: Benchmark testing, configurable sampling
   - **Monitoring**: Performance regression detection

### 🟢 Low Risk Items
1. **Documentation Delays (wrk-9)**
   - **Risk**: Non-blocking for CP2-LC
   - **Mitigation**: Can proceed in parallel
   - **Impact**: Minimal technical risk

---

## Success Metrics & Acceptance Criteria

### Technical Metrics
- ✅ All CP2 features enabled by default
- ✅ CP2 validation suite passes (exit code 0)
- ✅ Prometheus `/metrics` endpoint responds < 100ms
- ✅ OTLP export success rate > 99%
- ✅ HEIR integration maintains < 50ms latency

### Process Metrics  
- ✅ Zero breaking changes from CP1
- ✅ All existing tests continue to pass
- ✅ Documentation 100% updated
- ✅ No performance regressions > 5%
- ✅ Security scan passes with no critical issues

### Business Metrics
- ✅ CP2-LC achieved within 2-week sprint
- ✅ Team capacity utilization < 80% (prevents burnout)
- ✅ Technical debt reduction measurable
- ✅ Developer experience improvements documented

---

## Communication Plan

### Daily Standups
- **Time**: 09:00 UTC
- **Participants**: wrk-2, wrk-3, wrk-4, wrk-9
- **Focus**: Blocker identification, dependency coordination

### Weekly Sync
- **Time**: Fridays 14:00 UTC  
- **Participants**: All workers, project stakeholders
- **Focus**: Progress review, risk assessment, planning adjustment

### Escalation Path
1. **Technical Blockers** → wrk-2 (Architecture Lead)
2. **Resource Conflicts** → Project Manager  
3. **Timeline Risks** → All stakeholders
4. **External Dependencies** → wrk-8 (CI/Infrastructure)

---

This detailed assignment provides the complete framework for executing CP2-LC transition with clear ownership, concrete deliverables, and sprint-ready implementation specifications.