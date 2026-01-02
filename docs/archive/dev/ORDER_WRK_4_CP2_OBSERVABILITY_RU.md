---
version: 1.0
order_id: ORDER-WRK-4-CP2-003
from: mgr-2 (Architecture Manager)
to: wrk-4 (Gateway Lead)
created_at: 2025-01-27T15:00:00Z
status: pending
priority: MEDIUM
rule_version: v10
message_protocol: v1
---

# ORDER: CP2 Gateway Observability (Prometheus + OTLP)

## Order Information

**ORDER ID**: ORDER-WRK-4-CP2-003  
**From**: mgr-2 (Architecture Manager)  
**To**: wrk-4 (Gateway Lead)  
**Priority**: 🟡 **MEDIUM** - Enhances CP2 observability  
**Timeline**: 2 days (1 day per task)  
**Dependencies**: None (parallel safe)  
**Blocks**: None

## Task Description

Реализовать два компонента observability для Gateway: Prometheus metrics export (CP2.4) и OTLP trace export (CP2.5), согласно CP2 спецификации.

**Цель**: Обеспечить полную observability для Gateway через стандартные протоколы (Prometheus для metrics, OTLP для traces).

## Expected Artifacts

### Task CP2.4: Prometheus Metrics Export (Day 1)

#### New Files

**File**: `apps/gateway/src/observability/prometheus.controller.ts`

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

**File**: `apps/gateway/src/observability/prometheus.service.ts`

```typescript
import { Injectable } from '@nestjs/common';
import { Registry, collectDefaultMetrics, Counter, Histogram, Gauge } from 'prom-client';
import { IdempotencyService } from '../common/services/idempotency.service';

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

  constructor(private readonly idempotencyService: IdempotencyService) {
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

    this.activeConnections = new Gauge({
      name: 'gateway_active_connections',
      help: 'Number of active HTTP connections',
      registers: [this.registry]
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

#### Modified Files

**File**: `apps/gateway/src/app.module.ts`

```typescript
import { PrometheusController } from './observability/prometheus.controller';
import { PrometheusService } from './observability/prometheus.service';

@Module({
  controllers: [PrometheusController, /* ... existing controllers ... */],
  providers: [PrometheusService, /* ... existing providers ... */]
})
export class AppModule {}
```

**File**: `apps/gateway/package.json`

```json
{
  "dependencies": {
    "prom-client": "^15.1.0"
  }
}
```

### Task CP2.5: OTLP Trace Export (Day 2)

#### Modified Files

**File**: `apps/gateway/src/observability/tracing.service.ts`

```typescript
import { Injectable, OnModuleInit, OnModuleDestroy } from '@nestjs/common';
import { ConfigService } from '@nestjs/config';
import { NodeSDK } from '@opentelemetry/sdk-node';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-grpc';
import { Resource } from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';
import { getNodeAutoInstrumentations } from '@opentelemetry/auto-instrumentations-node';

@Injectable()
export class TracingService implements OnModuleInit, OnModuleDestroy {
  private sdk: NodeSDK;

  constructor(private readonly configService: ConfigService) {
    const otlpEndpoint = this.configService.get<string>(
      'OTLP_ENDPOINT',
      'http://localhost:4317'
    );

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

#### Configuration Files

**File**: `apps/gateway/.env.development`

```bash
# OTLP Configuration
OTLP_ENDPOINT=http://localhost:4317
```

**File**: `apps/gateway/.env.production`

```bash
# OTLP Configuration
OTLP_ENDPOINT=http://otel-collector.observability.svc.cluster.local:4317
```

**File**: `apps/gateway/src/config/env.validation.ts`

```typescript
OTLP_ENDPOINT: Joi.string().uri().default('http://localhost:4317')
```

## Context and Purpose

### Why This Is Important

1. **Standard Observability**: Prometheus и OTLP - стандартные протоколы для metrics и traces
2. **Production Ready**: Необходимо для production monitoring и debugging
3. **CP2 Compliance**: Требование CP2 спецификации для observability
4. **Integration**: Интеграция с существующими observability инструментами

### Current State

**Missing**: 
- ❌ Prometheus metrics export endpoint
- ❌ OTLP trace export

**Existing**:
- ✅ Idempotency service (metrics source)
- ✅ Tracing service (traces source, но без OTLP export)
- ✅ Rate limiting (metrics source)

### Target State

- ✅ `/metrics` endpoint возвращает Prometheus format
- ✅ Traces экспортируются через OTLP
- ✅ Все существующие metrics доступны через Prometheus
- ✅ Traces доступны в OpenTelemetry collector

## Technical Requirements

### Prometheus Metrics (CP2.4)

**Required Metrics**:
1. **Idempotency Metrics**:
   - `gateway_idempotency_hits_total` (Counter)
   - `gateway_idempotency_misses_total` (Counter)
   - `gateway_idempotency_evictions_total` (Counter)

2. **Rate Limiting Metrics**:
   - `gateway_rate_limit_hits_total` (Counter, labels: `tenant_id`, `endpoint`)

3. **HTTP Request Metrics**:
   - `gateway_http_request_duration_seconds` (Histogram, labels: `method`, `route`, `status_code`)

4. **Connection Metrics**:
   - `gateway_active_connections` (Gauge)

**Performance Requirements**:
- Response time < 100ms under normal load
- Metrics collection overhead < 5% CPU
- Memory usage < 50MB for metrics registry

### OTLP Trace Export (CP2.5)

**Required Configuration**:
- Configurable OTLP endpoint via environment variable
- Service resource attributes (name, version, namespace)
- Auto-instrumentation for HTTP requests
- Graceful shutdown without trace loss

**Performance Requirements**:
- Trace export success rate > 99%
- Export latency < 50ms (p95)
- Zero trace loss during graceful shutdown

## Acceptance Criteria

### CP2.4: Prometheus Export

- ✅ `/metrics` endpoint returns valid Prometheus format
- ✅ All existing Gateway metrics exported (idempotency, rate limiting)
- ✅ HTTP request duration histogram with proper labels
- ✅ Response time < 100ms under normal load
- ✅ Integrated into health check validation
- ✅ Unit tests for PrometheusService

### CP2.5: OTLP Export

- ✅ Traces exported via OpenTelemetry Protocol (OTLP)
- ✅ Configurable OTLP endpoint via environment variables
- ✅ Service resource attributes properly set
- ✅ Health check validates OTLP connection
- ✅ Zero trace loss during graceful shutdown
- ✅ Integration tests for OTLP export

### Integration

- ✅ Both features integrated into Gateway module
- ✅ Environment configuration documented
- ✅ Health checks include observability validation
- ✅ Documentation updated with usage examples

## Dependencies

### Required From

- **None**: Эти задачи могут выполняться параллельно с Router work

### External Dependencies

- `prom-client` package для Prometheus metrics
- `@opentelemetry/sdk-node` для OTLP export
- `@opentelemetry/exporter-trace-otlp-grpc` для gRPC OTLP export
- OpenTelemetry collector (для production, опционально для development)

## Risks and Mitigations

### Risk 1: Performance Overhead

**Risk**: Metrics collection и trace export могут замедлить Gateway.

**Mitigation**:
- Benchmark testing перед и после внедрения
- Configurable sampling rates для traces
- Async metrics collection где возможно
- Monitoring performance metrics

### Risk 2: OTLP Collector Dependency

**Risk**: OTLP export требует доступный OpenTelemetry collector.

**Mitigation**:
- Graceful degradation если collector недоступен
- Local development без collector (traces буферизуются)
- Clear error messages если collector недоступен
- Health check для OTLP connection

### Risk 3: Metrics Cardinality

**Risk**: Высокая кардинальность метрик может вызвать проблемы с памятью.

**Mitigation**:
- Ограничение labels на метриках
- Регулярная очистка старых метрик
- Monitoring memory usage
- Configurable metric retention

## Reporting Requirements

### Progress Report (Day 1 - CP2.4)

**Status**: `in_progress` или `done`

**Summary**:
- Prometheus controller и service реализованы
- Metrics endpoint работает
- Тесты пройдены

### Final Report (Day 2 - CP2.5)

**Status**: `done`

**Summary**:
- OTLP trace export реализован
- Оба observability компонента работают
- Интеграция завершена

**Artifacts**:
- Все созданные/обновленные файлы
- Результаты тестирования
- Performance benchmarks
- Обновленная документация

## References

- `docs/archive/dev/CP2_WORKER_ASSIGNMENTS_DETAILED.md` - Детальный план CP2 задач
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md` - CP2 readiness document
- `apps/gateway/src/common/services/idempotency.service.ts` - Idempotency service
- `apps/gateway/src/observability/tracing.service.ts` - Existing tracing service
- Prometheus documentation: https://prometheus.io/docs/
- OpenTelemetry documentation: https://opentelemetry.io/docs/

---

**ORDER ID**: ORDER-WRK-4-CP2-003  
**Status**: Pending  
**Priority**: 🟡 MEDIUM  
**Timeline**: 2 days (1 day per task)  
**Rule Version**: v10  
**Message Protocol**: v1

