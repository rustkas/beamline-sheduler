---
version: 1.0
order_id: ORDER-WRK-4-AUTH-001
from: mgr-2 (Architecture Manager)
to: wrk-4 (Gateway Lead)
created_at: 2025-01-27T12:00:00Z
status: pending
rule_version: v10
message_protocol: v1
---

# ORDER: Router-Gateway Authentication Integration Implementation

## Order Information

**ORDER ID**: ORDER-WRK-4-AUTH-001  
**From**: mgr-2 (Architecture Manager)  
**To**: wrk-4 (Gateway Lead)  
**Priority**: HIGH  
**Timeline**: 3-4 weeks (6 phases)  
**Dependencies**: 
- wrk-2 (Architecture/Tech Lead) - ✅ Architecture specification completed
- wrk-3 (Router Core) - Router Admin gRPC endpoints must be available

## Task Description

Реализовать унифицированную систему аутентификации и авторизации между Gateway (NestJS) и Router (Erlang/OTP) через Router Admin gRPC сервис, согласно архитектурной спецификации от wrk-2.

**Цель**: Устранить дублирование логики между Gateway и Router, создать единый источник правды для аутентификации, авторизации, RBAC, квот и rate limits.

## Expected Artifacts

### Phase 1: Infrastructure Setup (Week 1)

**Artifacts**:
1. `apps/gateway/src/router-admin/router-admin-client.interface.ts` - Interface для Router Admin gRPC client
2. `apps/gateway/src/router-admin/router-admin-client.service.ts` - Реализация gRPC client
3. `apps/gateway/src/router-admin/router-admin.module.ts` - NestJS module для Router Admin
4. Generated TypeScript types from `proto/beamline/flow/v1/flow.proto`
5. `apps/gateway/src/config/router-admin.config.ts` - Конфигурация Router Admin

**Requirements**:
- Использовать `@grpc/grpc-js` или `@nestjs/microservices`
- Генерация TypeScript типов из proto файлов
- Connection pooling и retry logic
- Circuit breaker для надежности
- TLS поддержка для production

### Phase 2: Auth Integration (Week 1-2)

**Artifacts**:
1. `apps/gateway/src/auth/router-auth.service.ts` - Реализация RouterAuthService
2. `apps/gateway/src/auth/unified-auth-context.service.ts` - Реализация UnifiedAuthContextService
3. `apps/gateway/src/auth/auth.module.ts` - Обновленный auth module
4. `apps/gateway/src/common/guards/rbac.guard.ts` - Обновленный RBACGuard с feature flag
5. Unit tests для всех сервисов

**Requirements**:
- Интеграция с Router Admin gRPC для валидации токенов
- Кэширование результатов (TTL: 60s для RBAC, 5min для policies)
- Поддержка JWT, API keys, sessions, anonymous
- Feature flag: `USE_ROUTER_AUTH` для gradual rollout
- Fail-closed: если Router недоступен → deny access

### Phase 3: Policy Integration (Week 2)

**Artifacts**:
1. `apps/gateway/src/policies/router-policies.service.ts` - Реализация RouterPoliciesService
2. `apps/gateway/src/policies/policies.service.ts` - Обновленный с feature flag
3. `apps/gateway/src/policies/migration/policy-export.ts` - Export tool для существующих policies
4. `apps/gateway/src/policies/migration/policy-import.ts` - Import tool для Router Admin
5. Unit tests для policy services

**Requirements**:
- Интеграция с Router Admin для управления политиками
- Кэширование политик (TTL: 5min)
- Маппинг Router формата → Gateway формат
- Инвалидация кэша при изменениях
- Feature flag: `USE_ROUTER_POLICIES`

### Phase 4: Protocol Unification (Week 2-3)

**Artifacts**:
1. `apps/gateway/src/router-protocol/router-protocol.service.ts` - Реализация RouterProtocolService
2. `apps/gateway/src/routes/adapters/router-client.service.ts` - Обновленный с gRPC support
3. `apps/gateway/src/router-protocol/protocol-metrics.service.ts` - Метрики производительности протоколов
4. `apps/gateway/src/router-protocol/router-protocol.module.ts` - NestJS module
5. Integration tests для protocol selection

**Requirements**:
- Автоматический выбор gRPC/NATS на основе payload size и latency
- gRPC для больших payload (>1MB) и высокой надежности
- NATS для низкой latency (<50ms) и малых payload
- Метрики производительности для оптимизации выбора
- Fallback на альтернативный протокол при ошибках

### Phase 5: Error Handling (Week 3)

**Artifacts**:
1. `apps/gateway/src/common/errors/router-error-mapping.service.ts` - Реализация RouterErrorMappingService
2. `apps/gateway/src/common/filters/router-error.filter.ts` - Global error filter
3. `apps/gateway/src/common/interceptors/router-context.interceptor.ts` - RouterContextInterceptor
4. Обновление всех controllers для использования unified error handling
5. Unit tests для error mapping

**Requirements**:
- Маппинг Router errors → HTTP status codes
- Маппинг gRPC status codes → Router errors
- Стандартизированные error codes и messages
- Retry logic на основе error types
- Логирование с correlation IDs

### Phase 6: Local Allowlist Removal (Week 3-4)

**Artifacts**:
1. Migration script: `apps/gateway/scripts/migrate-allowlists-to-router.ts`
2. Удаление файлов:
   - `apps/gateway/src/auth/local-auth.service.ts` (если существует)
   - `apps/gateway/src/auth/api-key.service.ts` (если существует)
   - Локальные allowlist логика из `rbac.guard.ts`
3. Обновление конфигурации:
   - `apps/gateway/src/config/env.validation.ts` - удалить `GATEWAY_TENANT_ALLOWLIST`, `GATEWAY_POLICY_ALLOWLIST`
   - `apps/gateway/.env.example` - обновить переменные окружения
4. Database migration scripts (если используется БД)
5. Migration report: `docs/archive/dev/ALLOWLIST_MIGRATION_REPORT.md`

**Requirements**:
- Export существующих allowlists в Router policies
- Удаление локальных allowlist services
- Обновление всех зависимостей
- Валидация миграции
- Rollback plan готов

## File Paths

### Implementation Files

```
apps/gateway/src/
├── router-admin/
│   ├── router-admin-client.interface.ts
│   ├── router-admin-client.service.ts
│   └── router-admin.module.ts
├── auth/
│   ├── router-auth.service.ts
│   ├── unified-auth-context.service.ts
│   ├── auth-context.service.ts
│   └── auth.module.ts
├── policies/
│   ├── router-policies.service.ts
│   ├── policies.service.ts (updated)
│   └── migration/
│       ├── policy-export.ts
│       └── policy-import.ts
├── router-protocol/
│   ├── router-protocol.service.ts
│   ├── protocol-metrics.service.ts
│   └── router-protocol.module.ts
├── common/
│   ├── errors/
│   │   ├── router-error-mapping.service.ts
│   │   └── router-error.filter.ts
│   ├── interceptors/
│   │   └── router-context.interceptor.ts
│   └── guards/
│       └── rbac.guard.ts (updated)
└── routes/
    └── adapters/
        └── router-client.service.ts (updated with gRPC)
```

### Configuration Files

```
apps/gateway/
├── src/config/
│   ├── router-admin.config.ts
│   └── env.validation.ts (updated)
├── .env.example (updated)
└── scripts/
    └── migrate-allowlists-to-router.ts
```

### Documentation Files

```
docs/archive/dev/
├── ALLOWLIST_MIGRATION_REPORT.md
└── GATEWAY_ROUTER_AUTH_IMPLEMENTATION.md
```

## Context and Purpose

### Why This Is Important

1. **Eliminate Duplication**: Устранить дублирование логики между Gateway и Router
2. **Single Source of Truth**: Router становится единственным источником правды для auth, RBAC, policies
3. **Consistency**: Обеспечить консистентность между компонентами
4. **Maintainability**: Упростить поддержку (изменения в одном месте)
5. **Scalability**: Router может обрабатывать multiple Gateway instances

### Current State

**Gateway Current Implementation**:
- ❌ Local allowlist via `GATEWAY_TENANT_ALLOWLIST` env variable
- ❌ Local allowlist via `GATEWAY_POLICY_ALLOWLIST` env variable
- ❌ Hardcoded `roleMatrix` for RBAC checks
- ❌ Local in-memory `Map<string, PolicyDto>` storage
- ✅ Basic tenant/policy format validation

**Router Current Implementation**:
- ✅ RouterAdmin service в `proto/beamline/flow/v1/flow.proto`
- ✅ `UpsertPolicy`, `DeletePolicy`, `GetPolicy`, `ListPolicies` endpoints
- ⏳ RBAC endpoints могут требовать реализации в Router (wrk-3)

### Target State

- ✅ Все RBAC checks идут через Router Admin gRPC
- ✅ Все политики управляются через Router Admin
- ✅ Единая аутентификация через Router Admin
- ✅ Protocol auto-selection (gRPC/NATS)
- ✅ Унифицированная обработка ошибок
- ✅ Нет локальных allowlists или policy storage

## Technical Requirements

### Performance Requirements

- **Authentication latency**: < 100ms (p95), < 50ms (cached)
- **Authorization latency**: < 50ms (p95), < 30ms (cached)
- **Context retrieval**: < 100ms (cached), < 300ms (uncached)
- **Cache hit rate**: > 90% для auth, > 85% для permissions
- **gRPC connection pool**: 10-50 connections

### Security Requirements

- **Token validation**: Zero tolerance для invalid tokens
- **Permission elevation**: Strictly controlled через RBAC
- **Audit logging**: 100% auth events logged
- **Rate limiting**: Per-user и per-tenant limits enforced
- **TLS**: Required для production gRPC connections

### Reliability Requirements

- **Circuit breaker**: Prevents cascading failures
- **Graceful degradation**: Fallback на локальную логику если Router unavailable
- **Health checks**: Для Router Admin connection
- **Retry logic**: Exponential backoff для transient errors
- **Monitoring**: Comprehensive metrics и alerting

## Implementation Guidelines

### Code Quality Standards

1. **Type Safety**: Strict TypeScript с comprehensive interfaces
2. **Error Handling**: Graceful degradation с proper fallbacks
3. **Logging**: Structured logging с correlation IDs
4. **Metrics**: Comprehensive auth metrics collection
5. **Testing**: 100% coverage для auth critical paths

### Testing Requirements

1. **Unit Tests**: Все сервисы должны иметь unit tests
2. **Integration Tests**: End-to-end Router Admin integration
3. **E2E Tests**: Complete authentication flow
4. **Load Tests**: Authentication под нагрузкой (1000+ req/s)
5. **Performance Tests**: Latency и throughput measurements

### Migration Strategy

1. **Feature Flags**: Gradual rollout через feature flags
2. **Dual-Write**: Валидация через dual-write (если возможно)
3. **Monitoring**: Continuous monitoring во время миграции
4. **Rollback Plan**: Готовый план отката на каждом этапе

## Acceptance Criteria

### Functional Criteria

- ✅ Все RBAC checks идут через Router Admin gRPC
- ✅ Все политики управляются через Router Admin
- ✅ Protocol auto-selection работает корректно
- ✅ Error handling унифицирован и консистентен
- ✅ Нет локальных allowlists или policy storage
- ✅ Feature flags работают для gradual rollout
- ✅ Health checks для Router Admin connection

### Performance Criteria

- ✅ Authentication latency < 100ms (p95)
- ✅ Authorization latency < 50ms (p95)
- ✅ Router Admin availability > 99.9%
- ✅ Authentication success rate > 99.5%
- ✅ Cache hit rate > 90% для auth checks
- ✅ Cache hit rate > 85% для permission checks

### Quality Criteria

- ✅ Unit test coverage > 80% для всех сервисов
- ✅ Integration tests проходят
- ✅ E2E tests проходят
- ✅ Load tests показывают требуемую производительность
- ✅ Code review пройден
- ✅ Documentation обновлена

## Dependencies

### Required from Other WORKERs

1. **wrk-3 (Router Core)**: 
   - Router Admin gRPC endpoints должны быть доступны
   - RBAC endpoints (если еще не реализованы)
   - Health checks для gRPC

2. **wrk-2 (Architecture/Tech Lead)**: 
   - ✅ Architecture specification completed
   - ✅ Contracts and interfaces defined

3. **wrk-11 (DevOps)**: 
   - gRPC endpoint configuration
   - TLS certificates для production
   - Monitoring и alerting setup

### External Dependencies

- `@grpc/grpc-js` или `@nestjs/microservices` для gRPC client
- `@nestjs/cache-manager` для caching
- `@opentelemetry/api` для distributed tracing
- Generated TypeScript types из proto files

## Risks and Mitigations

### Risk 1: Router Dependency

**Risk**: Gateway зависит от доступности Router.

**Mitigation**:
- Circuit breaker для предотвращения cascading failures
- Graceful degradation (fallback на локальную логику)
- Health checks и мониторинг
- Feature flags для gradual rollout

### Risk 2: Latency Increase

**Risk**: Дополнительный network hop для RBAC/policy checks.

**Mitigation**:
- Кэширование (60s для RBAC, 5min для policies)
- Оптимизация протоколов (NATS для low latency)
- Метрики и мониторинг latency
- Connection pooling

### Risk 3: Migration Complexity

**Risk**: Сложность миграции существующих деплоев.

**Mitigation**:
- Feature flags для gradual rollout
- Export/import tools для allowlists
- Подробный migration guide
- Rollback plan на каждом этапе

## References

- `docs/WORKER_2_AUTH_INTEGRATION_SPEC.md` - Детальная техническая спецификация
- `docs/archive/dev/ROUTER_GATEWAY_INTEGRATION_ARCHITECTURE.md` - Архитектурная спецификация
- `docs/ADR/ADR-014-Router-Gateway-Integration.md` - Architecture Decision Record
- `docs/LOCAL_ALLOWLIST_MIGRATION_GUIDE.md` - Migration guide
- `docs/archive/dev/WRK_2_AUTH_INTEGRATION_ARCHITECTURE_REPORT.md` - Architecture report
- `proto/beamline/flow/v1/flow.proto` - Router и RouterAdmin service definitions
- Reference implementation files в `apps/gateway/src/auth/` (для демонстрации архитектуры)

## Reporting Requirements

### Progress Reports

**Weekly Status Reports** (каждую пятницу):
- Статус каждой фазы (completed/in_progress/blocked)
- Количество созданных/обновленных файлов
- Тесты пройдены/не пройдены
- Блокеры и риски

### Final Report

**Upon Completion**:
- `docs/archive/dev/WRK_4_AUTH_INTEGRATION_IMPLEMENTATION_REPORT.md`
- Список всех созданных/обновленных файлов
- Результаты тестирования
- Performance metrics
- Migration report
- Lessons learned

## Notes

- Все изменения должны быть backward compatible через feature flags
- Миграция должна быть gradual, без breaking changes
- Все сервисы должны иметь comprehensive error handling
- Monitoring и alerting должны быть настроены перед production deployment
- Code review обязателен перед merge

---

**ORDER ID**: ORDER-WRK-4-AUTH-001  
**Status**: Pending  
**Rule Version**: v10  
**Message Protocol**: v1

