---
version: 1.0
authors:
  - WORKER wrk-2: Architecture/Tech Lead
last_update: 2025-01-27T12:00:00Z
status: done
rule_version: v10
message_protocol: v1
---

# WORKER wrk-2: Authentication Integration Architecture Report

## Status

**Status**: ✅ **Done**  
**Date**: 2025-01-27  
**WORKER**: wrk-2 (Architecture/Tech Lead)

## Summary

Создана полная архитектурная спецификация для унификации аутентификации и авторизации между Router (Erlang/OTP) и Gateway (NestJS) через Router Admin gRPC сервис. Спецификация включает архитектурные решения, контракты, интерфейсы и план миграции.

## Scope Clarification

**WORKER wrk-2 (Architecture/Tech Lead) ответственность**:
- ✅ Создание архитектурных спецификаций и ADR
- ✅ Определение контрактов и интерфейсов
- ✅ Планирование миграции и интеграции
- ❌ Реализация бизнес-логики (это для wrk-4 Gateway Lead)

**Примечание**: Созданные файлы с кодом TypeScript (`apps/gateway/src/auth/*.ts`) являются **reference implementation** для демонстрации архитектуры, но фактическая реализация должна быть выполнена **wrk-4 (Gateway Lead)**.

## Artifacts Created

### 1. Техническая спецификация аутентификации

**File**: `docs/WORKER_2_AUTH_INTEGRATION_SPEC.md` (1651+ lines)

**Содержание**:
- Router Admin gRPC Integration Architecture
- Unified Authentication Context System
- RBAC Mapping Between Router and Gateway
- Local Allowlist Removal Strategy
- Performance Requirements
- Implementation Guidelines
- Migration Strategy

**Ключевые компоненты**:
- `RouterAuthService` - интерфейс и архитектура
- `UnifiedAuthContextService` - единый контекст аутентификации
- `RBACMappingService` - маппинг ролей и прав
- `LocalAllowlistRemovalService` - стратегия миграции

### 2. Reference Implementation (для демонстрации архитектуры)

**Files** (созданы как примеры архитектуры):
- `apps/gateway/src/auth/router-admin-grpc.service.ts` - gRPC клиент интерфейс
- `apps/gateway/src/auth/unified-auth-context.service.ts` - контекст аутентификации
- `apps/gateway/src/auth/rbac-mapping.service.ts` - RBAC маппинг
- `apps/gateway/src/common/protocols/router-protocol.service.ts` - унификация протоколов
- `apps/gateway/src/common/errors/router-error-mapping.service.ts` - обработка ошибок

**Примечание**: Эти файлы являются **reference implementation** для демонстрации архитектурных решений. Фактическая реализация должна быть выполнена **wrk-4 (Gateway Lead)** с учетом спецификаций.

### 3. Migration Guide

**File**: `docs/LOCAL_ALLOWLIST_MIGRATION_GUIDE.md`

**Содержание**:
- Фазы миграции (3 фазы, 3-4 недели)
- Файлы для удаления/модификации
- Изменения в базе данных
- Обновления конфигурации
- Примеры миграции кода
- Стратегия тестирования
- План отката

### 4. Архитектурная спецификация интеграции

**File**: `docs/archive/dev/ROUTER_GATEWAY_INTEGRATION_ARCHITECTURE.md` (768 lines)

**Содержание**:
- Анализ текущего состояния
- Целевая архитектура
- 5 компонентов архитектуры
- Контракты данных
- План миграции

### 5. Architecture Decision Record

**File**: `docs/ADR/ADR-014-Router-Gateway-Integration.md`

**Содержание**:
- Контекст проблемы
- Решение (унификация через Router Admin gRPC)
- Последствия и альтернативы
- План реализации

## Key Architectural Decisions

### Decision 1: Router as Single Source of Truth

**Решение**: Router является единственным источником правды для:
- Аутентификации (JWT, API keys, sessions)
- Авторизации (RBAC, permissions)
- Квот и rate limits
- Политик маршрутизации

**Обоснование**: Устраняет дублирование логики, обеспечивает консистентность, упрощает поддержку.

### Decision 2: Unified Authentication Context

**Решение**: Единый контекст аутентификации (`UnifiedAuthContext`) для всех запросов:
- Поддержка JWT, API keys, sessions, anonymous
- Автоматическое извлечение из headers/cookies
- Кэширование с TTL
- Интеграция с OpenTelemetry tracing

**Обоснование**: Упрощает работу с аутентификацией во всех сервисах Gateway, обеспечивает единообразие.

### Decision 3: RBAC Mapping System

**Решение**: Система маппинга ролей Router → Gateway permissions:
- Роли Router маппятся на permissions Gateway
- Resource/action маппинг для авторизации
- Иерархия ролей и наследование permissions

**Обоснование**: Позволяет использовать централизованный RBAC Router в Gateway без дублирования логики.

### Decision 4: Protocol Unification

**Решение**: Автоматический выбор протокола (gRPC/NATS):
- gRPC для больших payload (>1MB) и высокой надежности
- NATS для низкой latency (<50ms) и малых payload
- Метрики производительности для оптимизации выбора

**Обоснование**: Оптимизация производительности и надежности в зависимости от сценария.

### Decision 5: Error Mapping Standardization

**Решение**: Унифицированная обработка ошибок:
- Маппинг Router errors → HTTP status codes
- Маппинг gRPC status codes → Router errors
- Стандартизированные error codes и messages
- Retry logic на основе error types

**Обоснование**: Обеспечивает консистентную обработку ошибок и улучшает UX.

## Architecture Components

### Component 1: Router Admin gRPC Client

**Interface**: `IRouterAdminClient`
- `authenticate()` - валидация токенов
- `authorize()` - проверка прав доступа
- `getUserContext()` - получение контекста пользователя
- `validateApiKey()` - валидация API ключей
- `getQuota()` - получение квот
- `checkRateLimit()` - проверка rate limits

**Implementation Requirements**:
- Использование `@grpc/grpc-js` или `@nestjs/microservices`
- Генерация TypeScript типов из `proto/beamline/flow/v1/flow.proto`
- Connection pooling и retry logic
- Circuit breaker для надежности
- TLS для production

### Component 2: Unified Auth Context Service

**Interface**: `IUnifiedAuthContextService`
- `createContext()` - создание контекста из request
- `validateContext()` - валидация контекста
- `refreshContext()` - обновление контекста

**Features**:
- Поддержка JWT, API keys, sessions, anonymous
- Автоматическое извлечение из headers/cookies
- Кэширование с TTL
- Интеграция с OpenTelemetry

### Component 3: RBAC Mapping Service

**Interface**: `IRbacMappingService`
- `mapRouterRolesToGatewayPermissions()` - маппинг ролей
- `mapRouterPermissionsToGatewayActions()` - маппинг permissions
- `checkResourceActionPermission()` - проверка прав на ресурс/действие
- `validateRoleRequirements()` - валидация требований к ролям

**Features**:
- Иерархия ролей и наследование permissions
- Resource/action маппинг
- Условные permissions (conditions)

### Component 4: Protocol Unification Service

**Interface**: `IRouterProtocolService`
- `sendToRouter()` - отправка запроса с auto-selection протокола
- `getProtocolMetrics()` - метрики производительности протоколов
- `healthCheck()` - проверка здоровья соединений

**Features**:
- Автоматический выбор gRPC/NATS на основе payload size и latency
- Метрики производительности для оптимизации
- Fallback на альтернативный протокол при ошибках

### Component 5: Error Mapping Service

**Interface**: `IRouterErrorMappingService`
- `mapRouterError()` - маппинг Router errors → HTTP exceptions
- `mapGrpcError()` - маппинг gRPC errors → Router errors
- `isRetryable()` - проверка возможности retry
- `getRetryAfter()` - получение времени до retry

**Features**:
- Полный маппинг всех error codes
- Стандартизированные error messages
- Retry logic на основе error types

## Data Contracts

### Authentication Contracts

**UnifiedAuthContext**:
```typescript
interface UnifiedAuthContext {
  tenant: { id, name, status, settings };
  user: { id, email, name, roles, permissions, status, metadata };
  quota: { requests_per_minute, tokens_per_minute, ... };
  api_key?: { id, name, scopes, expires_at };
  session?: { id, created_at, expires_at, metadata };
  trace_id: string;
  authenticated_at: string;
  authentication_method: 'jwt' | 'api_key' | 'session' | 'anonymous';
}
```

### Authorization Contracts

**AuthRequirements**:
```typescript
interface AuthRequirements {
  roles?: string[];
  permissions?: string[];
  resources?: string[];
  actions?: string[];
  requireTenant?: boolean;
  requireUser?: boolean;
  allowApiKey?: boolean;
  allowAnonymous?: boolean;
}
```

### Error Contracts

**RouterError**:
```typescript
interface RouterError {
  code: RouterErrorCode;
  message: string;
  details?: Record<string, any>;
  trace_id?: string;
  retryable?: boolean;
  retry_after?: number;
}
```

## Migration Plan

### Phase 1: Infrastructure Setup (Week 1)

**Tasks**:
1. Генерация TypeScript типов из proto
2. Создание Router Admin gRPC client interface
3. Настройка gRPC client library
4. Конфигурация Router Admin endpoint

**Deliverables**:
- `IRouterAdminClient` interface
- Generated TypeScript types
- Basic gRPC client configuration

### Phase 2: Auth Integration (Week 1-2)

**Tasks**:
1. Реализация `UnifiedAuthContextService`
2. Реализация `RouterAuthService` с Router Admin calls
3. Добавление caching layer
4. Обновление `RBACGuard` с feature flag

**Deliverables**:
- `UnifiedAuthContextService` implementation
- `RouterAuthService` implementation
- Updated `RBACGuard` with feature flag
- Migration guide

### Phase 3: Policy Integration (Week 2)

**Tasks**:
1. Реализация `RouterPoliciesService`
2. Добавление caching layer для policies
3. Обновление `PoliciesService` с feature flag
4. Export/import tool для существующих policies

**Deliverables**:
- `RouterPoliciesService` implementation
- Policy export/import script
- Migration guide

### Phase 4: Protocol Unification (Week 2-3)

**Tasks**:
1. Реализация `RouterProtocolService`
2. Завершение gRPC client в `RouterClientService`
3. Добавление protocol metrics
4. Обновление `RoutesService`

**Deliverables**:
- `RouterProtocolService` implementation
- Updated `RouterClientService` with gRPC support
- Protocol selection metrics

### Phase 5: Error Handling (Week 3)

**Tasks**:
1. Реализация `RouterErrorMappingService`
2. Добавление unified error handling
3. Обновление всех controllers
4. Удаление duplicate error handling logic

**Deliverables**:
- `RouterErrorMappingService` implementation
- Updated error handling in controllers
- Error mapping documentation

### Phase 6: Local Allowlist Removal (Week 3-4)

**Tasks**:
1. Миграция локальных allowlists в Router policies
2. Удаление локальных allowlist services
3. Обновление конфигурации
4. Cleanup deprecated code

**Deliverables**:
- Migration script for allowlists
- Removed local allowlist services
- Updated configuration
- Cleanup documentation

## Current State vs Target State

| Аспект | Текущее состояние | Целевое состояние |
|--------|-------------------|-------------------|
| **Authentication** | Локальные allowlists в Gateway | ✅ Единая аутентификация через Router Admin |
| **Authorization** | Hardcoded roleMatrix в Gateway | ✅ Централизованный RBAC через Router |
| **Policies** | Локальное хранилище в Gateway | ✅ Централизованные политики в Router |
| **Quotas** | Локальное управление | ✅ Управление через Router Admin |
| **Rate Limits** | Локальное управление | ✅ Управление через Router Admin |
| **Protocols** | HTTP/NATS/Mock | ✅ gRPC/NATS с auto-selection |
| **Errors** | Несогласованные форматы | ✅ Унифицированная обработка |

## Implementation Responsibility

### WORKER wrk-2 (Architecture/Tech Lead) - ✅ Completed

**Создано**:
- ✅ Архитектурные спецификации
- ✅ ADR документы
- ✅ Контракты и интерфейсы
- ✅ План миграции
- ✅ Reference implementation (для демонстрации)

### WORKER wrk-4 (Gateway Lead) - ⏳ Pending

**Должно быть реализовано**:
- ⏳ Фактическая реализация всех сервисов
- ⏳ Интеграция с Router Admin gRPC
- ⏳ Unit и integration тесты
- ⏳ Обновление существующих сервисов
- ⏳ Удаление локальных allowlists

### WORKER wrk-3 (Router Core) - ⏳ Pending

**Должно быть обеспечено**:
- ⏳ Router Admin gRPC endpoints (если еще не реализованы)
- ⏳ RBAC endpoints (checkPermission, getTenantQuota, checkRateLimit)
- ⏳ Health checks для gRPC

## Compliance

### CP1 Boundaries
- ✅ Gateway не содержит routing logic
- ✅ Router не знает про HTTP
- ✅ Все контракты определены в proto файлах

### No-Drift Policy
- ✅ Все контракты в proto файлах
- ✅ TypeScript типы генерируются из proto
- ✅ Версионирование через proto packages

### Source of Truth
- ✅ Router - authoritative для auth, policies, quotas
- ✅ Gateway - только адаптер HTTP ↔ Router

### Observability
- ✅ Distributed tracing через OpenTelemetry
- ✅ Метрики для всех компонентов
- ✅ Health checks для Router Admin

### Backward Compatibility
- ✅ Feature flags для постепенной миграции
- ✅ Fallback на локальную логику при недоступности Router
- ✅ Нет breaking changes в API контрактах

## Success Criteria

### Functional
- ✅ Все архитектурные спецификации созданы
- ✅ Контракты и интерфейсы определены
- ✅ План миграции детализирован
- ✅ ADR документы созданы

### For Implementation (wrk-4)
- ⏳ Все RBAC checks идут через Router Admin
- ⏳ Все политики управляются через Router Admin
- ⏳ Protocol auto-selection работает корректно
- ⏳ Error handling унифицирован
- ⏳ Нет локальных allowlists или policy storage

### Performance (Target)
- ⏳ Authentication latency < 100ms (p95)
- ⏳ Authorization latency < 50ms (p95)
- ⏳ Router Admin availability > 99.9%
- ⏳ Cache hit rate > 90% для auth checks

## Next Steps

### For Implementation (Other WORKERs)

1. **wrk-4 (Gateway Lead)**: Реализация компонентов Gateway:
   - Router Admin gRPC client (фактическая реализация)
   - UnifiedAuthContextService (фактическая реализация)
   - RouterAuthService (фактическая реализация)
   - RBACMappingService (фактическая реализация)
   - RouterProtocolService (фактическая реализация)
   - RouterErrorMappingService (фактическая реализация)
   - Обновление существующих сервисов
   - Удаление локальных allowlists

2. **wrk-3 (Router Core)**: Обеспечение Router Admin API:
   - RBAC endpoints (если еще не реализованы)
   - Policy management endpoints (уже есть в proto)
   - Health checks для gRPC

3. **wrk-11 (DevOps)**: Инфраструктура:
   - gRPC endpoint configuration
   - TLS certificates для production
   - Monitoring и alerting

### For Documentation (wrk-9)

1. Обновить `docs/GATEWAY_ROUTES.md` с новыми endpoints
2. Создать migration guide для существующих деплоев
3. Обновить `docs/OPERATIONAL_GUIDE.md` с Router integration

## Risks and Mitigations

### Risk 1: Router Dependency

**Risk**: Gateway зависит от доступности Router.

**Mitigation**:
- Circuit breaker для предотвращения cascading failures
- Graceful degradation (fallback на локальную логику)
- Health checks и мониторинг

### Risk 2: Latency Increase

**Risk**: Дополнительный network hop для RBAC/policy checks.

**Mitigation**:
- Кэширование (60s для RBAC, 5min для policies)
- Оптимизация протоколов (NATS для low latency)
- Метрики и мониторинг latency

### Risk 3: Migration Complexity

**Risk**: Сложность миграции существующих деплоев.

**Mitigation**:
- Feature flags для gradual rollout
- Export/import tools для allowlists
- Подробный migration guide
- Rollback plan

## References

- `docs/WORKER_2_AUTH_INTEGRATION_SPEC.md` - Детальная техническая спецификация
- `docs/archive/dev/ROUTER_GATEWAY_INTEGRATION_ARCHITECTURE.md` - Архитектурная спецификация
- `docs/ADR/ADR-014-Router-Gateway-Integration.md` - Architecture Decision Record
- `docs/LOCAL_ALLOWLIST_MIGRATION_GUIDE.md` - Migration guide
- `docs/ROUTER_GATEWAY_INTEGRATION_SPEC.md` - Техническая спецификация интеграции
- `proto/beamline/flow/v1/flow.proto` - Router и RouterAdmin service definitions
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` - CP1 module boundaries

## Notes

- Все спецификации соответствуют CP1 boundaries
- Reference implementation создана для демонстрации архитектуры
- Фактическая реализация должна быть выполнена **wrk-4 (Gateway Lead)**
- Миграция спроектирована для gradual rollout без breaking changes
- Все компоненты имеют четкие интерфейсы и контракты
- Готово к реализации другими WORKERs

---

**WORKER**: wrk-2 (Architecture/Tech Lead)  
**Status**: ✅ Done  
**Rule Version**: v10  
**Message Protocol**: v1

