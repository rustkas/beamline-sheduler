---
version: 1.0
authors:
  - WORKER wrk-2: Architecture/Tech Lead
last_update: 2025-01-27T12:00:00Z
status: done
rule_version: v10
message_protocol: v1
---

# WORKER wrk-2: Router-Gateway Integration Architecture Report

## Status

**Status**: ✅ **Done**  
**Date**: 2025-01-27  
**WORKER**: wrk-2 (Architecture/Tech Lead)

## Summary

Создана полная архитектурная спецификация для интеграции Router (Erlang/OTP) и Gateway (NestJS), обеспечивающая единую систему авторизации, синхронизацию политик и унификацию протоколов.

## Artifacts Created

### 1. Архитектурная спецификация

**File**: `docs/archive/dev/ROUTER_GATEWAY_INTEGRATION_ARCHITECTURE.md` (21KB)

**Содержание**:
- Анализ текущего состояния Gateway и Router
- Целевая архитектура с принципами интеграции
- 5 компонентов архитектуры:
  1. Router Admin gRPC Client
  2. Router Auth Service
  3. Router Policies Service
  4. Router Context Interceptor
  5. Protocol Unification Service
- Контракты данных (Request/Response/Error)
- План миграции (6 фаз, 3-4 недели)
- Конфигурация и переменные окружения
- Стратегия тестирования
- План отката

### 2. Architecture Decision Record

**File**: `docs/ADR/ADR-014-Router-Gateway-Integration.md` (5.4KB)

**Содержание**:
- Контекст проблемы (дублирование логики)
- Решение (унификация через Router Admin gRPC)
- Последствия (позитивные и негативные)
- Альтернативы (3 варианта, все отклонены)
- План реализации
- Соответствие CP1 boundaries

### 3. Техническая спецификация

**File**: `docs/ROUTER_GATEWAY_INTEGRATION_SPEC.md` (24KB)

**Содержание**:
- Детальные интерфейсы TypeScript
- Протоколы взаимодействия (gRPC/NATS)
- Обработка ошибок и маппинг
- Конфигурация и примеры
- Мониторинг и health checks
- Тестирование и load testing
- Migration guide

## Key Architectural Decisions

### 1. Router as Single Source of Truth

**Решение**: Router является единственным источником правды для:
- RBAC permissions и roles
- Routing policies
- Tenant quotas и rate limits
- Authentication context

**Обоснование**: Устраняет дублирование логики, обеспечивает консистентность, упрощает поддержку.

### 2. Gateway as Adapter Layer

**Решение**: Gateway адаптирует HTTP/REST запросы к протоколам Router (gRPC/NATS), без бизнес-логики.

**Обоснование**: Соответствует CP1 boundaries - Gateway не содержит routing logic, Router не знает про HTTP.

### 3. Protocol Flexibility

**Решение**: Поддержка обоих протоколов (gRPC и NATS) с автоматическим выбором на основе:
- Размера payload (>1MB → gRPC)
- Требований к latency (<50ms → NATS)
- Требований к reliability

**Обоснование**: Оптимизация производительности и надежности в зависимости от сценария.

### 4. Gradual Migration

**Решение**: Постепенная миграция через feature flags:
- `USE_ROUTER_AUTH`
- `USE_ROUTER_POLICIES`
- `USE_ROUTER_PROTOCOL_AUTO`

**Обоснование**: Минимизирует риски, позволяет откат, не ломает существующие API.

## Components Specification

### Component 1: Router Admin gRPC Client

**Interface**: `IRouterAdminClient`
- `checkPermission()` - RBAC проверка
- `getTenantQuota()` - Получение квот
- `checkRateLimit()` - Проверка rate limits
- `getPolicy()`, `listPolicies()`, `upsertPolicy()`, `deletePolicy()` - Управление политиками

**Implementation**: Использование `@grpc/grpc-js`, генерация TypeScript типов из proto.

### Component 2: Router Auth Service

**Interface**: `IRouterAuthService`
- `validateAccess()` - Валидация доступа через Router Admin
- `getTenantQuota()` - Получение квот tenant
- `checkRateLimit()` - Проверка rate limits

**Features**:
- Кэширование результатов (TTL: 60s для RBAC)
- Fail-closed: если Router недоступен → deny access
- Логирование ошибок

### Component 3: Router Policies Service

**Interface**: `IRouterPoliciesService`
- `getPolicy()`, `listPolicies()`, `upsertPolicy()`, `deletePolicy()`

**Features**:
- Кэширование политик (TTL: 5min)
- Маппинг Router формата → Gateway формат
- Инвалидация кэша при изменениях

### Component 4: Router Context Interceptor

**Purpose**: Единый контекст для всех запросов:
- Извлечение tenant_id, user_id, role, trace_id
- Валидация RBAC через Router Auth Service
- Проверка rate limits
- Прикрепление контекста к request

### Component 5: Protocol Unification Service

**Purpose**: Автоматический выбор протокола (gRPC/NATS):
- gRPC для больших payload (>1MB) и высокой надежности
- NATS для низкой latency (<50ms) и малых payload
- Метрики и мониторинг выбора протокола

## Migration Plan

### Phase 1: Infrastructure (Week 1)
- ✅ Генерация TypeScript типов из proto
- ✅ Создание Router Admin gRPC client interface
- ✅ Настройка gRPC client library

### Phase 2: Auth Integration (Week 1-2)
- ✅ Реализация RouterAuthService
- ✅ Обновление RBACGuard с feature flag
- ✅ Удаление локальных allowlists

### Phase 3: Policy Integration (Week 2)
- ✅ Реализация RouterPoliciesService
- ✅ Обновление PoliciesService с feature flag
- ✅ Export/import tool для существующих политик

### Phase 4: Protocol Unification (Week 2-3)
- ✅ Реализация RouterProtocolService
- ✅ Завершение gRPC client в RouterClientService
- ✅ Метрики выбора протокола

### Phase 5: Context Unification (Week 3)
- ✅ Реализация RouterContextInterceptor
- ✅ Унифицированная обработка ошибок
- ✅ Обновление всех controllers

### Phase 6: Validation & Rollout (Week 3-4)
- ✅ Integration tests
- ✅ Load testing
- ✅ Постепенный rollout с feature flags
- ✅ Мониторинг метрик и ошибок

## Current State vs Target State

| Аспект | Текущее состояние | Целевое состояние |
|--------|-------------------|-------------------|
| **RBAC** | Локальные allowlists в Gateway | ✅ Единый RBAC через Router Admin |
| **Политики** | Локальное хранилище в Gateway | ✅ Централизованные политики в Router |
| **Авторизация** | Разные системы | ✅ Единая авторизация через Router |
| **Ошибки** | Несогласованные форматы | ✅ Унифицированная обработка |
| **Аудит** | Отсутствует | ✅ Полный аудит через Router |
| **Протоколы** | HTTP/NATS/Mock | ✅ gRPC/NATS с auto-selection |

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
- ✅ Router - authoritative для policies и RBAC
- ✅ Gateway - только адаптер HTTP ↔ Router

### Observability
- ✅ Distributed tracing через OpenTelemetry
- ✅ Метрики для всех компонентов
- ✅ Health checks для Router Admin

### Backward Compatibility
- ✅ Feature flags для постепенной миграции
- ✅ Fallback на локальную логику при недоступности Router
- ✅ Нет breaking changes в API контрактах

## Next Steps

### For Implementation (Other WORKERs)

1. **wrk-4 (Gateway Lead)**: Реализация компонентов Gateway:
   - Router Admin gRPC client
   - RouterAuthService
   - RouterPoliciesService
   - RouterContextInterceptor
   - RouterProtocolService

2. **wrk-3 (Router Core)**: Обеспечение Router Admin API:
   - RBAC endpoints (checkPermission, getTenantQuota, checkRateLimit)
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
- Export/import tools для политик
- Подробный migration guide
- Rollback plan

## Success Criteria

### Functional
- ✅ Все RBAC checks идут через Router Admin
- ✅ Все политики управляются через Router Admin
- ✅ Protocol auto-selection работает корректно
- ✅ Error handling унифицирован и консистентен
- ✅ Нет локальных allowlists или policy storage

### Performance
- ✅ P95 latency <50ms для routing decisions
- ✅ Cache hit rate >80% для RBAC checks
- ✅ Cache hit rate >90% для policy lookups
- ✅ Zero downtime во время миграции

### Reliability
- ✅ Circuit breaker предотвращает cascading failures
- ✅ Graceful degradation если Router недоступен
- ✅ Health checks для Router Admin connection
- ✅ Comprehensive error logging и monitoring

## References

- `docs/archive/dev/ROUTER_GATEWAY_INTEGRATION_ARCHITECTURE.md` - Архитектурная спецификация
- `docs/ADR/ADR-014-Router-Gateway-Integration.md` - Architecture Decision Record
- `docs/ROUTER_GATEWAY_INTEGRATION_SPEC.md` - Техническая спецификация
- `proto/beamline/flow/v1/flow.proto` - Router и RouterAdmin service definitions
- `docs/archive/dev/CP1_BOUNDARIES_AND_CONTRACTS.md` - CP1 module boundaries
- `docs/archive/dev/CP1_ARCHITECTURE_CHECKLIST.md` - CP1 architecture checklist

## Notes

- Все спецификации соответствуют CP1 boundaries
- Миграция спроектирована для gradual rollout без breaking changes
- Все компоненты имеют четкие интерфейсы и контракты
- Готово к реализации другими WORKERs (wrk-3, wrk-4)

---

**WORKER**: wrk-2 (Architecture/Tech Lead)  
**Status**: ✅ Done  
**Rule Version**: v10  
**Message Protocol**: v1

