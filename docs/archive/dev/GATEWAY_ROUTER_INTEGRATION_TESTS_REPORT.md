# Gateway ↔ Router Integration Tests Implementation Report

**Date**: 2025-01-27  
**Status**: ✅ **Tests Created**  
**Location**: `tests/integration/gateway-router-error-handling.test.ts`

## Executive Summary

Созданы интеграционные тесты для полного flow: **Gateway HTTP request → Router validation → DLQ/audit → HTTP error response**. Тесты используют существующий маппинг error codes из `router_intake_error_handler.erl` и проверяют корректность HTTP статус кодов, структуру error responses, и интеграцию с DLQ/audit.

## Выполненные задачи

### ✅ 1. Создание интеграционных тестов

**Файл**: `tests/integration/gateway-router-error-handling.test.ts`

**Тестовые группы**:

1. **Error Code Mapping & HTTP Status**:
   - `SCHEMA_VALIDATION_FAILED → 400 Bad Request`
   - `VERSION_UNSUPPORTED → 400 Bad Request`
   - `CORRELATION_FIELDS_INVALID → 400 Bad Request`
   - `TENANT_FORBIDDEN → 401 Unauthorized`
   - `IDEMPOTENCY_VIOLATION → 400 Bad Request`
   - `INTERNAL_VALIDATION_ERROR → 500 Internal Server Error`
   - `Error Response Structure`

2. **DLQ Publication Verification**:
   - Проверка error responses при ошибках валидации
   - Проверка graceful handling при DLQ failures

3. **Audit Logging Verification**:
   - Проверка наличия `trace_id` в error responses
   - Проверка наличия `tenant_id` в error responses
   - Проверка наличия `error_code` в error responses

4. **End-to-End Error Flow**:
   - Полный flow: HTTP request → Router validation → DLQ/audit → HTTP error response
   - Обработка множественных validation errors

5. **Success Flow (for comparison)**:
   - Проверка успешных запросов для сравнения

### ✅ 2. Использование существующего маппинга error codes

**Маппинг из `router_intake_error_handler.erl`**:

| Intake Error Code | Gateway Error Code | HTTP Status |
|-------------------|-------------------|-------------|
| `SCHEMA_VALIDATION_FAILED` | `invalid_request` | 400 |
| `VERSION_UNSUPPORTED` | `invalid_request` | 400 |
| `CORRELATION_FIELDS_INVALID` | `invalid_request` | 400 |
| `TENANT_FORBIDDEN` | `unauthorized` | 401 |
| `IDEMPOTENCY_VIOLATION` | `invalid_request` | 400 |
| `INTERNAL_VALIDATION_ERROR` | `internal` | 500 |

**Реализация**:
- Router: `router_intake_error_handler.erl` → `map_intake_error_to_gateway_code/1`
- Router: `build_error_response/3` → включает `intake_error_code` в response
- Gateway: `apps/c-gateway/src/http_server.c` → `map_router_error_status()` → маппит Gateway error codes на HTTP статусы

### ✅ 3. Тесты уровня Gateway → Router → DLQ/audit/HTTP

**Покрытие**:

1. **Gateway HTTP Request**:
   - Отправка HTTP POST запросов в Gateway
   - Различные сценарии ошибок (missing fields, invalid data, etc.)

2. **Router Validation**:
   - Проверка, что Router корректно валидирует запросы
   - Проверка, что Router возвращает правильные error codes

3. **DLQ Publication**:
   - Проверка, что error responses возвращаются корректно
   - Проверка, что Router не блокируется на DLQ failures
   - **Примечание**: Фактическая публикация в DLQ проверяется в Router e2e тестах

4. **Audit Logging**:
   - Проверка, что error responses включают необходимую информацию для audit
   - Проверка наличия `trace_id`, `tenant_id`, `error_code`
   - **Примечание**: Фактические audit log entries проверяются в Router e2e тестах

5. **HTTP Response**:
   - Проверка HTTP статус кодов (400, 401, 500)
   - Проверка структуры error response
   - Проверка наличия `context` с `request_id` и `trace_id`

## Технические детали

### Структура тестов

**Helper функции**:
- `gateway()` - создает Axios client для Gateway
- `router()` - создает Axios client для Router (для прямого тестирования Router, если нужно)
- `generateId(prefix)` - генерирует уникальные ID для тестов

**Тестовые сценарии**:

1. **Schema Validation Errors**:
   - Missing `tenant_id`
   - Invalid JSON payload
   - Missing required fields

2. **Version Errors**:
   - Unsupported version in request

3. **Correlation Field Errors**:
   - Invalid UUID format
   - Invalid field dependencies

4. **Tenant Validation Errors**:
   - Tenant not in allowlist
   - Missing tenant header

5. **Idempotency Errors**:
   - Duplicate request with conflicting data

6. **Internal Errors**:
   - Internal validation errors (если возможно триггерить)

### Error Response Structure

**Ожидаемая структура**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",  // Gateway-compatible code
    "message": "Schema validation failed: missing tenant_id",
    "intake_error_code": "SCHEMA_VALIDATION_FAILED"  // Original intake code (optional)
  },
  "context": {
    "request_id": "req-123",
    "trace_id": "trace-456"
  }
}
```

**HTTP Status Mapping**:
- `invalid_request` → 400 Bad Request
- `unauthorized` → 401 Unauthorized
- `internal` → 500 Internal Server Error

## Интеграция с Router E2E Tests

**Router E2E Tests** (`router_intake_e2e_SUITE.erl`):
- ✅ Проверяют DLQ публикацию
- ✅ Проверяют audit log entries
- ✅ Проверяют metrics emission
- ✅ Тестируют Router internals

**Gateway Integration Tests** (`gateway-router-error-handling.test.ts`):
- ✅ Проверяют HTTP статус коды
- ✅ Проверяют структуру error response
- ✅ Проверяют Gateway → Router → Gateway flow
- ✅ Тестируют Gateway error handling

**Вместе** они обеспечивают полное покрытие error handling flow.

## Запуск тестов

### Prerequisites

1. **Gateway (C-Gateway)** running on `http://localhost:8081` (или установить `C_GATEWAY_URL`)
2. **Router** running и доступен через Gateway
3. **NATS** running (опционально, для полной интеграции)

### Команды

```bash
# Запустить все тесты
cd tests/integration
npm test gateway-router-error-handling.test.ts

# Запустить конкретную группу тестов
npm test gateway-router-error-handling.test.ts -t "Error Code Mapping"
npm test gateway-router-error-handling.test.ts -t "DLQ Publication"
npm test gateway-router-error-handling.test.ts -t "Audit Logging"
```

### Environment Variables

```bash
export C_GATEWAY_URL=http://localhost:8081
export ROUTER_URL=http://localhost:3081  # Optional
export NATS_URL=nats://localhost:4222     # Optional
```

## Покрытие тестами

### Error Scenarios

1. ✅ Schema validation errors (missing fields, invalid JSON)
2. ✅ Version errors (unsupported version)
3. ✅ Correlation field errors (invalid UUID, invalid dependencies)
4. ✅ Tenant validation errors (forbidden tenant, missing header)
5. ✅ Idempotency errors (duplicate with conflicting data)
6. ✅ Internal errors (если возможно триггерить)

### Response Structure

1. ✅ HTTP status codes (400, 401, 500)
2. ✅ Error response format (`ok: false`, `error.code`, `error.message`)
3. ✅ Context fields (`request_id`, `trace_id`)
4. ✅ Intake error code (если Router предоставляет)

## Известные ограничения

1. **DLQ Verification**: Gateway тесты проверяют error responses, но фактическая DLQ публикация проверяется в Router e2e тестах (требует доступа к NATS).

2. **Audit Log Verification**: Gateway тесты проверяют структуру error response, но фактические audit log entries проверяются в Router e2e тестах (требует доступа к логам).

3. **Metrics Verification**: Gateway тесты не проверяют metrics emission (проверяется в Router e2e тестах).

4. **Internal Error Triggering**: Некоторые internal errors могут быть сложно триггерить без мокирования Router internals.

## Следующие шаги

1. **Запустить тесты** и проверить, что они проходят с реальным Gateway и Router
2. **Добавить NATS subscription** для прямой проверки DLQ messages (если нужно)
3. **Добавить log parsing** для проверки audit entries (если нужно)
4. **Добавить metrics scraping** для проверки error metrics (если нужно)

## Файлы

### Созданные файлы:
- ✅ `tests/integration/gateway-router-error-handling.test.ts` - интеграционные тесты
- ✅ `docs/archive/dev/GATEWAY_ROUTER_ERROR_HANDLING_TESTS.md` - документация по тестам
- ✅ `docs/archive/dev/GATEWAY_ROUTER_INTEGRATION_TESTS_REPORT.md` - этот отчет

### Используемые файлы:
- `apps/otp/router/src/router_intake_error_codes.erl` - определения error codes
- `apps/otp/router/src/router_intake_error_handler.erl` - обработка ошибок и маппинг
- `apps/c-gateway/src/http_server.c` - Gateway error status mapping
- `apps/otp/router/test/router_intake_e2e_SUITE.erl` - Router e2e тесты

## Ссылки

- `tests/integration/gateway-router-error-handling.test.ts` - интеграционные тесты
- `docs/archive/dev/GATEWAY_ROUTER_ERROR_HANDLING_TESTS.md` - документация по тестам
- `docs/ARCHITECTURE/api-registry.md` - документация по error code mapping
- `docs/archive/dev/ROUTER_INTAKE_E2E_GATEWAY_INTEGRATION_REPORT.md` - отчет по Gateway интеграции

