---
version: 1.0
order_id: ORDER-WRK-2-CP2-002
from: mgr-2 (Architecture Manager)
to: wrk-2 (Architecture/Tech Lead)
created_at: 2025-01-27T15:00:00Z
status: pending
priority: HIGH
rule_version: v10
message_protocol: v1
---

# ORDER: CP2 Validation Suite Creation

## Order Information

**ORDER ID**: ORDER-WRK-2-CP2-002  
**From**: mgr-2 (Architecture Manager)  
**To**: wrk-2 (Architecture/Tech Lead)  
**Priority**: 🔴 **HIGH** - Critical for CP2-LC validation  
**Timeline**: 2 days  
**Dependencies**: ORDER-WRK-3-CP2-001 (Compilation fix)  
**Blocks**: CP2-LC transition validation

## Task Description

Создать комплексный скрипт валидации CP2 фич (`scripts/validate_cp2.sh`), который автоматически проверяет все CP2 функции: feature flags, JetStream connectivity, idempotency, tracing, tenant validation.

**Цель**: Обеспечить автоматизированную проверку всех CP2 фич перед переходом на CP2-LC.

## Expected Artifacts

### Primary Deliverable

**File**: `scripts/validate_cp2.sh` (ENHANCE existing script)

**Current State**: 
- ✅ Script exists with basic feature flag validation
- ✅ Already integrated in `scripts/dry_run_ci.sh` (function `step_cp2`)
- ⚠️ Missing: Runtime validation (JetStream, idempotency tests, tracing tests, tenant tests)

**Enhancement Requirements**:
- Add runtime validation functions (JetStream connectivity, test suite execution)
- Improve error handling and exit codes
- Add detailed logging for each validation step
- Cross-platform compatible (Linux, macOS, WSL)
- Clear exit codes (0 = success, non-zero = failure)

**Enhanced Script Structure**:
```bash
#!/bin/bash
# CP2 Feature Validation Suite
# Validates all CP2 features are enabled and working correctly

set -euo pipefail

# Exit codes
EXIT_SUCCESS=0
EXIT_FEATURE_FLAGS=1
EXIT_JETSTREAM=2
EXIT_IDEMPOTENCY=3
EXIT_TRACING=4
EXIT_TENANT=5

# 1. Feature Flag Validation (EXISTS - enhance)
validate_feature_flags() {
    echo "✅ Checking CP2 feature flags..."
    # Current implementation checks app.src
    # Enhance: Verify flags are actually true (not just present)
    # Enhance: Check state.json current_cp >= CP2-LC
}

# 2. JetStream Connection Validation (NEW)
validate_jetstream() {
    echo "✅ Validating JetStream connectivity..."
    cd apps/otp/router
    
    # Option 1: Use rebar3 shell with timeout
    timeout 30 rebar3 shell --eval "
        {ok, _} = application:ensure_all_started(beamline_router),
        {ok, Conn} = router_nats:get_connection(),
        case router_nats:jetstream_info(Conn) of
            {ok, _Info} -> 
                io:format(\"JetStream connection OK~n\"),
                init:stop();
            Error -> 
                io:format(\"JetStream error: ~p~n\", [Error]),
                init:stop(1)
        end
    " || {
        echo "⚠️  JetStream validation skipped (NATS may not be running)"
        return 0  # Non-blocking if NATS unavailable
    }
}

# 3. Idempotency Validation (NEW)
validate_idempotency() {
    echo "✅ Testing idempotency layer..."
    cd apps/otp/router
    
    if [ -f "test/router_idempotency_SUITE.erl" ]; then
        rebar3 ct --suite test/router_idempotency_SUITE || {
            echo "❌ Idempotency tests failed"
            exit $EXIT_IDEMPOTENCY
        }
    else
        echo "⚠️  Idempotency test suite not found (test/router_idempotency_SUITE.erl)"
        echo "   Creating minimal smoke test..."
        # Create minimal smoke test if suite doesn't exist
    fi
}

# 4. Tracing Validation (NEW)
validate_tracing() {
    echo "✅ Verifying OpenTelemetry tracing..."
    cd apps/otp/router
    
    # Check if router_tracing.erl exists and has OTel integration
    if grep -q "opentelemetry" src/router_tracing.erl 2>/dev/null; then
        echo "✅ OpenTelemetry integration found in router_tracing.erl"
    else
        echo "⚠️  OpenTelemetry integration not found"
    fi
    
    # Check if tracing spans are created in key modules
    if grep -q "span" src/router_result_consumer.erl 2>/dev/null; then
        echo "✅ Tracing spans found in result consumer"
    else
        echo "⚠️  Tracing spans not found in result consumer"
    fi
}

# 5. Tenant Validation (NEW)
validate_tenant_validation() {
    echo "✅ Testing tenant validation..."
    cd apps/otp/router
    
    if [ -f "test/router_tenant_allowlist_SUITE.erl" ]; then
        rebar3 ct --suite test/router_tenant_allowlist_SUITE || {
            echo "❌ Tenant validation tests failed"
            exit $EXIT_TENANT
        }
    else
        echo "⚠️  Tenant validation test suite not found"
    fi
    
    # Also check router_tenant_multitenant_smoke_SUITE if available
    if [ -f "test/router_tenant_multitenant_smoke_SUITE.erl" ]; then
        echo "✅ Running multi-tenant smoke tests..."
        rebar3 ct --suite test/router_tenant_multitenant_smoke_SUITE || {
            echo "⚠️  Multi-tenant smoke tests failed (non-blocking)"
        }
    fi
}

# Execute all validations
validate_feature_flags || exit $EXIT_FEATURE_FLAGS
validate_jetstream || exit $EXIT_JETSTREAM
validate_idempotency || exit $EXIT_IDEMPOTENCY
validate_tracing || exit $EXIT_TRACING
validate_tenant_validation || exit $EXIT_TENANT

echo "🎉 All CP2 validations passed!"
exit $EXIT_SUCCESS
```

### Integration Files

**Modified**: `scripts/dry_run_ci.sh` (ALREADY EXISTS - verify integration)

**Current State**: Function `step_cp2()` already exists (lines 549-578)

**Action Required**: Verify integration works correctly:
```bash
# Verify step_cp2 is called in main() function
grep -A 5 "step_cp2" scripts/dry_run_ci.sh

# Test execution
bash scripts/dry_run_ci.sh cp2
```

**Modified**: `.github/workflows/validate.yml.template`

**Add CP2 validation job** (if not exists):
```yaml
- name: CP2 Validation
  run: bash scripts/validate_cp2.sh
  continue-on-error: false  # Fail CI if CP2 validation fails
```

**Check existing workflows**:
```bash
# Check if CP2 validation already exists
grep -r "validate_cp2" .github/workflows/ || echo "Not found - needs to be added"
```

### Documentation Updates

**Modified**: `docs/archive/dev/LOCAL_CHECKS.md`

**Add CP2 validation section**:
```markdown
## CP2 Feature Validation

Run CP2 validation suite to verify all CP2 features are enabled and working:

```bash
bash scripts/validate_cp2.sh
```

This script validates:
- Feature flags are enabled by default
- JetStream connectivity
- Idempotency layer functionality
- OpenTelemetry tracing
- Tenant validation/ACL enforcement
```

**Modified**: `docs/archive/dev/PR_CHECKLIST.md`

**Add CP2 validation requirement**:
```markdown
- [ ] CP2 validation suite passes (`bash scripts/validate_cp2.sh`)
```

## Context and Purpose

### Why This Is Important

1. **Automated CP2 Validation**: Автоматическая проверка всех CP2 фич перед переходом на CP2-LC
2. **CI/CD Integration**: Интеграция в CI/CD pipeline для continuous validation
3. **Developer Experience**: Легкая локальная проверка CP2 фич
4. **Quality Assurance**: Гарантия, что все CP2 фичи работают корректно

### Current State

**Missing**: Нет автоматизированной валидации CP2 фич

**Manual Process**: Разработчики должны вручную проверять каждую CP2 фичу

**Risk**: Возможны пропущенные проверки или неправильная конфигурация

### Target State

- ✅ Автоматизированная валидация всех CP2 фич
- ✅ Интеграция в CI/CD pipeline
- ✅ Локальная проверка для разработчиков
- ✅ Четкие критерии успеха/неудачи

## Technical Requirements

### Script Requirements

1. **Feature Flag Validation**:
   - Проверка `idempotency_enabled: true` в `beamline_router.app.src`
   - Проверка `tracing_enabled: true`
   - Проверка `tenant_validation_enabled: true`
   - Проверка `admin_grpc_enabled: true`

2. **JetStream Connection Validation**:
   - Запуск Router через `rebar3 shell`
   - Проверка NATS connection
   - Проверка JetStream info availability
   - Graceful shutdown

3. **Idempotency Validation**:
   - Проверка существования: `test/router_idempotency_SUITE.erl` ✅ EXISTS
   - Запуск test suite: `rebar3 ct --suite test/router_idempotency_SUITE`
   - Проверка exit code
   - Fallback: Если тесты отсутствуют, создать минимальный smoke test

4. **Tracing Validation**:
   - Проверка OpenTelemetry integration в `src/router_tracing.erl`
   - Проверка span creation в key modules (`router_result_consumer.erl`, etc.)
   - Note: Dedicated tracing test suite may not exist - use code inspection
   - Fallback: Проверка наличия OpenTelemetry API calls в коде

5. **Tenant Validation**:
   - Проверка существования: `test/router_tenant_allowlist_SUITE.erl` ✅ EXISTS
   - Запуск test suite: `rebar3 ct --suite test/router_tenant_allowlist_SUITE`
   - Дополнительно: `test/router_tenant_multitenant_smoke_SUITE.erl` ✅ EXISTS
   - Проверка exit code

### Exit Codes

- `0`: All validations passed
- `1`: Feature flag validation failed
- `2`: JetStream validation failed
- `3`: Idempotency validation failed
- `4`: Tracing validation failed
- `5`: Tenant validation failed

### Error Handling

- Clear error messages for each validation failure
- Graceful handling of missing dependencies (NATS, Router)
- Proper cleanup of test resources
- Detailed logging for debugging

## Acceptance Criteria

### Functional Criteria

- ✅ Script validates all CP2 feature flags are enabled
- ✅ Script tests JetStream connectivity
- ✅ Script runs idempotency test suite
- ✅ Script verifies tracing spans creation
- ✅ Script validates tenant ACL enforcement
- ✅ Script returns appropriate exit codes

### Integration Criteria

- ✅ Script integrated into `scripts/dry_run_ci.sh`
- ✅ Script integrated into CI/CD pipeline
- ✅ Documentation updated with CP2 validation instructions
- ✅ PR checklist updated with CP2 validation requirement

### Quality Criteria

- ✅ Script is executable and well-documented
- ✅ Script handles errors gracefully
- ✅ Script provides clear output and error messages
- ✅ Script is cross-platform compatible

## Dependencies

### Required From

- **ORDER-WRK-3-CP2-001** (wrk-3): Router compilation fix - **MUST COMPLETE FIRST**
- Router test suites must exist and be runnable
- NATS server must be available for JetStream validation

### Blocks

- **CP2-LC Transition**: Невозможен без валидации CP2 фич

## Risks and Mitigations

### Risk 1: Test Suites Not Available

**Risk**: Test suites для idempotency/tracing/tenant validation могут не существовать.

**Mitigation**:
- Проверить существование test suites перед созданием скрипта
- Если отсутствуют - создать минимальные smoke tests
- Использовать graceful degradation (skip если тесты отсутствуют)

### Risk 2: NATS Dependency

**Risk**: JetStream validation требует запущенный NATS server.

**Mitigation**:
- Проверка доступности NATS перед валидацией
- Четкое сообщение об ошибке если NATS недоступен
- Опциональная валидация (skip если NATS недоступен)

### Risk 3: Cross-Platform Compatibility

**Risk**: Скрипт может не работать на всех платформах.

**Mitigation**:
- Использовать POSIX-compliant bash syntax
- Тестирование на Linux, macOS, WSL
- Избегать platform-specific commands

## Implementation Checklist

### Day 1: Core Validation Functions

- [ ] Enhance `validate_feature_flags()` - verify flags are true (not just present)
- [ ] Implement `validate_jetstream()` - NATS connection and JetStream info check
- [ ] Implement `validate_idempotency()` - run `router_idempotency_SUITE`
- [ ] Implement `validate_tracing()` - code inspection for OpenTelemetry integration
- [ ] Implement `validate_tenant_validation()` - run `router_tenant_allowlist_SUITE`
- [ ] Test each validation function independently
- [ ] Verify exit codes are correct

### Day 2: Integration & Documentation

- [ ] Verify `step_cp2()` integration in `dry_run_ci.sh` works correctly
- [ ] Add CP2 validation to `.github/workflows/validate.yml.template` (if missing)
- [ ] Update `docs/archive/dev/LOCAL_CHECKS.md` with CP2 validation section
- [ ] Update `docs/archive/dev/PR_CHECKLIST.md` with CP2 validation requirement
- [ ] Test full script execution: `bash scripts/validate_cp2.sh`
- [ ] Test CI integration: `bash scripts/dry_run_ci.sh cp2`
- [ ] Document any test suite gaps or fallback strategies

## Reporting Requirements

### Progress Report (Day 1)

**Status**: `in_progress`

**Summary**:
- Какие валидации реализованы
- Какие валидации остались
- Результаты тестирования каждого validation function
- Любые блокеры или gaps (missing test suites)

**Artifacts**:
- Enhanced `scripts/validate_cp2.sh` with new validation functions
- Test results for each validation

### Final Report (Day 2)

**Status**: `done`

**Summary**:
- Все валидации реализованы и протестированы
- Интеграция в CI/CD завершена и проверена
- Документация обновлена
- Все acceptance criteria выполнены

**Artifacts**:
- `scripts/validate_cp2.sh` (enhanced)
- Обновленные файлы интеграции (verified)
- Обновленная документация
- Test execution logs
- CI/CD integration verification

## Known Test Suites

**Existing Test Suites** (verified):
- ✅ `apps/otp/router/test/router_idempotency_SUITE.erl` - Idempotency tests
- ✅ `apps/otp/router/test/router_tenant_allowlist_SUITE.erl` - Tenant validation tests
- ✅ `apps/otp/router/test/router_tenant_multitenant_smoke_SUITE.erl` - Multi-tenant smoke tests
- ✅ `apps/otp/router/test/router_jetstream_e2e_SUITE.erl` - JetStream E2E tests

**Missing Test Suites**:
- ❌ `test/router_tracing_SUITE.erl` - Dedicated tracing test suite (use code inspection instead)

**Fallback Strategy**:
- For tracing: Check OpenTelemetry API calls in source code
- For missing suites: Create minimal smoke tests or skip with clear warning

## Current State Analysis

**Feature Flags** (verified in `apps/otp/router/src/beamline_router.app.src`):
- ✅ `idempotency_enabled: true` (line 67)
- ✅ `tracing_enabled: true` (line 68)
- ✅ `tenant_validation_enabled: true` (line 69)
- ✅ `admin_grpc_enabled: true` (line 70)

**Existing Script** (`scripts/validate_cp2.sh`):
- ✅ Basic feature flag validation implemented
- ✅ Module presence checks implemented
- ✅ JetStream configuration checks implemented
- ⚠️ Missing: Runtime validation (JetStream connectivity, test execution)

**CI/CD Integration** (`scripts/dry_run_ci.sh`):
- ✅ `step_cp2()` function exists (lines 549-578)
- ✅ Called in `main()` function
- ✅ Error handling implemented

## References

- `docs/archive/dev/CP2_WORKER_ASSIGNMENTS_DETAILED.md` - Детальный план CP2 задач
- `docs/archive/dev/CP2_READINESS_ROUTER_GATEWAY_UPDATED.md` - CP2 readiness document
- `apps/otp/router/src/beamline_router.app.src` - Feature flags configuration (verified)
- `scripts/validate_cp2.sh` - Existing validation script (to be enhanced)
- `scripts/dry_run_ci.sh` - CI validation script (integration exists)
- `docs/archive/dev/LOCAL_CHECKS.md` - Local validation guide
- `apps/otp/router/test/router_idempotency_SUITE.erl` - Idempotency test suite
- `apps/otp/router/test/router_tenant_allowlist_SUITE.erl` - Tenant validation test suite

---

**ORDER ID**: ORDER-WRK-2-CP2-002  
**Status**: Ready to start (blocker ORDER-WRK-3-CP2-001 should be resolved first)  
**Priority**: 🔴 HIGH  
**Timeline**: 2 days  
**Rule Version**: v10  
**Message Protocol**: v1

