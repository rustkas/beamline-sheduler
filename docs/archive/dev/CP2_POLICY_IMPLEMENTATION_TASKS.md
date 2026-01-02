# CP2 Policy Features Implementation Tasks

## Purpose

This document breaks down CP2 policy features into concrete implementation tasks with dependencies and ordering.

## Status

📅 **CP2+ Planning** - Detailed task breakdown for implementation

## Overview

This document provides:
- **Task Breakdown**: 3-7 technical subtasks per CP2 feature
- **Dependencies**: What can be done in parallel, what requires sequential execution
- **Module/File Mapping**: Specific files and modules to implement
- **Test Suites**: Test suites to create/update

## Circuit Breaker Implementation Tasks

### Task 1: Circuit Breaker State Machine Module

**Module**: `router_circuit_breaker.erl` (new)

**Files**:
- `apps/otp/router/src/router_circuit_breaker.erl`
- `apps/otp/router/include/router_circuit_breaker.hrl` (if needed)

**Sub-tasks**:
1. Define circuit breaker state record (`#circuit_breaker_state{}`)
2. Implement state transitions (Closed → Open → Half-Open → Closed)
3. Implement state persistence (ETS or external store)
4. Implement state query functions (`get_state/2`, `is_open/2`)

**Dependencies**: None (can start immediately)

**Test Suite**: `router_circuit_breaker_SUITE.erl` (new)

### Task 2: Circuit Breaker Metrics Collection

**Module**: `router_circuit_breaker_metrics.erl` (new)

**Files**:
- `apps/otp/router/src/router_circuit_breaker_metrics.erl`

**Sub-tasks**:
1. Track error counts per provider
2. Track timeout counts per provider
3. Track error rate (errors / total requests)
4. Implement sliding window for error rate calculation
5. Emit metrics to Prometheus (if metrics infrastructure exists)

**Dependencies**: Task 1 (needs circuit breaker state)

**Test Suite**: `router_circuit_breaker_metrics_SUITE.erl` (new)

### Task 3: Circuit Breaker Integration with Policy Applier

**Module**: `router_policy_applier.erl` (modify)

**Files**:
- `apps/otp/router/src/router_policy_applier.erl`

**Sub-tasks**:
1. Check circuit breaker state before provider selection
2. Skip providers with open circuit breaker
3. Update circuit breaker state after provider call (success/failure)
4. Integrate with fallback logic (circuit open → fallback)
5. Update explanation to include circuit breaker state

**Dependencies**: Task 1, Task 2 (needs state machine and metrics)

**Test Suite**: `router_policy_applier_dsl_SUITE.erl` (update), `router_policy_integration_SUITE.erl` (update)

### Task 4: Circuit Breaker Configuration Parsing

**Module**: `router_policy_store.erl` (modify)

**Files**:
- `apps/otp/router/src/router_policy_store.erl`

**Sub-tasks**:
1. Parse `circuit_breaker` block from policy JSON
2. Store circuit breaker config in `#policy{}` record
3. Validate circuit breaker configuration (thresholds, timeouts)
4. Handle per-provider overrides (if implemented)

**Dependencies**: None (can be done in parallel with Task 1)

**Test Suite**: `router_policy_store_SUITE.erl` (update)

### Task 5: Circuit Breaker Schema and Fixtures

**Files**:
- `apps/otp/router/docs/schemas/policy.schema.json` (update)
- `apps/otp/router/priv/fixtures/policies/*/circuit_breaker*.json` (new)

**Sub-tasks**:
1. Add `circuit_breaker` to policy schema
2. Create fixtures with circuit breaker configuration
3. Update schema validation tests

**Dependencies**: Task 4 (needs configuration structure)

**Test Suite**: `router_policy_validator_SUITE.erl` (update)

### Task 6: Circuit Breaker Observability

**Module**: `router_audit.erl` (modify)

**Files**:
- `apps/otp/router/src/router_audit.erl`

**Sub-tasks**:
1. Log circuit breaker state transitions
2. Include circuit breaker state in decision explanations
3. Emit circuit breaker events to audit trail

**Dependencies**: Task 1, Task 3 (needs state machine and integration)

**Test Suite**: `router_policy_decision_logging_SUITE.erl` (new or update)

### Task 7: Circuit Breaker Documentation

**Files**:
- `docs/ROUTING_POLICY.md` (update - already has section)
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` (update with implementation notes)

**Sub-tasks**:
1. Update ROUTING_POLICY.md with implementation details
2. Add operational guide section for circuit breaker
3. Update API documentation

**Dependencies**: All previous tasks (documentation after implementation)

**Test Suite**: N/A (documentation)

**Parallel Execution**: Tasks 1, 4, 5 can be done in parallel

**Sequential Dependencies**:
- Task 2 → Task 1
- Task 3 → Task 1, Task 2
- Task 6 → Task 1, Task 3
- Task 7 → All tasks

## Rate Limit Implementation Tasks

### Task 1: Rate Limit Storage Module

**Module**: `router_rate_limit_store.erl` (new)

**Files**:
- `apps/otp/router/src/router_rate_limit_store.erl`

**Sub-tasks**:
1. Implement Token Bucket algorithm
2. Implement rate limit storage (ETS or external store)
3. Implement per-tenant, per-policy rate limit tracking
4. Implement rate limit check function (`check_rate_limit/4`)
5. Implement rate limit update function (`consume_tokens/4`)

**Dependencies**: None (can start immediately)

**Test Suite**: `router_rate_limit_store_SUITE.erl` (new)

### Task 2: Rate Limit Configuration Parsing

**Module**: `router_policy_store.erl` (modify)

**Files**:
- `apps/otp/router/src/router_policy_store.erl`

**Sub-tasks**:
1. Parse `rate_limit` block from policy JSON
2. Store rate limit config in `#policy{}` record
3. Validate rate limit configuration (per_minute, burst, scope)
4. Handle per-tenant overrides (if implemented)

**Dependencies**: None (can be done in parallel with Task 1)

**Test Suite**: `router_policy_store_SUITE.erl` (update)

### Task 3: Rate Limit Integration with Policy Applier

**Module**: `router_policy_applier.erl` (modify)

**Files**:
- `apps/otp/router/src/router_policy_applier.erl`

**Sub-tasks**:
1. Check rate limit before provider selection
2. Return rate limit error if limit exceeded
3. Update rate limit counters after request
4. Integrate with Gateway rate limiting (coordination)
5. Update explanation to include rate limit status

**Dependencies**: Task 1, Task 2 (needs storage and config)

**Test Suite**: `router_policy_applier_dsl_SUITE.erl` (update), `router_policy_integration_SUITE.erl` (update)

### Task 4: Rate Limit Schema and Fixtures

**Files**:
- `apps/otp/router/docs/schemas/policy.schema.json` (update)
- `apps/otp/router/priv/fixtures/policies/*/rate_limit*.json` (new)

**Sub-tasks**:
1. Add `rate_limit` to policy schema
2. Create fixtures with rate limit configuration
3. Update schema validation tests

**Dependencies**: Task 2 (needs configuration structure)

**Test Suite**: `router_policy_validator_SUITE.erl` (update)

### Task 5: Rate Limit Observability

**Module**: `router_audit.erl` (modify)

**Files**:
- `apps/otp/router/src/router_audit.erl`

**Sub-tasks**:
1. Log rate limit violations
2. Include rate limit status in decision explanations
3. Emit rate limit metrics to Prometheus

**Dependencies**: Task 1, Task 3 (needs storage and integration)

**Test Suite**: `router_policy_decision_logging_SUITE.erl` (new or update)

### Task 6: Rate Limit Gateway Coordination

**Module**: `router_rate_limit_gateway.erl` (new, optional)

**Files**:
- `apps/otp/router/src/router_rate_limit_gateway.erl`

**Sub-tasks**:
1. Implement Gateway↔Router rate limit coordination protocol
2. Handle global rate limits (shared between Gateway and Router)
3. Implement rate limit synchronization (if needed)

**Dependencies**: Task 1, Task 3 (needs storage and integration)

**Test Suite**: `router_rate_limit_gateway_SUITE.erl` (new)

**Parallel Execution**: Tasks 1, 2, 4 can be done in parallel

**Sequential Dependencies**:
- Task 3 → Task 1, Task 2
- Task 5 → Task 1, Task 3
- Task 6 → Task 1, Task 3

## Timeout and Health Check Implementation Tasks

### Task 1: Timeout Configuration Module

**Module**: `router_timeout_manager.erl` (new)

**Files**:
- `apps/otp/router/src/router_timeout_manager.erl`

**Sub-tasks**:
1. Implement per-policy timeout configuration
2. Implement per-provider timeout override
3. Implement timeout enforcement (kill request after timeout)
4. Implement timeout event generation

**Dependencies**: None (can start immediately)

**Test Suite**: `router_timeout_manager_SUITE.erl` (new)

### Task 2: Health Check Module

**Module**: `router_health_check.erl` (new)

**Files**:
- `apps/otp/router/src/router_health_check.erl`

**Sub-tasks**:
1. Implement health check state tracking (healthy/unhealthy)
2. Implement health check criteria evaluation
3. Implement health check scheduling (periodic checks)
4. Implement health check storage (ETS or external store)

**Dependencies**: None (can be done in parallel with Task 1)

**Test Suite**: `router_health_check_SUITE.erl` (new)

### Task 3: Health Check Configuration Parsing

**Module**: `router_policy_store.erl` (modify)

**Files**:
- `apps/otp/router/src/router_policy_store.erl`

**Sub-tasks**:
1. Parse `health_check` block from policy JSON
2. Store health check config in `#policy{}` record
3. Validate health check configuration (criteria, interval)
4. Handle per-provider overrides

**Dependencies**: None (can be done in parallel with Task 1, Task 2)

**Test Suite**: `router_policy_store_SUITE.erl` (update)

### Task 4: Timeout Configuration Parsing

**Module**: `router_policy_store.erl` (modify)

**Files**:
- `apps/otp/router/src/router_policy_store.erl`

**Sub-tasks**:
1. Parse `timeout_ms` from policy JSON
2. Store timeout config in `#policy{}` record
3. Validate timeout configuration (positive integer)
4. Handle per-provider overrides

**Dependencies**: None (can be done in parallel with Task 1, Task 2, Task 3)

**Test Suite**: `router_policy_store_SUITE.erl` (update)

### Task 5: Integration with Policy Applier

**Module**: `router_policy_applier.erl` (modify)

**Files**:
- `apps/otp/router/src/router_policy_applier.erl`

**Sub-tasks**:
1. Check health check state before provider selection
2. Skip unhealthy providers
3. Apply timeout configuration to provider calls
4. Integrate with fallback logic (timeout → fallback, unhealthy → fallback)
5. Integrate with circuit breaker (unhealthy → circuit open)
6. Update explanation to include timeout and health check status

**Dependencies**: Task 1, Task 2, Task 3, Task 4 (needs all components)

**Test Suite**: `router_policy_applier_dsl_SUITE.erl` (update), `router_policy_integration_SUITE.erl` (update)

### Task 6: Timeout and Health Check Schema and Fixtures

**Files**:
- `apps/otp/router/docs/schemas/policy.schema.json` (update)
- `apps/otp/router/priv/fixtures/policies/*/timeout*.json` (new)
- `apps/otp/router/priv/fixtures/policies/*/health_check*.json` (new)

**Sub-tasks**:
1. Add `timeout_ms` and `health_check` to policy schema
2. Create fixtures with timeout and health check configuration
3. Update schema validation tests

**Dependencies**: Task 3, Task 4 (needs configuration structure)

**Test Suite**: `router_policy_validator_SUITE.erl` (update)

### Task 7: Timeout and Health Check Observability

**Module**: `router_audit.erl` (modify)

**Files**:
- `apps/otp/router/src/router_audit.erl`

**Sub-tasks**:
1. Log timeout events
2. Log health check state transitions
3. Include timeout and health check status in decision explanations
4. Emit metrics to Prometheus

**Dependencies**: Task 1, Task 2, Task 5 (needs all components and integration)

**Test Suite**: `router_policy_decision_logging_SUITE.erl` (new or update)

**Parallel Execution**: Tasks 1, 2, 3, 4 can be done in parallel

**Sequential Dependencies**:
- Task 5 → Task 1, Task 2, Task 3, Task 4
- Task 6 → Task 3, Task 4
- Task 7 → Task 1, Task 2, Task 5

## Policy Admin Tooling Implementation Tasks

### Task 1: Policy Versioning Module

**Module**: `router_policy_versioning.erl` (new)

**Files**:
- `apps/otp/router/src/router_policy_versioning.erl`

**Sub-tasks**:
1. Implement policy version storage (version history)
2. Implement version creation (auto-increment or specified)
3. Implement version activation (deactivate previous, activate new)
4. Implement version rollback
5. Implement version history query

**Dependencies**: None (can start immediately)

**Test Suite**: `router_policy_versioning_SUITE.erl` (new)

### Task 2: Policy Validation Module

**Module**: `router_policy_validator.erl` (new or extend existing)

**Files**:
- `apps/otp/router/src/router_policy_validator.erl`

**Sub-tasks**:
1. Implement schema validation (using `policy.schema.json`)
2. Implement business logic validation (provider existence, extension existence)
3. Implement validation error/warning reporting
4. Integrate with policy store

**Dependencies**: None (can be done in parallel with Task 1)

**Test Suite**: `router_policy_validator_SUITE.erl` (update or new)

### Task 3: Dry-Run Engine Module

**Module**: `router_policy_dry_run.erl` (new)

**Files**:
- `apps/otp/router/src/router_policy_dry_run.erl`

**Sub-tasks**:
1. Implement dry-run decision logic (same as production, no side effects)
2. Mock dependencies (NATS, provider calls, sticky store updates)
3. Return decision result with explanation
4. Integrate with policy applier

**Dependencies**: None (can be done in parallel with Task 1, Task 2)

**Test Suite**: `router_policy_dry_run_SUITE.erl` (new)

### Task 4: gRPC Admin API Extension

**Module**: `router_admin_grpc.erl` (modify)

**Files**:
- `apps/otp/router/src/router_admin_grpc.erl`
- `proto/beamline/router/v1/admin.proto` (update)

**Sub-tasks**:
1. Add policy management RPCs to admin.proto
2. Implement `ListPolicies` RPC
3. Implement `GetPolicy` RPC
4. Implement `ValidatePolicy` RPC
5. Implement `DryRunPolicyDecision` RPC
6. Implement `UpdatePolicy` RPC
7. Implement `RollbackPolicy` RPC
8. Implement `GetPolicyHistory` RPC

**Dependencies**: Task 1, Task 2, Task 3 (needs versioning, validation, dry-run)

**Test Suite**: `router_admin_grpc_policy_SUITE.erl` (new)

### Task 5: HTTP Admin API

**Module**: `router_admin_http.erl` (new or extend existing)

**Files**:
- `apps/otp/router/src/router_admin_http.erl`

**Sub-tasks**:
1. Implement REST endpoints for policy management
2. Implement authentication and authorization
3. Implement JSON request/response handling
4. Integrate with gRPC admin service or policy modules

**Dependencies**: Task 4 (can reuse gRPC logic or implement independently)

**Test Suite**: `router_admin_http_policy_SUITE.erl` (new)

### Task 6: CLI Tool

**Module**: `router_policy_cli` (new escript or separate tool)

**Files**:
- `apps/otp/router/priv/bin/router-policy` (new)
- Or separate tool in `tools/router-policy/`

**Sub-tasks**:
1. Implement CLI command parsing
2. Implement gRPC/HTTP client for admin API
3. Implement output formatting (table, JSON, YAML)
4. Implement authentication support
5. Implement all commands (list, get, validate, dry-run, update, rollback, history)

**Dependencies**: Task 4, Task 5 (needs admin API)

**Test Suite**: `router_policy_cli_SUITE.erl` (new)

### Task 7: Admin Tooling Documentation

**Files**:
- `docs/archive/dev/POLICY_ADMIN_TOOLING_SPEC.md` (update with implementation notes)
- `docs/OPERATIONAL_GUIDE.md` (add admin tooling section)

**Sub-tasks**:
1. Update specification with implementation details
2. Add operational guide for admin tooling
3. Add API documentation (OpenAPI/Swagger for HTTP API)

**Dependencies**: All previous tasks (documentation after implementation)

**Test Suite**: N/A (documentation)

**Parallel Execution**: Tasks 1, 2, 3 can be done in parallel

**Sequential Dependencies**:
- Task 4 → Task 1, Task 2, Task 3
- Task 5 → Task 4 (or can be parallel if independent implementation)
- Task 6 → Task 4, Task 5
- Task 7 → All tasks

## Policy Performance Testing Tasks

### Task 1: Load Test Infrastructure

**Files**:
- `apps/otp/router/test/router_policy_applier_load_SUITE.erl` (new)
- `apps/otp/router/test/router_policy_nats_load_SUITE.erl` (new)

**Sub-tasks**:
1. Set up load test infrastructure (Common Test load testing)
2. Implement test data generation (policies, requests)
3. Implement metrics collection
4. Implement test scenarios (8 scenarios from plan)

**Dependencies**: None (can start immediately)

**Test Suite**: New test suites

### Task 2: Performance Metrics Collection

**Module**: `router_policy_metrics.erl` (new or extend existing)

**Files**:
- `apps/otp/router/src/router_policy_metrics.erl`

**Sub-tasks**:
1. Implement Router-level metrics (latency, throughput, errors)
2. Implement Policy Store metrics (cache hit rate, load time)
3. Implement Sticky Store metrics (lookup time, storage size)
4. Implement Provider Selection metrics (distribution, selection time)
5. Implement Extension Pipeline metrics (execution time, throughput)
6. Implement NATS metrics (publish latency, message size)

**Dependencies**: None (can be done in parallel with Task 1)

**Test Suite**: `router_policy_metrics_SUITE.erl` (new)

### Task 3: Performance Test Execution

**Files**:
- Test execution scripts and configuration

**Sub-tasks**:
1. Execute all 8 test scenarios
2. Collect metrics for each scenario
3. Generate performance reports
4. Compare against performance targets

**Dependencies**: Task 1, Task 2 (needs infrastructure and metrics)

**Test Suite**: N/A (test execution)

### Task 4: Performance Analysis and Optimization

**Files**:
- Performance analysis reports

**Sub-tasks**:
1. Analyze performance bottlenecks
2. Identify optimization opportunities
3. Implement optimizations (if needed)
4. Re-run tests to verify improvements

**Dependencies**: Task 3 (needs test results)

**Test Suite**: N/A (analysis and optimization)

**Parallel Execution**: Tasks 1, 2 can be done in parallel

**Sequential Dependencies**:
- Task 3 → Task 1, Task 2
- Task 4 → Task 3

## Overall Implementation Order

### Phase 1: Foundation (Can be done in parallel)

1. Circuit Breaker: Task 1, Task 4, Task 5
2. Rate Limit: Task 1, Task 2, Task 4
3. Timeout/Health Check: Task 1, Task 2, Task 3, Task 4
4. Admin Tooling: Task 1, Task 2, Task 3
5. Performance Testing: Task 1, Task 2

### Phase 2: Integration (Sequential)

1. Circuit Breaker: Task 2, Task 3, Task 6
2. Rate Limit: Task 3, Task 5, Task 6
3. Timeout/Health Check: Task 5, Task 6, Task 7
4. Admin Tooling: Task 4, Task 5, Task 6, Task 7

### Phase 3: Testing and Documentation

1. All features: Final test suites and documentation
2. Performance Testing: Task 3, Task 4

## References

- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Circuit breaker design
- `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` - Rate limit design
- `docs/archive/dev/TIMEOUT_HEALTH_CHECK_DESIGN.md` - Timeout and health check design
- `docs/archive/dev/POLICY_ADMIN_TOOLING_SPEC.md` - Admin tooling specification
- `docs/archive/dev/POLICY_PERFORMANCE_PLAN.md` - Performance testing plan
- `docs/archive/dev/CP2_ROUTER_PLAN.md` - CP2 Router plan

## Change History

**v1.0 (2025-01-27)**:
- Initial task breakdown
- Circuit breaker: 7 tasks
- Rate limit: 6 tasks
- Timeout/Health check: 7 tasks
- Admin tooling: 7 tasks
- Performance testing: 4 tasks

