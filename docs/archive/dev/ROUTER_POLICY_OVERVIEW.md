# Router Policy Engine Overview

## Purpose

This document provides a comprehensive overview of the Router Policy Engine implementation, covering what has been implemented, key features, test coverage, and links to all relevant documentation.

## Status

**CP1**: ✅ **COMPLETE** - Core policy engine fully implemented, tested, and documented

**CP2**: 📅 **PLANNED** - Enhancements and admin tooling planned for CP2+

## Executive Summary

The Router Policy Engine provides a JSON-based Domain Specific Language (DSL) for defining routing policies that control provider selection, fallback behavior, sticky sessions, and extension execution. The implementation is complete for CP1 with comprehensive test coverage and documentation.

### Key Achievements

- ✅ **Full JSON-DSL Implementation**: Complete support for providers, fallbacks, sticky sessions, extensions
- ✅ **Comprehensive Test Coverage**: 11 test suites covering unit, integration, legacy compatibility, and property-based tests
- ✅ **Complete Documentation**: Full specification, implementation details, test reports, and gap analysis
- ✅ **CI Integration**: Policy schema validation integrated into CI pipeline
- ✅ **Observability**: Structured JSON logging with decision explanations
- ✅ **Backward Compatibility**: Full support for legacy policy formats

## What Has Been Implemented

### 1. Policy Engine Core

**Modules**:
- `router_policy_store.erl` - Policy loading, parsing, and storage
- `router_decider.erl` - Core decision logic (sticky, weighted, fallbacks, retry, backoff)
- `router_policy_applier.erl` - Unified policy application with explanation
- `router_sticky_store.erl` - Sticky session management

**Features**:
- **Provider Selection**: Sticky sessions, weighted round-robin, fallback chains
- **Fallback Mechanisms**: Complex fallback chains with retry and backoff
- **Sticky Sessions**: Session binding with configurable TTL
- **Extensions**: Pre-processors, validators, and post-processors
- **Decision Explanation**: Detailed audit trail of routing decisions

### 2. JSON-DSL Support

**Policy Structure**:
- `providers[]` - Provider list with weights
- `fallbacks[]` - Fallback rules with conditions (timeout, 5xx, etc.)
- `sticky` - Sticky session configuration (key, ttl)
- `pre[]` - Pre-processor extensions
- `validators[]` - Validator extensions
- `post[]` - Post-processor extensions
- `retry` - Retry configuration for fallbacks
- `backoff` - Backoff strategy (exponential, linear, fixed)

**Schema Validation**:
- `policy.schema.json` - Complete JSON Schema for policy validation
- 10 policy fixtures covering all DSL scenarios

### 3. Retry and Backoff

**Implementation**:
- Retry logic for fallback evaluation
- Backoff strategies: exponential, linear, fixed
- Configurable retry count and backoff parameters
- Integration with fallback chains

**Documentation**: `docs/archive/dev/RETRY_BACKOFF_IMPLEMENTATION.md`

### 4. Decision Explanation

**Format**: Structured JSON with required fields:
- `reason` - Decision reason (sticky, weighted, fallback, retry)
- `provider_id` - Selected provider
- `policy_id` - Policy identifier
- `policy_version` - Policy version
- `priority` - Decision priority
- `steps[]` - Step-by-step decision narrative
- `context` - Additional context (tenant_id, trace_id, etc.)

**Detail Levels**: `minimal`, `detailed`, `verbose` (CP1-nice, implemented)

**Documentation**: `docs/ROUTING_POLICY.md` (Decision Explanation Format section)

### 5. Extensions Support

**Types**:
- **Pre-processors**: Execute before provider selection
- **Validators**: Validate request before routing
- **Post-processors**: Execute after provider response

**Configuration**:
- Extension ID, type, subject, timeout
- Mode (sync/async), on_fail behavior
- Extension-specific configuration

**Documentation**: `docs/ROUTING_POLICY.md` (Extensions section), `docs/EXTENSIONS_API.md`

### 6. Observability

**Decision Logging**:
- Structured JSON logging with PII filtering
- Decision explanations logged to audit trail
- Integration with `router_audit.erl`

**Documentation**: `docs/archive/dev/POLICY_DECISION_LOGGING_REPORT.md`

### 7. Backward Compatibility

**Legacy Format Support**:
- Full support for legacy JSON policy format
- Mixed format (legacy + new) handling
- Priority rules: new format overrides legacy

**Documentation**: `docs/archive/dev/POLICY_LEGACY_COMPATIBILITY_REPORT.md`

## Test Coverage

### Test Suites

**11 Test Suites** covering all aspects of policy engine:

1. **`router_policy_applier_dsl_SUITE.erl`** - DSL unit tests
   - Provider selection (sticky, weighted, fallback)
   - Fallback chains with retry and backoff
   - Extension extraction
   - Explanation format validation
   - Edge cases

2. **`router_policy_integration_SUITE.erl`** - Integration tests
   - Full Router pipeline with JSON policies
   - Sticky-hit/sticky-miss scenarios
   - Weighted routing
   - Fallback chains
   - Extension pipeline

3. **`router_policy_legacy_compatibility_SUITE.erl`** - Legacy compatibility
   - Legacy format parsing
   - Mixed format handling
   - Behavior comparison

4. **`router_policy_structure_prop_SUITE.erl`** - Property-based tests
   - Random valid policy generation
   - Weight normalization invariants
   - Fallback chain finiteness
   - Crash absence verification

5. **`router_policy_store_SUITE.erl`** - Policy store operations
6. **`router_policy_validator_SUITE.erl`** - Policy validation
7. **`router_policy_enforcement_SUITE.erl`** - Policy enforcement
8. **`router_policy_store_prop_SUITE.erl`** - Property-based store tests
9. **`router_policy_store_load_SUITE.erl`** - Load tests
10. **`router_policy_store_fault_tolerance_SUITE.erl`** - Fault tolerance
11. **`router_policy_SUITE.erl`** - General policy tests

**Documentation**: `docs/archive/dev/POLICY_DSL_TESTS_REPORT.md`

## Documentation

### Core Documentation

1. **`docs/ROUTING_POLICY.md`** - Main policy specification
   - Complete JSON-DSL specification
   - Provider selection mechanisms
   - Fallback rules and retry/backoff
   - Extensions specification
   - Decision explanation format
   - Edge case behavior (inconsistent weights, conflicting fallbacks)
   - Deprecated fields
   - CP2 enhancements (future)

2. **`docs/archive/dev/POLICY_ENGINE_JSON_DSL_MAPPING.md`** - DSL mapping
   - Mapping between JSON-DSL and internal `#policy{}` record
   - Parsing logic and transformations

3. **`docs/archive/dev/POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md`** - Implementation details
   - Implementation architecture
   - Module responsibilities
   - Edge case handling
   - Invariants for inconsistent weights and conflicting fallbacks

### Implementation Reports

4. **`docs/archive/dev/POLICY_APPLIER_IMPLEMENTATION.md`** - Policy applier implementation
   - Unified policy application module
   - Extension extraction
   - Decision explanation generation
   - Integration with Router

5. **`docs/archive/dev/RETRY_BACKOFF_IMPLEMENTATION.md`** - Retry and backoff implementation
   - Retry logic implementation
   - Backoff strategies
   - Integration with fallbacks

6. **`docs/archive/dev/EXPLANATION_FORMAT_SPECIFICATION.md`** - Explanation format specification
   - Formal specification synchronization
   - Implementation alignment

7. **`docs/archive/dev/EXTENSIONS_IN_ROUTING_POLICY_SYNC.md`** - Extensions synchronization
   - Extensions specification in ROUTING_POLICY.md
   - Cross-links with EXTENSIONS_API.md

### Test Reports

8. **`docs/archive/dev/POLICY_DSL_TESTS_REPORT.md`** - DSL unit tests report
   - Test coverage
   - Edge cases covered
   - CP1/CP2 invariants

9. **`docs/archive/dev/POLICY_INTEGRATION_TESTS_REPORT.md`** - Integration tests report
   - Full pipeline testing
   - Real policy loading
   - Extension pipeline

10. **`docs/archive/dev/POLICY_EDGE_CASES_TESTS_REPORT.md`** - Edge cases report
    - Inconsistent weights
    - Conflicting fallbacks
    - Invalid durations
    - Unknown extension fields

11. **`docs/archive/dev/POLICY_LEGACY_COMPATIBILITY_REPORT.md`** - Legacy compatibility report
    - Legacy format support
    - Mixed format handling
    - Behavior comparison

12. **`docs/archive/dev/POLICY_DECISION_LOGGING_REPORT.md`** - Decision logging report
    - Observability integration
    - Structured JSON logging
    - PII filtering

### Gap Analysis and Classification

13. **`docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md`** - Spec vs implementation gaps
    - Gap analysis
    - Status tracking
    - Resolution notes

14. **`docs/archive/dev/POLICY_DSL_GAPS_CP_CLASSIFICATION.md`** - CP classification of gaps
    - CP1-blocker, CP1-nice, CP2 classification
    - Priority assignment

15. **`docs/archive/dev/POLICY_DSL_ALL_GAPS_FIXED_REPORT.md`** - All gaps fixed report
    - Closure of all gaps
    - CP2 enhancements specified

### Planning and Design

16. **`docs/archive/dev/POLICY_PERFORMANCE_PLAN.md`** - Performance testing plan
    - Test scenarios
    - Metrics
    - Integration with existing test suites
    - Performance targets

17. **`docs/archive/dev/POLICY_ADMIN_TOOLING_SPEC.md`** - Admin tooling specification
    - gRPC Admin API design
    - HTTP Admin API design
    - CLI tool specification
    - Policy versioning
    - Dry-run engine
    - Safe update workflow

### CP2 Enhancement Designs

18. **`docs/archive/dev/EXPLANATION_LEVELS_SPEC.md`** - Explanation detail levels specification
    - Minimal, detailed, verbose levels
    - Impact on steps and context

19. **`docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md`** - Circuit breaker design
    - Per-provider circuit breaker
    - States and transitions
    - Integration with retry/backoff and fallbacks

20. **`docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md`** - Rate limit design
    - Multi-level rate limiting (global, tenant, policy)
    - Token Bucket algorithm
    - Gateway vs Router responsibility

21. **`docs/archive/dev/TIMEOUT_HEALTH_CHECK_DESIGN.md`** - Timeout and health check design
    - Per-policy timeout
    - Health check configuration
    - Integration with fallbacks and circuit breaker

22. **`docs/archive/dev/PROVIDER_PRIORITY_DESIGN.md`** - Provider priority analysis
    - Use case analysis
    - Comparison with existing mechanisms
    - Decision to defer to CP2+

### Schema and Fixtures

23. **`apps/otp/router/docs/schemas/policy.schema.json`** - Policy JSON Schema
    - Complete schema definition
    - Validation rules
    - Deprecated fields marked

24. **`apps/otp/router/priv/fixtures/policies/`** - Policy fixtures
    - 10 fixtures covering all DSL scenarios
    - Legacy format examples
    - Mixed format examples

## CI Integration

### Validation Scripts

- **`scripts/check_policy_schema.sh`** - Policy schema and fixtures validation
  - Validates `policy.schema.json` as JSON Schema (Draft 7)
  - Validates all policy fixtures against schema
  - Exit codes: 0 (success), 1 (schema error), 2 (fixture error), 3 (tool error)

- **`scripts/check_schema_changes.sh`** - Schema changes validation
  - Includes policy schema validation
  - Checks for version bumps

- **`scripts/run_checks.sh`** - Main validation script
  - Includes policy schema validation
  - Part of CI pipeline

**Status**: ✅ **Integrated** - Policy validation is part of CI pipeline

## CP1 Status

### ✅ CP1-Complete Features

1. ✅ **JSON-DSL Implementation** - Full support for providers, fallbacks, sticky, extensions
2. ✅ **Retry and Backoff** - Implemented and tested
3. ✅ **Decision Explanation** - Formal specification and implementation
4. ✅ **Extensions Support** - Pre, validators, post extensions
5. ✅ **Observability** - Structured JSON logging with decision explanations
6. ✅ **Backward Compatibility** - Legacy format support
7. ✅ **Test Coverage** - 11 test suites covering all scenarios
8. ✅ **Documentation** - Complete specification and implementation docs
9. ✅ **CI Integration** - Policy validation in CI pipeline
10. ✅ **Schema Validation** - Complete JSON Schema with 10 fixtures

### 📋 CP2 Enhancements (Planned)

1. **Explanation Detail Levels** - ✅ Specified (CP1-nice, implemented)
2. **Future Extension Fields** - 📅 CP2 (timeout_ms, retry in extensions)
3. **Circuit Breaker** - 📅 CP2 (per-provider circuit breaker)
4. **Rate Limit** - 📅 CP2 (per-policy rate limiting)
5. **Per-Policy Timeout** - 📅 CP2 (timeout configuration)
6. **Provider Priority** - 📅 CP2+ (deferred, analysis complete)
7. **Health Check** - 📅 CP2 (health check configuration)
8. **Admin Tooling** - 📅 CP2+ (gRPC/HTTP API, CLI tool)

**Reference**: `docs/archive/dev/POLICY_DSL_GAPS_CP_CLASSIFICATION.md`

## Architecture

### Policy Application Flow

```
Route Request
    ↓
router_core.erl
    ↓
router_policy_applier.erl
    ├─→ router_policy_store.erl (load policy)
    ├─→ router_decider.erl (make decision)
    │   ├─→ router_sticky_store.erl (check sticky)
    │   └─→ Fallback evaluation (with retry/backoff)
    ├─→ Extension extraction (pre, validators, post)
    └─→ Build explanation
    ↓
Route Decision
    ├─→ Provider ID
    ├─→ Extensions
    └─→ Explanation (for audit)
```

### Key Modules

- **`router_policy_store.erl`**: Policy loading, parsing, storage
- **`router_decider.erl`**: Core decision logic
- **`router_policy_applier.erl`**: Unified policy application
- **`router_sticky_store.erl`**: Sticky session management
- **`router_audit.erl`**: Decision logging

## Integration Points

### Router Core

- **`router_core.erl`**: Integrated with `router_policy_applier` for all routing decisions

### Observability

- **`router_audit.erl`**: Logs policy decisions with structured JSON format

### Extensions

- **Extension Registry**: Policy extensions registered and executed via extension pipeline

### NATS

- **Policy Updates**: Policies can be updated via NATS (future: admin API)

## Validation Results

### Final Checks (2025-01-27)

**Status**: ✅ **ALL POLICY CHECKS PASSED**

- ✅ Policy Schema Validation: PASSED
- ✅ Policy Fixtures Validation: 10/10 PASSED
- ✅ Policy Test Suites: 11 suites verified
- ✅ Policy Documentation: Complete and linked
- ✅ CI Integration: Policy validation in CI

**Reference**: `docs/archive/dev/ROUTER_POLICY_CP1_FINAL_CHECKS.md`

## References

### Core Documentation

- `docs/ROUTING_POLICY.md` - Main policy specification
- `docs/EXTENSIONS_API.md` - Extensions API specification
- `docs/ARCHITECTURE/compatibility-rules.md` - Compatibility rules

### Implementation Documentation

- `docs/archive/dev/POLICY_ENGINE_JSON_DSL_MAPPING.md` - DSL mapping
- `docs/archive/dev/POLICY_ENGINE_JSON_DSL_IMPLEMENTATION.md` - Implementation details
- `docs/archive/dev/POLICY_APPLIER_IMPLEMENTATION.md` - Policy applier implementation

### Test Documentation

- `docs/archive/dev/POLICY_DSL_TESTS_REPORT.md` - DSL unit tests
- `docs/archive/dev/POLICY_INTEGRATION_TESTS_REPORT.md` - Integration tests
- `docs/archive/dev/POLICY_EDGE_CASES_TESTS_REPORT.md` - Edge cases
- `docs/archive/dev/POLICY_LEGACY_COMPATIBILITY_REPORT.md` - Legacy compatibility

### Gap Analysis

- `docs/archive/dev/POLICY_DSL_SPEC_VS_IMPL_GAP.md` - Spec vs implementation gaps
- `docs/archive/dev/POLICY_DSL_GAPS_CP_CLASSIFICATION.md` - CP classification
- `docs/archive/dev/POLICY_DSL_ALL_GAPS_FIXED_REPORT.md` - All gaps fixed

### Planning and Design

- `docs/archive/dev/POLICY_PERFORMANCE_PLAN.md` - Performance testing plan
- `docs/archive/dev/POLICY_ADMIN_TOOLING_SPEC.md` - Admin tooling specification
- `docs/archive/dev/CP2_ROUTER_PLAN.md` - CP2 Router plan

### CP2 Enhancement Designs

- `docs/archive/dev/EXPLANATION_LEVELS_SPEC.md` - Explanation detail levels
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Circuit breaker design
- `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` - Rate limit design
- `docs/archive/dev/TIMEOUT_HEALTH_CHECK_DESIGN.md` - Timeout and health check
- `docs/archive/dev/PROVIDER_PRIORITY_DESIGN.md` - Provider priority analysis

### Schema and Fixtures

- `apps/otp/router/docs/schemas/policy.schema.json` - Policy schema
- `apps/otp/router/priv/fixtures/policies/` - Policy fixtures

### Validation Scripts

- `scripts/check_policy_schema.sh` - Policy schema validation
- `scripts/check_schema_changes.sh` - Schema changes validation
- `scripts/run_checks.sh` - Main validation script

### Test Suites

- `apps/otp/router/test/router_policy_applier_dsl_SUITE.erl` - DSL unit tests
- `apps/otp/router/test/router_policy_integration_SUITE.erl` - Integration tests
- `apps/otp/router/test/router_policy_legacy_compatibility_SUITE.erl` - Legacy compatibility
- `apps/otp/router/test/router_policy_structure_prop_SUITE.erl` - Property-based tests

## Change History

**v1.0 (2025-01-27)**:
- Initial overview document
- Complete summary of policy engine implementation
- Links to all relevant documentation
- CP1 status and CP2 enhancements
- Final validation results

