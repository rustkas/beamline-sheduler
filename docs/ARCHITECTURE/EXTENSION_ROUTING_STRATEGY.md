# Extension Routing Strategy

**Version**: 1.0  
**Date**: 2025-01-27  
**Status**: ✅ Approved  
**Author**: wrk-2 (Router OTP) + wrk-4 (Docs/Architecture)

---

## Purpose

This document defines the strategy for routing extension versions based on multi-tenant and multi-environment contexts. It provides clear examples, best practices, and rollout procedures for safely deploying new extension versions without global risk.

---

## Overview

Extension routing allows different versions of the same extension to be used based on:

- **Tenant ID**: Route specific versions to specific tenants
- **Environment**: Route different versions for dev/stage/prod
- **Policy ID**: Route versions based on routing policy
- **Combined Conditions**: Multiple conditions (tenant + environment + policy)

This enables:

- **Canary Deployments**: Gradual rollout to subset of tenants
- **A/B Testing**: Test new versions with specific tenants
- **Environment Isolation**: Different behavior for dev/stage/prod
- **Tenant-specific Features**: Premium features for specific tenants

---

## Routing Rules Format

### JSONB Structure

Routing rules are stored in the `extension_versions.routing_rules` JSONB column:

```json
{
  "tenant_id": ["tenant1", "tenant2"],
  "environment": "prod",
  "policy_id": ["policy1", "policy2"]
}
```

### Matching Logic

1. **Empty Rules (`{}`)**: Matches by default (fallback version)
2. **List Values**: Uses "IN" semantics (tenant_id in list)
3. **String Values**: Uses exact match (environment = "prod")
4. **AND Logic**: All specified keys must match
5. **Priority**: More specific rules checked first (order by `created_at DESC`)

### Supported Context Keys

- `tenant_id` (binary/list): Tenant identifier
- `environment` (binary): Environment (dev/stage/prod)
- `policy_id` (binary/list): Routing policy identifier
- Custom keys: Any context key can be used for routing

---

## Context Propagation

### How Context is Built

Context is built in `router_decider:decide/3` and includes:

1. **From Request Context**: `ReqContext` parameter
2. **From Message**: `tenant_id` extracted from message
3. **From Application Config**: `environment` from `beamline_router.environment`
4. **From Environment Variable**: `ENVIRONMENT` env var (fallback)
5. **Merged**: All sources merged into single Context map

### Context Structure

```erlang
Context = #{
    <<"tenant_id">> => <<"tenant-123">>,
    <<"environment">> => <<"prod">>,
    <<"policy_id">> => <<"policy-default">>,
    <<"trace_id">> => <<"uuid">>,
    ... % Other context fields
}
```

### Context Usage

Context is passed to:
- `router_extension_versioning:lookup_with_version/2`
- `router_extension_versioning:select_version/2`
- `router_extension_invoker:invoke/3`

---

## Routing Scenarios

### Scenario 1: Multi-Tenant Routing

**Use Case**: Different versions for different tenants

**Example**: v1 for standard tenants, v2 for premium tenants

```sql
-- Version v1 (default for all tenants)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('normalize_text', 'v1', 'beamline.ext.pre.normalize_text.v1', '{}', true);

-- Version v2 (premium tenants only)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('normalize_text', 'v2', 'beamline.ext.pre.normalize_text.v2', 
     '{"tenant_id": ["tenant_premium_1", "tenant_premium_2", "tenant_enterprise"]}', true);
```

**Routing Logic**:
- If `tenant_id` in premium list → use v2
- Otherwise → use v1 (default)

### Scenario 2: Multi-Environment Routing

**Use Case**: Different versions for dev/stage/prod

**Example**: Different PII guard behavior per environment

```sql
-- Version v1 (production - strict)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('pii_guard', 'v1', 'beamline.ext.validate.pii_guard.v1', 
     '{"environment": "prod"}', true);

-- Version v2 (staging - relaxed for testing)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('pii_guard', 'v2', 'beamline.ext.validate.pii_guard.v2', 
     '{"environment": "stage"}', true);

-- Version v3 (development - permissive)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('pii_guard', 'v3', 'beamline.ext.validate.pii_guard.v3', 
     '{"environment": "dev"}', true);
```

**Routing Logic**:
- If `environment = "prod"` → use v1
- If `environment = "stage"` → use v2
- If `environment = "dev"` → use v3

### Scenario 3: Combined Tenant + Environment

**Use Case**: Different versions based on both tenant and environment

**Example**: Premium features in staging for premium tenants only

```sql
-- Version v1 (production for all tenants)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('mask_pii', 'v1', 'beamline.ext.post.mask_pii.v1', 
     '{"environment": "prod"}', true);

-- Version v2 (staging for premium tenants only)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('mask_pii', 'v2', 'beamline.ext.post.mask_pii.v2', 
     '{"environment": "stage", "tenant_id": ["tenant_premium_1", "tenant_premium_2"]}', true);
```

**Routing Logic**:
- If `environment = "prod"` → use v1
- If `environment = "stage"` AND `tenant_id` in premium list → use v2
- Otherwise → use v1 (default)

### Scenario 4: Canary Deployment

**Use Case**: Gradual rollout to subset of tenants

**Example**: Rollout v2 to 10% of tenants (canary), rest use v1

```sql
-- Version v1 (default for 90% of tenants)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('custom_provider_openai', 'v1', 'beamline.provider.openai.v1', '{}', true);

-- Version v2 (canary - 10% of tenants)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('custom_provider_openai', 'v2', 'beamline.provider.openai.v2', 
     '{"tenant_id": ["tenant_canary_1", "tenant_canary_2", "tenant_canary_3"]}', true);
```

**Rollout Process**:
1. **Phase 1**: Deploy v2 with canary tenant list (10%)
2. **Monitor**: Check metrics, errors, latency
3. **Phase 2**: Expand canary list (25%)
4. **Monitor**: Continue monitoring
5. **Phase 3**: Expand to all tenants (100%)
6. **Cleanup**: Remove v1, make v2 default

### Scenario 5: Policy-based Routing

**Use Case**: Different versions based on routing policy

**Example**: High-traffic policies use different rate limiter

```sql
-- Version v1 (default policy)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('rate_limiter', 'v1', 'beamline.ext.pre.rate_limiter.v1', '{}', true);

-- Version v2 (high-traffic policy)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('rate_limiter', 'v2', 'beamline.ext.pre.rate_limiter.v2', 
     '{"policy_id": ["policy_high_traffic", "policy_enterprise"]}', true);
```

**Routing Logic**:
- If `policy_id` in high-traffic list → use v2
- Otherwise → use v1 (default)

---

## Best Practices

### 1. Always Provide Default Version

**Rule**: Always have at least one version with empty routing rules (`{}`)

**Why**: Ensures fallback if no routing rules match

**Example**:
```sql
-- Default version (matches all)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('extension_id', 'v1', 'beamline.ext.pre.extension_id.v1', '{}', true);
```

### 2. Use Specific Rules First

**Rule**: More specific routing rules should be checked first

**Why**: Ensures correct version selection when multiple rules could match

**Implementation**: Versions are ordered by `created_at DESC` (newer versions checked first)

**Example**:
```sql
-- Specific rule (created later, checked first)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled, created_at) VALUES
    ('extension_id', 'v2', 'beamline.ext.pre.extension_id.v2', 
     '{"tenant_id": ["tenant1"], "environment": "prod"}', true, CURRENT_TIMESTAMP);

-- General rule (created earlier, checked later)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled, created_at) VALUES
    ('extension_id', 'v1', 'beamline.ext.pre.extension_id.v1', 
     '{"environment": "prod"}', true, CURRENT_TIMESTAMP - INTERVAL '1 day');
```

### 3. Gradual Rollout Strategy

**Rule**: Use canary deployment for risky changes

**Process**:
1. **Start Small**: 5-10% of tenants
2. **Monitor**: Watch metrics, errors, latency
3. **Expand**: Gradually increase to 25%, 50%, 100%
4. **Rollback**: If issues, disable canary version

**Example**:
```sql
-- Phase 1: 5% canary
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('extension_id', 'v2', 'beamline.ext.pre.extension_id.v2', 
     '{"tenant_id": ["tenant_canary_1"]}', true);

-- Phase 2: 25% canary (update routing_rules)
UPDATE extension_versions SET routing_rules = '{"tenant_id": ["tenant_canary_1", "tenant_canary_2", "tenant_canary_3", "tenant_canary_4", "tenant_canary_5"]}'
WHERE id = 'extension_id' AND version = 'v2';

-- Phase 3: 100% (make default)
UPDATE extension_versions SET routing_rules = '{}' WHERE id = 'extension_id' AND version = 'v2';
UPDATE extension_versions SET enabled = false WHERE id = 'extension_id' AND version = 'v1';
```

### 4. Environment Isolation

**Rule**: Always separate dev/stage/prod versions

**Why**: Prevents production issues from affecting development

**Example**:
```sql
-- Production version
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('extension_id', 'v1', 'beamline.ext.pre.extension_id.v1', 
     '{"environment": "prod"}', true);

-- Staging version (with test features)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('extension_id', 'v1-staging', 'beamline.ext.pre.extension_id.v1.staging', 
     '{"environment": "stage"}', true);
```

### 5. Tenant Segmentation

**Rule**: Use tenant lists for feature flags and A/B testing

**Why**: Enables safe testing with specific tenants

**Example**:
```sql
-- Beta feature for beta tenants
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('extension_id', 'v2-beta', 'beamline.ext.pre.extension_id.v2', 
     '{"tenant_id": ["tenant_beta_1", "tenant_beta_2", "tenant_beta_3"]}', true);
```

### 6. Monitoring and Alerting

**Rule**: Monitor version usage and errors per version

**Metrics to Track**:
- Version selection rate (how often each version is selected)
- Error rate per version
- Latency per version
- Circuit breaker state per version

**Example Queries**:
```sql
-- Version usage statistics
SELECT 
    ev.id,
    ev.version,
    ev.routing_rules,
    COUNT(*) as usage_count,
    AVG(eh.avg_latency_ms) as avg_latency,
    SUM(eh.failure_count) as total_failures
FROM extension_versions ev
LEFT JOIN extension_health eh ON ev.id = eh.extension_id
GROUP BY ev.id, ev.version, ev.routing_rules;
```

---

## Rollout Procedures

### Standard Rollout (Low Risk)

**Use Case**: Non-breaking changes, well-tested

**Process**:
1. Deploy new version with empty routing rules (default)
2. Monitor for 24-48 hours
3. If stable, disable old version

**SQL**:
```sql
-- Deploy v2 as default
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('extension_id', 'v2', 'beamline.ext.pre.extension_id.v2', '{}', true);

-- After monitoring, disable v1
UPDATE extension_versions SET enabled = false WHERE id = 'extension_id' AND version = 'v1';
```

### Canary Rollout (Medium Risk)

**Use Case**: New features, potential breaking changes

**Process**:
1. Deploy v2 with canary tenant list (5-10%)
2. Monitor for 24-48 hours
3. Expand canary list (25%, 50%, 100%)
4. If issues, disable canary version

**SQL**:
```sql
-- Phase 1: Canary (10%)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('extension_id', 'v2', 'beamline.ext.pre.extension_id.v2', 
     '{"tenant_id": ["tenant_canary_1", "tenant_canary_2"]}', true);

-- Phase 2: Expand (25%)
UPDATE extension_versions SET routing_rules = '{"tenant_id": ["tenant_canary_1", "tenant_canary_2", "tenant_canary_3", "tenant_canary_4", "tenant_canary_5"]}'
WHERE id = 'extension_id' AND version = 'v2';

-- Phase 3: Full rollout (100%)
UPDATE extension_versions SET routing_rules = '{}' WHERE id = 'extension_id' AND version = 'v2';
UPDATE extension_versions SET enabled = false WHERE id = 'extension_id' AND version = 'v1';
```

### Blue-Green Rollout (High Risk)

**Use Case**: Major version changes, breaking changes

**Process**:
1. Deploy v2 with specific tenant list (blue)
2. Keep v1 for all other tenants (green)
3. Gradually migrate tenants from v1 to v2
4. Monitor each migration
5. Complete migration, disable v1

**SQL**:
```sql
-- Blue: v2 for specific tenants
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('extension_id', 'v2', 'beamline.ext.pre.extension_id.v2', 
     '{"tenant_id": ["tenant_blue_1", "tenant_blue_2"]}', true);

-- Green: v1 for all others (default)
-- (Already exists with routing_rules = '{}')
```

---

## Rollback Procedures

### Immediate Rollback

**Use Case**: Critical issues detected

**Process**:
1. Disable problematic version
2. Verify fallback to default version
3. Investigate and fix issues

**SQL**:
```sql
-- Disable problematic version
UPDATE extension_versions SET enabled = false 
WHERE id = 'extension_id' AND version = 'v2';

-- Verify default version is active
SELECT * FROM extension_versions 
WHERE id = 'extension_id' AND enabled = true AND routing_rules = '{}';
```

### Gradual Rollback

**Use Case**: Minor issues, want to reduce exposure

**Process**:
1. Reduce canary tenant list
2. Monitor improvement
3. Continue rollback if needed

**SQL**:
```sql
-- Reduce canary list (from 5 tenants to 1)
UPDATE extension_versions SET routing_rules = '{"tenant_id": ["tenant_canary_1"]}'
WHERE id = 'extension_id' AND version = 'v2';
```

---

## Implementation Details

### Code Flow

1. **Request Arrives**: `router_decider:decide/3` receives request
2. **Context Built**: Context includes `tenant_id`, `environment`, `policy_id`
3. **Extension Lookup**: `router_extension_invoker:invoke/3` called
4. **Version Selection**: `router_extension_versioning:lookup_with_version/2` called
5. **Routing Rules Match**: `select_version/2` matches routing rules against context
6. **Version Selected**: Matching version returned
7. **Extension Invoked**: Selected version used for NATS request

### Database Schema

**Table**: `extension_versions`

**Key Columns**:
- `id` (VARCHAR): Extension identifier
- `version` (VARCHAR): Version identifier (v1, v2, etc.)
- `routing_rules` (JSONB): Routing rules for version selection
- `enabled` (BOOLEAN): Enable/disable flag

**Primary Key**: `(id, version)`

### Matching Algorithm

**Function**: `router_extension_versioning:match_routing_rules/2`

**Algorithm**:
1. If routing_rules is empty → match (default)
2. For each key in routing_rules:
   - Get context value for key
   - If list → check if context value in list
   - If string → check exact match
   - If no match → return false
3. If all keys match → return true

---

## Examples

See `sql/012_extension_routing_examples.sql` for complete SQL examples.

### Example 1: Multi-Tenant

```sql
-- Default version
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('normalize_text', 'v1', 'beamline.ext.pre.normalize_text.v1', '{}', true);

-- Premium version
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('normalize_text', 'v2', 'beamline.ext.pre.normalize_text.v2', 
     '{"tenant_id": ["tenant_premium_1", "tenant_premium_2"]}', true);
```

### Example 2: Multi-Environment

```sql
-- Production
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('pii_guard', 'v1', 'beamline.ext.validate.pii_guard.v1', 
     '{"environment": "prod"}', true);

-- Staging
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('pii_guard', 'v2', 'beamline.ext.validate.pii_guard.v2', 
     '{"environment": "stage"}', true);
```

### Example 3: Canary Deployment

```sql
-- Default (90%)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('custom_provider_openai', 'v1', 'beamline.provider.openai.v1', '{}', true);

-- Canary (10%)
INSERT INTO extension_versions (id, version, subject, routing_rules, enabled) VALUES
    ('custom_provider_openai', 'v2', 'beamline.provider.openai.v2', 
     '{"tenant_id": ["tenant_canary_1", "tenant_canary_2", "tenant_canary_3"]}', true);
```

---

## Testing

### Unit Tests

**Module**: `router_extension_versioning_SUITE.erl` (to be created)

**Test Cases**:
- `test_routing_tenant_id_match`
- `test_routing_environment_match`
- `test_routing_policy_id_match`
- `test_routing_combined_conditions`
- `test_routing_fallback_to_default`
- `test_routing_list_values`
- `test_routing_exact_match`

### Integration Tests

**Module**: `router_extensions_e2e_SUITE.erl`

**Test Cases**:
- `test_e2e_tenant_routing`
- `test_e2e_environment_routing`
- `test_e2e_canary_deployment`

---

## Monitoring

### Metrics

**Telemetry Events**:
- `[router_extension_versioning, version_selected]` - Version selection event
- `[router_extension_versioning, routing_rule_matched]` - Routing rule match event
- `[router_extension_versioning, fallback_to_default]` - Fallback event

**Metrics**:
- Version selection rate per extension
- Routing rule match rate
- Fallback rate

### Logging

**Structured Logs**:
```json
{
  "level": "INFO",
  "message": "Extension version selected",
  "extension_id": "normalize_text",
  "version": "v2",
  "tenant_id": "tenant_premium_1",
  "environment": "prod",
  "routing_rules_matched": {"tenant_id": ["tenant_premium_1", "tenant_premium_2"]}
}
```

---

## References

- `apps/otp/router/src/router_extension_versioning.erl` - Version selection implementation
- `apps/otp/router/src/router_extension_registry_db.erl` - Database integration
- `sql/011_extensions_registry.sql` - Database schema
- `sql/012_extension_routing_examples.sql` - SQL examples
- `docs/EXTENSIONS_API.md` - Extensions API documentation
- `docs/archive/dev/EXTENSION_ADVANCED_FEATURES_REPORT.md` - Advanced features report

---

## Change History

**v1.0 (2025-01-27)**:
- Initial version
- Multi-tenant and multi-environment routing
- Canary deployment procedures
- Best practices and examples

