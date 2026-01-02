# Explanation Detail Levels Specification (Draft)

## Purpose

This document defines the concept and specification for explanation detail levels in routing policy decisions. Detail levels allow controlling the verbosity of explanation data in logs and audit trails without breaking the contract.

## Status

📅 **CP2 Enhancement** (Draft Specification)

**Current Implementation**: Basic support exists in `router_policy_applier.erl` with three levels (minimal, detailed, verbose). This document provides a comprehensive specification for CP2 implementation.

## Overview

**Problem**: Explanation data can be verbose, especially in high-traffic scenarios. Different use cases require different levels of detail:
- **Production logs**: Minimal data for performance and storage efficiency
- **Audit trails**: Standard detail for compliance and debugging
- **Development/Debugging**: Maximum detail for troubleshooting

**Solution**: Detail levels control what information is included in `steps` and `context` fields of explanation.

## Detail Levels

### Level 1: `"minimal"`

**Purpose**: Production logging with minimal data footprint

**Use Cases**:
- High-traffic production environments
- Cost-sensitive log storage
- Performance-critical paths

**Impact on `steps`**:
- Only essential decision steps (1-2 steps maximum)
- Short, concise messages (e.g., `"1. Sticky session: found"`, `"2. Weighted: 2 providers"`)
- No detailed context in step messages
- Skipped steps are omitted (not mentioned)

**Impact on `context`**:
- Only required fields: `tenant_id`
- Optional fields excluded: `trace_id`, `retry_attempt`, `retry_max`, `backoff_ms`, `backoff_strategy`, `fallback_rule_id`
- No provider-specific metadata
- No timing information

**Example**:
```json
{
  "reason": "sticky",
  "provider_id": "provider_a",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 100,
  "steps": [
    "1. Sticky session: found"
  ],
  "context": {
    "tenant_id": "tenant_123"
  }
}
```

### Level 2: `"detailed"` (Default)

**Purpose**: Standard audit trail with key information

**Use Cases**:
- Standard production audit trails
- Compliance requirements
- General debugging

**Impact on `steps`**:
- All decision steps included (sticky, weighted, fallback)
- Key information in step messages (e.g., `"1. Checked sticky session: found existing provider for key session_id = abc123"`)
- Retry/backoff information included when applicable
- Skipped steps mentioned briefly (e.g., `"2. Skipped weighted distribution (provider selected via sticky)"`)

**Impact on `context`**:
- Required fields: `tenant_id`
- Key optional fields: `trace_id`, `retry_attempt`, `retry_max`, `backoff_ms` (when applicable)
- Fallback rule identifier (when applicable)
- No provider-specific metadata
- No detailed timing information

**Example**:
```json
{
  "reason": "sticky",
  "provider_id": "provider_a",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 100,
  "steps": [
    "1. Checked sticky session: found existing provider for key session_id = abc123",
    "2. Skipped weighted distribution (provider selected via sticky)"
  ],
  "context": {
    "tenant_id": "tenant_123",
    "trace_id": "trace_456"
  }
}
```

### Level 3: `"verbose"` (Debug/Audit)

**Purpose**: Maximum detail for debugging and deep audit

**Use Cases**:
- Development and testing
- Deep troubleshooting
- Comprehensive audit requirements
- Performance analysis

**Impact on `steps`**:
- All decision steps with maximum detail
- Full context in step messages (e.g., TTL, individual provider weights, backoff strategy, fallback rule ID)
- All skipped steps explained in detail
- Timing information included where available
- Provider-specific metadata included

**Impact on `context`**:
- All available fields included:
  - Required: `tenant_id`
  - Optional: `trace_id`, `retry_attempt`, `retry_max`, `retry_attempts_used`, `backoff_ms`, `backoff_strategy`, `fallback_rule_id`
  - Provider metadata: `provider_timeout_ms`, `provider_priority`, `provider_weight`
  - Timing: `decision_latency_ms`, `sticky_lookup_latency_ms`
  - Policy metadata: `policy_loaded_at`, `policy_cache_hit`

**Example**:
```json
{
  "reason": "sticky",
  "provider_id": "provider_a",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 100,
  "steps": [
    "1. Checked sticky session: found existing provider for key session_id = abc123 (TTL: 10m, lookup_latency: 2ms)",
    "2. Skipped weighted distribution (provider selected via sticky): 2 providers available (provider_a: 0.70, provider_b: 0.30, total: 1.00)"
  ],
  "context": {
    "tenant_id": "tenant_123",
    "trace_id": "trace_456",
    "sticky_lookup_latency_ms": 2,
    "decision_latency_ms": 5,
    "policy_cache_hit": true,
    "provider_weight": 0.70,
    "provider_priority": 10
  }
}
```

## Level Comparison Matrix

| Field/Step | Minimal | Detailed | Verbose |
|------------|---------|----------|---------|
| **Steps Count** | 1-2 essential | All steps | All steps + details |
| **Step Detail** | Short messages | Key information | Full context |
| **Skipped Steps** | Omitted | Mentioned briefly | Explained in detail |
| **context.tenant_id** | ✅ Required | ✅ Required | ✅ Required |
| **context.trace_id** | ❌ Excluded | ✅ Included | ✅ Included |
| **context.retry_*** | ❌ Excluded | ✅ When applicable | ✅ Always (if available) |
| **context.backoff_*** | ❌ Excluded | ✅ When applicable | ✅ Always (if available) |
| **context.fallback_rule_id** | ❌ Excluded | ✅ When applicable | ✅ Always (if available) |
| **context.provider_metadata** | ❌ Excluded | ❌ Excluded | ✅ Included |
| **context.timing** | ❌ Excluded | ❌ Excluded | ✅ Included |
| **context.policy_metadata** | ❌ Excluded | ❌ Excluded | ✅ Included |

## Implementation Guidelines

### Step Generation Rules

**Minimal Level**:
- Include only steps that directly led to provider selection
- Omit skipped steps (sticky not found, weights not used, fallbacks not triggered)
- Use shortest possible messages
- No context details in step messages

**Detailed Level**:
- Include all decision steps (sticky check, weighted distribution, fallback evaluation)
- Mention skipped steps with brief explanation
- Include key identifiers (session key, provider count, retry counts)
- No timing or metadata in step messages

**Verbose Level**:
- Include all decision steps with full context
- Explain all skipped steps with details
- Include timing information in step messages
- Include provider-specific metadata in step messages

### Context Field Rules

**Minimal Level**:
- Only `tenant_id` (required by specification)
- All other fields excluded

**Detailed Level**:
- `tenant_id` (required)
- `trace_id` (if available)
- Retry/backoff fields (only when retry/backoff occurred)
- Fallback rule ID (only when fallback triggered)
- No provider metadata
- No timing information

**Verbose Level**:
- All available fields included
- Provider metadata (weight, priority, timeout)
- Timing information (decision latency, lookup latency)
- Policy metadata (cache hit, loaded at)
- All retry/backoff fields (even if not used, show as `null` or `0`)

## Contract Guarantees

**Invariants** (applied to all levels):
- `reason` field: **Always** present and valid
- `provider_id` field: **Always** present and valid
- `policy_id` field: **Always** present and valid
- `policy_version` field: **Always** present and valid
- `priority` field: **Always** present and valid (25, 50, or 100)
- `steps` field: **Always** present (may be empty array `[]`)
- `context` field: **Always** present (may be empty object `{}`)
- `context.tenant_id`: **Always** present (required by specification)

**Level-Specific Guarantees**:
- **Minimal**: `steps` array contains 1-2 items maximum
- **Detailed**: `steps` array contains all decision steps (may be 1-3 items)
- **Verbose**: `steps` array contains all decision steps with full context (may be 1-5 items)

## Usage

### Setting Detail Level

**Via Request Context**:
```erlang
Context = #{<<"detail_level">> => <<"minimal">>},
router_policy_applier:apply_policy(Request, TenantId, PolicyId, Context).
```

**Via Policy Configuration** (Future CP2 Enhancement):
```json
{
  "version": "1.0",
  "explanation": {
    "detail_level": "minimal"
  },
  "providers": [...]
}
```

**Default**: `"detailed"` if not specified

### Environment-Based Defaults

**Production**: `"minimal"` (configurable via environment variable)
**Development**: `"verbose"` (configurable via environment variable)
**CI/CD**: `"detailed"` (default)

## Performance Considerations

### Minimal Level
- **Steps**: ~50-100 bytes per explanation
- **Context**: ~20-30 bytes (tenant_id only)
- **Total**: ~70-130 bytes per decision
- **Impact**: Minimal storage and network overhead

### Detailed Level
- **Steps**: ~200-400 bytes per explanation
- **Context**: ~100-200 bytes (tenant_id + trace_id + optional fields)
- **Total**: ~300-600 bytes per decision
- **Impact**: Standard overhead, suitable for most use cases

### Verbose Level
- **Steps**: ~500-1000 bytes per explanation
- **Context**: ~300-500 bytes (all fields)
- **Total**: ~800-1500 bytes per decision
- **Impact**: Higher storage and network overhead, use for debugging only

## Migration Path

### CP1 → CP2

**CP1 (Current)**:
- Basic support for `detail_level` in `router_policy_applier.erl`
- Three levels: minimal, detailed, verbose
- Default: `"detailed"`

**CP2 (Planned)**:
- Full specification implementation
- Policy-level configuration support
- Environment-based defaults
- Performance optimizations for minimal level

### Backward Compatibility

- **Default behavior**: If `detail_level` not specified, use `"detailed"` (maintains CP1 behavior)
- **Unknown levels**: If invalid level specified, fall back to `"detailed"` and log warning
- **Contract stability**: All levels maintain the same required fields (reason, provider_id, policy_id, etc.)

## Examples by Scenario

### Scenario 1: Sticky Session Hit

**Minimal**:
```json
{
  "steps": ["1. Sticky session: found"],
  "context": {"tenant_id": "t1"}
}
```

**Detailed**:
```json
{
  "steps": [
    "1. Checked sticky session: found existing provider for key session_id = abc123",
    "2. Skipped weighted distribution (provider selected via sticky)"
  ],
  "context": {"tenant_id": "t1", "trace_id": "trace_1"}
}
```

**Verbose**:
```json
{
  "steps": [
    "1. Checked sticky session: found existing provider for key session_id = abc123 (TTL: 10m, lookup_latency: 2ms)",
    "2. Skipped weighted distribution (provider selected via sticky): 2 providers available (provider_a: 0.70, provider_b: 0.30, total: 1.00)"
  ],
  "context": {
    "tenant_id": "t1",
    "trace_id": "trace_1",
    "sticky_lookup_latency_ms": 2,
    "decision_latency_ms": 5,
    "policy_cache_hit": true
  }
}
```

### Scenario 2: Weighted Distribution

**Minimal**:
```json
{
  "steps": ["2. Weighted: 2 providers"],
  "context": {"tenant_id": "t1"}
}
```

**Detailed**:
```json
{
  "steps": [
    "1. Checked sticky session: no existing session found",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00"
  ],
  "context": {"tenant_id": "t1", "trace_id": "trace_2"}
}
```

**Verbose**:
```json
{
  "steps": [
    "1. Checked sticky session: no existing session found (sticky enabled: true, session_key: session_id)",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00 (provider_a: 0.70, provider_b: 0.30)"
  ],
  "context": {
    "tenant_id": "t1",
    "trace_id": "trace_2",
    "decision_latency_ms": 3,
    "policy_cache_hit": true,
    "provider_weight": 0.70,
    "provider_priority": 10
  }
}
```

### Scenario 3: Fallback After Retry

**Minimal**:
```json
{
  "steps": ["3. Fallback after retry"],
  "context": {"tenant_id": "t1"}
}
```

**Detailed**:
```json
{
  "steps": [
    "1. Checked sticky session: no existing session found",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00",
    "3. Applied fallback rule after 2/3 retry attempts exhausted"
  ],
  "context": {
    "tenant_id": "t1",
    "trace_id": "trace_3",
    "retry_attempts_used": 2,
    "retry_max": 3,
    "fallback_rule_id": "rule_1"
  }
}
```

**Verbose**:
```json
{
  "steps": [
    "1. Checked sticky session: no existing session found (sticky enabled: true, session_key: session_id)",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00 (provider_a: 0.70, provider_b: 0.30)",
    "3. Retry attempt 1/3 with backoff 100ms (strategy: exponential, base_ms: 100, max_ms: 1000)",
    "4. Retry attempt 2/3 with backoff 200ms (strategy: exponential, base_ms: 100, max_ms: 1000)",
    "5. Applied fallback rule after 2/3 retry attempts exhausted (rule: rule_1, condition: timeout, provider: provider_b)"
  ],
  "context": {
    "tenant_id": "t1",
    "trace_id": "trace_3",
    "retry_attempts_used": 2,
    "retry_max": 3,
    "backoff_ms": 200,
    "backoff_strategy": "exponential",
    "fallback_rule_id": "rule_1",
    "decision_latency_ms": 350,
    "policy_cache_hit": true
  }
}
```

## Future Enhancements (CP2+)

### Policy-Level Configuration

Allow setting default detail level in policy:
```json
{
  "version": "1.0",
  "explanation": {
    "detail_level": "minimal",
    "override_context": true  // Allow context to override policy default
  },
  "providers": [...]
}
```

### Per-Tenant Defaults

Configure default detail level per tenant:
- High-traffic tenants: `"minimal"`
- Standard tenants: `"detailed"`
- Debug tenants: `"verbose"`

### Dynamic Level Adjustment

Automatically adjust detail level based on:
- Traffic volume (high traffic → minimal)
- Error rate (high errors → verbose for debugging)
- Time of day (peak hours → minimal)

## References

- `docs/ROUTING_POLICY.md` - Main routing policy specification
- `apps/otp/router/src/router_policy_applier.erl` - Current implementation
- `docs/archive/dev/POLICY_DSL_GAPS_CP_CLASSIFICATION.md` - CP classification

## Change History

**v1.0 (2025-01-27)**:
- Initial draft specification
- Three levels defined: minimal, detailed, verbose
- Impact on steps and context documented
- Examples provided for all scenarios

