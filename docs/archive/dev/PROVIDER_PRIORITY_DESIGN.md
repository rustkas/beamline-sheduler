# Provider Priority Design Analysis

## Purpose

This document analyzes whether Provider Priority (separate from weights) is needed in Router Policy DSL, evaluates use cases, and provides a decision with justification.

## Status

📅 **CP2 Enhancement** (Design Analysis)

**Decision**: **DEFERRED to CP2+** (not implemented in CP2-LC)

**Rationale**: Current mechanisms (weights, fallbacks, sticky) are sufficient for CP2. Priority adds complexity without clear benefit over existing patterns.

## Overview

### Question

**Is Provider Priority (separate from weights) needed?**

**Current Mechanisms**:
- **Weights**: Probabilistic distribution (e.g., 70% provider_a, 30% provider_b)
- **Fallbacks**: Conditional routing based on error conditions
- **Sticky**: Session-based provider binding
- **Circuit Breaker**: Fail-fast for unhealthy providers

**Proposed Addition**:
- **Priority**: Strict ordering (e.g., always use primary, only if unavailable use backup)

## Use Case Analysis

### Use Case 1: Strict Primary/Backup

**Scenario**: Always use primary provider, only use backup if primary is unavailable

**Current Solution with Weights**:
```json
{
  "providers": [
    { "name": "primary", "weight": 100 },
    { "name": "backup", "weight": 0 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx", "unhealthy"]},
      "to": "backup"
    }
  ]
}
```

**Problem**: Weights don't express "strict primary" intent clearly. Weight 100/0 works but is not intuitive.

**Proposed Solution with Priority**:
```json
{
  "providers": [
    { "name": "primary", "priority": 10, "weight": 100 },
    { "name": "backup", "priority": 5, "weight": 100 }
  ]
}
```

**Behavior**: Select highest priority provider first, use weights only within same priority tier.

**Analysis**:
- ✅ More intuitive for primary/backup scenarios
- ❌ Adds complexity (priority tiers, priority + weights interaction)
- ❌ Can be achieved with weights + fallbacks (current solution)

**Verdict**: **Not essential** - current solution (weights + fallbacks) works, priority adds complexity

### Use Case 2: Priority Tiers with Weighted Distribution

**Scenario**: Multiple priority tiers, weighted distribution within each tier

**Example**:
- **Tier 1 (High Priority)**: provider_a (70%), provider_b (30%)
- **Tier 2 (Medium Priority)**: provider_c (50%), provider_d (50%)
- **Tier 3 (Low Priority)**: provider_e (100%)

**Behavior**: 
1. Select from Tier 1 (weighted: 70% a, 30% b)
2. If Tier 1 unavailable → select from Tier 2 (weighted: 50% c, 50% d)
3. If Tier 2 unavailable → select from Tier 3 (provider_e)

**Current Solution with Weights + Fallbacks**:
```json
{
  "providers": [
    { "name": "provider_a", "weight": 70 },
    { "name": "provider_b", "weight": 30 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx", "unhealthy"]},
      "to": "provider_c"  // Tier 2
    },
    {
      "when": {"status": ["timeout", "5xx", "unhealthy"]},
      "to": "provider_e"  // Tier 3
    }
  ]
}
```

**Problem**: Fallbacks are sequential, not tiered. Can't express "select from Tier 2 with weights" easily.

**Proposed Solution with Priority**:
```json
{
  "providers": [
    { "name": "provider_a", "priority": 10, "weight": 70 },
    { "name": "provider_b", "priority": 10, "weight": 30 },
    { "name": "provider_c", "priority": 5, "weight": 50 },
    { "name": "provider_d", "priority": 5, "weight": 50 },
    { "name": "provider_e", "priority": 1, "weight": 100 }
  ]
}
```

**Behavior**: 
1. Filter providers by highest priority (10)
2. Apply weighted distribution within priority 10 (70% a, 30% b)
3. If no providers available in priority 10 → filter by priority 5
4. Apply weighted distribution within priority 5 (50% c, 50% d)
5. Continue until provider selected

**Analysis**:
- ✅ Expresses tiered priority clearly
- ✅ Supports weighted distribution within tiers
- ❌ Complex implementation (priority filtering + weighted selection)
- ❌ Can be achieved with multiple policies or complex fallback chains

**Verdict**: **Nice to have** - useful for complex scenarios, but not essential for CP2

### Use Case 3: Priority Override for Emergency

**Scenario**: Temporarily prioritize backup provider during primary maintenance

**Current Solution**: Update policy (change weights or fallback order)

**Proposed Solution with Priority**: Update priority values (faster, no weight recalculation)

**Analysis**:
- ✅ Faster policy updates (change priority, not weights)
- ❌ Adds another dimension to manage
- ❌ Can be achieved with policy updates (current solution)

**Verdict**: **Not essential** - policy updates work fine

## Comparison: Priority vs Current Mechanisms

### Priority vs Weights

| Aspect | Weights | Priority |
|--------|---------|----------|
| **Purpose** | Probabilistic distribution | Strict ordering |
| **Use Case** | Load balancing, A/B testing | Primary/backup, tiered selection |
| **Complexity** | Low (single dimension) | Medium (priority + weights interaction) |
| **Flexibility** | High (any distribution) | Medium (tiered selection) |

**Conclusion**: Weights are sufficient for probabilistic distribution. Priority adds strict ordering but increases complexity.

### Priority vs Fallbacks

| Aspect | Fallbacks | Priority |
|--------|-----------|----------|
| **Purpose** | Conditional routing on errors | Pre-selection ordering |
| **Trigger** | After provider call fails | Before provider selection |
| **Use Case** | Error handling, retry logic | Tiered provider selection |
| **Complexity** | Low (conditional rules) | Medium (priority tiers) |

**Conclusion**: Fallbacks handle error-based routing well. Priority handles pre-selection ordering, but can be achieved with fallbacks.

### Priority vs Sticky

| Aspect | Sticky | Priority |
|--------|--------|----------|
| **Purpose** | Session-based binding | Provider ordering |
| **Use Case** | User session consistency | Provider tiering |
| **Trigger** | Session key match | Always (pre-selection) |
| **Complexity** | Low (session lookup) | Medium (priority filtering) |

**Conclusion**: Sticky and priority serve different purposes. No conflict, but priority not essential.

## Decision Matrix

### Criteria

1. **Expressiveness**: Can priority express scenarios that weights/fallbacks cannot?
2. **Complexity**: Does priority add significant implementation complexity?
3. **Use Case Frequency**: How often is priority needed in real scenarios?
4. **Alternative Solutions**: Can current mechanisms achieve the same result?

### Evaluation

| Criterion | Score | Notes |
|-----------|-------|-------|
| **Expressiveness** | ⚠️ Medium | Priority expresses tiered selection better, but fallbacks can achieve similar results |
| **Complexity** | ❌ High | Priority + weights interaction, priority filtering, tier management |
| **Use Case Frequency** | ⚠️ Low | Primary/backup is common, but weights + fallbacks work |
| **Alternative Solutions** | ✅ Yes | Weights + fallbacks can achieve primary/backup and tiered selection |

### Decision

**Recommendation**: **DEFER to CP2+** (not implement in CP2-LC)

**Rationale**:
1. **Current mechanisms are sufficient**: Weights + fallbacks can achieve primary/backup and tiered selection
2. **Complexity vs benefit**: Priority adds significant complexity (priority filtering, priority + weights interaction) without clear benefit
3. **Use case frequency**: Primary/backup is common, but weights + fallbacks work well
4. **CP2 scope**: CP2 focuses on core reliability (circuit breaker, health checks, timeouts). Priority is a "nice to have" enhancement

**Alternative for CP2**:
- Use **weights + fallbacks** for primary/backup scenarios
- Use **multiple policies** for complex tiered selection
- Document patterns in `ROUTING_POLICY.md` for common scenarios

## Proposed Specification (If Implemented in CP2+)

### JSON-DSL Structure

**Location**: `providers[].priority` (optional field)

**Format**:
```json
{
  "providers": [
    {
      "name": "provider_a",
      "weight": 70,
      "priority": 10  // Optional: higher = higher priority
    },
    {
      "name": "provider_b",
      "weight": 30,
      "priority": 10  // Same priority tier
    },
    {
      "name": "provider_c",
      "weight": 100,
      "priority": 5   // Lower priority tier
    }
  ]
}
```

### Behavior

**Selection Algorithm**:
1. **Filter by Priority**: Select providers with highest priority
2. **Apply Weights**: Within same priority tier, apply weighted distribution
3. **Fallback to Lower Priority**: If no providers available in current priority tier, try next tier
4. **Continue**: Until provider selected or all tiers exhausted

**Priority Values**:
- **Higher number = Higher priority** (e.g., 10 > 5 > 1)
- **Default**: If priority not specified, assume priority `0` (lowest)
- **Range**: `1` to `100` (configurable)

### Integration with Existing Mechanisms

**Priority + Sticky**:
- Sticky takes precedence (if sticky session found, use that provider regardless of priority)
- If sticky not found → apply priority + weights

**Priority + Fallbacks**:
- Priority affects **initial selection** (which provider to try first)
- Fallbacks affect **error handling** (which provider to use if first fails)
- Both can work together: Priority selects initial provider, fallbacks handle errors

**Priority + Circuit Breaker**:
- Circuit breaker affects **availability** (skip providers with open circuit)
- Priority affects **selection order** (which available provider to choose)
- Filter by circuit breaker first, then apply priority + weights

**Priority + Health Check**:
- Health check affects **availability** (skip unhealthy providers)
- Priority affects **selection order** (which healthy provider to choose)
- Filter by health check first, then apply priority + weights

### Execution Order

```
1. Health Check (if enabled)
   └─ Filter out unhealthy providers

2. Circuit Breaker Check
   └─ Filter out providers with open circuit

3. Sticky Check (if enabled)
   └─ If sticky session found → use that provider (skip priority)

4. Priority + Weights (if sticky not found)
   ├─ Filter by highest priority
   ├─ Apply weighted distribution within priority tier
   └─ If no providers in tier → try next priority tier

5. Fallback (if no providers available or provider fails)
   └─ Use fallback provider
```

## Examples (If Implemented)

### Example 1: Strict Primary/Backup

```json
{
  "providers": [
    { "name": "primary", "priority": 10, "weight": 100 },
    { "name": "backup", "priority": 5, "weight": 100 }
  ]
}
```

**Behavior**:
- Always try `primary` first (priority 10)
- If `primary` unavailable → use `backup` (priority 5)

### Example 2: Priority Tiers with Weights

```json
{
  "providers": [
    { "name": "provider_a", "priority": 10, "weight": 70 },
    { "name": "provider_b", "priority": 10, "weight": 30 },
    { "name": "provider_c", "priority": 5, "weight": 50 },
    { "name": "provider_d", "priority": 5, "weight": 50 }
  ]
}
```

**Behavior**:
- Tier 1 (priority 10): 70% provider_a, 30% provider_b
- If Tier 1 unavailable → Tier 2 (priority 5): 50% provider_c, 50% provider_d

### Example 3: Priority + Fallbacks

```json
{
  "providers": [
    { "name": "primary", "priority": 10, "weight": 100 },
    { "name": "backup", "priority": 5, "weight": 100 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout"]},
      "to": "backup"
    }
  ]
}
```

**Behavior**:
- Priority selects `primary` first
- If `primary` times out → fallback to `backup` (regardless of priority)

## Alternative Solutions for CP2

### Solution 1: Weights + Fallbacks (Recommended)

**For Primary/Backup**:
```json
{
  "providers": [
    { "name": "primary", "weight": 100 },
    { "name": "backup", "weight": 0 }
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx", "unhealthy", "circuit_breaker_open"]},
      "to": "backup"
    }
  ]
}
```

**Pros**:
- ✅ Simple and intuitive
- ✅ Already implemented
- ✅ Works for most scenarios

**Cons**:
- ⚠️ Weight 0 is not ideal (but works)

### Solution 2: Multiple Policies

**For Tiered Selection**:
- **Policy 1**: Tier 1 providers (weights: 70% a, 30% b)
- **Policy 2**: Tier 2 providers (weights: 50% c, 50% d)
- **Policy 3**: Tier 3 providers (provider_e)

**Router Logic**: Try policies in order, fallback to next policy if no providers available

**Pros**:
- ✅ Clear separation of tiers
- ✅ Reusable policies

**Cons**:
- ⚠️ Requires multiple policies (more management)

## Recommendations

### For CP2-LC

**Decision**: **DO NOT implement Priority in CP2-LC**

**Actions**:
1. ✅ Document primary/backup pattern using weights + fallbacks
2. ✅ Document tiered selection pattern using multiple policies
3. ✅ Mark priority as "CP2+ Enhancement" in `ROUTING_POLICY.md`
4. ✅ Add examples in `ROUTING_POLICY.md` for common scenarios

### For CP2+ (Future)

**Decision**: **EVALUATE based on user feedback**

**Criteria for Implementation**:
- User requests for priority-based selection
- Evidence that weights + fallbacks are insufficient
- Clear use cases that cannot be achieved with current mechanisms

**If Implemented**:
- Follow specification in this document
- Ensure backward compatibility (priority optional, default behavior unchanged)
- Add comprehensive tests for priority + weights interaction

## References

- `docs/ROUTING_POLICY.md` - Main routing policy specification
- `apps/otp/router/src/router_decider.erl` - Current provider selection logic
- `docs/archive/dev/POLICY_DSL_GAPS_CP_CLASSIFICATION.md` - CP classification

## Change History

**v1.0 (2025-01-27)**:
- Initial design analysis
- Use case evaluation
- Decision: DEFER to CP2+
- Alternative solutions documented

