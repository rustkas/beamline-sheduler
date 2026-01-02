# Routing Policy (JSON-DSL)

## Scope

This document defines routing policy JSON-DSL for Router. Policies control provider selection via weights, fallbacks, and sticky sessions.

**Implementation**: `apps/otp/router/src/router_policy_store.erl`, `apps/otp/router/src/router_decider.erl`  
**Tests**: `apps/otp/router/test/router_policy_store_SUITE.erl`, `apps/otp/router/test/router_decider_SUITE.erl`  
**Schema**: `apps/otp/router/docs/schemas/policy.schema.json`

## Structure
```json
{
  "version": "1.0",
  "providers": [
    { "name": "provider_a", "weight": 70 },
    { "name": "provider_b", "weight": 30 }
  ],
  "fallbacks": [
    {
      "when": { "status": ["timeout", "5xx"] },
      "retry": 2,
      "to": "provider_b"
    }
  ],
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  },
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "config": {
        "lowercase": true,
        "trim_whitespace": true
      }
    }
  ],
  "validators": [
    {
      "id": "pii_guard",
      "on_fail": "block"
    }
  ],
  "post": [
    {
      "id": "mask_pii",
      "mode": "required",
      "config": {
        "mask_email": true
      }
    }
  ]
}
```

## Weights

### Invariants

Router **guarantees**:
- Provider selection **always** uses weighted round-robin algorithm
- Weights **always** used proportionally (no normalization to 100/1.0)
- If sum of weights = 0 → `{error, no_providers}`
- If sum of weights > 0 → weights used as-is (proportional distribution)
- Policy **always** cached in `policy_cache` ETS table for fast decisions

### Inconsistent Weights Behavior

**When sum ≠ 100/1.0**:

Router **does NOT normalize** weights to 100/1.0. Instead, weights are used **proportionally** based on their actual sum.

**Examples**:

1. **Sum < 100** (e.g., 70):
   ```json
   {
     "providers": [
       {"name": "provider_a", "weight": 30},  // 30/70 = 42.9%
       {"name": "provider_b", "weight": 40}   // 40/70 = 57.1%
     ]
   }
   ```
   - `provider_a`: 30/70 = 42.9% of traffic
   - `provider_b`: 40/70 = 57.1% of traffic
   - **Behavior**: Works correctly, proportional distribution

2. **Sum > 100** (e.g., 120):
   ```json
   {
     "providers": [
       {"name": "provider_a", "weight": 70},  // 70/120 = 58.3%
       {"name": "provider_b", "weight": 50}   // 50/120 = 41.7%
     ]
   }
   ```
   - `provider_a`: 70/120 = 58.3% of traffic
   - `provider_b`: 50/120 = 41.7% of traffic
   - **Behavior**: Works correctly, proportional distribution

3. **Sum = 0**:
   ```json
   {
     "providers": [
       {"name": "provider_a", "weight": 0},
       {"name": "provider_b", "weight": 0}
     ]
   }
   ```
   - **Behavior**: `{error, no_providers}` (no provider can be selected)

**Implementation Details**:
- `TotalWeight = sum(weights)` (actual sum, not normalized)
- `Random = rand:uniform() * TotalWeight` (random value in [0, TotalWeight])
- Provider selected when `Random <= cumulative_weight`

**Logging**:
- Router logs warning if sum ≠ 100 (for visibility, but does not fail)
- Explanation includes actual weights used (not normalized)

**Recommendation**:
- For clarity, use weights that sum to 100 (or 1.0 for normalized format)
- Router accepts any positive sum and uses proportional distribution

### Implementation

**Module**: `router_decider.erl`  
**Storage**: `policy_cache` ETS table (cached from PostgreSQL)  
**Tests**: `router_decider_SUITE.erl`, `router_policy_SUITE.erl`

## Fallbacks

### Invariants

Router **guarantees**:
- Fallback **only** triggered when condition matches (status/error type)
- Fallback provider **always** selected after retry count exhausted
- Multiple fallback rules **always** evaluated in order (first match wins)
- Conflicting/overlapping fallback rules **always** resolved by order (first matching rule wins)
- Retry attempts **always** tracked per fallback rule
- Backoff delay **always** applied between retry attempts (if configured)

### Conflicting and Overlapping Fallback Rules

**When multiple fallback rules have identical or overlapping conditions**:

Router uses **first match wins** strategy: the first fallback rule in the array that matches the condition is used, and subsequent matching rules are ignored.

**Examples**:

1. **Identical Conditions** (conflicting):
   ```json
   {
     "fallbacks": [
       {
         "when": {"status": ["timeout"]},
         "to": "provider_a"  // ← This one wins
       },
       {
         "when": {"status": ["timeout"]},
         "to": "provider_b"  // ← Ignored (same condition, but later in array)
       }
     ]
   }
   ```
   - **Behavior**: If `status = "timeout"`, Router uses `provider_a` (first matching rule)
   - **Rationale**: Order matters - first matching rule takes precedence

2. **Overlapping Conditions** (subset/superset):
   ```json
   {
     "fallbacks": [
       {
         "when": {"status": ["timeout", "5xx"]},
         "to": "provider_a"  // ← This one wins for "timeout"
       },
       {
         "when": {"status": ["timeout"]},
         "to": "provider_b"  // ← Ignored (overlapping condition, but later)
       }
     ]
   }
   ```
   - **Behavior**: If `status = "timeout"`, Router uses `provider_a` (first matching rule)
   - **Rationale**: First match wins, even if later rules have more specific conditions

3. **Non-Overlapping Conditions** (no conflict):
   ```json
   {
     "fallbacks": [
       {
         "when": {"status": ["timeout"]},
         "to": "provider_a"
       },
       {
         "when": {"status": ["5xx"]},
         "to": "provider_b"
       }
     ]
   }
   ```
   - **Behavior**: Each condition matches independently
   - `status = "timeout"` → `provider_a`
   - `status = "5xx"` → `provider_b`

**Implementation Details**:
- Fallback rules evaluated **sequentially** in array order
- First rule with matching `when` condition is used
- Subsequent matching rules are **not evaluated** (short-circuit)
- Retry count and backoff are tracked per fallback rule (by rule ID)

**Logging**:
- Router logs which fallback rule was used (by position/index)
- Explanation includes fallback rule identifier

**Recommendation**:
- Order fallback rules from most specific to least specific
- Avoid duplicate conditions (use single rule with multiple values: `{"status": ["timeout", "5xx"]}`)
- If duplicate conditions are needed, ensure order reflects priority

### Retry Semantics

**Retry Count** (`fallbacks[].retry`):
- **Type**: `integer` (0-10, default: 1)
- **Semantics**: Number of retry attempts before applying fallback
- **Behavior**:
  - When condition matches, Router retries the current provider `retry` times
  - After `retry` attempts exhausted, fallback provider is selected
  - Retry count is tracked per fallback rule in request context
  - Each retry attempt increments the attempt counter

**Example**:
```json
{
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx"]},
      "retry": 2,
      "to": "provider_b"
    }
  ]
}
```

**Behavior**:
1. Primary provider fails with `timeout` or `5xx`
2. Router retries primary provider **2 times** (retry count = 2)
3. If all retries fail, fallback to `provider_b`

### Backoff Semantics

**Backoff Strategy** (`fallbacks[].backoff`):
- **Type**: `object` (optional)
- **Default**: Exponential backoff with jitter (if not specified)
- **Format**:
```json
{
  "strategy": "exponential" | "linear" | "fixed",
  "base_ms": 100,
  "max_ms": 5000,
  "jitter": true
}
```

**Strategies**:
- **exponential** (default): `delay = base_ms * 2^(attempt - 1) + jitter`
- **linear**: `delay = base_ms * attempt + jitter`
- **fixed**: `delay = base_ms + jitter`

**Jitter**:
- If `jitter: true`: adds random 0-10% of delay to prevent thundering herd
- If `jitter: false`: no jitter, deterministic delay

**Example**:
```json
{
  "fallbacks": [
    {
      "when": {"status": ["timeout"]},
      "retry": 3,
      "backoff": {
        "strategy": "exponential",
        "base_ms": 100,
        "max_ms": 2000,
        "jitter": true
      },
      "to": "provider_b"
    }
  ]
}
```

**Behavior**:
1. Primary provider fails with `timeout`
2. Retry attempt 1: wait ~100ms (base_ms * 2^0 + jitter)
3. Retry attempt 2: wait ~200ms (base_ms * 2^1 + jitter)
4. Retry attempt 3: wait ~400ms (base_ms * 2^2 + jitter)
5. If all retries fail, fallback to `provider_b`

### Implementation

**Module**: `router_decider.erl`  
**Retry Tracking**: Request context (`retry_attempts` map)  
**Backoff Calculation**: `calculate_backoff/3` function  
**Tests**: `router_decider_SUITE.erl`, `router_policy_SUITE.erl`

**Circuit Breaker Integration** (CP2):
- Circuit breaker check happens **before** provider call
- If circuit is **open** → fail fast, skip retry, immediate fallback
- If circuit is **half-open** → allow request (rate limited), retry applies if fails
- If circuit is **closed** → normal operation, retry/backoff applies as configured
- See `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` for detailed integration rules

## Sticky Sessions

### Invariants

Router **guarantees**:
- Sticky routing **only** active if `sticky.enabled: true`
- Sticky session **always** stored in `sticky_sessions` ETS table
- Sticky session **always** expires after TTL (automatic cleanup)

### Implementation

**Module**: `router_sticky_store.erl`  
**Storage**: `sticky_sessions` ETS table  
**Tests**: `router_sticky_store_SUITE.erl`

## Extensions

### Overview

Extensions are separate NATS services that integrate into the routing pipeline without changing core Router code. They are configured in the routing policy and executed in a fixed order: `pre[] → validators[] → routing → provider → post[]`.

**Reference**: See `docs/EXTENSIONS_API.md` for complete extension API documentation.

### Extension Types

1. **Pre-processor** (`pre[]`) — modifies/enriches incoming message *before* routing
2. **Validator** (`validators[]`) — decides whether processing can continue (accept/reject)
3. **Post-processor** (`post[]`) — modifies provider response *after* routing

### Invariants

Router **guarantees**:
- Extensions **always** executed in order: pre → validators → routing → post
- Extension configuration **always** loaded from Extension Registry by `id`
- Extension failures **always** handled according to `mode`/`on_fail` settings
- Extensions **never** modify core Router code (hot-reload via Registry)

### Pre-processor Extensions (`pre[]`)

**Structure**:
```json
{
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required" | "optional",
      "config": {
        "lowercase": true,
        "trim_whitespace": true
      }
    }
  ]
}
```

**Fields**:
- `id` (required, string) — Extension identifier (logical ID from Extension Registry)
- `mode` (optional, string) — Extension mode: `"required"` (fail if extension fails) or `"optional"` (continue if extension fails), default: `"optional"`
- `config` (optional, object) — Per-policy extension configuration (extension-specific)

**Behavior**:
- Pre-processors execute **before** provider selection
- If `mode: "required"` and extension fails → routing fails
- If `mode: "optional"` and extension fails → routing continues with original message
- Modified message from pre-processor is used for subsequent steps

### Validator Extensions (`validators[]`)

**Structure**:
```json
{
  "validators": [
    {
      "id": "pii_guard",
      "on_fail": "block" | "warn" | "ignore"
    }
  ]
}
```

**Fields**:
- `id` (required, string) — Extension identifier (logical ID from Extension Registry)
- `on_fail` (optional, string) — Behavior on validation failure:
  - `"block"` — reject request, return error (default)
  - `"warn"` — log warning, continue processing
  - `"ignore"` — ignore failure, continue processing

**Behavior**:
- Validators execute **after** pre-processors, **before** provider selection
- If `on_fail: "block"` and validation fails → routing fails
- If `on_fail: "warn"` and validation fails → warning logged, routing continues
- If `on_fail: "ignore"` and validation fails → routing continues silently

### Post-processor Extensions (`post[]`)

**Structure**:
```json
{
  "post": [
    {
      "id": "mask_pii",
      "mode": "required" | "optional",
      "config": {
        "mask_email": true,
        "mask_phone": true
      }
    }
  ]
}
```

**Fields**:
- `id` (required, string) — Extension identifier (logical ID from Extension Registry)
- `mode` (optional, string) — Extension mode: `"required"` (fail if extension fails) or `"optional"` (return original response if extension fails), default: `"optional"`
- `config` (optional, object) — Per-policy extension configuration (extension-specific)

**Behavior**:
- Post-processors execute **after** provider response
- If `mode: "required"` and extension fails → return error
- If `mode: "optional"` and extension fails → return original provider response
- Modified response from post-processor is returned to client

### Extension Registry

Extensions must be registered in **Extension Registry** before use in policies:

- **Registry Storage**: PostgreSQL (persistent) + Mnesia/ETS (cache)
- **Registry Fields**: `id`, `type` (pre/validator/post), `subject` (NATS subject), `timeout_ms`, `retry`, `enabled`
- **Lookup**: Router looks up extension by `id` from Registry to get NATS subject and timeout

**Reference**: See `docs/EXTENSIONS_API.md` for Extension Registry details.

### Implementation

**Module**: `router_extension_invoker.erl`, `router_decider.erl`  
**Registry**: `router_extension_registry_db.erl`  
**Pipeline**: `router_decider:decide/3` executes extensions in order  
**Tests**: `router_decider_SUITE.erl`, `router_extension_invoker_SUITE.erl`

## Decision Explanation Format

### Purpose

Every routing decision produces an **explanation** object that describes how the decision was made. This explanation is used for:
- **Audit trail**: Logging routing decisions for compliance and debugging
- **Debugging**: Understanding why a specific provider was selected
- **Analytics**: Analyzing routing patterns and decision distribution

### Detail Levels

**Detail Level** controls the verbosity of explanation steps and context:

- **`"minimal"`**: Production logging with minimal data footprint
  - Steps: 1-2 essential steps only (e.g., `"1. Sticky session: found"`, `"2. Weighted: 2 providers"`)
  - Context: Only `tenant_id` (required field)
  - Use case: High-traffic production, cost-sensitive log storage

- **`"detailed"`** (default): Standard audit trail with key information
  - Steps: All decision steps with key information (e.g., `"1. Checked sticky session: found existing provider for key session_id = abc123"`)
  - Context: `tenant_id` + `trace_id` + retry/backoff fields (when applicable)
  - Use case: Standard production audit, compliance requirements

- **`"verbose"`**: Maximum detail for debugging and deep audit
  - Steps: All decision steps with full context (e.g., `"1. Checked sticky session: found existing provider for key session_id = abc123 (TTL: 10m, lookup_latency: 2ms)"`)
  - Context: All available fields (timing, provider metadata, policy metadata)
  - Use case: Development, deep troubleshooting, comprehensive audit

**Usage**: Set `detail_level` in request context:
```erlang
Context = #{<<"detail_level">> => <<"minimal">>},
router_policy_applier:apply_policy(Request, TenantId, PolicyId, Context).
```

**Default**: `"detailed"` if not specified

**CP2 Enhancement**: Full specification and policy-level configuration planned for CP2. See `docs/archive/dev/EXPLANATION_LEVELS_SPEC.md` for comprehensive draft specification.

### JSON Structure

**Required Fields**:
```json
{
  "reason": "sticky" | "weighted" | "fallback" | "retry",
  "provider_id": "string",
  "policy_id": "string",
  "policy_version": "string",
  "priority": 25 | 50 | 100,
  "steps": ["string"],
  "context": {
    "tenant_id": "string",
    "trace_id": "string (optional)",
    ...
  }
}
```

### Field Specifications

#### `reason` (required, string)

**Type**: `binary()` in Erlang, `string` in JSON

**Possible Values**:
- `"sticky"` - Provider selected via sticky session
- `"weighted"` - Provider selected via weighted distribution
- `"fallback"` - Provider selected via fallback rule (after retry exhausted)
- `"retry"` - Retry attempt (retry count not exhausted yet)

**Invariants**:
- **Always** present in explanation
- **Always** one of the four values above
- **Never** empty or undefined

#### `provider_id` (required, string)

**Type**: `binary()` in Erlang, `string` in JSON

**Description**: Selected provider identifier

**Invariants**:
- **Always** present in explanation
- **Always** matches a provider from policy
- **Never** empty or undefined

#### `policy_id` (required, string)

**Type**: `binary()` in Erlang, `string` in JSON

**Description**: Policy identifier used for decision

**Invariants**:
- **Always** present in explanation
- **Always** matches a policy in store
- Default: `"default"` if not specified

#### `policy_version` (required, string)

**Type**: `binary()` in Erlang, `string` in JSON

**Description**: Policy version (e.g., `"1.0"`)

**Format**: `MAJOR.MINOR` (e.g., `"1.0"`, `"2.1"`)

**Invariants**:
- **Always** present in explanation
- **Always** matches policy version format
- Default: `"1.0"` if not specified

#### `priority` (required, integer)

**Type**: `integer()` in Erlang, `integer` in JSON

**Description**: Decision priority (higher = more important)

**Possible Values**:
- `100` - Sticky session (highest priority)
- `50` - Weighted distribution or retry (medium priority)
- `25` - Fallback (lowest priority)

**Invariants**:
- **Always** present in explanation
- **Always** one of: 25, 50, 100
- Default: `50` if not specified

#### `steps` (required, array of strings)

**Type**: `[binary()]` in Erlang, `string[]` in JSON

**Description**: Step-by-step explanation of decision process

**Format**: Array of human-readable strings describing decision steps in chronological order

**Example**:
```json
[
  "1. Checked sticky session: no existing session found",
  "2. Applied weighted distribution: 2 providers, total weight: 1.00",
  "3. Applied fallback rule after 2/3 retry attempts exhausted"
]
```

**Step Patterns**:
- **Sticky**: `"1. Checked sticky session: found existing provider for key <key> = <value>"` or `"1. Checked sticky session: no existing session found"`
- **Weighted**: `"2. Applied weighted distribution: <count> providers, total weight: <weight>"` or `"2. Skipped weighted distribution (provider selected via <reason>)"`
- **Fallback**: `"3. Applied fallback rule: <count> fallback rules evaluated"` or `"3. Applied fallback rule after <used>/<max> retry attempts exhausted"`
- **Retry**: `"3. Retry attempt <attempt>/<max> with backoff <ms>ms"` or `"3. Retry attempt (retry count not exhausted)"`

**Detail Level Impact**:
- **`"minimal"`**: Only essential steps (e.g., `"1. Sticky session: found"`, `"2. Weighted: 2 providers"`, `"3. Fallback applied"`)
- **`"detailed"`** (default): Standard steps with key information (e.g., `"1. Checked sticky session: found existing provider for key session_id = abc123"`)
- **`"verbose"`**: Maximum detail including all context (e.g., TTL, individual provider weights, backoff strategy, fallback rule ID)

**Invariants**:
- **Always** present in explanation (may be empty array `[]`)
- **Always** array of strings
- **Always** in chronological order (step 1, step 2, step 3, ...)
- **Never** contains null or undefined values
- **Detail level** controlled by `detail_level` in request context (default: `"detailed"`)

#### `context` (required, object)

**Type**: `map()` in Erlang, `object` in JSON

**Description**: Additional context for decision (tenant_id, trace_id, retry info, etc.)

**Required Fields**:
- `tenant_id` (string) - Tenant identifier

**Optional Fields**:
- `trace_id` (string) - Trace identifier for distributed tracing
- `retry_attempt` (integer) - Current retry attempt (if retry)
- `retry_max` (integer) - Maximum retry count (if retry)
- `retry_attempts_used` (integer) - Retry attempts used (if fallback after retry)
- `backoff_ms` (integer) - Backoff delay in milliseconds (if retry)
- `fallback_rule_id` (string) - Fallback rule identifier (if fallback)
- Other context fields as needed

**Invariants**:
- **Always** present in explanation (may be empty object `{}`)
- **Always** object/map
- **Always** contains `tenant_id` (may be `"unknown"` if not available)
- **Never** contains sensitive data (PII filtered before logging)

### Examples

#### Sticky Session Decision

```json
{
  "reason": "sticky",
  "provider_id": "openai",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 100,
  "steps": [
    "1. Checked sticky session: found existing provider for key session_id = abc123"
  ],
  "context": {
    "tenant_id": "tenant_1",
    "trace_id": "trace_abc123"
  }
}
```

#### Weighted Distribution Decision

```json
{
  "reason": "weighted",
  "provider_id": "anthropic",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 50,
  "steps": [
    "1. Checked sticky session: no existing session found",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00"
  ],
  "context": {
    "tenant_id": "tenant_1",
    "trace_id": "trace_abc123"
  }
}
```

#### Fallback After Retry Decision

```json
{
  "reason": "fallback",
  "provider_id": "cohere",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 25,
  "steps": [
    "1. Checked sticky session: no existing session found",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00",
    "3. Applied fallback rule after 2/3 retry attempts exhausted"
  ],
  "context": {
    "tenant_id": "tenant_1",
    "trace_id": "trace_abc123",
    "retry_attempts_used": 2,
    "retry_max": 3,
    "fallback_rule_id": "abc123def456",
    "fallback_applied": true
  }
}
```

#### Retry Attempt Decision

```json
{
  "reason": "retry",
  "provider_id": "openai",
  "policy_id": "default",
  "policy_version": "1.0",
  "priority": 50,
  "steps": [
    "1. Checked sticky session: no existing session found",
    "2. Applied weighted distribution: 2 providers, total weight: 1.00",
    "3. Retry attempt 1/3 with backoff 100ms"
  ],
  "context": {
    "tenant_id": "tenant_1",
    "trace_id": "trace_abc123",
    "retry_attempt": 1,
    "retry_max": 3,
    "backoff_ms": 100,
    "backoff_applied": true,
    "fallback_rule_id": "abc123def456"
  }
}
```

### Audit Trail Integration

**Logging**: Explanation is logged via `router_audit:log_decision/1` in structured JSON format

**Format**: Strict JSON, PII filtered, all required fields present

**Storage**: Audit entries stored in ETS table `audit_logs` with retention policy

**Invariants**:
- **Always** logged for every routing decision
- **Always** in structured JSON format
- **Always** PII filtered before logging
- **Always** contains all required fields

### Implementation

**Module**: `router_policy_applier.erl`  
**Function**: `build_explanation/3`  
**Logging**: `router_audit:log_decision/1`  
**Tests**: `router_policy_applier_dsl_SUITE.erl`, `router_policy_integration_SUITE.erl`

## Policy Enforcement

### Invariants

Router **guarantees**:
- Policy access **always** validated against tenant RBAC
- Policy JSON **always** validated against schema before storage
- Policy changes **always** logged with actor/timestamp (audit trail)
- Policy version **always** tracked (rollback supported)
- Routing decisions **always** produce explanation for audit trail

### Implementation

**Module**: `router_policy_store.erl`  
**Storage**: PostgreSQL (persistent) + Mnesia/ETS (cache)  
**Tests**: `router_policy_store_SUITE.erl`, `router_rbac_SUITE.erl`

## Examples

### Heavy weight on `provider_a`:
```json
{ 
  "providers": [ 
    {"name": "provider_a", "weight": 90}, 
    {"name": "provider_b", "weight": 10} 
  ] 
}
```

### Sticky by `message_id` for 5 minutes:
```json
{ 
  "sticky": { 
    "enabled": true, 
    "session_key": "message_id", 
    "ttl": "5m" 
  } 
}
```

### Complex fallback chain:
```json
{
  "fallbacks": [
    {
      "when": { "status": ["timeout", "5xx"] },
      "retry": 2,
      "to": "provider_b"
    },
    {
      "when": { "status": ["rate_limited"] },
      "retry": 1,
      "to": "provider_c"
    }
  ]
}
```

### Complete policy with extensions:
```json
{
  "version": "1.0",
  "providers": [
    { "name": "provider_a", "weight": 70 },
    { "name": "provider_b", "weight": 30 }
  ],
  "fallbacks": [
    {
      "when": { "status": ["timeout", "5xx"] },
      "retry": 2,
      "to": "provider_b"
    }
  ],
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  },
  "pre": [
    {
      "id": "normalize_text",
      "mode": "required",
      "config": {
        "lowercase": true,
        "trim_whitespace": true
      }
    },
    {
      "id": "add_context",
      "mode": "optional",
      "config": {
        "include_timestamp": true
      }
    }
  ],
  "validators": [
    {
      "id": "pii_guard",
      "on_fail": "block"
    },
    {
      "id": "content_filter",
      "on_fail": "warn"
    }
  ],
  "post": [
    {
      "id": "mask_pii",
      "mode": "required",
      "config": {
        "mask_email": true,
        "mask_phone": true
      }
    }
  ]
}
```

## Deprecated Fields

### Overview

The following fields are **deprecated** and **not part of the public routing DSL**:
- `metadata` - Additional policy metadata (not used in routing logic)
- `defaults` - Default values (not used in routing logic)
- `escalate_on` - Escalation conditions (not used in routing logic)

### Status

**Deprecated**: These fields are parsed and stored, but **do not affect routing decisions**.

**Rationale**:
- These fields are not used in `router_decider.erl` or `router_policy_applier.erl`
- They do not influence provider selection, fallbacks, or sticky sessions
- They are reserved for future use or internal storage only

### Behavior

**Current Implementation**:
- Fields are parsed from JSON and stored in `#policy{}` record
- Fields are **ignored** during routing decisions
- Fields may be used for internal storage or future features (CP2)

**Schema**:
- Fields are marked as `deprecated: true` in `policy.schema.json`
- Fields are **not included** in the main JSON-DSL example in this document

**Recommendation**:
- **Do not use** these fields in new policies
- Existing policies with these fields will continue to work (fields are ignored)
- If you need metadata, use `metadata` field in request context instead

## Storage

### Invariants

Router **guarantees**:
- Policies **always** stored in PostgreSQL with version history
- Policies **always** compiled and cached in Mnesia/ETS (`policy_cache` table)
- Policy updates **always** trigger cache invalidation (hot-reload without downtime)

### Implementation

**Module**: `router_policy_store.erl`  
**Storage**: PostgreSQL (persistent) + Mnesia/ETS `policy_cache` (cache)  
**Tests**: `router_policy_store_SUITE.erl`, `router_policy_store_load_SUITE.erl`

## Policy DSL to Proto Conversion

### Invariants

Router **guarantees**:
- DSL JSON **always** converted to Proto `AdminPolicy` for gRPC communication
- Conversion **always** preserves provider weights (0-100 → 0.0-1.0)
- Conversion **always** preserves fallback rules (DSL `when` → Proto `match`)
- Sticky session key and TTL **never** stored in Proto (stored in Router internal state)

### Implementation

**Module**: `router_policy_store.erl`  
**Tests**: `router_policy_store_SUITE.erl`

### DSL JSON Format (User Input)

**What users write** (stored in database):
```json
{
  "version": "1.0",
  "providers": [
    {"name": "provider_a", "weight": 70},
    {"name": "provider_b", "weight": 30}
  ],
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx"]},
      "retry": 2,
      "to": "provider_b"
    }
  ],
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  }
}
```

### Proto Representation (gRPC / Storage)

**What's used in Proto/gRPC** (converted from DSL):
```protobuf
AdminPolicy {
  policy_id = "string"
  providers = [
    AdminProvider {id = "provider_a", weight = 0.7},
    AdminProvider {id = "provider_b", weight = 0.3}
  ]
  sticky = true  // bool only
  rules = [
    AdminRule {
      match = "status:timeout|5xx",  // converted from when
      prefer = []                     // empty
      fallback = "provider_b"         // from to
    }
  ]
}
```

**Note**: Proto `AdminPolicy` does not include:
- `version` - stored separately in database
- `tenant_id` - stored separately in database
- `sticky.session_key` - stored in Router internal state (ETS/Mnesia)
- `sticky.ttl` - stored in Router internal state (ETS/Mnesia)
- `fallbacks[].retry` - stored in Router internal state or metadata

### Conversion Logic

#### Providers Conversion

**DSL Format**:
```json
{
  "providers": [
    {"name": "provider_a", "weight": 70},
    {"name": "provider_b", "weight": 30}
  ]
}
```

**Proto Format**:
```protobuf
providers = [
  AdminProvider {id = "provider_a", weight = 0.7},
  AdminProvider {id = "provider_b", weight = 0.3}
]
```

**Conversion Rules**:
- `providers[].name` → `AdminProvider.id`
- `providers[].weight` (0-100) → `AdminProvider.weight` (0.0-1.0, divide by 100)
- Array order preserved

#### Fallbacks Conversion

**DSL Format**:
```json
{
  "fallbacks": [
    {
      "when": {"status": ["timeout", "5xx"]},
      "retry": 2,
      "to": "provider_b"
    }
  ]
}
```

**Proto Format**:
```protobuf
rules = [
  AdminRule {
    match = "status:timeout|5xx",  // converted from when
    prefer = [],                    // empty (no preferred providers)
    fallback = "provider_b"         // from to
  }
]
```

**Conversion Rules**:
- `fallbacks[].when` → `AdminRule.match` (converted to match expression)
- `fallbacks[].to` → `AdminRule.fallback`
- `fallbacks[].retry` → stored in Router internal state (not in Proto)
- `fallbacks[]` array → `AdminRule[]` array

**Match Expression Conversion**:
- `{"status": ["timeout", "5xx"]}` → `"status:timeout|5xx"`
- `{"message_type": "chat"}` → `"message_type:chat"`
- Complex conditions converted to match expression format

#### Sticky Conversion

**DSL Format**:
```json
{
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "10m"
  }
}
```

**Proto Format**:
```protobuf
sticky = true  // bool only
```

**Conversion Rules**:
- `sticky.enabled` → `AdminPolicy.sticky` (bool)
- `sticky.session_key` → stored in Router internal state (ETS/Mnesia `sticky_sessions` table)
- `sticky.ttl` → stored in Router internal state (ETS/Mnesia `sticky_sessions` table)

**Internal Storage**:
- Router stores `session_key` and `ttl` in ETS/Mnesia `sticky_sessions` table
- Not part of Proto `AdminPolicy` message
- Used for sticky session lookup and TTL expiration

### Conversion Implementation

**Router Code** (pseudo-code):
```erlang
convert_dsl_to_proto(DSLJson) ->
    Providers = [convert_provider(P) || P <- maps:get(<<"providers">>, DSLJson, [])],
    Rules = [convert_fallback(F) || F <- maps:get(<<"fallbacks">>, DSLJson, [])],
    Sticky = maps:get(<<"enabled">>, maps:get(<<"sticky">>, DSLJson, #{}), false),
    #admin_policy{
        providers = Providers,
        sticky = Sticky,
        rules = Rules
    }.

convert_provider(#{<<"name">> := Name, <<"weight">> := Weight}) ->
    #admin_provider{
        id = Name,
        weight = Weight / 100.0  % Convert 0-100 to 0.0-1.0
    }.

convert_fallback(#{<<"when">> := When, <<"to">> := To}) ->
    Match = convert_when_to_match(When),
    #admin_rule{
        match = Match,
        prefer = [],  % No preferred providers
        fallback = To
    }.
```

### Examples

#### Example 1: Simple Weighted Routing

**DSL**:
```json
{
  "providers": [
    {"name": "openai", "weight": 70},
    {"name": "anthropic", "weight": 30}
  ]
}
```

**Proto**:
```protobuf
AdminPolicy {
  providers = [
    AdminProvider {id = "openai", weight = 0.7},
    AdminProvider {id = "anthropic", weight = 0.3}
  ]
  sticky = false
  rules = []
}
```

#### Example 2: Complex Policy with Fallbacks and Sticky

**DSL**:
```json
{
  "providers": [
    {"name": "openai", "weight": 60},
    {"name": "anthropic", "weight": 40}
  ],
  "fallbacks": [
    {"when": {"status": ["timeout"]}, "retry": 2, "to": "anthropic"},
    {"when": {"status": ["5xx"]}, "retry": 1, "to": "openai"}
  ],
  "sticky": {
    "enabled": true,
    "session_key": "user_id",
    "ttl": "5m"
  }
}
```

**Proto**:
```protobuf
AdminPolicy {
  providers = [
    AdminProvider {id = "openai", weight = 0.6},
    AdminProvider {id = "anthropic", weight = 0.4}
  ]
  sticky = true
  rules = [
    AdminRule {
      match = "status:timeout",
      prefer = [],
      fallback = "anthropic"
    },
    AdminRule {
      match = "status:5xx",
      prefer = [],
      fallback = "openai"
    }
  ]
}
```

**Router Internal State** (not in Proto):
- `sticky_sessions` table: `{session_key: "user_id", ttl: "5m"}`
- Fallback retry counts: stored in Router internal state

### Field Mapping Summary

| DSL Field | Proto Field | Conversion | Internal Storage |
|-----------|-------------|------------|------------------|
| `version` | ❌ Not in Proto | N/A | Database (separate column) |
| `tenant_id` | ❌ Not in Proto | N/A | Database (separate column) |
| `providers[].name` | `AdminProvider.id` | Direct mapping | N/A |
| `providers[].weight` | `AdminProvider.weight` | Divide by 100 (0-100 → 0.0-1.0) | N/A |
| `fallbacks[].when` | `AdminRule.match` | Convert to match expression | N/A |
| `fallbacks[].to` | `AdminRule.fallback` | Direct mapping | N/A |
| `fallbacks[].retry` | ❌ Not in Proto | N/A | Router internal state |
| `sticky.enabled` | `AdminPolicy.sticky` | Direct mapping (bool) | N/A |
| `sticky.session_key` | ❌ Not in Proto | N/A | ETS/Mnesia `sticky_sessions` |
| `sticky.ttl` | ❌ Not in Proto | N/A | ETS/Mnesia `sticky_sessions` |

### References

- **DSL Schema**: `apps/otp/router/docs/schemas/policy.schema.json`
- **Proto Definition**: `apps/otp/router/src/flow_pb.erl` (generated)
- **Router Implementation**: `apps/otp/router/src/router_policy_store.erl`

## Integration

### Invariants

Router **guarantees**:
- Policy **always** used for provider selection per `DecideRequest`
- Policy **always** exposed via Gateway endpoints (read-only for CP1)
- Policy changes **always** trigger cache invalidation

**Implementation**: `router_core.erl` → `router_decider.erl`  
**Tests**: `router_core_SUITE.erl`, `router_decider_SUITE.erl`

## Rate Limiting Integration

### Invariants

Router **guarantees**:
- Per-tenant policy quotas **always** enforced via `rate_counters` ETS table
- Policy updates **always** respect rate limits (unless emergency policy)
- Emergency policies **always** bypass rate limits

**Implementation**: `router_policy_enforcement.erl`  
**Tests**: `router_policy_enforcement_SUITE.erl`

## Delivery Count Tracking

### Invariants

Router **guarantees**:
- Delivery count **always** tracked in `router_delivery_count` ETS table (CP2-LC)
- Delivery count **always** incremented on each JetStream redelivery
- MaxDeliver exhaustion **always** detected when `DeliveryCount >= MaxDeliver`
- Delivery count **always** cleaned up on successful ACK

### Implementation

**Table**: `router_delivery_count` (ETS, `set`, `named_table`, `public`, `write_concurrency`, `read_concurrency`)  
**Modules**: `router_result_consumer.erl`, `router_ack_consumer.erl`, `router_delivery_tracker.erl`  
**Config**: `nats_js_max_deliver` (default: 3)

**Behavior**:
- First delivery: inserts `{MsgId, 1}` into ETS table
- Subsequent deliveries: uses `ets:update_counter(Table, MsgId, 1)`
- MaxDeliver exhaustion: emits `router_jetstream_maxdeliver_exhausted_total` metric, logs WARN, deletes ETS entry, returns `ok`
- Cleanup on ACK: `cleanup_delivery_count/1` deletes `{MsgId, Count}` and returns `ok`
- Lookup: `ets:lookup(router_delivery_count, MsgId)` returns `[{MsgId, Count}]` or `[]` after cleanup/exhaustion

**Tests**: `router_delivery_count_tracking_SUITE.erl`, `router_jetstream_e2e_SUITE.erl`

## Monitoring & Observability

### Metrics

Router **emits**:
- `router_policy_decision_latency_ms` - Policy decision latency
- `router_policy_cache_hit_total` - Cache hit count
- `router_policy_cache_miss_total` - Cache miss count
- `router_fallback_activation_total` - Fallback activation count
- `router_sticky_session_total` - Sticky session count

**Implementation**: `router_telemetry_handler.erl`  
**Tests**: `router_core_telemetry_contract_SUITE.erl`

## Error Handling

### Invariants

Router **guarantees**:
- Invalid policies **always** rejected with detailed error (JSON schema validation)
- Default policy **always** used on cache corruption
- Cache misses **always** trigger database lookup (graceful degradation)

**Implementation**: `router_policy_store.erl`, `router_policy_validator.erl`  
**Tests**: `router_policy_validator_SUITE.erl`, `router_policy_store_fault_tolerance_SUITE.erl`

## Security

### Invariants

Router **guarantees**:
- Tenant isolation **always** enforced at policy level (RBAC)
- Policy changes **always** require appropriate permissions
- All policy modifications **always** logged in audit trail (actor/timestamp)

**Implementation**: `router_rbac.erl`, `router_policy_store.erl`  
**Tests**: `router_rbac_SUITE.erl`, `router_policy_store_SUITE.erl`

## CP2 Enhancements (Future)

The following enhancements are planned for CP2-LC or CP2+:

### Extension Timeout and Retry Override

**Status**: 📅 **CP2** (Planned)

**Description**: Per-policy override for extension timeout and retry (currently only in Extension Registry)

**Fields**:
```json
{
  "pre": [
    {
      "id": "normalize_text",
      "timeout_ms": 5000,  // Override registry timeout
      "retry": 2            // Override registry retry count
    }
  ]
}
```

**Impact**: Allows per-policy customization of extension behavior

### Circuit Breaker Configuration

**Status**: ✅ **CP2 IMPLEMENTED** (Phase 1-3 Complete)

**Description**: Circuit breaker configuration for providers to prevent cascading failures

**Architecture**: **Per-provider** circuit breaker (not per-policy)

**Implementation**:
- **Module**: `router_circuit_breaker.erl` - State machine (Closed/Open/Half-Open)
- **Integration**: `router_decider.erl` - CB check before provider selection
- **Storage**: ETS table `router_provider_circuit_breaker` (key: `{TenantId, ProviderId}`)
- **Tests**: `router_circuit_breaker_SUITE.erl`, `router_circuit_breaker_integration_SUITE.erl`

**Fields**:
```json
{
  "circuit_breaker": {
    "enabled": true,
    "failure_threshold": 5,
    "success_threshold": 2,
    "timeout_ms": 60000,
    "half_open_max_calls": 3,
    "error_rate_threshold": 0.5,
    "error_rate_window_seconds": 60
  }
}
```

**Circuit States**:
- **Closed** (normal): Requests flow through, failures tracked
- **Open** (failing): Requests fail fast, immediate fallback (no retry)
- **Half-Open** (testing): Limited requests allowed to test recovery

**Integration with Retry/Backoff and Fallbacks**:
1. **Circuit Breaker Check** (first): If open → fail fast, skip to fallback
2. **Provider Call**: If closed/half-open → call provider
3. **Retry/Backoff**: If failure and retry configured → retry with backoff
4. **Fallback**: If retry exhausted or circuit open → use fallback provider

**Metrics That Trigger Circuit Breaker**:
- **Consecutive Failures**: `failure_count >= failure_threshold` → open circuit
- **Error Rate**: `error_rate >= error_rate_threshold` (over time window) → open circuit
- **Error Types**: Timeout, 5xx, connection errors (4xx and validation errors do NOT count)

**Error Types**:
- **Count as Failures**: `timeout`, `5xx`, `connection_error`, `provider_unavailable`
- **Do NOT Count**: `4xx`, `validation_error`, `rate_limit_exceeded`

**Metrics**:
- `router_circuit_breaker_events_total{tenant_id, provider_id, event_type}` - Counter for success/failure events
- `router_circuit_breaker_state_transitions_total{tenant_id, provider_id, state}` - Counter for state transitions

**Logging**: Circuit breaker events logged with structured JSON format (see `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md`)

**Alerting**: Prometheus alert rules defined (see `apps/otp/router/docs/PROMETHEUS_ALERTS.md`)

**Reference**: 
- `docs/archive/dev/CIRCUIT_BREAKER_DESIGN.md` - Comprehensive design document
- `docs/archive/dev/CIRCUIT_BREAKER_OBSERVABILITY.md` - Observability specification

**Impact**: Improves reliability by preventing cascading failures, reduces wasted resources on unhealthy providers

### Per-Policy Rate Limiting

**Status**: 📅 **CP2** (Design Document)

**Description**: Multi-level rate limiting (global, per-tenant, per-policy) to enforce quotas and prevent resource exhaustion

**Architecture**: Multi-level rate limiting with scope-based enforcement

**Fields**:
```json
{
  "rate_limit": {
    "enabled": true,
    "requests_per_second": 100,
    "burst": 50,
    "scope": "policy"
  }
}
```

**Alternative Format** (per-minute):
```json
{
  "rate_limit": {
    "enabled": true,
    "limit": 6000,
    "window_seconds": 60,
    "burst": 1000,
    "scope": "policy"
  }
}
```

**Scopes**:
- **`"policy"`**: Per-policy rate limit (within tenant)
- **`"tenant"`**: Per-tenant rate limit (across all policies, configured outside policy JSON)
- **`"global"`**: Global rate limit (all tenants, all policies, configured in system config)

**Algorithm**: Token Bucket (sustained rate + burst capacity)

**Integration with Gateway Rate Limiting**:
1. **Gateway Rate Limit Check** (first): Endpoint-level, before Router call
2. **Router Rate Limit Check** (second): Policy/tenant-level, after Gateway check
3. **Provider Selection**: Only if rate limits pass

**Responsibility Separation**:
- **Gateway**: Endpoint-level rate limiting (CP1, already implemented)
- **Router**: Policy/tenant-level rate limiting (CP2, to be implemented)

**Enforcement Order** (most restrictive first):
1. Gateway Rate Limit (endpoint-level)
2. Global Rate Limit (system-wide)
3. Tenant Rate Limit (per-tenant)
4. Policy Rate Limit (per-policy)

**Reference**: See `docs/archive/dev/RATE_LIMIT_POLICY_DSL_DESIGN.md` for comprehensive design document.

**Impact**: Enforces per-policy quotas, tenant SLA tiers, burst protection, prevents resource exhaustion

### Per-Policy Timeout

**Status**: 📅 **CP2** (Design Document)

**Description**: Multi-level timeout configuration (per-policy default, per-provider override) to control request duration limits

**Architecture**: Policy-level default timeout with per-provider overrides

**Fields**:
```json
{
  "timeout_ms": 30000,
  "providers": [
    {
      "name": "provider_a",
      "weight": 70,
      "timeout_ms": 10000  // Override: faster timeout
    },
    {
      "name": "provider_b",
      "weight": 30,
      "timeout_ms": 60000  // Override: slower timeout
    }
  ]
}
```

**Behavior**:
- Policy-level `timeout_ms`: Default timeout for all providers (milliseconds)
- Provider-level `timeout_ms`: Override timeout for specific provider (optional)
- Priority: Per-provider timeout takes precedence over policy-level timeout
- Constraints: Minimum `100` ms, Maximum `300000` ms (5 minutes)

**When Timeout Exceeded**:
1. Provider call is cancelled or abandoned
2. Request fails with status: `timeout`
3. Fallback rules evaluated for `{"status": ["timeout"]}` condition
4. Circuit breaker tracks timeout as failure (increment `failure_count`)

**Integration with Fallbacks and Circuit Breaker**:
- **Timeout → Fallback**: If timeout exceeded → trigger fallback matching `{"status": ["timeout"]}`
- **Timeout → Circuit Breaker**: Timeout counts as failure, can open circuit if threshold exceeded
- **Timeout vs Retry**: Timeout occurs during call, retry occurs after call fails

**Reference**: See `docs/archive/dev/TIMEOUT_HEALTH_CHECK_DESIGN.md` for comprehensive design document.

**Impact**: Controls request timeout, prevents long-running requests, enables timeout-based fallbacks

### Provider Priority (Separate from Weights)

**Status**: 📅 **CP2+** (DEFERRED - Design Analysis Complete)

**Description**: Provider priority field for strict ordering (separate from weighted distribution)

**Decision**: **DEFERRED to CP2+** (not implemented in CP2-LC)

**Rationale**: 
- Current mechanisms (weights + fallbacks) are sufficient for primary/backup and tiered selection scenarios
- Priority adds complexity (priority filtering, priority + weights interaction) without clear benefit
- CP2 focuses on core reliability (circuit breaker, health checks, timeouts). Priority is a "nice to have" enhancement

**Alternative Solutions for CP2**:
1. **Primary/Backup with Weights + Fallbacks**:
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

2. **Tiered Selection with Multiple Policies**: Use separate policies for each tier, fallback between policies

**Proposed Specification (If Implemented in CP2+)**:
```json
{
  "providers": [
    { "name": "provider_a", "weight": 70, "priority": 10 },
    { "name": "provider_b", "weight": 30, "priority": 10 },
    { "name": "provider_c", "weight": 100, "priority": 5 }
  ]
}
```

**Behavior** (if implemented):
- Filter providers by highest priority first
- Apply weighted distribution within same priority tier
- If no providers available in tier → try next priority tier

**Reference**: See `docs/archive/dev/PROVIDER_PRIORITY_DESIGN.md` for comprehensive design analysis and decision rationale.

**Impact**: Would enable strict primary/backup and tiered selection, but adds complexity. Current mechanisms (weights + fallbacks) are sufficient for CP2.

### Health Check Configuration

**Status**: 📅 **CP2+** (Design Document)

**Description**: Proactive health monitoring for providers with correlation to fallbacks and circuit breakers

**Architecture**: Per-policy health check configuration with per-provider overrides

**Fields**:
```json
{
  "health_check": {
    "enabled": true,
    "url": "https://provider_a.example.com/health",
    "interval_ms": 10000,
    "timeout_ms": 5000,
    "unhealthy_threshold": 3,
    "healthy_threshold": 2
  },
  "providers": [
    {
      "name": "provider_a",
      "weight": 70,
      "health_check": {
        "url": "https://provider_a.example.com/health",
        "interval_ms": 5000  // Override: more frequent checks
      }
    }
  ]
}
```

**Health Check States**:
- **Healthy**: Provider responds with 2xx status code within timeout
- **Unhealthy**: Provider fails health check (timeout, 4xx, 5xx, connection error)

**Health Check Process**:
1. **Periodic Checks**: Health check runs every `interval_ms` milliseconds
2. **HTTP GET Request**: `GET {url}` with timeout `timeout_ms`
3. **Response Evaluation**: 2xx = healthy, non-2xx/timeout = unhealthy
4. **State Transition**: 
   - If `unhealthy_count >= unhealthy_threshold` → mark provider **unhealthy**
   - If `healthy_count >= healthy_threshold` → mark provider **healthy**

**Health Check Criteria**:
- **Unhealthy**: Timeout, 4xx, 5xx, connection error, invalid response
- **Healthy**: 2xx status code within timeout
- **Thresholds**: Configurable `unhealthy_threshold` (default: 3) and `healthy_threshold` (default: 2)

**Integration with Fallbacks and Circuit Breaker**:
- **Health Check → Fallback**: If provider unhealthy → excluded from selection → trigger fallback
- **Health Check vs Circuit Breaker**: 
  - Health check: Proactive monitoring (periodic checks)
  - Circuit breaker: Reactive protection (request-based failures)
  - Both can trigger fallbacks independently

**Execution Order**:
1. **Health Check** (if enabled): If provider unhealthy → skip provider, proceed to fallback
2. **Circuit Breaker Check**: If circuit open → fail fast, trigger fallback
3. **Provider Selection**: Only healthy providers with closed circuits
4. **Provider Call**: With timeout (if exceeded → trigger fallback)

**Reference**: See `docs/archive/dev/TIMEOUT_HEALTH_CHECK_DESIGN.md` for comprehensive design document.

**Impact**: Proactive health monitoring, prevents routing to unhealthy providers, enables health-based fallbacks

**Reference**: See `docs/archive/dev/POLICY_DSL_GAPS_CP_CLASSIFICATION.md` for detailed CP classification and planning.

### Explanation Detail Levels

**Status**: 📅 **CP2** (Draft Specification)

**Description**: Control verbosity of explanation data in logs and audit trails

**Current State**: Basic support exists in `router_policy_applier.erl` with three levels (minimal, detailed, verbose). Full specification and policy-level configuration planned for CP2.

**Detail Levels**:
- **`"minimal"`**: Production logging with minimal data footprint (1-2 steps, tenant_id only in context)
- **`"detailed"`** (default): Standard audit trail with key information (all steps, key context fields)
- **`"verbose"`**: Maximum detail for debugging (all steps with full context, timing, metadata)

**Usage**:
```erlang
Context = #{<<"detail_level">> => <<"minimal">>},
router_policy_applier:apply_policy(Request, TenantId, PolicyId, Context).
```

**Impact**:
- **Steps**: Minimal (1-2 items) vs Detailed (all steps) vs Verbose (all steps + full context)
- **Context**: Minimal (tenant_id only) vs Detailed (key fields) vs Verbose (all available fields)

**Reference**: See `docs/archive/dev/EXPLANATION_LEVELS_SPEC.md` for comprehensive specification (draft).

## References
- `docs/MNESIA_ETS_KEYS.md` - caches and sticky tables
- `docs/GATEWAY_ROUTES.md` - endpoints related to routing decisions
- `docs/GATEWAY_RATE_LIMITING.md` - rate limiting integration
- `apps/otp/router/src/router_policy_store.erl` - policy storage implementation

## Router and DevState State Flow
- Source of Truth: CP and global state live in DevState (Postgres + HTTP); `.trae/state.json`/`.trae/history.json` are DevState exports, not hand-edited files.
- Workflow: CP changes occur via DevState HTTP/CLI (locks, verify, export); Router reads exported `.trae/state.json` and gates features by `current_cp`; Router does not write `.trae/*`.
- Router Expectations: requires `current_cp` (e.g., CP1-LC → baseline; CP2-LC+ → advanced features via flags); honors No-Drift and HMAC verification; missing/invalid state → CP1 baseline fallback.
- References: `docs/ADR/ADR-015-router-devstate-integration.md`, `docs/STATE.schema.json`, `docs/ADR/ADR-002-state-management.md`, `devstate/docs/DEVSTATE_OVERVIEW.md`.
