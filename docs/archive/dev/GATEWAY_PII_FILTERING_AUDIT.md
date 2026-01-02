# Gateway PII Filtering Audit Report

**Date**: 2025-01-27  
**Component**: Gateway (C)  
**Status**: ⚠️ **ISSUES FOUND**

## Executive Summary

The current PII filtering implementation in Gateway has **critical limitations** that do not meet CP1 observability requirements. The filtering is too simplistic and only applies to the `message` field, leaving sensitive data in `context` objects unprotected.

## Current Implementation Analysis

### Function: `filter_pii()` (lines 607-642)

**Location**: `apps/c-gateway/src/http_server.c`

**Current Behavior**:
```c
static void filter_pii(const char *input, char *output, size_t outlen)
{
    // Simple keyword check: if input contains any sensitive keyword,
    // replace entire string with "[REDACTED]"
    if (strstr(input, sensitive_keywords[i]) != NULL) {
        snprintf(output, outlen, "[REDACTED]");
    }
}
```

**Problems**:
1. ❌ **Only checks if string contains keywords** - not field names
2. ❌ **Replaces entire string** - not just sensitive fields
3. ❌ **No JSON field-level filtering** - cannot filter specific fields in JSON objects
4. ❌ **No recursive filtering** - cannot handle nested objects
5. ❌ **Case-sensitive** - may miss variations

### Usage in Logging Functions

**Applied to**:
- ✅ `log_error()` - line 666
- ✅ `log_warn()` - line 885
- ✅ `log_debug()` - line 922
- ❌ `log_info()` - **NOT APPLIED** (line 810-862)

**Scope**:
- ✅ Only filters `message` field
- ❌ **Does NOT filter `context` object fields**
- ❌ **Does NOT filter top-level fields** (tenant_id, trace_id, etc.)

## Comparison with Router Implementation

### Router (Erlang) - Correct Implementation

**Location**: `apps/otp/router/src/router_logger.erl`

**Features**:
- ✅ **Recursive filtering** of nested maps
- ✅ **Field name checking** - filters fields named `api_key`, `password`, etc.
- ✅ **Value pattern matching** - detects secrets in values (e.g., "Bearer token")
- ✅ **Case-insensitive** for header-like fields
- ✅ **Applied to entire context** before logging

**Example**:
```erlang
filter_pii(Context) when is_map(Context) ->
    maps:fold(fun
        (Key, Value, Acc) ->
            IsPII = lists:member(KeyBin, ?PII_FIELDS),
            case IsPII of
                true -> maps:put(KeyBin, <<"[REDACTED]">>, Acc);
                false -> 
                    case is_map(Value) of
                        true -> maps:put(KeyBin, filter_pii(Value), Acc);
                        false -> maps:put(KeyBin, Value, Acc)
                    end
            end
    end, #{}, Context).
```

## Requirements from Documentation

### From `docs/OBSERVABILITY_CONVENTIONS.md`:

**Filtered Fields** (lines 190-200):
- `password`
- `api_key`
- `secret`
- `token`
- `access_token`
- `refresh_token`
- `authorization`
- `credit_card`
- `ssn`
- `email` (optional, configurable)
- `phone` (optional, configurable)

**Expected Behavior** (lines 202-222):
```json
{
  "context": {
    "tenant_id": "tenant_123",
    "api_key": "[REDACTED]",  // Field value filtered
    "message": "Hello"        // Other fields preserved
  }
}
```

### From `config/observability/logging.json`:

**PII Filtering Configuration** (lines 109-125):
```json
"pii_filtering": {
  "enabled": true,
  "fields": [
    "password", "api_key", "secret", "token",
    "access_token", "refresh_token", "authorization",
    "credit_card", "ssn", "email", "phone"
  ],
  "replacement": "[REDACTED]"
}
```

## Identified Issues

### Issue 1: No Field-Level Filtering in Context

**Problem**: Fields in `context` object are not filtered.

**Example**:
```c
// Current output (WRONG):
{
  "context": {
    "api_key": "sk-1234567890abcdef",  // ❌ NOT FILTERED
    "method": "POST"
  }
}

// Expected output:
{
  "context": {
    "api_key": "[REDACTED]",  // ✅ FILTERED
    "method": "POST"
  }
}
```

**Impact**: **HIGH** - Sensitive data leaks in logs.

### Issue 2: `log_info()` Does Not Use Filtering

**Problem**: `log_info()` function (lines 810-862) does not call `filter_pii()`.

**Impact**: **MEDIUM** - INFO logs may contain sensitive data in messages.

### Issue 3: No Recursive Filtering

**Problem**: Cannot filter nested objects in `context`.

**Example**:
```json
{
  "context": {
    "headers": {
      "Authorization": "Bearer sk-123",  // ❌ NOT FILTERED
      "X-Api-Key": "key-456"             // ❌ NOT FILTERED
    }
  }
}
```

**Impact**: **MEDIUM** - Nested sensitive data leaks.

### Issue 4: Incorrect Filtering Logic

**Problem**: Current implementation replaces entire string if it contains keywords, not just sensitive fields.

**Example**:
```c
// Input: "Error: Invalid api_key provided"
// Current output: "[REDACTED]"  // ❌ Entire message lost
// Expected: "Error: Invalid [REDACTED] provided"  // ✅ Only sensitive part filtered
```

**Impact**: **LOW** - Reduces log usefulness but prevents leaks.

## Recommendations

### Priority 1: Implement JSON Field-Level Filtering

**Required Changes**:
1. Parse JSON `context` object before logging
2. Filter fields by name (not by value content)
3. Preserve non-sensitive fields

**Approach**:
- Use `jansson` library (already used in Gateway) to parse/filter JSON
- Create `filter_pii_json()` function that recursively filters JSON objects
- Apply to `context` object before serialization

### Priority 2: Apply Filtering to All Log Functions

**Required Changes**:
1. Add `filter_pii()` call to `log_info()`
2. Filter `context` object in all log functions
3. Filter top-level fields if they contain sensitive data

### Priority 3: Implement Recursive Filtering

**Required Changes**:
1. Support nested JSON objects
2. Recursively filter all levels
3. Handle arrays (filter elements if they contain sensitive fields)

### Priority 4: Improve Message Filtering

**Required Changes**:
1. Replace sensitive patterns in messages (not entire message)
2. Use regex to find and replace secrets in text
3. Preserve message structure

## Implementation Plan

### Step 1: Create JSON Filtering Function

```c
/* Filter PII from JSON object recursively */
static json_t *filter_pii_json(json_t *obj)
{
    if (!json_is_object(obj)) {
        return obj;
    }
    
    json_t *filtered = json_object();
    const char *key;
    json_t *value;
    
    json_object_foreach(obj, key, value) {
        /* Check if key is sensitive */
        if (is_sensitive_field(key)) {
            json_object_set_new(filtered, key, json_string("[REDACTED]"));
        } else if (json_is_object(value)) {
            /* Recursively filter nested objects */
            json_object_set(filtered, key, filter_pii_json(value));
        } else {
            json_object_set(filtered, key, value);
        }
    }
    
    return filtered;
}
```

### Step 2: Update Log Functions

```c
static void log_info(...)
{
    /* Build context JSON object */
    json_t *context = json_object();
    json_object_set_new(context, "stage", json_string(stage));
    json_object_set_new(context, "method", json_string(method));
    json_object_set_new(context, "path", json_string(path));
    /* ... other fields ... */
    
    /* Filter PII from context */
    json_t *filtered_context = filter_pii_json(context);
    
    /* Serialize and log */
    char *context_str = json_dumps(filtered_context, JSON_COMPACT);
    /* ... log ... */
    json_decref(filtered_context);
    free(context_str);
}
```

### Step 3: Add Field Name Checking

```c
static int is_sensitive_field(const char *field_name)
{
    const char *sensitive_fields[] = {
        "password", "api_key", "secret", "token",
        "access_token", "refresh_token", "authorization",
        "credit_card", "ssn", "email", "phone"
    };
    
    /* Case-insensitive comparison */
    for (size_t i = 0; i < sizeof(sensitive_fields) / sizeof(sensitive_fields[0]); i++) {
        if (strcasecmp(field_name, sensitive_fields[i]) == 0) {
            return 1;
        }
    }
    
    return 0;
}
```

## Testing Requirements

### Test Cases

1. **Field Name Filtering**:
   - Input: `{"api_key": "sk-123", "method": "POST"}`
   - Expected: `{"api_key": "[REDACTED]", "method": "POST"}`

2. **Nested Object Filtering**:
   - Input: `{"headers": {"Authorization": "Bearer token"}}`
   - Expected: `{"headers": {"Authorization": "[REDACTED]"}}`

3. **Multiple Sensitive Fields**:
   - Input: `{"api_key": "sk-1", "password": "pass123", "method": "POST"}`
   - Expected: `{"api_key": "[REDACTED]", "password": "[REDACTED]", "method": "POST"}`

4. **Non-Sensitive Fields Preserved**:
   - Input: `{"tenant_id": "t1", "method": "POST"}`
   - Expected: `{"tenant_id": "t1", "method": "POST"}` (no changes)

5. **All Log Functions**:
   - Verify `log_error()`, `log_warn()`, `log_info()`, `log_debug()` all filter correctly

## Compliance Status

### CP1 Observability Requirements

- ❌ **PII Filtering**: Not fully compliant
  - Field-level filtering: **MISSING**
  - Recursive filtering: **MISSING**
  - Applied to all log functions: **PARTIAL** (missing in `log_info()`)

### Documentation Compliance

- ❌ Does not match `docs/OBSERVABILITY_CONVENTIONS.md` requirements
- ❌ Does not match `config/observability/logging.json` specification

## Conclusion

The current PII filtering implementation in Gateway is **insufficient** and does not meet CP1 observability requirements. Critical sensitive data may leak in logs through `context` objects and nested structures.

**Immediate Action Required**: Implement proper JSON field-level filtering before Gateway is used in production.

## References

- `docs/OBSERVABILITY_CONVENTIONS.md` - PII filtering requirements
- `config/observability/logging.json` - PII filtering configuration
- `apps/otp/router/src/router_logger.erl` - Reference implementation (correct)
- `apps/c-gateway/src/http_server.c` - Current implementation (needs fixes)

