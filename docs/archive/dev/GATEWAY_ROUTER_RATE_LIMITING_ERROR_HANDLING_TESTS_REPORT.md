# Gateway ↔ Router: Rate Limiting + Intake Error Handling Tests Report

**Date**: 2025-01-27  
**Status**: ✅ Implementation Complete  
**Purpose**: Integration tests for the interaction between Gateway rate limiting (429) and Router intake errors (400/401/500).

## Summary

Created comprehensive integration tests to verify that Gateway rate limiting (429) and Router intake errors (400/401/500) do not conflict and are handled with clear priority:
1. **Rate Limiting (429)** - Checked FIRST, blocks Router call
2. **Router Intake Errors (400/401/500)** - Checked AFTER rate limit passes

## Test File

### `tests/integration/gateway-router-rate-limiting-error-handling.test.ts`

**Test Suites**:

1. **Rate Limiting Takes Priority (429 Before Router Validation)**
   - `should return 429 when rate limit exceeded, even if request would trigger Router intake error`
   - `should return 429 when rate limit exceeded, even if request has invalid correlation fields`
   - `should return 429 when rate limit exceeded, even if tenant is forbidden`

2. **Router Intake Errors After Rate Limit Passes**
   - `should return Router intake error (400) when rate limit OK but schema validation fails`
   - `should return Router intake error (401) when rate limit OK but tenant is forbidden`
   - `should return Router intake error (400) when rate limit OK but correlation fields invalid`

3. **Error Response Format Distinction**
   - `should distinguish rate limit error (429) from Router intake error (400) in response format`
   - `should include rate limit headers only for 429 responses, not for Router intake errors`

4. **Mixed Scenarios: Rate Limit Reset After Intake Errors**
   - `should handle rate limit reset correctly after Router intake errors`

5. **Error Code Mapping Consistency**
   - `should map Router intake errors correctly while preserving rate limit error format`

## Test Coverage

### Scenarios Covered

1. **Rate Limit Priority**:
   - ✅ Rate limit exceeded → 429 (Router NOT called)
   - ✅ Rate limit exceeded with invalid request data → 429 (not 400)
   - ✅ Rate limit exceeded with invalid correlation fields → 429 (not 400)
   - ✅ Rate limit exceeded with forbidden tenant → 429 (not 401)

2. **Router Intake Errors After Rate Limit**:
   - ✅ Rate limit OK + schema validation fails → 400 (not 429)
   - ✅ Rate limit OK + tenant forbidden → 401 (not 429)
   - ✅ Rate limit OK + correlation fields invalid → 400 (not 429)

3. **Error Response Format**:
   - ✅ 429 responses: `error.code = "rate_limit_exceeded"`, no `intake_error_code`, includes rate limit headers
   - ✅ 400/401/500 responses: `error.code = "invalid_request"/"unauthorized"/"internal"`, includes `intake_error_code`, may include rate limit headers but not `Retry-After`

4. **Mixed Scenarios**:
   - ✅ Rate limit reset after Router intake errors
   - ✅ Error code mapping consistency

## Key Test Assertions

### Rate Limit Error (429)

```typescript
expect(res.status).toBe(429);
expect(res.data?.error?.code).toBe('rate_limit_exceeded');
expect(res.data?.error?.intake_error_code).toBeUndefined(); // Router not called
expect(res.headers['x-ratelimit-limit']).toBeDefined();
expect(res.headers['x-ratelimit-remaining']).toBe('0');
expect(res.headers['retry-after']).toBeDefined();
```

### Router Intake Error (400/401/500)

```typescript
expect(res.status).toBe(400); // or 401, 500
expect(res.data?.error?.code).toBe('invalid_request'); // or 'unauthorized', 'internal'
expect(res.data?.error?.intake_error_code).toBeDefined(); // Router error code
const remaining = parseInt(res.headers['x-ratelimit-remaining'] || '0');
expect(remaining).toBeGreaterThan(0); // Should have remaining quota
```

## Running Tests

```bash
# Install dependencies (if not already done)
cd tests
pnpm install

# Start Gateway (in another terminal)
cd apps/c-gateway
make run

# Start Router (in another terminal)
cd apps/otp/router
rebar3 shell

# Run tests
cd tests
pnpm test tests/integration/gateway-router-rate-limiting-error-handling.test.ts
```

## Expected Behavior

### Request Flow

```
Client Request
    ↓
Gateway Rate Limit Check
    ↓
    ├─ Rate Limit Exceeded? → YES → Return 429 (Router NOT called)
    │
    └─ NO → Forward to Router
            ↓
        Router Intake Validation
            ↓
            ├─ Validation Failed? → YES → Return Router Error (400/401/500)
            │
            └─ NO → Process Request → Return 200/503
```

### Error Priority

1. **Rate Limiting (429)** - Highest Priority
   - Checked BEFORE Router validation
   - Blocks Router call when exceeded
   - No `intake_error_code` in response

2. **Router Intake Errors (400/401/500)** - Lower Priority
   - Checked AFTER rate limit passes
   - Router validates request
   - Includes `intake_error_code` in response

## Documentation

- `docs/GATEWAY_ROUTER_ERROR_HANDLING_PRIORITY.md`: Complete specification of error handling priority
- `docs/ARCHITECTURE/api-registry.md`: Updated with error handling priority section
- `tests/integration/gateway-router-rate-limiting-error-handling.test.ts`: Integration tests

## Key Invariants Verified

1. ✅ Rate limiting is **always checked first** (before Router call)
2. ✅ Router intake errors **only occur after** rate limit passes
3. ✅ Error types **never conflict** (mutually exclusive)
4. ✅ A request cannot return both 429 and 400/401/500
5. ✅ Error response format clearly distinguishes rate limit errors from Router intake errors

## References

- `docs/GATEWAY_ROUTER_ERROR_HANDLING_PRIORITY.md`: Error handling priority specification
- `docs/GATEWAY_RATE_LIMITING.md`: Rate limiting specification
- `docs/ARCHITECTURE/api-registry.md`: Error code mapping
- `apps/c-gateway/src/http_server.c`: Gateway rate limiting implementation
- `apps/otp/router/src/router_intake_error_handler.erl`: Router intake error handling
- `tests/integration/gateway-router-error-handling.test.ts`: Router intake error tests

