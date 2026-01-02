# Gateway ↔ Router Contract Smoke Test

## Purpose

This document describes the smoke test for verifying the contract between Gateway (C11 HTTP Gateway) and Router (Erlang/OTP) over NATS/REST.

## Goals

- Verify that `DecideRequest`/`DecideResponse` structure matches `PROTO_NATS_MAPPING.md` and `API_CONTRACTS.md`
- Verify that headers (`trace_id`, `tenant_id`, `version`, `Nats-Msg-Id`) pass through the chain
- Protect against DTO/subject divergence between Gateway and Router

## Test Components

### 1. Router Contract Smoke Test

**Location**: `apps/otp/router/test/router_gateway_contract_smoke_SUITE.erl`

**Test Cases**:
- `test_decide_request_response_structure`: Verifies DecideRequest/DecideResponse structure matches contract
- `test_headers_pass_through`: Verifies headers (trace_id, tenant_id, version) are correctly extracted and passed through
- `test_error_response_structure`: Verifies ErrorResponse structure matches contract
- `test_invalid_request_missing_fields`: Verifies ErrorResponse for missing required fields
- `test_invalid_request_wrong_version`: Verifies ErrorResponse for unsupported version
- `test_tenant_rejected`: Verifies ErrorResponse for tenant validation failures
- `test_internal_router_error`: Verifies ErrorResponse for internal router errors

**Tags**: `@test_category cp1_smoke, fast, integration`

**Run**:
```bash
cd apps/otp/router
rebar3 ct --suite router_gateway_contract_smoke_SUITE
```

### 2. E2E Smoke Script

**Location**: `scripts/gateway_router_contract_smoke.sh`

**Modes**:
- `--router-only`: Run only Router contract test
- `--gateway-only`: Run only Gateway contract test (if available)
- `--full`: Run both Router and Gateway tests (default)

**Usage**:
```bash
# Show help and available options
./scripts/gateway_router_contract_smoke.sh --help

# Run Router test only
./scripts/gateway_router_contract_smoke.sh --router-only

# Run Gateway test only (if available)
./scripts/gateway_router_contract_smoke.sh --gateway-only

# Run full E2E test
./scripts/gateway_router_contract_smoke.sh --full
./scripts/gateway_router_contract_smoke.sh  # (same as --full)
```

**Help Output Example**:
```bash
$ ./scripts/gateway_router_contract_smoke.sh --help
Gateway ↔ Router Contract Smoke Test

DESCRIPTION:
  Verifies the contract between Gateway (C-Gateway) and Router (Erlang/OTP):
  - DecideRequest/DecideResponse structure matches PROTO_NATS_MAPPING and API_CONTRACTS
  - Headers (trace_id, tenant_id, version, Nats-Msg-Id) pass through the chain
  - Error responses follow the contract (invalid_request, unauthorized, etc.)

USAGE:
  ./scripts/gateway_router_contract_smoke.sh [OPTIONS]

OPTIONS:
  --router-only         Run only Router contract test (default if Gateway not available)
  --gateway-only        Run only Gateway contract test (if available)
  --full                Run both Router and Gateway tests (default)
  -h, --help            Show this help message

EXAMPLES:
  # Run Router test only (fastest)
  ./scripts/gateway_router_contract_smoke.sh --router-only

  # Run Gateway test only (if Gateway tests available)
  ./scripts/gateway_router_contract_smoke.sh --gateway-only

  # Run full E2E test (Router + Gateway)
  ./scripts/gateway_router_contract_smoke.sh --full
  ./scripts/gateway_router_contract_smoke.sh  # (same as --full)

EXIT CODES:
  0  - Success (all tests passed)
  1  - Test failure (one or more tests failed)
  2  - Prerequisites not met (see error message for details)
  3  - Invalid arguments (see usage above)

TROUBLESHOOTING:
  Exit code 2 (Prerequisites not met):
    - Router directory not found: Check that apps/otp/router exists
    - Router not compiled: Run 'cd apps/otp/router && rebar3 compile'
    - rebar3 not found: Install Erlang/OTP and rebar3
    - Gateway directory not found: Gateway tests will be skipped (use --router-only)

  Exit code 3 (Invalid arguments):
    - Check that mode is one of: --router-only, --gateway-only, --full
    - Use --help to see all available options

For more information, see: docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md
```

**Exit Codes**:
- `0`: Success
- `1`: Test failure
- `2`: Prerequisites not met
- `3`: Invalid arguments

## Contract Verification

### DecideRequest Structure

According to `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` and `docs/API_CONTRACTS.md`:

```json
{
  "version": "1",
  "tenant_id": "string",
  "request_id": "string",
  "trace_id": "string (optional)",
  "task": {
    "type": "chat|completion|embedding",
    "payload": { ... }
  },
  "policy_id": "string (optional)",
  "constraints": { ... },
  "metadata": { ... },
  "push_assignment": false
}
```

**Required Fields**:
- `version`: Must be `"1"`
- `tenant_id`: String
- `request_id`: String
- `task`: Object with `type` and `payload`

**Optional Fields**:
- `trace_id`: String (hex-32 format)
- `policy_id`: String
- `constraints`: Object
- `metadata`: Object
- `push_assignment`: Boolean

### DecideResponse Structure

```json
{
  "ok": true,
  "decision": {
    "provider_id": "string",
    "reason": "string",
    "priority": 0,
    "expected_latency_ms": 0,
    "expected_cost": 0.0,
    "metadata": { ... }
  },
  "context": {
    "request_id": "string",
    "trace_id": "string (optional)"
  }
}
```

**Required Fields**:
- `ok`: Boolean
- `decision`: Object with `provider_id`, `reason`, `priority`
- `context`: Object with `request_id`

### ErrorResponse Structure

```json
{
  "ok": false,
  "error": {
    "code": "invalid_request|unauthorized|policy_not_found|decision_failed|internal",
    "message": "string",
    "details": { ... }
  },
  "context": {
    "request_id": "string",
    "trace_id": "string (optional)"
  }
}
```

### Headers

According to `docs/NATS_SUBJECTS.md`:

- `trace_id`: hex-32 (optional)
- `tenant_id`: string (optional)
- `version`: `"1"`
- `Nats-Msg-Id`: Set by NATS (not in payload)

**Note**: In NATS, headers are separate from payload. Router extracts headers from NATS message headers, not from JSON payload.

## Error Contract Scenarios

This section describes how errors in Router/CAF are reflected in HTTP responses from C-Gateway, ensuring consistent error handling across the stack.

### Error Response Flow

```
C-Gateway (HTTP) → NATS (ErrorResponse) → Router (Error Logic) → C-Gateway (HTTP Response)
```

**Key Principle**: Each error scenario has a clear mapping:
- **HTTP Status Code**: C-Gateway maps ErrorResponse code to HTTP status
- **NATS ErrorResponse**: Router generates ErrorResponse with standardized codes
- **Router Logic**: Router's error handling generates appropriate error codes

### Error Scenarios

#### 1. Invalid Request - Missing Required Fields

**Scenario**: DecideRequest is missing required fields (`tenant_id`, `request_id`, `task`).

**Router Behavior**:
- Router detects missing fields during request parsing
- Generates ErrorResponse with code `"invalid_request"`
- Error message describes which fields are missing

**ErrorResponse**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Missing required field: tenant_id",
    "details": {
      "missing_fields": ["tenant_id", "request_id", "task"]
    }
  },
  "context": {
    "request_id": "<generated-if-missing>",
    "trace_id": "<optional>"
  }
}
```

**HTTP Status**: `400 Bad Request`

**NATS ErrorResponse**: Code `"invalid_request"`

**Router Logic**: `router_nats_subscriber.erl` → `handle_decide_request/2` → Request validation → `build_error_response/3`

**Test**: `test_invalid_request_missing_fields/1`

#### 2. Invalid Request - Wrong Version

**Scenario**: DecideRequest has unsupported version (e.g., `version: "2"` instead of `"1"`).

**Router Behavior**:
- Router checks version during request parsing
- Generates ErrorResponse with code `"invalid_request"`
- Error message indicates unsupported version

**ErrorResponse**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Unsupported version: 2",
    "details": {
      "version": "2",
      "supported_versions": ["1"]
    }
  },
  "context": {
    "request_id": "req-version-test",
    "trace_id": "<optional>"
  }
}
```

**HTTP Status**: `400 Bad Request`

**NATS ErrorResponse**: Code `"invalid_request"`

**Router Logic**: `router_nats_subscriber.erl` → `handle_decide_request/2` → Version check → `build_error_response/3`

**Test**: `test_invalid_request_wrong_version/1`

#### 3. Tenant Rejected

**Scenario**: DecideRequest has `tenant_id` that fails tenant validation (CP2+ feature).

**Router Behavior**:
- Router validates tenant_id against allowlist/policy registry (if tenant validation enabled)
- Generates ErrorResponse with code `"unauthorized"` or `"invalid_request"`
- Error message indicates tenant validation failure

**ErrorResponse**:
```json
{
  "ok": false,
  "error": {
    "code": "unauthorized",
    "message": "Tenant validation failed: tenant not in allowlist",
    "details": {
      "tenant_id": "rejected-tenant",
      "reason": "tenant_not_in_allowlist"
    }
  },
  "context": {
    "request_id": "req-tenant-test",
    "trace_id": "<optional>"
  }
}
```

**HTTP Status**: `401 Unauthorized` or `400 Bad Request`

**NATS ErrorResponse**: Code `"unauthorized"` or `"invalid_request"`

**Router Logic**: `router_nats_subscriber.erl` → Tenant validation (if enabled) → `build_error_response/3`

**Note**: In CP1 baseline, tenant validation is disabled by default (CP2+ feature). This test verifies the contract structure.

**Test**: `test_tenant_rejected/1`

#### 4. Internal Router Error

**Scenario**: Router encounters an internal error (e.g., no provider available, policy not found, routing failure).

**Router Behavior**:
- Router detects internal error during routing decision
- Generates ErrorResponse with code `"internal"`, `"decision_failed"`, or `"policy_not_found"`
- Error message describes the failure

**ErrorResponse Examples**:

**No Provider Available**:
```json
{
  "ok": false,
  "error": {
    "code": "decision_failed",
    "message": "No provider available for routing",
    "details": {
      "policy_id": "non-existent-policy",
      "reason": "no_provider_available"
    }
  },
  "context": {
    "request_id": "req-internal-test",
    "trace_id": "<optional>"
  }
}
```

**Policy Not Found**:
```json
{
  "ok": false,
  "error": {
    "code": "policy_not_found",
    "message": "Policy not found: non-existent-policy",
    "details": {
      "policy_id": "non-existent-policy"
    }
  },
  "context": {
    "request_id": "req-internal-test",
    "trace_id": "<optional>"
  }
}
```

**Generic Internal Error**:
```json
{
  "ok": false,
  "error": {
    "code": "internal",
    "message": "Internal router error",
    "details": {}
  },
  "context": {
    "request_id": "req-internal-test",
    "trace_id": "<optional>"
  }
}
```

**HTTP Status**: `500 Internal Server Error`

**NATS ErrorResponse**: Code `"internal"`, `"decision_failed"`, or `"policy_not_found"`

**Router Logic**: `router_nats_subscriber.erl` → `handle_decide_request/2` → Routing decision → Error handling → `build_error_response/3`

**Error Code Mapping** (from `router_nats_subscriber.erl`):
- `no_provider_available` → `"decision_failed"`
- `policy_not_found` → `"policy_not_found"`
- Other errors → `"internal"`

**Test**: `test_internal_router_error/1`

### Error Code Mapping

**Router Error Codes** (from `router_nats_subscriber.erl`):
- `missing_tenant_id` → `"invalid_request"`
- `policy_not_found` → `"policy_not_found"`
- `no_provider_available` → `"decision_failed"`
- `invalid_policy` → `"invalid_request"`
- Other errors → `"internal"`

**HTTP Status Mapping** (Gateway responsibility):
- `"invalid_request"` → `400 Bad Request`
- `"unauthorized"` → `401 Unauthorized`
- `"policy_not_found"` → `404 Not Found`
- `"decision_failed"` → `500 Internal Server Error`
- `"internal"` → `500 Internal Server Error`

### Error Response Structure

All error responses follow the same structure (from `API_CONTRACTS.md`):

```json
{
  "ok": false,
  "error": {
    "code": "invalid_request|unauthorized|policy_not_found|decision_failed|internal",
    "message": "<string>",
    "details": { "any": "map" }
  },
  "context": {
    "request_id": "<uuid>",
    "trace_id": "<string>"
  }
}
```

**Required Fields**:
- `ok`: Always `false` for errors
- `error.code`: One of the standardized error codes
- `error.message`: Human-readable error message
- `context.request_id`: Request ID for correlation

**Optional Fields**:
- `error.details`: Additional error context (may contain sensitive data, filtered by Router)
- `context.trace_id`: Trace ID for distributed tracing

## Integration with CI

### Optional CI Stage

Add to `.github/workflows/ci.yml`:

```yaml
  gateway-router-contract-smoke:
    name: Gateway ↔ Router Contract Smoke
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Setup Erlang
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'
      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '20'
      - name: Run Contract Smoke Test
        run: ./scripts/gateway_router_contract_smoke.sh --router-only
```

### Local Development

Run before committing changes to Gateway or Router:

```bash
# Quick check (Router only)
./scripts/gateway_router_contract_smoke.sh --router-only

# Full check (if Gateway tests available)
./scripts/gateway_router_contract_smoke.sh --full
```

## Future Enhancements

1. **Full E2E Test**: Implement full E2E test that:
   - Starts NATS server (or uses mock)
   - Starts Router in mock mode
   - Starts Gateway (or uses test client)
   - Sends DecideRequest via NATS
   - Verifies DecideResponse structure and headers

2. **Gateway Contract Test**: Add Gateway-side contract test that:
   - Verifies DecideRequest structure before sending
   - Verifies DecideResponse structure after receiving
   - Checks headers are correctly set

3. **Contract Schema Validation**: Add JSON Schema validation for:
   - DecideRequest
   - DecideResponse
   - ErrorResponse

## Runbook: Contract Violation Alerts

### Alert: RouterNATSContractViolations

**When this alert fires**: Router detected contract violations (rate > 0.1 violations/sec for 5 minutes).

**Steps**:
1. **Check violation details**:
   ```bash
   # Query Prometheus or check Router logs
   rate(router_nats_contract_violations_total[5m]) by (subject, violation_count)
   ```

2. **Identify violation type**:
   - Check alert labels: `subject`, `violation_count`
   - Review Router logs for specific violation messages
   - Common violations: missing headers, wrong format, invalid version

3. **Check client-side issues**:
   - Verify Gateway is sending correct headers
   - Check NATS message format matches contract
   - Review `PROTO_NATS_MAPPING.md` for expected format

4. **Fix client-side issues**:
   - Update Gateway to send correct headers
   - Fix message format if incorrect
   - Verify version matches contract

**Related Documentation**:
- `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md#error-contract-scenarios` - Error contract scenarios
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Contract mapping

### Alert: RouterHighParseFailureRate

**When this alert fires**: Router parse failure rate > 5% for 5 minutes.

**Steps**:
1. **Check parse failure metrics**:
   ```bash
   rate(router_results_parse_failed_total[5m]) / rate(router_results_total[5m])
   ```

2. **Identify malformed messages**:
   - Check Router logs for parse errors
   - Review message payload format
   - Check if JSON is valid

3. **Fix message format**:
   - Verify messages match `API_CONTRACTS.md` format
   - Check JSON encoding/decoding
   - Verify required fields are present

**Related Documentation**:
- `docs/archive/dev/GATEWAY_ROUTER_CONTRACT_SMOKE.md#error-contract-scenarios` - Error scenarios
- `docs/API_CONTRACTS.md` - API contract definitions

## References

- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`: Protobuf to NATS mapping
- `docs/API_CONTRACTS.md`: API contracts and message formats
- `docs/NATS_SUBJECTS.md`: NATS subjects and headers
- `docs/archive/dev/ROUTER_CONTRACT_SNAPSHOT.md`: Static contract snapshot check
- `apps/otp/router/test/router_gateway_contract_smoke_SUITE.erl`: Router contract test
- `scripts/gateway_router_contract_smoke.sh`: E2E smoke script
- `scripts/router_contract_snapshot.sh`: Static contract snapshot script

