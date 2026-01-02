# Security Guide (Local Development)

Practices for masking secrets and keeping artifacts clean during local development and CI dry-run.

## Secrets
- Do not use real secrets; use placeholders in examples
- Mask secrets in logs and reports (`[REDACTED]` or `[MASKED]`)
- Provide validators that detect and redact tokens automatically
- All headers and payload fields with potential secrets are automatically filtered before logging

## Confidentiality
- Do not include PII or sensitive data in repository
- Validators check for patterns and fail with guidance
- All logging goes through `router_logger` which automatically filters PII fields
- NATS message headers and payloads are filtered for secrets before logging

## Repository Checks
- Link checker for `docs/` and repo root
- Docs validator ensures headings, code block languages, quotes, spacing

## Validator Behavior
- Sensitive values are redacted in outputs
- Non-zero exit codes indicate failures with actionable messages

## Examples
- Good:
```
API_TOKEN=tr_*****_example
```
- Bad:
```
API_TOKEN=real-production-secret-value
```

## PII Filtering in Logs

All logging through `router_logger` automatically filters the following fields:
- `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`
- `authorization` (case-insensitive, includes `Authorization`, `AUTHORIZATION`)
- `bearer`, `Bearer` (case-insensitive)
- `x-api-key`, `X-Api-Key` (case-insensitive)
- `x-auth-token`, `X-Auth-Token` (case-insensitive)
- `x-authorization`, `X-Authorization` (case-insensitive)
- `credit_card`, `ssn`, `email`, `phone`

**Replacement**: `[REDACTED]`

**Recursive filtering**: Nested maps are filtered recursively.

**Pattern matching**: Values containing secret patterns (e.g., "api_key=secret123") are also masked.

## NATS Headers and Payloads

NATS message headers and payloads are **not logged directly** in CP1 baseline. Only metadata is logged:
- Subject, error codes, assignment_id, request_id, trace_id, tenant_id
- Headers and payloads are **never logged in full** to prevent secret leakage

**Header Logging**:
- Only header **keys** are logged (not values) in contract violation logs
- Header values are **never logged** to prevent secret leakage
- Example: `header_keys: ["trace_id", "tenant_id", "version"]` (safe) vs `headers: {"authorization": "Bearer secret123"}` (forbidden)

If headers/payload need to be logged for debugging:
1. Extract only non-sensitive fields
2. Pass through `router_logger:filter_pii/1` before logging
3. Never log full headers/payload objects

## DevState/CP State Logging

**CRITICAL**: DevState/CP state failures are logged with **sanitized context** to prevent logging sensitive data from `state.json`.

**Safe Fields** (logged):
- `current_cp` (e.g., "CP1-LC")
- `no_drift` (boolean)
- `project` (project name)
- `version` (version string)
- `reason` (error reason)

**Forbidden Fields** (never logged):
- Entire `state.json` object
- Any fields not explicitly listed as safe
- Nested objects from state.json
- Any fields that may contain secrets or PII

**Implementation**:
- `router_state:log_fallback/2` extracts only safe fields via `extract_safe_state_fields/1`
- All context is filtered through `router_logger:filter_pii/1` before logging
- See `apps/otp/router/src/router_state.erl` for implementation details

## Error Context Sanitization

**CRITICAL**: Error contexts are sanitized before logging to prevent secret leakage.

**Sanitized Fields**:
- Error messages containing secret patterns (e.g., "api_key=secret123") → `[REDACTED: contains sensitive data]`
- Error contexts passed through `router_logger:filter_pii/1`
- Tenant validation error contexts filtered before logging

**Implementation**:
- `router_result_consumer:sanitize_error_for_logging/1` - sanitizes error messages
- `router_nats_subscriber:sanitize_error_for_logging/1` - sanitizes error messages
- All error contexts filtered through `router_logger:filter_pii/1` before logging

## Extensions Security

**For detailed Extensions security guide, see**:
- **`apps/otp/router/docs/EXTENSIONS_SECURITY_GUIDE.md`** - Complete security guide for Extensions ecosystem covering:
  - Authorization and RBAC for extension registry operations
  - Payload validation and DoS protection
  - Abuse prevention (retry limits, timeouts, pipeline depth)
  - Production deployment security checklist
  - Security testing guidelines

## Gateway + Router Abuse Scenarios

**For detailed abuse scenarios and protection mechanisms, see**:
- **`docs/ARCHITECTURE/gateway-router-abuse-scenarios.md`** - Complete abuse scenarios specification covering:
  - Empty payload flood attacks
  - Targeted tenant attacks
  - Rate limit evasion attempts
  - Heavy payload attacks
  - Multi-tenant flood attacks

### Abuse Detection Metrics

**Gateway Metrics**:
- `gateway_abuse_empty_payload_total`: Counter for empty payload abuse events
- `gateway_abuse_targeted_tenant_total`: Counter for targeted tenant attacks
- `gateway_abuse_rate_limit_evasion_total`: Counter for rate limit evasion attempts
- `gateway_abuse_heavy_payload_total`: Counter for heavy payload abuse events
- `gateway_abuse_multi_tenant_flood_total`: Counter for multi-tenant flood attacks
- `gateway_abuse_blocked_tenants`: Gauge for number of currently blocked tenants

**Router Metrics**:
- `router_abuse_empty_payload_total`: Counter for empty payload abuse events
- `router_abuse_heavy_payload_total`: Counter for heavy payload abuse events
- `router_abuse_targeted_tenant_total`: Counter for targeted tenant attacks
- `router_abuse_payload_size_distribution`: Histogram for payload size distribution per tenant

### Abuse Detection Alerts

**When alerts fire**:
1. **RouterAbuseEmptyPayloadHigh** / **GatewayAbuseEmptyPayloadHigh**:
   - **Meaning**: High rate of empty payload requests (> 20 events/min)
   - **Action**: Check logs for tenant_id, review payload validation thresholds
   - **Remediation**: Block offending tenant if confirmed abuse, adjust min_payload_size if false positive

2. **RouterAbuseHeavyPayloadHigh** / **GatewayAbuseHeavyPayloadHigh**:
   - **Meaning**: High rate of heavy payload abuse (> 5-10 events/min)
   - **Action**: Check payload size distribution, review tenant behavior
   - **Remediation**: Block tenant if confirmed abuse, adjust large_payload_threshold if false positive

3. **RouterAbuseTargetedTenantHigh** / **GatewayAbuseTargetedTenantHigh**:
   - **Meaning**: Targeted attack on specific tenant (> 5 events/min)
   - **Action**: **CRITICAL** - Review tenant logs immediately, check for DDoS
   - **Remediation**: Temporarily block tenant, contact tenant owner, review rate limits

4. **GatewayAbuseRateLimitEvasionHigh**:
   - **Meaning**: Multiple API keys or IPs detected for same tenant (> 3 events/min)
   - **Action**: Review tenant's API key usage, check for compromised keys
   - **Remediation**: Revoke suspicious API keys, temporarily block tenant

5. **GatewayAbuseMultiTenantFloodHigh**:
   - **Meaning**: Coordinated attack across multiple tenants (> 5 events/min)
   - **Action**: **CRITICAL** - Review global rate limits, check for coordinated attack
   - **Remediation**: Increase global rate limits, block suspicious IPs, contact security team

6. **GatewayAbuseBlockedTenantsHigh**:
   - **Meaning**: High number of blocked tenants (> 100)
   - **Action**: Review abuse detection thresholds, check for false positives
   - **Remediation**: Adjust thresholds if too strict, review blocked tenant list

**Interpreting Metrics**:
- **Normal**: All abuse metrics ≈ 0, no alerts firing
- **Warning**: Occasional abuse events (< threshold), alerts firing but not critical
- **Critical**: High rate of abuse events (> threshold), multiple alerts firing, requires immediate action

**Steps When Abuse Detected**:
1. **Check Metrics**: Review abuse metrics in Prometheus/Grafana
2. **Check Logs**: Review structured logs for abuse events (tenant_id, request_id, trace_id)
3. **Identify Pattern**: Determine abuse type (empty payload, heavy payload, targeted tenant, etc.)
4. **Verify Legitimacy**: Check if false positive (legitimate use case vs actual abuse)
5. **Take Action**: Block tenant, adjust thresholds, or contact tenant owner
6. **Monitor**: Continue monitoring metrics and logs after remediation
  - Flood attacks with valid but "empty" requests
  - Targeted attacks on specific tenants
  - Rate limit evasion attempts (multiple API keys, IP rotation)
  - Heavy payload attacks (large payload_ref / blobs)
  - Multi-tenant flood (distributed attack)
  - Detection layers (Gateway rate limiting / Router intake validation)
  - Logging and alerting requirements
  - Test scenarios and implementation plan

**Key Protection Mechanisms**:
- **Gateway**: Per-endpoint rate limiting, per-tenant limits, global limits, aggregate tracking
- **Router**: Payload size validation (1MB limit), payload content validation (empty payload detection), tenant validation, payload size distribution tracking

**Operational Runbooks**:
- **`docs/OPS_RUNBOOK_ROUTER_INTAKE.md`** - Router intake troubleshooting (includes abuse detection)
- **`docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`** - Gateway rate limiting operations (includes abuse detection)

## References
- `docs/archive/dev/DOCS_DEVEX_SETUP.md`  link checker and DevEx scripts
- `docs/TYPOGRAPHY_STYLE.md`  documentation style rules
- `apps/otp/router/docs/EXTENSIONS_SECURITY_GUIDE.md` - Extensions security guide
- `docs/ARCHITECTURE/gateway-router-abuse-scenarios.md` - Gateway + Router abuse scenarios specification
- `docs/OPS_RUNBOOK_ROUTER_INTAKE.md` - Router intake operations runbook
- `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md` - Gateway rate limiting operations runbook
