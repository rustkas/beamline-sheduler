# Privacy Policy

**Version**: 1.0  
**Last Updated**: 2025-11-06  
**Author**: WORKER wrk-12 - Compliance & Governance

## Purpose

This policy defines data privacy requirements for the BeamLine Constructor project, including PII handling, logging policies, and data retention.

## PII Categories

### Personal Identifiers

- Email addresses
- Phone numbers
- Social Security Numbers (SSN)
- Passport numbers
- Driver license numbers

### Authentication Data

- Passwords
- API keys
- Tokens
- Session IDs (not PII by itself)

### Financial Data

- Credit card numbers
- Bank account numbers
- Payment information

### Location Data

- Full IP addresses (anonymize last octet)
- GPS coordinates
- Street addresses

### Biometric Data

- Fingerprints
- Face recognition data
- Voice samples

## Logging Policy (No PII)

### Prohibited in Logs

**CRITICAL**: The following PII must **NEVER** be logged:

- Email addresses (must be redacted or hashed)
- Phone numbers (must be redacted)
- SSN, credit card numbers (must be redacted)
- Passwords, API keys, tokens (must be redacted)
- Full IP addresses (anonymize: `192.168.1.***`)
- GPS coordinates, street addresses (must be redacted)

### Allowed in Logs

The following data **can be logged** (not considered PII):

- Tenant IDs (internal identifiers)
- Trace IDs, Request IDs (internal identifiers)
- Timestamps (ISO 8601)
- Error messages (technical, without user data)
- Component names, HTTP status codes
- Latency metrics
- Anonymized IP addresses (last octet redacted)

### Logging Implementation

All logging must use PII filtering:

**Erlang/OTP**:
```erlang
SanitizedContext = router_logger:sanitize_context(Context),
router_logger:info(Message, SanitizedContext).
```

**Node.js/TypeScript**:
```typescript
const sanitized = sanitizeContext(context);
logger.info(message, sanitized);
```

## Data Retention

### Retention Rules

**CRITICAL**: All logs must comply with retention policies. Local development logs must not exceed retention limits.

- **Production logs**: 90 days maximum
- **Development logs**: 30 days maximum (local development)
- **Audit logs**: 7 years (compliance requirement)
- **Error logs**: 180 days maximum
- **Telemetry data**: 365 days maximum

### Local Development Retention

**For local development environments**:
- **Local logs**: Not more than 30 days
- **Local telemetry**: Not more than 7 days
- **Local audit trails**: Not more than 90 days

**Enforcement**:
- Automated cleanup scripts should enforce retention limits
- Manual cleanup required if automated scripts are not available
- Log rotation must be configured to respect retention policies

## Telemetry Policy

### Collected Data (Allowed)

- Feature usage statistics (anonymized)
- Performance metrics
- Error rates and types
- System health metrics
- Component interaction patterns

### Prohibited Data

- User personal information
- Content of user requests
- User behavior patterns (individual)
- Location data
- Device identifiers

### Opt-Out Mechanism

Users can disable telemetry via:
- Environment variable: `BEAMLINE_DISABLE_TELEMETRY=true`
- Configuration file: `telemetry.enabled: false`

## Storage Rules

- **Prohibited in logs**: Yes
- **Prohibited in configs**: Yes
- **Prohibited in code**: Yes
- **Allowed in secrets manager**: Yes
- **Encryption required**: Yes
- **Access control required**: Yes

## Transfer Rules

- **Encryption required**: Yes
- **Secure channels only**: Yes
- **Audit trail required**: Yes
- **Consent required**: No (for system telemetry)

## References

- **Compliance Documentation**: `docs/COMPLIANCE_GOVERNANCE.md`
- **Observability Conventions**: `docs/OBSERVABILITY_CONVENTIONS.md`
- **Security Policy**: See compliance validators

