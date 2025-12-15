# Observability Development Tools (MVP)

**Version**: 1.0 (MVP)  
**Last Updated**: 2025-01-27  
**Author**: Agent 10 - Observability/Telemetry

## Purpose

This directory contains tools and documentation for minimal observability (MVP): structured JSON logging and health endpoints.

**Note**: Prometheus, Alertmanager, Grafana, Loki, and Tempo are excluded at MVP stage. These will be added in future iterations.

## Configuration Files

### Logging Configuration

**File**: `config/observability/logging.json`

Defines the structured JSON log format for all components:

- **Required fields**: `timestamp`, `level`, `component`, `message`
- **Optional fields**: `context`, `tenant_id`, `trace_id`, `error`
- **Log levels**: ERROR, WARN, INFO, DEBUG
- **PII filtering**: Automatic redaction of sensitive fields
- **Health endpoint**: Definition of `/_health` endpoint format

## Quick Start

### Prerequisites

- Bash (Linux/macOS/WSL) or PowerShell (Windows)
- jq or python3 (for JSON validation)
- curl or wget (for health endpoint checks, optional)

### Validation

Run the observability validation script:

**Linux/macOS/WSL:**
```bash
bash scripts/observability/validate_observability.sh
```

**Windows (PowerShell):**
```powershell
powershell -File scripts/observability/validate_observability.ps1
```

The script checks:
- ✅ JSON log format configuration (`config/observability/logging.json`)
- ✅ Required fields (timestamp, level, component, message)
- ✅ Log levels (ERROR, WARN, INFO, DEBUG)
- ✅ Health endpoint definition
- ✅ JSON syntax validation
- ✅ Health endpoint availability (if services are running)
- ✅ No real secrets (only placeholders or examples)

**Exit Codes:**
- `0` - Success (may have warnings)
- `2` - External endpoints unavailable (services not running - expected in local validation)
- `3` - Local configs missing or invalid

**Logs:**
Validation results are logged to `reports/dry-run-logs/observability/validation.log` with timestamps and masked secrets.

## Log Format

### Required Fields

All logs must include:
- `timestamp` (ISO 8601, UTC)
- `level` (ERROR, WARN, INFO, DEBUG)
- `component` (router, provider, ingress, gateway, usage)
- `message` (human-readable text)

### Example Logs

**INFO level:**
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "INFO",
  "component": "router",
  "message": "Request processed",
  "context": {
    "provider": "openai",
    "latency_ms": 250
  }
}
```

**ERROR level:**
```json
{
  "timestamp": "2025-01-27T12:00:00Z",
  "level": "ERROR",
  "component": "router",
  "message": "Failed to load policy",
  "error": {
    "type": "PolicyLoadError",
    "message": "Policy file not found"
  }
}
```

## Health Endpoints

All components must provide `GET /_health` endpoint:

**Format:**
```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z",
  "checks": {
    "database": {
      "status": "ok",
      "message": "Connection successful"
    }
  }
}
```

**Status values:**
- `healthy` - Service operating normally
- `degraded` - Service operating with limitations
- `unhealthy` - Service unavailable or critically broken

**Component endpoints:**
- Router: `http://localhost:8080/_health`
- Provider: `http://localhost:8081/_health`
- Ingress: `http://localhost:8082/_health`
- Gateway: `http://localhost:3000/_health`
- Usage: `http://localhost:8083/_health`

**Note**: Real ports will be configured by Agent 11 in docker-compose.

## PII/Secret Filtering

All components must automatically filter sensitive data before logging.

**Filtered fields:**
- `password`, `api_key`, `secret`, `token`, `access_token`, `refresh_token`
- `authorization`, `credit_card`, `ssn`, `email`, `phone`

**Replacement:** `[REDACTED]`

## Integration with Other Agents

### Agent 11 (DevOps/Infrastructure)

- **Health Endpoints**: Agent 11 will configure real service ports in docker-compose
- **Log Aggregation**: Agent 11 may set up log collection (future iteration)

### Agent 5 (CI/CD)

- **Dry-Run**: Validation script is called in `scripts/dry_run_ci.sh`
- **Logs**: Results logged to `reports/dry-run-logs/observability/validation.log`
- **Exit Codes**: Used for CI/CD gates

### Agent 6 (Security & Secrets)

- **Secret Detection**: Validation script checks for real secrets
- **Masking**: All token-like strings are masked in logs
- **PII Filtering**: All components must implement PII filtering

### Agent 2/4/5 (Architecture/Implementation)

- **Log Format**: See `config/observability/logging.json` for required log format
- **Health Endpoint**: All components must implement `/_health` endpoint
- **PII Filtering**: All components must filter sensitive data

## Future Iterations

Future iterations will add:

1. **Metrics (Prometheus)**: Export metrics via `/metrics` endpoint
2. **Tracing (OpenTelemetry)**: Distributed tracing with context propagation
3. **Log Aggregation (Loki)**: Centralized log collection and storage
4. **Alerting (Alertmanager)**: Alert rules and notifications
5. **Dashboards (Grafana)**: Visualization and monitoring

## References

- `docs/OBSERVABILITY.md` - Original specification (source of truth)
- `config/observability/logging.json` - Log format schema
- `scripts/observability/` - Validation scripts
- `reports/dry-run-logs/observability/` - Validation logs
