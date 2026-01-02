# Environment Configuration Guide

This guide explains environment variables and configuration for BeamLine Constructor.

## Environment Files

### `.env.example`

Template file with all required environment variables and placeholders.

**IMPORTANT**: Copy to `.env` and replace placeholders with actual values.

### `.env`

Actual environment configuration (not committed to version control).

## Environment Variables

### Database Configuration

```bash
# Database name
DB_NAME=beamline

# Database user
DB_USER=beamline

# Database password (REQUIRED - replace placeholder)
DB_PASSWORD=YOUR_DB_PASSWORD_HERE

# Database port
DB_PORT=5432

# Full database URL
DATABASE_URL=postgresql://beamline:password@localhost:5432/beamline
```

### NATS Configuration

```bash
# NATS server URL
NATS_URL=nats://localhost:4222

# NATS port
NATS_PORT=4222

# NATS monitoring port
NATS_MONITOR_PORT=8222
```

### Gateway Configuration

```bash
# Gateway HTTP port
GATEWAY_PORT=3000

# Node.js environment
NODE_ENV=development
```

### Router (gRPC) Configuration

```bash
# gRPC port
GRPC_PORT=9000

# Router gRPC URL
ROUTER_GRPC_URL=router:9000

# Database connection pool size
DB_POOL_SIZE=10

# Policy cache TTL (seconds)
POLICY_CACHE_TTL_SECONDS=300
```

### Security Configuration

```bash
# HMAC secret for audit trail
# Minimum: 16 characters
# Recommended: 64 characters
# REQUIRED in production/CI, optional in development
# For local development, use config/env/.env.dev
# For production, set in CI/CD secrets
# NOTE: In examples, we use abstract mask "16+..." - never publish real secret prefixes
BEAMLINE_HMAC_SECRET=16+...

# API keys (placeholders only - no real secrets)
API_KEY=YOUR_API_KEY_HERE
```

**CRITICAL**: 
- Never commit real secrets to version control
- Use CI/CD secrets for production
- Minimum HMAC secret length: 16 characters
- Recommended HMAC secret length: 64 characters

### Observability Configuration

```bash
# Prometheus metrics port
PROMETHEUS_PORT=9090

# OpenTelemetry endpoint
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317
```

### Development Configuration

```bash
# Enable debug logging
DEBUG=false

# Enable hot reload (development only)
HOT_RELOAD=true
```

## Configuration Profiles

### Development

```bash
NODE_ENV=development
DEBUG=true
HOT_RELOAD=true
```

### Staging

```bash
NODE_ENV=staging
DEBUG=false
HOT_RELOAD=false
```

### Production

```bash
NODE_ENV=production
DEBUG=false
HOT_RELOAD=false
```

## Service-Specific Configuration

### Erlang/OTP Router

Configuration via environment variables or `sys.config`:

```erlang
[
  {beamline_router, [
    {grpc_port, list_to_integer(os:getenv("GRPC_PORT", "9000"))},
    {nats_url, os:getenv("NATS_URL", "nats://localhost:4222")},
    {db_host, os:getenv("DB_HOST", "localhost")},
    {db_port, list_to_integer(os:getenv("DB_PORT", "5432"))},
    {db_name, os:getenv("DB_NAME", "beamline")},
    {db_user, os:getenv("DB_USER", "beamline")},
    {db_pool_size, list_to_integer(os:getenv("DB_POOL_SIZE", "10"))}
  ]}
].
```

### NestJS Gateway

**Package Manager**: Gateway uses `npm` (package-lock.json). Tests/workspace use `pnpm` for test harness.

Configuration via `@nestjs/config`:

```typescript
// Loads from .env file automatically
ConfigModule.forRoot({
  isGlobal: true,
  envFilePath: ['.env.local', '.env'],
})
```

Access in code:

```typescript
const port = process.env.PORT || 3000;
const dbUrl = process.env.DATABASE_URL;
```

**Installation**:

```bash
cd apps/gateway
npm ci          # Install dependencies (CI/production)
npm install     # Install dependencies (development)
npm run build   # Build for production
npm test        # Run tests
```

## Validation

### Validate Environment

```bash
bash scripts/validate-env.sh
```

Checks:
- Required variables are set
- No placeholder values remain
- Docker is available
- Configuration files exist

### Check Placeholders

```bash
# Find remaining placeholders
grep -r "YOUR_.*_HERE" .env
```

## Security Best Practices

### Secret Management

1. **Never commit secrets** to version control
2. **Use placeholders** in `.env.example`
3. **Store real secrets** in CI/CD variables
4. **Rotate secrets** regularly
5. **Use strong secrets** (min 16 chars, recommended 64)

### HMAC Secret

- **Purpose**: Audit trail integrity
- **Format**: Hex string
- **Length**: Minimum 16, recommended 64
- **Storage**: CI/CD secrets only

### Database Password

- **Purpose**: Database authentication
- **Format**: Alphanumeric + special chars
- **Length**: Minimum 12 characters
- **Storage**: CI/CD secrets or secure vault

## Environment-Specific Settings

### Local Development

- Use simple passwords (not for production)
- Enable debug logging
- Enable hot reload
- Use local services

### CI/CD

- Use CI/CD secrets
- Disable debug logging
- Use test databases
- Isolated environments

### Production

- Strong passwords required
- No debug logging
- No hot reload
- Secure service endpoints

## Troubleshooting

### Missing Variables

If a service fails with "undefined" errors:

1. Check `.env` file exists
2. Verify variable names match
3. Run validation script
4. Check service logs

### Placeholder Values

If services fail with placeholder values:

1. Check `.env` for placeholders
2. Replace all `YOUR_*_HERE` values
3. Restart services
4. Re-run validation

### Connection Issues

If services can't connect:

1. Verify service URLs in `.env`
2. Check service health
3. Verify network connectivity
4. Check firewall rules

## References

- [dev/LOCAL_SETUP.md](archive/dev/LOCAL_SETUP.md) - Local setup instructions
- [DOCKER.md](DOCKER.md) - Docker configuration
- [README.md](../README.md) - Project overview

