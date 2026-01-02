# Local Development Setup

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Author**: wrk-11 - DevOps/Infrastructure

## Quick Start

### Prerequisites

- Docker and Docker Compose installed
- Git repository cloned

### Setup

1. **Copy environment template**:
   ```bash
   cp config/env/ENV_TEMPLATE.md .env
   # Or manually create .env from template content
   ```

2. **Start services**:
   ```bash
   ./scripts/local/up.sh
   ```

   Or manually:
   ```bash
   docker compose up -d
   ```

### Verify Services

```bash
# Check service status
docker compose ps

# Check C-Gateway health
curl http://localhost:8081/_health

# Check NATS health
curl http://localhost:8222/healthz
```

## Services

### C-Gateway (C11)

- **Role**: HTTP entrypoint (HTTP → NATS → Router)
- **Port**: `8081` (external, default) → `8080` (internal)
- **Health**: `http://localhost:8081/_health`
- **API Base**: `http://localhost:8081`

### NATS

- **Client Port**: `4222`
- **Monitoring Port**: `8222`
- **Health**: `http://localhost:8222/healthz`

### Router (Erlang/OTP)

- **Port**: `9000` (gRPC)
- **Status**: Included in local stack

## Management Commands

### Start Services

```bash
./scripts/local/up.sh
```

Or:
```bash
docker compose up -d
```

### Stop Services

```bash
./scripts/local/down.sh
```

Or:
```bash
docker compose down
```

### Build Services

```bash
./scripts/local/build.sh
```

Or:
```bash
docker compose build
```

### View Logs

```bash
# All services
./scripts/local/logs.sh

# Specific service
./scripts/local/logs.sh c-gateway
./scripts/local/logs.sh nats
```

Or:
```bash
docker compose logs -f
docker compose logs -f c-gateway
```

## Environment Variables

See `config/env/ENV_TEMPLATE.md` for all available environment variables.

Key variables:
- `NATS_URL`: NATS connection URL (default: `nats://nats:4222`)
- `NATS_TIMEOUT_MS`: NATS request timeout (default: `5000`)
- `C_GATEWAY_HOST_PORT`: External C-Gateway port (default: `8081`)
- `C_GATEWAY_PORT`: Internal C-Gateway port (default: `8080`)

## Testing

### Test C-Gateway API

```bash
# Health check
curl http://localhost:8081/_health

# API endpoint (if implemented)
curl -X POST http://localhost:8081/api/v1/routes/decide \
  -H "Content-Type: application/json" \
  -d '{"message": "test"}'
```

### Test NATS Connection

```bash
# Using nats CLI (if installed)
nats pub test.subject "Hello, NATS!"
nats sub test.subject
```

## Troubleshooting

### Services Not Starting

1. Check Docker is running:
   ```bash
   docker ps
   ```

2. Check ports are available:
   ```bash
   # Check if ports are in use
   lsof -i :8080
   lsof -i :4222
   ```

3. View service logs:
   ```bash
   docker compose logs c-gateway
   docker compose logs nats
   ```

### C-Gateway Not Responding

1. Check Gateway is healthy:
   ```bash
   curl http://localhost:8081/_health
   ```

2. Check Gateway logs:
   ```bash
   docker compose logs -f c-gateway
   ```

3. Verify NATS connection:
   ```bash
   docker compose logs c-gateway | grep -i nats
   ```

### NATS Connection Issues

1. Check NATS is running:
   ```bash
   curl http://localhost:8222/healthz
   ```

2. Check NATS logs:
   ```bash
   docker compose logs -f nats
   ```

3. Verify NATS URL in Gateway:
   ```bash
   docker compose exec c-gateway env | grep NATS
   ```

## Next Steps

- See `docs/archive/dev/INFRA_DEVEX.md` for detailed infrastructure documentation
- See `docs/NATS_SUBJECTS.md` for NATS subject definitions
- See `docs/ARCHITECTURE/api-registry.md` for API documentation
