# Infrastructure Developer Experience Guide

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Author**: wrk-11 - DevOps/Infrastructure

## Purpose

Simple developer experience guide for building, running, and stopping BeamLine Constructor infrastructure locally.

## Quick Start

### Prerequisites

- Docker and Docker Compose
- Environment file (`.env`)

### Setup

1. **Copy environment template**:
   ```bash
   cp config/env/.env.example .env
   # Or manually create .env from template content
   ```

2. **Edit `.env`** (replace `PLACEHOLDER_*` values):
   ```bash
   # Required changes:
   DB_PASSWORD=your_actual_password
   ```

## Dev-Run Scenarios

### Setup Environment

1. **Create `.env` file** from template:
   ```bash
   # Copy template content to .env
   cat config/env/ENV_TEMPLATE.md
   # Edit .env with your values (use dev_password for local dev)
   ```

2. **Verify Docker and Docker Compose**:
   ```bash
   docker --version
   docker compose version
   ```

### Build Services

```bash
docker-compose -f infra/compose/local-dev.yml build
```

Builds:
- PostgreSQL (official image)
- NATS (official image)
- Router (Erlang/OTP gRPC)
- Gateway (NestJS REST/SSE)

### Start Services

```bash
docker-compose -f infra/compose/local-dev.yml up -d
```

Starts services in order:
1. PostgreSQL (port 5432) - waits for health check
2. NATS (ports 4222, 8222) - waits for health check
3. Router (port 9000 gRPC) - waits for PostgreSQL and NATS
4. Gateway (port 8080 HTTP) - waits for Router, PostgreSQL, and NATS

### Verify Services

```bash
# Check all services are running
docker-compose -f infra/compose/local-dev.yml ps

# Run verification script
bash scripts/infra/verify-services.sh
```

Expected services:
- PostgreSQL on port 5432
- NATS on ports 4222, 8222
- Router on port 9000 (gRPC)
- Gateway on port 8080 (HTTP)

### Check Status

```bash
# All services
docker-compose -f infra/compose/local-dev.yml ps

# Health status
docker-compose -f infra/compose/local-dev.yml ps --format json | jq '.[] | {name: .Name, status: .Health}'
```

### View Logs

```bash
# All services
docker-compose -f infra/compose/local-dev.yml logs -f

# Specific service
docker-compose -f infra/compose/local-dev.yml logs -f postgres
docker-compose -f infra/compose/local-dev.yml logs -f nats
docker-compose -f infra/compose/local-dev.yml logs -f router
docker-compose -f infra/compose/local-dev.yml logs -f gateway
```

### Test Endpoints

```bash
# Gateway health
curl http://localhost:8080/_health

# Gateway routes endpoint
curl -X POST http://localhost:8080/api/v1/routes/decide \
  -H "Content-Type: application/json" \
  -d '{"tenant_id": "00000000-0000-0000-0000-000000000001", "message": "test"}'

# NATS monitoring
curl http://localhost:8222/healthz

# PostgreSQL connection
docker exec beamline-postgres psql -U beamline -d beamline -c "SELECT COUNT(*) FROM beamline.projects;"
```

### Stop Services

```bash
docker-compose -f infra/compose/local-dev.yml down
```

### Stop and Remove Volumes

```bash
docker-compose -f infra/compose/local-dev.yml down -v
```

## Service URLs

Once services are running:

- **Router gRPC**: `localhost:9000`
- **Ingress HTTP**: `http://localhost:8080`

## Validation

### Validate Infrastructure

**Windows PowerShell**:
```powershell
powershell -File scripts/infra/validate_infra.ps1
```

**Linux/macOS/WSL**:
```bash
bash scripts/infra/validate_infra.sh
```

**Exit codes**:
- `0`: All checks passed
- `2`: External errors (Docker not available)
- `3`: Local problems (missing files, syntax errors, secrets detected)

**Note**: WARN messages do not cause exit code 2, only ERROR messages cause exit code 3.

**Logs**: `reports/dry-run-logs/infra/validation.log`

### Validate Compose Syntax

```bash
docker compose -f infra/compose/local-dev.yml config
```

## Troubleshooting

### Services Won't Start

1. Check Docker is running:
   ```bash
   docker info
   ```

2. Check for port conflicts:
   ```bash
   lsof -i :9000
   lsof -i :8080
   ```

3. Check service logs:
   ```bash
   docker-compose -f infra/compose/local-dev.yml logs
   ```

### Build Failures

1. Check Dockerfile syntax
2. Verify base images are available
3. Check build logs:
   ```bash
   docker-compose -f infra/compose/local-dev.yml build --no-cache
   ```

## References

- `infra/compose/local-dev.yml` - Docker Compose configuration
- `infra/docker/README.md` - Docker infrastructure documentation
- `config/env/.env.example` - Environment variables template

