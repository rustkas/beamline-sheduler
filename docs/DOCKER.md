# Docker Guide

This guide provides detailed information about Docker setup for BeamLine Constructor.

## Dockerfile Structure

### Multi-Stage Builds

All Dockerfiles use multi-stage builds for optimization:

1. **Build Stage**: Compiles and builds the application
2. **Runtime Stage**: Minimal image with only runtime dependencies

### Service Dockerfiles

#### Erlang/OTP Router (`apps/otp/router/Dockerfile`)

- **Base Image**: `erlang:26-alpine`
- **Build Tools**: rebar3, git, curl, build-base
- **Runtime**: Minimal Erlang runtime with required libraries
- **Port**: 9000 (gRPC)
- **Health Check**: Process check via `pgrep`

#### NestJS Gateway (`apps/gateway/Dockerfile`)

- **Base Image**: `node:20-alpine`
- **Package Manager**: pnpm 9.15.9
- **Port**: 3000 (HTTP)
- **Health Check**: HTTP endpoint `/health`

## docker-compose.yml

### Services

1. **postgres**: PostgreSQL 15 database
2. **nats**: NATS message broker
3. **router**: Erlang/OTP router service
4. **gateway**: NestJS Gateway API

### Networking

All services are connected via `beamline-network` bridge network.

### Volumes

- **postgres_data**: Persistent PostgreSQL data
- **Code mounts**: Development code mounted as read-only

### Health Checks

All services include health checks:

```yaml
healthcheck:
  test: ["CMD", "health-check-command"]
  interval: 30s
  timeout: 3s
  retries: 3
```

### Dependencies

Services start in order based on `depends_on`:

1. PostgreSQL and NATS start first
2. Router waits for PostgreSQL and NATS
3. Gateway waits for all dependencies

## Building Images

### Build All Services

```bash
docker-compose build
```

### Build Specific Service

```bash
docker-compose build gateway
```

### Build Without Cache

```bash
docker-compose build --no-cache
```

### Build with Progress

```bash
docker-compose build --progress=plain
```

## Image Management

### List Images

```bash
docker images | grep beamline
```

### Remove Images

```bash
# Remove specific image
docker rmi beamline-gateway

# Remove all beamline images
docker rmi $(docker images -q beamline*)
```

### Image Sizes

Check image sizes:

```bash
docker images --format "table {{.Repository}}\t{{.Tag}}\t{{.Size}}"
```

## Container Management

### List Containers

```bash
# Running containers
docker-compose ps

# All containers
docker-compose ps -a
```

### Container Logs

```bash
# Follow logs
docker-compose logs -f

# Last 100 lines
docker-compose logs --tail=100

# Specific service
docker-compose logs gateway
```

### Execute Commands in Container

```bash
# Execute shell
docker-compose exec gateway sh

# Execute command
docker-compose exec postgres psql -U beamline -d beamline
```

### Container Stats

```bash
docker stats
```

## Volume Management

### List Volumes

```bash
docker volume ls | grep beamline
```

### Inspect Volume

```bash
docker volume inspect beamline_postgres_data
```

### Backup Volume

```bash
docker run --rm -v beamline_postgres_data:/data -v $(pwd):/backup alpine tar czf /backup/postgres_backup.tar.gz /data
```

### Restore Volume

```bash
docker run --rm -v beamline_postgres_data:/data -v $(pwd):/backup alpine tar xzf /backup/postgres_backup.tar.gz -C /
```

## Network Management

### List Networks

```bash
docker network ls | grep beamline
```

### Inspect Network

```bash
docker network inspect beamline_beamline-network
```

### Connect to Network

```bash
docker network connect beamline_beamline-network <container-name>
```

## Development vs Production

### Development

- Code mounted as volumes for hot reload
- Debug logging enabled
- Development dependencies included

### Production

- Code copied into image
- Optimized builds
- Minimal runtime images
- Production dependencies only

## Best Practices

### Layer Caching

Dockerfiles are optimized for layer caching:
- Dependencies installed before code copy
- Code changes don't invalidate dependency layers

### Security

- Non-root users in all containers
- Minimal base images (Alpine)
- No secrets in images
- Health checks for all services

### Performance

- Multi-stage builds reduce image size
- Layer caching speeds up builds
- Health checks ensure service readiness

## Troubleshooting

### Build Failures

1. Check Dockerfile syntax
2. Verify base images are available
3. Check build logs for errors

### Container Won't Start

1. Check container logs
2. Verify health checks
3. Check resource limits

### Volume Issues

1. Verify volume permissions
2. Check volume mount paths
3. Inspect volume contents

## References

- [Docker Documentation](https://docs.docker.com/)
- [docker-compose Documentation](https://docs.docker.com/compose/)
- [Multi-stage Builds](https://docs.docker.com/build/building/multi-stage/)


