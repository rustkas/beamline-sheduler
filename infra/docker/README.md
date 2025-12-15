# Docker Infrastructure

**Purpose**: Minimal Docker configuration for Beamline Constructor services.

## Structure

```
infra/docker/
├── router/
│   └── Dockerfile    # Erlang/OTP router
└── ingress/
    └── Dockerfile    # C++ CAF ingress
```

## Dockerfiles

### Router (`router/Dockerfile`)

- **Base**: `erlang:26-alpine`
- **Port**: 9000 (gRPC)
- **Status**: Stub (to be implemented)

### Ingress (`ingress/Dockerfile`)

- **Base**: `alpine:latest`
- **Port**: 8080 (HTTP)
- **Status**: Stub (to be implemented)

## Usage

Build and run with docker-compose:

```bash
docker-compose -f infra/compose/local-dev.yml up --build
```

## References

- `docs/INFRA_DEVEX.md` - Developer experience guide
