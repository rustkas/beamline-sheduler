# BeamLine Scheduler Deployment Guide

> Complete guide for deploying BeamLine Scheduler in various environments

## Table of Contents

- [Prerequisites](#prerequisites)
- [Quick Start (Docker Compose)](#quick-start-docker-compose)
- [Kubernetes Deployment](#kubernetes-deployment)
- [Systemd Deployment](#systemd-deployment)
- [Configuration](#configuration)
- [Monitoring \& Observability](#monitoring--observability)
- [Security](#security)
- [Troubleshooting](#troubleshooting)

## Prerequisites

### Software Requirements

```bash
# Required for all deployments
- Docker 20.10+ or compatible container runtime
- NATS Server 2.9+
- PostgreSQL 14+ (for UI and state persistence)

# For Kubernetes deployment
- Kubernetes 1.24+
- kubectl configured
- Helm 3+ (optional, recommended)

# For source builds
- Erlang/OTP 26+
- Rust 1.70+
- C++20 compiler (g++ 11+ or clang 14+)
- C11 compiler (gcc or clang)
- Elixir 1.15+ (for Phoenix UI)
- CMake 3.20+
```

### Network Requirements

| Service | Port | Protocol | Purpose |
|---------|------|----------|---------|
| C-Gateway | 8080 | HTTP | API ingress |
| Router | 9000 | gRPC | Admin interface |
| CAF Processor | 9090 | HTTP | Health/metrics |
| Rust Worker | 9091 | HTTP | Health/metrics |
| Phoenix UI | 4000 | HTTP | Web interface |
| NATS | 4222 | TCP | Messaging |
| NATS Monitor | 8222 | HTTP | Monitoring |
| PostgreSQL | 5432 | TCP | Database |

## Quick Start (Docker Compose)

### 1. Clone Repository

```bash
git clone --recursive https://github.com/rustkas/beamline-scheduler.git
cd beamline-scheduler

# If already cloned without submodules
git submodule init
git submodule update --recursive
```

### 2. Start Services

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Check status
docker-compose ps
```

### 3. Verify Deployment

```bash
# C-Gateway health
curl http://localhost:8080/_health

# Phoenix UI (if running)
curl http://localhost:4000/health

# NATS monitoring
curl http://localhost:8222/varz
```

### 4. Stop Services

```bash
docker-compose down

# Remove volumes (CAUTION: deletes data)
docker-compose down -v
```

## Kubernetes Deployment

### Prerequisites

```bash
# Verify kubectl access
kubectl cluster-info

# Create namespace
kubectl create namespace beamline
```

### Option 1: Raw Manifests

```bash
# Apply all manifests
kubectl apply -f infra/k8s/ -n beamline

# Check deployment status
kubectl get pods -n beamline
kubectl get services -n beamline
```

### Option 2: Helm Chart (Recommended)

```bash
# Add Helm repo (if available)
helm repo add beamline https://charts.beamline.io
helm repo update

# Install chart
helm install beamline beamline/beamline \
  --namespace beamline \
  --create-namespace \
  --values values-production.yaml

# Or install from local charts
helm install beamline ./charts/beamline \
  --namespace beamline \
  --create-namespace
```

### Configuration Values

Create `values-production.yaml`:

```yaml
# Global settings
global:
  environment: production
  natsUrl: nats://nats.beamline.svc.cluster.local:4222
  
# C-Gateway
gateway:
  replicas: 3
  resources:
    requests:
      cpu: 100m
      memory: 128Mi
    limits:
      cpu: 500m
      memory: 512Mi
  
# Erlang Router
router:
  replicas: 3
  resources:
    requests:
      cpu: 200m
      memory: 256Mi
    limits:
      cpu: 1000m
      memory: 1Gi
  grpcPort: 9000

# Workers
workers:
  rust:
    replicas: 5
    resources:
      requests:
        cpu: 200m
        memory: 256Mi
      limits:
        cpu: 2000m
        memory: 2Gi
  
  caf:
    replicas: 3
    resources:
      requests:
        cpu: 500m
        memory: 512Mi
      limits:
        cpu: 4000m
        memory: 4Gi

# Phoenix UI
ui:
  replicas: 2
  resources:
    requests:
      cpu: 100m
      memory: 256Mi
    limits:
      cpu: 500m
      memory: 1Gi

# NATS
nats:
  enabled: true
  cluster:
    enabled: true
    replicas: 3
  jetstream:
    enabled: true
    memStorage: 1Gi
    fileStorage: 10Gi

# PostgreSQL
postgresql:
  enabled: true
  auth:
    existingSecret: beamline-postgres-secret
  primary:
    persistence:
      size: 20Gi
```

### Ingress Configuration

```yaml
# ingress.yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: beamline-gateway
  namespace: beamline
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
    nginx.ingress.kubernetes.io/rate-limit: "100"
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - api.beamline.example.com
    secretName: beamline-tls
  rules:
  - host: api.beamline.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: c-gateway
            port:
              number: 8080
---
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: beamline-ui
  namespace: beamline
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - ui.beamline.example.com
    secretName: beamline-ui-tls
  rules:
  - host: ui.beamline.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: phoenix-ui
            port:
              number: 4000
```

### Monitoring

```bash
# Check logs
kubectl logs -f deployment/c-gateway -n beamline
kubectl logs -f deployment/router -n beamline

# Port-forward for local access
kubectl port-forward service/c-gateway 8080:8080 -n beamline
kubectl port-forward service/phoenix-ui 4000:4000 -n beamline

# Check metrics
kubectl port-forward service/prometheus 9090:9090 -n beamline
# Visit http://localhost:9090
```

## Systemd Deployment

### C-Gateway Service

```ini
# /etc/systemd/system/beamline-gateway.service
[Unit]
Description=BeamLine C-Gateway
After=network.target nats.service
Requires=nats.service

[Service]
Type=simple
User=beamline
Group=beamline
WorkingDirectory=/opt/beamline/c-gateway
Environment="NATS_URL=nats://localhost:4222"
Environment="ROUTER_DECIDE_SUBJECT=beamline.router.v1.decide"
Environment="GATEWAY_BASE_URL=http://localhost:8080"
ExecStart=/opt/beamline/c-gateway/bin/c-gateway
Restart=always
RestartSec=10
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
```

### Erlang Router Service

```ini
# /etc/systemd/system/beamline-router.service
[Unit]
Description=BeamLine Erlang Router
After=network.target nats.service
Requires=nats.service

[Service]
Type=simple
User=beamline
Group=beamline
WorkingDirectory=/opt/beamline/router
Environment="NATS_URL=nats://localhost:4222"
Environment="ROUTER_GRPC_PORT=9000"
ExecStart=/opt/beamline/router/bin/router foreground
Restart=always
RestartSec=10
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
```

### Enable and Start Services

```bash
# Reload systemd
sudo systemctl daemon-reload

# Enable services
sudo systemctl enable beamline-gateway
sudo systemctl enable beamline-router

# Start services
sudo systemctl start beamline-gateway
sudo systemctl start beamline-router

# Check status
sudo systemctl status beamline-gateway
sudo systemctl status beamline-router

# View logs
sudo journalctl -u beamline-gateway -f
sudo journalctl -u beamline-router -f
```

## Configuration

### Environment Variables

#### C-Gateway
```bash
GATEWAY_BASE_URL=http://localhost:8080
NATS_URL=nats://localhost:4222
ROUTER_DECIDE_SUBJECT=beamline.router.v1.decide
LOG_LEVEL=info
MAX_CONNECTIONS=1024
REQUEST_TIMEOUT_MS=30000
```

#### Erlang Router
```bash
NATS_URL=nats://localhost:4222
ROUTER_GRPC_PORT=9000
LOG_LEVEL=info
POOL_SIZE=10
JETSTREAM_ENABLED=true
IDEMPOTENCY_TTL_SEC=3600
```

#### Rust Worker
```bash
NATS_URL=nats://localhost:4222
WORKER_MAX_CONCURRENCY=8
LOG_LEVEL=info
DATABASE_URL=postgresql://user:pass@localhost:5432/beamline
```

#### CAF Processor
```bash
NATS_URL=nats://localhost:4222
CAF_SCHEDULER_POLICY=sharing
LOG_LEVEL=info
RESOURCE_POOL_CPU=4
RESOURCE_POOL_GPU=1
```

#### Phoenix UI
```bash
PORT=4000
GATEWAY_BASE_URL=http://localhost:8080
DATABASE_URL=postgresql://user:pass@localhost:5432/beamline_ui
SECRET_KEY_BASE=<generate_with_mix_phx.gen.secret>
AUTH_ENABLED=true
OIDC_ISSUER=https://auth.example.com
OIDC_CLIENT_ID=beamline-ui
OIDC_CLIENT_SECRET=<secret>
```

### Configuration Files

Component-specific configuration files are documented in:
- [C-Gateway README](../apps/c-gateway/README.md)
- [Router README](../apps/otp/router/README.md)
- [CAF Processor README](../apps/caf/processor/README.md)
- [Rust Worker README](../apps/worker/README.md)
- [Phoenix UI README](../apps/ui_web/README.md)

## Monitoring & Observability

### Prometheus Metrics

All services expose Prometheus-compatible metrics:

```bash
# C-Gateway metrics
curl http://localhost:8080/metrics

# Router metrics (via gRPC reflection or HTTP endpoint if configured)
# CAF Processor metrics
curl http://localhost:9090/metrics

# Rust Worker metrics
curl http://localhost:9091/metrics
```

### Grafana Dashboards

Import pre-built dashboards from `infra/grafana/dashboards/`:
- Router Dashboard
- Gateway Dashboard
- Worker Dashboard
- System Overview

### Health Checks

```bash
# Kubernetes health probes
livenessProbe:
  httpGet:
    path: /_health
    port: 8080
  initialDelaySeconds: 10
  periodSeconds: 10

readinessProbe:
  httpGet:
    path: /_health
    port: 8080
  initialDelaySeconds: 5
  periodSeconds: 5
```

### Logging

All services use structured JSON logging:

```json
{
  "timestamp": "2026-01-02T12:00:00Z",
  "level": "info",
  "service": "c-gateway",
  "message": "Request processed",
  "request_id": "abc-123",
  "duration_ms": 45,
  "status": 200
}
```

**Documentation**: [OBSERVABILITY.md](OBSERVABILITY.md)

## Security

### TLS Configuration

```bash
# Generate certificates (development)
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes

# Production: Use cert-manager or similar
```

### NATS TLS

```bash
# NATS server configuration
tls {
  cert_file: "/path/to/cert.pem"
  key_file: "/path/to/key.pem"
  ca_file: "/path/to/ca.pem"
  verify: true
}
```

### Secrets Management

```bash
# Kubernetes secrets
kubectl create secret generic beamline-secrets \
  --from-literal=database-url='postgresql://...' \
  --from-literal=jwt-secret='...' \
  -n beamline

# Mount in deployment
env:
- name: DATABASE_URL
  valueFrom:
    secretKeyRef:
      name: beamline-secrets
      key: database-url
```

**Documentation**: [SECURITY_GUIDE.md](SECURITY_GUIDE.md)

## Troubleshooting

### Common Issues

#### Services Not Starting

```bash
# Check logs
docker-compose logs <service>
kubectl logs deployment/<service> -n beamline

# Verify NATS connectivity
nats-top  # or check NATS monitoring endpoint
curl http://localhost:8222/connz
```

#### High Latency

```bash
# Check NATS queue
curl http://localhost:8222/subsz

# Check component metrics
curl http://localhost:8080/metrics | grep latency

# Verify resource utilization
kubectl top pods -n beamline
```

#### Database Connection Issues

```bash
# Verify PostgreSQL is running
psql -h localhost -U beamline -d beamline -c "SELECT 1;"

# Check connection pool
# In Phoenix UI logs, look for Ecto pool warnings
```

### Debug Mode

```bash
# Enable debug logging
export LOG_LEVEL=debug

# C-Gateway verbose mode
./c-gateway --verbose

# Router debug
export ROUTER_LOG_LEVEL=debug
```

### Performance Tuning

```bash
# Increase worker concurrency
export WORKER_MAX_CONCURRENCY=16

# Adjust NATS connection pool
export NATS_POOL_SIZE=20

# Router ETS table tuning (in sys.config)
{router, [
  {idempotency_cache_size, 100000},
  {policy_cache_ttl_sec, 300}
]}
```

## Backup & Recovery

### Database Backup

```bash
# PostgreSQL backup
pg_dump -h localhost -U beamline beamline > backup.sql

# Kubernetes CronJob for automated backups
kubectl apply -f infra/k8s/backup-cronjob.yaml
```

### State Recovery

```bash
# NATS JetStream backup
nats stream backup <stream-name> /backup/path

# Restore
nats stream restore <stream-name> /backup/path
```

## Scaling

### Horizontal Scaling

```bash
# Scale workers
kubectl scale deployment/rust-worker --replicas=10 -n beamline

# Scale gateway
kubectl scale deployment/c-gateway --replicas=5 -n beamline

# Autoscaling (HPA)
kubectl autoscale deployment/rust-worker \
  --cpu-percent=70 \
  --min=3 \
  --max=20 \
  -n beamline
```

### Vertical Scaling

```bash
# Increase resource limits
kubectl set resources deployment/caf-processor \
  --limits=cpu=8,memory=8Gi \
  --requests=cpu=4,memory=4Gi \
  -n beamline
```

## Production Checklist

- [ ] TLS enabled for all external endpoints
- [ ] NATS authentication configured
- [ ] Database credentials in secrets
- [ ] Resource limits set for all pods
- [ ] Horizontal Pod Autoscaler configured
- [ ] Prometheus monitoring enabled
- [ ] Grafana dashboards imported
- [ ] Alerting rules configured
- [ ] Log aggregation configured (e.g., Loki, ELK)
- [ ] Backup strategy implemented
- [ ] Disaster recovery plan documented
- [ ] Load testing completed
- [ ] Security scanning passed

## Related Documentation

- [Architecture Overview](ARCHITECTURE.md)
- [Operations Guide (Russian)](OPERATIONS_GUIDE_RU.md)
- [Security Guide](SECURITY_GUIDE.md)
- [Observability Guide](OBSERVABILITY.md)
- [Docker Guide](DOCKER.md)
- [Environment Guide](ENVIRONMENT.md)

## Support

For deployment issues:
- Check [GitHub Issues](https://github.com/rustkas/beamline-scheduler/issues)
- Review [GitHub Discussions](https://github.com/rustkas/beamline-scheduler/discussions)
- Consult [BeamLine Master AI](https://aistudio.instagram.com/ai/4815329165457920/)

---

**Last Updated**: January 2026  
**Version**: 1.0 (CP3)
