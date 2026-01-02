# Extension Registry Configuration Examples

⚠️ **APPENDIX**: Configuration examples document. See `docs/archive/dev/EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md` for complete implementation details.

**Status**: ⚠️ **APPENDIX** (Examples Reference)

**Date**: 2025-01-27  
**Status**: Configuration Examples  
**Target**: CP2-LC

---

## Environment Variables

### Required (CP2-LC)

```bash
# Extension Registry Database
EXTENSION_REGISTRY_DB_ENABLED=true
EXTENSION_REGISTRY_DB_HOST=localhost
EXTENSION_REGISTRY_DB_PORT=5432
EXTENSION_REGISTRY_DB_NAME=beamline
EXTENSION_REGISTRY_DB_USER=beamline
EXTENSION_REGISTRY_DB_PASSWORD=your_password_here
```

### Optional

```bash
# Extension Registry Cache
EXTENSION_REGISTRY_CACHE_TYPE=ets  # ets | mnesia
EXTENSION_REGISTRY_SYNC_INTERVAL_SECONDS=60
EXTENSION_REGISTRY_SOURCE=auto  # database | fixtures | auto

# Extension Registry Health (CP3)
EXTENSION_REGISTRY_CIRCUIT_BREAKER_ENABLED=false
EXTENSION_REGISTRY_CIRCUIT_BREAKER_FAILURE_THRESHOLD=5
EXTENSION_REGISTRY_CIRCUIT_BREAKER_TIMEOUT_SECONDS=60
```

---

## Application Configuration

### Development (`config/dev.config`)

```erlang
[
  {beamline_router, [
    {extension_registry, [
      {source, fixtures},  % Use fixtures in development
      {db_enabled, false},
      {cache_type, ets},
      {sync_interval_seconds, 60}
    ]}
  ]}
].
```

### Staging (`config/staging.config`)

```erlang
[
  {beamline_router, [
    {extension_registry, [
      {source, auto},  % Try database, fallback to fixtures
      {db_enabled, true},
      {db_host, os:getenv("EXTENSION_REGISTRY_DB_HOST", "localhost")},
      {db_port, list_to_integer(os:getenv("EXTENSION_REGISTRY_DB_PORT", "5432"))},
      {db_name, os:getenv("EXTENSION_REGISTRY_DB_NAME", "beamline")},
      {db_user, os:getenv("EXTENSION_REGISTRY_DB_USER", "beamline")},
      {db_password, os:getenv("EXTENSION_REGISTRY_DB_PASSWORD", "")},
      {db_pool_size, 10},
      {cache_type, ets},
      {sync_interval_seconds, 60}
    ]}
  ]}
].
```

### Production (`config/prod.config`)

```erlang
[
  {beamline_router, [
    {extension_registry, [
      {source, database},  % Database only in production
      {db_enabled, true},
      {db_host, os:getenv("EXTENSION_REGISTRY_DB_HOST")},
      {db_port, list_to_integer(os:getenv("EXTENSION_REGISTRY_DB_PORT", "5432"))},
      {db_name, os:getenv("EXTENSION_REGISTRY_DB_NAME")},
      {db_user, os:getenv("EXTENSION_REGISTRY_DB_USER")},
      {db_password, os:getenv("EXTENSION_REGISTRY_DB_PASSWORD")},
      {db_pool_size, 20},  % Larger pool for production
      {cache_type, mnesia},  % Mnesia for distributed deployments
      {sync_interval_seconds, 30},  % More frequent sync
      {circuit_breaker_enabled, false}  % CP3: Enable in CP3
    ]}
  ]}
].
```

---

## Configuration Modes

### Mode 1: Fixtures Only (CP1, Development)

```erlang
{extension_registry, [
  {source, fixtures},
  {db_enabled, false}
]}
```

**Use Case**: Local development, no database required

### Mode 2: Database Only (CP2+, Production)

```erlang
{extension_registry, [
  {source, database},
  {db_enabled, true},
  {db_host, "localhost"},
  {db_port, 5432},
  {db_name, "beamline"},
  {db_user, "beamline"},
  {db_password, "..."}
]}
```

**Use Case**: Production, database as single source of truth

### Mode 3: Auto (CP2-LC, Transition)

```erlang
{extension_registry, [
  {source, auto},  % Try database, fallback to fixtures
  {db_enabled, true},
  {db_host, "localhost"},
  {db_port, 5432},
  {db_name, "beamline"},
  {db_user, "beamline"},
  {db_password, "..."}
]}
```

**Use Case**: Migration period, gradual rollout

---

## Circuit Breaker Configuration (CP3 Backlog)

### Enabled

```erlang
{extension_registry, [
  {circuit_breaker_enabled, true},
  {circuit_breaker_failure_threshold, 5},  % Open circuit after 5 failures
  {circuit_breaker_timeout_seconds, 60}  % Attempt recovery after 60 seconds
]}
```

### Disabled (Default)

```erlang
{extension_registry, [
  {circuit_breaker_enabled, false}
]}
```

---

## Connection Pool Configuration

### Small Deployment

```erlang
{db_pool_size, 5}
```

### Medium Deployment

```erlang
{db_pool_size, 10}
```

### Large Deployment

```erlang
{db_pool_size, 20}
```

---

## Cache Configuration

### ETS (Single Node)

```erlang
{cache_type, ets}
```

**Use Case**: Single-node deployments, faster startup

### Mnesia (Distributed)

```erlang
{cache_type, mnesia}
```

**Use Case**: Multi-node deployments, distributed cache

---

## Sync Configuration

### Frequent Sync (Production)

```erlang
{sync_interval_seconds, 30}  % Sync every 30 seconds
```

### Standard Sync

```erlang
{sync_interval_seconds, 60}  % Sync every 60 seconds
```

### Infrequent Sync (Development)

```erlang
{sync_interval_seconds, 300}  % Sync every 5 minutes
```

---

## Docker Compose Example

```yaml
services:
  router:
    environment:
      - EXTENSION_REGISTRY_DB_ENABLED=true
      - EXTENSION_REGISTRY_DB_HOST=postgres
      - EXTENSION_REGISTRY_DB_PORT=5432
      - EXTENSION_REGISTRY_DB_NAME=beamline
      - EXTENSION_REGISTRY_DB_USER=beamline
      - EXTENSION_REGISTRY_DB_PASSWORD=${DB_PASSWORD}
      - EXTENSION_REGISTRY_SOURCE=auto
      - EXTENSION_REGISTRY_CACHE_TYPE=ets
      - EXTENSION_REGISTRY_SYNC_INTERVAL_SECONDS=60
```

---

## Kubernetes ConfigMap Example

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: extension-registry-config
data:
  EXTENSION_REGISTRY_DB_ENABLED: "true"
  EXTENSION_REGISTRY_DB_HOST: "postgres-service"
  EXTENSION_REGISTRY_DB_PORT: "5432"
  EXTENSION_REGISTRY_DB_NAME: "beamline"
  EXTENSION_REGISTRY_SOURCE: "database"
  EXTENSION_REGISTRY_CACHE_TYPE: "mnesia"
  EXTENSION_REGISTRY_SYNC_INTERVAL_SECONDS: "30"
```

---

## Validation

### Check Configuration

```erlang
%% In Erlang shell
application:get_env(beamline_router, extension_registry).
```

### Verify Database Connection

```erlang
%% Test database connection
router_extension_registry_db:test_connection().
```

---

## Troubleshooting

### Database Connection Failed

**Symptom**: Router falls back to fixtures

**Solution**:
1. Check database is running
2. Verify credentials
3. Check network connectivity
4. Review logs for connection errors

### Sync Not Working

**Symptom**: Extensions not updating

**Solution**:
1. Check sync interval configuration
2. Verify database permissions
3. Check for sync errors in logs
4. Manually trigger sync: `router_extension_registry:reload()`

### Cache Inconsistency

**Symptom**: Stale extension data

**Solution**:
1. Reduce sync interval
2. Enable on-demand sync
3. Check for sync errors
4. Force full reload

---

## References

- `docs/archive/dev/EXTENSION_REGISTRY_PRODUCTION_DESIGN.md` - Design document
- `docs/archive/dev/EXTENSION_REGISTRY_MIGRATION_PLAN.md` - Migration plan
- `sql/011_extensions_registry.sql` - SQL migration
- `apps/otp/router/src/beamline_router.app.src` - Application config

