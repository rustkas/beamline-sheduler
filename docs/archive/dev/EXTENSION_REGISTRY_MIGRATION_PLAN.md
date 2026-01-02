# Extension Registry Migration Plan

⚠️ **LEGACY**: Migration planning document. See `docs/archive/dev/EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md` for current implementation.

**Date**: 2025-01-27  
**Status**: ⚠️ **LEGACY** (Migration Planning)  
**Target**: CP2-LC  
**Workers**: wrk-2 (Router OTP) + wrk-1 (Infra/DB)  
**Current Source of Truth**: `docs/archive/dev/EXTENSION_REGISTRY_IMPLEMENTATION_REPORT.md`

---

## Executive Summary

Migration plan for transitioning Extension Registry from ETS-fixtures to PostgreSQL + Mnesia/ETS cache.

**Migration Phases**:
1. **Phase 1**: Dual-mode support (database + fixtures) - CP2-LC
2. **Phase 2**: Database-only (remove fixtures) - CP2+
3. **Phase 3**: Advanced features (circuit breaker, health monitoring) - CP3

---

## Migration Phases

### Phase 1: Dual-Mode Support (CP2-LC)

**Goal**: Support both fixtures and database without breaking existing functionality

**Timeline**: 1-2 weeks

**Tasks**:

1. **SQL Migration** (wrk-1)
   - Create `sql/011_extensions_registry.sql`
   - Run migration in development
   - Verify schema

2. **Database Integration** (wrk-2)
   - Create `router_extension_registry_db.erl`
   - Add PostgreSQL connection management
   - Implement load/sync functions

3. **Cache Layer Enhancement** (wrk-2)
   - Update `router_extension_registry.erl`
   - Add load-on-boot from database
   - Add hot-reload mechanism
   - Keep fixtures as fallback

4. **Configuration** (wrk-2)
   - Add environment variables
   - Add application config
   - Support `source` mode: `database | fixtures | auto`

5. **Testing** (wrk-2)
   - Unit tests for database integration
   - Integration tests for sync
   - Verify backward compatibility

**Configuration**:
```erlang
{extension_registry, [
    {source, auto},  % Try database, fallback to fixtures
    {db_enabled, true},
    {sync_interval_seconds, 60}
]}
```

**Rollout Strategy**:
- Deploy with `source = auto`
- Monitor for database availability
- Gradually migrate extensions to database
- Keep fixtures for development

### Phase 2: Database-Only (CP2+)

**Goal**: Remove fixtures dependency, database becomes primary source

**Timeline**: 1 week

**Tasks**:

1. **Remove Fixtures Loading** (wrk-2)
   - Remove fixtures loading from production code
   - Keep fixtures only for testing

2. **Migration Script** (wrk-1)
   - Script to migrate fixtures to database
   - Verify data integrity

3. **Update Configuration** (wrk-2)
   - Change default `source` to `database`
   - Remove fixtures-related config

4. **Testing** (wrk-2)
   - Verify database-only mode works
   - Test migration script

**Configuration**:
```erlang
{extension_registry, [
    {source, database},  % Database only
    {db_enabled, true}
]}
```

### Phase 3: Advanced Features (CP3)

**Goal**: Circuit breaker, health monitoring, automatic disable

**Timeline**: 2-3 weeks

**Tasks**:

1. **Circuit Breaker** (wrk-2)
   - Implement circuit breaker state machine
   - Track failure thresholds
   - Automatic disable/enable

2. **Health Monitoring** (wrk-2)
   - Real-time health tracking
   - Metrics collection
   - Dashboard integration

3. **Automatic Management** (wrk-2)
   - Auto-disable on repeated failures
   - Auto-enable on recovery
   - Health-based routing

**Status**: Backlog (CP3)

---

## Data Migration

### Fixtures to Database

**Source**: `apps/otp/router/priv/fixtures/extensions/*.json`

**Target**: PostgreSQL `extensions` table

**Migration Script**:
```erlang
%% Load fixtures and insert into database
migrate_fixtures_to_db() ->
    FixturesDir = "priv/fixtures/extensions",
    Files = filelib:wildcard("*.json", FixturesDir),
    lists:foreach(fun(File) ->
        {ok, Content} = file:read_file(File),
        Extensions = jsx:decode(Content, [return_maps]),
        lists:foreach(fun(Ext) ->
            insert_extension_to_db(Ext)
        end, Extensions)
    end, Files).
```

**Verification**:
- Count extensions in database
- Compare with fixtures
- Verify all fields migrated correctly

---

## Rollout Management

### Strategy: Feature Flags via `enabled` Field

**Workflow**:

1. **Create Extension** (disabled):
   ```sql
   INSERT INTO extensions (id, type, subject, enabled) 
   VALUES ('new_extension', 'pre', 'beamline.ext.pre.new_extension.v1', false);
   ```

2. **Test in Staging**:
   - Extension enabled for staging tenant only
   - Test functionality
   - Monitor health metrics

3. **Gradual Rollout**:
   - Enable for specific tenants (via policy)
   - Monitor for issues
   - Gradually expand

4. **Global Enable**:
   ```sql
   UPDATE extensions SET enabled = true WHERE id = 'new_extension';
   ```

5. **Disable if Issues**:
   ```sql
   UPDATE extensions SET enabled = false WHERE id = 'new_extension';
   ```

### Versioning Strategy

**Multiple Versions**:
- Store in `extension_versions` table
- Current version in `extensions` table
- Policies reference by `id` (not version)
- Router uses latest enabled version

**Example**:
```sql
-- Create v1
INSERT INTO extensions (id, type, subject, version) 
VALUES ('normalize_text', 'pre', 'beamline.ext.pre.normalize_text.v1', 'v1');

-- Create v2 (preserve v1 in extension_versions)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry)
SELECT id, version, subject, timeout_ms, retry FROM extensions WHERE id = 'normalize_text';

-- Update to v2
UPDATE extensions 
SET version = 'v2', subject = 'beamline.ext.pre.normalize_text.v2', updated_at = NOW()
WHERE id = 'normalize_text';
```

---

## Configuration Management

### Environment Variables

**Required**:
```bash
EXTENSION_REGISTRY_DB_ENABLED=true
EXTENSION_REGISTRY_DB_HOST=localhost
EXTENSION_REGISTRY_DB_PORT=5432
EXTENSION_REGISTRY_DB_NAME=beamline
EXTENSION_REGISTRY_DB_USER=beamline
EXTENSION_REGISTRY_DB_PASSWORD=...
```

**Optional**:
```bash
EXTENSION_REGISTRY_CACHE_TYPE=ets  # ets | mnesia
EXTENSION_REGISTRY_SYNC_INTERVAL_SECONDS=60
EXTENSION_REGISTRY_SOURCE=auto  # database | fixtures | auto
```

### Application Config

**File**: `apps/otp/router/src/beamline_router.app.src`

```erlang
{extension_registry, [
    {source, auto},  % database | fixtures | auto
    {db_enabled, true},
    {db_host, "localhost"},
    {db_port, 5432},
    {db_name, "beamline"},
    {db_user, "beamline"},
    {db_password, "..."},
    {cache_type, ets},  % ets | mnesia
    {sync_interval_seconds, 60}
]}
```

### Config Files

**Development**: `config/dev.config`
```erlang
{extension_registry, [
    {source, fixtures},  % Use fixtures in development
    {db_enabled, false}
]}
```

**Production**: `config/prod.config`
```erlang
{extension_registry, [
    {source, database},  % Use database in production
    {db_enabled, true},
    {db_host, os:getenv("EXTENSION_REGISTRY_DB_HOST")},
    {db_port, list_to_integer(os:getenv("EXTENSION_REGISTRY_DB_PORT", "5432"))},
    {db_name, os:getenv("EXTENSION_REGISTRY_DB_NAME")},
    {db_user, os:getenv("EXTENSION_REGISTRY_DB_USER")},
    {db_password, os:getenv("EXTENSION_REGISTRY_DB_PASSWORD")}
]}
```

---

## Implementation Tasks

### Task 1: SQL Migration (wrk-1)

**File**: `sql/011_extensions_registry.sql`

**Status**: ✅ **COMPLETED**

**Deliverables**:
- Extensions table
- Extension versions table
- Extension health table
- Extension audit log table
- Indexes
- Seed data

**Dependencies**: None

**Estimated Time**: 2-3 hours

### Task 2: Database Integration Module (wrk-2)

**File**: `apps/otp/router/src/router_extension_registry_db.erl`

**Functions**:
- `load_all_extensions/0` - Load all enabled extensions
- `load_extension/1` - Load single extension
- `update_health/2` - Update health metrics
- `log_audit/4` - Log audit event

**Dependencies**: Task 1

**Estimated Time**: 4-6 hours

### Task 3: Cache Layer Enhancement (wrk-2)

**File**: `apps/otp/router/src/router_extension_registry.erl` (update)

**Changes**:
- Add database loading on init
- Add hot-reload mechanism
- Add periodic sync
- Keep fixtures as fallback

**Dependencies**: Task 2

**Estimated Time**: 4-6 hours

### Task 4: Configuration Support (wrk-2)

**Files**:
- `apps/otp/router/src/beamline_router.app.src`
- `config/dev.config`
- `config/prod.config`

**Changes**:
- Add extension_registry config section
- Support environment variables
- Default values
- Documentation

**Dependencies**: None

**Estimated Time**: 2-3 hours

### Task 5: Circuit Breaker (CP3 Backlog)

**File**: `apps/otp/router/src/router_extension_circuit_breaker.erl`

**Functions**:
- `check_circuit/1` - Check if circuit is open
- `record_success/1` - Record successful invocation
- `record_failure/1` - Record failed invocation
- `update_state/1` - Update circuit breaker state

**Dependencies**: Task 2, Task 3

**Estimated Time**: 6-8 hours

**Status**: Backlog (CP3)

---

## Testing Strategy

### Unit Tests

**Database Module**:
- Load extensions from database
- Update health metrics
- Audit logging

**Cache Layer**:
- Load-on-boot
- Hot-reload
- Sync operations

### Integration Tests

**PostgreSQL ↔ Cache**:
- Load from database
- Sync to cache
- Hot-reload on changes

**Rollout Management**:
- Enable/disable extensions
- Version management
- Policy integration

### Load Tests

**Performance**:
- Cache lookup < 1ms
- Sync performance < 100ms
- Concurrent access

---

## Risk Mitigation

### Risk 1: Database Unavailable

**Mitigation**:
- Fallback to fixtures (Phase 1)
- Graceful degradation
- Health checks

### Risk 2: Sync Lag

**Mitigation**:
- Periodic sync (60 seconds)
- On-demand sync via admin API
- Event-driven sync (future)

### Risk 3: Cache Inconsistency

**Mitigation**:
- Version tracking
- Timestamp-based sync
- Full reload on inconsistency

---

## Success Criteria

### Phase 1 (CP2-LC)

- ✅ Database schema created
- ✅ Extensions load from database
- ✅ Cache layer works
- ✅ Fixtures fallback works
- ✅ Hot-reload functional

### Phase 2 (CP2+)

- ✅ Database-only mode works
- ✅ Fixtures removed from production
- ✅ Migration script verified

### Phase 3 (CP3)

- ✅ Circuit breaker functional
- ✅ Health monitoring operational
- ✅ Automatic disable/enable works

---

## References

- `docs/archive/dev/EXTENSION_REGISTRY_PRODUCTION_DESIGN.md` - Design document
- `sql/011_extensions_registry.sql` - SQL migration
- `apps/otp/router/src/router_extension_registry.erl` - Current implementation
- `docs/EXTENSIONS_API.md` - Extensions API specification

