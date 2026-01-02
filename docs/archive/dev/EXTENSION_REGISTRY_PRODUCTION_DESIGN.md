# Extension Registry Production Design

**Date**: 2025-01-27  
**Status**: Design  
**Target**: CP2-LC  
**Workers**: wrk-2 (Router OTP) + wrk-1 (Infra/DB)

---

## Executive Summary

Design for transitioning Extension Registry from ETS-fixtures (CP1) to production-ready PostgreSQL + Mnesia/ETS cache (CP2+).

**Key Components**:
- PostgreSQL schema for extensions storage
- Mnesia/ETS cache layer with load-on-boot and hot-reload
- Migration path from fixtures to database
- Rollout management with enabled flags and versioning
- Circuit-breaker/health-check (CP3 backlog)

---

## Architecture Overview

### Current State (CP1)

```
Extension Registry (ETS)
  └── Loads from fixtures (priv/fixtures/extensions/*.json)
  └── No persistence
  └── No hot-reload
  └── No versioning
```

### Target State (CP2+)

```
PostgreSQL (Primary Storage)
  └── extensions table
  └── extension_versions table
  └── extension_health table
  └── extension_audit_log table
  ↓
Mnesia/ETS Cache (Hot Path)
  └── Load-on-boot from PostgreSQL
  └── Hot-reload on changes
  └── Fast lookup (< 1ms)
  ↓
Router Extension Invoker
  └── Uses cached data
  └── Health tracking
```

---

## PostgreSQL Schema Design

### 1. Extensions Table

**Purpose**: Primary storage for extension metadata

**Schema**:
```sql
CREATE TABLE IF NOT EXISTS extensions (
    id VARCHAR(255) PRIMARY KEY,
    type VARCHAR(32) NOT NULL CHECK (type IN ('pre', 'validator', 'post', 'provider')),
    subject VARCHAR(512) NOT NULL,
    timeout_ms INTEGER NOT NULL DEFAULT 5000 CHECK (timeout_ms > 0),
    retry INTEGER NOT NULL DEFAULT 0 CHECK (retry >= 0),
    enabled BOOLEAN NOT NULL DEFAULT TRUE,
    version VARCHAR(32) NOT NULL DEFAULT 'v1',
    config JSONB DEFAULT '{}'::jsonb,
    metadata JSONB DEFAULT '{}'::jsonb,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(255),
    updated_by VARCHAR(255)
);

CREATE INDEX idx_extensions_type ON extensions(type);
CREATE INDEX idx_extensions_enabled ON extensions(enabled) WHERE enabled = TRUE;
CREATE INDEX idx_extensions_subject ON extensions(subject);
CREATE INDEX idx_extensions_updated_at ON extensions(updated_at);
```

**Fields**:
- `id` - Logical extension ID (e.g., "normalize_text")
- `type` - Extension type: pre, validator, post, provider
- `subject` - NATS subject (e.g., "beamline.ext.pre.normalize_text.v1")
- `timeout_ms` - Request timeout in milliseconds
- `retry` - Retry count on timeout
- `enabled` - Enable/disable flag for rollout management
- `version` - Extension version (v1, v2, etc.)
- `config` - Extension-specific configuration (JSONB)
- `metadata` - Additional metadata (JSONB)
- `created_at`, `updated_at` - Timestamps
- `created_by`, `updated_by` - Audit fields

### 2. Extension Versions Table

**Purpose**: Track extension version history

**Schema**:
```sql
CREATE TABLE IF NOT EXISTS extension_versions (
    id VARCHAR(255) NOT NULL,
    version VARCHAR(32) NOT NULL,
    subject VARCHAR(512) NOT NULL,
    timeout_ms INTEGER NOT NULL,
    retry INTEGER NOT NULL,
    config JSONB DEFAULT '{}'::jsonb,
    metadata JSONB DEFAULT '{}'::jsonb,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(255),
    PRIMARY KEY (id, version),
    FOREIGN KEY (id) REFERENCES extensions(id) ON DELETE CASCADE
);

CREATE INDEX idx_extension_versions_id ON extension_versions(id);
CREATE INDEX idx_extension_versions_created_at ON extension_versions(created_at);
```

**Use Cases**:
- Version migration tracking
- Rollback capability
- Audit trail

### 3. Extension Health Table

**Purpose**: Track extension health metrics

**Schema**:
```sql
CREATE TABLE IF NOT EXISTS extension_health (
    extension_id VARCHAR(255) PRIMARY KEY,
    last_success TIMESTAMP WITH TIME ZONE,
    last_failure TIMESTAMP WITH TIME ZONE,
    success_count BIGINT DEFAULT 0,
    failure_count BIGINT DEFAULT 0,
    avg_latency_ms NUMERIC(10, 2),
    last_latency_ms INTEGER,
    circuit_breaker_state VARCHAR(32) DEFAULT 'closed' CHECK (circuit_breaker_state IN ('closed', 'open', 'half_open')),
    circuit_breaker_opened_at TIMESTAMP WITH TIME ZONE,
    circuit_breaker_failure_threshold INTEGER DEFAULT 5,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (extension_id) REFERENCES extensions(id) ON DELETE CASCADE
);

CREATE INDEX idx_extension_health_circuit_state ON extension_health(circuit_breaker_state);
CREATE INDEX idx_extension_health_last_failure ON extension_health(last_failure);
```

**Metrics**:
- Success/failure counts
- Average latency
- Circuit breaker state (CP3)

### 4. Extension Audit Log Table

**Purpose**: Audit trail for extension changes

**Schema**:
```sql
CREATE TABLE IF NOT EXISTS extension_audit_log (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    extension_id VARCHAR(255) NOT NULL,
    action VARCHAR(32) NOT NULL CHECK (action IN ('create', 'update', 'delete', 'enable', 'disable')),
    old_values JSONB,
    new_values JSONB,
    changed_by VARCHAR(255),
    changed_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (extension_id) REFERENCES extensions(id) ON DELETE CASCADE
);

CREATE INDEX idx_extension_audit_extension_id ON extension_audit_log(extension_id);
CREATE INDEX idx_extension_audit_changed_at ON extension_audit_log(changed_at);
CREATE INDEX idx_extension_audit_action ON extension_audit_log(action);
```

**Audit Actions**:
- `create` - Extension created
- `update` - Extension updated
- `delete` - Extension deleted
- `enable` - Extension enabled
- `disable` - Extension disabled

---

## Mnesia/ETS Cache Design

### Cache Structure

**Mnesia Table** (for distributed cache):
```erlang
-record(extension_cache, {
    id :: binary(),
    type :: binary(),
    subject :: binary(),
    timeout_ms :: integer(),
    retry :: integer(),
    enabled :: boolean(),
    config :: map(),
    metadata :: map(),
    version :: binary(),
    updated_at :: integer()  % Unix timestamp
}).
```

**ETS Table** (for local cache):
- Same structure as Mnesia
- Faster lookup for single-node deployments
- Fallback if Mnesia unavailable

### Cache Loading Strategy

**Load-on-Boot**:
1. Router starts
2. Extension Registry initializes
3. Load all enabled extensions from PostgreSQL
4. Populate Mnesia/ETS cache
5. Router ready to serve requests

**Hot-Reload**:
1. Extension updated in PostgreSQL
2. Trigger reload (via admin API or periodic sync)
3. Reload changed extensions from PostgreSQL
4. Update Mnesia/ETS cache
5. No Router restart required

**Sync Interval**:
- Periodic sync: Every 60 seconds (configurable)
- On-demand sync: Via admin API
- Event-driven sync: Via PostgreSQL NOTIFY/LISTEN (future)

---

## Migration Path

### Phase 1: Dual-Mode Support (CP2-LC)

**Goal**: Support both fixtures and database

**Implementation**:
1. Keep fixtures for backward compatibility
2. Add PostgreSQL support
3. Load from database if available, fallback to fixtures
4. Cache in Mnesia/ETS

**Configuration**:
```erlang
{extension_registry, [
    {source, database},  % database | fixtures | auto
    {db_enabled, true},
    {sync_interval_seconds, 60},
    {cache_type, ets}  % ets | mnesia
]}
```

**Auto Mode**:
- Try database first
- Fallback to fixtures if database unavailable
- Log warning on fallback

### Phase 2: Database-Only (CP2+)

**Goal**: Remove fixtures dependency

**Implementation**:
1. Remove fixtures loading
2. Database becomes primary source
3. Fixtures only for development/testing

### Phase 3: Advanced Features (CP3)

**Goal**: Circuit breaker, health monitoring

**Implementation**:
1. Circuit breaker for frequently failing extensions
2. Health-based routing
3. Automatic disable on repeated failures

---

## Rollout Management

### Enabled Flag Strategy

**Purpose**: Gradual rollout of new extensions

**Workflow**:
1. Create extension with `enabled = false`
2. Test extension in staging
3. Enable for specific tenants (via policy)
4. Enable globally (`enabled = true`)
5. Monitor health metrics

**Example**:
```sql
-- Create extension (disabled)
INSERT INTO extensions (id, type, subject, enabled) 
VALUES ('new_extension', 'pre', 'beamline.ext.pre.new_extension.v1', false);

-- Enable for specific tenant (via policy)
-- Policy references extension, but extension is disabled globally
-- Policy can still work if extension is enabled for that tenant

-- Enable globally
UPDATE extensions SET enabled = true WHERE id = 'new_extension';
```

### Versioning Strategy

**Purpose**: Support multiple versions of same extension

**Workflow**:
1. Create new version in `extension_versions`
2. Update `extensions` table with new version
3. Old version preserved in `extension_versions`
4. Policies reference extension by `id` (not version)
5. Router uses latest enabled version

**Example**:
```sql
-- Create v1
INSERT INTO extensions (id, type, subject, version) 
VALUES ('normalize_text', 'pre', 'beamline.ext.pre.normalize_text.v1', 'v1');

-- Create v2
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry)
VALUES ('normalize_text', 'v2', 'beamline.ext.pre.normalize_text.v2', 200, 1);

-- Update to v2
UPDATE extensions 
SET version = 'v2', subject = 'beamline.ext.pre.normalize_text.v2', updated_at = NOW()
WHERE id = 'normalize_text';
```

---

## Configuration

### Environment Variables

```bash
# Extension Registry Database
EXTENSION_REGISTRY_DB_ENABLED=true
EXTENSION_REGISTRY_DB_HOST=localhost
EXTENSION_REGISTRY_DB_PORT=5432
EXTENSION_REGISTRY_DB_NAME=beamline
EXTENSION_REGISTRY_DB_USER=beamline
EXTENSION_REGISTRY_DB_PASSWORD=...

# Extension Registry Cache
EXTENSION_REGISTRY_CACHE_TYPE=ets  # ets | mnesia
EXTENSION_REGISTRY_SYNC_INTERVAL_SECONDS=60
EXTENSION_REGISTRY_SOURCE=auto  # database | fixtures | auto

# Extension Registry Health (CP3)
EXTENSION_REGISTRY_CIRCUIT_BREAKER_ENABLED=false
EXTENSION_REGISTRY_CIRCUIT_BREAKER_FAILURE_THRESHOLD=5
EXTENSION_REGISTRY_CIRCUIT_BREAKER_TIMEOUT_SECONDS=60
```

### Erlang Application Config

```erlang
{beamline_router, [
    {extension_registry, [
        {source, auto},  % database | fixtures | auto
        {db_enabled, true},
        {db_host, "localhost"},
        {db_port, 5432},
        {db_name, "beamline"},
        {db_user, "beamline"},
        {db_password, "..."},
        {cache_type, ets},  % ets | mnesia
        {sync_interval_seconds, 60},
        {circuit_breaker_enabled, false},  % CP3
        {circuit_breaker_failure_threshold, 5},  % CP3
        {circuit_breaker_timeout_seconds, 60}  % CP3
    ]}
]}
```

---

## Implementation Tasks

### Task 1: SQL Migration

**File**: `sql/011_extensions_registry.sql`

**Deliverables**:
- Extensions table
- Extension versions table
- Extension health table
- Extension audit log table
- Indexes
- Seed data for testing

**Dependencies**: None

**Estimated Time**: 2-3 hours

### Task 2: Database Integration

**File**: `apps/otp/router/src/router_extension_registry_db.erl`

**Deliverables**:
- PostgreSQL connection management
- Load extensions from database
- Update extension health
- Audit logging

**Dependencies**: Task 1

**Estimated Time**: 4-6 hours

### Task 3: Cache Layer Enhancement

**File**: `apps/otp/router/src/router_extension_registry.erl` (update)

**Deliverables**:
- Load-on-boot from database
- Hot-reload mechanism
- Mnesia/ETS cache sync
- Periodic sync

**Dependencies**: Task 2

**Estimated Time**: 4-6 hours

### Task 4: Configuration

**Files**: 
- `apps/otp/router/src/beamline_router.app.src`
- `config/*.config`

**Deliverables**:
- Environment variable support
- Application config
- Default values
- Documentation

**Dependencies**: None

**Estimated Time**: 2-3 hours

### Task 5: Circuit Breaker (CP3 Backlog)

**File**: `apps/otp/router/src/router_extension_circuit_breaker.erl`

**Deliverables**:
- Circuit breaker state machine
- Failure threshold tracking
- Automatic disable/enable
- Health-based routing

**Dependencies**: Task 2, Task 3

**Estimated Time**: 6-8 hours

**Status**: Backlog (CP3)

---

## Data Storage Strategy

### Primary Data Location

**Production**: PostgreSQL (single source of truth)
- All extensions stored in database
- Admin API manages extensions
- Version control via database

**Development**: Fixtures (optional)
- Local development can use fixtures
- Faster iteration
- No database required

**Migration**:
- CP2-LC: Dual-mode (database + fixtures)
- CP2+: Database-only (fixtures for testing only)

### Rollout Management

**Strategy**: Feature flags via `enabled` field

**Workflow**:
1. **Create**: Extension created with `enabled = false`
2. **Test**: Test in staging environment
3. **Rollout**: Enable for specific tenants (via policy configuration)
4. **Monitor**: Track health metrics
5. **Enable**: Set `enabled = true` for global rollout
6. **Disable**: Set `enabled = false` to disable (circuit breaker or manual)

**Example**:
```sql
-- Step 1: Create extension (disabled)
INSERT INTO extensions (id, type, subject, enabled) 
VALUES ('new_preprocessor', 'pre', 'beamline.ext.pre.new_preprocessor.v1', false);

-- Step 2: Test in staging (extension enabled for staging tenant only)
-- (Managed via policy, not extension table)

-- Step 3: Enable globally
UPDATE extensions SET enabled = true WHERE id = 'new_preprocessor';

-- Step 4: Disable if issues found
UPDATE extensions SET enabled = false WHERE id = 'new_preprocessor';
```

---

## Health Check and Circuit Breaker (CP3 Backlog)

### Health Metrics

**Tracked Metrics**:
- Success count
- Failure count
- Average latency
- Last success timestamp
- Last failure timestamp

**Update Frequency**:
- On every extension invocation
- Async update to database
- Cached in Mnesia/ETS

### Circuit Breaker States

**Closed** (Normal):
- Extension working normally
- All requests pass through

**Open** (Failing):
- Extension failing repeatedly
- Requests fail fast (no invocation)
- Health check periodically attempts recovery

**Half-Open** (Testing):
- Testing if extension recovered
- Limited requests allowed
- On success → Closed
- On failure → Open

### Circuit Breaker Configuration

**Thresholds**:
- `failure_threshold`: 5 consecutive failures
- `timeout_seconds`: 60 seconds before attempting recovery
- `half_open_max_requests`: 3 requests in half-open state

**Automatic Actions**:
- Open circuit on threshold exceeded
- Attempt recovery after timeout
- Close circuit on successful recovery
- Update `enabled` flag based on circuit state (optional)

---

## Performance Requirements

### Cache Lookup

**Target**: < 1ms (99th percentile)

**Implementation**:
- ETS table with read_concurrency
- Mnesia for distributed deployments
- In-memory cache (no disk I/O)

### Database Load

**Target**: Minimal impact on hot path

**Implementation**:
- Async health updates
- Batch sync operations
- Connection pooling

### Sync Performance

**Target**: < 100ms for full sync

**Implementation**:
- Incremental sync (only changed extensions)
- Efficient queries with indexes
- Connection pooling

---

## Security Considerations

### Access Control

**Database Access**:
- Read-only access for Router
- Admin API requires RBAC
- Audit logging for all changes

### Secret Management

**Configuration**:
- Database credentials in environment variables
- Never commit secrets to repository
- Use CI/CD secrets in production

### Audit Trail

**Requirements**:
- All extension changes logged
- Who made the change
- When the change was made
- What changed (old/new values)

---

## Monitoring and Observability

### Metrics

**Extension Registry**:
- `extension_registry_size` - Total extensions
- `extension_registry_enabled_count` - Enabled extensions
- `extension_registry_sync_duration_seconds` - Sync latency
- `extension_registry_sync_errors_total` - Sync errors

**Extension Health**:
- `extension_health_success_rate{extension_id}` - Success rate
- `extension_health_avg_latency_ms{extension_id}` - Average latency
- `extension_health_circuit_state{extension_id}` - Circuit breaker state

### Logs

**Required Fields**:
- `timestamp`, `level`, `extension_id`, `action`
- `sync_duration_ms`, `sync_status`
- `cache_hit`, `cache_miss`

---

## Testing Strategy

### Unit Tests

- Database queries
- Cache operations
- Sync logic

### Integration Tests

- PostgreSQL ↔ Mnesia/ETS sync
- Hot-reload functionality
- Rollout management

### Load Tests

- Cache lookup performance
- Sync performance
- Concurrent access

---

## References

- `docs/archive/dev/CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md` - Implementation plan
- `docs/EXTENSIONS_API.md` - Extensions API specification
- `apps/otp/router/src/router_extension_registry.erl` - Current implementation
- `apps/otp/router/src/router_policy_store.erl` - Policy Store (similar pattern)

