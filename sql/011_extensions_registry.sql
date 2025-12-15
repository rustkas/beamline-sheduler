-- Extension Registry Migration
-- Version: 1.0
-- Date: 2025-01-27
-- Description: Creates tables for Extension Registry (PostgreSQL primary storage)

-- Extensions table: Primary storage for extension metadata
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

-- Indexes for extensions table
CREATE INDEX IF NOT EXISTS idx_extensions_type ON extensions(type);
CREATE INDEX IF NOT EXISTS idx_extensions_enabled ON extensions(enabled) WHERE enabled = TRUE;
CREATE INDEX IF NOT EXISTS idx_extensions_subject ON extensions(subject);
CREATE INDEX IF NOT EXISTS idx_extensions_updated_at ON extensions(updated_at);

-- Extension versions table: Track extension version history
CREATE TABLE IF NOT EXISTS extension_versions (
    id VARCHAR(255) NOT NULL,
    version VARCHAR(32) NOT NULL,
    subject VARCHAR(512) NOT NULL,
    timeout_ms INTEGER NOT NULL,
    retry INTEGER NOT NULL,
    config JSONB DEFAULT '{}'::jsonb,
    metadata JSONB DEFAULT '{}'::jsonb,
    routing_rules JSONB DEFAULT '{}'::jsonb,  -- Version routing rules (e.g., {"tenant_id": ["tenant1", "tenant2"]})
    enabled BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by VARCHAR(255),
    PRIMARY KEY (id, version),
    FOREIGN KEY (id) REFERENCES extensions(id) ON DELETE CASCADE
);

-- Extension instances table: Multiple instances/subjects for load balancing
CREATE TABLE IF NOT EXISTS extension_instances (
    extension_id VARCHAR(255) NOT NULL,
    instance_id VARCHAR(255) NOT NULL,
    subject VARCHAR(512) NOT NULL,
    weight INTEGER DEFAULT 100 CHECK (weight >= 0 AND weight <= 100),  -- Load balancing weight (0-100)
    enabled BOOLEAN NOT NULL DEFAULT TRUE,
    health_status VARCHAR(32) DEFAULT 'unknown' CHECK (health_status IN ('healthy', 'degraded', 'unhealthy', 'unknown')),
    last_health_check TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (extension_id, instance_id),
    FOREIGN KEY (extension_id) REFERENCES extensions(id) ON DELETE CASCADE
);

-- Indexes for extension_instances table
CREATE INDEX IF NOT EXISTS idx_extension_instances_extension_id ON extension_instances(extension_id);
CREATE INDEX IF NOT EXISTS idx_extension_instances_enabled ON extension_instances(extension_id, enabled) WHERE enabled = TRUE;
CREATE INDEX IF NOT EXISTS idx_extension_instances_health ON extension_instances(extension_id, health_status);

-- Indexes for extension_versions table
CREATE INDEX IF NOT EXISTS idx_extension_versions_id ON extension_versions(id);
CREATE INDEX IF NOT EXISTS idx_extension_versions_created_at ON extension_versions(created_at);

-- Extension health table: Track extension health metrics
CREATE TABLE IF NOT EXISTS extension_health (
    extension_id VARCHAR(255) PRIMARY KEY,
    last_success TIMESTAMP WITH TIME ZONE,
    last_failure TIMESTAMP WITH TIME ZONE,
    success_count BIGINT DEFAULT 0,
    failure_count BIGINT DEFAULT 0,
    avg_latency_ms NUMERIC(10, 2),
    p50_latency_ms NUMERIC(10, 2),
    p95_latency_ms NUMERIC(10, 2),
    p99_latency_ms NUMERIC(10, 2),
    last_latency_ms INTEGER,
    latency_samples BIGINT DEFAULT 0,
    latency_sum BIGINT DEFAULT 0,
    latency_samples_window_start TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    circuit_breaker_state VARCHAR(32) DEFAULT 'closed' CHECK (circuit_breaker_state IN ('closed', 'open', 'half_open')),
    circuit_breaker_opened_at TIMESTAMP WITH TIME ZONE,
    circuit_breaker_failure_threshold INTEGER DEFAULT 5,
    circuit_breaker_error_rate_threshold NUMERIC(5, 2) DEFAULT 0.5 CHECK (circuit_breaker_error_rate_threshold >= 0 AND circuit_breaker_error_rate_threshold <= 1),
    circuit_breaker_window_seconds INTEGER DEFAULT 60,
    half_open_max_requests INTEGER DEFAULT 3,
    half_open_requests_count INTEGER DEFAULT 0,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (extension_id) REFERENCES extensions(id) ON DELETE CASCADE
);

-- Indexes for extension_health table
CREATE INDEX IF NOT EXISTS idx_extension_health_circuit_state ON extension_health(circuit_breaker_state);
CREATE INDEX IF NOT EXISTS idx_extension_health_last_failure ON extension_health(last_failure);
CREATE INDEX IF NOT EXISTS idx_extension_health_p95_latency ON extension_health(p95_latency_ms);

-- Extension audit log table: Audit trail for extension changes
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

-- Indexes for extension_audit_log table
CREATE INDEX IF NOT EXISTS idx_extension_audit_extension_id ON extension_audit_log(extension_id);
CREATE INDEX IF NOT EXISTS idx_extension_audit_changed_at ON extension_audit_log(changed_at);
CREATE INDEX IF NOT EXISTS idx_extension_audit_action ON extension_audit_log(action);

-- Seed data for testing (development only)
-- These match the default fixtures in router_extension_registry.erl
INSERT INTO extensions (id, type, subject, timeout_ms, retry, enabled, version) VALUES
    ('normalize_text', 'pre', 'beamline.ext.pre.normalize_text.v1', 100, 0, true, 'v1'),
    ('pii_guard', 'validator', 'beamline.ext.validate.pii_guard.v1', 200, 0, true, 'v1'),
    ('mask_pii', 'post', 'beamline.ext.post.mask_pii.v1', 150, 0, true, 'v1')
ON CONFLICT (id) DO NOTHING;

-- Initialize health records for seed extensions
INSERT INTO extension_health (extension_id, success_count, failure_count, circuit_breaker_state) VALUES
    ('normalize_text', 0, 0, 'closed'),
    ('pii_guard', 0, 0, 'closed'),
    ('mask_pii', 0, 0, 'closed')
ON CONFLICT (extension_id) DO NOTHING;

-- Comments for documentation
COMMENT ON TABLE extensions IS 'Primary storage for extension metadata';
COMMENT ON TABLE extension_versions IS 'Version history for extensions with routing rules';
COMMENT ON TABLE extension_instances IS 'Multiple instances/subjects for load balancing';
COMMENT ON TABLE extension_health IS 'Health metrics and circuit breaker state for extensions';
COMMENT ON TABLE extension_audit_log IS 'Audit trail for extension changes';

COMMENT ON COLUMN extensions.enabled IS 'Enable/disable flag for rollout management';
COMMENT ON COLUMN extensions.version IS 'Extension version (v1, v2, etc.)';
COMMENT ON COLUMN extension_health.circuit_breaker_state IS 'Circuit breaker state: closed (normal), open (failing), half_open (testing)';

