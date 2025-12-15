-- Platform Constructor v1.0
-- Initial DDL for PostgreSQL (neutral schema)
-- Version: 1.0
-- Last Update: 2024-01-XX

-- Create schema
CREATE SCHEMA IF NOT EXISTS platform;

-- Projects table
CREATE TABLE IF NOT EXISTS platform.projects (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id VARCHAR(255) NOT NULL UNIQUE,
    name VARCHAR(255) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    status VARCHAR(50) DEFAULT 'active',
    metadata JSONB DEFAULT '{}'::jsonb
);

CREATE INDEX idx_projects_tenant_id ON platform.projects(tenant_id);
CREATE INDEX idx_projects_status ON platform.projects(status);

-- API keys table
CREATE TABLE IF NOT EXISTS platform.api_keys (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id VARCHAR(255) NOT NULL,
    key_hash VARCHAR(255) NOT NULL UNIQUE,
    key_prefix VARCHAR(20) NOT NULL,
    name VARCHAR(255),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP WITH TIME ZONE,
    revoked_at TIMESTAMP WITH TIME ZONE,
    last_used_at TIMESTAMP WITH TIME ZONE,
    scopes TEXT[] DEFAULT ARRAY[]::TEXT[],
    metadata JSONB DEFAULT '{}'::jsonb,
    FOREIGN KEY (tenant_id) REFERENCES platform.projects(tenant_id) ON DELETE CASCADE
);

CREATE INDEX idx_api_keys_tenant_id ON platform.api_keys(tenant_id);
CREATE INDEX idx_api_keys_key_hash ON platform.api_keys(key_hash);
CREATE INDEX idx_api_keys_key_prefix ON platform.api_keys(key_prefix);
CREATE INDEX idx_api_keys_revoked_at ON platform.api_keys(revoked_at) WHERE revoked_at IS NULL;

-- Usage events table
CREATE TABLE IF NOT EXISTS platform.usage_events (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id VARCHAR(255) NOT NULL,
    message_id VARCHAR(255) NOT NULL,
    trace_id VARCHAR(255),
    provider_id VARCHAR(255),
    event_type VARCHAR(100) NOT NULL,
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    latency_ms INTEGER,
    cost DECIMAL(10, 6),
    currency VARCHAR(10) DEFAULT 'USD',
    status VARCHAR(50),
    metadata JSONB DEFAULT '{}'::jsonb,
    FOREIGN KEY (tenant_id) REFERENCES platform.projects(tenant_id) ON DELETE CASCADE
);

CREATE INDEX idx_usage_events_tenant_id ON platform.usage_events(tenant_id);
CREATE INDEX idx_usage_events_timestamp ON platform.usage_events(timestamp);
CREATE INDEX idx_usage_events_provider_id ON platform.usage_events(provider_id);
CREATE INDEX idx_usage_events_event_type ON platform.usage_events(event_type);
CREATE INDEX idx_usage_events_trace_id ON platform.usage_events(trace_id);

-- Routing policies table
CREATE TABLE IF NOT EXISTS platform.policies (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id VARCHAR(255) NOT NULL,
    policy_id VARCHAR(255) NOT NULL,
    name VARCHAR(255) NOT NULL,
    version VARCHAR(50) DEFAULT '1.0',
    policy_json JSONB NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    is_active BOOLEAN DEFAULT TRUE,
    metadata JSONB DEFAULT '{}'::jsonb,
    FOREIGN KEY (tenant_id) REFERENCES platform.projects(tenant_id) ON DELETE CASCADE,
    UNIQUE(tenant_id, policy_id)
);

CREATE INDEX idx_policies_tenant_id ON platform.policies(tenant_id);
CREATE INDEX idx_policies_policy_id ON platform.policies(policy_id);
CREATE INDEX idx_policies_is_active ON platform.policies(is_active) WHERE is_active = TRUE;

-- Audit table
CREATE TABLE IF NOT EXISTS platform.audit_logs (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    tenant_id VARCHAR(255),
    actor VARCHAR(255) NOT NULL,
    action VARCHAR(100) NOT NULL,
    resource_type VARCHAR(100),
    resource_id VARCHAR(255),
    cp_from VARCHAR(50),
    cp_to VARCHAR(50),
    state_checksum VARCHAR(64),
    hmac_prev VARCHAR(64),
    hmac VARCHAR(64) NOT NULL,
    timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    metadata JSONB DEFAULT '{}'::jsonb
);

CREATE INDEX idx_audit_logs_tenant_id ON platform.audit_logs(tenant_id);
CREATE INDEX idx_audit_logs_actor ON platform.audit_logs(actor);
CREATE INDEX idx_audit_logs_action ON platform.audit_logs(action);
CREATE INDEX idx_audit_logs_timestamp ON platform.audit_logs(timestamp);
CREATE INDEX idx_audit_logs_cp_transition ON platform.audit_logs(cp_from, cp_to);

-- Table comments
COMMENT ON TABLE platform.projects IS 'Projects and tenants';
COMMENT ON TABLE platform.api_keys IS 'API keys for authentication';
COMMENT ON TABLE platform.usage_events IS 'System usage events';
COMMENT ON TABLE platform.policies IS 'Routing policies';
COMMENT ON TABLE platform.audit_logs IS 'Operation audit log';
