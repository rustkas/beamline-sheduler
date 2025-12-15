-- Quota Tables Migration
-- Version: 1.0
-- Date: 2025-11-13
-- Description: Creates tables for quota management per tenant

-- Tenant quotas table: Defines quota limits per tenant
CREATE TABLE IF NOT EXISTS tenant_quotas (
    tenant_id VARCHAR(255) PRIMARY KEY,
    max_policies INTEGER NOT NULL DEFAULT 10,
    max_rules_per_policy INTEGER NOT NULL DEFAULT 50,
    max_providers_per_policy INTEGER NOT NULL DEFAULT 20,
    max_rate_requests_per_minute INTEGER NOT NULL DEFAULT 100,
    tier VARCHAR(32) NOT NULL DEFAULT 'basic' CHECK (tier IN ('basic', 'premium', 'enterprise')),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Quota usage tracking: Tracks current usage per tenant
CREATE TABLE IF NOT EXISTS quota_usage (
    tenant_id VARCHAR(255) PRIMARY KEY REFERENCES tenant_quotas(tenant_id) ON DELETE CASCADE,
    current_policies INTEGER NOT NULL DEFAULT 0,
    current_rules INTEGER NOT NULL DEFAULT 0,
    current_providers INTEGER NOT NULL DEFAULT 0,
    last_updated TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Quota violations log: Tracks quota violations for monitoring
CREATE TABLE IF NOT EXISTS quota_violations (
    violation_id BIGSERIAL PRIMARY KEY,
    tenant_id VARCHAR(255) NOT NULL REFERENCES tenant_quotas(tenant_id) ON DELETE CASCADE,
    user_id VARCHAR(255),
    quota_type VARCHAR(64) NOT NULL,
    limit_value INTEGER NOT NULL,
    current_value INTEGER NOT NULL,
    resource_id VARCHAR(255),
    ts TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    trace_id VARCHAR(255)
);

-- Indexes for performance
CREATE INDEX IF NOT EXISTS idx_quota_violations_tenant_ts ON quota_violations(tenant_id, ts DESC);
CREATE INDEX IF NOT EXISTS idx_quota_violations_type ON quota_violations(quota_type);
CREATE INDEX IF NOT EXISTS idx_quota_violations_trace ON quota_violations(trace_id) WHERE trace_id IS NOT NULL;

-- Function to check quota before operation
CREATE OR REPLACE FUNCTION check_quota(
    p_tenant_id VARCHAR(255),
    p_quota_type VARCHAR(64),
    p_increment INTEGER DEFAULT 1
)
RETURNS TABLE(
    allowed BOOLEAN,
    current_value INTEGER,
    max_value INTEGER,
    remaining INTEGER
) AS $$
DECLARE
    v_quota tenant_quotas%ROWTYPE;
    v_usage quota_usage%ROWTYPE;
    v_current INTEGER;
    v_max INTEGER;
    v_allowed BOOLEAN;
BEGIN
    -- Get quota limits
    SELECT * INTO v_quota FROM tenant_quotas WHERE tenant_id = p_tenant_id;
    
    IF NOT FOUND THEN
        -- Use default quota
        v_quota.max_policies := 10;
        v_quota.max_rules_per_policy := 50;
        v_quota.max_providers_per_policy := 20;
    END IF;
    
    -- Get current usage
    SELECT * INTO v_usage FROM quota_usage WHERE tenant_id = p_tenant_id;
    
    IF NOT FOUND THEN
        INSERT INTO quota_usage (tenant_id) VALUES (p_tenant_id)
        ON CONFLICT (tenant_id) DO NOTHING;
        SELECT * INTO v_usage FROM quota_usage WHERE tenant_id = p_tenant_id;
    END IF;
    
    -- Determine current and max values based on quota type
    CASE p_quota_type
        WHEN 'policies' THEN
            v_current := v_usage.current_policies;
            v_max := v_quota.max_policies;
        WHEN 'rules_per_policy' THEN
            v_current := v_usage.current_rules;
            v_max := v_quota.max_rules_per_policy;
        WHEN 'providers_per_policy' THEN
            v_current := v_usage.current_providers;
            v_max := v_quota.max_providers_per_policy;
        ELSE
            RAISE EXCEPTION 'Unknown quota type: %', p_quota_type;
    END CASE;
    
    -- Check if operation is allowed
    v_allowed := (v_current + p_increment) <= v_max;
    
    RETURN QUERY SELECT
        v_allowed,
        v_current,
        v_max,
        GREATEST(0, v_max - v_current)::INTEGER;
END;
$$ LANGUAGE plpgsql;

-- Function to update quota usage
CREATE OR REPLACE FUNCTION update_quota_usage(
    p_tenant_id VARCHAR(255),
    p_quota_type VARCHAR(64),
    p_delta INTEGER
)
RETURNS VOID AS $$
BEGIN
    INSERT INTO quota_usage (tenant_id)
    VALUES (p_tenant_id)
    ON CONFLICT (tenant_id) DO NOTHING;
    
    CASE p_quota_type
        WHEN 'policies' THEN
            UPDATE quota_usage
            SET current_policies = GREATEST(0, current_policies + p_delta),
                last_updated = CURRENT_TIMESTAMP
            WHERE tenant_id = p_tenant_id;
        WHEN 'rules_per_policy' THEN
            UPDATE quota_usage
            SET current_rules = GREATEST(0, current_rules + p_delta),
                last_updated = CURRENT_TIMESTAMP
            WHERE tenant_id = p_tenant_id;
        WHEN 'providers_per_policy' THEN
            UPDATE quota_usage
            SET current_providers = GREATEST(0, current_providers + p_delta),
                last_updated = CURRENT_TIMESTAMP
            WHERE tenant_id = p_tenant_id;
    END CASE;
END;
$$ LANGUAGE plpgsql;

-- Comments
COMMENT ON TABLE tenant_quotas IS 'Defines quota limits per tenant';
COMMENT ON TABLE quota_usage IS 'Tracks current quota usage per tenant';
COMMENT ON TABLE quota_violations IS 'Logs quota violations for monitoring';
COMMENT ON FUNCTION check_quota IS 'Checks if quota operation is allowed';
COMMENT ON FUNCTION update_quota_usage IS 'Updates quota usage counters';

