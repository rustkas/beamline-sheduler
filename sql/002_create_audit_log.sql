-- Audit Log Table Migration
-- Version: 1.0
-- Date: 2025-11-13
-- Description: Creates audit log table for tracking all RBAC and policy operations

-- Audit log table: Tracks all operations for compliance and security
CREATE TABLE IF NOT EXISTS audit_log (
    audit_id BIGSERIAL PRIMARY KEY,
    ts TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    tenant_id VARCHAR(255) NOT NULL,
    user_id VARCHAR(255) NOT NULL,
    action VARCHAR(64) NOT NULL,
    resource_type VARCHAR(64) NOT NULL,
    resource_id VARCHAR(255),
    success BOOLEAN NOT NULL DEFAULT true,
    error_code VARCHAR(64),
    error_message TEXT,
    trace_id VARCHAR(255),
    ip_address INET,
    user_agent TEXT,
    context JSONB,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for performance and querying
CREATE INDEX IF NOT EXISTS idx_audit_log_tenant_ts ON audit_log(tenant_id, ts DESC);
CREATE INDEX IF NOT EXISTS idx_audit_log_user_ts ON audit_log(user_id, ts DESC);
CREATE INDEX IF NOT EXISTS idx_audit_log_action ON audit_log(action);
CREATE INDEX IF NOT EXISTS idx_audit_log_resource ON audit_log(resource_type, resource_id);
CREATE INDEX IF NOT EXISTS idx_audit_log_trace ON audit_log(trace_id) WHERE trace_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_audit_log_success ON audit_log(success, ts DESC);
CREATE INDEX IF NOT EXISTS idx_audit_log_created ON audit_log(created_at DESC);

-- Partitioning by month for large-scale deployments (optional, uncomment if needed)
-- CREATE TABLE audit_log_2025_11 PARTITION OF audit_log
--     FOR VALUES FROM ('2025-11-01') TO ('2025-12-01');

-- Retention policy: Create function to clean old audit logs
CREATE OR REPLACE FUNCTION cleanup_audit_log(retention_days INTEGER DEFAULT 90)
RETURNS INTEGER AS $$
DECLARE
    deleted_count INTEGER;
BEGIN
    DELETE FROM audit_log
    WHERE created_at < CURRENT_TIMESTAMP - (retention_days || ' days')::INTERVAL;
    
    GET DIAGNOSTICS deleted_count = ROW_COUNT;
    RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;

-- Comments
COMMENT ON TABLE audit_log IS 'Audit trail for all RBAC and policy operations';
COMMENT ON COLUMN audit_log.context IS 'Additional context data in JSON format';
COMMENT ON COLUMN audit_log.trace_id IS 'Correlation ID for distributed tracing';
COMMENT ON FUNCTION cleanup_audit_log IS 'Cleans up audit logs older than retention_days (default: 90)';

