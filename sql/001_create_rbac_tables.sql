-- RBAC Tables Migration
-- Version: 1.0
-- Date: 2025-11-13
-- Description: Creates tables for Role-Based Access Control (RBAC) system

-- Roles table: Defines available roles (admin, operator, viewer)
CREATE TABLE IF NOT EXISTS rbac_roles (
    role_id VARCHAR(64) PRIMARY KEY,
    role_name VARCHAR(128) NOT NULL UNIQUE,
    description TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Permissions table: Defines available permissions
CREATE TABLE IF NOT EXISTS rbac_permissions (
    permission_id VARCHAR(64) PRIMARY KEY,
    permission_name VARCHAR(128) NOT NULL UNIQUE,
    resource_type VARCHAR(64) NOT NULL,
    action VARCHAR(64) NOT NULL,
    description TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(resource_type, action)
);

-- Role-Permission mapping: Defines which permissions each role has
CREATE TABLE IF NOT EXISTS rbac_role_permissions (
    role_id VARCHAR(64) NOT NULL REFERENCES rbac_roles(role_id) ON DELETE CASCADE,
    permission_id VARCHAR(64) NOT NULL REFERENCES rbac_permissions(permission_id) ON DELETE CASCADE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (role_id, permission_id)
);

-- User-Role mapping: Defines which roles each user has per tenant
CREATE TABLE IF NOT EXISTS rbac_user_roles (
    user_id VARCHAR(255) NOT NULL,
    tenant_id VARCHAR(255) NOT NULL,
    role_id VARCHAR(64) NOT NULL REFERENCES rbac_roles(role_id) ON DELETE CASCADE,
    assigned_by VARCHAR(255),
    assigned_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP WITH TIME ZONE,
    PRIMARY KEY (user_id, tenant_id, role_id)
);

-- Indexes for performance
CREATE INDEX IF NOT EXISTS idx_user_roles_user_tenant ON rbac_user_roles(user_id, tenant_id);
CREATE INDEX IF NOT EXISTS idx_user_roles_tenant ON rbac_user_roles(tenant_id);
CREATE INDEX IF NOT EXISTS idx_user_roles_expires ON rbac_user_roles(expires_at) WHERE expires_at IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_role_permissions_role ON rbac_role_permissions(role_id);
CREATE INDEX IF NOT EXISTS idx_role_permissions_permission ON rbac_role_permissions(permission_id);

-- Insert default roles
INSERT INTO rbac_roles (role_id, role_name, description) VALUES
    ('admin', 'Administrator', 'Full access to all resources and administrative functions'),
    ('operator', 'Operator', 'Read and write access to policies, read-only access to metrics'),
    ('viewer', 'Viewer', 'Read-only access to policies and metrics')
ON CONFLICT (role_id) DO NOTHING;

-- Insert default permissions
INSERT INTO rbac_permissions (permission_id, permission_name, resource_type, action, description) VALUES
    ('policy:read', 'Read Policy', 'policy', 'read', 'Read access to routing policies'),
    ('policy:write', 'Write Policy', 'policy', 'write', 'Create and update routing policies'),
    ('policy:delete', 'Delete Policy', 'policy', 'delete', 'Delete routing policies'),
    ('config:read', 'Read Config', 'config', 'read', 'Read access to configuration'),
    ('config:write', 'Write Config', 'config', 'write', 'Modify configuration settings'),
    ('metrics:read', 'Read Metrics', 'metrics', 'read', 'Read access to metrics and telemetry'),
    ('audit:read', 'Read Audit', 'audit', 'read', 'Read access to audit logs')
ON CONFLICT (permission_id) DO NOTHING;

-- Map permissions to roles
-- Admin: all permissions
INSERT INTO rbac_role_permissions (role_id, permission_id) VALUES
    ('admin', 'policy:read'),
    ('admin', 'policy:write'),
    ('admin', 'policy:delete'),
    ('admin', 'config:read'),
    ('admin', 'config:write'),
    ('admin', 'metrics:read'),
    ('admin', 'audit:read')
ON CONFLICT (role_id, permission_id) DO NOTHING;

-- Operator: read/write policies, read metrics
INSERT INTO rbac_role_permissions (role_id, permission_id) VALUES
    ('operator', 'policy:read'),
    ('operator', 'policy:write'),
    ('operator', 'metrics:read')
ON CONFLICT (role_id, permission_id) DO NOTHING;

-- Viewer: read-only access
INSERT INTO rbac_role_permissions (role_id, permission_id) VALUES
    ('viewer', 'policy:read'),
    ('viewer', 'metrics:read')
ON CONFLICT (role_id, permission_id) DO NOTHING;

-- Comments
COMMENT ON TABLE rbac_roles IS 'Defines available RBAC roles (admin, operator, viewer)';
COMMENT ON TABLE rbac_permissions IS 'Defines available permissions for resources';
COMMENT ON TABLE rbac_role_permissions IS 'Maps permissions to roles';
COMMENT ON TABLE rbac_user_roles IS 'Maps users to roles per tenant';

