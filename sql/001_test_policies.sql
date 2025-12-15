-- Beamline Constructor v1.0
-- Test Policies for Local Development
-- Version: 1.0
-- Last Update: 2025-01-27

-- Test tenant and project setup
DO $$
DECLARE
  test_tenant_id UUID := '00000000-0000-0000-0000-000000000001';
  test_project_id UUID := '00000000-0000-0000-0000-000000000002';
BEGIN
  -- Insert test tenant project
  INSERT INTO beamline.projects (
    project_id,
    tenant_id,
    name,
    status,
    created_at,
    updated_at
  ) VALUES (
    test_project_id,
    test_tenant_id,
    'test-project',
    'active',
    NOW(),
    NOW()
  ) ON CONFLICT (project_id) DO NOTHING;

  -- Insert test routing policy
  INSERT INTO beamline.policies (
    policy_id,
    tenant_id,
    project_id,
    name,
    priority,
    conditions,
    actions,
    is_active,
    created_at,
    updated_at
  ) VALUES (
    '00000000-0000-0000-0000-000000000003',
    test_tenant_id,
    test_project_id,
    'test-default-policy',
    100,
    '{"match_all": true}'::jsonb,
    '{"route_to": "openai", "model": "gpt-4"}'::jsonb,
    true,
    NOW(),
    NOW()
  ) ON CONFLICT (policy_id) DO NOTHING;

  RAISE NOTICE 'Test policies initialized for tenant: %', test_tenant_id;
END $$;

-- Grant necessary permissions for test user
GRANT USAGE ON SCHEMA beamline TO beamline;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA beamline TO beamline;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA beamline TO beamline;

