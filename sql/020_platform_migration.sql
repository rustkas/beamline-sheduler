-- Migrate schema from beamline to platform (neutral naming)
-- Safe to run multiple times; moves tables if they exist in beamline schema.

BEGIN;

-- Create target schema
CREATE SCHEMA IF NOT EXISTS platform;

-- Move core domain tables
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='beamline' AND table_name='projects') THEN
    EXECUTE 'ALTER TABLE beamline.projects SET SCHEMA platform';
  END IF;
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='beamline' AND table_name='api_keys') THEN
    EXECUTE 'ALTER TABLE beamline.api_keys SET SCHEMA platform';
  END IF;
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='beamline' AND table_name='usage_events') THEN
    EXECUTE 'ALTER TABLE beamline.usage_events SET SCHEMA platform';
  END IF;
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='beamline' AND table_name='policies') THEN
    EXECUTE 'ALTER TABLE beamline.policies SET SCHEMA platform';
  END IF;
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='beamline' AND table_name='audit_logs') THEN
    EXECUTE 'ALTER TABLE beamline.audit_logs SET SCHEMA platform';
  END IF;
END $$;

-- Optional: update comments to reflect neutral description
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='platform' AND table_name='projects') THEN
    EXECUTE 'COMMENT ON TABLE platform.projects IS ''Projects and tenants''';
  END IF;
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='platform' AND table_name='api_keys') THEN
    EXECUTE 'COMMENT ON TABLE platform.api_keys IS ''API keys for authentication''';
  END IF;
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='platform' AND table_name='usage_events') THEN
    EXECUTE 'COMMENT ON TABLE platform.usage_events IS ''System usage events''';
  END IF;
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='platform' AND table_name='policies') THEN
    EXECUTE 'COMMENT ON TABLE platform.policies IS ''Routing policies''';
  END IF;
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema='platform' AND table_name='audit_logs') THEN
    EXECUTE 'COMMENT ON TABLE platform.audit_logs IS ''Operation audit log''';
  END IF;
END $$;

COMMIT;

