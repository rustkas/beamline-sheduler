-- Extension Routing Rules Examples
-- Multi-tenant and Multi-environment Examples
-- Version: 1.0
-- Date: 2025-01-27

-- This file contains example SQL INSERT statements demonstrating
-- how to configure extension versions with routing rules for:
-- 1. Multi-tenant scenarios (different versions per tenant)
-- 2. Multi-environment scenarios (dev/stage/prod)
-- 3. Canary deployments (gradual rollout to subset of tenants)

-- ============================================================================
-- Example 1: Multi-tenant Routing (Different Versions per Tenant)
-- ============================================================================

-- Extension: normalize_text
-- Scenario: v1 for most tenants, v2 for premium tenants

-- Version v1 (default for all tenants)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('normalize_text', 'v1', 'beamline.ext.pre.normalize_text.v1', 100, 0, '{}', '{}', '{}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- Version v2 (only for premium tenants)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('normalize_text', 'v2', 'beamline.ext.pre.normalize_text.v2', 100, 0, '{}', '{"features": ["advanced_normalization"]}', 
     '{"tenant_id": ["tenant_premium_1", "tenant_premium_2", "tenant_enterprise"]}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- ============================================================================
-- Example 2: Multi-environment Routing (dev/stage/prod)
-- ============================================================================

-- Extension: pii_guard
-- Scenario: Different versions for dev/stage/prod environments

-- Version v1 (production)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('pii_guard', 'v1', 'beamline.ext.validate.pii_guard.v1', 200, 0, '{"strict_mode": true}', '{}', 
     '{"environment": "prod"}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- Version v2 (staging - with relaxed rules for testing)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('pii_guard', 'v2', 'beamline.ext.validate.pii_guard.v2', 200, 0, '{"strict_mode": false, "test_mode": true}', '{}', 
     '{"environment": "stage"}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- Version v3 (development - permissive for local testing)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('pii_guard', 'v3', 'beamline.ext.validate.pii_guard.v3', 200, 0, '{"strict_mode": false, "test_mode": true, "log_only": true}', '{}', 
     '{"environment": "dev"}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- ============================================================================
-- Example 3: Combined Tenant + Environment Routing
-- ============================================================================

-- Extension: mask_pii
-- Scenario: Different versions based on both tenant and environment

-- Version v1 (production for all tenants)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('mask_pii', 'v1', 'beamline.ext.post.mask_pii.v1', 150, 0, '{"masking_level": "full"}', '{}', 
     '{"environment": "prod"}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- Version v2 (staging for premium tenants only)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('mask_pii', 'v2', 'beamline.ext.post.mask_pii.v2', 150, 0, '{"masking_level": "partial", "test_mode": true}', '{}', 
     '{"environment": "stage", "tenant_id": ["tenant_premium_1", "tenant_premium_2"]}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- ============================================================================
-- Example 4: Canary Deployment (Gradual Rollout)
-- ============================================================================

-- Extension: custom_provider_openai
-- Scenario: Rollout v2 to 10% of tenants (canary), rest use v1

-- Version v1 (default for 90% of tenants)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('custom_provider_openai', 'v1', 'beamline.provider.openai.v1', 5000, 3, '{"model": "gpt-3.5-turbo"}', '{}', 
     '{}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- Version v2 (canary - 10% of tenants)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('custom_provider_openai', 'v2', 'beamline.provider.openai.v2', 5000, 3, '{"model": "gpt-4", "temperature": 0.7}', '{}', 
     '{"tenant_id": ["tenant_canary_1", "tenant_canary_2", "tenant_canary_3"]}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- ============================================================================
-- Example 5: Policy-based Routing
-- ============================================================================

-- Extension: rate_limiter
-- Scenario: Different versions based on policy_id

-- Version v1 (default policy)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('rate_limiter', 'v1', 'beamline.ext.pre.rate_limiter.v1', 50, 0, '{"rpm_limit": 100}', '{}', 
     '{}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- Version v2 (high-traffic policy)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('rate_limiter', 'v2', 'beamline.ext.pre.rate_limiter.v2', 50, 0, '{"rpm_limit": 1000}', '{}', 
     '{"policy_id": ["policy_high_traffic", "policy_enterprise"]}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- ============================================================================
-- Example 6: Complex Multi-condition Routing
-- ============================================================================

-- Extension: advanced_analyzer
-- Scenario: Multiple conditions (tenant + environment + policy)

-- Version v1 (production, premium tenants, specific policies)
INSERT INTO extension_versions (id, version, subject, timeout_ms, retry, config, metadata, routing_rules, enabled, created_at) VALUES
    ('advanced_analyzer', 'v1', 'beamline.ext.pre.advanced_analyzer.v1', 300, 1, '{"analysis_depth": "deep"}', '{}', 
     '{"environment": "prod", "tenant_id": ["tenant_premium_1", "tenant_enterprise"], "policy_id": ["policy_analytics"]}', true, CURRENT_TIMESTAMP)
ON CONFLICT (id, version) DO UPDATE SET
    subject = EXCLUDED.subject,
    routing_rules = EXCLUDED.routing_rules;

-- ============================================================================
-- Notes on Routing Rules Format
-- ============================================================================

-- Routing rules are stored as JSONB with the following structure:
-- {
--   "tenant_id": ["tenant1", "tenant2"],      -- List of tenant IDs
--   "environment": "prod",                    -- Single environment value
--   "policy_id": ["policy1", "policy2"]      -- List of policy IDs
-- }
--
-- Matching logic:
-- - If routing_rules is empty {}, version matches by default (fallback)
-- - All specified keys must match (AND logic)
-- - List values use "IN" semantics (tenant_id in list)
-- - String values use exact match (environment = "prod")
--
-- Priority:
-- - More specific routing rules (with more conditions) are checked first
-- - If multiple versions match, first match is used (order by created_at DESC)
-- - If no version matches, fallback to default version from extensions table

