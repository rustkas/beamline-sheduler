-- DevState tables for state and audit HMAC chain
-- Applies to public schema to match unqualified queries in DevState server

BEGIN;

-- Enable pgcrypto for checksum defaults (safe if already installed)
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Current state singleton
CREATE TABLE IF NOT EXISTS public.state_current (
    id INTEGER PRIMARY KEY,
    json JSONB NOT NULL DEFAULT '{}'::jsonb,
    checksum VARCHAR(128) NOT NULL DEFAULT 'sha256:' || encode(digest('{}', 'sha256'), 'hex'),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Ensure singleton row exists
INSERT INTO public.state_current (id, json, checksum)
VALUES (1, '{}'::jsonb, 'sha256:' || encode(digest('{}', 'sha256'), 'hex'))
ON CONFLICT (id) DO NOTHING;

-- History entries with HMAC chain
CREATE TABLE IF NOT EXISTS public.history_entries (
    id BIGSERIAL PRIMARY KEY,
    ts TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    actor VARCHAR(255) NOT NULL,
    action VARCHAR(100) NOT NULL,
    cp_from VARCHAR(50),
    cp_to VARCHAR(50),
    state_checksum VARCHAR(64),
    hmac_prev VARCHAR(64),
    hmac VARCHAR(64) NOT NULL,
    metadata JSONB DEFAULT '{}'::jsonb
);

CREATE INDEX IF NOT EXISTS idx_history_ts ON public.history_entries(ts);

COMMIT;

