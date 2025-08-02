-- Initial database setup
-- This file runs automatically when the PostgreSQL container starts

-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- Set default timezone
SET timezone = 'UTC';

-- Create application role with limited privileges (for production use)
CREATE ROLE swvtt_app WITH LOGIN PASSWORD 'app_password_change_me';

-- Grant necessary permissions
GRANT CONNECT ON DATABASE swvtt_db TO swvtt_app;

-- Note: Additional permissions should be granted after migrations run
-- Example (run after migrations):
-- GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO swvtt_app;
-- GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO swvtt_app;