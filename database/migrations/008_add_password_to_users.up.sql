-- Add password column to users table for local authentication
-- This enables local authentication alongside AWS Cognito
ALTER TABLE users ADD COLUMN IF NOT EXISTS password VARCHAR(255);

-- Update the comment to reflect the dual authentication support
COMMENT ON COLUMN users.password IS 'Hashed password for local authentication (nullable for Cognito-only users)';

-- Make password required for new users but allow null for existing Cognito users
-- We'll handle the NOT NULL constraint in application logic to support both auth methods