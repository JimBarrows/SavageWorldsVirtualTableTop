-- Migration: Remove username column and related constraints
-- Description: Remove username column from users table as we're using email as the primary identifier

-- First, we need to ensure all existing users have an email
-- This should already be the case, but let's verify
DO $$
BEGIN
    IF EXISTS (SELECT 1 FROM users WHERE email IS NULL OR email = '') THEN
        RAISE EXCEPTION 'Cannot remove username column: some users have no email address';
    END IF;
END $$;

-- Drop the username unique constraint
ALTER TABLE users DROP CONSTRAINT IF EXISTS users_username_key;

-- Drop the username index
DROP INDEX IF EXISTS idx_users_username;

-- Drop the username column
ALTER TABLE users DROP COLUMN IF EXISTS username;

-- Update the user validation function to not check username
CREATE OR REPLACE FUNCTION validate_user_data()
RETURNS TRIGGER AS $$
BEGIN
    -- Validate email
    IF NEW.email IS NULL OR NEW.email = '' THEN
        RAISE EXCEPTION 'Email is required';
    END IF;
    
    IF NOT NEW.email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$' THEN
        RAISE EXCEPTION 'Invalid email format';
    END IF;
    
    -- Validate password (only on insert or if changed)
    IF TG_OP = 'INSERT' OR (TG_OP = 'UPDATE' AND NEW.password IS DISTINCT FROM OLD.password) THEN
        IF LENGTH(NEW.password) < 60 THEN -- bcrypt hashes are at least 60 chars
            RAISE EXCEPTION 'Password must be properly hashed';
        END IF;
    END IF;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Add comment to document the change
COMMENT ON TABLE users IS 'Users table - email is now the primary identifier (username column removed in migration 009)';