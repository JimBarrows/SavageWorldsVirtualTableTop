-- Rollback: Re-add username column and constraints

-- Add username column back
ALTER TABLE users ADD COLUMN username VARCHAR(30);

-- Generate usernames from email for existing users
UPDATE users SET username = LOWER(SPLIT_PART(email, '@', 1));

-- Make username NOT NULL after populating
ALTER TABLE users ALTER COLUMN username SET NOT NULL;

-- Add back unique constraint
ALTER TABLE users ADD CONSTRAINT users_username_key UNIQUE (username);

-- Add back index
CREATE INDEX idx_users_username ON users(username);

-- Restore original validation function
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
    
    -- Validate username
    IF NEW.username IS NULL OR NEW.username = '' THEN
        RAISE EXCEPTION 'Username is required';
    END IF;
    
    IF NOT NEW.username ~* '^[a-zA-Z0-9_-]{3,30}$' THEN
        RAISE EXCEPTION 'Username must be 3-30 characters and contain only letters, numbers, underscores, and hyphens';
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

-- Remove comment
COMMENT ON TABLE users IS NULL;