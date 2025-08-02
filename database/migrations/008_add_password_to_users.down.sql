-- Remove password column to revert to Cognito-only authentication
ALTER TABLE users DROP COLUMN IF EXISTS password;