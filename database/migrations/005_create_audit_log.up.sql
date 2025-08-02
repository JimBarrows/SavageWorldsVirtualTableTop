-- Create audit_log table for tracking changes
CREATE TABLE IF NOT EXISTS audit_log (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID REFERENCES users(id) ON DELETE SET NULL,
    action VARCHAR(50) NOT NULL,
    entity_type VARCHAR(50) NOT NULL,
    entity_id UUID NOT NULL,
    entity_name VARCHAR(255),
    changes JSONB,
    metadata JSONB DEFAULT '{}'::jsonb,
    ip_address INET,
    user_agent TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create indexes for audit_log
CREATE INDEX idx_audit_log_user_id ON audit_log(user_id);
CREATE INDEX idx_audit_log_action ON audit_log(action);
CREATE INDEX idx_audit_log_entity_type ON audit_log(entity_type);
CREATE INDEX idx_audit_log_entity_id ON audit_log(entity_id);
CREATE INDEX idx_audit_log_created_at ON audit_log(created_at);
CREATE INDEX idx_audit_log_entity_composite ON audit_log(entity_type, entity_id);

-- Create partial index for recent entries
CREATE INDEX idx_audit_log_recent ON audit_log(created_at DESC) 
    WHERE created_at > (NOW() - INTERVAL '30 days');

-- Add comments
COMMENT ON TABLE audit_log IS 'Audit trail for all system changes';
COMMENT ON COLUMN audit_log.action IS 'Action performed (create, update, delete, etc.)';
COMMENT ON COLUMN audit_log.entity_type IS 'Type of entity affected';
COMMENT ON COLUMN audit_log.entity_id IS 'ID of the affected entity';
COMMENT ON COLUMN audit_log.changes IS 'JSON diff of changes made';
COMMENT ON COLUMN audit_log.metadata IS 'Additional context about the action';