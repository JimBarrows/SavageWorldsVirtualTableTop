-- Create game_entities table for individual game objects (characters, beasts, etc.)
CREATE TABLE IF NOT EXISTS game_entities (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    plot_point_id UUID NOT NULL REFERENCES plot_points(id) ON DELETE CASCADE,
    owner_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    entity_type VARCHAR(50) NOT NULL CHECK (entity_type IN (
        'character', 'beast', 'npc', 'vehicle', 'item', 'weapon', 'armor'
    )),
    name VARCHAR(255) NOT NULL,
    description TEXT,
    data JSONB NOT NULL DEFAULT '{}'::jsonb,
    is_template BOOLEAN DEFAULT false,
    is_active BOOLEAN DEFAULT true,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX idx_game_entities_plot_point_id ON game_entities(plot_point_id);
CREATE INDEX idx_game_entities_owner_id ON game_entities(owner_id);
CREATE INDEX idx_game_entities_entity_type ON game_entities(entity_type);
CREATE INDEX idx_game_entities_name ON game_entities(name);
CREATE INDEX idx_game_entities_is_template ON game_entities(is_template);
CREATE INDEX idx_game_entities_is_active ON game_entities(is_active);
CREATE INDEX idx_game_entities_created_at ON game_entities(created_at);

-- Create GIN index for JSONB data column
CREATE INDEX idx_game_entities_data_gin ON game_entities USING gin (data);

-- Create composite indexes for common query patterns
CREATE INDEX idx_game_entities_plot_point_type ON game_entities(plot_point_id, entity_type);
CREATE INDEX idx_game_entities_owner_type ON game_entities(owner_id, entity_type);
CREATE INDEX idx_game_entities_active_templates ON game_entities(is_active, is_template) WHERE is_active = true;

-- Create trigger to automatically update updated_at
CREATE TRIGGER update_game_entities_updated_at BEFORE UPDATE
    ON game_entities FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Add comments for documentation
COMMENT ON TABLE game_entities IS 'Individual game entities like characters, beasts, items, etc.';
COMMENT ON COLUMN game_entities.id IS 'Primary key UUID';
COMMENT ON COLUMN game_entities.plot_point_id IS 'Foreign key to plot_points table';
COMMENT ON COLUMN game_entities.owner_id IS 'Foreign key to users table';
COMMENT ON COLUMN game_entities.entity_type IS 'Type of game entity';
COMMENT ON COLUMN game_entities.name IS 'Name of the entity';
COMMENT ON COLUMN game_entities.description IS 'Description of the entity';
COMMENT ON COLUMN game_entities.data IS 'JSONB data containing all entity-specific attributes';
COMMENT ON COLUMN game_entities.is_template IS 'Whether this entity is a template for creating other entities';
COMMENT ON COLUMN game_entities.is_active IS 'Whether this entity is currently active/in use';