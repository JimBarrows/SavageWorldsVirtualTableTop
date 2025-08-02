-- Create plot_points table
CREATE TABLE IF NOT EXISTS plot_points (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    owner_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    basic_rules JSONB NOT NULL DEFAULT '{
        "maximumAttributePoints": 5,
        "maximumMajorHindrances": 1,
        "maximumMinorHindrances": 2,
        "maximumSkillPoints": 12
    }'::jsonb,
    air_vehicles JSONB DEFAULT '[]'::jsonb,
    ammunition JSONB DEFAULT '[]'::jsonb,
    arcane_backgrounds JSONB DEFAULT '[]'::jsonb,
    armor JSONB DEFAULT '[]'::jsonb,
    beasts JSONB DEFAULT '[]'::jsonb,
    characters JSONB DEFAULT '[]'::jsonb,
    edges JSONB DEFAULT '[]'::jsonb,
    gear_eras JSONB DEFAULT '[]'::jsonb,
    gear_kinds JSONB DEFAULT '[]'::jsonb,
    ground_vehicles JSONB DEFAULT '[]'::jsonb,
    hand_weapons JSONB DEFAULT '[]'::jsonb,
    hindrances JSONB DEFAULT '[]'::jsonb,
    mundane_items JSONB DEFAULT '[]'::jsonb,
    powers JSONB DEFAULT '[]'::jsonb,
    races JSONB DEFAULT '[]'::jsonb,
    ranged_weapons JSONB DEFAULT '[]'::jsonb,
    setting_rules JSONB DEFAULT '[]'::jsonb,
    skills JSONB DEFAULT '[]'::jsonb,
    special_weapons JSONB DEFAULT '[]'::jsonb,
    vehicle_and_at_mounted_weapons JSONB DEFAULT '[]'::jsonb,
    water_vehicles JSONB DEFAULT '[]'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX idx_plot_points_owner_id ON plot_points(owner_id);
CREATE INDEX idx_plot_points_name ON plot_points(name);
CREATE INDEX idx_plot_points_created_at ON plot_points(created_at);
CREATE INDEX idx_plot_points_updated_at ON plot_points(updated_at);

-- Create GIN indexes for JSONB columns to enable efficient querying
CREATE INDEX idx_plot_points_characters_gin ON plot_points USING gin (characters);
CREATE INDEX idx_plot_points_beasts_gin ON plot_points USING gin (beasts);
CREATE INDEX idx_plot_points_edges_gin ON plot_points USING gin (edges);
CREATE INDEX idx_plot_points_hindrances_gin ON plot_points USING gin (hindrances);
CREATE INDEX idx_plot_points_skills_gin ON plot_points USING gin (skills);
CREATE INDEX idx_plot_points_powers_gin ON plot_points USING gin (powers);

-- Create trigger to automatically update updated_at
CREATE TRIGGER update_plot_points_updated_at BEFORE UPDATE
    ON plot_points FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Add comments for documentation
COMMENT ON TABLE plot_points IS 'Plot points containing all game settings and configurations';
COMMENT ON COLUMN plot_points.id IS 'Primary key UUID';
COMMENT ON COLUMN plot_points.owner_id IS 'Foreign key to users table';
COMMENT ON COLUMN plot_points.name IS 'Name of the plot point';
COMMENT ON COLUMN plot_points.description IS 'Description of the plot point';
COMMENT ON COLUMN plot_points.basic_rules IS 'Basic game rules configuration';
COMMENT ON COLUMN plot_points.characters IS 'Array of character definitions';
COMMENT ON COLUMN plot_points.beasts IS 'Array of beast/creature definitions';
COMMENT ON COLUMN plot_points.edges IS 'Array of edge (advantage) definitions';
COMMENT ON COLUMN plot_points.hindrances IS 'Array of hindrance definitions';
COMMENT ON COLUMN plot_points.skills IS 'Array of skill definitions';
COMMENT ON COLUMN plot_points.powers IS 'Array of power definitions';