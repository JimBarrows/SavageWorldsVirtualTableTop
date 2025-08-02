-- Create validation functions for JSONB data

-- Function to validate dice type
CREATE OR REPLACE FUNCTION validate_dice_type(dice_value TEXT)
RETURNS BOOLEAN AS $$
BEGIN
    RETURN dice_value IN ('d4', 'd6', 'd8', 'd10', 'd12');
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to validate attribute type
CREATE OR REPLACE FUNCTION validate_attribute_type(attr_value TEXT)
RETURNS BOOLEAN AS $$
BEGIN
    RETURN attr_value IN ('strength', 'agility', 'vigor', 'smarts', 'spirit');
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to validate rank type
CREATE OR REPLACE FUNCTION validate_rank_type(rank_value TEXT)
RETURNS BOOLEAN AS $$
BEGIN
    RETURN rank_value IN ('novice', 'seasoned', 'veteran', 'heroic', 'legendary');
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to validate hindrance severity
CREATE OR REPLACE FUNCTION validate_hindrance_severity(severity_value TEXT)
RETURNS BOOLEAN AS $$
BEGIN
    RETURN severity_value IN ('minor', 'major', 'either');
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to validate burst template
CREATE OR REPLACE FUNCTION validate_burst_template(template_value TEXT)
RETURNS BOOLEAN AS $$
BEGIN
    RETURN template_value IN ('small', 'medium', 'large');
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to validate trait structure
CREATE OR REPLACE FUNCTION validate_trait(trait_data JSONB)
RETURNS BOOLEAN AS $$
BEGIN
    RETURN trait_data ? 'dice' 
        AND validate_dice_type(trait_data->>'dice')
        AND (NOT trait_data ? 'bonus' OR (trait_data->>'bonus')::INTEGER IS NOT NULL);
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to validate damage structure
CREATE OR REPLACE FUNCTION validate_damage(damage_data JSONB)
RETURNS BOOLEAN AS $$
DECLARE
    dice_item JSONB;
BEGIN
    -- Check if dice array exists
    IF NOT damage_data ? 'dice' OR jsonb_typeof(damage_data->'dice') != 'array' THEN
        RETURN FALSE;
    END IF;
    
    -- Validate each dice in the array
    FOR dice_item IN SELECT * FROM jsonb_array_elements(damage_data->'dice')
    LOOP
        IF NOT (dice_item ? 'type' AND dice_item ? 'howMany') THEN
            RETURN FALSE;
        END IF;
        IF NOT validate_dice_type(dice_item->>'type') THEN
            RETURN FALSE;
        END IF;
        IF (dice_item->>'howMany')::INTEGER IS NULL OR (dice_item->>'howMany')::INTEGER < 1 THEN
            RETURN FALSE;
        END IF;
    END LOOP;
    
    -- Validate attribute if present
    IF damage_data ? 'attribute' AND damage_data->>'attribute' IS NOT NULL THEN
        IF NOT validate_attribute_type(damage_data->>'attribute') THEN
            RETURN FALSE;
        END IF;
    END IF;
    
    RETURN TRUE;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Function to validate character/beast attributes
CREATE OR REPLACE FUNCTION validate_character_attributes(entity_data JSONB)
RETURNS BOOLEAN AS $$
BEGIN
    -- Check all required attributes exist and are valid traits
    RETURN entity_data ? 'agility' AND validate_trait(entity_data->'agility')
        AND entity_data ? 'smarts' AND validate_trait(entity_data->'smarts')
        AND entity_data ? 'spirit' AND validate_trait(entity_data->'spirit')
        AND entity_data ? 'strength' AND validate_trait(entity_data->'strength')
        AND entity_data ? 'vigor' AND validate_trait(entity_data->'vigor')
        AND entity_data ? 'name';
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Add check constraints to game_entities table for validation
ALTER TABLE game_entities 
ADD CONSTRAINT check_character_beast_attributes
CHECK (
    entity_type NOT IN ('character', 'beast', 'npc') 
    OR validate_character_attributes(data)
);

-- Add check constraint for basic rules in plot_points
ALTER TABLE plot_points
ADD CONSTRAINT check_basic_rules_structure
CHECK (
    basic_rules ? 'maximumAttributePoints' 
    AND basic_rules ? 'maximumMajorHindrances'
    AND basic_rules ? 'maximumMinorHindrances'
    AND basic_rules ? 'maximumSkillPoints'
    AND (basic_rules->>'maximumAttributePoints')::INTEGER >= 0
    AND (basic_rules->>'maximumMajorHindrances')::INTEGER >= 0
    AND (basic_rules->>'maximumMinorHindrances')::INTEGER >= 0
    AND (basic_rules->>'maximumSkillPoints')::INTEGER >= 0
);

-- Add comments
COMMENT ON FUNCTION validate_dice_type IS 'Validates dice type values (d4, d6, d8, d10, d12)';
COMMENT ON FUNCTION validate_attribute_type IS 'Validates attribute type values';
COMMENT ON FUNCTION validate_rank_type IS 'Validates rank values';
COMMENT ON FUNCTION validate_trait IS 'Validates trait structure with dice and optional bonus';
COMMENT ON FUNCTION validate_damage IS 'Validates damage structure with dice array and optional attribute';
COMMENT ON FUNCTION validate_character_attributes IS 'Validates character/beast has all required attributes';