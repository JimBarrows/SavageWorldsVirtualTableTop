-- Drop validation constraints
ALTER TABLE game_entities DROP CONSTRAINT IF EXISTS check_character_beast_attributes;
ALTER TABLE plot_points DROP CONSTRAINT IF EXISTS check_basic_rules_structure;

-- Drop validation functions
DROP FUNCTION IF EXISTS validate_character_attributes(JSONB);
DROP FUNCTION IF EXISTS validate_damage(JSONB);
DROP FUNCTION IF EXISTS validate_trait(JSONB);
DROP FUNCTION IF EXISTS validate_burst_template(TEXT);
DROP FUNCTION IF EXISTS validate_hindrance_severity(TEXT);
DROP FUNCTION IF EXISTS validate_rank_type(TEXT);
DROP FUNCTION IF EXISTS validate_attribute_type(TEXT);
DROP FUNCTION IF EXISTS validate_dice_type(TEXT);