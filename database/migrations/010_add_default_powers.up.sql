-- Create a function to add default powers to a plot point
CREATE OR REPLACE FUNCTION add_default_powers_to_plot_point()
RETURNS TRIGGER AS $$
BEGIN
    -- Only add default powers if the powers array is empty
    IF NEW.powers = '[]'::jsonb THEN
        NEW.powers = '[
            {
                "name": "Arcane Protection",
                "description": "Opposing powers suffer a penalty",
                "powerPoints": 1,
                "range": "Touch",
                "duration": "5 rounds",
                "trappings": "Mystical glow, runes, sigils",
                "modifiers": [],
                "rank": "Novice"
            },
            {
                "name": "Bolt",
                "description": "Hurls damaging bolts of energy",
                "powerPoints": 1,
                "range": "Smarts x2",
                "duration": "Instant",
                "trappings": "Fire, ice, light, darkness, colored bolts",
                "modifiers": [],
                "rank": "Novice"
            },
            {
                "name": "Boost/Lower Trait",
                "description": "Increases or decreases a trait",
                "powerPoints": 2,
                "range": "Smarts",
                "duration": "5 rounds",
                "trappings": "Physical change, glowing aura, potions",
                "modifiers": [],
                "rank": "Novice"
            },
            {
                "name": "Detect/Conceal Arcana",
                "description": "Detects or conceals magical effects",
                "powerPoints": 2,
                "range": "Sight",
                "duration": "5 rounds",
                "trappings": "Waving hands, whispered words",
                "modifiers": [],
                "rank": "Novice"
            },
            {
                "name": "Healing",
                "description": "Restores wounds",
                "powerPoints": 3,
                "range": "Touch",
                "duration": "Instant",
                "trappings": "Laying on hands, prayer, mystic energy",
                "modifiers": [],
                "rank": "Novice"
            }
        ]'::jsonb;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create trigger to add default powers on plot point creation
CREATE TRIGGER add_default_powers_on_insert
    BEFORE INSERT ON plot_points
    FOR EACH ROW
    EXECUTE FUNCTION add_default_powers_to_plot_point();

-- Update existing plot points with empty powers to have default powers
UPDATE plot_points
SET powers = '[
    {
        "name": "Arcane Protection",
        "description": "Opposing powers suffer a penalty",
        "powerPoints": 1,
        "range": "Touch",
        "duration": "5 rounds",
        "trappings": "Mystical glow, runes, sigils",
        "modifiers": [],
        "rank": "Novice"
    },
    {
        "name": "Bolt",
        "description": "Hurls damaging bolts of energy",
        "powerPoints": 1,
        "range": "Smarts x2",
        "duration": "Instant",
        "trappings": "Fire, ice, light, darkness, colored bolts",
        "modifiers": [],
        "rank": "Novice"
    },
    {
        "name": "Boost/Lower Trait",
        "description": "Increases or decreases a trait",
        "powerPoints": 2,
        "range": "Smarts",
        "duration": "5 rounds",
        "trappings": "Physical change, glowing aura, potions",
        "modifiers": [],
        "rank": "Novice"
    },
    {
        "name": "Detect/Conceal Arcana",
        "description": "Detects or conceals magical effects",
        "powerPoints": 2,
        "range": "Sight",
        "duration": "5 rounds",
        "trappings": "Waving hands, whispered words",
        "modifiers": [],
        "rank": "Novice"
    },
    {
        "name": "Healing",
        "description": "Restores wounds",
        "powerPoints": 3,
        "range": "Touch",
        "duration": "Instant",
        "trappings": "Laying on hands, prayer, mystic energy",
        "modifiers": [],
        "rank": "Novice"
    }
]'::jsonb
WHERE powers = '[]'::jsonb;