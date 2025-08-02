-- Seed test data for SWVTT

-- Insert test users
INSERT INTO users (id, email, avatar_url) VALUES
    ('a0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e', 'gm@swvtt.test',  'https://example.com/avatars/gm.png'),
    ('a0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e', 'player1@swvtt.test',  'https://example.com/avatars/player1.png'),
    ('a0eebc99-3333-4ef8-8bb6-2d1a1f5c5e1e', 'player2@swvtt.test',  'https://example.com/avatars/player2.png');

-- Insert test plot point
INSERT INTO plot_points (id, owner_id, name, description, basic_rules) VALUES
    ('b0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'a0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'Deadlands: The Weird West',
     'A Savage Worlds campaign set in the Weird West',
     '{
        "maximumAttributePoints": 5,
        "maximumMajorHindrances": 1,
        "maximumMinorHindrances": 2,
        "maximumSkillPoints": 15
     }'::jsonb);

-- Update plot point with game data
UPDATE plot_points
SET
    skills = '[
        {"name": "Fighting", "description": "Proficiency with melee weapons", "attribute": "agility"},
        {"name": "Shooting", "description": "Proficiency with ranged weapons", "attribute": "agility"},
        {"name": "Notice", "description": "General awareness and perception", "attribute": "smarts"},
        {"name": "Stealth", "description": "Moving quietly and hiding", "attribute": "agility"},
        {"name": "Persuasion", "description": "Convincing others", "attribute": "spirit"},
        {"name": "Intimidation", "description": "Frightening or coercing others", "attribute": "spirit"},
        {"name": "Investigation", "description": "Finding clues and solving mysteries", "attribute": "smarts"},
        {"name": "Healing", "description": "Treating wounds and injuries", "attribute": "smarts"},
        {"name": "Riding", "description": "Controlling mounts and vehicles", "attribute": "agility"},
        {"name": "Survival", "description": "Finding food, water, and shelter", "attribute": "smarts"}
    ]'::jsonb,
    edges = '[
        {
            "name": "Quick Draw",
            "description": "Draw weapon as a free action",
            "category": {"name": "Combat", "description": "Combat-related advantages"},
            "requirements": [{"name": "Agility d8", "description": "Minimum Agility of d8"}],
            "effects": "Character can draw a weapon as a free action"
        },
        {
            "name": "Marksman",
            "description": "Increased accuracy at range",
            "category": {"name": "Combat", "description": "Combat-related advantages"},
            "requirements": [{"name": "Shooting d8", "description": "Minimum Shooting of d8"}],
            "effects": "+1 to Shooting rolls if character doesnt move"
        }
    ]'::jsonb,
    hindrances = '[
        {"name": "Bad Luck", "description": "One less Benny per session", "severity": "major"},
        {"name": "Curious", "description": "Character wants to know about everything", "severity": "major"},
        {"name": "Loyal", "description": "The hero tries to never betray or disappoint his friends", "severity": "minor"},
        {"name": "Phobia", "description": "Irrational fear of something", "severity": "either"}
    ]'::jsonb,
    hand_weapons = '[
        {
            "name": "Bowie Knife",
            "description": "Large fighting knife",
            "cost": 25,
            "weight": 1,
            "damage": {"dice": [{"type": "d6", "howMany": 1}], "attribute": "strength"}
        },
        {
            "name": "Cavalry Saber",
            "description": "Military sword",
            "cost": 200,
            "weight": 3,
            "damage": {"dice": [{"type": "d8", "howMany": 1}], "attribute": "strength"}
        }
    ]'::jsonb,
    ranged_weapons = '[
        {
            "name": "Colt Peacemaker",
            "description": "Single Action Army revolver",
            "cost": 200,
            "weight": 3,
            "shortRange": 12,
            "mediumRange": 24,
            "longRange": 48,
            "damage": {"dice": [{"type": "d6", "howMany": 2}]},
            "rateOfFire": 1,
            "shots": 6,
            "minimumStrength": "d4"
        },
        {
            "name": "Winchester 73",
            "description": "Lever action rifle",
            "cost": 300,
            "weight": 8,
            "shortRange": 24,
            "mediumRange": 48,
            "longRange": 96,
            "damage": {"dice": [{"type": "d8", "howMany": 2}]},
            "rateOfFire": 1,
            "shots": 15,
            "minimumStrength": "d6"
        }
    ]'::jsonb,
    setting_rules = '[
        {"name": "Gritty Damage", "description": "Injuries are more severe and take longer to heal"},
        {"name": "No Power Points", "description": "Powers use a different system instead of Power Points"}
    ]'::jsonb
WHERE id = 'b0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e';

-- Insert test characters as game entities
INSERT INTO game_entities (id, plot_point_id, owner_id, entity_type, name, description, data, is_template) VALUES
    -- Template character
    ('c0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'b0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'a0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'character',
     'Typical Gunslinger',
     'A template for creating gunslinger characters',
     '{
        "name": "Typical Gunslinger",
        "agility": {"dice": "d10", "bonus": 0},
        "smarts": {"dice": "d6", "bonus": 0},
        "spirit": {"dice": "d6", "bonus": 0},
        "strength": {"dice": "d6", "bonus": 0},
        "vigor": {"dice": "d8", "bonus": 0},
        "pace": 6,
        "charisma": 0,
        "skills": [
            {"name": "Fighting", "dice": "d6", "attribute": "agility"},
            {"name": "Shooting", "dice": "d10", "attribute": "agility"},
            {"name": "Notice", "dice": "d6", "attribute": "smarts"},
            {"name": "Riding", "dice": "d8", "attribute": "agility"}
        ],
        "edges": [
            {"edge": {"name": "Quick Draw"}, "description": "Fast on the draw"},
            {"edge": {"name": "Marksman"}, "description": "Deadly accurate"}
        ],
        "hindrances": [
            {"hindrance": {"name": "Loyal"}, "selectedSeverity": "minor", "description": "Loyal to friends"},
            {"hindrance": {"name": "Bad Luck"}, "selectedSeverity": "major", "description": "Things just go wrong"}
        ]
     }'::jsonb,
     true),

    -- Player character
    ('c0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e',
     'b0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'a0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e',
     'character',
     'Jake "Dead Eye" Morrison',
     'Veteran gunslinger with a mysterious past',
     '{
        "name": "Jake \"Dead Eye\" Morrison",
        "agility": {"dice": "d12", "bonus": 0},
        "smarts": {"dice": "d6", "bonus": 0},
        "spirit": {"dice": "d8", "bonus": 0},
        "strength": {"dice": "d6", "bonus": 0},
        "vigor": {"dice": "d8", "bonus": 0},
        "pace": 6,
        "charisma": -2,
        "skills": [
            {"name": "Fighting", "dice": "d8", "attribute": "agility"},
            {"name": "Shooting", "dice": "d12", "attribute": "agility", "bonus": 2},
            {"name": "Notice", "dice": "d8", "attribute": "smarts"},
            {"name": "Intimidation", "dice": "d8", "attribute": "spirit"},
            {"name": "Riding", "dice": "d8", "attribute": "agility"},
            {"name": "Stealth", "dice": "d6", "attribute": "agility"}
        ],
        "edges": [
            {"edge": {"name": "Quick Draw"}, "description": "Lightning fast draw"},
            {"edge": {"name": "Marksman"}, "description": "Incredible accuracy"}
        ],
        "hindrances": [
            {"hindrance": {"name": "Bad Luck"}, "selectedSeverity": "major", "description": "Cursed by fate"}
        ],
        "gear": [
            {"gear": {"name": "Colt Peacemaker"}, "quantity": 2, "notes": "Matched pair, ivory grips"},
            {"gear": {"name": "Winchester 73"}, "quantity": 1, "notes": "Well-maintained"},
            {"gear": {"name": "Bowie Knife"}, "quantity": 1, "notes": "Boot knife"}
        ]
     }'::jsonb,
     false);

-- Insert a test beast
INSERT INTO game_entities (id, plot_point_id, owner_id, entity_type, name, description, data, is_template) VALUES
    ('c0eebc99-3333-4ef8-8bb6-2d1a1f5c5e1e',
     'b0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'a0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'beast',
     'Prairie Tick',
     'Dog-sized bloodsucking insect',
     '{
        "name": "Prairie Tick",
        "agility": {"dice": "d8", "bonus": 0},
        "smarts": {"dice": "d4", "bonus": -2},
        "spirit": {"dice": "d6", "bonus": 0},
        "strength": {"dice": "d6", "bonus": 0},
        "vigor": {"dice": "d6", "bonus": 0},
        "animalIntelligence": true,
        "pace": 8,
        "skills": [
            {"name": "Fighting", "dice": "d6", "attribute": "agility"},
            {"name": "Notice", "dice": "d6", "attribute": "smarts"},
            {"name": "Stealth", "dice": "d10", "attribute": "agility"}
        ],
        "specialAbilities": [
            {"name": "Blood Drain", "description": "Victims must make Vigor roll or gain Fatigue"},
            {"name": "Size -1", "description": "Prairie ticks are dog-sized"},
            {"name": "Wall Walker", "description": "Can walk on walls and ceilings at Pace 4"}
        ]
     }'::jsonb,
     true);

-- Insert a test game session
INSERT INTO game_sessions (id, plot_point_id, gm_id, name, description, status, scheduled_at) VALUES
    ('d0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'b0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'a0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'Session 1: Welcome to Deadwood',
     'The posse arrives in the mining town of Deadwood',
     'preparing',
     NOW() + INTERVAL '7 days');

-- Add players to the session
INSERT INTO game_session_players (session_id, player_id, character_id) VALUES
    ('d0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'a0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e',
     'c0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e'),
    ('d0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
     'a0eebc99-3333-4ef8-8bb6-2d1a1f5c5e1e',
     NULL); -- Player hasn't chosen character yet

-- Insert sample audit log entries
INSERT INTO audit_log (user_id, action, entity_type, entity_id, entity_name, changes) VALUES
    ('a0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e', 'create', 'plot_point', 'b0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e', 'Deadlands: The Weird West', '{"created": true}'::jsonb),
    ('a0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e', 'create', 'character', 'c0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e', 'Jake "Dead Eye" Morrison', '{"created": true}'::jsonb);
