CREATE TYPE ATTRIBUTE_VALUE
AS ENUM ('d4',
  'd6',
  'd8',
  'd10',
  'd12');

CREATE TYPE ABILITY
AS ENUM ('agility',
  'smarts',
  'spirit',
  'strength',
  'vigor');

CREATE TYPE BURST_TEMPLATE
AS ENUM ('small',
  'medium',
  'large',
  'cone');

CREATE TYPE VEHICLE_TYPE
AS ENUM ('air',
  'sea',
  'land',
  'space');

CREATE TYPE HINDRANCE_TYPE
AS ENUM ('minor',
  'major',
  'minor_or_major');

CREATE TYPE RANK_VALUE
AS ENUM ('novice',
  'seasoned',
  'veteran',
  'heroic',
  'legendary');


CREATE TABLE IF NOT EXISTS plot_points (
  id                UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name              TEXT NOT NULL CHECK (name <> ''),
  description       TEXT NOT NULL CHECK (description <> ''),
  brief_description TEXT NOT NULL CHECK (brief_description <> '')
);

CREATE TABLE IF NOT EXISTS monstrous_abilities (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name          TEXT NOT NULL CONSTRAINT monstrous_abilities_name_not_empty CHECK (name <> ''),
  description   TEXT NOT NULL CONSTRAINT monstrous_abilities_description_not_empty CHECK (description <> ''),
  plot_point_id UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS skills (
  id            UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  name          TEXT NOT NULL CONSTRAINT beast_name_not_empty CHECK (name <> ''),
  description   TEXT NOT NULL CONSTRAINT beast_description_not_empty CHECK (description <> ''),
  ability       ABILITY DEFAULT 'strength',
  plot_point_id UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS arcane_backgrounds (
  id                    UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  skill_id              UUID REFERENCES skills (id),
  plot_point_id         UUID REFERENCES plot_points (id),
  name                  TEXT NOT NULL CHECK (name <> ''),
  description           TEXT NOT NULL CHECK (description <> ''),
  starting_powers       INTEGER DEFAULT 1 CHECK ( starting_powers >= 0),
  starting_power_points INTEGER DEFAULT 1 CHECK ( starting_power_points >= 0)

);

CREATE TABLE IF NOT EXISTS edge_types (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name          TEXT NOT NULL CONSTRAINT edge_type_name_not_empty CHECK (name <> ''),
  description   TEXT NOT NULL CONSTRAINT edge_type_description_not_empty CHECK (description <> ''),
  plot_point_id UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS edges (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name          TEXT NOT NULL CONSTRAINT edge_name_not_empty CHECK (name <> ''),
  description   TEXT NOT NULL CONSTRAINT edge_description_not_empty CHECK (description <> ''),
  effects       TEXT NOT NULL CONSTRAINT edge_effects_not_empty CHECK (description <> ''),
  edge_type_id  UUID REFERENCES edge_types (id),
  plot_point_id UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS edge_requirements (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  requirement   TEXT NOT NULL CONSTRAINT edge_requirement_requirement_not_empty CHECK (requirement <> ''),
  plot_point_id UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS edge_edge_requirement (
  id                  UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  edge_id             UUID REFERENCES edges (id),
  edge_requirement_id UUID REFERENCES edge_requirements (id)
);

CREATE TABLE IF NOT EXISTS gear_notes (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name          TEXT NOT NULL CHECK (name <> ''),
  description   TEXT NOT NULL CHECK (description <> ''),
  plot_point_id UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS gear_category (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name          TEXT NOT NULL CHECK (name <> ''),
  description   TEXT NOT NULL CHECK (description <> ''),
  plot_point_id UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS technology_levels (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name          TEXT NOT NULL CHECK (name <> ''),
  description   TEXT NOT NULL CHECK (description <> ''),
  plot_point_id UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS ammunition (
  id               UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name             TEXT NOT NULL CONSTRAINT ammunition_name_not_empty CHECK (name <> ''),
  description      TEXT NOT NULL CONSTRAINT ammunition_description_not_empty CHECK (description <> ''),
  weight           TEXT NOT NULL CHECK (weight <> ''),
  cost             TEXT NOT NULL CHECK (cost <> ''),
  gear_category_id UUID REFERENCES gear_category (id),
  plot_point_id    UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS ammunition_gear_note (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  ammunition_id UUID REFERENCES ammunition (id),
  gear_note_id  UUID REFERENCES gear_notes (id)
);

CREATE TABLE IF NOT EXISTS armors (
  id                  UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  name                TEXT NOT NULL CONSTRAINT ammunition_name_not_empty CHECK (name <> ''),
  description         TEXT NOT NULL CONSTRAINT ammunition_description_not_empty CHECK (description <> ''),
  weight              INTEGER DEFAULT 1 CHECK (weight >= 0),
  cost                INTEGER DEFAULT 0 CHECK (cost >= 0),
  gear_category_id    UUID REFERENCES gear_category (id),
  technology_level_id UUID REFERENCES technology_levels (id),
  armor               INTEGER DEFAULT 1 CHECK (armor > 0),
  plot_point_id       UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS armor_gear_note (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  armor_id      UUID REFERENCES armors (id),
  gear_notes_id UUID REFERENCES gear_notes (id)
);

CREATE TABLE IF NOT EXISTS hand_weapons (
  id                  UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  name                TEXT NOT NULL CHECK (name <> ''),
  description         TEXT NOT NULL CHECK (description <> ''),
  weight              INTEGER DEFAULT 1 CHECK (weight >= 0),
  cost                INTEGER DEFAULT 0 CHECK (cost >= 0),
  damage              TEXT NOT NULL CHECK (damage <> ''),
  gear_category_id    UUID REFERENCES gear_category (id),
  technology_level_id UUID REFERENCES technology_levels (id),
  plot_point_id       UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS hand_weapon_gear_note (
  id             UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  hand_weapon_id UUID REFERENCES hand_weapons (id),
  gear_notes_id  UUID REFERENCES gear_notes (id)
);

CREATE TABLE IF NOT EXISTS mundane_items (
  id                  UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  name                TEXT NOT NULL CHECK (name <> ''),
  description         TEXT NOT NULL CHECK (description <> ''),
  weight              INTEGER DEFAULT 1 CHECK (weight >= 0),
  cost                INTEGER DEFAULT 0 CHECK (cost >= 0),
  gear_category_id    UUID REFERENCES gear_category (id),
  technology_level_id UUID REFERENCES technology_levels (id),
  plot_point_id       UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS mundane_item_gear_note (
  id              UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  mundane_item_id UUID REFERENCES mundane_items (id),
  gear_notes_id   UUID REFERENCES gear_notes (id)
);

CREATE TABLE IF NOT EXISTS ranged_weapons (
  id                  UUID            DEFAULT uuid_generate_v4() PRIMARY KEY,
  name                TEXT NOT NULL CHECK (name <> ''),
  description         TEXT NOT NULL CHECK (description <> ''),
  weight              INTEGER         DEFAULT 1 CHECK (weight >= 0),
  cost                INTEGER         DEFAULT 0 CHECK (cost >= 0),
  damage              TEXT NOT NULL CHECK (damage <> ''),
  short_range         INTEGER         DEFAULT 10 CHECK (short_range > 0),
  medium_range        INTEGER         DEFAULT 20 CHECK (medium_range > short_range),
  long_range          INTEGER         DEFAULT 30 CHECK (long_range > medium_range),
  rate_of_fire        INTEGER         DEFAULT 1 CHECK (rate_of_fire > 0),
  shots               INTEGER         DEFAULT 1 CHECK (shots > 0),
  minimum_strength    ATTRIBUTE_VALUE DEFAULT 'd4',
  gear_category_id    UUID REFERENCES gear_category (id),
  technology_level_id UUID REFERENCES technology_levels (id),
  plot_point_id       UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS ranged_weapon_gear_note (
  id               UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  ranged_weapon_id UUID REFERENCES ranged_weapons (id),
  gear_notes_id    UUID REFERENCES gear_notes (id)
);

CREATE TABLE IF NOT EXISTS special_weapons (
  id                  UUID            DEFAULT uuid_generate_v4() PRIMARY KEY,
  name                TEXT NOT NULL CHECK (name <> ''),
  description         TEXT NOT NULL CHECK (description <> ''),
  weight              INTEGER         DEFAULT 1 CHECK (weight >= 0),
  cost                INTEGER         DEFAULT 0 CHECK (cost >= 0),
  damage              TEXT NOT NULL CHECK (damage <> ''),
  short_range         INTEGER         DEFAULT 10 CHECK (short_range > 0),
  medium_range        INTEGER         DEFAULT 20 CHECK (medium_range > short_range),
  long_range          INTEGER         DEFAULT 30 CHECK (long_range > medium_range),
  rate_of_fire        INTEGER         DEFAULT 1 CHECK (rate_of_fire > 0),
  armor_pierce        INTEGER         DEFAULT 0 CHECK (armor_pierce >= 0),
  minimum_strength    ATTRIBUTE_VALUE DEFAULT 'd4',
  burst_template      BURST_TEMPLATE,
  gear_category_id    UUID REFERENCES gear_category (id),
  technology_level_id UUID REFERENCES technology_levels (id),
  plot_point_id       UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS special_weapon_gear_note (
  id                UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  special_weapon_id UUID REFERENCES special_weapons (id),
  gear_notes_id     UUID REFERENCES gear_notes (id)
);

CREATE TABLE IF NOT EXISTS vehicle_mounted_and_at_guns (
  id                            UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  name                          TEXT NOT NULL CHECK (name <> ''),
  description                   TEXT NOT NULL CHECK (description <> ''),
  plot_point_id                 UUID REFERENCES plot_points (id),
  short_range                   INTEGER DEFAULT 10 CHECK (short_range > 0),
  medium_range                  INTEGER DEFAULT 20 CHECK (medium_range > short_range),
  long_range                    INTEGER DEFAULT 30 CHECK (long_range > medium_range),
  armor_armor_pierce            INTEGER DEFAULT 0 CHECK (armor_armor_pierce >= 0),
  armor_burst_template          BURST_TEMPLATE,
  armor_damage                  TEXT NOT NULL CHECK (armor_damage <> ''),
  high_explosive_armor_pierce   INTEGER DEFAULT 0 CHECK (armor_armor_pierce >= 0),
  high_explosive_burst_template BURST_TEMPLATE,
  high_explosive_damage         TEXT NOT NULL CHECK (armor_damage <> ''),
  gear_category_id              UUID REFERENCES gear_category (id),
  technology_level_id           UUID REFERENCES technology_levels (id),
  rate_of_fire                  INTEGER DEFAULT 1 CHECK (rate_of_fire > 0)
);

CREATE TABLE IF NOT EXISTS vehicle_mounted_and_at_gun_gear_note (
  id                            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  vehicle_mounted_and_at_gun_id UUID REFERENCES vehicle_mounted_and_at_guns (id),
  gear_notes_id                 UUID REFERENCES gear_notes (id)
);

CREATE TABLE IF NOT EXISTS vehicles (
  id                  UUID         DEFAULT uuid_generate_v4() PRIMARY KEY,
  plot_point_id       UUID REFERENCES plot_points (id),
  name                TEXT NOT NULL CHECK (name <> ''),
  description         TEXT NOT NULL CHECK (description <> ''),
  acceleration        INTEGER      DEFAULT 1 CHECK (acceleration > 0),
  top_speed           INTEGER      DEFAULT 1 CHECK (top_speed > 0),
  overall             INTEGER,
  overall_armor       INTEGER,
  front               INTEGER,
  front_armor         INTEGER,
  side                INTEGER,
  side_armor          INTEGER,
  back                INTEGER,
  back_armor          INTEGER,
  min_cost            INTEGER      DEFAULT 1 CHECK (min_cost > 0),
  max_cost            INTEGER      DEFAULT 2 CHECK (max_cost > min_cost),
  gear_category_id    UUID REFERENCES gear_category (id),
  technology_level_id UUID REFERENCES technology_levels (id),
  "type"              VEHICLE_TYPE DEFAULT 'land'
);

CREATE TABLE IF NOT EXISTS vehicle_gear_note (
  id                UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  vehiclevehicle_id UUID REFERENCES vehicles (id),
  gear_notes_id     UUID REFERENCES gear_notes (id)
);

CREATE TABLE IF NOT EXISTS vehicle_vehicle_mounted_and_at_guns (
  id                            UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  vehicle_id                    UUID REFERENCES vehicles (id),
  vehicle_mounted_and_at_gun_id UUID REFERENCES vehicle_mounted_and_at_guns (id),
  amount                        INTEGER DEFAULT 1 CHECK (amount > 0),
  gear_category_id              UUID REFERENCES gear_category (id),
  technology_level_id           UUID REFERENCES technology_levels (id),
  note                          TEXT
);

CREATE TABLE IF NOT EXISTS hindrances (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  plot_point_id UUID REFERENCES plot_points (id),
  name          TEXT                           NOT NULL CHECK (name <> ''),
  description   TEXT                           NOT NULL CHECK (description <> ''),
  "type"        HINDRANCE_TYPE DEFAULT 'minor' NOT NULL
);

CREATE TABLE IF NOT EXISTS arcane_backgrounds (
  id                    UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  plot_point_id         UUID REFERENCES plot_points (id),
  name                  TEXT NOT NULL CHECK (name <> ''),
  description           TEXT NOT NULL CHECK (description <> ''),
  skill_id              UUID REFERENCES skills (id),
  starting_power_points INTEGER DEFAULT 1 CHECK (starting_power_points > 0),
  starting_powers       INTEGER DEFAULT 1 CHECK (starting_powers > 0)
);

CREATE TABLE IF NOT EXISTS trappings (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  plot_point_id UUID REFERENCES plot_points (id),
  name          TEXT NOT NULL CHECK (name <> ''),
  description   TEXT NOT NULL CHECK (description <> '')
);

CREATE TABLE IF NOT EXISTS trapping_effects (
  id          UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  trapping_id UUID REFERENCES trappings (id),
  name        TEXT NOT NULL CHECK (name <> ''),
  description TEXT NOT NULL CHECK (description <> '')
);

CREATE TABLE IF NOT EXISTS powers (
  id            UUID       DEFAULT uuid_generate_v4() PRIMARY KEY,
  plot_point_id UUID REFERENCES plot_points (id),
  name          TEXT NOT NULL CHECK (name <> ''),
  description   TEXT NOT NULL CHECK (description <> ''),
  rank          RANK_VALUE DEFAULT 'novice',
  power_points  INTEGER    DEFAULT 1 CHECK (power_points > 0),
  "range"       TEXT NOT NULL CHECK ("range" <> ''),
  duration      TEXT NOT NULL CHECK (duration <> ''),
  trappings     TEXT NOT NULL CHECK (trappings <> '')
);

CREATE TABLE IF NOT EXISTS power_arcane_backgrounds (
  id                   UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  power_id             UUID REFERENCES powers (id),
  arcane_background_id UUID REFERENCES arcane_backgrounds (id)
);

CREATE TABLE IF NOT EXISTS racial_abilities (
  id            UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  plot_point_id UUID REFERENCES plot_points (id),
  name          TEXT NOT NULL CHECK (name <> ''),
  description   TEXT NOT NULL CHECK (description <> ''),
  cost          INTEGER DEFAULT 1 CHECK (cost > 0)
);

CREATE TABLE IF NOT EXISTS races (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  plot_point_id UUID REFERENCES plot_points (id),
  name          TEXT NOT NULL CHECK (name <> ''),
  description   TEXT NOT NULL CHECK (description <> '')
);

CREATE TABLE IF NOT EXISTS race_racial_ability (
  id                UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  race_id           UUID REFERENCES races (id),
  racial_ability_id UUID REFERENCES racial_abilities (id)
);

CREATE TABLE IF NOT EXISTS characters (
  id                UUID            DEFAULT uuid_generate_v4() PRIMARY KEY,
  agility           ATTRIBUTE_VALUE DEFAULT 'd4',
  armor             INTEGER         DEFAULT 4 CHECK (toughness >= 0),
  charisma          INTEGER         DEFAULT 0,
  description       TEXT NOT NULL CHECK (description <> ''),
  experience_points INTEGER         DEFAULT 0 CHECK (experience_points >= 0),
  name              TEXT NOT NULL CHECK (name <> ''),
  pace              INTEGER         DEFAULT 6 CHECK (pace >= 0),
  parry             INTEGER         DEFAULT 4 CHECK (parry >= 0),
  power_points      INTEGER         DEFAULT 4 CHECK (power_points >= 0),
  race_id           UUID REFERENCES races (id),
  rank              RANK_VALUE      DEFAULT 'novice',
  smarts            ATTRIBUTE_VALUE DEFAULT 'd4',
  spirit            ATTRIBUTE_VALUE DEFAULT 'd4',
  strength          ATTRIBUTE_VALUE DEFAULT 'd4',
  toughness         INTEGER         DEFAULT 4 CHECK (toughness >= 0),
  vigor             ATTRIBUTE_VALUE DEFAULT 'd4',
  wild_card         BOOLEAN         DEFAULT FALSE,
  plot_point_id     UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS character_ammunition (
  id            UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id  UUID REFERENCES characters (id),
  ammunition_id UUID REFERENCES ammunition (id),
  amount        INTEGER DEFAULT 1 CHECK (amount >= 0)
);

CREATE TABLE IF NOT EXISTS character_edge (
  id           UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id UUID REFERENCES characters (id),
  edge_id      UUID REFERENCES edges (id)
);

CREATE TABLE IF NOT EXISTS character_hand_weapon (
  id             UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id   UUID REFERENCES characters (id),
  hand_weapon_id UUID REFERENCES hand_weapons (id),
  amount         INTEGER DEFAULT 1 CHECK (amount >= 0)
);

CREATE TABLE IF NOT EXISTS character_mundane_item (
  id              UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id    UUID REFERENCES characters (id),
  mundane_item_id UUID REFERENCES mundane_items (id),
  amount          INTEGER DEFAULT 1 CHECK (amount >= 0)
);

CREATE TABLE IF NOT EXISTS character_ranged_weapon (
  id               UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id     UUID REFERENCES characters (id),
  ranged_weapon_id UUID REFERENCES ranged_weapons (id),
  amount           INTEGER DEFAULT 1 CHECK (amount >= 0)
);

CREATE TABLE IF NOT EXISTS character_special_weapon (
  id                UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id      UUID REFERENCES characters (id),
  special_weapon_id UUID REFERENCES special_weapons (id),
  amount            INTEGER DEFAULT 1 CHECK (amount >= 0)
);

CREATE TABLE IF NOT EXISTS character_vehicle (
  id           UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id UUID REFERENCES characters (id),
  vehicle_id   UUID REFERENCES vehicles (id),
  amount       INTEGER DEFAULT 1 CHECK (amount >= 0)
);

CREATE TABLE IF NOT EXISTS character_vehicle_mounted_and_at_gun (
  id                            UUID    DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id                  UUID REFERENCES characters (id),
  vehicle_mounted_and_at_gun_id UUID REFERENCES vehicle_mounted_and_at_guns (id),
  amount                        INTEGER DEFAULT 1 CHECK (amount >= 0)
);

CREATE TABLE IF NOT EXISTS character_hindrance (
  id           UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id UUID REFERENCES characters (id),
  hindrance_id UUID REFERENCES hindrances (id),
  taken_as     HINDRANCE_TYPE NOT NULL
);

CREATE TABLE IF NOT EXISTS character_power (
  id           UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  name         TEXT NOT NULL CHECK (name <> ''),
  character_id UUID REFERENCES characters (id),
  power_id     UUID REFERENCES powers (id),
  trapping_id  UUID REFERENCES trappings (id),
  notes        TEXT
);

CREATE TABLE IF NOT EXISTS character_skill (
  id           UUID            DEFAULT uuid_generate_v4() PRIMARY KEY,
  character_id UUID REFERENCES characters (id),
  skill_id     UUID REFERENCES skills (id),
  rank         ATTRIBUTE_VALUE DEFAULT 'd4'
);

CREATE TABLE IF NOT EXISTS beasts (
  id                  UUID            DEFAULT uuid_generate_v4() PRIMARY KEY,
  name                TEXT NOT NULL CONSTRAINT beast_name_not_empty CHECK (name <> ''),
  description         TEXT NOT NULL CONSTRAINT beast_description_not_empty CHECK (description <> ''),
  agility             ATTRIBUTE_VALUE DEFAULT 'd4',
  smarts              ATTRIBUTE_VALUE DEFAULT 'd4',
  spirit              ATTRIBUTE_VALUE DEFAULT 'd4',
  strength            ATTRIBUTE_VALUE DEFAULT 'd4',
  vigor               ATTRIBUTE_VALUE DEFAULT 'd4',
  animal_intelligence BOOLEAN         DEFAULT TRUE,
  pace                INTEGER         DEFAULT 6 CHECK (pace >= 0),
  parry               INTEGER         DEFAULT 4 CHECK (parry >= 0),
  toughness           INTEGER         DEFAULT 4 CHECK (toughness >= 0),
  armor               INTEGER         DEFAULT 4 CHECK (toughness >= 0),
  wild_card           BOOLEAN         DEFAULT FALSE,
  plot_point_id       UUID REFERENCES plot_points (id)
);

CREATE TABLE IF NOT EXISTS beast_skill (
  id       UUID            DEFAULT uuid_generate_v4() PRIMARY KEY,
  beast_id UUID REFERENCES beasts (id),
  skill_id UUID REFERENCES skills (id),
  "value"  ATTRIBUTE_VALUE DEFAULT 'd4',
  bonus    INTEGER         DEFAULT 0 CONSTRAINT beasts_skills_bonus_is_positive CHECK (bonus >= 0)
);

CREATE TABLE IF NOT EXISTS beast_monstrous_ability (
  id                   UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  beast_id             UUID REFERENCES beasts (id),
  monstrous_ability_id UUID REFERENCES monstrous_abilities (id)
);

CREATE TABLE IF NOT EXISTS setting_rules (
  id            UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  plot_point_id UUID REFERENCES plot_points (id),
  name          TEXT NOT NULL CHECK (name <> ''),
  description   TEXT NOT NULL CHECK (description <> '')
);
