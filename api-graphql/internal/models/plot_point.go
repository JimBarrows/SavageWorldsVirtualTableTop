package models

import (
	"encoding/json"
	"time"

	"github.com/google/uuid"
)

// PlotPoint represents a plot point in the system
type PlotPoint struct {
	ID                         uuid.UUID       `json:"id" db:"id"`
	OwnerID                    uuid.UUID       `json:"owner_id" db:"owner_id"`
	Name                       string          `json:"name" db:"name"`
	Description                *string         `json:"description,omitempty" db:"description"`
	BasicRules                 BasicRules      `json:"basic_rules" db:"basic_rules"`
	AirVehicles                json.RawMessage `json:"air_vehicles" db:"air_vehicles"`
	Ammunition                 json.RawMessage `json:"ammunition" db:"ammunition"`
	ArcaneBackgrounds          json.RawMessage `json:"arcane_backgrounds" db:"arcane_backgrounds"`
	Armor                      json.RawMessage `json:"armor" db:"armor"`
	Beasts                     json.RawMessage `json:"beasts" db:"beasts"`
	Characters                 json.RawMessage `json:"characters" db:"characters"`
	Edges                      json.RawMessage `json:"edges" db:"edges"`
	GearEras                   json.RawMessage `json:"gear_eras" db:"gear_eras"`
	GearKinds                  json.RawMessage `json:"gear_kinds" db:"gear_kinds"`
	GroundVehicles             json.RawMessage `json:"ground_vehicles" db:"ground_vehicles"`
	HandWeapons                json.RawMessage `json:"hand_weapons" db:"hand_weapons"`
	Hindrances                 json.RawMessage `json:"hindrances" db:"hindrances"`
	MundaneItems               json.RawMessage `json:"mundane_items" db:"mundane_items"`
	Powers                     json.RawMessage `json:"powers" db:"powers"`
	Races                      json.RawMessage `json:"races" db:"races"`
	RangedWeapons              json.RawMessage `json:"ranged_weapons" db:"ranged_weapons"`
	SettingRules               json.RawMessage `json:"setting_rules" db:"setting_rules"`
	Skills                     json.RawMessage `json:"skills" db:"skills"`
	SpecialWeapons             json.RawMessage `json:"special_weapons" db:"special_weapons"`
	VehicleAndATMountedWeapons json.RawMessage `json:"vehicle_and_at_mounted_weapons" db:"vehicle_and_at_mounted_weapons"`
	WaterVehicles              json.RawMessage `json:"water_vehicles" db:"water_vehicles"`
	CreatedAt                  time.Time       `json:"created_at" db:"created_at"`
	UpdatedAt                  time.Time       `json:"updated_at" db:"updated_at"`
}

// BasicRules represents the basic rules configuration
type BasicRules struct {
	MaximumAttributePoints int `json:"maximumAttributePoints" db:"maximum_attribute_points"`
	MaximumMajorHindrances int `json:"maximumMajorHindrances" db:"maximum_major_hindrances"`
	MaximumMinorHindrances int `json:"maximumMinorHindrances" db:"maximum_minor_hindrances"`
	MaximumSkillPoints     int `json:"maximumSkillPoints" db:"maximum_skill_points"`
}

// Scan implements the sql.Scanner interface for BasicRules
func (b *BasicRules) Scan(value interface{}) error {
	if value == nil {
		return nil
	}

	switch v := value.(type) {
	case []byte:
		return json.Unmarshal(v, b)
	case string:
		return json.Unmarshal([]byte(v), b)
	default:
		return json.Unmarshal(value.([]byte), b)
	}
}

// CreatePlotPointRequest represents the request to create a new plot point
type CreatePlotPointRequest struct {
	Name        string      `json:"name" binding:"required,min=1,max=255"`
	Description string      `json:"description,omitempty"`
	BasicRules  *BasicRules `json:"basic_rules,omitempty"`
}

// UpdatePlotPointRequest represents the request to update a plot point
type UpdatePlotPointRequest struct {
	Name                       *string          `json:"name,omitempty" binding:"omitempty,min=1,max=255"`
	Description                *string          `json:"description,omitempty"`
	BasicRules                 *BasicRules      `json:"basic_rules,omitempty"`
	AirVehicles                *json.RawMessage `json:"air_vehicles,omitempty"`
	Ammunition                 *json.RawMessage `json:"ammunition,omitempty"`
	ArcaneBackgrounds          *json.RawMessage `json:"arcane_backgrounds,omitempty"`
	Armor                      *json.RawMessage `json:"armor,omitempty"`
	Beasts                     *json.RawMessage `json:"beasts,omitempty"`
	Characters                 *json.RawMessage `json:"characters,omitempty"`
	Edges                      *json.RawMessage `json:"edges,omitempty"`
	GearEras                   *json.RawMessage `json:"gear_eras,omitempty"`
	GearKinds                  *json.RawMessage `json:"gear_kinds,omitempty"`
	GroundVehicles             *json.RawMessage `json:"ground_vehicles,omitempty"`
	HandWeapons                *json.RawMessage `json:"hand_weapons,omitempty"`
	Hindrances                 *json.RawMessage `json:"hindrances,omitempty"`
	MundaneItems               *json.RawMessage `json:"mundane_items,omitempty"`
	Powers                     *json.RawMessage `json:"powers,omitempty"`
	Races                      *json.RawMessage `json:"races,omitempty"`
	RangedWeapons              *json.RawMessage `json:"ranged_weapons,omitempty"`
	SettingRules               *json.RawMessage `json:"setting_rules,omitempty"`
	Skills                     *json.RawMessage `json:"skills,omitempty"`
	SpecialWeapons             *json.RawMessage `json:"special_weapons,omitempty"`
	VehicleAndATMountedWeapons *json.RawMessage `json:"vehicle_and_at_mounted_weapons,omitempty"`
	WaterVehicles              *json.RawMessage `json:"water_vehicles,omitempty"`
}

// PlotPointList represents a list of plot points with pagination
type PlotPointList struct {
	PlotPoints []*PlotPoint `json:"plot_points"`
	TotalCount int64        `json:"total_count"`
}

// PlotPointSummary represents a summary view of a plot point
type PlotPointSummary struct {
	ID          uuid.UUID `json:"id" db:"id"`
	OwnerID     uuid.UUID `json:"owner_id" db:"owner_id"`
	Name        string    `json:"name" db:"name"`
	Description *string   `json:"description,omitempty" db:"description"`
	CreatedAt   time.Time `json:"created_at" db:"created_at"`
	UpdatedAt   time.Time `json:"updated_at" db:"updated_at"`
}

// ToSummary converts a PlotPoint to PlotPointSummary
func (p *PlotPoint) ToSummary() *PlotPointSummary {
	return &PlotPointSummary{
		ID:          p.ID,
		OwnerID:     p.OwnerID,
		Name:        p.Name,
		Description: p.Description,
		CreatedAt:   p.CreatedAt,
		UpdatedAt:   p.UpdatedAt,
	}
}

// DefaultBasicRules returns the default basic rules configuration
func DefaultBasicRules() BasicRules {
	return BasicRules{
		MaximumAttributePoints: 5,
		MaximumMajorHindrances: 1,
		MaximumMinorHindrances: 2,
		MaximumSkillPoints:     12,
	}
}

// EntityType represents a specific entity type within a plot point
type EntityType string

const (
	EntityTypeCharacter                 EntityType = "characters"
	EntityTypeBeast                     EntityType = "beasts"
	EntityTypeEdge                      EntityType = "edges"
	EntityTypeHindrance                 EntityType = "hindrances"
	EntityTypeSkill                     EntityType = "skills"
	EntityTypePower                     EntityType = "powers"
	EntityTypeRace                      EntityType = "races"
	EntityTypeArcaneBackground          EntityType = "arcane_backgrounds"
	EntityTypeArmor                     EntityType = "armor"
	EntityTypeHandWeapon                EntityType = "hand_weapons"
	EntityTypeRangedWeapon              EntityType = "ranged_weapons"
	EntityTypeSpecialWeapon             EntityType = "special_weapons"
	EntityTypeAmmunition                EntityType = "ammunition"
	EntityTypeMundaneItem               EntityType = "mundane_items"
	EntityTypeGroundVehicle             EntityType = "ground_vehicles"
	EntityTypeAirVehicle                EntityType = "air_vehicles"
	EntityTypeWaterVehicle              EntityType = "water_vehicles"
	EntityTypeVehicleAndATMountedWeapon EntityType = "vehicle_and_at_mounted_weapons"
	EntityTypeGearEra                   EntityType = "gear_eras"
	EntityTypeGearKind                  EntityType = "gear_kinds"
	EntityTypeSettingRule               EntityType = "setting_rules"
)

// IsValid checks if the entity type is valid
func (e EntityType) IsValid() bool {
	switch e {
	case EntityTypeCharacter, EntityTypeBeast, EntityTypeEdge, EntityTypeHindrance,
		EntityTypeSkill, EntityTypePower, EntityTypeRace, EntityTypeArcaneBackground,
		EntityTypeArmor, EntityTypeHandWeapon, EntityTypeRangedWeapon, EntityTypeSpecialWeapon,
		EntityTypeAmmunition, EntityTypeMundaneItem, EntityTypeGroundVehicle, EntityTypeAirVehicle,
		EntityTypeWaterVehicle, EntityTypeVehicleAndATMountedWeapon, EntityTypeGearEra,
		EntityTypeGearKind, EntityTypeSettingRule:
		return true
	}
	return false
}

// String returns the string representation of the entity type
func (e EntityType) String() string {
	return string(e)
}
