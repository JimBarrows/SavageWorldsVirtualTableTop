package models

import (
	"encoding/json"
	"time"

	"github.com/google/uuid"
)

// GameEntity represents a game entity in the system
type GameEntity struct {
	ID          uuid.UUID       `json:"id" db:"id"`
	PlotPointID uuid.UUID       `json:"plot_point_id" db:"plot_point_id"`
	OwnerID     uuid.UUID       `json:"owner_id" db:"owner_id"`
	EntityType  string          `json:"entity_type" db:"entity_type"`
	Name        string          `json:"name" db:"name"`
	Description *string         `json:"description,omitempty" db:"description"`
	Data        json.RawMessage `json:"data" db:"data"`
	IsTemplate  bool            `json:"is_template" db:"is_template"`
	IsActive    bool            `json:"is_active" db:"is_active"`
	CreatedAt   time.Time       `json:"created_at" db:"created_at"`
	UpdatedAt   time.Time       `json:"updated_at" db:"updated_at"`
}

// CreateGameEntityRequest represents the request to create a new game entity
type CreateGameEntityRequest struct {
	PlotPointID uuid.UUID       `json:"plot_point_id" binding:"required"`
	EntityType  string          `json:"entity_type" binding:"required,oneof=character beast npc vehicle item weapon armor"`
	Name        string          `json:"name" binding:"required,min=1,max=255"`
	Description string          `json:"description,omitempty"`
	Data        json.RawMessage `json:"data" binding:"required"`
	IsTemplate  bool            `json:"is_template"`
}

// UpdateGameEntityRequest represents the request to update a game entity
type UpdateGameEntityRequest struct {
	Name        *string          `json:"name,omitempty" binding:"omitempty,min=1,max=255"`
	Description *string          `json:"description,omitempty"`
	Data        *json.RawMessage `json:"data,omitempty"`
	IsTemplate  *bool            `json:"is_template,omitempty"`
	IsActive    *bool            `json:"is_active,omitempty"`
}

// GameEntityList represents a list of game entities with pagination
type GameEntityList struct {
	Entities   []*GameEntity `json:"entities"`
	TotalCount int64         `json:"total_count"`
}

// GameEntityFilter represents filter options for querying game entities
type GameEntityFilter struct {
	PlotPointID *uuid.UUID `json:"plot_point_id,omitempty"`
	EntityType  *string    `json:"entity_type,omitempty"`
	IsTemplate  *bool      `json:"is_template,omitempty"`
	IsActive    *bool      `json:"is_active,omitempty"`
	Search      *string    `json:"search,omitempty"`
}

// CharacterData represents character-specific data
type CharacterData struct {
	Attributes   map[string]int         `json:"attributes"`
	Skills       map[string]int         `json:"skills"`
	Edges        []string               `json:"edges"`
	Hindrances   []string               `json:"hindrances"`
	Gear         []string               `json:"gear"`
	Powers       []string               `json:"powers"`
	Wounds       int                    `json:"wounds"`
	Fatigue      int                    `json:"fatigue"`
	Bennies      int                    `json:"bennies"`
	Experience   int                    `json:"experience"`
	Rank         string                 `json:"rank"`
	Notes        string                 `json:"notes"`
	CustomFields map[string]interface{} `json:"custom_fields,omitempty"`
}

// BeastData represents beast-specific data
type BeastData struct {
	Attributes       map[string]int         `json:"attributes"`
	Skills           map[string]int         `json:"skills"`
	SpecialAbilities []string               `json:"special_abilities"`
	Size             int                    `json:"size"`
	Pace             int                    `json:"pace"`
	Parry            int                    `json:"parry"`
	Toughness        int                    `json:"toughness"`
	Notes            string                 `json:"notes"`
	CustomFields     map[string]interface{} `json:"custom_fields,omitempty"`
}

// VehicleData represents vehicle-specific data
type VehicleData struct {
	Type         string                 `json:"type"` // ground, air, water
	Acceleration int                    `json:"acceleration"`
	TopSpeed     int                    `json:"top_speed"`
	Toughness    int                    `json:"toughness"`
	Crew         int                    `json:"crew"`
	Cost         string                 `json:"cost"`
	Notes        string                 `json:"notes"`
	Weapons      []string               `json:"weapons"`
	CustomFields map[string]interface{} `json:"custom_fields,omitempty"`
}

// ItemData represents item-specific data
type ItemData struct {
	Type         string                 `json:"type"` // gear, consumable, etc.
	Weight       float64                `json:"weight"`
	Cost         string                 `json:"cost"`
	Notes        string                 `json:"notes"`
	CustomFields map[string]interface{} `json:"custom_fields,omitempty"`
}

// WeaponData represents weapon-specific data
type WeaponData struct {
	Type         string                 `json:"type"` // melee, ranged, special
	Damage       string                 `json:"damage"`
	Range        string                 `json:"range,omitempty"`
	RateOfFire   int                    `json:"rate_of_fire,omitempty"`
	Shots        int                    `json:"shots,omitempty"`
	MinStrength  string                 `json:"min_strength,omitempty"`
	Weight       float64                `json:"weight"`
	Cost         string                 `json:"cost"`
	Notes        string                 `json:"notes"`
	CustomFields map[string]interface{} `json:"custom_fields,omitempty"`
}

// ArmorData represents armor-specific data
type ArmorData struct {
	ArmorBonus   int                    `json:"armor_bonus"`
	MinStrength  string                 `json:"min_strength,omitempty"`
	Weight       float64                `json:"weight"`
	Cost         string                 `json:"cost"`
	Notes        string                 `json:"notes"`
	CustomFields map[string]interface{} `json:"custom_fields,omitempty"`
}

// ValidEntityTypes returns a slice of valid entity types
func ValidEntityTypes() []string {
	return []string{"character", "beast", "npc", "vehicle", "item", "weapon", "armor"}
}

// IsValidEntityType checks if the given entity type is valid
func IsValidEntityType(entityType string) bool {
	validTypes := ValidEntityTypes()
	for _, valid := range validTypes {
		if entityType == valid {
			return true
		}
	}
	return false
}
