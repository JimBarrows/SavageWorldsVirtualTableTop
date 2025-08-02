package repository

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"
	"github.com/jimbarrows/savage-worlds-api/internal/db"
	"github.com/jimbarrows/savage-worlds-api/internal/models"
	"github.com/jimbarrows/savage-worlds-api/pkg/errors"
)

// PlotPointRepository handles plot point data operations
type PlotPointRepository struct {
	db *db.DB
}

// NewPlotPointRepository creates a new plot point repository
func NewPlotPointRepository(database *db.DB) *PlotPointRepository {
	return &PlotPointRepository{
		db: database,
	}
}

// Create creates a new plot point
func (r *PlotPointRepository) Create(ctx context.Context, plotPoint *models.PlotPoint) error {
	// Marshal basic rules to JSON
	basicRulesJSON, err := json.Marshal(plotPoint.BasicRules)
	if err != nil {
		return errors.NewInternalError("Failed to marshal basic rules", err)
	}

	query := `
		INSERT INTO plot_points (
			owner_id, name, description, basic_rules
		) VALUES (
			$1, $2, $3, $4
		) RETURNING id, created_at, updated_at`

	err = r.db.QueryRow(ctx, query,
		plotPoint.OwnerID,
		plotPoint.Name,
		plotPoint.Description,
		basicRulesJSON,
	).Scan(&plotPoint.ID, &plotPoint.CreatedAt, &plotPoint.UpdatedAt)

	if err != nil {
		return errors.NewDatabaseError(err)
	}

	// Initialize empty JSON arrays for all fields
	plotPoint.AirVehicles = json.RawMessage("[]")
	plotPoint.Ammunition = json.RawMessage("[]")
	plotPoint.ArcaneBackgrounds = json.RawMessage("[]")
	plotPoint.Armor = json.RawMessage("[]")
	plotPoint.Beasts = json.RawMessage("[]")
	plotPoint.Characters = json.RawMessage("[]")
	plotPoint.Edges = json.RawMessage("[]")
	plotPoint.GearEras = json.RawMessage("[]")
	plotPoint.GearKinds = json.RawMessage("[]")
	plotPoint.GroundVehicles = json.RawMessage("[]")
	plotPoint.HandWeapons = json.RawMessage("[]")
	plotPoint.Hindrances = json.RawMessage("[]")
	plotPoint.MundaneItems = json.RawMessage("[]")
	plotPoint.Powers = json.RawMessage("[]")
	plotPoint.Races = json.RawMessage("[]")
	plotPoint.RangedWeapons = json.RawMessage("[]")
	plotPoint.SettingRules = json.RawMessage("[]")
	plotPoint.Skills = json.RawMessage("[]")
	plotPoint.SpecialWeapons = json.RawMessage("[]")
	plotPoint.VehicleAndATMountedWeapons = json.RawMessage("[]")
	plotPoint.WaterVehicles = json.RawMessage("[]")

	return nil
}

// GetByID retrieves a plot point by ID
func (r *PlotPointRepository) GetByID(ctx context.Context, id uuid.UUID) (*models.PlotPoint, error) {
	plotPoint := &models.PlotPoint{}
	var basicRulesJSON []byte

	query := `
		SELECT 
			id, owner_id, name, description, basic_rules,
			air_vehicles, ammunition, arcane_backgrounds, armor,
			beasts, characters, edges, gear_eras, gear_kinds,
			ground_vehicles, hand_weapons, hindrances, mundane_items,
			powers, races, ranged_weapons, setting_rules, skills,
			special_weapons, vehicle_and_at_mounted_weapons,
			water_vehicles, created_at, updated_at
		FROM plot_points
		WHERE id = $1`

	err := r.db.QueryRow(ctx, query, id).Scan(
		&plotPoint.ID,
		&plotPoint.OwnerID,
		&plotPoint.Name,
		&plotPoint.Description,
		&basicRulesJSON,
		&plotPoint.AirVehicles,
		&plotPoint.Ammunition,
		&plotPoint.ArcaneBackgrounds,
		&plotPoint.Armor,
		&plotPoint.Beasts,
		&plotPoint.Characters,
		&plotPoint.Edges,
		&plotPoint.GearEras,
		&plotPoint.GearKinds,
		&plotPoint.GroundVehicles,
		&plotPoint.HandWeapons,
		&plotPoint.Hindrances,
		&plotPoint.MundaneItems,
		&plotPoint.Powers,
		&plotPoint.Races,
		&plotPoint.RangedWeapons,
		&plotPoint.SettingRules,
		&plotPoint.Skills,
		&plotPoint.SpecialWeapons,
		&plotPoint.VehicleAndATMountedWeapons,
		&plotPoint.WaterVehicles,
		&plotPoint.CreatedAt,
		&plotPoint.UpdatedAt,
	)

	if err != nil {
		if err == pgx.ErrNoRows {
			return nil, errors.NewNotFoundError("Plot point")
		}
		return nil, errors.NewDatabaseError(err)
	}

	// Unmarshal basic rules
	if err := json.Unmarshal(basicRulesJSON, &plotPoint.BasicRules); err != nil {
		return nil, errors.NewInternalError("Failed to unmarshal basic rules", err)
	}

	return plotPoint, nil
}

// Update updates a plot point
func (r *PlotPointRepository) Update(ctx context.Context, id uuid.UUID, updates *models.UpdatePlotPointRequest) error {
	// Build dynamic update query
	setClause := "SET updated_at = NOW()"
	args := []interface{}{id}
	argCount := 1

	if updates.Name != nil {
		argCount++
		setClause += fmt.Sprintf(", name = $%d", argCount)
		args = append(args, *updates.Name)
	}

	if updates.Description != nil {
		argCount++
		setClause += fmt.Sprintf(", description = $%d", argCount)
		args = append(args, *updates.Description)
	}

	if updates.BasicRules != nil {
		basicRulesJSON, err := json.Marshal(updates.BasicRules)
		if err != nil {
			return errors.NewInternalError("Failed to marshal basic rules", err)
		}
		argCount++
		setClause += fmt.Sprintf(", basic_rules = $%d", argCount)
		args = append(args, basicRulesJSON)
	}

	// Handle all the JSON fields
	jsonFields := map[string]*json.RawMessage{
		"air_vehicles":                   updates.AirVehicles,
		"ammunition":                     updates.Ammunition,
		"arcane_backgrounds":             updates.ArcaneBackgrounds,
		"armor":                          updates.Armor,
		"beasts":                         updates.Beasts,
		"characters":                     updates.Characters,
		"edges":                          updates.Edges,
		"gear_eras":                      updates.GearEras,
		"gear_kinds":                     updates.GearKinds,
		"ground_vehicles":                updates.GroundVehicles,
		"hand_weapons":                   updates.HandWeapons,
		"hindrances":                     updates.Hindrances,
		"mundane_items":                  updates.MundaneItems,
		"powers":                         updates.Powers,
		"races":                          updates.Races,
		"ranged_weapons":                 updates.RangedWeapons,
		"setting_rules":                  updates.SettingRules,
		"skills":                         updates.Skills,
		"special_weapons":                updates.SpecialWeapons,
		"vehicle_and_at_mounted_weapons": updates.VehicleAndATMountedWeapons,
		"water_vehicles":                 updates.WaterVehicles,
	}

	for field, value := range jsonFields {
		if value != nil {
			argCount++
			setClause += fmt.Sprintf(", %s = $%d", field, argCount)
			args = append(args, *value)
		}
	}

	query := fmt.Sprintf("UPDATE plot_points %s WHERE id = $1", setClause)

	result, err := r.db.Exec(ctx, query, args...)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("Plot point")
	}

	return nil
}

// Delete deletes a plot point
func (r *PlotPointRepository) Delete(ctx context.Context, id uuid.UUID) error {
	query := `DELETE FROM plot_points WHERE id = $1`

	result, err := r.db.Exec(ctx, query, id)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("Plot point")
	}

	return nil
}

// List retrieves a paginated list of plot points for a user
func (r *PlotPointRepository) List(ctx context.Context, ownerID uuid.UUID, offset, limit int) ([]*models.PlotPointSummary, int64, error) {
	// Count total plot points for the user
	var total int64
	countQuery := `SELECT COUNT(*) FROM plot_points WHERE owner_id = $1`
	err := r.db.QueryRow(ctx, countQuery, ownerID).Scan(&total)
	if err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}

	// Get paginated plot points
	query := `
		SELECT 
			id, owner_id, name, description, created_at, updated_at
		FROM plot_points
		WHERE owner_id = $1
		ORDER BY updated_at DESC
		LIMIT $2 OFFSET $3`

	rows, err := r.db.Query(ctx, query, ownerID, limit, offset)
	if err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}
	defer rows.Close()

	var plotPoints []*models.PlotPointSummary
	for rows.Next() {
		pp := &models.PlotPointSummary{}
		err := rows.Scan(
			&pp.ID,
			&pp.OwnerID,
			&pp.Name,
			&pp.Description,
			&pp.CreatedAt,
			&pp.UpdatedAt,
		)
		if err != nil {
			return nil, 0, errors.NewDatabaseError(err)
		}
		plotPoints = append(plotPoints, pp)
	}

	if err := rows.Err(); err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}

	return plotPoints, total, nil
}

// ListAll retrieves all plot points (admin function)
func (r *PlotPointRepository) ListAll(ctx context.Context, offset, limit int) ([]*models.PlotPointSummary, int64, error) {
	// Count total plot points
	var total int64
	countQuery := `SELECT COUNT(*) FROM plot_points`
	err := r.db.QueryRow(ctx, countQuery).Scan(&total)
	if err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}

	// Get paginated plot points
	query := `
		SELECT 
			id, owner_id, name, description, created_at, updated_at
		FROM plot_points
		ORDER BY updated_at DESC
		LIMIT $1 OFFSET $2`

	rows, err := r.db.Query(ctx, query, limit, offset)
	if err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}
	defer rows.Close()

	var plotPoints []*models.PlotPointSummary
	for rows.Next() {
		pp := &models.PlotPointSummary{}
		err := rows.Scan(
			&pp.ID,
			&pp.OwnerID,
			&pp.Name,
			&pp.Description,
			&pp.CreatedAt,
			&pp.UpdatedAt,
		)
		if err != nil {
			return nil, 0, errors.NewDatabaseError(err)
		}
		plotPoints = append(plotPoints, pp)
	}

	if err := rows.Err(); err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}

	return plotPoints, total, nil
}

// IsOwner checks if a user owns a plot point
func (r *PlotPointRepository) IsOwner(ctx context.Context, plotPointID, userID uuid.UUID) (bool, error) {
	var exists bool
	query := `SELECT EXISTS(SELECT 1 FROM plot_points WHERE id = $1 AND owner_id = $2)`

	err := r.db.QueryRow(ctx, query, plotPointID, userID).Scan(&exists)
	if err != nil {
		return false, errors.NewDatabaseError(err)
	}

	return exists, nil
}

// GetEntityData retrieves specific entity data from a plot point
func (r *PlotPointRepository) GetEntityData(ctx context.Context, plotPointID uuid.UUID, entityType models.EntityType) (json.RawMessage, error) {
	var data json.RawMessage

	// Map entity type to column name
	columnName := string(entityType)

	query := fmt.Sprintf(`SELECT %s FROM plot_points WHERE id = $1`, columnName)

	err := r.db.QueryRow(ctx, query, plotPointID).Scan(&data)
	if err != nil {
		if err == pgx.ErrNoRows {
			return nil, errors.NewNotFoundError("Plot point")
		}
		return nil, errors.NewDatabaseError(err)
	}

	return data, nil
}

// UpdateEntityData updates specific entity data in a plot point
func (r *PlotPointRepository) UpdateEntityData(ctx context.Context, plotPointID uuid.UUID, entityType models.EntityType, data json.RawMessage) error {
	// Map entity type to column name
	columnName := string(entityType)

	query := fmt.Sprintf(`
		UPDATE plot_points 
		SET %s = $2, updated_at = NOW() 
		WHERE id = $1`, columnName)

	result, err := r.db.Exec(ctx, query, plotPointID, data)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("Plot point")
	}

	return nil
}
