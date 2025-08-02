package repository

import (
	"context"
	"fmt"
	"strings"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"
	"github.com/jimbarrows/savage-worlds-api/internal/db"
	"github.com/jimbarrows/savage-worlds-api/internal/models"
	"github.com/jimbarrows/savage-worlds-api/pkg/errors"
)

// GameEntityRepository handles game entity data operations
type GameEntityRepository struct {
	db *db.DB
}

// NewGameEntityRepository creates a new game entity repository
func NewGameEntityRepository(database *db.DB) *GameEntityRepository {
	return &GameEntityRepository{
		db: database,
	}
}

// Create creates a new game entity
func (r *GameEntityRepository) Create(ctx context.Context, entity *models.GameEntity) error {
	query := `
		INSERT INTO game_entities (
			plot_point_id, owner_id, entity_type, name, description,
			data, is_template, is_active
		) VALUES (
			$1, $2, $3, $4, $5, $6, $7, $8
		) RETURNING id, created_at, updated_at`

	err := r.db.QueryRow(ctx, query,
		entity.PlotPointID,
		entity.OwnerID,
		entity.EntityType,
		entity.Name,
		entity.Description,
		entity.Data,
		entity.IsTemplate,
		entity.IsActive,
	).Scan(&entity.ID, &entity.CreatedAt, &entity.UpdatedAt)

	if err != nil {
		return errors.NewDatabaseError(err)
	}

	return nil
}

// GetByID retrieves a game entity by ID
func (r *GameEntityRepository) GetByID(ctx context.Context, id uuid.UUID) (*models.GameEntity, error) {
	entity := &models.GameEntity{}
	query := `
		SELECT 
			id, plot_point_id, owner_id, entity_type, name, description,
			data, is_template, is_active, created_at, updated_at
		FROM game_entities
		WHERE id = $1`

	err := r.db.QueryRow(ctx, query, id).Scan(
		&entity.ID,
		&entity.PlotPointID,
		&entity.OwnerID,
		&entity.EntityType,
		&entity.Name,
		&entity.Description,
		&entity.Data,
		&entity.IsTemplate,
		&entity.IsActive,
		&entity.CreatedAt,
		&entity.UpdatedAt,
	)

	if err != nil {
		if err == pgx.ErrNoRows {
			return nil, errors.NewNotFoundError("Game entity")
		}
		return nil, errors.NewDatabaseError(err)
	}

	return entity, nil
}

// Update updates a game entity
func (r *GameEntityRepository) Update(ctx context.Context, id uuid.UUID, updates *models.UpdateGameEntityRequest) error {
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

	if updates.Data != nil {
		argCount++
		setClause += fmt.Sprintf(", data = $%d", argCount)
		args = append(args, *updates.Data)
	}

	if updates.IsTemplate != nil {
		argCount++
		setClause += fmt.Sprintf(", is_template = $%d", argCount)
		args = append(args, *updates.IsTemplate)
	}

	if updates.IsActive != nil {
		argCount++
		setClause += fmt.Sprintf(", is_active = $%d", argCount)
		args = append(args, *updates.IsActive)
	}

	query := fmt.Sprintf("UPDATE game_entities %s WHERE id = $1", setClause)

	result, err := r.db.Exec(ctx, query, args...)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("Game entity")
	}

	return nil
}

// Delete deletes a game entity
func (r *GameEntityRepository) Delete(ctx context.Context, id uuid.UUID) error {
	query := `DELETE FROM game_entities WHERE id = $1`

	result, err := r.db.Exec(ctx, query, id)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("Game entity")
	}

	return nil
}

// List retrieves a filtered and paginated list of game entities
func (r *GameEntityRepository) List(ctx context.Context, filter *models.GameEntityFilter, offset, limit int) ([]*models.GameEntity, int64, error) {
	// Build WHERE clause
	whereClause, countArgs, listArgs := r.buildWhereClause(filter, offset, limit)

	// Count total entities
	var total int64
	countQuery := fmt.Sprintf("SELECT COUNT(*) FROM game_entities %s", whereClause)
	err := r.db.QueryRow(ctx, countQuery, countArgs...).Scan(&total)
	if err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}

	// Get paginated entities
	query := fmt.Sprintf(`
		SELECT 
			id, plot_point_id, owner_id, entity_type, name, description,
			data, is_template, is_active, created_at, updated_at
		FROM game_entities
		%s
		ORDER BY updated_at DESC
		LIMIT $%d OFFSET $%d`, whereClause, len(listArgs)-1, len(listArgs))

	rows, err := r.db.Query(ctx, query, listArgs...)
	if err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}
	defer rows.Close()

	var entities []*models.GameEntity
	for rows.Next() {
		entity := &models.GameEntity{}
		err := rows.Scan(
			&entity.ID,
			&entity.PlotPointID,
			&entity.OwnerID,
			&entity.EntityType,
			&entity.Name,
			&entity.Description,
			&entity.Data,
			&entity.IsTemplate,
			&entity.IsActive,
			&entity.CreatedAt,
			&entity.UpdatedAt,
		)
		if err != nil {
			return nil, 0, errors.NewDatabaseError(err)
		}
		entities = append(entities, entity)
	}

	if err := rows.Err(); err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}

	return entities, total, nil
}

// ListByPlotPoint retrieves all entities for a specific plot point
func (r *GameEntityRepository) ListByPlotPoint(ctx context.Context, plotPointID uuid.UUID, entityType string, offset, limit int) ([]*models.GameEntity, int64, error) {
	filter := &models.GameEntityFilter{
		PlotPointID: &plotPointID,
	}
	
	if entityType != "" {
		filter.EntityType = &entityType
	}

	return r.List(ctx, filter, offset, limit)
}

// IsOwner checks if a user owns a game entity
func (r *GameEntityRepository) IsOwner(ctx context.Context, entityID, userID uuid.UUID) (bool, error) {
	var exists bool
	query := `SELECT EXISTS(SELECT 1 FROM game_entities WHERE id = $1 AND owner_id = $2)`
	
	err := r.db.QueryRow(ctx, query, entityID, userID).Scan(&exists)
	if err != nil {
		return false, errors.NewDatabaseError(err)
	}

	return exists, nil
}

// GetOwnerAndPlotPoint retrieves the owner ID and plot point ID for an entity
func (r *GameEntityRepository) GetOwnerAndPlotPoint(ctx context.Context, entityID uuid.UUID) (ownerID, plotPointID uuid.UUID, err error) {
	query := `SELECT owner_id, plot_point_id FROM game_entities WHERE id = $1`
	
	err = r.db.QueryRow(ctx, query, entityID).Scan(&ownerID, &plotPointID)
	if err != nil {
		if err == pgx.ErrNoRows {
			return uuid.UUID{}, uuid.UUID{}, errors.NewNotFoundError("Game entity")
		}
		return uuid.UUID{}, uuid.UUID{}, errors.NewDatabaseError(err)
	}

	return ownerID, plotPointID, nil
}

// CreateFromTemplate creates a new entity based on a template
func (r *GameEntityRepository) CreateFromTemplate(ctx context.Context, templateID, plotPointID, ownerID uuid.UUID, name string) (*models.GameEntity, error) {
	// Get template
	template, err := r.GetByID(ctx, templateID)
	if err != nil {
		return nil, err
	}

	if !template.IsTemplate {
		return nil, errors.NewValidationError("Entity is not a template")
	}

	// Create new entity based on template
	newEntity := &models.GameEntity{
		PlotPointID: plotPointID,
		OwnerID:     ownerID,
		EntityType:  template.EntityType,
		Name:        name,
		Description: template.Description,
		Data:        template.Data,
		IsTemplate:  false,
		IsActive:    true,
	}

	if err := r.Create(ctx, newEntity); err != nil {
		return nil, err
	}

	return newEntity, nil
}

// buildWhereClause builds the WHERE clause for filtering
func (r *GameEntityRepository) buildWhereClause(filter *models.GameEntityFilter, offset, limit int) (string, []interface{}, []interface{}) {
	conditions := []string{}
	args := []interface{}{}
	argCount := 0

	if filter != nil {
		if filter.PlotPointID != nil {
			argCount++
			conditions = append(conditions, fmt.Sprintf("plot_point_id = $%d", argCount))
			args = append(args, *filter.PlotPointID)
		}

		if filter.EntityType != nil {
			argCount++
			conditions = append(conditions, fmt.Sprintf("entity_type = $%d", argCount))
			args = append(args, *filter.EntityType)
		}

		if filter.IsTemplate != nil {
			argCount++
			conditions = append(conditions, fmt.Sprintf("is_template = $%d", argCount))
			args = append(args, *filter.IsTemplate)
		}

		if filter.IsActive != nil {
			argCount++
			conditions = append(conditions, fmt.Sprintf("is_active = $%d", argCount))
			args = append(args, *filter.IsActive)
		}

		if filter.Search != nil && *filter.Search != "" {
			argCount++
			conditions = append(conditions, fmt.Sprintf("(name ILIKE $%d OR description ILIKE $%d)", argCount, argCount))
			searchPattern := fmt.Sprintf("%%%s%%", *filter.Search)
			args = append(args, searchPattern)
		}
	}

	whereClause := ""
	if len(conditions) > 0 {
		whereClause = "WHERE " + strings.Join(conditions, " AND ")
	}

	// Create separate args for list query (includes limit and offset)
	listArgs := make([]interface{}, len(args))
	copy(listArgs, args)
	listArgs = append(listArgs, limit, offset)

	return whereClause, args, listArgs
}