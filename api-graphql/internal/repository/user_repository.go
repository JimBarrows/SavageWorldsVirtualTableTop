package repository

import (
	"context"

	"github.com/google/uuid"
	"github.com/jackc/pgx/v5"
	"github.com/jimbarrows/savage-worlds-api/internal/db"
	"github.com/jimbarrows/savage-worlds-api/internal/models"
	"github.com/jimbarrows/savage-worlds-api/pkg/errors"
)

// UserRepository handles user data operations
type UserRepository struct {
	db *db.DB
}

// NewUserRepository creates a new user repository
func NewUserRepository(database *db.DB) *UserRepository {
	return &UserRepository{
		db: database,
	}
}

// Create creates a new user
func (r *UserRepository) Create(ctx context.Context, user *models.User) error {
	// For email-only authentication, we don't insert username
	query := `
		INSERT INTO users (
			email, password, full_name, avatar_url, 
			is_active, metadata
		) VALUES (
			$1, $2, $3, $4, $5, $6
		) RETURNING id, created_at, updated_at`

	err := r.db.QueryRow(ctx, query,
		user.Email,
		user.Password,
		user.FullName,
		user.AvatarURL,
		user.IsActive,
		user.Metadata,
	).Scan(&user.ID, &user.CreatedAt, &user.UpdatedAt)

	if err != nil {
		// Check for unique constraint violations
		if isDuplicateError(err, "users_email_key") {
			return errors.ErrDuplicateEmail
		}
		return errors.NewDatabaseError(err)
	}

	return nil
}

// GetByID retrieves a user by ID
func (r *UserRepository) GetByID(ctx context.Context, id uuid.UUID) (*models.User, error) {
	user := &models.User{}
	query := `
		SELECT 
			id, email, password, full_name, avatar_url,
			cognito_sub, created_at, updated_at, last_login_at,
			is_active, metadata
		FROM users
		WHERE id = $1`

	err := r.db.QueryRow(ctx, query, id).Scan(
		&user.ID,
		&user.Email,
		&user.Password,
		&user.FullName,
		&user.AvatarURL,
		&user.CognitoSub,
		&user.CreatedAt,
		&user.UpdatedAt,
		&user.LastLoginAt,
		&user.IsActive,
		&user.Metadata,
	)

	if err != nil {
		if err == pgx.ErrNoRows {
			return nil, errors.NewNotFoundError("User")
		}
		return nil, errors.NewDatabaseError(err)
	}

	return user, nil
}

// GetByEmail retrieves a user by email
func (r *UserRepository) GetByEmail(ctx context.Context, email string) (*models.User, error) {
	user := &models.User{}
	query := `
		SELECT 
			id, email, password, full_name, avatar_url,
			cognito_sub, created_at, updated_at, last_login_at,
			is_active, metadata
		FROM users
		WHERE email = $1`

	err := r.db.QueryRow(ctx, query, email).Scan(
		&user.ID,
		&user.Email,
		&user.Password,
		&user.FullName,
		&user.AvatarURL,
		&user.CognitoSub,
		&user.CreatedAt,
		&user.UpdatedAt,
		&user.LastLoginAt,
		&user.IsActive,
		&user.Metadata,
	)

	if err != nil {
		if err == pgx.ErrNoRows {
			return nil, errors.NewNotFoundError("User")
		}
		return nil, errors.NewDatabaseError(err)
	}

	return user, nil
}

// GetByUsername retrieves a user by username
func (r *UserRepository) GetByUsername(ctx context.Context, username string) (*models.User, error) {
	user := &models.User{}
	query := `
		SELECT 
			id, email, username, password, full_name, avatar_url,
			cognito_sub, created_at, updated_at, last_login_at,
			is_active, metadata
		FROM users
		WHERE username = $1`

	err := r.db.QueryRow(ctx, query, username).Scan(
		&user.ID,
		&user.Email,
		&user.Username,
		&user.Password,
		&user.FullName,
		&user.AvatarURL,
		&user.CognitoSub,
		&user.CreatedAt,
		&user.UpdatedAt,
		&user.LastLoginAt,
		&user.IsActive,
		&user.Metadata,
	)

	if err != nil {
		if err == pgx.ErrNoRows {
			return nil, errors.NewNotFoundError("User")
		}
		return nil, errors.NewDatabaseError(err)
	}

	return user, nil
}

// GetByUsernameOrEmail retrieves a user by username or email (for login)
func (r *UserRepository) GetByUsernameOrEmail(ctx context.Context, identifier string) (*models.User, error) {
	user := &models.User{}
	query := `
		SELECT 
			id, email, username, password, full_name, avatar_url,
			cognito_sub, created_at, updated_at, last_login_at,
			is_active, metadata
		FROM users
		WHERE username = $1 OR email = $1`

	err := r.db.QueryRow(ctx, query, identifier).Scan(
		&user.ID,
		&user.Email,
		&user.Username,
		&user.Password,
		&user.FullName,
		&user.AvatarURL,
		&user.CognitoSub,
		&user.CreatedAt,
		&user.UpdatedAt,
		&user.LastLoginAt,
		&user.IsActive,
		&user.Metadata,
	)

	if err != nil {
		if err == pgx.ErrNoRows {
			return nil, errors.NewNotFoundError("User")
		}
		return nil, errors.NewDatabaseError(err)
	}

	return user, nil
}

// Update updates a user
func (r *UserRepository) Update(ctx context.Context, id uuid.UUID, updates *models.UpdateUserRequest) error {
	query := `
		UPDATE users
		SET full_name = COALESCE($2, full_name),
		    avatar_url = COALESCE($3, avatar_url),
		    updated_at = NOW()
		WHERE id = $1`

	result, err := r.db.Exec(ctx, query, id, updates.FullName, updates.AvatarURL)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("User")
	}

	return nil
}

// UpdatePassword updates a user's password
func (r *UserRepository) UpdatePassword(ctx context.Context, id uuid.UUID, hashedPassword string) error {
	query := `
		UPDATE users
		SET password = $2,
		    updated_at = NOW()
		WHERE id = $1`

	result, err := r.db.Exec(ctx, query, id, hashedPassword)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("User")
	}

	return nil
}

// UpdateLastLogin updates the user's last login timestamp
func (r *UserRepository) UpdateLastLogin(ctx context.Context, id uuid.UUID) error {
	query := `
		UPDATE users
		SET last_login_at = NOW()
		WHERE id = $1`

	result, err := r.db.Exec(ctx, query, id)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("User")
	}

	return nil
}

// Delete soft deletes a user by setting is_active to false
func (r *UserRepository) Delete(ctx context.Context, id uuid.UUID) error {
	query := `
		UPDATE users
		SET is_active = false,
		    updated_at = NOW()
		WHERE id = $1`

	result, err := r.db.Exec(ctx, query, id)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("User")
	}

	return nil
}

// HardDelete permanently deletes a user
func (r *UserRepository) HardDelete(ctx context.Context, id uuid.UUID) error {
	query := `DELETE FROM users WHERE id = $1`

	result, err := r.db.Exec(ctx, query, id)
	if err != nil {
		return errors.NewDatabaseError(err)
	}

	if result.RowsAffected() == 0 {
		return errors.NewNotFoundError("User")
	}

	return nil
}

// List retrieves a paginated list of users
func (r *UserRepository) List(ctx context.Context, offset, limit int) ([]*models.User, int64, error) {
	// Count total users
	var total int64
	countQuery := `SELECT COUNT(*) FROM users WHERE is_active = true`
	err := r.db.QueryRow(ctx, countQuery).Scan(&total)
	if err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}

	// Get paginated users
	query := `
		SELECT 
			id, email, username, password, full_name, avatar_url,
			cognito_sub, created_at, updated_at, last_login_at,
			is_active, metadata
		FROM users
		WHERE is_active = true
		ORDER BY created_at DESC
		LIMIT $1 OFFSET $2`

	rows, err := r.db.Query(ctx, query, limit, offset)
	if err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}
	defer rows.Close()

	var users []*models.User
	for rows.Next() {
		user := &models.User{}
		err := rows.Scan(
			&user.ID,
			&user.Email,
			&user.Username,
			&user.Password,
			&user.FullName,
			&user.AvatarURL,
			&user.CognitoSub,
			&user.CreatedAt,
			&user.UpdatedAt,
			&user.LastLoginAt,
			&user.IsActive,
			&user.Metadata,
		)
		if err != nil {
			return nil, 0, errors.NewDatabaseError(err)
		}
		users = append(users, user)
	}

	if err := rows.Err(); err != nil {
		return nil, 0, errors.NewDatabaseError(err)
	}

	return users, total, nil
}

// ExistsByEmail checks if a user exists with the given email
func (r *UserRepository) ExistsByEmail(ctx context.Context, email string) (bool, error) {
	var exists bool
	query := `SELECT EXISTS(SELECT 1 FROM users WHERE email = $1)`

	err := r.db.QueryRow(ctx, query, email).Scan(&exists)
	if err != nil {
		return false, errors.NewDatabaseError(err)
	}

	return exists, nil
}

// ExistsByUsername checks if a user exists with the given username
func (r *UserRepository) ExistsByUsername(ctx context.Context, username string) (bool, error) {
	var exists bool
	query := `SELECT EXISTS(SELECT 1 FROM users WHERE username = $1)`

	err := r.db.QueryRow(ctx, query, username).Scan(&exists)
	if err != nil {
		return false, errors.NewDatabaseError(err)
	}

	return exists, nil
}

// isDuplicateError checks if the error is a duplicate key violation
func isDuplicateError(err error, constraint string) bool {
	if err == nil {
		return false
	}
	return contains(err.Error(), "duplicate key") && contains(err.Error(), constraint)
}

// contains checks if a string contains a substring
func contains(s, substr string) bool {
	return len(s) >= len(substr) && containsAt(s, substr, 0)
}

// containsAt checks if a string contains a substring at any position
func containsAt(s, substr string, start int) bool {
	for i := start; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
