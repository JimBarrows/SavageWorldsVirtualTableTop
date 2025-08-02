package repository

import (
	"context"
	"database/sql"
	"strings"

	"github.com/google/uuid"
	"github.com/JimBarrows/SavageWorldsVirtualTableTop/api-graphql/internal/errors"
	"github.com/JimBarrows/SavageWorldsVirtualTableTop/api-graphql/internal/models"
)

// GetByEmail retrieves a user by email address
func (r *userRepository) GetByEmail(ctx context.Context, email string) (*models.User, error) {
	user := &models.User{}
	
	query := `
		SELECT id, email, password, full_name, avatar_url, cognito_sub, 
		       created_at, updated_at, last_login_at, is_active, metadata
		FROM users
		WHERE LOWER(email) = LOWER($1)
	`
	
	err := r.db.QueryRowContext(ctx, query, strings.ToLower(email)).Scan(
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
		if err == sql.ErrNoRows {
			return nil, errors.ErrUserNotFound
		}
		return nil, errors.ErrDatabase.WithInternal(err)
	}
	
	return user, nil
}

// ExistsByEmail checks if a user exists with the given email
func (r *userRepository) ExistsByEmail(ctx context.Context, email string) (bool, error) {
	var exists bool
	query := `SELECT EXISTS(SELECT 1 FROM users WHERE LOWER(email) = LOWER($1))`
	
	err := r.db.QueryRowContext(ctx, query, strings.ToLower(email)).Scan(&exists)
	if err != nil {
		return false, errors.ErrDatabase.WithInternal(err)
	}
	
	return exists, nil
}

// CreateEmailOnly creates a new user without username
func (r *userRepository) CreateEmailOnly(ctx context.Context, user *models.User) error {
	user.ID = uuid.New()
	
	query := `
		INSERT INTO users (id, email, password, full_name, is_active, metadata, created_at, updated_at)
		VALUES ($1, $2, $3, $4, $5, $6, NOW(), NOW())
		RETURNING created_at, updated_at
	`
	
	err := r.db.QueryRowContext(
		ctx, query,
		user.ID,
		strings.ToLower(user.Email),
		user.Password,
		user.FullName,
		user.IsActive,
		user.Metadata,
	).Scan(&user.CreatedAt, &user.UpdatedAt)
	
	if err != nil {
		if strings.Contains(err.Error(), "duplicate key") && strings.Contains(err.Error(), "email") {
			return errors.ErrDuplicateEmail
		}
		return errors.ErrDatabase.WithInternal(err)
	}
	
	return nil
}