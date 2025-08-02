package auth

import (
	"fmt"
	"time"

	"github.com/golang-jwt/jwt/v5"
	"github.com/google/uuid"
	"github.com/jimbarrows/savage-worlds-api/config"
	"github.com/jimbarrows/savage-worlds-api/internal/models"
	"github.com/jimbarrows/savage-worlds-api/pkg/errors"
)

// TokenType represents the type of JWT token
type TokenType string

const (
	AccessToken  TokenType = "access"
	RefreshToken TokenType = "refresh"
)

// Claims represents JWT claims
type Claims struct {
	UserID   uuid.UUID `json:"user_id"`
	Username string    `json:"username"`
	Email    string    `json:"email"`
	Type     TokenType `json:"type"`
	jwt.RegisteredClaims
}

// JWTService handles JWT operations
type JWTService struct {
	config *config.JWTConfig
	secret []byte
}

// NewJWTService creates a new JWT service
func NewJWTService(cfg *config.JWTConfig) *JWTService {
	return &JWTService{
		config: cfg,
		secret: []byte(cfg.Secret),
	}
}

// GenerateTokenPair generates both access and refresh tokens
func (s *JWTService) GenerateTokenPair(user *models.User) (*models.TokenResponse, error) {
	// Generate access token
	accessToken, accessExpiry, err := s.GenerateToken(user, AccessToken)
	if err != nil {
		return nil, errors.ErrTokenGenerateFailed.WithInternal(err)
	}

	// Generate refresh token
	refreshToken, _, err := s.GenerateToken(user, RefreshToken)
	if err != nil {
		return nil, errors.ErrTokenGenerateFailed.WithInternal(err)
	}

	return &models.TokenResponse{
		AccessToken:  accessToken,
		RefreshToken: refreshToken,
		TokenType:    "Bearer",
		ExpiresIn:    int64(accessExpiry.Sub(time.Now()).Seconds()),
		User:         user.ToUserInfo(),
	}, nil
}

// GenerateToken generates a JWT token
func (s *JWTService) GenerateToken(user *models.User, tokenType TokenType) (string, time.Time, error) {
	var expiry time.Duration
	switch tokenType {
	case AccessToken:
		expiry = s.config.AccessExpiry
	case RefreshToken:
		expiry = s.config.RefreshExpiry
	default:
		return "", time.Time{}, fmt.Errorf("invalid token type: %s", tokenType)
	}

	expirationTime := time.Now().Add(expiry)

	claims := &Claims{
		UserID:   user.ID,
		Username: user.Username,
		Email:    user.Email,
		Type:     tokenType,
		RegisteredClaims: jwt.RegisteredClaims{
			ExpiresAt: jwt.NewNumericDate(expirationTime),
			IssuedAt:  jwt.NewNumericDate(time.Now()),
			NotBefore: jwt.NewNumericDate(time.Now()),
			Issuer:    s.config.Issuer,
			Subject:   user.ID.String(),
			ID:        uuid.New().String(),
		},
	}

	token := jwt.NewWithClaims(jwt.SigningMethodHS256, claims)
	tokenString, err := token.SignedString(s.secret)
	if err != nil {
		return "", time.Time{}, err
	}

	return tokenString, expirationTime, nil
}

// ValidateToken validates and parses a JWT token
func (s *JWTService) ValidateToken(tokenString string, expectedType TokenType) (*Claims, error) {
	token, err := jwt.ParseWithClaims(tokenString, &Claims{}, func(token *jwt.Token) (interface{}, error) {
		// Validate signing method
		if _, ok := token.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("unexpected signing method: %v", token.Header["alg"])
		}
		return s.secret, nil
	})

	if err != nil {
		if err == jwt.ErrTokenExpired {
			return nil, errors.ErrTokenExpired
		}
		return nil, errors.ErrTokenInvalid.WithInternal(err)
	}

	claims, ok := token.Claims.(*Claims)
	if !ok || !token.Valid {
		return nil, errors.ErrTokenInvalid
	}

	// Validate token type
	if claims.Type != expectedType {
		return nil, errors.ErrTokenInvalid.WithMessage(fmt.Sprintf("expected %s token, got %s", expectedType, claims.Type))
	}

	// Additional validation
	if claims.UserID == uuid.Nil {
		return nil, errors.ErrTokenInvalid.WithMessage("invalid user ID in token")
	}

	return claims, nil
}

// RefreshAccessToken generates a new access token from a refresh token
func (s *JWTService) RefreshAccessToken(refreshTokenString string) (*models.TokenResponse, error) {
	// Validate refresh token
	claims, err := s.ValidateToken(refreshTokenString, RefreshToken)
	if err != nil {
		return nil, err
	}

	// Create a temporary user object from claims for token generation
	// In a real implementation, you might want to fetch fresh user data from the database
	user := &models.User{
		ID:       claims.UserID,
		Username: claims.Username,
		Email:    claims.Email,
	}

	// Generate new access token only
	accessToken, accessExpiry, err := s.GenerateToken(user, AccessToken)
	if err != nil {
		return nil, errors.ErrTokenGenerateFailed.WithInternal(err)
	}

	return &models.TokenResponse{
		AccessToken:  accessToken,
		RefreshToken: refreshTokenString, // Return the same refresh token
		TokenType:    "Bearer",
		ExpiresIn:    int64(accessExpiry.Sub(time.Now()).Seconds()),
		User: &models.UserInfo{
			ID:       claims.UserID,
			Username: claims.Username,
			Email:    claims.Email,
		},
	}, nil
}

// ExtractTokenFromHeader extracts the token from the Authorization header
func ExtractTokenFromHeader(authHeader string) (string, error) {
	if authHeader == "" {
		return "", errors.ErrUnauthorized.WithMessage("missing authorization header")
	}

	// Expected format: "Bearer <token>"
	const bearerPrefix = "Bearer "
	if len(authHeader) < len(bearerPrefix) || authHeader[:len(bearerPrefix)] != bearerPrefix {
		return "", errors.ErrUnauthorized.WithMessage("invalid authorization header format")
	}

	return authHeader[len(bearerPrefix):], nil
}