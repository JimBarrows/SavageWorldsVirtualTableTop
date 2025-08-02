package auth

import (
	"time"
	"github.com/google/uuid"

	"github.com/golang-jwt/jwt/v5"
	"github.com/JimBarrows/SavageWorldsVirtualTableTop/api-graphql/internal/models"
)

// EmailClaims represents JWT claims with email as identifier
type EmailClaims struct {
	UserID   string `json:"user_id"`
	Email    string `json:"email"`
	TokenType string `json:"type"` // "access" or "refresh"
	jwt.RegisteredClaims
}

// GenerateEmailTokenPair generates access and refresh tokens using email as identifier
func (s *JWTService) GenerateEmailTokenPair(user *models.User) (*models.EmailTokenResponse, error) {
	now := time.Now()
	
	// Access token claims
	accessClaims := EmailClaims{
		UserID:    user.ID.String(),
		Email:     user.Email,
		TokenType: "access",
		RegisteredClaims: jwt.RegisteredClaims{
			Issuer:    "savage-worlds-api",
			Subject:   user.ID.String(),
			ExpiresAt: jwt.NewNumericDate(now.Add(s.config.AccessTokenDuration)),
			NotBefore: jwt.NewNumericDate(now),
			IssuedAt:  jwt.NewNumericDate(now),
			ID:        generateTokenID(),
		},
	}
	
	// Create access token
	accessToken := jwt.NewWithClaims(jwt.SigningMethodHS256, accessClaims)
	accessTokenString, err := accessToken.SignedString([]byte(s.config.SecretKey))
	if err != nil {
		return nil, err
	}
	
	// Refresh token claims
	refreshClaims := EmailClaims{
		UserID:    user.ID.String(),
		Email:     user.Email,
		TokenType: "refresh",
		RegisteredClaims: jwt.RegisteredClaims{
			Issuer:    "savage-worlds-api",
			Subject:   user.ID.String(),
			ExpiresAt: jwt.NewNumericDate(now.Add(s.config.RefreshTokenDuration)),
			NotBefore: jwt.NewNumericDate(now),
			IssuedAt:  jwt.NewNumericDate(now),
			ID:        generateTokenID(),
		},
	}
	
	// Create refresh token
	refreshToken := jwt.NewWithClaims(jwt.SigningMethodHS256, refreshClaims)
	refreshTokenString, err := refreshToken.SignedString([]byte(s.config.SecretKey))
	if err != nil {
		return nil, err
	}
	
	return &models.EmailTokenResponse{
		AccessToken:  accessTokenString,
		RefreshToken: refreshTokenString,
		TokenType:    "Bearer",
		ExpiresIn:    int64(s.config.AccessTokenDuration.Seconds()),
		User:         user.ToEmailUserInfo(),
	}, nil
}

// ValidateEmailToken validates a token and returns the email claims
func (s *JWTService) ValidateEmailToken(tokenString string) (*EmailClaims, error) {
	token, err := jwt.ParseWithClaims(tokenString, &EmailClaims{}, func(token *jwt.Token) (interface{}, error) {
		return []byte(s.config.SecretKey), nil
	})
	
	if err != nil {
		return nil, err
	}
	
	if claims, ok := token.Claims.(*EmailClaims); ok && token.Valid {
		return claims, nil
	}
	
	return nil, jwt.ErrTokenInvalidClaims
}

// generateTokenID generates a unique token ID
func generateTokenID() string {
	return uuid.New().String()
}