package middleware

import (
	"strings"

	"github.com/gin-gonic/gin"
	"github.com/jimbarrows/savage-worlds-api/internal/auth"
	"github.com/jimbarrows/savage-worlds-api/pkg/response"
)

// AuthMiddleware creates JWT authentication middleware
func AuthMiddleware(jwtService *auth.JWTService) gin.HandlerFunc {
	return func(c *gin.Context) {
		// Get Authorization header
		authHeader := c.GetHeader("Authorization")
		if authHeader == "" {
			response.Unauthorized(c, "Missing authorization header")
			c.Abort()
			return
		}

		// Extract token from header
		tokenString, err := auth.ExtractTokenFromHeader(authHeader)
		if err != nil {
			response.Unauthorized(c, err.Error())
			c.Abort()
			return
		}

		// Validate token
		claims, err := jwtService.ValidateToken(tokenString, auth.AccessToken)
		if err != nil {
			response.Unauthorized(c, err.Error())
			c.Abort()
			return
		}

		// Set user information in context
		c.Set("userID", claims.UserID)
		c.Set("username", claims.Username)
		c.Set("email", claims.Email)
		c.Set("claims", claims)

		c.Next()
	}
}

// OptionalAuthMiddleware creates optional JWT authentication middleware
// It validates the token if present but doesn't require it
func OptionalAuthMiddleware(jwtService *auth.JWTService) gin.HandlerFunc {
	return func(c *gin.Context) {
		// Get Authorization header
		authHeader := c.GetHeader("Authorization")
		if authHeader == "" {
			// No auth header, continue without authentication
			c.Next()
			return
		}

		// Extract token from header
		tokenString, err := auth.ExtractTokenFromHeader(authHeader)
		if err != nil {
			// Invalid format, continue without authentication
			c.Next()
			return
		}

		// Validate token
		claims, err := jwtService.ValidateToken(tokenString, auth.AccessToken)
		if err != nil {
			// Invalid token, continue without authentication
			c.Next()
			return
		}

		// Set user information in context
		c.Set("userID", claims.UserID)
		c.Set("username", claims.Username)
		c.Set("email", claims.Email)
		c.Set("claims", claims)
		c.Set("authenticated", true)

		c.Next()
	}
}

// RequireOwnership creates middleware that checks if the authenticated user owns the resource
func RequireOwnership(paramName string) gin.HandlerFunc {
	return func(c *gin.Context) {
		// Get user ID from context (set by AuthMiddleware)
		userID, exists := c.Get("userID")
		if !exists {
			response.Unauthorized(c, "User not authenticated")
			c.Abort()
			return
		}

		// Get owner ID from route parameter
		ownerID := c.Param(paramName)
		if ownerID == "" {
			response.BadRequest(c, "Missing owner ID parameter")
			c.Abort()
			return
		}

		// Compare user ID with owner ID
		if userID != ownerID {
			response.Forbidden(c, "You don't have permission to access this resource")
			c.Abort()
			return
		}

		c.Next()
	}
}

// RequireRole creates middleware that checks if the user has a specific role
func RequireRole(requiredRoles ...string) gin.HandlerFunc {
	return func(c *gin.Context) {
		// Get claims from context
		claimsInterface, exists := c.Get("claims")
		if !exists {
			response.Unauthorized(c, "User not authenticated")
			c.Abort()
			return
		}

		claims, ok := claimsInterface.(*auth.Claims)
		if !ok {
			response.InternalServerError(c)
			c.Abort()
			return
		}

		// Check if user has any of the required roles
		// Note: This is a placeholder. In a real implementation,
		// you would need to add roles to the JWT claims or fetch from database
		userRoles := getUserRoles(claims)
		hasRole := false
		for _, requiredRole := range requiredRoles {
			for _, userRole := range userRoles {
				if strings.EqualFold(userRole, requiredRole) {
					hasRole = true
					break
				}
			}
			if hasRole {
				break
			}
		}

		if !hasRole {
			response.Forbidden(c, "Insufficient permissions")
			c.Abort()
			return
		}

		c.Next()
	}
}

// getUserRoles extracts user roles from claims
// This is a placeholder implementation
func getUserRoles(claims *auth.Claims) []string {
	// In a real implementation, you would:
	// 1. Include roles in JWT claims, or
	// 2. Fetch roles from database using user ID
	// For now, return empty slice
	return []string{}
}