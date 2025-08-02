package middleware

import (
	"github.com/gin-gonic/gin"
	"github.com/gin-gonic/gin/binding"
	"github.com/go-playground/validator/v10"
	"github.com/google/uuid"
)

// SetupValidation sets up custom validators
func SetupValidation() {
	if v, ok := binding.Validator.Engine().(*validator.Validate); ok {
		// Register UUID validation
		_ = v.RegisterValidation("uuid", validateUUID)
		
		// Register custom validations
		_ = v.RegisterValidation("username", validateUsername)
		_ = v.RegisterValidation("password_strength", validatePasswordStrength)
	}
}

// validateUUID validates UUID format
func validateUUID(fl validator.FieldLevel) bool {
	value := fl.Field().String()
	_, err := uuid.Parse(value)
	return err == nil
}

// validateUsername validates username format
func validateUsername(fl validator.FieldLevel) bool {
	username := fl.Field().String()
	if len(username) < 3 || len(username) > 30 {
		return false
	}
	
	// Check characters
	for _, char := range username {
		if !((char >= 'a' && char <= 'z') || 
			 (char >= 'A' && char <= 'Z') || 
			 (char >= '0' && char <= '9') || 
			 char == '_' || char == '-') {
			return false
		}
	}
	
	return true
}

// validatePasswordStrength validates password strength
func validatePasswordStrength(fl validator.FieldLevel) bool {
	password := fl.Field().String()
	
	if len(password) < 8 {
		return false
	}
	
	hasUpper := false
	hasLower := false
	hasDigit := false
	hasSpecial := false
	
	for _, char := range password {
		switch {
		case 'A' <= char && char <= 'Z':
			hasUpper = true
		case 'a' <= char && char <= 'z':
			hasLower = true
		case '0' <= char && char <= '9':
			hasDigit = true
		case isSpecialChar(char):
			hasSpecial = true
		}
	}
	
	return hasUpper && hasLower && hasDigit && hasSpecial
}

// isSpecialChar checks if a character is special
func isSpecialChar(char rune) bool {
	specialChars := "!@#$%^&*()_+-=[]{}|;':\",./<>?"
	for _, special := range specialChars {
		if char == special {
			return true
		}
	}
	return false
}

// ContentTypeJSON ensures the request has JSON content type
func ContentTypeJSON() gin.HandlerFunc {
	return func(c *gin.Context) {
		contentType := c.GetHeader("Content-Type")
		if contentType != "application/json" && contentType != "application/json; charset=utf-8" {
			c.AbortWithStatusJSON(400, gin.H{
				"error": "Content-Type must be application/json",
			})
			return
		}
		c.Next()
	}
}