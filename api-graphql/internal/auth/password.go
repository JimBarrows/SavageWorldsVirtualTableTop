package auth

import (
	"golang.org/x/crypto/bcrypt"
)

// PasswordService handles password operations
type PasswordService struct {
	cost int
}

// NewPasswordService creates a new password service
func NewPasswordService() *PasswordService {
	return &PasswordService{
		cost: bcrypt.DefaultCost,
	}
}

// HashPassword hashes a plain text password
func (s *PasswordService) HashPassword(password string) (string, error) {
	bytes, err := bcrypt.GenerateFromPassword([]byte(password), s.cost)
	if err != nil {
		return "", err
	}
	return string(bytes), nil
}

// ComparePassword compares a plain text password with a hashed password
func (s *PasswordService) ComparePassword(hashedPassword, password string) error {
	return bcrypt.CompareHashAndPassword([]byte(hashedPassword), []byte(password))
}

// ValidatePasswordStrength validates password strength
func (s *PasswordService) ValidatePasswordStrength(password string) error {
	// Basic validation - at least 8 characters
	if len(password) < 8 {
		return ErrPasswordTooShort
	}

	// Check for at least one uppercase letter
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

	if !hasUpper {
		return ErrPasswordNoUppercase
	}
	if !hasLower {
		return ErrPasswordNoLowercase
	}
	if !hasDigit {
		return ErrPasswordNoDigit
	}
	if !hasSpecial {
		return ErrPasswordNoSpecial
	}

	return nil
}

// isSpecialChar checks if a character is a special character
func isSpecialChar(char rune) bool {
	specialChars := "!@#$%^&*()_+-=[]{}|;':\",./<>?"
	for _, special := range specialChars {
		if char == special {
			return true
		}
	}
	return false
}

// Password validation errors
var (
	ErrPasswordTooShort    = NewPasswordError("password must be at least 8 characters long")
	ErrPasswordNoUppercase = NewPasswordError("password must contain at least one uppercase letter")
	ErrPasswordNoLowercase = NewPasswordError("password must contain at least one lowercase letter")
	ErrPasswordNoDigit     = NewPasswordError("password must contain at least one digit")
	ErrPasswordNoSpecial   = NewPasswordError("password must contain at least one special character")
)

// PasswordError represents a password validation error
type PasswordError struct {
	Message string
}

// Error implements the error interface
func (e PasswordError) Error() string {
	return e.Message
}

// NewPasswordError creates a new password error
func NewPasswordError(message string) PasswordError {
	return PasswordError{Message: message}
}
