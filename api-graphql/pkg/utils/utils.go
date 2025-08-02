package utils

import (
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"strconv"
	"strings"

	"github.com/gin-gonic/gin"
	"github.com/google/uuid"
)

// GenerateRandomString generates a random string of specified length
func GenerateRandomString(length int) (string, error) {
	bytes := make([]byte, length)
	if _, err := rand.Read(bytes); err != nil {
		return "", err
	}
	return base64.URLEncoding.EncodeToString(bytes)[:length], nil
}

// ParseUUID parses and validates a UUID string
func ParseUUID(id string) (uuid.UUID, error) {
	return uuid.Parse(id)
}

// GetUserIDFromContext extracts the user ID from the Gin context
func GetUserIDFromContext(c *gin.Context) (uuid.UUID, error) {
	userIDStr, exists := c.Get("userID")
	if !exists {
		return uuid.UUID{}, fmt.Errorf("user ID not found in context")
	}

	userID, ok := userIDStr.(uuid.UUID)
	if !ok {
		// Try to parse string
		userIDString, ok := userIDStr.(string)
		if !ok {
			return uuid.UUID{}, fmt.Errorf("user ID is not a valid UUID")
		}
		return ParseUUID(userIDString)
	}

	return userID, nil
}

// GetUsernameFromContext extracts the username from the Gin context
func GetUsernameFromContext(c *gin.Context) (string, error) {
	username, exists := c.Get("username")
	if !exists {
		return "", fmt.Errorf("username not found in context")
	}

	usernameStr, ok := username.(string)
	if !ok {
		return "", fmt.Errorf("username is not a string")
	}

	return usernameStr, nil
}

// PaginationParams represents pagination parameters
type PaginationParams struct {
	Page    int
	PerPage int
	Offset  int
}

// GetPaginationParams extracts pagination parameters from query string
func GetPaginationParams(c *gin.Context) PaginationParams {
	page := 1
	perPage := 20 // Default per page

	if p := c.Query("page"); p != "" {
		if parsed, err := strconv.Atoi(p); err == nil && parsed > 0 {
			page = parsed
		}
	}

	if pp := c.Query("per_page"); pp != "" {
		if parsed, err := strconv.Atoi(pp); err == nil && parsed > 0 && parsed <= 100 {
			perPage = parsed
		}
	}

	offset := (page - 1) * perPage

	return PaginationParams{
		Page:    page,
		PerPage: perPage,
		Offset:  offset,
	}
}

// CalculateTotalPages calculates the total number of pages
func CalculateTotalPages(total int64, perPage int) int {
	if total == 0 || perPage == 0 {
		return 0
	}
	pages := int(total) / perPage
	if int(total)%perPage > 0 {
		pages++
	}
	return pages
}

// SanitizeString performs basic string sanitization
func SanitizeString(input string) string {
	// Trim whitespace
	input = strings.TrimSpace(input)

	// Remove null bytes
	input = strings.ReplaceAll(input, "\x00", "")

	return input
}

// StringSliceContains checks if a string slice contains a specific value
func StringSliceContains(slice []string, value string) bool {
	for _, item := range slice {
		if item == value {
			return true
		}
	}
	return false
}

// RemoveDuplicates removes duplicate strings from a slice
func RemoveDuplicates(slice []string) []string {
	seen := make(map[string]bool)
	result := make([]string, 0, len(slice))

	for _, value := range slice {
		if !seen[value] {
			seen[value] = true
			result = append(result, value)
		}
	}

	return result
}

// TruncateString truncates a string to a maximum length
func TruncateString(str string, maxLength int) string {
	if len(str) <= maxLength {
		return str
	}
	return str[:maxLength-3] + "..."
}

// IsValidEmail performs basic email validation
func IsValidEmail(email string) bool {
	// Basic email validation
	parts := strings.Split(email, "@")
	if len(parts) != 2 {
		return false
	}

	// Check local part
	if len(parts[0]) == 0 || len(parts[0]) > 64 {
		return false
	}

	// Check domain part
	if len(parts[1]) == 0 || len(parts[1]) > 255 {
		return false
	}

	// Check for dot in domain
	if !strings.Contains(parts[1], ".") {
		return false
	}

	return true
}

// IsValidUsername validates username format
func IsValidUsername(username string) bool {
	// Username should be 3-30 characters, alphanumeric with underscores and hyphens
	if len(username) < 3 || len(username) > 30 {
		return false
	}

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

// GetClientIP attempts to get the real client IP address
func GetClientIP(c *gin.Context) string {
	// Check X-Forwarded-For header
	if ip := c.GetHeader("X-Forwarded-For"); ip != "" {
		// X-Forwarded-For can contain multiple IPs, get the first one
		parts := strings.Split(ip, ",")
		return strings.TrimSpace(parts[0])
	}

	// Check X-Real-IP header
	if ip := c.GetHeader("X-Real-IP"); ip != "" {
		return ip
	}

	// Fall back to RemoteAddr
	return c.ClientIP()
}
