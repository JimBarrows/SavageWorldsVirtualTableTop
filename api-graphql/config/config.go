package config

import (
	"fmt"
	"os"
	"strconv"
	"time"

	"github.com/joho/godotenv"
)

// Config holds all configuration for the application
type Config struct {
	Server   ServerConfig
	Database DatabaseConfig
	JWT      JWTConfig
	CORS     CORSConfig
}

// ServerConfig holds server-specific configuration
type ServerConfig struct {
	Port            string
	Environment     string
	ReadTimeout     time.Duration
	WriteTimeout    time.Duration
	ShutdownTimeout time.Duration
}

// DatabaseConfig holds database connection configuration
type DatabaseConfig struct {
	Host         string
	Port         int
	User         string
	Password     string
	Name         string
	SSLMode      string
	MaxOpenConns int
	MaxIdleConns int
	MaxLifetime  time.Duration
}

// JWTConfig holds JWT-specific configuration
type JWTConfig struct {
	Secret           string
	AccessExpiry     time.Duration
	RefreshExpiry    time.Duration
	Issuer           string
	AccessTokenPath  string
	RefreshTokenPath string
}

// CORSConfig holds CORS configuration
type CORSConfig struct {
	AllowedOrigins   []string
	AllowedMethods   []string
	AllowedHeaders   []string
	ExposedHeaders   []string
	AllowCredentials bool
	MaxAge           int
}

// Load loads configuration from environment variables
func Load() (*Config, error) {
	// Load .env file if it exists
	_ = godotenv.Load()

	cfg := &Config{
		Server: ServerConfig{
			Port:            getEnvOrDefault("SERVER_PORT", "8080"),
			Environment:     getEnvOrDefault("ENVIRONMENT", "development"),
			ReadTimeout:     getDurationOrDefault("SERVER_READ_TIMEOUT", 15*time.Second),
			WriteTimeout:    getDurationOrDefault("SERVER_WRITE_TIMEOUT", 15*time.Second),
			ShutdownTimeout: getDurationOrDefault("SERVER_SHUTDOWN_TIMEOUT", 10*time.Second),
		},
		Database: DatabaseConfig{
			Host:         getEnvOrDefault("DB_HOST", "localhost"),
			Port:         getIntOrDefault("DB_PORT", 5432),
			User:         getEnvOrDefault("DB_USER", "swvtt_user"),
			Password:     getEnvOrDefault("DB_PASSWORD", ""),
			Name:         getEnvOrDefault("DB_NAME", "swvtt"),
			SSLMode:      getEnvOrDefault("DB_SSL_MODE", "disable"),
			MaxOpenConns: getIntOrDefault("DB_MAX_OPEN_CONNS", 25),
			MaxIdleConns: getIntOrDefault("DB_MAX_IDLE_CONNS", 5),
			MaxLifetime:  getDurationOrDefault("DB_MAX_LIFETIME", 5*time.Minute),
		},
		JWT: JWTConfig{
			Secret:           getEnvOrDefault("JWT_SECRET", ""),
			AccessExpiry:     getDurationOrDefault("JWT_ACCESS_EXPIRY", 24*time.Hour),
			RefreshExpiry:    getDurationOrDefault("JWT_REFRESH_EXPIRY", 7*24*time.Hour),
			Issuer:           getEnvOrDefault("JWT_ISSUER", "savage-worlds-api"),
			AccessTokenPath:  "/api/v1/auth/token",
			RefreshTokenPath: "/api/v1/auth/refresh",
		},
		CORS: CORSConfig{
			AllowedOrigins:   getEnvSliceOrDefault("CORS_ALLOWED_ORIGINS", []string{"http://localhost:3000"}),
			AllowedMethods:   getEnvSliceOrDefault("CORS_ALLOWED_METHODS", []string{"GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"}),
			AllowedHeaders:   getEnvSliceOrDefault("CORS_ALLOWED_HEADERS", []string{"Accept", "Authorization", "Content-Type", "X-CSRF-Token"}),
			ExposedHeaders:   getEnvSliceOrDefault("CORS_EXPOSED_HEADERS", []string{"Link", "X-Total-Count"}),
			AllowCredentials: getBoolOrDefault("CORS_ALLOW_CREDENTIALS", true),
			MaxAge:           getIntOrDefault("CORS_MAX_AGE", 300),
		},
	}

	// Validate required configuration
	if err := cfg.validate(); err != nil {
		return nil, err
	}

	return cfg, nil
}

// validate ensures all required configuration is present
func (c *Config) validate() error {
	if c.JWT.Secret == "" {
		return fmt.Errorf("JWT_SECRET is required")
	}
	if c.Database.Password == "" {
		return fmt.Errorf("DB_PASSWORD is required")
	}
	return nil
}

// ConnectionString returns the database connection string
func (c *DatabaseConfig) ConnectionString() string {
	return fmt.Sprintf(
		"host=%s port=%d user=%s password=%s dbname=%s sslmode=%s",
		c.Host, c.Port, c.User, c.Password, c.Name, c.SSLMode,
	)
}

// Helper functions

func getEnvOrDefault(key, defaultValue string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return defaultValue
}

func getIntOrDefault(key string, defaultValue int) int {
	if value := os.Getenv(key); value != "" {
		if intValue, err := strconv.Atoi(value); err == nil {
			return intValue
		}
	}
	return defaultValue
}

func getBoolOrDefault(key string, defaultValue bool) bool {
	if value := os.Getenv(key); value != "" {
		if boolValue, err := strconv.ParseBool(value); err == nil {
			return boolValue
		}
	}
	return defaultValue
}

func getDurationOrDefault(key string, defaultValue time.Duration) time.Duration {
	if value := os.Getenv(key); value != "" {
		if duration, err := time.ParseDuration(value); err == nil {
			return duration
		}
	}
	return defaultValue
}

func getEnvSliceOrDefault(key string, defaultValue []string) []string {
	if value := os.Getenv(key); value != "" {
		// Simple comma-separated parsing
		// In production, you might want more sophisticated parsing
		return []string{value}
	}
	return defaultValue
}