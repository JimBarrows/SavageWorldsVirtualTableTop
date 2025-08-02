package middleware

import (
	"time"

	"github.com/gin-contrib/cors"
	"github.com/gin-gonic/gin"
	"github.com/jimbarrows/savage-worlds-api/config"
)

// CORSMiddleware creates CORS middleware with the given configuration
func CORSMiddleware(cfg *config.CORSConfig) gin.HandlerFunc {
	config := cors.Config{
		AllowOrigins:     cfg.AllowedOrigins,
		AllowMethods:     cfg.AllowedMethods,
		AllowHeaders:     cfg.AllowedHeaders,
		ExposeHeaders:    cfg.ExposedHeaders,
		AllowCredentials: cfg.AllowCredentials,
		MaxAge:           time.Duration(cfg.MaxAge) * time.Second,
	}

	// If wildcard is not used, we don't need a custom AllowOriginFunc
	// The library will handle the origin matching automatically
	hasWildcard := false
	for _, origin := range cfg.AllowedOrigins {
		if origin == "*" {
			hasWildcard = true
			break
		}
	}

	// Only use AllowOriginFunc if we need custom logic
	if hasWildcard && len(cfg.AllowedOrigins) > 1 {
		config.AllowOriginFunc = func(origin string) bool {
			for _, allowed := range cfg.AllowedOrigins {
				if allowed == "*" || allowed == origin {
					return true
				}
			}
			return false
		}
	}

	return cors.New(config)
}
