package handlers

import (
	"net/http"

	"github.com/gin-gonic/gin"
	"github.com/jimbarrows/savage-worlds-api/internal/db"
)

// HealthHandler handles health check endpoints
type HealthHandler struct {
	db *db.DB
}

// NewHealthHandler creates a new health handler
func NewHealthHandler(database *db.DB) *HealthHandler {
	return &HealthHandler{
		db: database,
	}
}

// Health performs a basic health check
// @Summary Health check
// @Description Check if the service is running
// @Tags health
// @Produce json
// @Success 200 {object} map[string]interface{}
// @Router /health [get]
func (h *HealthHandler) Health(c *gin.Context) {
	c.JSON(http.StatusOK, gin.H{
		"status":  "healthy",
		"service": "savage-worlds-api",
	})
}

// Readiness checks if the service is ready to handle requests
// @Summary Readiness check
// @Description Check if the service is ready to handle requests
// @Tags health
// @Produce json
// @Success 200 {object} map[string]interface{}
// @Failure 503 {object} response.ErrorResponse
// @Router /ready [get]
func (h *HealthHandler) Readiness(c *gin.Context) {
	// Check database connection
	if err := h.db.HealthCheck(c.Request.Context()); err != nil {
		c.JSON(http.StatusServiceUnavailable, gin.H{
			"status": "not ready",
			"error":  "database connection failed",
		})
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"status":  "ready",
		"service": "savage-worlds-api",
	})
}

// Metrics returns basic service metrics
// @Summary Service metrics
// @Description Get basic service metrics
// @Tags health
// @Produce json
// @Success 200 {object} map[string]interface{}
// @Router /metrics [get]
func (h *HealthHandler) Metrics(c *gin.Context) {
	// Get database stats
	dbStats := h.db.GetStats()

	c.JSON(http.StatusOK, gin.H{
		"database": gin.H{
			"acquired_conns":     dbStats.AcquiredConns(),
			"idle_conns":         dbStats.IdleConns(),
			"total_conns":        dbStats.TotalConns(),
			"max_conns":          dbStats.MaxConns(),
			"empty_acquire_count": dbStats.EmptyAcquireCount(),
		},
	})
}