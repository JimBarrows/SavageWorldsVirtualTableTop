package middleware

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/gin-gonic/gin"
	"github.com/jimbarrows/savage-worlds-api/config"
	"github.com/stretchr/testify/assert"
)

func TestCORSMiddleware(t *testing.T) {
	gin.SetMode(gin.TestMode)

	tests := []struct {
		name                string
		corsConfig          *config.CORSConfig
		requestOrigin       string
		requestMethod       string
		expectedAllowOrigin string
		expectedStatusCode  int
		expectCORSHeaders   bool
	}{
		{
			name: "Allow localhost:3000 origin",
			corsConfig: &config.CORSConfig{
				AllowedOrigins:   []string{"http://localhost:3000"},
				AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
				AllowedHeaders:   []string{"Content-Type", "Authorization"},
				ExposedHeaders:   []string{"X-Total-Count"},
				AllowCredentials: true,
				MaxAge:           300,
			},
			requestOrigin:       "http://localhost:3000",
			requestMethod:       "GET",
			expectedAllowOrigin: "http://localhost:3000",
			expectedStatusCode:  http.StatusOK,
			expectCORSHeaders:   true,
		},
		{
			name: "Allow all origins with wildcard",
			corsConfig: &config.CORSConfig{
				AllowedOrigins:   []string{"*"},
				AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
				AllowedHeaders:   []string{"Content-Type", "Authorization"},
				ExposedHeaders:   []string{"X-Total-Count"},
				AllowCredentials: false, // Can't use credentials with wildcard
				MaxAge:           300,
			},
			requestOrigin:       "http://unauthorized-origin.com",
			requestMethod:       "GET",
			expectedAllowOrigin: "*",
			expectedStatusCode:  http.StatusOK,
			expectCORSHeaders:   true,
		},
		{
			name: "Block unauthorized origin",
			corsConfig: &config.CORSConfig{
				AllowedOrigins:   []string{"http://localhost:3000"},
				AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
				AllowedHeaders:   []string{"Content-Type", "Authorization"},
				ExposedHeaders:   []string{"X-Total-Count"},
				AllowCredentials: true,
				MaxAge:           300,
			},
			requestOrigin:       "http://unauthorized-origin.com",
			requestMethod:       "GET",
			expectedAllowOrigin: "",
			expectedStatusCode:  http.StatusOK,
			expectCORSHeaders:   false,
		},
		{
			name: "Handle preflight OPTIONS request",
			corsConfig: &config.CORSConfig{
				AllowedOrigins:   []string{"http://localhost:3000"},
				AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
				AllowedHeaders:   []string{"Content-Type", "Authorization"},
				ExposedHeaders:   []string{"X-Total-Count"},
				AllowCredentials: true,
				MaxAge:           300,
			},
			requestOrigin:       "http://localhost:3000",
			requestMethod:       "OPTIONS",
			expectedAllowOrigin: "http://localhost:3000",
			expectedStatusCode:  http.StatusNoContent,
			expectCORSHeaders:   true,
		},
		{
			name: "Multiple allowed origins",
			corsConfig: &config.CORSConfig{
				AllowedOrigins:   []string{"http://localhost:3000", "http://localhost:8080", "https://production.com"},
				AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
				AllowedHeaders:   []string{"Content-Type", "Authorization"},
				ExposedHeaders:   []string{"X-Total-Count"},
				AllowCredentials: true,
				MaxAge:           300,
			},
			requestOrigin:       "http://localhost:8080",
			requestMethod:       "POST",
			expectedAllowOrigin: "http://localhost:8080",
			expectedStatusCode:  http.StatusOK,
			expectCORSHeaders:   true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a new gin router
			router := gin.New()
			router.Use(CORSMiddleware(tt.corsConfig))

			// Add test endpoints - don't manually handle OPTIONS as middleware does it
			router.GET("/test", func(c *gin.Context) {
				c.JSON(http.StatusOK, gin.H{"message": "success"})
			})
			router.POST("/test", func(c *gin.Context) {
				c.JSON(http.StatusOK, gin.H{"message": "success"})
			})

			// Create a test request
			req, _ := http.NewRequest(tt.requestMethod, "/test", nil)
			req.Header.Set("Origin", tt.requestOrigin)
			if tt.requestMethod == "OPTIONS" {
				req.Header.Set("Access-Control-Request-Method", "POST")
				req.Header.Set("Access-Control-Request-Headers", "Content-Type")
			}

			// Create response recorder
			w := httptest.NewRecorder()

			// Perform the request
			router.ServeHTTP(w, req)

			// Assertions
			assert.Equal(t, tt.expectedStatusCode, w.Code)

			if tt.expectCORSHeaders {
				assert.Equal(t, tt.expectedAllowOrigin, w.Header().Get("Access-Control-Allow-Origin"))
				assert.NotEmpty(t, w.Header().Get("Access-Control-Allow-Methods"))

				if tt.corsConfig.AllowCredentials {
					assert.Equal(t, "true", w.Header().Get("Access-Control-Allow-Credentials"))
				}
			} else {
				// When origin is not allowed, CORS headers should not be present
				assert.Empty(t, w.Header().Get("Access-Control-Allow-Origin"))
			}
		})
	}
}

func TestCORSMiddleware_SignupEndpoint(t *testing.T) {
	gin.SetMode(gin.TestMode)

	// Test specifically for the signup endpoint
	corsConfig := &config.CORSConfig{
		AllowedOrigins:   []string{"http://localhost:3000"},
		AllowedMethods:   []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowedHeaders:   []string{"Content-Type", "Authorization"},
		ExposedHeaders:   []string{"X-Total-Count"},
		AllowCredentials: true,
		MaxAge:           300,
	}

	router := gin.New()
	router.Use(CORSMiddleware(corsConfig))

	// Mock signup endpoint
	router.POST("/api/v1/auth/register", func(c *gin.Context) {
		c.JSON(http.StatusCreated, gin.H{"message": "User created"})
	})

	// Test preflight request
	t.Run("Preflight request for signup", func(t *testing.T) {
		req, _ := http.NewRequest("OPTIONS", "/api/v1/auth/register", nil)
		req.Header.Set("Origin", "http://localhost:3000")
		req.Header.Set("Access-Control-Request-Method", "POST")
		req.Header.Set("Access-Control-Request-Headers", "Content-Type")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusNoContent, w.Code)
		assert.Equal(t, "http://localhost:3000", w.Header().Get("Access-Control-Allow-Origin"))
		assert.Contains(t, w.Header().Get("Access-Control-Allow-Methods"), "POST")
		assert.Contains(t, w.Header().Get("Access-Control-Allow-Headers"), "Content-Type")
	})

	// Test actual signup request
	t.Run("Actual signup request with CORS", func(t *testing.T) {
		req, _ := http.NewRequest("POST", "/api/v1/auth/register", nil)
		req.Header.Set("Origin", "http://localhost:3000")
		req.Header.Set("Content-Type", "application/json")

		w := httptest.NewRecorder()
		router.ServeHTTP(w, req)

		assert.Equal(t, http.StatusCreated, w.Code)
		assert.Equal(t, "http://localhost:3000", w.Header().Get("Access-Control-Allow-Origin"))
		assert.Equal(t, "true", w.Header().Get("Access-Control-Allow-Credentials"))
	})
}
