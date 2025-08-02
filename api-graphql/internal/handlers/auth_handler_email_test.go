package handlers

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/gin-gonic/gin"
	"github.com/google/uuid"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"github.com/JimBarrows/SavageWorldsVirtualTableTop/api-graphql/internal/models"
)

// Test that login accepts email field instead of username
func TestAuthHandler_Login_WithEmail(t *testing.T) {
	gin.SetMode(gin.TestMode)

	tests := []struct {
		name           string
		requestBody    map[string]interface{}
		setupMocks     func(*mockUserRepository, *mockJWTService, *mockPasswordService)
		expectedStatus int
		checkResponse  func(*testing.T, map[string]interface{})
	}{
		{
			name: "successful login with email",
			requestBody: map[string]interface{}{
				"email":    "test@example.com",
				"password": "SecurePass123!",
			},
			setupMocks: func(userRepo *mockUserRepository, jwtService *mockJWTService, passwordService *mockPasswordService) {
				user := &models.User{
					ID:       uuid.New(),
					Email:    "test@example.com",
					Password: "hashedpassword",
					IsActive: true,
				}
				userRepo.On("GetByEmail", mock.Anything, "test@example.com").Return(user, nil)
				passwordService.On("CheckPassword", "SecurePass123!", "hashedpassword").Return(nil)
				
				tokenResponse := &models.TokenResponse{
					AccessToken:  "access_token",
					RefreshToken: "refresh_token",
					TokenType:    "Bearer",
					ExpiresIn:    3600,
					User:         user.ToUserInfo(),
				}
				jwtService.On("GenerateTokenPair", user).Return(tokenResponse, nil)
			},
			expectedStatus: http.StatusOK,
			checkResponse: func(t *testing.T, resp map[string]interface{}) {
				assert.Contains(t, resp, "data")
				data := resp["data"].(map[string]interface{})
				assert.Equal(t, "access_token", data["access_token"])
				assert.Contains(t, data, "user")
				user := data["user"].(map[string]interface{})
				assert.Equal(t, "test@example.com", user["email"])
			},
		},
		{
			name: "login fails with invalid email",
			requestBody: map[string]interface{}{
				"email":    "invalid-email",
				"password": "SecurePass123!",
			},
			expectedStatus: http.StatusBadRequest,
			checkResponse: func(t *testing.T, resp map[string]interface{}) {
				assert.Contains(t, resp, "error")
				assert.Equal(t, "Invalid email format", resp["error"])
			},
		},
		{
			name: "login fails without email field",
			requestBody: map[string]interface{}{
				"password": "SecurePass123!",
			},
			expectedStatus: http.StatusBadRequest,
			checkResponse: func(t *testing.T, resp map[string]interface{}) {
				assert.Contains(t, resp, "error")
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Setup
			userRepo := new(mockUserRepository)
			jwtService := new(mockJWTService)
			passwordService := new(mockPasswordService)
			
			if tt.setupMocks != nil {
				tt.setupMocks(userRepo, jwtService, passwordService)
			}

			handler := NewAuthHandler(userRepo, jwtService, passwordService)

			// Create request
			body, _ := json.Marshal(tt.requestBody)
			req := httptest.NewRequest(http.MethodPost, "/api/v1/auth/login", bytes.NewReader(body))
			req.Header.Set("Content-Type", "application/json")

			// Create response recorder
			w := httptest.NewRecorder()

			// Create gin context
			c, _ := gin.CreateTestContext(w)
			c.Request = req

			// Execute
			handler.Login(c)

			// Assert
			assert.Equal(t, tt.expectedStatus, w.Code)

			var response map[string]interface{}
			err := json.Unmarshal(w.Body.Bytes(), &response)
			assert.NoError(t, err)

			if tt.checkResponse != nil {
				tt.checkResponse(t, response)
			}

			// Verify mocks
			userRepo.AssertExpectations(t)
			jwtService.AssertExpectations(t)
			passwordService.AssertExpectations(t)
		})
	}
}

// Test that register works without username field
func TestAuthHandler_Register_WithoutUsername(t *testing.T) {
	gin.SetMode(gin.TestMode)

	tests := []struct {
		name           string
		requestBody    map[string]interface{}
		setupMocks     func(*mockUserRepository, *mockJWTService, *mockPasswordService)
		expectedStatus int
		checkResponse  func(*testing.T, map[string]interface{})
	}{
		{
			name: "successful registration without username",
			requestBody: map[string]interface{}{
				"email":    "newuser@example.com",
				"password": "SecurePass123!",
			},
			setupMocks: func(userRepo *mockUserRepository, jwtService *mockJWTService, passwordService *mockPasswordService) {
				userRepo.On("ExistsByEmail", mock.Anything, "newuser@example.com").Return(false, nil)
				passwordService.On("ValidatePasswordStrength", "SecurePass123!").Return(nil)
				passwordService.On("HashPassword", "SecurePass123!").Return("hashedpassword", nil)
				
				userRepo.On("Create", mock.Anything, mock.MatchedBy(func(u *models.User) bool {
					return u.Email == "newuser@example.com" && u.Password == "hashedpassword"
				})).Return(nil)

				tokenResponse := &models.TokenResponse{
					AccessToken:  "access_token",
					RefreshToken: "refresh_token",
					TokenType:    "Bearer",
					ExpiresIn:    3600,
					User: &models.UserInfo{
						ID:    uuid.New(),
						Email: "newuser@example.com",
					},
				}
				jwtService.On("GenerateTokenPair", mock.Anything).Return(tokenResponse, nil)
			},
			expectedStatus: http.StatusCreated,
			checkResponse: func(t *testing.T, resp map[string]interface{}) {
				assert.Contains(t, resp, "data")
				data := resp["data"].(map[string]interface{})
				assert.Contains(t, data, "user")
				user := data["user"].(map[string]interface{})
				assert.Equal(t, "newuser@example.com", user["email"])
				// Username should not be required
				_, hasUsername := user["username"]
				assert.False(t, hasUsername, "Username should not be present in response")
			},
		},
		{
			name: "registration with optional full name",
			requestBody: map[string]interface{}{
				"email":     "newuser@example.com",
				"password":  "SecurePass123!",
				"full_name": "John Doe",
			},
			setupMocks: func(userRepo *mockUserRepository, jwtService *mockJWTService, passwordService *mockPasswordService) {
				userRepo.On("ExistsByEmail", mock.Anything, "newuser@example.com").Return(false, nil)
				passwordService.On("ValidatePasswordStrength", "SecurePass123!").Return(nil)
				passwordService.On("HashPassword", "SecurePass123!").Return("hashedpassword", nil)
				
				userRepo.On("Create", mock.Anything, mock.MatchedBy(func(u *models.User) bool {
					return u.Email == "newuser@example.com" && 
						   u.FullName != nil && *u.FullName == "John Doe"
				})).Return(nil)

				fullName := "John Doe"
				tokenResponse := &models.TokenResponse{
					AccessToken:  "access_token",
					RefreshToken: "refresh_token",
					TokenType:    "Bearer",
					ExpiresIn:    3600,
					User: &models.UserInfo{
						ID:       uuid.New(),
						Email:    "newuser@example.com",
						FullName: &fullName,
					},
				}
				jwtService.On("GenerateTokenPair", mock.Anything).Return(tokenResponse, nil)
			},
			expectedStatus: http.StatusCreated,
			checkResponse: func(t *testing.T, resp map[string]interface{}) {
				assert.Contains(t, resp, "data")
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Setup
			userRepo := new(mockUserRepository)
			jwtService := new(mockJWTService)
			passwordService := new(mockPasswordService)
			
			if tt.setupMocks != nil {
				tt.setupMocks(userRepo, jwtService, passwordService)
			}

			handler := NewAuthHandler(userRepo, jwtService, passwordService)

			// Create request
			body, _ := json.Marshal(tt.requestBody)
			req := httptest.NewRequest(http.MethodPost, "/api/v1/auth/register", bytes.NewReader(body))
			req.Header.Set("Content-Type", "application/json")

			// Create response recorder
			w := httptest.NewRecorder()

			// Create gin context
			c, _ := gin.CreateTestContext(w)
			c.Request = req

			// Execute
			handler.Register(c)

			// Assert
			assert.Equal(t, tt.expectedStatus, w.Code)

			var response map[string]interface{}
			err := json.Unmarshal(w.Body.Bytes(), &response)
			assert.NoError(t, err)

			if tt.checkResponse != nil {
				tt.checkResponse(t, response)
			}

			// Verify mocks
			userRepo.AssertExpectations(t)
			jwtService.AssertExpectations(t)
			passwordService.AssertExpectations(t)
		})
	}
}