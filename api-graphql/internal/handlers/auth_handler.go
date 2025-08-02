package handlers

import (
	"net/http"
	"strings"

	"github.com/gin-gonic/gin"
	"github.com/jimbarrows/savage-worlds-api/internal/auth"
	"github.com/jimbarrows/savage-worlds-api/internal/models"
	"github.com/jimbarrows/savage-worlds-api/internal/repository"
	"github.com/jimbarrows/savage-worlds-api/pkg/errors"
	"github.com/jimbarrows/savage-worlds-api/pkg/response"
	"github.com/jimbarrows/savage-worlds-api/pkg/utils"
)

// AuthHandler handles authentication endpoints
type AuthHandler struct {
	userRepo        *repository.UserRepository
	jwtService      *auth.JWTService
	passwordService *auth.PasswordService
}

// NewAuthHandler creates a new auth handler
func NewAuthHandler(userRepo *repository.UserRepository, jwtService *auth.JWTService, passwordService *auth.PasswordService) *AuthHandler {
	return &AuthHandler{
		userRepo:        userRepo,
		jwtService:      jwtService,
		passwordService: passwordService,
	}
}

// Register handles user registration
// @Summary Register a new user
// @Description Create a new user account
// @Tags auth
// @Accept json
// @Produce json
// @Param request body models.CreateUserRequest true "Registration details"
// @Success 201 {object} models.TokenResponse
// @Failure 400 {object} response.ErrorResponse
// @Failure 409 {object} response.ErrorResponse
// @Router /api/v1/auth/register [post]
func (h *AuthHandler) Register(c *gin.Context) {
	var req models.CreateUserRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	// Validate email format
	if !utils.IsValidEmail(req.Email) {
		response.ValidationError(c, "Invalid email format")
		return
	}

	// Validate username format
	if !utils.IsValidUsername(req.Username) {
		response.ValidationError(c, "Username must be 3-30 characters and contain only letters, numbers, underscores, and hyphens")
		return
	}

	// Validate password strength
	if err := h.passwordService.ValidatePasswordStrength(req.Password); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	// Check if email already exists
	exists, err := h.userRepo.ExistsByEmail(c.Request.Context(), req.Email)
	if err != nil {
		response.Error(c, err)
		return
	}
	if exists {
		response.Error(c, errors.ErrDuplicateEmail)
		return
	}

	// Check if username already exists
	exists, err = h.userRepo.ExistsByUsername(c.Request.Context(), req.Username)
	if err != nil {
		response.Error(c, err)
		return
	}
	if exists {
		response.Error(c, errors.ErrDuplicateUsername)
		return
	}

	// Hash password
	hashedPassword, err := h.passwordService.HashPassword(req.Password)
	if err != nil {
		response.Error(c, errors.ErrPasswordHashFailed.WithInternal(err))
		return
	}

	// Create user
	user := &models.User{
		Email:    strings.ToLower(req.Email),
		Username: req.Username,
		Password: hashedPassword,
		IsActive: true,
		Metadata: make(map[string]interface{}),
	}

	if req.FullName != "" {
		user.FullName = &req.FullName
	}

	if err := h.userRepo.Create(c.Request.Context(), user); err != nil {
		response.Error(c, err)
		return
	}

	// Generate tokens
	tokenResponse, err := h.jwtService.GenerateTokenPair(user)
	if err != nil {
		response.Error(c, err)
		return
	}

	response.Created(c, tokenResponse)
}

// Login handles user login
// @Summary Login user
// @Description Authenticate user and return tokens
// @Tags auth
// @Accept json
// @Produce json
// @Param request body models.LoginRequest true "Login credentials"
// @Success 200 {object} models.TokenResponse
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Router /api/v1/auth/login [post]
func (h *AuthHandler) Login(c *gin.Context) {
	var req models.LoginRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	// Find user by username or email
	user, err := h.userRepo.GetByUsernameOrEmail(c.Request.Context(), strings.ToLower(req.Username))
	if err != nil {
		if appErr, ok := errors.GetAppError(err); ok && appErr.Code == errors.CodeNotFound {
			response.Error(c, errors.ErrInvalidCredentials)
			return
		}
		response.Error(c, err)
		return
	}

	// Check if user is active
	if !user.IsActive {
		response.Error(c, errors.ErrUserNotActive)
		return
	}

	// Verify password
	if err := h.passwordService.ComparePassword(user.Password, req.Password); err != nil {
		response.Error(c, errors.ErrInvalidCredentials)
		return
	}

	// Update last login
	if err := h.userRepo.UpdateLastLogin(c.Request.Context(), user.ID); err != nil {
		// Log error but don't fail the login
		// In production, you might want to log this properly
	}

	// Generate tokens
	tokenResponse, err := h.jwtService.GenerateTokenPair(user)
	if err != nil {
		response.Error(c, err)
		return
	}

	response.OK(c, tokenResponse)
}

// Refresh handles token refresh
// @Summary Refresh access token
// @Description Generate new access token using refresh token
// @Tags auth
// @Accept json
// @Produce json
// @Param request body models.RefreshTokenRequest true "Refresh token"
// @Success 200 {object} models.TokenResponse
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Router /api/v1/auth/refresh [post]
func (h *AuthHandler) Refresh(c *gin.Context) {
	var req models.RefreshTokenRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	// Validate and refresh token
	tokenResponse, err := h.jwtService.RefreshAccessToken(req.RefreshToken)
	if err != nil {
		response.Error(c, err)
		return
	}

	// Optionally, fetch fresh user data from database
	user, err := h.userRepo.GetByID(c.Request.Context(), tokenResponse.User.ID)
	if err != nil {
		response.Error(c, err)
		return
	}

	// Check if user is still active
	if !user.IsActive {
		response.Error(c, errors.ErrUserNotActive)
		return
	}

	// Update user info in response
	tokenResponse.User = user.ToUserInfo()

	response.OK(c, tokenResponse)
}

// Logout handles user logout
// @Summary Logout user
// @Description Logout current user (client should remove tokens)
// @Tags auth
// @Security BearerAuth
// @Success 200 {object} response.SuccessResponse
// @Router /api/v1/auth/logout [post]
func (h *AuthHandler) Logout(c *gin.Context) {
	// In a stateless JWT system, logout is typically handled client-side
	// by removing the tokens. However, you could implement token blacklisting
	// or revocation here if needed.

	// For now, just return success
	response.SuccessWithMessage(c, http.StatusOK, "Logged out successfully", nil)
}

// Me returns the current user's information
// @Summary Get current user
// @Description Get information about the authenticated user
// @Tags auth
// @Security BearerAuth
// @Produce json
// @Success 200 {object} models.UserInfo
// @Failure 401 {object} response.ErrorResponse
// @Router /api/v1/auth/me [get]
func (h *AuthHandler) Me(c *gin.Context) {
	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	user, err := h.userRepo.GetByID(c.Request.Context(), userID)
	if err != nil {
		response.Error(c, err)
		return
	}

	response.OK(c, user.ToUserInfo())
}

// ChangePassword handles password change for authenticated user
// @Summary Change password
// @Description Change password for the authenticated user
// @Tags auth
// @Security BearerAuth
// @Accept json
// @Produce json
// @Param request body models.ChangePasswordRequest true "Password change details"
// @Success 200 {object} response.SuccessResponse
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Router /api/v1/auth/change-password [post]
func (h *AuthHandler) ChangePassword(c *gin.Context) {
	var req models.ChangePasswordRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Validate new password strength
	if err := h.passwordService.ValidatePasswordStrength(req.NewPassword); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	// Get user
	user, err := h.userRepo.GetByID(c.Request.Context(), userID)
	if err != nil {
		response.Error(c, err)
		return
	}

	// Verify current password
	if err := h.passwordService.ComparePassword(user.Password, req.CurrentPassword); err != nil {
		response.Error(c, errors.ErrInvalidCredentials.WithMessage("Current password is incorrect"))
		return
	}

	// Hash new password
	hashedPassword, err := h.passwordService.HashPassword(req.NewPassword)
	if err != nil {
		response.Error(c, errors.ErrPasswordHashFailed.WithInternal(err))
		return
	}

	// Update password
	if err := h.userRepo.UpdatePassword(c.Request.Context(), userID, hashedPassword); err != nil {
		response.Error(c, err)
		return
	}

	response.SuccessWithMessage(c, http.StatusOK, "Password changed successfully", nil)
}