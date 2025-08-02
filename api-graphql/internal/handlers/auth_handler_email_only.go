package handlers

import (
	"strings"

	"github.com/gin-gonic/gin"
	"github.com/JimBarrows/SavageWorldsVirtualTableTop/api-graphql/internal/errors"
	"github.com/JimBarrows/SavageWorldsVirtualTableTop/api-graphql/internal/models"
	"github.com/JimBarrows/SavageWorldsVirtualTableTop/api-graphql/internal/response"
	"github.com/JimBarrows/SavageWorldsVirtualTableTop/api-graphql/internal/utils"
)

// RegisterEmailOnly handles user registration without username
// @Summary Register new user with email only
// @Description Create a new user account using email and password
// @Tags auth
// @Accept json
// @Produce json
// @Param request body models.EmailCreateUserRequest true "Registration data"
// @Success 201 {object} models.EmailTokenResponse
// @Failure 400 {object} response.ErrorResponse
// @Failure 409 {object} response.ErrorResponse
// @Router /api/v1/auth/register [post]
func (h *AuthHandler) RegisterEmailOnly(c *gin.Context) {
	var req models.EmailCreateUserRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	// Validate email format
	if !utils.IsValidEmail(req.Email) {
		response.ValidationError(c, "Invalid email format")
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

	// Hash password
	hashedPassword, err := h.passwordService.HashPassword(req.Password)
	if err != nil {
		response.Error(c, errors.ErrPasswordHashFailed.WithInternal(err))
		return
	}

	// Create user without username
	user := &models.User{
		Email:    strings.ToLower(req.Email),
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

	// Convert to email-only response
	emailTokenResponse := &models.EmailTokenResponse{
		AccessToken:  tokenResponse.AccessToken,
		RefreshToken: tokenResponse.RefreshToken,
		TokenType:    tokenResponse.TokenType,
		ExpiresIn:    tokenResponse.ExpiresIn,
		User:         user.ToEmailUserInfo(),
	}

	response.Created(c, emailTokenResponse)
}

// LoginEmailOnly handles user login with email
// @Summary Login user with email
// @Description Authenticate user using email and password
// @Tags auth
// @Accept json
// @Produce json
// @Param request body models.EmailLoginRequest true "Login credentials"
// @Success 200 {object} models.EmailTokenResponse
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Router /api/v1/auth/login [post]
func (h *AuthHandler) LoginEmailOnly(c *gin.Context) {
	var req models.EmailLoginRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	// Validate email format
	if !utils.IsValidEmail(req.Email) {
		response.ValidationError(c, "Invalid email format")
		return
	}

	// Find user by email
	user, err := h.userRepo.GetByEmail(c.Request.Context(), strings.ToLower(req.Email))
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
		response.Error(c, errors.ErrUserInactive)
		return
	}

	// Check password
	if err := h.passwordService.CheckPassword(req.Password, user.Password); err != nil {
		response.Error(c, errors.ErrInvalidCredentials)
		return
	}

	// Update last login
	if err := h.userRepo.UpdateLastLogin(c.Request.Context(), user.ID); err != nil {
		// Log error but don't fail login
		c.Error(err)
	}

	// Generate tokens
	tokenResponse, err := h.jwtService.GenerateTokenPair(user)
	if err != nil {
		response.Error(c, err)
		return
	}

	// Convert to email-only response
	emailTokenResponse := &models.EmailTokenResponse{
		AccessToken:  tokenResponse.AccessToken,
		RefreshToken: tokenResponse.RefreshToken,
		TokenType:    tokenResponse.TokenType,
		ExpiresIn:    tokenResponse.ExpiresIn,
		User:         user.ToEmailUserInfo(),
	}

	response.Success(c, emailTokenResponse)
}