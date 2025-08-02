package models

import "github.com/google/uuid"

// EmailLoginRequest represents the login request with email only
type EmailLoginRequest struct {
	Email    string `json:"email" binding:"required,email"`
	Password string `json:"password" binding:"required"`
}

// EmailCreateUserRequest represents the request to create a new user without username
type EmailCreateUserRequest struct {
	Email    string `json:"email" binding:"required,email"`
	Password string `json:"password" binding:"required,min=8"`
	FullName string `json:"full_name,omitempty"`
}

// EmailUserInfo represents public user information without username
type EmailUserInfo struct {
	ID        uuid.UUID `json:"id"`
	Email     string    `json:"email"`
	FullName  *string   `json:"full_name,omitempty"`
	AvatarURL *string   `json:"avatar_url,omitempty"`
}

// ToEmailUserInfo converts User to EmailUserInfo (without username)
func (u *User) ToEmailUserInfo() *EmailUserInfo {
	return &EmailUserInfo{
		ID:        u.ID,
		Email:     u.Email,
		FullName:  u.FullName,
		AvatarURL: u.AvatarURL,
	}
}

// EmailTokenResponse represents the authentication token response without username
type EmailTokenResponse struct {
	AccessToken  string         `json:"access_token"`
	RefreshToken string         `json:"refresh_token"`
	TokenType    string         `json:"token_type"`
	ExpiresIn    int64          `json:"expires_in"`
	User         *EmailUserInfo `json:"user"`
}