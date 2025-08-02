package errors

import (
	"errors"
	"fmt"
	"net/http"
)

// AppError represents an application-specific error
type AppError struct {
	Code       string `json:"code"`
	Message    string `json:"message"`
	StatusCode int    `json:"-"`
	Internal   error  `json:"-"`
}

// Error implements the error interface
func (e AppError) Error() string {
	if e.Internal != nil {
		return fmt.Sprintf("%s: %v", e.Message, e.Internal)
	}
	return e.Message
}

// Unwrap allows errors.Is and errors.As to work
func (e AppError) Unwrap() error {
	return e.Internal
}

// Common error codes
const (
	CodeValidation     = "VALIDATION_ERROR"
	CodeUnauthorized   = "UNAUTHORIZED"
	CodeForbidden      = "FORBIDDEN"
	CodeNotFound       = "NOT_FOUND"
	CodeConflict       = "CONFLICT"
	CodeInternal       = "INTERNAL_ERROR"
	CodeBadRequest     = "BAD_REQUEST"
	CodeTokenExpired   = "TOKEN_EXPIRED"
	CodeTokenInvalid   = "TOKEN_INVALID"
	CodeDatabaseError  = "DATABASE_ERROR"
	CodeDuplicateEntry = "DUPLICATE_ENTRY"
)

// Pre-defined errors
var (
	ErrUnauthorized        = AppError{Code: CodeUnauthorized, Message: "Unauthorized", StatusCode: http.StatusUnauthorized}
	ErrForbidden           = AppError{Code: CodeForbidden, Message: "Forbidden", StatusCode: http.StatusForbidden}
	ErrNotFound            = AppError{Code: CodeNotFound, Message: "Resource not found", StatusCode: http.StatusNotFound}
	ErrBadRequest          = AppError{Code: CodeBadRequest, Message: "Bad request", StatusCode: http.StatusBadRequest}
	ErrInternalServer      = AppError{Code: CodeInternal, Message: "Internal server error", StatusCode: http.StatusInternalServerError}
	ErrTokenExpired        = AppError{Code: CodeTokenExpired, Message: "Token has expired", StatusCode: http.StatusUnauthorized}
	ErrTokenInvalid        = AppError{Code: CodeTokenInvalid, Message: "Invalid token", StatusCode: http.StatusUnauthorized}
	ErrInvalidCredentials  = AppError{Code: CodeUnauthorized, Message: "Invalid credentials", StatusCode: http.StatusUnauthorized}
	ErrUserNotActive       = AppError{Code: CodeForbidden, Message: "User account is not active", StatusCode: http.StatusForbidden}
	ErrDuplicateEmail      = AppError{Code: CodeDuplicateEntry, Message: "Email already exists", StatusCode: http.StatusConflict}
	ErrDuplicateUsername   = AppError{Code: CodeDuplicateEntry, Message: "Username already exists", StatusCode: http.StatusConflict}
	ErrInvalidInput        = AppError{Code: CodeValidation, Message: "Invalid input", StatusCode: http.StatusBadRequest}
	ErrPasswordHashFailed  = AppError{Code: CodeInternal, Message: "Failed to hash password", StatusCode: http.StatusInternalServerError}
	ErrTokenGenerateFailed = AppError{Code: CodeInternal, Message: "Failed to generate token", StatusCode: http.StatusInternalServerError}
)

// Constructor functions

// NewValidationError creates a new validation error
func NewValidationError(message string) AppError {
	return AppError{
		Code:       CodeValidation,
		Message:    message,
		StatusCode: http.StatusBadRequest,
	}
}

// NewNotFoundError creates a new not found error
func NewNotFoundError(resource string) AppError {
	return AppError{
		Code:       CodeNotFound,
		Message:    fmt.Sprintf("%s not found", resource),
		StatusCode: http.StatusNotFound,
	}
}

// NewConflictError creates a new conflict error
func NewConflictError(message string) AppError {
	return AppError{
		Code:       CodeConflict,
		Message:    message,
		StatusCode: http.StatusConflict,
	}
}

// NewDatabaseError creates a new database error
func NewDatabaseError(err error) AppError {
	return AppError{
		Code:       CodeDatabaseError,
		Message:    "Database operation failed",
		StatusCode: http.StatusInternalServerError,
		Internal:   err,
	}
}

// NewInternalError creates a new internal server error
func NewInternalError(message string, err error) AppError {
	return AppError{
		Code:       CodeInternal,
		Message:    message,
		StatusCode: http.StatusInternalServerError,
		Internal:   err,
	}
}

// WithInternal adds an internal error to an existing AppError
func (e AppError) WithInternal(err error) AppError {
	e.Internal = err
	return e
}

// WithMessage creates a new AppError with a custom message
func (e AppError) WithMessage(message string) AppError {
	e.Message = message
	return e
}

// IsAppError checks if an error is an AppError
func IsAppError(err error) bool {
	var appErr AppError
	return errors.As(err, &appErr)
}

// GetAppError extracts AppError from an error
func GetAppError(err error) (AppError, bool) {
	var appErr AppError
	if errors.As(err, &appErr) {
		return appErr, true
	}
	return AppError{}, false
}
