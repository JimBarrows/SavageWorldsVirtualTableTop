package response

import (
	"net/http"

	"github.com/gin-gonic/gin"
	"github.com/jimbarrows/savage-worlds-api/pkg/errors"
)

// SuccessResponse represents a successful API response
type SuccessResponse struct {
	Data    interface{} `json:"data,omitempty"`
	Message string      `json:"message,omitempty"`
}

// ErrorResponse represents an error API response
type ErrorResponse struct {
	Error   string      `json:"error"`
	Code    string      `json:"code"`
	Details interface{} `json:"details,omitempty"`
}

// PaginatedResponse represents a paginated API response
type PaginatedResponse struct {
	Data       interface{} `json:"data"`
	Pagination Pagination  `json:"pagination"`
}

// Pagination represents pagination metadata
type Pagination struct {
	Page       int   `json:"page"`
	PerPage    int   `json:"per_page"`
	Total      int64 `json:"total"`
	TotalPages int   `json:"total_pages"`
}

// Success sends a successful response
func Success(c *gin.Context, statusCode int, data interface{}) {
	c.JSON(statusCode, SuccessResponse{
		Data: data,
	})
}

// SuccessWithMessage sends a successful response with a message
func SuccessWithMessage(c *gin.Context, statusCode int, message string, data interface{}) {
	c.JSON(statusCode, SuccessResponse{
		Data:    data,
		Message: message,
	})
}

// Created sends a 201 Created response
func Created(c *gin.Context, data interface{}) {
	Success(c, http.StatusCreated, data)
}

// OK sends a 200 OK response
func OK(c *gin.Context, data interface{}) {
	Success(c, http.StatusOK, data)
}

// NoContent sends a 204 No Content response
func NoContent(c *gin.Context) {
	c.Status(http.StatusNoContent)
}

// Paginated sends a paginated response
func Paginated(c *gin.Context, data interface{}, pagination Pagination) {
	c.JSON(http.StatusOK, PaginatedResponse{
		Data:       data,
		Pagination: pagination,
	})
}

// Error sends an error response based on AppError
func Error(c *gin.Context, err error) {
	// Check if it's an AppError
	if appErr, ok := errors.GetAppError(err); ok {
		c.JSON(appErr.StatusCode, ErrorResponse{
			Error: appErr.Message,
			Code:  appErr.Code,
		})
		return
	}

	// Default to internal server error
	c.JSON(http.StatusInternalServerError, ErrorResponse{
		Error: "Internal server error",
		Code:  errors.CodeInternal,
	})
}

// ErrorWithDetails sends an error response with additional details
func ErrorWithDetails(c *gin.Context, err error, details interface{}) {
	// Check if it's an AppError
	if appErr, ok := errors.GetAppError(err); ok {
		c.JSON(appErr.StatusCode, ErrorResponse{
			Error:   appErr.Message,
			Code:    appErr.Code,
			Details: details,
		})
		return
	}

	// Default to internal server error
	c.JSON(http.StatusInternalServerError, ErrorResponse{
		Error:   "Internal server error",
		Code:    errors.CodeInternal,
		Details: details,
	})
}

// ValidationError sends a validation error response
func ValidationError(c *gin.Context, validationErrors interface{}) {
	c.JSON(http.StatusBadRequest, ErrorResponse{
		Error:   "Validation failed",
		Code:    errors.CodeValidation,
		Details: validationErrors,
	})
}

// Unauthorized sends an unauthorized response
func Unauthorized(c *gin.Context, message string) {
	if message == "" {
		message = "Unauthorized"
	}
	c.JSON(http.StatusUnauthorized, ErrorResponse{
		Error: message,
		Code:  errors.CodeUnauthorized,
	})
}

// Forbidden sends a forbidden response
func Forbidden(c *gin.Context, message string) {
	if message == "" {
		message = "Forbidden"
	}
	c.JSON(http.StatusForbidden, ErrorResponse{
		Error: message,
		Code:  errors.CodeForbidden,
	})
}

// NotFound sends a not found response
func NotFound(c *gin.Context, resource string) {
	message := "Resource not found"
	if resource != "" {
		message = resource + " not found"
	}
	c.JSON(http.StatusNotFound, ErrorResponse{
		Error: message,
		Code:  errors.CodeNotFound,
	})
}

// BadRequest sends a bad request response
func BadRequest(c *gin.Context, message string) {
	if message == "" {
		message = "Bad request"
	}
	c.JSON(http.StatusBadRequest, ErrorResponse{
		Error: message,
		Code:  errors.CodeBadRequest,
	})
}

// InternalServerError sends an internal server error response
func InternalServerError(c *gin.Context) {
	c.JSON(http.StatusInternalServerError, ErrorResponse{
		Error: "Internal server error",
		Code:  errors.CodeInternal,
	})
}
