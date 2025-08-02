package routes

import (
	"github.com/gin-gonic/gin"
	"github.com/jimbarrows/savage-worlds-api/internal/handlers"
)

// SetupEmailAuthRoutes sets up authentication routes that use email only (no username)
func SetupEmailAuthRoutes(router *gin.RouterGroup, authHandler *handlers.AuthHandler) {
	auth := router.Group("/auth")
	{
		// Public routes - these use the email-only versions
		auth.POST("/register", authHandler.RegisterEmailOnly)
		auth.POST("/login", authHandler.LoginEmailOnly)
		auth.POST("/refresh", authHandler.RefreshToken)
		auth.POST("/forgot-password", authHandler.ForgotPassword)
		auth.POST("/reset-password", authHandler.ResetPassword)
		
		// Protected routes (require authentication)
		protected := auth.Group("")
		protected.Use(authMiddleware())
		{
			protected.POST("/logout", authHandler.Logout)
			protected.GET("/me", authHandler.GetCurrentUser)
			protected.POST("/change-password", authHandler.ChangePassword)
		}
	}
}