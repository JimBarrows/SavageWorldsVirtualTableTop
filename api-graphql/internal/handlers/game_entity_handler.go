package handlers

import (
	"github.com/gin-gonic/gin"
	"github.com/google/uuid"
	"github.com/jimbarrows/savage-worlds-api/internal/models"
	"github.com/jimbarrows/savage-worlds-api/internal/repository"
	"github.com/jimbarrows/savage-worlds-api/pkg/response"
	"github.com/jimbarrows/savage-worlds-api/pkg/utils"
)

// GameEntityHandler handles game entity endpoints
type GameEntityHandler struct {
	gameEntityRepo *repository.GameEntityRepository
	plotPointRepo  *repository.PlotPointRepository
}

// NewGameEntityHandler creates a new game entity handler
func NewGameEntityHandler(gameEntityRepo *repository.GameEntityRepository, plotPointRepo *repository.PlotPointRepository) *GameEntityHandler {
	return &GameEntityHandler{
		gameEntityRepo: gameEntityRepo,
		plotPointRepo:  plotPointRepo,
	}
}

// Create creates a new game entity
// @Summary Create game entity
// @Description Create a new game entity
// @Tags game-entities
// @Security BearerAuth
// @Accept json
// @Produce json
// @Param request body models.CreateGameEntityRequest true "Game entity details"
// @Success 201 {object} models.GameEntity
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Failure 403 {object} response.ErrorResponse
// @Router /api/v1/game-entities [post]
func (h *GameEntityHandler) Create(c *gin.Context) {
	var req models.CreateGameEntityRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Verify user has access to the plot point
	isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), req.PlotPointID, userID)
	if err != nil {
		response.Error(c, err)
		return
	}
	if !isOwner {
		response.Forbidden(c, "You don't have permission to create entities in this plot point")
		return
	}

	// Validate entity type
	if !models.IsValidEntityType(req.EntityType) {
		response.BadRequest(c, "Invalid entity type")
		return
	}

	gameEntity := &models.GameEntity{
		PlotPointID: req.PlotPointID,
		OwnerID:     userID,
		EntityType:  req.EntityType,
		Name:        req.Name,
		Data:        req.Data,
		IsTemplate:  req.IsTemplate,
		IsActive:    true,
	}

	if req.Description != "" {
		gameEntity.Description = &req.Description
	}

	if err := h.gameEntityRepo.Create(c.Request.Context(), gameEntity); err != nil {
		response.Error(c, err)
		return
	}

	response.Created(c, gameEntity)
}

// GetByID retrieves a game entity by ID
// @Summary Get game entity
// @Description Get a game entity by ID
// @Tags game-entities
// @Security BearerAuth
// @Produce json
// @Param id path string true "Game entity ID"
// @Success 200 {object} models.GameEntity
// @Failure 401 {object} response.ErrorResponse
// @Failure 404 {object} response.ErrorResponse
// @Router /api/v1/game-entities/{id} [get]
func (h *GameEntityHandler) GetByID(c *gin.Context) {
	entityID, err := uuid.Parse(c.Param("id"))
	if err != nil {
		response.BadRequest(c, "Invalid entity ID")
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	gameEntity, err := h.gameEntityRepo.GetByID(c.Request.Context(), entityID)
	if err != nil {
		response.Error(c, err)
		return
	}

	// Check ownership or plot point access
	if gameEntity.OwnerID != userID {
		// Check if user owns the plot point
		isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), gameEntity.PlotPointID, userID)
		if err != nil {
			response.Error(c, err)
			return
		}
		if !isOwner {
			response.Forbidden(c, "You don't have permission to access this entity")
			return
		}
	}

	response.OK(c, gameEntity)
}

// Update updates a game entity
// @Summary Update game entity
// @Description Update a game entity
// @Tags game-entities
// @Security BearerAuth
// @Accept json
// @Produce json
// @Param id path string true "Game entity ID"
// @Param request body models.UpdateGameEntityRequest true "Update details"
// @Success 200 {object} models.GameEntity
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Failure 404 {object} response.ErrorResponse
// @Router /api/v1/game-entities/{id} [put]
func (h *GameEntityHandler) Update(c *gin.Context) {
	entityID, err := uuid.Parse(c.Param("id"))
	if err != nil {
		response.BadRequest(c, "Invalid entity ID")
		return
	}

	var req models.UpdateGameEntityRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Get entity to check ownership
	ownerID, plotPointID, err := h.gameEntityRepo.GetOwnerAndPlotPoint(c.Request.Context(), entityID)
	if err != nil {
		response.Error(c, err)
		return
	}

	// Check ownership or plot point access
	if ownerID != userID {
		// Check if user owns the plot point
		isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), plotPointID, userID)
		if err != nil {
			response.Error(c, err)
			return
		}
		if !isOwner {
			response.Forbidden(c, "You don't have permission to update this entity")
			return
		}
	}

	// Update entity
	if err := h.gameEntityRepo.Update(c.Request.Context(), entityID, &req); err != nil {
		response.Error(c, err)
		return
	}

	// Fetch updated entity
	gameEntity, err := h.gameEntityRepo.GetByID(c.Request.Context(), entityID)
	if err != nil {
		response.Error(c, err)
		return
	}

	response.OK(c, gameEntity)
}

// Delete deletes a game entity
// @Summary Delete game entity
// @Description Delete a game entity
// @Tags game-entities
// @Security BearerAuth
// @Param id path string true "Game entity ID"
// @Success 204
// @Failure 401 {object} response.ErrorResponse
// @Failure 404 {object} response.ErrorResponse
// @Router /api/v1/game-entities/{id} [delete]
func (h *GameEntityHandler) Delete(c *gin.Context) {
	entityID, err := uuid.Parse(c.Param("id"))
	if err != nil {
		response.BadRequest(c, "Invalid entity ID")
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Get entity to check ownership
	ownerID, plotPointID, err := h.gameEntityRepo.GetOwnerAndPlotPoint(c.Request.Context(), entityID)
	if err != nil {
		response.Error(c, err)
		return
	}

	// Check ownership or plot point access
	if ownerID != userID {
		// Check if user owns the plot point
		isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), plotPointID, userID)
		if err != nil {
			response.Error(c, err)
			return
		}
		if !isOwner {
			response.Forbidden(c, "You don't have permission to delete this entity")
			return
		}
	}

	// Delete entity
	if err := h.gameEntityRepo.Delete(c.Request.Context(), entityID); err != nil {
		response.Error(c, err)
		return
	}

	response.NoContent(c)
}

// List retrieves a filtered list of game entities
// @Summary List game entities
// @Description Get a filtered list of game entities
// @Tags game-entities
// @Security BearerAuth
// @Produce json
// @Param plot_point_id query string false "Filter by plot point ID"
// @Param entity_type query string false "Filter by entity type"
// @Param is_template query bool false "Filter by template status"
// @Param is_active query bool false "Filter by active status"
// @Param search query string false "Search in name and description"
// @Param page query int false "Page number" default(1)
// @Param per_page query int false "Items per page" default(20)
// @Success 200 {object} response.PaginatedResponse
// @Failure 401 {object} response.ErrorResponse
// @Router /api/v1/game-entities [get]
func (h *GameEntityHandler) List(c *gin.Context) {
	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	filter := &models.GameEntityFilter{}

	// Parse filter parameters
	if plotPointIDStr := c.Query("plot_point_id"); plotPointIDStr != "" {
		plotPointID, err := uuid.Parse(plotPointIDStr)
		if err != nil {
			response.BadRequest(c, "Invalid plot point ID")
			return
		}

		// Verify user has access to the plot point
		isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), plotPointID, userID)
		if err != nil {
			response.Error(c, err)
			return
		}
		if !isOwner {
			response.Forbidden(c, "You don't have permission to access entities in this plot point")
			return
		}

		filter.PlotPointID = &plotPointID
	}

	if entityType := c.Query("entity_type"); entityType != "" {
		if !models.IsValidEntityType(entityType) {
			response.BadRequest(c, "Invalid entity type")
			return
		}
		filter.EntityType = &entityType
	}

	if isTemplateStr := c.Query("is_template"); isTemplateStr != "" {
		isTemplate := isTemplateStr == "true"
		filter.IsTemplate = &isTemplate
	}

	if isActiveStr := c.Query("is_active"); isActiveStr != "" {
		isActive := isActiveStr == "true"
		filter.IsActive = &isActive
	}

	if search := c.Query("search"); search != "" {
		filter.Search = &search
	}

	pagination := utils.GetPaginationParams(c)

	entities, total, err := h.gameEntityRepo.List(
		c.Request.Context(),
		filter,
		pagination.Offset,
		pagination.PerPage,
	)
	if err != nil {
		response.Error(c, err)
		return
	}

	response.Paginated(c, entities, response.Pagination{
		Page:       pagination.Page,
		PerPage:    pagination.PerPage,
		Total:      total,
		TotalPages: utils.CalculateTotalPages(total, pagination.PerPage),
	})
}

// ListByPlotPoint retrieves entities for a specific plot point
// @Summary List entities by plot point
// @Description Get entities for a specific plot point
// @Tags game-entities
// @Security BearerAuth
// @Produce json
// @Param plot_point_id path string true "Plot point ID"
// @Param entity_type query string false "Filter by entity type"
// @Param page query int false "Page number" default(1)
// @Param per_page query int false "Items per page" default(20)
// @Success 200 {object} response.PaginatedResponse
// @Failure 401 {object} response.ErrorResponse
// @Failure 403 {object} response.ErrorResponse
// @Router /api/v1/plot-points/{plot_point_id}/entities [get]
func (h *GameEntityHandler) ListByPlotPoint(c *gin.Context) {
	plotPointID, err := uuid.Parse(c.Param("id"))
	if err != nil {
		response.BadRequest(c, "Invalid plot point ID")
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Verify user has access to the plot point
	isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), plotPointID, userID)
	if err != nil {
		response.Error(c, err)
		return
	}
	if !isOwner {
		response.Forbidden(c, "You don't have permission to access entities in this plot point")
		return
	}

	entityType := c.Query("entity_type")
	if entityType != "" && !models.IsValidEntityType(entityType) {
		response.BadRequest(c, "Invalid entity type")
		return
	}

	pagination := utils.GetPaginationParams(c)

	entities, total, err := h.gameEntityRepo.ListByPlotPoint(
		c.Request.Context(),
		plotPointID,
		entityType,
		pagination.Offset,
		pagination.PerPage,
	)
	if err != nil {
		response.Error(c, err)
		return
	}

	response.Paginated(c, entities, response.Pagination{
		Page:       pagination.Page,
		PerPage:    pagination.PerPage,
		Total:      total,
		TotalPages: utils.CalculateTotalPages(total, pagination.PerPage),
	})
}

// CreateFromTemplate creates a new entity from a template
// @Summary Create entity from template
// @Description Create a new entity based on a template
// @Tags game-entities
// @Security BearerAuth
// @Accept json
// @Produce json
// @Param template_id path string true "Template ID"
// @Param request body struct{PlotPointID string `json:"plot_point_id" binding:"required,uuid"`; Name string `json:"name" binding:"required,min=1,max=255"`} true "Entity details"
// @Success 201 {object} models.GameEntity
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Failure 403 {object} response.ErrorResponse
// @Failure 404 {object} response.ErrorResponse
// @Router /api/v1/game-entities/templates/{template_id}/create [post]
func (h *GameEntityHandler) CreateFromTemplate(c *gin.Context) {
	templateID, err := uuid.Parse(c.Param("template_id"))
	if err != nil {
		response.BadRequest(c, "Invalid template ID")
		return
	}

	var req struct {
		PlotPointID string `json:"plot_point_id" binding:"required,uuid"`
		Name        string `json:"name" binding:"required,min=1,max=255"`
	}
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	plotPointID, err := uuid.Parse(req.PlotPointID)
	if err != nil {
		response.BadRequest(c, "Invalid plot point ID")
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Verify user has access to the plot point
	isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), plotPointID, userID)
	if err != nil {
		response.Error(c, err)
		return
	}
	if !isOwner {
		response.Forbidden(c, "You don't have permission to create entities in this plot point")
		return
	}

	// Create entity from template
	gameEntity, err := h.gameEntityRepo.CreateFromTemplate(
		c.Request.Context(),
		templateID,
		plotPointID,
		userID,
		req.Name,
	)
	if err != nil {
		response.Error(c, err)
		return
	}

	response.Created(c, gameEntity)
}