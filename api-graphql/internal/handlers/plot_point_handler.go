package handlers

import (
	"encoding/json"
	"net/http"

	"github.com/gin-gonic/gin"
	"github.com/google/uuid"
	"github.com/jimbarrows/savage-worlds-api/internal/models"
	"github.com/jimbarrows/savage-worlds-api/internal/repository"
	"github.com/jimbarrows/savage-worlds-api/pkg/response"
	"github.com/jimbarrows/savage-worlds-api/pkg/utils"
)

// PlotPointHandler handles plot point endpoints
type PlotPointHandler struct {
	plotPointRepo *repository.PlotPointRepository
}

// NewPlotPointHandler creates a new plot point handler
func NewPlotPointHandler(plotPointRepo *repository.PlotPointRepository) *PlotPointHandler {
	return &PlotPointHandler{
		plotPointRepo: plotPointRepo,
	}
}

// Create creates a new plot point
// @Summary Create plot point
// @Description Create a new plot point
// @Tags plot-points
// @Security BearerAuth
// @Accept json
// @Produce json
// @Param request body models.CreatePlotPointRequest true "Plot point details"
// @Success 201 {object} models.PlotPoint
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Router /api/v1/plot-points [post]
func (h *PlotPointHandler) Create(c *gin.Context) {
	var req models.CreatePlotPointRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Set default basic rules if not provided
	basicRules := models.DefaultBasicRules()
	if req.BasicRules != nil {
		basicRules = *req.BasicRules
	}

	plotPoint := &models.PlotPoint{
		OwnerID:    userID,
		Name:       req.Name,
		BasicRules: basicRules,
	}

	if req.Description != "" {
		plotPoint.Description = &req.Description
	}

	if err := h.plotPointRepo.Create(c.Request.Context(), plotPoint); err != nil {
		response.Error(c, err)
		return
	}

	response.Created(c, plotPoint)
}

// GetByID retrieves a plot point by ID
// @Summary Get plot point
// @Description Get a plot point by ID
// @Tags plot-points
// @Security BearerAuth
// @Produce json
// @Param id path string true "Plot point ID"
// @Success 200 {object} models.PlotPoint
// @Failure 401 {object} response.ErrorResponse
// @Failure 404 {object} response.ErrorResponse
// @Router /api/v1/plot-points/{id} [get]
func (h *PlotPointHandler) GetByID(c *gin.Context) {
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

	plotPoint, err := h.plotPointRepo.GetByID(c.Request.Context(), plotPointID)
	if err != nil {
		response.Error(c, err)
		return
	}

	// Check ownership
	if plotPoint.OwnerID != userID {
		response.Forbidden(c, "You don't have permission to access this plot point")
		return
	}

	response.OK(c, plotPoint)
}

// Update updates a plot point
// @Summary Update plot point
// @Description Update a plot point
// @Tags plot-points
// @Security BearerAuth
// @Accept json
// @Produce json
// @Param id path string true "Plot point ID"
// @Param request body models.UpdatePlotPointRequest true "Update details"
// @Success 200 {object} models.PlotPoint
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Failure 404 {object} response.ErrorResponse
// @Router /api/v1/plot-points/{id} [put]
func (h *PlotPointHandler) Update(c *gin.Context) {
	plotPointID, err := uuid.Parse(c.Param("id"))
	if err != nil {
		response.BadRequest(c, "Invalid plot point ID")
		return
	}

	var req models.UpdatePlotPointRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Check ownership
	isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), plotPointID, userID)
	if err != nil {
		response.Error(c, err)
		return
	}
	if !isOwner {
		response.Forbidden(c, "You don't have permission to update this plot point")
		return
	}

	// Update plot point
	if err := h.plotPointRepo.Update(c.Request.Context(), plotPointID, &req); err != nil {
		response.Error(c, err)
		return
	}

	// Fetch updated plot point
	plotPoint, err := h.plotPointRepo.GetByID(c.Request.Context(), plotPointID)
	if err != nil {
		response.Error(c, err)
		return
	}

	response.OK(c, plotPoint)
}

// Delete deletes a plot point
// @Summary Delete plot point
// @Description Delete a plot point
// @Tags plot-points
// @Security BearerAuth
// @Param id path string true "Plot point ID"
// @Success 204
// @Failure 401 {object} response.ErrorResponse
// @Failure 404 {object} response.ErrorResponse
// @Router /api/v1/plot-points/{id} [delete]
func (h *PlotPointHandler) Delete(c *gin.Context) {
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

	// Check ownership
	isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), plotPointID, userID)
	if err != nil {
		response.Error(c, err)
		return
	}
	if !isOwner {
		response.Forbidden(c, "You don't have permission to delete this plot point")
		return
	}

	// Delete plot point
	if err := h.plotPointRepo.Delete(c.Request.Context(), plotPointID); err != nil {
		response.Error(c, err)
		return
	}

	response.NoContent(c)
}

// List retrieves a paginated list of plot points for the current user
// @Summary List plot points
// @Description Get a paginated list of plot points for the authenticated user
// @Tags plot-points
// @Security BearerAuth
// @Produce json
// @Param page query int false "Page number" default(1)
// @Param per_page query int false "Items per page" default(20)
// @Success 200 {object} response.PaginatedResponse
// @Failure 401 {object} response.ErrorResponse
// @Router /api/v1/plot-points [get]
func (h *PlotPointHandler) List(c *gin.Context) {
	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	pagination := utils.GetPaginationParams(c)

	plotPoints, total, err := h.plotPointRepo.List(
		c.Request.Context(),
		userID,
		pagination.Offset,
		pagination.PerPage,
	)
	if err != nil {
		response.Error(c, err)
		return
	}

	response.Paginated(c, plotPoints, response.Pagination{
		Page:       pagination.Page,
		PerPage:    pagination.PerPage,
		Total:      total,
		TotalPages: utils.CalculateTotalPages(total, pagination.PerPage),
	})
}

// GetEntityData retrieves specific entity data from a plot point
// @Summary Get entity data
// @Description Get specific entity data (e.g., characters, beasts) from a plot point
// @Tags plot-points
// @Security BearerAuth
// @Produce json
// @Param id path string true "Plot point ID"
// @Param entity_type path string true "Entity type (characters, beasts, etc.)"
// @Success 200 {object} json.RawMessage
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Failure 404 {object} response.ErrorResponse
// @Router /api/v1/plot-points/{id}/{entity_type} [get]
func (h *PlotPointHandler) GetEntityData(c *gin.Context) {
	plotPointID, err := uuid.Parse(c.Param("id"))
	if err != nil {
		response.BadRequest(c, "Invalid plot point ID")
		return
	}

	entityTypeStr := c.Param("entity_type")
	entityType := models.EntityType(entityTypeStr)
	if !entityType.IsValid() {
		response.BadRequest(c, "Invalid entity type")
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Check ownership
	isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), plotPointID, userID)
	if err != nil {
		response.Error(c, err)
		return
	}
	if !isOwner {
		response.Forbidden(c, "You don't have permission to access this plot point")
		return
	}

	// Get entity data
	data, err := h.plotPointRepo.GetEntityData(c.Request.Context(), plotPointID, entityType)
	if err != nil {
		response.Error(c, err)
		return
	}

	// Return raw JSON
	c.Data(http.StatusOK, "application/json", data)
}

// UpdateEntityData updates specific entity data in a plot point
// @Summary Update entity data
// @Description Update specific entity data (e.g., characters, beasts) in a plot point
// @Tags plot-points
// @Security BearerAuth
// @Accept json
// @Produce json
// @Param id path string true "Plot point ID"
// @Param entity_type path string true "Entity type (characters, beasts, etc.)"
// @Param data body json.RawMessage true "Entity data"
// @Success 200 {object} response.SuccessResponse
// @Failure 400 {object} response.ErrorResponse
// @Failure 401 {object} response.ErrorResponse
// @Failure 404 {object} response.ErrorResponse
// @Router /api/v1/plot-points/{id}/{entity_type} [put]
func (h *PlotPointHandler) UpdateEntityData(c *gin.Context) {
	plotPointID, err := uuid.Parse(c.Param("id"))
	if err != nil {
		response.BadRequest(c, "Invalid plot point ID")
		return
	}

	entityTypeStr := c.Param("entity_type")
	entityType := models.EntityType(entityTypeStr)
	if !entityType.IsValid() {
		response.BadRequest(c, "Invalid entity type")
		return
	}

	userID, err := utils.GetUserIDFromContext(c)
	if err != nil {
		response.Unauthorized(c, "User not authenticated")
		return
	}

	// Check ownership
	isOwner, err := h.plotPointRepo.IsOwner(c.Request.Context(), plotPointID, userID)
	if err != nil {
		response.Error(c, err)
		return
	}
	if !isOwner {
		response.Forbidden(c, "You don't have permission to update this plot point")
		return
	}

	// Read raw JSON from request body
	var data json.RawMessage
	if err := c.ShouldBindJSON(&data); err != nil {
		response.ValidationError(c, err.Error())
		return
	}

	// Validate that it's a valid JSON array
	var testArray []interface{}
	if err := json.Unmarshal(data, &testArray); err != nil {
		response.ValidationError(c, "Entity data must be a valid JSON array")
		return
	}

	// Update entity data
	if err := h.plotPointRepo.UpdateEntityData(c.Request.Context(), plotPointID, entityType, data); err != nil {
		response.Error(c, err)
		return
	}

	response.SuccessWithMessage(c, http.StatusOK, "Entity data updated successfully", nil)
}