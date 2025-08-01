import api from './api';

const plotPointService = {
  // Get all plot points with pagination
  async getPlotPoints(page = 1, limit = 20, filters = {}) {
    try {
      const params = new URLSearchParams({
        page: page.toString(),
        limit: limit.toString(),
        ...filters
      });
      
      const response = await api.get(`/plot-points?${params}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Get a single plot point by ID
  async getPlotPoint(id) {
    try {
      const response = await api.get(`/plot-points/${id}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Create a new plot point
  async createPlotPoint(plotPointData) {
    try {
      const response = await api.post('/plot-points', plotPointData);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Update an existing plot point
  async updatePlotPoint(id, plotPointData) {
    try {
      const response = await api.put(`/plot-points/${id}`, plotPointData);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Delete a plot point
  async deletePlotPoint(id) {
    try {
      const response = await api.delete(`/plot-points/${id}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Search plot points
  async searchPlotPoints(query, page = 1, limit = 20) {
    try {
      const params = new URLSearchParams({
        q: query,
        page: page.toString(),
        limit: limit.toString()
      });
      
      const response = await api.get(`/plot-points/search?${params}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Get plot point by name (for backward compatibility)
  async getPlotPointByName(name) {
    try {
      const response = await api.get(`/plot-points/by-name/${encodeURIComponent(name)}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Batch operations
  async batchCreatePlotPoints(plotPoints) {
    try {
      const response = await api.post('/plot-points/batch', { plotPoints });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  async batchUpdatePlotPoints(updates) {
    try {
      const response = await api.put('/plot-points/batch', { updates });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  async batchDeletePlotPoints(ids) {
    try {
      const response = await api.delete('/plot-points/batch', { data: { ids } });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Export plot points
  async exportPlotPoints(format = 'json', ids = []) {
    try {
      const params = new URLSearchParams({ format });
      if (ids.length > 0) {
        params.append('ids', ids.join(','));
      }
      
      const response = await api.get(`/plot-points/export?${params}`, {
        responseType: format === 'json' ? 'json' : 'blob'
      });
      
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Import plot points
  async importPlotPoints(file, format = 'json') {
    try {
      const formData = new FormData();
      formData.append('file', file);
      formData.append('format', format);
      
      const response = await api.post('/plot-points/import', formData, {
        headers: {
          'Content-Type': 'multipart/form-data'
        }
      });
      
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  }
};

export default plotPointService;