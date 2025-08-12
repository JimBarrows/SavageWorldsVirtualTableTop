import api from './api';

const sceneService = {
  // Get all scenes with pagination
  async getScenes(page = 1, limit = 20, filters = {}) {
    try {
      const params = new URLSearchParams({
        page: page.toString(),
        limit: limit.toString(),
        ...filters
      });
      
      const response = await api.get(`/scenes?${params}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Get a single scene by ID
  async getScene(id) {
    if (!id) {
      throw new Error('Scene ID is required');
    }

    try {
      const response = await api.get(`/scenes/${id}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Create a new scene
  async createScene(scene) {
    if (!scene) {
      throw new Error('Scene is required');
    }
    if (!scene.name) {
      throw new Error('Scene name is required');
    }

    try {
      const response = await api.post('/scenes', scene);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Update an existing scene
  async updateScene(scene) {
    if (!scene) {
      throw new Error('Scene is required');
    }
    if (!scene.id) {
      throw new Error('Scene ID is required');
    }
    if (!scene.name) {
      throw new Error('Scene name is required');
    }

    try {
      const response = await api.put(`/scenes/${scene.id}`, scene);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Delete a scene
  async deleteScene(id) {
    if (!id) {
      throw new Error('Scene ID is required');
    }

    try {
      const response = await api.delete(`/scenes/${id}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Get scene by name (searches through all scenes)
  async getSceneByName(name) {
    if (!name) {
      throw new Error('Scene name is required');
    }

    try {
      const response = await this.getScenes(1, 1000); // Get large batch to search
      const scenes = response.items || response.data || [];
      return scenes.find(scene => scene.name === name) || null;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Legacy method name for compatibility
  async listScenes() {
    const response = await this.getScenes();
    return response.items || response.data || [];
  }
};

export default sceneService;