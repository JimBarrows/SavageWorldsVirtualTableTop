import api from './api';

const gameEntityService = {
  // Get all game entities with pagination
  async getGameEntities(type, page = 1, limit = 20, filters = {}) {
    try {
      const params = new URLSearchParams({
        page: page.toString(),
        limit: limit.toString(),
        ...filters
      });
      
      const response = await api.get(`/game-entities/${type}?${params}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Get a single game entity by ID
  async getGameEntity(type, id) {
    try {
      const response = await api.get(`/game-entities/${type}/${id}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Create a new game entity
  async createGameEntity(type, entityData) {
    try {
      const response = await api.post(`/game-entities/${type}`, entityData);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Update an existing game entity
  async updateGameEntity(type, id, entityData) {
    try {
      const response = await api.put(`/game-entities/${type}/${id}`, entityData);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Delete a game entity
  async deleteGameEntity(type, id) {
    try {
      const response = await api.delete(`/game-entities/${type}/${id}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Search game entities
  async searchGameEntities(type, query, page = 1, limit = 20) {
    try {
      const params = new URLSearchParams({
        q: query,
        page: page.toString(),
        limit: limit.toString()
      });
      
      const response = await api.get(`/game-entities/${type}/search?${params}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Entity type specific methods
  async getCharacters(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('characters', page, limit, filters);
  },

  async getCharacter(id) {
    return this.getGameEntity('characters', id);
  },

  async createCharacter(characterData) {
    return this.createGameEntity('characters', characterData);
  },

  async updateCharacter(id, characterData) {
    return this.updateGameEntity('characters', id, characterData);
  },

  async deleteCharacter(id) {
    return this.deleteGameEntity('characters', id);
  },

  // Beasts
  async getBeasts(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('beasts', page, limit, filters);
  },

  async getBeast(id) {
    return this.getGameEntity('beasts', id);
  },

  async createBeast(beastData) {
    return this.createGameEntity('beasts', beastData);
  },

  async updateBeast(id, beastData) {
    return this.updateGameEntity('beasts', id, beastData);
  },

  async deleteBeast(id) {
    return this.deleteGameEntity('beasts', id);
  },

  // Vehicles
  async getVehicles(vehicleType, page = 1, limit = 20, filters = {}) {
    return this.getGameEntities(`vehicles/${vehicleType}`, page, limit, filters);
  },

  async getVehicle(vehicleType, id) {
    return this.getGameEntity(`vehicles/${vehicleType}`, id);
  },

  async createVehicle(vehicleType, vehicleData) {
    return this.createGameEntity(`vehicles/${vehicleType}`, vehicleData);
  },

  async updateVehicle(vehicleType, id, vehicleData) {
    return this.updateGameEntity(`vehicles/${vehicleType}`, id, vehicleData);
  },

  async deleteVehicle(vehicleType, id) {
    return this.deleteGameEntity(`vehicles/${vehicleType}`, id);
  },

  // Gear
  async getGear(gearType, page = 1, limit = 20, filters = {}) {
    return this.getGameEntities(`gear/${gearType}`, page, limit, filters);
  },

  async getGearItem(gearType, id) {
    return this.getGameEntity(`gear/${gearType}`, id);
  },

  async createGearItem(gearType, gearData) {
    return this.createGameEntity(`gear/${gearType}`, gearData);
  },

  async updateGearItem(gearType, id, gearData) {
    return this.updateGameEntity(`gear/${gearType}`, id, gearData);
  },

  async deleteGearItem(gearType, id) {
    return this.deleteGameEntity(`gear/${gearType}`, id);
  },

  // Rules and backgrounds
  async getEdges(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('edges', page, limit, filters);
  },

  async getHindrances(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('hindrances', page, limit, filters);
  },

  async getSkills(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('skills', page, limit, filters);
  },

  async getPowers(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('powers', page, limit, filters);
  },

  async getRaces(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('races', page, limit, filters);
  },

  async getArcaneBackgrounds(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('arcane-backgrounds', page, limit, filters);
  },

  async getSettingRules(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('setting-rules', page, limit, filters);
  },

  // Batch operations
  async batchCreateEntities(type, entities) {
    try {
      const response = await api.post(`/game-entities/${type}/batch`, { entities });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  async batchUpdateEntities(type, updates) {
    try {
      const response = await api.put(`/game-entities/${type}/batch`, { updates });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  async batchDeleteEntities(type, ids) {
    try {
      const response = await api.delete(`/game-entities/${type}/batch`, { data: { ids } });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  }
};

export default gameEntityService;