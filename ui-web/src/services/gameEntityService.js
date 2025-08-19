import api from './api';

const gameEntityService = {
  // Get all game entities with pagination
  async getGameEntities(type, page = 1, limit = 20, filters = {}) {
    if (!type) {
      throw new Error('Entity type is required');
    }

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
    if (!type) {
      throw new Error('Entity type is required');
    }
    if (!id) {
      throw new Error('Entity ID is required');
    }

    try {
      const response = await api.get(`/game-entities/${type}/${id}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Create a new game entity
  async createGameEntity(type, entityData) {
    if (!type) {
      throw new Error('Entity type is required');
    }
    if (!entityData) {
      throw new Error('Entity data is required');
    }
    if (!entityData.name) {
      throw new Error('Entity name is required');
    }

    try {
      const response = await api.post(`/game-entities/${type}`, entityData);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Update an existing game entity
  async updateGameEntity(type, id, entityData) {
    if (!type) {
      throw new Error('Entity type is required');
    }
    if (!id) {
      throw new Error('Entity ID is required');
    }
    if (!entityData) {
      throw new Error('Entity data is required');
    }
    if (!entityData.name) {
      throw new Error('Entity name is required');
    }

    try {
      const response = await api.put(`/game-entities/${type}/${id}`, entityData);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Delete a game entity
  async deleteGameEntity(type, id) {
    if (!type) {
      throw new Error('Entity type is required');
    }
    if (!id) {
      throw new Error('Entity ID is required');
    }

    try {
      const response = await api.delete(`/game-entities/${type}/${id}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Search game entities
  async searchGameEntities(type, query, page = 1, limit = 20) {
    if (!type) {
      throw new Error('Entity type is required');
    }
    if (!query) {
      throw new Error('Search query is required');
    }

    try {
      const params = new URLSearchParams({
        query: query,
        page: page.toString(),
        limit: limit.toString()
      });
      
      const response = await api.get(`/game-entities/${type}/search?${params}`);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Duplicate a game entity
  async duplicateGameEntity(type, id, newName) {
    if (!type) {
      throw new Error('Entity type is required');
    }
    if (!id) {
      throw new Error('Entity ID is required');
    }

    try {
      // Fetch the original entity
      const original = await this.getGameEntity(type, id);
      
      // Create a copy with a new name
      const duplicate = {
        ...original,
        id: undefined,
        name: newName || `Copy of ${original.name}`
      };
      
      // Create the duplicate
      return await this.createGameEntity(type, duplicate);
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Bulk update game entities
  async bulkUpdateGameEntities(type, entities) {
    if (!type) {
      throw new Error('Entity type is required');
    }
    if (!entities || !Array.isArray(entities)) {
      throw new Error('Entities array is required');
    }
    if (entities.length === 0) {
      throw new Error('At least one entity is required');
    }

    try {
      const response = await api.put(`/game-entities/${type}/bulk`, { entities });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Bulk delete game entities
  async bulkDeleteGameEntities(type, ids) {
    if (!type) {
      throw new Error('Entity type is required');
    }
    if (!ids || !Array.isArray(ids)) {
      throw new Error('IDs array is required');
    }
    if (ids.length === 0) {
      throw new Error('At least one ID is required');
    }

    try {
      const response = await api.delete(`/game-entities/${type}/bulk`, { data: { ids } });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Get game entity by name
  async getGameEntityByName(type, name) {
    if (!type) {
      throw new Error('Entity type is required');
    }
    if (!name) {
      throw new Error('Entity name is required');
    }

    try {
      const response = await this.getGameEntities(type, 1, 1000);
      const items = response.items || response.data || [];
      return items.find(entity => entity.name === name) || null;
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

  // Equipment
  async getEquipment(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('equipment', page, limit, filters);
  },

  async getEquipmentItem(id) {
    return this.getGameEntity('equipment', id);
  },

  async createEquipmentItem(equipmentData) {
    return this.createGameEntity('equipment', equipmentData);
  },

  async updateEquipmentItem(id, equipmentData) {
    return this.updateGameEntity('equipment', id, equipmentData);
  },

  async deleteEquipmentItem(id) {
    return this.deleteGameEntity('equipment', id);
  },

  // Powers
  async getPowers(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('powers', page, limit, filters);
  },

  async getPower(id) {
    return this.getGameEntity('powers', id);
  },

  async createPower(powerData) {
    return this.createGameEntity('powers', powerData);
  },

  async updatePower(id, powerData) {
    return this.updateGameEntity('powers', id, powerData);
  },

  async deletePower(id) {
    return this.deleteGameEntity('powers', id);
  },

  // Edges
  async getEdges(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('edges', page, limit, filters);
  },

  async getEdge(id) {
    return this.getGameEntity('edges', id);
  },

  async createEdge(edgeData) {
    return this.createGameEntity('edges', edgeData);
  },

  async updateEdge(id, edgeData) {
    return this.updateGameEntity('edges', id, edgeData);
  },

  async deleteEdge(id) {
    return this.deleteGameEntity('edges', id);
  },

  // Hindrances
  async getHindrances(page = 1, limit = 20, filters = {}) {
    return this.getGameEntities('hindrances', page, limit, filters);
  },

  async getHindrance(id) {
    return this.getGameEntity('hindrances', id);
  },

  async createHindrance(hindranceData) {
    return this.createGameEntity('hindrances', hindranceData);
  },

  async updateHindrance(id, hindranceData) {
    return this.updateGameEntity('hindrances', id, hindranceData);
  },

  async deleteHindrance(id) {
    return this.deleteGameEntity('hindrances', id);
  }
};

export default gameEntityService;