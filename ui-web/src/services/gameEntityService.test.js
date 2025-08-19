import gameEntityService from './gameEntityService';
import api from './api';

// Mock the api module
jest.mock('./api');

describe('GameEntityService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('getGameEntities', () => {
    it('should fetch game entities successfully', async () => {
      const mockData = {
        items: [
          { id: '1', name: 'Entity 1' },
          { id: '2', name: 'Entity 2' }
        ],
        total: 2
      };

      api.get.mockResolvedValue({ data: mockData });

      const result = await gameEntityService.getGameEntities('characters');

      expect(api.get).toHaveBeenCalledWith('/game-entities/characters?page=1&limit=20');
      expect(result).toEqual(mockData);
    });

    it('should fetch entities with custom pagination', async () => {
      const mockData = { items: [], total: 0 };
      api.get.mockResolvedValue({ data: mockData });

      await gameEntityService.getGameEntities('beasts', 2, 50);

      expect(api.get).toHaveBeenCalledWith('/game-entities/beasts?page=2&limit=50');
    });

    it('should fetch entities with filters', async () => {
      const mockData = { items: [], total: 0 };
      api.get.mockResolvedValue({ data: mockData });

      await gameEntityService.getGameEntities('characters', 1, 20, { level: 5 });

      expect(api.get).toHaveBeenCalledWith('/game-entities/characters?page=1&limit=20&level=5');
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.getGameEntities()).rejects.toThrow('Entity type is required');
      expect(api.get).not.toHaveBeenCalled();
    });

    it('should handle API errors', async () => {
      const error = {
        response: {
          data: { message: 'Server error' }
        }
      };

      api.get.mockRejectedValue(error);

      await expect(gameEntityService.getGameEntities('characters')).rejects.toEqual({ message: 'Server error' });
    });

    it('should handle network errors', async () => {
      const error = new Error('Network error');
      api.get.mockRejectedValue(error);

      await expect(gameEntityService.getGameEntities('characters')).rejects.toEqual(error);
    });
  });

  describe('getGameEntity', () => {
    it('should fetch a single entity successfully', async () => {
      const mockEntity = { id: '123', name: 'Test Entity' };
      api.get.mockResolvedValue({ data: mockEntity });

      const result = await gameEntityService.getGameEntity('characters', '123');

      expect(api.get).toHaveBeenCalledWith('/game-entities/characters/123');
      expect(result).toEqual(mockEntity);
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.getGameEntity(null, '123')).rejects.toThrow('Entity type is required');
    });

    it('should throw error if id is not provided', async () => {
      await expect(gameEntityService.getGameEntity('characters', null)).rejects.toThrow('Entity ID is required');
    });

    it('should handle API errors', async () => {
      const error = {
        response: {
          data: { message: 'Entity not found' }
        }
      };

      api.get.mockRejectedValue(error);

      await expect(gameEntityService.getGameEntity('characters', '123')).rejects.toEqual({ message: 'Entity not found' });
    });
  });

  describe('createGameEntity', () => {
    it('should create entity successfully', async () => {
      const entityData = { name: 'New Entity', description: 'Test' };
      const mockResponse = { id: '123', ...entityData };

      api.post.mockResolvedValue({ data: mockResponse });

      const result = await gameEntityService.createGameEntity('characters', entityData);

      expect(api.post).toHaveBeenCalledWith('/game-entities/characters', entityData);
      expect(result).toEqual(mockResponse);
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.createGameEntity(null, { name: 'Test' }))
        .rejects.toThrow('Entity type is required');
    });

    it('should throw error if entityData is not provided', async () => {
      await expect(gameEntityService.createGameEntity('characters', null))
        .rejects.toThrow('Entity data is required');
    });

    it('should throw error if name is not provided', async () => {
      await expect(gameEntityService.createGameEntity('characters', { description: 'Test' }))
        .rejects.toThrow('Entity name is required');
    });

    it('should handle API errors', async () => {
      const error = {
        response: {
          data: { message: 'Validation error' }
        }
      };

      api.post.mockRejectedValue(error);

      await expect(gameEntityService.createGameEntity('characters', { name: 'Test' }))
        .rejects.toEqual({ message: 'Validation error' });
    });
  });

  describe('updateGameEntity', () => {
    it('should update entity successfully', async () => {
      const entityData = { name: 'Updated Entity', description: 'Updated' };
      const mockResponse = { id: '123', ...entityData };

      api.put.mockResolvedValue({ data: mockResponse });

      const result = await gameEntityService.updateGameEntity('characters', '123', entityData);

      expect(api.put).toHaveBeenCalledWith('/game-entities/characters/123', entityData);
      expect(result).toEqual(mockResponse);
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.updateGameEntity(null, '123', { name: 'Test' }))
        .rejects.toThrow('Entity type is required');
    });

    it('should throw error if id is not provided', async () => {
      await expect(gameEntityService.updateGameEntity('characters', null, { name: 'Test' }))
        .rejects.toThrow('Entity ID is required');
    });

    it('should throw error if entityData is not provided', async () => {
      await expect(gameEntityService.updateGameEntity('characters', '123', null))
        .rejects.toThrow('Entity data is required');
    });

    it('should throw error if name is not provided', async () => {
      await expect(gameEntityService.updateGameEntity('characters', '123', { description: 'Test' }))
        .rejects.toThrow('Entity name is required');
    });

    it('should handle API errors', async () => {
      const error = {
        response: {
          data: { message: 'Update failed' }
        }
      };

      api.put.mockRejectedValue(error);

      await expect(gameEntityService.updateGameEntity('characters', '123', { name: 'Test' }))
        .rejects.toEqual({ message: 'Update failed' });
    });
  });

  describe('deleteGameEntity', () => {
    it('should delete entity successfully', async () => {
      const mockResponse = { success: true };
      api.delete.mockResolvedValue({ data: mockResponse });

      const result = await gameEntityService.deleteGameEntity('characters', '123');

      expect(api.delete).toHaveBeenCalledWith('/game-entities/characters/123');
      expect(result).toEqual(mockResponse);
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.deleteGameEntity(null, '123'))
        .rejects.toThrow('Entity type is required');
    });

    it('should throw error if id is not provided', async () => {
      await expect(gameEntityService.deleteGameEntity('characters', null))
        .rejects.toThrow('Entity ID is required');
    });

    it('should handle API errors', async () => {
      const error = {
        response: {
          data: { message: 'Delete failed' }
        }
      };

      api.delete.mockRejectedValue(error);

      await expect(gameEntityService.deleteGameEntity('characters', '123'))
        .rejects.toEqual({ message: 'Delete failed' });
    });
  });

  describe('searchGameEntities', () => {
    it('should search entities successfully', async () => {
      const mockData = {
        items: [{ id: '1', name: 'Matching Entity' }],
        total: 1
      };

      api.get.mockResolvedValue({ data: mockData });

      const result = await gameEntityService.searchGameEntities('characters', 'test');

      expect(api.get).toHaveBeenCalledWith('/game-entities/characters/search?query=test&page=1&limit=20');
      expect(result).toEqual(mockData);
    });

    it('should search with custom pagination', async () => {
      const mockData = { items: [], total: 0 };
      api.get.mockResolvedValue({ data: mockData });

      await gameEntityService.searchGameEntities('beasts', 'dragon', 2, 10);

      expect(api.get).toHaveBeenCalledWith('/game-entities/beasts/search?query=dragon&page=2&limit=10');
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.searchGameEntities(null, 'test'))
        .rejects.toThrow('Entity type is required');
    });

    it('should throw error if query is not provided', async () => {
      await expect(gameEntityService.searchGameEntities('characters', null))
        .rejects.toThrow('Search query is required');
    });

    it('should handle API errors', async () => {
      const error = {
        response: {
          data: { message: 'Search failed' }
        }
      };

      api.get.mockRejectedValue(error);

      await expect(gameEntityService.searchGameEntities('characters', 'test'))
        .rejects.toEqual({ message: 'Search failed' });
    });
  });

  describe('duplicateGameEntity', () => {
    it('should duplicate entity successfully', async () => {
      const originalEntity = { id: '123', name: 'Original', description: 'Test' };
      const newEntity = { id: '456', name: 'Copy of Original', description: 'Test' };

      // Mock the internal calls
      gameEntityService.getGameEntity = jest.fn().mockResolvedValue(originalEntity);
      gameEntityService.createGameEntity = jest.fn().mockResolvedValue(newEntity);

      const result = await gameEntityService.duplicateGameEntity('characters', '123');

      expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('characters', '123');
      expect(gameEntityService.createGameEntity).toHaveBeenCalledWith('characters', {
        name: 'Copy of Original',
        description: 'Test',
        id: undefined
      });
      expect(result).toEqual(newEntity);
    });

    it('should duplicate with custom name', async () => {
      const originalEntity = { id: '123', name: 'Original', description: 'Test' };
      const newEntity = { id: '456', name: 'Custom Name', description: 'Test' };

      gameEntityService.getGameEntity = jest.fn().mockResolvedValue(originalEntity);
      gameEntityService.createGameEntity = jest.fn().mockResolvedValue(newEntity);

      await gameEntityService.duplicateGameEntity('characters', '123', 'Custom Name');

      expect(gameEntityService.createGameEntity).toHaveBeenCalledWith('characters', {
        name: 'Custom Name',
        description: 'Test',
        id: undefined
      });
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.duplicateGameEntity(null, '123'))
        .rejects.toThrow('Entity type is required');
    });

    it('should throw error if id is not provided', async () => {
      await expect(gameEntityService.duplicateGameEntity('characters', null))
        .rejects.toThrow('Entity ID is required');
    });
  });

  describe('bulkUpdateGameEntities', () => {
    it('should bulk update entities successfully', async () => {
      const entities = [
        { id: '1', name: 'Updated 1' },
        { id: '2', name: 'Updated 2' }
      ];
      const mockResponse = { updated: 2, items: entities };

      api.put.mockResolvedValue({ data: mockResponse });

      const result = await gameEntityService.bulkUpdateGameEntities('characters', entities);

      expect(api.put).toHaveBeenCalledWith('/game-entities/characters/bulk', { entities });
      expect(result).toEqual(mockResponse);
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.bulkUpdateGameEntities(null, []))
        .rejects.toThrow('Entity type is required');
    });

    it('should throw error if entities is not an array', async () => {
      await expect(gameEntityService.bulkUpdateGameEntities('characters', 'not-array'))
        .rejects.toThrow('Entities array is required');
    });

    it('should throw error if entities array is empty', async () => {
      await expect(gameEntityService.bulkUpdateGameEntities('characters', []))
        .rejects.toThrow('At least one entity is required');
    });

    it('should handle API errors', async () => {
      const error = {
        response: {
          data: { message: 'Bulk update failed' }
        }
      };

      api.put.mockRejectedValue(error);

      await expect(gameEntityService.bulkUpdateGameEntities('characters', [{ id: '1' }]))
        .rejects.toEqual({ message: 'Bulk update failed' });
    });
  });

  describe('bulkDeleteGameEntities', () => {
    it('should bulk delete entities successfully', async () => {
      const ids = ['1', '2', '3'];
      const mockResponse = { deleted: 3 };

      api.delete.mockResolvedValue({ data: mockResponse });

      const result = await gameEntityService.bulkDeleteGameEntities('characters', ids);

      expect(api.delete).toHaveBeenCalledWith('/game-entities/characters/bulk', { data: { ids } });
      expect(result).toEqual(mockResponse);
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.bulkDeleteGameEntities(null, ['1']))
        .rejects.toThrow('Entity type is required');
    });

    it('should throw error if ids is not an array', async () => {
      await expect(gameEntityService.bulkDeleteGameEntities('characters', 'not-array'))
        .rejects.toThrow('IDs array is required');
    });

    it('should throw error if ids array is empty', async () => {
      await expect(gameEntityService.bulkDeleteGameEntities('characters', []))
        .rejects.toThrow('At least one ID is required');
    });

    it('should handle API errors', async () => {
      const error = {
        response: {
          data: { message: 'Bulk delete failed' }
        }
      };

      api.delete.mockRejectedValue(error);

      await expect(gameEntityService.bulkDeleteGameEntities('characters', ['1']))
        .rejects.toEqual({ message: 'Bulk delete failed' });
    });
  });

  describe('getGameEntityByName', () => {
    it('should find entity by name', async () => {
      const mockResponse = {
        items: [
          { id: '1', name: 'Entity One' },
          { id: '2', name: 'Entity Two' },
          { id: '3', name: 'Target Entity' }
        ]
      };

      gameEntityService.getGameEntities = jest.fn().mockResolvedValue(mockResponse);

      const result = await gameEntityService.getGameEntityByName('characters', 'Target Entity');

      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('characters', 1, 1000);
      expect(result).toEqual({ id: '3', name: 'Target Entity' });
    });

    it('should return null if entity not found', async () => {
      const mockResponse = {
        items: [
          { id: '1', name: 'Entity One' },
          { id: '2', name: 'Entity Two' }
        ]
      };

      gameEntityService.getGameEntities = jest.fn().mockResolvedValue(mockResponse);

      const result = await gameEntityService.getGameEntityByName('characters', 'Nonexistent');

      expect(result).toBeNull();
    });

    it('should handle data property in response', async () => {
      const mockResponse = {
        data: [
          { id: '1', name: 'Entity One' },
          { id: '2', name: 'Target Entity' }
        ]
      };

      gameEntityService.getGameEntities = jest.fn().mockResolvedValue(mockResponse);

      const result = await gameEntityService.getGameEntityByName('characters', 'Target Entity');

      expect(result).toEqual({ id: '2', name: 'Target Entity' });
    });

    it('should throw error if type is not provided', async () => {
      await expect(gameEntityService.getGameEntityByName(null, 'Test'))
        .rejects.toThrow('Entity type is required');
    });

    it('should throw error if name is not provided', async () => {
      await expect(gameEntityService.getGameEntityByName('characters', null))
        .rejects.toThrow('Entity name is required');
    });
  });

  describe('Entity Type Specific Methods', () => {
    describe('Characters', () => {
      it('should get characters', async () => {
        gameEntityService.getGameEntities = jest.fn().mockResolvedValue({ items: [] });
        
        await gameEntityService.getCharacters(2, 30, { level: 5 });
        
        expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('characters', 2, 30, { level: 5 });
      });

      it('should get single character', async () => {
        gameEntityService.getGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        
        await gameEntityService.getCharacter('123');
        
        expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('characters', '123');
      });

      it('should create character', async () => {
        const characterData = { name: 'Hero' };
        gameEntityService.createGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        
        await gameEntityService.createCharacter(characterData);
        
        expect(gameEntityService.createGameEntity).toHaveBeenCalledWith('characters', characterData);
      });

      it('should update character', async () => {
        const characterData = { name: 'Updated Hero' };
        gameEntityService.updateGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        
        await gameEntityService.updateCharacter('123', characterData);
        
        expect(gameEntityService.updateGameEntity).toHaveBeenCalledWith('characters', '123', characterData);
      });

      it('should delete character', async () => {
        gameEntityService.deleteGameEntity = jest.fn().mockResolvedValue({ success: true });
        
        await gameEntityService.deleteCharacter('123');
        
        expect(gameEntityService.deleteGameEntity).toHaveBeenCalledWith('characters', '123');
      });
    });

    describe('Beasts', () => {
      it('should get beasts', async () => {
        gameEntityService.getGameEntities = jest.fn().mockResolvedValue({ items: [] });
        
        await gameEntityService.getBeasts();
        
        expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('beasts', 1, 20, {});
      });

      it('should get single beast', async () => {
        gameEntityService.getGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        
        await gameEntityService.getBeast('123');
        
        expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('beasts', '123');
      });

      it('should create beast', async () => {
        const beastData = { name: 'Dragon' };
        gameEntityService.createGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        
        await gameEntityService.createBeast(beastData);
        
        expect(gameEntityService.createGameEntity).toHaveBeenCalledWith('beasts', beastData);
      });

      it('should update beast', async () => {
        const beastData = { name: 'Ancient Dragon' };
        gameEntityService.updateGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        
        await gameEntityService.updateBeast('123', beastData);
        
        expect(gameEntityService.updateGameEntity).toHaveBeenCalledWith('beasts', '123', beastData);
      });

      it('should delete beast', async () => {
        gameEntityService.deleteGameEntity = jest.fn().mockResolvedValue({ success: true });
        
        await gameEntityService.deleteBeast('123');
        
        expect(gameEntityService.deleteGameEntity).toHaveBeenCalledWith('beasts', '123');
      });
    });

    describe('Equipment', () => {
      it('should handle all equipment operations', async () => {
        gameEntityService.getGameEntities = jest.fn().mockResolvedValue({ items: [] });
        gameEntityService.getGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.createGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.updateGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.deleteGameEntity = jest.fn().mockResolvedValue({ success: true });

        await gameEntityService.getEquipment();
        expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('equipment', 1, 20, {});

        await gameEntityService.getEquipmentItem('123');
        expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('equipment', '123');

        const equipmentData = { name: 'Sword' };
        await gameEntityService.createEquipmentItem(equipmentData);
        expect(gameEntityService.createGameEntity).toHaveBeenCalledWith('equipment', equipmentData);

        await gameEntityService.updateEquipmentItem('123', equipmentData);
        expect(gameEntityService.updateGameEntity).toHaveBeenCalledWith('equipment', '123', equipmentData);

        await gameEntityService.deleteEquipmentItem('123');
        expect(gameEntityService.deleteGameEntity).toHaveBeenCalledWith('equipment', '123');
      });
    });

    describe('Powers', () => {
      it('should handle all power operations', async () => {
        gameEntityService.getGameEntities = jest.fn().mockResolvedValue({ items: [] });
        gameEntityService.getGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.createGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.updateGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.deleteGameEntity = jest.fn().mockResolvedValue({ success: true });

        await gameEntityService.getPowers();
        expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('powers', 1, 20, {});

        await gameEntityService.getPower('123');
        expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('powers', '123');

        const powerData = { name: 'Fireball' };
        await gameEntityService.createPower(powerData);
        expect(gameEntityService.createGameEntity).toHaveBeenCalledWith('powers', powerData);

        await gameEntityService.updatePower('123', powerData);
        expect(gameEntityService.updateGameEntity).toHaveBeenCalledWith('powers', '123', powerData);

        await gameEntityService.deletePower('123');
        expect(gameEntityService.deleteGameEntity).toHaveBeenCalledWith('powers', '123');
      });
    });

    describe('Edges', () => {
      it('should handle all edge operations', async () => {
        gameEntityService.getGameEntities = jest.fn().mockResolvedValue({ items: [] });
        gameEntityService.getGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.createGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.updateGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.deleteGameEntity = jest.fn().mockResolvedValue({ success: true });

        await gameEntityService.getEdges();
        expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('edges', 1, 20, {});

        await gameEntityService.getEdge('123');
        expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('edges', '123');

        const edgeData = { name: 'Quick' };
        await gameEntityService.createEdge(edgeData);
        expect(gameEntityService.createGameEntity).toHaveBeenCalledWith('edges', edgeData);

        await gameEntityService.updateEdge('123', edgeData);
        expect(gameEntityService.updateGameEntity).toHaveBeenCalledWith('edges', '123', edgeData);

        await gameEntityService.deleteEdge('123');
        expect(gameEntityService.deleteGameEntity).toHaveBeenCalledWith('edges', '123');
      });
    });

    describe('Hindrances', () => {
      it('should handle all hindrance operations', async () => {
        gameEntityService.getGameEntities = jest.fn().mockResolvedValue({ items: [] });
        gameEntityService.getGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.createGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.updateGameEntity = jest.fn().mockResolvedValue({ id: '123' });
        gameEntityService.deleteGameEntity = jest.fn().mockResolvedValue({ success: true });

        await gameEntityService.getHindrances();
        expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('hindrances', 1, 20, {});

        await gameEntityService.getHindrance('123');
        expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('hindrances', '123');

        const hindranceData = { name: 'Bad Luck' };
        await gameEntityService.createHindrance(hindranceData);
        expect(gameEntityService.createGameEntity).toHaveBeenCalledWith('hindrances', hindranceData);

        await gameEntityService.updateHindrance('123', hindranceData);
        expect(gameEntityService.updateGameEntity).toHaveBeenCalledWith('hindrances', '123', hindranceData);

        await gameEntityService.deleteHindrance('123');
        expect(gameEntityService.deleteGameEntity).toHaveBeenCalledWith('hindrances', '123');
      });
    });
  });
});