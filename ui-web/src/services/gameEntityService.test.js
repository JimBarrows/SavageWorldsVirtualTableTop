import gameEntityService from './gameEntityService';
import api from './api';

// Mock the API service
jest.mock('./api', () => ({
  get: jest.fn(),
  post: jest.fn(),
  put: jest.fn(),
  delete: jest.fn()
}));

describe('GameEntityService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('getGameEntities', () => {
    it('fetches all game entities successfully', async () => {
      const mockEntities = [
        { id: '1', name: 'Character 1', type: 'character' },
        { id: '2', name: 'Beast 1', type: 'beast' }
      ];
      
      api.get.mockResolvedValue({
        data: {
          items: mockEntities,
          pagination: { total: 2, page: 1, limit: 20 }
        }
      });
      
      const result = await gameEntityService.getGameEntities('character');
      
      expect(api.get).toHaveBeenCalledWith('/game-entities/character?page=1&limit=20');
      expect(result.items).toEqual(mockEntities);
    });

    it('handles pagination parameters', async () => {
      api.get.mockResolvedValue({ data: { items: [], pagination: {} } });
      
      await gameEntityService.getGameEntities('beast', 2, 50);
      
      expect(api.get).toHaveBeenCalledWith('/game-entities/beast?page=2&limit=50');
    });

    it('handles filters', async () => {
      api.get.mockResolvedValue({ data: { items: [], pagination: {} } });
      
      await gameEntityService.getGameEntities('character', 1, 20, { race: 'Elf' });
      
      expect(api.get).toHaveBeenCalledWith('/game-entities/character?page=1&limit=20&race=Elf');
    });

    it('throws error when type is not provided', async () => {
      await expect(gameEntityService.getGameEntities()).rejects.toThrow('Entity type is required');
      await expect(gameEntityService.getGameEntities('')).rejects.toThrow('Entity type is required');
    });

    it('handles API errors', async () => {
      const error = new Error('Network error');
      error.response = { data: { message: 'API Error' } };
      api.get.mockRejectedValue(error);

      await expect(gameEntityService.getGameEntities('character')).rejects.toEqual({ message: 'API Error' });
    });

    it('handles errors without response data', async () => {
      const error = new Error('Network error');
      api.get.mockRejectedValue(error);

      await expect(gameEntityService.getGameEntities('character')).rejects.toEqual(error);
    });
  });

  describe('getGameEntity', () => {
    it('fetches a single game entity by ID', async () => {
      const mockEntity = { id: '1', name: 'Test Character', type: 'character' };
      api.get.mockResolvedValue({ data: mockEntity });

      const result = await gameEntityService.getGameEntity('character', '1');

      expect(api.get).toHaveBeenCalledWith('/game-entities/character/1');
      expect(result).toEqual(mockEntity);
    });

    it('throws error when type is not provided', async () => {
      await expect(gameEntityService.getGameEntity()).rejects.toThrow('Entity type is required');
      await expect(gameEntityService.getGameEntity('')).rejects.toThrow('Entity type is required');
    });

    it('throws error when ID is not provided', async () => {
      await expect(gameEntityService.getGameEntity('character')).rejects.toThrow('Entity ID is required');
      await expect(gameEntityService.getGameEntity('character', '')).rejects.toThrow('Entity ID is required');
    });

    it('handles API errors', async () => {
      const error = new Error('Not found');
      error.response = { data: { message: 'Entity not found' } };
      api.get.mockRejectedValue(error);

      await expect(gameEntityService.getGameEntity('character', '999')).rejects.toEqual({ message: 'Entity not found' });
    });
  });

  describe('createGameEntity', () => {
    it('creates a new game entity', async () => {
      const newEntity = { name: 'New Character', attributes: {} };
      const createdEntity = { id: '3', ...newEntity, type: 'character' };
      api.post.mockResolvedValue({ data: createdEntity });

      const result = await gameEntityService.createGameEntity('character', newEntity);

      expect(api.post).toHaveBeenCalledWith('/game-entities/character', newEntity);
      expect(result).toEqual(createdEntity);
    });

    it('throws error when type is not provided', async () => {
      const entity = { name: 'Test' };
      await expect(gameEntityService.createGameEntity(null, entity)).rejects.toThrow('Entity type is required');
      await expect(gameEntityService.createGameEntity('', entity)).rejects.toThrow('Entity type is required');
    });

    it('throws error when entity is not provided', async () => {
      await expect(gameEntityService.createGameEntity('character')).rejects.toThrow('Entity data is required');
      await expect(gameEntityService.createGameEntity('character', null)).rejects.toThrow('Entity data is required');
    });

    it('throws error when entity name is not provided', async () => {
      await expect(gameEntityService.createGameEntity('character', {})).rejects.toThrow('Entity name is required');
      await expect(gameEntityService.createGameEntity('character', { attributes: {} })).rejects.toThrow('Entity name is required');
    });

    it('handles API errors', async () => {
      const error = new Error('Validation error');
      error.response = { data: { message: 'Invalid entity data' } };
      api.post.mockRejectedValue(error);

      const entity = { name: 'Test' };
      await expect(gameEntityService.createGameEntity('character', entity)).rejects.toEqual({ message: 'Invalid entity data' });
    });
  });

  describe('updateGameEntity', () => {
    it('updates an existing game entity', async () => {
      const updatedEntity = { id: '1', name: 'Updated Character', type: 'character' };
      api.put.mockResolvedValue({ data: updatedEntity });

      const result = await gameEntityService.updateGameEntity('character', '1', updatedEntity);

      expect(api.put).toHaveBeenCalledWith('/game-entities/character/1', updatedEntity);
      expect(result).toEqual(updatedEntity);
    });

    it('throws error when type is not provided', async () => {
      const entity = { name: 'Test' };
      await expect(gameEntityService.updateGameEntity(null, '1', entity)).rejects.toThrow('Entity type is required');
      await expect(gameEntityService.updateGameEntity('', '1', entity)).rejects.toThrow('Entity type is required');
    });

    it('throws error when ID is not provided', async () => {
      const entity = { name: 'Test' };
      await expect(gameEntityService.updateGameEntity('character', null, entity)).rejects.toThrow('Entity ID is required');
      await expect(gameEntityService.updateGameEntity('character', '', entity)).rejects.toThrow('Entity ID is required');
    });

    it('throws error when entity is not provided', async () => {
      await expect(gameEntityService.updateGameEntity('character', '1')).rejects.toThrow('Entity data is required');
      await expect(gameEntityService.updateGameEntity('character', '1', null)).rejects.toThrow('Entity data is required');
    });

    it('throws error when entity name is not provided', async () => {
      await expect(gameEntityService.updateGameEntity('character', '1', {})).rejects.toThrow('Entity name is required');
    });

    it('handles API errors', async () => {
      const error = new Error('Update failed');
      error.response = { data: { message: 'Concurrent modification' } };
      api.put.mockRejectedValue(error);

      const entity = { name: 'Test' };
      await expect(gameEntityService.updateGameEntity('character', '1', entity)).rejects.toEqual({ message: 'Concurrent modification' });
    });
  });

  describe('deleteGameEntity', () => {
    it('deletes a game entity', async () => {
      api.delete.mockResolvedValue({ data: { success: true } });

      const result = await gameEntityService.deleteGameEntity('character', '1');

      expect(api.delete).toHaveBeenCalledWith('/game-entities/character/1');
      expect(result).toEqual({ success: true });
    });

    it('throws error when type is not provided', async () => {
      await expect(gameEntityService.deleteGameEntity()).rejects.toThrow('Entity type is required');
      await expect(gameEntityService.deleteGameEntity('')).rejects.toThrow('Entity type is required');
    });

    it('throws error when ID is not provided', async () => {
      await expect(gameEntityService.deleteGameEntity('character')).rejects.toThrow('Entity ID is required');
      await expect(gameEntityService.deleteGameEntity('character', '')).rejects.toThrow('Entity ID is required');
    });

    it('handles API errors', async () => {
      const error = new Error('Delete failed');
      error.response = { data: { message: 'Entity in use' } };
      api.delete.mockRejectedValue(error);

      await expect(gameEntityService.deleteGameEntity('character', '1')).rejects.toEqual({ message: 'Entity in use' });
    });
  });

  describe('searchGameEntities', () => {
    it('searches game entities by query', async () => {
      const mockEntities = [
        { id: '1', name: 'Elf Warrior' },
        { id: '2', name: 'Elf Mage' }
      ];
      api.get.mockResolvedValue({ data: { items: mockEntities } });

      const result = await gameEntityService.searchGameEntities('character', 'elf');

      expect(api.get).toHaveBeenCalledWith('/game-entities/character/search?query=elf&page=1&limit=20');
      expect(result.items).toEqual(mockEntities);
    });

    it('throws error when type is not provided', async () => {
      await expect(gameEntityService.searchGameEntities(null, 'query')).rejects.toThrow('Entity type is required');
      await expect(gameEntityService.searchGameEntities('', 'query')).rejects.toThrow('Entity type is required');
    });

    it('throws error when query is not provided', async () => {
      await expect(gameEntityService.searchGameEntities('character')).rejects.toThrow('Search query is required');
      await expect(gameEntityService.searchGameEntities('character', '')).rejects.toThrow('Search query is required');
    });

    it('handles API errors', async () => {
      const error = new Error('Search failed');
      error.response = { data: { message: 'Search service unavailable' } };
      api.get.mockRejectedValue(error);

      await expect(gameEntityService.searchGameEntities('character', 'test')).rejects.toEqual({ message: 'Search service unavailable' });
    });
  });

  describe('duplicateGameEntity', () => {
    it('duplicates an existing game entity', async () => {
      const originalEntity = { id: '1', name: 'Original Character', type: 'character' };
      const duplicatedEntity = { id: '2', name: 'Copy of Original Character', type: 'character' };
      
      api.get.mockResolvedValue({ data: originalEntity });
      api.post.mockResolvedValue({ data: duplicatedEntity });

      const result = await gameEntityService.duplicateGameEntity('character', '1');

      expect(api.get).toHaveBeenCalledWith('/game-entities/character/1');
      expect(api.post).toHaveBeenCalledWith('/game-entities/character', {
        ...originalEntity,
        id: undefined,
        name: 'Copy of Original Character'
      });
      expect(result).toEqual(duplicatedEntity);
    });

    it('duplicates with custom name', async () => {
      const originalEntity = { id: '1', name: 'Original', type: 'character' };
      const duplicatedEntity = { id: '2', name: 'Custom Name', type: 'character' };
      
      api.get.mockResolvedValue({ data: originalEntity });
      api.post.mockResolvedValue({ data: duplicatedEntity });

      const result = await gameEntityService.duplicateGameEntity('character', '1', 'Custom Name');

      expect(api.post).toHaveBeenCalledWith('/game-entities/character', {
        ...originalEntity,
        id: undefined,
        name: 'Custom Name'
      });
      expect(result).toEqual(duplicatedEntity);
    });

    it('throws error when type is not provided', async () => {
      await expect(gameEntityService.duplicateGameEntity()).rejects.toThrow('Entity type is required');
    });

    it('throws error when ID is not provided', async () => {
      await expect(gameEntityService.duplicateGameEntity('character')).rejects.toThrow('Entity ID is required');
    });

    it('handles errors when fetching original entity', async () => {
      const error = new Error('Not found');
      error.response = { data: { message: 'Entity not found' } };
      api.get.mockRejectedValue(error);

      await expect(gameEntityService.duplicateGameEntity('character', '999')).rejects.toEqual({ message: 'Entity not found' });
    });
  });

  describe('bulkUpdateGameEntities', () => {
    it('updates multiple game entities', async () => {
      const updates = [
        { id: '1', name: 'Updated 1' },
        { id: '2', name: 'Updated 2' }
      ];
      const response = { updated: 2, results: updates };
      api.put.mockResolvedValue({ data: response });

      const result = await gameEntityService.bulkUpdateGameEntities('character', updates);

      expect(api.put).toHaveBeenCalledWith('/game-entities/character/bulk', { entities: updates });
      expect(result).toEqual(response);
    });

    it('throws error when type is not provided', async () => {
      await expect(gameEntityService.bulkUpdateGameEntities()).rejects.toThrow('Entity type is required');
    });

    it('throws error when entities array is not provided', async () => {
      await expect(gameEntityService.bulkUpdateGameEntities('character')).rejects.toThrow('Entities array is required');
      await expect(gameEntityService.bulkUpdateGameEntities('character', null)).rejects.toThrow('Entities array is required');
    });

    it('throws error when entities array is empty', async () => {
      await expect(gameEntityService.bulkUpdateGameEntities('character', [])).rejects.toThrow('At least one entity is required');
    });

    it('handles API errors', async () => {
      const error = new Error('Bulk update failed');
      error.response = { data: { message: 'Validation errors in batch' } };
      api.put.mockRejectedValue(error);

      const entities = [{ id: '1', name: 'Test' }];
      await expect(gameEntityService.bulkUpdateGameEntities('character', entities)).rejects.toEqual({ message: 'Validation errors in batch' });
    });
  });

  describe('bulkDeleteGameEntities', () => {
    it('deletes multiple game entities', async () => {
      const ids = ['1', '2', '3'];
      const response = { deleted: 3 };
      api.delete.mockResolvedValue({ data: response });

      const result = await gameEntityService.bulkDeleteGameEntities('character', ids);

      expect(api.delete).toHaveBeenCalledWith('/game-entities/character/bulk', { data: { ids } });
      expect(result).toEqual(response);
    });

    it('throws error when type is not provided', async () => {
      await expect(gameEntityService.bulkDeleteGameEntities()).rejects.toThrow('Entity type is required');
    });

    it('throws error when IDs array is not provided', async () => {
      await expect(gameEntityService.bulkDeleteGameEntities('character')).rejects.toThrow('IDs array is required');
      await expect(gameEntityService.bulkDeleteGameEntities('character', null)).rejects.toThrow('IDs array is required');
    });

    it('throws error when IDs array is empty', async () => {
      await expect(gameEntityService.bulkDeleteGameEntities('character', [])).rejects.toThrow('At least one ID is required');
    });

    it('handles API errors', async () => {
      const error = new Error('Bulk delete failed');
      error.response = { data: { message: 'Some entities in use' } };
      api.delete.mockRejectedValue(error);

      const ids = ['1', '2'];
      await expect(gameEntityService.bulkDeleteGameEntities('character', ids)).rejects.toEqual({ message: 'Some entities in use' });
    });
  });

  describe('getGameEntityByName', () => {
    it('fetches a game entity by name', async () => {
      const mockEntities = [
        { id: '1', name: 'Test Character', type: 'character' }
      ];
      api.get.mockResolvedValue({ data: { items: mockEntities } });

      const result = await gameEntityService.getGameEntityByName('character', 'Test Character');

      expect(api.get).toHaveBeenCalledWith('/game-entities/character?page=1&limit=1000');
      expect(result).toEqual(mockEntities[0]);
    });

    it('returns null when entity not found', async () => {
      api.get.mockResolvedValue({ data: { items: [] } });

      const result = await gameEntityService.getGameEntityByName('character', 'Non-existent');

      expect(result).toBeNull();
    });

    it('throws error when type is not provided', async () => {
      await expect(gameEntityService.getGameEntityByName()).rejects.toThrow('Entity type is required');
    });

    it('throws error when name is not provided', async () => {
      await expect(gameEntityService.getGameEntityByName('character')).rejects.toThrow('Entity name is required');
      await expect(gameEntityService.getGameEntityByName('character', '')).rejects.toThrow('Entity name is required');
    });
  });
});