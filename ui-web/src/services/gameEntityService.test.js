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

    it('handles errors properly', async () => {
      const error = new Error('Network error');
      error.response = { data: { message: 'Server error' } };
      api.get.mockRejectedValue(error);
      
      await expect(gameEntityService.getGameEntities('character')).rejects.toEqual({ message: 'Server error' });
    });
  });

  describe('getGameEntity', () => {
    it('fetches single game entity successfully', async () => {
      const mockEntity = { id: '123', name: 'Character 1', type: 'character' };
      
      api.get.mockResolvedValue({ data: mockEntity });
      
      const result = await gameEntityService.getGameEntity('character', '123');
      
      expect(api.get).toHaveBeenCalledWith('/game-entities/character/123');
      expect(result).toEqual(mockEntity);
    });

    it('handles not found errors', async () => {
      const error = new Error('Not found');
      error.response = { data: { message: 'Entity not found' } };
      api.get.mockRejectedValue(error);
      
      await expect(gameEntityService.getGameEntity('character', '999')).rejects.toEqual({ message: 'Entity not found' });
    });
  });

  describe('createGameEntity', () => {
    it('creates new game entity successfully', async () => {
      const newEntity = { name: 'New Character', type: 'character' };
      const createdEntity = { id: '123', ...newEntity };
      
      api.post.mockResolvedValue({ data: createdEntity });
      
      const result = await gameEntityService.createGameEntity('character', newEntity);
      
      expect(api.post).toHaveBeenCalledWith('/game-entities/character', newEntity);
      expect(result).toEqual(createdEntity);
    });
  });

  describe('updateGameEntity', () => {
    it('updates game entity successfully', async () => {
      const updates = { name: 'Updated Name' };
      const updatedEntity = { id: '123', name: 'Updated Name', type: 'character' };
      
      api.put.mockResolvedValue({ data: updatedEntity });
      
      const result = await gameEntityService.updateGameEntity('character', '123', updates);
      
      expect(api.put).toHaveBeenCalledWith('/game-entities/character/123', updates);
      expect(result).toEqual(updatedEntity);
    });
  });

  describe('deleteGameEntity', () => {
    it('deletes game entity successfully', async () => {
      api.delete.mockResolvedValue({ data: { success: true } });
      
      const result = await gameEntityService.deleteGameEntity('character', '123');
      
      expect(api.delete).toHaveBeenCalledWith('/game-entities/character/123');
      expect(result).toEqual({ success: true });
    });
  });

  describe('searchGameEntities', () => {
    it('searches game entities successfully', async () => {
      const mockResults = [{ id: '1', name: 'Matching Entity', type: 'character' }];
      
      api.get.mockResolvedValue({ data: { items: mockResults } });
      
      const result = await gameEntityService.searchGameEntities('character', 'test query');
      
      expect(api.get).toHaveBeenCalledWith('/game-entities/character/search?q=test+query&page=1&limit=20');
      expect(result.items).toEqual(mockResults);
    });
  });
});