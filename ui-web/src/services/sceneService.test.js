import sceneService from './sceneService';
import api from './api';

// Mock the api module
jest.mock('./api');

describe('sceneService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('getScenes', () => {
    it('should fetch scenes with default parameters', async () => {
      const mockScenes = [
        { id: '1', name: 'Scene 1' },
        { id: '2', name: 'Scene 2' }
      ];
      api.get.mockResolvedValue({ data: mockScenes });

      const result = await sceneService.getScenes();

      expect(api.get).toHaveBeenCalledWith('/scenes?page=1&limit=20');
      expect(result).toEqual(mockScenes);
    });

    it('should fetch scenes with custom parameters', async () => {
      const mockScenes = [{ id: '3', name: 'Scene 3' }];
      api.get.mockResolvedValue({ data: mockScenes });

      const result = await sceneService.getScenes(2, 10, { type: 'combat' });

      expect(api.get).toHaveBeenCalledWith('/scenes?page=2&limit=10&type=combat');
      expect(result).toEqual(mockScenes);
    });

    it('should handle API errors', async () => {
      const error = new Error('Network error');
      error.response = { data: { message: 'API Error' } };
      api.get.mockRejectedValue(error);

      await expect(sceneService.getScenes()).rejects.toEqual({ message: 'API Error' });
    });

    it('should handle errors without response data', async () => {
      const error = new Error('Network error');
      api.get.mockRejectedValue(error);

      await expect(sceneService.getScenes()).rejects.toEqual(error);
    });
  });

  describe('getScene', () => {
    it('should fetch a single scene by ID', async () => {
      const mockScene = { id: '1', name: 'Scene 1', description: 'Test scene' };
      api.get.mockResolvedValue({ data: mockScene });

      const result = await sceneService.getScene('1');

      expect(api.get).toHaveBeenCalledWith('/scenes/1');
      expect(result).toEqual(mockScene);
    });

    it('should throw error when ID is not provided', async () => {
      await expect(sceneService.getScene()).rejects.toThrow('Scene ID is required');
      await expect(sceneService.getScene(null)).rejects.toThrow('Scene ID is required');
      await expect(sceneService.getScene('')).rejects.toThrow('Scene ID is required');
    });

    it('should handle API errors', async () => {
      const error = new Error('Not found');
      error.response = { data: { message: 'Scene not found' } };
      api.get.mockRejectedValue(error);

      await expect(sceneService.getScene('999')).rejects.toEqual({ message: 'Scene not found' });
    });
  });

  describe('createScene', () => {
    it('should create a new scene', async () => {
      const newScene = { name: 'New Scene', description: 'A new scene' };
      const createdScene = { id: '4', ...newScene };
      api.post.mockResolvedValue({ data: createdScene });

      const result = await sceneService.createScene(newScene);

      expect(api.post).toHaveBeenCalledWith('/scenes', newScene);
      expect(result).toEqual(createdScene);
    });

    it('should throw error when scene is not provided', async () => {
      await expect(sceneService.createScene()).rejects.toThrow('Scene is required');
      await expect(sceneService.createScene(null)).rejects.toThrow('Scene is required');
    });

    it('should throw error when scene name is not provided', async () => {
      await expect(sceneService.createScene({})).rejects.toThrow('Scene name is required');
      await expect(sceneService.createScene({ description: 'Test' })).rejects.toThrow('Scene name is required');
    });

    it('should handle API errors', async () => {
      const error = new Error('Validation error');
      error.response = { data: { message: 'Invalid scene data' } };
      api.post.mockRejectedValue(error);

      const newScene = { name: 'Test Scene' };
      await expect(sceneService.createScene(newScene)).rejects.toEqual({ message: 'Invalid scene data' });
    });
  });

  describe('updateScene', () => {
    it('should update an existing scene', async () => {
      const updatedScene = { id: '1', name: 'Updated Scene', description: 'Updated description' };
      api.put.mockResolvedValue({ data: updatedScene });

      const result = await sceneService.updateScene('1', updatedScene);

      expect(api.put).toHaveBeenCalledWith('/scenes/1', updatedScene);
      expect(result).toEqual(updatedScene);
    });

    it('should throw error when ID is not provided', async () => {
      const scene = { name: 'Test' };
      await expect(sceneService.updateScene(null, scene)).rejects.toThrow('Scene ID is required');
      await expect(sceneService.updateScene('', scene)).rejects.toThrow('Scene ID is required');
    });

    it('should throw error when scene is not provided', async () => {
      await expect(sceneService.updateScene('1')).rejects.toThrow('Scene is required');
      await expect(sceneService.updateScene('1', null)).rejects.toThrow('Scene is required');
    });

    it('should throw error when scene name is not provided', async () => {
      await expect(sceneService.updateScene('1', {})).rejects.toThrow('Scene name is required');
    });

    it('should handle API errors', async () => {
      const error = new Error('Update failed');
      error.response = { data: { message: 'Concurrent modification' } };
      api.put.mockRejectedValue(error);

      const scene = { name: 'Test Scene' };
      await expect(sceneService.updateScene('1', scene)).rejects.toEqual({ message: 'Concurrent modification' });
    });
  });

  describe('deleteScene', () => {
    it('should delete a scene', async () => {
      api.delete.mockResolvedValue({ data: { success: true } });

      const result = await sceneService.deleteScene('1');

      expect(api.delete).toHaveBeenCalledWith('/scenes/1');
      expect(result).toEqual({ success: true });
    });

    it('should throw error when ID is not provided', async () => {
      await expect(sceneService.deleteScene()).rejects.toThrow('Scene ID is required');
      await expect(sceneService.deleteScene(null)).rejects.toThrow('Scene ID is required');
      await expect(sceneService.deleteScene('')).rejects.toThrow('Scene ID is required');
    });

    it('should handle API errors', async () => {
      const error = new Error('Delete failed');
      error.response = { data: { message: 'Scene in use' } };
      api.delete.mockRejectedValue(error);

      await expect(sceneService.deleteScene('1')).rejects.toEqual({ message: 'Scene in use' });
    });
  });

  describe('searchScenes', () => {
    it('should search scenes by query', async () => {
      const mockScenes = [
        { id: '1', name: 'Combat Scene' },
        { id: '2', name: 'Combat Encounter' }
      ];
      api.get.mockResolvedValue({ data: mockScenes });

      const result = await sceneService.searchScenes('combat');

      expect(api.get).toHaveBeenCalledWith('/scenes/search?query=combat');
      expect(result).toEqual(mockScenes);
    });

    it('should throw error when query is not provided', async () => {
      await expect(sceneService.searchScenes()).rejects.toThrow('Search query is required');
      await expect(sceneService.searchScenes('')).rejects.toThrow('Search query is required');
    });

    it('should handle API errors', async () => {
      const error = new Error('Search failed');
      error.response = { data: { message: 'Search service unavailable' } };
      api.get.mockRejectedValue(error);

      await expect(sceneService.searchScenes('test')).rejects.toEqual({ message: 'Search service unavailable' });
    });
  });
});