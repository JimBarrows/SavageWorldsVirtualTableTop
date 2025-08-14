import plotPointService from './plotPointService';
import api from './api';

// Mock the API service
jest.mock('./api', () => ({
  get: jest.fn(),
  post: jest.fn(),
  put: jest.fn(),
  delete: jest.fn()
}));

describe('PlotPointService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('getPlotPoints', () => {
    it('fetches all plot points successfully', async () => {
      const mockPlotPoints = [
        { id: '1', name: 'Plot Point 1' },
        { id: '2', name: 'Plot Point 2' }
      ];
      
      api.get.mockResolvedValue({
        data: {
          items: mockPlotPoints,
          pagination: { total: 2, page: 1, limit: 20 }
        }
      });
      
      const result = await plotPointService.getPlotPoints();
      
      expect(api.get).toHaveBeenCalledWith('/plot-points?page=1&limit=20');
      expect(result.items).toEqual(mockPlotPoints);
    });

    it('handles pagination parameters', async () => {
      api.get.mockResolvedValue({ data: { items: [], pagination: {} } });
      
      await plotPointService.getPlotPoints(2, 50);
      
      expect(api.get).toHaveBeenCalledWith('/plot-points?page=2&limit=50');
    });

    it('handles filters', async () => {
      api.get.mockResolvedValue({ data: { items: [], pagination: {} } });
      
      await plotPointService.getPlotPoints(1, 20, { genre: 'Fantasy' });
      
      expect(api.get).toHaveBeenCalledWith('/plot-points?page=1&limit=20&genre=Fantasy');
    });

    it('handles errors properly', async () => {
      const error = new Error('Network error');
      error.response = { data: { message: 'Server error' } };
      api.get.mockRejectedValue(error);
      
      await expect(plotPointService.getPlotPoints()).rejects.toEqual({ message: 'Server error' });
    });
  });

  describe('getPlotPoint', () => {
    it('fetches single plot point successfully', async () => {
      const mockPlotPoint = { id: '123', name: 'Plot Point 1' };
      
      api.get.mockResolvedValue({ data: mockPlotPoint });
      
      const result = await plotPointService.getPlotPoint('123');
      
      expect(api.get).toHaveBeenCalledWith('/plot-points/123');
      expect(result).toEqual(mockPlotPoint);
    });

    it('handles not found errors', async () => {
      const error = new Error('Not found');
      error.response = { data: { message: 'Plot point not found' } };
      api.get.mockRejectedValue(error);
      
      await expect(plotPointService.getPlotPoint('999')).rejects.toEqual({ message: 'Plot point not found' });
    });
  });

  describe('getPlotPointByName', () => {
    it('fetches plot point by name successfully', async () => {
      const mockPlotPoint = { id: '123', name: 'Test Plot' };
      
      api.get.mockResolvedValue({ data: mockPlotPoint });
      
      const result = await plotPointService.getPlotPointByName('Test Plot');
      
      expect(api.get).toHaveBeenCalledWith('/plot-points/by-name/Test%20Plot');
      expect(result).toEqual(mockPlotPoint);
    });
  });

  describe('createPlotPoint', () => {
    it('creates new plot point successfully', async () => {
      const newPlotPoint = { name: 'New Plot Point', description: 'Test description' };
      const createdPlotPoint = { id: '123', ...newPlotPoint };
      
      api.post.mockResolvedValue({ data: createdPlotPoint });
      
      const result = await plotPointService.createPlotPoint(newPlotPoint);
      
      expect(api.post).toHaveBeenCalledWith('/plot-points', newPlotPoint);
      expect(result).toEqual(createdPlotPoint);
    });
  });

  describe('updatePlotPoint', () => {
    it('updates plot point successfully', async () => {
      const updates = { name: 'Updated Name' };
      const updatedPlotPoint = { id: '123', name: 'Updated Name' };
      
      api.put.mockResolvedValue({ data: updatedPlotPoint });
      
      const result = await plotPointService.updatePlotPoint('123', updates);
      
      expect(api.put).toHaveBeenCalledWith('/plot-points/123', updates);
      expect(result).toEqual(updatedPlotPoint);
    });
  });

  describe('deletePlotPoint', () => {
    it('deletes plot point successfully', async () => {
      api.delete.mockResolvedValue({ data: { success: true } });
      
      const result = await plotPointService.deletePlotPoint('123');
      
      expect(api.delete).toHaveBeenCalledWith('/plot-points/123');
      expect(result).toEqual({ success: true });
    });
  });

  describe('searchPlotPoints', () => {
    it('searches plot points successfully', async () => {
      const mockResults = [{ id: '1', name: 'Matching Plot' }];
      
      api.get.mockResolvedValue({ data: { items: mockResults } });
      
      const result = await plotPointService.searchPlotPoints('test query');
      
      expect(api.get).toHaveBeenCalledWith('/plot-points/search?q=test+query&page=1&limit=20');
      expect(result.items).toEqual(mockResults);
    });
  });

  describe('exportPlotPoints', () => {
    it('exports plot points successfully', async () => {
      const mockData = 'exported,data\n1,Test Plot';
      
      api.get.mockResolvedValue({ data: mockData });
      
      const result = await plotPointService.exportPlotPoints('csv', ['1', '2']);
      
      expect(api.get).toHaveBeenCalledWith('/plot-points/export?format=csv&ids=1%2C2', {
        responseType: 'blob'
      });
      expect(result).toEqual(mockData);
    });
  });
});