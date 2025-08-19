import plotPointService from './plotPointService';
import api from './api';

// Mock the API service
jest.mock('./api');

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
    it('exports plot points successfully as JSON', async () => {
      const mockData = { plotPoints: [] };
      
      api.get.mockResolvedValue({ data: mockData });
      
      const result = await plotPointService.exportPlotPoints();
      
      expect(api.get).toHaveBeenCalledWith('/plot-points/export?format=json', {
        responseType: 'json'
      });
      expect(result).toEqual(mockData);
    });

    it('exports plot points as CSV', async () => {
      const mockData = 'exported,data\n1,Test Plot';
      
      api.get.mockResolvedValue({ data: mockData });
      
      const result = await plotPointService.exportPlotPoints('csv', ['1', '2']);
      
      expect(api.get).toHaveBeenCalledWith('/plot-points/export?format=csv&ids=1%2C2', {
        responseType: 'blob'
      });
      expect(result).toEqual(mockData);
    });

    it('handles export errors', async () => {
      const error = new Error('Export failed');
      error.response = { data: { message: 'Export error' } };
      api.get.mockRejectedValue(error);
      
      await expect(plotPointService.exportPlotPoints()).rejects.toEqual({ message: 'Export error' });
    });
  });

  describe('importPlotPoints', () => {
    it('imports plot points from file successfully', async () => {
      const file = new File(['{}'], 'test.json', { type: 'application/json' });
      const mockResponse = { imported: 1, items: [{ id: '1', name: 'Imported' }] };
      
      api.post.mockResolvedValue({ data: mockResponse });
      
      const result = await plotPointService.importPlotPoints(file);
      
      expect(api.post).toHaveBeenCalledWith(
        '/plot-points/import',
        expect.any(FormData),
        {
          headers: {
            'Content-Type': 'multipart/form-data'
          }
        }
      );
      expect(result).toEqual(mockResponse);
    });

    it('imports with custom format', async () => {
      const file = new File(['csv'], 'test.csv', { type: 'text/csv' });
      const mockResponse = { imported: 3 };
      
      api.post.mockResolvedValue({ data: mockResponse });
      
      await plotPointService.importPlotPoints(file, 'csv');
      
      const callArgs = api.post.mock.calls[0];
      const formData = callArgs[1];
      expect(formData.get('format')).toEqual('csv');
    });

    it('handles import errors', async () => {
      const file = new File(['bad'], 'test.json');
      const error = new Error('Import failed');
      error.response = { data: { message: 'Invalid format' } };
      api.post.mockRejectedValue(error);
      
      await expect(plotPointService.importPlotPoints(file)).rejects.toEqual({ message: 'Invalid format' });
    });
  });

  describe('batchCreatePlotPoints', () => {
    it('creates multiple plot points successfully', async () => {
      const plotPoints = [
        { name: 'Plot 1', description: 'First' },
        { name: 'Plot 2', description: 'Second' }
      ];
      const mockResponse = { created: 2, items: plotPoints };
      
      api.post.mockResolvedValue({ data: mockResponse });
      
      const result = await plotPointService.batchCreatePlotPoints(plotPoints);
      
      expect(api.post).toHaveBeenCalledWith('/plot-points/batch', { plotPoints });
      expect(result).toEqual(mockResponse);
    });

    it('handles batch create errors', async () => {
      const error = new Error('Batch failed');
      error.response = { data: { message: 'Validation errors' } };
      api.post.mockRejectedValue(error);
      
      await expect(plotPointService.batchCreatePlotPoints([])).rejects.toEqual({ message: 'Validation errors' });
    });
  });

  describe('batchUpdatePlotPoints', () => {
    it('updates multiple plot points successfully', async () => {
      const updates = [
        { id: '1', name: 'Updated 1' },
        { id: '2', name: 'Updated 2' }
      ];
      const mockResponse = { updated: 2, items: updates };
      
      api.put.mockResolvedValue({ data: mockResponse });
      
      const result = await plotPointService.batchUpdatePlotPoints(updates);
      
      expect(api.put).toHaveBeenCalledWith('/plot-points/batch', { updates });
      expect(result).toEqual(mockResponse);
    });

    it('handles batch update errors', async () => {
      const error = new Error('Update failed');
      error.response = { data: { message: 'Update errors' } };
      api.put.mockRejectedValue(error);
      
      await expect(plotPointService.batchUpdatePlotPoints([])).rejects.toEqual({ message: 'Update errors' });
    });
  });

  describe('batchDeletePlotPoints', () => {
    it('deletes multiple plot points successfully', async () => {
      const ids = ['1', '2', '3'];
      const mockResponse = { deleted: 3 };
      
      api.delete.mockResolvedValue({ data: mockResponse });
      
      const result = await plotPointService.batchDeletePlotPoints(ids);
      
      expect(api.delete).toHaveBeenCalledWith('/plot-points/batch', { data: { ids } });
      expect(result).toEqual(mockResponse);
    });

    it('handles batch delete errors', async () => {
      const error = new Error('Delete failed');
      error.response = { data: { message: 'Delete errors' } };
      api.delete.mockRejectedValue(error);
      
      await expect(plotPointService.batchDeletePlotPoints([])).rejects.toEqual({ message: 'Delete errors' });
    });
  });

  describe('error handling without response data', () => {
    it('handles network errors in getPlotPoints', async () => {
      const error = new Error('Network error');
      api.get.mockRejectedValue(error);
      
      await expect(plotPointService.getPlotPoints()).rejects.toEqual(error);
    });

    it('handles network errors in createPlotPoint', async () => {
      const error = new Error('Network error');
      api.post.mockRejectedValue(error);
      
      await expect(plotPointService.createPlotPoint({})).rejects.toEqual(error);
    });

    it('handles network errors in updatePlotPoint', async () => {
      const error = new Error('Network error');
      api.put.mockRejectedValue(error);
      
      await expect(plotPointService.updatePlotPoint('123', {})).rejects.toEqual(error);
    });

    it('handles network errors in deletePlotPoint', async () => {
      const error = new Error('Network error');
      api.delete.mockRejectedValue(error);
      
      await expect(plotPointService.deletePlotPoint('123')).rejects.toEqual(error);
    });
  });
});