import * as plotPointService from './plotPointService';
import { API, graphqlOperation } from 'aws-amplify';

// Mock AWS Amplify
jest.mock('aws-amplify', () => ({
  API: {
    graphql: jest.fn()
  },
  graphqlOperation: jest.fn((query, variables) => ({ query, variables }))
}));

// Mock GraphQL queries and mutations
jest.mock('../graphql/queries', () => ({
  listPlotPoints: 'LIST_PLOT_POINTS_QUERY',
  getPlotPoint: 'GET_PLOT_POINT_QUERY'
}));

jest.mock('../graphql/mutations', () => ({
  createPlotPoint: 'CREATE_PLOT_POINT_MUTATION',
  updatePlotPoint: 'UPDATE_PLOT_POINT_MUTATION',
  deletePlotPoint: 'DELETE_PLOT_POINT_MUTATION'
}));

describe('PlotPointService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('listPlotPoints', () => {
    it('fetches all plot points successfully', async () => {
      const mockPlotPoints = [
        { id: '1', name: 'Plot Point 1' },
        { id: '2', name: 'Plot Point 2' }
      ];
      
      API.graphql.mockResolvedValue({
        data: {
          listPlotPoints: {
            items: mockPlotPoints
          }
        }
      });
      
      const result = await plotPointService.listPlotPoints();
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'LIST_PLOT_POINTS_QUERY'
        })
      );
      expect(result).toEqual(mockPlotPoints);
    });

    it('handles empty list', async () => {
      API.graphql.mockResolvedValue({
        data: {
          listPlotPoints: {
            items: []
          }
        }
      });
      
      const result = await plotPointService.listPlotPoints();
      
      expect(result).toEqual([]);
    });

    it('handles API errors gracefully', async () => {
      API.graphql.mockRejectedValue(new Error('Network error'));
      
      await expect(plotPointService.listPlotPoints()).rejects.toThrow('Network error');
    });

    it('applies filters when provided', async () => {
      const filter = { genre: { eq: 'Fantasy' } };
      
      API.graphql.mockResolvedValue({
        data: {
          listPlotPoints: {
            items: []
          }
        }
      });
      
      await plotPointService.listPlotPoints(filter);
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          variables: { filter }
        })
      );
    });

    it('handles pagination', async () => {
      const nextToken = 'next-page-token';
      
      API.graphql.mockResolvedValue({
        data: {
          listPlotPoints: {
            items: [],
            nextToken: 'new-next-token'
          }
        }
      });
      
      const result = await plotPointService.listPlotPoints(null, 10, nextToken);
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          variables: {
            limit: 10,
            nextToken
          }
        })
      );
    });
  });

  describe('getPlotPoint', () => {
    it('fetches a single plot point by id', async () => {
      const mockPlotPoint = {
        id: '123',
        name: 'Test Plot Point',
        description: 'Test Description'
      };
      
      API.graphql.mockResolvedValue({
        data: {
          getPlotPoint: mockPlotPoint
        }
      });
      
      const result = await plotPointService.getPlotPoint('123');
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'GET_PLOT_POINT_QUERY',
          variables: { id: '123' }
        })
      );
      expect(result).toEqual(mockPlotPoint);
    });

    it('returns null when plot point not found', async () => {
      API.graphql.mockResolvedValue({
        data: {
          getPlotPoint: null
        }
      });
      
      const result = await plotPointService.getPlotPoint('nonexistent');
      
      expect(result).toBeNull();
    });

    it('throws error for invalid id', async () => {
      await expect(plotPointService.getPlotPoint(null)).rejects.toThrow('Plot point ID is required');
      await expect(plotPointService.getPlotPoint('')).rejects.toThrow('Plot point ID is required');
    });

    it('handles API errors', async () => {
      API.graphql.mockRejectedValue(new Error('Unauthorized'));
      
      await expect(plotPointService.getPlotPoint('123')).rejects.toThrow('Unauthorized');
    });
  });

  describe('createPlotPoint', () => {
    it('creates a new plot point', async () => {
      const input = {
        name: 'New Plot Point',
        description: 'New Description',
        genre: 'Fantasy'
      };
      
      const mockCreatedPlotPoint = {
        id: '456',
        ...input
      };
      
      API.graphql.mockResolvedValue({
        data: {
          createPlotPoint: mockCreatedPlotPoint
        }
      });
      
      const result = await plotPointService.createPlotPoint(input);
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'CREATE_PLOT_POINT_MUTATION',
          variables: { input }
        })
      );
      expect(result).toEqual(mockCreatedPlotPoint);
    });

    it('validates required fields', async () => {
      const invalidInput = { description: 'Missing name' };
      
      await expect(plotPointService.createPlotPoint(invalidInput))
        .rejects.toThrow('Name is required');
    });

    it('validates field lengths', async () => {
      const longName = 'a'.repeat(256);
      const input = { name: longName };
      
      await expect(plotPointService.createPlotPoint(input))
        .rejects.toThrow('Name must be less than 255 characters');
    });

    it('handles creation errors', async () => {
      const input = { name: 'Test' };
      
      API.graphql.mockRejectedValue(new Error('Database error'));
      
      await expect(plotPointService.createPlotPoint(input))
        .rejects.toThrow('Database error');
    });

    it('sanitizes input data', async () => {
      const input = {
        name: '  Test Plot Point  ',
        description: '  Description with spaces  '
      };
      
      API.graphql.mockResolvedValue({
        data: { createPlotPoint: { id: '1' } }
      });
      
      await plotPointService.createPlotPoint(input);
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          variables: {
            input: {
              name: 'Test Plot Point',
              description: 'Description with spaces'
            }
          }
        })
      );
    });
  });

  describe('updatePlotPoint', () => {
    it('updates an existing plot point', async () => {
      const input = {
        id: '123',
        name: 'Updated Name',
        description: 'Updated Description'
      };
      
      const mockUpdatedPlotPoint = { ...input };
      
      API.graphql.mockResolvedValue({
        data: {
          updatePlotPoint: mockUpdatedPlotPoint
        }
      });
      
      const result = await plotPointService.updatePlotPoint(input);
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'UPDATE_PLOT_POINT_MUTATION',
          variables: { input }
        })
      );
      expect(result).toEqual(mockUpdatedPlotPoint);
    });

    it('requires id for update', async () => {
      const input = { name: 'Updated' };
      
      await expect(plotPointService.updatePlotPoint(input))
        .rejects.toThrow('Plot point ID is required for update');
    });

    it('handles partial updates', async () => {
      const input = {
        id: '123',
        description: 'Only updating description'
      };
      
      API.graphql.mockResolvedValue({
        data: { updatePlotPoint: input }
      });
      
      const result = await plotPointService.updatePlotPoint(input);
      
      expect(result).toEqual(input);
    });

    it('handles version conflicts', async () => {
      const input = { id: '123', name: 'Updated' };
      
      API.graphql.mockRejectedValue({
        errors: [{
          errorType: 'DynamoDB:ConditionalCheckFailedException'
        }]
      });
      
      await expect(plotPointService.updatePlotPoint(input))
        .rejects.toThrow();
    });

    it('validates update fields', async () => {
      const input = {
        id: '123',
        name: 'a'.repeat(256)
      };
      
      await expect(plotPointService.updatePlotPoint(input))
        .rejects.toThrow('Name must be less than 255 characters');
    });
  });

  describe('deletePlotPoint', () => {
    it('deletes a plot point by id', async () => {
      const mockDeletedPlotPoint = {
        id: '123',
        name: 'Deleted Plot Point'
      };
      
      API.graphql.mockResolvedValue({
        data: {
          deletePlotPoint: mockDeletedPlotPoint
        }
      });
      
      const result = await plotPointService.deletePlotPoint('123');
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'DELETE_PLOT_POINT_MUTATION',
          variables: {
            input: { id: '123' }
          }
        })
      );
      expect(result).toEqual(mockDeletedPlotPoint);
    });

    it('requires id for deletion', async () => {
      await expect(plotPointService.deletePlotPoint(null))
        .rejects.toThrow('Plot point ID is required for deletion');
      
      await expect(plotPointService.deletePlotPoint(''))
        .rejects.toThrow('Plot point ID is required for deletion');
    });

    it('handles deletion of non-existent plot point', async () => {
      API.graphql.mockResolvedValue({
        data: {
          deletePlotPoint: null
        }
      });
      
      const result = await plotPointService.deletePlotPoint('nonexistent');
      
      expect(result).toBeNull();
    });

    it('handles deletion errors', async () => {
      API.graphql.mockRejectedValue(new Error('Unauthorized'));
      
      await expect(plotPointService.deletePlotPoint('123'))
        .rejects.toThrow('Unauthorized');
    });
  });

  describe('searchPlotPoints', () => {
    it('searches plot points by name', async () => {
      const searchTerm = 'Fantasy';
      const mockResults = [
        { id: '1', name: 'Fantasy Adventure' },
        { id: '2', name: 'Fantasy Quest' }
      ];
      
      API.graphql.mockResolvedValue({
        data: {
          searchPlotPoints: {
            items: mockResults
          }
        }
      });
      
      const result = await plotPointService.searchPlotPoints(searchTerm);
      
      expect(result).toEqual(mockResults);
    });

    it('handles empty search results', async () => {
      API.graphql.mockResolvedValue({
        data: {
          searchPlotPoints: {
            items: []
          }
        }
      });
      
      const result = await plotPointService.searchPlotPoints('nonexistent');
      
      expect(result).toEqual([]);
    });

    it('searches with multiple fields', async () => {
      const searchParams = {
        name: 'Fantasy',
        genre: 'Adventure',
        tags: ['magic', 'quest']
      };
      
      API.graphql.mockResolvedValue({
        data: {
          searchPlotPoints: {
            items: []
          }
        }
      });
      
      await plotPointService.searchPlotPoints(searchParams);
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          variables: expect.objectContaining(searchParams)
        })
      );
    });
  });

  describe('batchOperations', () => {
    it('performs batch create', async () => {
      const items = [
        { name: 'Plot 1' },
        { name: 'Plot 2' },
        { name: 'Plot 3' }
      ];
      
      API.graphql.mockResolvedValue({
        data: { batchCreatePlotPoints: items }
      });
      
      const result = await plotPointService.batchCreate(items);
      
      expect(result).toEqual(items);
    });

    it('performs batch delete', async () => {
      const ids = ['1', '2', '3'];
      
      API.graphql.mockResolvedValue({
        data: { batchDeletePlotPoints: { count: 3 } }
      });
      
      const result = await plotPointService.batchDelete(ids);
      
      expect(result.count).toBe(3);
    });

    it('handles partial batch failures', async () => {
      const items = [
        { name: 'Plot 1' },
        { name: '' }, // Invalid
        { name: 'Plot 3' }
      ];
      
      API.graphql.mockResolvedValue({
        data: {
          batchCreatePlotPoints: {
            successful: [items[0], items[2]],
            failed: [{ item: items[1], error: 'Name required' }]
          }
        }
      });
      
      const result = await plotPointService.batchCreate(items);
      
      expect(result.successful).toHaveLength(2);
      expect(result.failed).toHaveLength(1);
    });
  });

  describe('caching', () => {
    it('caches frequently accessed plot points', async () => {
      const mockPlotPoint = { id: '123', name: 'Cached Plot' };
      
      API.graphql.mockResolvedValue({
        data: { getPlotPoint: mockPlotPoint }
      });
      
      // First call - hits API
      await plotPointService.getPlotPoint('123');
      expect(API.graphql).toHaveBeenCalledTimes(1);
      
      // Second call - uses cache
      const cached = await plotPointService.getPlotPoint('123');
      expect(cached).toEqual(mockPlotPoint);
      expect(API.graphql).toHaveBeenCalledTimes(1);
    });

    it('invalidates cache on update', async () => {
      const updated = { id: '123', name: 'Updated' };
      
      API.graphql.mockResolvedValue({
        data: { updatePlotPoint: updated }
      });
      
      await plotPointService.updatePlotPoint(updated);
      
      // Cache should be invalidated
      API.graphql.mockResolvedValue({
        data: { getPlotPoint: updated }
      });
      
      await plotPointService.getPlotPoint('123');
      expect(API.graphql).toHaveBeenCalledTimes(2);
    });
  });
});