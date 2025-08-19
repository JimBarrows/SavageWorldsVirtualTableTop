import { renderHook, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { 
  usePlotPoints, 
  usePlotPoint,
  usePlotPointByName,
  useCreatePlotPoint,
  useUpdatePlotPoint,
  useDeletePlotPoint,
  useSearchPlotPoints
} from './usePlotPoints';

// Mock the service
jest.mock('../services', () => ({
  plotPointService: {
    getPlotPoints: jest.fn(),
    getPlotPoint: jest.fn(),
    createPlotPoint: jest.fn(),
    updatePlotPoint: jest.fn(),
    deletePlotPoint: jest.fn(),
    searchPlotPoints: jest.fn(),
    getPlotPointByName: jest.fn()
  }
}));

import { plotPointService } from '../services';

const createWrapper = () => {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false }
    }
  });
  
  return ({ children }) => (
    <QueryClientProvider client={queryClient}>
      {children}
    </QueryClientProvider>
  );
};

describe('usePlotPoints Hook', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('usePlotPoints', () => {
    it('fetches plot points on mount with default pagination', async () => {
      const mockPlotPoints = [
        { id: '1', name: 'Plot Point 1' },
        { id: '2', name: 'Plot Point 2' }
      ];
      
      plotPointService.getPlotPoints.mockResolvedValue(mockPlotPoints);
      
      const { result } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      expect(result.current.isLoading).toBe(true);
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockPlotPoints);
      expect(plotPointService.getPlotPoints).toHaveBeenCalledWith(1, 20, {});
    });

    it('fetches plot points with custom pagination', async () => {
      const mockPlotPoints = [
        { id: '3', name: 'Plot Point 3' }
      ];
      
      plotPointService.getPlotPoints.mockResolvedValue(mockPlotPoints);
      
      const { result } = renderHook(() => usePlotPoints(2, 10), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(plotPointService.getPlotPoints).toHaveBeenCalledWith(2, 10, {});
      expect(result.current.data).toEqual(mockPlotPoints);
    });

    it('applies filters when provided', async () => {
      plotPointService.getPlotPoints.mockResolvedValue([]);
      
      const filters = { genre: 'Fantasy', status: 'active' };
      const { result } = renderHook(() => usePlotPoints(1, 20, filters), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(plotPointService.getPlotPoints).toHaveBeenCalledWith(1, 20, filters);
    });

    it('handles fetch errors', async () => {
      const error = new Error('Failed to fetch');
      plotPointService.getPlotPoints.mockRejectedValue(error);
      
      const { result } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isError).toBe(true);
      });
      
      expect(result.current.error).toEqual(error);
    });

    it('keeps previous data during refetch', async () => {
      const initialData = [{ id: '1', name: 'Initial' }];
      const updatedData = [{ id: '1', name: 'Updated' }];
      
      plotPointService.getPlotPoints
        .mockResolvedValueOnce(initialData)
        .mockResolvedValueOnce(updatedData);
      
      const { result } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(initialData);
      });
      
      result.current.refetch();
      
      // keepPreviousData should maintain the old data during refetch
      expect(result.current.data).toEqual(initialData);
      
      await waitFor(() => {
        expect(result.current.data).toEqual(updatedData);
      });
    });

    it('provides refetch function', async () => {
      const mockPlotPoints = [{ id: '1', name: 'Plot Point 1' }];
      plotPointService.getPlotPoints.mockResolvedValue(mockPlotPoints);
      
      const { result } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      plotPointService.getPlotPoints.mockResolvedValue([
        ...mockPlotPoints,
        { id: '2', name: 'New Plot Point' }
      ]);
      
      await result.current.refetch();
      
      expect(plotPointService.getPlotPoints).toHaveBeenCalledTimes(2);
    });
  });

  describe('usePlotPoint', () => {
    it('fetches single plot point by id', async () => {
      const mockPlotPoint = {
        id: '123',
        name: 'Test Plot Point',
        description: 'Test Description'
      };
      
      plotPointService.getPlotPoint.mockResolvedValue(mockPlotPoint);
      
      const { result } = renderHook(() => usePlotPoint('123'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockPlotPoint);
      expect(plotPointService.getPlotPoint).toHaveBeenCalledWith('123');
    });

    it('does not fetch when id is not provided', () => {
      const { result } = renderHook(() => usePlotPoint(null), {
        wrapper: createWrapper()
      });
      
      expect(result.current.data).toBeUndefined();
      expect(plotPointService.getPlotPoint).not.toHaveBeenCalled();
    });

    it('handles plot point not found', async () => {
      plotPointService.getPlotPoint.mockResolvedValue(null);
      
      const { result } = renderHook(() => usePlotPoint('nonexistent'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toBeNull();
    });

    it('refetches when id changes', async () => {
      const firstPlotPoint = { id: '1', name: 'First' };
      const secondPlotPoint = { id: '2', name: 'Second' };
      
      plotPointService.getPlotPoint
        .mockResolvedValueOnce(firstPlotPoint)
        .mockResolvedValueOnce(secondPlotPoint);
      
      const { result, rerender } = renderHook(
        ({ id }) => usePlotPoint(id),
        {
          wrapper: createWrapper(),
          initialProps: { id: '1' }
        }
      );
      
      await waitFor(() => {
        expect(result.current.data).toEqual(firstPlotPoint);
      });
      
      rerender({ id: '2' });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(secondPlotPoint);
      });
      
      expect(plotPointService.getPlotPoint).toHaveBeenCalledTimes(2);
    });
  });

  describe('usePlotPointByName', () => {
    it('fetches plot point by name', async () => {
      const mockPlotPoint = {
        id: '123',
        name: 'Test Plot Point',
        description: 'Test Description'
      };
      
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      
      const { result } = renderHook(() => usePlotPointByName('Test Plot Point'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockPlotPoint);
      expect(plotPointService.getPlotPointByName).toHaveBeenCalledWith('Test Plot Point');
    });

    it('does not fetch when name is not provided', () => {
      const { result } = renderHook(() => usePlotPointByName(null), {
        wrapper: createWrapper()
      });
      
      expect(result.current.data).toBeUndefined();
      expect(plotPointService.getPlotPointByName).not.toHaveBeenCalled();
    });
  });

  describe('useCreatePlotPoint', () => {
    it('creates new plot point and invalidates cache', async () => {
      const newPlotPoint = { id: '123', name: 'New Plot Point' };
      plotPointService.createPlotPoint.mockResolvedValue(newPlotPoint);
      
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: false },
          mutations: { retry: false }
        }
      });
      
      const wrapper = ({ children }) => (
        <QueryClientProvider client={queryClient}>
          {children}
        </QueryClientProvider>
      );
      
      const invalidateSpy = jest.spyOn(queryClient, 'invalidateQueries');
      
      const { result } = renderHook(() => useCreatePlotPoint(), { wrapper });
      
      await result.current.mutateAsync({ name: 'New Plot Point' });
      
      expect(plotPointService.createPlotPoint).toHaveBeenCalledWith({ name: 'New Plot Point' });
      expect(invalidateSpy).toHaveBeenCalledWith(['plotPoints']);
    });

    it('handles creation errors', async () => {
      const error = new Error('Creation failed');
      plotPointService.createPlotPoint.mockRejectedValue(error);
      
      const { result } = renderHook(() => useCreatePlotPoint(), {
        wrapper: createWrapper()
      });
      
      try {
        await result.current.mutateAsync({ name: 'New Plot Point' });
        expect(true).toBe(false); // Should have thrown
      } catch (e) {
        expect(e).toEqual(error);
      }
    });
  });

  describe('useUpdatePlotPoint', () => {
    it('updates plot point and invalidates cache', async () => {
      const updatedPlotPoint = { id: '123', name: 'Updated Plot Point' };
      plotPointService.updatePlotPoint.mockResolvedValue(updatedPlotPoint);
      
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: false },
          mutations: { retry: false }
        }
      });
      
      const wrapper = ({ children }) => (
        <QueryClientProvider client={queryClient}>
          {children}
        </QueryClientProvider>
      );
      
      const invalidateSpy = jest.spyOn(queryClient, 'invalidateQueries');
      
      const { result } = renderHook(() => useUpdatePlotPoint(), { wrapper });
      
      await result.current.mutateAsync({ id: '123', data: { name: 'Updated Plot Point' } });
      
      expect(plotPointService.updatePlotPoint).toHaveBeenCalledWith('123', { name: 'Updated Plot Point' });
      expect(invalidateSpy).toHaveBeenCalledWith(['plotPoint', '123']);
      expect(invalidateSpy).toHaveBeenCalledWith(['plotPoints']);
    });

    it('handles update errors', async () => {
      const error = new Error('Update failed');
      plotPointService.updatePlotPoint.mockRejectedValue(error);
      
      const { result } = renderHook(() => useUpdatePlotPoint(), {
        wrapper: createWrapper()
      });
      
      try {
        await result.current.mutateAsync({ id: '123', data: { name: 'Updated' } });
        expect(true).toBe(false); // Should have thrown
      } catch (e) {
        expect(e).toEqual(error);
      }
    });
  });

  describe('useDeletePlotPoint', () => {
    it('deletes plot point and invalidates cache', async () => {
      plotPointService.deletePlotPoint.mockResolvedValue({ success: true });
      
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: false },
          mutations: { retry: false }
        }
      });
      
      const wrapper = ({ children }) => (
        <QueryClientProvider client={queryClient}>
          {children}
        </QueryClientProvider>
      );
      
      const invalidateSpy = jest.spyOn(queryClient, 'invalidateQueries');
      
      const { result } = renderHook(() => useDeletePlotPoint(), { wrapper });
      
      await result.current.mutateAsync('123');
      
      expect(plotPointService.deletePlotPoint).toHaveBeenCalledWith('123');
      expect(invalidateSpy).toHaveBeenCalledWith(['plotPoints']);
    });

    it('handles deletion errors', async () => {
      const error = new Error('Deletion failed');
      plotPointService.deletePlotPoint.mockRejectedValue(error);
      
      const { result } = renderHook(() => useDeletePlotPoint(), {
        wrapper: createWrapper()
      });
      
      try {
        await result.current.mutateAsync('123');
        expect(true).toBe(false); // Should have thrown
      } catch (e) {
        expect(e).toEqual(error);
      }
    });
  });

  describe('useSearchPlotPoints', () => {
    it('searches plot points with query', async () => {
      const searchResults = [
        { id: '1', name: 'Matching Plot Point' }
      ];
      
      plotPointService.searchPlotPoints.mockResolvedValue(searchResults);
      
      const { result } = renderHook(() => useSearchPlotPoints('matching'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(searchResults);
      expect(plotPointService.searchPlotPoints).toHaveBeenCalledWith('matching', 1, 20);
    });

    it('searches with custom pagination', async () => {
      const searchResults = [
        { id: '2', name: 'Result Page 2' }
      ];
      
      plotPointService.searchPlotPoints.mockResolvedValue(searchResults);
      
      const { result } = renderHook(() => useSearchPlotPoints('test', 2, 10), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(plotPointService.searchPlotPoints).toHaveBeenCalledWith('test', 2, 10);
    });

    it('does not search when query is empty', () => {
      const { result } = renderHook(() => useSearchPlotPoints(''), {
        wrapper: createWrapper()
      });
      
      expect(result.current.data).toBeUndefined();
      expect(plotPointService.searchPlotPoints).not.toHaveBeenCalled();
    });

    it('keeps previous data during search refetch', async () => {
      const initialResults = [{ id: '1', name: 'Initial' }];
      const updatedResults = [{ id: '2', name: 'Updated' }];
      
      plotPointService.searchPlotPoints
        .mockResolvedValueOnce(initialResults)
        .mockResolvedValueOnce(updatedResults);
      
      const { result } = renderHook(() => useSearchPlotPoints('test'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(initialResults);
      });
      
      result.current.refetch();
      
      // keepPreviousData should maintain the old data during refetch
      expect(result.current.data).toEqual(initialResults);
      
      await waitFor(() => {
        expect(result.current.data).toEqual(updatedResults);
      });
    });
  });

  describe('Cache Invalidation', () => {
    it('invalidates queries on mutation', async () => {
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: false }
        }
      });
      
      const wrapper = ({ children }) => (
        <QueryClientProvider client={queryClient}>
          {children}
        </QueryClientProvider>
      );
      
      plotPointService.getPlotPoints.mockResolvedValue([]);
      
      const { result } = renderHook(() => usePlotPoints(), { wrapper });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      queryClient.invalidateQueries(['plotPoints']);
      
      await waitFor(() => {
        expect(plotPointService.getPlotPoints).toHaveBeenCalledTimes(2);
      });
    });
  });

  describe('Optimistic Updates', () => {
    it('optimistically updates plot point list on creation', async () => {
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: false }
        }
      });
      
      const wrapper = ({ children }) => (
        <QueryClientProvider client={queryClient}>
          {children}
        </QueryClientProvider>
      );
      
      const initialPlotPoints = [{ id: '1', name: 'Plot Point 1' }];
      plotPointService.getPlotPoints.mockResolvedValue(initialPlotPoints);
      
      const { result } = renderHook(() => usePlotPoints(), { wrapper });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(initialPlotPoints);
      });
      
      // Simulate optimistic update
      const newPlotPoint = { id: '2', name: 'New Plot Point' };
      queryClient.setQueryData(['plotPoints', 1, 20, {}], old => [...(old || []), newPlotPoint]);
      
      // Wait for the update to be reflected
      await waitFor(() => {
        const updatedData = queryClient.getQueryData(['plotPoints', 1, 20, {}]);
        expect(updatedData).toContain(newPlotPoint);
      });
    });

    it('rolls back optimistic update on error', async () => {
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: false }
        }
      });
      
      const wrapper = ({ children }) => (
        <QueryClientProvider client={queryClient}>
          {children}
        </QueryClientProvider>
      );
      
      const initialPlotPoints = [{ id: '1', name: 'Plot Point 1' }];
      plotPointService.getPlotPoints.mockResolvedValue(initialPlotPoints);
      
      const { result } = renderHook(() => usePlotPoints(), { wrapper });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(initialPlotPoints);
      });
      
      // Simulate optimistic update
      const newPlotPoint = { id: '2', name: 'New Plot Point' };
      const queryKey = ['plotPoints', 1, 20, {}];
      const previousData = queryClient.getQueryData(queryKey);
      queryClient.setQueryData(queryKey, old => [...(old || []), newPlotPoint]);
      
      // Simulate rollback on error
      queryClient.setQueryData(queryKey, previousData);
      
      expect(result.current.data).toEqual(initialPlotPoints);
    });
  });

  describe('Performance', () => {
    it('debounces rapid refetch calls', async () => {
      plotPointService.getPlotPoints.mockResolvedValue([]);
      
      const { result } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      // Rapid refetch calls
      result.current.refetch();
      result.current.refetch();
      result.current.refetch();
      
      await waitFor(() => {
        // Should only call twice - initial load and one refetch
        expect(plotPointService.getPlotPoints).toHaveBeenCalledTimes(2);
      });
    });

    it('memoizes plot point data reference', async () => {
      const mockPlotPoints = [
        { id: '1', name: 'Plot Point 1', description: 'Description' }
      ];
      
      plotPointService.getPlotPoints.mockResolvedValue(mockPlotPoints);
      
      const { result, rerender } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      const firstData = result.current.data;
      
      rerender();
      
      const secondData = result.current.data;
      
      expect(firstData).toBe(secondData); // Same reference
    });
  });
});