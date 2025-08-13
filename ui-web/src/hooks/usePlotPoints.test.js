import { renderHook, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { usePlotPoints, usePlotPoint } from './usePlotPoints';
import * as plotPointService from '../services/plotPointService';

// Mock the service
jest.mock('../services/plotPointService');

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
    it('fetches plot points on mount', async () => {
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
      expect(plotPointService.getPlotPoints).toHaveBeenCalledTimes(1);
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

    it('applies filters when provided', async () => {
      plotPointService.getPlotPoints.mockResolvedValue([]);
      
      const filter = { genre: 'Fantasy' };
      const { result } = renderHook(() => usePlotPoints(filter), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(plotPointService.getPlotPoints).toHaveBeenCalledWith(filter);
    });

    it('returns empty array when no data', async () => {
      plotPointService.getPlotPoints.mockResolvedValue(null);
      
      const { result } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual([]);
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

  describe('Caching', () => {
    it('uses cached data on subsequent renders', async () => {
      const mockPlotPoints = [{ id: '1', name: 'Cached' }];
      plotPointService.getPlotPoints.mockResolvedValue(mockPlotPoints);
      
      const wrapper = createWrapper();
      
      const { result: result1 } = renderHook(() => usePlotPoints(), { wrapper });
      
      await waitFor(() => {
        expect(result1.current.data).toEqual(mockPlotPoints);
      });
      
      const { result: result2 } = renderHook(() => usePlotPoints(), { wrapper });
      
      expect(result2.current.data).toEqual(mockPlotPoints);
      expect(plotPointService.getPlotPoints).toHaveBeenCalledTimes(1);
    });

    it('invalidates cache on mutation', async () => {
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
      
      plotPointService.getPlotPoints.mockResolvedValue([]);
      
      const { result } = renderHook(() => usePlotPoints(), { wrapper });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      // Simulate mutation invalidating cache
      queryClient.invalidateQueries('plotPoints');
      
      await waitFor(() => {
        expect(plotPointService.getPlotPoints).toHaveBeenCalledTimes(2);
      });
    });
  });

  describe('Error Recovery', () => {
    it('retries on failure when configured', async () => {
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: 1, retryDelay: 0 }
        }
      });
      
      const wrapper = ({ children }) => (
        <QueryClientProvider client={queryClient}>
          {children}
        </QueryClientProvider>
      );
      
      plotPointService.getPlotPoints
        .mockRejectedValueOnce(new Error('Network error'))
        .mockResolvedValueOnce([]);
      
      const { result } = renderHook(() => usePlotPoints(), { wrapper });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual([]);
      expect(plotPointService.getPlotPoints).toHaveBeenCalledTimes(2);
    });

    it('provides manual retry function', async () => {
      plotPointService.getPlotPoints.mockRejectedValue(new Error('Error'));
      
      const { result } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isError).toBe(true);
      });
      
      plotPointService.getPlotPoints.mockResolvedValue([]);
      
      await result.current.refetch();
      
      expect(result.current.isError).toBe(false);
      expect(result.current.data).toEqual([]);
    });
  });

  describe('Loading States', () => {
    it('shows loading state during initial fetch', () => {
      plotPointService.getPlotPoints.mockImplementation(
        () => new Promise(() => {}) // Never resolves
      );
      
      const { result } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      expect(result.current.isLoading).toBe(true);
      expect(result.current.isFetching).toBe(true);
      expect(result.current.data).toBeUndefined();
    });

    it('shows fetching state during refetch', async () => {
      plotPointService.getPlotPoints.mockResolvedValue([]);
      
      const { result } = renderHook(() => usePlotPoints(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      plotPointService.getPlotPoints.mockImplementation(
        () => new Promise(() => {}) // Never resolves
      );
      
      result.current.refetch();
      
      expect(result.current.isFetching).toBe(true);
      expect(result.current.isLoading).toBe(false);
    });
  });

  describe('Stale Data', () => {
    it('marks data as stale after configured time', async () => {
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: {
            retry: false,
            staleTime: 100 // 100ms
          }
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
        expect(result.current.isStale).toBe(false);
      });
      
      await new Promise(resolve => setTimeout(resolve, 150));
      
      expect(result.current.isStale).toBe(true);
    });
  });
});