import { renderHook, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { 
  useGameEntities, 
  useGameEntity,
  useCreateGameEntity,
  useUpdateGameEntity,
  useDeleteGameEntity,
  useCharacters,
  useCharacter,
  useBeasts,
  useBeast
} from './useGameEntities';

// Mock the service
jest.mock('../services', () => ({
  gameEntityService: {
    getGameEntities: jest.fn(),
    getGameEntity: jest.fn(),
    createGameEntity: jest.fn(),
    updateGameEntity: jest.fn(),
    deleteGameEntity: jest.fn(),
    searchGameEntities: jest.fn()
  }
}));

import { gameEntityService } from '../services';

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

describe('useGameEntities Hook', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('useGameEntities', () => {
    it('fetches all game entities on mount', async () => {
      const mockEntities = [
        { id: '1', name: 'Character 1', type: 'CHARACTER' },
        { id: '2', name: 'Beast 1', type: 'BEAST' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockEntities);
      
      const { result } = renderHook(() => useGameEntities('characters'), {
        wrapper: createWrapper()
      });
      
      expect(result.current.isLoading).toBe(true);
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockEntities);
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('characters', 1, 20, {});
    });

    it('filters entities by type with pagination', async () => {
      const mockCharacters = [
        { id: '1', name: 'Hero', type: 'CHARACTER' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockCharacters);
      
      const { result } = renderHook(() => useGameEntities('characters', 2, 10), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('characters', 2, 10, {});
      expect(result.current.data).toEqual(mockCharacters);
    });

    it('applies filters to entities', async () => {
      const mockEntities = [
        { id: '1', name: 'Entity 1', plotPointId: 'plot123' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockEntities);
      
      const filters = { plotPointId: 'plot123', active: true };
      const { result } = renderHook(() => useGameEntities('characters', 1, 20, filters), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('characters', 1, 20, filters);
    });

    it('handles fetch errors', async () => {
      const error = new Error('Failed to fetch entities');
      gameEntityService.getGameEntities.mockRejectedValue(error);
      
      const { result } = renderHook(() => useGameEntities('characters'), {
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
      
      gameEntityService.getGameEntities
        .mockResolvedValueOnce(initialData)
        .mockResolvedValueOnce(updatedData);
      
      const { result } = renderHook(() => useGameEntities('characters'), {
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
  });

  describe('useGameEntity', () => {
    it('fetches single entity by type and id', async () => {
      const mockEntity = {
        id: '123',
        name: 'Test Character',
        type: 'CHARACTER',
        attributes: {
          agility: 'd8',
          smarts: 'd6'
        }
      };
      
      gameEntityService.getGameEntity.mockResolvedValue(mockEntity);
      
      const { result } = renderHook(() => useGameEntity('characters', '123'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockEntity);
      expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('characters', '123');
    });

    it('does not fetch when id or type is not provided', () => {
      const { result } = renderHook(() => useGameEntity('characters', null), {
        wrapper: createWrapper()
      });
      
      expect(result.current.data).toBeUndefined();
      expect(gameEntityService.getGameEntity).not.toHaveBeenCalled();
    });

    it('does not fetch when type is not provided', () => {
      const { result } = renderHook(() => useGameEntity(null, '123'), {
        wrapper: createWrapper()
      });
      
      expect(result.current.data).toBeUndefined();
      expect(gameEntityService.getGameEntity).not.toHaveBeenCalled();
    });

    it('handles entity not found', async () => {
      gameEntityService.getGameEntity.mockResolvedValue(null);
      
      const { result } = renderHook(() => useGameEntity('characters', 'nonexistent'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toBeNull();
    });

    it('refetches when id changes', async () => {
      const firstEntity = { id: '1', name: 'First' };
      const secondEntity = { id: '2', name: 'Second' };
      
      gameEntityService.getGameEntity
        .mockResolvedValueOnce(firstEntity)
        .mockResolvedValueOnce(secondEntity);
      
      const { result, rerender } = renderHook(
        ({ id }) => useGameEntity('characters', id),
        {
          wrapper: createWrapper(),
          initialProps: { id: '1' }
        }
      );
      
      await waitFor(() => {
        expect(result.current.data).toEqual(firstEntity);
      });
      
      rerender({ id: '2' });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(secondEntity);
      });
      
      expect(gameEntityService.getGameEntity).toHaveBeenCalledTimes(2);
    });
  });

  describe('useCreateGameEntity', () => {
    it('creates new entity and invalidates cache', async () => {
      const newEntity = { id: '123', name: 'New Character' };
      gameEntityService.createGameEntity.mockResolvedValue(newEntity);
      
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
      
      const { result } = renderHook(() => useCreateGameEntity('characters'), { wrapper });
      
      await result.current.mutateAsync({ name: 'New Character' });
      
      expect(gameEntityService.createGameEntity).toHaveBeenCalledWith('characters', { name: 'New Character' });
      expect(invalidateSpy).toHaveBeenCalledWith(['gameEntities', 'characters']);
    });

    it('handles creation errors', async () => {
      const error = new Error('Creation failed');
      gameEntityService.createGameEntity.mockRejectedValue(error);
      
      const { result } = renderHook(() => useCreateGameEntity('characters'), {
        wrapper: createWrapper()
      });
      
      try {
        await result.current.mutateAsync({ name: 'New Character' });
        expect(true).toBe(false); // Should have thrown
      } catch (e) {
        expect(e).toEqual(error);
      }
    });
  });

  describe('useUpdateGameEntity', () => {
    it('updates entity and invalidates cache', async () => {
      const updatedEntity = { id: '123', name: 'Updated Character' };
      gameEntityService.updateGameEntity.mockResolvedValue(updatedEntity);
      
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
      
      const { result } = renderHook(() => useUpdateGameEntity('characters'), { wrapper });
      
      await result.current.mutateAsync({ id: '123', data: { name: 'Updated Character' } });
      
      expect(gameEntityService.updateGameEntity).toHaveBeenCalledWith('characters', '123', { name: 'Updated Character' });
      expect(invalidateSpy).toHaveBeenCalledWith(['gameEntity', 'characters', '123']);
      expect(invalidateSpy).toHaveBeenCalledWith(['gameEntities', 'characters']);
    });
  });

  describe('useDeleteGameEntity', () => {
    it('deletes entity and invalidates cache', async () => {
      gameEntityService.deleteGameEntity.mockResolvedValue({ success: true });
      
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
      
      const { result } = renderHook(() => useDeleteGameEntity('characters'), { wrapper });
      
      await result.current.mutateAsync('123');
      
      expect(gameEntityService.deleteGameEntity).toHaveBeenCalledWith('characters', '123');
      expect(invalidateSpy).toHaveBeenCalledWith(['gameEntities', 'characters']);
    });
  });

  describe('Entity Type Specific Hooks', () => {
    it('useCharacters fetches characters', async () => {
      const mockCharacters = [
        { id: '1', name: 'Hero', type: 'CHARACTER' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockCharacters);
      
      const { result } = renderHook(() => useCharacters(1, 10, { active: true }), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('characters', 1, 10, { active: true });
      expect(result.current.data).toEqual(mockCharacters);
    });

    it('useCharacter fetches single character', async () => {
      const mockCharacter = { id: '1', name: 'Hero', type: 'CHARACTER' };
      
      gameEntityService.getGameEntity.mockResolvedValue(mockCharacter);
      
      const { result } = renderHook(() => useCharacter('1'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('characters', '1');
      expect(result.current.data).toEqual(mockCharacter);
    });

    it('useBeasts fetches beasts', async () => {
      const mockBeasts = [
        { id: '1', name: 'Wolf', type: 'BEAST' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockBeasts);
      
      const { result } = renderHook(() => useBeasts(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('beasts', 1, 20, {});
      expect(result.current.data).toEqual(mockBeasts);
    });

    it('useBeast fetches single beast', async () => {
      const mockBeast = { id: '1', name: 'Wolf', type: 'BEAST' };
      
      gameEntityService.getGameEntity.mockResolvedValue(mockBeast);
      
      const { result } = renderHook(() => useBeast('1'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('beasts', '1');
      expect(result.current.data).toEqual(mockBeast);
    });
  });

  describe('Refetch and Invalidation', () => {
    it('provides refetch function', async () => {
      const mockEntities = [{ id: '1', name: 'Entity 1' }];
      gameEntityService.getGameEntities.mockResolvedValue(mockEntities);
      
      const { result } = renderHook(() => useGameEntities('characters'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      gameEntityService.getGameEntities.mockResolvedValue([
        ...mockEntities,
        { id: '2', name: 'New Entity' }
      ]);
      
      await result.current.refetch();
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledTimes(2);
    });

    it('invalidates cache on mutation', async () => {
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
      
      gameEntityService.getGameEntities.mockResolvedValue([]);
      
      const { result } = renderHook(() => useGameEntities('characters'), { wrapper });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      queryClient.invalidateQueries(['gameEntities', 'characters']);
      
      await waitFor(() => {
        expect(gameEntityService.getGameEntities).toHaveBeenCalledTimes(2);
      });
    });
  });

  describe('Optimistic Updates', () => {
    it('optimistically updates entity list on creation', async () => {
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
      
      const initialEntities = [{ id: '1', name: 'Entity 1' }];
      gameEntityService.getGameEntities.mockResolvedValue(initialEntities);
      
      const { result } = renderHook(() => useGameEntities('characters'), { wrapper });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(initialEntities);
      });
      
      // Simulate optimistic update
      const newEntity = { id: '2', name: 'New Entity' };
      queryClient.setQueryData(['gameEntities', 'characters', 1, 20, {}], old => [...(old || []), newEntity]);
      
      // Wait for the update to be reflected
      await waitFor(() => {
        const updatedData = queryClient.getQueryData(['gameEntities', 'characters', 1, 20, {}]);
        expect(updatedData).toContain(newEntity);
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
      
      const initialEntities = [{ id: '1', name: 'Entity 1' }];
      gameEntityService.getGameEntities.mockResolvedValue(initialEntities);
      
      const { result } = renderHook(() => useGameEntities('characters'), { wrapper });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(initialEntities);
      });
      
      // Simulate optimistic update
      const newEntity = { id: '2', name: 'New Entity' };
      const queryKey = ['gameEntities', 'characters', 1, 20, {}];
      const previousData = queryClient.getQueryData(queryKey);
      queryClient.setQueryData(queryKey, old => [...(old || []), newEntity]);
      
      // Simulate rollback on error
      queryClient.setQueryData(queryKey, previousData);
      
      expect(result.current.data).toEqual(initialEntities);
    });
  });

  describe('Search functionality', () => {
    it('handles search queries with debouncing', async () => {
      const mockResults = [
        { id: '1', name: 'John Doe', type: 'CHARACTER' }
      ];
      
      gameEntityService.searchGameEntities.mockResolvedValue(mockResults);
      
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
      
      // Simulate search hook usage
      const { result } = renderHook(() => {
        const searchQuery = useQuery(
          ['gameEntities', 'search', 'characters', 'John'],
          () => gameEntityService.searchGameEntities('characters', 'John'),
          { enabled: !!('John'), keepPreviousData: true }
        );
        return searchQuery;
      }, { wrapper });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.searchGameEntities).toHaveBeenCalledWith('characters', 'John');
      expect(result.current.data).toEqual(mockResults);
    });

    it('handles empty search results', async () => {
      gameEntityService.searchGameEntities.mockResolvedValue([]);
      
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
      
      const { result } = renderHook(() => {
        const searchQuery = useQuery(
          ['gameEntities', 'search', 'characters', 'nonexistent'],
          () => gameEntityService.searchGameEntities('characters', 'nonexistent'),
          { enabled: true }
        );
        return searchQuery;
      }, { wrapper });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual([]);
    });

    it('handles search errors gracefully', async () => {
      const error = new Error('Search failed');
      gameEntityService.searchGameEntities.mockRejectedValue(error);
      
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
      
      const { result } = renderHook(() => {
        const searchQuery = useQuery(
          ['gameEntities', 'search', 'characters', 'error'],
          () => gameEntityService.searchGameEntities('characters', 'error'),
          { enabled: true }
        );
        return searchQuery;
      }, { wrapper });
      
      await waitFor(() => {
        expect(result.current.isError).toBe(true);
      });
      
      expect(result.current.error).toEqual(error);
    });
  });

  describe('Cache management', () => {
    it('uses proper cache keys for different entity types', async () => {
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
      
      gameEntityService.getGameEntities.mockResolvedValue([]);
      
      // Test different entity types use different cache keys
      const { result: charactersResult } = renderHook(() => useGameEntities('characters'), { wrapper });
      const { result: beastsResult } = renderHook(() => useGameEntities('beasts'), { wrapper });
      
      await waitFor(() => {
        expect(charactersResult.current.isLoading).toBe(false);
        expect(beastsResult.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('characters', 1, 20, {});
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('beasts', 1, 20, {});
      expect(gameEntityService.getGameEntities).toHaveBeenCalledTimes(2);
    });

    it('invalidates related caches on mutation', async () => {
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
      gameEntityService.updateGameEntity.mockResolvedValue({ id: '123', name: 'Updated' });
      
      const { result } = renderHook(() => useUpdateGameEntity('characters'), { wrapper });
      
      await result.current.mutateAsync({ id: '123', data: { name: 'Updated Character' } });
      
      // Should invalidate both specific entity and entity list caches
      expect(invalidateSpy).toHaveBeenCalledWith(['gameEntity', 'characters', '123']);
      expect(invalidateSpy).toHaveBeenCalledWith(['gameEntities', 'characters']);
    });

    it('preserves cache data structure across rerenders', async () => {
      const mockEntities = [
        { id: '1', name: 'Entity 1', nested: { value: 'test' } }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockEntities);
      
      const { result, rerender } = renderHook(
        ({ filters }) => useGameEntities('characters', 1, 20, filters),
        {
          wrapper: createWrapper(),
          initialProps: { filters: {} }
        }
      );
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      const originalData = result.current.data;
      
      // Rerender with same filters should return same reference
      rerender({ filters: {} });
      
      expect(result.current.data).toBe(originalData);
      
      // Rerender with different filters should trigger new query
      rerender({ filters: { active: true } });
      
      await waitFor(() => {
        expect(gameEntityService.getGameEntities).toHaveBeenCalledTimes(2);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenLastCalledWith('characters', 1, 20, { active: true });
    });
  });

  describe('Edge cases and error handling', () => {
    it('handles malformed entity data', async () => {
      const malformedEntity = {
        id: '123',
        // Missing required fields
        invalidField: 'should not crash'
      };
      
      gameEntityService.getGameEntity.mockResolvedValue(malformedEntity);
      
      const { result } = renderHook(() => useGameEntity('characters', '123'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(malformedEntity);
      expect(result.current.isError).toBe(false);
    });

    it('handles concurrent mutations gracefully', async () => {
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
      
      gameEntityService.updateGameEntity
        .mockResolvedValueOnce({ id: '123', name: 'First Update' })
        .mockResolvedValueOnce({ id: '123', name: 'Second Update' });
      
      const { result } = renderHook(() => useUpdateGameEntity('characters'), { wrapper });
      
      // Trigger concurrent mutations
      const firstUpdate = result.current.mutateAsync({ id: '123', data: { name: 'First Update' } });
      const secondUpdate = result.current.mutateAsync({ id: '123', data: { name: 'Second Update' } });
      
      await Promise.all([firstUpdate, secondUpdate]);
      
      expect(gameEntityService.updateGameEntity).toHaveBeenCalledTimes(2);
    });

    it('handles network timeout gracefully', async () => {
      const timeoutError = new Error('Request timeout');
      timeoutError.code = 'TIMEOUT';
      gameEntityService.getGameEntities.mockRejectedValue(timeoutError);
      
      const { result } = renderHook(() => useGameEntities('characters'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isError).toBe(true);
      });
      
      expect(result.current.error).toEqual(timeoutError);
      expect(result.current.data).toBeUndefined();
    });

    it('handles null/undefined entity IDs gracefully', async () => {
      const testCases = [null, undefined, '', 0, false];
      
      testCases.forEach(invalidId => {
        const { result } = renderHook(() => useGameEntity('characters', invalidId), {
          wrapper: createWrapper()
        });
        
        expect(result.current.data).toBeUndefined();
        expect(gameEntityService.getGameEntity).not.toHaveBeenCalled();
      });
    });

    it('handles large datasets efficiently', async () => {
      const largeDataset = Array.from({ length: 1000 }, (_, i) => ({
        id: `entity-${i}`,
        name: `Entity ${i}`,
        data: { value: i }
      }));
      
      gameEntityService.getGameEntities.mockResolvedValue(largeDataset);
      
      const startTime = Date.now();
      
      const { result } = renderHook(() => useGameEntities('characters'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      const endTime = Date.now();
      
      expect(result.current.data).toHaveLength(1000);
      expect(endTime - startTime).toBeLessThan(1000); // Should complete within 1 second
    });
  });

  describe('Performance', () => {
    it('debounces rapid refetch calls', async () => {
      gameEntityService.getGameEntities.mockResolvedValue([]);
      
      const { result } = renderHook(() => useGameEntities('characters'), {
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
        expect(gameEntityService.getGameEntities).toHaveBeenCalledTimes(2);
      });
    });

    it('memoizes entity data reference', async () => {
      const mockEntities = [
        { id: '1', name: 'Entity 1', attributes: { str: 'd8' } }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockEntities);
      
      const { result, rerender } = renderHook(() => useGameEntities('characters'), {
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

    it('optimizes repeated queries with same parameters', async () => {
      gameEntityService.getGameEntities.mockResolvedValue([]);
      
      const wrapper = createWrapper();
      
      // Render multiple hooks with same parameters
      const { result: result1 } = renderHook(() => useGameEntities('characters', 1, 20, {}), { wrapper });
      const { result: result2 } = renderHook(() => useGameEntities('characters', 1, 20, {}), { wrapper });
      
      await waitFor(() => {
        expect(result1.current.isLoading).toBe(false);
        expect(result2.current.isLoading).toBe(false);
      });
      
      // Should only call service once due to caching
      expect(gameEntityService.getGameEntities).toHaveBeenCalledTimes(1);
    });

    it('handles rapid state changes efficiently', async () => {
      let resolveQuery;
      gameEntityService.getGameEntities.mockImplementation(() => 
        new Promise(resolve => { resolveQuery = resolve; })
      );
      
      const { result, rerender } = renderHook(
        ({ filters }) => useGameEntities('characters', 1, 20, filters),
        {
          wrapper: createWrapper(),
          initialProps: { filters: {} }
        }
      );
      
      // Rapidly change filters
      rerender({ filters: { active: true } });
      rerender({ filters: { active: false } });
      rerender({ filters: { name: 'test' } });
      
      // Resolve the query
      resolveQuery([]);
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      // Should handle rapid changes without issues
      expect(result.current.isError).toBe(false);
    });
  });
});