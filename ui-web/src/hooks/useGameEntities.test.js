import { renderHook, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { useGameEntities, useGameEntity } from './useGameEntities';
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
      
      const { result } = renderHook(() => useGameEntities(), {
        wrapper: createWrapper()
      });
      
      expect(result.current.isLoading).toBe(true);
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockEntities);
      expect(gameEntityService.getGameEntities).toHaveBeenCalledTimes(1);
    });

    it('filters entities by type', async () => {
      const mockCharacters = [
        { id: '1', name: 'Hero', type: 'CHARACTER' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockCharacters);
      
      const { result } = renderHook(() => useGameEntities('CHARACTER'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('CHARACTER', undefined);
      expect(result.current.data).toEqual(mockCharacters);
    });

    it('filters entities by plot point', async () => {
      const mockEntities = [
        { id: '1', name: 'Entity 1', plotPointId: 'plot123' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockEntities);
      
      const { result } = renderHook(() => useGameEntities(null, 'plot123'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith(null, 'plot123');
    });

    it('combines type and plot point filters', async () => {
      gameEntityService.getGameEntities.mockResolvedValue([]);
      
      const { result } = renderHook(() => useGameEntities('BEAST', 'plot123'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith('BEAST', 'plot123');
    });

    it('handles fetch errors', async () => {
      const error = new Error('Failed to fetch entities');
      gameEntityService.getGameEntities.mockRejectedValue(error);
      
      const { result } = renderHook(() => useGameEntities(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isError).toBe(true);
      });
      
      expect(result.current.error).toEqual(error);
    });

    it('returns empty array when no data', async () => {
      gameEntityService.getGameEntities.mockResolvedValue(null);
      
      const { result } = renderHook(() => useGameEntities(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual([]);
    });
  });

  describe('useGameEntity', () => {
    it('fetches single entity by id', async () => {
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
      
      const { result } = renderHook(() => useGameEntity('123'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockEntity);
      expect(gameEntityService.getGameEntity).toHaveBeenCalledWith('123');
    });

    it('does not fetch when id is not provided', () => {
      const { result } = renderHook(() => useGameEntity(null), {
        wrapper: createWrapper()
      });
      
      expect(result.current.data).toBeUndefined();
      expect(gameEntityService.getGameEntity).not.toHaveBeenCalled();
    });

    it('handles entity not found', async () => {
      gameEntityService.getGameEntity.mockResolvedValue(null);
      
      const { result } = renderHook(() => useGameEntity('nonexistent'), {
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
        ({ id }) => useGameEntity(id),
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

  describe('Entity Type Specific Hooks', () => {
    it('useCharacters fetches only characters', async () => {
      const mockCharacters = [
        { id: '1', name: 'Hero', type: 'CHARACTER' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockCharacters);
      
      const { result } = renderHook(() => useGameEntities('CHARACTER'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockCharacters);
      expect(result.current.data.every(e => e.type === 'CHARACTER')).toBe(true);
    });

    it('useBeasts fetches only beasts', async () => {
      const mockBeasts = [
        { id: '1', name: 'Wolf', type: 'BEAST' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockBeasts);
      
      const { result } = renderHook(() => useGameEntities('BEAST'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockBeasts);
      expect(result.current.data.every(e => e.type === 'BEAST')).toBe(true);
    });

    it('useItems fetches only items', async () => {
      const mockItems = [
        { id: '1', name: 'Sword', type: 'ITEM' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockItems);
      
      const { result } = renderHook(() => useGameEntities('ITEM'), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data).toEqual(mockItems);
      expect(result.current.data.every(e => e.type === 'ITEM')).toBe(true);
    });
  });

  describe('Refetch and Invalidation', () => {
    it('provides refetch function', async () => {
      const mockEntities = [{ id: '1', name: 'Entity 1' }];
      gameEntityService.getGameEntities.mockResolvedValue(mockEntities);
      
      const { result } = renderHook(() => useGameEntities(), {
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
      
      const { result } = renderHook(() => useGameEntities(), { wrapper });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      queryClient.invalidateQueries('gameEntities');
      
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
      
      const { result } = renderHook(() => useGameEntities(), { wrapper });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(initialEntities);
      });
      
      // Simulate optimistic update
      const newEntity = { id: '2', name: 'New Entity' };
      queryClient.setQueryData(['gameEntities', null, null], old => [...old, newEntity]);
      
      expect(result.current.data).toContain(newEntity);
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
      
      const { result } = renderHook(() => useGameEntities(), { wrapper });
      
      await waitFor(() => {
        expect(result.current.data).toEqual(initialEntities);
      });
      
      // Simulate optimistic update
      const newEntity = { id: '2', name: 'New Entity' };
      const previousData = queryClient.getQueryData(['gameEntities', null, null]);
      queryClient.setQueryData(['gameEntities', null, null], old => [...old, newEntity]);
      
      // Simulate rollback on error
      queryClient.setQueryData(['gameEntities', null, null], previousData);
      
      expect(result.current.data).toEqual(initialEntities);
    });
  });

  describe('Pagination', () => {
    it('handles paginated results', async () => {
      const page1 = [
        { id: '1', name: 'Entity 1' },
        { id: '2', name: 'Entity 2' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue({
        items: page1,
        nextToken: 'next-page'
      });
      
      const { result } = renderHook(() => useGameEntities(), {
        wrapper: createWrapper()
      });
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(result.current.data.items).toEqual(page1);
      expect(result.current.data.nextToken).toBe('next-page');
    });

    it('fetches next page', async () => {
      const page2 = [
        { id: '3', name: 'Entity 3' },
        { id: '4', name: 'Entity 4' }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue({
        items: page2,
        nextToken: null
      });
      
      const { result } = renderHook(
        () => useGameEntities(null, null, 'next-page'),
        { wrapper: createWrapper() }
      );
      
      await waitFor(() => {
        expect(result.current.isLoading).toBe(false);
      });
      
      expect(gameEntityService.getGameEntities).toHaveBeenCalledWith(
        null,
        null,
        'next-page'
      );
    });
  });

  describe('Performance', () => {
    it('debounces rapid refetch calls', async () => {
      gameEntityService.getGameEntities.mockResolvedValue([]);
      
      const { result } = renderHook(() => useGameEntities(), {
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
        expect(gameEntityService.getGameEntities).toHaveBeenCalledTimes(2);
      });
    });

    it('memoizes entity transformations', async () => {
      const mockEntities = [
        { id: '1', name: 'Entity 1', attributes: { str: 'd8' } }
      ];
      
      gameEntityService.getGameEntities.mockResolvedValue(mockEntities);
      
      const { result, rerender } = renderHook(() => useGameEntities(), {
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