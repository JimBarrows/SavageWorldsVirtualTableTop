import * as gameEntityService from './gameEntityService';
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
  listGameEntities: 'LIST_GAME_ENTITIES_QUERY',
  getGameEntity: 'GET_GAME_ENTITY_QUERY'
}));

jest.mock('../graphql/mutations', () => ({
  createGameEntity: 'CREATE_GAME_ENTITY_MUTATION',
  updateGameEntity: 'UPDATE_GAME_ENTITY_MUTATION',
  deleteGameEntity: 'DELETE_GAME_ENTITY_MUTATION'
}));

describe('GameEntityService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Entity Types', () => {
    it('defines all entity types', () => {
      expect(gameEntityService.ENTITY_TYPES).toBeDefined();
      expect(gameEntityService.ENTITY_TYPES.CHARACTER).toBe('CHARACTER');
      expect(gameEntityService.ENTITY_TYPES.BEAST).toBe('BEAST');
      expect(gameEntityService.ENTITY_TYPES.VEHICLE).toBe('VEHICLE');
      expect(gameEntityService.ENTITY_TYPES.ITEM).toBe('ITEM');
    });

    it('validates entity type', () => {
      expect(gameEntityService.isValidEntityType('CHARACTER')).toBe(true);
      expect(gameEntityService.isValidEntityType('BEAST')).toBe(true);
      expect(gameEntityService.isValidEntityType('INVALID')).toBe(false);
    });
  });

  describe('listGameEntities', () => {
    it('fetches all game entities', async () => {
      const mockEntities = [
        { id: '1', name: 'Hero', type: 'CHARACTER' },
        { id: '2', name: 'Dragon', type: 'BEAST' }
      ];
      
      API.graphql.mockResolvedValue({
        data: {
          listGameEntities: {
            items: mockEntities
          }
        }
      });
      
      const result = await gameEntityService.listGameEntities();
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'LIST_GAME_ENTITIES_QUERY'
        })
      );
      expect(result).toEqual(mockEntities);
    });

    it('filters by entity type', async () => {
      API.graphql.mockResolvedValue({
        data: {
          listGameEntities: {
            items: []
          }
        }
      });
      
      await gameEntityService.listGameEntities('CHARACTER');
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          variables: {
            filter: {
              type: { eq: 'CHARACTER' }
            }
          }
        })
      );
    });

    it('filters by plot point id', async () => {
      API.graphql.mockResolvedValue({
        data: {
          listGameEntities: {
            items: []
          }
        }
      });
      
      await gameEntityService.listGameEntitiesByPlotPoint('plot123');
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          variables: {
            filter: {
              plotPointId: { eq: 'plot123' }
            }
          }
        })
      );
    });

    it('combines multiple filters', async () => {
      API.graphql.mockResolvedValue({
        data: {
          listGameEntities: {
            items: []
          }
        }
      });
      
      await gameEntityService.listGameEntities('BEAST', 'plot123');
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          variables: {
            filter: {
              type: { eq: 'BEAST' },
              plotPointId: { eq: 'plot123' }
            }
          }
        })
      );
    });
  });

  describe('getGameEntity', () => {
    it('fetches a single game entity', async () => {
      const mockEntity = {
        id: '123',
        name: 'Test Character',
        type: 'CHARACTER',
        attributes: {
          agility: 'd8',
          smarts: 'd6'
        }
      };
      
      API.graphql.mockResolvedValue({
        data: {
          getGameEntity: mockEntity
        }
      });
      
      const result = await gameEntityService.getGameEntity('123');
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'GET_GAME_ENTITY_QUERY',
          variables: { id: '123' }
        })
      );
      expect(result).toEqual(mockEntity);
    });

    it('returns null for non-existent entity', async () => {
      API.graphql.mockResolvedValue({
        data: {
          getGameEntity: null
        }
      });
      
      const result = await gameEntityService.getGameEntity('nonexistent');
      expect(result).toBeNull();
    });

    it('validates entity id', async () => {
      await expect(gameEntityService.getGameEntity(null))
        .rejects.toThrow('Entity ID is required');
    });
  });

  describe('createGameEntity', () => {
    it('creates a character entity', async () => {
      const input = {
        name: 'New Hero',
        type: 'CHARACTER',
        plotPointId: 'plot123',
        attributes: {
          agility: 'd6',
          smarts: 'd8',
          spirit: 'd6',
          strength: 'd6',
          vigor: 'd6'
        },
        skills: [],
        edges: [],
        hindrances: []
      };
      
      const mockCreated = { id: '456', ...input };
      
      API.graphql.mockResolvedValue({
        data: {
          createGameEntity: mockCreated
        }
      });
      
      const result = await gameEntityService.createCharacter(input);
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'CREATE_GAME_ENTITY_MUTATION',
          variables: { input }
        })
      );
      expect(result).toEqual(mockCreated);
    });

    it('creates a beast entity', async () => {
      const input = {
        name: 'Wolf',
        type: 'BEAST',
        plotPointId: 'plot123',
        attributes: {
          agility: 'd8',
          smarts: 'd6',
          spirit: 'd6',
          strength: 'd6',
          vigor: 'd6'
        },
        specialAbilities: ['Bite', 'Fleet-Footed']
      };
      
      API.graphql.mockResolvedValue({
        data: {
          createGameEntity: { id: '789', ...input }
        }
      });
      
      const result = await gameEntityService.createBeast(input);
      
      expect(result.type).toBe('BEAST');
      expect(result.specialAbilities).toContain('Bite');
    });

    it('validates required fields', async () => {
      const invalidInput = { type: 'CHARACTER' };
      
      await expect(gameEntityService.createCharacter(invalidInput))
        .rejects.toThrow('Name is required');
    });

    it('validates entity type', async () => {
      const input = {
        name: 'Test',
        type: 'INVALID_TYPE'
      };
      
      await expect(gameEntityService.createGameEntity(input))
        .rejects.toThrow('Invalid entity type');
    });

    it('validates attributes for characters', async () => {
      const input = {
        name: 'Hero',
        type: 'CHARACTER',
        attributes: {
          agility: 'd20' // Invalid die value
        }
      };
      
      await expect(gameEntityService.createCharacter(input))
        .rejects.toThrow('Invalid attribute value');
    });
  });

  describe('updateGameEntity', () => {
    it('updates an existing entity', async () => {
      const input = {
        id: '123',
        name: 'Updated Hero',
        attributes: {
          agility: 'd10'
        }
      };
      
      const mockUpdated = { ...input, type: 'CHARACTER' };
      
      API.graphql.mockResolvedValue({
        data: {
          updateGameEntity: mockUpdated
        }
      });
      
      const result = await gameEntityService.updateGameEntity(input);
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'UPDATE_GAME_ENTITY_MUTATION',
          variables: { input }
        })
      );
      expect(result).toEqual(mockUpdated);
    });

    it('validates entity exists before update', async () => {
      API.graphql.mockResolvedValue({
        data: {
          getGameEntity: null
        }
      });
      
      await expect(gameEntityService.updateGameEntity({ id: 'nonexistent' }))
        .rejects.toThrow('Entity not found');
    });

    it('preserves entity type on update', async () => {
      const existing = {
        id: '123',
        type: 'CHARACTER',
        name: 'Hero'
      };
      
      API.graphql
        .mockResolvedValueOnce({
          data: { getGameEntity: existing }
        })
        .mockResolvedValueOnce({
          data: { updateGameEntity: { ...existing, name: 'Updated' } }
        });
      
      const result = await gameEntityService.updateGameEntity({
        id: '123',
        name: 'Updated'
      });
      
      expect(result.type).toBe('CHARACTER');
    });
  });

  describe('deleteGameEntity', () => {
    it('deletes an entity', async () => {
      const mockDeleted = {
        id: '123',
        name: 'Deleted Entity'
      };
      
      API.graphql.mockResolvedValue({
        data: {
          deleteGameEntity: mockDeleted
        }
      });
      
      const result = await gameEntityService.deleteGameEntity('123');
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          query: 'DELETE_GAME_ENTITY_MUTATION',
          variables: {
            input: { id: '123' }
          }
        })
      );
      expect(result).toEqual(mockDeleted);
    });

    it('handles deletion of non-existent entity', async () => {
      API.graphql.mockResolvedValue({
        data: {
          deleteGameEntity: null
        }
      });
      
      const result = await gameEntityService.deleteGameEntity('nonexistent');
      expect(result).toBeNull();
    });
  });

  describe('Entity Relationships', () => {
    it('links entity to plot point', async () => {
      const entityId = '123';
      const plotPointId = 'plot456';
      
      API.graphql.mockResolvedValue({
        data: {
          updateGameEntity: {
            id: entityId,
            plotPointId: plotPointId
          }
        }
      });
      
      const result = await gameEntityService.linkToPlotPoint(entityId, plotPointId);
      
      expect(result.plotPointId).toBe(plotPointId);
    });

    it('unlinks entity from plot point', async () => {
      const entityId = '123';
      
      API.graphql.mockResolvedValue({
        data: {
          updateGameEntity: {
            id: entityId,
            plotPointId: null
          }
        }
      });
      
      const result = await gameEntityService.unlinkFromPlotPoint(entityId);
      
      expect(result.plotPointId).toBeNull();
    });

    it('gets entities by relationship', async () => {
      API.graphql.mockResolvedValue({
        data: {
          listGameEntities: {
            items: [
              { id: '1', parentId: 'parent123' },
              { id: '2', parentId: 'parent123' }
            ]
          }
        }
      });
      
      const result = await gameEntityService.getChildEntities('parent123');
      
      expect(result).toHaveLength(2);
      expect(result[0].parentId).toBe('parent123');
    });
  });

  describe('Bulk Operations', () => {
    it('creates multiple entities in batch', async () => {
      const entities = [
        { name: 'Entity1', type: 'CHARACTER' },
        { name: 'Entity2', type: 'BEAST' },
        { name: 'Entity3', type: 'ITEM' }
      ];
      
      API.graphql.mockResolvedValue({
        data: {
          batchCreateGameEntities: entities.map((e, i) => ({ ...e, id: String(i) }))
        }
      });
      
      const result = await gameEntityService.batchCreate(entities);
      
      expect(result).toHaveLength(3);
    });

    it('updates multiple entities in batch', async () => {
      const updates = [
        { id: '1', name: 'Updated1' },
        { id: '2', name: 'Updated2' }
      ];
      
      API.graphql.mockResolvedValue({
        data: {
          batchUpdateGameEntities: updates
        }
      });
      
      const result = await gameEntityService.batchUpdate(updates);
      
      expect(result).toHaveLength(2);
    });

    it('deletes multiple entities in batch', async () => {
      const ids = ['1', '2', '3'];
      
      API.graphql.mockResolvedValue({
        data: {
          batchDeleteGameEntities: { count: 3 }
        }
      });
      
      const result = await gameEntityService.batchDelete(ids);
      
      expect(result.count).toBe(3);
    });
  });

  describe('Search and Query', () => {
    it('searches entities by name', async () => {
      const searchTerm = 'Hero';
      
      API.graphql.mockResolvedValue({
        data: {
          searchGameEntities: {
            items: [
              { id: '1', name: 'Hero Knight' },
              { id: '2', name: 'Heroic Mage' }
            ]
          }
        }
      });
      
      const result = await gameEntityService.searchByName(searchTerm);
      
      expect(result).toHaveLength(2);
      expect(result[0].name).toContain('Hero');
    });

    it('queries entities with complex filters', async () => {
      const query = {
        type: 'CHARACTER',
        minLevel: 5,
        maxLevel: 10,
        tags: ['warrior', 'tank']
      };
      
      API.graphql.mockResolvedValue({
        data: {
          queryGameEntities: {
            items: []
          }
        }
      });
      
      await gameEntityService.queryEntities(query);
      
      expect(API.graphql).toHaveBeenCalledWith(
        expect.objectContaining({
          variables: expect.objectContaining(query)
        })
      );
    });
  });

  describe('Validation', () => {
    it('validates character creation rules', () => {
      const character = {
        name: 'Test',
        type: 'CHARACTER',
        attributes: {
          agility: 'd4',
          smarts: 'd4',
          spirit: 'd4',
          strength: 'd4',
          vigor: 'd4'
        }
      };
      
      const errors = gameEntityService.validateCharacter(character);
      expect(errors).toEqual([]);
    });

    it('validates beast special abilities', () => {
      const beast = {
        name: 'Wolf',
        type: 'BEAST',
        specialAbilities: ['Bite', 'InvalidAbility']
      };
      
      const errors = gameEntityService.validateBeast(beast);
      expect(errors).toContain('Invalid special ability: InvalidAbility');
    });

    it('validates item properties', () => {
      const item = {
        name: 'Sword',
        type: 'ITEM',
        weight: -5 // Invalid negative weight
      };
      
      const errors = gameEntityService.validateItem(item);
      expect(errors).toContain('Weight cannot be negative');
    });
  });
});