import { useQuery, useMutation, useQueryClient } from 'react-query';
import { gameEntityService } from '../services';

// Generic hook for fetching game entities
export const useGameEntities = (type, page = 1, limit = 20, filters = {}) => {
  return useQuery(
    ['gameEntities', type, page, limit, filters],
    () => gameEntityService.getGameEntities(type, page, limit, filters),
    { keepPreviousData: true }
  );
};

// Generic hook for fetching a single game entity
export const useGameEntity = (type, id) => {
  return useQuery(
    ['gameEntity', type, id],
    () => gameEntityService.getGameEntity(type, id),
    { enabled: !!id && !!type }
  );
};

// Generic hook for creating a game entity
export const useCreateGameEntity = (type) => {
  const queryClient = useQueryClient();
  
  return useMutation(
    (data) => gameEntityService.createGameEntity(type, data),
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['gameEntities', type]);
      },
    }
  );
};

// Generic hook for updating a game entity
export const useUpdateGameEntity = (type) => {
  const queryClient = useQueryClient();
  
  return useMutation(
    ({ id, data }) => gameEntityService.updateGameEntity(type, id, data),
    {
      onSuccess: (_, { id }) => {
        queryClient.invalidateQueries(['gameEntity', type, id]);
        queryClient.invalidateQueries(['gameEntities', type]);
      },
    }
  );
};

// Generic hook for deleting a game entity
export const useDeleteGameEntity = (type) => {
  const queryClient = useQueryClient();
  
  return useMutation(
    (id) => gameEntityService.deleteGameEntity(type, id),
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['gameEntities', type]);
      },
    }
  );
};

// Specific hooks for common entity types
export const useCharacters = (page, limit, filters) => 
  useGameEntities('characters', page, limit, filters);

export const useCharacter = (id) => 
  useGameEntity('characters', id);

export const useBeasts = (page, limit, filters) => 
  useGameEntities('beasts', page, limit, filters);

export const useBeast = (id) => 
  useGameEntity('beasts', id);

export const useEdges = (page, limit, filters) => 
  useGameEntities('edges', page, limit, filters);

export const useHindrances = (page, limit, filters) => 
  useGameEntities('hindrances', page, limit, filters);

export const useSkills = (page, limit, filters) => 
  useGameEntities('skills', page, limit, filters);

export const usePowers = (page, limit, filters) => 
  useGameEntities('powers', page, limit, filters);

export const useRaces = (page, limit, filters) => 
  useGameEntities('races', page, limit, filters);

export const useArcaneBackgrounds = (page, limit, filters) => 
  useGameEntities('arcane-backgrounds', page, limit, filters);

export const useSettingRules = (page, limit, filters) => 
  useGameEntities('setting-rules', page, limit, filters);

// Vehicle hooks
export const useVehicles = (vehicleType, page, limit, filters) => 
  useGameEntities(`vehicles/${vehicleType}`, page, limit, filters);

export const useVehicle = (vehicleType, id) => 
  useGameEntity(`vehicles/${vehicleType}`, id);

// Gear hooks
export const useGear = (gearType, page, limit, filters) => 
  useGameEntities(`gear/${gearType}`, page, limit, filters);

export const useGearItem = (gearType, id) => 
  useGameEntity(`gear/${gearType}`, id);