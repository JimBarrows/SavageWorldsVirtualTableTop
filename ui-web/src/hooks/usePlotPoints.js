import { useQuery, useMutation, useQueryClient } from 'react-query';
import { plotPointService } from '../services';

// Hook for fetching plot points list
export const usePlotPoints = (page = 1, limit = 20, filters = {}) => {
  return useQuery(
    ['plotPoints', page, limit, filters],
    () => plotPointService.getPlotPoints(page, limit, filters),
    { keepPreviousData: true }
  );
};

// Hook for fetching a single plot point
export const usePlotPoint = (id) => {
  return useQuery(
    ['plotPoint', id],
    () => plotPointService.getPlotPoint(id),
    { enabled: !!id }
  );
};

// Hook for fetching plot point by name
export const usePlotPointByName = (name) => {
  return useQuery(
    ['plotPoint', 'byName', name],
    () => plotPointService.getPlotPointByName(name),
    { enabled: !!name }
  );
};

// Hook for creating a plot point
export const useCreatePlotPoint = () => {
  const queryClient = useQueryClient();
  
  return useMutation(
    plotPointService.createPlotPoint,
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['plotPoints']);
      },
    }
  );
};

// Hook for updating a plot point
export const useUpdatePlotPoint = () => {
  const queryClient = useQueryClient();
  
  return useMutation(
    ({ id, data }) => plotPointService.updatePlotPoint(id, data),
    {
      onSuccess: (_, { id }) => {
        queryClient.invalidateQueries(['plotPoint', id]);
        queryClient.invalidateQueries(['plotPoints']);
      },
    }
  );
};

// Hook for deleting a plot point
export const useDeletePlotPoint = () => {
  const queryClient = useQueryClient();
  
  return useMutation(
    plotPointService.deletePlotPoint,
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['plotPoints']);
      },
    }
  );
};

// Hook for searching plot points
export const useSearchPlotPoints = (query, page = 1, limit = 20) => {
  return useQuery(
    ['plotPoints', 'search', query, page, limit],
    () => plotPointService.searchPlotPoints(query, page, limit),
    { enabled: !!query, keepPreviousData: true }
  );
};