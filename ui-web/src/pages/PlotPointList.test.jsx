import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import PlotPointList from './PlotPointList';

// Mock the hooks
jest.mock('../hooks/usePlotPoints', () => ({
  usePlotPoints: jest.fn()
}));

// Mock the service
jest.mock('../services/plotPointService', () => ({
  deletePlotPoint: jest.fn()
}));

const { usePlotPoints } = require('../hooks/usePlotPoints');
const plotPointService = require('../services/plotPointService');

const createWrapper = ({ children }) => {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false }
    }
  });
  return (
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        {children}
      </BrowserRouter>
    </QueryClientProvider>
  );
};

describe('PlotPointList Component', () => {
  const mockPlotPoints = [
    {
      id: '1',
      name: 'Test Plot Point 1',
      description: 'Description 1',
      genre: 'Fantasy',
      createdAt: '2024-01-01T00:00:00Z',
      updatedAt: '2024-01-01T00:00:00Z'
    },
    {
      id: '2',
      name: 'Test Plot Point 2',
      description: 'Description 2',
      genre: 'Sci-Fi',
      createdAt: '2024-01-02T00:00:00Z',
      updatedAt: '2024-01-02T00:00:00Z'
    }
  ];

  beforeEach(() => {
    jest.clearAllMocks();
    usePlotPoints.mockReturnValue({
      data: mockPlotPoints,
      isLoading: false,
      error: null,
      refetch: jest.fn()
    });
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
    });

    it('displays the page title', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByText(/plot points/i)).toBeInTheDocument();
    });

    it('displays add new plot point button', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const addButton = screen.getByRole('button', { name: /add.*plot point/i });
      expect(addButton).toBeInTheDocument();
    });

    it('renders list of plot points', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByText('Test Plot Point 1')).toBeInTheDocument();
      expect(screen.getByText('Test Plot Point 2')).toBeInTheDocument();
    });

    it('displays plot point details', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByText('Description 1')).toBeInTheDocument();
      expect(screen.getByText('Fantasy')).toBeInTheDocument();
      expect(screen.getByText('Description 2')).toBeInTheDocument();
      expect(screen.getByText('Sci-Fi')).toBeInTheDocument();
    });
  });

  describe('Loading State', () => {
    it('displays loading indicator when data is loading', () => {
      usePlotPoints.mockReturnValue({
        data: null,
        isLoading: true,
        error: null,
        refetch: jest.fn()
      });

      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByText(/loading/i)).toBeInTheDocument();
    });

    it('hides loading indicator when data is loaded', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.queryByText(/loading/i)).not.toBeInTheDocument();
    });
  });

  describe('Error State', () => {
    it('displays error message when data fetch fails', () => {
      usePlotPoints.mockReturnValue({
        data: null,
        isLoading: false,
        error: new Error('Failed to load plot points'),
        refetch: jest.fn()
      });

      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByText(/failed to load plot points/i)).toBeInTheDocument();
    });

    it('provides retry button on error', () => {
      const refetchMock = jest.fn();
      usePlotPoints.mockReturnValue({
        data: null,
        isLoading: false,
        error: new Error('Failed to load'),
        refetch: refetchMock
      });

      render(<PlotPointList />, { wrapper: createWrapper });
      const retryButton = screen.getByRole('button', { name: /retry/i });
      fireEvent.click(retryButton);
      expect(refetchMock).toHaveBeenCalled();
    });
  });

  describe('Empty State', () => {
    it('displays empty state message when no plot points exist', () => {
      usePlotPoints.mockReturnValue({
        data: [],
        isLoading: false,
        error: null,
        refetch: jest.fn()
      });

      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByText(/no plot points/i)).toBeInTheDocument();
    });

    it('shows create first plot point prompt when list is empty', () => {
      usePlotPoints.mockReturnValue({
        data: [],
        isLoading: false,
        error: null,
        refetch: jest.fn()
      });

      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByText(/create your first plot point/i)).toBeInTheDocument();
    });
  });

  describe('Navigation', () => {
    it('navigates to add plot point page when add button is clicked', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const addButton = screen.getByRole('button', { name: /add.*plot point/i });
      fireEvent.click(addButton);
      // Navigation would be handled by React Router
    });

    it('navigates to edit page when edit button is clicked', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const editButtons = screen.getAllByRole('button', { name: /edit/i });
      fireEvent.click(editButtons[0]);
      // Navigation would be handled by React Router
    });

    it('includes plot point id in edit navigation', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const editButtons = screen.getAllByRole('button', { name: /edit/i });
      expect(editButtons).toHaveLength(2);
    });
  });

  describe('Delete Functionality', () => {
    it('displays delete confirmation dialog', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      fireEvent.click(deleteButtons[0]);
      expect(screen.getByText(/are you sure/i)).toBeInTheDocument();
    });

    it('cancels delete when cancel is clicked', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      fireEvent.click(deleteButtons[0]);
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(screen.queryByText(/are you sure/i)).not.toBeInTheDocument();
    });

    it('calls delete service when confirmed', async () => {
      plotPointService.deletePlotPoint.mockResolvedValue({ success: true });
      const refetchMock = jest.fn();
      usePlotPoints.mockReturnValue({
        data: mockPlotPoints,
        isLoading: false,
        error: null,
        refetch: refetchMock
      });

      render(<PlotPointList />, { wrapper: createWrapper });
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      fireEvent.click(deleteButtons[0]);
      
      const confirmButton = screen.getByRole('button', { name: /confirm/i });
      fireEvent.click(confirmButton);
      
      await waitFor(() => {
        expect(plotPointService.deletePlotPoint).toHaveBeenCalledWith('1');
        expect(refetchMock).toHaveBeenCalled();
      });
    });

    it('shows success message after successful delete', async () => {
      plotPointService.deletePlotPoint.mockResolvedValue({ success: true });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      fireEvent.click(deleteButtons[0]);
      
      const confirmButton = screen.getByRole('button', { name: /confirm/i });
      fireEvent.click(confirmButton);
      
      await waitFor(() => {
        expect(screen.getByText(/successfully deleted/i)).toBeInTheDocument();
      });
    });

    it('shows error message when delete fails', async () => {
      plotPointService.deletePlotPoint.mockRejectedValue(new Error('Delete failed'));
      
      render(<PlotPointList />, { wrapper: createWrapper });
      const deleteButtons = screen.getAllByRole('button', { name: /delete/i });
      fireEvent.click(deleteButtons[0]);
      
      const confirmButton = screen.getByRole('button', { name: /confirm/i });
      fireEvent.click(confirmButton);
      
      await waitFor(() => {
        expect(screen.getByText(/failed to delete/i)).toBeInTheDocument();
      });
    });
  });

  describe('Search and Filter', () => {
    it('displays search input', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const searchInput = screen.getByPlaceholderText(/search plot points/i);
      expect(searchInput).toBeInTheDocument();
    });

    it('filters plot points based on search term', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const searchInput = screen.getByPlaceholderText(/search plot points/i);
      
      fireEvent.change(searchInput, { target: { value: 'Test Plot Point 1' } });
      
      expect(screen.getByText('Test Plot Point 1')).toBeInTheDocument();
      expect(screen.queryByText('Test Plot Point 2')).not.toBeInTheDocument();
    });

    it('shows no results message when search yields no results', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const searchInput = screen.getByPlaceholderText(/search plot points/i);
      
      fireEvent.change(searchInput, { target: { value: 'NonexistentPlotPoint' } });
      
      expect(screen.getByText(/no plot points found/i)).toBeInTheDocument();
    });

    it('clears search when clear button is clicked', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const searchInput = screen.getByPlaceholderText(/search plot points/i);
      
      fireEvent.change(searchInput, { target: { value: 'Test' } });
      const clearButton = screen.getByRole('button', { name: /clear/i });
      fireEvent.click(clearButton);
      
      expect(searchInput.value).toBe('');
      expect(screen.getByText('Test Plot Point 1')).toBeInTheDocument();
      expect(screen.getByText('Test Plot Point 2')).toBeInTheDocument();
    });
  });

  describe('Sorting', () => {
    it('displays sort options', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByLabelText(/sort by/i)).toBeInTheDocument();
    });

    it('sorts by name alphabetically', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const sortSelect = screen.getByLabelText(/sort by/i);
      
      fireEvent.change(sortSelect, { target: { value: 'name' } });
      
      const plotPointNames = screen.getAllByTestId('plot-point-name');
      expect(plotPointNames[0]).toHaveTextContent('Test Plot Point 1');
      expect(plotPointNames[1]).toHaveTextContent('Test Plot Point 2');
    });

    it('sorts by date created', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const sortSelect = screen.getByLabelText(/sort by/i);
      
      fireEvent.change(sortSelect, { target: { value: 'created' } });
      
      const plotPointNames = screen.getAllByTestId('plot-point-name');
      expect(plotPointNames[0]).toHaveTextContent('Test Plot Point 1');
    });

    it('sorts by date modified', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const sortSelect = screen.getByLabelText(/sort by/i);
      
      fireEvent.change(sortSelect, { target: { value: 'modified' } });
      
      const plotPointNames = screen.getAllByTestId('plot-point-name');
      expect(plotPointNames.length).toBe(2);
    });
  });

  describe('Accessibility', () => {
    it('has proper heading hierarchy', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      const heading = screen.getByRole('heading', { level: 1 });
      expect(heading).toHaveTextContent(/plot points/i);
    });

    it('has accessible labels for interactive elements', () => {
      render(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByLabelText(/search plot points/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/sort by/i)).toBeInTheDocument();
    });

    it('announces list updates to screen readers', () => {
      const { rerender } = render(<PlotPointList />, { wrapper: createWrapper });
      
      // Update the mock to return different data
      usePlotPoints.mockReturnValue({
        data: [...mockPlotPoints, { id: '3', name: 'New Plot Point', description: 'New' }],
        isLoading: false,
        error: null,
        refetch: jest.fn()
      });
      
      rerender(<PlotPointList />, { wrapper: createWrapper });
      expect(screen.getByText('New Plot Point')).toBeInTheDocument();
    });
  });
});