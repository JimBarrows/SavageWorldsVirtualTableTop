import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import PlotPointList from './PlotPointList';

// Mock the navigate hook
const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate
}));

// Mock the PlotPointList component
jest.mock('../components/plotpoint/list/index', () => {
  return function MockPlotPointList({ plotPoints }) {
    return (
      <div data-testid="plot-point-list">
        {plotPoints.map(pp => (
          <div key={pp.id}>{pp.name}</div>
        ))}
      </div>
    );
  };
});

// Mock the service
jest.mock('../services', () => ({
  plotPointService: {
    getPlotPoints: jest.fn()
  }
}));

// Mock FontAwesome
jest.mock('@fortawesome/react-fontawesome', () => ({
  FontAwesomeIcon: ({ icon }) => <span>{icon}</span>
}));

const { plotPointService } = require('../services');

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
    mockNavigate.mockClear();
  });

  describe('Rendering', () => {
    it('renders without crashing', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints,
        pagination: { total: 2 }
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Plot Points')).toBeInTheDocument();
      });
    });

    it('displays the page title', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Plot Points')).toBeInTheDocument();
      });
    });

    it('displays add new plot point button', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        const addButton = screen.getByRole('button', { name: /add/i });
        expect(addButton).toBeInTheDocument();
      });
    });

    it('renders list of plot points', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Test Plot Point 1')).toBeInTheDocument();
        expect(screen.getByText('Test Plot Point 2')).toBeInTheDocument();
      });
    });

    it('displays pagination info when provided', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints,
        pagination: { total: 10 }
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Showing 2 of 10 plot points')).toBeInTheDocument();
      });
    });
  });

  describe('Loading State', () => {
    it('displays loading indicator when data is loading', async () => {
      plotPointService.getPlotPoints.mockImplementation(() => new Promise(() => {})); // Never resolves
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      expect(screen.getByText('Loading plot points...')).toBeInTheDocument();
      expect(screen.getByRole('status')).toBeInTheDocument();
    });
  });

  describe('Error State', () => {
    it('displays error message when fetch fails', async () => {
      const error = new Error('Failed to fetch plot points');
      plotPointService.getPlotPoints.mockRejectedValue(error);
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Error loading plot points')).toBeInTheDocument();
        expect(screen.getByText('Failed to fetch plot points')).toBeInTheDocument();
      });
    });

    it('displays generic error message when error has no message', async () => {
      plotPointService.getPlotPoints.mockRejectedValue(new Error());
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Error loading plot points')).toBeInTheDocument();
        expect(screen.getByText('An unexpected error occurred')).toBeInTheDocument();
      });
    });
  });

  describe('Empty State', () => {
    it('displays empty message when no plot points', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: []
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('There are no plot points, please add one')).toBeInTheDocument();
      });
    });

    it('still shows add button when list is empty', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: []
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        const addButton = screen.getByRole('button', { name: /add/i });
        expect(addButton).toBeInTheDocument();
      });
    });
  });

  describe('Navigation', () => {
    it('navigates to add page when add button clicked', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        const addButton = screen.getByRole('button', { name: /add/i });
        fireEvent.click(addButton);
      });
      
      expect(mockNavigate).toHaveBeenCalledWith('/plot_point/add');
    });
  });

  describe('Data Fetching', () => {
    it('fetches plot points on mount', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(plotPointService.getPlotPoints).toHaveBeenCalledWith(1, 50);
      });
    });

    it('passes correct props to PlotPointList component', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        const listComponent = screen.getByTestId('plot-point-list');
        expect(listComponent).toBeInTheDocument();
      });
    });
  });

  describe('Edge Cases', () => {
    it('handles null data gracefully', async () => {
      plotPointService.getPlotPoints.mockResolvedValue(null);
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('There are no plot points, please add one')).toBeInTheDocument();
      });
    });

    it('handles undefined data gracefully', async () => {
      plotPointService.getPlotPoints.mockResolvedValue(undefined);
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('There are no plot points, please add one')).toBeInTheDocument();
      });
    });

    it('handles missing pagination data', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
        // No pagination property
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Test Plot Point 1')).toBeInTheDocument();
        // Should not show pagination text
        expect(screen.queryByText(/Showing.*of.*plot points/)).not.toBeInTheDocument();
      });
    });
  });
});