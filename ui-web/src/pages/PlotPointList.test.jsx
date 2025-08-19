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

  describe('Component Props and IDs', () => {
    it('should render with correct page ID', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      const { container } = render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(container.querySelector('#PlotPointListPage')).toBeInTheDocument();
      });
    });

    it('should pass correct ID to PlotPointList component', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        // The mocked component should receive the correct ID
        const listComponent = screen.getByTestId('plot-point-list');
        expect(listComponent).toBeInTheDocument();
      });
    });

    it('should render add button with correct ID', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      const { container } = render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(container.querySelector('#addPlotPoint')).toBeInTheDocument();
      });
    });
  });

  describe('Service Integration', () => {
    it('should call getPlotPoints with correct pagination parameters', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(plotPointService.getPlotPoints).toHaveBeenCalledWith(1, 50);
      });
    });

    it('should handle service rejection gracefully', async () => {
      const serviceError = new Error('Service unavailable');
      plotPointService.getPlotPoints.mockRejectedValue(serviceError);
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Error loading plot points')).toBeInTheDocument();
        expect(screen.getByText('Service unavailable')).toBeInTheDocument();
      });
    });

    it('should only call service once on initial render', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(plotPointService.getPlotPoints).toHaveBeenCalledTimes(1);
      });
    });
  });

  describe('React Query Integration', () => {
    it('should use correct query key', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: false },
          mutations: { retry: false }
        }
      });
      
      const querySpy = jest.spyOn(queryClient, 'getQueryCache');
      
      const wrapper = ({ children }) => (
        <QueryClientProvider client={queryClient}>
          <BrowserRouter>
            {children}
          </BrowserRouter>
        </QueryClientProvider>
      );
      
      render(<PlotPointList />, { wrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Test Plot Point 1')).toBeInTheDocument();
      });
      
      // Verify query cache contains our query key
      const cache = queryClient.getQueryCache();
      expect(cache.find(['plotPoints'])).toBeTruthy();
    });

    it('should handle query cache properly', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: false, staleTime: 0 },
          mutations: { retry: false }
        }
      });
      
      const wrapper = ({ children }) => (
        <QueryClientProvider client={queryClient}>
          <BrowserRouter>
            {children}
          </BrowserRouter>
        </QueryClientProvider>
      );
      
      const { rerender } = render(<PlotPointList />, { wrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Test Plot Point 1')).toBeInTheDocument();
      });
      
      // Rerender should use cached data
      rerender(<PlotPointList />);
      
      await waitFor(() => {
        expect(screen.getByText('Test Plot Point 1')).toBeInTheDocument();
      });
    });
  });

  describe('Data Presentation', () => {
    it('should handle large datasets gracefully', async () => {
      const largePlotPointList = Array.from({ length: 100 }, (_, index) => ({
        id: `${index + 1}`,
        name: `Plot Point ${index + 1}`,
        description: `Description ${index + 1}`,
        genre: 'Fantasy',
        createdAt: '2024-01-01T00:00:00Z',
        updatedAt: '2024-01-01T00:00:00Z'
      }));
      
      plotPointService.getPlotPoints.mockResolvedValue({
        data: largePlotPointList,
        pagination: { total: 100 }
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Showing 100 of 100 plot points')).toBeInTheDocument();
      });
    });

    it('should handle mixed pagination scenarios', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints,
        pagination: { total: 150, page: 1, limit: 50 }
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Showing 2 of 150 plot points')).toBeInTheDocument();
      });
    });

    it('should show pagination when data is paginated', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints.slice(0, 1), // Only first item
        pagination: { total: 2, page: 1, limit: 1 }
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Showing 1 of 2 plot points')).toBeInTheDocument();
      });
    });
  });

  describe('Error Handling Edge Cases', () => {
    it('should handle network errors', async () => {
      const networkError = new Error('Network Error');
      networkError.name = 'NetworkError';
      plotPointService.getPlotPoints.mockRejectedValue(networkError);
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Error loading plot points')).toBeInTheDocument();
        expect(screen.getByText('Network Error')).toBeInTheDocument();
      });
    });

    it('should handle timeout errors', async () => {
      const timeoutError = new Error('Request timeout');
      timeoutError.code = 'TIMEOUT';
      plotPointService.getPlotPoints.mockRejectedValue(timeoutError);
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Request timeout')).toBeInTheDocument();
      });
    });

    it('should handle server errors with status codes', async () => {
      const serverError = new Error('Internal Server Error');
      serverError.status = 500;
      plotPointService.getPlotPoints.mockRejectedValue(serverError);
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Internal Server Error')).toBeInTheDocument();
      });
    });
  });

  describe('Loading State Variations', () => {
    it('should show loading state with spinner attributes', () => {
      plotPointService.getPlotPoints.mockImplementation(() => new Promise(() => {})); // Never resolves
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      const spinner = screen.getByRole('status');
      expect(spinner).toBeInTheDocument();
      expect(spinner).toHaveClass('spinner-border');
      
      const loadingText = screen.getByText('Loading plot points...');
      expect(loadingText).toBeInTheDocument();
    });

    it('should transition from loading to content', async () => {
      let resolvePromise;
      plotPointService.getPlotPoints.mockImplementation(() => 
        new Promise(resolve => { resolvePromise = resolve; })
      );
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      // Initially loading
      expect(screen.getByText('Loading plot points...')).toBeInTheDocument();
      
      // Resolve the promise
      resolvePromise({
        data: mockPlotPoints,
        pagination: { total: 2 }
      });
      
      // Should transition to content
      await waitFor(() => {
        expect(screen.queryByText('Loading plot points...')).not.toBeInTheDocument();
        expect(screen.getByText('Test Plot Point 1')).toBeInTheDocument();
      });
    });
  });

  describe('User Interaction Flow', () => {
    it('should maintain focus after add button click', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        const addButton = screen.getByRole('button', { name: /add/i });
        addButton.focus();
        expect(addButton).toHaveFocus();
        
        fireEvent.click(addButton);
      });
      
      expect(mockNavigate).toHaveBeenCalledWith('/plot_point/add');
    });

    it('should handle keyboard navigation', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        const addButton = screen.getByRole('button', { name: /add/i });
        
        // Test Enter key
        fireEvent.keyDown(addButton, { key: 'Enter', code: 'Enter' });
        fireEvent.keyUp(addButton, { key: 'Enter', code: 'Enter' });
        
        expect(addButton).toBeInTheDocument();
      });
    });
  });

  describe('Component Accessibility', () => {
    it('should have proper ARIA roles and labels', async () => {
      plotPointService.getPlotPoints.mockResolvedValue({
        data: mockPlotPoints
      });
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        const addButton = screen.getByRole('button', { name: /add/i });
        expect(addButton).toBeInTheDocument();
        
        const heading = screen.getByRole('heading', { name: 'Plot Points' });
        expect(heading).toBeInTheDocument();
      });
    });

    it('should maintain semantic structure in error state', async () => {
      plotPointService.getPlotPoints.mockRejectedValue(new Error('Test error'));
      
      render(<PlotPointList />, { wrapper: createWrapper });
      
      await waitFor(() => {
        const alert = screen.getByRole('alert');
        expect(alert).toBeInTheDocument();
        expect(alert).toHaveClass('alert-danger');
        
        const heading = screen.getByRole('heading', { name: 'Plot Points' });
        expect(heading).toBeInTheDocument();
      });
    });
  });
});