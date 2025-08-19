import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import PlotPointEdit from './PlotPointEdit';

// Mock the service
jest.mock('../services', () => ({
  plotPointService: {
    getPlotPointByName: jest.fn(),
    updatePlotPoint: jest.fn()
  }
}));

// Mock react-router-dom hooks
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: jest.fn(),
  useParams: jest.fn()
}));

// After mocks are set up, we can access them
const { useNavigate, useParams } = jest.requireMock('react-router-dom');
const mockNavigate = jest.fn();

// Mock PlotPointForm component
jest.mock('../components/plotpoint/editor', () => {
  const React = require('react');
  return function MockPlotPointForm({ onSave, onCancel, errors, disabled, plotPoint }) {
    return React.createElement('div', { 'data-testid': 'plot-point-form' },
      errors && errors.length > 0 && React.createElement('div', { 'data-testid': 'form-errors' },
        errors.map((err, idx) => React.createElement('div', { key: idx }, err))
      ),
      React.createElement('div', {}, 'Editing: ' + (plotPoint ? plotPoint.name : '')),
      React.createElement('button', { 
        onClick: () => onSave({ name: 'Updated Plot Point' }), 
        disabled: disabled 
      }, 'Save'),
      React.createElement('button', { onClick: onCancel }, 'Cancel')
    );
  };
});

const { plotPointService } = require('../services');

const createWrapper = ({ children }) => {
  const queryClient = new QueryClient({
    defaultOptions: {
      queries: { retry: false },
      mutations: { retry: false }
    }
  });
  return React.createElement(
    QueryClientProvider,
    { client: queryClient },
    React.createElement(BrowserRouter, {}, children)
  );
};

describe('PlotPointEdit Component', () => {
  const mockPlotPoint = {
    id: '123',
    name: 'Existing Plot Point',
    description: 'Test description',
    basicRules: {
      maximumAttributePoints: 5,
      maximumMajorHindrances: 1,
      maximumMinorHindrances: 2,
      maximumSkillPoints: 15
    },
    beasts: [],
    edges: []
  };

  beforeEach(() => {
    jest.clearAllMocks();
    mockNavigate.mockClear();
    useNavigate.mockReturnValue(mockNavigate);
    useParams.mockReturnValue({ name: 'test-plot-point' });
  });

  describe('Rendering', () => {
    it('renders without crashing', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Edit Plot Point')).toBeInTheDocument();
      });
    });

    it('displays the page title', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Edit Plot Point')).toBeInTheDocument();
      });
    });

    it('renders the plot point form with data', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByTestId('plot-point-form')).toBeInTheDocument();
        expect(screen.getByText('Editing: Existing Plot Point')).toBeInTheDocument();
      });
    });

    it('displays save and cancel buttons', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByRole('button', { name: /save/i })).toBeInTheDocument();
        expect(screen.getByRole('button', { name: /cancel/i })).toBeInTheDocument();
      });
    });

    it('uses custom id when provided', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      
      const { container } = render(React.createElement(PlotPointEdit, { id: "custom-id" }), { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(container.querySelector('#custom-id')).toBeInTheDocument();
      });
    });
  });

  describe('Loading State', () => {
    it('displays loading indicator while fetching data', async () => {
      plotPointService.getPlotPointByName.mockImplementation(() => new Promise(() => {})); // Never resolves
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      expect(screen.getByText('Loading plot point...')).toBeInTheDocument();
      expect(screen.getByRole('status')).toBeInTheDocument();
    });

    it('displays title during loading', () => {
      plotPointService.getPlotPointByName.mockImplementation(() => new Promise(() => {}));
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      expect(screen.getByText('Edit Plot Point')).toBeInTheDocument();
    });
  });

  describe('Error State', () => {
    it('displays error message when fetch fails', async () => {
      const error = new Error('Failed to fetch plot point');
      plotPointService.getPlotPointByName.mockRejectedValue(error);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Error loading plot point')).toBeInTheDocument();
        expect(screen.getByText('Failed to fetch plot point')).toBeInTheDocument();
      });
    });

    it('displays default error message when error has no message', async () => {
      plotPointService.getPlotPointByName.mockRejectedValue({});
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByText('Error loading plot point')).toBeInTheDocument();
        expect(screen.getByText('Plot point not found')).toBeInTheDocument();
      });
    });

    it('provides back button on error', async () => {
      plotPointService.getPlotPointByName.mockRejectedValue(new Error());
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const backButton = screen.getByRole('button', { name: /back to list/i });
        expect(backButton).toBeInTheDocument();
        
        fireEvent.click(backButton);
        expect(mockNavigate).toHaveBeenCalledWith('/');
      });
    });
  });

  describe('Form Submission', () => {
    it('calls updatePlotPoint service when form is saved', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      plotPointService.updatePlotPoint.mockResolvedValue({ ...mockPlotPoint, name: 'Updated' });
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(plotPointService.updatePlotPoint).toHaveBeenCalledWith('123', { name: 'Updated Plot Point' });
      });
    });

    it('navigates to home page on successful save', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      plotPointService.updatePlotPoint.mockResolvedValue({ ...mockPlotPoint, name: 'Updated' });
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(mockNavigate).toHaveBeenCalledWith('/');
      });
    });

    it('shows loading state during update', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      plotPointService.updatePlotPoint.mockImplementation(() => 
        new Promise(resolve => setTimeout(resolve, 100))
      );
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      expect(screen.getByText('Updating plot point...')).toBeInTheDocument();
      
      await waitFor(() => {
        expect(screen.queryByText('Updating plot point...')).not.toBeInTheDocument();
      });
    });

    it('disables form during submission', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      plotPointService.updatePlotPoint.mockImplementation(() => 
        new Promise(resolve => setTimeout(resolve, 100))
      );
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
        expect(saveButton).toBeDisabled();
      });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        expect(saveButton).not.toBeDisabled();
      });
    });
  });

  describe('Error Handling', () => {
    it('displays error message on update failure', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      const error = { message: 'Failed to update plot point' };
      plotPointService.updatePlotPoint.mockRejectedValue(error);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(screen.getByText('Failed to update plot point')).toBeInTheDocument();
      });
    });

    it('displays multiple error messages when errors array is provided', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      const error = { 
        errors: ['Name is required', 'Description is too short'] 
      };
      plotPointService.updatePlotPoint.mockRejectedValue(error);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(screen.getByText('Name is required')).toBeInTheDocument();
        expect(screen.getByText('Description is too short')).toBeInTheDocument();
      });
    });

    it('shows error when plot point has no ID', async () => {
      const plotPointWithoutId = { ...mockPlotPoint, id: undefined };
      plotPointService.getPlotPointByName.mockResolvedValue(plotPointWithoutId);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(screen.getByText('Plot point ID not found')).toBeInTheDocument();
      });
      
      expect(plotPointService.updatePlotPoint).not.toHaveBeenCalled();
    });

    it('clears errors when submitting again', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      const error = { message: 'First error' };
      plotPointService.updatePlotPoint
        .mockRejectedValueOnce(error)
        .mockResolvedValueOnce({ ...mockPlotPoint, name: 'Updated' });
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(screen.getByText('First error')).toBeInTheDocument();
      });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.queryByText('First error')).not.toBeInTheDocument();
      });
    });

    it('does not navigate on error', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      const error = { message: 'Failed to update' };
      plotPointService.updatePlotPoint.mockRejectedValue(error);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(screen.getByText('Failed to update')).toBeInTheDocument();
      });
      
      expect(mockNavigate).not.toHaveBeenCalled();
    });
  });

  describe('Navigation', () => {
    it('navigates to home when cancel is clicked', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const cancelButton = screen.getByRole('button', { name: /cancel/i });
        fireEvent.click(cancelButton);
      });
      
      expect(mockNavigate).toHaveBeenCalledWith('/');
    });

    it('does not save when cancel is clicked', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const cancelButton = screen.getByRole('button', { name: /cancel/i });
        fireEvent.click(cancelButton);
      });
      
      expect(plotPointService.updatePlotPoint).not.toHaveBeenCalled();
    });
  });

  describe('Data Fetching', () => {
    it('fetches plot point by name from URL params', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(plotPointService.getPlotPointByName).toHaveBeenCalledWith('test-plot-point');
      });
    });

    it('does not fetch when name param is missing', async () => {
      // Temporarily override the mock params
      jest.spyOn(require('react-router-dom'), 'useParams').mockReturnValue({});
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      expect(plotPointService.getPlotPointByName).not.toHaveBeenCalled();
    });
  });

  describe('Query Cache', () => {
    it('invalidates queries on successful update', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      plotPointService.updatePlotPoint.mockResolvedValue({ ...mockPlotPoint, name: 'Updated' });
      
      const queryClient = new QueryClient({
        defaultOptions: {
          queries: { retry: false },
          mutations: { retry: false }
        }
      });
      
      const invalidateSpy = jest.spyOn(queryClient, 'invalidateQueries');
      
      const wrapper = ({ children }) => React.createElement(
        QueryClientProvider,
        { client: queryClient },
        React.createElement(BrowserRouter, {}, children)
      );
      
      render(React.createElement(PlotPointEdit), { wrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(invalidateSpy).toHaveBeenCalledWith(['plotPoint', 'test-plot-point']);
        expect(invalidateSpy).toHaveBeenCalledWith(['plotPoints']);
      });
    });
  });

  describe('Edge Cases', () => {
    it('handles plot point with default structure when data is incomplete', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(null);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByTestId('plot-point-form')).toBeInTheDocument();
      });
    });

    it('handles array of error strings', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      const errorArray = ['Error 1', 'Error 2'];
      plotPointService.updatePlotPoint.mockRejectedValue({ errors: errorArray });
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(screen.getByText('Error 1')).toBeInTheDocument();
        expect(screen.getByText('Error 2')).toBeInTheDocument();
      });
    });

    it('handles single error string in errors property', async () => {
      plotPointService.getPlotPointByName.mockResolvedValue(mockPlotPoint);
      const error = { errors: 'Single error message' };
      plotPointService.updatePlotPoint.mockRejectedValue(error);
      
      render(React.createElement(PlotPointEdit), { wrapper: createWrapper });
      
      await waitFor(() => {
        const saveButton = screen.getByRole('button', { name: /save/i });
        fireEvent.click(saveButton);
      });
      
      await waitFor(() => {
        expect(screen.getByText('Single error message')).toBeInTheDocument();
      });
    });
  });
});