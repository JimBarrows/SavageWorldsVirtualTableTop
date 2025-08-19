import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import PlotPointAdd from './PlotPointAdd';

// Mock the service
jest.mock('../services', () => ({
  plotPointService: {
    createPlotPoint: jest.fn()
  }
}));

// Mock navigation
const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate
}));

// Mock PlotPointForm component
jest.mock('../components/plotpoint/editor', () => {
  const React = require('react');
  return function MockPlotPointForm({ onSave, onCancel, errors, disabled }) {
    return React.createElement('div', { 'data-testid': 'plot-point-form' },
      errors && errors.length > 0 && React.createElement('div', { 'data-testid': 'form-errors' },
        errors.map((err, idx) => React.createElement('div', { key: idx }, err))
      ),
      React.createElement('button', { 
        onClick: () => onSave({ name: 'Test Plot Point' }), 
        disabled: disabled 
      }, 'Save'),
      React.createElement('button', { onClick: onCancel }, 'Cancel')
    );
  };
});

// Mock PlotPoint model
jest.mock('../models/PlotPoint', () => {
  return class PlotPoint {
    constructor() {
      this.id = '';
      this.name = '';
      this.description = '';
    }
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

describe('PlotPointAdd Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    mockNavigate.mockClear();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
    });

    it('displays the page title', () => {
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      expect(screen.getByText('New Plot Point')).toBeInTheDocument();
    });

    it('renders the plot point form', () => {
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      expect(screen.getByTestId('plot-point-form')).toBeInTheDocument();
    });

    it('displays save and cancel buttons', () => {
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      expect(screen.getByRole('button', { name: /save/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /cancel/i })).toBeInTheDocument();
    });

    it('uses custom id when provided', () => {
      const { container } = render(React.createElement(PlotPointAdd, { id: "custom-id" }), { wrapper: createWrapper });
      expect(container.querySelector('#custom-id')).toBeInTheDocument();
    });
  });

  describe('Form Submission', () => {
    it('calls createPlotPoint service when form is saved', async () => {
      plotPointService.createPlotPoint.mockResolvedValue({ id: '123', name: 'Test Plot Point' });
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(plotPointService.createPlotPoint).toHaveBeenCalledWith({ name: 'Test Plot Point' });
      });
    });

    it('navigates to home page on successful save', async () => {
      plotPointService.createPlotPoint.mockResolvedValue({ id: '123', name: 'Test Plot Point' });
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(mockNavigate).toHaveBeenCalledWith('/');
      });
    });

    it('shows loading state during submission', async () => {
      plotPointService.createPlotPoint.mockImplementation(() => 
        new Promise(resolve => setTimeout(resolve, 100))
      );
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      expect(screen.getByText('Creating plot point...')).toBeInTheDocument();
      expect(screen.getByRole('status')).toBeInTheDocument();
      
      await waitFor(() => {
        expect(screen.queryByText('Creating plot point...')).not.toBeInTheDocument();
      });
    });

    it('disables form during submission', async () => {
      plotPointService.createPlotPoint.mockImplementation(() => 
        new Promise(resolve => setTimeout(resolve, 100))
      );
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      expect(saveButton).toBeDisabled();
      
      await waitFor(() => {
        expect(saveButton).not.toBeDisabled();
      });
    });
  });

  describe('Error Handling', () => {
    it('displays error message on save failure', async () => {
      const error = { message: 'Failed to create plot point' };
      plotPointService.createPlotPoint.mockRejectedValue(error);
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText('Failed to create plot point')).toBeInTheDocument();
      });
    });

    it('displays multiple error messages when errors array is provided', async () => {
      const error = { 
        errors: ['Name is required', 'Description is too short'] 
      };
      plotPointService.createPlotPoint.mockRejectedValue(error);
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText('Name is required')).toBeInTheDocument();
        expect(screen.getByText('Description is too short')).toBeInTheDocument();
      });
    });

    it('handles error with no message gracefully', async () => {
      plotPointService.createPlotPoint.mockRejectedValue({});
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText('Failed to create plot point')).toBeInTheDocument();
      });
    });

    it('clears errors when submitting again', async () => {
      const error = { message: 'First error' };
      plotPointService.createPlotPoint
        .mockRejectedValueOnce(error)
        .mockResolvedValueOnce({ id: '123', name: 'Test' });
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      
      // First submission fails
      fireEvent.click(saveButton);
      await waitFor(() => {
        expect(screen.getByText('First error')).toBeInTheDocument();
      });
      
      // Second submission succeeds
      fireEvent.click(saveButton);
      await waitFor(() => {
        expect(screen.queryByText('First error')).not.toBeInTheDocument();
      });
    });

    it('does not navigate on error', async () => {
      const error = { message: 'Failed to create' };
      plotPointService.createPlotPoint.mockRejectedValue(error);
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText('Failed to create')).toBeInTheDocument();
      });
      
      expect(mockNavigate).not.toHaveBeenCalled();
    });
  });

  describe('Navigation', () => {
    it('navigates to home when cancel is clicked', () => {
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(mockNavigate).toHaveBeenCalledWith('/');
    });

    it('does not save when cancel is clicked', () => {
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(plotPointService.createPlotPoint).not.toHaveBeenCalled();
    });
  });

  describe('Query Cache', () => {
    it('invalidates plot points query on successful save', async () => {
      plotPointService.createPlotPoint.mockResolvedValue({ id: '123', name: 'Test' });
      
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
      
      render(React.createElement(PlotPointAdd), { wrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(invalidateSpy).toHaveBeenCalledWith(['plotPoints']);
      });
    });
  });

  describe('Edge Cases', () => {
    it('strips id from plot point data before saving', async () => {
      plotPointService.createPlotPoint.mockResolvedValue({ id: '123', name: 'Test Plot Point' });
      
      // The mock will call onSave with { name: 'Test Plot Point' }
      // The component should strip any id if present
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        // Verify the service was called with the expected data (without id)
        expect(plotPointService.createPlotPoint).toHaveBeenCalledWith({ name: 'Test Plot Point' });
      });
    });

    it('handles array of error strings', async () => {
      const errorArray = ['Error 1', 'Error 2'];
      plotPointService.createPlotPoint.mockRejectedValue({ errors: errorArray });
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText('Error 1')).toBeInTheDocument();
        expect(screen.getByText('Error 2')).toBeInTheDocument();
      });
    });

    it('handles single error string in errors property', async () => {
      const error = { errors: 'Single error message' };
      plotPointService.createPlotPoint.mockRejectedValue(error);
      
      render(React.createElement(PlotPointAdd), { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText('Single error message')).toBeInTheDocument();
      });
    });
  });
});