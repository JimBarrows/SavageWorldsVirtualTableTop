import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import PlotPointEdit from './PlotPointEdit';

// Mock the service
jest.mock('../services/plotPointService', () => ({
  getPlotPoint: jest.fn(),
  updatePlotPoint: jest.fn()
}));

// Mock hooks
jest.mock('../hooks/usePlotPoints', () => ({
  usePlotPoint: jest.fn()
}));

// Mock navigation and params
const mockNavigate = jest.fn();
const mockParams = { id: '123' };
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate,
  useParams: () => mockParams
}));

const plotPointService = require('../services/plotPointService');
const { usePlotPoint } = require('../hooks/usePlotPoints');

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

describe('PlotPointEdit Component', () => {
  const mockPlotPoint = {
    id: '123',
    name: 'Existing Plot Point',
    description: 'Existing Description',
    genre: 'Fantasy',
    setting: 'Medieval',
    customAttributes: [],
    createdAt: '2024-01-01T00:00:00Z',
    updatedAt: '2024-01-01T00:00:00Z'
  };

  beforeEach(() => {
    jest.clearAllMocks();
    usePlotPoint.mockReturnValue({
      data: mockPlotPoint,
      isLoading: false,
      error: null,
      refetch: jest.fn()
    });
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
    });

    it('displays the page title', () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      expect(screen.getByText(/edit.*plot point/i)).toBeInTheDocument();
    });

    it('loads and displays existing plot point data', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByDisplayValue('Existing Plot Point')).toBeInTheDocument();
        expect(screen.getByDisplayValue('Existing Description')).toBeInTheDocument();
        expect(screen.getByDisplayValue('Fantasy')).toBeInTheDocument();
        expect(screen.getByDisplayValue('Medieval')).toBeInTheDocument();
      });
    });

    it('displays save and cancel buttons', () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      expect(screen.getByRole('button', { name: /save/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /cancel/i })).toBeInTheDocument();
    });
  });

  describe('Loading State', () => {
    it('displays loading indicator while fetching data', () => {
      usePlotPoint.mockReturnValue({
        data: null,
        isLoading: true,
        error: null,
        refetch: jest.fn()
      });

      render(<PlotPointEdit />, { wrapper: createWrapper });
      expect(screen.getByText(/loading/i)).toBeInTheDocument();
    });

    it('hides loading indicator when data is loaded', () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      expect(screen.queryByText(/loading/i)).not.toBeInTheDocument();
    });
  });

  describe('Error State', () => {
    it('displays error message when plot point not found', () => {
      usePlotPoint.mockReturnValue({
        data: null,
        isLoading: false,
        error: new Error('Plot point not found'),
        refetch: jest.fn()
      });

      render(<PlotPointEdit />, { wrapper: createWrapper });
      expect(screen.getByText(/plot point not found/i)).toBeInTheDocument();
    });

    it('provides retry button on error', () => {
      const refetchMock = jest.fn();
      usePlotPoint.mockReturnValue({
        data: null,
        isLoading: false,
        error: new Error('Failed to load'),
        refetch: refetchMock
      });

      render(<PlotPointEdit />, { wrapper: createWrapper });
      const retryButton = screen.getByRole('button', { name: /retry/i });
      fireEvent.click(retryButton);
      expect(refetchMock).toHaveBeenCalled();
    });
  });

  describe('Form Editing', () => {
    it('allows editing of plot point name', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Updated Plot Point' } });
      
      expect(nameInput.value).toBe('Updated Plot Point');
    });

    it('allows editing of description', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const descriptionInput = await screen.findByDisplayValue('Existing Description');
      fireEvent.change(descriptionInput, { target: { value: 'Updated Description' } });
      
      expect(descriptionInput.value).toBe('Updated Description');
    });

    it('tracks form changes', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Changed' } });
      
      // Form should indicate unsaved changes
      expect(screen.getByText(/unsaved changes/i)).toBeInTheDocument();
    });
  });

  describe('Form Validation', () => {
    it('validates required fields before saving', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: '' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/name is required/i)).toBeInTheDocument();
      });
    });

    it('validates field lengths', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      const longName = 'a'.repeat(256);
      fireEvent.change(nameInput, { target: { value: longName } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/name must be less than 255 characters/i)).toBeInTheDocument();
      });
    });
  });

  describe('Form Submission', () => {
    it('submits updated data successfully', async () => {
      plotPointService.updatePlotPoint.mockResolvedValue({
        success: true,
        data: { ...mockPlotPoint, name: 'Updated Plot Point' }
      });
      
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Updated Plot Point' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(plotPointService.updatePlotPoint).toHaveBeenCalledWith('123', {
          name: 'Updated Plot Point',
          description: 'Existing Description',
          genre: 'Fantasy',
          setting: 'Medieval',
          customAttributes: []
        });
      });
    });

    it('shows success message after update', async () => {
      plotPointService.updatePlotPoint.mockResolvedValue({
        success: true
      });
      
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Updated' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/successfully updated/i)).toBeInTheDocument();
      });
    });

    it('handles update errors gracefully', async () => {
      plotPointService.updatePlotPoint.mockRejectedValue(new Error('Update failed'));
      
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Updated' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/failed to update/i)).toBeInTheDocument();
      });
    });

    it('disables save button during submission', async () => {
      plotPointService.updatePlotPoint.mockImplementation(
        () => new Promise(resolve => setTimeout(resolve, 100))
      );
      
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Updated' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      expect(saveButton).toBeDisabled();
      expect(screen.getByText(/saving/i)).toBeInTheDocument();
    });

    it('navigates to list after successful save', async () => {
      plotPointService.updatePlotPoint.mockResolvedValue({ success: true });
      
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Updated' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(mockNavigate).toHaveBeenCalledWith('/plot-points');
      });
    });
  });

  describe('Cancel Functionality', () => {
    it('shows confirmation when canceling with unsaved changes', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Changed' } });
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(screen.getByText(/unsaved changes/i)).toBeInTheDocument();
      expect(screen.getByText(/are you sure/i)).toBeInTheDocument();
    });

    it('navigates away when confirming cancel', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Changed' } });
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      const confirmButton = screen.getByRole('button', { name: /confirm/i });
      fireEvent.click(confirmButton);
      
      expect(mockNavigate).toHaveBeenCalledWith(-1);
    });

    it('stays on page when canceling discard', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Changed' } });
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      const keepEditingButton = screen.getByRole('button', { name: /keep editing/i });
      fireEvent.click(keepEditingButton);
      
      expect(mockNavigate).not.toHaveBeenCalled();
      expect(nameInput.value).toBe('Changed');
    });
  });

  describe('Reset Functionality', () => {
    it('resets form to original values', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Changed' } });
      
      const resetButton = screen.getByRole('button', { name: /reset/i });
      fireEvent.click(resetButton);
      
      expect(nameInput.value).toBe('Existing Plot Point');
    });

    it('clears validation errors on reset', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: '' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/name is required/i)).toBeInTheDocument();
      });
      
      const resetButton = screen.getByRole('button', { name: /reset/i });
      fireEvent.click(resetButton);
      
      expect(screen.queryByText(/name is required/i)).not.toBeInTheDocument();
    });
  });

  describe('Version Conflict Handling', () => {
    it('detects version conflicts', async () => {
      plotPointService.updatePlotPoint.mockRejectedValue({
        code: 'VERSION_CONFLICT',
        message: 'Plot point has been modified by another user'
      });
      
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Updated' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/modified by another user/i)).toBeInTheDocument();
      });
    });

    it('offers to reload on version conflict', async () => {
      const refetchMock = jest.fn();
      usePlotPoint.mockReturnValue({
        data: mockPlotPoint,
        isLoading: false,
        error: null,
        refetch: refetchMock
      });
      
      plotPointService.updatePlotPoint.mockRejectedValue({
        code: 'VERSION_CONFLICT'
      });
      
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Updated' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        const reloadButton = screen.getByRole('button', { name: /reload/i });
        fireEvent.click(reloadButton);
        expect(refetchMock).toHaveBeenCalled();
      });
    });
  });

  describe('Accessibility', () => {
    it('has proper heading hierarchy', () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      const heading = screen.getByRole('heading', { level: 1 });
      expect(heading).toHaveTextContent(/edit.*plot point/i);
    });

    it('has proper form labels', async () => {
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      await waitFor(() => {
        expect(screen.getByLabelText(/name/i)).toBeInTheDocument();
        expect(screen.getByLabelText(/description/i)).toBeInTheDocument();
        expect(screen.getByLabelText(/genre/i)).toBeInTheDocument();
      });
    });

    it('announces save status to screen readers', async () => {
      plotPointService.updatePlotPoint.mockResolvedValue({ success: true });
      
      render(<PlotPointEdit />, { wrapper: createWrapper });
      
      const nameInput = await screen.findByDisplayValue('Existing Plot Point');
      fireEvent.change(nameInput, { target: { value: 'Updated' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        const successMessage = screen.getByText(/successfully updated/i);
        expect(successMessage).toHaveAttribute('role', 'status');
      });
    });
  });
});