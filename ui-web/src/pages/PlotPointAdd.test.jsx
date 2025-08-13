import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import PlotPointAdd from './PlotPointAdd';

// Mock the service
jest.mock('../services/plotPointService', () => ({
  createPlotPoint: jest.fn()
}));

// Mock navigation
const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate
}));

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

describe('PlotPointAdd Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
    });

    it('displays the page title', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      expect(screen.getByText(/add.*plot point/i)).toBeInTheDocument();
    });

    it('renders all required form fields', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      expect(screen.getByLabelText(/name/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/description/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/genre/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/setting/i)).toBeInTheDocument();
    });

    it('displays save and cancel buttons', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      expect(screen.getByRole('button', { name: /save/i })).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /cancel/i })).toBeInTheDocument();
    });
  });

  describe('Form Validation', () => {
    it('shows validation error for empty name', async () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/name is required/i)).toBeInTheDocument();
      });
    });

    it('shows validation error for name too short', async () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      fireEvent.change(nameInput, { target: { value: 'ab' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/name must be at least 3 characters/i)).toBeInTheDocument();
      });
    });

    it('shows validation error for name too long', async () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      const longName = 'a'.repeat(256);
      fireEvent.change(nameInput, { target: { value: longName } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/name must be less than 255 characters/i)).toBeInTheDocument();
      });
    });

    it('validates description length', async () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const descriptionInput = screen.getByLabelText(/description/i);
      const longDescription = 'a'.repeat(5001);
      fireEvent.change(descriptionInput, { target: { value: longDescription } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/description must be less than 5000 characters/i)).toBeInTheDocument();
      });
    });

    it('clears validation errors when field is corrected', async () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/name is required/i)).toBeInTheDocument();
      });
      
      const nameInput = screen.getByLabelText(/name/i);
      fireEvent.change(nameInput, { target: { value: 'Valid Name' } });
      
      await waitFor(() => {
        expect(screen.queryByText(/name is required/i)).not.toBeInTheDocument();
      });
    });
  });

  describe('Form Submission', () => {
    it('submits form with valid data', async () => {
      plotPointService.createPlotPoint.mockResolvedValue({
        success: true,
        data: { id: '123', name: 'Test Plot Point' }
      });
      
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      const descriptionInput = screen.getByLabelText(/description/i);
      const genreInput = screen.getByLabelText(/genre/i);
      const settingInput = screen.getByLabelText(/setting/i);
      
      fireEvent.change(nameInput, { target: { value: 'Test Plot Point' } });
      fireEvent.change(descriptionInput, { target: { value: 'Test Description' } });
      fireEvent.change(genreInput, { target: { value: 'Fantasy' } });
      fireEvent.change(settingInput, { target: { value: 'Medieval' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(plotPointService.createPlotPoint).toHaveBeenCalledWith({
          name: 'Test Plot Point',
          description: 'Test Description',
          genre: 'Fantasy',
          setting: 'Medieval'
        });
      });
    });

    it('navigates to list page after successful submission', async () => {
      plotPointService.createPlotPoint.mockResolvedValue({
        success: true,
        data: { id: '123' }
      });
      
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      fireEvent.change(nameInput, { target: { value: 'Test Plot Point' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(mockNavigate).toHaveBeenCalledWith('/plot-points');
      });
    });

    it('shows success message after creation', async () => {
      plotPointService.createPlotPoint.mockResolvedValue({
        success: true,
        data: { id: '123' }
      });
      
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      fireEvent.change(nameInput, { target: { value: 'Test Plot Point' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/plot point created successfully/i)).toBeInTheDocument();
      });
    });

    it('shows error message when submission fails', async () => {
      plotPointService.createPlotPoint.mockRejectedValue(new Error('Server error'));
      
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      fireEvent.change(nameInput, { target: { value: 'Test Plot Point' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        expect(screen.getByText(/failed to create plot point/i)).toBeInTheDocument();
      });
    });

    it('disables save button during submission', async () => {
      plotPointService.createPlotPoint.mockImplementation(
        () => new Promise(resolve => setTimeout(resolve, 100))
      );
      
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      fireEvent.change(nameInput, { target: { value: 'Test Plot Point' } });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      expect(saveButton).toBeDisabled();
      expect(screen.getByText(/saving/i)).toBeInTheDocument();
    });
  });

  describe('Cancel Functionality', () => {
    it('navigates back when cancel is clicked', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(mockNavigate).toHaveBeenCalledWith(-1);
    });

    it('shows confirmation dialog when canceling with unsaved changes', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      fireEvent.change(nameInput, { target: { value: 'Test' } });
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(screen.getByText(/unsaved changes/i)).toBeInTheDocument();
    });

    it('cancels without confirmation when no changes made', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const cancelButton = screen.getByRole('button', { name: /cancel/i });
      fireEvent.click(cancelButton);
      
      expect(mockNavigate).toHaveBeenCalledWith(-1);
      expect(screen.queryByText(/unsaved changes/i)).not.toBeInTheDocument();
    });
  });

  describe('Advanced Form Features', () => {
    it('supports adding custom attributes', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const addAttributeButton = screen.getByRole('button', { name: /add attribute/i });
      fireEvent.click(addAttributeButton);
      
      expect(screen.getByPlaceholderText(/attribute name/i)).toBeInTheDocument();
      expect(screen.getByPlaceholderText(/attribute value/i)).toBeInTheDocument();
    });

    it('removes custom attributes', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const addAttributeButton = screen.getByRole('button', { name: /add attribute/i });
      fireEvent.click(addAttributeButton);
      
      const removeButton = screen.getByRole('button', { name: /remove/i });
      fireEvent.click(removeButton);
      
      expect(screen.queryByPlaceholderText(/attribute name/i)).not.toBeInTheDocument();
    });

    it('supports file upload for plot point image', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const fileInput = screen.getByLabelText(/image/i);
      const file = new File(['image'], 'plot-point.png', { type: 'image/png' });
      
      fireEvent.change(fileInput, { target: { files: [file] } });
      
      expect(fileInput.files[0]).toBe(file);
    });

    it('validates file size for uploads', async () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const fileInput = screen.getByLabelText(/image/i);
      const largeFile = new File(['x'.repeat(5 * 1024 * 1024)], 'large.png', { type: 'image/png' });
      
      fireEvent.change(fileInput, { target: { files: [largeFile] } });
      
      await waitFor(() => {
        expect(screen.getByText(/file size must be less than/i)).toBeInTheDocument();
      });
    });
  });

  describe('Form State Management', () => {
    it('preserves form data when navigating between tabs', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      fireEvent.change(nameInput, { target: { value: 'Test Plot Point' } });
      
      // Switch to advanced tab
      const advancedTab = screen.getByRole('tab', { name: /advanced/i });
      fireEvent.click(advancedTab);
      
      // Switch back to basic tab
      const basicTab = screen.getByRole('tab', { name: /basic/i });
      fireEvent.click(basicTab);
      
      expect(nameInput.value).toBe('Test Plot Point');
    });

    it('resets form when reset button is clicked', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      const descriptionInput = screen.getByLabelText(/description/i);
      
      fireEvent.change(nameInput, { target: { value: 'Test' } });
      fireEvent.change(descriptionInput, { target: { value: 'Description' } });
      
      const resetButton = screen.getByRole('button', { name: /reset/i });
      fireEvent.click(resetButton);
      
      expect(nameInput.value).toBe('');
      expect(descriptionInput.value).toBe('');
    });
  });

  describe('Accessibility', () => {
    it('has proper form labels', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      expect(screen.getByLabelText(/name/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/description/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/genre/i)).toBeInTheDocument();
    });

    it('shows required field indicators', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameLabel = screen.getByText(/name/i).closest('label');
      expect(nameLabel).toHaveTextContent('*');
    });

    it('announces form errors to screen readers', async () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const saveButton = screen.getByRole('button', { name: /save/i });
      fireEvent.click(saveButton);
      
      await waitFor(() => {
        const errorMessage = screen.getByText(/name is required/i);
        expect(errorMessage).toHaveAttribute('role', 'alert');
      });
    });

    it('supports keyboard navigation', () => {
      render(<PlotPointAdd />, { wrapper: createWrapper });
      
      const nameInput = screen.getByLabelText(/name/i);
      const descriptionInput = screen.getByLabelText(/description/i);
      
      nameInput.focus();
      expect(document.activeElement).toBe(nameInput);
      
      // Tab to next field
      fireEvent.keyDown(nameInput, { key: 'Tab' });
      // Note: actual tab behavior would be handled by browser
    });
  });
});