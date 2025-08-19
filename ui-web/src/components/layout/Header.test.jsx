import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import '@testing-library/jest-dom';
import Header from './Header';
import { useAuth } from '../../contexts/AuthContext';

// Mock the AuthContext
jest.mock('../../contexts/AuthContext', () => ({
  useAuth: jest.fn()
}));

// Mock Bootstrap React Components
jest.mock('bootstrap-react-components/distribution/bootstrap/components/Navbar', () => {
  return function MockNavbar({ children, id }) {
    return <nav data-testid="navbar" id={id}>{children}</nav>;
  };
});

jest.mock('bootstrap-react-components/distribution/bootstrap/components/Navbar/Brand', () => {
  return function MockBrand({ children, id, onClick }) {
    return (
      <div data-testid="brand" id={id} onClick={onClick} role="button" tabIndex={0}>
        {children}
      </div>
    );
  };
});

const mockNavigate = jest.fn();

// Mock react-router-dom
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate
}));

// Mock window.confirm
const originalConfirm = window.confirm;
beforeEach(() => {
  jest.clearAllMocks();
  window.confirm = jest.fn();
});

afterEach(() => {
  window.confirm = originalConfirm;
});

const renderHeader = (props = {}) => {
  const defaultProps = {
    id: 'test-header',
    ...props
  };

  return render(
    <MemoryRouter>
      <Header {...defaultProps} />
    </MemoryRouter>
  );
};

describe('Header Component', () => {
  describe('Rendering', () => {
    it('renders navbar with correct id', () => {
      useAuth.mockReturnValue({
        user: null,
        logout: jest.fn()
      });

      renderHeader({ id: 'main' });
      
      expect(screen.getByTestId('navbar')).toHaveAttribute('id', 'header-main');
    });

    it('renders brand with correct text', () => {
      useAuth.mockReturnValue({
        user: null,
        logout: jest.fn()
      });

      renderHeader();
      
      expect(screen.getByTestId('brand')).toHaveTextContent('Savage Worlds');
    });

    it('does not show user info when no user is logged in', () => {
      useAuth.mockReturnValue({
        user: null,
        logout: jest.fn()
      });

      renderHeader();
      
      expect(screen.queryByTestId('logout-button')).not.toBeInTheDocument();
      expect(screen.queryByText(/Welcome/)).not.toBeInTheDocument();
    });

    it('shows user info and logout button when user is logged in', () => {
      const mockUser = { email: 'test@example.com' };
      useAuth.mockReturnValue({
        user: mockUser,
        logout: jest.fn()
      });

      renderHeader();
      
      expect(screen.getByText('Welcome, test@example.com')).toBeInTheDocument();
      expect(screen.getByTestId('logout-button')).toBeInTheDocument();
    });

    it('displays username when email is not available', () => {
      const mockUser = { username: 'testuser' };
      useAuth.mockReturnValue({
        user: mockUser,
        logout: jest.fn()
      });

      renderHeader();
      
      expect(screen.getByText('Welcome, testuser')).toBeInTheDocument();
    });

    it('displays email when both email and username are available', () => {
      const mockUser = { email: 'test@example.com', username: 'testuser' };
      useAuth.mockReturnValue({
        user: mockUser,
        logout: jest.fn()
      });

      renderHeader();
      
      expect(screen.getByText('Welcome, test@example.com')).toBeInTheDocument();
    });
  });

  describe('Navigation', () => {
    it('navigates to home when brand is clicked', () => {
      useAuth.mockReturnValue({
        user: null,
        logout: jest.fn()
      });

      renderHeader();
      
      fireEvent.click(screen.getByTestId('brand'));
      
      expect(mockNavigate).toHaveBeenCalledWith('/');
    });

    it('can handle brand click with keyboard', () => {
      useAuth.mockReturnValue({
        user: null,
        logout: jest.fn()
      });

      renderHeader();
      
      const brand = screen.getByTestId('brand');
      fireEvent.keyDown(brand, { key: 'Enter', code: 'Enter' });
      
      // The click should still work for keyboard navigation
      fireEvent.click(brand);
      expect(mockNavigate).toHaveBeenCalledWith('/');
    });
  });

  describe('Logout functionality', () => {
    it('calls logout and navigates when logout button is clicked with no unsaved changes', async () => {
      const mockLogout = jest.fn().mockResolvedValue();
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(mockLogout).toHaveBeenCalled();
      });
      
      expect(mockNavigate).toHaveBeenCalledWith('/login', {
        state: {
          message: 'You have been logged out successfully',
          type: 'success'
        }
      });
    });

    it('shows confirmation dialog when there are unsaved changes', async () => {
      const mockLogout = jest.fn().mockResolvedValue();
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      // Mock DOM with unsaved form changes
      const mockForm = document.createElement('form');
      const mockInput = document.createElement('input');
      mockInput.defaultValue = 'original';
      mockInput.value = 'changed';
      mockForm.appendChild(mockInput);
      document.body.appendChild(mockForm);

      window.confirm.mockReturnValue(true);

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(window.confirm).toHaveBeenCalledWith('You have unsaved changes. Are you sure you want to logout?');
      });
      
      expect(mockLogout).toHaveBeenCalled();
      
      // Cleanup
      document.body.removeChild(mockForm);
    });

    it('cancels logout when user rejects confirmation dialog', async () => {
      const mockLogout = jest.fn().mockResolvedValue();
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      // Mock DOM with unsaved form changes
      const mockForm = document.createElement('form');
      const mockInput = document.createElement('input');
      mockInput.defaultValue = 'original';
      mockInput.value = 'changed';
      mockForm.appendChild(mockInput);
      document.body.appendChild(mockForm);

      window.confirm.mockReturnValue(false);

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(window.confirm).toHaveBeenCalledWith('You have unsaved changes. Are you sure you want to logout?');
      });
      
      expect(mockLogout).not.toHaveBeenCalled();
      expect(mockNavigate).not.toHaveBeenCalled();
      
      // Cleanup
      document.body.removeChild(mockForm);
    });

    it('handles logout errors gracefully', async () => {
      const mockLogout = jest.fn().mockRejectedValue(new Error('Logout failed'));
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(mockLogout).toHaveBeenCalled();
      });
      
      expect(consoleSpy).toHaveBeenCalledWith('Logout error in header:', expect.any(Error));
      expect(mockNavigate).toHaveBeenCalledWith('/login');
      
      consoleSpy.mockRestore();
    });

    it('handles logout errors even with unsaved changes confirmation', async () => {
      const mockLogout = jest.fn().mockRejectedValue(new Error('Logout failed'));
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      // Mock DOM with unsaved form changes
      const mockForm = document.createElement('form');
      const mockInput = document.createElement('input');
      mockInput.defaultValue = 'original';
      mockInput.value = 'changed';
      mockForm.appendChild(mockInput);
      document.body.appendChild(mockForm);

      window.confirm.mockReturnValue(true);
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(window.confirm).toHaveBeenCalled();
        expect(mockLogout).toHaveBeenCalled();
      });
      
      expect(consoleSpy).toHaveBeenCalledWith('Logout error in header:', expect.any(Error));
      expect(mockNavigate).toHaveBeenCalledWith('/login');
      
      // Cleanup
      document.body.removeChild(mockForm);
      consoleSpy.mockRestore();
    });
  });

  describe('Unsaved changes detection', () => {
    it('detects no unsaved changes when forms are unchanged', async () => {
      const mockLogout = jest.fn().mockResolvedValue();
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      // Mock DOM with unchanged form
      const mockForm = document.createElement('form');
      const mockInput = document.createElement('input');
      mockInput.defaultValue = 'unchanged';
      mockInput.value = 'unchanged';
      mockForm.appendChild(mockInput);
      document.body.appendChild(mockForm);

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(mockLogout).toHaveBeenCalled();
      });
      
      expect(window.confirm).not.toHaveBeenCalled();
      
      // Cleanup
      document.body.removeChild(mockForm);
    });

    it('detects unsaved changes in textarea elements', async () => {
      const mockLogout = jest.fn().mockResolvedValue();
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      // Mock DOM with changed textarea
      const mockForm = document.createElement('form');
      const mockTextarea = document.createElement('textarea');
      mockTextarea.defaultValue = 'original text';
      mockTextarea.value = 'changed text';
      mockForm.appendChild(mockTextarea);
      document.body.appendChild(mockForm);

      window.confirm.mockReturnValue(true);

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(window.confirm).toHaveBeenCalled();
      });
      
      // Cleanup
      document.body.removeChild(mockForm);
    });

    it('detects unsaved changes in select elements', async () => {
      const mockLogout = jest.fn().mockResolvedValue();
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      // Mock DOM with changed select
      const mockForm = document.createElement('form');
      const mockSelect = document.createElement('select');
      mockSelect.defaultValue = 'option1';
      mockSelect.value = 'option2';
      mockForm.appendChild(mockSelect);
      document.body.appendChild(mockForm);

      window.confirm.mockReturnValue(true);

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(window.confirm).toHaveBeenCalled();
      });
      
      // Cleanup
      document.body.removeChild(mockForm);
    });

    it('handles multiple forms with mixed change states', async () => {
      const mockLogout = jest.fn().mockResolvedValue();
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      // Mock DOM with multiple forms - one unchanged, one changed
      const unchangedForm = document.createElement('form');
      const unchangedInput = document.createElement('input');
      unchangedInput.defaultValue = 'same';
      unchangedInput.value = 'same';
      unchangedForm.appendChild(unchangedInput);

      const changedForm = document.createElement('form');
      const changedInput = document.createElement('input');
      changedInput.defaultValue = 'original';
      changedInput.value = 'changed';
      changedForm.appendChild(changedInput);

      document.body.appendChild(unchangedForm);
      document.body.appendChild(changedForm);

      window.confirm.mockReturnValue(true);

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(window.confirm).toHaveBeenCalled();
      });
      
      // Cleanup
      document.body.removeChild(unchangedForm);
      document.body.removeChild(changedForm);
    });

    it('handles empty forms gracefully', async () => {
      const mockLogout = jest.fn().mockResolvedValue();
      const mockUser = { email: 'test@example.com' };
      
      useAuth.mockReturnValue({
        user: mockUser,
        logout: mockLogout
      });

      // Mock DOM with empty form
      const mockForm = document.createElement('form');
      document.body.appendChild(mockForm);

      renderHeader();
      
      fireEvent.click(screen.getByTestId('logout-button'));
      
      await waitFor(() => {
        expect(mockLogout).toHaveBeenCalled();
      });
      
      expect(window.confirm).not.toHaveBeenCalled();
      
      // Cleanup
      document.body.removeChild(mockForm);
    });
  });

  describe('Accessibility', () => {
    it('has proper button role and attributes for logout', () => {
      const mockUser = { email: 'test@example.com' };
      useAuth.mockReturnValue({
        user: mockUser,
        logout: jest.fn()
      });

      renderHeader();
      
      const logoutButton = screen.getByTestId('logout-button');
      expect(logoutButton).toHaveClass('btn', 'btn-outline-secondary', 'btn-sm');
    });

    it('has proper tabindex for brand element', () => {
      useAuth.mockReturnValue({
        user: null,
        logout: jest.fn()
      });

      renderHeader();
      
      const brand = screen.getByTestId('brand');
      expect(brand).toHaveAttribute('tabIndex', '0');
      expect(brand).toHaveAttribute('role', 'button');
    });
  });

  describe('PropTypes validation', () => {
    it('renders with required id prop', () => {
      useAuth.mockReturnValue({
        user: null,
        logout: jest.fn()
      });

      expect(() => renderHeader({ id: 'test' })).not.toThrow();
    });
  });
});