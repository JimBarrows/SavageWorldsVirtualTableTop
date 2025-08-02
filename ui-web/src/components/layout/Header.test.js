import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import Header from './Header';
import { useAuth } from '../../contexts/AuthContext';

// Mock the useAuth hook
jest.mock('../../contexts/AuthContext');
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => jest.fn(),
}));

describe('Header Component - Logout Functionality', () => {
  const mockLogout = jest.fn();
  const mockNavigate = jest.fn();
  
  beforeEach(() => {
    jest.clearAllMocks();
    
    // Mock react-router-dom useNavigate
    const mockUseNavigate = jest.requireMock('react-router-dom').useNavigate;
    mockUseNavigate.mockReturnValue(mockNavigate);
  });

  it('should display logout button when user is authenticated', () => {
    useAuth.mockReturnValue({
      user: { email: 'test@example.com', id: '1' },
      logout: mockLogout
    });

    render(
      <MemoryRouter>
        <Header id="test" />
      </MemoryRouter>
    );

    expect(screen.getByText('Logout')).toBeInTheDocument();
    expect(screen.getByText('Welcome, test@example.com')).toBeInTheDocument();
  });

  it('should not display logout button when user is not authenticated', () => {
    useAuth.mockReturnValue({
      user: null,
      logout: mockLogout
    });

    render(
      <MemoryRouter>
        <Header id="test" />
      </MemoryRouter>
    );

    expect(screen.queryByText('Logout')).not.toBeInTheDocument();
    expect(screen.queryByText(/Welcome/)).not.toBeInTheDocument();
  });

  it('should call logout and navigate to login page when logout button is clicked', async () => {
    useAuth.mockReturnValue({
      user: { email: 'test@example.com', id: '1' },
      logout: mockLogout
    });

    render(
      <MemoryRouter>
        <Header id="test" />
      </MemoryRouter>
    );

    const logoutButton = screen.getByText('Logout');
    fireEvent.click(logoutButton);

    await waitFor(() => {
      expect(mockLogout).toHaveBeenCalledTimes(1);
      expect(mockNavigate).toHaveBeenCalledWith('/login');
    });
  });

  it('should handle logout errors gracefully', async () => {
    const mockLogoutWithError = jest.fn().mockRejectedValue(new Error('Logout failed'));
    
    useAuth.mockReturnValue({
      user: { email: 'test@example.com', id: '1' },
      logout: mockLogoutWithError
    });

    const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});

    render(
      <MemoryRouter>
        <Header id="test" />
      </MemoryRouter>
    );

    const logoutButton = screen.getByText('Logout');
    fireEvent.click(logoutButton);

    // Should still navigate even if logout fails
    await waitFor(() => {
      expect(mockLogoutWithError).toHaveBeenCalledTimes(1);
      expect(mockNavigate).toHaveBeenCalledWith('/login');
    });

    consoleSpy.mockRestore();
  });

  it('should display username when email is not available', () => {
    useAuth.mockReturnValue({
      user: { username: 'testuser', id: '1' },
      logout: mockLogout
    });

    render(
      <MemoryRouter>
        <Header id="test" />
      </MemoryRouter>
    );

    expect(screen.getByText('Welcome, testuser')).toBeInTheDocument();
  });

  it('should have proper test attributes for E2E testing', () => {
    useAuth.mockReturnValue({
      user: { email: 'test@example.com', id: '1' },
      logout: mockLogout
    });

    const { container } = render(
      <MemoryRouter>
        <Header id="test" />
      </MemoryRouter>
    );

    const logoutButton = screen.getByText('Logout');
    expect(logoutButton).toHaveClass('btn', 'btn-outline-secondary', 'btn-sm');
    
    // Verify the header has the expected ID structure
    expect(container.querySelector('#header-test')).toBeInTheDocument();
  });
});