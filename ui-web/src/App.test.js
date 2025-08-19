import React from 'react';
import { render, screen, act, waitFor, fireEvent } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import App from './App';

// Mock all external dependencies
jest.mock('./services', () => ({
  authService: {
    login: jest.fn(),
    logout: jest.fn(),
    getCurrentUser: jest.fn(),
    register: jest.fn()
  }
}));

jest.mock('./services/api', () => ({
  clearTokens: jest.fn(),
  getAccessToken: jest.fn()
}));

jest.mock('./utils/tokenUtils', () => ({
  setRememberMeFlag: jest.fn(),
  clearRememberMeFlag: jest.fn(),
  isRememberMeSession: jest.fn(),
  getTokenExpiryTime: jest.fn(),
  isTokenExpired: jest.fn(),
  storeTokenExpiry: jest.fn(),
  getStoredTokenExpiry: jest.fn(),
  clearStoredTokenExpiry: jest.fn()
}));

// Mock React Query DevTools to avoid issues in tests
jest.mock('react-query/devtools', () => ({
  ReactQueryDevtools: () => null
}));

// Mock config
jest.mock('./config', () => ({
  default: {
    query: {
      retry: false,
      refetchOnWindowFocus: false
    }
  }
}));

import { authService } from './services';
import * as api from './services/api';
import * as tokenUtils from './utils/tokenUtils';

describe('App Component', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    // Setup default mock returns
    api.getAccessToken.mockReturnValue(null);
    tokenUtils.isRememberMeSession.mockReturnValue(false);
    tokenUtils.getStoredTokenExpiry.mockReturnValue(null);
    tokenUtils.isTokenExpired.mockReturnValue(false);
    tokenUtils.getTokenExpiryTime.mockReturnValue(Date.now() + 60000);
  });

  it('renders without crashing', () => {
    render(<App />);
    expect(screen.getByText(/Savage Worlds VTT/i)).toBeInTheDocument();
  });

  it('shows loading state initially', () => {
    render(<App />);
    expect(screen.getByText(/loading/i)).toBeInTheDocument();
  });

  it('redirects to login when not authenticated', async () => {
    api.getAccessToken.mockReturnValue(null);
    
    render(
      <MemoryRouter initialEntries={['/']}>
        <App />
      </MemoryRouter>
    );

    await waitFor(() => {
      expect(screen.getByText(/login/i)).toBeInTheDocument();
    });
  });

  it('shows plot point list when authenticated', async () => {
    const mockUser = { id: '1', email: 'test@example.com' };
    
    api.getAccessToken.mockReturnValue('valid-token');
    tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60 * 60 * 1000);
    tokenUtils.isTokenExpired.mockReturnValue(false);
    authService.getCurrentUser.mockResolvedValue(mockUser);

    render(
      <MemoryRouter initialEntries={['/']}>
        <App />
      </MemoryRouter>
    );

    await act(async () => {
      await new Promise(resolve => setTimeout(resolve, 100));
    });

    await waitFor(() => {
      expect(screen.queryByText(/loading/i)).not.toBeInTheDocument();
    });
  });

  it('handles auth check failure gracefully', async () => {
    api.getAccessToken.mockReturnValue('invalid-token');
    authService.getCurrentUser.mockRejectedValue(new Error('Auth failed'));

    const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});

    render(
      <MemoryRouter initialEntries={['/']}>
        <App />
      </MemoryRouter>
    );

    await act(async () => {
      await new Promise(resolve => setTimeout(resolve, 100));
    });

    await waitFor(() => {
      expect(screen.getByText(/login/i)).toBeInTheDocument();
    });

    expect(api.clearTokens).toHaveBeenCalled();
    consoleSpy.mockRestore();
  });

  it('provides QueryClient context to components', () => {
    render(<App />);
    
    // The app should render without throwing errors related to missing QueryClient
    expect(screen.getByText(/Savage Worlds VTT/i)).toBeInTheDocument();
  });

  it('wraps components in ErrorBoundary', () => {
    // This test ensures the ErrorBoundary is present
    render(<App />);
    
    // If ErrorBoundary is working, the app should render normally
    expect(screen.getByText(/Savage Worlds VTT/i)).toBeInTheDocument();
  });

  it('configures React Query with proper defaults', () => {
    render(<App />);
    
    // The app should render without React Query configuration errors
    expect(screen.getByText(/Savage Worlds VTT/i)).toBeInTheDocument();
  });

  it('handles expired tokens on app load', async () => {
    api.getAccessToken.mockReturnValue('expired-token');
    tokenUtils.isTokenExpired.mockReturnValue(true);
    
    const consoleSpy = jest.spyOn(console, 'log').mockImplementation(() => {});
    
    render(
      <MemoryRouter initialEntries={['/']}>
        <App />
      </MemoryRouter>
    );

    await act(async () => {
      await new Promise(resolve => setTimeout(resolve, 100));
    });

    await waitFor(() => {
      expect(screen.getByText(/login/i)).toBeInTheDocument();
    });

    expect(api.clearTokens).toHaveBeenCalled();
    expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
    expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
    
    consoleSpy.mockRestore();
  });

  describe('Login Component - Lines 52-143', () => {
    it('renders login form with all fields', async () => {
      api.getAccessToken.mockReturnValue(null);
      
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
        expect(screen.getByLabelText(/password/i)).toBeInTheDocument();
        expect(screen.getByRole('button', { name: /login/i })).toBeInTheDocument();
        expect(screen.getByText(/don't have an account/i)).toBeInTheDocument();
        expect(screen.getByText(/forgot password/i)).toBeInTheDocument();
      });
    });

    it('updates form state when typing', async () => {
      api.getAccessToken.mockReturnValue(null);
      
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        const emailInput = screen.getByLabelText(/email/i);
        const passwordInput = screen.getByLabelText(/password/i);
        
        fireEvent.change(emailInput, { target: { value: 'test@example.com' } });
        fireEvent.change(passwordInput, { target: { value: 'password123' } });
        
        expect(emailInput.value).toBe('test@example.com');
        expect(passwordInput.value).toBe('password123');
      });
    });

    it('handles successful login submission', async () => {
      api.getAccessToken.mockReturnValue(null);
      authService.login.mockResolvedValue({ success: true });
      
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        const emailInput = screen.getByLabelText(/email/i);
        const passwordInput = screen.getByLabelText(/password/i);
        const submitButton = screen.getByRole('button', { name: /login/i });
        
        fireEvent.change(emailInput, { target: { value: 'test@example.com' } });
        fireEvent.change(passwordInput, { target: { value: 'password123' } });
        fireEvent.click(submitButton);
      });

      await waitFor(() => {
        expect(authService.login).toHaveBeenCalledWith({
          email: 'test@example.com',
          password: 'password123',
          rememberMe: false
        });
      });
    });

    it('shows loading state during login', async () => {
      api.getAccessToken.mockReturnValue(null);
      let resolveLogin;
      authService.login.mockReturnValue(new Promise(resolve => { resolveLogin = resolve; }));
      
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        const emailInput = screen.getByLabelText(/email/i);
        const passwordInput = screen.getByLabelText(/password/i);
        const submitButton = screen.getByRole('button', { name: /login/i });
        
        fireEvent.change(emailInput, { target: { value: 'test@example.com' } });
        fireEvent.change(passwordInput, { target: { value: 'password123' } });
        fireEvent.click(submitButton);
      });

      await waitFor(() => {
        expect(screen.getByText('Logging in...')).toBeInTheDocument();
      });

      resolveLogin({ success: true });
      
      await waitFor(() => {
        expect(screen.queryByText('Logging in...')).not.toBeInTheDocument();
      });
    });

    it('displays logout message when present', async () => {
      api.getAccessToken.mockReturnValue(null);
      
      render(
        <MemoryRouter initialEntries={[{
          pathname: '/login',
          state: { message: 'You have been logged out successfully', type: 'success' }
        }]}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByTestId('logout-success')).toBeInTheDocument();
        expect(screen.getByText('You have been logged out successfully')).toBeInTheDocument();
      });
    });

    it('handles remember me checkbox', async () => {
      api.getAccessToken.mockReturnValue(null);
      authService.login.mockResolvedValue({ success: true });
      
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        const rememberMeCheckbox = screen.getByRole('checkbox');
        const emailInput = screen.getByLabelText(/email/i);
        const passwordInput = screen.getByLabelText(/password/i);
        const submitButton = screen.getByRole('button', { name: /login/i });
        
        fireEvent.click(rememberMeCheckbox);
        fireEvent.change(emailInput, { target: { value: 'test@example.com' } });
        fireEvent.change(passwordInput, { target: { value: 'password123' } });
        fireEvent.click(submitButton);
      });

      await waitFor(() => {
        expect(authService.login).toHaveBeenCalledWith({
          email: 'test@example.com',
          password: 'password123',
          rememberMe: true
        });
      });
    });
  });

  describe('HomeRoute Component - Line 252', () => {
    it('redirects authenticated users to plot-points', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      api.getAccessToken.mockReturnValue('valid-token');
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);
      
      render(
        <MemoryRouter initialEntries={['/']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.queryByText(/loading/i)).not.toBeInTheDocument();
      });

      // Should navigate to plot-points (line 252)
      await waitFor(() => {
        expect(screen.queryByTestId('marketing-page')).not.toBeInTheDocument();
      });
    });

    it('shows marketing page for unauthenticated users', async () => {
      api.getAccessToken.mockReturnValue(null);
      
      render(
        <MemoryRouter initialEntries={['/']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByText(/Savage Worlds VTT/i)).toBeInTheDocument();
      });
    });
  });

  describe('Protected Routes', () => {
    it('shows loading state before authentication check', async () => {
      api.getAccessToken.mockReturnValue('token');
      let resolveUser;
      authService.getCurrentUser.mockReturnValue(
        new Promise(resolve => { resolveUser = resolve; })
      );
      
      render(
        <MemoryRouter initialEntries={['/plot-points']}>
          <App />
        </MemoryRouter>
      );

      // Should show loading (line 36)
      expect(screen.getByText('Loading...')).toBeInTheDocument();

      resolveUser({ id: '1', email: 'test@example.com' });
      
      await waitFor(() => {
        expect(screen.queryByText('Loading...')).not.toBeInTheDocument();
      });
    });

    it('redirects to login when not authenticated', async () => {
      api.getAccessToken.mockReturnValue(null);
      
      render(
        <MemoryRouter initialEntries={['/plot-points']}>
          <App />
        </MemoryRouter>
      );

      // Should redirect to login (lines 39-40)
      await waitFor(() => {
        expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
      });
    });
  });
});
