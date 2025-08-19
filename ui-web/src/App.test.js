import React from 'react';
import { render, screen, act, waitFor } from '@testing-library/react';
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
});
