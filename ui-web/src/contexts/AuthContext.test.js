import React from 'react';
import { render, act, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';

// Mock dependencies before importing AuthContext
jest.mock('../services', () => ({
  authService: {
    login: jest.fn(),
    logout: jest.fn(),
    getCurrentUser: jest.fn(),
    register: jest.fn()
  }
}));

jest.mock('../services/api', () => ({
  setTokens: jest.fn(),
  clearTokens: jest.fn(),
  getAccessToken: jest.fn()
}));

jest.mock('../utils/tokenUtils', () => ({
  setRememberMeFlag: jest.fn(),
  getRememberMeFlag: jest.fn(),
  clearRememberMeFlag: jest.fn(),
  isRememberMeSession: jest.fn(),
  getTokenExpiryTime: jest.fn(),
  isTokenExpired: jest.fn(),
  storeTokenExpiry: jest.fn(),
  getStoredTokenExpiry: jest.fn(),
  clearStoredTokenExpiry: jest.fn()
}));

// Import after mocking
// eslint-disable-next-line import/first
import { AuthProvider, useAuth } from './AuthContext';
// eslint-disable-next-line import/first
import { authService } from '../services';
// eslint-disable-next-line import/first
import * as api from '../services/api';
// eslint-disable-next-line import/first
import * as tokenUtils from '../utils/tokenUtils';

describe('AuthContext - Remember Me Functionality', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    // Setup default mock returns
    api.getAccessToken.mockReturnValue(null);
    tokenUtils.isRememberMeSession.mockReturnValue(false);
    tokenUtils.getStoredTokenExpiry.mockReturnValue(null);
    tokenUtils.isTokenExpired.mockReturnValue(false);
    tokenUtils.getTokenExpiryTime.mockReturnValue(Date.now() + 60000);
  });

  describe('login with remember me', () => {
    it('sets remember me flag when login includes rememberMe option', async () => {
      authService.login.mockResolvedValue({
        user: { id: 1, email: 'test@example.com' },
        accessToken: 'access-token',
        refreshToken: 'refresh-token'
      });

      let authContext;
      const TestComponent = () => {
        authContext = useAuth();
        return <div>Test</div>;
      };

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext).toBeTruthy();
      });

      const credentials = { 
        email: 'test@example.com', 
        password: 'password',
        rememberMe: true 
      };

      await act(async () => {
        await authContext.login(credentials);
      });

      expect(tokenUtils.setRememberMeFlag).toHaveBeenCalledWith(true);
      expect(tokenUtils.storeTokenExpiry).toHaveBeenCalled();
    });

    it('does not set remember me flag when rememberMe is false', async () => {
      authService.login.mockResolvedValue({
        user: { id: 1, email: 'test@example.com' },
        accessToken: 'access-token',
        refreshToken: 'refresh-token'
      });

      let authContext;
      const TestComponent = () => {
        authContext = useAuth();
        return <div>Test</div>;
      };

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext).toBeTruthy();
      });

      const credentials = { 
        email: 'test@example.com', 
        password: 'password',
        rememberMe: false 
      };

      await act(async () => {
        await authContext.login(credentials);
      });

      expect(tokenUtils.setRememberMeFlag).toHaveBeenCalledWith(false);
    });
  });

  describe('logout functionality', () => {
    it('clears remember me flag on logout', async () => {
      authService.logout.mockResolvedValue();

      let authContext;
      const TestComponent = () => {
        authContext = useAuth();
        return <div>Test</div>;
      };

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext).toBeTruthy();
      });

      await act(async () => {
        await authContext.logout();
      });

      expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
      expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
    });

    it('clears user state and calls authService.logout', async () => {
      authService.logout.mockResolvedValue();

      let authContext;
      const TestComponent = () => {
        authContext = useAuth();
        return <div>Test</div>;
      };

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext).toBeTruthy();
      });

      // Set user state first
      await act(async () => {
        authContext.setUser = jest.fn();
        authContext.setError = jest.fn();
      });

      await act(async () => {
        await authContext.logout();
      });

      expect(authService.logout).toHaveBeenCalledTimes(1);
      expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
      expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
    });

    it('clears user state even if authService.logout fails', async () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
      authService.logout.mockRejectedValue(new Error('Logout failed'));

      let authContext;
      const TestComponent = () => {
        authContext = useAuth();
        return <div>Test</div>;
      };

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext).toBeTruthy();
      });

      await act(async () => {
        await authContext.logout();
      });

      expect(authService.logout).toHaveBeenCalledTimes(1);
      expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
      expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();

      consoleSpy.mockRestore();
    });

    it('sets loading state during logout process', async () => {
      let resolveLogout;
      authService.logout.mockImplementation(() => new Promise(resolve => {
        resolveLogout = resolve;
      }));

      let authContext;
      let rerender;
      const TestComponent = () => {
        authContext = useAuth();
        return <div data-testid="test">{authContext.loading ? 'Loading' : 'Not Loading'}</div>;
      };

      const { rerender: rerenderFn } = render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );
      rerender = rerenderFn;

      await waitFor(() => {
        expect(authContext).toBeTruthy();
      });

      // Start logout but don't await it yet
      let logoutPromise;
      await act(async () => {
        logoutPromise = authContext.logout();
      });

      // Rerender to get updated loading state
      rerender(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      // Loading should be true during logout
      await waitFor(() => {
        expect(authContext.loading).toBe(true);
      });

      // Resolve the logout promise
      await act(async () => {
        resolveLogout();
        await logoutPromise;
      });

      // Loading should be false after logout
      await waitFor(() => {
        expect(authContext.loading).toBe(false);
      });
    });

    it('maintains correct authentication state after logout', async () => {
      authService.logout.mockResolvedValue();

      let authContext;
      const TestComponent = () => {
        authContext = useAuth();
        return (
          <div>
            <div data-testid="auth-status">{authContext.isAuthenticated ? 'Authenticated' : 'Not Authenticated'}</div>
            <div data-testid="user">{authContext.user ? 'Has User' : 'No User'}</div>
            <div data-testid="error">{authContext.error ? 'Has Error' : 'No Error'}</div>
          </div>
        );
      };

      const { rerender } = render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext).toBeTruthy();
      });

      await act(async () => {
        await authContext.logout();
      });

      // Rerender to get updated state
      rerender(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext.isAuthenticated).toBe(false);
        expect(authContext.user).toBeNull();
        expect(authContext.error).toBeNull();
      });
    });
  });

  describe('token validation with remember me', () => {
    it('checks token expiry on context initialization with remember me', async () => {
      api.getAccessToken.mockReturnValue('mock-token');
      tokenUtils.isRememberMeSession.mockReturnValue(true);
      tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60000);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      
      authService.getCurrentUser.mockResolvedValue({
        id: 1,
        email: 'test@example.com'
      });

      let authContext;
      const TestComponent = () => {
        authContext = useAuth();
        return (
          <div>
            <div data-testid="user-email">{authContext.user?.email || 'No User'}</div>
            <div data-testid="loading">{authContext.loading ? 'Loading' : 'Not Loading'}</div>
          </div>
        );
      };

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext).toBeTruthy();
        expect(authContext.loading).toBe(false);
      }, { timeout: 3000 });

      await waitFor(() => {
        expect(authContext.user).toEqual({
          id: 1,
          email: 'test@example.com'
        });
      }, { timeout: 3000 });

      expect(tokenUtils.isRememberMeSession).toHaveBeenCalled();
      expect(tokenUtils.getStoredTokenExpiry).toHaveBeenCalled();
    });

    it('clears session when remember me token is expired', async () => {
      api.getAccessToken.mockReturnValue('mock-token');
      tokenUtils.isRememberMeSession.mockReturnValue(true);
      tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() - 60000);
      tokenUtils.isTokenExpired.mockReturnValue(true);

      let authContext;
      const TestComponent = () => {
        authContext = useAuth();
        return (
          <div>
            <div data-testid="user">{authContext.user ? 'Has User' : 'No User'}</div>
            <div data-testid="loading">{authContext.loading ? 'Loading' : 'Not Loading'}</div>
          </div>
        );
      };

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext).toBeTruthy();
        expect(authContext.loading).toBe(false);
      }, { timeout: 3000 });

      await waitFor(() => {
        expect(api.clearTokens).toHaveBeenCalled();
        expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
        expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
      });

      expect(authContext.user).toBeNull();
    });
  });
});