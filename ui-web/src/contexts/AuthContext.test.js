import React from 'react';
import { render, screen, act, waitFor } from '@testing-library/react';
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

// Test component to access auth context
const TestComponent = ({ onAuthChange } = {}) => {
  const auth = useAuth();
  
  React.useEffect(() => {
    if (onAuthChange) {
      onAuthChange(auth);
    }
  }, [auth, onAuthChange]);
  
  return (
    <div>
      <div data-testid="user">{auth.user ? JSON.stringify(auth.user) : 'null'}</div>
      <div data-testid="loading">{auth.loading.toString()}</div>
      <div data-testid="error">{auth.error || 'null'}</div>
      <div data-testid="isAuthenticated">{auth.isAuthenticated.toString()}</div>
      <button data-testid="login" onClick={() => auth.login({ email: 'test@example.com', password: 'password' })}>
        Login
      </button>
      <button data-testid="logout" onClick={auth.logout}>Logout</button>
      <button data-testid="register" onClick={() => auth.register({ email: 'test@example.com', password: 'password' })}>
        Register
      </button>
      <button data-testid="refresh" onClick={auth.refreshUser}>Refresh</button>
      <button data-testid="clearError" onClick={auth.clearError}>Clear Error</button>
    </div>
  );
};

const renderWithAuthProvider = (onAuthChange) => {
  return render(
    <AuthProvider>
      <TestComponent onAuthChange={onAuthChange} />
    </AuthProvider>
  );
};

describe('AuthContext', () => {
  const originalConsoleLog = console.log;
  const originalConsoleError = console.error;

  beforeEach(() => {
    jest.clearAllMocks();
    console.log = jest.fn();
    console.error = jest.fn();
    jest.clearAllTimers();
    jest.useFakeTimers();
    // Setup default mock returns
    api.getAccessToken.mockReturnValue(null);
    tokenUtils.isRememberMeSession.mockReturnValue(false);
    tokenUtils.getStoredTokenExpiry.mockReturnValue(null);
    tokenUtils.isTokenExpired.mockReturnValue(false);
    tokenUtils.getTokenExpiryTime.mockReturnValue(Date.now() + 60000);
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
    jest.runOnlyPendingTimers();
    jest.useRealTimers();
    jest.restoreAllMocks();
  });

  describe('useAuth hook', () => {
    it('should throw error when used outside AuthProvider', () => {
      const TestComponentOutside = () => {
        useAuth(); // This should throw
        return <div>Test</div>;
      };

      // Suppress the React error boundary warning for this test
      const originalError = console.error;
      console.error = jest.fn();

      expect(() => render(<TestComponentOutside />)).toThrow('useAuth must be used within an AuthProvider');

      console.error = originalError;
    });
  });

  describe('AuthProvider initialization', () => {
    it('should render with initial loading state', async () => {
      api.getAccessToken.mockReturnValue(null);
      renderWithAuthProvider();

      expect(screen.getByTestId('user')).toHaveTextContent('null');
      expect(screen.getByTestId('loading')).toHaveTextContent('true');
      expect(screen.getByTestId('error')).toHaveTextContent('null');
      expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('false');

      // Wait for loading to complete
      await act(async () => {
        jest.runAllTimers();
      });

      await waitFor(() => {
        expect(screen.getByTestId('loading')).toHaveTextContent('false');
      });
    });

    it('should check authentication on mount with valid token', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      const mockToken = 'valid-token';
      const mockExpiryTime = Date.now() + 60 * 60 * 1000;

      api.getAccessToken.mockReturnValue(mockToken);
      tokenUtils.isRememberMeSession.mockReturnValue(false);
      tokenUtils.getStoredTokenExpiry.mockReturnValue(null);
      tokenUtils.getTokenExpiryTime.mockReturnValue(mockExpiryTime);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      await waitFor(() => {
        expect(screen.getByTestId('user')).toHaveTextContent(JSON.stringify(mockUser));
        expect(screen.getByTestId('loading')).toHaveTextContent('false');
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('true');
      });

      expect(authService.getCurrentUser).toHaveBeenCalled();
      expect(tokenUtils.storeTokenExpiry).toHaveBeenCalledWith(mockExpiryTime);
    });

    it('should clear session when token is expired', async () => {
      const mockToken = 'expired-token';
      const mockExpiryTime = Date.now() - 60 * 60 * 1000;

      api.getAccessToken.mockReturnValue(mockToken);
      tokenUtils.isRememberMeSession.mockReturnValue(false);
      tokenUtils.getStoredTokenExpiry.mockReturnValue(null);
      tokenUtils.getTokenExpiryTime.mockReturnValue(mockExpiryTime);
      tokenUtils.isTokenExpired.mockReturnValue(true);

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      await waitFor(() => {
        expect(screen.getByTestId('loading')).toHaveTextContent('false');
      });

      expect(api.clearTokens).toHaveBeenCalled();
      expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
      expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
      expect(console.log).toHaveBeenCalledWith('Token expired, clearing session');
    });

    it('should handle auth check failure', async () => {
      api.getAccessToken.mockReturnValue('invalid-token');
      authService.getCurrentUser.mockRejectedValue(new Error('Auth failed'));

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      await waitFor(() => {
        expect(screen.getByTestId('loading')).toHaveTextContent('false');
      });

      expect(api.clearTokens).toHaveBeenCalled();
      expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
      expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
      expect(console.error).toHaveBeenCalledWith('Auth check failed:', expect.any(Error));
    });
  });

  describe('Login functionality', () => {
    it('should handle successful login', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      const mockToken = 'new-token';
      const mockExpiryTime = Date.now() + 60 * 60 * 1000;

      api.getAccessToken.mockReturnValue(null);
      authService.login.mockResolvedValue({ user: mockUser, accessToken: mockToken });
      tokenUtils.getTokenExpiryTime.mockReturnValue(mockExpiryTime);

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      await waitFor(() => {
        expect(screen.getByTestId('loading')).toHaveTextContent('false');
      });

      await act(async () => {
        screen.getByTestId('login').click();
      });

      await waitFor(() => {
        expect(screen.getByTestId('user')).toHaveTextContent(JSON.stringify(mockUser));
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('true');
        expect(screen.getByTestId('error')).toHaveTextContent('null');
      });

      expect(authService.login).toHaveBeenCalledWith({ email: 'test@example.com', password: 'password' });
      expect(tokenUtils.setRememberMeFlag).toHaveBeenCalledWith(false);
      expect(tokenUtils.storeTokenExpiry).toHaveBeenCalledWith(mockExpiryTime);
    });

    it('should handle login failure', async () => {
      api.getAccessToken.mockReturnValue(null);
      authService.login.mockRejectedValue(new Error('Invalid credentials'));

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      await act(async () => {
        screen.getByTestId('login').click();
      });

      await waitFor(() => {
        expect(screen.getByTestId('error')).toHaveTextContent('Invalid credentials');
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('false');
      });
    });
  });

  describe('Registration functionality', () => {
    it('should handle successful registration with auto-login', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      const mockToken = 'new-token';
      const mockExpiryTime = Date.now() + 60 * 60 * 1000;

      api.getAccessToken.mockReturnValue(null);
      authService.register.mockResolvedValue({ success: true });
      authService.login.mockResolvedValue({ user: mockUser, accessToken: mockToken });
      tokenUtils.getTokenExpiryTime.mockReturnValue(mockExpiryTime);

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      await act(async () => {
        screen.getByTestId('register').click();
      });

      await waitFor(() => {
        expect(screen.getByTestId('user')).toHaveTextContent(JSON.stringify(mockUser));
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('true');
      });

      expect(authService.register).toHaveBeenCalledWith({ email: 'test@example.com', password: 'password' });
      expect(authService.login).toHaveBeenCalledWith({ email: 'test@example.com', password: 'password' });
    });

    it('should handle registration failure', async () => {
      api.getAccessToken.mockReturnValue(null);
      authService.register.mockRejectedValue(new Error('Registration failed'));

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      await act(async () => {
        screen.getByTestId('register').click();
      });

      await waitFor(() => {
        expect(screen.getByTestId('error')).toHaveTextContent('Registration failed');
      });
    });
  });

  describe('RefreshUser functionality', () => {
    it('should handle successful user refresh', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      const updatedUser = { id: '1', email: 'test@example.com', name: 'Test User' };

      api.getAccessToken.mockReturnValue('valid-token');
      tokenUtils.isRememberMeSession.mockReturnValue(false);
      tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60 * 60 * 1000);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser
        .mockResolvedValueOnce(mockUser) // Initial auth check
        .mockResolvedValueOnce(updatedUser); // Refresh call

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      // Wait for initial auth check
      await waitFor(() => {
        expect(screen.getByTestId('user')).toHaveTextContent(JSON.stringify(mockUser));
      });

      await act(async () => {
        screen.getByTestId('refresh').click();
      });

      await waitFor(() => {
        expect(screen.getByTestId('user')).toHaveTextContent(JSON.stringify(updatedUser));
      });

      expect(authService.getCurrentUser).toHaveBeenCalledTimes(2);
    });

    it('should handle refresh user failure', async () => {
      api.getAccessToken.mockReturnValue(null);
      authService.getCurrentUser.mockRejectedValue(new Error('Failed to refresh'));

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      await act(async () => {
        screen.getByTestId('refresh').click();
      });

      await waitFor(() => {
        expect(screen.getByTestId('error')).toHaveTextContent('Failed to refresh');
      });
    });
  });

  describe('Error handling', () => {
    it('should clear error when clearError is called', async () => {
      api.getAccessToken.mockReturnValue(null);
      authService.login.mockRejectedValue(new Error('Login failed'));

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      // Trigger error
      await act(async () => {
        screen.getByTestId('login').click();
      });

      await waitFor(() => {
        expect(screen.getByTestId('error')).toHaveTextContent('Login failed');
      });

      // Clear error
      await act(async () => {
        screen.getByTestId('clearError').click();
      });

      expect(screen.getByTestId('error')).toHaveTextContent('null');
    });
  });

  describe('Session timeout', () => {
    it('should setup session timeout and auto-logout when expired', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      const mockToken = 'valid-token';
      const mockExpiryTime = Date.now() + 1000; // 1 second from now

      api.getAccessToken.mockReturnValue(mockToken);
      tokenUtils.isRememberMeSession.mockReturnValue(false);
      tokenUtils.getStoredTokenExpiry.mockReturnValue(mockExpiryTime);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);
      authService.logout.mockResolvedValue();

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      // Wait for initial auth check
      await waitFor(() => {
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('true');
      });

      // Fast-forward time to trigger timeout
      await act(async () => {
        jest.advanceTimersByTime(1001);
      });

      await waitFor(() => {
        expect(screen.getByTestId('user')).toHaveTextContent('null');
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('false');
      });

      expect(console.log).toHaveBeenCalledWith('Session timeout - automatically logging out');
      expect(authService.logout).toHaveBeenCalled();
      expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
      expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
    });

    it('should handle session timeout logout error', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      const mockToken = 'valid-token';
      const mockExpiryTime = Date.now() + 1000;

      api.getAccessToken.mockReturnValue(mockToken);
      tokenUtils.isRememberMeSession.mockReturnValue(false);
      tokenUtils.getStoredTokenExpiry.mockReturnValue(mockExpiryTime);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);
      authService.logout.mockRejectedValue(new Error('Logout failed'));

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      // Wait for initial auth check
      await waitFor(() => {
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('true');
      });

      // Fast-forward time to trigger timeout
      await act(async () => {
        jest.advanceTimersByTime(1001);
      });

      await waitFor(() => {
        expect(screen.getByTestId('user')).toHaveTextContent('null');
      });

      expect(console.error).toHaveBeenCalledWith('Session timeout logout error:', expect.any(Error));
      expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
      expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
    });
  });

  describe('Multi-tab synchronization', () => {
    it('should logout when token is removed in another tab', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      
      // Setup initial authenticated state
      api.getAccessToken.mockReturnValue('valid-token');
      tokenUtils.isRememberMeSession.mockReturnValue(false);
      tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60 * 60 * 1000);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      // Wait for initial auth check
      await waitFor(() => {
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('true');
      });

      // Simulate storage event (token removed in another tab)
      const storageEvent = new StorageEvent('storage', {
        key: 'authToken',
        newValue: null,
        oldValue: 'some-token'
      });

      await act(async () => {
        window.dispatchEvent(storageEvent);
      });

      await waitFor(() => {
        expect(screen.getByTestId('user')).toHaveTextContent('null');
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('false');
        expect(screen.getByTestId('error')).toHaveTextContent('null');
      });

      expect(console.log).toHaveBeenCalledWith('Token removed in another tab - logging out');
      expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
      expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
    });

    it('should ignore storage events for other keys', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      
      // Setup initial authenticated state
      api.getAccessToken.mockReturnValue('valid-token');
      tokenUtils.isRememberMeSession.mockReturnValue(false);
      tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60 * 60 * 1000);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);

      renderWithAuthProvider();

      await act(async () => {
        jest.runAllTimers();
      });

      // Wait for initial auth check
      await waitFor(() => {
        expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('true');
      });

      // Simulate storage event for different key
      const storageEvent = new StorageEvent('storage', {
        key: 'someOtherKey',
        newValue: null,
        oldValue: 'some-value'
      });

      await act(async () => {
        window.dispatchEvent(storageEvent);
      });

      // User should still be authenticated
      expect(screen.getByTestId('isAuthenticated')).toHaveTextContent('true');
      expect(tokenUtils.clearRememberMeFlag).not.toHaveBeenCalled();
    });
  });

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