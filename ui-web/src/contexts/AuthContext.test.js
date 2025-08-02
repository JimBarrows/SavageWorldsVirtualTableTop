import React from 'react';
import { render, act, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';
import { AuthProvider, useAuth } from './AuthContext';
import { authService } from '../services';

// Mock the auth service
jest.mock('../services', () => ({
  authService: {
    login: jest.fn(),
    logout: jest.fn(),
    getCurrentUser: jest.fn(),
    register: jest.fn()
  }
}));

// Mock the API module
jest.mock('../services/api', () => ({
  setTokens: jest.fn(),
  clearTokens: jest.fn(),
  getAccessToken: jest.fn()
}));

// Mock token utils
jest.mock('../utils/tokenUtils', () => ({
  setRememberMeFlag: jest.fn(),
  getRememberMeFlag: jest.fn(),
  clearRememberMeFlag: jest.fn(),
  isRememberMeSession: jest.fn(),
  getTokenExpiryTime: jest.fn(),
  isTokenExpired: jest.fn()
}));

// Test component to access context
const TestComponent = ({ onAuthChange }) => {
  const auth = useAuth();
  
  React.useEffect(() => {
    onAuthChange(auth);
  }, [auth, onAuthChange]);
  
  return <div>Test Component</div>;
};

describe('AuthContext - Remember Me Functionality', () => {
  let authContextValue;
  const mockOnAuthChange = jest.fn((auth) => {
    authContextValue = auth;
  });

  beforeEach(() => {
    jest.clearAllMocks();
    authContextValue = null;
  });

  describe('login with remember me', () => {
    it('sets remember me flag when login includes rememberMe option', async () => {
      const { setRememberMeFlag } = require('../utils/tokenUtils');
      
      authService.login.mockResolvedValue({
        user: { id: 1, email: 'test@example.com' },
        accessToken: 'access-token',
        refreshToken: 'refresh-token'
      });

      render(
        <AuthProvider>
          <TestComponent onAuthChange={mockOnAuthChange} />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContextValue).toBeTruthy();
      });

      const credentials = { 
        email: 'test@example.com', 
        password: 'password',
        rememberMe: true 
      };

      await act(async () => {
        const result = await authContextValue.login(credentials);
        expect(result.success).toBe(true);
      });

      expect(setRememberMeFlag).toHaveBeenCalledWith(true);
    });

    it('does not set remember me flag when rememberMe is false', async () => {
      const { setRememberMeFlag } = require('../utils/tokenUtils');
      
      authService.login.mockResolvedValue({
        user: { id: 1, email: 'test@example.com' },
        accessToken: 'access-token',
        refreshToken: 'refresh-token'
      });

      render(
        <AuthProvider>
          <TestComponent onAuthChange={mockOnAuthChange} />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContextValue).toBeTruthy();
      });

      const credentials = { 
        email: 'test@example.com', 
        password: 'password',
        rememberMe: false 
      };

      await act(async () => {
        const result = await authContextValue.login(credentials);
        expect(result.success).toBe(true);
      });

      expect(setRememberMeFlag).toHaveBeenCalledWith(false);
    });

    it('does not set remember me flag when rememberMe is not provided', async () => {
      const { setRememberMeFlag } = require('../utils/tokenUtils');
      
      authService.login.mockResolvedValue({
        user: { id: 1, email: 'test@example.com' },
        accessToken: 'access-token',
        refreshToken: 'refresh-token'
      });

      render(
        <AuthProvider>
          <TestComponent onAuthChange={mockOnAuthChange} />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContextValue).toBeTruthy();
      });

      const credentials = { 
        email: 'test@example.com', 
        password: 'password'
      };

      await act(async () => {
        const result = await authContextValue.login(credentials);
        expect(result.success).toBe(true);
      });

      expect(setRememberMeFlag).toHaveBeenCalledWith(false);
    });
  });

  describe('logout with remember me', () => {
    it('clears remember me flag on logout', async () => {
      const { clearRememberMeFlag } = require('../utils/tokenUtils');
      
      authService.logout.mockResolvedValue();

      render(
        <AuthProvider>
          <TestComponent onAuthChange={mockOnAuthChange} />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContextValue).toBeTruthy();
      });

      await act(async () => {
        await authContextValue.logout();
      });

      expect(clearRememberMeFlag).toHaveBeenCalled();
    });
  });

  describe('token validation with remember me', () => {
    it('checks token expiry on context initialization', async () => {
      const { getAccessToken } = require('../services/api');
      const { isRememberMeSession, getTokenExpiryTime, isTokenExpired } = require('../utils/tokenUtils');
      
      getAccessToken.mockReturnValue('mock-token');
      isRememberMeSession.mockReturnValue(true);
      getTokenExpiryTime.mockReturnValue(Date.now() + 60000); // 1 minute future
      isTokenExpired.mockReturnValue(false);
      
      authService.getCurrentUser.mockResolvedValue({
        id: 1,
        email: 'test@example.com'
      });

      render(
        <AuthProvider>
          <TestComponent onAuthChange={mockOnAuthChange} />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContextValue?.user).toBeTruthy();
      });

      expect(isRememberMeSession).toHaveBeenCalled();
      expect(getTokenExpiryTime).toHaveBeenCalledWith('mock-token', true);
      expect(isTokenExpired).toHaveBeenCalled();
    });

    it('clears session when remember me token is expired', async () => {
      const { getAccessToken, clearTokens } = require('../services/api');
      const { isRememberMeSession, getTokenExpiryTime, isTokenExpired, clearRememberMeFlag } = require('../utils/tokenUtils');
      
      getAccessToken.mockReturnValue('mock-token');
      isRememberMeSession.mockReturnValue(true);
      getTokenExpiryTime.mockReturnValue(Date.now() - 60000); // 1 minute past
      isTokenExpired.mockReturnValue(true);

      render(
        <AuthProvider>
          <TestComponent onAuthChange={mockOnAuthChange} />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContextValue).toBeTruthy();
      });

      expect(clearTokens).toHaveBeenCalled();
      expect(clearRememberMeFlag).toHaveBeenCalled();
    });
  });
});