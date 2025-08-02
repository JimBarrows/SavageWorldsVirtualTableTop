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
import { AuthProvider, useAuth } from './AuthContext';
import { authService } from '../services';
import * as api from '../services/api';
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

  describe('logout with remember me', () => {
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
        return <div>Test</div>;
      };

      render(
        <AuthProvider>
          <TestComponent />
        </AuthProvider>
      );

      await waitFor(() => {
        expect(authContext?.user).toBeTruthy();
      });

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

      expect(api.clearTokens).toHaveBeenCalled();
      expect(tokenUtils.clearRememberMeFlag).toHaveBeenCalled();
      expect(tokenUtils.clearStoredTokenExpiry).toHaveBeenCalled();
    });
  });
});