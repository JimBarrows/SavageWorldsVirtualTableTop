import authService from './authService';
import api, { setTokens, clearTokens } from './api';

// Mock the api module
jest.mock('./api');

// Mock console methods to keep test output clean
const originalConsoleLog = console.log;
const originalConsoleError = console.error;

beforeAll(() => {
  console.log = jest.fn();
  console.error = jest.fn();
});

afterAll(() => {
  console.log = originalConsoleLog;
  console.error = originalConsoleError;
});

describe('AuthService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('register', () => {
    it('should register a new user', async () => {
      const userData = {
        email: 'test@example.com',
        password: 'Password123!',
        name: 'Test User'
      };
      const mockResponse = { 
        id: '123', 
        email: userData.email, 
        name: userData.name 
      };
      api.post.mockResolvedValue({ data: mockResponse });

      const result = await authService.register(userData);

      expect(api.post).toHaveBeenCalledWith('/auth/register', userData);
      expect(result).toEqual(mockResponse);
    });

    it('should handle registration errors', async () => {
      const error = new Error('Registration failed');
      error.response = { data: { message: 'Email already exists' } };
      api.post.mockRejectedValue(error);

      const userData = { email: 'test@example.com', password: 'pass' };
      await expect(authService.register(userData)).rejects.toEqual({ 
        message: 'Email already exists' 
      });
    });

    it('should handle network errors without response data', async () => {
      const error = new Error('Network error');
      api.post.mockRejectedValue(error);

      const userData = { email: 'test@example.com', password: 'pass' };
      await expect(authService.register(userData)).rejects.toEqual(error);
    });
  });

  describe('login', () => {
    it('should login user and store tokens', async () => {
      const credentials = { email: 'test@example.com', password: 'password' };
      const mockResponse = {
        data: {
          access_token: 'access-token-123',
          refresh_token: 'refresh-token-456',
          user: { id: '1', email: credentials.email }
        }
      };
      api.post.mockResolvedValue({ data: mockResponse });

      const result = await authService.login(credentials);

      expect(api.post).toHaveBeenCalledWith('/auth/login', credentials);
      expect(setTokens).toHaveBeenCalledWith('access-token-123', 'refresh-token-456');
      expect(result).toEqual({
        user: mockResponse.data.user,
        accessToken: 'access-token-123',
        refreshToken: 'refresh-token-456'
      });
    });

    it('should handle nested data structure from backend', async () => {
      const credentials = { email: 'test@example.com', password: 'password' };
      const mockResponse = {
        data: {
          data: {
            access_token: 'access-token-789',
            refresh_token: 'refresh-token-012',
            user: { id: '2', email: credentials.email }
          }
        }
      };
      api.post.mockResolvedValue({ data: mockResponse.data });

      const result = await authService.login(credentials);

      expect(setTokens).toHaveBeenCalledWith('access-token-789', 'refresh-token-012');
      expect(result.accessToken).toBe('access-token-789');
    });

    it('should handle login errors', async () => {
      const error = new Error('Login failed');
      error.response = { data: { message: 'Invalid credentials' } };
      api.post.mockRejectedValue(error);

      const credentials = { email: 'test@example.com', password: 'wrong' };
      await expect(authService.login(credentials)).rejects.toEqual({ 
        message: 'Invalid credentials' 
      });
    });
  });

  describe('logout', () => {
    it('should call api.post with correct endpoint', async () => {
      api.post.mockResolvedValue({ data: { success: true } });

      await authService.logout();

      expect(api.post).toHaveBeenCalledWith('/auth/logout');
    });

    it('should call clearTokens even if api call succeeds', async () => {
      api.post.mockResolvedValue({ data: { success: true } });

      await authService.logout();

      expect(clearTokens).toHaveBeenCalledTimes(1);
    });

    it('should call clearTokens even if api call fails', async () => {
      api.post.mockRejectedValue(new Error('Network error'));

      await authService.logout();

      expect(clearTokens).toHaveBeenCalledTimes(1);
    });

    it('should handle server errors gracefully', async () => {
      const serverError = new Error('Server error');
      serverError.response = { status: 500, data: { message: 'Internal server error' } };
      
      api.post.mockRejectedValue(serverError);

      await expect(authService.logout()).resolves.not.toThrow();
      
      expect(clearTokens).toHaveBeenCalledTimes(1);
    });

    it('should not throw error even if logout fails', async () => {
      api.post.mockRejectedValue(new Error('Logout failed'));

      await expect(authService.logout()).resolves.not.toThrow();
      expect(clearTokens).toHaveBeenCalledTimes(1);
    });
  });

  describe('getCurrentUser', () => {
    it('should fetch current user information', async () => {
      const mockUser = { 
        id: '123', 
        email: 'user@example.com', 
        name: 'Current User' 
      };
      api.get.mockResolvedValue({ data: mockUser });

      const result = await authService.getCurrentUser();

      expect(api.get).toHaveBeenCalledWith('/auth/me');
      expect(result).toEqual(mockUser);
    });

    it('should handle errors when fetching current user', async () => {
      const error = new Error('Unauthorized');
      error.response = { data: { message: 'Token expired' } };
      api.get.mockRejectedValue(error);

      await expect(authService.getCurrentUser()).rejects.toEqual({ 
        message: 'Token expired' 
      });
    });
  });

  describe('refreshToken', () => {
    it('should refresh access token and update stored tokens', async () => {
      const refreshToken = 'old-refresh-token';
      const mockResponse = {
        data: {
          accessToken: 'new-access-token',
          refreshToken: 'new-refresh-token'
        }
      };
      api.post.mockResolvedValue(mockResponse);

      const result = await authService.refreshToken(refreshToken);

      expect(api.post).toHaveBeenCalledWith('/auth/refresh', { refreshToken });
      expect(setTokens).toHaveBeenCalledWith('new-access-token', 'new-refresh-token');
      expect(result).toEqual(mockResponse.data);
    });

    it('should clear tokens on refresh failure', async () => {
      const error = new Error('Refresh failed');
      error.response = { data: { message: 'Invalid refresh token' } };
      api.post.mockRejectedValue(error);

      await expect(authService.refreshToken('invalid-token')).rejects.toEqual({ 
        message: 'Invalid refresh token' 
      });
      expect(clearTokens).toHaveBeenCalledTimes(1);
    });
  });

  describe('changePassword', () => {
    it('should change user password', async () => {
      const currentPassword = 'oldPassword123';
      const newPassword = 'newPassword456';
      const mockResponse = { data: { message: 'Password changed successfully' } };
      api.post.mockResolvedValue(mockResponse);

      const result = await authService.changePassword(currentPassword, newPassword);

      expect(api.post).toHaveBeenCalledWith('/auth/change-password', {
        currentPassword,
        newPassword
      });
      expect(result).toEqual(mockResponse.data);
    });

    it('should handle password change errors', async () => {
      const error = new Error('Change failed');
      error.response = { data: { message: 'Current password is incorrect' } };
      api.post.mockRejectedValue(error);

      await expect(authService.changePassword('wrong', 'new')).rejects.toEqual({ 
        message: 'Current password is incorrect' 
      });
    });
  });

  describe('requestPasswordReset', () => {
    it('should request password reset', async () => {
      const email = 'user@example.com';
      const mockResponse = { 
        data: { message: 'Password reset email sent' } 
      };
      api.post.mockResolvedValue(mockResponse);

      const result = await authService.requestPasswordReset(email);

      expect(api.post).toHaveBeenCalledWith('/auth/forgot-password', { email });
      expect(result).toEqual(mockResponse.data);
    });

    it('should handle request password reset errors', async () => {
      const error = new Error('Request failed');
      error.response = { data: { message: 'Email not found' } };
      api.post.mockRejectedValue(error);

      await expect(authService.requestPasswordReset('unknown@example.com')).rejects.toEqual({ 
        message: 'Email not found' 
      });
    });
  });

  describe('resetPassword', () => {
    it('should reset password with token', async () => {
      const token = 'reset-token-123';
      const newPassword = 'newSecurePassword123';
      const mockResponse = { 
        data: { message: 'Password reset successful' } 
      };
      api.post.mockResolvedValue(mockResponse);

      const result = await authService.resetPassword(token, newPassword);

      expect(api.post).toHaveBeenCalledWith('/auth/reset-password', {
        token,
        newPassword
      });
      expect(result).toEqual(mockResponse.data);
    });

    it('should handle password reset errors', async () => {
      const error = new Error('Reset failed');
      error.response = { data: { message: 'Invalid or expired token' } };
      api.post.mockRejectedValue(error);

      await expect(authService.resetPassword('invalid', 'password')).rejects.toEqual({ 
        message: 'Invalid or expired token' 
      });
    });
  });

  // Additional test for edge cases
  describe('Edge Cases', () => {
    it('should handle undefined error response data', async () => {
      const error = new Error('Network error');
      api.post.mockRejectedValue(error);

      await expect(authService.login({ email: 'test', password: 'test' }))
        .rejects.toEqual(error);
    });

    it('should handle empty response data in login', async () => {
      api.post.mockResolvedValue({ data: {} });

      const result = await authService.login({ email: 'test', password: 'test' });

      expect(setTokens).toHaveBeenCalledWith(undefined, undefined);
      expect(result).toEqual({
        user: undefined,
        accessToken: undefined,
        refreshToken: undefined
      });
    });
  });
});