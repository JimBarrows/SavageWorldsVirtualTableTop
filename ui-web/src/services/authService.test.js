import authService from './authService';
import api, { setTokens, clearTokens } from './api';

// Mock the api module
jest.mock('./api');

// Mock console methods
const originalConsoleLog = console.log;
const originalConsoleError = console.error;

describe('AuthService', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    console.log = jest.fn();
    console.error = jest.fn();
  });

  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  describe('register', () => {
    it('should register a new user successfully', async () => {
      const userData = {
        email: 'test@example.com',
        password: 'password123',
        name: 'Test User'
      };
      const mockResponse = {
        data: {
          id: '123',
          email: 'test@example.com',
          name: 'Test User'
        }
      };

      api.post.mockResolvedValue(mockResponse);

      const result = await authService.register(userData);

      expect(api.post).toHaveBeenCalledWith('/auth/register', userData);
      expect(result).toEqual(mockResponse.data);
    });

    it('should handle registration errors', async () => {
      const userData = { email: 'test@example.com', password: 'short' };
      const error = {
        response: {
          data: {
            message: 'Password too short'
          }
        }
      };

      api.post.mockRejectedValue(error);

      await expect(authService.register(userData)).rejects.toEqual({
        message: 'Password too short'
      });
    });

    it('should handle network errors without response', async () => {
      const userData = { email: 'test@example.com', password: 'password123' };
      const error = new Error('Network error');

      api.post.mockRejectedValue(error);

      await expect(authService.register(userData)).rejects.toEqual(error);
    });
  });

  describe('login', () => {
    it('should login user successfully with nested data structure', async () => {
      const credentials = { email: 'test@example.com', password: 'password123' };
      const mockResponse = {
        data: {
          data: {
            access_token: 'access123',
            refresh_token: 'refresh123',
            user: { id: '1', email: 'test@example.com' }
          }
        }
      };

      api.post.mockResolvedValue(mockResponse);

      const result = await authService.login(credentials);

      expect(api.post).toHaveBeenCalledWith('/auth/login', credentials);
      expect(setTokens).toHaveBeenCalledWith('access123', 'refresh123');
      expect(result).toEqual({
        user: { id: '1', email: 'test@example.com' },
        accessToken: 'access123',
        refreshToken: 'refresh123'
      });
      expect(console.log).toHaveBeenCalledWith('Auth service login called with:', credentials);
    });

    it('should login user successfully with flat data structure', async () => {
      const credentials = { email: 'test@example.com', password: 'password123' };
      const mockResponse = {
        data: {
          access_token: 'access456',
          refresh_token: 'refresh456',
          user: { id: '2', email: 'test@example.com' }
        }
      };

      api.post.mockResolvedValue(mockResponse);

      const result = await authService.login(credentials);

      expect(setTokens).toHaveBeenCalledWith('access456', 'refresh456');
      expect(result).toEqual({
        user: { id: '2', email: 'test@example.com' },
        accessToken: 'access456',
        refreshToken: 'refresh456'
      });
    });

    it('should handle login errors', async () => {
      const credentials = { email: 'test@example.com', password: 'wrong' };
      const error = {
        response: {
          data: {
            message: 'Invalid credentials'
          }
        }
      };

      api.post.mockRejectedValue(error);

      await expect(authService.login(credentials)).rejects.toEqual({
        message: 'Invalid credentials'
      });
      expect(console.error).toHaveBeenCalledWith('Auth service login error:', error.response.data);
    });

    it('should handle network errors during login', async () => {
      const credentials = { email: 'test@example.com', password: 'password123' };
      const error = new Error('Network error');

      api.post.mockRejectedValue(error);

      await expect(authService.login(credentials)).rejects.toEqual(error);
      expect(console.error).toHaveBeenCalledWith('Auth service login error:', error);
    });
  });

  describe('logout', () => {
    it('should logout user successfully', async () => {
      api.post.mockResolvedValue({ data: { message: 'Logged out' } });

      await authService.logout();

      expect(api.post).toHaveBeenCalledWith('/auth/logout');
      expect(clearTokens).toHaveBeenCalled();
    });

    it('should clear tokens even if logout fails on server', async () => {
      const error = new Error('Server error');
      api.post.mockRejectedValue(error);

      await authService.logout();

      expect(api.post).toHaveBeenCalledWith('/auth/logout');
      expect(clearTokens).toHaveBeenCalled();
      expect(console.error).toHaveBeenCalledWith('Logout error:', error);
    });
  });

  describe('getCurrentUser', () => {
    it('should fetch current user successfully', async () => {
      const mockUser = {
        data: {
          id: '123',
          email: 'test@example.com',
          name: 'Test User'
        }
      };

      api.get.mockResolvedValue(mockUser);

      const result = await authService.getCurrentUser();

      expect(api.get).toHaveBeenCalledWith('/auth/me');
      expect(result).toEqual(mockUser.data);
    });

    it('should handle errors when fetching current user', async () => {
      const error = {
        response: {
          data: {
            message: 'Unauthorized'
          }
        }
      };

      api.get.mockRejectedValue(error);

      await expect(authService.getCurrentUser()).rejects.toEqual({
        message: 'Unauthorized'
      });
    });

    it('should handle network errors when fetching current user', async () => {
      const error = new Error('Network error');

      api.get.mockRejectedValue(error);

      await expect(authService.getCurrentUser()).rejects.toEqual(error);
    });
  });

  describe('refreshToken', () => {
    it('should refresh token successfully', async () => {
      const oldRefreshToken = 'oldRefresh123';
      const mockResponse = {
        data: {
          accessToken: 'newAccess123',
          refreshToken: 'newRefresh123'
        }
      };

      api.post.mockResolvedValue(mockResponse);

      const result = await authService.refreshToken(oldRefreshToken);

      expect(api.post).toHaveBeenCalledWith('/auth/refresh', {
        refreshToken: oldRefreshToken
      });
      expect(setTokens).toHaveBeenCalledWith('newAccess123', 'newRefresh123');
      expect(result).toEqual({
        accessToken: 'newAccess123',
        refreshToken: 'newRefresh123'
      });
    });

    it('should clear tokens on refresh error', async () => {
      const oldRefreshToken = 'expiredToken';
      const error = {
        response: {
          data: {
            message: 'Invalid refresh token'
          }
        }
      };

      api.post.mockRejectedValue(error);

      await expect(authService.refreshToken(oldRefreshToken)).rejects.toEqual({
        message: 'Invalid refresh token'
      });
      expect(clearTokens).toHaveBeenCalled();
    });

    it('should handle network errors during token refresh', async () => {
      const oldRefreshToken = 'refresh123';
      const error = new Error('Network error');

      api.post.mockRejectedValue(error);

      await expect(authService.refreshToken(oldRefreshToken)).rejects.toEqual(error);
      expect(clearTokens).toHaveBeenCalled();
    });
  });

  describe('changePassword', () => {
    it('should change password successfully', async () => {
      const currentPassword = 'oldPassword123';
      const newPassword = 'newPassword456';
      const mockResponse = {
        data: {
          message: 'Password changed successfully'
        }
      };

      api.post.mockResolvedValue(mockResponse);

      const result = await authService.changePassword(currentPassword, newPassword);

      expect(api.post).toHaveBeenCalledWith('/auth/change-password', {
        currentPassword,
        newPassword
      });
      expect(result).toEqual(mockResponse.data);
    });

    it('should handle password change errors', async () => {
      const currentPassword = 'wrongPassword';
      const newPassword = 'newPassword456';
      const error = {
        response: {
          data: {
            message: 'Current password is incorrect'
          }
        }
      };

      api.post.mockRejectedValue(error);

      await expect(authService.changePassword(currentPassword, newPassword)).rejects.toEqual({
        message: 'Current password is incorrect'
      });
    });

    it('should handle network errors during password change', async () => {
      const error = new Error('Network error');

      api.post.mockRejectedValue(error);

      await expect(authService.changePassword('old', 'new')).rejects.toEqual(error);
    });
  });

  describe('requestPasswordReset', () => {
    it('should request password reset successfully', async () => {
      const email = 'test@example.com';
      const mockResponse = {
        data: {
          message: 'Password reset email sent'
        }
      };

      api.post.mockResolvedValue(mockResponse);

      const result = await authService.requestPasswordReset(email);

      expect(api.post).toHaveBeenCalledWith('/auth/forgot-password', { email });
      expect(result).toEqual(mockResponse.data);
    });

    it('should handle password reset request errors', async () => {
      const email = 'nonexistent@example.com';
      const error = {
        response: {
          data: {
            message: 'Email not found'
          }
        }
      };

      api.post.mockRejectedValue(error);

      await expect(authService.requestPasswordReset(email)).rejects.toEqual({
        message: 'Email not found'
      });
    });

    it('should handle network errors during password reset request', async () => {
      const error = new Error('Network error');

      api.post.mockRejectedValue(error);

      await expect(authService.requestPasswordReset('test@example.com')).rejects.toEqual(error);
    });
  });

  describe('resetPassword', () => {
    it('should reset password successfully', async () => {
      const token = 'resetToken123';
      const newPassword = 'newPassword789';
      const mockResponse = {
        data: {
          message: 'Password reset successfully'
        }
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
      const token = 'invalidToken';
      const newPassword = 'newPassword789';
      const error = {
        response: {
          data: {
            message: 'Invalid or expired token'
          }
        }
      };

      api.post.mockRejectedValue(error);

      await expect(authService.resetPassword(token, newPassword)).rejects.toEqual({
        message: 'Invalid or expired token'
      });
    });

    it('should handle network errors during password reset', async () => {
      const error = new Error('Network error');

      api.post.mockRejectedValue(error);

      await expect(authService.resetPassword('token', 'password')).rejects.toEqual(error);
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty credentials in login', async () => {
      const credentials = { email: '', password: '' };
      const error = {
        response: {
          data: {
            message: 'Email and password are required'
          }
        }
      };

      api.post.mockRejectedValue(error);

      await expect(authService.login(credentials)).rejects.toEqual({
        message: 'Email and password are required'
      });
    });

    it('should handle missing user data in login response', async () => {
      const credentials = { email: 'test@example.com', password: 'password123' };
      const mockResponse = {
        data: {
          access_token: 'access123',
          refresh_token: 'refresh123'
          // Missing user object
        }
      };

      api.post.mockResolvedValue(mockResponse);

      const result = await authService.login(credentials);

      expect(result).toEqual({
        user: undefined,
        accessToken: 'access123',
        refreshToken: 'refresh123'
      });
    });

    it('should handle malformed error response', async () => {
      const error = { message: 'Something went wrong' };

      api.post.mockRejectedValue(error);

      await expect(authService.register({})).rejects.toEqual(error);
    });
  });
});