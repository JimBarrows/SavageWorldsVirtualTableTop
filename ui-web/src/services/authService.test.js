import authService from './authService';
import api, { clearTokens } from './api';

// Mock the api module
jest.mock('./api');

describe('AuthService - Logout Functionality', () => {
  beforeEach(() => {
    jest.clearAllMocks();
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
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
      api.post.mockRejectedValue(new Error('Network error'));

      await authService.logout();

      expect(clearTokens).toHaveBeenCalledTimes(1);
      expect(consoleSpy).toHaveBeenCalledWith('Logout error:', expect.any(Error));

      consoleSpy.mockRestore();
    });

    it('should handle server errors gracefully', async () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
      const serverError = new Error('Server error');
      serverError.response = { status: 500, data: { message: 'Internal server error' } };
      
      api.post.mockRejectedValue(serverError);

      await expect(authService.logout()).resolves.not.toThrow();
      
      expect(clearTokens).toHaveBeenCalledTimes(1);
      expect(consoleSpy).toHaveBeenCalledWith('Logout error:', serverError);

      consoleSpy.mockRestore();
    });

    it('should handle network errors gracefully', async () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
      const networkError = new Error('Network error');
      
      api.post.mockRejectedValue(networkError);

      await expect(authService.logout()).resolves.not.toThrow();
      
      expect(clearTokens).toHaveBeenCalledTimes(1);
      expect(consoleSpy).toHaveBeenCalledWith('Logout error:', networkError);

      consoleSpy.mockRestore();
    });

    it('should handle timeout errors gracefully', async () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
      const timeoutError = new Error('Request timeout');
      timeoutError.code = 'TIMEOUT';
      
      api.post.mockRejectedValue(timeoutError);

      await expect(authService.logout()).resolves.not.toThrow();
      
      expect(clearTokens).toHaveBeenCalledTimes(1);
      expect(consoleSpy).toHaveBeenCalledWith('Logout error:', timeoutError);

      consoleSpy.mockRestore();
    });

    it('should complete logout process regardless of server response', async () => {
      // Test with successful response
      api.post.mockResolvedValueOnce({ data: { success: true } });
      await authService.logout();
      expect(clearTokens).toHaveBeenCalledTimes(1);

      // Reset and test with error response
      jest.clearAllMocks();
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
      api.post.mockRejectedValueOnce(new Error('Server unavailable'));
      
      await authService.logout();
      expect(clearTokens).toHaveBeenCalledTimes(1);

      consoleSpy.mockRestore();
    });

    it('should not throw errors to calling code', async () => {
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
      api.post.mockRejectedValue(new Error('Critical server error'));

      // Should not throw
      await expect(authService.logout()).resolves.toBeUndefined();

      consoleSpy.mockRestore();
    });
  });

  describe('logout integration with other methods', () => {
    it('should clear tokens before any other auth operations after logout', async () => {
      api.post.mockResolvedValue({ data: { success: true } });

      await authService.logout();

      expect(clearTokens).toHaveBeenCalledTimes(1);
      
      // Verify tokens are cleared before any subsequent operations
      api.get.mockRejectedValue(new Error('No auth token'));
      
      try {
        await authService.getCurrentUser();
      } catch (error) {
        expect(error.message).toBe('No auth token');
      }
    });
  });
});