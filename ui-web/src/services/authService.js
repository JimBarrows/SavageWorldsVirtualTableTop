import api, { setTokens, clearTokens } from './api';

const authService = {
  // Register a new user
  async register(userData) {
    try {
      const response = await api.post('/auth/register', userData);
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Login user
  async login(credentials) {
    try {
      console.log('Auth service login called with:', credentials);
      const response = await api.post('/auth/login', credentials);
      console.log('Auth service login response:', response.data);
      
      // Handle nested data structure from backend
      const responseData = response.data.data || response.data;
      const { access_token, refresh_token, user } = responseData;
      
      // Store tokens (backend uses snake_case)
      setTokens(access_token, refresh_token);
      
      return { user, accessToken: access_token, refreshToken: refresh_token };
    } catch (error) {
      console.error('Auth service login error:', error.response?.data || error);
      throw error.response?.data || error;
    }
  },

  // Logout user
  async logout() {
    try {
      await api.post('/auth/logout');
    } catch (error) {
      // Even if logout fails on server, clear local tokens
      console.error('Logout error:', error);
    } finally {
      clearTokens();
    }
  },

  // Get current user
  async getCurrentUser() {
    try {
      const response = await api.get('/auth/me');
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Refresh access token
  async refreshToken(refreshToken) {
    try {
      const response = await api.post('/auth/refresh', { refreshToken });
      const { accessToken, refreshToken: newRefreshToken } = response.data;
      
      // Update tokens
      setTokens(accessToken, newRefreshToken);
      
      return { accessToken, refreshToken: newRefreshToken };
    } catch (error) {
      clearTokens();
      throw error.response?.data || error;
    }
  },

  // Change password
  async changePassword(currentPassword, newPassword) {
    try {
      const response = await api.post('/auth/change-password', {
        currentPassword,
        newPassword
      });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Request password reset
  async requestPasswordReset(email) {
    try {
      const response = await api.post('/auth/forgot-password', { email });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  },

  // Reset password with token
  async resetPassword(token, newPassword) {
    try {
      const response = await api.post('/auth/reset-password', {
        token,
        newPassword
      });
      return response.data;
    } catch (error) {
      throw error.response?.data || error;
    }
  }
};

export default authService;