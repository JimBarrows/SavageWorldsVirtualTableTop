import React, { createContext, useState, useContext, useEffect, useCallback } from 'react';
import PropTypes from 'prop-types';
import { authService } from '../services';
import { clearTokens, getAccessToken } from '../services/api';
import { 
  setRememberMeFlag, 
  clearRememberMeFlag, 
  isRememberMeSession, 
  getTokenExpiryTime, 
  isTokenExpired,
  storeTokenExpiry,
  getStoredTokenExpiry,
  clearStoredTokenExpiry
} from '../utils/tokenUtils';

const AuthContext = createContext({
  user: null,
  loading: true,
  error: null,
  login: async () => {},
  logout: async () => {},
  register: async () => {},
  refreshUser: async () => {},
  clearError: () => {}
});

export const useAuth = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return context;
};

export const AuthProvider = ({ children }) => {
  const [user, setUser] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [sessionTimeoutId, setSessionTimeoutId] = useState(null);

  // Clear error
  const clearError = useCallback(() => {
    setError(null);
  }, []);

  // Setup session timeout
  const setupSessionTimeout = useCallback((expiryTime) => {
    // Clear any existing timeout
    if (sessionTimeoutId) {
      clearTimeout(sessionTimeoutId);
    }

    const now = Date.now();
    const timeUntilExpiry = expiryTime - now;

    if (timeUntilExpiry > 0) {
      const timeoutId = setTimeout(async () => {
        console.log('Session timeout - automatically logging out');
        try {
          await authService.logout();
        } catch (err) {
          console.error('Session timeout logout error:', err);
        }
        setUser(null);
        setError(null);
        clearRememberMeFlag();
        clearStoredTokenExpiry();
      }, timeUntilExpiry);
      setSessionTimeoutId(timeoutId);
    }
  }, [sessionTimeoutId]);

  // Multi-tab logout synchronization using localStorage events
  useEffect(() => {
    const handleStorageChange = (e) => {
      // Listen for token removal from other tabs
      if (e.key === 'authToken' && e.newValue === null) {
        console.log('Token removed in another tab - logging out');
        setUser(null);
        setError(null);
        clearRememberMeFlag();
        clearStoredTokenExpiry();
        
        // Clear session timeout
        if (sessionTimeoutId) {
          clearTimeout(sessionTimeoutId);
          setSessionTimeoutId(null);
        }
      }
    };

    window.addEventListener('storage', handleStorageChange);
    return () => {
      window.removeEventListener('storage', handleStorageChange);
    };
  }, [sessionTimeoutId]);

  // Check if user is authenticated on mount
  useEffect(() => {
    const checkAuth = async () => {
      try {
        const token = getAccessToken();
        if (token) {
          // Check if token is expired based on remember me setting
          const isRememberMe = isRememberMeSession();
          const storedExpiry = getStoredTokenExpiry();
          const expiryTime = storedExpiry || getTokenExpiryTime(token, isRememberMe);
          
          if (isTokenExpired(expiryTime)) {
            console.log('Token expired, clearing session');
            clearTokens();
            clearRememberMeFlag();
            clearStoredTokenExpiry();
          } else {
            const userData = await authService.getCurrentUser();
            setUser(userData);
            // Store expiry if not already stored
            if (!storedExpiry) {
              storeTokenExpiry(expiryTime);
            }
            // Setup session timeout
            setupSessionTimeout(expiryTime);
          }
        }
      } catch (err) {
        console.error('Auth check failed:', err);
        clearTokens();
        clearRememberMeFlag();
        clearStoredTokenExpiry();
      } finally {
        setLoading(false);
      }
    };

    checkAuth();
  }, [setupSessionTimeout]);

  // Login function
  const login = useCallback(async (credentials) => {
    try {
      setError(null);
      setLoading(true);
      const { user, accessToken } = await authService.login(credentials);
      setUser(user);
      
      // Handle remember me functionality
      const rememberMe = credentials.rememberMe || false;
      setRememberMeFlag(rememberMe);
      
      // Calculate and store token expiry
      const expiryTime = getTokenExpiryTime(accessToken, rememberMe);
      storeTokenExpiry(expiryTime);
      
      // Setup session timeout
      setupSessionTimeout(expiryTime);
      
      return { success: true, user };
    } catch (err) {
      const errorMessage = err.message || 'Login failed';
      setError(errorMessage);
      return { success: false, error: errorMessage };
    } finally {
      setLoading(false);
    }
  }, [setupSessionTimeout]);

  // Register function
  const register = useCallback(async (userData) => {
    try {
      setError(null);
      setLoading(true);
      const response = await authService.register(userData);
      
      // Auto-login after successful registration
      if (userData.email && userData.password) {
        const loginResult = await login({ 
          email: userData.email, 
          password: userData.password 
        });
        return loginResult;
      }
      
      return { success: true, data: response };
    } catch (err) {
      const errorMessage = err.message || 'Registration failed';
      setError(errorMessage);
      return { success: false, error: errorMessage };
    } finally {
      setLoading(false);
    }
  }, [login]);

  // Logout function
  const logout = useCallback(async () => {
    try {
      setLoading(true);
      await authService.logout();
      setUser(null);
      setError(null);
      clearRememberMeFlag();
      clearStoredTokenExpiry();
      
      // Clear session timeout
      if (sessionTimeoutId) {
        clearTimeout(sessionTimeoutId);
        setSessionTimeoutId(null);
      }
    } catch (err) {
      console.error('Logout error:', err);
      // Still clear user even if logout fails
      setUser(null);
      clearRememberMeFlag();
      clearStoredTokenExpiry();
      
      // Clear session timeout even on error
      if (sessionTimeoutId) {
        clearTimeout(sessionTimeoutId);
        setSessionTimeoutId(null);
      }
    } finally {
      setLoading(false);
    }
  }, [sessionTimeoutId]);

  // Refresh user data
  const refreshUser = useCallback(async () => {
    try {
      setLoading(true);
      const userData = await authService.getCurrentUser();
      setUser(userData);
      return { success: true, user: userData };
    } catch (err) {
      const errorMessage = err.message || 'Failed to refresh user data';
      setError(errorMessage);
      return { success: false, error: errorMessage };
    } finally {
      setLoading(false);
    }
  }, []);

  const value = {
    user,
    loading,
    error,
    login,
    logout,
    register,
    refreshUser,
    clearError,
    isAuthenticated: !!user
  };

  return (
    <AuthContext.Provider value={value}>
      {children}
    </AuthContext.Provider>
  );
};

AuthProvider.propTypes = {
  children: PropTypes.node.isRequired
};

export default AuthContext;