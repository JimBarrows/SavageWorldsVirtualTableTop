import {
  setRememberMeFlag,
  getRememberMeFlag,
  clearRememberMeFlag,
  isRememberMeSession,
  clearStoredTokenExpiry,
  getStoredTokenExpiry,
  storeTokenExpiry
} from './tokenUtils';

// Mock localStorage
const localStorageMock = (() => {
  let store = {};
  return {
    getItem: jest.fn((key) => store[key] || null),
    setItem: jest.fn((key, value) => {
      store[key] = value.toString();
    }),
    removeItem: jest.fn((key) => {
      delete store[key];
    }),
    clear: jest.fn(() => {
      store = {};
    })
  };
})();

Object.defineProperty(window, 'localStorage', {
  value: localStorageMock
});

describe('TokenUtils - Logout Related Functionality', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    localStorageMock.clear();
  });

  describe('Remember Me Flag Management', () => {
    it('should set remember me flag correctly', () => {
      setRememberMeFlag(true);
      
      expect(localStorageMock.setItem).toHaveBeenCalledWith('rememberMe', 'true');
    });

    it('should set remember me flag to false correctly', () => {
      setRememberMeFlag(false);
      
      expect(localStorageMock.setItem).toHaveBeenCalledWith('rememberMe', 'false');
    });

    it('should get remember me flag correctly when true', () => {
      localStorageMock.getItem.mockReturnValue('true');
      
      const result = getRememberMeFlag();
      
      expect(result).toBe(true);
      expect(localStorageMock.getItem).toHaveBeenCalledWith('rememberMe');
    });

    it('should get remember me flag correctly when false', () => {
      localStorageMock.getItem.mockReturnValue('false');
      
      const result = getRememberMeFlag();
      
      expect(result).toBe(false);
      expect(localStorageMock.getItem).toHaveBeenCalledWith('rememberMe');
    });

    it('should get remember me flag as false when not set', () => {
      localStorageMock.getItem.mockReturnValue(null);
      
      const result = getRememberMeFlag();
      
      expect(result).toBe(false);
      expect(localStorageMock.getItem).toHaveBeenCalledWith('rememberMe');
    });

    it('should clear remember me flag', () => {
      clearRememberMeFlag();
      
      expect(localStorageMock.removeItem).toHaveBeenCalledWith('rememberMe');
    });

    it('should check if current session is remember me session', () => {
      localStorageMock.getItem.mockReturnValue('true');
      
      const result = isRememberMeSession();
      
      expect(result).toBe(true);
      expect(localStorageMock.getItem).toHaveBeenCalledWith('rememberMe');
    });

    it('should return false for remember me session when flag is not set', () => {
      localStorageMock.getItem.mockReturnValue(null);
      
      const result = isRememberMeSession();
      
      expect(result).toBe(false);
    });
  });

  describe('Token Expiry Management', () => {
    it('should store token expiry time', () => {
      const expiryTime = Date.now() + 3600000; // 1 hour from now
      
      storeTokenExpiry(expiryTime);
      
      expect(localStorageMock.setItem).toHaveBeenCalledWith('tokenExpiry', expiryTime.toString());
    });

    it('should get stored token expiry time', () => {
      const expiryTime = Date.now() + 3600000;
      localStorageMock.getItem.mockReturnValue(expiryTime.toString());
      
      const result = getStoredTokenExpiry();
      
      expect(result).toBe(expiryTime);
      expect(localStorageMock.getItem).toHaveBeenCalledWith('tokenExpiry');
    });

    it('should return null when no token expiry is stored', () => {
      localStorageMock.getItem.mockReturnValue(null);
      
      const result = getStoredTokenExpiry();
      
      expect(result).toBeNull();
    });

    it('should clear stored token expiry', () => {
      clearStoredTokenExpiry();
      
      expect(localStorageMock.removeItem).toHaveBeenCalledWith('tokenExpiry');
    });

    it('should handle invalid stored expiry time gracefully', () => {
      localStorageMock.getItem.mockReturnValue('invalid-number');
      
      const result = getStoredTokenExpiry();
      
      // Should return NaN but not throw
      expect(isNaN(result)).toBe(true);
    });
  });

  describe('Logout Session Clearing Integration', () => {
    it('should clear all logout-related data in sequence', () => {
      // Set up initial state
      setRememberMeFlag(true);
      storeTokenExpiry(Date.now() + 3600000);
      
      // Verify initial state is set
      expect(localStorageMock.setItem).toHaveBeenCalledWith('rememberMe', 'true');
      expect(localStorageMock.setItem).toHaveBeenCalledWith('tokenExpiry', expect.any(String));
      
      // Clear all logout-related data
      clearRememberMeFlag();
      clearStoredTokenExpiry();
      
      // Verify all data is cleared
      expect(localStorageMock.removeItem).toHaveBeenCalledWith('rememberMe');
      expect(localStorageMock.removeItem).toHaveBeenCalledWith('tokenExpiry');
    });

    it('should handle multiple clear operations safely', () => {
      // Clear multiple times should not cause errors
      clearRememberMeFlag();
      clearRememberMeFlag();
      clearStoredTokenExpiry();
      clearStoredTokenExpiry();
      
      expect(localStorageMock.removeItem).toHaveBeenCalledWith('rememberMe');
      expect(localStorageMock.removeItem).toHaveBeenCalledWith('tokenExpiry');
      // Should be called twice for each
      expect(localStorageMock.removeItem).toHaveBeenCalledTimes(4);
    });

    it('should verify session state after logout clearing', () => {
      // Set up remember me session
      setRememberMeFlag(true);
      storeTokenExpiry(Date.now() + 3600000);
      
      // Verify remember me session is active
      localStorageMock.getItem.mockReturnValue('true');
      expect(isRememberMeSession()).toBe(true);
      
      // Clear session data
      clearRememberMeFlag();
      clearStoredTokenExpiry();
      
      // Verify session is no longer remember me
      localStorageMock.getItem.mockReturnValue(null);
      expect(isRememberMeSession()).toBe(false);
      expect(getStoredTokenExpiry()).toBeNull();
    });
  });

  describe('Edge Cases and Error Handling', () => {
    it('should handle localStorage unavailable gracefully', () => {
      // Mock localStorage to throw error
      localStorageMock.setItem.mockImplementation(() => {
        throw new Error('localStorage unavailable');
      });
      
      expect(() => setRememberMeFlag(true)).toThrow('localStorage unavailable');
    });

    it('should handle localStorage get errors gracefully', () => {
      localStorageMock.getItem.mockImplementation(() => {
        throw new Error('localStorage read error');
      });
      
      expect(() => getRememberMeFlag()).toThrow('localStorage read error');
    });

    it('should handle localStorage remove errors gracefully', () => {
      localStorageMock.removeItem.mockImplementation(() => {
        throw new Error('localStorage remove error');
      });
      
      expect(() => clearRememberMeFlag()).toThrow('localStorage remove error');
    });
  });
});