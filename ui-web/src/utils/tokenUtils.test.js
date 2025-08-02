import { 
  setRememberMeFlag, 
  getRememberMeFlag, 
  clearRememberMeFlag,
  isRememberMeSession,
  getTokenExpiryTime,
  isTokenExpired 
} from './tokenUtils';

// Mock localStorage
const localStorageMock = {
  getItem: jest.fn(),
  setItem: jest.fn(),
  removeItem: jest.fn(),
  clear: jest.fn()
};
global.localStorage = localStorageMock;

describe('Token Utils - Remember Me Functionality', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('setRememberMeFlag', () => {
    it('sets remember me flag to true', () => {
      setRememberMeFlag(true);
      expect(localStorage.setItem).toHaveBeenCalledWith('rememberMe', 'true');
    });

    it('sets remember me flag to false', () => {
      setRememberMeFlag(false);
      expect(localStorage.setItem).toHaveBeenCalledWith('rememberMe', 'false');
    });
  });

  describe('getRememberMeFlag', () => {
    it('returns true when flag is set to true', () => {
      localStorage.getItem.mockReturnValue('true');
      expect(getRememberMeFlag()).toBe(true);
    });

    it('returns false when flag is set to false', () => {
      localStorage.getItem.mockReturnValue('false');
      expect(getRememberMeFlag()).toBe(false);
    });

    it('returns false when flag is not set', () => {
      localStorage.getItem.mockReturnValue(null);
      expect(getRememberMeFlag()).toBe(false);
    });
  });

  describe('clearRememberMeFlag', () => {
    it('removes remember me flag from localStorage', () => {
      clearRememberMeFlag();
      expect(localStorage.removeItem).toHaveBeenCalledWith('rememberMe');
    });
  });

  describe('isRememberMeSession', () => {
    it('returns true when remember me flag is true', () => {
      localStorage.getItem.mockReturnValue('true');
      expect(isRememberMeSession()).toBe(true);
    });

    it('returns false when remember me flag is false', () => {
      localStorage.getItem.mockReturnValue('false');
      expect(isRememberMeSession()).toBe(false);
    });

    it('returns false when no flag is set', () => {
      localStorage.getItem.mockReturnValue(null);
      expect(isRememberMeSession()).toBe(false);
    });
  });

  describe('getTokenExpiryTime', () => {
    it('returns extended expiry for remember me sessions', () => {
      const mockToken = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c';
      const isRememberMe = true;
      
      const expiryTime = getTokenExpiryTime(mockToken, isRememberMe);
      const currentTime = Date.now();
      const expectedExpiry = currentTime + (30 * 24 * 60 * 60 * 1000); // 30 days
      
      // Allow for small time differences in test execution
      expect(expiryTime).toBeGreaterThan(currentTime);
      expect(expiryTime).toBeLessThanOrEqual(expectedExpiry);
    });

    it('returns standard expiry for regular sessions', () => {
      const mockToken = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c';
      const isRememberMe = false;
      
      const expiryTime = getTokenExpiryTime(mockToken, isRememberMe);
      const currentTime = Date.now();
      const expectedExpiry = currentTime + (24 * 60 * 60 * 1000); // 24 hours
      
      // Allow for small time differences in test execution
      expect(expiryTime).toBeGreaterThan(currentTime);
      expect(expiryTime).toBeLessThanOrEqual(expectedExpiry);
    });

    it('uses token exp claim when available', () => {
      // Create a token with exp claim (expires in 1 hour)
      const futureExp = Math.floor(Date.now() / 1000) + 3600; // 1 hour from now
      const tokenWithExp = `eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.${btoa(JSON.stringify({ exp: futureExp }))}.signature`;
      
      const expiryTime = getTokenExpiryTime(tokenWithExp, false);
      const expectedExpiry = futureExp * 1000;
      
      expect(expiryTime).toBe(expectedExpiry);
    });
  });

  describe('isTokenExpired', () => {
    it('returns false for valid tokens', () => {
      const futureTime = Date.now() + 60000; // 1 minute in future
      expect(isTokenExpired(futureTime)).toBe(false);
    });

    it('returns true for expired tokens', () => {
      const pastTime = Date.now() - 60000; // 1 minute in past
      expect(isTokenExpired(pastTime)).toBe(true);
    });

    it('returns true for null expiry time', () => {
      expect(isTokenExpired(null)).toBe(true);
    });

    it('returns true for undefined expiry time', () => {
      expect(isTokenExpired(undefined)).toBe(true);
    });
  });
});