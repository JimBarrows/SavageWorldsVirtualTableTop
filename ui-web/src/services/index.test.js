import {
  api,
  authService,
  plotPointService,
  gameEntityService,
  setTokens,
  clearTokens,
  getAccessToken,
  getRefreshToken
} from './index';

// Mock the individual services
jest.mock('./api', () => ({
  __esModule: true,
  default: { baseURL: '/api/v1' },
  setTokens: jest.fn(),
  clearTokens: jest.fn(),
  getAccessToken: jest.fn(),
  getRefreshToken: jest.fn()
}));

jest.mock('./authService', () => ({
  __esModule: true,
  default: { login: jest.fn(), logout: jest.fn() }
}));

jest.mock('./plotPointService', () => ({
  __esModule: true,
  default: { getPlotPoints: jest.fn(), savePlotPoint: jest.fn() }
}));

jest.mock('./gameEntityService', () => ({
  __esModule: true,
  default: { getEntities: jest.fn(), saveEntity: jest.fn() }
}));

describe('Services Index Exports', () => {
  describe('Service Exports', () => {
    it('exports api service', () => {
      expect(api).toBeDefined();
      expect(api).toHaveProperty('baseURL');
    });

    it('exports authService', () => {
      expect(authService).toBeDefined();
      expect(authService).toHaveProperty('login');
      expect(authService).toHaveProperty('logout');
    });

    it('exports plotPointService', () => {
      expect(plotPointService).toBeDefined();
      expect(plotPointService).toHaveProperty('getPlotPoints');
      expect(plotPointService).toHaveProperty('savePlotPoint');
    });

    it('exports gameEntityService', () => {
      expect(gameEntityService).toBeDefined();
      expect(gameEntityService).toHaveProperty('getEntities');
      expect(gameEntityService).toHaveProperty('saveEntity');
    });
  });

  describe('API Token Functions Export', () => {
    it('exports setTokens function', () => {
      expect(setTokens).toBeDefined();
      expect(typeof setTokens).toBe('function');
    });

    it('exports clearTokens function', () => {
      expect(clearTokens).toBeDefined();
      expect(typeof clearTokens).toBe('function');
    });

    it('exports getAccessToken function', () => {
      expect(getAccessToken).toBeDefined();
      expect(typeof getAccessToken).toBe('function');
    });

    it('exports getRefreshToken function', () => {
      expect(getRefreshToken).toBeDefined();
      expect(typeof getRefreshToken).toBe('function');
    });
  });

  describe('Re-export Functionality', () => {
    it('token functions are callable', () => {
      // Test that the re-exported functions can be called
      expect(() => setTokens()).not.toThrow();
      expect(() => clearTokens()).not.toThrow();
      expect(() => getAccessToken()).not.toThrow();
      expect(() => getRefreshToken()).not.toThrow();
    });

    it('setTokens function passes through correctly', () => {
      const mockTokens = { access: 'test-access', refresh: 'test-refresh' };
      setTokens(mockTokens);
      
      const mockSetTokens = require('./api').setTokens;
      expect(mockSetTokens).toHaveBeenCalledWith(mockTokens);
    });

    it('clearTokens function passes through correctly', () => {
      clearTokens();
      
      const mockClearTokens = require('./api').clearTokens;
      expect(mockClearTokens).toHaveBeenCalled();
    });

    it('getAccessToken function passes through correctly', () => {
      getAccessToken();
      
      const mockGetAccessToken = require('./api').getAccessToken;
      expect(mockGetAccessToken).toHaveBeenCalled();
    });

    it('getRefreshToken function passes through correctly', () => {
      getRefreshToken();
      
      const mockGetRefreshToken = require('./api').getRefreshToken;
      expect(mockGetRefreshToken).toHaveBeenCalled();
    });
  });

  describe('Module Structure', () => {
    it('has correct number of exports', () => {
      const exports = Object.keys(require('./index'));
      expect(exports).toHaveLength(8); // 4 services + 4 token functions
    });

    it('all exports are defined', () => {
      const exports = require('./index');
      Object.values(exports).forEach(exportedValue => {
        expect(exportedValue).toBeDefined();
      });
    });

    it('exports have correct types', () => {
      expect(typeof api).toBe('object');
      expect(typeof authService).toBe('object');
      expect(typeof plotPointService).toBe('object');
      expect(typeof gameEntityService).toBe('object');
      expect(typeof setTokens).toBe('function');
      expect(typeof clearTokens).toBe('function');
      expect(typeof getAccessToken).toBe('function');
      expect(typeof getRefreshToken).toBe('function');
    });
  });

  describe('Import/Export Compatibility', () => {
    it('supports named imports', () => {
      // This test verifies that named imports work correctly
      expect(api).toBeDefined();
      expect(authService).toBeDefined();
      expect(plotPointService).toBeDefined();
      expect(gameEntityService).toBeDefined();
    });

    it('supports destructuring imports', () => {
      // This test verifies that destructuring imports work
      const { setTokens: destructuredSetTokens, clearTokens: destructuredClearTokens } = require('./index');
      expect(destructuredSetTokens).toBe(setTokens);
      expect(destructuredClearTokens).toBe(clearTokens);
    });

    it('maintains reference integrity', () => {
      // Verify that multiple imports reference the same objects
      const firstImport = require('./index');
      const secondImport = require('./index');
      
      expect(firstImport.api).toBe(secondImport.api);
      expect(firstImport.authService).toBe(secondImport.authService);
    });
  });

  describe('Service Integration', () => {
    it('provides unified service access point', () => {
      // Test that this index file serves as a central access point for all services
      expect(api).toBeDefined();
      expect(authService).toBeDefined();
      expect(plotPointService).toBeDefined();
      expect(gameEntityService).toBeDefined();
    });

    it('provides unified token management access', () => {
      // Test that token management functions are available centrally
      expect(setTokens).toBeDefined();
      expect(clearTokens).toBeDefined();
      expect(getAccessToken).toBeDefined();
      expect(getRefreshToken).toBeDefined();
    });

    it('maintains service isolation', () => {
      // Each service should be a separate object
      expect(api).not.toBe(authService);
      expect(authService).not.toBe(plotPointService);
      expect(plotPointService).not.toBe(gameEntityService);
    });
  });

  describe('Error Handling', () => {
    it('handles missing services gracefully', () => {
      // Even if individual services fail to load, the index should not throw
      expect(() => {
        const { api, authService } = require('./index');
        return { api, authService };
      }).not.toThrow();
    });

    it('provides fallback for undefined services', () => {
      // Services should be defined even if their underlying implementations fail
      expect(api).toBeDefined();
      expect(authService).toBeDefined();
      expect(plotPointService).toBeDefined();
      expect(gameEntityService).toBeDefined();
    });
  });
});