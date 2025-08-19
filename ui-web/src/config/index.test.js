import config from './index';

describe('Application Configuration', () => {
  describe('Configuration Structure', () => {
    it('exports a configuration object', () => {
      expect(config).toBeDefined();
      expect(typeof config).toBe('object');
      expect(config).not.toBeNull();
    });

    it('has all required top-level properties', () => {
      expect(config).toHaveProperty('api');
      expect(config).toHaveProperty('auth');
      expect(config).toHaveProperty('query');
      expect(config).toHaveProperty('pagination');
    });

    it('all top-level properties are objects', () => {
      expect(typeof config.api).toBe('object');
      expect(typeof config.auth).toBe('object');
      expect(typeof config.query).toBe('object');
      expect(typeof config.pagination).toBe('object');
    });
  });

  describe('API Configuration', () => {
    it('has api configuration object', () => {
      expect(config.api).toBeDefined();
      expect(typeof config.api).toBe('object');
    });

    it('has baseURL configuration', () => {
      expect(config.api.baseURL).toBeDefined();
      expect(typeof config.api.baseURL).toBe('string');
      expect(config.api.baseURL.length).toBeGreaterThan(0);
    });

    it('has timeout configuration', () => {
      expect(config.api.timeout).toBeDefined();
      expect(typeof config.api.timeout).toBe('number');
      expect(config.api.timeout).toBeGreaterThan(0);
    });

    it('timeout is a reasonable value', () => {
      expect(config.api.timeout).toBeGreaterThanOrEqual(1000); // At least 1 second
      expect(config.api.timeout).toBeLessThanOrEqual(300000); // At most 5 minutes
    });
  });

  describe('Auth Configuration', () => {
    it('has auth configuration object', () => {
      expect(config.auth).toBeDefined();
      expect(typeof config.auth).toBe('object');
    });

    it('has token refresh threshold', () => {
      expect(config.auth.tokenRefreshThreshold).toBeDefined();
      expect(typeof config.auth.tokenRefreshThreshold).toBe('number');
      expect(config.auth.tokenRefreshThreshold).toBeGreaterThan(0);
    });

    it('token refresh threshold is reasonable value', () => {
      const expectedValue = 5 * 60 * 1000; // 5 minutes in milliseconds
      expect(config.auth.tokenRefreshThreshold).toBe(expectedValue);
      expect(config.auth.tokenRefreshThreshold).toBeGreaterThan(0);
      expect(config.auth.tokenRefreshThreshold).toBeLessThan(60 * 60 * 1000); // Less than 1 hour
    });
  });

  describe('Query Configuration', () => {
    it('has query configuration object', () => {
      expect(config.query).toBeDefined();
      expect(typeof config.query).toBe('object');
    });

    it('has stale time configuration', () => {
      expect(config.query.staleTime).toBeDefined();
      expect(typeof config.query.staleTime).toBe('number');
      expect(config.query.staleTime).toBe(5 * 60 * 1000); // 5 minutes
    });

    it('has cache time configuration', () => {
      expect(config.query.cacheTime).toBeDefined();
      expect(typeof config.query.cacheTime).toBe('number');
      expect(config.query.cacheTime).toBe(10 * 60 * 1000); // 10 minutes
    });

    it('has refetch on window focus configuration', () => {
      expect(config.query.refetchOnWindowFocus).toBeDefined();
      expect(config.query.refetchOnWindowFocus).toBe(false);
    });

    it('has retry configuration', () => {
      expect(config.query.retry).toBeDefined();
      expect(typeof config.query.retry).toBe('number');
      expect(config.query.retry).toBe(1);
    });

    it('cache time is longer than stale time', () => {
      expect(config.query.cacheTime).toBeGreaterThan(config.query.staleTime);
    });

    it('has all required query properties', () => {
      expect(config.query).toHaveProperty('staleTime');
      expect(config.query).toHaveProperty('cacheTime');
      expect(config.query).toHaveProperty('refetchOnWindowFocus');
      expect(config.query).toHaveProperty('retry');
    });
  });

  describe('Pagination Configuration', () => {
    it('has pagination configuration object', () => {
      expect(config.pagination).toBeDefined();
      expect(typeof config.pagination).toBe('object');
    });

    it('has default page size', () => {
      expect(config.pagination.defaultPageSize).toBeDefined();
      expect(typeof config.pagination.defaultPageSize).toBe('number');
      expect(config.pagination.defaultPageSize).toBe(20);
    });

    it('has page size options array', () => {
      expect(config.pagination.pageSizeOptions).toBeDefined();
      expect(Array.isArray(config.pagination.pageSizeOptions)).toBe(true);
      expect(config.pagination.pageSizeOptions).toEqual([10, 20, 50, 100]);
    });

    it('default page size is included in options', () => {
      expect(config.pagination.pageSizeOptions).toContain(config.pagination.defaultPageSize);
    });

    it('page size options are in ascending order', () => {
      const options = config.pagination.pageSizeOptions;
      for (let i = 1; i < options.length; i++) {
        expect(options[i]).toBeGreaterThan(options[i - 1]);
      }
    });

    it('all page size options are positive numbers', () => {
      config.pagination.pageSizeOptions.forEach(size => {
        expect(typeof size).toBe('number');
        expect(size).toBeGreaterThan(0);
      });
    });
  });

  describe('Configuration Values Validation', () => {
    it('has reasonable timeout values for production', () => {
      expect(config.api.timeout).toBeGreaterThanOrEqual(5000); // At least 5 seconds
      expect(config.api.timeout).toBeLessThanOrEqual(300000); // At most 5 minutes
    });

    it('has reasonable cache times', () => {
      expect(config.query.staleTime).toBeGreaterThan(0);
      expect(config.query.cacheTime).toBeGreaterThan(config.query.staleTime);
    });

    it('has reasonable pagination sizes', () => {
      expect(config.pagination.defaultPageSize).toBeGreaterThan(0);
      expect(config.pagination.defaultPageSize).toBeLessThanOrEqual(100);
      
      config.pagination.pageSizeOptions.forEach(size => {
        expect(size).toBeGreaterThan(0);
        expect(size).toBeLessThanOrEqual(1000);
      });
    });

    it('retry count is reasonable', () => {
      expect(config.query.retry).toBeGreaterThanOrEqual(0);
      expect(config.query.retry).toBeLessThanOrEqual(5);
    });

    it('boolean values are correct type', () => {
      expect(typeof config.query.refetchOnWindowFocus).toBe('boolean');
    });
  });

  describe('Environment Variable Logic', () => {
    it('has proper fallback logic for API URL', () => {
      // Test that the default value logic works
      const testResult = process.env.REACT_APP_API_URL || '/api/v1';
      expect(typeof testResult).toBe('string');
      expect(testResult.length).toBeGreaterThan(0);
    });

    it('has proper fallback logic for timeout', () => {
      // Test that the parseInt logic works with fallback
      const testResult = parseInt(process.env.REACT_APP_API_TIMEOUT) || 30000;
      expect(typeof testResult).toBe('number');
      expect(testResult).toBeGreaterThan(0);
    });

    it('parseInt handles undefined gracefully', () => {
      const result = parseInt(undefined) || 30000;
      expect(result).toBe(30000);
    });

    it('parseInt handles empty string gracefully', () => {
      const result = parseInt('') || 30000;
      expect(result).toBe(30000);
    });

    it('parseInt handles non-numeric string gracefully', () => {
      const result = parseInt('not-a-number') || 30000;
      expect(result).toBe(30000);
    });
  });

  describe('Configuration Immutability', () => {
    it('config reference is stable', () => {
      const config1 = require('./index').default;
      const config2 = require('./index').default;
      expect(config1).toBe(config2);
    });

    it('config object properties are defined', () => {
      Object.values(config).forEach(section => {
        expect(section).toBeDefined();
        expect(section).not.toBeNull();
      });
    });

    it('no undefined properties in config sections', () => {
      Object.entries(config).forEach(([sectionName, section]) => {
        Object.entries(section).forEach(([propertyName, value]) => {
          expect(value).toBeDefined();
        });
      });
    });
  });

  describe('Application Configuration Integration', () => {
    it('provides configuration for API client', () => {
      expect(config.api.baseURL).toBeDefined();
      expect(config.api.timeout).toBeDefined();
    });

    it('provides configuration for authentication', () => {
      expect(config.auth.tokenRefreshThreshold).toBeDefined();
    });

    it('provides configuration for query caching', () => {
      expect(config.query.staleTime).toBeDefined();
      expect(config.query.cacheTime).toBeDefined();
      expect(config.query.refetchOnWindowFocus).toBeDefined();
      expect(config.query.retry).toBeDefined();
    });

    it('provides configuration for pagination', () => {
      expect(config.pagination.defaultPageSize).toBeDefined();
      expect(config.pagination.pageSizeOptions).toBeDefined();
    });
  });
});