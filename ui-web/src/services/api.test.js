import axios from 'axios';
import MockAdapter from 'axios-mock-adapter';
import api, { setTokens, clearTokens, getAccessToken, getRefreshToken } from './api';
import config from '../config';

// Create a mock adapter for axios
const mock = new MockAdapter(axios);

// Mock localStorage
const localStorageMock = {
  getItem: jest.fn(),
  setItem: jest.fn(),
  removeItem: jest.fn(),
  clear: jest.fn()
};
Object.defineProperty(window, 'localStorage', { value: localStorageMock });

// Mock window.location
delete window.location;
window.location = { href: '' };

describe('API Module', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    localStorageMock.getItem.mockReturnValue(null);
    window.location.href = '';
  });

  afterEach(() => {
    mock.reset();
  });

  describe('Token Management', () => {
    it('should set tokens in memory and localStorage', () => {
      const accessToken = 'test-access-token';
      const refreshToken = 'test-refresh-token';

      setTokens(accessToken, refreshToken);

      expect(localStorageMock.setItem).toHaveBeenCalledWith('accessToken', accessToken);
      expect(localStorageMock.setItem).toHaveBeenCalledWith('refreshToken', refreshToken);
      expect(getAccessToken()).toBe(accessToken);
      expect(getRefreshToken()).toBe(refreshToken);
    });

    it('should clear tokens from memory and localStorage', () => {
      setTokens('token1', 'token2');
      clearTokens();

      expect(localStorageMock.removeItem).toHaveBeenCalledWith('accessToken');
      expect(localStorageMock.removeItem).toHaveBeenCalledWith('refreshToken');
      expect(getAccessToken()).toBeNull();
      expect(getRefreshToken()).toBeNull();
    });

    it('should get access token', () => {
      const token = 'my-access-token';
      setTokens(token, 'refresh');
      expect(getAccessToken()).toBe(token);
    });

    it('should get refresh token', () => {
      const token = 'my-refresh-token';
      setTokens('access', token);
      expect(getRefreshToken()).toBe(token);
    });
  });

  describe('Request Interceptor', () => {
    it('should add Authorization header when access token exists', async () => {
      const accessToken = 'bearer-token-123';
      setTokens(accessToken, 'refresh');

      // Create a new mock for the api instance
      const apiMock = new MockAdapter(api);
      apiMock.onGet('/test').reply(200, { data: 'test' });

      const response = await api.get('/test');

      expect(response.config.headers.Authorization).toBe(`Bearer ${accessToken}`);
    });

    it('should not override existing Authorization header', async () => {
      setTokens('default-token', 'refresh');

      const apiMock = new MockAdapter(api);
      apiMock.onGet('/test').reply(200, { data: 'test' });

      const customToken = 'custom-auth-token';
      const response = await api.get('/test', {
        headers: { Authorization: customToken }
      });

      expect(response.config.headers.Authorization).toBe(customToken);
    });

    it('should not add Authorization header when no token exists', async () => {
      clearTokens();

      const apiMock = new MockAdapter(api);
      apiMock.onGet('/test').reply(200, { data: 'test' });

      const response = await api.get('/test');

      expect(response.config.headers.Authorization).toBeUndefined();
    });
  });

  describe('Response Interceptor', () => {
    it('should pass through successful responses', async () => {
      const apiMock = new MockAdapter(api);
      const responseData = { message: 'success' };
      apiMock.onGet('/test').reply(200, responseData);

      const response = await api.get('/test');

      expect(response.data).toEqual(responseData);
      expect(response.status).toBe(200);
    });

    it('should handle 401 error and attempt token refresh', async () => {
      setTokens('old-access', 'valid-refresh');

      const apiMock = new MockAdapter(api);
      
      // First request fails with 401
      apiMock.onGet('/test').replyOnce(401);
      
      // Token refresh succeeds
      mock.onPost(`${config.api.baseURL}/auth/refresh`).reply(200, {
        accessToken: 'new-access',
        refreshToken: 'new-refresh'
      });
      
      // Retry of original request succeeds
      apiMock.onGet('/test').reply(200, { data: 'success' });

      const response = await api.get('/test');

      expect(response.data).toEqual({ data: 'success' });
      expect(getAccessToken()).toBe('new-access');
      expect(getRefreshToken()).toBe('new-refresh');
    });

    it('should redirect to login when token refresh fails', async () => {
      setTokens('old-access', 'invalid-refresh');

      const apiMock = new MockAdapter(api);
      
      // First request fails with 401
      apiMock.onGet('/test').replyOnce(401);
      
      // Token refresh fails
      mock.onPost(`${config.api.baseURL}/auth/refresh`).reply(401);

      try {
        await api.get('/test');
      } catch (error) {
        expect(error.response.status).toBe(401);
      }

      expect(window.location.href).toBe('/login');
      expect(getAccessToken()).toBeNull();
      expect(getRefreshToken()).toBeNull();
    });

    it('should not retry if request already has _retry flag', async () => {
      setTokens('access', 'refresh');

      const apiMock = new MockAdapter(api);
      apiMock.onGet('/test').reply(401);

      try {
        await api.get('/test', { _retry: true });
      } catch (error) {
        expect(error.response.status).toBe(401);
      }

      // Should not attempt refresh
      expect(window.location.href).toBe('');
    });

    it('should handle non-401 errors without token refresh', async () => {
      const apiMock = new MockAdapter(api);
      apiMock.onGet('/test').reply(500, { error: 'Server error' });

      try {
        await api.get('/test');
      } catch (error) {
        expect(error.response.status).toBe(500);
        expect(error.response.data).toEqual({ error: 'Server error' });
      }

      // Should not redirect
      expect(window.location.href).toBe('');
    });

    it('should handle network errors', async () => {
      const apiMock = new MockAdapter(api);
      apiMock.onGet('/test').networkError();

      try {
        await api.get('/test');
      } catch (error) {
        expect(error.message).toBe('Network Error');
      }
    });
  });

  describe('API Instance Configuration', () => {
    it('should use config values for baseURL and timeout', () => {
      expect(api.defaults.baseURL).toBe(config.api.baseURL);
      expect(api.defaults.timeout).toBe(config.api.timeout);
    });

    it('should set default Content-Type header', () => {
      expect(api.defaults.headers['Content-Type']).toBe('application/json');
    });
  });
});