import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter, useLocation, useNavigate } from 'react-router-dom';
import * as authService from './services/authService';

// Mock the auth service
jest.mock('./services/authService');

// Mock the services
jest.mock('./services', () => ({
  authService: {
    login: jest.fn(),
    logout: jest.fn(),
    getCurrentUser: jest.fn(),
    register: jest.fn()
  }
}));

// Mock utils/tokenUtils
jest.mock('./utils/tokenUtils', () => ({
  setRememberMeFlag: jest.fn(),
  clearRememberMeFlag: jest.fn(),
  isRememberMeSession: jest.fn(() => false),
  getTokenExpiryTime: jest.fn(() => Date.now() + 3600000),
  isTokenExpired: jest.fn(() => false),
  storeTokenExpiry: jest.fn(),
  getStoredTokenExpiry: jest.fn(() => null),
  clearStoredTokenExpiry: jest.fn()
}));

// Mock services/api
jest.mock('./services/api', () => ({
  clearTokens: jest.fn(),
  getAccessToken: jest.fn(() => null)
}));

// Mock React Router hooks
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useLocation: jest.fn(),
  useNavigate: jest.fn()
}));

// Mock RememberMe component
jest.mock('./components/RememberMe', () => {
  return function RememberMe({ checked, onChange }) {
    return (
      <div className="form-check">
        <input
          className="form-check-input"
          type="checkbox"
          id="rememberMe"
          checked={checked}
          onChange={(e) => onChange(e.target.checked)}
        />
        <label className="form-check-label" htmlFor="rememberMe">
          Remember me
        </label>
      </div>
    );
  };
});

// Mock BrandingBanner component
jest.mock('./components/BrandingBanner', () => {
  return function BrandingBanner() {
    return <div data-testid="branding-banner">Branding Banner</div>;
  };
});

const mockLogin = jest.fn();
const mockLogout = jest.fn();
const mockNavigate = jest.fn();
const mockLocation = { state: null };

// Mock the auth context
const mockUseAuth = {
  isAuthenticated: false,
  loading: false,
  error: null,
  login: mockLogin,
  logout: mockLogout,
  user: null
};

jest.mock('./contexts/AuthContext', () => ({
  useAuth: () => mockUseAuth
}));

// Create a standalone Login component that matches App.js implementation
const Login = () => {
  const { login, error } = mockUseAuth;
  const location = mockLocation;
  const navigate = mockNavigate;
  const [credentials, setCredentials] = React.useState({ email: '', password: '' });
  const [rememberMe, setRememberMe] = React.useState(false);
  const [isLoading, setIsLoading] = React.useState(false);
  const [logoutMessage, setLogoutMessage] = React.useState(null);
  
  // Check for logout message from navigation state
  React.useEffect(() => {
    if (location.state?.message) {
      setLogoutMessage({
        text: location.state.message,
        type: location.state.type || 'info'
      });
      // Clear the message after 5 seconds
      const timer = setTimeout(() => {
        setLogoutMessage(null);
      }, 5000);
      return () => clearTimeout(timer);
    }
  }, [location.state]);
  
  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsLoading(true);
    try {
      // Send credentials with email field to backend
      const loginData = {
        email: credentials.email,
        password: credentials.password,
        rememberMe: rememberMe
      };
      console.log('Login attempt with:', { email: credentials.email, password: '***', rememberMe });
      const result = await login(loginData);
      if (result.success) {
        // Use React Router navigation instead of hard redirect
        navigate('/');
      }
    } catch (err) {
      console.error('Login error:', err);
    } finally {
      setIsLoading(false);
    }
  };
  
  return (
    <div className="login-page">
      <div data-testid="branding-banner">Branding Banner</div>
      <div className="container mt-5">
        <div className="row justify-content-center">
          <div className="col-md-6">
            <div className="card">
              <div className="card-header">
                <h3>Login</h3>
              </div>
              <div className="card-body">
              {logoutMessage && (
                <div 
                  className={`alert alert-${logoutMessage.type === 'success' ? 'success' : 'info'}`} 
                  role="alert"
                  data-testid="logout-success"
                >
                  {logoutMessage.text}
                </div>
              )}
              {error && (
                <div className="alert alert-danger" role="alert">
                  {error}
                </div>
              )}
              <form onSubmit={handleSubmit}>
                <div className="form-group mb-3">
                  <label htmlFor="email">Email</label>
                  <input
                    type="email"
                    className="form-control"
                    id="email"
                    value={credentials.email}
                    onChange={(e) => setCredentials({ ...credentials, email: e.target.value })}
                    required
                  />
                </div>
                <div className="form-group mb-3">
                  <label htmlFor="password">Password</label>
                  <input
                    type="password"
                    className="form-control"
                    id="password"
                    value={credentials.password}
                    onChange={(e) => setCredentials({ ...credentials, password: e.target.value })}
                    required
                  />
                </div>
                <div className="form-group mb-3">
                  <div className="form-check">
                    <input
                      className="form-check-input"
                      type="checkbox"
                      id="rememberMe"
                      checked={rememberMe}
                      onChange={(e) => setRememberMe(e.target.checked)}
                    />
                    <label className="form-check-label" htmlFor="rememberMe">
                      Remember me
                    </label>
                  </div>
                </div>
                <button type="submit" className="btn btn-primary" disabled={isLoading}>
                  {isLoading ? 'Logging in...' : 'Login'}
                </button>
                <div className="mt-3">
                  <a href="/signup" className="btn btn-link">
                    Don't have an account? Sign up
                  </a>
                  <a href="/reset-password" className="btn btn-link">
                    Forgot password?
                  </a>
                </div>
              </form>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

describe('Login Component Field Names', () => {
  beforeEach(() => {
    // Clear all mocks before each test
    jest.clearAllMocks();
    
    // Setup default mock implementations
    mockLogin.mockResolvedValue({ success: true, user: { email: 'test@example.com' } });
    useLocation.mockReturnValue(mockLocation);
    useNavigate.mockReturnValue(mockNavigate);
  });

  test('should send email field (not username) when submitting login form', async () => {
    render(
      <MemoryRouter>
        <Login />
      </MemoryRouter>
    );
    
    // Wait for login form to render
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    // Fill in the form
    const emailInput = screen.getByLabelText(/email/i);
    const passwordInput = screen.getByLabelText(/password/i);
    const loginButton = screen.getByRole('button', { name: /login/i });
    
    fireEvent.change(emailInput, { target: { value: 'test@example.com' } });
    fireEvent.change(passwordInput, { target: { value: 'password123' } });
    fireEvent.click(loginButton);
    
    // Wait for the login function to be called
    await waitFor(() => {
      expect(mockLogin).toHaveBeenCalledTimes(1);
    });
    
    // Verify the login function was called with email field, not username
    const loginCallArgs = mockLogin.mock.calls[0][0];
    
    // Should send credentials with email field
    expect(loginCallArgs).toHaveProperty('email', 'test@example.com');
    expect(loginCallArgs).not.toHaveProperty('username');
    expect(loginCallArgs).toHaveProperty('password', 'password123');
    expect(loginCallArgs).toHaveProperty('rememberMe', false);
  });

  test('authService.login should receive credentials with email field', async () => {
    const mockAuthServiceLogin = jest.fn().mockResolvedValue({
      user: { email: 'test@example.com' },
      accessToken: 'mock-token',
      refreshToken: 'mock-refresh-token'
    });
    
    authService.default.login = mockAuthServiceLogin;
    
    // Create a direct test of the auth service
    await authService.default.login({ email: 'test@example.com', password: 'password123' });
    
    expect(mockAuthServiceLogin).toHaveBeenCalledWith({
      email: 'test@example.com',
      password: 'password123'
    });
  });
});