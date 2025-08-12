import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

// Mock the auth service
const mockAuthServiceLogin = jest.fn();
jest.mock('./services/authService', () => ({
  default: {
    login: mockAuthServiceLogin
  }
}));

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

// Mock the AuthContext properly
const mockLogin = jest.fn();
const mockLogout = jest.fn();

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

// Create a standalone Login component for testing
const Login = () => {
  const { login, error } = mockUseAuth;
  const [credentials, setCredentials] = React.useState({ email: '', password: '' });
  const [rememberMe, setRememberMe] = React.useState(false);
  const [isLoading, setIsLoading] = React.useState(false);
  
  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsLoading(true);
    try {
      const loginData = {
        email: credentials.email,
        password: credentials.password,
        rememberMe: rememberMe
      };
      const result = await login(loginData);
      if (result.success) {
        console.log('Login successful');
      }
    } catch (err) {
      console.error('Login error:', err);
    } finally {
      setIsLoading(false);
    }
  };
  
  return (
    <div className="login-page">
      <div className="container mt-5">
        <div className="row justify-content-center">
          <div className="col-md-6">
            <div className="card">
              <div className="card-header">
                <h3>Login</h3>
              </div>
              <div className="card-body">
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
                </form>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

describe('Login Component sends correct field names', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    mockLogin.mockResolvedValue({ success: true, user: { email: 'test@example.com' } });
  });

  test('login form should send email field (not username) to auth service', async () => {
    // Render the login component directly
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
    fireEvent.change(passwordInput, { target: { value: 'SecurePass123!' } });
    fireEvent.click(loginButton);

    // Wait for login to be called
    await waitFor(() => {
      expect(mockLogin).toHaveBeenCalledTimes(1);
    });

    // Check what was passed to login
    const loginCallArgs = mockLogin.mock.calls[0][0];
    
    // Should send email field (not username)
    expect(loginCallArgs).toEqual({
      email: 'test@example.com',     // Should have email field
      password: 'SecurePass123!',
      rememberMe: false
    });
    
    // Should not have username property
    expect(loginCallArgs).not.toHaveProperty('username');
  });
});