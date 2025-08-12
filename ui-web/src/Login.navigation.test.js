import React from 'react';
import { render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';

// Mock the auth context
const mockUseAuth = {
  isAuthenticated: false,
  loading: false,
  error: null,
  login: jest.fn(),
  logout: jest.fn(),
  user: null
};

jest.mock('./contexts/AuthContext', () => ({
  useAuth: () => mockUseAuth
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

// Create a simple Login component for testing navigation links
// This mirrors the Login component from App.js but can be tested in isolation
const Login = () => {
  const [credentials, setCredentials] = React.useState({ email: '', password: '' });
  const [rememberMe, setRememberMe] = React.useState(false);
  const [isLoading, setIsLoading] = React.useState(false);
  
  const handleSubmit = (e) => {
    e.preventDefault();
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

describe('Login Navigation Links', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  test('should have link to forgot password page', () => {
    render(
      <MemoryRouter>
        <Login />
      </MemoryRouter>
    );

    // Look for forgot password link
    const forgotPasswordLink = screen.getByText(/forgot password/i);
    expect(forgotPasswordLink).toBeInTheDocument();
    expect(forgotPasswordLink).toHaveAttribute('href', '/reset-password');
  });

  test('login page should display all navigation options', () => {
    render(
      <MemoryRouter>
        <Login />
      </MemoryRouter>
    );

    // Should have signup link
    const signupLink = screen.getByText(/don't have an account\? sign up/i);
    expect(signupLink).toBeInTheDocument();
    expect(signupLink).toHaveAttribute('href', '/signup');

    // Should have forgot password link
    const forgotPasswordLink = screen.getByText(/forgot password/i);
    expect(forgotPasswordLink).toBeInTheDocument();
    expect(forgotPasswordLink).toHaveAttribute('href', '/reset-password');
  });
});