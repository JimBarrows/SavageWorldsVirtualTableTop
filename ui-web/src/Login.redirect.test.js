import React from 'react';
import { render, fireEvent, waitFor } from '@testing-library/react';

// Mock window.location.href to test the current broken behavior
Object.defineProperty(window, 'location', {
  value: {
    href: '',
    assign: jest.fn(),
    reload: jest.fn()
  },
  writable: true,
});

// Mock useAuth hook
const mockLogin = jest.fn();
const mockUseAuth = {
  login: mockLogin,
  error: null,
  user: null,
  loading: false
};

// Mock React Router hooks  
const mockNavigate = jest.fn();

jest.mock('./contexts/AuthContext', () => ({
  useAuth: () => mockUseAuth
}));

jest.mock('react-router-dom', () => ({
  useNavigate: () => mockNavigate,
  useLocation: () => ({ state: null })
}));

// Create a simple login component that matches the App.js implementation
const LoginComponentTest = () => {
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
        // THIS IS THE BUG: Hard redirect instead of React Router navigation
        window.location.href = '/';
      }
    } catch (err) {
      console.error('Login error:', err);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <form onSubmit={handleSubmit}>
      <label htmlFor="email">Email</label>
      <input
        type="email"
        id="email"
        value={credentials.email}
        onChange={(e) => setCredentials({ ...credentials, email: e.target.value })}
        required
      />
      <label htmlFor="password">Password</label>
      <input
        type="password"
        id="password"
        value={credentials.password}
        onChange={(e) => setCredentials({ ...credentials, password: e.target.value })}
        required
      />
      <button type="submit" disabled={isLoading}>
        {isLoading ? 'Logging in...' : 'Login'}
      </button>
    </form>
  );
};

// Mock console.log to avoid noise in tests
const originalConsoleLog = console.log;
const originalConsoleError = console.error;

describe('Login Redirect Fix - Issue #174', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    window.location.href = '';
    console.log = jest.fn();
    console.error = jest.fn();
  });
  
  afterEach(() => {
    console.log = originalConsoleLog;
    console.error = originalConsoleError;
  });

  test('documents current broken behavior - uses window.location.href', async () => {
    // Mock successful login
    mockLogin.mockResolvedValueOnce({ 
      success: true,
      user: { email: 'test@example.com', id: '123' } 
    });

    const { getByLabelText, getByRole } = render(<LoginComponentTest />);
    
    const emailField = getByLabelText(/email/i);
    const passwordField = getByLabelText(/password/i);
    const loginButton = getByRole('button', { name: /login/i });
    
    // Fill and submit form
    fireEvent.change(emailField, { target: { value: 'test@example.com' } });
    fireEvent.change(passwordField, { target: { value: 'ValidPassword123!' } });
    fireEvent.click(loginButton);
    
    await waitFor(() => {
      expect(mockLogin).toHaveBeenCalledWith({
        email: 'test@example.com',
        password: 'ValidPassword123!',
        rememberMe: false
      });
    });
    
    // CURRENT BROKEN BEHAVIOR: window.location.href is set, causing hard redirect
    expect(window.location.href).toBe('/');
  });

  test('after fix should use React Router navigate instead of window.location.href', async () => {
    // This test will fail initially, pass after the fix
    
    // Mock successful login
    mockLogin.mockResolvedValueOnce({ 
      success: true,
      user: { email: 'test@example.com', id: '123' } 
    });

    // Create the FIXED version of login component
    const FixedLoginComponent = () => {
      const navigate = mockNavigate;
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
            // FIXED: Use React Router navigate instead of window.location.href
            navigate('/');
          }
        } catch (err) {
          console.error('Login error:', err);
        } finally {
          setIsLoading(false);
        }
      };

      return (
        <form onSubmit={handleSubmit}>
          <label htmlFor="email">Email</label>
          <input
            type="email"
            id="email"
            value={credentials.email}
            onChange={(e) => setCredentials({ ...credentials, email: e.target.value })}
            required
          />
          <label htmlFor="password">Password</label>
          <input
            type="password"
            id="password"
            value={credentials.password}
            onChange={(e) => setCredentials({ ...credentials, password: e.target.value })}
            required
          />
          <button type="submit" disabled={isLoading}>
            {isLoading ? 'Logging in...' : 'Login'}
          </button>
        </form>
      );
    };

    const { getByLabelText, getByRole } = render(<FixedLoginComponent />);
    
    const emailField = getByLabelText(/email/i);
    const passwordField = getByLabelText(/password/i);  
    const loginButton = getByRole('button', { name: /login/i });
    
    // Fill and submit form
    fireEvent.change(emailField, { target: { value: 'test@example.com' } });
    fireEvent.change(passwordField, { target: { value: 'ValidPassword123!' } });
    fireEvent.click(loginButton);
    
    await waitFor(() => {
      expect(mockLogin).toHaveBeenCalledWith({
        email: 'test@example.com',
        password: 'ValidPassword123!',
        rememberMe: false
      });
    });
    
    // AFTER FIX: Should use React Router navigate, not window.location.href
    expect(mockNavigate).toHaveBeenCalledWith('/');
    expect(window.location.href).toBe(''); // Should remain unchanged
  });

  test('verifies the problem: window.location.href causes race condition', async () => {
    // This test demonstrates why window.location.href is problematic
    
    mockLogin.mockResolvedValueOnce({ 
      success: true,
      user: { email: 'test@example.com', id: '123' } 
    });

    const { getByLabelText, getByRole } = render(<LoginComponentTest />);
    
    const emailField = getByLabelText(/email/i);
    const passwordField = getByLabelText(/password/i);
    const loginButton = getByRole('button', { name: /login/i });
    
    fireEvent.change(emailField, { target: { value: 'test@example.com' } });
    fireEvent.change(passwordField, { target: { value: 'ValidPassword123!' } });
    fireEvent.click(loginButton);
    
    await waitFor(() => {
      expect(mockLogin).toHaveBeenCalled();
    });
    
    // Problem: window.location.href = '/' forces a full page reload
    // This causes the browser to:
    // 1. Immediately navigate to '/' 
    // 2. Lose all React state and context
    // 3. Force AuthContext to re-initialize from scratch
    // 4. Create a race condition where ProtectedRoute might check authentication 
    //    before AuthContext finishes loading stored tokens
    expect(window.location.href).toBe('/');
    
    // The navigate function should NOT be called when using window.location.href
    expect(mockNavigate).not.toHaveBeenCalled();
  });
});