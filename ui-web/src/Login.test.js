import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import App from './App';
import { QueryClient, QueryClientProvider } from 'react-query';

// Mock config first
jest.mock('./config', () => ({
  default: {
    query: {
      retry: false
    },
    api: {
      baseURL: 'http://localhost:3001',
      timeout: 30000
    }
  }
}));

// Mock the auth service
jest.mock('./services/authService', () => ({
  default: {
    login: jest.fn(),
    getCurrentUser: jest.fn(),
    logout: jest.fn(),
    register: jest.fn()
  }
}));

// Mock services/api
jest.mock('./services/api', () => ({
  default: {
    get: jest.fn(),
    post: jest.fn(),
    put: jest.fn(),
    delete: jest.fn()
  },
  setTokens: jest.fn(),
  clearTokens: jest.fn(),
  getAccessToken: jest.fn()
}));

const authService = require('./services/authService').default;

describe('Login Component', () => {
  let queryClient;

  beforeEach(() => {
    jest.clearAllMocks();
    queryClient = new QueryClient({
      defaultOptions: {
        queries: { retry: false },
        mutations: { retry: false }
      }
    });
    
    // Mock getCurrentUser to simulate not logged in
    authService.getCurrentUser.mockRejectedValue(new Error('Not authenticated'));
  });

  const renderLoginComponent = () => {
    return render(
      <MemoryRouter initialEntries={['/login']}>
        <QueryClientProvider client={queryClient}>
          <App />
        </QueryClientProvider>
      </MemoryRouter>
    );
  };

  test('renders login form with email and password fields', async () => {
    renderLoginComponent();
    
    // Wait for the login form to appear
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    expect(screen.getByLabelText(/password/i)).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /login/i })).toBeInTheDocument();
  });

  test('displays email field instead of username field', async () => {
    renderLoginComponent();
    
    // Wait for the form to load
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    // Should have email field
    const emailField = screen.getByLabelText(/email/i);
    expect(emailField).toBeInTheDocument();
    expect(emailField).toHaveAttribute('type', 'email');
    expect(emailField).toHaveAttribute('id', 'email');
    
    // Should NOT have username field
    expect(screen.queryByLabelText(/username/i)).not.toBeInTheDocument();
  });

  test('requires email field to be filled', async () => {
    renderLoginComponent();
    
    // Wait for the form to appear
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    const passwordField = screen.getByLabelText(/password/i);
    const loginButton = screen.getByRole('button', { name: /login/i });
    
    // Fill only password
    fireEvent.change(passwordField, { target: { value: 'TestPassword123!' } });
    
    // Try to submit
    fireEvent.click(loginButton);
    
    // Email field should have required validation
    const emailField = screen.getByLabelText(/email/i);
    expect(emailField).toBeRequired();
  });

  test('requires password field to be filled', async () => {
    renderLoginComponent();
    
    // Wait for the form to appear
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    const emailField = screen.getByLabelText(/email/i);
    const loginButton = screen.getByRole('button', { name: /login/i });
    
    // Fill only email
    fireEvent.change(emailField, { target: { value: 'test@example.com' } });
    
    // Try to submit
    fireEvent.click(loginButton);
    
    // Password field should have required validation
    const passwordField = screen.getByLabelText(/password/i);
    expect(passwordField).toBeRequired();
  });

  test('validates email format', async () => {
    renderLoginComponent();
    
    // Wait for the form to appear
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    const emailField = screen.getByLabelText(/email/i);
    
    // Test invalid email
    fireEvent.change(emailField, { target: { value: 'invalid-email' } });
    
    // HTML5 email validation should kick in
    expect(emailField).toHaveAttribute('type', 'email');
    expect(emailField.checkValidity()).toBe(false);
  });

  test('successful login with valid credentials', async () => {
    authService.login.mockResolvedValueOnce({
      user: { email: 'test@example.com', id: '123' }
    });

    renderLoginComponent();
    
    // Wait for the form to appear
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    const emailField = screen.getByLabelText(/email/i);
    const passwordField = screen.getByLabelText(/password/i);
    const loginButton = screen.getByRole('button', { name: /login/i });
    
    // Fill form
    fireEvent.change(emailField, { target: { value: 'test@example.com' } });
    fireEvent.change(passwordField, { target: { value: 'ValidPassword123!' } });
    
    // Submit
    fireEvent.click(loginButton);
    
    // Should show loading state
    expect(screen.getByText(/logging in/i)).toBeInTheDocument();
    
    // Wait for login to complete
    await waitFor(() => {
      expect(authService.login).toHaveBeenCalledWith({
        email: 'test@example.com',
        password: 'ValidPassword123!'
      });
    });
  });

  test('displays error message on login failure', async () => {
    authService.login.mockRejectedValueOnce({
      message: 'Incorrect username or password'
    });

    renderLoginComponent();
    
    // Wait for the form to appear
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    const emailField = screen.getByLabelText(/email/i);
    const passwordField = screen.getByLabelText(/password/i);
    const loginButton = screen.getByRole('button', { name: /login/i });
    
    // Fill form
    fireEvent.change(emailField, { target: { value: 'test@example.com' } });
    fireEvent.change(passwordField, { target: { value: 'WrongPassword!' } });
    
    // Submit
    fireEvent.click(loginButton);
    
    // Wait for error to appear
    await waitFor(() => {
      expect(screen.getByText(/incorrect username or password/i)).toBeInTheDocument();
    });
  });

  test('disables submit button while logging in', async () => {
    authService.login.mockImplementation(() => 
      new Promise(resolve => setTimeout(resolve, 1000))
    );

    renderLoginComponent();
    
    // Wait for the form to appear
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    const emailField = screen.getByLabelText(/email/i);
    const passwordField = screen.getByLabelText(/password/i);
    const loginButton = screen.getByRole('button', { name: /login/i });
    
    // Fill form
    fireEvent.change(emailField, { target: { value: 'test@example.com' } });
    fireEvent.change(passwordField, { target: { value: 'ValidPassword123!' } });
    
    // Submit
    fireEvent.click(loginButton);
    
    // Button should be disabled
    expect(loginButton).toBeDisabled();
    expect(screen.getByText(/logging in/i)).toBeInTheDocument();
  });

  test('has link to signup page', async () => {
    renderLoginComponent();
    
    // Wait for the form to appear
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    const signupLink = screen.getByText(/need an account\? sign up/i);
    expect(signupLink).toBeInTheDocument();
    expect(signupLink).toHaveAttribute('href', '/signup');
  });

  test('updates credentials state on input change', async () => {
    renderLoginComponent();
    
    // Wait for the form to appear
    await waitFor(() => {
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
    });
    
    const emailField = screen.getByLabelText(/email/i);
    const passwordField = screen.getByLabelText(/password/i);
    
    // Type in email field
    fireEvent.change(emailField, { target: { value: 'new@example.com' } });
    expect(emailField.value).toBe('new@example.com');
    
    // Type in password field
    fireEvent.change(passwordField, { target: { value: 'NewPassword123!' } });
    expect(passwordField.value).toBe('NewPassword123!');
  });
});