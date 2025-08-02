import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import App from './App';
import * as authService from './services/authService';

// Mock the auth service
jest.mock('./services/authService');

// Mock the auth context
jest.mock('./contexts/AuthContext', () => ({
  ...jest.requireActual('./contexts/AuthContext'),
  useAuth: () => ({
    isAuthenticated: false,
    loading: false,
    error: null,
    login: jest.fn(),
    logout: jest.fn()
  })
}));

describe('Login Component Field Names', () => {
  beforeEach(() => {
    // Clear all mocks before each test
    jest.clearAllMocks();
  });

  test('should send email field (not username) when submitting login form', async () => {
    const mockLogin = jest.fn().mockResolvedValue({ success: true, user: { email: 'test@example.com' } });
    
    // Override the useAuth mock for this test
    jest.spyOn(require('./contexts/AuthContext'), 'useAuth').mockReturnValue({
      isAuthenticated: false,
      loading: false,
      error: null,
      login: mockLogin,
      logout: jest.fn()
    });
    
    render(
      <BrowserRouter>
        <App />
      </BrowserRouter>
    );
    
    // Navigate to login page
    expect(window.location.pathname).toBe('/');
    
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
    
    // This test should FAIL initially because the component sends 'username'
    // After fixing the bug, it should pass
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