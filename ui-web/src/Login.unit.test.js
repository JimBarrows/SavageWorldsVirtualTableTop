import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import App from './App';

// Mock the auth service
jest.mock('./services/authService', () => ({
  default: {
    login: jest.fn()
  }
}));

// Mock the AuthContext properly
const mockLogin = jest.fn();
const mockLogout = jest.fn();

jest.mock('./contexts/AuthContext', () => ({
  AuthProvider: ({ children }) => children,
  useAuth: () => ({
    isAuthenticated: false,
    loading: false,
    error: null,
    login: mockLogin,
    logout: mockLogout,
    user: null
  })
}));

describe('Login Component sends correct field names', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    mockLogin.mockResolvedValue({ success: true, user: { email: 'test@example.com' } });
  });

  test('login form should send email field (not username) to auth service', async () => {
    // Render the app with MemoryRouter starting at /login
    render(
      <MemoryRouter initialEntries={['/login']}>
        <App />
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
    
    // EXPECTED TO FAIL: Component currently sends 'username' instead of 'email'
    // This assertion documents the bug
    expect(loginCallArgs).toEqual({
      email: 'test@example.com',     // Should have email field
      password: 'SecurePass123!',
      rememberMe: false
    });
    
    // This documents the current buggy behavior
    expect(loginCallArgs).not.toHaveProperty('username');
  });
});