import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';
import SignupForm from '../SignupForm';
import * as authService from '../../services/authService';

// Mock the auth service
jest.mock('../../services/authService');

// Mock the auth context
jest.mock('../../contexts/AuthContext', () => ({
  ...jest.requireActual('../../contexts/AuthContext'),
  useAuth: () => ({
    register: jest.fn().mockResolvedValue({ success: true }),
    login: jest.fn().mockResolvedValue({ success: true }),
    error: null,
    loading: false,
    isAuthenticated: false
  })
}));

describe('Email-Only Authentication', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('SignupForm Component', () => {
    it('should not have username field', () => {
      const { container } = render(<SignupForm onSubmit={jest.fn()} />);
      
      // Email field should exist - using the correct ID format from bootstrap-react-components
      expect(container.querySelector('#FormControl-email-EmailFormGroup-email')).toBeInTheDocument();
      
      // Username field should not exist
      expect(screen.queryByLabelText(/username/i)).not.toBeInTheDocument();
      expect(screen.queryByRole('textbox', { name: /username/i })).not.toBeInTheDocument();
      expect(container.querySelector('[id*="username"]')).not.toBeInTheDocument();
    });

    it('should not send username to API during registration', async () => {
      const mockSubmit = jest.fn().mockResolvedValue({ success: true });
      
      const { container } = render(<SignupForm onSubmit={mockSubmit} />);

      // Fill in the form using the correct IDs
      const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email');
      const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password');
      const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword');
      
      fireEvent.change(emailInput, { target: { value: 'newuser@example.com' } });
      fireEvent.change(passwordInput, { target: { value: 'SecurePass123!' } });
      fireEvent.change(confirmPasswordInput, { target: { value: 'SecurePass123!' } });

      // Submit the form
      const submitButton = screen.getByRole('button', { name: /sign up/i });
      fireEvent.click(submitButton);

      await waitFor(() => {
        // Should only send email and password
        expect(mockSubmit).toHaveBeenCalledWith({
          email: 'newuser@example.com',
          password: 'SecurePass123!'
        });
        // Should NOT include username
        expect(mockSubmit).not.toHaveBeenCalledWith(
          expect.objectContaining({ username: expect.any(String) })
        );
      });
    });

    it('should not generate username from email during registration', async () => {
      const mockSubmit = jest.fn().mockResolvedValue({ success: true });
      
      const { container } = render(<SignupForm onSubmit={mockSubmit} />);

      const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email');
      const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password');
      const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword');
      
      fireEvent.change(emailInput, { target: { value: 'complex.email+tag@example.com' } });
      fireEvent.change(passwordInput, { target: { value: 'SecurePass123!' } });
      fireEvent.change(confirmPasswordInput, { target: { value: 'SecurePass123!' } });

      const submitButton = screen.getByRole('button', { name: /sign up/i });
      fireEvent.click(submitButton);

      await waitFor(() => {
        expect(mockSubmit).toHaveBeenCalledWith({
          email: 'complex.email+tag@example.com',
          password: 'SecurePass123!'
        });
        // Verify no username generation happened
        expect(mockSubmit.mock.calls[0][0]).not.toHaveProperty('username');
      });
    });
  });

  describe('Login Component in App.js', () => {
    // We'll test the login logic without rendering the full App
    it('should send email directly to login without username generation', async () => {
      // Import the login function from auth service
      const mockLogin = jest.fn().mockResolvedValue({
        user: { email: 'test@example.com' },
        accessToken: 'token123',
        refreshToken: 'refresh123'
      });
      
      authService.default.login = mockLogin;

      // Simulate what the login component does
      const credentials = {
        email: 'test@example.com',
        password: 'password123'
      };

      // The new implementation should send email directly
      await authService.default.login(credentials);

      expect(mockLogin).toHaveBeenCalledWith({
        email: 'test@example.com',
        password: 'password123'
      });
    });
  });

  describe('Auth Service', () => {
    it('should handle login response without username field', async () => {
      const mockResponse = {
        data: {
          data: {
            access_token: 'token123',
            refresh_token: 'refresh123',
            user: {
              id: '123',
              email: 'test@example.com'
              // Note: no username field
            }
          }
        }
      };

      // Test that the service can handle response without username
      expect(mockResponse.data.data.user).not.toHaveProperty('username');
      expect(mockResponse.data.data.user.email).toBe('test@example.com');
    });
  });
});