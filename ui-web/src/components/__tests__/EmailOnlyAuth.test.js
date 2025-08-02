import React from 'react';
import { render, screen, fireEvent, waitFor, renderHook, act } from '@testing-library/react';
import '@testing-library/jest-dom';
import { BrowserRouter } from 'react-router-dom';
import App from '../../App';
import SignupForm from '../SignupForm';
import { AuthProvider, useAuth } from '../../contexts/AuthContext';
import * as authService from '../../services/authService';

// Mock the auth service
jest.mock('../../services/authService');

describe('Email-Only Authentication', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Login Component', () => {
    it('should not generate username from email', async () => {
      const mockLogin = jest.fn().mockResolvedValue({
        success: true,
        user: { email: 'test@example.com' }
      });
      
      authService.default.login = mockLogin;

      render(
        <BrowserRouter>
          <AuthProvider>
            <App />
          </AuthProvider>
        </BrowserRouter>
      );

      // Navigate to login
      window.history.pushState({}, '', '/login');

      // Fill in the form
      const emailInput = screen.getByLabelText(/email/i);
      const passwordInput = screen.getByLabelText(/password/i);
      
      fireEvent.change(emailInput, { target: { value: 'test@example.com' } });
      fireEvent.change(passwordInput, { target: { value: 'password123' } });

      // Submit the form
      const submitButton = screen.getByRole('button', { name: /login/i });
      fireEvent.click(submitButton);

      await waitFor(() => {
        // Should send email directly, not username
        expect(mockLogin).toHaveBeenCalledWith({
          email: 'test@example.com',
          password: 'password123'
        });
      });
    });

    it('should send email field to API instead of username', async () => {
      const mockLogin = jest.fn().mockResolvedValue({
        success: true,
        user: { email: 'john.doe@example.com' }
      });
      
      authService.default.login = mockLogin;

      render(
        <BrowserRouter>
          <AuthProvider>
            <App />
          </AuthProvider>
        </BrowserRouter>
      );

      window.history.pushState({}, '', '/login');

      const emailInput = screen.getByLabelText(/email/i);
      const passwordInput = screen.getByLabelText(/password/i);
      
      fireEvent.change(emailInput, { target: { value: 'john.doe@example.com' } });
      fireEvent.change(passwordInput, { target: { value: 'SecurePass123!' } });

      const submitButton = screen.getByRole('button', { name: /login/i });
      fireEvent.click(submitButton);

      await waitFor(() => {
        expect(mockLogin).toHaveBeenCalledWith({
          email: 'john.doe@example.com',
          password: 'SecurePass123!'
        });
        // Should NOT have been called with username
        expect(mockLogin).not.toHaveBeenCalledWith(
          expect.objectContaining({ username: expect.any(String) })
        );
      });
    });
  });

  describe('SignupForm Component', () => {
    it('should not have username field', () => {
      render(<SignupForm />);
      
      // Email field should exist
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
      
      // Username field should not exist
      expect(screen.queryByLabelText(/username/i)).not.toBeInTheDocument();
      expect(screen.queryByRole('textbox', { name: /username/i })).not.toBeInTheDocument();
    });

    it('should not send username to API during registration', async () => {
      const mockRegister = jest.fn().mockResolvedValue({
        success: true,
        data: { email: 'newuser@example.com' }
      });
      
      authService.default.register = mockRegister;

      render(
        <AuthProvider>
          <SignupForm />
        </AuthProvider>
      );

      // Fill in the form
      const emailInput = screen.getByLabelText(/email/i);
      const passwordInput = screen.getByLabelText(/password/i);
      const confirmPasswordInput = screen.getByLabelText(/confirm password/i);
      
      fireEvent.change(emailInput, { target: { value: 'newuser@example.com' } });
      fireEvent.change(passwordInput, { target: { value: 'SecurePass123!' } });
      fireEvent.change(confirmPasswordInput, { target: { value: 'SecurePass123!' } });

      // Submit the form
      const submitButton = screen.getByRole('button', { name: /sign up/i });
      fireEvent.click(submitButton);

      await waitFor(() => {
        // Should only send email and password
        expect(mockRegister).toHaveBeenCalledWith({
          email: 'newuser@example.com',
          password: 'SecurePass123!'
        });
        // Should NOT include username
        expect(mockRegister).not.toHaveBeenCalledWith(
          expect.objectContaining({ username: expect.any(String) })
        );
      });
    });

    it('should not generate username from email during registration', async () => {
      const mockRegister = jest.fn().mockResolvedValue({
        success: true,
        data: { email: 'complex.email+tag@example.com' }
      });
      
      authService.default.register = mockRegister;

      render(
        <AuthProvider>
          <SignupForm />
        </AuthProvider>
      );

      const emailInput = screen.getByLabelText(/email/i);
      const passwordInput = screen.getByLabelText(/password/i);
      const confirmPasswordInput = screen.getByLabelText(/confirm password/i);
      
      fireEvent.change(emailInput, { target: { value: 'complex.email+tag@example.com' } });
      fireEvent.change(passwordInput, { target: { value: 'SecurePass123!' } });
      fireEvent.change(confirmPasswordInput, { target: { value: 'SecurePass123!' } });

      const submitButton = screen.getByRole('button', { name: /sign up/i });
      fireEvent.click(submitButton);

      await waitFor(() => {
        expect(mockRegister).toHaveBeenCalledWith({
          email: 'complex.email+tag@example.com',
          password: 'SecurePass123!'
        });
        // Verify no username generation happened
        expect(mockRegister.mock.calls[0][0]).not.toHaveProperty('username');
      });
    });
  });

  describe('User Display', () => {
    it('should display email instead of username in UI', async () => {
      // Mock authenticated state
      const mockAuth = {
        user: {
          email: 'user@example.com',
          id: '123'
        },
        isAuthenticated: true,
        loading: false
      };

      jest.spyOn(require('../../contexts/AuthContext'), 'useAuth').mockReturnValue(mockAuth);

      render(
        <BrowserRouter>
          <AuthProvider>
            <App />
          </AuthProvider>
        </BrowserRouter>
      );

      // Should display email in header/navigation
      await waitFor(() => {
        expect(screen.getByText(/user@example\.com/i)).toBeInTheDocument();
      });

      // Should not display any username
      expect(screen.queryByText(/username:/i)).not.toBeInTheDocument();
    });
  });

  describe('API Response Handling', () => {
    it('should handle API response without username field', async () => {
      const mockLogin = jest.fn().mockResolvedValue({
        user: {
          id: '123',
          email: 'test@example.com'
          // Note: no username field
        },
        accessToken: 'token123',
        refreshToken: 'refresh123'
      });
      
      authService.default.login = mockLogin;

      const { result } = renderHook(() => useAuth(), {
        wrapper: AuthProvider
      });

      await act(async () => {
        const response = await result.current.login({
          email: 'test@example.com',
          password: 'password123'
        });

        expect(response.success).toBe(true);
        expect(response.user).toEqual({
          id: '123',
          email: 'test@example.com'
        });
        expect(response.user).not.toHaveProperty('username');
      });
    });
  });
});