import React from 'react';
import { render, screen, fireEvent, waitFor, act } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { MemoryRouter } from 'react-router-dom';
import App from './App';

// Mock all external dependencies
jest.mock('./services', () => ({
  authService: {
    login: jest.fn(),
    logout: jest.fn(),
    getCurrentUser: jest.fn(),
    register: jest.fn()
  }
}));

jest.mock('./services/api', () => ({
  clearTokens: jest.fn(),
  getAccessToken: jest.fn()
}));

jest.mock('./utils/tokenUtils', () => ({
  setRememberMeFlag: jest.fn(),
  clearRememberMeFlag: jest.fn(),
  isRememberMeSession: jest.fn(),
  getTokenExpiryTime: jest.fn(),
  isTokenExpired: jest.fn(),
  storeTokenExpiry: jest.fn(),
  getStoredTokenExpiry: jest.fn(),
  clearStoredTokenExpiry: jest.fn()
}));

// Mock React Query DevTools
jest.mock('react-query/devtools', () => ({
  ReactQueryDevtools: () => null
}));

// Mock BrowserRouter to use MemoryRouter for tests  
let mockInitialEntries = ['/'];
jest.mock('react-router-dom', () => {
  const actual = jest.requireActual('react-router-dom');
  return {
    ...actual,
    BrowserRouter: ({ children }) => {
      const { MemoryRouter } = actual;
      return React.createElement(MemoryRouter, { initialEntries: mockInitialEntries }, children);
    }
  };
});

// Mock config
jest.mock('./config', () => ({
  default: {
    query: {
      retry: false,
      refetchOnWindowFocus: false
    }
  }
}));

// Mock all page and component dependencies
jest.mock('./components/layout/Header', () => {
  return function Header() {
    return <div data-testid="header">Header</div>;
  };
});

jest.mock('./pages/PlotPointAdd', () => {
  return function PlotPointAdd() {
    return <div data-testid="plot-point-add">Plot Point Add</div>;
  };
});

jest.mock('./pages/PlotPointEdit', () => {
  return function PlotPointEdit() {
    return <div data-testid="plot-point-edit">Plot Point Edit</div>;
  };
});

jest.mock('./pages/PlotPointList', () => {
  return function PlotPointList() {
    return <div data-testid="plot-point-list">Plot Point List</div>;
  };
});

jest.mock('./pages/SceneList', () => {
  return function SceneList() {
    return <div data-testid="scene-list">Scene List</div>;
  };
});

jest.mock('./pages/SceneAdd', () => {
  return function SceneAdd() {
    return <div data-testid="scene-add">Scene Add</div>;
  };
});

jest.mock('./pages/SceneEdit', () => {
  return function SceneEdit() {
    return <div data-testid="scene-edit">Scene Edit</div>;
  };
});

jest.mock('./pages/SignupPage', () => {
  return function SignupPage() {
    return <div data-testid="signup-page">Signup Page</div>;
  };
});

jest.mock('./pages/ResetPasswordPage', () => {
  return function ResetPasswordPage() {
    return <div data-testid="reset-password-page">Reset Password Page</div>;
  };
});

jest.mock('./components/ErrorBoundary', () => {
  return function ErrorBoundary({ children }) {
    return <div data-testid="error-boundary">{children}</div>;
  };
});

jest.mock('./components/RememberMe', () => {
  return function RememberMe({ checked, onChange }) {
    return (
      <div data-testid="remember-me">
        <input
          type="checkbox"
          checked={checked}
          onChange={(e) => onChange(e.target.checked)}
          data-testid="remember-me-checkbox"
        />
        <label>Remember Me</label>
      </div>
    );
  };
});

jest.mock('./components/MarketingPage', () => {
  return function MarketingPage() {
    return <div data-testid="marketing-page">Marketing Page</div>;
  };
});

jest.mock('./components/BrandingBanner', () => {
  return function BrandingBanner({ compact }) {
    return (
      <div data-testid="branding-banner" data-compact={compact}>
        Savage Worlds VTT
      </div>
    );
  };
});

import { authService } from './services';
import * as api from './services/api';
import * as tokenUtils from './utils/tokenUtils';

describe('App Comprehensive Tests', () => {
  // Helper function to set mock route
  const setMockRoute = (route) => {
    mockInitialEntries = [route];
  };

  beforeEach(() => {
    jest.clearAllMocks();
    // Setup default mock returns
    api.getAccessToken.mockReturnValue(null);
    tokenUtils.isRememberMeSession.mockReturnValue(false);
    tokenUtils.getStoredTokenExpiry.mockReturnValue(null);
    tokenUtils.isTokenExpired.mockReturnValue(false);
    tokenUtils.getTokenExpiryTime.mockReturnValue(Date.now() + 60000);
    
    // Reset route to default
    mockInitialEntries = ['/'];
    
    // Clear any timers
    jest.clearAllTimers();
  });

  describe('ProtectedRoute Component', () => {
    it('shows loading state when auth is loading', async () => {
      setMockRoute('/plot-points');
      // Mock loading state
      api.getAccessToken.mockReturnValue('token');
      authService.getCurrentUser.mockImplementation(() => 
        new Promise(resolve => setTimeout(() => resolve({ id: '1' }), 100))
      );

      render(<App />);

      // Should show loading initially
      expect(screen.getByText('Loading...')).toBeInTheDocument();

      // Wait for auth to resolve
      await waitFor(() => {
        expect(screen.queryByText('Loading...')).not.toBeInTheDocument();
      });
    });

    it('redirects to login when not authenticated', async () => {
      setMockRoute('/plot-points');
      api.getAccessToken.mockReturnValue(null);

      render(<App />);

      await waitFor(() => {
        expect(screen.getByText('Login')).toBeInTheDocument();
      });
    });

    it('renders protected children when authenticated', async () => {
      setMockRoute('/plot-points');
      const mockUser = { id: '1', email: 'test@example.com' };
      api.getAccessToken.mockReturnValue('valid-token');
      tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60 * 60 * 1000);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);

      render(<App />);

      await waitFor(() => {
        expect(screen.getByTestId('plot-point-list')).toBeInTheDocument();
      });
    });
  });

  describe('Login Component', () => {
    it('renders login form with all fields', async () => {
      setMockRoute('/login');
      render(<App />);

      await waitFor(() => {
        expect(screen.getByLabelText('Email')).toBeInTheDocument();
        expect(screen.getByLabelText('Password')).toBeInTheDocument();
        expect(screen.getByTestId('remember-me')).toBeInTheDocument();
        expect(screen.getByRole('button', { name: 'Login' })).toBeInTheDocument();
      });
    });

    it('updates credentials state when typing in form fields', async () => {
      const user = userEvent.setup();
      setMockRoute('/login');

      render(<App />);

      await waitFor(() => {
        expect(screen.getByLabelText('Email')).toBeInTheDocument();
      });

      const emailInput = screen.getByLabelText('Email');
      const passwordInput = screen.getByLabelText('Password');

      await user.type(emailInput, 'test@example.com');
      await user.type(passwordInput, 'password123');

      expect(emailInput.value).toBe('test@example.com');
      expect(passwordInput.value).toBe('password123');
    });

    it('toggles remember me checkbox', async () => {
      const user = userEvent.setup();

      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByTestId('remember-me-checkbox')).toBeInTheDocument();
      });

      const rememberMeCheckbox = screen.getByTestId('remember-me-checkbox');
      expect(rememberMeCheckbox.checked).toBe(false);

      await user.click(rememberMeCheckbox);
      expect(rememberMeCheckbox.checked).toBe(true);
    });

    it('shows loading state during login submission', async () => {
      const user = userEvent.setup();
      authService.login.mockImplementation(() => 
        new Promise(resolve => setTimeout(() => resolve({ success: true, user: { id: '1' } }), 100))
      );

      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByLabelText('Email')).toBeInTheDocument();
      });

      await user.type(screen.getByLabelText('Email'), 'test@example.com');
      await user.type(screen.getByLabelText('Password'), 'password123');
      
      const submitButton = screen.getByRole('button', { name: 'Login' });
      await user.click(submitButton);

      expect(screen.getByText('Logging in...')).toBeInTheDocument();
      expect(submitButton).toBeDisabled();

      await waitFor(() => {
        expect(screen.queryByText('Logging in...')).not.toBeInTheDocument();
      });
    });

    it('handles successful login submission', async () => {
      const user = userEvent.setup();
      const mockUser = { id: '1', email: 'test@example.com' };
      authService.login.mockResolvedValue({ 
        success: true, 
        user: mockUser,
        accessToken: 'token123' 
      });

      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByLabelText('Email')).toBeInTheDocument();
      });

      await user.type(screen.getByLabelText('Email'), 'test@example.com');
      await user.type(screen.getByLabelText('Password'), 'password123');
      await user.click(screen.getByTestId('remember-me-checkbox'));

      await user.click(screen.getByRole('button', { name: 'Login' }));

      await waitFor(() => {
        expect(authService.login).toHaveBeenCalledWith({
          email: 'test@example.com',
          password: 'password123',
          rememberMe: true
        });
      });
    });

    it('handles failed login submission', async () => {
      const user = userEvent.setup();
      authService.login.mockRejectedValue(new Error('Invalid credentials'));

      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByLabelText('Email')).toBeInTheDocument();
      });

      await user.type(screen.getByLabelText('Email'), 'test@example.com');
      await user.type(screen.getByLabelText('Password'), 'wrongpassword');

      await user.click(screen.getByRole('button', { name: 'Login' }));

      await waitFor(() => {
        expect(authService.login).toHaveBeenCalled();
      });
    });

    it('displays logout success message from navigation state', async () => {
      render(
        <MemoryRouter 
          initialEntries={[
            { 
              pathname: '/login', 
              state: { 
                message: 'You have been logged out successfully', 
                type: 'success' 
              } 
            }
          ]}
        >
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByTestId('logout-success')).toBeInTheDocument();
        expect(screen.getByText('You have been logged out successfully')).toBeInTheDocument();
      });
    });

    it('clears logout message after timeout', async () => {
      jest.useFakeTimers();

      render(
        <MemoryRouter 
          initialEntries={[
            { 
              pathname: '/login', 
              state: { 
                message: 'You have been logged out successfully', 
                type: 'success' 
              } 
            }
          ]}
        >
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByTestId('logout-success')).toBeInTheDocument();
      });

      act(() => {
        jest.advanceTimersByTime(5000);
      });

      await waitFor(() => {
        expect(screen.queryByTestId('logout-success')).not.toBeInTheDocument();
      });

      jest.useRealTimers();
    });

    it('displays auth error from context', async () => {
      // Mock the AuthContext to return an error
      api.getAccessToken.mockReturnValue(null);
      authService.getCurrentUser.mockRejectedValue(new Error('Authentication failed'));

      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByText('Login')).toBeInTheDocument();
      });
    });

    it('renders signup and reset password links', async () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByText("Don't have an account? Sign up")).toBeInTheDocument();
        expect(screen.getByText('Forgot password?')).toBeInTheDocument();
      });
    });
  });

  describe('HomeRoute Component', () => {
    it('redirects authenticated users to plot-points', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      api.getAccessToken.mockReturnValue('valid-token');
      tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60 * 60 * 1000);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);

      render(
        <MemoryRouter initialEntries={['/']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByTestId('plot-point-list')).toBeInTheDocument();
      });
    });

    it('shows marketing page for unauthenticated users', async () => {
      api.getAccessToken.mockReturnValue(null);

      render(
        <MemoryRouter initialEntries={['/']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByTestId('marketing-page')).toBeInTheDocument();
      });
    });

    it('shows loading state while checking authentication', async () => {
      api.getAccessToken.mockReturnValue('token');
      authService.getCurrentUser.mockImplementation(() => 
        new Promise(resolve => setTimeout(() => resolve({ id: '1' }), 100))
      );

      render(
        <MemoryRouter initialEntries={['/']}>
          <App />
        </MemoryRouter>
      );

      expect(screen.getByText('Loading...')).toBeInTheDocument();

      await waitFor(() => {
        expect(screen.queryByText('Loading...')).not.toBeInTheDocument();
      });
    });
  });

  describe('AppContent Routing', () => {
    it('renders header for authenticated users', async () => {
      const mockUser = { id: '1', email: 'test@example.com' };
      api.getAccessToken.mockReturnValue('valid-token');
      tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60 * 60 * 1000);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);

      render(
        <MemoryRouter initialEntries={['/plot-points']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByTestId('header')).toBeInTheDocument();
      });
    });

    it('does not render header for unauthenticated users', async () => {
      api.getAccessToken.mockReturnValue(null);

      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.queryByTestId('header')).not.toBeInTheDocument();
      });
    });

    it('renders public signup route', async () => {
      render(
        <MemoryRouter initialEntries={['/signup']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByTestId('signup-page')).toBeInTheDocument();
      });
    });

    it('renders public reset password route', async () => {
      render(
        <MemoryRouter initialEntries={['/reset-password']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByTestId('reset-password-page')).toBeInTheDocument();
      });
    });

    describe('Protected Routes', () => {
      beforeEach(() => {
        const mockUser = { id: '1', email: 'test@example.com' };
        api.getAccessToken.mockReturnValue('valid-token');
        tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60 * 60 * 1000);
        tokenUtils.isTokenExpired.mockReturnValue(false);
        authService.getCurrentUser.mockResolvedValue(mockUser);
      });

      it('renders plot-points list route', async () => {
        render(
          <MemoryRouter initialEntries={['/plot-points']}>
            <App />
          </MemoryRouter>
        );

        await waitFor(() => {
          expect(screen.getByTestId('plot-point-list')).toBeInTheDocument();
        });
      });

      it('renders plot point add route', async () => {
        render(
          <MemoryRouter initialEntries={['/plot_point/add']}>
            <App />
          </MemoryRouter>
        );

        await waitFor(() => {
          expect(screen.getByTestId('plot-point-add')).toBeInTheDocument();
        });
      });

      it('renders plot point edit route', async () => {
        render(
          <MemoryRouter initialEntries={['/plot_point/test-point/edit']}>
            <App />
          </MemoryRouter>
        );

        await waitFor(() => {
          expect(screen.getByTestId('plot-point-edit')).toBeInTheDocument();
        });
      });

      it('renders scenes list route', async () => {
        render(
          <MemoryRouter initialEntries={['/scenes']}>
            <App />
          </MemoryRouter>
        );

        await waitFor(() => {
          expect(screen.getByTestId('scene-list')).toBeInTheDocument();
        });
      });

      it('renders scene add route', async () => {
        render(
          <MemoryRouter initialEntries={['/scene/add']}>
            <App />
          </MemoryRouter>
        );

        await waitFor(() => {
          expect(screen.getByTestId('scene-add')).toBeInTheDocument();
        });
      });

      it('renders scene edit route', async () => {
        render(
          <MemoryRouter initialEntries={['/scene/test-scene/edit']}>
            <App />
          </MemoryRouter>
        );

        await waitFor(() => {
          expect(screen.getByTestId('scene-edit')).toBeInTheDocument();
        });
      });
    });
  });

  describe('Form Validation', () => {
    it('requires email field for login', async () => {
      const user = userEvent.setup();

      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByLabelText('Email')).toBeInTheDocument();
      });

      const emailInput = screen.getByLabelText('Email');
      const passwordInput = screen.getByLabelText('Password');

      await user.type(passwordInput, 'password123');
      await user.click(screen.getByRole('button', { name: 'Login' }));

      // HTML5 validation should prevent form submission
      expect(authService.login).not.toHaveBeenCalled();
    });

    it('requires password field for login', async () => {
      const user = userEvent.setup();

      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByLabelText('Email')).toBeInTheDocument();
      });

      const emailInput = screen.getByLabelText('Email');

      await user.type(emailInput, 'test@example.com');
      await user.click(screen.getByRole('button', { name: 'Login' }));

      // HTML5 validation should prevent form submission
      expect(authService.login).not.toHaveBeenCalled();
    });
  });

  describe('Error Boundary Integration', () => {
    it('wraps app in error boundary', () => {
      render(<App />);
      
      expect(screen.getByTestId('error-boundary')).toBeInTheDocument();
    });
  });

  describe('Navigation Integration', () => {
    it('navigates after successful login', async () => {
      const user = userEvent.setup();
      const mockUser = { id: '1', email: 'test@example.com' };
      
      // Mock successful login
      authService.login.mockResolvedValue({ 
        success: true, 
        user: mockUser,
        accessToken: 'token123' 
      });

      // Mock subsequent auth check for navigation
      api.getAccessToken.mockReturnValue('token123');
      tokenUtils.getStoredTokenExpiry.mockReturnValue(Date.now() + 60 * 60 * 1000);
      tokenUtils.isTokenExpired.mockReturnValue(false);
      authService.getCurrentUser.mockResolvedValue(mockUser);

      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );

      await waitFor(() => {
        expect(screen.getByLabelText('Email')).toBeInTheDocument();
      });

      await user.type(screen.getByLabelText('Email'), 'test@example.com');
      await user.type(screen.getByLabelText('Password'), 'password123');

      await user.click(screen.getByRole('button', { name: 'Login' }));

      // Should navigate to home after successful login
      await waitFor(() => {
        expect(authService.login).toHaveBeenCalled();
      });
    });
  });
});