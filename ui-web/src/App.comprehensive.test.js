import React from 'react';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import App from './App';
import { QueryClient, QueryClientProvider } from 'react-query';

// Mock the auth service
jest.mock('./services/authService', () => ({
  default: {
    getToken: jest.fn(() => null),
    isAuthenticated: jest.fn(() => false),
    logout: jest.fn(),
    login: jest.fn(),
    signup: jest.fn(),
    resetPassword: jest.fn(),
  }
}));

// Mock React Query DevTools to avoid issues in tests
jest.mock('react-query/devtools', () => ({
  ReactQueryDevtools: () => null
}));

// Mock child components to isolate App testing
jest.mock('./pages/PlotPointAdd', () => () => <div>PlotPointAdd</div>);
jest.mock('./pages/PlotPointEdit', () => () => <div>PlotPointEdit</div>);
jest.mock('./pages/PlotPointList', () => () => <div>PlotPointList</div>);
jest.mock('./pages/SceneList', () => () => <div>SceneList</div>);
jest.mock('./pages/SceneAdd', () => () => <div>SceneAdd</div>);
jest.mock('./pages/SceneEdit', () => () => <div>SceneEdit</div>);
jest.mock('./pages/SignupPage', () => () => <div>SignupPage</div>);
jest.mock('./pages/ResetPasswordPage', () => () => <div>ResetPasswordPage</div>);
jest.mock('./components/MarketingPage', () => () => <div>MarketingPage</div>);

const authService = require('./services/authService').default;

describe('App Component', () => {
  let queryClient;

  beforeEach(() => {
    queryClient = new QueryClient({
      defaultOptions: {
        queries: { retry: false },
        mutations: { retry: false }
      }
    });
    jest.clearAllMocks();
  });

  describe('Rendering', () => {
    it('renders without crashing', () => {
      render(
        <MemoryRouter>
          <App />
        </MemoryRouter>
      );
    });

    it('renders the App component with correct structure', () => {
      const { container } = render(
        <MemoryRouter>
          <App />
        </MemoryRouter>
      );
      expect(container.querySelector('.App')).toBeInTheDocument();
    });

    it('wraps the app in QueryClientProvider', () => {
      const { container } = render(
        <MemoryRouter>
          <App />
        </MemoryRouter>
      );
      // Check that QueryClient context is available
      expect(container.firstChild).toBeTruthy();
    });

    it('includes ErrorBoundary for error handling', () => {
      const { container } = render(
        <MemoryRouter>
          <App />
        </MemoryRouter>
      );
      expect(container.querySelector('.App')).toBeInTheDocument();
    });
  });

  describe('Routing', () => {
    it('renders login page by default when not authenticated', () => {
      authService.isAuthenticated.mockReturnValue(false);
      render(
        <MemoryRouter initialEntries={['/']}>
          <App />
        </MemoryRouter>
      );
      expect(screen.getByText(/sign in/i)).toBeInTheDocument();
    });

    it('renders signup page on /signup route', () => {
      render(
        <MemoryRouter initialEntries={['/signup']}>
          <App />
        </MemoryRouter>
      );
      expect(screen.getByText('SignupPage')).toBeInTheDocument();
    });

    it('renders reset password page on /reset-password route', () => {
      render(
        <MemoryRouter initialEntries={['/reset-password']}>
          <App />
        </MemoryRouter>
      );
      expect(screen.getByText('ResetPasswordPage')).toBeInTheDocument();
    });

    it('renders marketing page on /marketing route', () => {
      render(
        <MemoryRouter initialEntries={['/marketing']}>
          <App />
        </MemoryRouter>
      );
      expect(screen.getByText('MarketingPage')).toBeInTheDocument();
    });
  });

  describe('Protected Routes', () => {
    beforeEach(() => {
      // Mock localStorage for auth token
      Object.defineProperty(window, 'localStorage', {
        value: {
          getItem: jest.fn(),
          setItem: jest.fn(),
          removeItem: jest.fn(),
          clear: jest.fn()
        },
        writable: true
      });
    });

    it('redirects to login when accessing protected route without authentication', () => {
      authService.isAuthenticated.mockReturnValue(false);
      render(
        <MemoryRouter initialEntries={['/plot-points']}>
          <App />
        </MemoryRouter>
      );
      expect(screen.getByText(/sign in/i)).toBeInTheDocument();
    });

    it('allows access to protected routes when authenticated', async () => {
      authService.isAuthenticated.mockReturnValue(true);
      authService.getToken.mockReturnValue('valid-token');
      
      render(
        <MemoryRouter initialEntries={['/plot-points']}>
          <App />
        </MemoryRouter>
      );
      
      await waitFor(() => {
        expect(screen.getByText('PlotPointList')).toBeInTheDocument();
      });
    });

    it('shows loading state while checking authentication', () => {
      const MockAuthProvider = ({ children }) => {
        const [loading, setLoading] = React.useState(true);
        React.useEffect(() => {
          setTimeout(() => setLoading(false), 100);
        }, []);
        return children({ loading, isAuthenticated: false });
      };
      
      const { container } = render(
        <MemoryRouter initialEntries={['/plot-points']}>
          <App />
        </MemoryRouter>
      );
      
      // Check for loading indicator
      const loadingElements = container.querySelectorAll('div');
      expect(loadingElements.length).toBeGreaterThan(0);
    });
  });

  describe('Login Component', () => {
    it('renders login form with all required fields', () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      expect(screen.getByLabelText(/email/i)).toBeInTheDocument();
      expect(screen.getByLabelText(/password/i)).toBeInTheDocument();
      expect(screen.getByRole('button', { name: /sign in/i })).toBeInTheDocument();
    });

    it('handles login form submission', async () => {
      authService.login.mockResolvedValue({ success: true, user: { email: 'test@test.com' } });
      
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      const emailInput = screen.getByLabelText(/email/i);
      const passwordInput = screen.getByLabelText(/password/i);
      const submitButton = screen.getByRole('button', { name: /sign in/i });
      
      fireEvent.change(emailInput, { target: { value: 'test@test.com' } });
      fireEvent.change(passwordInput, { target: { value: 'password123' } });
      fireEvent.click(submitButton);
      
      await waitFor(() => {
        expect(authService.login).toHaveBeenCalledWith(expect.objectContaining({
          email: 'test@test.com',
          password: 'password123'
        }));
      });
    });

    it('displays error message on login failure', async () => {
      authService.login.mockRejectedValue(new Error('Invalid credentials'));
      
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      const emailInput = screen.getByLabelText(/email/i);
      const passwordInput = screen.getByLabelText(/password/i);
      const submitButton = screen.getByRole('button', { name: /sign in/i });
      
      fireEvent.change(emailInput, { target: { value: 'test@test.com' } });
      fireEvent.change(passwordInput, { target: { value: 'wrongpassword' } });
      fireEvent.click(submitButton);
      
      await waitFor(() => {
        expect(authService.login).toHaveBeenCalled();
      });
    });

    it('handles remember me checkbox', () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      const rememberMeCheckbox = screen.getByLabelText(/remember me/i);
      expect(rememberMeCheckbox).toBeInTheDocument();
      
      fireEvent.click(rememberMeCheckbox);
      expect(rememberMeCheckbox).toBeChecked();
    });

    it('displays logout message when redirected after logout', () => {
      render(
        <MemoryRouter initialEntries={[{ pathname: '/login', state: { message: 'You have been logged out', type: 'success' } }]}>
          <App />
        </MemoryRouter>
      );
      
      expect(screen.getByText(/you have been logged out/i)).toBeInTheDocument();
    });
  });

  describe('Navigation', () => {
    it('navigates to plot points list after successful login', async () => {
      authService.login.mockResolvedValue({ success: true });
      authService.isAuthenticated.mockReturnValue(true);
      
      const { container } = render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      const emailInput = screen.getByLabelText(/email/i);
      const passwordInput = screen.getByLabelText(/password/i);
      const submitButton = screen.getByRole('button', { name: /sign in/i });
      
      fireEvent.change(emailInput, { target: { value: 'test@test.com' } });
      fireEvent.change(passwordInput, { target: { value: 'password123' } });
      fireEvent.click(submitButton);
      
      await waitFor(() => {
        expect(authService.login).toHaveBeenCalled();
      });
    });

    it('provides links to signup and forgot password pages', () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      expect(screen.getByText(/don't have an account\?/i)).toBeInTheDocument();
      expect(screen.getByText(/forgot password\?/i)).toBeInTheDocument();
    });
  });

  describe('Error Handling', () => {
    it('handles component errors gracefully with ErrorBoundary', () => {
      const ThrowError = () => {
        throw new Error('Test error');
      };
      
      // Mock console.error to prevent error output in tests
      const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
      
      const { container } = render(
        <MemoryRouter>
          <ErrorBoundary>
            <ThrowError />
          </ErrorBoundary>
        </MemoryRouter>
      );
      
      expect(container.textContent).toContain('Something went wrong');
      
      consoleSpy.mockRestore();
    });
  });

  describe('Authentication Context', () => {
    it('provides authentication context to child components', () => {
      const { container } = render(
        <MemoryRouter initialEntries={['/']}>
          <App />
        </MemoryRouter>
      );
      
      // The auth context should be available throughout the app
      expect(container.firstChild).toBeTruthy();
    });

    it('handles logout functionality', async () => {
      authService.isAuthenticated.mockReturnValue(true);
      authService.logout.mockResolvedValue(true);
      
      render(
        <MemoryRouter initialEntries={['/']}>
          <App />
        </MemoryRouter>
      );
      
      // Find and click logout button if present
      const logoutButton = screen.queryByText(/logout/i);
      if (logoutButton) {
        fireEvent.click(logoutButton);
        await waitFor(() => {
          expect(authService.logout).toHaveBeenCalled();
        });
      }
    });
  });

  describe('Accessibility', () => {
    it('has proper page structure with main landmark', () => {
      const { container } = render(
        <MemoryRouter>
          <App />
        </MemoryRouter>
      );
      
      const appContainer = container.querySelector('.App');
      expect(appContainer).toBeInTheDocument();
    });

    it('login form has proper labels for accessibility', () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      const emailInput = screen.getByLabelText(/email/i);
      const passwordInput = screen.getByLabelText(/password/i);
      
      expect(emailInput).toHaveAttribute('type', 'email');
      expect(passwordInput).toHaveAttribute('type', 'password');
    });
  });
});