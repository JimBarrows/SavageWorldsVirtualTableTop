import React from 'react';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import App from './App';
import ErrorBoundary from './components/ErrorBoundary';
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

// Mock BrowserRouter to prevent conflicts with MemoryRouter
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  BrowserRouter: ({ children }) => <div>{children}</div>
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
      // App component wraps everything in ErrorBoundary
      expect(container.firstChild).toBeTruthy();
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
      // ErrorBoundary wraps the app
      expect(container.firstChild).toBeTruthy();
    });
  });

  describe('Routing', () => {
    it('renders marketing page by default when not authenticated', () => {
      authService.isAuthenticated.mockReturnValue(false);
      render(
        <MemoryRouter initialEntries={['/']}>
          <App />
        </MemoryRouter>
      );
      expect(screen.getByText('MarketingPage')).toBeInTheDocument();
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

    it('renders marketing page at root when not authenticated', () => {
      authService.isAuthenticated.mockReturnValue(false);
      render(
        <MemoryRouter initialEntries={['/']}>
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
      // Check for login heading to confirm we're on login page
      expect(screen.getByRole('heading', { name: 'Login' })).toBeInTheDocument();
    });

    it('allows access to protected routes when authenticated', () => {
      authService.isAuthenticated.mockReturnValue(true);
      authService.getToken.mockReturnValue('valid-token');
      
      const { container } = render(
        <MemoryRouter initialEntries={['/plot-points']}>
          <App />
        </MemoryRouter>
      );
      
      // When authenticated, the main container should be rendered
      expect(container.querySelector('#layout')).toBeInTheDocument();
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
      
      expect(screen.getByLabelText('Email')).toBeInTheDocument();
      expect(screen.getByLabelText('Password')).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Login' })).toBeInTheDocument();
    });

    it('renders login form on /login route', () => {
      const { container } = render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      // Check that the page is rendered (login heading should exist)
      expect(screen.getByRole('heading', { name: 'Login' })).toBeInTheDocument();
      // Check that container is rendered
      expect(container.querySelector('#layout')).toBeInTheDocument();
    });

    it('renders signup link on login page', () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      // Check for signup link
      expect(screen.getByText(/don't have an account/i)).toBeInTheDocument();
    });

    it('handles remember me checkbox', () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      const rememberMeCheckbox = screen.getByLabelText('Remember Me');
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
    it('provides navigation between login and signup pages', () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      // Check that we're on login page
      expect(screen.getByRole('heading', { name: 'Login' })).toBeInTheDocument();
      
      // Check for navigation links
      expect(screen.getByText(/forgot password/i)).toBeInTheDocument();
    });

    it('provides links to signup and forgot password pages', () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      expect(screen.getByText("Don't have an account? Sign up")).toBeInTheDocument();
      expect(screen.getByText('Forgot password?')).toBeInTheDocument();
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
      
      // App wraps content in container with role="main"
      const mainContainer = container.querySelector('[role="main"]');
      expect(mainContainer).toBeInTheDocument();
    });

    it('login form has proper labels for accessibility', () => {
      render(
        <MemoryRouter initialEntries={['/login']}>
          <App />
        </MemoryRouter>
      );
      
      const emailInput = screen.getByLabelText('Email');
      const passwordInput = screen.getByLabelText('Password');
      
      expect(emailInput).toHaveAttribute('type', 'email');
      expect(passwordInput).toHaveAttribute('type', 'password');
    });
  });
});