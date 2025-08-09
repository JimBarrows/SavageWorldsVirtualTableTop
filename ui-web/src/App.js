import React from 'react';
import PropTypes from 'prop-types';
import { BrowserRouter as Router, Route, Routes, Navigate, useLocation, useNavigate } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';
import './App.css';
import { AuthProvider, useAuth } from './contexts/AuthContext';
import Header from './components/layout/Header';
import PlotPointAdd from './pages/PlotPointAdd';
import PlotPointEdit from './pages/PlotPointEdit';
import PlotPointList from './pages/PlotPointList';
import SignupPage from './pages/SignupPage';
import ResetPasswordPage from './pages/ResetPasswordPage';
import ErrorBoundary from './components/ErrorBoundary';
import RememberMe from './components/RememberMe';
import MarketingPage from './components/MarketingPage';
import BrandingBanner from './components/BrandingBanner';

import config from './config';

// Configure React Query
const queryClient = new QueryClient({
  defaultOptions: {
    queries: config.query,
  },
});

// Protected Route Component
const ProtectedRoute = ({ children }) => {
  const { isAuthenticated, loading } = useAuth();
  
  if (loading) {
    return <div>Loading...</div>;
  }
  
  if (!isAuthenticated) {
    return <Navigate to="/login" replace />;
  }
  
  return children;
};

ProtectedRoute.propTypes = {
  children: PropTypes.node.isRequired
};

// Login Component
const Login = () => {
  const { login, error } = useAuth();
  const location = useLocation();
  const navigate = useNavigate();
  const [credentials, setCredentials] = React.useState({ email: '', password: '' });
  const [rememberMe, setRememberMe] = React.useState(false);
  const [isLoading, setIsLoading] = React.useState(false);
  const [logoutMessage, setLogoutMessage] = React.useState(null);
  
  // Check for logout message from navigation state
  React.useEffect(() => {
    if (location.state?.message) {
      setLogoutMessage({
        text: location.state.message,
        type: location.state.type || 'info'
      });
      // Clear the message after 5 seconds
      const timer = setTimeout(() => {
        setLogoutMessage(null);
      }, 5000);
      return () => clearTimeout(timer);
    }
  }, [location.state]);
  
  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsLoading(true);
    try {
      // Send credentials with email field to backend
      const loginData = {
        email: credentials.email,
        password: credentials.password,
        rememberMe: rememberMe
      };
      console.log('Login attempt with:', { email: credentials.email, password: '***', rememberMe });
      const result = await login(loginData);
      if (result.success) {
        // Use React Router navigation instead of hard redirect
        // This prevents race conditions and maintains React state
        navigate('/');
      }
    } catch (err) {
      console.error('Login error:', err);
    } finally {
      setIsLoading(false);
    }
  };
  
  return (
    <div className="login-page">
      <BrandingBanner compact={true} />
      <div className="container mt-5">
        <div className="row justify-content-center">
          <div className="col-md-6">
            <div className="card">
            <div className="card-header">
              <h3>Login</h3>
            </div>
            <div className="card-body">
              {logoutMessage && (
                <div 
                  className={`alert alert-${logoutMessage.type === 'success' ? 'success' : 'info'}`} 
                  role="alert"
                  data-testid="logout-success"
                >
                  {logoutMessage.text}
                </div>
              )}
              {error && (
                <div className="alert alert-danger" role="alert">
                  {error}
                </div>
              )}
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
                <div className="form-group mb-3">
                  <RememberMe 
                    checked={rememberMe} 
                    onChange={setRememberMe} 
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
  );
};

// Home Route Component - shows marketing page or redirects to plot points
const HomeRoute = () => {
  const { isAuthenticated, loading } = useAuth();
  
  if (loading) {
    return <div>Loading...</div>;
  }
  
  if (isAuthenticated) {
    return <Navigate to="/plot-points" replace />;
  }
  
  return <MarketingPage />;
};

// Main App Component
function AppContent() {
  const { isAuthenticated } = useAuth();
  
  return (
    <Router>
      <div>
        {isAuthenticated && <Header id={'app'} />}
        <div id="layout" className="container" role="main">
          <Routes>
            <Route path="/" element={<HomeRoute />} />
            <Route path="/login" element={<Login />} />
            <Route path="/signup" element={<SignupPage />} />
            <Route path="/reset-password" element={<ResetPasswordPage />} />
            <Route
              path="/plot-points"
              element={
                <ProtectedRoute>
                  <PlotPointList />
                </ProtectedRoute>
              }
            />
            <Route
              path="/plot_point/add"
              element={
                <ProtectedRoute>
                  <PlotPointAdd />
                </ProtectedRoute>
              }
            />
            <Route
              path="/plot_point/:name/edit"
              element={
                <ProtectedRoute>
                  <PlotPointEdit />
                </ProtectedRoute>
              }
            />
          </Routes>
        </div>
      </div>
    </Router>
  );
}

export default function App() {
  return (
    <ErrorBoundary>
      <QueryClientProvider client={queryClient}>
        <AuthProvider>
          <AppContent />
        </AuthProvider>
        <ReactQueryDevtools initialIsOpen={false} />
      </QueryClientProvider>
    </ErrorBoundary>
  );
}