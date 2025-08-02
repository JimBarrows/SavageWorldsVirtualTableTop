import React from 'react';
import PropTypes from 'prop-types';
import { BrowserRouter as Router, Route, Routes, Navigate } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import { ReactQueryDevtools } from 'react-query/devtools';
import './App.css';
import { AuthProvider, useAuth } from './contexts/AuthContext';
import Header from './components/layout/Header';
import PlotPointAdd from './pages/PlotPointAdd';
import PlotPointEdit from './pages/PlotPointEdit';
import PlotPointList from './pages/PlotPointList';
import SignupPage from './pages/SignupPage';
import ErrorBoundary from './components/ErrorBoundary';

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
  const [credentials, setCredentials] = React.useState({ email: '', password: '' });
  const [isLoading, setIsLoading] = React.useState(false);
  
  const handleSubmit = async (e) => {
    e.preventDefault();
    setIsLoading(true);
    try {
      // Backend expects username, not email
      // Generate username from email (same logic as signup)
      const emailParts = credentials.email.split('@');
      let username = emailParts[0]
        .toLowerCase()
        .replace(/[^a-z0-9_-]/g, '')
        .substring(0, 30);
      
      if (username.length < 3) {
        username = 'user' + Date.now().toString().slice(-6);
      }

      const loginData = {
        username: username, // TODO: Backend should be updated to accept email for login
        password: credentials.password
      };
      console.log('Login attempt with:', { email: credentials.email, username, password: '***' });
      const result = await login(loginData);
      console.log('Login result:', result);
      if (result.success) {
        window.location.href = '/';
      }
    } catch (err) {
      console.error('Login error:', err);
    } finally {
      setIsLoading(false);
    }
  };
  
  return (
    <div className="container mt-5">
      <div className="row justify-content-center">
        <div className="col-md-6">
          <div className="card">
            <div className="card-header">
              <h3>Login</h3>
            </div>
            <div className="card-body">
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
                <button type="submit" className="btn btn-primary" disabled={isLoading}>
                  {isLoading ? 'Logging in...' : 'Login'}
                </button>
                <a href="/signup" className="btn btn-link">
                  Need an account? Sign up
                </a>
              </form>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
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
            <Route path="/login" element={<Login />} />
            <Route path="/signup" element={<SignupPage />} />
            <Route
              path="/"
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