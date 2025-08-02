import React from 'react';
import { render } from '@testing-library/react';
import App from './App';

// Mock the auth service
jest.mock('./services/authService', () => ({
  default: {
    getToken: jest.fn(() => null),
    isAuthenticated: jest.fn(() => false),
    logout: jest.fn(),
  }
}));

// Mock React Query DevTools to avoid issues in tests
jest.mock('react-query/devtools', () => ({
  ReactQueryDevtools: () => null
}));

it('renders without crashing', () => {
  render(<App />);
});
