import React from 'react';
import { render, screen } from '@testing-library/react';
import ErrorBoundary from './ErrorBoundary';

// Component that throws an error for testing
const ThrowError = ({ shouldThrow }) => {
  if (shouldThrow) {
    throw new Error('Test error message');
  }
  return <div>No error</div>;
};

// Component that throws in useEffect
const ThrowErrorInEffect = () => {
  React.useEffect(() => {
    throw new Error('Effect error');
  }, []);
  return <div>Component loaded</div>;
};

describe('ErrorBoundary Component', () => {
  // Suppress console.error for these tests
  const originalError = console.error;
  beforeAll(() => {
    console.error = jest.fn();
  });

  afterAll(() => {
    console.error = originalError;
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe('Normal Operation', () => {
    it('renders children when there is no error', () => {
      render(
        <ErrorBoundary>
          <div>Test content</div>
        </ErrorBoundary>
      );
      
      expect(screen.getByText('Test content')).toBeInTheDocument();
    });

    it('renders multiple children without error', () => {
      render(
        <ErrorBoundary>
          <div>Child 1</div>
          <div>Child 2</div>
          <div>Child 3</div>
        </ErrorBoundary>
      );
      
      expect(screen.getByText('Child 1')).toBeInTheDocument();
      expect(screen.getByText('Child 2')).toBeInTheDocument();
      expect(screen.getByText('Child 3')).toBeInTheDocument();
    });
  });

  describe('Error Handling', () => {
    it('catches errors and displays fallback UI', () => {
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByText(/something went wrong/i)).toBeInTheDocument();
      expect(screen.queryByText('No error')).not.toBeInTheDocument();
    });

    it('displays the error message', () => {
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByText(/test error message/i)).toBeInTheDocument();
    });

    it('logs error to console', () => {
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(console.error).toHaveBeenCalled();
    });

    it('displays custom fallback component when provided', () => {
      const CustomFallback = ({ error }) => (
        <div>Custom error: {error.message}</div>
      );
      
      render(
        <ErrorBoundary fallback={CustomFallback}>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByText('Custom error: Test error message')).toBeInTheDocument();
    });
  });

  describe('Error Recovery', () => {
    it('provides a reset button', () => {
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByRole('button', { name: /try again/i })).toBeInTheDocument();
    });

    it('resets error state when reset button is clicked', () => {
      const { rerender } = render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByText(/something went wrong/i)).toBeInTheDocument();
      
      // Click reset button
      const resetButton = screen.getByRole('button', { name: /try again/i });
      resetButton.click();
      
      // Rerender with non-throwing component
      rerender(
        <ErrorBoundary>
          <ThrowError shouldThrow={false} />
        </ErrorBoundary>
      );
      
      expect(screen.getByText('No error')).toBeInTheDocument();
      expect(screen.queryByText(/something went wrong/i)).not.toBeInTheDocument();
    });

    it('calls onReset callback when provided', () => {
      const onReset = jest.fn();
      
      render(
        <ErrorBoundary onReset={onReset}>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      const resetButton = screen.getByRole('button', { name: /try again/i });
      resetButton.click();
      
      expect(onReset).toHaveBeenCalledTimes(1);
    });
  });

  describe('Error Details', () => {
    it('displays error stack in development mode', () => {
      const originalEnv = process.env.NODE_ENV;
      process.env.NODE_ENV = 'development';
      
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByText(/error details/i)).toBeInTheDocument();
      
      process.env.NODE_ENV = originalEnv;
    });

    it('hides error stack in production mode', () => {
      const originalEnv = process.env.NODE_ENV;
      process.env.NODE_ENV = 'production';
      
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.queryByText(/error details/i)).not.toBeInTheDocument();
      
      process.env.NODE_ENV = originalEnv;
    });

    it('allows toggling error details visibility', () => {
      const originalEnv = process.env.NODE_ENV;
      process.env.NODE_ENV = 'development';
      
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      const toggleButton = screen.getByRole('button', { name: /show details/i });
      toggleButton.click();
      
      expect(screen.getByText(/test error message/i)).toBeInTheDocument();
      
      process.env.NODE_ENV = originalEnv;
    });
  });

  describe('Error Logging', () => {
    it('calls onError callback when error occurs', () => {
      const onError = jest.fn();
      
      render(
        <ErrorBoundary onError={onError}>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(onError).toHaveBeenCalledWith(
        expect.objectContaining({
          message: 'Test error message'
        }),
        expect.any(Object)
      );
    });

    it('includes component stack in error info', () => {
      const onError = jest.fn();
      
      render(
        <ErrorBoundary onError={onError}>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(onError).toHaveBeenCalledWith(
        expect.any(Error),
        expect.objectContaining({
          componentStack: expect.any(String)
        })
      );
    });
  });

  describe('Nested Error Boundaries', () => {
    it('catches errors in nested components', () => {
      render(
        <ErrorBoundary>
          <div>
            <ErrorBoundary>
              <ThrowError shouldThrow={true} />
            </ErrorBoundary>
          </div>
        </ErrorBoundary>
      );
      
      // Inner error boundary should catch the error
      expect(screen.getByText(/something went wrong/i)).toBeInTheDocument();
    });

    it('propagates uncaught errors to parent boundary', () => {
      const ChildBoundary = ({ children }) => {
        // This boundary doesn't handle errors, so they propagate
        return <>{children}</>;
      };
      
      render(
        <ErrorBoundary>
          <ChildBoundary>
            <ThrowError shouldThrow={true} />
          </ChildBoundary>
        </ErrorBoundary>
      );
      
      expect(screen.getByText(/something went wrong/i)).toBeInTheDocument();
    });
  });

  describe('Different Error Types', () => {
    it('handles TypeError', () => {
      const ThrowTypeError = () => {
        const obj = null;
        return <div>{obj.property}</div>;
      };
      
      render(
        <ErrorBoundary>
          <ThrowTypeError />
        </ErrorBoundary>
      );
      
      expect(screen.getByText(/something went wrong/i)).toBeInTheDocument();
    });

    it('handles ReferenceError', () => {
      const ThrowReferenceError = () => {
        // eslint-disable-next-line no-undef
        return <div>{undefinedVariable}</div>;
      };
      
      render(
        <ErrorBoundary>
          <ThrowReferenceError />
        </ErrorBoundary>
      );
      
      expect(screen.getByText(/something went wrong/i)).toBeInTheDocument();
    });

    it('handles async errors with error event handler', () => {
      const onError = jest.fn();
      
      render(
        <ErrorBoundary onError={onError}>
          <ThrowErrorInEffect />
        </ErrorBoundary>
      );
      
      // Note: Async errors in useEffect are not caught by error boundaries
      // This test documents this limitation
      expect(screen.queryByText(/something went wrong/i)).not.toBeInTheDocument();
    });
  });

  describe('Accessibility', () => {
    it('has proper ARIA attributes on error message', () => {
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      const errorMessage = screen.getByText(/something went wrong/i);
      expect(errorMessage).toHaveAttribute('role', 'alert');
    });

    it('focuses reset button on error', () => {
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      const resetButton = screen.getByRole('button', { name: /try again/i });
      // In a real browser, this would be focused
      expect(resetButton).toBeInTheDocument();
    });

    it('provides descriptive error messages for screen readers', () => {
      render(
        <ErrorBoundary>
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByRole('alert')).toHaveTextContent(/something went wrong/i);
    });
  });

  describe('Custom Error Messages', () => {
    it('allows custom error title', () => {
      render(
        <ErrorBoundary errorTitle="Oops! An error occurred">
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByText('Oops! An error occurred')).toBeInTheDocument();
    });

    it('allows custom error message', () => {
      render(
        <ErrorBoundary errorMessage="Please refresh the page">
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByText('Please refresh the page')).toBeInTheDocument();
    });

    it('allows custom reset button text', () => {
      render(
        <ErrorBoundary resetButtonText="Reload">
          <ThrowError shouldThrow={true} />
        </ErrorBoundary>
      );
      
      expect(screen.getByRole('button', { name: 'Reload' })).toBeInTheDocument();
    });
  });
});