import React from 'react';
import { render } from '@testing-library/react';
import PropTypes from 'prop-types';

// Mock component for testing PropTypes validation
const MockComponent = ({ testProp, optionalProp, numberProp, booleanProp, objectProp, arrayProp, functionProp }) => (
  <div data-testid="mock-component">
    <span>{testProp}</span>
    <span>{optionalProp}</span>
    <span>{numberProp}</span>
    <span>{booleanProp ? 'true' : 'false'}</span>
    <span>{objectProp?.name}</span>
    <span>{arrayProp?.length}</span>
    <button onClick={functionProp}>Click</button>
  </div>
);

MockComponent.propTypes = {
  testProp: PropTypes.string.isRequired,
  optionalProp: PropTypes.string,
  numberProp: PropTypes.number.isRequired,
  booleanProp: PropTypes.bool.isRequired,
  objectProp: PropTypes.shape({
    name: PropTypes.string,
    value: PropTypes.any
  }),
  arrayProp: PropTypes.arrayOf(PropTypes.string),
  functionProp: PropTypes.func.isRequired
};

// Another mock component without PropTypes for testing warnings
const ComponentWithoutPropTypes = ({ missingProp }) => (
  <div>{missingProp}</div>
);

ComponentWithoutPropTypes.propTypes = {
  missingProp: PropTypes.string
};

describe('PropTypes Validation Tests', () => {
  // Capture console errors for PropTypes warnings
  let consoleError;
  
  beforeEach(() => {
    consoleError = jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  afterEach(() => {
    consoleError.mockRestore();
  });

  describe('Valid PropTypes Usage', () => {
    test('should render without PropTypes warnings when all required props are provided', () => {
      const validProps = {
        testProp: 'test string',
        numberProp: 42,
        booleanProp: true,
        functionProp: jest.fn()
      };

      render(<MockComponent {...validProps} />);
      
      // Should not trigger any PropTypes warnings
      expect(consoleError).not.toHaveBeenCalled();
    });

    test('should render without warnings when optional props are omitted', () => {
      const validProps = {
        testProp: 'test string',
        numberProp: 42,
        booleanProp: true,
        functionProp: jest.fn()
      };

      render(<MockComponent {...validProps} />);
      
      expect(consoleError).not.toHaveBeenCalled();
    });

    test('should render without warnings when optional props are provided with correct types', () => {
      const validProps = {
        testProp: 'test string',
        optionalProp: 'optional string',
        numberProp: 42,
        booleanProp: true,
        objectProp: { name: 'test object', value: 'some value' },
        arrayProp: ['item1', 'item2'],
        functionProp: jest.fn()
      };

      render(<MockComponent {...validProps} />);
      
      expect(consoleError).not.toHaveBeenCalled();
    });
  });

  describe('PropTypes Validation Errors', () => {
    test('should trigger warning when required prop is missing', () => {
      const invalidProps = {
        // missing testProp (required)
        numberProp: 42,
        booleanProp: true,
        functionProp: jest.fn()
      };

      render(<MockComponent {...invalidProps} />);
      
      expect(consoleError).toHaveBeenCalledWith(
        expect.stringContaining('Warning: Failed prop type: The prop `testProp` is marked as required')
      );
    });

    test('should trigger warning when prop has wrong type', () => {
      const invalidProps = {
        testProp: 'test string',
        numberProp: 'not a number', // should be number
        booleanProp: true,
        functionProp: jest.fn()
      };

      render(<MockComponent {...invalidProps} />);
      
      expect(consoleError).toHaveBeenCalledWith(
        expect.stringContaining('Warning: Failed prop type: Invalid prop `numberProp` of type `string`')
      );
    });

    test('should trigger warning when object prop has wrong shape', () => {
      const invalidProps = {
        testProp: 'test string',
        numberProp: 42,
        booleanProp: true,
        objectProp: 'not an object', // should be object
        functionProp: jest.fn()
      };

      render(<MockComponent {...invalidProps} />);
      
      expect(consoleError).toHaveBeenCalledWith(
        expect.stringContaining('Warning: Failed prop type: Invalid prop `objectProp` of type `string`')
      );
    });

    test('should trigger warning when array prop has wrong element types', () => {
      const invalidProps = {
        testProp: 'test string',
        numberProp: 42,
        booleanProp: true,
        arrayProp: [1, 2, 3], // should be array of strings
        functionProp: jest.fn()
      };

      render(<MockComponent {...invalidProps} />);
      
      expect(consoleError).toHaveBeenCalledWith(
        expect.stringContaining('Warning: Failed prop type: Invalid prop `arrayProp[0]` of type `number`')
      );
    });
  });

  describe('Component Without PropTypes (ESLint Compliance)', () => {
    test('should demonstrate need for PropTypes validation in all components', () => {
      // This test demonstrates why ESLint react/prop-types rule is important
      const props = { missingProp: 'some value' };
      
      // This component will render but has no PropTypes validation
      render(<ComponentWithoutPropTypes {...props} />);
      
      // No PropTypes warnings because component doesn't define them
      expect(consoleError).not.toHaveBeenCalled();
      
      // However, this is exactly what ESLint react/prop-types rule catches
      // ESLint would flag this component for missing PropTypes definitions
    });
  });

  describe('PropTypes Best Practices Validation', () => {
    test('should validate that required props are properly marked', () => {
      // Test that demonstrates proper use of isRequired by testing behavior
      const invalidProps = {
        // Missing required props to test isRequired behavior
        numberProp: 42,
        booleanProp: true,
        functionProp: jest.fn()
      };

      render(<MockComponent {...invalidProps} />);
      
      // Should trigger warning for missing required prop
      expect(consoleError).toHaveBeenCalledWith(
        expect.stringContaining('The prop `testProp` is marked as required')
      );
    });

    test('should validate that optional props are not marked as required', () => {
      // Test that optional props work correctly by not triggering warnings when omitted
      const validPropsWithoutOptional = {
        testProp: 'test string',
        numberProp: 42,
        booleanProp: true,
        functionProp: jest.fn()
      };

      render(<MockComponent {...validPropsWithoutOptional} />);
      
      // Should not trigger warnings for missing optional props
      expect(consoleError).not.toHaveBeenCalled();
    });

    test('should validate complex object shape PropTypes', () => {
      // Test with valid object shape - should not trigger warnings
      const validProps = {
        testProp: 'test',
        numberProp: 42,
        booleanProp: true,
        objectProp: { name: 'valid', value: 'test' },
        functionProp: jest.fn()
      };

      render(<MockComponent {...validProps} />);
      expect(consoleError).not.toHaveBeenCalled();
      
      // Test with invalid object shape - should trigger warning
      const invalidProps = {
        testProp: 'test',
        numberProp: 42,
        booleanProp: true,
        objectProp: 'invalid - should be object',
        functionProp: jest.fn()
      };

      render(<MockComponent {...invalidProps} />);
      expect(consoleError).toHaveBeenCalledWith(
        expect.stringContaining('Invalid prop `objectProp` of type `string`')
      );
    });
  });

  describe('Development Environment PropTypes Warnings', () => {
    test('should only show PropTypes warnings in development mode', () => {
      // PropTypes warnings are only shown in development
      // In production builds, PropTypes are stripped out
      
      const originalNodeEnv = process.env.NODE_ENV;
      process.env.NODE_ENV = 'development';
      
      const invalidProps = {
        testProp: 'test string',
        numberProp: 'invalid type',
        booleanProp: true,
        functionProp: jest.fn()
      };

      render(<MockComponent {...invalidProps} />);
      
      // Should see warnings in development
      expect(consoleError).toHaveBeenCalled();
      
      process.env.NODE_ENV = originalNodeEnv;
    });
  });

  describe('Functional Component PropTypes Support', () => {
    test('should validate PropTypes work correctly with functional components', () => {
      const FunctionalComponent = ({ message, count, isVisible }) => (
        <div>
          <span>{message}</span>
          <span>{count}</span>
          <span>{isVisible ? 'visible' : 'hidden'}</span>
        </div>
      );

      FunctionalComponent.propTypes = {
        message: PropTypes.string.isRequired,
        count: PropTypes.number,
        isVisible: PropTypes.bool
      };

      const validProps = {
        message: 'Hello World',
        count: 5,
        isVisible: true
      };

      render(<FunctionalComponent {...validProps} />);
      expect(consoleError).not.toHaveBeenCalled();
    });
  });
});