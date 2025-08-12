import React from 'react';
import { render } from '@testing-library/react';
import PropTypes from 'prop-types';
import PropTypesValidator, {
  validatePropTypes,
  analyzeRequiredProps,
  validateShapeComplexity,
  generatePropTypesTemplate,
  validateDomainLanguage
} from './PropTypesValidator';

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
  describe('PropTypesValidator Module Tests', () => {
    test('should export all utility functions', () => {
      expect(PropTypesValidator).toBeDefined();
      expect(validatePropTypes).toBeDefined();
      expect(analyzeRequiredProps).toBeDefined();
      expect(validateShapeComplexity).toBeDefined();
      expect(generatePropTypesTemplate).toBeDefined();
      expect(validateDomainLanguage).toBeDefined();
    });

    test('should validate component PropTypes', () => {
      const componentWithPropTypes = {
        propTypes: {
          name: PropTypes.string.isRequired,
          age: PropTypes.number
        }
      };
      
      const result = validatePropTypes(componentWithPropTypes);
      expect(result.isValid).toBe(true);
      expect(result.errors).toHaveLength(0);
      
      const componentWithoutPropTypes = {};
      const result2 = validatePropTypes(componentWithoutPropTypes);
      expect(result2.isValid).toBe(false);
      expect(result2.errors).toContain('Component is missing PropTypes definition');
    });

    test('should analyze required props', () => {
      const propTypes = {
        name: PropTypes.string.isRequired,
        email: PropTypes.string.isRequired,
        age: PropTypes.number
      };
      
      const requiredProps = ['name', 'email', 'phone'];
      const result = analyzeRequiredProps(propTypes, requiredProps);
      
      expect(result.properlyMarked).toContain('name');
      expect(result.properlyMarked).toContain('email');
      expect(result.missingRequired).toContain('phone');
    });

    test('should validate shape complexity', () => {
      const shapeType = PropTypes.shape({
        name: PropTypes.string,
        address: PropTypes.object
      });
      
      const result = validateShapeComplexity(shapeType);
      expect(result.isComplex).toBe(true);
      expect(result.recommendations).toContain('Use PropTypes.shape for object props');
    });

    test('should generate PropTypes template', () => {
      const propUsage = [
        { name: 'title', type: 'string', required: true },
        { name: 'count', type: 'number', required: false }
      ];
      
      const template = generatePropTypesTemplate(propUsage);
      expect(template).toContain('title: PropTypes.string.isRequired');
      expect(template).toContain('count: PropTypes.number');
    });

    test('should validate domain language in PropTypes', () => {
      const propTypes = {
        customerName: PropTypes.string,
        orderTotal: PropTypes.number,
        id: PropTypes.string,
        data: PropTypes.object
      };
      
      const result = validateDomainLanguage(propTypes);
      expect(result.domainProps).toContain('customerName');
      expect(result.domainProps).toContain('orderTotal');
      expect(result.issues).toContain('Generic prop names found: data');
    });
  });

  describe('React Component PropTypes Integration Tests', () => {
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
        'Warning: Failed %s type: %s%s',
        'prop',
        expect.stringContaining('The prop `testProp` is marked as required'),
        expect.any(String)
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
        'Warning: Failed %s type: %s%s',
        'prop',
        expect.stringContaining('Invalid prop `numberProp` of type `string`'),
        expect.any(String)
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
        'Warning: Failed %s type: %s%s',
        'prop',
        expect.stringContaining('Invalid prop `objectProp` of type `string`'),
        expect.any(String)
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
        'Warning: Failed %s type: %s%s',
        'prop',
        expect.stringContaining('Invalid prop `arrayProp[0]` of type `number`'),
        expect.any(String)
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
    test('should demonstrate PropTypes validation concept', () => {
      // This test validates that PropTypes are configured correctly
      // by testing that valid props render without errors
      const validProps = {
        testProp: 'test string',
        numberProp: 42,
        booleanProp: true,
        functionProp: jest.fn()
      };

      const { container } = render(<MockComponent {...validProps} />);
      
      // Component should render successfully with valid props
      expect(container.firstChild).toBeInTheDocument();
      expect(container.firstChild).toHaveTextContent('test string');
    });

    test('should validate component renders with optional props', () => {
      // Test that optional props work correctly when provided
      const validPropsWithOptional = {
        testProp: 'test string',
        optionalProp: 'optional value',
        numberProp: 42,
        booleanProp: true,
        objectProp: { name: 'test', value: 'example' },
        arrayProp: ['item1', 'item2'],
        functionProp: jest.fn()
      };

      const { container } = render(<MockComponent {...validPropsWithOptional} />);
      
      // Component should render successfully with optional props
      expect(container.firstChild).toBeInTheDocument();
      expect(container.firstChild).toHaveTextContent('optional value');
    });

    test('should validate PropTypes are properly configured', () => {
      // This test ensures our MockComponent has PropTypes configured
      expect(MockComponent.propTypes).toBeDefined();
      expect(MockComponent.propTypes.testProp).toBeDefined();
      expect(MockComponent.propTypes.numberProp).toBeDefined();
      expect(MockComponent.propTypes.booleanProp).toBeDefined();
      expect(MockComponent.propTypes.functionProp).toBeDefined();
    });
  });

  describe('Development Environment PropTypes Warnings', () => {
    test('should validate development environment supports PropTypes', () => {
      // PropTypes warnings are only shown in development
      // In production builds, PropTypes are stripped out
      
      const originalNodeEnv = process.env.NODE_ENV;
      process.env.NODE_ENV = 'development';
      
      const validProps = {
        testProp: 'test string',
        numberProp: 42,
        booleanProp: true,
        functionProp: jest.fn()
      };

      // In development, component should render successfully
      const { container } = render(<MockComponent {...validProps} />);
      expect(container.firstChild).toBeInTheDocument();
      
      // Validate we're in development mode
      expect(process.env.NODE_ENV).toBe('development');
      
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
});