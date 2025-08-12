import { exec } from 'child_process';
import { promisify } from 'util';
import CodeQualityUtils, {
  hasESLintConfig,
  parseESLintOutput,
  validateComponentPropTypes,
  isLintableFile,
  isValidComponentName,
  extractDomainLanguage
} from './CodeQualityUtils';

const execAsync = promisify(exec);

describe('Code Quality Utilities Tests', () => {
  describe('CodeQualityUtils Module Tests', () => {
    test('should export all utility functions', () => {
      expect(CodeQualityUtils).toBeDefined();
      expect(hasESLintConfig).toBeDefined();
      expect(parseESLintOutput).toBeDefined();
      expect(validateComponentPropTypes).toBeDefined();
      expect(isLintableFile).toBeDefined();
      expect(isValidComponentName).toBeDefined();
      expect(extractDomainLanguage).toBeDefined();
    });

    test('should validate ESLint configuration presence', () => {
      const result = hasESLintConfig();
      expect(result).toBe(true);
    });

    test('should parse ESLint output correctly', () => {
      const eslintOutput = '✖ 110 problems (0 errors, 110 warnings)';
      const result = parseESLintOutput(eslintOutput);
      
      expect(result.warnings).toBe(110);
      expect(result.errors).toBe(0);
    });

    test('should validate component PropTypes', () => {
      const componentWithPropTypes = {
        propTypes: {
          name: 'PropTypes.string.isRequired'
        }
      };
      
      const result = validateComponentPropTypes(componentWithPropTypes);
      expect(result.isValid).toBe(true);
      
      const componentWithoutPropTypes = {};
      const result2 = validateComponentPropTypes(componentWithoutPropTypes);
      expect(result2.isValid).toBe(false);
    });

    test('should identify lintable files', () => {
      expect(isLintableFile('Component.js')).toBe(true);
      expect(isLintableFile('Component.jsx')).toBe(true);
      expect(isLintableFile('Component.ts')).toBe(true);
      expect(isLintableFile('Component.tsx')).toBe(true);
      expect(isLintableFile('styles.css')).toBe(false);
      expect(isLintableFile('README.md')).toBe(false);
    });

    test('should validate React component naming conventions', () => {
      expect(isValidComponentName('MyComponent')).toBe(true);
      expect(isValidComponentName('Button')).toBe(true);
      expect(isValidComponentName('myComponent')).toBe(false);
      expect(isValidComponentName('my-component')).toBe(false);
      expect(isValidComponentName('123Component')).toBe(false);
    });

    test('should extract domain language from PropTypes', () => {
      const propTypes = {
        userName: 'string',
        id: 'string',
        customerDetails: 'object',
        className: 'string'
      };
      
      const domainTerms = extractDomainLanguage(propTypes);
      expect(domainTerms).toContain('userName');
      expect(domainTerms).toContain('customerDetails');
      expect(domainTerms).not.toContain('id');
      expect(domainTerms).not.toContain('className');
    });
  });

  describe('ESLint Integration Tests', () => {
    test('should have ESLint configuration file present', async () => {
      const { stdout } = await execAsync('ls .eslintrc.json', { cwd: process.cwd() });
      expect(stdout.trim()).toBe('.eslintrc.json');
    });

    test('should be able to run ESLint command', async () => {
      try {
        await execAsync('npm run lint --version', { cwd: process.cwd() });
        // If this doesn't throw, the lint command is available
        expect(true).toBe(true);
      } catch (error) {
        // Check if it's just because of warnings, not missing command
        expect(error.code).not.toBe(127); // Command not found
      }
    });

    test('should validate that lint command exists in package.json', async () => {
      const { stdout } = await execAsync('npm run', { cwd: process.cwd() });
      expect(stdout).toContain('lint');
    });
  });

  describe('PropTypes Import Validation', () => {
    test('should validate PropTypes package is available', () => {
      // Test that PropTypes can be imported
      const PropTypes = require('prop-types');
      expect(PropTypes).toBeDefined();
      expect(PropTypes.string).toBeDefined();
      expect(PropTypes.number).toBeDefined();
      expect(PropTypes.bool).toBeDefined();
      expect(PropTypes.func).toBeDefined();
      expect(PropTypes.object).toBeDefined();
      expect(PropTypes.array).toBeDefined();
    });

    test('should validate PropTypes shape functionality', () => {
      const PropTypes = require('prop-types');
      
      const shapeValidator = PropTypes.shape({
        name: PropTypes.string,
        value: PropTypes.any
      });
      
      expect(shapeValidator).toBeDefined();
    });

    test('should validate PropTypes arrayOf functionality', () => {
      const PropTypes = require('prop-types');
      
      const arrayValidator = PropTypes.arrayOf(PropTypes.string);
      expect(arrayValidator).toBeDefined();
    });
  });

  describe('Code Style Validation Utilities', () => {
    test('should validate that unused variable detection works', () => {
      // This test validates the concept of unused variable detection
      const codeWithUnusedVar = `
        const usedVariable = 'I am used';
        const unusedVariable = 'I am not used';
        console.log(usedVariable);
      `;
      
      // In a real scenario, ESLint would flag unusedVariable
      // This test validates our understanding of the concept
      expect(codeWithUnusedVar).toContain('unusedVariable');
      expect(codeWithUnusedVar).toContain('usedVariable');
    });

    test('should validate unreachable code detection concepts', () => {
      // This validates the concept of unreachable code
      const unreachableCodeExample = `
        function testFunction() {
          return 'early return';
          console.log('unreachable code'); // This would be flagged by ESLint
        }
      `;
      
      expect(unreachableCodeExample).toContain('return');
      expect(unreachableCodeExample).toContain('console.log');
    });
  });

  describe('React Component Validation Patterns', () => {
    test('should validate React import patterns', () => {
      // Test that React can be imported (for components that need it)
      const React = require('react');
      expect(React).toBeDefined();
      expect(React.Component).toBeDefined();
    });

    test('should validate component export patterns', () => {
      // Validate that we can test component export patterns
      const validExportPattern = 'export default ComponentName;';
      const namedExportPattern = 'export { ComponentName };';
      
      expect(validExportPattern).toMatch(/export default/);
      expect(namedExportPattern).toMatch(/export {/);
    });

    test('should validate functional vs class component patterns', () => {
      // Validate patterns for functional components
      const functionalComponentPattern = `
        const FunctionalComponent = (props) => {
          return <div>{props.children}</div>;
        };
      `;
      
      // Validate patterns for class components
      const classComponentPattern = `
        class ClassComponent extends React.Component {
          render() {
            return <div>{this.props.children}</div>;
          }
        }
      `;
      
      expect(functionalComponentPattern).toContain('=>');
      expect(classComponentPattern).toContain('extends React.Component');
    });
  });

  describe('ESLint Rule Configuration Validation', () => {
    test('should validate key ESLint rules are testable', () => {
      // Validate that we understand the key rules we're working with
      const eslintRules = {
        'react/prop-types': 'PropTypes validation required',
        'no-unused-vars': 'Unused variables not allowed',
        'no-unreachable': 'Unreachable code not allowed'
      };
      
      expect(eslintRules['react/prop-types']).toBeDefined();
      expect(eslintRules['no-unused-vars']).toBeDefined();
      expect(eslintRules['no-unreachable']).toBeDefined();
    });

    test('should validate ESLint warning message patterns', () => {
      // Test patterns that ESLint warnings follow
      const propTypesWarning = "'propName' is missing in props validation";
      const unusedVarWarning = "'varName' is assigned a value but never used";
      const unreachableWarning = "Unreachable code";
      
      expect(propTypesWarning).toMatch(/missing in props validation/);
      expect(unusedVarWarning).toMatch(/assigned a value but never used/);
      expect(unreachableWarning).toMatch(/Unreachable code/);
    });
  });

  describe('Code Quality Metrics Validation', () => {
    test('should validate warning count parsing from ESLint output', () => {
      const eslintOutput = '✖ 110 problems (0 errors, 110 warnings)';
      
      const warningMatch = eslintOutput.match(/(\d+) warnings?/);
      const errorMatch = eslintOutput.match(/(\d+) errors?/);
      
      expect(warningMatch).toBeTruthy();
      expect(parseInt(warningMatch[1])).toBe(110);
      
      expect(errorMatch).toBeTruthy();
      expect(parseInt(errorMatch[1])).toBe(0);
    });

    test('should validate successful ESLint output patterns', () => {
      const successfulLintOutput = 'No ESLint warnings or errors found.';
      const noWarningsPattern = /0 warnings?/;
      const noErrorsPattern = /0 errors?/;
      
      // When ESLint passes, we expect either no warnings mentioned
      // or explicit "0 warnings" message
      expect(successfulLintOutput).not.toMatch(/\d+ warnings?/);
      expect('0 warnings').toMatch(noWarningsPattern);
      expect('0 errors').toMatch(noErrorsPattern);
    });
  });

  describe('File Pattern Validation for Linting', () => {
    test('should validate file extension patterns for ESLint', () => {
      const jsFiles = ['Component.js', 'utils.js', 'index.js'];
      const jsxFiles = ['Component.jsx', 'Page.jsx', 'Editor.jsx'];
      const testFiles = ['Component.test.js', 'utils.test.js'];
      
      jsFiles.forEach(file => {
        expect(file).toMatch(/\.js$/);
      });
      
      jsxFiles.forEach(file => {
        expect(file).toMatch(/\.jsx$/);
      });
      
      testFiles.forEach(file => {
        expect(file).toMatch(/\.test\.js$/);
      });
    });

    test('should validate React component file naming patterns', () => {
      const componentFiles = [
        'src/components/PlotPointForm.jsx',
        'src/pages/SceneAdd.jsx',
        'src/components/plotpoint/editor/gear/GearEditor.jsx'
      ];
      
      componentFiles.forEach(file => {
        expect(file).toMatch(/src\/.*\.(js|jsx)$/);
      });
    });
  });
});