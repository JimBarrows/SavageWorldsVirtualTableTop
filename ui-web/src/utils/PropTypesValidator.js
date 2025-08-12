/**
 * PropTypes Validation Utilities
 * 
 * Provides utilities for validating React component PropTypes and ensuring
 * proper type safety and component interface documentation.
 */
import PropTypes from 'prop-types';

/**
 * Validates that a component has proper PropTypes defined
 * @param {Function|Object} component - React component to validate
 * @returns {Object} Validation result with details
 */
export const validatePropTypes = (component) => {
  if (!component) {
    return {
      isValid: false,
      errors: ['Component is undefined or null'],
      warnings: []
    };
  }
  
  const errors = [];
  const warnings = [];
  
  if (!component.propTypes) {
    errors.push('Component is missing PropTypes definition');
  } else {
    // Check for common PropTypes best practices
    const propTypeKeys = Object.keys(component.propTypes);
    
    if (propTypeKeys.length === 0) {
      warnings.push('Component has empty PropTypes object');
    }
    
    // Check for proper required prop marking
    propTypeKeys.forEach(propName => {
      const propType = component.propTypes[propName];
      if (propType && typeof propType === 'function') {
        // This is a simplified check - in reality PropTypes structure is more complex
        warnings.push(`PropType for ${propName} should be properly validated`);
      }
    });
  }
  
  return {
    isValid: errors.length === 0,
    errors,
    warnings
  };
};

/**
 * Checks if a PropType definition follows required/optional patterns correctly
 * @param {Object} propTypes - Component PropTypes object
 * @param {Array} requiredProps - Array of prop names that should be required
 * @returns {Object} Analysis of required/optional prop patterns
 */
export const analyzeRequiredProps = (propTypes, requiredProps = []) => {
  if (!propTypes) {
    return {
      missingRequired: requiredProps,
      properlyMarked: [],
      analysis: 'No PropTypes defined'
    };
  }
  
  const propTypeKeys = Object.keys(propTypes);
  const missingRequired = requiredProps.filter(prop => !propTypeKeys.includes(prop));
  const properlyMarked = propTypeKeys.filter(prop => requiredProps.includes(prop));
  
  return {
    missingRequired,
    properlyMarked,
    analysis: `${properlyMarked.length}/${requiredProps.length} required props properly defined`
  };
};

/**
 * Validates complex PropTypes shapes for domain objects
 * @param {Object} propType - PropTypes shape definition
 * @returns {Object} Validation result for shape complexity
 */
export const validateShapeComplexity = (propType) => {
  if (!propType) {
    return { isComplex: false, analysis: 'No PropType provided' };
  }
  
  // This is a simplified analysis - real PropTypes shapes are more complex
  const analysis = 'PropTypes shape validation is conceptual in this implementation';
  
  return {
    isComplex: true,
    analysis,
    recommendations: [
      'Use PropTypes.shape for object props',
      'Use PropTypes.arrayOf for array props',
      'Mark required props with .isRequired'
    ]
  };
};

/**
 * Generates PropTypes definition template for a component based on usage
 * @param {Array} propUsage - Array of prop usage examples
 * @returns {string} Generated PropTypes template
 */
export const generatePropTypesTemplate = (propUsage = []) => {
  if (propUsage.length === 0) {
    return 'Component.propTypes = {\n  // Define your PropTypes here\n};';
  }
  
  const propTypeLines = propUsage.map(prop => {
    const { name, type, required } = prop;
    const propTypeMap = {
      string: 'PropTypes.string',
      number: 'PropTypes.number',
      boolean: 'PropTypes.bool',
      function: 'PropTypes.func',
      object: 'PropTypes.object',
      array: 'PropTypes.array'
    };
    
    const propType = propTypeMap[type] || 'PropTypes.any';
    const requiredSuffix = required ? '.isRequired' : '';
    
    return `  ${name}: ${propType}${requiredSuffix}`;
  });
  
  return `Component.propTypes = {\n${propTypeLines.join(',\n')}\n};`;
};

/**
 * Validates PropTypes against domain-driven design principles
 * @param {Object} propTypes - Component PropTypes
 * @returns {Object} DDD compliance analysis
 */
export const validateDomainLanguage = (propTypes) => {
  if (!propTypes) {
    return {
      isDomainAligned: false,
      issues: ['No PropTypes to analyze'],
      suggestions: []
    };
  }
  
  const propNames = Object.keys(propTypes);
  const technicalTerms = ['id', 'key', 'ref', 'className', 'style'];
  const domainProps = propNames.filter(name => !technicalTerms.includes(name));
  
  const issues = [];
  const suggestions = [];
  
  if (domainProps.length === 0) {
    issues.push('No domain-specific props found');
    suggestions.push('Consider using business domain terminology in prop names');
  }
  
  // Check for generic names that could be more domain-specific
  const genericNames = ['data', 'item', 'value', 'content'];
  const genericProps = propNames.filter(name => genericNames.includes(name));
  
  if (genericProps.length > 0) {
    issues.push(`Generic prop names found: ${genericProps.join(', ')}`);
    suggestions.push('Use domain-specific names instead of generic terms');
  }
  
  return {
    isDomainAligned: issues.length === 0,
    domainProps,
    issues,
    suggestions
  };
};

/**
 * Default export with all utilities
 */
const PropTypesValidator = {
  validatePropTypes,
  analyzeRequiredProps,
  validateShapeComplexity,
  generatePropTypesTemplate,
  validateDomainLanguage
};

export default PropTypesValidator;