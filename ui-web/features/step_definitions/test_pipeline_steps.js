import { Given, When, Then } from 'cucumber';
import { execSync } from 'child_process';

// Background steps
Given('the application has a comprehensive test suite', function () {
  // Verify test files exist
  const testFiles = execSync('find ui-web/src -name "*.test.js" -o -name "*.test.jsx" | wc -l', { encoding: 'utf-8' });
  if (parseInt(testFiles.trim()) < 50) {
    throw new Error('Insufficient test coverage - need comprehensive test suite');
  }
});

Given('the CI/CD pipeline runs automated quality checks', function () {
  // Verify CI/CD configuration exists
  const hasPackageJsonTests = execSync('grep -q "test:" ui-web/package.json && echo "true" || echo "false"', { encoding: 'utf-8' });
  if (hasPackageJsonTests.trim() !== 'true') {
    throw new Error('CI/CD test configuration missing in package.json');
  }
});

Given('developers need reliable feedback on code changes', function () {
  // Business requirement - no technical validation needed
  this.developmentWorkflowRequirement = 'reliable_feedback';
});

// Unit test suite execution scenario
Given('the development team has written unit tests for components', function () {
  // Verify component tests exist for critical components
  const componentTests = execSync('find ui-web/src/components -name "*.test.jsx" | wc -l', { encoding: 'utf-8' });
  if (parseInt(componentTests.trim()) < 20) {
    throw new Error('Insufficient component test coverage');
  }
});

When('the CI/CD pipeline runs the complete test suite', function () {
  // Execute the test suite (this will reveal current failures)
  try {
    this.testResults = execSync('cd ui-web && CI=true npm run test:unit 2>&1', { encoding: 'utf-8' });
    this.testsPassed = true;
  } catch (error) {
    this.testResults = error.stdout || error.message;
    this.testsPassed = false;
  }
});

Then('all unit tests should pass', function () {
  if (!this.testsPassed) {
    throw new Error(`Unit tests failed. Output: ${this.testResults}`);
  }
});

Then('no import path errors should occur', function () {
  if (this.testResults.includes('Cannot find module') || this.testResults.includes('Module not found')) {
    throw new Error('Import path errors detected in test output');
  }
});

Then('no mock function configuration issues should prevent execution', function () {
  if (this.testResults.includes('jest.fn()') && this.testResults.includes('Number of calls: 0')) {
    throw new Error('Mock function configuration issues detected');
  }
});

Then('no constructor invocation errors should block test runs', function () {
  if (this.testResults.includes('Class constructor') && this.testResults.includes('cannot be invoked without')) {
    throw new Error('Constructor invocation errors detected in test output');
  }
});

// Test assertions scenario
Given('components have specific expected behaviors', function () {
  this.componentBehaviorRequirement = 'specific_expected_behaviors';
});

When('unit tests validate component functionality', function () {
  // This is covered by the test execution in the previous scenario
  if (!this.testResults) {
    throw new Error('Test results not available - run test suite first');
  }
});

Then('test assertions should match actual component output', function () {
  if (this.testResults.includes('Expected') && this.testResults.includes('Received') && !this.testsPassed) {
    throw new Error('Test assertion mismatches detected - assertions do not match component output');
  }
});

Then('array length expectations should align with component logic', function () {
  if (this.testResults.includes('Expected length:') && this.testResults.includes('Received length:')) {
    throw new Error('Array length assertion mismatches detected');
  }
});

Then('component state should be validated correctly', function () {
  if (this.testResults.includes('toHaveValue') && this.testResults.includes('Expected') && !this.testsPassed) {
    throw new Error('Component state validation issues detected');
  }
});

// Async operations scenario
Given('components perform asynchronous operations', function () {
  this.asyncComponentRequirement = 'async_operations';
});

When('tests validate async behavior', function () {
  // Check if async tests are present
  const asyncTests = execSync('grep -r "async\\|await\\|waitFor" ui-web/src --include="*.test.js" --include="*.test.jsx" | wc -l', { encoding: 'utf-8' });
  if (parseInt(asyncTests.trim()) < 5) {
    throw new Error('Insufficient async test coverage');
  }
});

Then('async operations should complete within expected timeout periods', function () {
  if (this.testResults.includes('Exceeded timeout') || this.testResults.includes('timeout of')) {
    throw new Error('Async timeout issues detected in test output');
  }
});

Then('proper test waiting strategies should be implemented', function () {
  if (this.testResults.includes('act()') && this.testResults.includes('Warning')) {
    throw new Error('Improper async test handling detected - missing act() wrappers');
  }
});

Then('authentication context changes should be handled appropriately', function () {
  if (this.testResults.includes('AuthContext') && !this.testsPassed) {
    throw new Error('Authentication context test issues detected');
  }
});

// Development workflow scenario
Given('developers are implementing new features', function () {
  this.developmentContext = 'feature_implementation';
});

When('they run the test suite locally or in CI/CD', function () {
  // Record test execution time
  const startTime = Date.now();
  try {
    execSync('cd ui-web && CI=true timeout 45 npm run test:unit', { encoding: 'utf-8' });
    this.testExecutionTime = Date.now() - startTime;
    this.testsCompletedInTime = true;
  } catch (error) {
    this.testExecutionTime = Date.now() - startTime;
    this.testsCompletedInTime = false;
  }
});

Then('the test suite should complete in under 30 seconds', function () {
  if (this.testExecutionTime > 30000) {
    throw new Error(`Test suite took ${this.testExecutionTime}ms - exceeds 30 second requirement`);
  }
});

Then('all quality gates should pass', function () {
  if (!this.testsPassed) {
    throw new Error('Quality gates failing - test suite not passing');
  }
});

Then('new pull requests should not be blocked by pre-existing test failures', function () {
  if (!this.testsPassed) {
    throw new Error('Pre-existing test failures are blocking development workflow');
  }
});

// Test reliability scenario
Given('the test suite validates business logic and component behavior', function () {
  this.businessLogicValidation = 'required';
});

When('developers make changes to the codebase', function () {
  this.codeChanges = 'simulated';
});

Then('test results should accurately reflect the impact of changes', function () {
  // Verify tests are deterministic and not flaky
  if (this.testResults.includes('Jest has detected') && this.testResults.includes('open handles')) {
    throw new Error('Test suite has resource leaks - may cause flaky results');
  }
});

Then('false positive failures should not block development workflow', function () {
  if (!this.testsPassed && this.testResults.includes('Warning')) {
    throw new Error('Test warnings may be causing false positive failures');
  }
});

Then('test execution should be consistent across different environments', function () {
  // Check for environment-specific test issues
  if (this.testResults.includes('ENOENT') || this.testResults.includes('permission denied')) {
    throw new Error('Environment-specific test execution issues detected');
  }
});