@code-quality @ci-cd
Feature: Comprehensive Code Quality and CI/CD Pipeline
  As a development team
  I want automated code quality checks and comprehensive testing
  So that we can maintain high code standards and catch issues early

  Background:
    Given the project has a CI/CD pipeline configured
    And code quality tools are properly integrated

  @unit-testing @coverage
  Scenario: Unit test coverage meets minimum threshold
    Given I have unit tests for the codebase
    When I run the unit test suite with coverage reporting
    Then the code coverage should be at least 85%
    And coverage reports should be generated in multiple formats
    And uncovered code areas should be clearly identified

  @bdd-testing @integration
  Scenario: BDD integration tests execute successfully
    Given I have BDD scenarios written in Gherkin syntax
    When I run the BDD test suite
    Then all BDD tests should pass (100% pass rate)
    And BDD test results should be properly reported
    And failed scenarios should provide detailed error information

  @linting @code-style
  Scenario: Code linting enforces quality standards
    Given ESLint is configured with strict rules
    When I run the linting checks on the codebase
    Then there should be zero linting warnings or errors
    And code formatting should be consistent across all files
    And deprecated or problematic patterns should be flagged

  @build-validation
  Scenario: Production build succeeds
    Given the codebase is properly configured for production
    When I run the production build process
    Then the build should complete successfully
    And all assets should be properly optimized
    And no build warnings should be present

  @security-scanning
  Scenario: Security vulnerabilities are detected and reported
    Given the project has dependencies and custom code
    When I run security vulnerability scans
    Then high and critical vulnerabilities should be identified
    And dependency vulnerabilities should be reported
    And security scan results should be actionable

  @ci-cd-pipeline @github-actions
  Scenario: GitHub Actions workflow validates pull requests
    Given a pull request is created with code changes
    When the CI/CD pipeline executes
    Then all unit tests should pass
    And all BDD tests should pass
    And code coverage should meet the 85% threshold
    And linting checks should pass with zero warnings
    And the production build should succeed
    And security scans should complete without critical issues
    And the PR should be blocked if any check fails

  @performance-testing
  Scenario: Performance regression is detected
    Given baseline performance metrics exist
    When I run performance tests on critical code paths
    Then performance should not degrade beyond acceptable thresholds
    And performance metrics should be tracked over time
    And significant regressions should trigger alerts

  @test-reliability
  Scenario: Test suite reliability and flaky test detection
    Given tests are executed multiple times
    When I analyze test execution results
    Then flaky tests should be identified and reported
    And test execution time should be optimized
    And parallel test execution should work correctly

  @quality-gates
  Scenario: Quality gates prevent poor code from merging
    Given a pull request contains code that fails quality checks
    When the CI/CD pipeline evaluates the PR
    Then the PR should be blocked from merging
    And clear feedback should be provided about failures
    And developers should know exactly what needs to be fixed

  @metrics-dashboard
  Scenario: Quality metrics are tracked and reported
    Given code quality checks run regularly
    When I view the quality metrics dashboard
    Then I should see coverage trends over time
    And I should see build performance metrics
    And I should see test reliability statistics
    And I should see overall project health scores