Feature: Fix AuthContext Unit Test Failures
  As a developer
  I need to fix the failing AuthContext unit tests
  So that the CI/CD pipeline can run successfully

  Background:
    Given the AuthContext test suite exists
    And React Testing Library is configured

  Scenario: Fix React act() warnings in async tests
    Given a test is using async operations without proper act() wrapping
    When the test executes an async state update
    Then the test should wrap the operation in "await act(async () => ...)"
    And no act() warnings should appear in the console

  Scenario: Properly handle loading state during logout
    Given the logout function is called
    When the logout process is in progress
    Then the loading state should be true
    And after logout completes, loading should be false

  Scenario: Ensure AuthContext is accessible in test components
    Given a test component is rendered within AuthProvider
    When the test component uses the useAuth hook
    Then the authContext should be defined and accessible
    And all context methods should be available

  Scenario: Handle async token validation on initialization
    Given the AuthProvider component is initialized
    And a valid token exists in storage
    When the component checks authentication status
    Then the async operation should complete properly
    And the user state should be updated correctly

  Scenario: Properly cleanup async operations in tests
    Given a test involves async operations with timeouts
    When the test completes
    Then all timers should be cleared
    And no memory leaks should occur
    And all promises should be resolved or rejected