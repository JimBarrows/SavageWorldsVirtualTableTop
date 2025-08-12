Feature: Marketing Page Test Fixes
  As a developer
  I want all MarketingPage component tests to pass
  So that the component behavior is properly validated

  Background:
    Given I have a MarketingPage component
    And I have a test suite for MarketingPage

  Scenario: All rendering tests pass successfully
    When I run the rendering test suite
    Then the application title should be found in the document
    And the marketing headline should be found as an h1 element
    And the Sign Up Now button should be found as a link
    And the Login link should be found

  Scenario: Features section tests handle duplicate text correctly
    Given the page contains "equipment tracking" in multiple locations
    When I test for key application features
    Then I should use getAllByText for elements that appear multiple times
    And I should verify the h4 heading specifically for "Equipment Tracking"
    And all four feature headings should be found

  Scenario: All branding element tests pass
    When I run the branding tests
    Then the application logo should be found with proper alt text
    And the marketing-page test id should be found with correct classes

  Scenario: Navigation link tests validate correct paths
    When I check navigation links
    Then the signup button should have href="/signup"
    And the login link should have href="/login"

  Scenario: Accessibility tests verify semantic structure
    When I run accessibility tests
    Then the main role should be found in the document
    And all images should have alt text
    And the h1 heading should exist

  Scenario: Console warnings are suppressed
    Given React Router v6 emits deprecation warnings
    When I run the test suite
    Then React Router warnings should be suppressed in test output
    And no other console errors should appear