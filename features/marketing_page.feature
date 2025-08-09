Feature: Marketing Landing Page
  As a visitor to the website
  I want to see a compelling marketing page
  So that I can understand what the application offers and sign up

  Background:
    Given I navigate to the application root URL

  Scenario: View marketing page without authentication
    When I visit the home page
    Then I see the marketing landing page
    And I see the application title "Savage Worlds Virtual Table Top"
    And I see a marketing headline
    And I see application features and benefits
    And I see a "Sign Up Now" call-to-action button
    And I am not required to log in

  Scenario: Navigate to signup from marketing page
    Given I am on the marketing landing page
    When I click the "Sign Up Now" button
    Then I am redirected to the signup page

  Scenario: Navigate to login from marketing page
    Given I am on the marketing landing page
    When I click the "Login" link
    Then I am redirected to the login page

  Scenario: Marketing page displays key features
    Given I am on the marketing landing page
    Then I see feature "Plot Point Management"
    And I see feature "Character Creation"
    And I see feature "Beast Management" 
    And I see feature "Equipment Tracking"
    And I see testimonial or benefit statements

  Scenario: Marketing page has proper branding
    Given I am on the marketing landing page
    Then I see the application logo
    And I see consistent branding colors
    And I see professional styling
    And the page title contains "Savage Worlds Virtual Table Top"