Feature: Login Redirect Fix - Issue #174
  As a registered user
  I want to be redirected to the default screen after login
  So that I can immediately access the plot points list page

  Background:
    Given I have started the application
    And I am on the login page

  Scenario: Login redirect takes user to default screen (plot points list)
    Given a user exists with email "chestertester@example.com" and password "ChesterTester1!"
    And I provide an email of "chestertester@example.com"
    And I provide a password of "ChesterTester1!"
    When I submit the login form
    Then I am successfully authenticated
    And I am redirected to the plot points list page within 3 seconds
    And the URL should be "/"
    And I should see the plot points list page content
    And I should see an "Add Plot Point" button or link
    And I should not be redirected back to the login page

  Scenario: Login redirect works without page reload
    Given a user exists with email "chestertester@example.com" and password "ChesterTester1!"
    And I provide an email of "chestertester@example.com"
    And I provide a password of "ChesterTester1!"
    When I submit the login form
    Then I am successfully authenticated
    And the page should not reload completely
    And I should navigate to the plot points list using React Router
    And the authentication state should persist across navigation

  Scenario: Authentication state persistence during navigation
    Given a user exists with email "chestertester@example.com" and password "ChesterTester1!"
    And I provide an email of "chestertester@example.com"
    And I provide a password of "ChesterTester1!"
    When I submit the login form
    Then I am successfully authenticated
    And the authentication context should be properly initialized
    And the protected route should allow access to the plot points list
    And I should remain authenticated without additional login prompts