Feature: User Logout
  As a logged-in user
  I want to be able to logout
  So that I can secure my session

  Background:
    Given I have started the application
    And I am logged in as a user

  Scenario: Successful logout
    When I click the logout button
    Then I am logged out successfully
    And I am redirected to the login page