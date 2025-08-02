Feature: Remember Me Functionality
  As a user of the Savage Worlds VTT
  I want to have a "Remember Me" option on login
  So that I can stay logged in longer without having to re-enter my credentials frequently

  Background:
    Given I am on the login page
    And I have valid user credentials

  Scenario: Login with Remember Me checked extends session duration
    Given I enter my valid credentials
    When I check the "Remember Me" checkbox
    And I click the login button
    Then I should be successfully logged in
    And my session should be extended for remember me duration
    And I should see the Plot Point list page

  Scenario: Login without Remember Me uses standard session duration
    Given I enter my valid credentials
    When I do not check the "Remember Me" checkbox
    And I click the login button
    Then I should be successfully logged in
    And my session should use standard duration
    And I should see the Plot Point list page

  Scenario: Remember Me checkbox is visible and functional
    When I visit the login page
    Then I should see a "Remember Me" checkbox
    And the checkbox should be unchecked by default
    And I should be able to check and uncheck the checkbox

  Scenario: Remember Me state persists across browser sessions
    Given I have logged in with "Remember Me" checked
    When I close the browser
    And I reopen the browser and visit the application
    Then I should still be logged in
    And I should see the Plot Point list page

  Scenario: Standard login session expires normally
    Given I have logged in without "Remember Me" checked
    When the standard session duration expires
    And I try to access a protected page
    Then I should be redirected to the login page
    And I should see the login form

  Scenario: Extended remember me session eventually expires
    Given I have logged in with "Remember Me" checked
    When the extended session duration expires
    And I try to access a protected page
    Then I should be redirected to the login page
    And I should see the login form