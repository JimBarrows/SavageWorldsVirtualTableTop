Feature: User Signup
  As a new user
  I want to create an account
  So that I can access the Savage Worlds Virtual Table Top

  Background:
    Given I have started the application
    And I am on the signup page

  Scenario: Successful signup with valid information
    Given I provide a username of "NewTester"
    And I provide an email of "newtester@example.com"
    And I provide a password of "ValidPassword123!"
    And I confirm the password with "ValidPassword123!"
    When I submit the signup form
    Then the account is created successfully
    And I receive a confirmation message
    And I am redirected to the login page

  Scenario: Signup fails with existing username
    Given a user "ExistingUser" already exists
    And I provide a username of "ExistingUser"
    And I provide an email of "different@example.com"
    And I provide a password of "ValidPassword123!"
    And I confirm the password with "ValidPassword123!"
    When I submit the signup form
    Then I see an error message "Username already exists"
    And the account is not created

  Scenario: Signup fails with mismatched passwords
    Given I provide a username of "NewTester"
    And I provide an email of "newtester@example.com"
    And I provide a password of "ValidPassword123!"
    And I confirm the password with "DifferentPassword123!"
    When I submit the signup form
    Then I see an error message "Passwords do not match"
    And the account is not created

  Scenario: Signup fails with weak password
    Given I provide a username of "NewTester"
    And I provide an email of "newtester@example.com"
    And I provide a password of "weak"
    And I confirm the password with "weak"
    When I submit the signup form
    Then I see an error message about password requirements
    And the account is not created

  Scenario: Signup fails with invalid email format
    Given I provide a username of "NewTester"
    And I provide an email of "invalid-email"
    And I provide a password of "ValidPassword123!"
    And I confirm the password with "ValidPassword123!"
    When I submit the signup form
    Then I see an error message "Please enter a valid email address"
    And the account is not created

  Scenario: Signup fails with missing required fields
    Given I leave the username field empty
    And I provide an email of "test@example.com"
    And I provide a password of "ValidPassword123!"
    And I confirm the password with "ValidPassword123!"
    When I submit the signup form
    Then I see an error message "Username is required"
    And the account is not created