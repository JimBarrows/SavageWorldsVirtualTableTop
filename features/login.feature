Feature: User Login
  As a registered user
  I want to log into my account
  So that I can access my plot points and game data

  Background:
    Given I have started the application
    And I am on the login page

  Scenario: Successful login with valid credentials
    Given a user exists with email "chestertester@example.com" and password "ChesterTester1!"
    And I provide an email of "chestertester@example.com"
    And I provide a password of "ChesterTester1!"
    When I submit the login form
    Then I am successfully authenticated
    And I am redirected to the plot points list page
    And I see a welcome message

  Scenario: Login fails with incorrect password
    Given a user exists with email "chestertester@example.com" and password "ChesterTester1!"
    And I provide an email of "chestertester@example.com"
    And I provide a password of "WrongPassword123!"
    When I submit the login form
    Then I see an error message "Incorrect username or password"
    And I remain on the login page
    And I am not authenticated

  Scenario: Login fails with non-existent email
    Given I provide an email of "nonexistent@example.com"
    And I provide a password of "SomePassword123!"
    When I submit the login form
    Then I see an error message "Incorrect username or password"
    And I remain on the login page
    And I am not authenticated

  Scenario: Login fails with empty email
    Given I leave the email field empty
    And I provide a password of "SomePassword123!"
    When I submit the login form
    Then I see an error message "Email is required"
    And I remain on the login page

  Scenario: Login fails with empty password
    Given I provide an email of "chestertester@example.com"
    And I leave the password field empty
    When I submit the login form
    Then I see an error message "Password is required"
    And I remain on the login page

  Scenario: Remember me functionality
    Given a user exists with email "chestertester@example.com" and password "ChesterTester1!"
    And I provide an email of "chestertester@example.com"
    And I provide a password of "ChesterTester1!"
    And I check the "Remember me" checkbox
    When I submit the login form
    Then I am successfully authenticated
    And my session is persisted
    When I close and reopen the application
    Then I am still logged in

  Scenario: Navigate to signup from login page
    Given I am on the login page
    When I click on "Don't have an account? Sign up"
    Then I am redirected to the signup page

  Scenario: Navigate to forgot password from login page
    Given I am on the login page
    When I click on "Forgot password?"
    Then I am redirected to the password reset page