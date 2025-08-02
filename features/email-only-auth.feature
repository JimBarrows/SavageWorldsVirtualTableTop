Feature: Email-only Authentication
  As a user
  I want to use only my email address for authentication
  So that I don't need to remember a separate username

  Background:
    Given I have started the application

  Scenario: Register with email only
    Given I am on the signup page
    When I provide an email of "newuser@example.com"
    And I provide a password of "SecurePass123!"
    And I submit the signup form
    Then I am successfully registered
    And I am automatically logged in
    And no username is stored in the system

  Scenario: Login with email only
    Given a user exists with email "existinguser@example.com" and password "SecurePass123!"
    And I am on the login page
    When I provide an email of "existinguser@example.com"
    And I provide a password of "SecurePass123!"
    And I submit the login form
    Then I am successfully authenticated
    And I am redirected to the plot points list page

  Scenario: API accepts email field for login
    Given a user exists with email "apiuser@example.com" and password "SecurePass123!"
    When I make a POST request to "/api/v1/auth/login" with:
      | email    | apiuser@example.com |
      | password | SecurePass123!      |
    Then the response status should be 200
    And the response should contain access_token
    And the response should contain the user's email

  Scenario: API accepts registration without username
    When I make a POST request to "/api/v1/auth/register" with:
      | email    | newapi@example.com |
      | password | SecurePass123!     |
    Then the response status should be 201
    And the response should contain the user's email
    And the response should not require a username field

  Scenario: Username field is removed from signup form
    Given I am on the signup page
    Then I should not see a username field
    And I should see an email field
    And I should see a password field

  Scenario: Username is not displayed in UI
    Given I am logged in as "testuser@example.com"
    When I view my profile
    Then I should see my email "testuser@example.com"
    And I should not see any username information

  Scenario: JWT tokens use email as identifier
    Given a user exists with email "jwtuser@example.com" and password "SecurePass123!"
    When I login with email "jwtuser@example.com"
    Then the JWT token should contain email as the identifier
    And the JWT token should not contain a username field