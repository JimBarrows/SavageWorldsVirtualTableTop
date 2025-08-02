Feature: Login Validation Fix for Issue #170
  As a developer
  I want to ensure the login endpoint properly validates email fields
  So that users can successfully login with their email credentials

  Background:
    Given I have started the application

  Scenario: API correctly validates email field in login request
    Given a user exists with email "testuser@example.com" and password "SecurePass123!"
    When I make a POST request to "/api/v1/auth/login" with:
      | email    | testuser@example.com |
      | password | SecurePass123!       |
    Then the response status should be 200
    And the response should contain access_token

  Scenario: API returns validation error for missing email
    When I make a POST request to "/api/v1/auth/login" with:
      | password | SecurePass123! |
    Then the response status should be 400
    And the response should contain error message about missing email

  Scenario: API returns validation error for empty email
    When I make a POST request to "/api/v1/auth/login" with:
      | email    |                |
      | password | SecurePass123! |
    Then the response status should be 400
    And the response should contain error message about missing email

  Scenario: API accepts email field (not Email with capital E)
    Given a user exists with email "lowercase@example.com" and password "SecurePass123!"
    When I make a POST request to "/api/v1/auth/login" with JSON:
      """
      {
        "email": "lowercase@example.com",
        "password": "SecurePass123!"
      }
      """
    Then the response status should be 200
    And the response should contain access_token

  Scenario: API rejects Email field (capital E)
    Given a user exists with email "capital@example.com" and password "SecurePass123!"
    When I make a POST request to "/api/v1/auth/login" with JSON:
      """
      {
        "Email": "capital@example.com",
        "password": "SecurePass123!"
      }
      """
    Then the response status should be 400
    And the error message should contain "Field validation for 'Email' failed"