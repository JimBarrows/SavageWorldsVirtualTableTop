Feature: CORS Support for User Signup
  As a frontend application
  I want to be able to make cross-origin requests to the API
  So that users can sign up from the web interface

  Background:
    Given I have started the application
    And the API is running on "http://localhost:8080"
    And the frontend is running on "http://localhost:3000"

  Scenario: Successful CORS preflight request for signup
    When I send a preflight OPTIONS request to "/api/v1/auth/register"
    Then the response should include "Access-Control-Allow-Origin" header
    And the "Access-Control-Allow-Origin" header should contain "http://localhost:3000"
    And the response should include "Access-Control-Allow-Methods" header
    And the "Access-Control-Allow-Methods" header should contain "POST"
    And the response should include "Access-Control-Allow-Headers" header
    And the response status should be 204

  Scenario: Successful signup with CORS headers
    Given I provide valid signup data
    When I submit the signup form from "http://localhost:3000"
    Then the response should include "Access-Control-Allow-Origin" header
    And the "Access-Control-Allow-Origin" header should contain "http://localhost:3000"
    And the account is created successfully
    And the response status should be 201

  Scenario: CORS error when origin is not allowed
    Given I provide valid signup data
    When I submit the signup form from "http://unauthorized-origin.com"
    Then the request should be blocked by CORS policy
    And the account is not created