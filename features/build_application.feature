Feature: Build Application with Node.js 18+ Compatibility
  As a developer
  I want to build the application successfully on Node.js 18+
  So that the application can be deployed to production

  Background:
    Given I am in the ui-web directory

  Scenario: Build application with Node.js 18+ using legacy OpenSSL provider
    When I run the build command with NODE_OPTIONS set to --openssl-legacy-provider
    Then the build should complete successfully
    And the build output directory should exist
    And the build should contain the necessary production files

  Scenario: Build command in package.json includes legacy OpenSSL option
    When I check the build script in package.json
    Then it should include NODE_OPTIONS=--openssl-legacy-provider
    And it should use craco build command

  Scenario: Verify build output structure
    Given the application has been built successfully
    When I check the build directory
    Then it should contain an index.html file
    And it should contain static assets in the correct folders
    And all JavaScript files should be minified