# Shared BDD Features

This directory contains the BDD feature files that are shared between the frontend (ui-web) and backend (API) projects.

## Structure

- `*.feature` - Gherkin feature files describing the behavior of the system
- `step_definitions/` - Step definitions for the frontend (JavaScript/Cucumber.js)
- `support/` - Support files and world configuration for the frontend tests

## Usage

### Frontend (ui-web)

The frontend uses Cucumber.js to run the BDD tests. From the `ui-web` directory:

```bash
npm run test:ui
```

The frontend is configured to look for features in this root directory via the `cucumber.js` configuration file.

### Backend (savage-worlds-api)

The backend can use Godog (Cucumber for Go) to run the same feature files. To set this up:

1. Install Godog:
```bash
go install github.com/cucumber/godog/cmd/godog@latest
```

2. Create step definitions in Go in the `savage-worlds-api/test/features` directory

3. Run the tests:
```bash
cd savage-worlds-api
make test-bdd
```

## Writing Features

Features should be written from a user perspective and be implementation-agnostic so they can be used to test both the frontend and backend.

Example:
```gherkin
Feature: Plot Point Management
  As a Game Master
  I want to manage plot points
  So that I can organize my game content

  Scenario: Create a new plot point
    Given I am authenticated as a Game Master
    When I create a plot point with name "The Lost City"
    Then the plot point should be saved successfully
    And I should see "The Lost City" in my plot points list
```