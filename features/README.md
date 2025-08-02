# BDD Feature Tests

Cucumber feature files for behavior-driven testing of SWVTT.

## Features

- `signup.feature` - User registration
- `login.feature` - User authentication  
- `logout.feature` - Session management
- `create_plot_point.feature` - Plot point creation

## Running Tests

From the ui-web directory:
```bash
npm run test:ui
```

Or run a specific feature:
```bash
./node_modules/.bin/cucumber-js ../features/login.feature
```

## Writing Features

Use Gherkin syntax focusing on user behavior:

```gherkin
Feature: Feature Name
  As a [role]
  I want [goal]
  So that [benefit]

  Scenario: Scenario name
    Given [context]
    When [action]
    Then [outcome]
```

Step definitions are in `ui-web/features/step_definitions/`.