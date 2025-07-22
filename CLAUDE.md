# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Savage Worlds Virtual Table Top (SWVTT) is a React-based web application for managing Savage Worlds tabletop RPG games. The application provides digital tools for Game Masters to create and manage Plot Points, Characters, Beasts, Equipment, and other game entities.

## Tech Stack

- **Frontend**: React 18.2.0 with class components
- **Backend**: AWS Amplify with AppSync (GraphQL API)
- **Authentication**: AWS Cognito (currently disabled in App.js)
- **Storage**: AWS S3 via Amplify Storage
- **Testing**: Jest (unit), Cucumber.js (BDD), Storybook (component docs)
- **UI Library**: Bootstrap React Components

## Development Commands

```bash
# Install dependencies (Note: resolve merge conflict in package.json first)
npm install

# Start development server using AWS Mobile CLI
npm start

# Alternative: Start React development server directly
npm run start:react

# Run all tests (unit + BDD)
npm test

# Run unit tests only
npm run test:unit

# Run Cucumber BDD tests
npm run test:ui

# Start Storybook for component development
npm run storybook

# Build production bundle
npm run build

# Docker commands
npm run start:docker  # Start with Docker
npm run stop         # Stop Docker containers
```

## CRITICAL: Package.json Merge Conflict

**Before starting development**, resolve the merge conflict in `ui-web/package.json` (lines 25-31). Choose the appropriate npm and postcss versions.

## Architecture Overview

### Application Structure
```
/ui-web                         # Main web application
  /src
    /components                 # Reusable UI components
    /pages                      # Page-level components
    /models                     # Data models (Character, Beast, etc.)
    /graphql                    # Auto-generated GraphQL operations
    /propTypes                  # PropType definitions
  /features                     # Cucumber BDD tests
  /stories                      # Storybook component stories
  /amplify                      # AWS Amplify backend configuration
/docs
  /diagrams                     # Architecture and design documentation
```

### Architecture Documentation

For detailed visual representations of the system architecture, see:
- **[Data Model ERD](../docs/diagrams/data-model-erd.md)** - Complete entity relationship diagram with all models and relationships
- **[Sequence Diagrams](../docs/diagrams/sequence-diagrams.md)** - Workflows for CRUD operations, authentication, and testing
- **[Architecture Diagrams](../docs/diagrams/architecture-diagram.md)** - System overview, component hierarchy, and deployment architecture

### State Management
- Uses React component state (no Redux implementation despite dependencies)
- Complex form state management in PlotPointForm component
- Props drilling for data flow between components

### GraphQL Integration
- Auto-generated queries/mutations/subscriptions in `/graphql`
- Direct API calls using `API.graphql(graphqlOperation())`
- Main operations: `listPlotPoints`, `getPlotPoint`, `createPlotPoint`, `updatePlotPoint`, `deletePlotPoint`

### Routing
- React Router v6 with three main routes:
  - `/` - Plot Point list
  - `/plot_point/add` - Add new Plot Point
  - `/plot_point/:name/edit` - Edit existing Plot Point

## Testing Strategy

### Unit Tests
- Located alongside components (e.g., `App.test.js`)
- Run with: `npm run test:unit`
- Uses Jest with jsdom environment
- Currently minimal coverage (only 2 test files)

### BDD Integration Tests
- Located in `/ui-web/features`
- Run with: `npm run test:ui`
- Uses Selenium WebDriver with ChromeDriver
- Tests authenticate with AWS Amplify using hardcoded credentials
- Takes screenshots on failure

### Running a Single Test
```bash
# Run a specific unit test file
npm run test:unit -- App.test.js

# Run a specific Cucumber feature
./node_modules/.bin/cucumber-js features/create_plot_point.feature
```

## AWS Amplify Development

### Key Files
- `aws-exports.js` - Auto-generated AWS configuration
- `amplify/` - Backend infrastructure configuration
- GraphQL schema: `amplify/backend/api/swvtt/schema.graphql`

### Common Amplify Commands
```bash
# Pull backend environment
amplify pull

# Push local changes to cloud
amplify push

# Open Amplify console
amplify console

# Generate GraphQL code
amplify codegen
```

## Component Development Patterns

### Class Components
Most components use class-based syntax:
```javascript
class ComponentName extends Component {
  static propTypes = {
    // PropType definitions
  }
  
  state = {
    // Initial state
  }
  
  render() {
    // Component JSX
  }
}
```

### Form Handling
Complex forms use nested state updates:
```javascript
handleFieldChange = (event) => {
  const { name, value } = event.target;
  this.setState(prevState => ({
    plotPoint: {
      ...prevState.plotPoint,
      [name]: value
    }
  }));
}
```

### GraphQL Operations
Standard pattern for API calls:
```javascript
const result = await API.graphql(
  graphqlOperation(listPlotPoints)
);
```

## Known Issues

1. **Authentication Disabled**: The `withAuthenticator` HOC is commented out in App.js
2. **Package.json Conflict**: Unresolved merge conflict needs resolution
3. **Limited Test Coverage**: Only 2 unit test files exist
4. **Mixed AWS Configurations**: Both `awsmobilejs` (legacy) and `amplify` directories present

## Development Workflow

1. Resolve package.json merge conflict before starting
2. Run `npm install` to install dependencies
3. Start development with `npm start` (uses AWS Mobile CLI)
4. Make changes to components in `/src`
5. GraphQL changes require updating schema and running `amplify codegen`
6. Test changes with `npm test`
7. Build for production with `npm run build`

## Code Standards

- Use PropTypes for all component props
- Follow existing class component patterns
- Maintain consistent indentation (2 spaces)
- Keep complex forms modular with sub-components
- Test new features with both unit and BDD tests