# Web UI

React-based frontend for the Savage Worlds Virtual Table Top (SWVTT).

## Overview

This directory contains the web frontend built with:
- React 18.2.0 with class components
- AWS Amplify for backend integration
- Bootstrap React for UI components
- React Router v6 for navigation
- Jest and Cucumber.js for testing

## Prerequisites

- Node.js v18 or higher
- npm or yarn
- AWS account (for Amplify/Cognito)

## Installation

1. Install dependencies:
```bash
npm install
```

2. Configure AWS Amplify:
```bash
amplify pull
```

3. Start development server:
```bash
npm start
```

## Development

### Available Scripts

- `npm start` - Start development server (port 3000)
- `npm test` - Run all tests (unit + integration)
- `npm run test:unit` - Run unit tests only
- `npm run test:ui` - Run Cucumber BDD tests
- `npm run build` - Build for production
- `npm run storybook` - Start Storybook

### Project Structure

```
ui-web/
├── src/
│   ├── components/        # Reusable UI components
│   ├── pages/            # Page-level components
│   ├── models/           # Data models
│   ├── graphql/          # Auto-generated GraphQL
│   ├── propTypes/        # PropType definitions
│   └── App.js            # Main application
├── amplify/              # AWS Amplify config
├── stories/              # Storybook stories
├── public/               # Static assets
└── package.json
```

## Key Features

### Current Implementation
- Plot Point CRUD operations
- AWS Amplify integration
- GraphQL API connectivity
- Bootstrap-based responsive UI

### Authentication
Currently disabled in App.js. To enable:
```javascript
// Uncomment in App.js
export default withAuthenticator(App);
```

### Routing
- `/` - Plot points list
- `/plot_point/add` - Create plot point
- `/plot_point/:name/edit` - Edit plot point

## Testing

### Unit Tests
```bash
npm run test:unit
# or specific file
npm run test:unit -- App.test.js
```

### Integration Tests
BDD tests are in `/features` at project root:
```bash
npm run test:ui
# or specific feature
./node_modules/.bin/cucumber-js ../features/login.feature
```

### Component Documentation
```bash
npm run storybook
```

## GraphQL Integration

### Updating Schema
After backend changes:
```bash
amplify codegen
```

### API Operations
- Queries: listPlotPoints, getPlotPoint
- Mutations: createPlotPoint, updatePlotPoint, deletePlotPoint
- Subscriptions: Available for real-time updates

## Building for Production

```bash
npm run build
```

Output in `build/` directory.

## Docker

### Development
```bash
docker build -f Dockerfile.dev -t swvtt-ui:dev .
docker run -p 3000:3000 swvtt-ui:dev
```

### Production
```bash
docker build -t swvtt-ui:latest .
docker run -p 80:80 swvtt-ui:latest
```

## Known Issues

1. Package.json has unresolved merge conflict (lines 25-31)
2. Authentication currently disabled
3. Limited test coverage
4. Mixed AWS configurations (awsmobilejs vs amplify)

## Contributing

See main project README.md for guidelines.