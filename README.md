# Savage Worlds Virtual Table Top (SWVTT)

A web-based virtual tabletop application for the Savage Worlds role-playing game system. This application helps Game Masters manage Plot Points, Characters, Beasts, Equipment, and other game entities digitally.

## Features

- **Plot Point Management**: Create, edit, and organize plot points for your campaigns
- **Character Management**: Track player characters and NPCs with full character sheets
- **Beast Compendium**: Manage creatures and monsters for encounters
- **Equipment Tracking**: Organize gear, weapons, armor, and other items
- **Game Mechanics**: Built-in support for Savage Worlds rules including:
  - Skills, Edges, and Hindrances
  - Powers and magic systems
  - Setting Rules customization
  - Race creation and management

## Technology Stack

- **Frontend**: React 18.2.0
- **Backend**: AWS Amplify with GraphQL (AppSync)
- **Authentication**: AWS Cognito
- **Storage**: AWS S3
- **Testing**: Jest, Cucumber.js, Storybook
- **UI Framework**: Bootstrap React Components

## Prerequisites

- Node.js (v14 or higher recommended)
- npm or yarn
- AWS account (for backend services)
- AWS Amplify CLI (`npm install -g @aws-amplify/cli`)

## Getting Started

### 1. Clone the Repository

```bash
git clone https://github.com/[your-username]/SavageWorldsVirtualTableTop.git
cd SavageWorldsVirtualTableTop
```

### 2. Install Dependencies

**Important**: There's currently a merge conflict in `ui-web/package.json` (lines 25-31) that needs to be resolved before installation.

```bash
cd ui-web
# Resolve the merge conflict in package.json first
npm install
```

### 3. Configure AWS Amplify

```bash
# Initialize Amplify (if not already configured)
amplify init

# Pull the backend environment
amplify pull

# Push any local backend changes
amplify push
```

### 4. Start Development Server

```bash
# Using AWS Mobile CLI (recommended)
npm start

# Or use React development server directly
npm run start:react
```

The application will be available at `http://localhost:3000`

## Available Scripts

In the `ui-web` directory, you can run:

### Development
- `npm start` - Start the development server using AWS Mobile CLI
- `npm run start:react` - Start React development server directly
- `npm run start:docker` - Start using Docker Compose
- `npm run stop` - Stop Docker containers

### Testing
- `npm test` - Run all tests (unit + integration)
- `npm run test:unit` - Run unit tests only
- `npm run test:ui` - Run Cucumber BDD tests
- `npm run storybook` - Start Storybook for component development

### Building
- `npm run build` - Build the app for production
- `npm run build-storybook` - Build Storybook static files

## Project Structure

```
SavageWorldsVirtualTableTop/
├── ui-web/                      # Main web application
│   ├── src/                     # Source code
│   │   ├── components/          # Reusable UI components
│   │   ├── pages/              # Page-level components
│   │   ├── models/             # Data models
│   │   ├── graphql/            # GraphQL operations (auto-generated)
│   │   └── propTypes/          # PropType definitions
│   ├── features/               # Cucumber BDD test scenarios
│   ├── stories/                # Storybook component stories
│   ├── amplify/                # AWS Amplify backend configuration
│   └── public/                 # Static assets
├── docs/                       # Documentation
│   └── diagrams/              # Architecture and design diagrams
├── CLAUDE.md                   # Development guide for Claude Code
└── README.md                   # This file
```

## Architecture Documentation

Detailed architecture diagrams and documentation are available in the `docs/diagrams/` directory:

- **[Data Model ERD](docs/diagrams/data-model-erd.md)** - Entity Relationship Diagram showing all data models and their relationships
- **[Sequence Diagrams](docs/diagrams/sequence-diagrams.md)** - Key workflows including CRUD operations, authentication, and testing flows
- **[Architecture Diagrams](docs/diagrams/architecture-diagram.md)** - System architecture, component hierarchy, and deployment structure

## GraphQL API

The application uses AWS AppSync for its GraphQL API. The endpoint is:
```
https://eipwvq2dufahpaygkjpx4x3r5m.appsync-api.us-west-2.amazonaws.com/graphql
```

### Key Operations
- `listPlotPoints` - Retrieve all plot points
- `getPlotPoint` - Get a specific plot point
- `createPlotPoint` - Create a new plot point
- `updatePlotPoint` - Update an existing plot point
- `deletePlotPoint` - Remove a plot point

## Testing

### Unit Tests
Unit tests are written using Jest and React Testing Library:
```bash
npm run test:unit
```

### Integration Tests
BDD tests are written in Gherkin and run with Cucumber.js:
```bash
npm run test:ui
```

### Component Documentation
View and develop components in isolation with Storybook:
```bash
npm run storybook
```

## Deployment

1. Build the production bundle:
   ```bash
   npm run build
   ```

2. Deploy to AWS Amplify:
   ```bash
   amplify publish
   ```

## Known Issues

1. **Authentication Disabled**: The authentication wrapper is currently commented out in `App.js`
2. **Package.json Conflict**: There's an unresolved merge conflict that needs resolution before installation
3. **Limited Test Coverage**: Currently only 2 unit test files exist
4. **Legacy Configuration**: Both `awsmobilejs` (legacy) and `amplify` directories are present

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Author

Jim Barrows - [Jim.Barrows@gmail.com](mailto:Jim.Barrows@gmail.com)

## Acknowledgments

- Savage Worlds is a trademark of Pinnacle Entertainment Group
- Built with React and AWS Amplify