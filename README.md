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

- **Frontend**: React 18.2.0 with React Query for state management
- **Backend**: Go 1.21 REST API with JWT authentication
- **Database**: PostgreSQL 15
- **Containerization**: Docker & Docker Compose
- **Testing**: Jest, Cucumber.js, Storybook
- **UI Framework**: Bootstrap React Components

## Prerequisites

### Option 1: Docker Setup (Recommended)
- Docker Desktop (version 20.10 or higher)
- Docker Compose (version 2.0 or higher)
- 4GB of available RAM

### Option 2: Local Development
- Node.js (v18 or higher)
- Go 1.21 or higher
- PostgreSQL 15
- npm or yarn

## Getting Started

### Quick Start with Docker (Recommended)

1. **Clone the Repository**
   ```bash
   git clone https://github.com/[your-username]/SavageWorldsVirtualTableTop.git
   cd SavageWorldsVirtualTableTop
   ```

2. **Configure Environment**
   ```bash
   cp .env.example .env
   # Edit .env with your settings (optional, defaults work for development)
   ```

3. **Start All Services**
   ```bash
   ./scripts/start.sh
   ```

The application will be available at:
- Frontend: `http://localhost:3000`
- API: `http://localhost:8080`
- PgAdmin: `http://localhost:5050`

For detailed Docker setup instructions, see [DOCKER_README.md](DOCKER_README.md).

### Manual Setup (Alternative)

1. **Clone and Install Dependencies**
   ```bash
   git clone https://github.com/[your-username]/SavageWorldsVirtualTableTop.git
   cd SavageWorldsVirtualTableTop
   ```

2. **Setup Database**
   ```bash
   # Create PostgreSQL database
   createdb swvtt_db
   # Run migrations
   cd database
   ./migrate.sh up
   ```

3. **Start Backend**
   ```bash
   cd savage-worlds-api
   go mod download
   go run cmd/api/main.go
   ```

4. **Start Frontend**
   ```bash
   cd ui-web
   npm install
   npm start
   ```

## Available Scripts

### Docker Commands (Recommended)
- `./scripts/start.sh [dev|prod]` - Start all services
- `./scripts/stop.sh [dev|prod]` - Stop all services
- `./scripts/logs.sh [service]` - View service logs
- `./scripts/backup.sh` - Backup database

### Frontend Scripts (in `ui-web` directory)
- `npm start` - Start React development server
- `npm test` - Run all tests
- `npm run test:unit` - Run unit tests only
- `npm run test:ui` - Run Cucumber BDD tests
- `npm run build` - Build for production
- `npm run storybook` - Start Storybook

### Backend Scripts (in `savage-worlds-api` directory)
- `go run cmd/api/main.go` - Start API server
- `go test ./...` - Run all tests
- `make build` - Build binary
- `make test` - Run tests with coverage

## Project Structure

```
SavageWorldsVirtualTableTop/
├── savage-worlds-api/          # Go REST API backend
│   ├── cmd/api/               # Application entry point
│   ├── internal/              # Internal packages
│   │   ├── handlers/         # HTTP request handlers
│   │   ├── models/           # Data models
│   │   ├── repository/       # Database access layer
│   │   └── middleware/       # HTTP middleware
│   └── config/               # Configuration management
├── ui-web/                    # React frontend application
│   ├── src/                  # Source code
│   │   ├── components/       # Reusable UI components
│   │   ├── pages/           # Page-level components
│   │   ├── services/        # API service layer
│   │   ├── hooks/           # Custom React hooks
│   │   └── contexts/        # React contexts
│   └── features/            # Cucumber BDD test scenarios
├── database/                 # Database migrations and scripts
│   ├── migrations/          # SQL migration files
│   └── init/               # Initial setup scripts
├── scripts/                 # Docker and utility scripts
├── docs/                   # Documentation
│   └── diagrams/          # Architecture diagrams
├── docker-compose.yml     # Main Docker configuration
├── docker-compose.dev.yml # Development Docker config
├── docker-compose.prod.yml # Production Docker config
└── DOCKER_README.md      # Docker setup guide
```

## Architecture Documentation

Detailed architecture diagrams and documentation are available in the `docs/diagrams/` directory:

- **[Data Model ERD](docs/diagrams/data-model-erd.md)** - Entity Relationship Diagram showing all data models and their relationships
- **[Sequence Diagrams](docs/diagrams/sequence-diagrams.md)** - Key workflows including CRUD operations, authentication, and testing flows
- **[Architecture Diagrams](docs/diagrams/architecture-diagram.md)** - System architecture, component hierarchy, and deployment structure

## REST API

The application provides a RESTful API built with Go. Default endpoint: `http://localhost:8080/api/v1`

### Key Endpoints
- `GET /health` - Health check endpoint
- `POST /auth/register` - User registration
- `POST /auth/login` - User login
- `GET /plot-points` - List all plot points
- `GET /plot-points/:id` - Get specific plot point
- `POST /plot-points` - Create plot point
- `PUT /plot-points/:id` - Update plot point
- `DELETE /plot-points/:id` - Delete plot point

### Authentication
The API uses JWT tokens for authentication. Include the token in the Authorization header:
```
Authorization: Bearer <your-jwt-token>
```

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

### Docker Deployment

1. **Production Build**:
   ```bash
   ./scripts/start.sh production
   ```

2. **With Docker Swarm**:
   ```bash
   docker stack deploy -c docker-compose.prod.yml swvtt
   ```

3. **With Kubernetes**:
   See `kubernetes/` directory for Helm charts (if available)

### Manual Deployment

1. **Build Frontend**:
   ```bash
   cd ui-web
   npm run build
   ```

2. **Build Backend**:
   ```bash
   cd savage-worlds-api
   make build
   ```

3. **Deploy to your infrastructure**

## Known Issues

1. **Frontend Node Version**: The frontend Dockerfile uses Node 18, ensure compatibility
2. **Hot Reload on Windows**: May need to set `CHOKIDAR_USEPOLLING=true` for file watching
3. **Database Migrations**: Ensure migrations run before starting the API

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