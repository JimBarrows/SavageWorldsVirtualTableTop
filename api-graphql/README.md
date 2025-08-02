# GraphQL API

The GraphQL API for the Savage Worlds Virtual Table Top (SWVTT) application, built with Go.

## Overview

This directory contains the GraphQL API backend that provides:
- JWT-based authentication with AWS Cognito integration
- GraphQL schema and resolvers for all game entities
- PostgreSQL database integration
- Real-time subscriptions for collaborative gameplay
- Docker support for containerization

## Prerequisites

- Go 1.21 or higher
- PostgreSQL 15
- Docker (optional)
- AWS Account (for Cognito authentication)

## Installation

1. Install dependencies:
```bash
go mod download
```

2. Configure environment variables (see main README.md for `.env` setup)

3. Run database migrations:
```bash
cd ../database
./migrate.sh
```

## Development

### Running Locally
```bash
go run cmd/api/main.go
```

### Running with Air (hot reload)
```bash
air -c .air.toml
```

### Running Tests
```bash
go test ./...
go test -cover ./... # With coverage
```

## GraphQL Schema

The API implements a comprehensive GraphQL schema for managing:
- **Plot Points**: Campaign management
- **Characters**: Player and NPC character sheets
- **Beasts**: Creature compendium
- **Equipment**: Items, weapons, and gear
- **Powers**: Supernatural abilities
- **Edges & Hindrances**: Character traits
- **Setting Rules**: Campaign-specific rules

### Key Types
- `PlotPoint`: Main campaign container
- `Character`: Complete character implementation
- `Beast`: Creature stats and abilities
- `Equipment`: Items with full stats
- `GameEntity`: Generic entity type for flexibility

## Project Structure

```
api-graphql/
├── cmd/
│   └── api/
│       └── main.go          # Application entry point
├── config/
│   └── config.go           # Configuration management
├── internal/
│   ├── auth/              # Authentication with Cognito
│   ├── db/                # Database connection
│   ├── graph/             # GraphQL schema and resolvers
│   ├── handlers/          # HTTP handlers
│   ├── middleware/        # HTTP middleware
│   ├── models/            # Data models
│   └── repository/        # Data access layer
├── pkg/
│   ├── errors/            # Custom error types
│   └── utils/             # Utility functions
├── Dockerfile             # Production container
├── Dockerfile.dev         # Development container with hot reload
└── go.mod                # Go module definition
```

## Authentication

The API uses AWS Cognito for authentication:
- JWT tokens in Authorization header
- Token validation via Cognito
- User pool integration for signup/login
- Refresh token support

## Database

PostgreSQL database with:
- JSONB fields for flexible game data
- UUID primary keys
- Optimized indexes for GraphQL queries
- Connection pooling for performance

## Docker

### Development
```bash
docker build -f Dockerfile.dev -t swvtt-api:dev .
docker run -p 8080:8080 --env-file ../.env swvtt-api:dev
```

### Production
```bash
docker build -t swvtt-api:latest .
docker run -p 8080:8080 --env-file ../.env swvtt-api:latest
```

## Contributing

See the main project README.md for contribution guidelines.