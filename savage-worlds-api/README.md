# Savage Worlds API

A REST API for the Savage Worlds Virtual Table Top (SWVTT) application, built with Go and the Gin framework.

## Features

- JWT-based authentication with access and refresh tokens
- Complete CRUD operations for plot points and game entities
- PostgreSQL database with connection pooling
- Comprehensive error handling and validation
- CORS support for frontend integration
- Health check and metrics endpoints
- Docker support for containerization

## Prerequisites

- Go 1.21 or higher
- PostgreSQL 12 or higher
- Docker (optional)

## Installation

1. Clone the repository:
```bash
cd savage-worlds-api
```

2. Install dependencies:
```bash
go mod download
```

3. Create a `.env` file with the following configuration:
```env
# Server Configuration
SERVER_PORT=8080
ENVIRONMENT=development

# Database Configuration
DB_HOST=localhost
DB_PORT=5432
DB_USER=swvtt_user
DB_PASSWORD=your_password
DB_NAME=swvtt
DB_SSL_MODE=disable

# JWT Configuration
JWT_SECRET=your-secret-key-here
JWT_ACCESS_EXPIRY=24h
JWT_REFRESH_EXPIRY=168h

# CORS Configuration
CORS_ALLOWED_ORIGINS=http://localhost:3000
```

4. Run database migrations (ensure the database exists):
```bash
cd ../database
./migrate.sh
```

## Running the Application

### Development Mode

```bash
go run cmd/api/main.go
```

### Production Mode

```bash
go build -o api cmd/api/main.go
ENVIRONMENT=production ./api
```

### Using Docker

```bash
# Build the image
docker build -t savage-worlds-api .

# Run the container
docker run -p 8080:8080 --env-file .env savage-worlds-api
```

## API Documentation

### Base URL

```
http://localhost:8080/api/v1
```

### Authentication

The API uses JWT bearer token authentication. Include the token in the Authorization header:

```
Authorization: Bearer <your-token>
```

### Endpoints

#### Health Check

- `GET /health` - Basic health check
- `GET /ready` - Readiness check (includes database)
- `GET /metrics` - Service metrics

#### Authentication

- `POST /auth/register` - Register a new user
- `POST /auth/login` - Login and receive tokens
- `POST /auth/refresh` - Refresh access token
- `POST /auth/logout` - Logout (authenticated)
- `GET /auth/me` - Get current user info (authenticated)
- `POST /auth/change-password` - Change password (authenticated)

#### Plot Points

All endpoints require authentication.

- `GET /plot-points` - List user's plot points
- `POST /plot-points` - Create a new plot point
- `GET /plot-points/:id` - Get a specific plot point
- `PUT /plot-points/:id` - Update a plot point
- `DELETE /plot-points/:id` - Delete a plot point
- `GET /plot-points/:id/:entity_type` - Get entity data (e.g., characters, beasts)
- `PUT /plot-points/:id/:entity_type` - Update entity data

#### Game Entities

All endpoints require authentication.

- `GET /game-entities` - List game entities with filters
- `POST /game-entities` - Create a new game entity
- `GET /game-entities/:id` - Get a specific game entity
- `PUT /game-entities/:id` - Update a game entity
- `DELETE /game-entities/:id` - Delete a game entity
- `GET /plot-points/:plot_point_id/entities` - List entities for a plot point
- `POST /game-entities/templates/:template_id/create` - Create entity from template

### Request/Response Examples

#### Register User

```bash
curl -X POST http://localhost:8080/api/v1/auth/register \
  -H "Content-Type: application/json" \
  -d '{
    "email": "user@example.com",
    "username": "testuser",
    "password": "SecurePass123!",
    "full_name": "Test User"
  }'
```

Response:
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIs...",
  "refresh_token": "eyJhbGciOiJIUzI1NiIs...",
  "token_type": "Bearer",
  "expires_in": 86400,
  "user": {
    "id": "123e4567-e89b-12d3-a456-426614174000",
    "email": "user@example.com",
    "username": "testuser",
    "full_name": "Test User"
  }
}
```

#### Create Plot Point

```bash
curl -X POST http://localhost:8080/api/v1/plot-points \
  -H "Authorization: Bearer <your-token>" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "My Campaign",
    "description": "A thrilling adventure",
    "basic_rules": {
      "maximumAttributePoints": 5,
      "maximumMajorHindrances": 1,
      "maximumMinorHindrances": 2,
      "maximumSkillPoints": 12
    }
  }'
```

#### Create Game Entity

```bash
curl -X POST http://localhost:8080/api/v1/game-entities \
  -H "Authorization: Bearer <your-token>" \
  -H "Content-Type: application/json" \
  -d '{
    "plot_point_id": "123e4567-e89b-12d3-a456-426614174000",
    "entity_type": "character",
    "name": "Brave Adventurer",
    "description": "A seasoned warrior",
    "data": {
      "attributes": {"agility": 6, "smarts": 8, "spirit": 6, "strength": 8, "vigor": 10},
      "skills": {"fighting": 8, "shooting": 6},
      "edges": ["Quick", "Brave"],
      "hindrances": ["Heroic"],
      "wounds": 0,
      "fatigue": 0,
      "bennies": 3
    }
  }'
```

### Error Responses

The API returns consistent error responses:

```json
{
  "error": "Error message",
  "code": "ERROR_CODE",
  "details": {} // Optional additional details
}
```

Common error codes:
- `VALIDATION_ERROR` - Invalid input data
- `UNAUTHORIZED` - Authentication required
- `FORBIDDEN` - Insufficient permissions
- `NOT_FOUND` - Resource not found
- `CONFLICT` - Resource conflict (e.g., duplicate email)
- `INTERNAL_ERROR` - Server error

### Pagination

List endpoints support pagination with query parameters:
- `page` - Page number (default: 1)
- `per_page` - Items per page (default: 20, max: 100)

Response includes pagination metadata:
```json
{
  "data": [...],
  "pagination": {
    "page": 1,
    "per_page": 20,
    "total": 100,
    "total_pages": 5
  }
}
```

## Project Structure

```
savage-worlds-api/
├── cmd/
│   └── api/
│       └── main.go          # Application entry point
├── config/
│   └── config.go           # Configuration management
├── internal/
│   ├── auth/              # Authentication services
│   ├── db/                # Database connection
│   ├── handlers/          # HTTP request handlers
│   ├── middleware/        # HTTP middleware
│   ├── models/            # Data models
│   └── repository/        # Data access layer
├── pkg/
│   ├── errors/            # Custom error types
│   ├── response/          # HTTP response helpers
│   └── utils/             # Utility functions
├── Dockerfile             # Docker configuration
├── go.mod                # Go module definition
├── go.sum                # Go module checksums
└── README.md             # This file
```

## Testing

Run tests with:
```bash
go test ./...
```

Run tests with coverage:
```bash
go test -cover ./...
```

## Performance Considerations

- Database connection pooling is configured with sensible defaults
- JWT tokens are stateless for horizontal scaling
- Prepared statements are used for all database queries
- Pagination is enforced on list endpoints
- GIN indexes are created on JSONB columns for efficient queries

## Security

- Passwords are hashed using bcrypt
- JWT tokens expire after 24 hours (configurable)
- Input validation on all endpoints
- SQL injection protection through parameterized queries
- CORS configuration for frontend security

## License

This project is part of the Savage Worlds Virtual Table Top application.