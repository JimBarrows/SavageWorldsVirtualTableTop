# Savage Worlds Virtual Table Top - React Frontend

This React application serves as the frontend for the Savage Worlds Virtual Table Top system, integrating with the Go REST API backend.

## Architecture Overview

The application uses:
- **React 18** with React Router for navigation
- **React Query (TanStack Query)** for server state management
- **Axios** for HTTP requests with automatic token refresh
- **JWT-based authentication** with access/refresh token pattern
- **Bootstrap** for UI components

## Key Features

### Authentication
- JWT-based authentication with automatic token refresh
- Protected routes that redirect to login when unauthenticated
- User registration and login flows
- Persistent authentication across browser sessions

### API Integration
- RESTful API client with interceptors for authentication
- Automatic retry with token refresh on 401 errors
- Service layer abstracting API calls
- React Query hooks for data fetching and caching

### Data Management
- Plot Points CRUD operations
- Game entities management (characters, beasts, gear, etc.)
- Optimistic updates with React Query mutations
- Automatic cache invalidation on data changes

## Project Structure

```
src/
├── components/          # Reusable UI components
├── contexts/           # React contexts (AuthContext)
├── hooks/              # Custom React hooks
├── models/             # Data models
├── pages/              # Page components
├── services/           # API service layer
│   ├── api.js         # Axios instance with interceptors
│   ├── authService.js # Authentication endpoints
│   ├── plotPointService.js # Plot point endpoints
│   └── gameEntityService.js # Game entity endpoints
└── config/            # Application configuration
```

## Setup and Configuration

### Environment Variables

Create a `.env` file based on `.env.example`:

```bash
cp .env.example .env
```

Configure the following variables:
- `REACT_APP_API_URL`: Backend API URL (default: http://localhost:8080/api/v1)
- `REACT_APP_API_TIMEOUT`: API request timeout in milliseconds

### Installation

```bash
npm install
```

### Development

```bash
npm start
```

The application will run on http://localhost:3000 and proxy API requests to the backend.

### Production Build

```bash
npm run build
```

## API Integration Details

### Authentication Flow

1. **Login**: POST to `/auth/login` returns access and refresh tokens
2. **Token Storage**: Tokens stored in localStorage
3. **Automatic Refresh**: When access token expires, interceptor uses refresh token
4. **Logout**: Clears tokens and redirects to login

### Service Layer

Each service module provides typed methods for API endpoints:

```javascript
// Example: Plot Point Service
plotPointService.getPlotPoints(page, limit, filters)
plotPointService.getPlotPoint(id)
plotPointService.createPlotPoint(data)
plotPointService.updatePlotPoint(id, data)
plotPointService.deletePlotPoint(id)
```

### React Query Hooks

Custom hooks wrap React Query for better developer experience:

```javascript
// Example usage in components
const { data, isLoading, error } = usePlotPoints(page, limit);
const createMutation = useCreatePlotPoint();
```

## Testing

```bash
# Unit tests
npm run test:unit

# Integration tests
npm run test:ui

# All tests
npm test
```

## Docker Support

Build and run with Docker:

```bash
docker build -t swvtt-frontend .
docker run -p 3000:3000 swvtt-frontend
```

Or use Docker Compose:

```bash
npm run start:docker
```

## Migration Notes

This application has been migrated from AWS Amplify/GraphQL to a REST API architecture. Key changes include:

- Removed all AWS Amplify dependencies
- Replaced GraphQL queries/mutations with REST endpoints
- Implemented JWT authentication instead of AWS Cognito
- Added React Query for server state management
- Created service layer for API abstraction

All UI functionality has been preserved during the migration.