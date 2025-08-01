# Frontend Migration from AWS Amplify to Go REST API

## Overview
This document describes the changes made to migrate the React frontend from AWS Amplify/GraphQL to a standalone REST API integration using React Query and Axios.

## Key Changes

### 1. Dependencies Updated
- **Removed**: 
  - `aws-amplify`
  - `aws-amplify-react`
  - `@aws-amplify/ui`
  - `graphql`

- **Added**:
  - `@tanstack/react-query` (v5.17.0) - For data fetching and caching
  - `@tanstack/react-query-devtools` - Development tools
  - `axios` (v1.6.5) - HTTP client

### 2. New Directory Structure
```
src/
├── services/          # API service layer
│   ├── api.js        # Axios configuration with interceptors
│   ├── authService.js # Authentication services
│   ├── plotPointService.js # Plot point CRUD operations
│   ├── gameEntityService.js # Game entity operations
│   └── index.js      # Service exports
├── contexts/         # React contexts
│   └── AuthContext.js # JWT authentication state management
├── hooks/           # Custom React hooks
│   ├── usePlotPoints.js # Plot point data hooks
│   ├── useGameEntities.js # Game entity hooks
│   └── index.js     # Hook exports
└── config/          # Application configuration
    └── index.js     # Centralized config
```

### 3. Authentication Changes

#### JWT Token Management
- Tokens stored in localStorage
- Automatic token refresh via Axios interceptors
- Auth context provides user state and auth methods

#### Login Flow
```javascript
const { login } = useAuth();
const result = await login({ email, password });
if (result.success) {
  // Navigate to protected route
}
```

#### Protected Routes
```javascript
<ProtectedRoute>
  <PlotPointList />
</ProtectedRoute>
```

### 4. Data Fetching Pattern

#### Using React Query Hooks
```javascript
// List data
const { data, isLoading, error } = usePlotPoints(page, limit);

// Single item
const { data: plotPoint } = usePlotPoint(id);

// Mutations
const createMutation = useCreatePlotPoint();
createMutation.mutate(plotPointData);
```

### 5. API Configuration

#### Environment Variables
Create a `.env` file:
```
REACT_APP_API_URL=http://localhost:8080/api/v1
```

#### Axios Interceptors
- Automatically adds JWT token to requests
- Handles token refresh on 401 responses
- Redirects to login on auth failure

### 6. Component Updates

All components have been updated to:
- Use functional components with hooks
- Replace GraphQL operations with REST API calls
- Implement proper loading and error states
- Use React Query for caching and synchronization

### 7. Removed Files
- `src/aws-exports.js`
- `src/graphql/` directory (queries, mutations, subscriptions)
- AWS Mobile configuration files

## Migration Checklist

- [x] Update package.json dependencies
- [x] Create API service layer
- [x] Implement JWT authentication
- [x] Create React Query hooks
- [x] Update all page components
- [x] Update navigation components
- [x] Configure environment variables
- [x] Remove AWS-specific code

## Usage Examples

### Fetching Data
```javascript
import { usePlotPoints } from '../hooks';

function PlotPointList() {
  const { data, isLoading, error } = usePlotPoints();
  
  if (isLoading) return <LoadingSpinner />;
  if (error) return <ErrorMessage error={error} />;
  
  return <List items={data.data} />;
}
```

### Creating Data
```javascript
import { useCreatePlotPoint } from '../hooks';

function AddPlotPoint() {
  const createMutation = useCreatePlotPoint();
  
  const handleSubmit = (data) => {
    createMutation.mutate(data, {
      onSuccess: () => navigate('/'),
      onError: (error) => setErrors([error.message])
    });
  };
}
```

### Authentication
```javascript
import { useAuth } from '../contexts/AuthContext';

function LoginPage() {
  const { login, error, loading } = useAuth();
  
  const handleLogin = async (credentials) => {
    const result = await login(credentials);
    if (result.success) {
      navigate('/');
    }
  };
}
```

## Running the Application

1. Install dependencies:
   ```bash
   npm install
   ```

2. Set environment variables:
   ```bash
   cp .env.example .env
   # Edit .env with your API URL
   ```

3. Start the development server:
   ```bash
   npm start
   ```

4. The app will be available at http://localhost:3000

## Notes

- All API endpoints follow RESTful conventions
- Authentication uses JWT tokens with refresh capability
- React Query handles caching, background refetching, and optimistic updates
- The proxy in package.json forwards API requests in development