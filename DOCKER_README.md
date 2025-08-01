# Docker Setup Guide for Savage Worlds VTT

This guide provides comprehensive instructions for running the Savage Worlds Virtual Table Top application using Docker.

## Prerequisites

- Docker Desktop (version 20.10 or higher)
- Docker Compose (version 2.0 or higher)
- 4GB of available RAM
- 10GB of available disk space

## Quick Start

1. **Clone the repository** (if you haven't already)
   ```bash
   git clone <repository-url>
   cd SavageWorldsVirtualTableTop
   ```

2. **Copy environment configuration**
   ```bash
   cp .env.example .env
   ```

3. **Start the application**
   ```bash
   ./scripts/start.sh
   ```

The application will be available at:
- Frontend: http://localhost:3000
- API: http://localhost:8080
- PgAdmin: http://localhost:5050

## Environment Configuration

### Development Environment

The development environment includes hot reload for both the frontend and backend:

```bash
./scripts/start.sh development
# or
./scripts/start.sh dev
```

Features:
- Go API with Air for hot reload
- React with hot module replacement
- Volume mounts for live code updates
- Detailed logging

### Production Environment

The production environment is optimized for performance:

```bash
./scripts/start.sh production
# or
./scripts/start.sh prod
```

Features:
- Optimized builds
- Nginx serving static files
- Resource limits
- Security headers

## Service Architecture

### Services Overview

1. **postgres** - PostgreSQL 15 database
   - Port: 5432
   - Health checks enabled
   - Automatic initialization scripts

2. **api** - Go REST API
   - Port: 8080
   - JWT authentication
   - Database migrations
   - Health endpoint: /health

3. **frontend** - React application
   - Port: 3000 (dev) / 80 (prod)
   - Hot reload in development
   - Nginx in production

4. **pgadmin** - Database management tool
   - Port: 5050
   - Default login: admin@swvtt.local / admin_password

## Common Operations

### Viewing Logs

```bash
# All services
./scripts/logs.sh

# Specific service
./scripts/logs.sh development api
./scripts/logs.sh development frontend
```

### Stopping Services

```bash
# Stop all services
./scripts/stop.sh

# Stop and remove volumes (WARNING: Deletes database)
docker-compose -f docker-compose.dev.yml down -v
```

### Database Operations

#### Backup Database
```bash
./scripts/backup.sh
```
Backups are stored in `./backups/` directory.

#### Access Database
```bash
# Via PgAdmin
# Navigate to http://localhost:5050
# Use connection details from .env file

# Via psql
docker exec -it swvtt-postgres psql -U swvtt_user -d swvtt_db
```

#### Run Migrations
```bash
docker exec -it swvtt-api /app/migrate up
```

### Rebuilding Services

```bash
# Rebuild all services
docker-compose -f docker-compose.dev.yml build

# Rebuild specific service
docker-compose -f docker-compose.dev.yml build api
```

## Troubleshooting

### Port Conflicts

If you encounter port conflicts:

1. Check what's using the port:
   ```bash
   lsof -i :3000  # Frontend
   lsof -i :8080  # API
   lsof -i :5432  # PostgreSQL
   ```

2. Either stop the conflicting service or change ports in `.env`

### Database Connection Issues

1. Ensure PostgreSQL is healthy:
   ```bash
   docker-compose -f docker-compose.dev.yml ps
   ```

2. Check database logs:
   ```bash
   ./scripts/logs.sh postgres
   ```

3. Verify connection settings in `.env`

### Frontend Not Updating

For development hot reload issues:

1. Check if volumes are mounted correctly:
   ```bash
   docker-compose -f docker-compose.dev.yml config
   ```

2. Restart the frontend service:
   ```bash
   docker-compose -f docker-compose.dev.yml restart frontend
   ```

### Memory Issues

If services are running slowly:

1. Increase Docker Desktop memory allocation
2. Check resource usage:
   ```bash
   docker stats
   ```

## Environment Variables

Key environment variables (see `.env.example` for full list):

| Variable | Description | Default |
|----------|-------------|---------|
| `DB_USER` | Database username | swvtt_user |
| `DB_PASSWORD` | Database password | swvtt_dev_password |
| `DB_NAME` | Database name | swvtt_db |
| `JWT_SECRET` | JWT signing secret | (must be set) |
| `API_PORT` | API port | 8080 |
| `FRONTEND_PORT` | Frontend port | 3000 |

## Security Considerations

### Development
- Uses default passwords (change for any public exposure)
- CORS configured for localhost only
- No SSL/TLS

### Production
- Always change default passwords
- Use strong JWT secrets
- Configure proper CORS origins
- Use reverse proxy with SSL/TLS
- Enable database SSL mode
- Implement rate limiting

## Maintenance

### Regular Tasks

1. **Database Backups**
   ```bash
   # Manual backup
   ./scripts/backup.sh
   
   # Automated backups (add to cron)
   0 2 * * * /path/to/project/scripts/backup.sh
   ```

2. **Update Dependencies**
   ```bash
   # Pull latest images
   docker-compose -f docker-compose.prod.yml pull
   
   # Rebuild with latest dependencies
   docker-compose -f docker-compose.prod.yml build --no-cache
   ```

3. **Monitor Logs**
   ```bash
   # Check for errors
   ./scripts/logs.sh | grep ERROR
   ```

4. **Clean Up**
   ```bash
   # Remove unused images
   docker image prune -a
   
   # Remove unused volumes
   docker volume prune
   ```

## Advanced Configuration

### Custom Networks

To integrate with existing Docker networks:

```yaml
networks:
  swvtt-network:
    external:
      name: my-existing-network
```

### Volume Persistence

To use named volumes for source code:

```yaml
volumes:
  api-source:
    driver: local
    driver_opts:
      type: none
      device: ${PWD}/savage-worlds-api
      o: bind
```

### Health Check Customization

Modify health check parameters in docker-compose files:

```yaml
healthcheck:
  test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
  interval: 30s
  timeout: 10s
  retries: 3
  start_period: 40s
```

## Support

For issues specific to Docker setup:
1. Check the logs: `./scripts/logs.sh`
2. Verify environment variables: `docker-compose config`
3. Ensure all prerequisites are met
4. Check the troubleshooting section above

For application-specific issues, refer to the main README.md.