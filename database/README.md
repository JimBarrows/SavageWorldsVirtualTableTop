# Database

PostgreSQL database schema and migrations for the Savage Worlds Virtual Table Top (SWVTT).

## Overview

The database uses PostgreSQL 15 with extensive JSONB columns for flexible game data storage while maintaining relational integrity.

### Core Tables

1. **users** - User accounts and authentication
   - UUID primary keys
   - AWS Cognito integration support
   - Metadata JSONB for extensibility

2. **plot_points** - Campaign containers
   - Game rules, settings, and definitions
   - JSONB for flexible game data
   - GIN indexes for performance

3. **game_entities** - Game objects (characters, beasts, items)
   - Polymorphic design with entity_type
   - JSONB data column for attributes
   - Template support

4. **game_sessions** - Active game management
   - Links players to characters
   - Session state tracking

5. **audit_log** - Complete change history
   - Tracks all system changes
   - JSONB diff storage

## Quick Start

### Using Docker Compose (Recommended)

From the project root:
```bash
./scripts/start.sh
```

### Manual Setup

1. Start PostgreSQL:
```bash
docker run -d \
  --name swvtt-postgres \
  -e POSTGRES_USER=swvtt_user \
  -e POSTGRES_PASSWORD=swvtt_dev_password \
  -e POSTGRES_DB=swvtt_db \
  -p 5432:5432 \
  postgres:15-alpine
```

2. Run migrations:
```bash
export DATABASE_URL="postgres://swvtt_user:swvtt_dev_password@localhost:5432/swvtt_db?sslmode=disable"
migrate -path migrations -database $DATABASE_URL up
```

## Migrations

### Running Migrations
```bash
# All migrations
migrate -path migrations -database $DATABASE_URL up

# Specific version
migrate -path migrations -database $DATABASE_URL goto 6

# Rollback one
migrate -path migrations -database $DATABASE_URL down 1
```

### Creating New Migrations
```bash
migrate create -ext sql -dir migrations -seq migration_name
```

### Migration Files

| Version | Description |
|---------|-------------|
| 001 | User accounts with auth support |
| 002 | Plot points (campaigns) |
| 003 | Game entities (characters, items) |
| 004 | Game sessions |
| 005 | Audit logging |
| 006 | JSONB validation functions |
| 007 | Test data seeding |

## Database Access

### psql
```bash
docker exec -it swvtt-postgres psql -U swvtt_user -d swvtt_db
```

### pgAdmin
- URL: http://localhost:5050
- Login: admin@swvtt.local / admin_password
- Server: postgres:5432

## Development

### Sample Queries
```sql
-- Find characters by attribute
SELECT * FROM game_entities 
WHERE entity_type = 'character' 
AND data->>'name' LIKE '%Hero%';

-- Get active sessions
SELECT * FROM game_sessions 
WHERE status = 'active';

-- Audit trail for entity
SELECT * FROM audit_log 
WHERE table_name = 'game_entities' 
AND record_id = 'some-uuid'
ORDER BY created_at DESC;
```

### Adding Entity Types
1. Update entity_type constraint in migration 003
2. Add validation functions if needed
3. Document JSONB structure

## Testing

Load test data:
```bash
migrate -path migrations -database $DATABASE_URL up 7
```

Remove test data:
```bash
migrate -path migrations -database $DATABASE_URL down 1
```

## Production Notes

- Use strong passwords
- Enable SSL connections
- Configure connection pooling
- Set up automated backups
- Monitor query performance

## Backup and Restore

### Backup
```bash
docker exec swvtt-postgres pg_dump -U swvtt_user swvtt_db > backup.sql
```

### Restore
```bash
docker exec -i swvtt-postgres psql -U swvtt_user swvtt_db < backup.sql
```