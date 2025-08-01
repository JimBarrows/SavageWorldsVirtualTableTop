# SWVTT Database Setup and Migration Guide

This directory contains the PostgreSQL database schema and migration files for the Savage Worlds Virtual Table Top (SWVTT) re-architecture project.

## Overview

The database schema has been designed to replace the existing AWS DynamoDB structure with a local PostgreSQL database. The schema uses JSONB columns extensively to maintain flexibility for game data while providing the benefits of a relational database.

## Database Structure

### Core Tables

1. **users** - User accounts and authentication
   - UUID primary keys
   - Support for AWS Cognito migration via `cognito_sub` column
   - Metadata JSONB column for extensibility

2. **plot_points** - Main game configuration containers
   - Contains all game rules, settings, and definitions
   - Extensive use of JSONB for flexible game data storage
   - GIN indexes for efficient JSONB querying

3. **game_entities** - Individual game objects (characters, beasts, items, etc.)
   - Polymorphic design using `entity_type` discriminator
   - JSONB `data` column for entity-specific attributes
   - Support for templates and active/inactive states

4. **game_sessions** - Game session management
   - Tracks active games and their participants
   - Links players to characters
   - Session state management

5. **audit_log** - Complete audit trail
   - Tracks all changes to the system
   - Stores change diffs in JSONB format

### Key Features

- **JSONB Validation**: Custom PostgreSQL functions validate game data structures
- **Automatic Timestamps**: Triggers maintain created_at/updated_at columns
- **Referential Integrity**: Foreign key constraints ensure data consistency
- **Performance Indexes**: Comprehensive indexing strategy for optimal query performance

## Setup Instructions

### Prerequisites

- Docker and Docker Compose installed
- golang-migrate tool (for running migrations)

### Quick Start

1. Start the PostgreSQL database:
   ```bash
   docker-compose up -d
   ```

2. Install golang-migrate:
   ```bash
   # macOS
   brew install golang-migrate
   
   # or download from https://github.com/golang-migrate/migrate
   ```

3. Run migrations:
   ```bash
   # Set database URL
   export DATABASE_URL="postgres://swvtt_user:swvtt_dev_password@localhost:5432/swvtt_db?sslmode=disable"
   
   # Run all migrations
   migrate -path database/migrations -database $DATABASE_URL up
   
   # Or run specific version
   migrate -path database/migrations -database $DATABASE_URL goto 6
   ```

### Migration Commands

```bash
# Create new migration
migrate create -ext sql -dir database/migrations -seq migration_name

# Run all pending migrations
migrate -path database/migrations -database $DATABASE_URL up

# Rollback one migration
migrate -path database/migrations -database $DATABASE_URL down 1

# Force version (use with caution)
migrate -path database/migrations -database $DATABASE_URL force VERSION

# Check current version
migrate -path database/migrations -database $DATABASE_URL version
```

### Accessing the Database

1. **Using psql**:
   ```bash
   docker exec -it swvtt-postgres psql -U swvtt_user -d swvtt_db
   ```

2. **Using pgAdmin**:
   - Open http://localhost:5050
   - Login: admin@swvtt.local / admin_password
   - Add server: postgres:5432, swvtt_user / swvtt_dev_password

## Migration Files

| File | Description |
|------|-------------|
| 001_create_users.up/down.sql | User accounts table with authentication support |
| 002_create_plot_points.up/down.sql | Main game configuration storage |
| 003_create_game_entities.up/down.sql | Polymorphic game objects table |
| 004_create_game_sessions.up/down.sql | Game session management |
| 005_create_audit_log.up/down.sql | Audit trail for all changes |
| 006_create_validation_functions.up/down.sql | JSONB validation functions and constraints |
| 007_seed_test_data.up/down.sql | Sample data for testing |

## Data Migration from DynamoDB

The schema includes support for migrating from AWS DynamoDB:

1. The `users.cognito_sub` column maps to AWS Cognito identities
2. Plot points can be imported with all their nested data intact
3. The JSONB structure closely mirrors the GraphQL schema

## Development Notes

### JSONB Query Examples

```sql
-- Find all plot points with specific skills
SELECT * FROM plot_points 
WHERE skills @> '[{"name": "Fighting"}]'::jsonb;

-- Get all characters with high agility
SELECT * FROM game_entities 
WHERE entity_type = 'character' 
AND (data->'agility'->>'dice')::text IN ('d10', 'd12');

-- Find all game sessions in progress
SELECT gs.*, u.username as gm_name 
FROM game_sessions gs 
JOIN users u ON gs.gm_id = u.id 
WHERE gs.status = 'active';
```

### Adding New Entity Types

1. Add the new type to the `entity_type` CHECK constraint in migration 003
2. Create validation functions if needed
3. Document the expected JSONB structure

### Performance Considerations

- GIN indexes are created on frequently queried JSONB columns
- Partial indexes optimize common query patterns
- The audit_log table includes a partial index for recent entries

## Testing

The seed data (migration 007) includes:
- 3 test users (GM and 2 players)
- 1 complete plot point with game data
- Sample characters and creatures
- A test game session

To load test data:
```bash
migrate -path database/migrations -database $DATABASE_URL up 7
```

To remove test data:
```bash
migrate -path database/migrations -database $DATABASE_URL down 1
```

## Security Notes

- Change default passwords before production use
- Enable SSL for production PostgreSQL connections
- Implement row-level security for multi-tenant access
- Regular backups recommended using pg_dump