#!/bin/bash

# Savage Worlds VTT - Database Backup Script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Create backups directory if it doesn't exist
BACKUP_DIR="./backups"
mkdir -p $BACKUP_DIR

# Generate timestamp
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

# Load environment variables
if [ -f .env ]; then
    export $(cat .env | grep -v '^#' | xargs)
fi

# Default values if not set
DB_USER=${DB_USER:-swvtt_user}
DB_NAME=${DB_NAME:-swvtt_db}

echo -e "${GREEN}Starting database backup...${NC}"

# Check if docker-compose is installed
if ! command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE="docker compose"
else
    DOCKER_COMPOSE="docker-compose"
fi

# Get the correct compose file based on running containers
if docker ps | grep -q "swvtt-postgres-prod"; then
    CONTAINER_NAME="swvtt-postgres-prod"
    echo -e "${YELLOW}Backing up production database${NC}"
else
    CONTAINER_NAME="swvtt-postgres"
    echo -e "${YELLOW}Backing up development database${NC}"
fi

# Perform backup
BACKUP_FILE="$BACKUP_DIR/swvtt_backup_${TIMESTAMP}.sql"
echo -e "${GREEN}Creating backup: $BACKUP_FILE${NC}"

docker exec $CONTAINER_NAME pg_dump -U $DB_USER $DB_NAME > $BACKUP_FILE

# Compress the backup
gzip $BACKUP_FILE

echo -e "${GREEN}Backup completed: ${BACKUP_FILE}.gz${NC}"

# Clean up old backups (keep last 7 days)
echo -e "${YELLOW}Cleaning up old backups...${NC}"
find $BACKUP_DIR -name "swvtt_backup_*.sql.gz" -mtime +7 -delete

echo -e "${GREEN}Backup process completed!${NC}"