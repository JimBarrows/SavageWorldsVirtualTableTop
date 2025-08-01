#!/bin/bash

# Savage Worlds VTT - Docker Stop Script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default environment
ENVIRONMENT=${1:-development}

echo -e "${YELLOW}Stopping Savage Worlds VTT...${NC}"

# Validate environment
case $ENVIRONMENT in
    development|dev)
        COMPOSE_FILE="docker-compose.dev.yml"
        ;;
    production|prod)
        COMPOSE_FILE="docker-compose.prod.yml"
        ;;
    *)
        echo -e "${RED}Invalid environment: $ENVIRONMENT${NC}"
        echo "Usage: $0 [development|dev|production|prod]"
        exit 1
        ;;
esac

# Check if docker-compose is installed
if ! command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE="docker compose"
else
    DOCKER_COMPOSE="docker-compose"
fi

# Stop services
echo -e "${GREEN}Stopping services...${NC}"
$DOCKER_COMPOSE -f $COMPOSE_FILE down

echo -e "${GREEN}Services stopped successfully!${NC}"
echo ""
echo -e "To remove volumes (database data): ${YELLOW}$DOCKER_COMPOSE -f $COMPOSE_FILE down -v${NC}"