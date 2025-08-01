#!/bin/bash

# Savage Worlds VTT - Docker Logs Script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default environment
ENVIRONMENT=${1:-development}
SERVICE=${2:-}

# Validate environment
case $ENVIRONMENT in
    development|dev)
        COMPOSE_FILE="docker-compose.dev.yml"
        ;;
    production|prod)
        COMPOSE_FILE="docker-compose.prod.yml"
        ;;
    *)
        # If first argument is a service name, use development by default
        if [ -n "$ENVIRONMENT" ]; then
            SERVICE=$ENVIRONMENT
            ENVIRONMENT="development"
            COMPOSE_FILE="docker-compose.dev.yml"
        else
            echo -e "${RED}Invalid environment: $ENVIRONMENT${NC}"
            echo "Usage: $0 [environment] [service]"
            echo "  environment: development|dev|production|prod (default: development)"
            echo "  service: postgres|api|frontend|pgadmin (optional)"
            exit 1
        fi
        ;;
esac

# Check if docker-compose is installed
if ! command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE="docker compose"
else
    DOCKER_COMPOSE="docker-compose"
fi

# Show logs
if [ -z "$SERVICE" ]; then
    echo -e "${GREEN}Showing logs for all services...${NC}"
    $DOCKER_COMPOSE -f $COMPOSE_FILE logs -f --tail=100
else
    echo -e "${GREEN}Showing logs for $SERVICE...${NC}"
    $DOCKER_COMPOSE -f $COMPOSE_FILE logs -f --tail=100 $SERVICE
fi