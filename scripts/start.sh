#!/bin/bash

# Savage Worlds VTT - Docker Startup Script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default environment
ENVIRONMENT=${1:-development}

echo -e "${GREEN}Starting Savage Worlds VTT in ${ENVIRONMENT} mode...${NC}"

# Check if .env file exists
if [ ! -f .env ]; then
    echo -e "${YELLOW}No .env file found. Creating from .env.example...${NC}"
    cp .env.example .env
    echo -e "${GREEN}.env file created. Please update it with your settings.${NC}"
fi

# Validate environment
case $ENVIRONMENT in
    development|dev)
        COMPOSE_FILE="docker-compose.dev.yml"
        echo -e "${GREEN}Using development configuration with hot reload${NC}"
        ;;
    production|prod)
        COMPOSE_FILE="docker-compose.prod.yml"
        echo -e "${YELLOW}Using production configuration${NC}"
        ;;
    *)
        echo -e "${RED}Invalid environment: $ENVIRONMENT${NC}"
        echo "Usage: $0 [development|dev|production|prod]"
        exit 1
        ;;
esac

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo -e "${RED}Docker is not running. Please start Docker and try again.${NC}"
    exit 1
fi

# Check if docker-compose is installed
if ! command -v docker-compose &> /dev/null; then
    echo -e "${YELLOW}docker-compose not found, trying docker compose...${NC}"
    DOCKER_COMPOSE="docker compose"
else
    DOCKER_COMPOSE="docker-compose"
fi

# Pull latest images
echo -e "${GREEN}Pulling latest images...${NC}"
$DOCKER_COMPOSE -f $COMPOSE_FILE pull

# Build services
echo -e "${GREEN}Building services...${NC}"
$DOCKER_COMPOSE -f $COMPOSE_FILE build

# Start services
echo -e "${GREEN}Starting services...${NC}"
$DOCKER_COMPOSE -f $COMPOSE_FILE up -d

# Wait for services to be healthy
echo -e "${GREEN}Waiting for services to be healthy...${NC}"
sleep 5

# Check service status
$DOCKER_COMPOSE -f $COMPOSE_FILE ps

echo -e "${GREEN}Services started successfully!${NC}"
echo ""
echo "Access the application at:"
echo -e "  Frontend: ${GREEN}http://localhost:3000${NC}"
echo -e "  API:      ${GREEN}http://localhost:8080${NC}"
echo -e "  PgAdmin:  ${GREEN}http://localhost:5050${NC}"
echo ""
echo "Default credentials:"
echo "  PgAdmin Email: admin@swvtt.local"
echo "  PgAdmin Password: admin_password"
echo ""
echo -e "To view logs: ${YELLOW}$DOCKER_COMPOSE -f $COMPOSE_FILE logs -f${NC}"
echo -e "To stop:      ${YELLOW}./scripts/stop.sh${NC}"