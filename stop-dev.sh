#!/bin/bash

# Development environment stop script
# This script stops the Savage Worlds Virtual Table Top development environment

set -e

echo "🛑 Stopping Savage Worlds Virtual Table Top - Development Environment"
echo "===================================================================="

# Determine which docker compose command to use
if command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE="docker-compose"
elif command -v docker &> /dev/null && docker compose version &> /dev/null; then
    DOCKER_COMPOSE="docker compose"
else
    echo "❌ Error: Neither 'docker-compose' nor 'docker compose' command found."
    echo "Please install Docker Desktop or Docker Compose."
    exit 1
fi

# Stop services
echo "🛑 Stopping services..."
$DOCKER_COMPOSE -f docker-compose.dev.yml down

echo "✅ All services stopped successfully!"
echo ""
echo "To start again, run: ./start-dev.sh"