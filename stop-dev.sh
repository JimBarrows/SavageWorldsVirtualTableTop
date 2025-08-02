#!/bin/bash

# Development environment stop script
# This script stops the Savage Worlds Virtual Table Top development environment

set -e

echo "🛑 Stopping Savage Worlds Virtual Table Top - Development Environment"
echo "===================================================================="

# Stop services
echo "🛑 Stopping services..."
docker-compose -f docker-compose.dev.yml down

echo "✅ All services stopped successfully!"
echo ""
echo "To start again, run: ./start-dev.sh"