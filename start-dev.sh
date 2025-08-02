#!/bin/bash

# Development environment startup script
# This script starts the Savage Worlds Virtual Table Top in development mode
# with hot reload for both frontend and backend

set -e

echo "🚀 Starting Savage Worlds Virtual Table Top - Development Environment"
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

echo "Using: $DOCKER_COMPOSE"

# Check if .env file exists
if [ ! -f .env ]; then
    echo "⚠️  Warning: .env file not found. Copying from .env.example..."
    if [ -f .env.example ]; then
        cp .env.example .env
        echo "✅ Created .env file from .env.example"
        echo "Please review and update the .env file with your settings."
    else
        echo "❌ Error: .env.example not found. Please create a .env file."
        exit 1
    fi
fi

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Error: Docker is not running. Please start Docker and try again."
    exit 1
fi

# Stop any existing containers
echo "🛑 Stopping any existing containers..."
$DOCKER_COMPOSE -f docker-compose.dev.yml down

# Build and start services
echo "🔨 Building services..."
$DOCKER_COMPOSE -f docker-compose.dev.yml build

echo "🚀 Starting services..."
$DOCKER_COMPOSE -f docker-compose.dev.yml up -d

# Wait for services to be healthy
echo "⏳ Waiting for services to be ready..."
sleep 5

# Check service health
echo "🔍 Checking service status..."
$DOCKER_COMPOSE -f docker-compose.dev.yml ps

# Display logs command
echo ""
echo "✅ Development environment is starting!"
echo ""
echo "🌐 Services:"
echo "   - Frontend: http://localhost:3000"
echo "   - API:      http://localhost:8080"
echo "   - Database: localhost:5432"
echo "   - PgAdmin:  http://localhost:5050 (if enabled)"
echo ""
echo "📝 Useful commands:"
echo "   - View logs:      $DOCKER_COMPOSE -f docker-compose.dev.yml logs -f"
echo "   - Stop services:  $DOCKER_COMPOSE -f docker-compose.dev.yml down"
echo "   - Restart:        $DOCKER_COMPOSE -f docker-compose.dev.yml restart"
echo ""
echo "🔥 Hot reload is enabled for both frontend and backend!"
echo "   - Frontend changes will auto-refresh"
echo "   - Backend changes will auto-rebuild and restart"
echo ""

# Follow logs
echo "Following logs (Ctrl+C to exit)..."
$DOCKER_COMPOSE -f docker-compose.dev.yml logs -f