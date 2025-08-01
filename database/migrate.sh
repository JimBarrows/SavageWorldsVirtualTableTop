#!/bin/bash

# SWVTT Database Migration Script
# This script helps manage database migrations for the SWVTT project

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
DB_HOST="${DB_HOST:-localhost}"
DB_PORT="${DB_PORT:-5432}"
DB_NAME="${DB_NAME:-swvtt_db}"
DB_USER="${DB_USER:-swvtt_user}"
DB_PASSWORD="${DB_PASSWORD:-swvtt_dev_password}"
MIGRATIONS_DIR="${MIGRATIONS_DIR:-database/migrations}"

# Construct database URL
DATABASE_URL="postgres://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${DB_PORT}/${DB_NAME}?sslmode=disable"

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Function to check if migrate is installed
check_migrate() {
    if ! command -v migrate &> /dev/null; then
        print_error "golang-migrate is not installed"
        echo "Install it with:"
        echo "  macOS: brew install golang-migrate"
        echo "  Linux: Download from https://github.com/golang-migrate/migrate/releases"
        exit 1
    fi
}

# Function to check database connection
check_db_connection() {
    print_status "Checking database connection..."
    if PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c '\q' 2>/dev/null; then
        print_status "Database connection successful"
    else
        print_error "Cannot connect to database"
        print_error "Make sure PostgreSQL is running: docker-compose up -d"
        exit 1
    fi
}

# Function to run migrations
run_migration() {
    local cmd=$1
    shift
    
    print_status "Running: migrate $cmd $@"
    migrate -path $MIGRATIONS_DIR -database "$DATABASE_URL" $cmd $@
    
    if [ $? -eq 0 ]; then
        print_status "Migration completed successfully"
    else
        print_error "Migration failed"
        exit 1
    fi
}

# Main script
case "${1:-help}" in
    up)
        check_migrate
        check_db_connection
        print_status "Running all pending migrations..."
        run_migration up
        ;;
    
    down)
        check_migrate
        check_db_connection
        COUNT="${2:-1}"
        print_warning "Rolling back $COUNT migration(s)..."
        run_migration down $COUNT
        ;;
    
    version)
        check_migrate
        check_db_connection
        print_status "Current migration version:"
        run_migration version
        ;;
    
    goto)
        if [ -z "$2" ]; then
            print_error "Version number required"
            echo "Usage: $0 goto VERSION"
            exit 1
        fi
        check_migrate
        check_db_connection
        print_warning "Going to version $2..."
        run_migration goto $2
        ;;
    
    force)
        if [ -z "$2" ]; then
            print_error "Version number required"
            echo "Usage: $0 force VERSION"
            exit 1
        fi
        check_migrate
        check_db_connection
        print_warning "FORCING version to $2 (use with caution!)..."
        run_migration force $2
        ;;
    
    create)
        if [ -z "$2" ]; then
            print_error "Migration name required"
            echo "Usage: $0 create MIGRATION_NAME"
            exit 1
        fi
        check_migrate
        print_status "Creating new migration: $2"
        migrate create -ext sql -dir $MIGRATIONS_DIR -seq $2
        ;;
    
    seed)
        check_migrate
        check_db_connection
        print_status "Loading test data..."
        run_migration up 7
        ;;
    
    reset)
        check_migrate
        check_db_connection
        print_warning "Resetting database (all data will be lost!)..."
        read -p "Are you sure? (y/N) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            run_migration down -all
            run_migration up
            print_status "Database reset complete"
        else
            print_status "Reset cancelled"
        fi
        ;;
    
    status)
        check_migrate
        check_db_connection
        print_status "Database status:"
        echo "Host: $DB_HOST:$DB_PORT"
        echo "Database: $DB_NAME"
        echo "User: $DB_USER"
        echo "Migrations directory: $MIGRATIONS_DIR"
        echo
        print_status "Current version:"
        run_migration version
        ;;
    
    help|*)
        echo "SWVTT Database Migration Tool"
        echo
        echo "Usage: $0 COMMAND [arguments]"
        echo
        echo "Commands:"
        echo "  up              Run all pending migrations"
        echo "  down [N]        Rollback N migrations (default: 1)"
        echo "  version         Show current migration version"
        echo "  goto VERSION    Go to specific version"
        echo "  force VERSION   Force database to version (use with caution)"
        echo "  create NAME     Create new migration files"
        echo "  seed            Load test data (runs up to migration 7)"
        echo "  reset           Reset database (removes all data)"
        echo "  status          Show database connection status"
        echo "  help            Show this help message"
        echo
        echo "Environment variables:"
        echo "  DB_HOST         Database host (default: localhost)"
        echo "  DB_PORT         Database port (default: 5432)"
        echo "  DB_NAME         Database name (default: swvtt_db)"
        echo "  DB_USER         Database user (default: swvtt_user)"
        echo "  DB_PASSWORD     Database password (default: swvtt_dev_password)"
        echo "  MIGRATIONS_DIR  Migrations directory (default: database/migrations)"
        ;;
esac