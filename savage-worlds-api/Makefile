.PHONY: help build run test test-bdd clean docker-build docker-run migrate lint fmt

# Default target
help:
	@echo "Available targets:"
	@echo "  build        Build the application"
	@echo "  run          Run the application"
	@echo "  test         Run tests"
	@echo "  test-cover   Run tests with coverage"
	@echo "  test-bdd     Run BDD tests with Godog"
	@echo "  clean        Clean build artifacts"
	@echo "  docker-build Build Docker image"
	@echo "  docker-run   Run Docker container"
	@echo "  migrate      Run database migrations"
	@echo "  lint         Run linter"
	@echo "  fmt          Format code"
	@echo "  dev          Run in development mode with hot reload"
	@echo "  deps         Download dependencies"
	@echo "  tidy         Tidy go.mod"

# Build the application
build:
	@echo "Building application..."
	@go build -o api cmd/api/main.go

# Run the application
run: build
	@echo "Running application..."
	@./api

# Run tests
test:
	@echo "Running tests..."
	@go test -v ./...

# Run tests with coverage
test-cover:
	@echo "Running tests with coverage..."
	@go test -v -coverprofile=coverage.out ./...
	@go tool cover -html=coverage.out -o coverage.html
	@echo "Coverage report generated: coverage.html"

# Run BDD tests with Godog
test-bdd:
	@echo "Running BDD tests..."
	@godog --paths ../features --definitions test/features

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -f api coverage.out coverage.html
	@rm -rf dist/ build/

# Build Docker image
docker-build:
	@echo "Building Docker image..."
	@docker build -t savage-worlds-api:latest .

# Run Docker container
docker-run:
	@echo "Running Docker container..."
	@docker run -p 8080:8080 --env-file .env savage-worlds-api:latest

# Run database migrations
migrate:
	@echo "Running database migrations..."
	@cd ../database && ./migrate.sh

# Run linter
lint:
	@echo "Running linter..."
	@golangci-lint run ./...

# Format code
fmt:
	@echo "Formatting code..."
	@go fmt ./...

# Run in development mode with hot reload (requires air)
dev:
	@echo "Running in development mode..."
	@air

# Download dependencies
deps:
	@echo "Downloading dependencies..."
	@go mod download

# Tidy go.mod
tidy:
	@echo "Tidying go.mod..."
	@go mod tidy

# Install development tools
install-tools:
	@echo "Installing development tools..."
	@go install github.com/cosmtrek/air@latest
	@go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
	@go install github.com/cucumber/godog/cmd/godog@latest

# Generate swagger documentation (requires swag)
swagger:
	@echo "Generating Swagger documentation..."
	@swag init -g cmd/api/main.go -o docs/swagger