# Development Dockerfile for React with hot reload
FROM node:18-alpine

# Set working directory
WORKDIR /app

# Copy package files
COPY package*.json ./
COPY yarn.lock ./

# Install dependencies
RUN yarn install --frozen-lockfile

# Copy application files
COPY . .

# Expose ports
EXPOSE 3000

# Start development server
CMD ["yarn", "start"]