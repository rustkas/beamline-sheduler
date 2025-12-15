#!/bin/bash
# Beamline Constructor - Service Startup Script
# This script starts all services in the correct order

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

echo "🚀 Starting Beamline Constructor services..."

cd "${PROJECT_ROOT}"

# Check if .env exists
if [ ! -f ".env" ]; then
    echo "❌ .env file not found. Please run scripts/setup-local.sh first"
    exit 1
fi

# Start services with docker-compose
echo "📦 Starting services with docker-compose..."
docker-compose up -d

# Wait for services to be healthy
echo "⏳ Waiting for services to be healthy..."
sleep 5

# Check service status
echo "📊 Service status:"
docker-compose ps

# Check health endpoints
echo ""
echo "🏥 Checking service health..."

# Check PostgreSQL
if docker-compose exec -T postgres pg_isready -U beamline >/dev/null 2>&1; then
    echo "✅ PostgreSQL is healthy"
else
    echo "⚠️  PostgreSQL health check failed"
fi

# Check NATS
if docker-compose exec -T nats wget --no-verbose --tries=1 --spider http://localhost:8222/healthz >/dev/null 2>&1; then
    echo "✅ NATS is healthy"
else
    echo "⚠️  NATS health check failed"
fi

# Check Gateway
max_attempts=30
attempt=0
while [ $attempt -lt $max_attempts ]; do
    if curl -f http://localhost:3000/health >/dev/null 2>&1; then
        echo "✅ Gateway is healthy"
        break
    fi
    attempt=$((attempt + 1))
    sleep 2
done

if [ $attempt -eq $max_attempts ]; then
    echo "⚠️  Gateway health check failed (may still be starting)"
fi

echo ""
echo "✅ Services started!"
echo ""
echo "📚 Useful commands:"
echo "   View logs:        docker-compose logs -f"
echo "   Stop services:    docker-compose down"
echo "   Restart service:  docker-compose restart <service-name>"
echo "   Service status:   docker-compose ps"

