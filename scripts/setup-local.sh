#!/bin/bash
# Beamline Constructor - Local Environment Setup Script
# This script sets up the local development environment

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

echo "🚀 Setting up Beamline Constructor local environment..."

# Check prerequisites
echo "📋 Checking prerequisites..."

command -v docker >/dev/null 2>&1 || { echo "❌ Docker is required but not installed. Aborting." >&2; exit 1; }
command -v docker-compose >/dev/null 2>&1 || { echo "❌ docker-compose is required but not installed. Aborting." >&2; exit 1; }

# Check Docker is running
if ! docker info >/dev/null 2>&1; then
    echo "❌ Docker daemon is not running. Please start Docker and try again." >&2
    exit 1
fi

echo "✅ Prerequisites check passed"

# Create .env file if it doesn't exist
if [ ! -f "${PROJECT_ROOT}/.env" ]; then
    echo "📝 Creating .env file from template..."
    if [ -f "${PROJECT_ROOT}/.env.example" ]; then
        cp "${PROJECT_ROOT}/.env.example" "${PROJECT_ROOT}/.env"
        echo "⚠️  Please edit .env file and set your configuration values"
        echo "⚠️  IMPORTANT: Replace all placeholders (YOUR_*_HERE) with actual values"
    else
        echo "⚠️  .env.example not found, creating basic .env file..."
        cat > "${PROJECT_ROOT}/.env" <<EOF
# Database Configuration
DB_NAME=beamline
DB_USER=beamline
DB_PASSWORD=dev_password_change_in_production
DB_PORT=5432

# NATS Configuration
NATS_URL=nats://localhost:4222
NATS_PORT=4222

# Gateway Configuration
GATEWAY_PORT=3000
NODE_ENV=development

# Router Configuration
GRPC_PORT=9000
DB_POOL_SIZE=10

# Security (placeholders - replace in production)
HMAC_SECRET=YOUR_HMAC_SECRET_HERE
EOF
    fi
else
    echo "✅ .env file already exists"
fi

# Create necessary directories
echo "📁 Creating necessary directories..."
mkdir -p "${PROJECT_ROOT}/logs"
mkdir -p "${PROJECT_ROOT}/data/postgres"

# Initialize database
echo "🗄️  Initializing database..."
cd "${PROJECT_ROOT}"
if [ -f "scripts/init-db.sh" ]; then
    bash scripts/init-db.sh
else
    echo "⚠️  init-db.sh not found, database will be initialized on first docker-compose up"
fi

echo "✅ Local environment setup complete!"
echo ""
echo "📚 Next steps:"
echo "   1. Review and update .env file with your configuration"
echo "   2. Run: docker-compose up -d"
echo "   3. Check service health: docker-compose ps"
echo "   4. View logs: docker-compose logs -f"
echo ""
echo "📖 For more information, see docs/LOCAL_SETUP.md"


