#!/bin/bash
# Beamline Constructor - Database Initialization Script
# This script initializes the PostgreSQL database

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

echo "🗄️  Initializing Beamline Constructor database..."

# Load environment variables
if [ -f "${PROJECT_ROOT}/.env" ]; then
    set -a
    source "${PROJECT_ROOT}/.env"
    set +a
fi

DB_NAME="${DB_NAME:-beamline}"
DB_USER="${DB_USER:-beamline}"
DB_PASSWORD="${DB_PASSWORD:-dev_password_change_in_production}"
DB_PORT="${DB_PORT:-5432}"
DB_HOST="${DB_HOST:-localhost}"

# Wait for PostgreSQL to be ready
echo "⏳ Waiting for PostgreSQL to be ready..."
max_attempts=30
attempt=0

while [ $attempt -lt $max_attempts ]; do
    if PGPASSWORD="${DB_PASSWORD}" psql -h "${DB_HOST}" -p "${DB_PORT}" -U "${DB_USER}" -d postgres -c "SELECT 1" >/dev/null 2>&1; then
        echo "✅ PostgreSQL is ready"
        break
    fi
    attempt=$((attempt + 1))
    echo "   Attempt ${attempt}/${max_attempts}..."
    sleep 2
done

if [ $attempt -eq $max_attempts ]; then
    echo "❌ PostgreSQL is not ready after ${max_attempts} attempts"
    echo "   Please ensure PostgreSQL is running and accessible"
    exit 1
fi

# Check if database exists
if PGPASSWORD="${DB_PASSWORD}" psql -h "${DB_HOST}" -p "${DB_PORT}" -U "${DB_USER}" -d postgres -tc "SELECT 1 FROM pg_database WHERE datname = '${DB_NAME}'" | grep -q 1; then
    echo "✅ Database '${DB_NAME}' already exists"
else
    echo "📝 Creating database '${DB_NAME}'..."
    PGPASSWORD="${DB_PASSWORD}" psql -h "${DB_HOST}" -p "${DB_PORT}" -U "${DB_USER}" -d postgres -c "CREATE DATABASE ${DB_NAME};"
    echo "✅ Database created"
fi

# Run initialization SQL
if [ -f "${PROJECT_ROOT}/sql/000_init.sql" ]; then
    echo "📝 Running initialization SQL..."
    PGPASSWORD="${DB_PASSWORD}" psql -h "${DB_HOST}" -p "${DB_PORT}" -U "${DB_USER}" -d "${DB_NAME}" -f "${PROJECT_ROOT}/sql/000_init.sql"
    echo "✅ Database schema initialized"
else
    echo "⚠️  sql/000_init.sql not found, skipping schema initialization"
fi

echo "✅ Database initialization complete!"

