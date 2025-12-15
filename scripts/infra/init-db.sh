#!/bin/bash
# Database Initialization Script
# Purpose: Initialize PostgreSQL database with schema and test policies
# Usage: Run inside postgres container or via docker-compose exec

set -euo pipefail

DB_NAME="${DB_NAME:-beamline}"
DB_USER="${DB_USER:-beamline}"

echo "Initializing database: $DB_NAME"

# Wait for PostgreSQL to be ready
until pg_isready -U "$DB_USER" -d "$DB_NAME"; do
  echo "Waiting for PostgreSQL to be ready..."
  sleep 1
done

# Run initialization SQL
if [ -f "/docker-entrypoint-initdb.d/000_init.sql" ]; then
  echo "Running schema initialization..."
  psql -U "$DB_USER" -d "$DB_NAME" -f /docker-entrypoint-initdb.d/000_init.sql
fi

# Run test policies initialization
if [ -f "/docker-entrypoint-initdb.d/001_test_policies.sql" ]; then
  echo "Running test policies initialization..."
  psql -U "$DB_USER" -d "$DB_NAME" -f /docker-entrypoint-initdb.d/001_test_policies.sql
fi

echo "Database initialization complete"

