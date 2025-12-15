#!/bin/bash
# Beamline Constructor - Environment Validation Script
# This script validates the environment configuration

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

echo "🔍 Validating Beamline Constructor environment..."

ERRORS=0
WARNINGS=0

# Check .env file exists (with fallback to config/env/.env.dev in development)
ENV_FILE="${PROJECT_ROOT}/.env"
if [ ! -f "${ENV_FILE}" ]; then
    if [ "${NODE_ENV:-development}" = "development" ] && [ -f "${PROJECT_ROOT}/config/env/.env.dev" ]; then
        echo "⚠️  .env file not found, using config/env/.env.dev (development mode)"
        ENV_FILE="${PROJECT_ROOT}/config/env/.env.dev"
    else
        echo "❌ .env file not found"
        echo "   Run: scripts/setup-local.sh"
        echo "   Or use: config/env/.env.dev for local development"
        ERRORS=$((ERRORS + 1))
        exit 1
    fi
fi

# Load environment variables
set -a
source "${ENV_FILE}"
set +a

# Check required variables
REQUIRED_VARS=(
    "DB_NAME"
    "DB_USER"
    "DB_PASSWORD"
    "NATS_URL"
    "GATEWAY_PORT"
)

# Check BEAMLINE_HMAC_SECRET (required in production, optional in dev)
echo ""
echo "🔐 Checking BEAMLINE_HMAC_SECRET..."
if [ -z "${BEAMLINE_HMAC_SECRET:-}" ]; then
    if [ "${NODE_ENV:-development}" = "production" ] || [ "${CI:-false}" = "true" ] || [ "${GITHUB_ACTIONS:-false}" = "true" ]; then
        echo "❌ BEAMLINE_HMAC_SECRET is not set (REQUIRED in production/CI)"
        echo "   Please set BEAMLINE_HMAC_SECRET in your CI/CD secrets"
        ERRORS=$((ERRORS + 1))
    else
        echo "⚠️  BEAMLINE_HMAC_SECRET is not set (optional in development)"
        echo "   For local development, use config/env/.env.dev or set BEAMLINE_HMAC_SECRET"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    # Validate secret length
    SECRET_LENGTH=${#BEAMLINE_HMAC_SECRET}
    if [ $SECRET_LENGTH -lt 16 ]; then
        if [ "${NODE_ENV:-development}" = "production" ] || [ "${CI:-false}" = "true" ]; then
            echo "❌ BEAMLINE_HMAC_SECRET is too short (minimum 16 characters, got ${SECRET_LENGTH})"
            ERRORS=$((ERRORS + 1))
        else
            echo "⚠️  BEAMLINE_HMAC_SECRET is too short (minimum 16 characters, got ${SECRET_LENGTH})"
            WARNINGS=$((WARNINGS + 1))
        fi
    elif [ $SECRET_LENGTH -lt 64 ]; then
        echo "⚠️  BEAMLINE_HMAC_SECRET is shorter than recommended (recommended 64 characters, got ${SECRET_LENGTH})"
        WARNINGS=$((WARNINGS + 1))
    else
        echo "✅ BEAMLINE_HMAC_SECRET is set (length: ${SECRET_LENGTH} characters)"
    fi
fi

echo "📋 Checking required environment variables..."
for var in "${REQUIRED_VARS[@]}"; do
    if [ -z "${!var:-}" ]; then
        echo "❌ ${var} is not set"
        ERRORS=$((ERRORS + 1))
    else
        echo "✅ ${var} is set"
    fi
done

# Check for placeholder values (check both .env and config/env/.env.dev)
echo ""
echo "🔍 Checking for placeholder values..."
PLACEHOLDERS=(
    "YOUR_DB_PASSWORD_HERE"
    "YOUR_HMAC_SECRET_HERE"
    "YOUR_API_KEY_HERE"
    "YOUR_BEAMLINE_HMAC_SECRET_HERE"
)

ENV_FILES_TO_CHECK=("${ENV_FILE}")
if [ -f "${PROJECT_ROOT}/config/env/.env.dev" ] && [ "${ENV_FILE}" != "${PROJECT_ROOT}/config/env/.env.dev" ]; then
    ENV_FILES_TO_CHECK+=("${PROJECT_ROOT}/config/env/.env.dev")
fi

for env_file in "${ENV_FILES_TO_CHECK[@]}"; do
    for placeholder in "${PLACEHOLDERS[@]}"; do
        if grep -q "${placeholder}" "${env_file}" 2>/dev/null; then
            echo "⚠️  Found placeholder in ${env_file}: ${placeholder}"
            echo "   Please replace with actual value"
            WARNINGS=$((WARNINGS + 1))
        fi
    done
done

# Check Docker
echo ""
echo "🐳 Checking Docker..."
if command -v docker >/dev/null 2>&1; then
    if docker info >/dev/null 2>&1; then
        echo "✅ Docker is running"
    else
        echo "❌ Docker daemon is not running"
        ERRORS=$((ERRORS + 1))
    fi
else
    echo "❌ Docker is not installed"
    ERRORS=$((ERRORS + 1))
fi

# Check docker-compose (support both docker-compose and docker compose plugin)
DOCKER_COMPOSE_AVAILABLE=false
if command -v docker-compose >/dev/null 2>&1; then
    echo "✅ docker-compose is installed"
    DOCKER_COMPOSE_AVAILABLE=true
elif docker compose version >/dev/null 2>&1; then
    echo "✅ docker compose plugin is available"
    DOCKER_COMPOSE_AVAILABLE=true
else
    echo "❌ docker-compose is not installed (neither binary nor plugin)"
    ERRORS=$((ERRORS + 1))
fi

# Check configuration files
echo ""
echo "📁 Checking configuration files..."

if [ -f "${PROJECT_ROOT}/docker-compose.yml" ]; then
    echo "✅ docker-compose.yml exists"
else
    echo "❌ docker-compose.yml not found"
    ERRORS=$((ERRORS + 1))
fi

if [ -f "${PROJECT_ROOT}/sql/000_init.sql" ]; then
    echo "✅ Database schema file exists"
else
    echo "⚠️  Database schema file not found"
    WARNINGS=$((WARNINGS + 1))
fi

# Summary
echo ""
echo "📊 Validation Summary:"
echo "   Errors:   ${ERRORS}"
echo "   Warnings: ${WARNINGS}"

if [ $ERRORS -eq 0 ]; then
    echo ""
    echo "✅ Environment validation passed!"
    if [ $WARNINGS -gt 0 ]; then
        echo "⚠️  Please address ${WARNINGS} warning(s)"
    fi
    exit 0
else
    echo ""
    echo "❌ Environment validation failed with ${ERRORS} error(s)"
    exit 1
fi

