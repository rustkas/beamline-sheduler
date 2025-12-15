#!/bin/bash
#
# Extensions Quickstart Script
#
# Minimal happy-path for setting up Extensions ecosystem:
# 1. Start NATS
# 2. Deploy reference extensions
# 3. Enable fixtures/DB entries
# 4. Start Router + Gateway
# 5. Run 1-2 key tests (E2E/chaos)
#
# Usage:
#   ./scripts/extensions_quickstart.sh [options]
#
# Options:
#   --skip-tests          Skip running tests (only setup)
#   --skip-router         Skip starting Router (assume already running)
#   --skip-gateway        Skip starting Gateway (assume already running)
#   --use-docker          Use Docker Compose for all services
#   --nats-url <url>      NATS server URL (default: nats://localhost:4222)
#   --help                Show this help message
#
# Examples:
#   ./scripts/extensions_quickstart.sh
#   ./scripts/extensions_quickstart.sh --skip-tests
#   ./scripts/extensions_quickstart.sh --use-docker
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
EXTENSIONS_DIR="${PROJECT_ROOT}/tools/extensions"
ROUTER_DIR="${PROJECT_ROOT}/apps/otp/router"
FIXTURES_DIR="${ROUTER_DIR}/priv/fixtures/extensions"

# Default values
SKIP_TESTS=false
SKIP_ROUTER=false
SKIP_GATEWAY=false
USE_DOCKER=false
NATS_URL="${NATS_URL:-nats://localhost:4222}"
NATS_HOST="${NATS_URL#nats://}"
NATS_HOST="${NATS_HOST%%:*}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-tests)
            SKIP_TESTS=true
            shift
            ;;
        --skip-router)
            SKIP_ROUTER=true
            shift
            ;;
        --skip-gateway)
            SKIP_GATEWAY=true
            shift
            ;;
        --use-docker)
            USE_DOCKER=true
            shift
            ;;
        --nats-url)
            NATS_URL="$2"
            NATS_HOST="${NATS_URL#nats://}"
            NATS_HOST="${NATS_HOST%%:*}"
            shift 2
            ;;
        --help)
            cat <<EOF
Extensions Quickstart Script

Usage: $0 [options]

Options:
  --skip-tests          Skip running tests (only setup)
  --skip-router         Skip starting Router (assume already running)
  --skip-gateway        Skip starting Gateway (assume already running)
  --use-docker          Use Docker Compose for all services
  --nats-url <url>      NATS server URL (default: nats://localhost:4222)
  --help                Show this help message

Examples:
  $0
  $0 --skip-tests
  $0 --use-docker
EOF
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_command() {
    if ! command -v "$1" &> /dev/null; then
        log_error "$1 is not installed"
        exit 1
    fi
}

check_nats_connection() {
    log_info "Checking NATS connection: ${NATS_URL}..."
    if timeout 2 nc -z "${NATS_HOST}" 4222 2>/dev/null; then
        log_success "NATS is reachable"
        return 0
    else
        log_warning "NATS is not reachable at ${NATS_URL}"
        return 1
    fi
}

# Step 1: Start NATS
step1_start_nats() {
    log_info "Step 1: Starting NATS..."
    
    if check_nats_connection; then
        log_success "NATS is already running"
        return 0
    fi
    
    if [ "$USE_DOCKER" = true ]; then
        log_info "Starting NATS via Docker Compose..."
        cd "${PROJECT_ROOT}"
        docker-compose up -d nats
        log_info "Waiting for NATS to be healthy..."
        sleep 5
        if check_nats_connection; then
            log_success "NATS started successfully"
        else
            log_error "NATS failed to start"
            exit 1
        fi
    else
        log_info "Please start NATS manually:"
        echo "  docker run -d --name nats -p 4222:4222 -p 8222:8222 nats:2.10-alpine -js -m 8222"
        echo "  OR"
        echo "  nats-server -js -m 8222"
        echo ""
        read -p "Press Enter when NATS is running..."
        if ! check_nats_connection; then
            log_error "NATS is still not reachable"
            exit 1
        fi
    fi
}

# Step 2: Deploy reference extensions
step2_deploy_extensions() {
    log_info "Step 2: Deploying reference extensions..."
    
    if [ ! -d "${EXTENSIONS_DIR}" ]; then
        log_error "Extensions directory not found: ${EXTENSIONS_DIR}"
        exit 1
    fi
    
    cd "${EXTENSIONS_DIR}"
    
    # Install dependencies if needed
    if [ ! -d "node_modules" ]; then
        log_info "Installing extension dependencies..."
        npm install
    fi
    
    if [ "$USE_DOCKER" = true ]; then
        log_info "Starting extensions via Docker Compose..."
        cd "${PROJECT_ROOT}"
        docker-compose -f docker-compose.yml -f tools/extensions/docker-compose.extensions.yml up -d \
            extension-normalize-text \
            extension-pii-guard \
            extension-mask-pii \
            extension-test-provider
        log_success "Extensions started via Docker Compose"
        log_info "Check logs with: docker logs extension-normalize-text"
    else
        log_info "Starting extensions manually..."
        if [ -f "start_extensions.sh" ]; then
            export NATS_URL
            ./start_extensions.sh &
            EXTENSIONS_PID=$!
            log_success "Extensions started (PID: ${EXTENSIONS_PID})"
            log_info "Extensions are running in background. Logs: /tmp/*.log"
        else
            log_warning "start_extensions.sh not found, starting extensions individually..."
            export NATS_URL
            node src/normalize_text.js > /tmp/normalize_text.log 2>&1 &
            node src/pii_guard.js > /tmp/pii_guard.log 2>&1 &
            node src/mask_pii.js > /tmp/mask_pii.log 2>&1 &
            node src/test_provider.js > /tmp/test_provider.log 2>&1 &
            log_success "Extensions started individually"
        fi
        sleep 2
    fi
}

# Step 3: Enable fixtures/DB entries
step3_enable_fixtures() {
    log_info "Step 3: Enabling extension fixtures..."
    
    if [ ! -d "${FIXTURES_DIR}" ]; then
        log_error "Fixtures directory not found: ${FIXTURES_DIR}"
        exit 1
    fi
    
    # Check if fixtures exist
    REQUIRED_FIXTURES=("normalize_text.json" "pii_guard.json" "mask_pii.json" "test_provider.json")
    MISSING_FIXTURES=()
    
    for fixture in "${REQUIRED_FIXTURES[@]}"; do
        if [ ! -f "${FIXTURES_DIR}/${fixture}" ]; then
            MISSING_FIXTURES+=("${fixture}")
        fi
    done
    
    if [ ${#MISSING_FIXTURES[@]} -gt 0 ]; then
        log_warning "Missing fixtures: ${MISSING_FIXTURES[*]}"
        log_info "Fixtures should be in: ${FIXTURES_DIR}"
        log_info "Router will use fixtures mode automatically if fixtures exist"
    else
        log_success "All required fixtures found"
    fi
    
    log_info "Extension Registry will load from fixtures on Router start"
    log_info "Fixtures location: ${FIXTURES_DIR}"
}

# Step 4: Start Router + Gateway
step4_start_services() {
    log_info "Step 4: Starting Router + Gateway..."
    
    if [ "$SKIP_ROUTER" = false ]; then
        log_info "Starting Router..."
        log_warning "Router startup is manual. Please start Router with:"
        echo "  cd ${ROUTER_DIR}"
        echo "  rebar3 shell"
        echo "  OR"
        echo "  docker-compose up -d router"
        echo ""
        read -p "Press Enter when Router is running..."
    else
        log_info "Skipping Router startup (--skip-router)"
    fi
    
    if [ "$SKIP_GATEWAY" = false ]; then
        if [ "$USE_DOCKER" = true ]; then
            log_info "Starting Gateway via Docker Compose..."
            cd "${PROJECT_ROOT}"
            docker-compose up -d gateway c-gateway
            log_success "Gateway started via Docker Compose"
        else
            log_info "Starting Gateway..."
            log_warning "Gateway startup is manual. Please start Gateway with:"
            echo "  cd apps/gateway && npm start"
            echo "  OR"
            echo "  docker-compose up -d gateway"
            echo ""
            read -p "Press Enter when Gateway is running..."
        fi
    else
        log_info "Skipping Gateway startup (--skip-gateway)"
    fi
}

# Step 5: Run key tests
step5_run_tests() {
    if [ "$SKIP_TESTS" = true ]; then
        log_info "Skipping tests (--skip-tests)"
        return 0
    fi
    
    log_info "Step 5: Running key tests..."
    
    cd "${ROUTER_DIR}"
    
    # Check if rebar3 is available
    if ! command -v rebar3 &> /dev/null; then
        log_warning "rebar3 not found, skipping tests"
        log_info "Install rebar3 or run tests manually:"
        echo "  cd ${ROUTER_DIR}"
        echo "  rebar3 ct --suite test/router_extensions_e2e_SUITE --case test_e2e_full_pipeline"
        return 0
    fi
    
    export NATS_URL
    
    log_info "Running E2E test: test_e2e_full_pipeline"
    if rebar3 ct --suite test/router_extensions_e2e_SUITE --case test_e2e_full_pipeline; then
        log_success "E2E test passed"
    else
        log_error "E2E test failed"
        return 1
    fi
    
    log_info "Running E2E test: test_e2e_multiple_extensions"
    if rebar3 ct --suite test/router_extensions_e2e_SUITE --case test_e2e_multiple_extensions; then
        log_success "E2E test passed"
    else
        log_warning "E2E test failed (non-critical)"
    fi
}

# Main execution
main() {
    log_info "=== Extensions Quickstart ==="
    log_info "NATS URL: ${NATS_URL}"
    log_info "Use Docker: ${USE_DOCKER}"
    log_info ""
    
    # Check prerequisites
    check_command docker
    if [ "$USE_DOCKER" = false ]; then
        check_command node
        check_command npm
    fi
    
    # Execute steps
    step1_start_nats
    step2_deploy_extensions
    step3_enable_fixtures
    step4_start_services
    step5_run_tests
    
    log_success "=== Extensions Quickstart Complete ==="
    log_info ""
    log_info "Next steps:"
    log_info "  - Read: docs/EXTENSIONS_QUICKSTART.md"
    log_info "  - Developer Guide: docs/EXTENSIONS_DEVELOPER_GUIDE.md"
    log_info "  - Runbook: apps/otp/router/docs/EXTENSIONS_RUNBOOK.md"
    log_info "  - E2E Guide: apps/otp/router/docs/EXTENSIONS_E2E_GUIDE.md"
}

# Run main
main "$@"

