#!/bin/bash
# NATS Chaos Script
# Intentionally breaks NATS during tests to verify Router resilience
#
# Usage:
#   ./scripts/nats_chaos.sh [MODE] [INTENSITY] [DURATION]
#
# Modes:
#   - mild: Single NATS restart (default)
#   - moderate: Multiple restarts during test
#   - hard: Randomized failures
#
# Intensity:
#   - low: Short outages (5-10 seconds)
#   - medium: Medium outages (20-30 seconds)
#   - high: Long outages (30-60 seconds)
#
# Duration: Total chaos duration in seconds (default: 300)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Configuration
CHAOS_MODE="${1:-mild}"
CHAOS_INTENSITY="${2:-low}"
CHAOS_DURATION="${3:-300}"
NATS_CONTAINER="${NATS_CONTAINER:-nats}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[CHAOS]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
}

log_warn() {
    echo -e "${YELLOW}[CHAOS]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
}

log_error() {
    echo -e "${RED}[CHAOS]${NC} $(date '+%Y-%m-%d %H:%M:%S') $1"
}

# Check if NATS container exists
check_nats() {
    if ! docker ps -a --format '{{.Names}}' | grep -q "^${NATS_CONTAINER}$"; then
        log_error "NATS container '${NATS_CONTAINER}' not found"
        return 1
    fi
    return 0
}

# Stop NATS
stop_nats() {
    log_warn "Stopping NATS container '${NATS_CONTAINER}'..."
    docker stop "${NATS_CONTAINER}" >/dev/null 2>&1 || true
    log_warn "NATS stopped"
}

# Start NATS
start_nats() {
    log_info "Starting NATS container '${NATS_CONTAINER}'..."
    docker start "${NATS_CONTAINER}" >/dev/null 2>&1 || true
    # Wait for NATS to be ready
    sleep 2
    log_info "NATS started"
}

# Check if NATS is running
is_nats_running() {
    docker ps --format '{{.Names}}' | grep -q "^${NATS_CONTAINER}$"
}

# Get outage duration based on intensity
get_outage_duration() {
    case "$CHAOS_INTENSITY" in
        low)
            echo $((5 + RANDOM % 6))  # 5-10 seconds
            ;;
        medium)
            echo $((20 + RANDOM % 11))  # 20-30 seconds
            ;;
        high)
            echo $((30 + RANDOM % 31))  # 30-60 seconds
            ;;
        *)
            echo 10
            ;;
    esac
}

# Get recovery duration (time between outages)
get_recovery_duration() {
    case "$CHAOS_INTENSITY" in
        low)
            echo $((30 + RANDOM % 21))  # 30-50 seconds
            ;;
        medium)
            echo $((20 + RANDOM % 11))  # 20-30 seconds
            ;;
        high)
            echo $((10 + RANDOM % 11))  # 10-20 seconds
            ;;
        *)
            echo 30
            ;;
    esac
}

# Mild Chaos: Single restart
chaos_mild() {
    log_info "Starting Mild Chaos mode (single restart)"
    
    check_nats || return 1
    
    # Wait a bit before first outage
    sleep 10
    
    # Single outage
    outage_duration=$(get_outage_duration)
    log_warn "Mild Chaos: Stopping NATS for ${outage_duration} seconds"
    stop_nats
    sleep "$outage_duration"
    start_nats
    
    log_info "Mild Chaos: NATS restarted, waiting for recovery"
    sleep 10
    
    log_info "Mild Chaos complete"
}

# Moderate Chaos: Multiple restarts
chaos_moderate() {
    log_info "Starting Moderate Chaos mode (multiple restarts)"
    
    check_nats || return 1
    
    start_time=$(date +%s)
    iteration=0
    
    while true; do
        current_time=$(date +%s)
        elapsed=$((current_time - start_time))
        
        if [ $elapsed -ge "$CHAOS_DURATION" ]; then
            break
        fi
        
        iteration=$((iteration + 1))
        log_warn "Moderate Chaos: Iteration $iteration"
        
        # Wait before outage
        recovery_duration=$(get_recovery_duration)
        log_info "Moderate Chaos: Running normally for ${recovery_duration} seconds"
        sleep "$recovery_duration"
        
        # Outage
        outage_duration=$(get_outage_duration)
        log_warn "Moderate Chaos: Stopping NATS for ${outage_duration} seconds"
        stop_nats
        sleep "$outage_duration"
        start_nats
        
        log_info "Moderate Chaos: NATS restarted, waiting for recovery"
        sleep 5
    done
    
    log_info "Moderate Chaos complete (${iteration} iterations)"
}

# Hard Chaos: Randomized failures
chaos_hard() {
    log_info "Starting Hard Chaos mode (randomized failures)"
    
    check_nats || return 1
    
    start_time=$(date +%s)
    iteration=0
    
    while true; do
        current_time=$(date +%s)
        elapsed=$((current_time - start_time))
        
        if [ $elapsed -ge "$CHAOS_DURATION" ]; then
            break
        fi
        
        iteration=$((iteration + 1))
        
        # Random decision: outage or normal operation
        decision=$((RANDOM % 3))
        
        case $decision in
            0)
                # Outage
                outage_duration=$(get_outage_duration)
                log_warn "Hard Chaos: Random outage for ${outage_duration} seconds"
                stop_nats
                sleep "$outage_duration"
                start_nats
                sleep 5
                ;;
            1)
                # Short freeze (kill and immediate restart)
                log_warn "Hard Chaos: Short freeze (kill + restart)"
                stop_nats
                sleep 2
                start_nats
                sleep 3
                ;;
            2)
                # Normal operation
                normal_duration=$(get_recovery_duration)
                log_info "Hard Chaos: Normal operation for ${normal_duration} seconds"
                sleep "$normal_duration"
                ;;
        esac
    done
    
    log_info "Hard Chaos complete (${iteration} iterations)"
}

# Main execution
main() {
    log_info "NATS Chaos Script"
    log_info "Mode: $CHAOS_MODE"
    log_info "Intensity: $CHAOS_INTENSITY"
    log_info "Duration: ${CHAOS_DURATION}s"
    log_info "Container: $NATS_CONTAINER"
    echo ""
    
    # Verify NATS container exists
    if ! check_nats; then
        log_error "NATS container not found. Exiting."
        exit 1
    fi
    
    # Ensure NATS is running at start
    if ! is_nats_running; then
        log_warn "NATS not running, starting..."
        start_nats
    fi
    
    # Run chaos based on mode
    case "$CHAOS_MODE" in
        mild)
            chaos_mild
            ;;
        moderate)
            chaos_moderate
            ;;
        hard)
            chaos_hard
            ;;
        *)
            log_error "Unknown chaos mode: $CHAOS_MODE"
            log_info "Available modes: mild, moderate, hard"
            exit 1
            ;;
    esac
    
    # Ensure NATS is running at end
    if ! is_nats_running; then
        log_warn "NATS not running at end, starting..."
        start_nats
    fi
    
    log_info "Chaos script complete"
}

# Handle signals
trap 'log_warn "Chaos script interrupted, ensuring NATS is running..."; start_nats; exit 0' INT TERM

# Run main
main

