#!/bin/bash
# Check Proto File Synchronization
# 
# Verifies that proto/beamline/flow/v1/flow.proto and 
# apps/otp/router/proto/beamline/flow/v1/flow.proto are identical.
#
# Note: If files are missing, this is expected for CP1 (proto restoration deferred to CP2-LC).
# See: apps/otp/router/docs/GENERATION.md
#
# Exit codes:
#   0 - Files are synchronized (identical) or both missing (expected for CP1)
#   1 - Files differ
#   2 - Error during comparison
#   3 - Only one file exists (inconsistency)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Paths
ROOT_PROTO="proto/beamline/flow/v1/flow.proto"
ROUTER_PROTO="apps/otp/router/proto/beamline/flow/v1/flow.proto"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

echo "=== Proto File Synchronization Check ==="
echo ""

# Check if files exist
ROOT_EXISTS=false
ROUTER_EXISTS=false

if [ -f "$ROOT_PROTO" ]; then
    ROOT_EXISTS=true
fi

if [ -f "$ROUTER_PROTO" ]; then
    ROUTER_EXISTS=true
fi

# Handle missing files (expected for CP1, proto restoration deferred to CP2-LC)
if [ "$ROOT_EXISTS" = false ] && [ "$ROUTER_EXISTS" = false ]; then
    echo -e "${YELLOW}⚠️  WARNING: Both proto files are missing (expected for CP1)${NC}"
    echo ""
    echo "Proto file restoration is deferred to CP2-LC."
    echo "See: apps/otp/router/docs/GENERATION.md"
    echo ""
    echo -e "${GREEN}✅ SUCCESS: No synchronization needed (files not yet restored)${NC}"
    exit 0
fi

# Handle inconsistency (only one file exists)
if [ "$ROOT_EXISTS" = false ] || [ "$ROUTER_EXISTS" = false ]; then
    if [ "$ROOT_EXISTS" = false ]; then
        echo -e "${RED}❌ ERROR: Root proto file not found: $ROOT_PROTO${NC}"
    fi
    if [ "$ROUTER_EXISTS" = false ]; then
        echo -e "${RED}❌ ERROR: Router proto file not found: $ROUTER_PROTO${NC}"
    fi
    echo ""
    echo "Inconsistency: Only one file exists. Both files should exist or both should be missing."
    exit 3
fi

# Calculate MD5 checksums
ROOT_MD5=$(md5sum "$ROOT_PROTO" | cut -d' ' -f1)
ROUTER_MD5=$(md5sum "$ROUTER_PROTO" | cut -d' ' -f1)

echo "Root proto MD5:    $ROOT_MD5"
echo "Router proto MD5:  $ROUTER_MD5"
echo ""

# Compare checksums
if [ "$ROOT_MD5" = "$ROUTER_MD5" ]; then
    echo -e "${GREEN}✅ SUCCESS: Proto files are synchronized (identical)${NC}"
    exit 0
else
    echo -e "${RED}❌ ERROR: Proto files are NOT synchronized (differ)${NC}"
    echo ""
    echo "Files differ. Run the following to synchronize:"
    echo "  cp $ROUTER_PROTO $ROOT_PROTO"
    echo ""
    echo "Or if root proto is the source of truth:"
    echo "  cp $ROOT_PROTO $ROUTER_PROTO"
    echo ""
    echo "Diff:"
    diff -u "$ROOT_PROTO" "$ROUTER_PROTO" || true
    exit 1
fi
