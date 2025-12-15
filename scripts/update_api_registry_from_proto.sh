#!/bin/bash
# Update api-registry.md with real message names from proto

set -euo pipefail

API_REGISTRY="docs/ARCHITECTURE/api-registry.md"
PROTO_DIR="proto"

echo "=========================================="
echo "Updating API Registry with Proto Messages"
echo "=========================================="

# Extract real messages from proto
echo "[INFO] Extracting messages from proto files..."

# Flow messages
FLOW_MSGS=$(grep -E "^message " "$PROTO_DIR/beamline/flow/v1/flow.proto" | sed 's/message //;s/ {.*//' | sort | tr '\n' ',' | sed 's/,$//')

# Provider messages  
PROVIDER_MSGS=$(grep -E "^message " "$PROTO_DIR/beamline/provider/v1/provider.proto" | sed 's/message //;s/ {.*//' | sort | tr '\n' ',' | sed 's/,$//')

# Ingress messages (deprecated - not part of core components)
if [ -f "$PROTO_DIR/beamline/ingress/v1/ingress.proto" ]; then
    INGRESS_MSGS=$(grep -E "^message " "$PROTO_DIR/beamline/ingress/v1/ingress.proto" | sed 's/message //;s/ {.*//' | sort | tr '\n' ',' | sed 's/,$//')
else
    INGRESS_MSGS="(deprecated, proto file not found)"
fi

echo "[OK] Messages found:"
echo "  Flow (beamline.flow.v1): $FLOW_MSGS"
echo "  Provider (beamline.provider.v1): $PROVIDER_MSGS"
echo "  Ingress (beamline.ingress.v1): $INGRESS_MSGS (deprecated - not part of core components)"

echo ""
echo "[INFO] Proto files location:"
echo "  - proto/beamline/flow/v1/flow.proto"
echo "  - proto/beamline/provider/v1/provider.proto"
if [ -f "$PROTO_DIR/beamline/ingress/v1/ingress.proto" ]; then
    echo "  - proto/beamline/ingress/v1/ingress.proto (deprecated)"
fi
echo ""
echo "[INFO] See proto/README.md for detailed message structures and NATS mapping"

echo "=========================================="
echo "[OK] API Registry update information ready"
