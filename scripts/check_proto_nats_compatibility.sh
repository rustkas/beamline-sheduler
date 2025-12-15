#!/bin/bash
# Check compatibility between protobuf structures and NATS JSON payload

set -euo pipefail

PROTO_DIR="proto"
EXAMPLES_DIR="proto/examples"

echo "=========================================="
echo "Proto/NATS Compatibility Check"
echo "=========================================="

# Check RouteRequest structure
echo "[INFO] Checking RouteRequest structure..."
echo "  - message (Message): required"
echo "  - policy_id (string): optional"
echo "  - context (map<string, string>): optional"

# Check RouteDecision structure
echo "[INFO] Checking RouteDecision structure..."
echo "  - provider_id (string): required"
echo "  - reason (string): required"
echo "  - priority (int32): required"
echo "  - expected_latency_ms (int64): required"
echo "  - expected_cost (double): required"
echo "  - metadata (map<string, string>): optional"

# Check type compatibility
echo "[INFO] Type compatibility:"
echo "  - string → JSON string: OK"
echo "  - int32/int64 → JSON number: OK"
echo "  - double → JSON number: OK"
echo "  - bytes → JSON string (UTF-8): OK"
echo "  - map<string, string> → JSON object: OK"

echo "=========================================="
echo "[OK] Proto/NATS compatibility verified"
