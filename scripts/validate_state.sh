#!/bin/bash
# CI check for validating .trae/state.json and .trae/history.json
# Checks JSON-Schema validation, artifact checksums, and HMAC chain
#
# Requirements:
# - python3 with jsonschema installed (pip install jsonschema)
# - Linux/WSL environment
# - BEAMLINE_HMAC_SECRET environment variable (optional, default 'beamline-secret-key-v1')

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Get secret from environment
# In production environment BEAMLINE_HMAC_SECRET must be set

# Production environment detection
# Production mode is activated if one of the following variables is set:
# - CI=true (common variable for CI/CD systems)
# - PRODUCTION=true (explicit production environment indication)
# - GITHUB_ACTIONS=true (automatically set in GitHub Actions)
# - DRONE=true (automatically set in Drone CI)

IS_PRODUCTION=false

if [ "${CI:-false}" = "true" ]; then
    IS_PRODUCTION=true
    echo "[INFO] CI environment detected (CI=true)"
elif [ "${PRODUCTION:-false}" = "true" ]; then
    IS_PRODUCTION=true
    echo "[INFO] Production environment detected (PRODUCTION=true)"
elif [ "${GITHUB_ACTIONS:-false}" = "true" ]; then
    IS_PRODUCTION=true
    echo "[INFO] GitHub Actions environment detected (GITHUB_ACTIONS=true)"
elif [ "${DRONE:-false}" = "true" ]; then
    IS_PRODUCTION=true
    echo "[INFO] Drone CI environment detected (DRONE=true)"
fi

# Check if secret is available
if [ -z "$BEAMLINE_HMAC_SECRET" ]; then
    # In production environment secret is required
    if [ "$IS_PRODUCTION" = "true" ]; then
        echo "[FAIL] BEAMLINE_HMAC_SECRET environment variable is not set"
        echo "[FAIL] This is required in CI/CD and production environments"
        echo "[FAIL] Current environment: Production/CI detected"
        echo "[FAIL] Please set BEAMLINE_HMAC_SECRET in your CI/CD secrets"
        exit 1
    else
        # In development can use default value
        echo "[WARN] BEAMLINE_HMAC_SECRET not set, using default (development only)"
        echo "[WARN] For production, set BEAMLINE_HMAC_SECRET and CI=true or PRODUCTION=true"
        HMAC_SECRET="beamline-secret-key-v1"
    fi
else
    # Secret is set
    HMAC_SECRET="$BEAMLINE_HMAC_SECRET"
    if [ "$IS_PRODUCTION" = "true" ]; then
        echo "[INFO] Using HMAC secret from: BEAMLINE_HMAC_SECRET (production mode)"
    else
        echo "[INFO] Using HMAC secret from: BEAMLINE_HMAC_SECRET"
    fi
fi

# Export for Python scripts
export BEAMLINE_HMAC_SECRET="$HMAC_SECRET"

echo "[INFO] Validating .trae/state.json and .trae/history.json..."

# Check 0: No-Drift flag
echo "[0/5] Checking no_drift flag..."
if ! python3 -c "
import json
state = json.load(open('.trae/state.json'))
if not state.get('no_drift', False):
    print('[FAIL] no_drift flag is not set to true')
    exit(1)
print('[OK] no_drift flag is set')
" 2>/dev/null; then
    echo "[FAIL] no_drift flag check failed"
    exit 1
fi

# Check 1: JSON validity
echo "[1/5] Checking JSON validity..."
if ! python3 -m json.tool .trae/state.json > /dev/null 2>&1; then
    echo "[FAIL] .trae/state.json is not valid JSON"
    exit 1
fi
echo "[OK] JSON is valid"

# Check 2: JSON-Schema validation
echo "[2/5] Validating against STATE.schema.json..."
if ! python3 -c "
import json, jsonschema, sys
try:
    schema = json.load(open('docs/STATE.schema.json'))
    state = json.load(open('.trae/state.json'))
    jsonschema.validate(state, schema)
    print('[OK] Schema validation passed')
except jsonschema.ValidationError as e:
    print(f'[FAIL] Schema validation failed: {e.message}')
    print(f'  Path: {\".\".join(str(p) for p in e.path)}')
    sys.exit(1)
except Exception as e:
    print(f'[FAIL] Error: {e}')
    sys.exit(1)
"; then
    exit 1
fi

# Check 3: Artifact checksums
echo "[3/5] Verifying artifact checksums..."
FAILED=0

while IFS= read -r artifact_path; do
    if [ -z "$artifact_path" ]; then
        continue
    fi
    
    # Get expected hash from state.json
    expected_hash=$(python3 -c "
import json, sys
state = json.load(open('.trae/state.json'))
for artifact in state.get('artifact_checksums', []):
    if artifact['path'] == '$artifact_path':
        print(artifact['hash'])
        sys.exit(0)
sys.exit(1)
" 2>/dev/null)
    
    if [ -z "$expected_hash" ]; then
        echo "[WARN] Artifact $artifact_path not found in state.json"
        continue
    fi
    
    # Calculate actual hash
    if [ ! -f "$artifact_path" ]; then
        echo "[FAIL] Artifact $artifact_path does not exist"
        FAILED=1
        continue
    fi
    
    actual_hash=$(sha256sum "$artifact_path" | cut -d' ' -f1)
    
    if [ "$expected_hash" != "$actual_hash" ]; then
        echo "[FAIL] Checksum mismatch for $artifact_path"
        echo "  Expected: $expected_hash"
        echo "  Actual:   $actual_hash"
        FAILED=1
    else
        echo "[OK] $artifact_path"
    fi
done < <(python3 -c "
import json
state = json.load(open('.trae/state.json'))
for artifact in state.get('artifact_checksums', []):
    print(artifact['path'])
" 2>/dev/null)

if [ $FAILED -eq 1 ]; then
    echo "[FAIL] Some checksums do not match"
    exit 1
fi

echo "[OK] All checksums verified"

# Check 4: history.json JSON validity
echo "[4/5] Checking .trae/history.json JSON validity..."
if ! python3 -m json.tool .trae/history.json > /dev/null 2>&1; then
    echo "[FAIL] .trae/history.json is not valid JSON"
    exit 1
fi
echo "[OK] history.json is valid JSON"

# Check 5: HMAC chain
echo "[5/5] Verifying HMAC chain integrity..."
HMAC_CHECK_FAILED=0

python3 << PYTHON_EOF
import json
import hmac
import hashlib
import sys
import os

secret = os.environ.get('BEAMLINE_HMAC_SECRET', 'beamline-secret-key-v1')
# Check production environment
is_production = (
    os.environ.get('CI') == 'true' or 
    os.environ.get('PRODUCTION') == 'true' or 
    os.environ.get('GITHUB_ACTIONS') == 'true'
)
if is_production and (not secret or secret == 'beamline-secret-key-v1'):
    print('[FAIL] BEAMLINE_HMAC_SECRET must be set in CI/CD and production')
    print('[FAIL] Current environment: CI/CD or Production detected')
    sys.exit(1)
history_data = json.load(open('.trae/history.json'))

# Handle both array and object with entries format
history = history_data if isinstance(history_data, list) else history_data.get('entries', [])

if len(history) == 0:
    print('[WARN] History is empty')
    sys.exit(0)

print(f'[INFO] Checking {len(history)} history entries...')

for i, entry in enumerate(history):
    ts = entry.get('ts', '')
    actor = entry.get('actor', '')
    action = entry.get('action', '')
    state_checksum = entry.get('state_checksum', '')
    hmac_prev = entry.get('hmac_prev', '')
    stored_hmac = entry.get('hmac', '')
    
    # Check required fields
    if not all([ts, actor, action, state_checksum, stored_hmac]):
        print(f'[FAIL] Entry {i}: Missing required fields')
        print(f'  Missing: {[k for k, v in {"ts": ts, "actor": actor, "action": action, "state_checksum": state_checksum, "hmac": stored_hmac}.items() if not v]}')
        sys.exit(1)
    
    # Check hmac_prev for all entries except first
    if i > 0:
        if not hmac_prev:
            print(f'[FAIL] Entry {i}: hmac_prev is empty (should reference previous entry)')
            print(f'  Actor: {actor}, Action: {action}')
            sys.exit(1)
        if hmac_prev != history[i-1]['hmac']:
            print(f'[FAIL] Entry {i}: hmac_prev does not match previous entry hmac')
            print(f'  Actor: {actor}, Action: {action}')
            print(f'  Expected: {history[i-1]["hmac"][:16]}...')
            print(f'  Actual:   {hmac_prev[:16]}...')
            sys.exit(1)
        print(f'[OK] Entry {i}: hmac_prev link verified')
    else:
        # First entry should have empty hmac_prev
        if hmac_prev:
            print(f'[WARN] Entry {i}: First entry should have empty hmac_prev')
        print(f'[OK] Entry {i}: Initial entry (no hmac_prev required)')
    
    # Calculate expected HMAC
    data = f"{ts}{actor}{action}{state_checksum}{hmac_prev}"
    expected_hmac = hmac.new(secret.encode(), data.encode(), hashlib.sha256).hexdigest()
    
    if stored_hmac != expected_hmac:
        print(f'[FAIL] Entry {i}: HMAC mismatch')
        print(f'  Summary: {actor} / {action} at {ts}')
        print(f'  Stored:   {stored_hmac[:16]}...')
        print(f'  Expected: {expected_hmac[:16]}...')
        sys.exit(1)
    else:
        print(f'[OK] Entry {i}: HMAC verified ({actor} / {action})')

print(f'[OK] HMAC chain integrity verified ({len(history)} entries)')
PYTHON_EOF

if [ $? -ne 0 ]; then
    HMAC_CHECK_FAILED=1
fi

if [ $HMAC_CHECK_FAILED -eq 1 ]; then
    echo "[FAIL] HMAC chain validation failed"
    exit 1
fi

echo "[OK] State and history validation completed successfully"
