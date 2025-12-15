#!/bin/bash
# Policy Schema and Fixtures Validation
# 
# Validates:
# 1. policy.schema.json is valid JSON Schema
# 2. All policy fixtures conform to policy.schema.json
#
# Exit codes:
#   0 - All validations passed
#   1 - Schema validation failed
#   2 - Fixture validation failed
#   3 - Missing files or tools

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
POLICY_SCHEMA="$REPO_ROOT/apps/otp/router/docs/schemas/policy.schema.json"
FIXTURES_DIR="$REPO_ROOT/apps/otp/router/priv/fixtures/policies"

cd "$REPO_ROOT"

# Track failures
SCHEMA_ERRORS=0
FIXTURE_ERRORS=0
MISSING_TOOLS=0

echo "=========================================="
echo "Policy Schema and Fixtures Validation"
echo "=========================================="
echo ""

# Check required tools
check_tool() {
    local tool=$1
    if ! command -v "$tool" &> /dev/null; then
        echo -e "${RED}✗${NC} $tool not found. Please install it first."
        return 1
    fi
    return 0
}

# Check Python3 and jsonschema
if ! check_tool "python3"; then
    MISSING_TOOLS=1
fi

if ! python3 -c "import jsonschema" 2>/dev/null; then
    echo -e "${RED}✗${NC} Python jsonschema library not found."
    echo "  Install with: pip3 install jsonschema"
    MISSING_TOOLS=1
fi

if [ $MISSING_TOOLS -eq 1 ]; then
    echo ""
    echo -e "${RED}❌ ERROR: Required tools missing${NC}"
    exit 3
fi

# Check if schema file exists
if [ ! -f "$POLICY_SCHEMA" ]; then
    echo -e "${RED}❌ ERROR: Policy schema file not found: $POLICY_SCHEMA${NC}"
    exit 3
fi

# Check if fixtures directory exists
if [ ! -d "$FIXTURES_DIR" ]; then
    echo -e "${YELLOW}⚠️  WARNING: Fixtures directory not found: $FIXTURES_DIR${NC}"
    echo "  Skipping fixture validation"
    FIXTURE_ERRORS=0
else
    FIXTURE_COUNT=$(find "$FIXTURES_DIR" -name "*.json" -type f 2>/dev/null | wc -l)
    if [ "$FIXTURE_COUNT" -eq 0 ]; then
        echo -e "${YELLOW}⚠️  WARNING: No JSON fixtures found in $FIXTURES_DIR${NC}"
        echo "  Skipping fixture validation"
        FIXTURE_ERRORS=0
    fi
fi

echo "[INFO] Schema file: $POLICY_SCHEMA"
if [ -d "$FIXTURES_DIR" ]; then
    echo "[INFO] Fixtures directory: $FIXTURES_DIR"
    echo "[INFO] Found $FIXTURE_COUNT JSON fixture(s)"
fi
echo ""

# ============================================================================
# Step 1: Validate schema.json is valid JSON
# ============================================================================

echo "Step 1: Validating policy.schema.json syntax..."
if ! python3 -m json.tool "$POLICY_SCHEMA" > /dev/null 2>&1; then
    echo -e "${RED}✗${NC} Policy schema is not valid JSON"
    SCHEMA_ERRORS=1
else
    echo -e "${GREEN}✓${NC} Policy schema is valid JSON"
fi
echo ""

# ============================================================================
# Step 2: Validate schema.json is valid JSON Schema
# ============================================================================

echo "Step 2: Validating policy.schema.json as JSON Schema..."
VALIDATE_SCHEMA_SCRIPT=$(cat <<'PYTHON_EOF'
import json
import sys
import jsonschema

try:
    with open(sys.argv[1], 'r') as f:
        schema = json.load(f)
    
    # Validate schema structure (Draft 7)
    jsonschema.Draft7Validator.check_schema(schema)
    print("OK")
except jsonschema.SchemaError as e:
    print(f"SCHEMA_ERROR: {e.message}")
    sys.exit(1)
except json.JSONDecodeError as e:
    print(f"JSON_ERROR: {e.msg}")
    sys.exit(1)
except Exception as e:
    print(f"ERROR: {e}")
    sys.exit(1)
PYTHON_EOF
)

SCHEMA_VALIDATION=$(python3 -c "$VALIDATE_SCHEMA_SCRIPT" "$POLICY_SCHEMA" 2>&1) || {
    echo -e "${RED}✗${NC} Policy schema is not valid JSON Schema"
    echo "  $SCHEMA_VALIDATION"
    SCHEMA_ERRORS=1
}

if [ $SCHEMA_ERRORS -eq 0 ] && [ "$SCHEMA_VALIDATION" = "OK" ]; then
    echo -e "${GREEN}✓${NC} Policy schema is valid JSON Schema (Draft 7)"
fi
echo ""

# ============================================================================
# Step 3: Validate all fixtures against schema
# ============================================================================

if [ -d "$FIXTURES_DIR" ] && [ "$FIXTURE_COUNT" -gt 0 ]; then
    echo "Step 3: Validating fixtures against policy.schema.json..."
    echo ""
    
    VALIDATE_FIXTURE_SCRIPT=$(cat <<'PYTHON_EOF'
import json
import sys
import jsonschema
from pathlib import Path

schema_file = sys.argv[1]
fixture_file = sys.argv[2]

try:
    # Load schema
    with open(schema_file, 'r') as f:
        schema = json.load(f)
    
    # Load fixture
    with open(fixture_file, 'r') as f:
        fixture = json.load(f)
    
    # Validate fixture against schema
    jsonschema.validate(instance=fixture, schema=schema)
    print("OK")
except jsonschema.ValidationError as e:
    print(f"VALIDATION_ERROR: {e.message}")
    if e.path:
        print(f"  Path: {' -> '.join(str(p) for p in e.path)}")
    sys.exit(1)
except json.JSONDecodeError as e:
    print(f"JSON_ERROR: {e.msg}")
    sys.exit(1)
except Exception as e:
    print(f"ERROR: {e}")
    sys.exit(1)
PYTHON_EOF
)
    
    FIXTURE_PASSED=0
    FIXTURE_FAILED=0
    
    while IFS= read -r -d '' fixture_file; do
        fixture_name=$(basename "$fixture_file")
        fixture_rel_path="${fixture_file#$REPO_ROOT/}"
        
        FIXTURE_VALIDATION=$(python3 -c "$VALIDATE_FIXTURE_SCRIPT" "$POLICY_SCHEMA" "$fixture_file" 2>&1) || {
            echo -e "${RED}✗${NC} $fixture_rel_path"
            echo "  $FIXTURE_VALIDATION"
            FIXTURE_FAILED=$((FIXTURE_FAILED + 1))
            FIXTURE_ERRORS=1
            continue
        }
        
        if [ "$FIXTURE_VALIDATION" = "OK" ]; then
            echo -e "${GREEN}✓${NC} $fixture_rel_path"
            FIXTURE_PASSED=$((FIXTURE_PASSED + 1))
        fi
    done < <(find "$FIXTURES_DIR" -name "*.json" -type f -print0 2>/dev/null)
    
    echo ""
    echo "Fixture validation summary:"
    echo "  Passed: $FIXTURE_PASSED"
    if [ $FIXTURE_FAILED -gt 0 ]; then
        echo "  Failed: $FIXTURE_FAILED"
    fi
    echo ""
fi

# ============================================================================
# Summary
# ============================================================================

echo "=========================================="
echo "Validation Summary"
echo "=========================================="

if [ $SCHEMA_ERRORS -eq 0 ] && [ $FIXTURE_ERRORS -eq 0 ]; then
    echo -e "${GREEN}✅ SUCCESS: All validations passed${NC}"
    exit 0
else
    if [ $SCHEMA_ERRORS -gt 0 ]; then
        echo -e "${RED}❌ Schema validation failed${NC}"
    fi
    if [ $FIXTURE_ERRORS -gt 0 ]; then
        echo -e "${RED}❌ Fixture validation failed${NC}"
    fi
    exit 2
fi

