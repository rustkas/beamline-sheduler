#!/bin/bash
# Schema changes validation: blocks PR when schemas are modified without version updates

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

MANIFEST_FILE=".trae/manifest.json"
STATE_SCHEMA="docs/STATE.schema.json"
HISTORY_SCHEMA="docs/HISTORY.schema.json"

ERRORS=0
WARNINGS=0

echo "=========================================="
echo "Schema Changes Validation"
echo "=========================================="
echo ""

# Policy schema validation (separate from STATE/HISTORY schemas)
POLICY_SCHEMA_SCRIPT="$SCRIPT_DIR/check_policy_schema.sh"
if [ -f "$POLICY_SCHEMA_SCRIPT" ]; then
    echo "----------------------------------------"
    echo "Policy Schema Validation"
    echo "----------------------------------------"
    echo ""
    if bash "$POLICY_SCHEMA_SCRIPT"; then
        echo "[OK] Policy schema validation passed"
    else
        echo "[FAIL] Policy schema validation failed"
        ERRORS=$((ERRORS + 1))
    fi
    echo ""
fi

# Check if manifest exists
if [ ! -f "$MANIFEST_FILE" ]; then
    echo "[FAIL] Manifest file not found: $MANIFEST_FILE"
    exit 1
fi

# Check manifest validity
if ! python3 -m json.tool "$MANIFEST_FILE" > /dev/null 2>&1; then
    echo "[FAIL] Manifest file is not valid JSON"
    exit 1
fi

# Get versions from manifest
MANIFEST_STATE_VERSION=$(jq -r '.schema_versions.state.version' "$MANIFEST_FILE")
MANIFEST_HISTORY_VERSION=$(jq -r '.schema_versions.history.version' "$MANIFEST_FILE")

echo "[INFO] Manifest state version: $MANIFEST_STATE_VERSION"
echo "[INFO] Manifest history version: $MANIFEST_HISTORY_VERSION"
echo ""

# Function to extract version from schema
get_schema_version() {
    local schema_file=$1
    if [ ! -f "$schema_file" ]; then
        echo ""
        return
    fi
    
    # Try to extract version from $id
    local version=$(jq -r '.["$id"] // empty' "$schema_file" 2>/dev/null | grep -oE 'v[0-9]+\.[0-9]+\.[0-9]+' | head -1 | sed 's/v//')
    
    if [ -z "$version" ]; then
        # Try to extract from version field
        version=$(jq -r '.version // empty' "$schema_file" 2>/dev/null)
    fi
    
    echo "$version"
}

# Check STATE schema
if [ -f "$STATE_SCHEMA" ]; then
    SCHEMA_VERSION=$(get_schema_version "$STATE_SCHEMA")
    
    if [ -z "$SCHEMA_VERSION" ]; then
        echo "[WARN] STATE schema version not found in schema file"
        ((WARNINGS++))
    elif [ "$SCHEMA_VERSION" != "$MANIFEST_STATE_VERSION" ]; then
        echo "[FAIL] STATE schema version mismatch:"
        echo "  Schema: $SCHEMA_VERSION"
        echo "  Manifest: $MANIFEST_STATE_VERSION"
        ((ERRORS++))
    else
        echo "[OK] STATE schema version matches manifest: $SCHEMA_VERSION"
    fi
else
    echo "[WARN] STATE schema file not found: $STATE_SCHEMA"
    ((WARNINGS++))
fi

# Check HISTORY schema
if [ -f "$HISTORY_SCHEMA" ]; then
    SCHEMA_VERSION=$(get_schema_version "$HISTORY_SCHEMA")
    
    if [ -z "$SCHEMA_VERSION" ]; then
        echo "[WARN] HISTORY schema version not found in schema file"
        ((WARNINGS++))
    elif [ "$SCHEMA_VERSION" != "$MANIFEST_HISTORY_VERSION" ]; then
        echo "[FAIL] HISTORY schema version mismatch:"
        echo "  Schema: $SCHEMA_VERSION"
        echo "  Manifest: $MANIFEST_HISTORY_VERSION"
        ((ERRORS++))
    else
        echo "[OK] HISTORY schema version matches manifest: $SCHEMA_VERSION"
    fi
else
    echo "[WARN] HISTORY schema file not found: $HISTORY_SCHEMA"
    ((WARNINGS++))
fi

# Check Git changes (if available)
if command -v git > /dev/null 2>&1 && git rev-parse --git-dir > /dev/null 2>&1; then
    echo ""
    echo "[INFO] Checking for schema changes in Git..."
    
    # Check STATE schema changes
    if git diff --name-only HEAD 2>/dev/null | grep -q "^$STATE_SCHEMA$" || git diff --cached --name-only 2>/dev/null | grep -q "^$STATE_SCHEMA$"; then
        echo "[WARN] STATE schema was modified"
        
        # Check if version changed
        OLD_VERSION=$(git show HEAD:"$STATE_SCHEMA" 2>/dev/null | jq -r '.["$id"] // .version // empty' | grep -oE 'v[0-9]+\.[0-9]+\.[0-9]+' | head -1 | sed 's/v//' || echo "")
        NEW_VERSION=$(get_schema_version "$STATE_SCHEMA")
        
        if [ -n "$OLD_VERSION" ] && [ -n "$NEW_VERSION" ] && [ "$OLD_VERSION" = "$NEW_VERSION" ]; then
            echo "[FAIL] STATE schema was modified but version was not updated"
            echo "  Current version: $NEW_VERSION"
            echo "  Please update version in schema and manifest.json"
            ((ERRORS++))
        elif [ -n "$OLD_VERSION" ] && [ -n "$NEW_VERSION" ] && [ "$OLD_VERSION" != "$NEW_VERSION" ]; then
            echo "[OK] STATE schema version was updated: $OLD_VERSION → $NEW_VERSION"
            
            # Check if manifest was updated
            if [ "$NEW_VERSION" != "$MANIFEST_STATE_VERSION" ]; then
                echo "[FAIL] STATE schema version updated but manifest.json was not updated"
                echo "  Schema version: $NEW_VERSION"
                echo "  Manifest version: $MANIFEST_STATE_VERSION"
                ((ERRORS++))
            fi
        fi
    fi
    
    # Check HISTORY schema changes
    if git diff --name-only HEAD | grep -q "^$HISTORY_SCHEMA$"; then
        echo "[WARN] HISTORY schema was modified"
        
        # Check if version changed
        OLD_VERSION=$(git show HEAD:"$HISTORY_SCHEMA" 2>/dev/null | jq -r '.["$id"] // .version // empty' | grep -oE 'v[0-9]+\.[0-9]+\.[0-9]+' | head -1 | sed 's/v//' || echo "")
        NEW_VERSION=$(get_schema_version "$HISTORY_SCHEMA")
        
        if [ -n "$OLD_VERSION" ] && [ -n "$NEW_VERSION" ] && [ "$OLD_VERSION" = "$NEW_VERSION" ]; then
            echo "[FAIL] HISTORY schema was modified but version was not updated"
            echo "  Current version: $NEW_VERSION"
            echo "  Please update version in schema and manifest.json"
            ((ERRORS++))
        elif [ -n "$OLD_VERSION" ] && [ -n "$NEW_VERSION" ] && [ "$OLD_VERSION" != "$NEW_VERSION" ]; then
            echo "[OK] HISTORY schema version was updated: $OLD_VERSION → $NEW_VERSION"
            
            # Check if manifest was updated
            if [ "$NEW_VERSION" != "$MANIFEST_HISTORY_VERSION" ]; then
                echo "[FAIL] HISTORY schema version updated but manifest.json was not updated"
                echo "  Schema version: $NEW_VERSION"
                echo "  Manifest version: $MANIFEST_HISTORY_VERSION"
                ((ERRORS++))
            fi
        fi
    fi
    
    # Check manifest changes
    if git diff --name-only HEAD | grep -q "^$MANIFEST_FILE$"; then
        echo "[INFO] Manifest was modified"
    fi
fi

# Final result
echo ""
echo "=========================================="
if [ $ERRORS -eq 0 ]; then
    if [ $WARNINGS -gt 0 ]; then
        echo "[WARN] Schema changes validation passed with $WARNINGS warning(s)"
        exit 0
    else
        echo "[OK] Schema changes validation passed"
        exit 0
    fi
else
    echo "[FAIL] Schema changes validation failed: $ERRORS error(s)"
    if [ $WARNINGS -gt 0 ]; then
        echo "[WARN] $WARNINGS warning(s)"
    fi
    echo ""
    echo "Please fix the following issues:"
    echo "  1. Update schema versions when modifying schemas"
    echo "  2. Update manifest.json when updating schema versions"
    echo "  3. Follow versioning policy: docs/SCHEMA_VERSIONING.md"
    exit 1
fi
