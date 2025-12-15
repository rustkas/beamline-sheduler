#!/bin/bash
# Router Contract Snapshot
# Static check that documented contracts match Router implementation
# Compares NATS subjects, required fields, and versions from docs vs code

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROUTER_DIR="$PROJECT_ROOT/apps/otp/router"
DOCS_DIR="$PROJECT_ROOT/docs"
REPORT_DIR="$PROJECT_ROOT/reports/router/contract_snapshots"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_FILE="$REPORT_DIR/contract_snapshot_${TIMESTAMP}.md"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Exit codes
EXIT_OK=0
EXIT_MISMATCH=1
EXIT_ERROR=2

# Initialize report
mkdir -p "$REPORT_DIR"

show_help() {
    cat <<EOF
Router Contract Snapshot

Purpose:
  Static check that documented contracts (NATS subjects, payload schemas) match
  Router implementation to catch drift before runtime.

Usage:
  $0 [OPTIONS]

Options:
  -h, --help          Show this help message
  -v, --verbose       Verbose output
  -o, --output FILE   Output report to FILE (default: reports/router/contract_snapshots/contract_snapshot_TIMESTAMP.md)

Examples:
  # Run contract snapshot check
  $0

  # Run with verbose output
  $0 --verbose

  # Output to specific file
  $0 --output reports/router/contract_snapshot_custom.md

Exit Codes:
  0  - All contracts match (OK)
  1  - Contract mismatches detected
  2  - Error during check (missing files, etc.)

EOF
}

# Parse arguments
VERBOSE=false
OUTPUT_FILE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -o|--output)
            OUTPUT_FILE="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            show_help
            exit 1
            ;;
    esac
done

if [[ -n "$OUTPUT_FILE" ]]; then
    REPORT_FILE="$OUTPUT_FILE"
fi

# Initialize report
{
    echo "# Router Contract Snapshot Report"
    echo ""
    echo "**Generated**: $(date -u +"%Y-%m-%d %H:%M:%S UTC")"
    echo "**Commit**: $(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')"
    echo "**Branch**: $(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'unknown')"
    echo ""
    echo "## Summary"
    echo ""
} > "$REPORT_FILE"

# Check prerequisites
check_prerequisites() {
    local errors=0
    
    if [[ ! -f "$DOCS_DIR/NATS_SUBJECTS.md" ]]; then
        echo "Error: $DOCS_DIR/NATS_SUBJECTS.md not found" >&2
        ((errors++))
    fi
    
    if [[ ! -f "$DOCS_DIR/ARCHITECTURE/PROTO_NATS_MAPPING.md" ]]; then
        echo "Error: $DOCS_DIR/ARCHITECTURE/PROTO_NATS_MAPPING.md not found" >&2
        ((errors++))
    fi
    
    if [[ ! -f "$DOCS_DIR/API_CONTRACTS.md" ]]; then
        echo "Error: $DOCS_DIR/API_CONTRACTS.md not found" >&2
        ((errors++))
    fi
    
    if [[ ! -f "$ROUTER_DIR/src/router_nats_subscriber.erl" ]]; then
        echo "Error: $ROUTER_DIR/src/router_nats_subscriber.erl not found" >&2
        ((errors++))
    fi
    
    if [[ ! -f "$ROUTER_DIR/src/router_result_consumer.erl" ]]; then
        echo "Error: $ROUTER_DIR/src/router_result_consumer.erl not found" >&2
        ((errors++))
    fi
    
    if [[ $errors -gt 0 ]]; then
        echo "Prerequisites check failed: $errors errors" >&2
        exit $EXIT_ERROR
    fi
}

# Extract NATS subject from code
extract_subject_from_code() {
    local file="$1"
    local pattern="$2"
    
    if [[ ! -f "$file" ]]; then
        echo ""
        return
    fi
    
    # Look for -define(SUBJECT, ...) or similar patterns
    # Escape special characters for grep
    local escaped_pattern=$(echo "$pattern" | sed 's/[()]/\\&/g')
    grep -E "$escaped_pattern" "$file" 2>/dev/null | head -1 | sed -E 's/.*<<"([^"]+)">>.*/\1/' || echo ""
}

# Extract required fields from code
extract_required_fields_from_code() {
    local file="$1"
    local fields=()
    
    if [[ ! -f "$file" ]]; then
        echo ""
        return
    fi
    
    # Check for version validation (required field)
    if grep -qE 'Missing version field|Unsupported request version' "$file"; then
        fields+=("version")
    fi
    
    # Check for tenant_id usage (may be required depending on allowlist)
    if grep -qE 'maps:get\(<<"tenant_id">>' "$file"; then
        fields+=("tenant_id")
    fi
    
    # Check for request_id usage
    if grep -qE 'maps:get\(<<"request_id">>' "$file"; then
        fields+=("request_id")
    fi
    
    # Check for task field (required for DecideRequest)
    if grep -qE 'maps:get\(<<"task">>' "$file"; then
        fields+=("task")
    fi
    
    # Remove duplicates and sort
    printf '%s\n' "${fields[@]}" | sort -u | tr '\n' ',' | sed 's/,$//'
}

# Extract version from code
extract_version_from_code() {
    local file="$1"
    
    if [[ ! -f "$file" ]]; then
        echo ""
        return
    fi
    
    # Look for version checks like <<"1">>
    grep -E '<<"1">>|version.*1|supported_versions.*1' "$file" | head -1 | sed -E 's/.*<<"1">>.*/1/' || echo "1"
}

# Extract NATS subject from docs
extract_subject_from_docs() {
    local doc_file="$1"
    local pattern="$2"
    
    if [[ ! -f "$doc_file" ]]; then
        echo ""
        return
    fi
    
    # Look for subject pattern in docs
    grep -E "$pattern" "$doc_file" | head -1 | sed -E 's/.*`?([^`\s]+)`?.*/\1/' | tr -d '`' || echo ""
}

# Extract required fields from docs
extract_required_fields_from_docs() {
    local doc_file="$1"
    local message_type="$2"
    local fields=()
    
    if [[ ! -f "$doc_file" ]]; then
        echo ""
        return
    fi
    
    # Look for required fields in API_CONTRACTS.md
    if [[ "$message_type" == "DecideRequest" ]]; then
        # Extract fields from DecideRequest section
        local in_section=false
        while IFS= read -r line; do
            if echo "$line" | grep -qE "^## DecideRequest"; then
                in_section=true
                continue
            fi
            if [[ "$in_section" == true ]] && echo "$line" | grep -qE "^## "; then
                break
            fi
            if [[ "$in_section" == true ]]; then
                # Match fields like "- `version`: string" or "- `tenant_id`: string"
                if echo "$line" | grep -qE '^- `(version|tenant_id|request_id|task)`:'; then
                    field=$(echo "$line" | sed -E 's/^- `([^`]+)`:.*/\1/')
                    # Skip if marked as optional (has ? or "optional")
                    if ! echo "$line" | grep -qE '\?|optional'; then
                        fields+=("$field")
                    fi
                fi
            fi
        done < "$doc_file"
    fi
    
    # Remove duplicates and sort
    printf '%s\n' "${fields[@]}" | sort -u | tr '\n' ',' | sed 's/,$//'
}

# Compare and report
compare_contracts() {
    local mismatches=0
    
    {
        echo "## Contract Checks"
        echo ""
    } >> "$REPORT_FILE"
    
    # Check 1: DecideRequest Subject
    echo "Checking DecideRequest NATS subject..."
    
    # Extract from docs
    DOC_SUBJECT=$(grep -E "beamline\.router\.v1\.decide" "$DOCS_DIR/NATS_SUBJECTS.md" 2>/dev/null | head -1 | sed -E 's/.*(beamline\.[^[:space:]]+).*/\1/' || echo "")
    if [[ -z "$DOC_SUBJECT" ]]; then
        DOC_SUBJECT=$(grep -E "beamline\.router\.v1\.decide" "$DOCS_DIR/ARCHITECTURE/PROTO_NATS_MAPPING.md" 2>/dev/null | head -1 | sed -E 's/.*(beamline\.[^[:space:]]+).*/\1/' || echo "")
    fi
    
    # Extract from code
    CODE_SUBJECT=$(grep -E '<<"beamline\.router\.v1\.decide">>' "$ROUTER_DIR/src/router_nats_subscriber.erl" 2>/dev/null | head -1 | sed -E 's/.*<<"([^"]+)">>.*/\1/' || echo "")
    if [[ -z "$CODE_SUBJECT" ]]; then
        CODE_SUBJECT=$(grep -E 'SUBJECT.*beamline\.router\.v1\.decide' "$ROUTER_DIR/src/router_nats_subscriber.erl" 2>/dev/null | head -1 | sed -E 's/.*<<"([^"]+)">>.*/\1/' || echo "")
    fi
    
    {
        echo "### 1. DecideRequest NATS Subject"
        echo ""
        echo "| Source | Subject |"
        echo "|--------|---------|"
        echo "| Documentation | \`$DOC_SUBJECT\` |"
        echo "| Code | \`$CODE_SUBJECT\` |"
        echo ""
    } >> "$REPORT_FILE"
    
    if [[ "$DOC_SUBJECT" != "$CODE_SUBJECT" ]]; then
        echo -e "${RED}✗ Mismatch: DecideRequest subject${NC}" >&2
        echo -e "  Docs: $DOC_SUBJECT" >&2
        echo -e "  Code: $CODE_SUBJECT" >&2
        {
            echo "**Status**: ❌ **MISMATCH**"
            echo ""
            echo "**Issue**: Subject in documentation does not match code."
        } >> "$REPORT_FILE"
        ((mismatches++))
    else
        echo -e "${GREEN}✓ DecideRequest subject matches${NC}"
        {
            echo "**Status**: ✅ **MATCH**"
        } >> "$REPORT_FILE"
    fi
    
    {
        echo ""
    } >> "$REPORT_FILE"
    
    # Check 2: ExecResult Subject
    echo "Checking ExecResult NATS subject..."
    
    # Extract from docs
    DOC_RESULT_SUBJECT=$(grep -E "caf\.exec\.result\.v1" "$DOCS_DIR/NATS_SUBJECTS.md" "$DOCS_DIR/API_CONTRACTS.md" 2>/dev/null | head -1 | sed -E 's/.*(caf\.[^[:space:]`]+).*/\1/' | sed 's/`//g' || echo "")
    
    # Extract from code
    CODE_RESULT_SUBJECT=$(grep -E '<<"caf\.exec\.result\.v1">>' "$ROUTER_DIR/src/router_result_consumer.erl" 2>/dev/null | head -1 | sed -E 's/.*<<"([^"]+)">>.*/\1/' || echo "")
    if [[ -z "$CODE_RESULT_SUBJECT" ]]; then
        CODE_RESULT_SUBJECT=$(grep -E 'DEFAULT_RESULT_SUBJECT.*caf\.exec\.result' "$ROUTER_DIR/src/router_result_consumer.erl" 2>/dev/null | head -1 | sed -E 's/.*<<"([^"]+)">>.*/\1/' || echo "")
    fi
    
    {
        echo "### 2. ExecResult NATS Subject"
        echo ""
        echo "| Source | Subject |"
        echo "|--------|---------|"
        echo "| Documentation | \`$DOC_RESULT_SUBJECT\` |"
        echo "| Code | \`$CODE_RESULT_SUBJECT\` |"
        echo ""
    } >> "$REPORT_FILE"
    
    if [[ -z "$DOC_RESULT_SUBJECT" ]] || [[ -z "$CODE_RESULT_SUBJECT" ]] || [[ "$DOC_RESULT_SUBJECT" != "$CODE_RESULT_SUBJECT" ]]; then
        if [[ -z "$DOC_RESULT_SUBJECT" ]] || [[ -z "$CODE_RESULT_SUBJECT" ]]; then
            echo -e "${YELLOW}⚠ Could not extract ExecResult subject${NC}" >&2
            {
                echo "**Status**: ⚠️ **UNKNOWN** (could not extract from docs or code)"
            } >> "$REPORT_FILE"
        else
            echo -e "${RED}✗ Mismatch: ExecResult subject${NC}" >&2
            echo -e "  Docs: $DOC_RESULT_SUBJECT" >&2
            echo -e "  Code: $CODE_RESULT_SUBJECT" >&2
            {
                echo "**Status**: ❌ **MISMATCH**"
                echo ""
                echo "**Issue**: Subject in documentation does not match code."
            } >> "$REPORT_FILE"
            ((mismatches++))
        fi
    else
        echo -e "${GREEN}✓ ExecResult subject matches${NC}"
        {
            echo "**Status**: ✅ **MATCH**"
        } >> "$REPORT_FILE"
    fi
    
    {
        echo ""
    } >> "$REPORT_FILE"
    
    # Check 3: DecideRequest Required Fields
    echo "Checking DecideRequest required fields..."
    DOC_FIELDS=$(extract_required_fields_from_docs "$DOCS_DIR/API_CONTRACTS.md" "DecideRequest")
    CODE_FIELDS=$(extract_required_fields_from_code "$ROUTER_DIR/src/router_nats_subscriber.erl")
    
    # Normalize: convert to sorted comma-separated
    DOC_FIELDS_NORM=$(echo "$DOC_FIELDS" | tr ',' '\n' | sort | tr '\n' ',' | sed 's/,$//')
    CODE_FIELDS_NORM=$(echo "$CODE_FIELDS" | tr ',' '\n' | sort | tr '\n' ',' | sed 's/,$//')
    
    {
        echo "### 3. DecideRequest Required Fields"
        echo ""
        echo "| Source | Required Fields |"
        echo "|--------|----------------|"
        echo "| Documentation | \`$DOC_FIELDS_NORM\` |"
        echo "| Code | \`$CODE_FIELDS_NORM\` |"
        echo ""
    } >> "$REPORT_FILE"
    
    if [[ "$DOC_FIELDS_NORM" != "$CODE_FIELDS_NORM" ]]; then
        echo -e "${RED}✗ Mismatch: DecideRequest required fields${NC}" >&2
        echo -e "  Docs: $DOC_FIELDS_NORM" >&2
        echo -e "  Code: $CODE_FIELDS_NORM" >&2
        {
            echo "**Status**: ❌ **MISMATCH**"
            echo ""
            echo "**Issue**: Required fields in documentation do not match code validation."
        } >> "$REPORT_FILE"
        ((mismatches++))
    else
        echo -e "${GREEN}✓ DecideRequest required fields match${NC}"
        {
            echo "**Status**: ✅ **MATCH**"
        } >> "$REPORT_FILE"
    fi
    
    {
        echo ""
    } >> "$REPORT_FILE"
    
    # Check 4: Version
    echo "Checking version..."
    DOC_VERSION=$(grep -E 'version.*"1"|version.*1|current.*"1"' "$DOCS_DIR/API_CONTRACTS.md" "$DOCS_DIR/NATS_SUBJECTS.md" 2>/dev/null | head -1 | sed -E 's/.*"1".*/1/' || echo "1")
    CODE_VERSION=$(extract_version_from_code "$ROUTER_DIR/src/router_nats_subscriber.erl")
    
    {
        echo "### 4. API Version"
        echo ""
        echo "| Source | Version |"
        echo "|--------|---------|"
        echo "| Documentation | \`$DOC_VERSION\` |"
        echo "| Code | \`$CODE_VERSION\` |"
        echo ""
    } >> "$REPORT_FILE"
    
    if [[ "$DOC_VERSION" != "$CODE_VERSION" ]]; then
        echo -e "${RED}✗ Mismatch: API version${NC}" >&2
        echo -e "  Docs: $DOC_VERSION" >&2
        echo -e "  Code: $CODE_VERSION" >&2
        {
            echo "**Status**: ❌ **MISMATCH**"
            echo ""
            echo "**Issue**: Version in documentation does not match code."
        } >> "$REPORT_FILE"
        ((mismatches++))
    else
        echo -e "${GREEN}✓ API version matches${NC}"
        {
            echo "**Status**: ✅ **MATCH**"
        } >> "$REPORT_FILE"
    fi
    
    {
        echo ""
    } >> "$REPORT_FILE"
    
    # Summary
    {
        echo "## Summary"
        echo ""
        if [[ $mismatches -eq 0 ]]; then
            echo "**Status**: ✅ **ALL CONTRACTS MATCH**"
            echo ""
            echo "All documented contracts match Router implementation."
        else
            echo "**Status**: ❌ **CONTRACT MISMATCHES DETECTED**"
            echo ""
            echo "**Mismatches**: $mismatches"
            echo ""
            echo "Please review the contract checks above and update either documentation or code to resolve mismatches."
        fi
        echo ""
        echo "## Files Checked"
        echo ""
        echo "- \`docs/NATS_SUBJECTS.md\`"
        echo "- \`docs/ARCHITECTURE/PROTO_NATS_MAPPING.md\`"
        echo "- \`docs/API_CONTRACTS.md\`"
        echo "- \`apps/otp/router/src/router_nats_subscriber.erl\`"
        echo "- \`apps/otp/router/src/router_result_consumer.erl\`"
        echo ""
    } >> "$REPORT_FILE"
    
    return $mismatches
}

# Main
main() {
    echo "Router Contract Snapshot Check"
    echo "=============================="
    echo ""
    
    check_prerequisites
    
    echo "Checking contracts..."
    echo ""
    
    compare_contracts
    local result=$?
    
    echo ""
    echo "Report saved to: $REPORT_FILE"
    echo ""
    
    if [[ $result -eq 0 ]]; then
        echo -e "${GREEN}✓ All contracts match${NC}"
        exit $EXIT_OK
    else
        echo -e "${RED}✗ Contract mismatches detected${NC}"
        echo "See report for details: $REPORT_FILE"
        exit $EXIT_MISMATCH
    fi
}

main "$@"

