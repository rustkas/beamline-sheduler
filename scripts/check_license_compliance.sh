#!/bin/bash
# check_license_compliance.sh
# Local license compliance check (no external services)
# Checks all dependencies against allowed license list

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Exit codes
EXIT_SUCCESS=0
EXIT_INCOMPATIBLE=1
EXIT_ERROR=2

# Project license
PROJECT_LICENSE="Apache-2.0"
PROJECT_LICENSE_FILE="LICENSE"

# Allowed licenses (SPDX identifiers)
ALLOWED_PERMISSIVE=(
    "MIT"
    "Apache-2.0"
    "BSD-2-Clause"
    "BSD-3-Clause"
    "ISC"
    "Unlicense"
)

ALLOWED_WEAK_COPYLEFT=(
    "LGPL-2.1"
    "LGPL-3.0"
    "MPL-2.0"
)

# Prohibited licenses
PROHIBITED_LICENSES=(
    "GPL-2.0"
    "GPL-3.0"
    "AGPL-3.0"
    "SSPL"
    "BUSL-1.1"
)

# Arrays to track results
INCOMPATIBLE_LICENSES=()
REVIEW_REQUIRED=()
COMPATIBLE_LICENSES=()

# Check if project license file exists
check_project_license() {
    if [ ! -f "$PROJECT_LICENSE_FILE" ]; then
        echo -e "${RED}[FAIL]${NC} Project license file not found: $PROJECT_LICENSE_FILE"
        echo "[INFO] Please create LICENSE file with Apache License 2.0"
        return $EXIT_ERROR
    fi
    
    if ! grep -q "Apache License" "$PROJECT_LICENSE_FILE"; then
        echo -e "${YELLOW}[WARN]${NC} LICENSE file exists but may not be Apache-2.0"
        echo "[INFO] Please verify LICENSE file contains Apache License 2.0"
    else
        echo -e "${GREEN}[OK]${NC} Project license file found: $PROJECT_LICENSE_FILE"
    fi
    
    return $EXIT_SUCCESS
}

# Check license compatibility
check_license_compatibility() {
    local license="$1"
    local package="$2"
    local version="$3"
    
    # Normalize license (remove spaces, convert to uppercase)
    local normalized_license=$(echo "$license" | tr '[:lower:]' '[:upper:]' | tr -d ' ')
    
    # Check if prohibited
    for prohibited in "${PROHIBITED_LICENSES[@]}"; do
        if [[ "$normalized_license" == *"$prohibited"* ]]; then
            INCOMPATIBLE_LICENSES+=("$package@$version: $license (PROHIBITED: $prohibited)")
            return $EXIT_INCOMPATIBLE
        fi
    done
    
    # Check if permissive (fully compatible)
    for allowed in "${ALLOWED_PERMISSIVE[@]}"; do
        if [[ "$normalized_license" == *"$allowed"* ]]; then
            COMPATIBLE_LICENSES+=("$package@$version: $license")
            return $EXIT_SUCCESS
        fi
    done
    
    # Check if weak copyleft (requires review)
    for weak in "${ALLOWED_WEAK_COPYLEFT[@]}"; do
        if [[ "$normalized_license" == *"$weak"* ]]; then
            REVIEW_REQUIRED+=("$package@$version: $license (REVIEW REQUIRED: $weak)")
            return $EXIT_SUCCESS
        fi
    done
    
    # Unknown license - requires review
    REVIEW_REQUIRED+=("$package@$version: $license (UNKNOWN - REVIEW REQUIRED)")
    return $EXIT_SUCCESS
}

# Check Node.js dependencies
check_nodejs_dependencies() {
    echo ""
    echo "=========================================="
    echo "Checking Node.js Dependencies"
    echo "=========================================="
    
    local found_deps=false
    
    # Check for package.json files
    while IFS= read -r -d '' package_file; do
        if [ ! -f "$package_file" ]; then
            continue
        fi
        
        found_deps=true
        local dir=$(dirname "$package_file")
        echo "[INFO] Checking: $package_file"
        
        # Check if jq is available
        if ! command -v jq >/dev/null 2>&1; then
            echo -e "${YELLOW}[WARN]${NC} jq not found, skipping detailed license check"
            echo "[INFO] Install jq for detailed license analysis: sudo apt-get install jq"
            continue
        fi
        
        # Check dependencies
        if jq -e '.dependencies' "$package_file" >/dev/null 2>&1; then
            while IFS= read -r dep; do
                local package=$(echo "$dep" | jq -r 'keys[0]')
                local version=$(echo "$dep" | jq -r '.[]')
                
                # Try to get license from package.json (if available locally)
                local license="UNKNOWN"
                if [ -d "node_modules/$package" ] && [ -f "node_modules/$package/package.json" ]; then
                    license=$(jq -r '.license // .licenses[0].type // "UNKNOWN"' "node_modules/$package/package.json" 2>/dev/null || echo "UNKNOWN")
                fi
                
                if [ "$license" != "UNKNOWN" ] && [ "$license" != "null" ]; then
                    check_license_compatibility "$license" "$package" "$version"
                else
                    REVIEW_REQUIRED+=("$package@$version: License not found in local package.json (REVIEW REQUIRED)")
                fi
            done < <(jq -c '.dependencies // {} | to_entries | .[]' "$package_file" 2>/dev/null || echo "")
        fi
        
        # Check devDependencies
        if jq -e '.devDependencies' "$package_file" >/dev/null 2>&1; then
            while IFS= read -r dep; do
                local package=$(echo "$dep" | jq -r 'keys[0]')
                local version=$(echo "$dep" | jq -r '.[]')
                
                local license="UNKNOWN"
                if [ -d "node_modules/$package" ] && [ -f "node_modules/$package/package.json" ]; then
                    license=$(jq -r '.license // .licenses[0].type // "UNKNOWN"' "node_modules/$package/package.json" 2>/dev/null || echo "UNKNOWN")
                fi
                
                if [ "$license" != "UNKNOWN" ] && [ "$license" != "null" ]; then
                    check_license_compatibility "$license" "$package" "$version"
                else
                    REVIEW_REQUIRED+=("$package@$version: License not found (dev dependency, REVIEW REQUIRED)")
                fi
            done < <(jq -c '.devDependencies // {} | to_entries | .[]' "$package_file" 2>/dev/null || echo "")
        fi
    done < <(find . -name "package.json" -type f -not -path "*/node_modules/*" -not -path "*/.git/*" -print0)
    
    if [ "$found_deps" = "false" ]; then
        echo "[SKIP] No package.json files found"
    fi
}

# Check Erlang dependencies
check_erlang_dependencies() {
    echo ""
    echo "=========================================="
    echo "Checking Erlang/OTP Dependencies"
    echo "=========================================="
    
    local found_deps=false
    
    # Check for rebar.config or rebar.lock
    while IFS= read -r -d '' config_file; do
        if [ ! -f "$config_file" ]; then
            continue
        fi
        
        found_deps=true
        echo "[INFO] Checking: $config_file"
        echo "[INFO] Erlang dependency license checking requires manual review"
        echo "[INFO] Please check rebar.config and document licenses in docs/THIRD_PARTY_LICENSES.md"
    done < <(find . -name "rebar.config" -o -name "rebar.lock" -type f -not -path "*/_build/*" -not -path "*/.git/*" -print0)
    
    if [ "$found_deps" = "false" ]; then
        echo "[SKIP] No rebar.config or rebar.lock files found"
    fi
}

# Check C++ dependencies
check_cpp_dependencies() {
    echo ""
    echo "=========================================="
    echo "Checking C++ Dependencies"
    echo "=========================================="
    
    local found_deps=false
    
    # Check for CMakeLists.txt or conanfile.txt
    while IFS= read -r -d '' config_file; do
        if [ ! -f "$config_file" ]; then
            continue
        fi
        
        found_deps=true
        echo "[INFO] Checking: $config_file"
        echo "[INFO] C++ dependency license checking requires manual review"
        echo "[INFO] Please check CMakeLists.txt/conanfile.txt and document licenses in docs/THIRD_PARTY_LICENSES.md"
    done < <(find . -name "CMakeLists.txt" -o -name "conanfile.txt" -type f -not -path "*/build/*" -not -path "*/.git/*" -print0)
    
    if [ "$found_deps" = "false" ]; then
        echo "[SKIP] No CMakeLists.txt or conanfile.txt files found"
    fi
}

# Generate report
generate_report() {
    echo ""
    echo "=========================================="
    echo "License Compliance Report"
    echo "=========================================="
    echo ""
    
    local total_deps=$((${#COMPATIBLE_LICENSES[@]} + ${#REVIEW_REQUIRED[@]} + ${#INCOMPATIBLE_LICENSES[@]}))
    
    echo "Summary:"
    echo "  Total dependencies checked: $total_deps"
    echo "  ✅ Compatible: ${#COMPATIBLE_LICENSES[@]}"
    echo "  ⚠️  Review required: ${#REVIEW_REQUIRED[@]}"
    echo "  ❌ Incompatible: ${#INCOMPATIBLE_LICENSES[@]}"
    echo ""
    
    if [ ${#COMPATIBLE_LICENSES[@]} -gt 0 ]; then
        echo -e "${GREEN}✅ Compatible Licenses:${NC}"
        for license in "${COMPATIBLE_LICENSES[@]}"; do
            echo "  - $license"
        done
        echo ""
    fi
    
    if [ ${#REVIEW_REQUIRED[@]} -gt 0 ]; then
        echo -e "${YELLOW}⚠️  Review Required:${NC}"
        for license in "${REVIEW_REQUIRED[@]}"; do
            echo "  - $license"
        done
        echo ""
    fi
    
    if [ ${#INCOMPATIBLE_LICENSES[@]} -gt 0 ]; then
        echo -e "${RED}❌ Incompatible Licenses:${NC}"
        for license in "${INCOMPATIBLE_LICENSES[@]}"; do
            echo "  - $license"
        done
        echo ""
        echo -e "${RED}[FAIL]${NC} Incompatible licenses found. Please remove or replace these dependencies."
        return $EXIT_INCOMPATIBLE
    fi
    
    if [ ${#REVIEW_REQUIRED[@]} -gt 0 ]; then
        echo -e "${YELLOW}[WARN]${NC} Some licenses require manual review."
        echo "[INFO] Please review and document in docs/THIRD_PARTY_LICENSES.md"
        return $EXIT_SUCCESS
    fi
    
    echo -e "${GREEN}[OK]${NC} All checked licenses are compatible with $PROJECT_LICENSE"
    return $EXIT_SUCCESS
}

# Main execution
main() {
    echo "=========================================="
    echo "License Compliance Check"
    echo "=========================================="
    echo ""
    echo "Project License: $PROJECT_LICENSE"
    echo "License File: $PROJECT_LICENSE_FILE"
    echo ""
    
    # Check project license
    check_project_license || exit $EXIT_ERROR
    
    # Check dependencies
    check_nodejs_dependencies
    check_erlang_dependencies
    check_cpp_dependencies
    
    # Generate report
    generate_report
    local exit_code=$?
    
    echo ""
    echo "=========================================="
    echo "Next Steps"
    echo "=========================================="
    echo ""
    echo "1. Update docs/THIRD_PARTY_LICENSES.md with all dependencies"
    echo "2. Run scripts/generate_sbom.sh to generate SBOM"
    echo "3. Review any licenses marked as 'REVIEW REQUIRED'"
    echo "4. Remove or replace any incompatible licenses"
    echo ""
    
    exit $exit_code
}

# Run main
main "$@"

