#!/bin/bash
# Check links in documentation files
# Usage: ./scripts/check_links.sh [roots...]
# Exit codes: 0=ok, 1=error, 2=external links, 3=local broken links

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_ROOT"

# Default roots if not provided
ROOTS=("${@:-docs .cursor README.md}")

EXTERNAL_LINKS=0
LOCAL_BROKEN=0
ERRORS=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "[INFO] Checking links in: ${ROOTS[*]}"
echo ""

# Find all markdown files
find_markdown_files() {
    local roots=("$@")
    for root in "${roots[@]}"; do
        if [ -f "$root" ]; then
            echo "$root"
        elif [ -d "$root" ]; then
            find "$root" -type f -name "*.md" -o -name "*.mdc" 2>/dev/null | sort
        fi
    done
}

# Check if local file exists
check_local_link() {
    local link="$1"
    local base_dir="$2"
    
    # Remove anchor (#section)
    local file_path="${link%%#*}"
    
    # Skip empty or special links
    if [ -z "$file_path" ] || [[ "$file_path" == http* ]] || [[ "$file_path" == mailto:* ]]; then
        return 0
    fi
    
    # Resolve relative path
    local resolved_path
    if [[ "$file_path" == /* ]]; then
        # Absolute path from repo root
        resolved_path="${PROJECT_ROOT}${file_path}"
    else
        # Relative path
        resolved_path="$(cd "$base_dir" && realpath -m "$file_path" 2>/dev/null || echo "")"
    fi
    
    if [ -n "$resolved_path" ] && [ -f "$resolved_path" ]; then
        return 0
    else
        return 1
    fi
}

# Extract links from markdown file
extract_links() {
    local file="$1"
    
    # Extract markdown links [text](url)
    grep -oE '\[([^\]]+)\]\(([^)]+)\)' "$file" 2>/dev/null | \
        sed -E 's/\[([^\]]+)\]\(([^)]+)\)/\2/' || true
    
    # Extract reference links [text][ref] and [ref]: url
    grep -oE '\[([^\]]+)\]:\s*(.+)' "$file" 2>/dev/null | \
        sed -E 's/\[([^\]]+)\]:\s*(.+)/\2/' || true
}

# Check links in a file
check_file_links() {
    local file="$1"
    local base_dir="$(dirname "$file")"
    local file_errors=0
    local file_external=0
    local file_broken=0
    
    echo "Checking: $file"
    
    while IFS= read -r link; do
        if [ -z "$link" ]; then
            continue
        fi
        
        # Skip external links (http/https)
        if [[ "$link" == http://* ]] || [[ "$link" == https://* ]]; then
            file_external=$((file_external + 1))
            EXTERNAL_LINKS=$((EXTERNAL_LINKS + 1))
            continue
        fi
        
        # Check local link
        if ! check_local_link "$link" "$base_dir"; then
            echo "  ${RED}[BROKEN]${NC} $link"
            file_broken=$((file_broken + 1))
            LOCAL_BROKEN=$((LOCAL_BROKEN + 1))
            file_errors=$((file_errors + 1))
        fi
    done < <(extract_links "$file")
    
    if [ $file_errors -eq 0 ]; then
        echo "  ${GREEN}[OK]${NC} All links valid"
    fi
    
    echo ""
    return $file_errors
}

# Main check
main() {
    local total_files=0
    local files_with_errors=0
    
    while IFS= read -r file; do
        if [ ! -f "$file" ]; then
            continue
        fi
        
        total_files=$((total_files + 1))
        
        if ! check_file_links "$file"; then
            files_with_errors=$((files_with_errors + 1))
            ERRORS=$((ERRORS + 1))
        fi
    done < <(find_markdown_files "${ROOTS[@]}")
    
    echo "=========================================="
    echo "Summary:"
    echo "  Files checked: $total_files"
    echo "  Files with errors: $files_with_errors"
    echo "  External links: $EXTERNAL_LINKS"
    echo "  Broken local links: $LOCAL_BROKEN"
    echo "=========================================="
    
    # Exit codes
    if [ $LOCAL_BROKEN -gt 0 ]; then
        echo "${RED}[FAIL]${NC} Found $LOCAL_BROKEN broken local link(s)"
        exit 3
    elif [ $EXTERNAL_LINKS -gt 0 ]; then
        echo "${YELLOW}[WARN]${NC} Found $EXTERNAL_LINKS external link(s) (not validated)"
        exit 2
    elif [ $ERRORS -gt 0 ]; then
        echo "${RED}[FAIL]${NC} Found $ERRORS error(s)"
        exit 1
    else
        echo "${GREEN}[OK]${NC} All links valid"
        exit 0
    fi
}

main

