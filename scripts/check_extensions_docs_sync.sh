#!/bin/bash
#
# Check Extensions Documentation Sync
#
# Validates that all extension-related reports and documentation are properly
# linked and referenced in the documentation structure.
#
# Usage:
#   ./scripts/check_extensions_docs_sync.sh
#
# Exit codes:
#   0 - All checks passed
#   1 - Missing links or broken references
#   2 - Missing documentation files
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
DOCS_DEV_DIR="${PROJECT_ROOT}/docs/dev"
ROUTER_DOCS_DIR="${PROJECT_ROOT}/apps/otp/router/docs"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ERROR_COUNT=0
WARN_COUNT=0

# Helper functions
error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
    ERROR_COUNT=$((ERROR_COUNT + 1))
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
    WARN_COUNT=$((WARN_COUNT + 1))
}

pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

# Expected extension-related documentation files
EXPECTED_DOCS=(
    "docs/archive/dev/EXTENSIONS_PIPELINE_CHECK_REPORT.md"
    "docs/archive/dev/EXTENSIONS_PIPELINE_IMPLEMENTATION_REPORT.md"
    "docs/archive/dev/EXTENSIONS_PIPELINE_PERF_REPORT.md"
    "docs/archive/dev/EXTENSIONS_E2E_IMPLEMENTATION_REPORT.md"
    "docs/archive/dev/EXTENSIONS_CONTRACT_E2E_REPORT.md"
    "docs/archive/dev/EXTENSIONS_PIPELINE_UI_IMPLEMENTATION_REPORT.md"
    "docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_REPORT.md"
    "docs/archive/dev/EXTENSIONS_PIPELINE_PRODUCTION_INTEGRATION_PLAN.md"
    "docs/archive/dev/PIPELINE_COMPLEXITY_MANAGEMENT_REPORT.md"
    "docs/archive/dev/CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md"
    "docs/EXTENSIONS_API.md"
    "apps/otp/router/docs/EXTENSIONS_RUNBOOK.md"
    "apps/otp/router/docs/EXTENSIONS_SECURITY_GUIDE.md"
    "apps/otp/router/docs/EXTENSIONS_E2E_GUIDE.md"
)

# Check if documentation files exist
check_doc_files() {
    echo ""
    echo "Checking extension documentation files..."
    
    local missing_files=()
    
    for doc in "${EXPECTED_DOCS[@]}"; do
        local doc_path="${PROJECT_ROOT}/${doc}"
        if [[ -f "${doc_path}" ]]; then
            pass "Found: ${doc}"
        else
            error "Missing: ${doc}"
            missing_files+=("${doc}")
        fi
    done
    
    if [[ ${#missing_files[@]} -gt 0 ]]; then
        return 1
    fi
    return 0
}

# Check links in main documentation
check_doc_links() {
    echo ""
    echo "Checking documentation links..."
    
    # Check if EXTENSIONS_API.md is referenced
    if grep -r "EXTENSIONS_API.md" "${PROJECT_ROOT}/docs" "${PROJECT_ROOT}/apps/otp/router/docs" &>/dev/null; then
        pass "EXTENSIONS_API.md is referenced in documentation"
    else
        warn "EXTENSIONS_API.md may not be referenced in documentation"
    fi
    
    # Check if extension reports are referenced
    local report_references=0
    for doc in "${EXPECTED_DOCS[@]}"; do
        local doc_name=$(basename "${doc}")
        if grep -r "${doc_name}" "${PROJECT_ROOT}/docs" "${PROJECT_ROOT}/apps/otp/router/docs" &>/dev/null; then
            report_references=$((report_references + 1))
        fi
    done
    
    if [[ ${report_references} -gt 0 ]]; then
        pass "Found ${report_references} extension report references in documentation"
    else
        warn "Extension reports may not be referenced in documentation"
    fi
    
    # Check if CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md is referenced
    if grep -r "CP2_EXTENSIONS_IMPLEMENTATION_PLAN" "${PROJECT_ROOT}/docs" "${PROJECT_ROOT}/apps/otp/router/docs" &>/dev/null; then
        pass "CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md is referenced"
    else
        warn "CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md may not be referenced"
    fi
    
    # Check if all extension reports are linked in CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md
    local plan_file="${PROJECT_ROOT}/docs/archive/dev/CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md"
    if [[ -f "${plan_file}" ]]; then
        local missing_links=0
        for doc in "${EXPECTED_DOCS[@]}"; do
            local doc_basename=$(basename "${doc}")
            if ! grep -q "${doc_basename}" "${plan_file}" 2>/dev/null; then
                warn "Report ${doc_basename} may not be linked in CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md"
                missing_links=$((missing_links + 1))
            fi
        done
        if [[ ${missing_links} -eq 0 ]]; then
            pass "All extension reports are linked in CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md"
        fi
    else
        warn "CP2_EXTENSIONS_IMPLEMENTATION_PLAN.md not found, skipping link check"
    fi
}

# Check test suite references
check_test_references() {
    echo ""
    echo "Checking test suite references..."
    
    local test_suites=(
        "router_extensions_e2e_SUITE"
        "router_extensions_pipeline_SUITE"
        "router_extensions_pipeline_load_SUITE"
        "router_extensions_security_SUITE"
        "router_extension_invoker_telemetry_SUITE"
    )
    
    for suite in "${test_suites[@]}"; do
        if grep -r "${suite}" "${PROJECT_ROOT}/scripts" "${PROJECT_ROOT}/.github" "${PROJECT_ROOT}/.drone.yml" "${PROJECT_ROOT}/.gitlab-ci.yml" &>/dev/null; then
            pass "Test suite ${suite} is referenced in CI configuration"
        else
            warn "Test suite ${suite} may not be referenced in CI configuration"
        fi
    done
}

# Main execution
main() {
    echo "========================================"
    echo "Extensions Documentation Sync Check"
    echo "========================================"
    
    check_doc_files
    check_doc_links
    check_test_references
    
    echo ""
    echo "========================================"
    echo "Summary"
    echo "========================================"
    echo "Errors: ${ERROR_COUNT}"
    echo "Warnings: ${WARN_COUNT}"
    
    if [[ ${ERROR_COUNT} -gt 0 ]]; then
        echo ""
        echo -e "${RED}❌ Documentation sync check FAILED${NC}"
        exit 1
    elif [[ ${WARN_COUNT} -gt 0 ]]; then
        echo ""
        echo -e "${YELLOW}⚠️  Documentation sync check completed with warnings${NC}"
        exit 0
    else
        echo ""
        echo -e "${GREEN}✅ Documentation sync check PASSED${NC}"
        exit 0
    fi
}

main "$@"

