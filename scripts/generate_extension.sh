#!/bin/bash
#
# Generate Extension Boilerplate
#
# Usage:
#   ./scripts/generate_extension.sh <extension_id> <extension_type> [options]
#
# Arguments:
#   extension_id    - Extension identifier (e.g., "my_extension")
#   extension_type  - Extension type: pre, validator, post, provider
#
# Options:
#   --name <name>           - Human-readable name (default: extension_id)
#   --description <desc>    - Extension description
#   --subject <subject>      - NATS subject (auto-generated if not provided)
#   --timeout <ms>          - Timeout in milliseconds (default: 1000)
#   --retry <count>         - Retry count (default: 0)
#   --output <dir>          - Output directory (default: tools/extensions-<extension_id>)
#   --author <author>       - Author name
#
# Examples:
#   ./scripts/generate_extension.sh my_pre_processor pre --description "My pre-processor"
#   ./scripts/generate_extension.sh my_validator validator --timeout 500
#   ./scripts/generate_extension.sh my_provider provider --subject "beamline.provider.my_provider.v1"
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
TEMPLATE_DIR="${PROJECT_ROOT}/tools/extension-template"
OUTPUT_DIR="${PROJECT_ROOT}/tools/extensions"

# Default values
EXTENSION_NAME=""
EXTENSION_DESCRIPTION=""
NATS_SUBJECT=""
TIMEOUT_MS=1000
RETRY=0
AUTHOR=""

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Parse arguments
if [ $# -lt 2 ]; then
    echo "Usage: $0 <extension_id> <extension_type> [options]"
    echo ""
    echo "Extension types: pre, validator, post, provider"
    echo ""
    echo "Options:"
    echo "  --name <name>           Human-readable name"
    echo "  --description <desc>   Extension description"
    echo "  --subject <subject>     NATS subject (auto-generated if not provided)"
    echo "  --timeout <ms>          Timeout in milliseconds (default: 1000)"
    echo "  --retry <count>         Retry count (default: 0)"
    echo "  --output <dir>          Output directory"
    echo "  --author <author>       Author name"
    exit 1
fi

EXTENSION_ID="$1"
EXTENSION_TYPE="$2"
shift 2

# Validate extension type
case "${EXTENSION_TYPE}" in
    pre|validator|post|provider)
        ;;
    *)
        echo "Error: Invalid extension type: ${EXTENSION_TYPE}"
        echo "Valid types: pre, validator, post, provider"
        exit 1
        ;;
esac

# Parse options
while [[ $# -gt 0 ]]; do
    case $1 in
        --name)
            EXTENSION_NAME="$2"
            shift 2
            ;;
        --description)
            EXTENSION_DESCRIPTION="$2"
            shift 2
            ;;
        --subject)
            NATS_SUBJECT="$2"
            shift 2
            ;;
        --timeout)
            TIMEOUT_MS="$2"
            shift 2
            ;;
        --retry)
            RETRY="$2"
            shift 2
            ;;
        --output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --author)
            AUTHOR="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Set defaults
if [ -z "${EXTENSION_NAME}" ]; then
    EXTENSION_NAME="${EXTENSION_ID}"
fi

if [ -z "${EXTENSION_DESCRIPTION}" ]; then
    EXTENSION_DESCRIPTION="${EXTENSION_TYPE} extension: ${EXTENSION_NAME}"
fi

if [ -z "${NATS_SUBJECT}" ]; then
    # Auto-generate NATS subject based on type
    case "${EXTENSION_TYPE}" in
        pre)
            NATS_SUBJECT="beamline.ext.pre.${EXTENSION_ID}.v1"
            ;;
        validator)
            NATS_SUBJECT="beamline.ext.validate.${EXTENSION_ID}.v1"
            ;;
        post)
            NATS_SUBJECT="beamline.ext.post.${EXTENSION_ID}.v1"
            ;;
        provider)
            NATS_SUBJECT="beamline.provider.${EXTENSION_ID}.v1"
            ;;
    esac
fi

# Create output directory
EXTENSION_DIR="${OUTPUT_DIR}/${EXTENSION_ID}"
mkdir -p "${EXTENSION_DIR}/src"
mkdir -p "${EXTENSION_DIR}/test"

echo -e "${GREEN}Generating extension: ${EXTENSION_ID}${NC}"
echo "  Type: ${EXTENSION_TYPE}"
echo "  Subject: ${NATS_SUBJECT}"
echo "  Output: ${EXTENSION_DIR}"
echo ""

# Generate extension logic based on type
case "${EXTENSION_TYPE}" in
    pre|post)
        EXTENSION_LOGIC=$(cat <<'EOF'
        // Pre/Post processor: modify payload
        const modifiedPayload = {
            ...payload,
            payload: payload.payload, // TODO: Modify payload here
            metadata: {
                ...(payload.metadata || {}),
                processed_by: '{{EXTENSION_ID}}'
            }
        };
        
        const response = {
            payload: modifiedPayload,
            metadata: {
                ...metadata,
                processed_at: new Date().toISOString()
            }
        };
EOF
)
        ERROR_RESPONSE_FORMAT='error: { code: "PROCESSING_ERROR", message: error.message }'
        RESPONSE_FORMAT_DESCRIPTION="Pre/Post processors return modified payload:\n\n\`\`\`json\n{\n  \"payload\": { /* modified payload */ },\n  \"metadata\": { /* enriched metadata */ }\n}\n\`\`\`"
        ;;
    validator)
        EXTENSION_LOGIC=$(cat <<'EOF'
        // Validator: check and return status
        const isValid = true; // TODO: Implement validation logic
        
        const response = isValid ? {
            status: 'ok'
        } : {
            status: 'reject',
            reason: 'validation_failed',
            details: {
                field: 'payload',
                message: 'Validation failed'
            }
        };
EOF
)
        ERROR_RESPONSE_FORMAT='status: "reject", reason: "processing_error", details: { error: error.message }'
        RESPONSE_FORMAT_DESCRIPTION="Validators return status:\n\n\`\`\`json\n{\n  \"status\": \"ok\" | \"reject\",\n  \"reason\": \"optional reason\",\n  \"details\": { /* optional details */ }\n}\n\`\`\`"
        ;;
    provider)
        EXTENSION_LOGIC=$(cat <<'EOF'
        // Provider: return CP2-style provider response
        const response = {
            status: 'success',
            provider_id: '{{EXTENSION_ID}}',
            response: {
                message_id: payload.message_id || 'generated-id',
                message_type: payload.message_type || 'chat',
                payload: 'Provider response', // TODO: Generate provider response
                metadata: {
                    ...(payload.metadata || {}),
                    provider: '{{EXTENSION_ID}}'
                }
            },
            metadata: {
                ...metadata,
                provider_latency_ms: Date.now() - startTime
            }
        };
EOF
)
        ERROR_RESPONSE_FORMAT='status: "error", error: { code: "PROCESSING_ERROR", message: error.message }'
        RESPONSE_FORMAT_DESCRIPTION="Providers return CP2-style response with provider_id, output, usage, and metadata fields."
        ;;
esac

# Function to replace template variables
replace_template() {
    local file="$1"
    local output="$2"
    
    # Use temporary file for complex replacements
    local temp_file=$(mktemp)
    cp "$file" "$temp_file"
    
    # Simple replacements
    sed -i "s/{{EXTENSION_ID}}/${EXTENSION_ID}/g" "$temp_file"
    sed -i "s/{{EXTENSION_NAME}}/${EXTENSION_NAME}/g" "$temp_file"
    sed -i "s/{{EXTENSION_TYPE}}/${EXTENSION_TYPE}/g" "$temp_file"
    sed -i "s/{{EXTENSION_DESCRIPTION}}/${EXTENSION_DESCRIPTION}/g" "$temp_file"
    sed -i "s/{{NATS_SUBJECT}}/${NATS_SUBJECT}/g" "$temp_file"
    sed -i "s/{{TIMEOUT_MS}}/${TIMEOUT_MS}/g" "$temp_file"
    sed -i "s/{{RETRY}}/${RETRY}/g" "$temp_file"
    sed -i "s/{{AUTHOR}}/${AUTHOR}/g" "$temp_file"
    
    # Complex multi-line replacements using awk
    awk -v logic="${EXTENSION_LOGIC}" \
        -v error_format="${ERROR_RESPONSE_FORMAT}" \
        -v response_desc="${RESPONSE_FORMAT_DESCRIPTION}" \
        '{
            if ($0 ~ /{{EXTENSION_LOGIC}}/) {
                gsub(/{{EXTENSION_LOGIC}}/, logic)
            }
            if ($0 ~ /{{ERROR_RESPONSE_FORMAT}}/) {
                gsub(/{{ERROR_RESPONSE_FORMAT}}/, error_format)
            }
            if ($0 ~ /{{RESPONSE_FORMAT_DESCRIPTION}}/) {
                gsub(/{{RESPONSE_FORMAT_DESCRIPTION}}/, response_desc)
            }
            print
        }' "$temp_file" > "$output"
    
    rm -f "$temp_file"
}

# Generate files
echo "Generating files..."

# package.json
replace_template "${TEMPLATE_DIR}/package.json.template" "${EXTENSION_DIR}/package.json"
echo "  ✓ package.json"

# Main handler
replace_template "${TEMPLATE_DIR}/src/extension.handler.js.template" "${EXTENSION_DIR}/src/${EXTENSION_ID}.js"
chmod +x "${EXTENSION_DIR}/src/${EXTENSION_ID}.js"
echo "  ✓ src/${EXTENSION_ID}.js"

# Unit tests
replace_template "${TEMPLATE_DIR}/test/extension.test.js.template" "${EXTENSION_DIR}/test/${EXTENSION_ID}.test.js"
chmod +x "${EXTENSION_DIR}/test/${EXTENSION_ID}.test.js"
echo "  ✓ test/${EXTENSION_ID}.test.js"

# E2E tests
replace_template "${TEMPLATE_DIR}/test/extension.e2e.js.template" "${EXTENSION_DIR}/test/${EXTENSION_ID}.e2e.js"
chmod +x "${EXTENSION_DIR}/test/${EXTENSION_ID}.e2e.js"
echo "  ✓ test/${EXTENSION_ID}.e2e.js"

# Registry configuration
replace_template "${TEMPLATE_DIR}/extension.registry.json.template" "${EXTENSION_DIR}/${EXTENSION_ID}.registry.json"
echo "  ✓ ${EXTENSION_ID}.registry.json"

# README
replace_template "${TEMPLATE_DIR}/README.md.template" "${EXTENSION_DIR}/README.md"
echo "  ✓ README.md"

# .gitignore
cp "${TEMPLATE_DIR}/.gitignore" "${EXTENSION_DIR}/.gitignore"
echo "  ✓ .gitignore"

echo ""
echo -e "${GREEN}✅ Extension generated successfully!${NC}"
echo ""
echo "Next steps:"
echo "  1. cd ${EXTENSION_DIR}"
echo "  2. npm install"
echo "  3. Implement your logic in src/${EXTENSION_ID}.js"
echo "  4. Add registry config: cp ${EXTENSION_ID}.registry.json ../../apps/otp/router/priv/fixtures/extensions/"
echo "  5. Test locally: npm start"
echo "  6. Run E2E tests: npm run test:e2e"
echo ""
echo "See docs/EXTENSIONS_DEVELOPER_GUIDE.md for complete guide."

