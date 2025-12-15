#!/bin/bash
# Lint Mermaid diagrams
#
# This script validates Mermaid diagram syntax before PDF generation.
# Uses mermaid-cli to check syntax and report errors.
#
# Usage:
#   bash scripts/lint_mermaid.sh
#   bash scripts/lint_mermaid.sh apps/ui_web/docs/MESSAGES_LIVE_EVENT_FLOWS.md

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SOURCE_FILE="${1:-$PROJECT_ROOT/apps/ui_web/docs/MESSAGES_LIVE_EVENT_FLOWS.md}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

ERRORS=0
WARNINGS=0

echo -e "${GREEN}Linting Mermaid diagrams in $SOURCE_FILE...${NC}"

# Check if mermaid-cli is available
if command -v mmdc &> /dev/null; then
  MMDC_CMD="mmdc"
elif command -v docker &> /dev/null; then
  echo -e "${YELLOW}Using Docker for mermaid-cli${NC}"
  MMDC_CMD="docker run -it --rm -v \"$PROJECT_ROOT:/data\" minlag/mermaid-cli"
else
  echo -e "${RED}Error: mermaid-cli not found. Install with: npm install -g @mermaid-js/mermaid-cli${NC}"
  exit 1
fi

# Create temp directory
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Extract Mermaid diagrams
DIAGRAM_INDEX=0
IN_MERMAID=false
MERMAID_CONTENT=""
CURRENT_DIAGRAM=""

while IFS= read -r line || [ -n "$line" ]; do
  # Check for mermaid code block start
  if [[ "$line" =~ ^\`\`\`mermaid ]]; then
    IN_MERMAID=true
    MERMAID_CONTENT=""
    DIAGRAM_INDEX=$((DIAGRAM_INDEX + 1))
    CURRENT_DIAGRAM="diagram_${DIAGRAM_INDEX}"
    continue
  fi
  
  # Check for code block end
  if [[ "$line" =~ ^\`\`\`$ ]] && [ "$IN_MERMAID" = true ]; then
    IN_MERMAID=false
    if [ -n "$MERMAID_CONTENT" ]; then
      # Write mermaid content to temp file
      TEMP_MMD="$TEMP_DIR/${CURRENT_DIAGRAM}.mmd"
      echo "$MERMAID_CONTENT" > "$TEMP_MMD"
      
      # Validate syntax
      echo -e "  Checking ${CURRENT_DIAGRAM}..."
      
      if [[ "$MMDC_CMD" == "mmdc" ]]; then
        # Try to parse/validate (mmdc will error on invalid syntax)
        if mmdc -i "$TEMP_MMD" -o "$TEMP_DIR/${CURRENT_DIAGRAM}.png" -b transparent 2>&1 | grep -q "error\|Error\|ERROR"; then
          echo -e "${RED}    ✗ Syntax error in ${CURRENT_DIAGRAM}${NC}"
          mmdc -i "$TEMP_MMD" -o "$TEMP_DIR/${CURRENT_DIAGRAM}.png" -b transparent 2>&1 | grep -E "error|Error|ERROR" || true
          ERRORS=$((ERRORS + 1))
        else
          echo -e "${GREEN}    ✓ ${CURRENT_DIAGRAM} is valid${NC}"
        fi
      else
        # Docker version
        if docker run -it --rm -v "$PROJECT_ROOT:/data" minlag/mermaid-cli \
          -i "/data/$(realpath --relative-to="$PROJECT_ROOT" "$TEMP_MMD")" \
          -o "/data/$(realpath --relative-to="$PROJECT_ROOT" "$TEMP_DIR/${CURRENT_DIAGRAM}.png")" \
          -b transparent 2>&1 | grep -q "error\|Error\|ERROR"; then
          echo -e "${RED}    ✗ Syntax error in ${CURRENT_DIAGRAM}${NC}"
          ERRORS=$((ERRORS + 1))
        else
          echo -e "${GREEN}    ✓ ${CURRENT_DIAGRAM} is valid${NC}"
        fi
      fi
    fi
    MERMAID_CONTENT=""
    continue
  fi
  
  # Collect mermaid content
  if [ "$IN_MERMAID" = true ]; then
    MERMAID_CONTENT="${MERMAID_CONTENT}${line}"$'\n'
  fi
  
  # Try to extract diagram name from headings
  if [[ "$line" =~ ^##+\ +(.+)$ ]] && [ "$IN_MERMAID" = false ]; then
    CURRENT_DIAGRAM=$(echo "${BASH_REMATCH[1]}" | tr ' ' '_' | tr -cd '[:alnum:]_')
  fi
done < "$SOURCE_FILE"

# Summary
echo ""
if [ $ERRORS -eq 0 ]; then
  echo -e "${GREEN}✓ All Mermaid diagrams are valid!${NC}"
  echo -e "${GREEN}Total diagrams checked: $DIAGRAM_INDEX${NC}"
  exit 0
else
  echo -e "${RED}✗ Found $ERRORS error(s) in Mermaid diagrams${NC}"
  echo -e "${YELLOW}Total diagrams checked: $DIAGRAM_INDEX${NC}"
  exit 1
fi

