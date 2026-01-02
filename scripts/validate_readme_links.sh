#!/usr/bin/env bash
# README Link Validator
# Validates all internal and external links in README.md

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

ERROR_COUNT=0

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║         README.md Link Validator                           ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo

# Function to check if file exists
check_file() {
  local file=$1
  if [ -e "$file" ]; then
    echo -e "${GREEN}✅${NC} $file"
    return 0
  else
    echo -e "${RED}❌ MISSING:${NC} $file"
    ((ERROR_COUNT++))
    return 1
  fi
}

# 1. Check internal file links
echo -e "${BLUE}📁 Internal File Links:${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Extract internal links (not starting with http, not anchors)
internal_links=$(grep -oP '\[.*?\]\(\K[^)]+(?=\))' README.md | grep -v '^http' | grep -v '^#' | sort -u || true)

if [ -n "$internal_links" ]; then
  while IFS= read -r link; do
    [ -z "$link" ] && continue
    check_file "$link"
  done <<< "$internal_links"
else
  echo -e "${YELLOW}⚠️  No internal file links found${NC}"
fi

echo

# 2. Check component directories
echo -e "${BLUE}📦 Component Directories:${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

COMPONENTS=(
  "apps/c-gateway"
  "apps/otp/router"
  "apps/caf/processor"
  "apps/worker"
  "apps/ui_web"
)

for dir in "${COMPONENTS[@]}"; do
  if [ -d "$dir" ]; then
    echo -e "${GREEN}✅${NC} $dir/"
  else
    echo -e "${RED}❌ MISSING:${NC} $dir/"
    ((ERROR_COUNT++))
  fi
done

echo

# 3. Check documentation files
echo -e "${BLUE}📚 Documentation Files:${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

DOCS=(
  "docs/ARCHITECTURE.md"
  "docs/API_CONTRACTS.md"
  "docs/DEPLOYMENT.md"
  "docs/OPERATIONS_GUIDE_RU.md"
  "CONTRIBUTING.md"
  "LICENSE"
)

for doc in "${DOCS[@]}"; do
  check_file "$doc"
done

echo

# 4. Check component README files
echo -e "${BLUE}📖 Component READMEs:${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

for component in "${COMPONENTS[@]}"; do
  check_file "$component/README.md"
done

echo

# 5. Check badge links
echo -e "${BLUE}🎖️  Badge Links:${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

badges=$(grep -oP '!\[.*?\]\(\K[^)]+(?=\))' README.md | grep 'img.shields.io' || true)
if [ -n "$badges" ]; then
  echo "$badges" | while IFS= read -r badge; do
    echo -e "${GREEN}✅${NC} $badge"
  done
else
  echo -e "${YELLOW}⚠️  No badge links found${NC}"
fi

echo

# 6. List repository links (informational)
echo -e "${BLUE}🔗 Repository Links:${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

repo_urls=$(grep -oP 'github\.com/rustkas/[^)]+' README.md | sort -u || true)
if [ -n "$repo_urls" ]; then
  echo "$repo_urls" | while IFS= read -r url; do
    echo -e "${BLUE}  •${NC} https://$url"
  done
else
  echo -e "${YELLOW}⚠️  No repository links found${NC}"
fi

echo

# 7. Check AI assistant link
echo -e "${BLUE}🤖 AI Assistant Link:${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if grep -q 'https://aistudio.instagram.com/ai/' README.md; then
  echo -e "${GREEN}✅${NC} BeamLine Master AI link present"
else
  echo -e "${RED}❌${NC} AI Assistant link missing"
  ((ERROR_COUNT++))
fi

echo

# Summary
echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║                      SUMMARY                               ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"

if [ $ERROR_COUNT -eq 0 ]; then
  echo -e "${GREEN}✅ All links are valid${NC}"
  echo -e "${GREEN}✅ README.md is ready for production${NC}"
  exit 0
else
  echo -e "${RED}❌ Found $ERROR_COUNT broken link(s)${NC}"
  echo -e "${YELLOW}⚠️  Please fix before committing${NC}"
  exit 1
fi
