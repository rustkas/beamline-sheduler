#!/bin/bash
# Generate Unified QA Dashboard
#
# This script aggregates test results, coverage, visual regression, and property test data
# into a single HTML dashboard for easy review.
#
# Usage:
#   bash scripts/generate_qa_dashboard.sh
#   bash scripts/generate_qa_dashboard.sh --output reports/qa-dashboard.html

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
OUTPUT_FILE="${1:-$PROJECT_ROOT/reports/qa-dashboard.html}"
REPORTS_DIR="$PROJECT_ROOT/reports"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${GREEN}Generating Unified QA Dashboard...${NC}"

# Create reports directory
mkdir -p "$(dirname "$OUTPUT_FILE")"
mkdir -p "$REPORTS_DIR"

# Collect data from various sources
COVERAGE_JSON="${COVERAGE_JSON:-$PROJECT_ROOT/apps/ui_web/cover/excoveralls.json}"
PROPERTY_TEST_OUTPUT="${PROPERTY_TEST_OUTPUT:-$PROJECT_ROOT/apps/ui_web/property_test_output.txt}"
E2E_RESULTS="${E2E_RESULTS:-$PROJECT_ROOT/apps/ui_web/test/e2e/test-results/results.json}"

# Extract coverage percentage
COVERAGE_PERCENT="N/A"
if [ -f "$COVERAGE_JSON" ] && command -v jq &> /dev/null; then
  COVERAGE_PERCENT=$(jq -r '.coverage // 0' "$COVERAGE_JSON" 2>/dev/null || echo "N/A")
fi

# Extract property test results
PROPERTY_TESTS_PASSED="N/A"
PROPERTY_TESTS_FAILED="N/A"
PROPERTY_SEED="N/A"
if [ -f "$PROPERTY_TEST_OUTPUT" ]; then
  PROPERTY_TESTS_PASSED=$(grep -oP '\d+(?=\s+properties, 0 failures)' "$PROPERTY_TEST_OUTPUT" | head -1 || echo "N/A")
  PROPERTY_TESTS_FAILED=$(grep -oP '\d+(?=\s+properties, \d+ failures)' "$PROPERTY_TEST_OUTPUT" | head -1 || echo "0")
  PROPERTY_SEED=$(grep -oP 'seed:\s*\K[0-9]+' "$PROPERTY_TEST_OUTPUT" | tail -1 || echo "N/A")
fi

# Extract E2E test results
E2E_TESTS_PASSED="N/A"
E2E_TESTS_FAILED="N/A"
if [ -f "$E2E_RESULTS" ] && command -v jq &> /dev/null; then
  E2E_TESTS_PASSED=$(jq -r '.stats.expected // 0' "$E2E_RESULTS" 2>/dev/null || echo "N/A")
  E2E_TESTS_FAILED=$(jq -r '.stats.unexpected // 0' "$E2E_RESULTS" 2>/dev/null || echo "0")
fi

# Get current timestamp
TIMESTAMP=$(date -u +"%Y-%m-%d %H:%M:%S UTC")

# Generate HTML dashboard
cat > "$OUTPUT_FILE" << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Unified QA Dashboard</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }
        
        .container {
            max-width: 1400px;
            margin: 0 auto;
        }
        
        .header {
            background: white;
            border-radius: 12px;
            padding: 30px;
            margin-bottom: 20px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
        
        .header h1 {
            color: #333;
            font-size: 2.5em;
            margin-bottom: 10px;
        }
        
        .header .timestamp {
            color: #666;
            font-size: 0.9em;
        }
        
        .metrics-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 20px;
            margin-bottom: 20px;
        }
        
        .metric-card {
            background: white;
            border-radius: 12px;
            padding: 25px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
            transition: transform 0.2s;
        }
        
        .metric-card:hover {
            transform: translateY(-2px);
            box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
        }
        
        .metric-card h3 {
            color: #666;
            font-size: 0.9em;
            text-transform: uppercase;
            letter-spacing: 1px;
            margin-bottom: 10px;
        }
        
        .metric-value {
            font-size: 2.5em;
            font-weight: bold;
            color: #333;
            margin-bottom: 10px;
        }
        
        .metric-value.success {
            color: #10b981;
        }
        
        .metric-value.warning {
            color: #f59e0b;
        }
        
        .metric-value.error {
            color: #ef4444;
        }
        
        .metric-details {
            color: #666;
            font-size: 0.9em;
        }
        
        .section {
            background: white;
            border-radius: 12px;
            padding: 25px;
            margin-bottom: 20px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
        
        .section h2 {
            color: #333;
            font-size: 1.5em;
            margin-bottom: 20px;
            padding-bottom: 10px;
            border-bottom: 2px solid #e5e7eb;
        }
        
        .artifact-list {
            list-style: none;
        }
        
        .artifact-list li {
            padding: 12px;
            margin-bottom: 8px;
            background: #f9fafb;
            border-radius: 8px;
            border-left: 4px solid #667eea;
        }
        
        .artifact-list a {
            color: #667eea;
            text-decoration: none;
            font-weight: 500;
        }
        
        .artifact-list a:hover {
            text-decoration: underline;
        }
        
        .status-badge {
            display: inline-block;
            padding: 4px 12px;
            border-radius: 12px;
            font-size: 0.8em;
            font-weight: 600;
            margin-left: 10px;
        }
        
        .status-badge.success {
            background: #d1fae5;
            color: #065f46;
        }
        
        .status-badge.warning {
            background: #fef3c7;
            color: #92400e;
        }
        
        .status-badge.error {
            background: #fee2e2;
            color: #991b1b;
        }
        
        .footer {
            text-align: center;
            color: white;
            padding: 20px;
            font-size: 0.9em;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>📊 Unified QA Dashboard</h1>
            <div class="timestamp">Generated: TIMESTAMP_PLACEHOLDER</div>
        </div>
        
        <div class="metrics-grid">
            <div class="metric-card">
                <h3>Code Coverage</h3>
                <div class="metric-value COVERAGE_CLASS">COVERAGE_PLACEHOLDER</div>
                <div class="metric-details">Overall test coverage</div>
            </div>
            
            <div class="metric-card">
                <h3>Property Tests</h3>
                <div class="metric-value PROPERTY_CLASS">PROPERTY_PASSED_PLACEHOLDER</div>
                <div class="metric-details">Passed: PROPERTY_PASSED_PLACEHOLDER | Failed: PROPERTY_FAILED_PLACEHOLDER</div>
                <div class="metric-details" style="margin-top: 5px;">Seed: PROPERTY_SEED_PLACEHOLDER</div>
            </div>
            
            <div class="metric-card">
                <h3>E2E Tests</h3>
                <div class="metric-value E2E_CLASS">E2E_PASSED_PLACEHOLDER</div>
                <div class="metric-details">Passed: E2E_PASSED_PLACEHOLDER | Failed: E2E_FAILED_PLACEHOLDER</div>
            </div>
        </div>
        
        <div class="section">
            <h2>📦 Artifacts & Reports</h2>
            <ul class="artifact-list">
                <li>
                    <a href="#coverage">Coverage HTML Report</a>
                    <span class="status-badge success">Available</span>
                </li>
                <li>
                    <a href="#playwright">Playwright HTML Report</a>
                    <span class="status-badge success">Available</span>
                </li>
                <li>
                    <a href="#visual">Visual Regression Results</a>
                    <span class="status-badge success">Available</span>
                </li>
                <li>
                    <a href="#property">Property Test Output</a>
                    <span class="status-badge success">Available</span>
                </li>
            </ul>
        </div>
        
        <div class="section">
            <h2>🔗 Quick Links</h2>
            <ul class="artifact-list">
                <li>
                    <a href="https://github.com/${{ github.repository }}/actions">GitHub Actions</a>
                </li>
                <li>
                    <a href="https://github.com/${{ github.repository }}/pulls">Pull Requests</a>
                </li>
                <li>
                    <a href="apps/ui_web/docs/CI_IMPROVEMENTS.md">CI Documentation</a>
                </li>
            </ul>
        </div>
        
        <div class="footer">
            Generated by Unified QA Dashboard | UI-Web Testing Infrastructure
        </div>
    </div>
</body>
</html>
EOF

# Replace placeholders
sed -i "s/TIMESTAMP_PLACEHOLDER/$TIMESTAMP/g" "$OUTPUT_FILE"
sed -i "s/COVERAGE_PLACEHOLDER/${COVERAGE_PERCENT}%/g" "$OUTPUT_FILE"

# Determine coverage class
if [ "$COVERAGE_PERCENT" != "N/A" ]; then
  COVERAGE_NUM=$(echo "$COVERAGE_PERCENT" | tr -d '%')
  if (( $(echo "$COVERAGE_NUM >= 80" | bc -l 2>/dev/null || echo 0) )); then
    sed -i "s/COVERAGE_CLASS/success/g" "$OUTPUT_FILE"
  elif (( $(echo "$COVERAGE_NUM >= 60" | bc -l 2>/dev/null || echo 0) )); then
    sed -i "s/COVERAGE_CLASS/warning/g" "$OUTPUT_FILE"
  else
    sed -i "s/COVERAGE_CLASS/error/g" "$OUTPUT_FILE"
  fi
else
  sed -i "s/COVERAGE_CLASS//g" "$OUTPUT_FILE"
fi

# Property tests
sed -i "s/PROPERTY_PASSED_PLACEHOLDER/$PROPERTY_TESTS_PASSED/g" "$OUTPUT_FILE"
sed -i "s/PROPERTY_FAILED_PLACEHOLDER/$PROPERTY_TESTS_FAILED/g" "$OUTPUT_FILE"
sed -i "s/PROPERTY_SEED_PLACEHOLDER/$PROPERTY_SEED/g" "$OUTPUT_FILE"

if [ "$PROPERTY_TESTS_FAILED" != "N/A" ] && [ "$PROPERTY_TESTS_FAILED" != "0" ]; then
  sed -i "s/PROPERTY_CLASS/error/g" "$OUTPUT_FILE"
else
  sed -i "s/PROPERTY_CLASS/success/g" "$OUTPUT_FILE"
fi

# E2E tests
sed -i "s/E2E_PASSED_PLACEHOLDER/$E2E_TESTS_PASSED/g" "$OUTPUT_FILE"
sed -i "s/E2E_FAILED_PLACEHOLDER/$E2E_TESTS_FAILED/g" "$OUTPUT_FILE"

if [ "$E2E_TESTS_FAILED" != "N/A" ] && [ "$E2E_TESTS_FAILED" != "0" ]; then
  sed -i "s/E2E_CLASS/error/g" "$OUTPUT_FILE"
else
  sed -i "s/E2E_CLASS/success/g" "$OUTPUT_FILE"
fi

echo -e "${GREEN}✓ QA Dashboard generated: $OUTPUT_FILE${NC}"
echo -e "${YELLOW}Open in browser: file://$OUTPUT_FILE${NC}"

