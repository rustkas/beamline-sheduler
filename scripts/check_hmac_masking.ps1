# HMAC Masking Check Script (PowerShell)
# Verifies HMAC values are masked in documentation
# Usage: pwsh -File scripts/check_hmac_masking.ps1 [files...]
# Exit codes: 0 = OK, 2 = masking violation, 1 = error

param(
    [string[]]$Files = @("docs/CI_VALIDATION.md", "docs/DRY_RUN_LOGS.md", "docs/CI_SECRETS_SETUP.md")
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptDir
Set-Location $ProjectRoot

# Configuration from manifest
$ManifestFile = ".trae/manifest.json"
$ReportDir = "reports/dry-run-logs"
$ReportFile = Join-Path $ReportDir "hmac_masking.log"
$Timestamp = (Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ")

# Create report directory
New-Item -ItemType Directory -Force -Path $ReportDir | Out-Null

# Load configuration from manifest
Write-Host "=========================================="
Write-Host "HMAC Masking Sanity Check"
Write-Host "=========================================="
Write-Host ""

Write-Host "[INFO] Reading configuration from manifest..."
$manifest = Get-Content $ManifestFile | ConvertFrom-Json
$HmacMaskingPolicy = $manifest.security.hmac_masking.policy
Write-Host "[INFO] HMAC Masking Policy: $HmacMaskingPolicy" | Tee-Object -FilePath $ReportFile -Append
Write-Host "" | Tee-Object -FilePath $ReportFile -Append

# Patterns
$FullHmacPattern = "[0-9a-f]{64}"
$MaskedHmacPattern = "[0-9a-f]{16}\.\.\."

$Errors = 0
$Warnings = 0

# Initialize report
@"
==========================================
HMAC Masking Check Report
Timestamp: $Timestamp
Policy: $HmacMaskingPolicy
==========================================

"@ | Out-File -FilePath $ReportFile -Encoding UTF8

foreach ($docFile in $Files) {
    if (-not (Test-Path $docFile)) {
        Write-Host "[WARN] File not found: $docFile" -ForegroundColor Yellow | Tee-Object -FilePath $ReportFile -Append
        $Warnings++
        continue
    }
    
    Write-Host "Checking: $docFile" | Tee-Object -FilePath $ReportFile -Append
    
    $content = Get-Content $docFile -Raw
    
    # Find full HMAC values (64 hex chars)
    $fullHmacs = [regex]::Matches($content, $FullHmacPattern)
    
    # Filter out comments and metadata
    $filteredHmacs = @()
    foreach ($match in $fullHmacs) {
        $lineNum = ($content.Substring(0, $match.Index) -split "`n").Count
        $line = ($content -split "`n")[$lineNum - 1]
        
        # Skip if in comment
        if ($line -match "^\s*#|^\s*\*|^\s*//") {
            continue
        }
        
        $filteredHmacs += @{
            Line = $lineNum
            Value = $match.Value
            Context = $line.Trim()
        }
    }
    
    if ($filteredHmacs.Count -gt 0) {
        Write-Host "[FAIL] Found unmasked HMAC values in $docFile`:" -ForegroundColor Red | Tee-Object -FilePath $ReportFile -Append
        foreach ($hmac in $filteredHmacs) {
            $masked = $hmac.Value.Substring(0, 16) + "..."
            Write-Host "  Line $($hmac.Line): $masked" | Tee-Object -FilePath $ReportFile -Append
        }
        $Errors++
    }
    else {
        Write-Host "[OK] No unmasked HMAC values found" -ForegroundColor Green | Tee-Object -FilePath $ReportFile -Append
    }
    
    # Check for masked HMACs
    $maskedHmacs = [regex]::Matches($content, $MaskedHmacPattern)
    if ($maskedHmacs.Count -gt 0) {
        Write-Host "[OK] Found $($maskedHmacs.Count) masked HMAC value(s)" -ForegroundColor Green | Tee-Object -FilePath $ReportFile -Append
    }
    
    Write-Host "" | Tee-Object -FilePath $ReportFile -Append
}

# Summary
Write-Host "==========================================" | Tee-Object -FilePath $ReportFile -Append
if ($Errors -eq 0) {
    Write-Host "[OK] Sanity check passed: All HMAC values are properly masked" -ForegroundColor Green | Tee-Object -FilePath $ReportFile -Append
    Write-Host "Report saved to: $ReportFile" | Tee-Object -FilePath $ReportFile -Append
    exit 0
}
else {
    Write-Host "[FAIL] Sanity check failed: Found $Errors file(s) with unmasked HMAC values" -ForegroundColor Red | Tee-Object -FilePath $ReportFile -Append
    if ($Warnings -gt 0) {
        Write-Host "[WARN] $Warnings warning(s)" -ForegroundColor Yellow | Tee-Object -FilePath $ReportFile -Append
    }
    Write-Host "Report saved to: $ReportFile" | Tee-Object -FilePath $ReportFile -Append
    exit 2
}

