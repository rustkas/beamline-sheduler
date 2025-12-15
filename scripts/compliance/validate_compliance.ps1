# validate_compliance.ps1
# Compliance validation script for Beamline Constructor
# Checks file presence, structure, and absence of real secrets
# Exit codes: 0=OK, 2=External broken, 3=Local broken

param(
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"

# Directories
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Resolve-Path (Join-Path $ScriptDir "..\..")
$LogsDir = Join-Path $ProjectRoot "reports\dry-run-logs\compliance"

# Ensure logs directory exists
New-Item -ItemType Directory -Force -Path $LogsDir | Out-Null

$LogFile = Join-Path $LogsDir "validation.log"
$SummaryFile = Join-Path $LogsDir "summary.json"

# Exit codes
$EXIT_OK = 0
$EXIT_EXTERNAL_BROKEN = 2
$EXIT_LOCAL_BROKEN = 3

# Counters
$script:FILES_CHECKED = 0
$script:SECRETS_MASKED = 0
$script:EXIT_CODE = $EXIT_OK

# Logging function
function Write-Log {
    param(
        [string]$Level,
        [string]$Message
    )
    
    $timestamp = Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ"
    $logMessage = "[$timestamp] [$Level] $Message"
    
    Write-Host $logMessage
    Add-Content -Path $LogFile -Value $logMessage
}

# Initialize log file
@"
==========================================
Compliance Validation Log
Started: $(Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ")
==========================================

"@ | Out-File -FilePath $LogFile -Encoding UTF8

Write-Log "INFO" "Starting compliance validation"

# Check required files
function Test-Structure {
    Write-Log "INFO" "Checking compliance structure..."
    
    $requiredFiles = @(
        "compliance\licenses\REGISTRY.md",
        "compliance\privacy\POLICY.md",
        "compliance\sbom\SBOM.template.json"
    )
    
    $missingFiles = @()
    
    foreach ($file in $requiredFiles) {
        $fullPath = Join-Path $ProjectRoot $file
        if (-not (Test-Path $fullPath)) {
            $missingFiles += $file
            Write-Log "ERROR" "Missing required file: $file"
        } else {
            $script:FILES_CHECKED++
            Write-Log "INFO" "Found: $file"
        }
    }
    
    if ($missingFiles.Count -gt 0) {
        Write-Log "ERROR" "Missing $($missingFiles.Count) required file(s)"
        $script:EXIT_CODE = $EXIT_LOCAL_BROKEN
        return $false
    }
    
    Write-Log "INFO" "All required files present"
    return $true
}

# Check basic structure of files
function Test-FileStructure {
    Write-Log "INFO" "Checking file structure..."
    
    $errors = 0
    
    # Check REGISTRY.md has license information
    $registryFile = Join-Path $ProjectRoot "compliance\licenses\REGISTRY.md"
    if (Test-Path $registryFile) {
        $content = Get-Content $registryFile -Raw
        if ($content -notmatch "Apache-2.0|MIT|allowlist|denylist") {
            Write-Log "ERROR" "REGISTRY.md missing license information"
            $errors++
        } else {
            Write-Log "INFO" "REGISTRY.md structure OK"
        }
    }
    
    # Check POLICY.md has privacy information
    $policyFile = Join-Path $ProjectRoot "compliance\privacy\POLICY.md"
    if (Test-Path $policyFile) {
        $content = Get-Content $policyFile -Raw
        if ($content -notmatch "PII|privacy|logging|retention") {
            Write-Log "ERROR" "POLICY.md missing privacy information"
            $errors++
        } else {
            Write-Log "INFO" "POLICY.md structure OK"
        }
    }
    
    # Check SBOM.template.json is valid JSON
    $sbomFile = Join-Path $ProjectRoot "compliance\sbom\SBOM.template.json"
    if (Test-Path $sbomFile) {
        try {
            $json = Get-Content $sbomFile -Raw | ConvertFrom-Json
            
            # Check for required fields
            if (-not $json.name -or -not $json.version -or -not $json.components) {
                Write-Log "ERROR" "SBOM.template.json missing required fields (name, version, components)"
                $errors++
            } else {
                # Check components have required fields (name, version, license)
                $componentsOk = $true
                for ($i = 0; $i -lt $json.components.Count; $i++) {
                    $component = $json.components[$i]
                    if (-not $component.name -or -not $component.version -or -not $component.license) {
                        Write-Log "ERROR" "SBOM component at index $i missing required fields (name, version, license)"
                        $componentsOk = $false
                        $errors++
                    }
                }
                if ($componentsOk) {
                    Write-Log "INFO" "SBOM.template.json structure OK (all components have name, version, license)"
                }
            }
        } catch {
            Write-Log "ERROR" "SBOM.template.json is not valid JSON: $($_.Exception.Message)"
            $errors++
        }
    }
    
    if ($errors -gt 0) {
        $script:EXIT_CODE = $EXIT_LOCAL_BROKEN
        return $false
    }
    
    return $true
}

# Check for secrets
function Test-Secrets {
    Write-Log "INFO" "Checking for secrets in compliance artifacts..."
    
    $complianceFiles = @(
        "compliance\licenses\REGISTRY.md",
        "compliance\privacy\POLICY.md",
        "compliance\sbom\SBOM.template.json"
    )
    
    $secretPatterns = @(
        "(?i)password\s*=\s*(?!PLACEHOLDER|EXAMPLE|TEST|YOUR)[a-zA-Z0-9_\-]{8,}",
        "sk_live_[a-zA-Z0-9]{32,}",
        "sk_test_[a-zA-Z0-9]{32,}",
        "-----BEGIN.*PRIVATE KEY-----",
        "ghp_[a-zA-Z0-9]{36,}",
        "(?i)api[_-]?key\s*=\s*(?!PLACEHOLDER|EXAMPLE|TEST|YOUR)[a-zA-Z0-9_\-]{16,}",
        "(?i)secret\s*=\s*(?!PLACEHOLDER|EXAMPLE|TEST|YOUR)[a-zA-Z0-9_\-]{12,}",
        "(?i)token\s*=\s*(?!PLACEHOLDER|EXAMPLE|TEST|YOUR)[a-zA-Z0-9_\-]{16,}"
    )
    
    $foundSecrets = $false
    
    foreach ($file in $complianceFiles) {
        $fullPath = Join-Path $ProjectRoot $file
        if (-not (Test-Path $fullPath)) {
            continue
        }
        
        $content = Get-Content $fullPath -Raw
        
        foreach ($pattern in $secretPatterns) {
            if ($content -match $pattern) {
                # Check if it's a placeholder
                if ($content -notmatch "PLACEHOLDER|EXAMPLE|TEST|YOUR|MASKED|REDACTED|template|placeholder") {
                    Write-Log "ERROR" "Potential secret found in $file (pattern: $pattern)"
                    $foundSecrets = $true
                    $script:EXIT_CODE = $EXIT_LOCAL_BROKEN
                }
            }
        }
    }
    
    if (-not $foundSecrets) {
        $script:SECRETS_MASKED = 1
        Write-Log "INFO" "No secrets detected in compliance artifacts"
    } else {
        Write-Log "ERROR" "Secrets detected - all secrets must be masked"
    }
    
    return $true
}

# Check links
function Test-Links {
    Write-Log "INFO" "Checking links..."
    
    $linkChecker = Join-Path $ProjectRoot "scripts\check_links_v2.ps1"
    
    if (Test-Path $linkChecker) {
        Write-Log "INFO" "Running link checker: scripts\check_links_v2.ps1"
        
        try {
            $result = & $linkChecker 2>&1
            $result | ForEach-Object { Write-Log "INFO" $_ }
            
            if ($LASTEXITCODE -ne 0) {
                Write-Log "ERROR" "Link check failed with exit code: $LASTEXITCODE"
                $script:EXIT_CODE = $EXIT_EXTERNAL_BROKEN
                return $false
            }
        } catch {
            Write-Log "ERROR" "Link check failed: $($_.Exception.Message)"
            $script:EXIT_CODE = $EXIT_EXTERNAL_BROKEN
            return $false
        }
    } else {
        Write-Log "WARN" "check_links_v2.ps1 not found, skipping link check"
    }
    
    return $true
}

# Generate summary JSON
function Write-Summary {
    Write-Log "INFO" "Generating summary..."
    
    $summary = @{
        files_checked = $script:FILES_CHECKED
        secrets_masked = $script:SECRETS_MASKED
        exit_code = $script:EXIT_CODE
        timestamp = Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ"
    } | ConvertTo-Json
    
    $summary | Out-File -FilePath $SummaryFile -Encoding UTF8
    Write-Log "INFO" "Summary saved to: $SummaryFile"
}

# Main execution
function Main {
    Write-Log "INFO" "=========================================="
    Write-Log "INFO" "Compliance Validation"
    Write-Log "INFO" "=========================================="
    Write-Log "INFO" ""
    
    Test-Structure | Out-Null
    Test-FileStructure | Out-Null
    Test-Secrets | Out-Null
    Test-Links | Out-Null
    
    Write-Summary
    
    Write-Log "INFO" ""
    Write-Log "INFO" "=========================================="
    Write-Log "INFO" "Validation completed"
    Write-Log "INFO" "Exit code: $($script:EXIT_CODE)"
    Write-Log "INFO" "=========================================="
    
    exit $script:EXIT_CODE
}

# Run main
Main
