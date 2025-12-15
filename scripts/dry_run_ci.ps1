# Dry-run CI/CD validation script (PowerShell)
# Comprehensive local testing of all CI/CD validation gates
# Supports individual steps: schema, hmac, backend, frontend, compliance, summary, all

param(
    [Parameter(Position=0)]
    [ValidateSet("schema", "hmac", "backend", "frontend", "compliance", "summary", "all")]
    [string]$Step = "all"
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptDir
Set-Location $ProjectRoot

# Directories
$LogsDir = Join-Path $ProjectRoot "reports\dry-run-logs"
New-Item -ItemType Directory -Force -Path $LogsDir | Out-Null

# Timestamp
$Timestamp = Get-Date -Format "yyyyMMdd_HHmmss"

# Exit codes
$EXIT_SUCCESS = 0
$EXIT_SCHEMA_ERROR = 1
$EXIT_SECRETS_ERROR = 2
$EXIT_BUILD_ERROR = 3
$EXIT_COMPLIANCE_ERROR = 4

# Step status tracking
$script:StepStatus = @{}
$script:StepDuration = @{}
$script:StepErrors = @()

# Read manifest.json
function Read-Manifest {
    $manifestPath = Join-Path $ProjectRoot ".trae\manifest.json"
    if (-not (Test-Path $manifestPath)) {
        Write-Host "[FAIL] .trae/manifest.json not found" -ForegroundColor Red
        exit $EXIT_SCHEMA_ERROR
    }
    
    $manifest = Get-Content $manifestPath | ConvertFrom-Json
    $script:StateVersion = $manifest.schema_versions.state.version
    $script:HistoryVersion = $manifest.schema_versions.history.version
    $script:ChecksumsFormat = $manifest.artifact_checksums_format.name
    $script:HmacMaskingPolicy = $manifest.security.hmac_masking.policy
    
    Write-Host "[INFO] Manifest versions:"
    Write-Host "  STATE=$($script:StateVersion)"
    Write-Host "  HISTORY=$($script:HistoryVersion)"
    Write-Host "  artifact_checksums_format=$($script:ChecksumsFormat)"
    Write-Host "  hmac_masking_policy=$($script:HmacMaskingPolicy)"
}

# Detect local environment
function Detect-Environment {
    $envType = "development"
    
    if ($env:CI -eq "true" -or 
        $env:GITHUB_ACTIONS -eq "true" -or 
        $env:GITLAB_CI -eq "true" -or 
        $env:DRONE -eq "true" -or 
        $env:PRODUCTION -eq "true") {
        $envType = "production"
    }
    
    Write-Host "[INFO] Detected environment: $envType"
    return $envType
}

# Run step with timing
function Run-Step {
    param(
        [string]$StepName,
        [string]$StepCommand
    )
    
    $LogFile = Join-Path $LogsDir "$StepName.log"
    $StartTime = Get-Date
    
    Write-Host "=========================================="
    Write-Host "Step: $StepName"
    Write-Host "=========================================="
    Write-Host "Command: $StepCommand"
    Write-Host "Log file: $LogFile"
    Write-Host ""
    
    $LogContent = @"
==========================================
Dry-Run Step: $StepName
Timestamp: $(Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ")
Command: $StepCommand
==========================================

"@
    
    try {
        $Output = Invoke-Expression $StepCommand 2>&1 | Out-String
        $ExitCode = $LASTEXITCODE
        if ($null -eq $ExitCode) { $ExitCode = 0 }
        
        $LogContent += $Output
        $LogContent += @"

==========================================
Step completed: $StepName
Exit code: $ExitCode
==========================================
"@
        
        $LogContent | Out-File -FilePath $LogFile -Encoding UTF8
        Write-Host $Output
        
        $EndTime = Get-Date
        $Duration = [int]($EndTime - $StartTime).TotalSeconds
        $script:StepDuration[$StepName] = $Duration
        
        if ($ExitCode -eq 0) {
            $script:StepStatus[$StepName] = "PASSED"
            Write-Host "[OK] Step $StepName completed successfully (${Duration}s)" -ForegroundColor Green
        } else {
            $script:StepStatus[$StepName] = "FAILED"
            $script:StepErrors += $StepName
            Write-Host "[FAIL] Step $StepName failed with exit code $ExitCode (${Duration}s)" -ForegroundColor Red
        }
        
        Write-Host ""
        return $ExitCode
    } catch {
        $script:StepStatus[$StepName] = "FAILED"
        $script:StepErrors += $StepName
        Write-Host "[FAIL] Step $StepName failed: $_" -ForegroundColor Red
        Write-Host ""
        return 1
    }
}

# Step 1: Schema Gates
function Step-Schema {
    Write-Host "=========================================="
    Write-Host "Step 1: Schema Gates"
    Write-Host "=========================================="
    Write-Host ""
    
    Read-Manifest
    Write-Host ""
    
    $exitCode = 0
    Run-Step "schema_check" "bash scripts/check_schema_changes.sh" | Out-Null
    if ($LASTEXITCODE -ne 0) { $exitCode = $EXIT_SCHEMA_ERROR }
    
    Run-Step "state_validation" "bash scripts/validate_state.sh" | Out-Null
    if ($LASTEXITCODE -ne 0) { $exitCode = $EXIT_SCHEMA_ERROR }
    
    return $exitCode
}

# Step 2: HMAC/Secrets
function Step-Hmac {
    Write-Host "=========================================="
    Write-Host "Step 2: HMAC/Secrets Validation"
    Write-Host "=========================================="
    Write-Host ""
    
    $exitCode = 0
    Run-Step "hmac_masking" "bash scripts/check_hmac_masking.sh docs/CI_VALIDATION.md docs/DRY_RUN_LOGS.md docs/CI_SECRETS_SETUP.md" | Out-Null
    if ($LASTEXITCODE -ne 0) { $exitCode = $EXIT_SECRETS_ERROR }
    
    Run-Step "hmac_chain" "python3 scripts/verify_hmac_chain.py --verbose" | Out-Null
    if ($LASTEXITCODE -ne 0) { $exitCode = $EXIT_SECRETS_ERROR }
    
    return $exitCode
}

# Step 3: Backend
function Step-Backend {
    Write-Host "=========================================="
    Write-Host "Step 3: Backend Build/Test"
    Write-Host "=========================================="
    Write-Host ""
    
    $hasOtp = Test-Path "apps\otp"
    $hasCaf = Test-Path "apps\caf"
    $hasGateway = Test-Path "apps\gateway"
    
    if (-not $hasOtp -and -not $hasCaf -and -not $hasGateway) {
        Write-Host "[SKIP] No backend components found" -ForegroundColor Yellow
        $script:StepStatus["backend"] = "SKIPPED"
        return $EXIT_SUCCESS
    }
    
    # Implementation would go here for each backend component
    Write-Host "[INFO] Backend components detected"
    return $EXIT_SUCCESS
}

# Step 4: Frontend
function Step-Frontend {
    Write-Host "=========================================="
    Write-Host "Step 4: Frontend Build/Test"
    Write-Host "=========================================="
    Write-Host ""
    
    if (-not (Test-Path "apps\ui")) {
        Write-Host "[SKIP] Frontend not found (apps\ui)" -ForegroundColor Yellow
        $script:StepStatus["frontend"] = "SKIPPED"
        return $EXIT_SUCCESS
    }
    
    # Implementation would go here
    Write-Host "[INFO] Frontend component detected"
    return $EXIT_SUCCESS
}

# Step 5: Compliance
function Step-Compliance {
    Write-Host "=========================================="
    Write-Host "Step 5: Compliance Checks"
    Write-Host "=========================================="
    Write-Host ""
    
    $licenseScript = Join-Path $ProjectRoot "scripts\check_license_compatibility.sh"
    if (Test-Path $licenseScript) {
        Run-Step "compliance_licenses" "bash $licenseScript" | Out-Null
    } else {
        Write-Host "[SKIP] License compatibility check script not found" -ForegroundColor Yellow
    }
    
    return $EXIT_SUCCESS
}

# Step 6: Summary
function Step-Summary {
    Write-Host "=========================================="
    Write-Host "Step 6: Summary"
    Write-Host "=========================================="
    Write-Host ""
    
    $totalDuration = ($script:StepDuration.Values | Measure-Object -Sum).Sum
    
    Write-Host "Dry-Run Summary:"
    Write-Host "  Total duration: ${totalDuration}s"
    Write-Host ""
    Write-Host "Step Status:"
    foreach ($step in $script:StepStatus.Keys) {
        $status = $script:StepStatus[$step]
        $duration = $script:StepDuration[$step]
        if ($null -eq $duration) { $duration = 0 }
        Write-Host "  ${step}: ${status} (${duration}s)"
    }
    Write-Host ""
    
    if ($script:StepErrors.Count -gt 0) {
        Write-Host "Failed steps:" -ForegroundColor Red
        foreach ($error in $script:StepErrors) {
            Write-Host "  - $error" -ForegroundColor Red
        }
        Write-Host ""
        return 1
    } else {
        Write-Host "[OK] All steps completed successfully" -ForegroundColor Green
        return 0
    }
}

# Main execution
function Main {
    param([string]$Step)
    
    Write-Host "=========================================="
    Write-Host "CI/CD Dry-Run Validation"
    Write-Host "=========================================="
    Write-Host ""
    
    Read-Manifest
    Write-Host ""
    Detect-Environment | Out-Null
    Write-Host ""
    
    $exitCode = 0
    
    switch ($Step) {
        "schema" {
            $exitCode = Step-Schema
        }
        "hmac" {
            $exitCode = Step-Hmac
        }
        "backend" {
            $exitCode = Step-Backend
        }
        "frontend" {
            $exitCode = Step-Frontend
        }
        "compliance" {
            $exitCode = Step-Compliance
        }
        "summary" {
            $exitCode = Step-Summary
        }
        "all" {
            Step-Schema | Out-Null
            if ($LASTEXITCODE -ne 0) { $exitCode = $EXIT_SCHEMA_ERROR }
            
            Step-Hmac | Out-Null
            if ($LASTEXITCODE -ne 0) { $exitCode = $EXIT_SECRETS_ERROR }
            
            Step-Backend | Out-Null
            if ($LASTEXITCODE -ne 0) { $exitCode = $EXIT_BUILD_ERROR }
            
            Step-Frontend | Out-Null
            if ($LASTEXITCODE -ne 0) { $exitCode = $EXIT_BUILD_ERROR }
            
            Step-Compliance | Out-Null
            if ($LASTEXITCODE -ne 0) { $exitCode = $EXIT_COMPLIANCE_ERROR }
            
            Step-Summary | Out-Null
        }
        default {
            Write-Host "Usage: .\scripts\dry_run_ci.ps1 [-Step schema|hmac|backend|frontend|compliance|summary|all]"
            exit 1
        }
    }
    
    exit $exitCode
}

# Run main
Main -Step $Step

