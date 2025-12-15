# Infrastructure Validation Script (PowerShell)
# Validates infrastructure configuration files
# Exit codes: 0 = success, 2 = external errors, 3 = local errors (WARN does not cause exit 2)

$ErrorActionPreference = "Stop"
$script:Errors = 0
$script:Warnings = 0
$script:ExitCode = 0

# Get script directory and project root
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent (Split-Path -Parent $ScriptDir)

# Normalize path separators (support both / and \)
function Normalize-Path {
    param([string]$Path)
    return $Path -replace '\\', [System.IO.Path]::DirectorySeparatorChar -replace '/', [System.IO.Path]::DirectorySeparatorChar
}

# Log directory
$LogDir = Join-Path $ProjectRoot "reports\dry-run-logs\infra"
$LogFile = Join-Path $LogDir "validation.log"

# Ensure log directory exists
if (-not (Test-Path $LogDir)) {
    New-Item -ItemType Directory -Path $LogDir -Force | Out-Null
}

# Mask function
function Mask {
    param([string]$s)
    if ([string]::IsNullOrWhiteSpace($s)) { return $s }
    return ($s -replace '([A-Za-z0-9+/]{16,})', '[MASKED]')
}

function Write-Log {
    param([string]$Message, [string]$Level = "INFO")
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "[$timestamp] [$Level] $Message"
    Add-Content -Path $LogFile -Value $logMessage
    switch ($Level) {
        "ERROR" { Write-Host $logMessage -ForegroundColor Red; $script:Errors++ }
        "WARN" { Write-Host $logMessage -ForegroundColor Yellow; $script:Warnings++ }
        "INFO" { Write-Host $logMessage -ForegroundColor Green }
        default { Write-Host $logMessage }
    }
}

# Check required files
function Test-RequiredFiles {
    Write-Log "Checking required infrastructure files..."
    
    $requiredFiles = @(
        @{ Path = "infra/compose/local-dev.yml"; Name = "Docker Compose file" }
        @{ Path = "infra/docker/README.md"; Name = "Docker README" }
    )
    
    # Check environment template (either .env.example or ENV_TEMPLATE.md)
    # Normalize paths to support both / and \
    $envExample = Join-Path $ProjectRoot (Normalize-Path "config/env/.env.example")
    $envTemplate = Join-Path $ProjectRoot (Normalize-Path "config/env/ENV_TEMPLATE.md")
    if (Test-Path $envExample) {
        Write-Log "Found: Environment template - config\env\.env.example"
    } elseif (Test-Path $envTemplate) {
        Write-Log "Found: Environment template - config\env\ENV_TEMPLATE.md"
    } else {
        Write-Log "Missing: Environment template (config\env\.env.example or ENV_TEMPLATE.md)" "ERROR"
        $script:ExitCode = 3
    }
    
    foreach ($file in $requiredFiles) {
        $normalizedPath = Normalize-Path $file.Path
        $fullPath = Join-Path $ProjectRoot $normalizedPath
        if (Test-Path $fullPath) {
            Write-Log "Found: $($file.Name) - $($file.Path)"
        } else {
            Write-Log "Missing: $($file.Name) - $($file.Path)" "ERROR"
            $script:ExitCode = 3
        }
    }
}

# Validate docker-compose syntax
function Test-ComposeSyntax {
    Write-Log "Validating docker-compose syntax..."
    
    $composePath = Normalize-Path "infra/compose/local-dev.yml"
    $composeFile = Join-Path $ProjectRoot $composePath
    if (-not (Test-Path $composeFile)) {
        Write-Log "Compose file not found" "ERROR"
        $script:ExitCode = 3
        return
    }
    
    try {
        if (-not (Get-Command docker -ErrorAction SilentlyContinue)) {
            Write-Log "Docker not found, skipping compose syntax check" "WARN"
            return
        }
        
        $composeArgs = @("compose", "-f", $composeFile, "config")
        $result = & docker $composeArgs 2>&1
        $exitCode = $LASTEXITCODE
        
        if ($exitCode -eq 0) {
            Write-Log "Docker Compose syntax is valid"
        } else {
            Write-Log "Docker Compose syntax validation failed" "ERROR"
            Write-Log (Mask ($result -join "`n")) "ERROR"
            $script:ExitCode = 3
        }
    } catch {
        Write-Log "Error running docker compose: $($_.Exception.Message)" "ERROR"
        $script:ExitCode = 2
    }
}

# Check port conflicts
function Test-PortConflicts {
    Write-Log "Checking for port conflicts..."
    
    $composeFile = Join-Path $ProjectRoot (Normalize-Path "infra/compose/local-dev.yml")
    if (-not (Test-Path $composeFile)) {
        return
    }
    
    try {
        $content = Get-Content $composeFile -Raw
        $portPattern = '(\d+):(\d+)'
        $matches = [regex]::Matches($content, $portPattern)
        
        $ports = @{}
        foreach ($match in $matches) {
            $hostPort = $match.Groups[1].Value
            if ($ports.ContainsKey($hostPort)) {
                Write-Log "Port conflict detected: $hostPort is used multiple times" "ERROR"
                $script:ExitCode = 3
            } else {
                $ports[$hostPort] = $true
            }
        }
        
        if ($ports.Count -gt 0) {
            Write-Log "Found $($ports.Count) unique host ports, no conflicts"
        }
    } catch {
        Write-Log "Error checking port conflicts: $($_.Exception.Message)" "WARN"
    }
}

# Validate environment variables
function Test-EnvironmentVariables {
    Write-Log "Validating environment variables..."
    
    # Check for environment template (either .env.example or ENV_TEMPLATE.md)
    # Normalize paths to support both / and \
    $envExample = Join-Path $ProjectRoot (Normalize-Path "config/env/.env.example")
    $envTemplate = Join-Path $ProjectRoot (Normalize-Path "config/env/ENV_TEMPLATE.md")
    $envFile = $null
    
    if (Test-Path $envExample) {
        $envFile = $envExample
    } elseif (Test-Path $envTemplate) {
        $envFile = $envTemplate
    } else {
        Write-Log "Environment template file not found (.env.example or ENV_TEMPLATE.md)" "ERROR"
        $script:ExitCode = 3
        return
    }
    
    try {
        $content = Get-Content $envFile -Raw
        
        # Check for placeholder values
        $placeholderPattern = 'PLACEHOLDER_\w+'
        $placeholders = [regex]::Matches($content, $placeholderPattern)
        
        if ($placeholders.Count -gt 0) {
            Write-Log "Found $($placeholders.Count) placeholder values (expected)"
        } else {
            Write-Log "No placeholder values found" "WARN"
        }
        
        # Check for potential secrets (only ERROR, not WARN)
        $secretPatterns = @(
            'sk_[A-Za-z0-9]{24,}',
            'eyJ[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}\.[A-Za-z0-9_-]{10,}',
            '[A-Za-z0-9+/]{32,}=?'
        )
        
        $secretsFound = $false
        foreach ($pattern in $secretPatterns) {
            if ($content -match $pattern) {
                Write-Log "Potential secret detected (masked): $pattern" "ERROR"
                Write-Log (Mask $matches[0]) "ERROR"
                $secretsFound = $true
                $script:ExitCode = 3
            }
        }
        
        if (-not $secretsFound) {
            Write-Log "No real secrets detected in environment file"
        }
    } catch {
        Write-Log "Error validating environment variables: $($_.Exception.Message)" "ERROR"
        $script:ExitCode = 3
    }
}

# Main execution
Write-Log "Starting infrastructure validation..."
Write-Log "Project root: $ProjectRoot"
Write-Log "Log file: $LogFile"

Test-RequiredFiles
Test-ComposeSyntax
Test-PortConflicts
Test-EnvironmentVariables

# Summary
Write-Log "Validation complete"
Write-Log "Errors: $script:Errors, Warnings: $script:Warnings"

# Exit code logic: WARN does not cause exit 2, only ERROR causes exit 3
if ($script:Errors -eq 0) {
    Write-Log "All checks passed"
    exit 0
} elseif ($script:ExitCode -eq 2) {
    Write-Log "External errors occurred" "ERROR"
    exit 2
} else {
    Write-Log "Local problems detected" "ERROR"
    exit 3
}
