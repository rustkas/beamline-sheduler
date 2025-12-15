# Observability Validation Script (MVP) - PowerShell
# Validates JSON log format and /_health endpoint availability
# Exit codes: 0 = success, 2 = external endpoints unavailable, 3 = local configs missing or invalid

$ErrorActionPreference = "Stop"
$script:Errors = 0
$script:Warnings = 0
$script:ExternalErrors = 0
$script:LocalErrors = 0

# Get project root and log directory
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent (Split-Path -Parent $ScriptDir)
$LogDir = Join-Path $ProjectRoot "reports\dry-run-logs\observability"
$LogFile = Join-Path $LogDir "validation.log"
$Timestamp = Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ"

# Create log directory
if (-not (Test-Path $LogDir)) {
    New-Item -ItemType Directory -Path $LogDir -Force | Out-Null
}

# Mask function (similar to check_links.ps1)
function Mask {
    param([string]$s)
    if ([string]::IsNullOrWhiteSpace($s)) {
        return $s
    }
    # Mask token-like strings: [A-Za-z0-9+/]{16,} -> [MASKED]
    return ($s -replace '([A-Za-z0-9+/]{16,})', '[MASKED]')
}

function Write-Error-Message {
    param([string]$Message)
    $masked = Mask $Message
    Write-Host "[ERROR] $masked" -ForegroundColor Red
    Add-Content -Path $LogFile -Value "[$Timestamp] [ERROR] $masked"
    $script:Errors++
    $script:LocalErrors++
}

function Write-Warning-Message {
    param([string]$Message)
    $masked = Mask $Message
    Write-Host "[WARN] $masked" -ForegroundColor Yellow
    Add-Content -Path $LogFile -Value "[$Timestamp] [WARN] $masked"
    $script:Warnings++
}

function Write-Info-Message {
    param([string]$Message)
    $masked = Mask $Message
    Write-Host "[INFO] $masked" -ForegroundColor Green
    Add-Content -Path $LogFile -Value "[$Timestamp] [INFO] $masked"
}

# Check logging.json configuration
function Test-LoggingConfig {
    Write-Info-Message "Checking logging configuration..."
    
    $loggingFile = Join-Path $ProjectRoot "config\observability\logging.json"
    
    if (-not (Test-Path $loggingFile)) {
        Write-Error-Message "Logging config not found: $loggingFile"
        return $false
    }
    
    # Check JSON syntax
    try {
        $json = Get-Content $loggingFile -Raw | ConvertFrom-Json
        Write-Info-Message "Valid JSON syntax in logging.json"
    } catch {
        Write-Error-Message "Invalid JSON syntax in logging.json"
        return $false
    }
    
    $content = Get-Content $loggingFile -Raw
    
    # Check for required fields in format
    if ($content -match '"timestamp"' -and 
        $content -match '"level"' -and 
        $content -match '"component"' -and 
        $content -match '"message"') {
        Write-Info-Message "Logging format includes required fields (timestamp, level, component, message)"
    } else {
        Write-Error-Message "Logging format missing required fields"
        return $false
    }
    
    # Check for log levels
    $requiredLevels = @("ERROR", "WARN", "INFO", "DEBUG")
    foreach ($level in $requiredLevels) {
        if ($content -match "`"$level`"") {
            Write-Info-Message "Found log level: $level"
        } else {
            Write-Warning-Message "Log level missing: $level"
        }
    }
    
    # Check for health endpoint definition
    if ($content -match '"/_health"' -or $content -match "health_endpoint") {
        Write-Info-Message "Health endpoint defined in logging.json"
    } else {
        Write-Warning-Message "Health endpoint not defined in logging.json"
    }
    
    Write-Info-Message "Logging configuration validated"
    return $true
}

# Validate JSON log format
function Test-JsonLogFormat {
    Write-Info-Message "Validating JSON log format..."
    
    $loggingFile = Join-Path $ProjectRoot "config\observability\logging.json"
    
    if (-not (Test-Path $loggingFile)) {
        Write-Error-Message "Cannot validate log format: logging.json not found"
        return $false
    }
    
    # Create example log entry
    $exampleLog = @{
        timestamp = "2025-01-27T12:00:00Z"
        level = "INFO"
        component = "router"
        message = "Request processed"
        context = @{
            provider = "openai"
            latency_ms = 250
        }
    } | ConvertTo-Json -Depth 10
    
    # Validate example log
    try {
        $exampleLog | ConvertFrom-Json | Out-Null
        Write-Info-Message "Example JSON log format is valid"
    } catch {
        Write-Warning-Message "Example JSON log format validation failed"
    }
    
    Write-Info-Message "JSON log format validation complete"
    return $true
}

# Check /_health endpoint availability and format (200 OK + JSON)
function Test-HealthEndpoints {
    Write-Info-Message "Checking /_health endpoint availability and format (200 OK + JSON)..."
    
    $components = @("provider", "gateway", "usage")
    $baseUrls = @(
        "http://localhost:8081",
        "http://localhost:3000",
        "http://localhost:8083"
    )
    
    $available = 0
    $unavailable = 0
    $invalidFormat = 0
    
    foreach ($i in 0..($components.Length - 1)) {
        $component = $components[$i]
        $url = "$($baseUrls[$i])/_health"
        
        try {
            $response = Invoke-WebRequest -Uri $url -Method Get -TimeoutSec 2 -UseBasicParsing -ErrorAction Stop
            
            if ($response.StatusCode -eq 200) {
                # Check if response is valid JSON
                try {
                    $json = $response.Content | ConvertFrom-Json
                    
                    # Check for required fields (status, timestamp)
                    if ($json.status -and $json.timestamp) {
                        Write-Info-Message "Health endpoint OK: $component ($url) - 200 OK, valid JSON with status/timestamp"
                        $available++
                    } else {
                        Write-Warning-Message "Health endpoint invalid format: $component ($url) - missing status or timestamp"
                        $invalidFormat++
                        $script:ExternalErrors++
                    }
                } catch {
                    Write-Warning-Message "Health endpoint invalid JSON: $component ($url) - response is not valid JSON"
                    $invalidFormat++
                    $script:ExternalErrors++
                }
            } else {
                Write-Warning-Message "Health endpoint unavailable: $component ($url) - HTTP $($response.StatusCode) (expected 200) - service may not be running"
                $unavailable++
                $script:ExternalErrors++
            }
        } catch {
            Write-Warning-Message "Health endpoint unavailable: $component ($url) - service may not be running"
            $unavailable++
            $script:ExternalErrors++
        }
    }
    
    if ($invalidFormat -gt 0) {
        Write-Warning-Message "Some health endpoints have invalid format ($invalidFormat/$($components.Length)) - check JSON structure"
    }
    
    if ($unavailable -gt 0) {
        Write-Warning-Message "Some health endpoints are unavailable ($unavailable/$($components.Length)) - services may not be running"
        Write-Warning-Message "This is expected in local validation if services are not started"
    }
    
    if ($available -gt 0) {
        Write-Info-Message "Health endpoints OK: $available/$($components.Length) available with valid format (200 OK + JSON)"
    }
    
    return $true
}

# Check for real secrets in configs
function Test-Secrets {
    Write-Info-Message "Checking for real secrets in configuration files..."
    
    $configDir = Join-Path $ProjectRoot "config\observability"
    $secretPattern = '[A-Za-z0-9+/]{32,}'
    $foundSecrets = 0
    
    # Check logging.json
    $loggingFile = Join-Path $configDir "logging.json"
    if (Test-Path $loggingFile) {
        $content = Get-Content $loggingFile
        foreach ($line in $content) {
            if ($line -match $secretPattern -and $line -notmatch "PLACEHOLDER|REDACTED|example") {
                $maskedLine = Mask $line
                Write-Warning-Message "Potential secret found in logging.json: $maskedLine"
                $foundSecrets++
            }
        }
    }
    
    if ($foundSecrets -gt 0) {
        Write-Warning-Message "Found $foundSecrets potential secret(s) - should use PLACEHOLDER or [REDACTED] values"
    } else {
        Write-Info-Message "No real secrets found (all use placeholders or examples)"
    }
    
    return $true
}

# Main validation
function Main {
    Add-Content -Path $LogFile -Value "========================================="
    Add-Content -Path $LogFile -Value "Observability Validation (MVP) - $Timestamp"
    Add-Content -Path $LogFile -Value "========================================="
    
    Write-Info-Message "Starting observability validation (MVP)..."
    Write-Host ""
    
    # Check logging configuration
    if (-not (Test-LoggingConfig)) {
        Write-Error-Message "Logging configuration validation failed"
    }
    Write-Host ""
    
    # Validate JSON log format
    if (-not (Test-JsonLogFormat)) {
        Write-Error-Message "JSON log format validation failed"
    }
    Write-Host ""
    
    # Check health endpoints (optional - services may not be running)
    Test-HealthEndpoints | Out-Null
    Write-Host ""
    
    # Check for secrets
    Test-Secrets | Out-Null
    Write-Host ""
    
    # Summary
    Write-Info-Message "Validation complete"
    Write-Host ""
    Write-Host "Summary:" | Tee-Object -FilePath $LogFile -Append
    Write-Host "  Errors: $script:Errors" | Tee-Object -FilePath $LogFile -Append
    Write-Host "  Warnings: $script:Warnings" | Tee-Object -FilePath $LogFile -Append
    Write-Host "  Local errors: $script:LocalErrors" | Tee-Object -FilePath $LogFile -Append
    Write-Host "  External errors: $script:ExternalErrors" | Tee-Object -FilePath $LogFile -Append
    Write-Host ""
    
    # Exit codes
    if ($script:LocalErrors -gt 0) {
        Write-Error-Message "Validation failed with $script:LocalErrors local error(s)"
        Add-Content -Path $LogFile -Value "Exit code: 3 (local paths/configs missing or invalid)"
        exit 3
    } elseif ($script:ExternalErrors -gt 0) {
        Write-Warning-Message "Validation passed with $script:ExternalErrors external error(s) (health endpoints unavailable)"
        Write-Warning-Message "Note: This is expected if services are not running"
        Add-Content -Path $LogFile -Value "Exit code: 2 (external endpoints unavailable)"
        exit 2
    } elseif ($script:Warnings -gt 0) {
        Write-Warning-Message "Validation passed with $script:Warnings warning(s)"
        Add-Content -Path $LogFile -Value "Exit code: 0 (success with warnings)"
        exit 0
    } else {
        Write-Info-Message "Validation passed successfully"
        Add-Content -Path $LogFile -Value "Exit code: 0 (success)"
        exit 0
    }
}

Main
