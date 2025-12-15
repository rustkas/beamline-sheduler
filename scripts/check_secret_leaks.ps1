# Secret Leak Detection Script (PowerShell)
# Checks for potential secret leaks using unified pattern registry from secret_patterns.json
# Usage: pwsh -File scripts/check_secret_leaks.ps1 [-Staged] [-All]
# Exit codes: 0 = OK, 2 = masking violation, 5 = leaks detected, 1 = error

param(
    [switch]$Staged,
    [switch]$All
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptDir
Set-Location $ProjectRoot

# Configuration from manifest
$ManifestFile = ".trae/manifest.json"
$PatternsFile = "scripts/secret_patterns.json"
$ReportDir = "reports/dry-run-logs"
$ReportFile = Join-Path $ReportDir "security.log"
$Timestamp = (Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ")

# Create report directory
New-Item -ItemType Directory -Force -Path $ReportDir | Out-Null

# Initialize report
@"
==========================================
Secret Leak Detection Report
Timestamp: $Timestamp
==========================================

"@ | Out-File -FilePath $ReportFile -Encoding UTF8

# Load configuration from manifest
Write-Host "=========================================="
Write-Host "Secret Leak Detection"
Write-Host "=========================================="
Write-Host ""

Write-Host "[INFO] Reading configuration from manifest..."
$manifest = Get-Content $ManifestFile | ConvertFrom-Json
$HmacSecretEnv = $manifest.security.hmac_secret.env_variable
$HmacMaskingPolicy = $manifest.security.hmac_masking.policy
Write-Host "[INFO] HMAC Secret Env: $HmacSecretEnv" | Tee-Object -FilePath $ReportFile -Append
Write-Host "[INFO] HMAC Masking Policy: $HmacMaskingPolicy" | Tee-Object -FilePath $ReportFile -Append
Write-Host "" | Tee-Object -FilePath $ReportFile -Append

# Load patterns from JSON
if (-not (Test-Path $PatternsFile)) {
    Write-Host "[ERROR] Pattern registry not found: $PatternsFile" | Tee-Object -FilePath $ReportFile -Append
    exit 1
}

$PatternsJson = Get-Content $PatternsFile | ConvertFrom-Json
$ExitCodes = $PatternsJson.exit_codes

# Build exclusion patterns
$ExcludeDirs = $PatternsJson.exclusions.directories
$ExcludeFiles = $PatternsJson.exclusions.file_patterns
$ExcludeCode = $PatternsJson.exclusions.code_patterns

$Errors = 0
$Warnings = 0
$MaskingViolations = 0
$LeaksDetected = 0

# Function to mask secret in output
function Mask-Secret {
    param(
        [string]$Secret,
        [string]$PatternType = "default"
    )
    
    switch ($PatternType) {
        "hmac" { return $Secret.Substring(0, [Math]::Min(16, $Secret.Length)) + "..." }
        { $_ -in "api_key", "secret" } { return $Secret.Substring(0, [Math]::Min(8, $Secret.Length)) + "..." }
        "token" { return $Secret.Substring(0, [Math]::Min(12, $Secret.Length)) + "..." }
        "ssh_key" { return "[REDACTED SSH KEY]" }
        default { return $Secret.Substring(0, [Math]::Min(8, $Secret.Length)) + "..." }
    }
}

# Function to check if file should be excluded
function Should-ExcludeFile {
    param([string]$File)
    
    # Check directory exclusions
    foreach ($dir in $ExcludeDirs) {
        if ($File -match $dir) {
            return $true
        }
    }
    
    # Check file pattern exclusions
    foreach ($pattern in $ExcludeFiles) {
        if ($File -match $pattern) {
            return $true
        }
    }
    
    # Exclude fixtures/leak_samples from scanning
    if ($File -match "fixtures/leak_samples/") {
        return $true
    }
    
    return $false
}

# Function to check if line should be excluded
function Should-ExcludeLine {
    param([string]$Line)
    
    # Check code pattern exclusions
    foreach ($pattern in $ExcludeCode) {
        if ($Line -match $pattern) {
            return $true
        }
    }
    
    return $false
}

# Extract all patterns from JSON
function Get-AllPatterns {
    param([object]$PatternsJson)
    
    $patterns = @()
    
    # Extract HMAC pattern
    if ($PatternsJson.patterns.hmac.pattern) {
        $patterns += $PatternsJson.patterns.hmac.pattern
    }
    
    # Extract API key patterns
    if ($PatternsJson.patterns.api_keys.patterns) {
        foreach ($p in $PatternsJson.patterns.api_keys.patterns) {
            if ($p.pattern) { $patterns += $p.pattern }
        }
    }
    
    # Extract token patterns
    if ($PatternsJson.patterns.tokens.patterns) {
        foreach ($p in $PatternsJson.patterns.tokens.patterns) {
            if ($p.pattern) { $patterns += $p.pattern }
        }
    }
    
    # Extract password patterns
    if ($PatternsJson.patterns.passwords.patterns) {
        foreach ($p in $PatternsJson.patterns.passwords.patterns) {
            if ($p.pattern) { $patterns += $p.pattern }
        }
    }
    
    # Extract secret patterns
    if ($PatternsJson.patterns.secrets.patterns) {
        foreach ($p in $PatternsJson.patterns.secrets.patterns) {
            if ($p.pattern) { $patterns += $p.pattern }
        }
    }
    
    # Extract SSH key patterns
    if ($PatternsJson.patterns.ssh_keys.patterns) {
        foreach ($p in $PatternsJson.patterns.ssh_keys.patterns) {
            if ($p.pattern) { $patterns += $p.pattern }
        }
    }
    
    # Extract base64 patterns
    if ($PatternsJson.patterns.base64_secrets.patterns) {
        foreach ($p in $PatternsJson.patterns.base64_secrets.patterns) {
            if ($p.pattern) { $patterns += $p.pattern }
        }
    }
    
    return $patterns
}

# Determine files to check
$FilesToCheck = @()

if ($Staged) {
    $FilesToCheck = git diff --cached --name-only --diff-filter=ACMR 2>$null
    if (-not $FilesToCheck) { $FilesToCheck = @() }
    Write-Host "[INFO] Checking staged files only" | Tee-Object -FilePath $ReportFile -Append
}
elseif ($All) {
    $FilesToCheck = git ls-files 2>$null
    if (-not $FilesToCheck) {
        $FilesToCheck = Get-ChildItem -Recurse -File | Where-Object { $_.FullName -notmatch "\.git" } | Select-Object -First 1000 -ExpandProperty FullName
    }
    Write-Host "[INFO] Checking all tracked files" | Tee-Object -FilePath $ReportFile -Append
}
else {
    $FilesToCheck = git diff --name-only origin/main...HEAD 2>$null
    if (-not $FilesToCheck) {
        $FilesToCheck = git diff --name-only main...HEAD 2>$null
    }
    if (-not $FilesToCheck) {
        $FilesToCheck = git diff --name-only --diff-filter=ACMR 2>$null
    }
    if (-not $FilesToCheck) { $FilesToCheck = @() }
    Write-Host "[INFO] Checking modified files" | Tee-Object -FilePath $ReportFile -Append
}

if ($FilesToCheck.Count -eq 0) {
    Write-Host "[WARN] No files to check" | Tee-Object -FilePath $ReportFile -Append
    Write-Host "" | Tee-Object -FilePath $ReportFile -Append
    Write-Host "==========================================" | Tee-Object -FilePath $ReportFile -Append
    Write-Host "[OK] No files to check" | Tee-Object -FilePath $ReportFile -Append
    exit 0
}

# Get all patterns
$AllPatterns = Get-AllPatterns -PatternsJson $PatternsJson

# Check each file
foreach ($file in $FilesToCheck) {
    if (Should-ExcludeFile -File $file) {
        continue
    }
    
    if (-not (Test-Path $file)) {
        continue
    }
    
    # Check each pattern
    foreach ($pattern in $AllPatterns) {
        if ([string]::IsNullOrEmpty($pattern)) {
            continue
        }
        
        try {
            $content = Get-Content $file -Raw
            $matches = [regex]::Matches($content, $pattern, [System.Text.RegularExpressions.RegexOptions]::Multiline)
            
            foreach ($match in $matches) {
                $lineNum = ($content.Substring(0, $match.Index) -split "`n").Count
                $line = $match.Value
                
                if (Should-ExcludeLine -Line $line) {
                    continue
                }
                
                # Check for masking violations
                if ($line -match "[0-9a-f]{64}|[A-Za-z0-9+/]{40,}={0,2}") {
                    $MaskingViolations++
                    Write-Host "[FAIL] Masking violation in $file`:" -ForegroundColor Red | Tee-Object -FilePath $ReportFile -Append
                    $masked = Mask-Secret -Secret $match.Value -PatternType "hmac"
                    Write-Host "  Line $lineNum : $masked" | Tee-Object -FilePath $ReportFile -Append
                }
                else {
                    $LeaksDetected++
                    Write-Host "[FAIL] Potential secret leak in $file`:" -ForegroundColor Red | Tee-Object -FilePath $ReportFile -Append
                    Write-Host "  Line $lineNum : $line" | Tee-Object -FilePath $ReportFile -Append
                }
                $Errors++
            }
        }
        catch {
            # Pattern might be invalid, skip
            continue
        }
    }
}

# Check for common secret file patterns
$SecretFiles = @(".env", ".secrets", "secrets.json", "config.json", "credentials.json")

foreach ($secretFile in $SecretFiles) {
    $gitFiles = git ls-files 2>$null
    if ($gitFiles -contains $secretFile) {
        Write-Host "[FAIL] Secret file found in repository: $secretFile" -ForegroundColor Red | Tee-Object -FilePath $ReportFile -Append
        Write-Host "  Secret files should be in .gitignore and never committed" | Tee-Object -FilePath $ReportFile -Append
        $Errors++
        $LeaksDetected++
    }
}

# Check .gitignore
if (Test-Path ".gitignore") {
    $gitignore = Get-Content ".gitignore"
    foreach ($secretFile in $SecretFiles) {
        if ($gitignore -notmatch "^$secretFile$|^/$secretFile$") {
            Write-Host "[WARN] Secret file pattern not in .gitignore: $secretFile" -ForegroundColor Yellow | Tee-Object -FilePath $ReportFile -Append
            $Warnings++
        }
    }
}

# Summary
Write-Host "" | Tee-Object -FilePath $ReportFile -Append
Write-Host "==========================================" | Tee-Object -FilePath $ReportFile -Append
@"
Summary:
  Errors: $Errors
  Warnings: $Warnings
  Masking Violations: $MaskingViolations
  Leaks Detected: $LeaksDetected

"@ | Tee-Object -FilePath $ReportFile -Append

# Determine exit code
$ExitCode = 0
if ($MaskingViolations -gt 0) {
    $ExitCode = 2
    Write-Host "[FAIL] Masking violations detected: $MaskingViolations" -ForegroundColor Red | Tee-Object -FilePath $ReportFile -Append
}
elseif ($LeaksDetected -gt 0) {
    $ExitCode = 5
    Write-Host "[FAIL] Secret leaks detected: $LeaksDetected" -ForegroundColor Red | Tee-Object -FilePath $ReportFile -Append
}
elseif ($Errors -eq 0 -and $Warnings -eq 0) {
    $ExitCode = 0
    Write-Host "[OK] No secret leaks detected" -ForegroundColor Green | Tee-Object -FilePath $ReportFile -Append
}
elseif ($Errors -eq 0) {
    $ExitCode = 0
    Write-Host "[WARN] Secret leak check completed with $Warnings warning(s)" -ForegroundColor Yellow | Tee-Object -FilePath $ReportFile -Append
    Write-Host "  Review warnings above - they may be false positives (masked values, examples)" | Tee-Object -FilePath $ReportFile -Append
}
else {
    $ExitCode = 5
    Write-Host "[FAIL] Secret leak check failed: Found $Errors potential leak(s)" -ForegroundColor Red | Tee-Object -FilePath $ReportFile -Append
}

Write-Host "" | Tee-Object -FilePath $ReportFile -Append
Write-Host "Report saved to: $ReportFile" | Tee-Object -FilePath $ReportFile -Append

exit $ExitCode
