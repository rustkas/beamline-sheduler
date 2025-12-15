# Quick local checks script (PowerShell)
# Runs minimal validation: Router compile/test, Gateway tests, ABI checks
# Exit codes: 0=success, 1=router error, 2=gateway error, 3=abi error

param(
    [switch]$Help
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptDir

Set-Location $ProjectRoot

# Exit codes
$EXIT_SUCCESS = 0
$EXIT_ROUTER_ERROR = 1
$EXIT_GATEWAY_ERROR = 2
$EXIT_ABI_ERROR = 3

$FailedChecks = 0

function Write-ColorOutput {
    param(
        [string]$Message,
        [string]$Color = "White"
    )
    Write-Host $Message -ForegroundColor $Color
}

function Test-Tool {
    param([string]$Tool)
    $toolPath = Get-Command $Tool -ErrorAction SilentlyContinue
    if (-not $toolPath) {
        Write-ColorOutput "✗ $Tool not found. Please install it first." "Red"
        return $false
    }
    return $true
}

function Test-Router {
    Write-Host "----------------------------------------"
    Write-Host "Router (Erlang/OTP) Checks"
    Write-Host "----------------------------------------"
    Write-Host ""

    $routerDir = Join-Path $ProjectRoot "apps\otp\router"
    
    if (-not (Test-Path (Join-Path $routerDir "rebar.config"))) {
        Write-ColorOutput "⚠ Router rebar.config not found, skipping Router checks" "Yellow"
        return $EXIT_SUCCESS
    }

    Set-Location $routerDir

    if (-not (Test-Tool "rebar3")) {
        Write-ColorOutput "✗ rebar3 not found. Skipping Router checks." "Red"
        return $EXIT_ROUTER_ERROR
    }

    # Compile
    Write-Host "Running: rebar3 compile"
    try {
        & rebar3 compile
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "✓ Router compilation: PASSED" "Green"
        } else {
            Write-ColorOutput "✗ Router compilation: FAILED" "Red"
            $script:FailedChecks++
            return $EXIT_ROUTER_ERROR
        }
    } catch {
        Write-ColorOutput "✗ Router compilation: FAILED" "Red"
        $script:FailedChecks++
        return $EXIT_ROUTER_ERROR
    }

    Write-Host ""

    # Tests (fast tests only for CP1 CI)
    Write-Host "Running: make test-fast (fast tests only)"
    try {
        & make test-fast
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "✓ Router tests: PASSED" "Green"
        } else {
            Write-ColorOutput "✗ Router tests: FAILED" "Red"
            $script:FailedChecks++
            return $EXIT_ROUTER_ERROR
        }
    } catch {
        Write-ColorOutput "✗ Router tests: FAILED" "Red"
        $script:FailedChecks++
        return $EXIT_ROUTER_ERROR
    }

    Write-Host ""
    return $EXIT_SUCCESS
}

function Test-Gateway {
    Write-Host "----------------------------------------"
    Write-Host "Gateway (NestJS) Checks"
    Write-Host "----------------------------------------"
    Write-Host ""

    $gatewayDir = Join-Path $ProjectRoot "apps\gateway"
    
    if (-not (Test-Path (Join-Path $gatewayDir "package.json"))) {
        Write-ColorOutput "⚠ Gateway package.json not found, skipping Gateway checks" "Yellow"
        return $EXIT_SUCCESS
    }

    Set-Location $gatewayDir

    if (-not (Test-Tool "npm")) {
        Write-ColorOutput "⚠ npm not found. Skipping Gateway checks." "Yellow"
        return $EXIT_SUCCESS
    }

    # Step 1: Lint
    Write-Host "Step 1: Running lint"
    try {
        & npm run lint:check
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "✓ Gateway lint: PASSED" "Green"
        } else {
            Write-ColorOutput "✗ Gateway lint: FAILED" "Red"
            $script:FailedChecks++
            return $EXIT_GATEWAY_ERROR
        }
    } catch {
        Write-ColorOutput "✗ Gateway lint: FAILED" "Red"
        $script:FailedChecks++
        return $EXIT_GATEWAY_ERROR
    }

    Write-Host ""

    # Step 2: Build
    Write-Host "Step 2: Running build"
    try {
        & npm run build
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "✓ Gateway build: PASSED" "Green"
        } else {
            Write-ColorOutput "✗ Gateway build: FAILED" "Red"
            $script:FailedChecks++
            return $EXIT_GATEWAY_ERROR
        }
    } catch {
        Write-ColorOutput "✗ Gateway build: FAILED" "Red"
        $script:FailedChecks++
        return $EXIT_GATEWAY_ERROR
    }

    Write-Host ""

    # Step 3: Tests
    Write-Host "Step 3: Running tests"
    try {
        & npm run test
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "✓ Gateway tests: PASSED" "Green"
        } else {
            Write-ColorOutput "✗ Gateway tests: FAILED" "Red"
            $script:FailedChecks++
            return $EXIT_GATEWAY_ERROR
        }
    } catch {
        Write-ColorOutput "✗ Gateway tests: FAILED" "Red"
        $script:FailedChecks++
        return $EXIT_GATEWAY_ERROR
    }

    Write-Host ""
    return $EXIT_SUCCESS
}

function Test-ABI {
    Write-Host "----------------------------------------"
    Write-Host "ABI (Protobuf) Checks"
    Write-Host "----------------------------------------"
    Write-Host ""

    $protoDir = Join-Path $ProjectRoot "proto"
    
    if (-not (Test-Path $protoDir)) {
        Write-ColorOutput "⚠ proto directory not found, skipping ABI checks" "Yellow"
        return $EXIT_SUCCESS
    }

    Set-Location $ProjectRoot

    if (-not (Test-Tool "buf")) {
        Write-ColorOutput "⚠ buf not found. Install from https://buf.build/docs/installation" "Yellow"
        Write-ColorOutput "⚠ Skipping ABI checks." "Yellow"
        return $EXIT_SUCCESS
    }

    # Check for buf.yaml or buf.work.yaml
    if (-not (Test-Path "buf.yaml") -and -not (Test-Path "buf.work.yaml")) {
        Write-ColorOutput "⚠ buf.yaml or buf.work.yaml not found, skipping ABI checks" "Yellow"
        return $EXIT_SUCCESS
    }

    # buf lint
    Write-Host "Running: buf lint"
    try {
        & buf lint
        if ($LASTEXITCODE -eq 0) {
            Write-ColorOutput "✓ buf lint: PASSED" "Green"
        } else {
            Write-ColorOutput "✗ buf lint: FAILED" "Red"
            $script:FailedChecks++
            return $EXIT_ABI_ERROR
        }
    } catch {
        Write-ColorOutput "✗ buf lint: FAILED" "Red"
        $script:FailedChecks++
        return $EXIT_ABI_ERROR
    }

    Write-Host ""

    # buf breaking (against main/master branch if available)
    Write-Host "Running: buf breaking"
    try {
        $hasMain = git rev-parse --verify origin/main 2>$null
        $hasMaster = git rev-parse --verify origin/master 2>$null
        
        if ($hasMain -or $hasMaster) {
            $baseBranch = if ($hasMain) { "origin/main" } else { "origin/master" }
            & buf breaking --against $baseBranch
            if ($LASTEXITCODE -eq 0) {
                Write-ColorOutput "✓ buf breaking: PASSED" "Green"
            } else {
                Write-ColorOutput "✗ buf breaking: FAILED (breaking changes detected)" "Red"
                $script:FailedChecks++
                return $EXIT_ABI_ERROR
            }
        } else {
            Write-ColorOutput "⚠ No origin/main or origin/master found, skipping buf breaking" "Yellow"
            Write-ColorOutput "⚠ (This is normal for local development without remote)" "Yellow"
        }
    } catch {
        Write-ColorOutput "⚠ Error checking git branches, skipping buf breaking" "Yellow"
    }

    Write-Host ""
    return $EXIT_SUCCESS
}

# Main execution
Write-Host "=========================================="
Write-Host "Local Checks Runner"
Write-Host "=========================================="
Write-Host ""

if ($Help) {
    Write-Host "Usage: .\scripts\run_checks.ps1"
    Write-Host ""
    Write-Host "Runs minimal validation checks:"
    Write-Host "  - Router (Erlang/OTP): compile + tests"
    Write-Host "  - Gateway (NestJS): tests"
    Write-Host "  - ABI (Protobuf): buf lint + buf breaking"
    Write-Host ""
    exit $EXIT_SUCCESS
}

$routerResult = Test-Router
Set-Location $ProjectRoot

$gatewayResult = Test-Gateway
Set-Location $ProjectRoot

$abiResult = Test-ABI

# Summary
Write-Host "=========================================="
Write-Host "Summary"
Write-Host "=========================================="
Write-Host ""

if ($routerResult -eq $EXIT_SUCCESS) {
    Write-ColorOutput "✓ Router: PASSED" "Green"
} else {
    Write-ColorOutput "✗ Router: FAILED" "Red"
}

if ($gatewayResult -eq $EXIT_SUCCESS) {
    Write-ColorOutput "✓ Gateway: PASSED" "Green"
} else {
    Write-ColorOutput "✗ Gateway: FAILED" "Red"
}

if ($abiResult -eq $EXIT_SUCCESS) {
    Write-ColorOutput "✓ ABI: PASSED" "Green"
} else {
    Write-ColorOutput "✗ ABI: FAILED" "Red"
}

Write-Host ""

# Overall result
if ($FailedChecks -eq 0) {
    Write-ColorOutput "✓ All checks passed" "Green"
    exit $EXIT_SUCCESS
} else {
    Write-ColorOutput "✗ $FailedChecks check(s) failed" "Red"
    if ($routerResult -ne $EXIT_SUCCESS) {
        exit $EXIT_ROUTER_ERROR
    } elseif ($gatewayResult -ne $EXIT_SUCCESS) {
        exit $EXIT_GATEWAY_ERROR
    } elseif ($abiResult -ne $EXIT_SUCCESS) {
        exit $EXIT_ABI_ERROR
    }
    exit 1
}

