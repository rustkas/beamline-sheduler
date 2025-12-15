# Check Proto File Synchronization (PowerShell)
# 
# Verifies that proto/beamline/flow/v1/flow.proto and 
# apps/otp/router/proto/beamline/flow/v1/flow.proto are identical.

param(
    [string]$RepoRoot = $PSScriptRoot + "\.."
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$RootProto = Join-Path $RepoRoot "proto\beamline\flow\v1\flow.proto"
$RouterProto = Join-Path $RepoRoot "apps\otp\router\proto\beamline\flow\v1\flow.proto"

Write-Host "=== Proto File Synchronization Check ===" -ForegroundColor Cyan
Write-Host ""

# Check if files exist
if (-not (Test-Path $RootProto)) {
    Write-Host "❌ ERROR: Root proto file not found: $RootProto" -ForegroundColor Red
    exit 1
}

if (-not (Test-Path $RouterProto)) {
    Write-Host "❌ ERROR: Router proto file not found: $RouterProto" -ForegroundColor Red
    exit 1
}

# Calculate MD5 checksums
$RootHash = (Get-FileHash -Path $RootProto -Algorithm MD5).Hash
$RouterHash = (Get-FileHash -Path $RouterProto -Algorithm MD5).Hash

Write-Host "Root proto MD5:    $RootHash"
Write-Host "Router proto MD5:  $RouterHash"
Write-Host ""

# Compare checksums
if ($RootHash -eq $RouterHash) {
    Write-Host "✅ SUCCESS: Proto files are synchronized (identical)" -ForegroundColor Green
    exit 0
} else {
    Write-Host "❌ ERROR: Proto files are NOT synchronized (differ)" -ForegroundColor Red
    Write-Host ""
    Write-Host "Files differ. Run the following to synchronize:"
    Write-Host "  Copy-Item '$RouterProto' '$RootProto'"
    Write-Host ""
    Write-Host "Or if root proto is the source of truth:"
    Write-Host "  Copy-Item '$RootProto' '$RouterProto'"
    Write-Host ""
    Write-Host "Diff:"
    Compare-Object (Get-Content $RootProto) (Get-Content $RouterProto) | Format-Table
    exit 1
}
