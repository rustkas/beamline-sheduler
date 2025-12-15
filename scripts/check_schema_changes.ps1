# PowerShell script for checking schema changes (Windows/cross-platform)
# Equivalent to scripts/check_schema_changes.sh

param(
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"

$ProjectRoot = Split-Path -Parent $PSScriptRoot
Set-Location $ProjectRoot

$ManifestFile = ".trae/manifest.json"
$StateSchema = "docs/STATE.schema.json"
$HistorySchema = "docs/HISTORY.schema.json"

$Errors = 0
$Warnings = 0

Write-Host "=========================================="
Write-Host "Schema Changes Validation (PowerShell)"
Write-Host "=========================================="
Write-Host ""

# Check if manifest exists
if (-not (Test-Path $ManifestFile)) {
    Write-Host "[FAIL] Manifest file not found: $ManifestFile" -ForegroundColor Red
    exit 1
}

# Check manifest validity
try {
    $manifest = Get-Content $ManifestFile -Raw | ConvertFrom-Json
} catch {
    Write-Host "[FAIL] Manifest file is not valid JSON: $_" -ForegroundColor Red
    exit 1
}

# Get versions from manifest
$ManifestStateVersion = $manifest.schema_versions.state.version
$ManifestHistoryVersion = $manifest.schema_versions.history.version

Write-Host "[INFO] Manifest state version: $ManifestStateVersion"
Write-Host "[INFO] Manifest history version: $ManifestHistoryVersion"
Write-Host ""

# Function to extract version from schema
function Get-SchemaVersion {
    param([string]$SchemaFile)
    
    if (-not (Test-Path $SchemaFile)) {
        return ""
    }
    
    try {
        $schema = Get-Content $SchemaFile -Raw | ConvertFrom-Json
        
        # Try to extract version from $id
        if ($schema.'$id') {
            if ($schema.'$id' -match 'v(\d+\.\d+\.\d+)') {
                return $matches[1]
            }
        }
        
        # Try to extract from version field
        if ($schema.version) {
            return $schema.version
        }
        
        return ""
    } catch {
        return ""
    }
}

# Check STATE schema
if (Test-Path $StateSchema) {
    $SchemaVersion = Get-SchemaVersion -SchemaFile $StateSchema
    
    if ([string]::IsNullOrEmpty($SchemaVersion)) {
        Write-Host "[WARN] STATE schema version not found in schema file" -ForegroundColor Yellow
        $Warnings++
    } elseif ($SchemaVersion -ne $ManifestStateVersion) {
        Write-Host "[FAIL] STATE schema version mismatch:" -ForegroundColor Red
        Write-Host "  Schema: $SchemaVersion"
        Write-Host "  Manifest: $ManifestStateVersion"
        $Errors++
    } else {
        Write-Host "[OK] STATE schema version matches manifest: $SchemaVersion" -ForegroundColor Green
    }
} else {
    Write-Host "[WARN] STATE schema file not found: $StateSchema" -ForegroundColor Yellow
    $Warnings++
}

# Check HISTORY schema
if (Test-Path $HistorySchema) {
    $SchemaVersion = Get-SchemaVersion -SchemaFile $HistorySchema
    
    if ([string]::IsNullOrEmpty($SchemaVersion)) {
        Write-Host "[WARN] HISTORY schema version not found in schema file" -ForegroundColor Yellow
        $Warnings++
    } elseif ($SchemaVersion -ne $ManifestHistoryVersion) {
        Write-Host "[FAIL] HISTORY schema version mismatch:" -ForegroundColor Red
        Write-Host "  Schema: $SchemaVersion"
        Write-Host "  Manifest: $ManifestHistoryVersion"
        $Errors++
    } else {
        Write-Host "[OK] HISTORY schema version matches manifest: $SchemaVersion" -ForegroundColor Green
    }
} else {
    Write-Host "[WARN] HISTORY schema file not found: $HistorySchema" -ForegroundColor Yellow
    $Warnings++
}

# Check Git changes (if available)
if (Get-Command git -ErrorAction SilentlyContinue) {
    try {
        $gitRoot = git rev-parse --git-dir 2>$null
        if ($gitRoot) {
            Write-Host ""
            Write-Host "[INFO] Checking for schema changes in Git..."
            
            # Check STATE schema changes
            $stateChanges = git diff --name-only HEAD | Select-String -Pattern "^$StateSchema$"
            if ($stateChanges) {
                Write-Host "[WARN] STATE schema was modified" -ForegroundColor Yellow
                
                # Try to get old version from Git
                try {
                    $oldContent = git show "HEAD:$StateSchema" 2>$null
                    if ($oldContent) {
                        $oldSchema = $oldContent | ConvertFrom-Json
                        $oldVersion = ""
                        
                        if ($oldSchema.'$id' -match 'v(\d+\.\d+\.\d+)') {
                            $oldVersion = $matches[1]
                        } elseif ($oldSchema.version) {
                            $oldVersion = $oldSchema.version
                        }
                        
                        $newVersion = Get-SchemaVersion -SchemaFile $StateSchema
                        
                        if ($oldVersion -and $newVersion) {
                            if ($oldVersion -eq $newVersion) {
                                Write-Host "[FAIL] STATE schema was modified but version was not updated" -ForegroundColor Red
                                Write-Host "  Current version: $newVersion"
                                Write-Host "  Please update version in schema and manifest.json"
                                $Errors++
                            } else {
                                Write-Host "[OK] STATE schema version was updated: $oldVersion → $newVersion" -ForegroundColor Green
                                
                                # Check if manifest was updated
                                if ($newVersion -ne $ManifestStateVersion) {
                                    Write-Host "[FAIL] STATE schema version updated but manifest.json was not updated" -ForegroundColor Red
                                    Write-Host "  Schema version: $newVersion"
                                    Write-Host "  Manifest version: $ManifestStateVersion"
                                    $Errors++
                                }
                            }
                        }
                    }
                } catch {
                    # Ignore errors when getting old version
                }
            }
            
            # Similarly for HISTORY schema
            $historyChanges = git diff --name-only HEAD | Select-String -Pattern "^$HistorySchema$"
            if ($historyChanges) {
                Write-Host "[WARN] HISTORY schema was modified" -ForegroundColor Yellow
                
                try {
                    $oldContent = git show "HEAD:$HistorySchema" 2>$null
                    if ($oldContent) {
                        $oldSchema = $oldContent | ConvertFrom-Json
                        $oldVersion = ""
                        
                        if ($oldSchema.'$id' -match 'v(\d+\.\d+\.\d+)') {
                            $oldVersion = $matches[1]
                        } elseif ($oldSchema.version) {
                            $oldVersion = $oldSchema.version
                        }
                        
                        $newVersion = Get-SchemaVersion -SchemaFile $HistorySchema
                        
                        if ($oldVersion -and $newVersion) {
                            if ($oldVersion -eq $newVersion) {
                                Write-Host "[FAIL] HISTORY schema was modified but version was not updated" -ForegroundColor Red
                                Write-Host "  Current version: $newVersion"
                                Write-Host "  Please update version in schema and manifest.json"
                                $Errors++
                            } else {
                                Write-Host "[OK] HISTORY schema version was updated: $oldVersion → $newVersion" -ForegroundColor Green
                                
                                if ($newVersion -ne $ManifestHistoryVersion) {
                                    Write-Host "[FAIL] HISTORY schema version updated but manifest.json was not updated" -ForegroundColor Red
                                    Write-Host "  Schema version: $newVersion"
                                    Write-Host "  Manifest version: $ManifestHistoryVersion"
                                    $Errors++
                                }
                            }
                        }
                    }
                } catch {
                    # Ignore errors
                }
            }
        }
    } catch {
        # Git not available or not in repository
    }
}

# Final result
Write-Host ""
Write-Host "=========================================="
if ($Errors -eq 0) {
    if ($Warnings -gt 0) {
        Write-Host "[WARN] Schema changes validation passed with $Warnings warning(s)" -ForegroundColor Yellow
        exit 0
    } else {
        Write-Host "[OK] Schema changes validation passed" -ForegroundColor Green
        exit 0
    }
} else {
    Write-Host "[FAIL] Schema changes validation failed: $Errors error(s)" -ForegroundColor Red
    if ($Warnings -gt 0) {
        Write-Host "[WARN] $Warnings warning(s)" -ForegroundColor Yellow
    }
    Write-Host ""
    Write-Host "Please fix the following issues:"
    Write-Host "  1. Update schema versions when modifying schemas"
    Write-Host "  2. Update manifest.json when updating schema versions"
    Write-Host "  3. Follow versioning policy: docs/SCHEMA_VERSIONING.md"
    exit 1
}
