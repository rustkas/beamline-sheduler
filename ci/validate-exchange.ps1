param(
  [string]$OrderPath = "orders\\order-skeleton"
)

$ErrorActionPreference = 'Stop'
$script:HasErrors = $false

function Check-File($Path) {
  if (!(Test-Path -LiteralPath $Path)) {
    Write-Error "[MISS] $Path"
    $script:HasErrors = $true
  } else {
    Write-Host "[OK]   $Path"
  }
}

function Test-NDJson($Path, $RequiredKeys) {
  $lines = Get-Content -LiteralPath $Path
  if ($lines.Count -eq 0) {
    Write-Error "[FAIL] $Path is empty"
    $script:HasErrors = $true
    return
  }
  foreach ($line in $lines) {
    try {
      $obj = $line | ConvertFrom-Json
    } catch {
      Write-Error "[FAIL] $Path contains invalid JSON line"
      $script:HasErrors = $true
      continue
    }
    foreach ($key in $RequiredKeys) {
      if (-not ($obj.PSObject.Properties.Name -contains $key)) {
        Write-Error "[FAIL] $Path: missing key '$key'"
        $script:HasErrors = $true
      }
    }
  }
}

Write-Host "Validating order structure in '$OrderPath'" -ForegroundColor Cyan

Check-File (Join-Path $OrderPath 'request.ndjson')
Check-File (Join-Path $OrderPath 'ledger.ndjson')

if (Test-Path (Join-Path $OrderPath 'summary/90-summary.json')) {
  Check-File (Join-Path $OrderPath 'summary/90-summary.json')
}

Test-NDJson (Join-Path $OrderPath 'request.ndjson') @('order_id','title','description')
Test-NDJson (Join-Path $OrderPath 'ledger.ndjson') @('agent_id','role','step','status')

if ($script:HasErrors) {
  Write-Host "Validation completed: errors found" -ForegroundColor Red
  exit 1
} else {
  Write-Host "Validation completed: OK" -ForegroundColor Green
  exit 0
}
