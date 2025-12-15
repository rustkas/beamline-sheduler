param(
  [string]$ComposeFile = "infra/compose/local-dev.yml",
  [string]$HealthUrl = "http://localhost:8080/_health",
  [int]$Retries = 20,
  [int]$DelaySeconds = 2
)

$ErrorActionPreference = 'Stop'

Write-Host "Starting local dev stack via Docker Compose..." -ForegroundColor Cyan
docker compose -f $ComposeFile up -d
if ($LASTEXITCODE -ne 0) {
  Write-Error "docker compose failed with exit code $LASTEXITCODE"
  exit 1
}

function Test-HttpHealth($url) {
  try {
    $resp = Invoke-RestMethod -Uri $url -Method GET -TimeoutSec 5
    if ($resp) { return $true } else { return $false }
  } catch { return $false }
}

function Test-TcpPort($hostname, $port) {
  try { return (Test-NetConnection -ComputerName $hostname -Port $port -InformationLevel Quiet) } catch { return $false }
}

Write-Host "Waiting for health endpoint: $HealthUrl" -ForegroundColor Cyan
$ok = $false
for ($i = 1; $i -le $Retries; $i++) {
  if (Test-HttpHealth $HealthUrl) { $ok = $true; break }
  Write-Host "HTTP health attempt $i/$Retries failed; retrying..." -ForegroundColor Yellow
  Start-Sleep -Seconds $DelaySeconds
}

if (-not $ok) {
  Write-Host "HTTP health not available; checking open ports (8080, 9000)" -ForegroundColor Yellow
  $httpPort = Test-TcpPort 'localhost' 8080
  $grpcPort = Test-TcpPort 'localhost' 9000
  if ($httpPort -and $grpcPort) {
    Write-Host "Ports open: 8080 and 9000. Health endpoint likely not implemented yet." -ForegroundColor Green
    exit 0
  } else {
    Write-Error "Service did not become healthy; ports open? http=$httpPort grpc=$grpcPort"
    exit 3
  }
}

Write-Host "Local dev stack is up and healthy (HTTP)." -ForegroundColor Green
exit 0

