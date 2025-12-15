param(
  [Parameter(Mandatory=$true, Position=0, ValueFromRemainingArguments=$true)]
  [string[]]$Targets
)

$ErrorActionPreference = 'Stop'

function Mask([string]$s) {
  if (-not $s) { return $s }
  return ($s -replace '([A-Za-z0-9+/]{16,})','[MASKED]')
}

function GetMarkdownFiles([string[]]$paths) {
  $files = @()
  foreach ($p in $paths) {
    if (Test-Path -LiteralPath $p -PathType Container) {
      $files += Get-ChildItem -LiteralPath $p -Recurse -File -Filter '*.md' | ForEach-Object { $_.FullName }
    } elseif (Test-Path -LiteralPath $p -PathType Leaf) {
      $files += (Resolve-Path -LiteralPath $p).Path
    } else {
      Write-Warning (Mask "Path not found: $p")
    }
  }
  $files | Sort-Object -Unique
}

function ExtractLinks([string]$file) {
  $content = Get-Content -LiteralPath $file
  $links = @()
  for ($i=0; $i -lt $content.Length; $i++) {
    $line = $content[$i]
    $matches = [regex]::Matches($line, '\\(([^)]+)\\)')
    foreach ($m in $matches) {
      $url = $m.Groups[1].Value.Trim()
      $url = $url.Trim('<','>')
      $links += @{ line = ($i+1); url = $url }
    }
  }
  return $links
}

function IsIgnored([string]$url) {
  return (
    $url.StartsWith('#') -or
    $url.StartsWith('mailto:') -or
    $url.StartsWith('data:') -or
    $url.StartsWith('javascript:')
  )
}

function ResolveLocalPath([string]$url, [string]$src, [string]$root) {
  if ($url -match '^(docs/|\\.cursor/|README\\.md$|CHANGELOG\\.md$|\\.github/|scripts/|reports/)') {
    $relative = $url -replace '^/', ''
    return (Join-Path -LiteralPath $root -ChildPath $relative)
  }
  if ($url.StartsWith('/')) {
    $relative = $url.TrimStart('/')
    return (Join-Path -LiteralPath $root -ChildPath $relative)
  }
  $baseDir = Split-Path -LiteralPath $src -Parent
  return (Join-Path -LiteralPath $baseDir -ChildPath $url)
}

$root = (Get-Location).Path
$brokenExternal = $false
$brokenLocal = $false

$files = GetMarkdownFiles $Targets
if (-not $files -or $files.Count -eq 0) {
  Write-Output "[INFO] No markdown files found"
  exit 0
}

foreach ($file in $files) {
  Write-Output (Mask "Checking: $file")
  $links = ExtractLinks $file
  if (-not $links -or $links.Count -eq 0) {
    Write-Output "  [OK] No links found"
    continue
  }
  foreach ($ln in $links) {
    $url = $ln.url
    if ([string]::IsNullOrWhiteSpace($url)) { continue }
    if (IsIgnored $url) { continue }
    if ($url -match '^https?://') {
      try {
        $resp = Invoke-WebRequest -Uri $url -Method Head -TimeoutSec 10 -UseBasicParsing -ErrorAction Stop
      } catch {
        Write-Output (Mask "  [BROKEN_EXTERNAL] $url")
        $brokenExternal = $true
      }
      continue
    }
    $resolved = ResolveLocalPath $url $file $root
    try {
      if (-not (Test-Path -LiteralPath $resolved)) {
        Write-Output (Mask "  [BROKEN_LOCAL] $url (resolved: $resolved)")
        $brokenLocal = $true
      }
    } catch {
      Write-Output (Mask "  [ERROR] Failed to check path: $resolved")
      $brokenLocal = $true
    }
  }
  if (-not $brokenLocal -and -not $brokenExternal) {
    Write-Output "  [OK] All links valid"
  }
}

if ($brokenLocal) { exit 3 }
if ($brokenExternal) { exit 2 }
Write-Output "[OK] Documentation links validated"
exit 0
