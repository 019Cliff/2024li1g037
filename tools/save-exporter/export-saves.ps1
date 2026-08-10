param(
  [string]$InputRoot,
  [string]$OutputPath
)

$ErrorActionPreference = "Stop"
$ToolRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepositoryRoot = Resolve-Path (Join-Path $ToolRoot "..\..")

if (-not $InputRoot) {
  $InputRoot = $RepositoryRoot
}
if (-not $OutputPath) {
  $OutputPath = Join-Path $RepositoryRoot "migration\samples\immutable-towers-transfer-v1.json"
}

Push-Location $ToolRoot
try {
  & cabal run immutable-towers-save-exporter -- --input $InputRoot --output $OutputPath
  exit $LASTEXITCODE
}
finally {
  Pop-Location
}
