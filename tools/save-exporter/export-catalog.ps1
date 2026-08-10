param(
  [string]$OutputPath
)

$ErrorActionPreference = "Stop"
$ToolRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepositoryRoot = Resolve-Path (Join-Path $ToolRoot "..\..")

if (-not $OutputPath) {
  $OutputPath = Join-Path $RepositoryRoot "godot\data\domain-catalog-v1.json"
}

Push-Location $ToolRoot
try {
  & cabal run immutable-towers-save-exporter -- --catalog-output $OutputPath
  exit $LASTEXITCODE
}
finally {
  Pop-Location
}
