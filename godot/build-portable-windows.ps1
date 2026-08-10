param(
  [string]$OutputDirectory
)

$ErrorActionPreference = "Stop"
$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepositoryRoot = Resolve-Path (Join-Path $ProjectRoot "..")
if (-not $OutputDirectory) {
  $OutputDirectory = Join-Path $RepositoryRoot "release\godot-windows-portable"
}

$candidates = @(@(
  $env:GODOT4,
  (Join-Path $env:USERPROFILE ".codex\tools\godot-4.7\bin\Godot_v4.7-stable_win64_console.exe"),
  (Get-Command godot4 -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source -ErrorAction SilentlyContinue),
  (Get-Command godot -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source -ErrorAction SilentlyContinue)
) | Where-Object { $_ -and (Test-Path -LiteralPath $_) })

if (-not $candidates) {
  throw "Godot 4.7 nao encontrado. Define GODOT4 com o caminho do executavel oficial."
}

$godotConsole = $candidates[0]
$godotGui = $godotConsole -replace '_console\.exe$', '.exe'
if (-not (Test-Path -LiteralPath $godotGui)) {
  $godotGui = $godotConsole
}

New-Item -ItemType Directory -Path $OutputDirectory -Force | Out-Null
& $godotConsole --headless --path $ProjectRoot --export-pack "Windows Desktop" (Join-Path $OutputDirectory "ImmutableTowers.pck")
if ($LASTEXITCODE -ne 0) {
  throw "A exportacao do pacote Godot falhou."
}

Copy-Item -LiteralPath $godotGui -Destination (Join-Path $OutputDirectory "ImmutableTowers.exe") -Force
Copy-Item -LiteralPath (Join-Path $ProjectRoot "portable-run-game.bat") -Destination (Join-Path $OutputDirectory "run-game.bat") -Force
Copy-Item -LiteralPath (Join-Path $ProjectRoot "portable-README.txt") -Destination (Join-Path $OutputDirectory "README.txt") -Force
Copy-Item -LiteralPath (Join-Path $ProjectRoot "LICENSE-GODOT.txt") -Destination (Join-Path $OutputDirectory "LICENSE-GODOT.txt") -Force

$archivePath = Join-Path (Split-Path -Parent $OutputDirectory) "ImmutableTowers-Godot-Windows.zip"
if (Test-Path -LiteralPath $archivePath) {
  Remove-Item -LiteralPath $archivePath -Force
}
Compress-Archive -Path (Join-Path $OutputDirectory "*") -DestinationPath $archivePath -CompressionLevel Optimal

Write-Output "Bundle Godot criado em $OutputDirectory"
Write-Output "ZIP para distribuicao criado em $archivePath"
