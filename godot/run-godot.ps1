$ErrorActionPreference = "Stop"
$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$Godot = $env:GODOT_EXE
$PortableGodot = Join-Path $env:USERPROFILE ".codex\tools\godot-4.7\bin\Godot_v4.7-stable_win64.exe"

if (-not $Godot -and (Test-Path $PortableGodot)) {
  $Godot = $PortableGodot
}

if (-not $Godot) {
  $Command = Get-Command godot, godot4 -ErrorAction SilentlyContinue | Select-Object -First 1
  if ($Command) { $Godot = $Command.Source }
}

if (-not $Godot -or -not (Test-Path $Godot)) {
  Write-Host "Godot 4.7 nao encontrado. Define GODOT_EXE com o caminho do executavel oficial."
  exit 1
}

$Process = Start-Process -FilePath $Godot -ArgumentList @("--path", $ProjectRoot) -Wait -PassThru
exit $Process.ExitCode
