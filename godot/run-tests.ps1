$ErrorActionPreference = "Stop"
$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$Godot = $env:GODOT_EXE
$PortableGodot = Join-Path $env:USERPROFILE ".codex\tools\godot-4.7\bin\Godot_v4.7-stable_win64_console.exe"

if (-not $Godot -and (Test-Path $PortableGodot)) {
  $Godot = $PortableGodot
}

if (-not $Godot) {
  $Command = Get-Command godot, godot4 -ErrorAction SilentlyContinue | Select-Object -First 1
  if ($Command) { $Godot = $Command.Source }
}

if (-not $Godot -or -not (Test-Path $Godot)) {
  Write-Host "Godot 4.7 nao encontrado. Define GODOT_EXE antes de executar os testes headless."
  exit 1
}

$LogPath = Join-Path $env:TEMP "immutable-towers-godot-tests.log"
if (Test-Path -LiteralPath $LogPath) {
  Remove-Item -LiteralPath $LogPath -Force
}
$Process = Start-Process -FilePath $Godot `
  -ArgumentList @("--headless", "--path", $ProjectRoot, "--script", "res://tests/run_tests.gd", "--log-file", $LogPath) `
  -Wait -PassThru -NoNewWindow

$HasEngineErrors = $false
$HasCurrentPass = $false
if (Test-Path $LogPath) {
  $LogContent = Get-Content -LiteralPath $LogPath -Raw
  $HasEngineErrors = $LogContent -match "SCRIPT ERROR|ERROR:"
  $HasCurrentPass = $LogContent -match "Godot tests: PASS"
}

if ($Process.ExitCode -ne 0 -or $HasEngineErrors -or -not $HasCurrentPass) {
  if (Test-Path -LiteralPath $LogPath) { Get-Content -LiteralPath $LogPath }
  exit 1
}

$AuditLogPath = Join-Path $env:TEMP "immutable-towers-gameplay-gate.log"
if (Test-Path -LiteralPath $AuditLogPath) {
  Remove-Item -LiteralPath $AuditLogPath -Force
}
$AuditProcess = Start-Process -FilePath $Godot `
  -ArgumentList @("--headless", "--path", $ProjectRoot, "--script", "res://tools/audit_gameplay.gd", "--log-file", $AuditLogPath, "--", "gate") `
  -Wait -PassThru -NoNewWindow

$AuditHasErrors = $false
$AuditPassed = $false
if (Test-Path $AuditLogPath) {
  $AuditContent = Get-Content -LiteralPath $AuditLogPath -Raw
  $AuditHasErrors = $AuditContent -match "SCRIPT ERROR|ERROR:"
  $AuditPassed = $AuditContent -match '"status": "pass"'
}

if ($AuditProcess.ExitCode -ne 0 -or $AuditHasErrors -or -not $AuditPassed) {
  if (Test-Path -LiteralPath $AuditLogPath) { Get-Content -LiteralPath $AuditLogPath }
  exit 1
}
exit 0
