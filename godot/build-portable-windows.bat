@echo off
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0build-portable-windows.ps1" %*
exit /b %errorlevel%
