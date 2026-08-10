@echo off
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0export-catalog.ps1" %*
exit /b %errorlevel%
