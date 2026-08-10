@echo off
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0export-saves.ps1" %*
exit /b %errorlevel%
