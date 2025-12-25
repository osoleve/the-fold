@echo off
REM START.cmd — The Fold REPL Entry Point
REM
REM This is the unified entry point for The Fold. It:
REM   1. Checks if Chez Scheme is installed
REM   2. Starts the REPL daemon in background (for Claude Code integration)
REM   3. Launches an interactive REPL for humans
REM
REM For Claude Code:
REM   - Use DAEMON.cmd to start the background daemon
REM   - Use fold.cmd to send expressions to the daemon
REM
REM For humans:
REM   - Just run START.cmd for an interactive session

setlocal enabledelayedexpansion
cd /d "%~dp0"

REM ============================================================
REM Check for Chez Scheme
REM ============================================================

set "SCHEME=C:\Program Files\Chez Scheme 10.3.0\bin\ta6nt\scheme.exe"

if not exist "%SCHEME%" (
    echo.
    echo ERROR: Chez Scheme not found at:
    echo   %SCHEME%
    echo.
    echo Please install Chez Scheme from:
    echo   https://cisco.github.io/ChezScheme/
    echo.
    echo Or update the SCHEME variable in this script.
    echo.
    exit /b 1
)

REM ============================================================
REM Display Banner
REM ============================================================

echo.
echo ========================================
echo        THE FOLD - GENESIS
echo ========================================
echo.

REM ============================================================
REM Start Interactive REPL
REM ============================================================

echo Starting interactive REPL...
echo After loading, login with:
echo   (hi 'opus 'your-name "message")
echo.

"%SCHEME%" --script start-repl.ss

endlocal
