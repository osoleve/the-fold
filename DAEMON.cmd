@echo off
REM DAEMON.cmd — Start the REPL daemon in background
REM
REM This starts a persistent REPL daemon that Claude Code can communicate with
REM via file-based IPC. Supports multi-session for parallel agents:
REM
REM   .fold-repl/requests/<session-id>.ss   — Write session requests here
REM   .fold-repl/responses/<session-id>.txt — Read session results here
REM   .fold-repl/ready                      — Exists when daemon is running
REM
REM Usage:
REM   DAEMON start   — Start the daemon in background
REM   DAEMON stop    — Stop the daemon
REM   DAEMON status  — Check if daemon is running
REM   DAEMON fg      — Run daemon in foreground (for debugging)
REM   DAEMON cleanup — Cleanup stale workers by heartbeat age

setlocal enabledelayedexpansion
cd /d "%~dp0"

set "SCHEME=C:\Program Files\Chez Scheme 10.3.0\bin\ta6nt\scheme.exe"
set "READY_FILE=.fold-repl\ready"

if "%1"=="" goto :usage
if "%1"=="start" goto :start
if "%1"=="stop" goto :stop
if "%1"=="status" goto :status
if "%1"=="fg" goto :foreground
if "%1"=="cleanup" goto :cleanup
goto :usage

:start
if exist "%READY_FILE%" (
    echo Daemon is already running.
    echo Use 'DAEMON stop' to stop it first.
    exit /b 1
)
echo Starting REPL daemon in background...
set "FOLD_SCHEME_CMD=%SCHEME%"
start /b "" "%SCHEME%" --script start-daemon-mcp.ss
REM Wait for ready file
:wait_ready
if not exist "%READY_FILE%" (
    timeout /t 1 /nobreak >nul
    goto :wait_ready
)
echo Daemon started. Write to .fold-repl\requests\^<session-id^>.ss.
goto :eof

:stop
if not exist "%READY_FILE%" (
    echo Daemon is not running.
    exit /b 0
)
echo Stopping daemon...
del "%READY_FILE%" 2>nul
if exist ".fold-repl\\workers\\*.pid" (
    for %%F in (.fold-repl\\workers\\*.pid) do (
        set /p WPID=<%%F
        taskkill /PID !WPID! /F >nul 2>nul
    )
)
echo Daemon stopped.
goto :eof

:cleanup
echo Cleaning up stale workers...
set "FOLD_SCHEME_CMD=%SCHEME%"
"%SCHEME%" --script thimble\\cleanup-workers.ss
goto :eof

:status
if exist "%READY_FILE%" (
    echo Daemon is running.
    echo Ready file: %READY_FILE%
) else (
    echo Daemon is not running.
)
goto :eof

:foreground
echo Running daemon in foreground (Ctrl+C to stop)...
set "FOLD_SCHEME_CMD=%SCHEME%"
"%SCHEME%" --script start-daemon-mcp.ss
goto :eof

:usage
echo.
echo Usage: DAEMON [start^|stop^|status^|fg^|cleanup]
echo.
echo   start    — Start the daemon in background
echo   stop     — Stop the daemon
echo   status   — Check if daemon is running
echo   fg       — Run daemon in foreground
echo   cleanup  — Cleanup stale workers by heartbeat age
echo.
exit /b 1
