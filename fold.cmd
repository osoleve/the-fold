@echo off
REM fold.cmd — Send expressions to the REPL daemon
REM
REM Usage:
REM   fold "(+ 1 2)"                    — Evaluate expression
REM   fold eval.ss                      — Run a script file
REM   fold -                            — Read from stdin
REM   echo "(+ 1 2)" | fold -           — Pipe expression
REM
REM The daemon must be running (use DAEMON start first).
REM
REM For Claude Code integration, this provides a way to interact
REM with a persistent REPL via file-based IPC.

setlocal enabledelayedexpansion
cd /d "%~dp0"

set "READY_FILE=.fold-repl\ready"
set "REQUEST_FILE=.fold-repl\request.ss"
set "RESPONSE_FILE=.fold-repl\response.txt"
set "ERROR_FILE=.fold-repl\error.txt"
set "SCHEME=C:\Program Files\Chez Scheme 10.3.0\bin\ta6nt\scheme.exe"

REM ============================================================
REM Check if daemon is running
REM ============================================================

if not exist "%READY_FILE%" (
    REM Daemon not running - fall back to direct execution
    if "%1"=="-" (
        REM Read from stdin
        "%SCHEME%" --script fold-stdin.ss
    ) else if exist "%1" (
        REM File argument - load the repl and run script
        echo (load "shell/repl.ss") > "%TEMP%\fold-run.ss"
        echo (load "%1") >> "%TEMP%\fold-run.ss"
        "%SCHEME%" --script "%TEMP%\fold-run.ss"
    ) else (
        REM Expression argument - wrap and execute
        echo (load "shell/repl.ss") > "%TEMP%\fold-run.ss"
        echo %* >> "%TEMP%\fold-run.ss"
        "%SCHEME%" --script "%TEMP%\fold-run.ss"
    )
    goto :eof
)

REM ============================================================
REM Daemon is running - use IPC
REM ============================================================

REM Clear any previous response
if exist "%RESPONSE_FILE%" del "%RESPONSE_FILE%"
if exist "%ERROR_FILE%" del "%ERROR_FILE%"

REM Write request
if "%1"=="-" (
    REM Read from stdin
    copy con "%REQUEST_FILE%" >nul
) else if exist "%1" (
    REM File argument
    copy "%1" "%REQUEST_FILE%" >nul
) else (
    REM Expression argument
    echo %* > "%REQUEST_FILE%"
)

REM Wait for response (with timeout)
set /a TIMEOUT=50
:wait_response
if exist "%RESPONSE_FILE%" goto :got_response
if %TIMEOUT% LEQ 0 (
    echo ERROR: Timeout waiting for response
    exit /b 1
)
set /a TIMEOUT=%TIMEOUT%-1
timeout /t 1 /nobreak >nul 2>nul
goto :wait_response

:got_response
REM Print response
type "%RESPONSE_FILE%"

REM Check for errors
if exist "%ERROR_FILE%" (
    echo.
    echo [Error details in .fold-repl\error.txt]
)

endlocal
