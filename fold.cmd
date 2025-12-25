@echo off
setlocal enabledelayedexpansion
set "REPO=C:\Users\andre\Documents\ccverse"
set "SCHEME=C:\Program Files\Chez Scheme 10.3.0\bin\ta6nt\scheme.exe"
set "SCRIPT=%~1"

if "%SCRIPT%"=="" (
    echo Usage: fold script-name
    exit /b 1
)

REM Add .ss if needed
if not "!SCRIPT:~-3!"==".ss" set "SCRIPT=%SCRIPT%.ss"

REM Find the script
if exist "%REPO%\%SCRIPT%" (
    cd /d "%REPO%"
    "%SCHEME%" --script "%SCRIPT%"
    goto :end
)
if exist "%REPO%\core\%SCRIPT%" (
    cd /d "%REPO%\core"
    "%SCHEME%" --script "%SCRIPT%"
    goto :end
)
if exist "%REPO%\shell\%SCRIPT%" (
    cd /d "%REPO%"
    "%SCHEME%" --script "shell/%SCRIPT%"
    goto :end
)
if exist "%REPO%\core\test-%SCRIPT%" (
    cd /d "%REPO%\core"
    "%SCHEME%" --script "test-%SCRIPT%"
    goto :end
)

echo Script not found: %SCRIPT%
exit /b 1

:end
