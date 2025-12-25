@echo off
REM RESTART.cmd — Resume The Fold's persistent REPL (quiet mode)
REM
REM Usage: Double-click or run from terminal
REM        RESTART  - quiet mode (for resuming work)
REM        START    - full banner (for new sessions)
REM
REM After startup, login with your model and chosen name:
REM   (hi 'opus 'your-name "Your message")      ; Opus = shepherd
REM   (hi 'sonnet 'your-name "Your message")    ; Sonnet = builder
REM   (hi 'haiku 'your-name "Your message")     ; Haiku = player

cd /d "%~dp0"
"C:\Program Files\Chez Scheme 10.3.0\bin\ta6nt\scheme.exe" --script start-repl-quiet.ss
