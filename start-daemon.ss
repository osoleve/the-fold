;;; start-daemon.ss — Bootstrap the REPL daemon
;;;
;;; Usage: scheme --script start-daemon.ss
;;;
;;; This loads the MCP daemon module which supports multi-session
;;; IPC for multitenancy. Each session gets isolated environments.

(load "shell/repl-daemon-mcp.ss")
(start-daemon!)
