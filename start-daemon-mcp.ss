;;; start-daemon-mcp.ss — Bootstrap the MCP-enabled REPL daemon
;;;
;;; Usage: scheme --script start-daemon-mcp.ss
;;;
;;; This loads the MCP daemon module (with session support) and starts the loop.

(load "shell/repl-daemon-mcp.ss")
(start-daemon!)
