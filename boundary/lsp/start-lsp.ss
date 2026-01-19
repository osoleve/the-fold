#!/usr/bin/env scheme-script
;;; boundary/lsp/start-lsp.ss — LSP Server Launcher
;;;
;;; Starts the fold-lsp server.
;;; Usage: scheme --script boundary/lsp/start-lsp.ss
;;;
;;; The server communicates over stdio using the LSP protocol.

;;; CRITICAL: Capture binary ports FIRST, before any loads.
;;; Chez Scheme's standard-input-port/standard-output-port can only be
;;; called reliably once - subsequent calls may return EOF ports.
(define *captured-stdin* (standard-input-port))
(define *captured-stdout* (standard-output-port))

;;; Change to project root if needed
(let ([cwd (current-directory)])
     (unless (file-exists? "core/base/prelude.ss")
             ;; Try to find the project root
             (when (file-exists? "/home/oso/the-fold/core/base/prelude.ss")
                   (current-directory "/home/oso/the-fold"))))

;;; Load and run the server
(load "boundary/lsp/lsp-server.ss")

;;; Load symbol index for hover/definition (redirect output to stderr
;;; to avoid corrupting LSP protocol on stdout)
(parameterize ([current-output-port (current-error-port)])
  (guard (e [else (display "Warning: failed to load symbol index\n"
                           (current-error-port))])
         (load "boundary/tools/index.ss")
         (index-refresh!)))

;;; Transfer captured ports to transport layer
(set! *lsp-stdin* *captured-stdin*)
(set! *lsp-stdout* *captured-stdout*)

(run-server!)
