#!/usr/bin/env scheme-script

(doc 'module 'lsp/start-lsp)
(doc 'description "Starts the fold-lsp server. Usage: scheme --script boundary/lsp/start-lsp.ss")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "The server communicates over stdio using the LSP protocol")

(doc 'note "CRITICAL: Capture binary ports FIRST, before any loads. Chez Scheme's standard-input-port/standard-output-port can only be called reliably once - subsequent calls may return EOF ports.")
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

;;; Refresh symbol index for hover/definition (redirect output to stderr
;;; to avoid corrupting LSP protocol on stdout)
;;; Note: index.ss is already loaded by capabilities.ss, just need to refresh
(parameterize ([current-output-port (current-error-port)])
  (guard (e [else (display "Warning: failed to refresh symbol index\n"
                           (current-error-port))])
         (when (top-level-bound? 'index-refresh!)
               (index-refresh!))))

;;; Transfer captured ports to transport layer
(set! *lsp-stdin* *captured-stdin*)
(set! *lsp-stdout* *captured-stdout*)

(run-server!)
