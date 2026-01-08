#!/usr/bin/env scheme-script
;;; shell/lsp/start-lsp.ss — LSP Server Launcher
;;;
;;; Starts the fold-lsp server.
;;; Usage: scheme --script shell/lsp/start-lsp.ss
;;;
;;; The server communicates over stdio using the LSP protocol.

;;; Change to project root if needed
(let ([cwd (current-directory)])
     (unless (file-exists? "core/base/prelude.ss")
             ;; Try to find the project root
             (when (file-exists? "/home/oso/the-fold/core/base/prelude.ss")
                   (current-directory "/home/oso/the-fold"))))

;;; Load and run the server
(load "shell/lsp/lsp-server.ss")
(run-server!)
