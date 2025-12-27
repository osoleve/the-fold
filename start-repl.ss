;;; start-repl.ss — Bootstrap script for persistent REPL
;;;
;;; Loads the full REPL with banner, then enters interactive mode.
;;; Usage: scheme --script start-repl.ss

(load "thimble/repl.ss")
(new-cafe)
