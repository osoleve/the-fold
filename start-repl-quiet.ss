;;; start-repl-quiet.ss — Bootstrap script for persistent REPL (quiet mode)
;;;
;;; Loads the REPL without banner, then enters interactive mode.
;;; Usage: scheme --script start-repl-quiet.ss

(load "shell/repl-quiet.ss")
(new-cafe)
