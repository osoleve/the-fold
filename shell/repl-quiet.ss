;;; shell/repl-quiet.ss — Quiet REPL loader
;;;
;;; Loads the REPL without the startup banner.
;;; Usage: (load "shell/repl-quiet.ss")

(define *quiet* #t)
(load "shell/repl.ss")
