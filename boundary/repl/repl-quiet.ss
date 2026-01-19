;;; boundary/repl/repl-quiet.ss — Quiet REPL loader
;;;
;;; Loads the REPL without the startup banner.
;;; Usage: (load "boundary/repl-quiet.ss")

(define *quiet* #t)
(load "boundary/repl.ss")
