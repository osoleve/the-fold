;;; thimble/repl-quiet.ss — Quiet REPL loader
;;;
;;; Loads the REPL without the startup banner.
;;; Usage: (load "thimble/repl-quiet.ss")

(define *quiet* #t)
(load "thimble/repl.ss")
