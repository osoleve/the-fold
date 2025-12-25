;;; start-daemon.ss — Bootstrap the REPL daemon
;;;
;;; Usage: scheme --script start-daemon.ss
;;;
;;; This loads the daemon module and starts the loop.

(load "shell/repl-daemon.ss")
(start-daemon!)
