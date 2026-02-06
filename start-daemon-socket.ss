;;; start-daemon-socket.ss — Bootstrap the socket-based REPL daemon
;;;
;;; Usage: scheme --script start-daemon-socket.ss

(load "boundary/repl/repl-daemon-socket.ss")
(start-socket-daemon!)
