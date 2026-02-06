;;; boundary/ipc/socket-client.ss — Scheme Socket Client for Fold IPC
;;;
;;; Drop-in replacement for file-based fold-ipc-eval in effects/fold.ss.
;;; Same interface: (fold-ipc-eval expr) → (list 'fold-result ok? value error)
;;;
;;; Uses the Rust FFI client functions from socket-ffi.ss for socket I/O.

(load "boundary/ffi/socket-ffi.ss")
(unless (top-level-bound? 'doc)
  (load "core/base/prelude.ss"))
(load "lattice/ipc/protocol.ss")

;;; ====
;;; Configuration
;;; ====

(define *ipc-socket-path* ".fold-repl/fold.sock")

;;; Current connection fd (lazily established)
(define *ipc-conn-fd* #f)

;;; Current pipeline session
(define *pipeline-session*
  (if (top-level-bound? '*pipeline-session*)
      *pipeline-session*
      (make-parameter "pipeline")))

;;; ====
;;; Connection Management
;;; ====

;;; fold-ipc-connect! : → Boolean
;;; Establish connection to the daemon if not already connected.
(define (fold-ipc-connect!)
  (if *ipc-conn-fd*
      #t
      (begin
        (unless (ipc-load!)
          (error 'fold-ipc-connect! "Could not load IPC FFI library"))
        (guard (ex [else
                    (set! *ipc-conn-fd* #f)
                    #f])
          (set! *ipc-conn-fd* (ipc-connect *ipc-socket-path*))
          #t))))

;;; fold-ipc-disconnect! : → Void
;;; Close the connection to the daemon.
(define (fold-ipc-disconnect!)
  (when *ipc-conn-fd*
    (guard (ex [else #f])
      (ipc-client-close *ipc-conn-fd*))
    (set! *ipc-conn-fd* #f)))

;;; fold-ipc-reconnect! : → Boolean
;;; Close and re-establish the connection.
(define (fold-ipc-reconnect!)
  (fold-ipc-disconnect!)
  (fold-ipc-connect!))

;;; ====
;;; Core IPC Function
;;; ====

;;; fold-ipc-eval-socket : String → FoldResult
;;; Evaluate an expression via the Fold socket daemon.
;;; Returns (list 'fold-result ok? value error)
;;; Same interface as the file-based fold-ipc-eval in effects/fold.ss.
(define (fold-ipc-eval-socket expr)
  (guard (ex [else
              (list 'fold-result #f #f
                    (format "fold-ipc-eval error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
    ;; Ensure connected
    (unless (fold-ipc-connect!)
      (error 'fold-ipc-eval "Could not connect to daemon"))

    (let* ([session (if (procedure? *pipeline-session*)
                        (*pipeline-session*)
                        "pipeline")]
           [req (ipc-make-request session expr)]
           [req-id (ipc-message-id req)]
           [frame (ipc-encode-frame req)])

      ;; Send request
      (guard (ex [else
                  ;; Send failed — try reconnecting once
                  (fold-ipc-reconnect!)
                  (unless *ipc-conn-fd*
                    (error 'fold-ipc-eval "Could not reconnect to daemon"))
                  (ipc-client-send *ipc-conn-fd* frame)])
        (ipc-client-send *ipc-conn-fd* frame))

      ;; Wait for response (blocking read with 120s timeout in Rust)
      (let ([resp-bv (ipc-client-recv *ipc-conn-fd*)])
        (if resp-bv
            (let ([msg (ipc-decode-payload resp-bv)])
              (case (ipc-message-type msg)
                [(result)
                 (list 'fold-result #t (ipc-message-get msg 'value) #f)]
                [(error)
                 (list 'fold-result #f #f
                       (or (ipc-message-get msg 'message) "unknown error"))]
                [else
                 (list 'fold-result #f #f
                       (format "unexpected response type: ~a"
                               (ipc-message-type msg)))]))
            ;; Timeout or connection lost
            (begin
              (fold-ipc-disconnect!)
              (list 'fold-result #f #f "timeout waiting for daemon response")))))))

;;; ====
;;; Result Accessors (same as effects/fold.ss)
;;; ====

(define (fold-result-ok? r) (list-ref r 1))
(define (fold-result-value r) (list-ref r 2))
(define (fold-result-error r) (list-ref r 3))
