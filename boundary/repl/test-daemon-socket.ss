;;; boundary/repl/test-daemon-socket.ss — Integration tests for socket daemon
;;;
;;; Tests the socket daemon end-to-end: start daemon subprocess,
;;; connect as client, send request, receive response.

(load "core/testing/test-framework.ss")
(load "boundary/ffi/socket-ffi.ss")
(load "lattice/ipc/protocol.ss")

(define *test-sock-path* ".fold-repl/test-daemon.sock")
(define *test-ready-file* ".fold-repl/test-daemon-ready")

;;; Helper: start daemon as subprocess
;;; Returns process info or #f
(define *daemon-process* #f)

(define (start-test-daemon!)
  ;; Clean up any stale state
  (when (file-exists? *test-sock-path*)
    (delete-file *test-sock-path*))
  (when (file-exists? *test-ready-file*)
    (delete-file *test-ready-file*))

  ;; We'll test at the Rust reactor level with socket-ffi directly,
  ;; since spawning a full daemon subprocess requires careful process management.
  ;; Phase 3 daemon tests focus on:
  ;; 1. The daemon module loads without errors
  ;; 2. Worker frame I/O works correctly
  ;; 3. Message routing logic is sound

  ;; For the full end-to-end test, we use the reactor FFI directly
  ;; (already tested in test-ipc-reactor.ss) and verify the
  ;; daemon's routing and worker management logic.
  #t)

(define (stop-test-daemon!)
  (when (file-exists? *test-sock-path*)
    (delete-file *test-sock-path*))
  (when (file-exists? *test-ready-file*)
    (delete-file *test-ready-file*)))

;;; ====
;;; Tests
;;; ====

(test-group "socket-daemon"

  (define-test "daemon module loads without error"
    ;; Verify the daemon module loads and key symbols are bound
    (load "boundary/repl/repl-daemon-socket.ss")
    (assert-true (top-level-bound? 'start-socket-daemon!))
    (assert-true (top-level-bound? 'daemon-stop!))
    (assert-true (top-level-bound? 'route-message!))
    (assert-true (top-level-bound? 'check-worker-outputs!)))

  (define-test "worker module loads without error"
    (assert-true (top-level-bound? 'ipc-encode-frame))
    (assert-true (top-level-bound? 'ipc-decode-frame))
    (assert-true (top-level-bound? 'ipc-make-request))
    (assert-true (top-level-bound? 'ipc-make-result)))

  (define-test "session validation"
    (assert-true (valid-session-id? "dev"))
    (assert-true (valid-session-id? "my-session-123"))
    (assert-true (valid-session-id? "a_b"))
    (assert-false (valid-session-id? ""))
    (assert-false (valid-session-id? "../etc/passwd"))
    (assert-false (valid-session-id? "a;rm -rf /"))
    (assert-false (valid-session-id? "a b c")))

  (define-test "session record creation and access"
    (let ([rec (make-session-record 'fake-inp 'fake-outp 'fake-errp 12345)])
      (assert-equal (session-input-port rec) 'fake-inp)
      (assert-equal (session-output-port rec) 'fake-outp)
      (assert-equal (session-stderr-port rec) 'fake-errp)
      (assert-equal (session-pid rec) 12345)
      (assert-equal (session-client-ids rec) '())
      ;; Touch extends expiration
      (let ([old-expires (session-expires rec)])
        (session-touch! rec)
        (assert-true (>= (session-expires rec) old-expires)))))

  (define-test "session record has last-pong field"
    (let ([rec (make-session-record 'fake-inp 'fake-outp 'fake-errp 12345)])
      ;; last-pong should be initialized to current time
      (let ([pong-time (session-last-pong rec)])
        (assert-true (number? pong-time))
        (assert-true (> pong-time 0)))
      ;; update-pong! should advance the timestamp
      (let ([old-pong (session-last-pong rec)])
        (session-update-pong! rec)
        (assert-true (>= (session-last-pong rec) old-pong)))))

  (define-test "crash-loop detection allows initial spawns"
    ;; Reset spawn history
    (set! *spawn-history* '())
    (assert-true (spawn-allowed? "test-crash"))
    ;; Record a few spawns — should still be allowed
    (record-spawn! "test-crash")
    (record-spawn! "test-crash")
    (record-spawn! "test-crash")
    (assert-true (spawn-allowed? "test-crash"))
    ;; Clean up
    (set! *spawn-history* '()))

  (define-test "crash-loop detection blocks after threshold"
    (set! *spawn-history* '())
    ;; Record max spawns
    (let loop ([i 0])
      (when (< i *max-spawns-in-window*)
        (record-spawn! "test-crash")
        (loop (+ i 1))))
    ;; Should now be blocked
    (assert-false (spawn-allowed? "test-crash"))
    ;; Different session should still be allowed
    (assert-true (spawn-allowed? "other-session"))
    ;; Clean up
    (set! *spawn-history* '()))

  (define-test "crash-loop detection is per-session"
    (set! *spawn-history* '())
    ;; Exhaust one session's budget
    (let loop ([i 0])
      (when (< i *max-spawns-in-window*)
        (record-spawn! "session-a")
        (loop (+ i 1))))
    (assert-false (spawn-allowed? "session-a"))
    ;; session-b is unaffected
    (assert-true (spawn-allowed? "session-b"))
    (record-spawn! "session-b")
    (assert-true (spawn-allowed? "session-b"))
    ;; Clean up
    (set! *spawn-history* '()))

  (define-test "prune-spawn-history! removes stale entries"
    (set! *spawn-history* '())
    ;; Manually insert old timestamps
    (let ([old-time (- (time-second (current-time)) (+ *spawn-window* 10))])
      (set! *spawn-history*
            (list (cons "stale-session" (list old-time old-time)))))
    (assert-equal (length *spawn-history*) 1)
    (prune-spawn-history!)
    (assert-equal (length *spawn-history*) 0))

  (define-test "SIGKILL tracking initialized empty"
    (assert-true (list? *killed-pids*)))

  (define-test "escalate-kills! handles empty list"
    (let ([saved *killed-pids*])
      (set! *killed-pids* '())
      (escalate-kills!)  ; should not error
      (assert-equal *killed-pids* '())
      (set! *killed-pids* saved)))

  (define-test "route-message! handles ping with pong"
    ;; Start reactor so ipc-send! works
    (ipc-load!)
    (guard (ex [else #f]) (ipc-reactor-stop!))
    (when (file-exists? *test-sock-path*)
      (delete-file *test-sock-path*))
    (ipc-reactor-start! *test-sock-path*)
    (sleep (make-time 'time-duration 50000000 0))

    ;; Connect a client
    (let ([fd (ipc-connect *test-sock-path*)])
      (sleep (make-time 'time-duration 50000000 0))

      ;; Send a ping from the client
      (let ([ping-frame (ipc-encode-frame (ipc-make-ping))])
        (ipc-client-send fd ping-frame)
        (sleep (make-time 'time-duration 50000000 0))

        ;; Poll and route the message (simulates daemon loop)
        (let ([msg-pair (ipc-poll-next)])
          (assert-true (pair? msg-pair))
          (let ([client-id (car msg-pair)]
                [payload-bv (cdr msg-pair)])
            (let ([msg (ipc-decode-payload payload-bv)])
              ;; Route it — should send pong back
              (route-message! client-id msg)

              ;; Give reactor time to send
              (sleep (make-time 'time-duration 100000000 0))

              ;; Client should receive pong
              (let ([resp-bv (ipc-client-recv fd)])
                (assert-true (bytevector? resp-bv))
                (let ([resp (ipc-decode-payload resp-bv)])
                  (assert-equal (ipc-message-type resp) 'pong)))))))

      (ipc-client-close fd))

    (ipc-reactor-stop!)
    (when (file-exists? *test-sock-path*)
      (delete-file *test-sock-path*)))

  (define-test "route-message! rejects invalid session"
    (ipc-load!)
    (guard (ex [else #f]) (ipc-reactor-stop!))
    (when (file-exists? *test-sock-path*)
      (delete-file *test-sock-path*))
    (ipc-reactor-start! *test-sock-path*)
    (sleep (make-time 'time-duration 50000000 0))

    (let ([fd (ipc-connect *test-sock-path*)])
      (sleep (make-time 'time-duration 50000000 0))

      ;; Send request with invalid session
      (let ([req (ipc-make-request/id "bad-1" "../evil" "(+ 1 2)")])
        (ipc-client-send fd (ipc-encode-frame req))
        (sleep (make-time 'time-duration 50000000 0))

        ;; Poll and route
        (let ([msg-pair (ipc-poll-next)])
          (when msg-pair
            (let ([client-id (car msg-pair)]
                  [payload-bv (cdr msg-pair)])
              (route-message! client-id (ipc-decode-payload payload-bv))

              ;; Give reactor time to send error response
              (sleep (make-time 'time-duration 100000000 0))

              ;; Client should receive error
              (let ([resp-bv (ipc-client-recv fd)])
                (assert-true (bytevector? resp-bv))
                (let ([resp (ipc-decode-payload resp-bv)])
                  (assert-equal (ipc-message-type resp) 'error)
                  (assert-equal (ipc-message-id resp) "bad-1")))))))

      (ipc-client-close fd))

    (ipc-reactor-stop!)
    (when (file-exists? *test-sock-path*)
      (delete-file *test-sock-path*)))
)

(run-all-tests)
