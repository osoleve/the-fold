;;; boundary/ffi/test-ipc-reactor.ss — Integration tests for IPC reactor FFI

(load "core/testing/test-framework.ss")
(load "boundary/ffi/socket-ffi.ss")
(load "lattice/ipc/protocol.ss")

(define *test-sock-path* ".fold-repl/test-ipc.sock")

;;; Helper: clean up any stale socket file and ensure reactor is stopped
(define (ipc-test-cleanup!)
  (guard (ex [else #f])  ; ignore errors during cleanup
    (ipc-reactor-stop!))
  (when (file-exists? *test-sock-path*)
    (delete-file *test-sock-path*)))

;;; Helper: run body with guaranteed reactor cleanup
(define-syntax with-reactor-cleanup
  (syntax-rules ()
    [(_ body ...)
     (dynamic-wind
       (lambda () #f)
       (lambda () body ...)
       (lambda () (ipc-test-cleanup!)))]))

(test-group "ipc-reactor-ffi"

  (define-test "ipc-load! succeeds"
    (assert-true (ipc-load!)))

  (define-test "reactor start and stop"
    (ipc-test-cleanup!)  ; ensure clean slate
    (with-reactor-cleanup
      (assert-true (ipc-reactor-start! *test-sock-path*))
      (assert-true (file-exists? *test-sock-path*))
      (assert-true (ipc-reactor-stop!))))

  (define-test "poll-next returns #f when empty"
    (ipc-test-cleanup!)
    (with-reactor-cleanup
      (ipc-reactor-start! *test-sock-path*)
      (assert-false (ipc-poll-next))))

  (define-test "client-count is 0 with no clients"
    (ipc-test-cleanup!)
    (with-reactor-cleanup
      (ipc-reactor-start! *test-sock-path*)
      (assert-equal (ipc-client-count) 0)))

  (define-test "full round-trip: connect, send, poll, respond, recv"
    (ipc-test-cleanup!)
    (with-reactor-cleanup
      (ipc-reactor-start! *test-sock-path*)
      ;; Small delay for reactor thread startup
      (sleep (make-time 'time-duration 50000000 0))  ; 50ms

      ;; Connect as client
      (let ([fd (ipc-connect *test-sock-path*)])
        ;; Wait for reactor to accept
        (sleep (make-time 'time-duration 50000000 0))
        (assert-equal (ipc-client-count) 1)

        ;; Send request frame from client
        (let* ([req (ipc-make-request/id "test-1" "dev" "(+ 1 2)")]
               [frame (ipc-encode-frame req)])
          (assert-true (ipc-client-send fd frame))

          ;; Wait for reactor to receive
          (sleep (make-time 'time-duration 50000000 0))

          ;; Poll on server side
          (let ([msg (ipc-poll-next)])
            (assert-true (pair? msg))
            (let ([client-id (car msg)]
                  [payload-bv (cdr msg)])
              (assert-true (> client-id 0))
              ;; Decode payload
              (let ([decoded (ipc-decode-payload payload-bv)])
                (assert-equal (ipc-message-type decoded) 'request)
                (assert-equal (ipc-message-id decoded) "test-1")
                (assert-equal (ipc-message-get decoded 'expr) "(+ 1 2)")

                ;; Send response back
                (let* ([resp (ipc-make-result "test-1" "3")]
                       [resp-frame (ipc-encode-frame resp)])
                  (assert-true (ipc-send! client-id resp-frame))

                  ;; Wait for reactor to send
                  (sleep (make-time 'time-duration 50000000 0))

                  ;; Receive on client side
                  (let ([resp-bv (ipc-client-recv fd)])
                    (assert-true (bytevector? resp-bv))
                    (let ([resp-decoded (ipc-decode-payload resp-bv)])
                      (assert-equal (ipc-message-type resp-decoded) 'result)
                      (assert-equal (ipc-message-id resp-decoded) "test-1")
                      (assert-equal (ipc-message-get resp-decoded 'value) "3"))))))))

        ;; Cleanup client connection
        (ipc-client-close fd))))
)

(run-all-tests)
