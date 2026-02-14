;;; lattice/ipc/test-protocol.ss — Tests for IPC wire protocol

(load "core/testing/test-framework.ss")
(load "lattice/ipc/protocol.ss")

(test-group "ipc-protocol"

  ;;; ====
  ;;; Frame encoding/decoding round-trip
  ;;; ====

  (define-test "encode-decode round-trip: simple alist"
    (let* ([msg '((type . request) (id . "req-1") (session . "dev") (expr . "(+ 1 2)"))]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-type decoded) 'request)
      (assert-equal (ipc-message-id decoded) "req-1")
      (assert-equal (ipc-message-get decoded 'session) "dev")
      (assert-equal (ipc-message-get decoded 'expr) "(+ 1 2)")))

  (define-test "encode-decode round-trip: result message"
    (let* ([msg (ipc-make-result "req-42" "3")]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-type decoded) 'result)
      (assert-equal (ipc-message-id decoded) "req-42")
      (assert-equal (ipc-message-get decoded 'value) "3")))

  (define-test "encode-decode round-trip: error message"
    (let* ([msg (ipc-make-error "req-7" 'eval-error "file not found")]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-type decoded) 'error)
      (assert-equal (ipc-message-id decoded) "req-7")
      (assert-equal (ipc-message-get decoded 'code) 'eval-error)
      (assert-equal (ipc-message-get decoded 'message) "file not found")))

  (define-test "encode-decode round-trip: stream message"
    (let* ([msg (ipc-make-stream "req-9" "partial data..." #f)]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-type decoded) 'stream)
      (assert-equal (ipc-message-get decoded 'chunk) "partial data...")
      (assert-equal (ipc-message-get decoded 'final) #f)))

  (define-test "encode-decode round-trip: stream final"
    (let* ([msg (ipc-make-stream "req-9" "done" #t)]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-get decoded 'final) #t)))

  (define-test "encode-decode round-trip: ping/pong"
    (let* ([ping (ipc-make-ping)]
           [pong (ipc-make-pong)]
           [ping-frame (ipc-encode-frame ping)]
           [pong-frame (ipc-encode-frame pong)]
           [ping-decoded (ipc-decode-frame ping-frame)]
           [pong-decoded (ipc-decode-frame pong-frame)])
      (assert-equal (ipc-message-type ping-decoded) 'ping)
      (assert-equal (ipc-message-type pong-decoded) 'pong)))

  (define-test "encode-decode round-trip: ready"
    (let* ([msg (ipc-make-ready)]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-type decoded) 'ready)))

  (define-test "encode-decode round-trip: shutdown/shutdown-ack"
    (let* ([shutdown (ipc-make-shutdown)]
           [ack (ipc-make-shutdown-ack)]
           [s-frame (ipc-encode-frame shutdown)]
           [a-frame (ipc-encode-frame ack)]
           [s-decoded (ipc-decode-frame s-frame)]
           [a-decoded (ipc-decode-frame a-frame)])
      (assert-equal (ipc-message-type s-decoded) 'shutdown)
      (assert-equal (ipc-message-type a-decoded) 'shutdown-ack)
      (assert-true (ipc-valid-message? shutdown))
      (assert-true (ipc-valid-message? ack))))

  ;;; ====
  ;;; Frame structure
  ;;; ====

  (define-test "frame header contains correct length"
    (let* ([msg '((type . ping))]
           [frame (ipc-encode-frame msg)]
           [payload-str (format "~s" msg)]
           [expected-len (bytevector-length (string->utf8 payload-str))])
      (assert-equal (ipc-frame-length frame) expected-len)))

  (define-test "frame total length = header + payload"
    (let* ([msg '((type . result) (id . "x") (value . "hello world"))]
           [frame (ipc-encode-frame msg)]
           [payload-len (ipc-frame-length frame)])
      (assert-equal (bytevector-length frame) (+ 4 payload-len))))

  ;;; ====
  ;;; Edge cases
  ;;; ====

  (define-test "round-trip: nested s-expression in value"
    (let* ([msg (ipc-make-result "req-1" "(list 1 2 (+ 3 4))")]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-get decoded 'value) "(list 1 2 (+ 3 4))")))

  (define-test "round-trip: empty string value"
    (let* ([msg (ipc-make-result "req-1" "")]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-get decoded 'value) "")))

  (define-test "round-trip: string with special characters"
    (let* ([msg (ipc-make-result "req-1" "hello\nworld\ttab\"quote")]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-get decoded 'value) "hello\nworld\ttab\"quote")))

  (define-test "round-trip: string with unicode"
    (let* ([msg (ipc-make-result "req-1" "lambda: \x3BB;")]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-get decoded 'value) "lambda: \x3BB;")))

  (define-test "round-trip: numeric values in alist"
    (let* ([msg '((type . info) (count . 42) (ratio . 3/4))]
           [frame (ipc-encode-frame msg)]
           [decoded (ipc-decode-frame frame)])
      (assert-equal (ipc-message-get decoded 'count) 42)
      (assert-equal (ipc-message-get decoded 'ratio) 3/4)))

  ;;; ====
  ;;; Decode payload (without header)
  ;;; ====

  (define-test "decode-payload: raw bytes without header"
    (let* ([msg '((type . result) (id . "r1") (value . "ok"))]
           [payload-bv (string->utf8 (format "~s" msg))]
           [decoded (ipc-decode-payload payload-bv)])
      (assert-equal (ipc-message-type decoded) 'result)
      (assert-equal (ipc-message-get decoded 'value) "ok")))

  ;;; ====
  ;;; Error handling
  ;;; ====

  (define-test "decode-frame: too short for header"
    (assert-error (lambda () (ipc-decode-frame (make-bytevector 2)))))

  (define-test "decode-frame: truncated payload"
    (let ([bv (make-bytevector 8)])
      ;; Header says 100 bytes, but only 4 payload bytes
      (bytevector-u32-set! bv 0 100 (endianness big))
      (assert-error (lambda () (ipc-decode-frame bv)))))

  (define-test "frame-length: too short"
    (assert-error (lambda () (ipc-frame-length (make-bytevector 2)))))

  ;;; ====
  ;;; Message constructors
  ;;; ====

  (define-test "make-request generates unique ids"
    (set! *ipc-request-counter* 0)
    (let ([r1 (ipc-make-request "dev" "(+ 1 2)")]
          [r2 (ipc-make-request "dev" "(+ 3 4)")])
      (assert-true (not (string=? (ipc-message-id r1) (ipc-message-id r2))))
      (assert-equal (ipc-message-type r1) 'request)
      (assert-equal (ipc-message-get r1 'session) "dev")
      (assert-equal (ipc-message-get r1 'expr) "(+ 1 2)")))

  (define-test "make-request/id uses provided id"
    (let ([r (ipc-make-request/id "my-id" "dev" "(+ 1 2)")])
      (assert-equal (ipc-message-id r) "my-id")))

  ;;; ====
  ;;; Validation
  ;;; ====

  (define-test "valid-message?: valid request"
    (assert-true (ipc-valid-message? (ipc-make-request "s" "e"))))

  (define-test "valid-message?: valid result"
    (assert-true (ipc-valid-message? (ipc-make-result "id" "val"))))

  (define-test "valid-message?: valid ping"
    (assert-true (ipc-valid-message? (ipc-make-ping))))

  (define-test "valid-message?: empty list"
    (assert-false (ipc-valid-message? '())))

  (define-test "valid-message?: not a list"
    (assert-false (ipc-valid-message? 42)))

  (define-test "valid-message?: missing type"
    (assert-false (ipc-valid-message? '((id . "x")))))

  (define-test "request?: valid"
    (assert-true (ipc-request? (ipc-make-request "dev" "(+ 1 2)"))))

  (define-test "request?: missing session"
    (assert-false (ipc-request? '((type . request) (id . "x") (expr . "e")))))

  (define-test "result?: valid"
    (assert-true (ipc-result? (ipc-make-result "id" "val"))))

  (define-test "error?: valid"
    (assert-true (ipc-error? (ipc-make-error "id" 'eval-error "msg"))))

  (define-test "error?: not an error"
    (assert-false (ipc-error? (ipc-make-result "id" "val"))))
)

(run-all-tests)
