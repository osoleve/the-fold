;;; core/lsp/test-protocol.ss — Tests for LSP Protocol

(load "boundary/lsp/protocol.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(display "Testing protocol.ss\n")
(display "====\n\n")

;;; ====
;;; Message Classification Tests
;;; ====

(display "Message Classification:\n")

;; Request with id
(define request-msg '(json-object ("jsonrpc" . "2.0")
                      ("id" . 1)
                      ("method" . "initialize")
                      ("params" . (json-object))))
(test "lsp-request? valid" #t (if (lsp-request? request-msg) #t #f))
(test "lsp-notification? request" #f (lsp-notification? request-msg))
(test "lsp-response? request" #f (lsp-response? request-msg))

;; Request with id: 0 (edge case from P0 fix)
(define request-id-0 '(json-object ("jsonrpc" . "2.0")
                       ("id" . 0)
                       ("method" . "test")))
(test "lsp-request? id:0" #t (if (lsp-request? request-id-0) #t #f))

;; Notification (no id)
(define notif-msg '(json-object ("jsonrpc" . "2.0")
                    ("method" . "initialized")
                    ("params" . (json-object))))
(test "lsp-notification? valid" #t (if (lsp-notification? notif-msg) #t #f))
(test "lsp-request? notification" #f (lsp-request? notif-msg))

;; Response with result
(define response-result '(json-object ("jsonrpc" . "2.0")
                          ("id" . 1)
                          ("result" . (json-object))))
(test "lsp-response? result" #t (if (lsp-response? response-result) #t #f))

;; Response with id: 0 (edge case)
(define response-id-0 '(json-object ("jsonrpc" . "2.0")
                        ("id" . 0)
                        ("result" . null)))
(test "lsp-response? id:0" #t (if (lsp-response? response-id-0) #t #f))

;; Response with error
(define response-error '(json-object ("jsonrpc" . "2.0")
                         ("id" . 1)
                         ("error" . (json-object ("code" . -32600)
                                                 ("message" . "Invalid")))))
(test "lsp-response? error" #t (if (lsp-response? response-error) #t #f))

;;; ====
;;; Message Accessor Tests
;;; ====

(display "\nMessage Accessors:\n")

(test "lsp-message-id" 1 (lsp-message-id request-msg))
(test "lsp-message-method" "initialize" (lsp-message-method request-msg))
(test "lsp-message-params type" 'json-object
      (car (lsp-message-params request-msg)))

;;; ====
;;; Constructor Tests
;;; ====

(display "\nConstructors:\n")

;; Position
(let ([pos (make-position 5 10)])
     (test "make-position line" 5 (json-get pos "line"))
     (test "make-position character" 10 (json-get pos "character")))

;; Range
(let* ([start (make-position 0 0)]
       [end (make-position 0 10)]
       [range (make-range start end)])
      (test "make-range start" start (json-get range "start"))
      (test "make-range end" end (json-get range "end")))

;; Location
(let ([loc (make-location "file:///test.ss"
                          (make-range (make-position 0 0)
                                      (make-position 0 10)))])
     (test "make-location uri" "file:///test.ss" (json-get loc "uri")))

;; Diagnostic (range, message, severity)
(let ([diag (make-diagnostic (make-range (make-position 0 0)
                                         (make-position 0 5))
                             "Test error"
                             *severity-error*)])
     (test "make-diagnostic severity" *severity-error* (json-get diag "severity"))
     (test "make-diagnostic message" "Test error" (json-get diag "message")))

;; Hover
(let ([hover (make-hover "**bold** text")])
     (test "make-hover has contents" #t
           (if (json-get hover "contents") #t #f)))

;; Completion item
(let ([item (make-completion-item "map" *completion-function* "(α→β)→List α→List β")])
     (test "make-completion-item label" "map" (json-get item "label"))
     (test "make-completion-item kind" *completion-function* (json-get item "kind")))

;; Signature help
(let* ([param (make-parameter-info "f" "Function parameter")]
       [sig (make-signature-info "(map f lst)" "Apply f to each element" (list param))]
       [help (make-signature-help (list sig) 0 0)])
      (test "make-signature-help activeSignature" 0 (json-get help "activeSignature"))
      (test "make-signature-help activeParameter" 0 (json-get help "activeParameter")))

;;; ====
;;; URI Utilities Tests
;;; ====

(display "\nURI Utilities:\n")

(test "path->uri unix" "file:///home/test.ss" (path->uri "/home/test.ss"))
(test "uri->path" "/home/test.ss" (uri->path "file:///home/test.ss"))
(test "uri->path passthrough" "/already/path" (uri->path "/already/path"))

;;; ====
;;; Response Construction Tests
;;; ====

(display "\nResponse Construction:\n")

(let ([resp (make-response 1 '(json-object ("result" . "ok")))])
     (test "make-response id" 1 (json-get resp "id"))
     (test "make-response jsonrpc" "2.0" (json-get resp "jsonrpc")))

(let ([err (make-error-response 1 -32600 "Invalid request")])
     (test "make-error-response has error" #t
           (if (json-get err "error") #t #f)))

;;; Summary
(display "\n====\n")
(printf "Passed: ~a, Failed: ~a\n" tests-passed tests-failed)
(when (> tests-failed 0)
      (exit 1))
