(load "core/testing/test-framework.ss")
(load "boundary/lsp/protocol.ss")

(doc 'module 'lsp/test-protocol)
(doc 'description "Tests for LSP Protocol")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "Testing protocol.ss\n")
(display "====\n\n")

;;; ====
;;; Message Classification Tests
;;; ====

;; Test fixtures
(define request-msg '(json-object ("jsonrpc" . "2.0")
                      ("id" . 1)
                      ("method" . "initialize")
                      ("params" . (json-object))))

(define request-id-0 '(json-object ("jsonrpc" . "2.0")
                       ("id" . 0)
                       ("method" . "test")))

(define notif-msg '(json-object ("jsonrpc" . "2.0")
                    ("method" . "initialized")
                    ("params" . (json-object))))

(define response-result '(json-object ("jsonrpc" . "2.0")
                          ("id" . 1)
                          ("result" . (json-object))))

(define response-id-0 '(json-object ("jsonrpc" . "2.0")
                        ("id" . 0)
                        ("result" . null)))

(define response-error '(json-object ("jsonrpc" . "2.0")
                         ("id" . 1)
                         ("error" . (json-object ("code" . -32600)
                                                 ("message" . "Invalid")))))

(test-group message-classification
  (define-test lsp-request-valid
    (assert-true (if (lsp-request? request-msg) #t #f)))

  (define-test lsp-notification-not-request
    (assert-false (lsp-notification? request-msg)))

  (define-test lsp-response-not-request
    (assert-false (lsp-response? request-msg)))

  (define-test lsp-request-id-zero
    (assert-true (if (lsp-request? request-id-0) #t #f)))

  (define-test lsp-notification-valid
    (assert-true (if (lsp-notification? notif-msg) #t #f)))

  (define-test lsp-request-not-notification
    (assert-false (lsp-request? notif-msg)))

  (define-test lsp-response-with-result
    (assert-true (if (lsp-response? response-result) #t #f)))

  (define-test lsp-response-id-zero
    (assert-true (if (lsp-response? response-id-0) #t #f)))

  (define-test lsp-response-with-error
    (assert-true (if (lsp-response? response-error) #t #f))))

;;; ====
;;; Message Accessor Tests
;;; ====

(test-group message-accessors
  (define-test lsp-message-id
    (assert-equal 1 (lsp-message-id request-msg)))

  (define-test lsp-message-method
    (assert-equal "initialize" (lsp-message-method request-msg)))

  (define-test lsp-message-params-type
    (assert-equal 'json-object (car (lsp-message-params request-msg)))))

;;; ====
;;; Constructor Tests
;;; ====

(test-group constructors
  (define-test make-position-line
    (let ([pos (make-position 5 10)])
      (assert-equal 5 (json-get pos "line"))))

  (define-test make-position-character
    (let ([pos (make-position 5 10)])
      (assert-equal 10 (json-get pos "character"))))

  (define-test make-range-start
    (let* ([start (make-position 0 0)]
           [end (make-position 0 10)]
           [range (make-range start end)])
      (assert-equal start (json-get range "start"))))

  (define-test make-range-end
    (let* ([start (make-position 0 0)]
           [end (make-position 0 10)]
           [range (make-range start end)])
      (assert-equal end (json-get range "end"))))

  (define-test make-location-uri
    (let ([loc (make-location "file:///test.ss"
                              (make-range (make-position 0 0)
                                          (make-position 0 10)))])
      (assert-equal "file:///test.ss" (json-get loc "uri"))))

  (define-test make-diagnostic-severity
    (let ([diag (make-diagnostic (make-range (make-position 0 0)
                                             (make-position 0 5))
                                 "Test error"
                                 *severity-error*)])
      (assert-equal *severity-error* (json-get diag "severity"))))

  (define-test make-diagnostic-message
    (let ([diag (make-diagnostic (make-range (make-position 0 0)
                                             (make-position 0 5))
                                 "Test error"
                                 *severity-error*)])
      (assert-equal "Test error" (json-get diag "message"))))

  (define-test make-hover-has-contents
    (let ([hover (make-hover "**bold** text")])
      (assert-true (if (json-get hover "contents") #t #f))))

  (define-test make-completion-item-label
    (let ([item (make-completion-item "map" *completion-function* "(α→β)→List α→List β")])
      (assert-equal "map" (json-get item "label"))))

  (define-test make-completion-item-kind
    (let ([item (make-completion-item "map" *completion-function* "(α→β)→List α→List β")])
      (assert-equal *completion-function* (json-get item "kind"))))

  (define-test make-signature-help-activeSignature
    (let* ([param (make-parameter-info "f" "Function parameter")]
           [sig (make-signature-info "(map f lst)" "Apply f to each element" (list param))]
           [help (make-signature-help (list sig) 0 0)])
      (assert-equal 0 (json-get help "activeSignature"))))

  (define-test make-signature-help-activeParameter
    (let* ([param (make-parameter-info "f" "Function parameter")]
           [sig (make-signature-info "(map f lst)" "Apply f to each element" (list param))]
           [help (make-signature-help (list sig) 0 0)])
      (assert-equal 0 (json-get help "activeParameter")))))

;;; ====
;;; URI Utilities Tests
;;; ====

(test-group uri-utilities
  (define-test path->uri-unix
    (assert-equal "file:///home/test.ss" (path->uri "/home/test.ss")))

  (define-test uri->path-basic
    (assert-equal "/home/test.ss" (uri->path "file:///home/test.ss")))

  (define-test uri->path-passthrough
    (assert-equal "/already/path" (uri->path "/already/path")))

  (define-test uri->path-with-space
    (assert-equal "/home/my project/test.ss"
                  (uri->path "file:///home/my%20project/test.ss")))

  (define-test uri->path-with-hash
    (assert-equal "/path/file#1.ss"
                  (uri->path "file:///path/file%231.ss")))

  (define-test uri->path-with-percent
    (assert-equal "/path/100%done.ss"
                  (uri->path "file:///path/100%25done.ss")))

  (define-test url-decode-plain
    (assert-equal "hello" (url-decode "hello")))

  (define-test url-decode-space
    (assert-equal "hello world" (url-decode "hello%20world")))

  (define-test url-decode-multiple
    (assert-equal "a b c" (url-decode "a%20b%20c"))))

;;; ====
;;; Response Construction Tests
;;; ====

(test-group response-construction
  (define-test make-response-id
    (let ([resp (make-response 1 '(json-object ("result" . "ok")))])
      (assert-equal 1 (json-get resp "id"))))

  (define-test make-response-jsonrpc
    (let ([resp (make-response 1 '(json-object ("result" . "ok")))])
      (assert-equal "2.0" (json-get resp "jsonrpc"))))

  (define-test make-error-response-has-error
    (let ([err (make-error-response 1 -32600 "Invalid request")])
      (assert-true (if (json-get err "error") #t #f)))))

;;; ====
;;; Run Tests
;;; ====

(print-summary)
(when (> *tests-failed* 0)
  (exit 1))
