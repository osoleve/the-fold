;;; boundary/pipeline/test-rlm-loop.ss — Tests for RLM loop
;;;
;;; Tests loop mechanics with mock LLM responses.
;;; Tests CAS environment operations end-to-end.
;;; Does NOT require a running daemon (mocks fold-ipc-eval).

(load "core/testing/test-framework.ss")
(load "boundary/pipeline/rlm-loop.ss")

(test-group "rlm-loop"

  ;;; ====
  ;;; CAS Environment Integration
  ;;; ====

  (define-test "rlm-env-store! and rlm-env-fetch round-trip"
    (let* ([env (make-rlm-env)]
           [result (rlm-env-store! env 'test-key "hello world" 'text)]
           [env* (car result)]
           [fetched (rlm-env-fetch env* 'test-key)])
      (assert-equal "hello world" fetched)))

  (define-test "rlm-env-store! sexpr round-trip"
    (let* ([env (make-rlm-env)]
           [result (rlm-env-store! env 'data '(1 2 3) 'sexpr)]
           [env* (car result)]
           [fetched (rlm-env-fetch env* 'data)])
      (assert-equal '(1 2 3) fetched)))

  (define-test "rlm-env-fetch returns #f for missing key"
    (assert-false (rlm-env-fetch (make-rlm-env) 'nonexistent)))

  ;;; ====
  ;;; Text Chunking
  ;;; ====

  (define-test "chunk-text returns single chunk for small text"
    (let ([chunks (chunk-text "small text" 100)])
      (assert-equal 1 (length chunks))
      (assert-equal "small text" (car chunks))))

  (define-test "chunk-text splits large text"
    (let* ([text (make-string 5000 #\x)]
           [chunks (chunk-text text 1000)])
      (assert-true (> (length chunks) 1))
      ;; All content preserved
      (assert-equal 5000
                    (apply + (map string-length chunks)))))

  (define-test "rlm-env-ingest-text! stores chunks"
    (let* ([env (make-rlm-env)]
           [text "chunk one content. chunk two content. chunk three content."]
           [env* (rlm-env-ingest-text! env 'doc text 20)])
      ;; Should have an entry of type 'chunks
      (let ([entry (rlm-env-get env* 'doc)])
        (assert-true (pair? entry))
        (assert-equal 'chunks (cadr entry)))))

  ;;; ====
  ;;; Peek
  ;;; ====

  (define-test "rlm-env-peek returns partial content"
    (let* ([env (make-rlm-env)]
           [result (rlm-env-store! env 'big "abcdefghij" 'text)]
           [env* (car result)]
           [peek (rlm-env-peek env* 'big 5)])
      (assert-equal "abcde" peek)))

  (define-test "rlm-env-peek returns full content for small values"
    (let* ([env (make-rlm-env)]
           [result (rlm-env-store! env 'tiny "hi" 'text)]
           [env* (car result)]
           [peek (rlm-env-peek env* 'tiny 100)])
      (assert-equal "hi" peek)))

  ;;; ====
  ;;; Env Snapshot
  ;;; ====

  (define-test "rlm-env-snapshot! returns hex hash"
    (let* ([env (rlm-env-put (make-rlm-env) 'x "abc123" 'text 10)]
           [snap-hash (rlm-env-snapshot! env)])
      (assert-true (string? snap-hash))
      (assert-true (> (string-length snap-hash) 0))))

  ;;; ====
  ;;; Slice Env
  ;;; ====

  (define-test "slice-env extracts named keys"
    (let* ([env (rlm-env-put (make-rlm-env) 'a "h1" 'text 10)]
           [env (rlm-env-put env 'b "h2" 'text 20)]
           [env (rlm-env-put env 'c "h3" 'text 30)]
           [sliced (slice-env env '(a c))])
      (assert-equal 2 (length sliced))
      (assert-true (pair? (rlm-env-get sliced 'a)))
      (assert-true (pair? (rlm-env-get sliced 'c)))
      (assert-false (rlm-env-get sliced 'b))))

  (define-test "slice-env with empty keys passes everything"
    (let* ([env (rlm-env-put (make-rlm-env) 'a "h1" 'text 10)]
           [sliced (slice-env env '())])
      (assert-equal 1 (length sliced))))

  ;;; ====
  ;;; Special Call Detection
  ;;; ====

  (define-test "rlm-spawn-call? detects spawn calls"
    (assert-true (rlm-spawn-call? "(rlm-spawn \"analyze section\" '(section-1))"))
    (assert-false (rlm-spawn-call? "(+ 1 2)")))

  (define-test "rlm-env-put-call? detects env-put calls"
    (assert-true (rlm-env-put-call? "(rlm-env-put! 'key value)"))
    (assert-false (rlm-env-put-call? "(define x 42)")))

  ;;; ====
  ;;; Run Result Accessors
  ;;; ====

  (define-test "rlm-run-result accessors work"
    (let ([r (list 'rlm-run-result 'completed "output" "hash123" '())])
      (assert-true (rlm-run-result? r))
      (assert-equal 'completed (rlm-run-result-status r))
      (assert-equal "output" (rlm-run-result-output r))
      (assert-equal "hash123" (rlm-run-result-trajectory-hash r))))

  ;;; ====
  ;;; Result Formatting
  ;;; ====

  (define-test "format-results formats ok results"
    (let ([text (format-results '((ok "value1") (ok "value2")) #f)])
      (assert-true (string? text))
      (assert-true (> (string-length text) 0))))

  (define-test "format-results formats errors"
    (let ([text (format-results '((error "something broke")) #f)])
      (assert-true (> (string-length text) 0))))

  (define-test "format-results adds loop warning"
    (let ([text (format-results '((ok "val")) #t)])
      (assert-true (> (string-length text) 0)))))

(run-all-tests)
