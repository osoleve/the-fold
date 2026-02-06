;;; lattice/pipeline/test-rlm.ss — Tests for pure RLM types
;;;
;;; Tests environment operations, config constructors, response parsing,
;;; and loop detection — all pure, no IO.

(load "core/testing/test-framework.ss")
(load "lattice/pipeline/rlm.ss")

(test-group "rlm-pure-types"

  ;;; ====
  ;;; Environment
  ;;; ====

  (define-test "make-rlm-env creates empty env"
    (assert-equal '() (make-rlm-env)))

  (define-test "rlm-env-put adds entry"
    (let* ([env (make-rlm-env)]
           [env* (rlm-env-put env 'foo "abc123" 'text 42)])
      (assert-true (pair? env*))
      (assert-equal 1 (length env*))))

  (define-test "rlm-env-get retrieves entry"
    (let* ([env (rlm-env-put (make-rlm-env) 'foo "hash1" 'text 100)]
           [entry (rlm-env-get env 'foo)])
      (assert-true (pair? entry))
      (assert-equal 'foo (car entry))
      (assert-equal 'text (cadr entry))
      (assert-equal 100 (caddr entry))
      (assert-equal "hash1" (cadddr entry))))

  (define-test "rlm-env-get returns #f for missing key"
    (assert-false (rlm-env-get (make-rlm-env) 'missing)))

  (define-test "rlm-env-put replaces existing key"
    (let* ([env (rlm-env-put (make-rlm-env) 'x "old" 'text 10)]
           [env* (rlm-env-put env 'x "new" 'sexpr 20)]
           [entry (rlm-env-get env* 'x)])
      (assert-equal 1 (length env*))
      (assert-equal "new" (cadddr entry))
      (assert-equal 'sexpr (cadr entry))))

  (define-test "rlm-env-keys lists all entries"
    (let* ([env (rlm-env-put (make-rlm-env) 'a "h1" 'text 10)]
           [env (rlm-env-put env 'b "h2" 'sexpr 20)]
           [keys (rlm-env-keys env)])
      (assert-equal 2 (length keys))))

  (define-test "rlm-env-summary formats correctly"
    (let* ([env (rlm-env-put (make-rlm-env) 'data "h1" 'text 1500)]
           [summary (rlm-env-summary env)])
      (assert-true (string? summary))
      (assert-true (> (string-length summary) 0))))

  (define-test "rlm-env-summary on empty env"
    (assert-equal "[Environment is empty]"
                  (rlm-env-summary (make-rlm-env))))

  ;;; ====
  ;;; Config
  ;;; ====

  (define-test "make-rlm-config creates valid config"
    (let ([cfg (make-rlm-config 'dummy-provider "system" 10 5000 2000 2 3)])
      (assert-true (rlm-config? cfg))
      (assert-equal 'dummy-provider (rlm-config-provider cfg))
      (assert-equal "system" (rlm-config-system-prompt cfg))
      (assert-equal 10 (rlm-config-max-steps cfg))
      (assert-equal 5000 (rlm-config-max-fuel cfg))
      (assert-equal 2000 (rlm-config-chunk-size cfg))
      (assert-equal 2 (rlm-config-max-depth cfg))
      (assert-equal 3 (rlm-config-loop-window cfg))))

  (define-test "make-rlm-config-default has sensible values"
    (let ([cfg (make-rlm-config-default 'p "sys")])
      (assert-equal 20 (rlm-config-max-steps cfg))
      (assert-equal 50000 (rlm-config-max-fuel cfg))
      (assert-equal 2 (rlm-config-max-depth cfg))))

  ;;; ====
  ;;; Step Record
  ;;; ====

  (define-test "make-rlm-step-record creates valid record"
    (let ([r (make-rlm-step-record 0 "prompt" "response" "(+ 1 2)" 3 '() 10)])
      (assert-true (rlm-step? r))
      (assert-equal 0 (rlm-step-num r))
      (assert-equal "prompt" (rlm-step-prompt r))
      (assert-equal 10 (rlm-step-fuel-used r))))

  ;;; ====
  ;;; Chunk Manifest
  ;;; ====

  (define-test "make-chunk-manifest creates valid manifest"
    (let ([m (make-chunk-manifest 'data 5000 3 '("h1" "h2" "h3"))])
      (assert-true (chunk-manifest? m))
      (assert-equal 'data (chunk-manifest-key m))
      (assert-equal 5000 (chunk-manifest-total-chars m))
      (assert-equal 3 (chunk-manifest-count m))
      (assert-equal '("h1" "h2" "h3") (chunk-manifest-hashes m))))

  ;;; ====
  ;;; Response Parsing
  ;;; ====

  (define-test "rlm-parse-response extracts scheme code block"
    (let* ([text "Here's some code:\n```scheme\n(+ 1 2)\n```\nDone."]
           [blocks (rlm-parse-response text)])
      (assert-true (pair? blocks))
      ;; Should have thought, code, thought
      (let ([code-blocks (filter rlm-block-code? blocks)])
        (assert-equal 1 (length code-blocks))
        (assert-equal "(+ 1 2)" (rlm-block-content (car code-blocks))))))

  (define-test "rlm-parse-response extracts fold code block"
    (let* ([text "```fold\n(define x 42)\n```"]
           [blocks (rlm-parse-response text)])
      (let ([code-blocks (filter rlm-block-code? blocks)])
        (assert-equal 1 (length code-blocks)))))

  (define-test "rlm-parse-response extracts template block"
    (let* ([text "```fold-template\ndefine $name $body --- $name := foo --- $body := 42\n```"]
           [blocks (rlm-parse-response text)])
      (let ([tmpl-blocks (filter rlm-block-template? blocks)])
        (assert-equal 1 (length tmpl-blocks)))))

  (define-test "rlm-parse-response detects DONE marker"
    (let* ([text "I'm finished.\nDONE(42)"]
           [blocks (rlm-parse-response text)])
      (let ([done-blocks (filter rlm-block-done? blocks)])
        (assert-equal 1 (length done-blocks))
        (assert-equal "42" (rlm-block-content (car done-blocks))))))

  (define-test "rlm-parse-response detects FINAL marker"
    (let* ([text "FINAL(the answer is 7)"]
           [blocks (rlm-parse-response text)])
      (let ([done-blocks (filter rlm-block-done? blocks)])
        (assert-equal 1 (length done-blocks))
        (assert-equal "the answer is 7" (rlm-block-content (car done-blocks))))))

  (define-test "rlm-parse-response handles multiple code blocks"
    (let* ([text "First:\n```scheme\n(+ 1 2)\n```\nSecond:\n```scheme\n(* 3 4)\n```"]
           [blocks (rlm-parse-response text)])
      (let ([code-blocks (filter rlm-block-code? blocks)])
        (assert-equal 2 (length code-blocks)))))

  (define-test "rlm-parse-response coalesces adjacent thoughts"
    (let* ([text "Line one\nLine two\nLine three"]
           [blocks (rlm-parse-response text)])
      ;; Should be a single thought block, not three
      (let ([thoughts (filter rlm-block-thought? blocks)])
        (assert-equal 1 (length thoughts)))))

  (define-test "rlm-parse-response handles empty input"
    (let ([blocks (rlm-parse-response "")])
      (assert-true (pair? blocks))))

  ;;; ====
  ;;; Loop Detection
  ;;; ====

  (define-test "rlm-loop-detected? returns #f with no history"
    (assert-false (rlm-loop-detected? '() "fp1" 3)))

  (define-test "rlm-loop-detected? detects repeated fingerprint"
    (assert-true (pair? (rlm-loop-detected? '("fp1" "fp2" "fp3") "fp2" 3))))

  (define-test "rlm-loop-detected? respects window size"
    ;; fp4 is outside the window of 2
    (assert-false (rlm-loop-detected? '("fp1" "fp2" "fp3" "fp4") "fp4" 2)))

  (define-test "take-up-to handles short lists"
    (assert-equal '(a b) (take-up-to 5 '(a b))))

  (define-test "take-up-to limits to n"
    (assert-equal '(a b) (take-up-to 2 '(a b c d))))

  ;;; ====
  ;;; String Utilities
  ;;; ====

  (define-test "string-split-lines splits on newlines"
    (assert-equal '("a" "b" "c") (string-split-lines "a\nb\nc")))

  (define-test "string-split-lines handles empty string"
    (assert-equal '("") (string-split-lines "")))

  (define-test "string-trim removes whitespace"
    (assert-equal "hello" (string-trim "  hello  ")))

  (define-test "string-index finds character"
    (assert-equal 3 (string-index "abc(def" #\()))

  (define-test "string-index returns #f when not found"
    (assert-false (string-index "abc" #\())))

(run-all-tests)
