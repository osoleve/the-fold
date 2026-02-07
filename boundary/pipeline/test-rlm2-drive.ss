;;; boundary/pipeline/test-rlm2-drive.ss — Tests for RLM v2 driver
;;;
;;; Tests the parts of the driver that can be exercised without a live
;;; daemon: env-ref expansion, code splitting, grep formatting,
;;; state update logic, and result accessors.
;;;
;;; Full integration tests (actual IPC + LLM calls) belong in the
;;; benchmark suite, not here.

(load "core/testing/test-framework.ss")
(load "boundary/pipeline/rlm2-drive.ss")

;;; ====
;;; Env Reference Expansion
;;; ====

(test-group "rlm2-expand-env-refs"

  (define-test "expands retrieve with literal key"
    (let* ([env (car (rlm-env-store! (make-rlm-env) 'x 42 'sexpr))]
           [expr '(+ 1 (retrieve 'x))]
           [expanded (rlm2-expand-env-refs expr env)])
      ;; x=42, so (retrieve 'x) should become 42
      (assert-equal '(+ 1 42) expanded)))

  (define-test "leaves retrieve with runtime key unchanged"
    (let* ([env (car (rlm-env-store! (make-rlm-env) 'x 42 'sexpr))]
           [expr '(retrieve some-var)]
           [expanded (rlm2-expand-env-refs expr env)])
      ;; some-var is not a literal — leave unchanged
      (assert-equal '(retrieve some-var) expanded)))

  (define-test "expands nested retrieve"
    (let* ([env (car (rlm-env-store! (make-rlm-env) 'data '(1 2 3) 'sexpr))]
           [expr '(map + (retrieve 'data))]
           [expanded (rlm2-expand-env-refs expr env)])
      (assert-equal '(map + '(1 2 3)) expanded)))

  (define-test "expands rlm-env-get for compatibility"
    (let* ([env (car (rlm-env-store! (make-rlm-env) 'y "hello" 'sexpr))]
           [expr '(string-length (rlm-env-get 'y))]
           [expanded (rlm2-expand-env-refs expr env)])
      (assert-equal '(string-length "hello") expanded)))

  (define-test "does not expand missing keys"
    (let* ([env (make-rlm-env)]
           [expr '(retrieve 'missing)]
           [expanded (rlm2-expand-env-refs expr env)])
      (assert-equal '(retrieve 'missing) expanded)))

  (define-test "does not expand large values"
    ;; Create a value > 2000 chars
    (let* ([big-string (make-string 3000 #\x)]
           [env (car (rlm-env-store! (make-rlm-env) 'big big-string 'text))]
           [expr '(string-length (retrieve 'big))]
           [expanded (rlm2-expand-env-refs expr env)])
      ;; Should leave the retrieve in place (too large for inline expansion)
      (assert-true (pair? expanded))
      ;; The retrieve call should still be there
      (assert-true (pair? (cadr expanded)))
      (assert-equal 'retrieve (caadr expanded))))
)

;;; ====
;;; Code Splitting
;;; ====

(test-group "rlm2-split-code-exprs"

  (define-test "splits multiple expressions"
    (let ([exprs (rlm2-split-code-exprs "(+ 1 2) (* 3 4)")])
      (assert-equal 2 (length exprs))))

  (define-test "handles single expression"
    (let ([exprs (rlm2-split-code-exprs "(define x 42)")])
      (assert-equal 1 (length exprs))))

  (define-test "handles empty string"
    (let ([exprs (rlm2-split-code-exprs "")])
      (assert-equal 0 (length exprs))))

  (define-test "skips comments"
    (let ([exprs (rlm2-split-code-exprs ";; comment\n(+ 1 2)\n;; another\n(* 3 4)")])
      (assert-equal 2 (length exprs))))

  (define-test "handles begin block"
    (let ([exprs (rlm2-split-code-exprs "(begin (define x 1) (+ x 2))")])
      ;; Single begin expression
      (assert-equal 1 (length exprs))))
)

;;; ====
;;; Grep Formatting
;;; ====

(test-group "rlm2-format-grep"

  (define-test "formats empty results"
    (assert-equal "No matches found."
                  (rlm2-format-grep-results '())))

  (define-test "formats multiple results"
    (let ([result (rlm2-format-grep-results
                   '(("line one" . 0.9) ("line two" . 0.7)))])
      (assert-true (string? result))
      (assert-true (> (string-length result) 0))))
)

;;; ====
;;; State Update Logic
;;; ====

(test-group "rlm2-update-state"

  (define-test "advances step and uses fuel"
    (let* ([s (make-initial-rlm2-state "t" #f 1000)]
           [obs (make-rlm2-observation 'search "q" "results" #t)]
           [s* (rlm2-update-state s '(search "q") obs "Found tools" 5)])
      (assert-equal 1 (rlm2-state-step s*))
      (assert-equal 995 (rlm2-state-fuel s*))))

  (define-test "adds note"
    (let* ([s (make-initial-rlm2-state "t" #f 1000)]
           [obs (make-rlm2-observation 'search "q" "results" #t)]
           [s* (rlm2-update-state s '(search "q") obs "Found tools" 5)])
      (assert-equal '("Found tools") (rlm2-state-notes s*))))

  (define-test "sets last-result to observation"
    (let* ([s (make-initial-rlm2-state "t" #f 1000)]
           [obs (make-rlm2-observation 'search "q" "results" #t)]
           [s* (rlm2-update-state s '(search "q") obs "Found tools" 5)])
      ;; For non-submit actions, last-result is the observation
      (assert-true (rlm2-observation? (rlm2-state-last-result s*)))))

  (define-test "submit preserves rlm2-result from exec"
    ;; Simulate what exec-submit does: set rlm2-result on state
    (let* ([s (make-initial-rlm2-state "t" #f 1000)]
           [s-with-result (rlm2-state-with-last-result s (make-rlm2-result 3))]
           [obs (make-rlm2-observation 'submit '(+ 1 2)
                   "Answer accepted: 3" #t)]
           [s* (rlm2-update-state s-with-result '(submit (+ 1 2)) obs
                                  "Final answer: 3" 1)])
      ;; update-state should preserve the rlm2-result, not overwrite with observation
      (assert-true (rlm2-state-complete? s*))
      (assert-equal 3 (rlm2-state-result s*))))

  (define-test "non-submit overwrites last-result with observation"
    (let* ([s (make-initial-rlm2-state "t" #f 1000)]
           [obs (make-rlm2-observation 'search "q" "results" #t)]
           [s* (rlm2-update-state s '(search "q") obs "Found tools" 5)])
      ;; Non-submit: last-result should be the observation
      (assert-true (rlm2-observation? (rlm2-state-last-result s*)))))
)

;;; ====
;;; Run Result Accessors
;;; ====

(test-group "rlm2-run-result"

  (define-test "run-result accessors work"
    (let ([r (list 'rlm2-run-result 'completed "42" "traj-hash" '())])
      (assert-true (rlm2-run-result? r))
      (assert-equal 'completed (rlm2-run-result-status r))
      (assert-equal "42" (rlm2-run-result-output r))
      (assert-equal "traj-hash" (rlm2-run-result-trajectory-hash r))
      (assert-equal '() (rlm2-run-result-env r))))
)

;;; ====
;;; Utility Functions
;;; ====

(test-group "rlm2-utilities"

  (define-test "first-line extracts first line"
    (assert-equal "hello" (rlm2-first-line "hello\nworld")))

  (define-test "first-line handles single line"
    (assert-equal "hello" (rlm2-first-line "hello")))

  (define-test "first-line handles empty"
    (assert-equal "" (rlm2-first-line "")))

  (define-test "mechanical-note produces readable note"
    (let* ([action '(search "eigenvalue")]
           [obs (make-rlm2-observation 'search "eigenvalue" "results" #t)]
           [note (rlm2-mechanical-note action obs 3)])
      (assert-true (string? note))
      (assert-true (> (string-length note) 0))))

  (define-test "literal-arg? recognizes literals"
    (assert-true (rlm2-literal-arg? 42))
    (assert-true (rlm2-literal-arg? "hello"))
    (assert-true (rlm2-literal-arg? #t))
    (assert-true (rlm2-literal-arg? '()))
    (assert-true (rlm2-literal-arg? '(quote foo)))
    (assert-false (rlm2-literal-arg? 'variable))
    (assert-false (rlm2-literal-arg? '(+ 1 2))))

  (define-test "unquote-key extracts symbol"
    (assert-equal 'foo (rlm2-unquote-key '(quote foo)))
    (assert-equal 'bar (rlm2-unquote-key 'bar)))

  (define-test "quote-if-needed wraps non-self-evaluating"
    (assert-equal 42 (rlm2-quote-if-needed 42))
    (assert-equal "hello" (rlm2-quote-if-needed "hello"))
    (assert-equal '(quote (1 2 3)) (rlm2-quote-if-needed '(1 2 3))))
)

(run-all-tests)
