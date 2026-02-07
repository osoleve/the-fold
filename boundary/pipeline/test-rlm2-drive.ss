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

  (define-test "expands large values for eval"
    ;; Values of any size should be expanded (cap removed — eval path, not HUD)
    (let* ([big-string (make-string 3000 #\x)]
           [env (car (rlm-env-store! (make-rlm-env) 'big big-string 'text))]
           [expr '(string-length (retrieve 'big))]
           [expanded (rlm2-expand-env-refs expr env)])
      ;; The retrieve should be replaced with the actual value
      (assert-true (pair? expanded))
      (assert-equal 'string-length (car expanded))
      (assert-true (string? (cadr expanded)))
      (assert-equal 3000 (string-length (cadr expanded)))))
)

;;; ====
;;; Escaping Normalization
;;; ====

(test-group "rlm2-normalize-expr-escaping"

  (define-test "collapses backslash-quote to bare quote"
    (assert-equal "foo \"bar\" baz"
                  (rlm2-normalize-expr-escaping "foo \\\"bar\\\" baz")))

  (define-test "stable on already-clean strings"
    (assert-equal "(+ 1 2)"
                  (rlm2-normalize-expr-escaping "(+ 1 2)")))

  (define-test "handles multiple layers of escaping"
    ;; \\\\\" -> \\\\" -> \\" -> \" -> "
    (assert-equal "x\"y"
                  (rlm2-normalize-expr-escaping "x\\\\\\\\\\\"y")))

  (define-test "preserves non-quote backslashes"
    (assert-equal "foo\\nbar"
                  (rlm2-normalize-expr-escaping "foo\\nbar")))

  (define-test "handles empty string"
    (assert-equal "" (rlm2-normalize-expr-escaping "")))
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

;;; ====
;;; String helper (same as test-rlm2.ss)
;;; ====

(define (rlm2-hud-contains? text needle)
  (let ([tlen (string-length text)]
        [nlen (string-length needle)])
    (if (> nlen tlen)
        -1
        (let loop ([i 0])
          (cond
            [(> (+ i nlen) tlen) -1]
            [(string=? (substring text i (+ i nlen)) needle) i]
            [else (loop (+ i 1))])))))

;;; ====
;;; Journal & Memory Actions
;;; ====

(test-group "rlm2-exec-journal"

  (define-test "exec-journal adds entry to state"
    (let* ([s (make-initial-rlm2-state "t" #f 1000)]
           [action '(journal hypothesis "Input is 50x50 matrix")]
           [result (rlm2-exec-journal s action)]
           [obs (car result)]
           [state* (cadr result)])
      (assert-true (rlm2-observation-ok? obs))
      (assert-equal '((hypothesis . "Input is 50x50 matrix"))
                    (rlm2-state-journal state*))))

  (define-test "exec-journal rejects non-string text"
    (let* ([s (make-initial-rlm2-state "t" #f 1000)]
           [action '(journal tag 42)]
           [result (rlm2-exec-journal s action)]
           [obs (car result)])
      (assert-false (rlm2-observation-ok? obs))))
)

(test-group "rlm2-exec-recall"

  (define-test "exec-recall filters by tag"
    (let* ([s (rlm2-state-add-journal
               (rlm2-state-add-journal
                (rlm2-state-add-journal
                 (make-initial-rlm2-state "t" #f 1000)
                 'hypothesis "matrix")
                'observation "has eigenvalues")
               'hypothesis "symmetric")]
           [action '(recall hypothesis)]
           [result (rlm2-exec-recall s action)]
           [obs (car result)])
      (assert-true (rlm2-observation-ok? obs))
      ;; Should find 2 hypothesis entries
      (assert-true (>= (rlm2-hud-contains?
                         (rlm2-observation-value obs) "2 entries") 0))))

  (define-test "exec-recall returns no entries for missing tag"
    (let* ([s (make-initial-rlm2-state "t" #f 1000)]
           [action '(recall nonexistent)]
           [result (rlm2-exec-recall s action)]
           [obs (car result)])
      (assert-true (rlm2-observation-ok? obs))
      (assert-true (>= (rlm2-hud-contains?
                         (rlm2-observation-value obs) "No journal entries") 0))))
)

(test-group "rlm2-exec-memorize"

  (define-test "exec-memorize writes to .rlm/ dir"
    (let ([test-dir (string-append (current-directory) "/.rlm-test-" (format "~a" (random 100000)))])
      (dynamic-wind
        (lambda ()
          ;; Redirect system memory to test dir
          (set! rlm2-system-memory-path
            (lambda () (string-append test-dir "/memories.sexp")))
          (set! rlm2-ensure-rlm-dir!
            (lambda () (unless (file-exists? test-dir) (mkdir test-dir)))))
        (lambda ()
          (rlm2-invalidate-system-memory-cache!)
          (let* ([s (make-initial-rlm2-state "t" #f 1000)]
                 [action '(memorize eigen-strategy "Use QR decomposition")]
                 [result (rlm2-exec-memorize s action)]
                 [obs (car result)])
            (assert-true (rlm2-observation-ok? obs))
            ;; Verify file was created
            (assert-true (file-exists? (string-append test-dir "/memories.sexp")))))
        (lambda ()
          ;; Cleanup
          (when (file-exists? (string-append test-dir "/memories.sexp"))
            (delete-file (string-append test-dir "/memories.sexp")))
          (when (file-exists? test-dir)
            (delete-directory test-dir))
          (rlm2-invalidate-system-memory-cache!)
          ;; Restore original
          (set! rlm2-system-memory-path
            (lambda () (string-append (current-directory) "/.rlm/memories.sexp")))
          (set! rlm2-ensure-rlm-dir!
            (lambda ()
              (let ([dir (string-append (current-directory) "/.rlm")])
                (unless (file-exists? dir) (mkdir dir)))))))))
)

(test-group "rlm2-exec-remember"

  (define-test "exec-remember searches over written memories"
    (let ([test-dir (string-append (current-directory) "/.rlm-test-" (format "~a" (random 100000)))])
      (dynamic-wind
        (lambda ()
          (set! rlm2-system-memory-path
            (lambda () (string-append test-dir "/memories.sexp")))
          (set! rlm2-ensure-rlm-dir!
            (lambda () (unless (file-exists? test-dir) (mkdir test-dir)))))
        (lambda ()
          (rlm2-invalidate-system-memory-cache!)
          ;; Write some memories
          (rlm2-append-system-memory! 'matrix-strategy "Use QR for symmetric matrices")
          (rlm2-append-system-memory! 'graph-strategy "Use BFS for shortest paths")
          ;; Search
          (let* ([s (make-initial-rlm2-state "t" #f 1000)]
                 [action '(remember "matrix decomposition")]
                 [result (rlm2-exec-remember s action)]
                 [obs (car result)])
            (assert-true (rlm2-observation-ok? obs))
            ;; Should find the matrix entry
            (assert-true (string? (rlm2-observation-value obs)))))
        (lambda ()
          (when (file-exists? (string-append test-dir "/memories.sexp"))
            (delete-file (string-append test-dir "/memories.sexp")))
          (when (file-exists? test-dir)
            (delete-directory test-dir))
          (rlm2-invalidate-system-memory-cache!)
          (set! rlm2-system-memory-path
            (lambda () (string-append (current-directory) "/.rlm/memories.sexp")))
          (set! rlm2-ensure-rlm-dir!
            (lambda ()
              (let ([dir (string-append (current-directory) "/.rlm")])
                (unless (file-exists? dir) (mkdir dir)))))))))
)

(run-all-tests)
