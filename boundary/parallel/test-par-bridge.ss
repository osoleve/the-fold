;;; boundary/parallel/test-par-bridge.ss — Tests for par/pool bridge
;;;
;;; Verifies that pool-backed par produces identical results to raw-thread par.

(load "core/testing/test-framework.ss")
(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/lang/prim.ss")
(load "core/lang/eval.ss")
(load "boundary/parallel/par-bridge.ss")

;;; ============================================================================
;;; Bridge lifecycle
;;; ============================================================================

(test-group bridge-lifecycle
  (define-test "par-backend starts as #f"
    (uninstall-par-bridge!)   ; ensure clean slate
    (assert-equal #f (unbox *par-backend*)))

  (define-test "install sets backend"
    (install-par-bridge!)
    (assert-true (procedure? (unbox *par-backend*))))

  (define-test "uninstall clears backend"
    (install-par-bridge!)
    (uninstall-par-bridge!)
    (assert-equal #f (unbox *par-backend*)))

  (define-test "double install is idempotent"
    (install-par-bridge!)
    (install-par-bridge!)
    (assert-true (procedure? (unbox *par-backend*)))
    (uninstall-par-bridge!)))

;;; ============================================================================
;;; Fuel semantics (must match existing test expectations exactly)
;;; ============================================================================

(install-par-bridge!)

(test-group bridge-basic-par
  (define-test "par returns second value"
    (let ([result (eval-expr '(par 1 2) empty-env 100)])
      (assert-equal '(ok 2 98) result)))

  (define-test "par evaluates first arg"
    (let ([result (eval-expr '(par (prim 'add 1 2) 5) empty-env 100)])
      (assert-equal '(ok 5 98) result)))

  (define-test "par with both complex exprs"
    (let ([result (eval-expr '(par (prim 'mul 2 3) (prim 'add 4 5)) empty-env 100)])
      (assert-equal '(ok 9 96) result)))

  (define-test "par with error value in first"
    (let ([result (eval-expr '(par (prim 'div 1 0) 42) empty-env 100)])
      (assert-equal '(ok 42 98) result)))

  (define-test "par with error value in second"
    (let ([result (eval-expr '(par 1 (prim 'div 1 0)) empty-env 100)])
      (assert-equal '(ok (error div-by-zero) 96) result))))

(test-group bridge-fuel
  (define-test "par fuel consumption"
    (let ([result (eval-expr '(par 1 2) empty-env 10)])
      (assert-equal '(ok 2 8) result)))

  (define-test "par errors on low fuel (background suspension)"
    (let ([result (eval-expr '(par (prim 'add 1 2) 5) empty-env 1)])
      (assert-equal '(error par-suspended "Background expression suspended (out of fuel)" (prim 'add 1 2))
                    result)))

  (define-test "par suspends on low fuel (second)"
    (let ([result (eval-expr '(par 1 (prim 'add 2 3)) empty-env 2)])
      (assert-equal '(suspended (prim 'add 2 3) ()) result))))

;;; ============================================================================
;;; Nested par
;;; ============================================================================

(test-group bridge-nested
  (define-test "nested par"
    (let ([result (eval-expr '(par (par 1 2) (par 3 4)) empty-env 100)])
      (assert-equal '(ok 4 97) result)))

  (define-test "par inside pseq"
    (let ([result (eval-expr '(pseq (par 1 2) (par 3 4)) empty-env 100)])
      (assert-equal '(ok 4 95) result)))

  (define-test "pseq inside par"
    (let ([result (eval-expr '(par (pseq 1 2) (pseq 3 4)) empty-env 100)])
      (assert-equal '(ok 4 96) result)))

  (define-test "deeply nested par (4 levels)"
    (let ([nested-par
           '(par (par (par (par 1 2) (par 3 4))
                      (par (par 5 6) (par 7 8)))
                 (par (par (par 9 10) (par 11 12))
                      (par (par 13 14) (par 15 16))))])
      (let ([result (eval-expr nested-par empty-env 10000)])
        (assert-equal 16 (cadr result)))))

  (define-test "linear par chain"
    (let ([chained-pars '(par 1 (par 2 (par 3 (par 4 5))))])
      (let ([result (eval-expr chained-pars empty-env 10000)])
        (assert-equal 5 (cadr result))))))

;;; ============================================================================
;;; Practical use cases
;;; ============================================================================

(test-group bridge-practical
  (define-test "par with duplicate computation"
    (let ([result (eval-expr '(par (prim 'mul 123 456)
                                   (prim 'add (prim 'mul 123 456) 789))
                             empty-env
                             100)])
      (assert-equal '(ok 56877 94) result)))

  (define-test "par with let bindings"
    (let ([result (eval-expr '(let ((x 5))
                                (par (prim 'mul x 2)
                                     (prim 'add x 3)))
                             empty-env
                             100)])
      (assert-equal '(ok 8 94) result)))

  (define-test "par with shared environment reads"
    (let* ([expr '(let ((x 42)
                        (y 17))
                    (par (prim 'add x y)
                         (prim 'mul x y)))]
           [result (eval-expr expr empty-env 1000)])
      (assert-equal 714 (cadr result))))

  (define-test "par with closure applications"
    (let* ([expr '(let ((f (fn (x) (prim 'mul x x))))
                    (par (f 5)
                         (f 7)))]
           [result (eval-expr expr empty-env 1000)])
      (assert-equal 49 (cadr result)))))

;;; ============================================================================
;;; Pool integration
;;; ============================================================================

(test-group bridge-pool-integration
  (define-test "pool is running after bridge use"
    ;; Previous tests should have triggered pool creation
    (let ([stats (pool-stats)])
      (assert-true (cdr (assq 'running stats)))))

  (define-test "pool worker count is positive"
    (let ([stats (pool-stats)])
      (assert-true (> (cdr (assq 'worker-count stats)) 0)))))

;;; ============================================================================
;;; Traced context fallback
;;; ============================================================================

(test-group bridge-traced-fallback
  (define-test "traced context bypasses bridge (sequential fallback)"
    ;; When ctx is a tape, par must fall back to sequential, not use pool.
    ;; We test this indirectly: with tracing, eval-par* takes the else branch.
    ;; eval-expr-traced uses a tape as ctx.
    (let ([result (eval-expr-traced '(par 1 2) empty-env 100 '())])
      ;; With tracing, par falls back to sequential: a then b
      ;; Sequential: eval a (fuel-1 for par = 99, eval 1 costs 1 = 98),
      ;;            then eval b (fuel=98, eval 2 costs 1 = 97)
      (assert-equal 2 (cadr result)))))

;;; ============================================================================
;;; Cleanup and run
;;; ============================================================================

(uninstall-par-bridge!)

(run-all-tests-and-exit)
