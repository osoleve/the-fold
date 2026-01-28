;;; core/lang/test-par-pseq.ss — Tests for par and pseq evaluation forms
;;; Standardized to use test-framework.ss
;;;
;;; Test parallel evaluation hints and sequential forcing.

(load "core/testing/test-framework.ss")
(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/lang/prim.ss")
(load "core/lang/eval.ss")

;;; ============================================================================
;;; Helper for timing tests
;;; ============================================================================

(define (make-slow-expr n)
  "Generate expression that computes fib(n) slowly"
  (if (<= n 1)
      n
      `(prim 'add ,(make-slow-expr (- n 1))
             ,(make-slow-expr (- n 2)))))

(define (current-ms)
  (let ([t (current-time)])
    (+ (* (time-second t) 1000)
       (quotient (time-nanosecond t) 1000000))))

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group basic-par
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

(test-group basic-pseq
  (define-test "pseq returns second value"
    (let ([result (eval-expr '(pseq 1 2) empty-env 100)])
      (assert-equal '(ok 2 97) result)))

  (define-test "pseq evaluates first arg strictly"
    (let ([result (eval-expr '(pseq (prim 'add 1 2) 5) empty-env 100)])
      (assert-equal '(ok 5 95) result)))

  (define-test "pseq with both complex exprs"
    (let ([result (eval-expr '(pseq (prim 'mul 2 3) (prim 'add 4 5)) empty-env 100)])
      (assert-equal '(ok 9 93) result)))

  (define-test "pseq with error value in first"
    (let ([result (eval-expr '(pseq (prim 'div 1 0) 42) empty-env 100)])
      (assert-equal '(ok 42 95) result)))

  (define-test "pseq with error value in second"
    (let ([result (eval-expr '(pseq 1 (prim 'div 1 0)) empty-env 100)])
      (assert-equal '(ok (error div-by-zero) 95) result))))

(test-group fuel-consumption
  (define-test "par fuel consumption"
    (let ([result (eval-expr '(par 1 2) empty-env 10)])
      (assert-equal '(ok 2 8) result)))

  (define-test "pseq fuel consumption"
    (let ([result (eval-expr '(pseq 1 2) empty-env 10)])
      (assert-equal '(ok 2 7) result)))

  (define-test "par errors on low fuel (background)"
    (let ([result (eval-expr '(par (prim 'add 1 2) 5) empty-env 1)])
      (assert-equal '(error par-suspended "Background expression suspended (out of fuel)" (prim 'add 1 2))
                    result)))

  (define-test "par suspends on low fuel (second)"
    (let ([result (eval-expr '(par 1 (prim 'add 2 3)) empty-env 2)])
      (assert-equal '(suspended (prim 'add 2 3) ()) result)))

  (define-test "pseq suspends on low fuel (first)"
    (let ([result (eval-expr '(pseq (prim 'add 1 2) 5) empty-env 1)])
      (assert-equal '(suspended (prim 'add 1 2) ()) result)))

  (define-test "pseq suspends on low fuel (second)"
    (let ([result (eval-expr '(pseq 1 (prim 'add 2 3)) empty-env 2)])
      (assert-equal '(suspended (prim 'add 2 3) ()) result))))

(test-group nested-par-pseq
  (define-test "nested par"
    (let ([result (eval-expr '(par (par 1 2) (par 3 4)) empty-env 100)])
      (assert-equal '(ok 4 97) result)))

  (define-test "nested pseq"
    (let ([result (eval-expr '(pseq (pseq 1 2) (pseq 3 4)) empty-env 100)])
      (assert-equal '(ok 4 93) result)))

  (define-test "par inside pseq"
    (let ([result (eval-expr '(pseq (par 1 2) (par 3 4)) empty-env 100)])
      (assert-equal '(ok 4 95) result)))

  (define-test "pseq inside par"
    (let ([result (eval-expr '(par (pseq 1 2) (pseq 3 4)) empty-env 100)])
      (assert-equal '(ok 4 96) result))))

(test-group practical-use-cases
  (define-test "par with duplicate computation"
    (let ([result (eval-expr '(par (prim 'mul 123 456)
                                   (prim 'add (prim 'mul 123 456) 789))
                             empty-env
                             100)])
      (assert-equal '(ok 56877 94) result)))

  (define-test "pseq ensures ordering"
    (let ([result (eval-expr '(pseq (prim 'add 1 2)
                                    (prim 'mul 3 4))
                             empty-env
                             100)])
      (assert-equal '(ok 12 93) result)))

  (define-test "par with let bindings"
    (let ([result (eval-expr '(let ((x 5))
                                (par (prim 'mul x 2)
                                     (prim 'add x 3)))
                             empty-env
                             100)])
      (assert-equal '(ok 8 94) result)))

  (define-test "pseq with let bindings"
    (let ([result (eval-expr '(let ((x 5))
                                (pseq (prim 'mul x 2)
                                      (prim 'add x 3)))
                             empty-env
                             100)])
      (assert-equal '(ok 8 91) result))))

(test-group concurrency
  (define-test "par completes (timing baseline)"
    (let* ([slow-a (make-slow-expr 15)]
           [slow-b (make-slow-expr 15)]
           [fuel 1000000]
           [par-result (eval-expr `(par ,slow-a ,slow-b) empty-env fuel)])
      (assert-equal 610 (cadr par-result))))

  (define-test "pseq completes (timing baseline)"
    (let* ([slow-a (make-slow-expr 15)]
           [slow-b (make-slow-expr 15)]
           [fuel 1000000]
           [pseq-result (eval-expr `(pseq ,slow-a ,slow-b) empty-env fuel)])
      (assert-equal 610 (cadr pseq-result))))

  (define-test "par evaluates both branches (via fuel consumed)"
    (let* ([expr '(par (prim 'add 100 (prim 'mul 7 7))
                       (prim 'sub 200 (prim 'mul 3 3)))]
           [result (eval-expr expr empty-env 1000)])
      (assert-equal 191 (cadr result))))

  (define-test "deeply nested par (4 levels, 16 values)"
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
        (assert-equal 5 (cadr result)))))

  (define-test "par with shared environment reads"
    (let* ([expr '(let ((x 42)
                        (y 17))
                    (par (prim 'add x y)
                         (prim 'mul x y)))]
           [result (eval-expr expr empty-env 1000)])
      (assert-equal 714 (cadr result))))

  (define-test "par with shadowed bindings"
    (let* ([expr '(let ((x 10))
                    (par (let ((x 20)) (prim 'mul x 2))
                         (let ((x 30)) (prim 'mul x 3))))]
           [result (eval-expr expr empty-env 1000)])
      (assert-equal 90 (cadr result))))

  (define-test "par with multi-step computations"
    (let* ([expr '(par (prim 'add 10 (prim 'mul 3 4))
                       (prim 'sub 100 (prim 'mul 5 6)))]
           [result (eval-expr expr empty-env 1000)])
      (assert-equal 70 (cadr result))))

  (define-test "par returns fast branch while slow runs"
    (let* ([expr `(par ,(make-slow-expr 12) 42)]
           [result (eval-expr expr empty-env 100000)])
      (assert-equal 42 (cadr result))))

  (define-test "par isolates background error"
    (let* ([expr '(par (prim 'div 1 0)
                       (prim 'add 1 2))]
           [result (eval-expr expr empty-env 1000)])
      (assert-equal 3 (cadr result))))

  (define-test "par with closure applications"
    (let* ([expr '(let ((f (fn (x) (prim 'mul x x))))
                    (par (f 5)
                         (f 7)))]
           [result (eval-expr expr empty-env 1000)])
      (assert-equal 49 (cadr result)))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
