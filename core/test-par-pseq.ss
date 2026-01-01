;;; test-par-pseq.ss — Tests for par and pseq evaluation forms
;;;
;;; Test parallel evaluation hints and sequential forcing.

(load "core/prelude.ss")
(load "core/block.ss")
(load "core/prim.ss")
(load "core/eval.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

(printf "\n=== Par/Pseq Evaluation Tests ===\n\n")

;;; ============================================================
;;; Basic Par Tests
;;; ============================================================

(printf "--- Basic Par Tests ---\n")

;; Test 1: par returns second argument
(let ([result (eval-expr '(par 1 2) empty-env 100)])
     (test "par returns second value"
           '(ok 2 97)
           result))

;; Test 2: par evaluates first argument
(let ([result (eval-expr '(par (prim 'add 1 2) 5) empty-env 100)])
     (test "par evaluates first arg"
           '(ok 5 95)
           result))

;; Test 3: par with both complex expressions
(let ([result (eval-expr '(par (prim 'mul 2 3) (prim 'add 4 5)) empty-env 100)])
     (test "par with both complex exprs"
           '(ok 9 93)
           result))

;; Test 4: par with error value from first arg (errors are values)
(let ([result (eval-expr '(par (prim 'div 1 0) 42) empty-env 100)])
     (test "par with error value in first"
           '(ok 42 95)
           result))

;; Test 5: par with error value from second arg
(let ([result (eval-expr '(par 1 (prim 'div 1 0)) empty-env 100)])
     (test "par with error value in second"
           '(ok (error div-by-zero) 95)
           result))

;;; ============================================================
;;; Basic Pseq Tests
;;; ============================================================

(printf "\n--- Basic Pseq Tests ---\n")

;; Test 6: pseq returns second argument
(let ([result (eval-expr '(pseq 1 2) empty-env 100)])
     (test "pseq returns second value"
           '(ok 2 97)
           result))

;; Test 7: pseq evaluates first argument strictly
(let ([result (eval-expr '(pseq (prim 'add 1 2) 5) empty-env 100)])
     (test "pseq evaluates first arg strictly"
           '(ok 5 95)
           result))

;; Test 8: pseq with both complex expressions
(let ([result (eval-expr '(pseq (prim 'mul 2 3) (prim 'add 4 5)) empty-env 100)])
     (test "pseq with both complex exprs"
           '(ok 9 93)
           result))

;; Test 9: pseq with error value from first arg (errors are values)
(let ([result (eval-expr '(pseq (prim 'div 1 0) 42) empty-env 100)])
     (test "pseq with error value in first"
           '(ok 42 95)
           result))

;; Test 10: pseq with error value from second arg
(let ([result (eval-expr '(pseq 1 (prim 'div 1 0)) empty-env 100)])
     (test "pseq with error value in second"
           '(ok (error div-by-zero) 95)
           result))

;;; ============================================================
;;; Fuel Consumption Tests
;;; ============================================================

(printf "\n--- Fuel Consumption Tests ---\n")

;; Test 11: par consumes fuel for both evaluations
(let ([result (eval-expr '(par 1 2) empty-env 10)])
     (test "par fuel consumption"
           '(ok 2 7)
           result))

;; Test 12: pseq consumes fuel for both evaluations
(let ([result (eval-expr '(pseq 1 2) empty-env 10)])
     (test "pseq fuel consumption"
           '(ok 2 7)
           result))

;; Test 13: par suspends on insufficient fuel (first arg)
(let ([result (eval-expr '(par (prim 'add 1 2) 5) empty-env 1)])
     (test "par suspends on low fuel (first)"
           '(suspended (prim 'add 1 2) ())
           result))

;; Test 14: par suspends on insufficient fuel (second arg)
(let ([result (eval-expr '(par 1 (prim 'add 2 3)) empty-env 2)])
     (test "par suspends on low fuel (second)"
           '(suspended (prim 'add 2 3) ())
           result))

;; Test 15: pseq suspends on insufficient fuel (first arg)
(let ([result (eval-expr '(pseq (prim 'add 1 2) 5) empty-env 1)])
     (test "pseq suspends on low fuel (first)"
           '(suspended (prim 'add 1 2) ())
           result))

;; Test 16: pseq suspends on insufficient fuel (second arg)
(let ([result (eval-expr '(pseq 1 (prim 'add 2 3)) empty-env 2)])
     (test "pseq suspends on low fuel (second)"
           '(suspended (prim 'add 2 3) ())
           result))

;;; ============================================================
;;; Nested Par/Pseq Tests
;;; ============================================================

(printf "\n--- Nested Par/Pseq Tests ---\n")

;; Test 17: nested par
(let ([result (eval-expr '(par (par 1 2) (par 3 4)) empty-env 100)])
     (test "nested par"
           '(ok 4 93)
           result))

;; Test 18: nested pseq
(let ([result (eval-expr '(pseq (pseq 1 2) (pseq 3 4)) empty-env 100)])
     (test "nested pseq"
           '(ok 4 93)
           result))

;; Test 19: par inside pseq
(let ([result (eval-expr '(pseq (par 1 2) (par 3 4)) empty-env 100)])
     (test "par inside pseq"
           '(ok 4 93)
           result))

;; Test 20: pseq inside par
(let ([result (eval-expr '(par (pseq 1 2) (pseq 3 4)) empty-env 100)])
     (test "pseq inside par"
           '(ok 4 93)
           result))

;;; ============================================================
;;; Practical Use Cases
;;; ============================================================

(printf "\n--- Practical Use Cases ---\n")

;; Test 21: par for parallel computation hints
;; In a real parallel implementation, both would compute simultaneously
(let ([result (eval-expr '(par (prim 'mul 123 456)
                           (prim 'add (prim 'mul 123 456) 789))
                         empty-env
                         100)])
     (test "par with duplicate computation"
           '(ok 56877 91)
           result))

;; Test 22: pseq for forcing strict evaluation order
(let ([result (eval-expr '(pseq (prim 'add 1 2)
                           (prim 'mul 3 4))
                         empty-env
                         100)])
     (test "pseq ensures ordering"
           '(ok 12 93)
           result))

;; Test 23: par with let bindings
(let ([result (eval-expr '(let ((x 5))
                           (par (prim 'mul x 2)
                                (prim 'add x 3)))
                         empty-env
                         100)])
     (test "par with let bindings"
           '(ok 8 91)
           result))

;; Test 24: pseq with let bindings
(let ([result (eval-expr '(let ((x 5))
                           (pseq (prim 'mul x 2)
                                 (prim 'add x 3)))
                         empty-env
                         100)])
     (test "pseq with let bindings"
           '(ok 8 91)
           result))

;;; ============================================================
;;; Results
;;; ============================================================

(printf "\n================================================================\n")
(printf "                    TEST RESULTS\n")
(printf "================================================================\n\n")
(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "\n[SUCCESS] All par/pseq tests passed.\n")
    (printf "\n[FAILURE] Some tests failed.\n"))
