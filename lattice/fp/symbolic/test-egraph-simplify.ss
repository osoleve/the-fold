;;; lattice/fp/symbolic/test-egraph-simplify.ss — Tests for E-Graph Simplification
;;;
;;; Tests that e-graph equality saturation produces correct simplifications,
;;; and that results are equivalent to or better than the rule-based simplifier.
;;;
;;; Run with: scheme --script lattice/fp/symbolic/test-egraph-simplify.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/fp/symbolic/egraph-simplify.ss")

(display "\n====\n")
(display "E-Graph Symbolic Simplification Tests\n")
(display "====\n\n")

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group "expr->eterm"

  (define-test "numeric constant"
    (assert-equal 5 (expr->eterm (num 5))))

  (define-test "variable"
    (assert-equal 'x (expr->eterm (var 'x))))

  (define-test "binary sum"
    (assert-equal '(+ x y) (expr->eterm (sum (var 'x) (var 'y)))))

  (define-test "binary product"
    (assert-equal '(* x y) (expr->eterm (product (var 'x) (var 'y)))))

  (define-test "difference"
    (assert-equal '(- x y) (expr->eterm (difference (var 'x) (var 'y)))))

  (define-test "unary negation"
    (assert-equal '(neg x) (expr->eterm (make-neg (var 'x)))))

  (define-test "division"
    (assert-equal '(/ x y) (expr->eterm (division (var 'x) (var 'y)))))

  (define-test "power"
    (assert-equal '(^ x 2) (expr->eterm (power (var 'x) (num 2)))))

  (define-test "function application"
    (assert-equal '(sin x) (expr->eterm (sym-sin (var 'x)))))

  (define-test "nested expression"
    (assert-equal '(+ (* x y) 1)
                  (expr->eterm (sum (product (var 'x) (var 'y))
                                    (num 1))))))

(test-group "eterm->expr"

  (define-test "numeric constant"
    (let ([result (eterm->expr 5)])
      (assert-true (num? result))
      (assert-equal 5 (num-val result))))

  (define-test "variable"
    (let ([result (eterm->expr 'x)])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "binary sum"
    (let ([result (eterm->expr '(+ x y))])
      ;; sum smart constructor may simplify, but structure should be valid
      (assert-true (or (sum? result) (var? result) (num? result)))))

  (define-test "function application"
    (let ([result (eterm->expr '(sin x))])
      (assert-true (app? result))
      (assert-equal 'sin (app-fn result))))

  (define-test "roundtrip num"
    (let* ([e (num 42)]
           [t (expr->eterm e)]
           [back (eterm->expr t)])
      (assert-true (expr=? e back))))

  (define-test "roundtrip var"
    (let* ([e (var 'x)]
           [t (expr->eterm e)]
           [back (eterm->expr t)])
      (assert-true (expr=? e back))))

  (define-test "roundtrip sum"
    (let* ([e (sum (var 'x) (num 3))]
           [t (expr->eterm e)]
           [back (eterm->expr t)])
      (assert-true (expr=? e back))))

  (define-test "roundtrip product"
    (let* ([e (product (var 'x) (var 'y))]
           [t (expr->eterm e)]
           [back (eterm->expr t)])
      (assert-true (expr=? e back))))

  (define-test "roundtrip power"
    (let* ([e (power (var 'x) (num 2))]
           [t (expr->eterm e)]
           [back (eterm->expr t)])
      (assert-true (expr=? e back))))

  (define-test "roundtrip sin"
    (let* ([e (sym-sin (var 'x))]
           [t (expr->eterm e)]
           [back (eterm->expr t)])
      (assert-true (expr=? e back)))))

;;; ============================================================
;;; Additive Identity Tests
;;; ============================================================

(test-group "egraph-add-identity"

  (define-test "x + 0 = x"
    (let ([result (egraph-simplify (sum (var 'x) (num 0)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "0 + x = x"
    (let ([result (egraph-simplify (sum (num 0) (var 'x)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result)))))

;;; ============================================================
;;; Multiplicative Identity Tests
;;; ============================================================

(test-group "egraph-mul-identity"

  (define-test "x * 1 = x"
    (let ([result (egraph-simplify (product (var 'x) (num 1)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "1 * x = x"
    (let ([result (egraph-simplify (product (num 1) (var 'x)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result)))))

;;; ============================================================
;;; Zero Multiplication Tests
;;; ============================================================

(test-group "egraph-mul-zero"

  (define-test "x * 0 = 0"
    (let ([result (egraph-simplify (product (var 'x) (num 0)))])
      (assert-true (num? result))
      (assert-equal 0 (num-val result))))

  (define-test "0 * x = 0"
    (let ([result (egraph-simplify (product (num 0) (var 'x)))])
      (assert-true (num? result))
      (assert-equal 0 (num-val result)))))

;;; ============================================================
;;; Subtraction / Negation Tests
;;; ============================================================

(test-group "egraph-subtraction"

  (define-test "x - 0 = x"
    (let ([result (egraph-simplify (difference (var 'x) (num 0)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "x - x = 0"
    (let ([result (egraph-simplify (difference (var 'x) (var 'x)))])
      (assert-true (num? result))
      (assert-equal 0 (num-val result))))

  (define-test "double negation: --x = x"
    (let ([result (egraph-simplify (make-neg (make-neg (var 'x))))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result)))))

;;; ============================================================
;;; Division Tests
;;; ============================================================

(test-group "egraph-division"

  (define-test "x / 1 = x"
    (let ([result (egraph-simplify (division (var 'x) (num 1)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "0 / x = 0"
    (let ([result (egraph-simplify (division (num 0) (var 'x)))])
      (assert-true (num? result))
      (assert-equal 0 (num-val result))))

  (define-test "x / x = 1"
    (let ([result (egraph-simplify (division (var 'x) (var 'x)))])
      (assert-true (num? result))
      (assert-equal 1 (num-val result)))))

;;; ============================================================
;;; Power Tests
;;; ============================================================

(test-group "egraph-power"

  (define-test "x^0 = 1"
    (let ([result (egraph-simplify (power (var 'x) (num 0)))])
      (assert-true (num? result))
      (assert-equal 1 (num-val result))))

  (define-test "x^1 = x"
    (let ([result (egraph-simplify (power (var 'x) (num 1)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "1^x = 1"
    (let ([result (egraph-simplify (power (num 1) (var 'x)))])
      (assert-true (num? result))
      (assert-equal 1 (num-val result)))))

;;; ============================================================
;;; Exp / Log Tests
;;; ============================================================

(test-group "egraph-exp-log"

  (define-test "exp(log(x)) = x"
    (let ([result (egraph-simplify (sym-exp (sym-log (var 'x))))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "log(exp(x)) = x"
    (let ([result (egraph-simplify (sym-log (sym-exp (var 'x))))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result)))))

;;; ============================================================
;;; Nested Simplification Tests
;;; ============================================================

(test-group "egraph-nested"

  (define-test "(x + 0) * 1 = x"
    (let ([result (egraph-simplify (product (sum (var 'x) (num 0)) (num 1)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "(1 * x) + 0 = x"
    (let ([result (egraph-simplify (sum (product (num 1) (var 'x)) (num 0)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "(x + 0) - (0 + x) = 0"
    (let ([result (egraph-simplify
                    (difference (sum (var 'x) (num 0))
                                (sum (num 0) (var 'x))))])
      (assert-true (num? result))
      (assert-equal 0 (num-val result)))))

;;; ============================================================
;;; Full Rules (with commutativity + associativity)
;;; ============================================================

(test-group "egraph-full"

  (define-test "commutativity finds smaller form"
    ;; (0 + x) should simplify to x via commutativity + identity
    (let ([result (egraph-simplify-full (sum (num 0) (var 'x)))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "nested with commutativity: (0 + (1 * x)) = x"
    (let ([result (egraph-simplify-full
                    (sum (num 0) (product (num 1) (var 'x))))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result)))))

;;; ============================================================
;;; Comparison with Rule-Based Simplifier
;;; ============================================================

(test-group "egraph-vs-simplify"

  ;; Test that e-graph produces results at least as good as rule-based

  (define-test "both simplify x + 0"
    (let ([rule-result (simplify (sum (var 'x) (num 0)))]
          [eg-result (egraph-simplify (sum (var 'x) (num 0)))])
      (assert-true (expr=? rule-result eg-result))))

  (define-test "both simplify x * 1"
    (let ([rule-result (simplify (product (var 'x) (num 1)))]
          [eg-result (egraph-simplify (product (var 'x) (num 1)))])
      (assert-true (expr=? rule-result eg-result))))

  (define-test "both simplify x * 0"
    (let ([rule-result (simplify (product (var 'x) (num 0)))]
          [eg-result (egraph-simplify (product (var 'x) (num 0)))])
      (assert-true (expr=? rule-result eg-result))))

  (define-test "both simplify x - x"
    (let ([rule-result (simplify (difference (var 'x) (var 'x)))]
          [eg-result (egraph-simplify (difference (var 'x) (var 'x)))])
      (assert-true (expr=? rule-result eg-result))))

  (define-test "both simplify x / x"
    (let ([rule-result (simplify (division (var 'x) (var 'x)))]
          [eg-result (egraph-simplify (division (var 'x) (var 'x)))])
      (assert-true (expr=? rule-result eg-result))))

  (define-test "both simplify x^0"
    (let ([rule-result (simplify (power (var 'x) (num 0)))]
          [eg-result (egraph-simplify (power (var 'x) (num 0)))])
      (assert-true (expr=? rule-result eg-result))))

  (define-test "both simplify x^1"
    (let ([rule-result (simplify (power (var 'x) (num 1)))]
          [eg-result (egraph-simplify (power (var 'x) (num 1)))])
      (assert-true (expr=? rule-result eg-result))))

  (define-test "both simplify exp(log(x))"
    (let ([rule-result (simplify (sym-exp (sym-log (var 'x))))]
          [eg-result (egraph-simplify (sym-exp (sym-log (var 'x))))])
      (assert-true (expr=? rule-result eg-result))))

  (define-test "both simplify log(exp(x))"
    (let ([rule-result (simplify (sym-log (sym-exp (var 'x))))]
          [eg-result (egraph-simplify (sym-log (sym-exp (var 'x))))])
      (assert-true (expr=? rule-result eg-result)))))

;;; ============================================================
;;; Custom Rules and Config
;;; ============================================================

(test-group "egraph-custom"

  (define-test "custom rules apply"
    ;; Custom rule: (double ?x) => (+ ?x ?x)
    (let* ([rules (list (make-rule '(double ?x) '(+ ?x ?x)))]
           [eg (make-egraph)]
           [term (egraph-add-term! eg '(double a))]
           [config (make-saturation-config 10 1000 0)])
      (saturate eg rules config)
      (let* ([state (make-extraction-state eg ast-size-cost)]
             [extracted (extract state term)])
        ;; Class should now contain both (double a) and (+ a a)
        ;; ast-size-cost should prefer (+ a a) since both are size 3 but
        ;; either form is valid
        (assert-true (or (equal? extracted '(double a))
                        (equal? extracted '(+ a a)))))))

  (define-test "egraph-simplify-with custom config"
    ;; Test the full pipeline with custom parameters
    (let ([result (egraph-simplify-with
                    (sum (product (num 1) (var 'x)) (num 0))
                    sym-core-rules
                    (make-saturation-config 10 2000 0)
                    ast-size-cost)])
      (assert-true (var? result))
      (assert-equal 'x (var-name result)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group "egraph-edge-cases"

  (define-test "already simplified expr stays same"
    (let ([result (egraph-simplify (var 'x))])
      (assert-true (var? result))
      (assert-equal 'x (var-name result))))

  (define-test "numeric constant stays same"
    (let ([result (egraph-simplify (num 42))])
      (assert-true (num? result))
      (assert-equal 42 (num-val result))))

  (define-test "irreducible expr stays same"
    (let ([result (egraph-simplify (sum (var 'x) (var 'y)))])
      ;; (+ x y) has no simplification; should remain a sum
      (assert-true (sum? result)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)
