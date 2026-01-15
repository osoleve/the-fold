;;; lattice/fp/symbolic/test-solve.ss — Tests for Symbolic Equation Solving
;;;
;;; Run: scheme --script lattice/fp/symbolic/test-solve.ss

(load "core/testing/test-framework.ss")
(load "lattice/fp/symbolic/solve.ss")

(display "\n")
(display "====\n")
(display "    SYMBOLIC EQUATION SOLVING TESTS\n")
(display "====\n")

;;; ====
;;; Helper: Check if solution is correct by substitution
;;; ====

(define (verify-solution expr var-sym solution)
  (let* ([substituted (subst expr var-sym solution)]
         [simplified (simplify substituted)])
    (and (num? simplified)
         (< (abs (num-val simplified)) 1e-10))))

(define (verify-solution-approx expr var-sym solution tolerance)
  (let* ([substituted (subst expr var-sym solution)]
         [simplified (simplify substituted)])
    (and (num? simplified)
         (< (abs (num-val simplified)) tolerance))))

;;; ====
;;; Linear Equation Tests
;;; ====

(test-group linear-tests

  (define-test linear-simple
    ;; 2x + 4 = 0 → x = -2
    (let* ([expr (sum (product (num 2) (var 'x)) (num 4))]
           [sol (solve-for expr 'x)])
      (assert-true (num? sol))
      (assert-equal -2 (num-val sol))))

  (define-test linear-with-negative
    ;; -3x + 9 = 0 → x = 3
    (let* ([expr (sum (product (num -3) (var 'x)) (num 9))]
           [sol (solve-for expr 'x)])
      (assert-true (num? sol))
      (assert-equal 3 (num-val sol))))

  (define-test linear-fractional-result
    ;; 3x + 2 = 0 → x = -2/3
    (let* ([expr (sum (product (num 3) (var 'x)) (num 2))]
           [sol (solve-for expr 'x)]
           [simplified (simplify sol)])
      ;; Solution should simplify to -2/3
      (assert-true (not (not sol)))  ; sol exists
      ;; Verify by substitution: 3*sol + 2 should = 0
      (let* ([check (simplify (sum (product (num 3) sol) (num 2)))])
        (assert-true (num? check))
        (assert-true (< (abs (num-val check)) 1e-10)))))

  (define-test linear-just-variable
    ;; x + 5 = 0 → x = -5
    (let* ([expr (sum (var 'x) (num 5))]
           [sol (solve-for expr 'x)])
      (assert-true (num? sol))
      (assert-equal -5 (num-val sol))))

  (define-test linear-equation-form
    ;; Equation: 2x = 6 → x = 3
    (let* ([eq (make-equation (product (num 2) (var 'x)) (num 6))]
           [sol (solve-for eq 'x)])
      (assert-true (num? sol))
      (assert-equal 3 (num-val sol))))

  (define-test linear-no-solution
    ;; 0*x + 5 = 0 → no solution
    (let* ([expr (num 5)]
           [sol (solve expr 'x)])
      (assert-true (null? sol))))

  (define-test linear-infinite-solutions
    ;; 0*x + 0 = 0 → infinite solutions
    (let* ([expr (num 0)]
           [sol (solve expr 'x)])
      (assert-equal 'infinite sol))))

;;; ====
;;; Quadratic Equation Tests
;;; ====

(test-group quadratic-tests

  (define-test quadratic-integer-roots
    ;; x² - 5x + 6 = 0 → x = 2, 3
    (let* ([expr (sum (power (var 'x) (num 2))
                      (sum (product (num -5) (var 'x))
                           (num 6)))]
           [sols (solve expr 'x)])
      (assert-true (list? sols))
      (assert-equal 2 (length sols))
      ;; Verify both solutions
      (assert-true (verify-solution-approx expr 'x (car sols) 1e-6))
      (assert-true (verify-solution-approx expr 'x (cadr sols) 1e-6))))

  (define-test quadratic-double-root
    ;; x² - 4x + 4 = 0 → x = 2 (double)
    (let* ([expr (sum (power (var 'x) (num 2))
                      (sum (product (num -4) (var 'x))
                           (num 4)))]
           [sols (solve expr 'x)])
      (assert-true (list? sols))
      (assert-equal 2 (length sols))
      ;; Both solutions should be 2
      (assert-true (verify-solution-approx expr 'x (car sols) 1e-6))))

  (define-test quadratic-formula-structure
    ;; x² + x + 1 = 0 (complex roots)
    ;; Should return expressions with sqrt(-3)
    (let* ([expr (sum (power (var 'x) (num 2))
                      (sum (var 'x) (num 1)))]
           [sols (solve expr 'x)])
      (assert-true (list? sols))
      (assert-equal 2 (length sols))))

  (define-test quadratic-coefficient-extraction
    ;; 2x² - 8x + 6 = 0 → x = 1, 3
    (let* ([expr (sum (product (num 2) (power (var 'x) (num 2)))
                      (sum (product (num -8) (var 'x))
                           (num 6)))]
           [sols (solve expr 'x)])
      (assert-true (list? sols))
      (assert-equal 2 (length sols)))))

;;; ====
;;; Cubic Equation Tests
;;; ====

(test-group cubic-tests

  (define-test cubic-simple
    ;; x³ - 6x² + 11x - 6 = 0 → x = 1, 2, 3
    (let* ([expr (sum (power (var 'x) (num 3))
                      (sum (product (num -6) (power (var 'x) (num 2)))
                           (sum (product (num 11) (var 'x))
                                (num -6))))]
           [sols (solve expr 'x)])
      (assert-true (list? sols))
      ;; Should return all 3 roots
      (assert-equal 3 (length sols))))

  (define-test cubic-returns-three-roots
    ;; x³ - 1 = 0 → x = 1, ω, ω² (complex roots of unity)
    (let* ([expr (difference (power (var 'x) (num 3)) (num 1))]
           [sols (solve expr 'x)])
      (assert-true (list? sols))
      ;; Should return all 3 roots
      (assert-equal 3 (length sols)))))

;;; ====
;;; Polynomial Degree Detection Tests
;;; ====

(test-group degree-tests

  (define-test degree-constant
    (assert-equal 0 (polynomial-degree (num 5) 'x)))

  (define-test degree-linear
    (assert-equal 1 (polynomial-degree (var 'x) 'x)))

  (define-test degree-quadratic
    (assert-equal 2 (polynomial-degree (power (var 'x) (num 2)) 'x)))

  (define-test degree-sum
    ;; x² + x + 1 has degree 2
    (let ([expr (sum (power (var 'x) (num 2))
                     (sum (var 'x) (num 1)))])
      (assert-equal 2 (polynomial-degree expr 'x))))

  (define-test degree-product
    ;; x * x = x² has degree 2
    (let ([expr (product (var 'x) (var 'x))])
      (assert-equal 2 (polynomial-degree expr 'x))))

  (define-test degree-other-var
    ;; y² has degree 0 in x
    (assert-equal 0 (polynomial-degree (power (var 'y) (num 2)) 'x))))

;;; ====
;;; Coefficient Extraction Tests
;;; ====

(test-group coefficient-tests

  (define-test extract-linear-coeffs
    ;; 3x + 5
    (let ([expr (sum (product (num 3) (var 'x)) (num 5))])
      (let-values ([(a b) (collect-linear-coeffs expr 'x)])
        (assert-true (num? a))
        (assert-equal 3 (num-val a))
        (assert-true (num? b))
        (assert-equal 5 (num-val b)))))

  (define-test extract-poly-coeffs
    ;; x² + 2x + 3 → [3, 2, 1]
    (let* ([expr (sum (power (var 'x) (num 2))
                      (sum (product (num 2) (var 'x))
                           (num 3)))]
           [coeffs (extract-poly-coefficients expr 'x)])
      (assert-true (list? coeffs))
      (assert-equal 3 (length coeffs))
      (assert-true (num? (car coeffs)))
      (assert-equal 3 (num-val (car coeffs)))     ; constant
      (assert-true (num? (cadr coeffs)))
      (assert-equal 2 (num-val (cadr coeffs)))    ; linear
      (assert-true (num? (caddr coeffs)))
      (assert-equal 1 (num-val (caddr coeffs))))) ; quadratic

  (define-test is-polynomial-true
    (assert-true (is-polynomial? (sum (var 'x) (num 1)) 'x))
    (assert-true (is-polynomial? (power (var 'x) (num 3)) 'x))
    (assert-true (is-polynomial? (product (num 2) (var 'x)) 'x)))

  (define-test is-polynomial-false
    ;; sin(x) is not polynomial
    (assert-false (is-polynomial? (sym-sin (var 'x)) 'x))
    ;; x^(-1) is not polynomial
    (assert-false (is-polynomial? (power (var 'x) (num -1)) 'x))))

;;; ====
;;; Linear System Tests
;;; ====

(test-group linear-system-tests

  (define-test system-2x2
    ;; x + y = 3
    ;; x - y = 1
    ;; Solution: x = 2, y = 1
    (let* ([eq1 (difference (sum (var 'x) (var 'y)) (num 3))]
           [eq2 (difference (difference (var 'x) (var 'y)) (num 1))]
           [solution (solve-linear-system (list eq1 eq2) '(x y))])
      (assert-true (list? solution))
      (let ([x-sol (cdr (assq 'x solution))]
            [y-sol (cdr (assq 'y solution))])
        (assert-true (num? x-sol))
        (assert-equal 2 (num-val x-sol))
        (assert-true (num? y-sol))
        (assert-equal 1 (num-val y-sol)))))

  (define-test system-2x2-fractional
    ;; 2x + 3y = 8
    ;; 4x - y = 2
    ;; Solution: x = 1, y = 2
    (let* ([eq1 (difference (sum (product (num 2) (var 'x))
                                 (product (num 3) (var 'y)))
                            (num 8))]
           [eq2 (difference (difference (product (num 4) (var 'x))
                                        (var 'y))
                            (num 2))]
           [solution (solve-linear-system (list eq1 eq2) '(x y))])
      (assert-true (list? solution))
      (let ([x-sol (cdr (assq 'x solution))]
            [y-sol (cdr (assq 'y solution))])
        ;; Verify solutions exist and simplify to expected values
        (assert-true (not (not x-sol)))
        (assert-true (not (not y-sol)))
        ;; Check x = 1
        (let ([x-simplified (simplify x-sol)])
          (assert-true (num? x-simplified))
          (assert-equal 1 (num-val x-simplified)))
        ;; Check y = 2
        (let ([y-simplified (simplify y-sol)])
          (assert-true (num? y-simplified))
          (assert-equal 2 (num-val y-simplified)))))))

;;; ====
;;; Equation Representation Tests
;;; ====

(test-group equation-tests

  (define-test make-equation-test
    (let ([eq (make-equation (var 'x) (num 5))])
      (assert-true (equation? eq))
      (assert-equal (var 'x) (equation-lhs eq))
      (assert-equal (num 5) (equation-rhs eq))))

  (define-test equation-to-expr
    ;; x = 5 → x - 5
    (let* ([eq (make-equation (var 'x) (num 5))]
           [expr (equation->expr eq)])
      (assert-true (difference? expr)))))

;;; ====
;;; Contains-var Tests
;;; ====

(test-group contains-var-tests

  (define-test contains-var-simple
    (assert-true (contains-var? (var 'x) 'x))
    (assert-false (contains-var? (var 'y) 'x))
    (assert-false (contains-var? (num 5) 'x)))

  (define-test contains-var-nested
    (assert-true (contains-var? (sum (var 'x) (num 1)) 'x))
    (assert-true (contains-var? (product (num 2) (var 'x)) 'x))
    (assert-false (contains-var? (sum (var 'y) (num 1)) 'x))))

;;; ====
;;; Factored Polynomial Tests (expand before solving)
;;; ====

(test-group factored-tests

  (define-test factored-quadratic
    ;; (x - 2)(x - 3) = 0 → x = 2, 3
    ;; This previously failed because expand wasn't called
    (let* ([expr (product (difference (var 'x) (num 2))
                          (difference (var 'x) (num 3)))]
           [sols (solve expr 'x)])
      (assert-true (list? sols))
      (assert-equal 2 (length sols))
      ;; Verify solutions
      (assert-true (verify-solution-approx expr 'x (car sols) 1e-6))
      (assert-true (verify-solution-approx expr 'x (cadr sols) 1e-6))))

  (define-test factored-with-constant
    ;; 2(x - 1)(x + 1) = 0 → x = 1, -1
    (let* ([expr (product (num 2)
                          (product (difference (var 'x) (num 1))
                                   (sum (var 'x) (num 1))))]
           [sols (solve expr 'x)])
      (assert-true (list? sols))
      (assert-equal 2 (length sols))))

  (define-test factored-linear
    ;; (x - 5) = 0 → x = 5
    (let* ([expr (difference (var 'x) (num 5))]
           [sol (solve-for expr 'x)])
      (assert-true (num? sol))
      (assert-equal 5 (num-val sol))))

  (define-test factored-cubic
    ;; (x - 1)(x - 2)(x - 3) = 0 → roots at 1, 2, 3
    (let* ([expr (product (difference (var 'x) (num 1))
                          (product (difference (var 'x) (num 2))
                                   (difference (var 'x) (num 3))))]
           [sols (solve expr 'x)])
      (assert-true (list? sols))
      ;; Should expand to cubic and return 3 roots
      (assert-equal 3 (length sols)))))

;;; ====
;;; Singular System Tests
;;; ====

(test-group singular-system-tests

  (define-test singular-system-no-solution
    ;; x + y = 1
    ;; x + y = 2
    ;; Parallel lines - no solution
    (let* ([eq1 (difference (sum (var 'x) (var 'y)) (num 1))]
           [eq2 (difference (sum (var 'x) (var 'y)) (num 2))]
           [solution (solve-linear-system (list eq1 eq2) '(x y))])
      ;; Should return #f for singular/inconsistent system
      (assert-false solution)))

  (define-test singular-system-dependent
    ;; x + y = 2
    ;; 2x + 2y = 4
    ;; Same line (dependent) - singular matrix
    (let* ([eq1 (difference (sum (var 'x) (var 'y)) (num 2))]
           [eq2 (difference (sum (product (num 2) (var 'x))
                                 (product (num 2) (var 'y)))
                            (num 4))]
           [solution (solve-linear-system (list eq1 eq2) '(x y))])
      ;; Returns #f for singular system (could be extended to return parameterized solution)
      (assert-false solution))))

(display "\nRunning tests...\n\n")
(run-all-tests)
