;;; lattice/algebra/test-multivariate-groebner.ss — Tests for Multivariate Polynomials and Gröbner Bases
;;; Run with: scheme --script lattice/algebra/test-multivariate-groebner.ss

(load "core/testing/test-framework.ss")
(load "lattice/algebra/groebner.ss")  ; loads multivariate.ss, field.ss transitively

;;; ============================================================
;;; Helper: Q-field for rational arithmetic
;;; ============================================================

(define F Q-field)
(define vars '(x y z))
(define lex (make-ordering 'lex vars))
(define grlex (make-ordering 'grlex vars))
(define grevlex (make-ordering 'grevlex vars))

;;; ============================================================
;;; Monomial Tests
;;; ============================================================

(test-group "monomial-construction"
  (define-test "make-monomial normalizes"
    (let ([m (make-monomial '((y . 2) (x . 1)))])
      (assert-equal '((x . 1) (y . 2)) m)))

  (define-test "make-monomial removes zero exponents"
    (let ([m (make-monomial '((x . 0) (y . 3)))])
      (assert-equal '((y . 3)) m)))

  (define-test "mono-one is empty list"
    (assert-equal '() (mono-one)))

  (define-test "mono-var creates single variable"
    (assert-equal '((x . 1)) (mono-var 'x)))

  (define-test "mono-power creates x^n"
    (assert-equal '((x . 3)) (mono-power 'x 3)))

  (define-test "mono-power of 0 is constant"
    (assert-equal '() (mono-power 'x 0))))

(test-group "monomial-properties"
  (define-test "mono-degree of x^2*y^3"
    (assert-equal 5 (mono-degree '((x . 2) (y . 3)))))

  (define-test "mono-degree of constant is 0"
    (assert-equal 0 (mono-degree (mono-one))))

  (define-test "mono-degree-in extracts specific variable"
    (assert-equal 2 (mono-degree-in '((x . 2) (y . 3)) 'x))
    (assert-equal 3 (mono-degree-in '((x . 2) (y . 3)) 'y)))

  (define-test "mono-degree-in missing variable is 0"
    (assert-equal 0 (mono-degree-in '((x . 2)) 'z)))

  (define-test "mono-vars lists variables"
    (assert-equal '(x y) (mono-vars '((x . 2) (y . 1))))))

(test-group "monomial-arithmetic"
  (define-test "mono-mul multiplies"
    ;; x^2 * y^3 = x^2*y^3
    (assert-equal '((x . 2) (y . 3))
                  (mono-mul '((x . 2)) '((y . 3)))))

  (define-test "mono-mul adds exponents for same variable"
    ;; x^2 * x^3 = x^5
    (assert-equal '((x . 5))
                  (mono-mul '((x . 2)) '((x . 3)))))

  (define-test "mono-divides? positive case"
    (assert-true (mono-divides? '((x . 1) (y . 1)) '((x . 2) (y . 3)))))

  (define-test "mono-divides? negative case"
    (assert-false (mono-divides? '((x . 3)) '((x . 2) (y . 3)))))

  (define-test "mono-divides? constant divides everything"
    (assert-true (mono-divides? (mono-one) '((x . 5)))))

  (define-test "mono-div subtracts exponents"
    ;; x^3*y^2 / x*y = x^2*y
    (assert-equal '((x . 2) (y . 1))
                  (mono-div '((x . 3) (y . 2)) '((x . 1) (y . 1)))))

  (define-test "mono-lcm takes max exponents"
    ;; lcm(x^2*y, x*y^3) = x^2*y^3
    (assert-equal '((x . 2) (y . 3))
                  (mono-lcm '((x . 2) (y . 1)) '((x . 1) (y . 3)))))

  (define-test "mono-gcd takes min exponents"
    ;; gcd(x^2*y, x*y^3) = x*y
    (assert-equal '((x . 1) (y . 1))
                  (mono-gcd '((x . 2) (y . 1)) '((x . 1) (y . 3)))))

  (define-test "mono-gcd of disjoint variables is 1"
    (assert-equal '() (mono-gcd '((x . 2)) '((y . 3))))))

;;; ============================================================
;;; Monomial Ordering Tests
;;; ============================================================

(test-group "monomial-orderings"
  (define-test "lex: x^2 > xy"
    (assert-equal 1 (mono-compare-lex '((x . 2)) '((x . 1) (y . 1)) vars)))

  (define-test "lex: xy > y^2"
    (assert-equal 1 (mono-compare-lex '((x . 1) (y . 1)) '((y . 2)) vars)))

  (define-test "lex: equal monomials"
    (assert-equal 0 (mono-compare-lex '((x . 1)) '((x . 1)) vars)))

  (define-test "grlex: degree dominates"
    ;; x^2*y (deg 3) > x*y (deg 2)
    (assert-equal 1 (mono-compare-grlex '((x . 2) (y . 1)) '((x . 1) (y . 1)) vars)))

  (define-test "grlex: same degree falls back to lex"
    ;; x^2 and xy both degree 2, but x^2 > xy in lex
    (assert-equal 1 (mono-compare-grlex '((x . 2)) '((x . 1) (y . 1)) vars)))

  (define-test "grevlex: same degree, reverse comparison"
    ;; For degree 2: x^2 vs y^2
    ;; In grevlex, compare from last variable (z, y, x)
    ;; y: 0 vs 2, x: 2 vs 0. Last var (y) differs: 0 < 2, so x^2 > y^2
    (assert-equal 1 (mono-compare-grevlex '((x . 2)) '((y . 2)) vars)))

  (define-test "make-ordering lex"
    (let ([cmp (make-ordering 'lex vars)])
      (assert-equal 1 (cmp '((x . 2)) '((x . 1))))))

  (define-test "make-ordering grlex alias deglex"
    (let ([cmp (make-ordering 'deglex vars)])
      (assert-equal 1 (cmp '((x . 1) (y . 1) (z . 1)) '((x . 2)))))))

;;; ============================================================
;;; Multivariate Polynomial Tests
;;; ============================================================

(test-group "mpoly-construction"
  (define-test "mpoly-zero is zero"
    (assert-true (mpoly-zero? (mpoly-zero F vars lex))))

  (define-test "mpoly-one is not zero"
    (assert-false (mpoly-zero? (mpoly-one F vars lex))))

  (define-test "mpoly-var creates x"
    (let ([x (mpoly-var F vars lex 'x)])
      (assert-false (mpoly-zero? x))
      (assert-equal 1 (mpoly-degree x))))

  (define-test "mpoly-constant creates c"
    (let ([p (mpoly-constant F vars lex 5)])
      (assert-equal 0 (mpoly-degree p))))

  (define-test "mpoly-from-terms with grlex"
    (let ([p (mpoly-from-terms F vars 'grlex
               (list (cons 1 '((x . 2))) (cons 1 '((y . 1)))))])
      (assert-equal 2 (mpoly-degree p)))))

(test-group "mpoly-properties"
  (define-test "mpoly-degree of x^2 + y^3"
    (let ([p (make-mpoly F vars lex
               (list (cons 1 '((x . 2))) (cons 1 '((y . 3)))))])
      (assert-equal 3 (mpoly-degree p))))

  (define-test "mpoly-degree-in"
    (let ([p (make-mpoly F vars lex
               (list (cons 1 '((x . 2) (y . 1))) (cons 1 '((y . 3)))))])
      (assert-equal 2 (mpoly-degree-in p 'x))
      (assert-equal 3 (mpoly-degree-in p 'y))))

  (define-test "mpoly-leading-term with lex ordering"
    ;; With lex on (x y z), x^2 > y^3
    (let* ([p (make-mpoly F vars lex
                (list (cons 3 '((x . 2))) (cons 7 '((y . 3)))))]
           [lt (mpoly-leading-term p)])
      (assert-equal 3 (car lt))
      (assert-true (mono-equal? '((x . 2)) (cdr lt))))))

(test-group "mpoly-arithmetic"
  (define-test "addition"
    (let* ([x (mpoly-var F vars lex 'x)]
           [y (mpoly-var F vars lex 'y)]
           [sum (mpoly-add x y)])
      (assert-equal 1 (mpoly-degree sum))))

  (define-test "addition cancels like terms"
    (let* ([p (mpoly-var F vars lex 'x)]
           [q (mpoly-neg p)]
           [sum (mpoly-add p q)])
      (assert-true (mpoly-zero? sum))))

  (define-test "subtraction"
    (let* ([p (mpoly-var F vars lex 'x)]
           [q (mpoly-sub p p)])
      (assert-true (mpoly-zero? q))))

  (define-test "scalar multiplication"
    (let* ([x (mpoly-var F vars lex 'x)]
           [scaled (mpoly-scale x 3)])
      (assert-equal 3 (mpoly-leading-coeff scaled))))

  (define-test "multiplication: x * y = xy"
    (let* ([x (mpoly-var F vars lex 'x)]
           [y (mpoly-var F vars lex 'y)]
           [prod (mpoly-mul x y)])
      (assert-equal 2 (mpoly-degree prod))
      (assert-true (mono-equal? '((x . 1) (y . 1)) (mpoly-leading-mono prod)))))

  (define-test "multiplication distributes: x(x+y) = x^2 + xy"
    (let* ([x (mpoly-var F vars lex 'x)]
           [y (mpoly-var F vars lex 'y)]
           [sum (mpoly-add x y)]
           [prod (mpoly-mul x sum)])
      (assert-equal 2 (mpoly-degree prod))
      ;; Leading term should be x^2 (lex ordering)
      (assert-true (mono-equal? '((x . 2)) (mpoly-leading-mono prod)))))

  (define-test "power: x^3"
    (let* ([x (mpoly-var F vars lex 'x)]
           [cubed (mpoly-power x 3)])
      (assert-equal 3 (mpoly-degree cubed))))

  (define-test "power of zero is one"
    (let* ([x (mpoly-var F vars lex 'x)]
           [p (mpoly-power x 0)])
      (assert-false (mpoly-zero? p))
      (assert-equal 0 (mpoly-degree p)))))

(test-group "mpoly-equality"
  (define-test "equal polynomials"
    (let* ([x (mpoly-var F vars lex 'x)]
           [y (mpoly-var F vars lex 'y)]
           [p1 (mpoly-add x y)]
           [p2 (mpoly-add x y)])
      (assert-true (mpoly-equal? p1 p2))))

  (define-test "unequal polynomials"
    (let* ([x (mpoly-var F vars lex 'x)]
           [y (mpoly-var F vars lex 'y)])
      (assert-false (mpoly-equal? x y)))))

(test-group "mpoly-evaluation"
  (define-test "eval x^2 + y at (2, 3)"
    (let* ([p (make-mpoly F vars lex
                (list (cons 1 '((x . 2))) (cons 1 '((y . 1)))))]
           [result (mpoly-eval p '((x . 2) (y . 3) (z . 0)))])
      ;; 2^2 + 3 = 7
      (assert-equal 7 result)))

  (define-test "eval 3xy - 2z at (1, 2, 5)"
    (let* ([p (make-mpoly F vars lex
                (list (cons 3 '((x . 1) (y . 1)))
                      (cons -2 '((z . 1)))))]
           [result (mpoly-eval p '((x . 1) (y . 2) (z . 5)))])
      ;; 3*1*2 - 2*5 = 6 - 10 = -4
      (assert-equal -4 result)))

  (define-test "eval constant polynomial"
    (let* ([p (mpoly-constant F vars lex 42)]
           [result (mpoly-eval p '((x . 0) (y . 0) (z . 0)))])
      (assert-equal 42 result))))

(test-group "mpoly-division"
  (define-test "divmod simple case"
    ;; x^2 + xy + y^2 divided by (x + y) in Q[x,y] with lex
    (let* ([F2 Q-field]
           [v2 '(x y)]
           [ord (make-ordering 'lex v2)]
           [p (make-mpoly F2 v2 ord
                (list (cons 1 '((x . 2)))
                      (cons 1 '((x . 1) (y . 1)))
                      (cons 1 '((y . 2)))))]
           [g (make-mpoly F2 v2 ord
                (list (cons 1 '((x . 1))) (cons 1 '((y . 1)))))]
           [result (mpoly-divmod p (list g))]
           [quotients (car result)]
           [remainder (cdr result)])
      ;; x^2 + xy + y^2 = (x + y)(x) + y^2
      ;; So quotient should be x, remainder y^2
      (assert-equal 1 (length quotients))
      (assert-true (mpoly-equal? remainder
                     (make-mpoly F2 v2 ord (list (cons 1 '((y . 2)))))))))

  (define-test "divmod zero remainder"
    ;; x^2 - 1 divided by (x - 1) and (x + 1)
    (let* ([F2 Q-field]
           [v2 '(x)]
           [ord (make-ordering 'lex v2)]
           [p (make-mpoly F2 v2 ord
                (list (cons 1 '((x . 2))) (cons -1 '())))]
           [g1 (make-mpoly F2 v2 ord
                 (list (cons 1 '((x . 1))) (cons -1 '())))]
           [result (mpoly-divmod p (list g1))]
           [remainder (cdr result)])
      ;; x^2 - 1 = (x - 1)(x + 1), so dividing by (x-1) gives remainder 0
      (assert-true (mpoly-zero? remainder)))))

(test-group "mpoly-display"
  (define-test "mpoly->string for x^2 + y"
    (let* ([p (make-mpoly F vars lex
                (list (cons 1 '((x . 2))) (cons 1 '((y . 1)))))]
           [s (mpoly->string p)])
      (assert-true (string? s))
      (assert-true (> (string-length s) 0))))

  (define-test "mpoly->string for zero"
    (assert-equal "0" (mpoly->string (mpoly-zero F vars lex)))))

;;; ============================================================
;;; Gröbner Basis Tests
;;; ============================================================

(test-group "s-polynomial"
  (define-test "s-poly of x^2 + y and xy - 1"
    ;; f = x^2 + y, g = xy - 1
    ;; LM(f) = x^2, LM(g) = xy
    ;; lcm = x^2*y
    ;; S(f,g) = y*f - x*g = (x^2*y + y^2) - (x^2*y - x) = y^2 + x
    (let* ([v2 '(x y)]
           [ord (make-ordering 'lex v2)]
           [f (make-mpoly F v2 ord
                (list (cons 1 '((x . 2))) (cons 1 '((y . 1)))))]
           [g (make-mpoly F v2 ord
                (list (cons 1 '((x . 1) (y . 1))) (cons -1 '())))]
           [s (s-polynomial f g)])
      ;; S(f,g) should have degree 2 (y^2 + x)
      (assert-equal 2 (mpoly-degree s))
      (assert-false (mpoly-zero? s)))))

(test-group "buchberger-algorithm"
  (define-test "buchberger on simple system"
    ;; Classic example: I = <x^2 + y - 1, xy - 1> in Q[x,y] with lex
    (let* ([v2 '(x y)]
           [ord (make-ordering 'lex v2)]
           [f1 (make-mpoly F v2 ord
                  (list (cons 1 '((x . 2))) (cons 1 '((y . 1))) (cons -1 '())))]
           [f2 (make-mpoly F v2 ord
                  (list (cons 1 '((x . 1) (y . 1))) (cons -1 '())))]
           [G (buchberger (list f1 f2))])
      ;; Should be a Gröbner basis
      (assert-true (is-groebner-basis? G))
      ;; Should have at least 2 elements
      (assert-true (>= (length G) 2))))

  (define-test "buchberger preserves ideal"
    ;; The Gröbner basis should generate the same ideal
    ;; Test: original generators should reduce to zero
    (let* ([v2 '(x y)]
           [ord (make-ordering 'grlex v2)]
           [f1 (make-mpoly F v2 ord
                  (list (cons 1 '((x . 2))) (cons -1 '((y . 1)))))]
           [f2 (make-mpoly F v2 ord
                  (list (cons 1 '((x . 1) (y . 1))) (cons -1 '())))]
           [G (buchberger (list f1 f2))])
      (assert-true (ideal-membership? f1 G))
      (assert-true (ideal-membership? f2 G))))

  (define-test "buchberger of empty list"
    (assert-equal '() (buchberger '()))))

(test-group "reduced-basis"
  (define-test "reduce-basis produces monic polynomials"
    (let* ([v2 '(x y)]
           [ord (make-ordering 'lex v2)]
           [f1 (make-mpoly F v2 ord
                  (list (cons 2 '((x . 2))) (cons 2 '((y . 1)))))]
           [f2 (make-mpoly F v2 ord
                  (list (cons 3 '((x . 1) (y . 1))) (cons -3 '())))]
           [G (buchberger (list f1 f2))]
           [R (reduce-basis G)])
      ;; All leading coefficients should be 1
      (assert-true (andmap (lambda (p) (= 1 (mpoly-leading-coeff p))) R))
      ;; Should still be a Gröbner basis
      (assert-true (is-groebner-basis? R)))))

(test-group "ideal-membership"
  (define-test "member of ideal reduces to zero"
    ;; f = x^2 - y, g = x*y - x. Then x^3 - x*y = x*(x^2 - y) is in <f,g>
    (let* ([v2 '(x y)]
           [ord (make-ordering 'grlex v2)]
           [f (make-mpoly F v2 ord
                (list (cons 1 '((x . 2))) (cons -1 '((y . 1)))))]
           [g (make-mpoly F v2 ord
                (list (cons 1 '((x . 1) (y . 1))) (cons -1 '((x . 1)))))]
           [G (buchberger (list f g))]
           ;; x * f = x^3 - xy, which is in the ideal
           [h (mpoly-mul (mpoly-var F v2 ord 'x) f)])
      (assert-true (ideal-membership? h G))))

  (define-test "non-member does not reduce to zero"
    (let* ([v2 '(x y)]
           [ord (make-ordering 'grlex v2)]
           [f (make-mpoly F v2 ord
                (list (cons 1 '((x . 2))) (cons -1 '((y . 1)))))]
           [G (buchberger (list f))]
           ;; y^2 is NOT in <x^2 - y> (it's irreducible mod f)
           [h (make-mpoly F v2 ord
                (list (cons 1 '((y . 2)))))])
      (assert-false (ideal-membership? h G)))))

(test-group "solve-system"
  (define-test "solve-system computes reduced Gröbner basis"
    (let* ([v2 '(x y)]
           [ord (make-ordering 'grevlex v2)]
           [f1 (make-mpoly F v2 ord
                  (list (cons 1 '((x . 2))) (cons 1 '((y . 2))) (cons -1 '())))]
           [f2 (make-mpoly F v2 ord
                  (list (cons 1 '((x . 1))) (cons -1 '((y . 1)))))]
           [G (solve-system (list f1 f2))])
      (assert-true (is-groebner-basis? G))
      ;; All leading coefficients should be 1 (reduced)
      (assert-true (andmap (lambda (p) (= 1 (mpoly-leading-coeff p))) G)))))

(test-group "groebner-display"
  (define-test "groebner->string produces output"
    (let* ([v2 '(x y)]
           [ord (make-ordering 'lex v2)]
           [G (list (mpoly-var F v2 ord 'x) (mpoly-var F v2 ord 'y))]
           [s (groebner->string G)])
      (assert-true (string? s))
      (assert-true (> (string-length s) 0)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
