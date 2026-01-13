;;; lattice/algebra/test-polynomial.ss — Tests for Polynomial Algebra
;;;
;;; Tests for univariate polynomials, multivariate polynomials, and Gröbner bases.

(load "core/testing/test-framework.ss")
(load "lattice/algebra/polynomial.ss")
(load "lattice/algebra/multivariate.ss")
(load "lattice/algebra/groebner.ss")

;;; ============================================================
;;; Test Helpers: Use Q-field from field.ss
;;; ============================================================

;;; Q-field is defined in field.ss and exported via polynomial.ss

;;; ============================================================
;;; Univariate Polynomial Tests
;;; ============================================================

(test-group "polynomial-construction"

  (define-test "zero-polynomial"
    (let ([p (poly-zero-over Q-field)])
      (assert-true (poly-zero? p))
      (assert-equal 0 (poly-degree p))))

  (define-test "constant-polynomial"
    (let ([p (poly-constant Q-field 5)])
      (assert-false (poly-zero? p))
      (assert-equal 0 (poly-degree p))
      (assert-equal 5 (poly-leading-coeff p))))

  (define-test "monomial-creation"
    (let ([p (poly-monomial Q-field 3 4)])  ; 3x^4
      (assert-equal 4 (poly-degree p))
      (assert-equal 3 (poly-leading-coeff p))
      (assert-equal 3 (poly-coeff-at p 4))
      (assert-equal 0 (poly-coeff-at p 3))))

  (define-test "polynomial-x"
    (let ([x (poly-x Q-field)])
      (assert-equal 1 (poly-degree x))
      (assert-equal 1 (poly-leading-coeff x)))))

(test-group "polynomial-arithmetic"

  (define-test "poly-add"
    ;; (1 + 2x) + (3 + x^2) = 4 + 2x + x^2
    (let* ([p1 (make-polynomial Q-field '(1 2))]      ; 1 + 2x
           [p2 (make-polynomial Q-field '(3 0 1))]    ; 3 + x^2
           [sum (poly-add p1 p2)])
      (assert-equal 2 (poly-degree sum))
      (assert-equal 4 (poly-coeff-at sum 0))
      (assert-equal 2 (poly-coeff-at sum 1))
      (assert-equal 1 (poly-coeff-at sum 2))))

  (define-test "poly-sub"
    ;; (3 + x^2) - (1 + 2x) = 2 - 2x + x^2
    (let* ([p1 (make-polynomial Q-field '(3 0 1))]
           [p2 (make-polynomial Q-field '(1 2))]
           [diff (poly-sub p1 p2)])
      (assert-equal 2 (poly-degree diff))
      (assert-equal 2 (poly-coeff-at diff 0))
      (assert-equal -2 (poly-coeff-at diff 1))))

  (define-test "poly-mul"
    ;; (1 + x) * (1 - x) = 1 - x^2
    (let* ([p1 (make-polynomial Q-field '(1 1))]    ; 1 + x
           [p2 (make-polynomial Q-field '(1 -1))]  ; 1 - x
           [prod (poly-mul p1 p2)])
      (assert-equal 2 (poly-degree prod))
      (assert-equal 1 (poly-coeff-at prod 0))
      (assert-equal 0 (poly-coeff-at prod 1))
      (assert-equal -1 (poly-coeff-at prod 2))))

  (define-test "poly-scale"
    ;; 3 * (1 + 2x) = 3 + 6x
    (let* ([p (make-polynomial Q-field '(1 2))]
           [scaled (poly-scale p 3)])
      (assert-equal 3 (poly-coeff-at scaled 0))
      (assert-equal 6 (poly-coeff-at scaled 1))))

  (define-test "poly-power"
    ;; (1 + x)^3 = 1 + 3x + 3x^2 + x^3
    (let* ([p (make-polynomial Q-field '(1 1))]
           [cubed (poly-power p 3)])
      (assert-equal 3 (poly-degree cubed))
      (assert-equal 1 (poly-coeff-at cubed 0))
      (assert-equal 3 (poly-coeff-at cubed 1))
      (assert-equal 3 (poly-coeff-at cubed 2))
      (assert-equal 1 (poly-coeff-at cubed 3)))))

(test-group "polynomial-evaluation"

  (define-test "poly-eval-linear"
    ;; p(x) = 2 + 3x evaluated at x=4 is 2 + 12 = 14
    (let ([p (make-polynomial Q-field '(2 3))])
      (assert-equal 14 (poly-eval p 4))))

  (define-test "poly-eval-quadratic"
    ;; p(x) = 1 + x + x^2 at x=2 is 1 + 2 + 4 = 7
    (let ([p (make-polynomial Q-field '(1 1 1))])
      (assert-equal 7 (poly-eval p 2)))))

(test-group "polynomial-division"

  (define-test "poly-divmod-exact"
    ;; x^2 - 1 = (x + 1)(x - 1), so (x^2-1)/(x-1) = x+1
    (let* ([dividend (make-polynomial Q-field '(-1 0 1))]  ; -1 + x^2
           [divisor (make-polynomial Q-field '(-1 1))]     ; -1 + x
           [result (poly-divmod dividend divisor)]
           [quotient (car result)]
           [remainder (cdr result)])
      (assert-true (poly-zero? remainder))
      (assert-equal 1 (poly-degree quotient))
      (assert-equal 1 (poly-coeff-at quotient 0))
      (assert-equal 1 (poly-coeff-at quotient 1))))

  (define-test "poly-divmod-with-remainder"
    ;; x^2 / x = x remainder 0
    ;; (x^2 + 1) / x = x remainder 1
    (let* ([dividend (make-polynomial Q-field '(1 0 1))]  ; 1 + x^2
           [divisor (make-polynomial Q-field '(0 1))]     ; x
           [result (poly-divmod dividend divisor)]
           [quotient (car result)]
           [remainder (cdr result)])
      (assert-equal 0 (poly-degree remainder))
      (assert-equal 1 (poly-coeff-at remainder 0))
      (assert-equal 1 (poly-degree quotient)))))

(test-group "polynomial-gcd"

  (define-test "poly-gcd-coprime"
    ;; GCD(x+1, x-1) = 1 (coprime)
    (let* ([p1 (make-polynomial Q-field '(1 1))]   ; 1 + x
           [p2 (make-polynomial Q-field '(-1 1))]  ; -1 + x
           [g (poly-gcd p1 p2)])
      (assert-equal 0 (poly-degree g))))

  (define-test "poly-gcd-common-factor"
    ;; GCD((x+1)^2, (x+1)(x-1)) = x+1
    (let* ([xp1 (make-polynomial Q-field '(1 1))]   ; x + 1
           [xm1 (make-polynomial Q-field '(-1 1))]  ; x - 1
           [p1 (poly-mul xp1 xp1)]                 ; (x+1)^2
           [p2 (poly-mul xp1 xm1)]                 ; (x+1)(x-1)
           [g (poly-gcd p1 p2)])
      (assert-equal 1 (poly-degree g))
      ;; GCD is monic, should be x+1
      (assert-equal 1 (poly-leading-coeff g))))

  (define-test "poly-extended-gcd"
    ;; Extended GCD gives Bezout coefficients
    (let* ([p1 (make-polynomial Q-field '(1 1))]   ; x + 1
           [p2 (make-polynomial Q-field '(-1 1))]  ; x - 1
           [result (poly-extended-gcd p1 p2)]
           [g (car result)]
           [s (cadr result)]
           [t (caddr result)]
           ;; Verify: g = s*p1 + t*p2
           [sp1 (poly-mul s p1)]
           [tp2 (poly-mul t p2)]
           [sum (poly-add sp1 tp2)])
      (assert-true (poly-equal? g sum)))))

(test-group "polynomial-derivative"

  (define-test "derivative-polynomial"
    ;; d/dx (x^3 + 2x^2 + x) = 3x^2 + 4x + 1
    (let* ([p (make-polynomial Q-field '(0 1 2 1))]  ; x + 2x^2 + x^3
           [dp (poly-derivative p)])
      (assert-equal 2 (poly-degree dp))
      (assert-equal 1 (poly-coeff-at dp 0))
      (assert-equal 4 (poly-coeff-at dp 1))
      (assert-equal 3 (poly-coeff-at dp 2))))

  (define-test "derivative-constant"
    (let* ([p (poly-constant Q-field 5)]
           [dp (poly-derivative p)])
      (assert-true (poly-zero? dp)))))

(test-group "polynomial-factorization"

  (define-test "square-free"
    ;; (x+1)^2 * (x-1) has square factor (x+1)
    ;; square-free part is (x+1)(x-1) = x^2-1
    (let* ([xp1 (make-polynomial Q-field '(1 1))]
           [xm1 (make-polynomial Q-field '(-1 1))]
           [p (poly-mul (poly-mul xp1 xp1) xm1)]  ; (x+1)^2(x-1)
           [sf (poly-square-free p)])
      ;; Degree should be 2 (one copy of each factor)
      (assert-equal 2 (poly-degree sf)))))

(test-group "polynomial-interpolation"

  (define-test "lagrange-interpolation"
    ;; Interpolate through (0,1), (1,2), (2,5) → 1 + 0.5x + 0.5x^2
    ;; Actually: p(x) such that p(0)=1, p(1)=2, p(2)=5
    ;; This is 1 + x^2 (verify: f(0)=1, f(1)=2, f(2)=5)
    (let* ([points '((0 . 1) (1 . 2) (2 . 5))]
           [p (poly-lagrange-interpolate Q-field points)])
      (assert-equal 1 (poly-eval p 0))
      (assert-equal 2 (poly-eval p 1))
      (assert-equal 5 (poly-eval p 2))))

  (define-test "newton-interpolation"
    ;; Same test with Newton's method
    (let* ([points '((0 . 1) (1 . 2) (2 . 5))]
           [p (poly-newton-interpolate Q-field points)])
      (assert-equal 1 (poly-eval p 0))
      (assert-equal 2 (poly-eval p 1))
      (assert-equal 5 (poly-eval p 2)))))

;;; ============================================================
;;; Multivariate Polynomial Tests
;;; ============================================================

(test-group "multivariate-construction"

  (define-test "mpoly-zero"
    (let* ([vars '(x y)]
           [ord (make-ordering 'grlex vars)]
           [p (mpoly-zero Q-field vars ord)])
      (assert-true (mpoly-zero? p))))

  (define-test "mpoly-variable"
    (let* ([vars '(x y)]
           [ord (make-ordering 'grlex vars)]
           [px (mpoly-var Q-field vars ord 'x)]
           [py (mpoly-var Q-field vars ord 'y)])
      (assert-equal 1 (mpoly-degree px))
      (assert-equal 1 (mpoly-degree-in px 'x))
      (assert-equal 0 (mpoly-degree-in px 'y))))

  (define-test "mpoly-from-terms"
    ;; 2xy + 3x^2 + 1
    (let* ([vars '(x y)]
           [terms (list (cons 2 (make-monomial '((x . 1) (y . 1))))   ; 2xy
                        (cons 3 (make-monomial '((x . 2))))           ; 3x^2
                        (cons 1 (mono-one)))]                         ; 1
           [p (mpoly-from-terms Q-field vars 'grlex terms)])
      (assert-equal 2 (mpoly-degree p)))))

(test-group "multivariate-arithmetic"

  (define-test "mpoly-add"
    ;; (x + y) + (x - y) = 2x
    (let* ([vars '(x y)]
           [ord (make-ordering 'grlex vars)]
           [x (mpoly-var Q-field vars ord 'x)]
           [y (mpoly-var Q-field vars ord 'y)]
           [p1 (mpoly-add x y)]
           [p2 (mpoly-sub x y)]
           [sum (mpoly-add p1 p2)])
      ;; 2x has leading coefficient 2, degree 1
      (assert-equal 1 (mpoly-degree sum))
      (assert-equal 2 (mpoly-leading-coeff sum))))

  (define-test "mpoly-mul"
    ;; (x + y) * (x - y) = x^2 - y^2
    (let* ([vars '(x y)]
           [ord (make-ordering 'grlex vars)]
           [x (mpoly-var Q-field vars ord 'x)]
           [y (mpoly-var Q-field vars ord 'y)]
           [xpy (mpoly-add x y)]
           [xmy (mpoly-sub x y)]
           [prod (mpoly-mul xpy xmy)])
      ;; x^2 - y^2 has degree 2
      (assert-equal 2 (mpoly-degree prod)))))

(test-group "monomial-orderings"

  (define-test "lex-ordering"
    ;; In lex with x > y: x^2 > xy > x > y^3 > y^2 > y > 1
    (let* ([vars '(x y)]
           [cmp (make-ordering 'lex vars)]
           [x2 (make-monomial '((x . 2)))]
           [xy (make-monomial '((x . 1) (y . 1)))]
           [y3 (make-monomial '((y . 3)))])
      (assert-true (> (cmp x2 xy) 0))
      (assert-true (> (cmp xy y3) 0))))

  (define-test "grlex-ordering"
    ;; In grlex: total degree first, then lex
    ;; x^2y > xy^2 (both degree 3, x > y in lex)
    (let* ([vars '(x y)]
           [cmp (make-ordering 'grlex vars)]
           [x2y (make-monomial '((x . 2) (y . 1)))]
           [xy2 (make-monomial '((x . 1) (y . 2)))])
      (assert-true (> (cmp x2y xy2) 0))))

  (define-test "grevlex-ordering"
    ;; In grevlex: total degree first, then reverse lex (smaller last-var exp wins)
    (let* ([vars '(x y)]
           [cmp (make-ordering 'grevlex vars)]
           [x2y (make-monomial '((x . 2) (y . 1)))]
           [xy2 (make-monomial '((x . 1) (y . 2)))])
      ;; In grevlex, compare from last variable; smaller exponent wins
      ;; x^2y has y-exp=1, xy^2 has y-exp=2, so x^2y > xy^2
      (assert-true (> (cmp x2y xy2) 0)))))

(test-group "multivariate-evaluation"

  (define-test "mpoly-eval"
    ;; f = x^2 + xy + y at (x=2, y=3) = 4 + 6 + 3 = 13
    (let* ([vars '(x y)]
           [ord (make-ordering 'grlex vars)]
           [x (mpoly-var Q-field vars ord 'x)]
           [y (mpoly-var Q-field vars ord 'y)]
           [f (mpoly-add (mpoly-add (mpoly-power x 2)
                                    (mpoly-mul x y))
                        y)]
           [result (mpoly-eval f '((x . 2) (y . 3)))])
      (assert-equal 13 result))))

;;; ============================================================
;;; Gröbner Basis Tests
;;; ============================================================

(test-group "s-polynomial"

  (define-test "s-poly-computation"
    ;; S(x^2 - y, xy - 1)
    ;; LM(f) = x^2, LM(g) = xy, LCM = x^2y
    ;; S(f,g) = y*f - x*g = y(x^2-y) - x(xy-1) = x^2y - y^2 - x^2y + x = x - y^2
    (let* ([vars '(x y)]
           [ord (make-ordering 'grlex vars)]
           [x (mpoly-var Q-field vars ord 'x)]
           [y (mpoly-var Q-field vars ord 'y)]
           ;; f = x^2 - y
           [f (mpoly-sub (mpoly-power x 2) y)]
           ;; g = xy - 1
           [one (mpoly-one Q-field vars ord)]
           [g (mpoly-sub (mpoly-mul x y) one)]
           [s (s-polynomial f g)])
      ;; Result should be x - y^2
      (assert-equal 2 (mpoly-degree s)))))

(test-group "groebner-basis"

  (define-test "simple-groebner"
    ;; Ideal <x^2-1, x-1> should have Gröbner basis with x-1
    ;; because x^2-1 = (x-1)(x+1)
    (let* ([vars '(x)]
           [ord (make-ordering 'grlex vars)]
           [x (mpoly-var Q-field vars ord 'x)]
           [one (mpoly-one Q-field vars ord)]
           ;; x^2 - 1
           [f1 (mpoly-sub (mpoly-power x 2) one)]
           ;; x - 1
           [f2 (mpoly-sub x one)]
           [G (buchberger (list f1 f2))])
      ;; Basis should be non-empty
      (assert-true (not (null? G)))))

  (define-test "groebner-reduces-s-poly"
    ;; If G is a Gröbner basis, all S-polynomials reduce to 0
    (let* ([vars '(x y)]
           [ord (make-ordering 'grlex vars)]
           [x (mpoly-var Q-field vars ord 'x)]
           [y (mpoly-var Q-field vars ord 'y)]
           ;; Simple system: x, y
           [G (buchberger (list x y))])
      (assert-true (is-groebner-basis? G))))

  (define-test "ideal-membership"
    ;; x^2 - 1 is in <x-1, x+1> because x^2-1 = (x-1)(x+1)
    (let* ([vars '(x)]
           [ord (make-ordering 'grlex vars)]
           [x (mpoly-var Q-field vars ord 'x)]
           [one (mpoly-one Q-field vars ord)]
           ;; Generators: x-1 and x+1
           [f1 (mpoly-sub x one)]
           [f2 (mpoly-add x one)]
           ;; Test polynomial: x^2 - 1
           [test-poly (mpoly-sub (mpoly-power x 2) one)]
           [G (buchberger (list f1 f2))])
      (assert-true (ideal-membership? test-poly G)))))

(test-group "reduced-groebner"

  (define-test "reduce-makes-monic"
    (let* ([vars '(x)]
           [ord (make-ordering 'grlex vars)]
           [x (mpoly-var Q-field vars ord 'x)]
           ;; 2x + 2
           [f (mpoly-scale (mpoly-add x (mpoly-one Q-field vars ord)) 2)]
           [monic (mpoly-make-monic f Q-field)])
      (assert-equal 1 (mpoly-leading-coeff monic)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
