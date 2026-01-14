;;; docs/examples/scientific-computing/polynomial-algebra.ss
;;;
;;; Demonstrates polynomial algebra capabilities: univariate polynomials,
;;; division, GCD, interpolation, multivariate polynomials, and Gröbner bases.
;;;
;;; Run: scheme --script docs/examples/scientific-computing/polynomial-algebra.ss

(load "lattice/algebra/polynomial.ss")
(load "lattice/algebra/multivariate.ss")
(load "lattice/algebra/groebner.ss")

(display "=== Polynomial Algebra Examples ===\n\n")

;;; Q-field is defined in field.ss (loaded via polynomial.ss)

;;; ====
;;; Example 1: Univariate Polynomial Basics
;;; ====

(display "--- Example 1: Univariate Polynomials ---\n\n")

;; Create polynomials: coefficients in ascending power order [a0, a1, a2, ...]
;; x^2 + 2x + 1 = (x + 1)^2
(define p1 (make-polynomial Q-field '(1 2 1)))
(display (format "p1 = ~a\n" (poly->string p1)))

;; x^2 - 1 = (x + 1)(x - 1)
(define p2 (make-polynomial Q-field '(-1 0 1)))
(display (format "p2 = ~a\n" (poly->string p2)))

;; Arithmetic
(display (format "p1 + p2 = ~a\n" (poly->string (poly-add p1 p2))))
(display (format "p1 * p2 = ~a\n" (poly->string (poly-mul p1 p2))))

;; Evaluation
(display (format "p1(2) = ~a\n" (poly-eval p1 2)))
(display (format "p2(3) = ~a\n" (poly-eval p2 3)))

(newline)

;;; ====
;;; Example 2: Polynomial Division and GCD
;;; ====

(display "--- Example 2: Division and GCD ---\n\n")

;; Divide p1 * p2 by p2 should give p1
(define product (poly-mul p1 p2))
(display (format "p1 * p2 = ~a\n" (poly->string product)))

(let* ([result (poly-divmod product p2)]
       [quotient (car result)]
       [remainder (cdr result)])
  (display (format "(p1 * p2) / p2:\n"))
  (display (format "  quotient  = ~a\n" (poly->string quotient)))
  (display (format "  remainder = ~a\n" (poly->string remainder))))

;; GCD of p1 and p2 should be (x + 1)
(define g (poly-gcd p1 p2))
(display (format "\nGCD(p1, p2) = ~a\n" (poly->string g)))

;; Extended GCD: find s, t such that g = s*p1 + t*p2
(let* ([ext-result (poly-extended-gcd p1 p2)]
       [gcd (car ext-result)]
       [s (cadr ext-result)]
       [t (caddr ext-result)])
  (display (format "Extended GCD:\n"))
  (display (format "  gcd = ~a\n" (poly->string gcd)))
  (display (format "  s   = ~a\n" (poly->string s)))
  (display (format "  t   = ~a\n" (poly->string t)))
  (display (format "  Verify: s*p1 + t*p2 = ~a\n"
                   (poly->string (poly-add (poly-mul s p1) (poly-mul t p2))))))

(newline)

;;; ====
;;; Example 3: Polynomial Interpolation
;;; ====

(display "--- Example 3: Interpolation ---\n\n")

;; Find polynomial through points (0,1), (1,4), (2,9), (3,16)
;; These are (n, (n+1)^2), so should get (x+1)^2 = x^2 + 2x + 1
(define points '((0 . 1) (1 . 4) (2 . 9) (3 . 16)))

(display "Points: ")
(for-each (lambda (p) (display (format "(~a,~a) " (car p) (cdr p)))) points)
(newline)

(define interp-lagrange (poly-lagrange-interpolate Q-field points))
(display (format "Lagrange interpolation: ~a\n" (poly->string interp-lagrange)))

(define interp-newton (poly-newton-interpolate Q-field points))
(display (format "Newton interpolation:   ~a\n" (poly->string interp-newton)))

;; Verify at interpolation points
(display "Verification: ")
(for-each (lambda (p)
            (display (format "p(~a)=~a " (car p) (poly-eval interp-lagrange (car p)))))
          points)
(newline)
(newline)

;;; ====
;;; Example 4: Derivative and Square-Free Decomposition
;;; ====

(display "--- Example 4: Derivatives and Factorization ---\n\n")

;; (x + 1)^3 = x^3 + 3x^2 + 3x + 1
(define p-cubed (poly-power (make-polynomial Q-field '(1 1)) 3))
(display (format "(x+1)^3 = ~a\n" (poly->string p-cubed)))

;; Derivative: 3x^2 + 6x + 3 = 3(x+1)^2
(define dp-cubed (poly-derivative p-cubed))
(display (format "d/dx[(x+1)^3] = ~a\n" (poly->string dp-cubed)))

;; Square-free part removes repeated factors
(define sf (poly-square-free p-cubed))
(display (format "square-free((x+1)^3) = ~a (removes multiplicity)\n" (poly->string sf)))

(newline)

;;; ====
;;; Example 5: Multivariate Polynomials
;;; ====

(display "--- Example 5: Multivariate Polynomials ---\n\n")

(define vars '(x y))
(define ord (make-ordering 'grlex vars))

;; Create variables
(define x (mpoly-var Q-field vars ord 'x))
(define y (mpoly-var Q-field vars ord 'y))
(define one (mpoly-one Q-field vars ord))

;; Build x^2 + xy + y^2
(define f (mpoly-add (mpoly-add (mpoly-power x 2)
                                (mpoly-mul x y))
                     (mpoly-power y 2)))
(display (format "f = ~a\n" (mpoly->string f)))

;; Build x - y
(define g (mpoly-sub x y))
(display (format "g = ~a\n" (mpoly->string g)))

;; Multiply: (x^2 + xy + y^2)(x - y) = x^3 - y^3
(define fg (mpoly-mul f g))
(display (format "f * g = ~a\n" (mpoly->string fg)))

;; Evaluate at x=2, y=1
(define result (mpoly-eval f '((x . 2) (y . 1))))
(display (format "f(2,1) = 4 + 2 + 1 = ~a\n" result))

(newline)

;;; ====
;;; Example 6: Monomial Orderings
;;; ====

(display "--- Example 6: Monomial Orderings ---\n\n")

;; Compare x^2y vs xy^2 in different orderings
(define m1 (make-monomial '((x . 2) (y . 1))))  ; x^2*y
(define m2 (make-monomial '((x . 1) (y . 2))))  ; x*y^2

(display "Comparing x^2*y vs x*y^2:\n")

(let ([cmp-lex (make-ordering 'lex vars)]
      [cmp-grlex (make-ordering 'grlex vars)]
      [cmp-grevlex (make-ordering 'grevlex vars)])
  (display (format "  lex:     x^2y ~a xy^2\n"
                   (if (> (cmp-lex m1 m2) 0) ">" "<")))
  (display (format "  grlex:   x^2y ~a xy^2\n"
                   (if (> (cmp-grlex m1 m2) 0) ">" "<")))
  (display (format "  grevlex: x^2y ~a xy^2\n"
                   (if (> (cmp-grevlex m1 m2) 0) ">" "<"))))

(newline)

;;; ====
;;; Example 7: Gröbner Bases
;;; ====

(display "--- Example 7: Gröbner Bases ---\n\n")

;; System: x^2 - y, xy - 1
;; These define the intersection of a parabola and hyperbola
(define vars2 '(x y))
(define ord2 (make-ordering 'grlex vars2))
(define x2 (mpoly-var Q-field vars2 ord2 'x))
(define y2 (mpoly-var Q-field vars2 ord2 'y))
(define one2 (mpoly-one Q-field vars2 ord2))

;; f1 = x^2 - y
(define f1 (mpoly-sub (mpoly-power x2 2) y2))
;; f2 = xy - 1
(define f2 (mpoly-sub (mpoly-mul x2 y2) one2))

(display (format "System:\n  f1 = ~a\n  f2 = ~a\n\n"
                 (mpoly->string f1) (mpoly->string f2)))

;; Compute Gröbner basis
(define G (buchberger (list f1 f2)))
(display "Gröbner basis:\n")
(for-each (lambda (g) (display (format "  ~a\n" (mpoly->string g)))) G)

;; Verify it's a Gröbner basis
(display (format "\nIs Gröbner basis? ~a\n" (is-groebner-basis? G)))

;; Test ideal membership: x^3 - 1 should be in <f1, f2>
;; because x^3 - 1 = x(x^2-y) + (xy-1) = x*f1 + f2 + xy - x^2 ...
;; Let's check with normal form
(define test-poly (mpoly-sub (mpoly-power x2 3) one2))
(display (format "\nTest: x^3 - 1 in ideal? ~a\n"
                 (ideal-membership? test-poly G)))

(newline)
(display "=== Examples Complete ===\n")
