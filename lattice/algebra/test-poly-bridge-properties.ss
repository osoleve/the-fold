;;; lattice/algebra/test-poly-bridge-properties.ss — QuickCheck properties for polynomial bridge

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(load "lattice/numeric/polynomial.ss")
(load "lattice/algebra/poly-bridge.ss")
(require 'quickcheck)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define gen-c
  (gen-int-range -6 6))

(define gen-coeff-list
  (gen-list gen-c))

(define (nonempty-coeffs xs)
  (if (null? xs) '(0) xs))

(define (num-degree p)
  (- (vector-length (cadr p)) 1))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group poly-bridge-properties

  (define-property "numeric->algebra->numeric preserves degree"
    gen-coeff-list
    (lambda (xs)
      (let* ([coeffs (nonempty-coeffs xs)]
             [p (make-numeric-from-ascending coeffs)]
             [back (algebra->numeric (numeric->algebra p))])
        (= (num-degree p) (num-degree back))))
    'tests 220)

  (define-property "bridge-simplify cancels shared linear factor"
    (gen-bind gen-c
      (lambda (r1)
        (gen-map (lambda (r2) (list r1 r2)) gen-c)))
    (lambda (roots)
      (let* ([r1 (car roots)]
             [r2 (cadr roots)]
             [num (make-numeric-from-roots (list r1 r2))]
             [den (make-numeric-from-roots (list r1))]
             [simp (bridge-simplify num den)]
             [num-s (car simp)]
             [den-s (cdr simp)])
        (and (= (num-degree num-s) 1)
             (= (num-degree den-s) 0))))
    'tests 220)

  (define-property "bridge-coprime? is symmetric"
    (gen-bind gen-c
      (lambda (r1)
        (gen-map (lambda (r2) (list r1 r2)) gen-c)))
    (lambda (roots)
      (let* ([r1 (car roots)]
             [r2 (cadr roots)]
             [p (make-numeric-from-roots (list r1))]
             [q (make-numeric-from-roots (list r2))])
        (equal? (bridge-coprime? p q)
                (bridge-coprime? q p))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
