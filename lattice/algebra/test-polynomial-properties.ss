;;; lattice/algebra/test-polynomial-properties.ss — QuickCheck properties for univariate polynomials

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'polynomial)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define gen-c
  (gen-int-range -6 6))

(define gen-coeff-list
  (gen-list gen-c))

(define gen-x
  (gen-int-range -5 5))

(define (coeffs->poly xs)
  (make-polynomial Q-field (if (null? xs) '(0) xs)))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group polynomial-properties

  (define-property "poly-add is commutative"
    (gen-bind gen-coeff-list
      (lambda (a)
        (gen-map (lambda (b) (list a b)) gen-coeff-list)))
    (lambda (ab)
      (let* ([p (coeffs->poly (car ab))]
             [q (coeffs->poly (cadr ab))])
        (poly-equal? (poly-add p q)
                     (poly-add q p))))
    'tests 240)

  (define-property "evaluation is additive"
    (gen-bind gen-coeff-list
      (lambda (a)
        (gen-bind gen-coeff-list
          (lambda (b)
            (gen-map (lambda (x) (list a b x)) gen-x)))))
    (lambda (abx)
      (let* ([p (coeffs->poly (car abx))]
             [q (coeffs->poly (cadr abx))]
             [x (caddr abx)])
        (= (poly-eval (poly-add p q) x)
           (+ (poly-eval p x)
              (poly-eval q x)))))
    'tests 220)

  (define-property "multiplication distributes over addition"
    (gen-bind gen-coeff-list
      (lambda (a)
        (gen-bind gen-coeff-list
          (lambda (b)
            (gen-map (lambda (c) (list a b c)) gen-coeff-list)))))
    (lambda (abc)
      (let* ([p (coeffs->poly (car abc))]
             [q (coeffs->poly (cadr abc))]
             [r (coeffs->poly (caddr abc))])
        (poly-equal? (poly-mul p (poly-add q r))
                     (poly-add (poly-mul p q)
                               (poly-mul p r)))))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
