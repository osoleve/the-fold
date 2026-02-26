;;; lattice/algebra/test-ring-field-properties.ss — QuickCheck properties for ring/field operations

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'field)

;;; ============================================================================
;;; Setup
;;; ============================================================================

(define R (make-ring-zn 11))
(define F (make-field-zp 11))

(define gen-z11
  (gen-int-range 0 10))

(define gen-z11-nonzero
  (gen-int-range 1 10))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group ring-field-properties

  (define-property "ring-add in Z11 is commutative"
    (gen-bind gen-z11
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-z11)))
    (lambda (ab)
      (let ([a (car ab)]
            [b (cdr ab)])
        (= (ring-add R a b)
           (ring-add R b a))))
    'tests 260)

  (define-property "ring-sub agrees with add of negation"
    (gen-bind gen-z11
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-z11)))
    (lambda (ab)
      (let ([a (car ab)]
            [b (cdr ab)])
        (= (ring-sub R a b)
           (ring-add R a (ring-neg R b)))))
    'tests 240)

  (define-property "field inverses satisfy a * inv(a) = 1 in GF(11)"
    gen-z11-nonzero
    (lambda (a)
      (= 1 (field-mul F a (field-inv F a))))
    'tests 240)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
