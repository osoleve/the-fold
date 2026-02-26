;;; lattice/algebra/test-tropical-properties.ss — QuickCheck properties for tropical semiring operations

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'tropical)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-w
  (gen-int-range -20 20))

(define gen-exp
  (gen-int-range 0 8))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group tropical-properties

  (define-property "min-plus tropical-add is commutative"
    (gen-bind gen-w
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-w)))
    (lambda (ab)
      (let ([a (car ab)]
            [b (cdr ab)])
        (= (tropical-add min-plus-semiring a b)
           (tropical-add min-plus-semiring b a))))
    'tests 260)

  (define-property "min-plus multiplication distributes over addition"
    (gen-bind gen-w
      (lambda (a)
        (gen-bind gen-w
          (lambda (b)
            (gen-map (lambda (c) (list a b c)) gen-w)))))
    (lambda (abc)
      (let ([a (car abc)]
            [b (cadr abc)]
            [c (caddr abc)]
            [sr min-plus-semiring])
        (= (tropical-mul sr a (tropical-add sr b c))
           (tropical-add sr (tropical-mul sr a b)
                            (tropical-mul sr a c)))))
    'tests 220)

  (define-property "tropical-power in min-plus equals repeated addition"
    (gen-bind gen-w
      (lambda (x)
        (gen-map (lambda (n) (list x n)) gen-exp)))
    (lambda (xn)
      (let* ([x (car xn)]
             [n (cadr xn)]
             [lhs (tropical-power min-plus-semiring x n)]
             [rhs (* x n)])
        (= lhs rhs)))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
