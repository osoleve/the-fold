;;; lattice/algebra/test-group-properties.ss — QuickCheck properties for group operations

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'group)

;;; ============================================================================
;;; Setup
;;; ============================================================================

(define G (Z 11))

(define gen-elt
  (gen-int-range 0 10))

(define gen-exp
  (gen-int-range -6 6))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group group-properties

  (define-property "cyclic group operation is associative"
    (gen-bind gen-elt
      (lambda (a)
        (gen-bind gen-elt
          (lambda (b)
            (gen-map (lambda (c) (list a b c)) gen-elt)))))
    (lambda (abc)
      (let ([a (car abc)]
            [b (cadr abc)]
            [c (caddr abc)])
        (= (group-compose G (group-compose G a b) c)
           (group-compose G a (group-compose G b c)))))
    'tests 280)

  (define-property "each element composed with its inverse is identity"
    gen-elt
    (lambda (a)
      (= (group-compose G a (group-inverse G a))
         (group-identity G)))
    'tests 240)

  (define-property "power law: a^(m+n) = a^m * a^n"
    (gen-bind gen-elt
      (lambda (a)
        (gen-bind gen-exp
          (lambda (m)
            (gen-map (lambda (n) (list a m n)) gen-exp)))))
    (lambda (amn)
      (let* ([a (car amn)]
             [m (cadr amn)]
             [n (caddr amn)]
             [lhs (group-power G a (+ m n))]
             [rhs (group-compose G (group-power G a m)
                                   (group-power G a n))])
        (= lhs rhs)))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
