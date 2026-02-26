;;; lattice/optics/test-profunctor-optics-properties.ss — QuickCheck properties for profunctor optics

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'profunctor-optics)

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group profunctor-optics-properties

  (define-property "p-lens-fst set then view returns set value"
    (gen-pair gen-int gen-int)
    (lambda (pair)
      (let* ([x (car pair)]
             [y (cdr pair)]
             [new-x (+ x 7)]
             [p (cons x y)]
             [p2 (p-set p-lens-fst new-x p)])
        (equal? new-x (p-view p-lens-fst p2))))
    'tests 220)

  (define-property "p-over on fst matches manual pair update"
    (gen-pair gen-int gen-int)
    (lambda (pair)
      (let* ([x (car pair)]
             [y (cdr pair)]
             [p (cons x y)]
             [lhs (p-over p-lens-fst (lambda (n) (* 2 n)) p)]
             [rhs (cons (* 2 x) y)])
        (equal? lhs rhs)))
    'tests 220)

  (define-property "p-prism-just review/preview roundtrip"
    gen-int
    (lambda (x)
      (let ([m (p-preview p-prism-just (p-review p-prism-just x))])
        (and (just? m)
             (equal? x (from-just m)))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
