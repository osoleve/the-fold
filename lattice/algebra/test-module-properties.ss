;;; lattice/algebra/test-module-properties.ss — QuickCheck properties for module operations

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'algebra/module)

;;; ============================================================================
;;; Setup
;;; ============================================================================

(define M (make-free-module-zn 7 2))

(define gen-z7
  (gen-int-range 0 6))

(define gen-vec2
  (gen-bind gen-z7
    (lambda (x)
      (gen-map (lambda (y) (list x y)) gen-z7))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group module-properties

  (define-property "module addition is commutative in Z7^2"
    (gen-bind gen-vec2
      (lambda (u)
        (gen-map (lambda (v) (list u v)) gen-vec2)))
    (lambda (uv)
      (let* ([u (car uv)]
             [v (cadr uv)]
             [lhs (module-add M u v)]
             [rhs (module-add M v u)])
        (module-equal? M lhs rhs)))
    'tests 240)

  (define-property "scalar multiplication distributes over vector addition"
    (gen-bind gen-z7
      (lambda (r)
        (gen-bind gen-vec2
          (lambda (u)
            (gen-map (lambda (v) (list r u v)) gen-vec2)))))
    (lambda (ruv)
      (let* ([r (car ruv)]
             [u (cadr ruv)]
             [v (caddr ruv)]
             [lhs (module-smul M r (module-add M u v))]
             [rhs (module-add M (module-smul M r u)
                                 (module-smul M r v))])
        (module-equal? M lhs rhs)))
    'tests 220)

  (define-property "zero behaves as additive identity"
    gen-vec2
    (lambda (v)
      (let ([z (module-zero M)])
        (and (module-equal? M (module-add M v z) v)
             (module-equal? M (module-add M z v) v))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
