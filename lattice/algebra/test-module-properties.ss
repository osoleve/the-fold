;;; lattice/algebra/test-module-properties.ss — QuickCheck properties for module operations

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'algebra/module)
(require 'ring)

;;; ============================================================================
;;; Setup
;;; ============================================================================

(define M (make-free-module-zn 7 2))

;; Small explicit finite module over Z2 for submodule/hom/tensor coverage.
(define R2 (make-ring-zn 2))
(define E2 (list (list 0) (list 1)))

(define (v2-add a b)
  (list (modulo (+ (car a) (car b)) 2)))

(define (v2-neg a)
  (list (modulo (- (car a)) 2)))

(define (v2-smul r a)
  (list (modulo (* r (car a)) 2)))

(define (v2-eq a b)
  (= (car a) (car b)))

(define M2 (make-module R2 E2 v2-add (list 0) v2-neg v2-smul v2-eq))
(define SM2 (make-submodule M2 (list (list 1))))
(define ID2 (make-module-hom M2 M2 (lambda (v) v)))

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

  (define-property "module-theory constructors, accessors, and derived structures are coherent on a finite model"
    (gen-pure #t)
    (lambda (_)
      (let* ([Q2 (make-quotient-module M2 SM2)]
             [PI2 (canonical-projection M2 SM2)]
             [T2 (make-tensor-product M2 M2)]
             [k2 (module-hom-kernel ID2)]
             [im2 (module-hom-image ID2)]
             [comp2 (module-hom-compose ID2 ID2)])
        (and (module? (make-free-module-z 1))
             (module? M2)
             (equal? (module-ring M2) R2)
             (equal? (module-elements M2) E2)
             (procedure? (module-add-op M2))
             (equal? ((module-add-op M2) (list 1) (list 1)) (list 0))
             (procedure? (module-neg-fn M2))
             (equal? ((module-neg-fn M2) (list 1)) (list 1))
             (procedure? (module-scalar-mul M2))
             (procedure? (module-equal-fn M2))
             (equal? (module-neg M2 (list 1)) (list 1))
             (equal? (module-sub M2 (list 1) (list 1)) (list 0))
             (equal? (module-sum M2 (list (list 1) (list 1))) (list 0))
             (equal? (module-linear-combination M2 (list 1 1) (list (list 1) (list 1))) (list 0))
             (submodule? SM2)
             (equal? (submodule-parent SM2) M2)
             (equal? (submodule-generators SM2) (list (list 1)))
             (is-in-submodule? SM2 (list 1))
             (module-hom? ID2)
             (equal? (module-hom-source ID2) M2)
             (equal? (module-hom-target ID2) M2)
             (procedure? (module-hom-phi ID2))
             (equal? (module-hom-apply ID2 (list 1)) (list 1))
             (is-valid-module-hom? ID2)
             (submodule? k2)
             (submodule? im2)
             (module-hom? comp2)
             (module? Q2)
             (module-hom? PI2)
             (module? T2)
             (pair? (tensor M2 M2 (list 1) (list 1))))))
    'tests 8)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
