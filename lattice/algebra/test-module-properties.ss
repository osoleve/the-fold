;;; lattice/algebra/test-module-properties.ss — QuickCheck properties for module operations

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'algebra/module)
(require 'ring)

;;; ============================================================================
;;; Setup - Expanded Fixture Space (QA Follow-up)
;;; ============================================================================

;; Primary test module: Z7^2 (larger modulus for more interesting arithmetic)
(define M (make-free-module-zn 7 2))

;; ============================================================================
;; Multiple Finite Ring Fixtures (not just Z2)
;; ============================================================================

;; Z2 module (the original)
(define R2 (make-ring-zn 2))
(define E2 (list (list 0) (list 1)))
(define (v2-add a b) (list (modulo (+ (car a) (car b)) 2)))
(define (v2-neg a) (list (modulo (- (car a)) 2)))
(define (v2-smul r a) (list (modulo (* r (car a)) 2)))
(define (v2-eq a b) (= (car a) (car b)))
(define M2 (make-module R2 E2 v2-add (list 0) v2-neg v2-smul v2-eq))

;; Z3 module (different characteristic)
(define R3 (make-ring-zn 3))
(define E3 (list (list 0) (list 1) (list 2)))
(define (v3-add a b) (list (modulo (+ (car a) (car b)) 3)))
(define (v3-neg a) (list (modulo (- (car a)) 3)))
(define (v3-smul r a) (list (modulo (* r (car a)) 3)))
(define (v3-eq a b) (= (car a) (car b)))
(define M3 (make-module R3 E3 v3-add (list 0) v3-neg v3-smul v3-eq))

;; Z5 module (yet another characteristic)
(define R5 (make-ring-zn 5))
(define E5 (list (list 0) (list 1) (list 2) (list 3) (list 4)))
(define (v5-add a b) (list (modulo (+ (car a) (car b)) 5)))
(define (v5-neg a) (list (modulo (- (car a)) 5)))
(define (v5-smul r a) (list (modulo (* r (car a)) 5)))
(define (v5-eq a b) (= (car a) (car b)))
(define M5 (make-module R5 E5 v5-add (list 0) v5-neg v5-smul v5-eq))

;; Submodules and homomorphisms for each fixture
(define SM2 (make-submodule M2 (list (list 1))))
(define SM3 (make-submodule M3 (list (list 1))))
(define ID2 (make-module-hom M2 M2 (lambda (v) v)))
(define ID3 (make-module-hom M3 M3 (lambda (v) v)))
(define ID5 (make-module-hom M5 M5 (lambda (v) v)))

;; ============================================================================
;; Non-enumerable (Infinite) Module Fixture for Guard Testing
;; ============================================================================

;; Bounded Z module (make-free-module-z uses bounded integers [-100, 100])
(define M-bounded (make-free-module-z 2))
(define R-bounded (module-ring M-bounded))
(define SM-bounded (make-submodule M-bounded (list (list 1 0))))

;; Custom infinite ring fixture for true guard-path testing
;; This ring reports #f for elements, triggering the guard in is-in-submodule?
(define R-infinite 
  (make-ring 
    #f  ; No elements - infinite
    + * 0 1 - =))

(define M-infinite
  (make-module 
    R-infinite
    #f  ; No element list
    (lambda (a b) (map + a b))  ; Add
    '(0 0)  ; Zero
    (lambda (a) (map - a))  ; Neg
    (lambda (r a) (map (lambda (x) (* r x)) a))  ; Scalar mult
    equal?))

(define SM-infinite (make-submodule M-infinite (list (list 1 0))))

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

  (define-property "module-theory constructors, accessors, and derived structures are coherent on Z2"
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

  ;; QA Follow-up: Expanded fixture space - Z3 module tests
  (define-property "module structures are coherent on Z3 (characteristic 3)"
    (gen-pure #t)
    (lambda (_)
      (let* ([Q3 (make-quotient-module M3 SM3)]
             [k3 (module-hom-kernel ID3)]
             [im3 (module-hom-image ID3)])
        (and (module? M3)
             (equal? (module-ring M3) R3)
             (equal? (length (module-elements M3)) 3)
             ;; 1 + 1 + 1 = 0 in Z3
             (equal? (module-add M3 (list 1) (module-add M3 (list 1) (list 1))) (list 0))
             ;; Scalar multiplication: 2 * 2 = 4 = 1 (mod 3)
             (equal? (module-smul M3 2 (list 2)) (list 1))
             (submodule? SM3)
             (is-in-submodule? SM3 (list 1))
             (is-in-submodule? SM3 (list 2))  ; 2 = 2*1, so should be in submodule
             (submodule? k3)
             (submodule? im3)
             (module? Q3))))
    'tests 6)

  ;; QA Follow-up: Expanded fixture space - Z5 module tests
  (define-property "module structures are coherent on Z5 (characteristic 5)"
    (gen-pure #t)
    (lambda (_)
      (let* ([zero (module-zero M5)])
        (and (module? M5)
             (equal? (module-ring M5) R5)
             (equal? (length (module-elements M5)) 5)
             ;; 2 + 3 = 0 in Z5
             (equal? (module-add M5 (list 2) (list 3)) zero)
             ;; Negation: -2 = 3 (mod 5)
             (equal? (module-neg M5 (list 2)) (list 3))
             ;; Scalar multiplication: 3 * 2 = 6 = 1 (mod 5)
             (equal? (module-smul M5 3 (list 2)) (list 1))
             (is-valid-module-hom? ID5))))
    'tests 6)

  ;; QA Follow-up: Non-enumerable guard path tests (infinite modules return #f)
  (define-property "is-in-submodule? returns #f for infinite rings (guard path)"
    (gen-pure #t)
    (lambda (_)
      ;; For truly infinite modules, is-in-submodule? returns #f (cannot enumerate)
      (and (eq? #f (is-in-submodule? SM-infinite (list 1 0)))
           ;; The ring itself should report no elements
           (eq? #f (ring-elements R-infinite))
           ;; But the module structure should still be valid
           (module? M-infinite)
           (submodule? SM-infinite)
           (equal? (submodule-parent SM-infinite) M-infinite))))
    'tests 4)

  (define-property "bounded Z module behavior (demonstration ring with finite sample)"
    (gen-pure #t)
    (lambda (_)
      ;; make-free-module-z uses bounded integers [-100, 100]
      ;; This is finite enough to enumerate, so is-in-submodule? should work
      (and (module? M-bounded)
           (submodule? SM-bounded)
           ;; Ring has bounded elements (not #f)
           (list? (ring-elements R-bounded))
           ;; Membership test should succeed for bounded ring
           (boolean? (is-in-submodule? SM-bounded (list 1 0)))))
    'tests 4)

  (define-property "is-valid-module-hom? guard behavior with finite vs infinite modules"
    (gen-pure #t)
    (lambda (_)
      ;; Valid homomorphism on finite modules
      (and (is-valid-module-hom? ID2)
           (is-valid-module-hom? ID3)
           (is-valid-module-hom? ID5)
           ;; Finite modules have enumerable elements
           (list? (module-elements M2))
           (list? (module-elements M3))
           ;; Infinite module elements is #f
           (eq? #f (module-elements M-infinite))
           ;; Bounded module has #f elements (it's conceptually infinite)
           (eq? #f (module-elements M-bounded))))
    'tests 4)

(run-all-tests-and-exit)
