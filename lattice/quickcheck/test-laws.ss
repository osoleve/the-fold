;;; lattice/quickcheck/test-laws.ss — Law verification for linalg types

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'qc-laws)
(require 'gen-linalg)

;;; ============================================================================
;;; Approximate equality for floating-point law checking
;;; ============================================================================

(define (make-approx= eps)
  (lambda (a b)
    (< (abs (- a b)) eps)))

(define (make-vec2-approx= eps)
  (lambda (a b)
    (and (< (abs (- (vec2-x a) (vec2-x b))) eps)
         (< (abs (- (vec2-y a) (vec2-y b))) eps))))

(define (make-vec3-approx= eps)
  (lambda (a b)
    (and (< (abs (- (vec3-x a) (vec3-x b))) eps)
         (< (abs (- (vec3-y a) (vec3-y b))) eps)
         (< (abs (- (vec3-z a) (vec3-z b))) eps))))

(define (make-mat-approx= eps)
  (lambda (a b)
    (matrix-approx-equal? a b eps)))

(define (make-quat-approx= eps)
  (lambda (a b)
    (quat-nearly-equal? a b eps)))

;; Tolerances — generous for float arithmetic through multiple operations
(define eps 1e-6)
(define ~= (make-approx= eps))
(define v2~= (make-vec2-approx= eps))
(define v3~= (make-vec3-approx= eps))
(define m~= (make-mat-approx= eps))
(define q~= (make-quat-approx= eps))

;;; ============================================================================
;;; Vec2: Additive abelian group + vector space
;;; ============================================================================

(test-group vec2-laws

  (define-test "Vec2 additive abelian group"
    (assert-laws "Vec2 add"
      (check-abelian-group-laws
        gen-vec2-small vec2-add (vec2-zero) vec2-neg v2~=)))

  (define-test "Vec2 vector space"
    (assert-laws "Vec2 vector space"
      (check-vector-space-laws
        gen-vec2-small gen-scalar
        vec2-add (vec2-zero) vec2-neg vec2-scale v2~=)))

  (define-test "Vec2 inner product"
    (assert-laws "Vec2 inner product"
      (check-inner-product-laws
        gen-vec2-small gen-scalar
        vec2-dot vec2-magnitude vec2-add vec2-scale (vec2-zero) ~=)))

  (define-test "Vec2 negation involution"
    (let ([result (check-involution-law gen-vec2 vec2-neg v2~=)])
      (assert-true (qc-success? result))))

  (define-test "Vec2 dot product commutativity"
    (let* ([gen2 (gen-bind gen-vec2-small
                   (lambda (u) (gen-map (lambda (v) (cons u v)) gen-vec2-small)))]
           [result (check-property gen2
                     (lambda (uv) (~= (vec2-dot (car uv) (cdr uv))
                                      (vec2-dot (cdr uv) (car uv))))
                     'tests 200)])
      (assert-true (qc-success? result))))
)

;;; ============================================================================
;;; Vec3: Additive abelian group + vector space + cross product
;;; ============================================================================

(test-group vec3-laws

  (define-test "Vec3 additive abelian group"
    (assert-laws "Vec3 add"
      (check-abelian-group-laws
        gen-vec3-small vec3-add (vec3-zero) vec3-neg v3~=)))

  (define-test "Vec3 vector space"
    (assert-laws "Vec3 vector space"
      (check-vector-space-laws
        gen-vec3-small gen-scalar
        vec3-add (vec3-zero) vec3-neg vec3-scale v3~=)))

  (define-test "Vec3 inner product"
    (assert-laws "Vec3 inner product"
      (check-inner-product-laws
        gen-vec3-small gen-scalar
        vec3-dot vec3-magnitude vec3-add vec3-scale (vec3-zero) ~=)))

  (define-test "Vec3 cross product anti-commutativity"
    (let ([result (check-anti-commutativity-law
                    gen-vec3-small vec3-cross vec3-neg v3~=)])
      (assert-true (qc-success? result))))

  (define-test "Vec3 cross product self is zero"
    (let ([result (check-property gen-vec3-small
                    (lambda (v) (v3~= (vec3-cross v v) (vec3-zero)))
                    'tests 200)])
      (assert-true (qc-success? result))))

  (define-test "Vec3 triple scalar cyclic"
    (let* ([gen3 (gen-bind gen-vec3-small (lambda (a)
                   (gen-bind gen-vec3-small (lambda (b)
                     (gen-map (lambda (c) (list a b c)) gen-vec3-small)))))]
           [result (check-property gen3
                     (lambda (abc)
                       (let ([a (car abc)] [b (cadr abc)] [c (caddr abc)])
                         (~= (vec3-triple-scalar a b c)
                             (vec3-triple-scalar b c a))))
                     'tests 200)])
      (assert-true (qc-success? result))))

  (define-test "Vec3 negation involution"
    (let ([result (check-involution-law gen-vec3 vec3-neg v3~=)])
      (assert-true (qc-success? result))))
)

;;; ============================================================================
;;; Matrix: Ring laws (2x2)
;;; ============================================================================

(test-group matrix-laws

  (define-test "Mat2x2 additive abelian group"
    (assert-laws "Mat2x2 add"
      (check-abelian-group-laws
        gen-mat2x2 matrix-add (zeros 2 2) matrix-negate m~=)))

  (define-test "Mat2x2 multiplicative monoid"
    (assert-laws "Mat2x2 mul monoid"
      (check-monoid-laws
        gen-mat2x2 matrix-mul (matrix-identity 2) m~=)))

  (define-test "Mat2x2 transpose involution"
    (let ([result (check-involution-law gen-mat2x2 matrix-transpose m~=)])
      (assert-true (qc-success? result))))

  (define-test "Mat2x2 transpose-of-product"
    (let ([result (check-transpose-product-law
                    gen-mat2x2 matrix-mul matrix-transpose m~=)])
      (assert-true (qc-success? result))))

  (define-test "Mat2x2 left distributivity"
    (let* ([gen3 (gen-bind gen-mat2x2 (lambda (a)
                   (gen-bind gen-mat2x2 (lambda (b)
                     (gen-map (lambda (c) (list a b c)) gen-mat2x2)))))]
           [result (check-property gen3
                     (lambda (abc)
                       (let ([a (car abc)] [b (cadr abc)] [c (caddr abc)])
                         (m~= (matrix-mul a (matrix-add b c))
                              (matrix-add (matrix-mul a b) (matrix-mul a c)))))
                     'tests 200)])
      (assert-true (qc-success? result))))

  (define-test "Mat2x2 scalar commutes with multiply"
    ;; (kA)B = k(AB) = A(kB)
    (let* ([gen-kab (gen-bind gen-scalar (lambda (k)
                      (gen-bind gen-mat2x2 (lambda (a)
                        (gen-map (lambda (b) (list k a b)) gen-mat2x2)))))]
           [result (check-property gen-kab
                     (lambda (kab)
                       (let ([k (car kab)] [a (cadr kab)] [b (caddr kab)])
                         (m~= (matrix-mul (matrix-scale k a) b)
                              (matrix-scale k (matrix-mul a b)))))
                     'tests 200)])
      (assert-true (qc-success? result))))
)

;;; ============================================================================
;;; Mat3x3: spot-check key laws at larger dimension
;;; ============================================================================

(test-group mat3x3-laws

  (define-test "Mat3x3 multiplicative identity"
    (let ([result (check-property gen-mat3x3
                    (lambda (m) (and (m~= (matrix-mul m (matrix-identity 3)) m)
                                     (m~= (matrix-mul (matrix-identity 3) m) m)))
                    'tests 100)])
      (assert-true (qc-success? result))))

  (define-test "Mat3x3 transpose involution"
    (let ([result (check-involution-law gen-mat3x3 matrix-transpose m~=)])
      (assert-true (qc-success? result))))

  (define-test "Mat3x3 multiply associativity"
    (let* ([gen3 (gen-bind gen-mat3x3 (lambda (a)
                   (gen-bind gen-mat3x3 (lambda (b)
                     (gen-map (lambda (c) (list a b c)) gen-mat3x3)))))]
           [result (check-property gen3
                     (lambda (abc)
                       (let ([a (car abc)] [b (cadr abc)] [c (caddr abc)])
                         (m~= (matrix-mul (matrix-mul a b) c)
                              (matrix-mul a (matrix-mul b c)))))
                     'tests 100)])
      (assert-true (qc-success? result))))
)

;;; ============================================================================
;;; Quaternion: Multiplicative group (unit quaternions)
;;; ============================================================================

(test-group quaternion-laws

  (define-test "Unit quaternion multiplicative group"
    (assert-laws "Quat group"
      (check-group-laws
        (gen-unit-quat) quat-mul (quat-identity) quat-inverse q~=)))

  (define-test "Quaternion double-cover — q and -q rotate identically"
    (let* ([gen-qv (gen-bind (gen-unit-quat)
                     (lambda (q) (gen-map (lambda (v) (cons q v)) gen-vec3-small)))]
           [result (check-property gen-qv
                     (lambda (qv)
                       (let ([q (car qv)] [v (cdr qv)])
                         (v3~= (quat-rotate-vec3 q v)
                                (quat-rotate-vec3 (quat-neg q) v))))
                     'tests 200)])
      (assert-true (qc-success? result))))

  (define-test "Quaternion conjugate is inverse for unit quaternions"
    (let ([result (check-property (gen-unit-quat)
                    (lambda (q)
                      (q~= (quat-mul q (quat-conjugate q))
                           (quat-identity)))
                    'tests 200)])
      (assert-true (qc-success? result))))

  (define-test "Quaternion rotation preserves magnitude"
    (let* ([gen-qv (gen-bind (gen-unit-quat)
                     (lambda (q) (gen-map (lambda (v) (cons q v)) gen-vec3-small)))]
           [result (check-property gen-qv
                     (lambda (qv)
                       (let ([q (car qv)] [v (cdr qv)])
                         (~= (vec3-magnitude (quat-rotate-vec3 q v))
                             (vec3-magnitude v))))
                     'tests 200)])
      (assert-true (qc-success? result))))
)

;;; ============================================================================
;;; Roundtrip / codec properties
;;; ============================================================================

(test-group roundtrip-laws

  (define-test "Vec2 list roundtrip"
    (let ([result (check-property gen-vec2
                    (lambda (v) (v2~= (list->vec2 (vec2->list v)) v))
                    'tests 200)])
      (assert-true (qc-success? result))))

  (define-test "Vec3 list roundtrip"
    (let ([result (check-property gen-vec3
                    (lambda (v) (v3~= (list->vec3 (vec3->list v)) v))
                    'tests 200)])
      (assert-true (qc-success? result))))

  (define-test "Matrix lists roundtrip"
    (let ([result (check-property gen-mat2x2
                    (lambda (m)
                      (m~= (matrix-from-lists (matrix->lists m)) m))
                    'tests 200)])
      (assert-true (qc-success? result))))

  (define-test "Quaternion list roundtrip"
    (let ([result (check-property gen-quat
                    (lambda (q)
                      (let ([lst (quat->list q)])
                        (q~= (list->quat lst) q)))
                    'tests 200)])
      (assert-true (qc-success? result))))
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
