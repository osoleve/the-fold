;;; lattice/quickcheck/gen-linalg.ss — Generators for linear algebra types
;;; @module gen-linalg
;;; @requires prelude qc-generators vec2 vec3 matrix quaternion

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'qc-generators)
(require 'vec2)
(require 'vec3)
(require 'matrix)
(require 'quaternion)

(doc 'module 'gen-linalg)
(doc 'description "QuickCheck generators for linear algebra types: Vec2, Vec3, Matrix, Quaternion.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Component generators
;;; ============================================================================

(doc 'section 'components)

(doc gen-component 'export #t)
(doc gen-component 'description "Size-scaled float component in [-size, size], biased toward small integers")
(define gen-component
  (gen-frequency
    (list (cons 3 (gen-sized (lambda (size)
                    (gen-map (lambda (n) (exact->inexact n))
                             (gen-int-range (- 0 (max 1 size)) (max 1 size))))))
          (cons 1 (gen-sized (lambda (size)
                    (gen-map (lambda (u) (* (- (* u 2) 1) (max 1 size)))
                             gen-float)))))))

(doc gen-small-component 'export #t)
(doc gen-small-component 'description "Small float in [-10, 10] for tests where overflow matters")
(define gen-small-component
  (gen-map (lambda (u) (- (* u 20.0) 10.0)) gen-float))

(doc gen-nonzero-component 'export #t)
(doc gen-nonzero-component 'description "Component guaranteed nonzero — for division/normalization tests")
(define gen-nonzero-component
  (gen-bind gen-component
            (lambda (x) (if (= x 0) (gen-pure 1.0) (gen-pure x)))))

(doc gen-positive-component 'export #t)
(doc gen-positive-component 'description "Positive component in (0, size]")
(define gen-positive-component
  (gen-sized (lambda (size)
    (gen-map (lambda (u) (+ 0.1 (* u (max 1 size))))
             gen-float))))

;;; ============================================================================
;;; Vec2 generators
;;; ============================================================================

(doc 'section 'vec2-generators)

(doc gen-vec2 'export #t)
(doc gen-vec2 'description "Random Vec2 with size-scaled components")
(define gen-vec2
  (gen-bind gen-component
            (lambda (x)
              (gen-map (lambda (y) (vec2 x y))
                       gen-component))))

(doc gen-vec2-small 'export #t)
(doc gen-vec2-small 'description "Vec2 with small components [-10, 10]")
(define gen-vec2-small
  (gen-bind gen-small-component
            (lambda (x)
              (gen-map (lambda (y) (vec2 x y))
                       gen-small-component))))

(define (gen-vec2-nonzero)
  (doc 'export #t)
  (doc 'description "Vec2 guaranteed nonzero")
  (gen-bind gen-vec2
            (lambda (v)
              (if (vec2-zero? v)
                  (gen-pure (vec2 1.0 0.0))
                  (gen-pure v)))))

;;; ============================================================================
;;; Vec3 generators
;;; ============================================================================

(doc 'section 'vec3-generators)

(doc gen-vec3 'export #t)
(doc gen-vec3 'description "Random Vec3 with size-scaled components")
(define gen-vec3
  (gen-bind gen-component
            (lambda (x)
              (gen-bind gen-component
                        (lambda (y)
                          (gen-map (lambda (z) (vec3 x y z))
                                   gen-component))))))

(doc gen-vec3-small 'export #t)
(doc gen-vec3-small 'description "Vec3 with small components [-10, 10]")
(define gen-vec3-small
  (gen-bind gen-small-component
            (lambda (x)
              (gen-bind gen-small-component
                        (lambda (y)
                          (gen-map (lambda (z) (vec3 x y z))
                                   gen-small-component))))))

(define (gen-vec3-nonzero)
  (doc 'export #t)
  (doc 'description "Vec3 guaranteed nonzero")
  (gen-bind gen-vec3
            (lambda (v)
              (if (vec3-zero? v)
                  (gen-pure (vec3 1.0 0.0 0.0))
                  (gen-pure v)))))

(define (gen-vec3-unit)
  (doc 'export #t)
  (doc 'description "Unit Vec3 (normalized)")
  (gen-map (lambda (v) (if (vec3-zero? v) (vec3 1.0 0.0 0.0) (vec3-normalize v)))
           gen-vec3))

;;; ============================================================================
;;; Scalar generator (for vector space laws)
;;; ============================================================================

(doc 'section 'scalar-generators)

(doc gen-scalar 'export #t)
(doc gen-scalar 'description "Scalar value for vector space tests")
(define gen-scalar gen-small-component)

;;; ============================================================================
;;; Matrix generators
;;; ============================================================================

(doc 'section 'matrix-generators)

(define (gen-matrix-of rows cols)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat (Gen Matrix)))
  (doc 'description "Matrix of fixed dimensions with size-scaled components")
  (gen-map (lambda (data)
             (matrix-from-vec rows cols (list->vector data)))
           (gen-list-of (* rows cols) gen-component)))

(define (gen-square-matrix n)
  (doc 'export #t)
  (doc 'type '(-> Nat (Gen Matrix)))
  (doc 'description "Square NxN matrix")
  (gen-matrix-of n n))

(doc gen-mat2x2 'export #t)
(doc gen-mat2x2 'description "Random 2x2 matrix")
(define gen-mat2x2 (gen-square-matrix 2))

(doc gen-mat3x3 'export #t)
(doc gen-mat3x3 'description "Random 3x3 matrix")
(define gen-mat3x3 (gen-square-matrix 3))

(define (gen-small-matrix)
  (doc 'export #t)
  (doc 'description "Small square matrix with random dimension [1, 4]")
  (gen-bind (gen-int-range 1 4)
            (lambda (n) (gen-square-matrix n))))

;;; ============================================================================
;;; Quaternion generators
;;; ============================================================================

(doc 'section 'quaternion-generators)

(doc gen-quat 'export #t)
(doc gen-quat 'description "Random quaternion (not necessarily unit)")
(define gen-quat
  (gen-bind gen-component
            (lambda (w)
              (gen-bind gen-component
                        (lambda (x)
                          (gen-bind gen-component
                                    (lambda (y)
                                      (gen-map (lambda (z) (quat w x y z))
                                               gen-component))))))))

(define (gen-unit-quat)
  (doc 'export #t)
  (doc 'description "Unit quaternion (valid rotation)")
  (gen-map (lambda (q)
             (let ([m (quat-magnitude q)])
               (if (< m 1e-10)
                   (quat-identity)
                   (quat-normalize q))))
           gen-quat))
