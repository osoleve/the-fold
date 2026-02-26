;;; lattice/linalg/test-quaternion-properties.ss — QuickCheck properties for quaternion algebra

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'vec3)
(require 'quaternion)

;;; ============================================================================
;;; Helpers and generators
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define gen-axis
  (gen-map (lambda (k)
             (cond
               [(= k 0) (vec3 1 0 0)]
               [(= k 1) (vec3 0 1 0)]
               [else (vec3 0 0 1)]))
           (gen-int-range 0 2)))

(define gen-angle
  (gen-map (lambda (x) (/ x 100.0))
           (gen-int-range -314 314)))

(define gen-rotation-quat
  (gen-bind gen-axis
    (lambda (axis)
      (gen-map (lambda (angle)
                 (quat-from-axis-angle axis angle))
               gen-angle))))

(define gen-vec3-small
  (gen-bind (gen-int-range -10 10)
    (lambda (x)
      (gen-bind (gen-int-range -10 10)
        (lambda (y)
          (gen-map (lambda (z) (vec3 x y z))
                   (gen-int-range -10 10)))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group quaternion-properties

  (define-property "identity quaternion is left and right multiplicative identity"
    gen-rotation-quat
    (lambda (q)
      (and (quat-nearly-equal? (quat-mul (quat-identity) q) q 1e-9)
           (quat-nearly-equal? (quat-mul q (quat-identity)) q 1e-9)))
    'tests 220)

  (define-property "normalization is idempotent for non-zero quaternions"
    (gen-bind gen-rotation-quat
      (lambda (q)
        (gen-map (lambda (scale) (quat-scale q scale))
                 (gen-int-range 1 6))))
    (lambda (q)
      (let* ([n1 (quat-normalize q)]
             [n2 (quat-normalize n1)])
        (and (approx= (quat-magnitude n1) 1.0 1e-9)
             (quat-nearly-equal? n1 n2 1e-9))))
    'tests 180)

  (define-property "unit quaternion rotation preserves vec3 magnitude"
    (gen-bind gen-rotation-quat
      (lambda (q)
        (gen-map (lambda (v) (cons q v))
                 gen-vec3-small)))
    (lambda (pair)
      (let* ([q (car pair)]
             [v (cdr pair)]
             [rot (quat-rotate-vec3 q v)])
        (approx= (vec3-magnitude v)
                 (vec3-magnitude rot)
                 1e-8)))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
