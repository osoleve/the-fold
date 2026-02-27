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

(define gen-t
  (gen-map (lambda (n) (/ n 100.0))
           (gen-int-range 0 100)))

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

(define gen-quat-surface-args
  (gen-bind gen-rotation-quat
    (lambda (q)
      (gen-bind gen-rotation-quat
        (lambda (p)
          (gen-bind gen-vec3-small
            (lambda (v)
              (gen-bind gen-t
                (lambda (t)
                  (gen-map (lambda (dt-int)
                             (list q p v t (/ dt-int 100.0)))
                           (gen-int-range 1 20)))))))))))

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

  (define-property "quaternion surface helper API returns consistent results"
    gen-quat-surface-args
    (lambda (args)
      (let* ([q (car args)]
             [p (cadr args)]
             [v (caddr args)]
             [t (list-ref args 3)]
             [dt (list-ref args 4)]
             [q-list (quat->list q)]
             [q2 (list->quat q-list)]
             [q-id (quat-identity)]
             [q-zero (quat-zero)]
             [sum (quat-add q p)]
             [diff (quat-sub sum p)]
             [neg (quat-neg q)]
             [conj (quat-conjugate q)]
             [inv (quat-inverse q)]
             [prod-id (quat-mul q inv)]
             [dot (quat-dot q q)]
             [mag-sq (quat-magnitude-sq q)]
             [ang0 (quat-angle q q)]
             [ql (quat-lerp q p t)]
             [qn (quat-nlerp q p t)]
             [qs (quat-slerp q p t)]
             [rot (quat-rotate-vec3 q v)]
             [back (quat-rotate-vec3-inverse q rot)]
             [axis-angle (quat-to-axis-angle q)]
             [euler (quat-to-euler q)]
             [from-euler (apply quat-from-euler euler)]
             [rot-mat (quat-to-rotation-matrix q)]
             [from-rot (quat-from-rotation-matrix rot-mat)]
             [from-two (quat-from-two-vectors (vec3 1 0 0) (vec3 0 1 0))]
             [look (quat-look-at (vec3 0 0 1) (vec3 0 1 0))]
             [omega (vec3 0.1 -0.2 0.3)]
             [wvec (quat-angular-velocity q p dt)]
             [q-int (quat-integrate q omega dt)]
             [fwd (quat-forward q)]
             [right (quat-right q)]
             [up (quat-up q)])
        (and (quat? q)
             (quat-equal? q q2)
             (quat-identity? q-id)
             (quat-unit? q-id)
             (approx= (quat-magnitude-sq q-zero) 0.0 1e-12)
             (quat-nearly-equal? diff q 1e-9)
             (quat-nearly-equal? (quat-add q neg) q-zero 1e-8)
             (approx= dot mag-sq 1e-9)
             (approx= ang0 0.0 1e-6)
             (quat-nearly-equal? prod-id q-id 1e-7)
             (quat? ql)
             (quat? qn)
             (quat? qs)
             (vec3-nearly-equal? back v 1e-7)
             (= (length axis-angle) 2)
             (= (length euler) 3)
             (quat? from-euler)
             (quat? from-rot)
             (quat? from-two)
             (quat? look)
             (vec3? wvec)
             (quat? q-int)
             (vec3? fwd)
             (vec3? right)
             (vec3? up)
             (approx= (quat-scalar q) (quat-w q) 1e-12)
             (vec3-equal? (quat-vector q) (vec3 (quat-x q) (quat-y q) (quat-z q)))
             (quat-equal? conj (quat-conjugate q)))))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
