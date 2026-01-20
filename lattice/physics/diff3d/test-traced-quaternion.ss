(load "core/test-framework.ss")
(load "lattice/physics/diff3d/traced-quaternion.ss")

(doc 'module 'test-traced-quaternion)
(doc 'description "Tests for traced-quaternion - Tests for differentiable quaternion operations")
(doc 'layer 'lattice)
(doc 'note "Run with: scheme --script lattice/physics/diff3d/test-traced-quaternion.ss")

(display "
====
         TRACED QUATERNION TESTS
====
")

;;; ====
;;; Test Utilities
;;; ====

(define (finite-diff f x epsilon)
  (/ (- (f (+ x epsilon)) (f (- x epsilon)))
     (* 2 epsilon)))

(define (list-update lst i f)
  (if (= i 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- i 1) f))))

(define (check-gradient-accuracy f args epsilon tolerance)
  (let* ([analytical (gradient (lambda xs (apply f xs)) args)]
         [numerical (map (lambda (i)
                                 (finite-diff
                                  (lambda (xi)
                                          (apply f (list-update args i (lambda (_) xi))))
                                  (list-ref args i)
                                  epsilon))
                         (iota (length args)))]
         [pairs (map cons analytical numerical)])
        (andmap (lambda (pair)
                        (< (abs (- (car pair) (cdr pair))) tolerance))
                pairs)))

(define (assert-gradient-correct f args)
  (assert-true (check-gradient-accuracy f args 1e-5 1e-4)))

;;; ====
;;; Traced Quaternion Tests
;;; ====

(test-group traced-quat-arithmetic
            
            (define-test traced-quat-add-gradient
              (assert-gradient-correct
               (lambda (aw ax ay az bw bx by bz)
                       (let* ([a (traced-quat aw ax ay az)]
                              [b (traced-quat bw bx by bz)]
                              [c (traced-quat-add a b)])
                             (traced-quat-w c)))
               (list 1 0 0 0 0.707 0.707 0 0)))
            
            (define-test traced-quat-scale-gradient
              (assert-gradient-correct
               (lambda (w x y z s)
                       (let* ([q (traced-quat w x y z)]
                              [qs (traced-quat-scale q s)])
                             (traced-add (traced-quat-w qs)
                                         (traced-add (traced-quat-x qs)
                                                     (traced-add (traced-quat-y qs)
                                                                 (traced-quat-z qs))))))
               (list 1 0 0 0 2))))

(test-group traced-quat-multiplication
            
            (define-test traced-quat-mul-w-gradient
              (assert-gradient-correct
               (lambda (aw ax ay az bw bx by bz)
                       (let* ([a (traced-quat aw ax ay az)]
                              [b (traced-quat bw bx by bz)]
                              [c (traced-quat-mul a b)])
                             (traced-quat-w c)))
               (list 0.707 0.707 0 0 0.707 0 0.707 0)))
            
            (define-test traced-quat-mul-x-gradient
              (assert-gradient-correct
               (lambda (aw ax ay az bw bx by bz)
                       (let* ([a (traced-quat aw ax ay az)]
                              [b (traced-quat bw bx by bz)]
                              [c (traced-quat-mul a b)])
                             (traced-quat-x c)))
               (list 0.707 0.707 0 0 0.707 0 0.707 0)))
            
            (define-test traced-quat-mul-y-gradient
              (assert-gradient-correct
               (lambda (aw ax ay az bw bx by bz)
                       (let* ([a (traced-quat aw ax ay az)]
                              [b (traced-quat bw bx by bz)]
                              [c (traced-quat-mul a b)])
                             (traced-quat-y c)))
               (list 0.707 0.707 0 0 0.707 0 0.707 0)))
            
            (define-test traced-quat-mul-z-gradient
              (assert-gradient-correct
               (lambda (aw ax ay az bw bx by bz)
                       (let* ([a (traced-quat aw ax ay az)]
                              [b (traced-quat bw bx by bz)]
                              [c (traced-quat-mul a b)])
                             (traced-quat-z c)))
               (list 0.707 0.707 0 0 0.707 0 0.707 0))))

(test-group traced-quat-magnitude
            
            (define-test traced-quat-magnitude-sq-gradient
              (assert-gradient-correct
               (lambda (w x y z)
                       (traced-quat-magnitude-sq (traced-quat w x y z)))
               (list 1 2 3 4)))
            
            (define-test traced-quat-magnitude-gradient
              (assert-gradient-correct
               (lambda (w x y z)
                       (traced-quat-magnitude (traced-quat w x y z)))
               (list 1 0 0 0)))  ; Unit quaternion
            
            (define-test traced-quat-smooth-magnitude-gradient
              (assert-gradient-correct
               (lambda (w x y z)
                       (traced-quat-smooth-magnitude (traced-quat w x y z) 1e-8))
               (list 0.5 0.5 0.5 0.5))))

(test-group traced-quat-normalize
            
            (define-test traced-quat-smooth-normalize-w-gradient
              (assert-gradient-correct
               (lambda (w x y z)
                       (traced-quat-w (traced-quat-smooth-normalize (traced-quat w x y z) 1e-8)))
               (list 1 1 0 0)))  ; Non-unit quaternion
            
            (define-test traced-quat-smooth-normalize-x-gradient
              (assert-gradient-correct
               (lambda (w x y z)
                       (traced-quat-x (traced-quat-smooth-normalize (traced-quat w x y z) 1e-8)))
               (list 1 1 0 0))))

(test-group traced-quat-conjugate
            
            (define-test traced-quat-conjugate-gradient
              (assert-gradient-correct
               (lambda (w x y z)
                       (let* ([q (traced-quat w x y z)]
                              [qc (traced-quat-conjugate q)])
                             (traced-add (traced-quat-w qc)
                                         (traced-add (traced-quat-x qc)
                                                     (traced-add (traced-quat-y qc)
                                                                 (traced-quat-z qc))))))
               (list 1 2 3 4))))

(test-group traced-quat-rotation
            
            (define-test traced-quat-rotate-vec3-x-gradient
              ;; Rotate (1,0,0) by identity quaternion - should stay (1,0,0)
              (assert-gradient-correct
               (lambda (qw qx qy qz vx vy vz)
                       (let* ([q (traced-quat qw qx qy qz)]
                              [v (traced-vec3 vx vy vz)]
                              [r (traced-quat-rotate-vec3 q v)])
                             (traced-vec3-x r)))
               (list 1 0 0 0 1 0 0)))
            
            (define-test traced-quat-rotate-vec3-by-90deg
              ;; 90-degree rotation around z-axis
              ;; q = cos(45) + sin(45)*k = (0.707, 0, 0, 0.707)
              ;; This rotates x-axis to y-axis
              (assert-gradient-correct
               (lambda (qw qx qy qz vx vy vz)
                       (let* ([q (traced-quat qw qx qy qz)]
                              [v (traced-vec3 vx vy vz)]
                              [r (traced-quat-rotate-vec3 q v)])
                             (traced-vec3-y r)))
               (list 0.7071067811865476 0 0 0.7071067811865476 1 0 0))))

(test-group traced-quat-integration
            
            (define-test traced-quat-derivative-gradient
              ;; Test derivative dq/dt = 0.5 * omega_quat * q
              (assert-gradient-correct
               (lambda (qw qx qy qz ox oy oz)
                       (let* ([q (traced-quat qw qx qy qz)]
                              [omega (traced-vec3 ox oy oz)]
                              [dq (traced-quat-derivative q omega)])
                             (traced-quat-w dq)))
               (list 1 0 0 0 0 0 1)))  ; Identity quat, rotation around z
            
            (define-test traced-quat-integrate-gradient
              ;; Test integration step
              (assert-gradient-correct
               (lambda (qw qx qy qz ox oy oz)
                       (let* ([q (traced-quat qw qx qy qz)]
                              [omega (traced-vec3 ox oy oz)]
                              [q-new (traced-quat-integrate q omega 0.01)])
                             (traced-quat-w q-new)))
               (list 1 0 0 0 0 0 1))))

(test-group traced-quat-values
            "Verify correctness of computed values"
            
            (define-test traced-quat-mul-identity-value
              ;; q * identity = q
              (let* ([q (quat 0.707 0.707 0 0)]  ; 90-deg rotation around x
                     [q-val (quat-mul q (quat-identity))])
                    (assert-true (quat-nearly-equal? q q-val 1e-6))))
            
            (define-test traced-quat-conjugate-value
              ;; conjugate of (w,x,y,z) is (w,-x,-y,-z)
              (let ([qc (quat-conjugate (quat 1 2 3 4))])
                   (and (assert-true (< (abs (- (quat-w qc) 1)) 1e-10))
                        (assert-true (< (abs (- (quat-x qc) -2)) 1e-10))
                        (assert-true (< (abs (- (quat-y qc) -3)) 1e-10))
                        (assert-true (< (abs (- (quat-z qc) -4)) 1e-10)))))
            
            (define-test traced-quat-normalize-value
              ;; |(1,0,0,0)| = 1, normalize should be identity
              (let ([qn (quat-normalize (quat 2 0 0 0))])
                   (assert-true (quat-nearly-equal? qn (quat-identity) 1e-10))))
            
            (define-test traced-quat-rotate-x-axis-around-z
              ;; Rotate x-axis by 90 degrees around z -> should get y-axis
              (let* ([q (quat-from-axis-angle (vec3 0 0 1) (/ 3.141592653589793 2))]
                     [v (quat-rotate-vec3 q (vec3 1 0 0))])
                    (and (assert-true (< (abs (vec3-x v)) 1e-6))
                         (assert-true (< (abs (- (vec3-y v) 1)) 1e-6))
                         (assert-true (< (abs (vec3-z v)) 1e-6))))))
