;;; core/diff-physics-3d/test-traced-vec3.ss --- Tests for traced-vec3
;;;
;;; Tests for differentiable 3D vector operations.
;;; Run with: scheme --script core/diff-physics-3d/test-traced-vec3.ss

(load "core/test-framework.ss")
(load "core/diff-physics-3d/traced-vec3.ss")

(display "
==============================================================
         TRACED VEC3 TESTS
==============================================================
")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

;;; finite-diff : (Number → Number) × Number × Number → Number
(define (finite-diff f x epsilon)
  (/ (- (f (+ x epsilon)) (f (- x epsilon)))
     (* 2 epsilon)))

;;; list-update : (List α) × Nat × (α → α) → (List α)
(define (list-update lst i f)
  (if (= i 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- i 1) f))))

;;; check-gradient-accuracy : ((List Number) → Number) × (List Number) × Number × Number → Boolean
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

;;; assert-gradient-correct : ((List Number) → Number) × (List Number) → Unit
(define (assert-gradient-correct f args)
  (assert-true (check-gradient-accuracy f args 1e-5 1e-4)))

;;; ============================================================
;;; Traced Vec3 Tests
;;; ============================================================

(test-group traced-vec3-construction
            
            (define-test traced-vec3-add-gradient
              (assert-gradient-correct
               (lambda (ax ay az bx by bz)
                       (let* ([a (traced-vec3 ax ay az)]
                              [b (traced-vec3 bx by bz)]
                              [c (traced-vec3-add a b)])
                             (traced-vec3-x c)))
               (list 1 2 3 4 5 6)))
            
            (define-test traced-vec3-sub-gradient
              (assert-gradient-correct
               (lambda (ax ay az bx by bz)
                       (let* ([a (traced-vec3 ax ay az)]
                              [b (traced-vec3 bx by bz)]
                              [c (traced-vec3-sub a b)])
                             (traced-vec3-y c)))
               (list 1 2 3 4 5 6))))

(test-group traced-vec3-products
            
            (define-test traced-vec3-dot-gradient
              (assert-gradient-correct
               (lambda (ax ay az bx by bz)
                       (let* ([a (traced-vec3 ax ay az)]
                              [b (traced-vec3 bx by bz)])
                             (traced-vec3-dot a b)))
               (list 1 2 3 4 5 6)))
            
            (define-test traced-vec3-cross-x-gradient
              (assert-gradient-correct
               (lambda (ax ay az bx by bz)
                       (let* ([a (traced-vec3 ax ay az)]
                              [b (traced-vec3 bx by bz)]
                              [c (traced-vec3-cross a b)])
                             (traced-vec3-x c)))
               (list 1 2 3 4 5 6)))
            
            (define-test traced-vec3-cross-y-gradient
              (assert-gradient-correct
               (lambda (ax ay az bx by bz)
                       (let* ([a (traced-vec3 ax ay az)]
                              [b (traced-vec3 bx by bz)]
                              [c (traced-vec3-cross a b)])
                             (traced-vec3-y c)))
               (list 1 2 3 4 5 6)))
            
            (define-test traced-vec3-cross-z-gradient
              (assert-gradient-correct
               (lambda (ax ay az bx by bz)
                       (let* ([a (traced-vec3 ax ay az)]
                              [b (traced-vec3 bx by bz)]
                              [c (traced-vec3-cross a b)])
                             (traced-vec3-z c)))
               (list 1 2 3 4 5 6))))

(test-group traced-vec3-magnitude
            
            (define-test traced-vec3-magnitude-sq-gradient
              (assert-gradient-correct
               (lambda (x y z)
                       (let ([v (traced-vec3 x y z)])
                            (traced-vec3-magnitude-sq v)))
               (list 1 2 3)))
            
            (define-test traced-vec3-magnitude-gradient
              (assert-gradient-correct
               (lambda (x y z)
                       (let ([v (traced-vec3 x y z)])
                            (traced-vec3-magnitude v)))
               (list 3 4 0)))  ; |v| = 5, non-zero
            
            (define-test traced-vec3-smooth-magnitude-gradient
              (assert-gradient-correct
               (lambda (x y z)
                       (let ([v (traced-vec3 x y z)])
                            (traced-vec3-smooth-magnitude v 1e-8)))
               (list 3 4 0)))
            
            (define-test traced-vec3-smooth-magnitude-near-zero-gradient
              ;; Test gradient stability near zero
              (assert-gradient-correct
               (lambda (x y z)
                       (let ([v (traced-vec3 x y z)])
                            (traced-vec3-smooth-magnitude v 0.1)))
               (list 0.01 0.01 0.01))))

(test-group traced-vec3-scale
            
            (define-test traced-vec3-scale-gradient
              (assert-gradient-correct
               (lambda (x y z s)
                       (let* ([v (traced-vec3 x y z)]
                              [w (traced-vec3-scale v s)])
                             (traced-add (traced-vec3-x w)
                                         (traced-add (traced-vec3-y w)
                                                     (traced-vec3-z w)))))
               (list 1 2 3 2))))

(test-group traced-vec3-normalize
            
            (define-test traced-vec3-smooth-normalize-gradient
              (assert-gradient-correct
               (lambda (x y z)
                       (let* ([v (traced-vec3 x y z)]
                              [n (traced-vec3-smooth-normalize v 1e-8)])
                             (traced-vec3-x n)))
               (list 3 4 0))))

(test-group traced-vec3-geometry
            
            (define-test traced-vec3-reflect-gradient
              (assert-gradient-correct
               (lambda (vx vy vz nx ny nz)
                       (let* ([v (traced-vec3 vx vy vz)]
                              [n (traced-vec3 nx ny nz)]
                              [r (traced-vec3-reflect v n)])
                             (traced-vec3-x r)))
               (list 1 1 0 0 1 0)))  ; Reflect (1,1,0) over y-axis normal
            
            (define-test traced-vec3-project-gradient
              (assert-gradient-correct
               (lambda (ax ay az bx by bz)
                       (let* ([a (traced-vec3 ax ay az)]
                              [b (traced-vec3 bx by bz)]
                              [p (traced-vec3-project a b)])
                             (traced-vec3-x p)))
               (list 3 4 0 1 0 0)))  ; Project (3,4,0) onto x-axis
            
            (define-test traced-vec3-lerp-gradient
              (assert-gradient-correct
               (lambda (ax ay az bx by bz t)
                       (let* ([a (traced-vec3 ax ay az)]
                              [b (traced-vec3 bx by bz)]
                              [c (traced-vec3-lerp a b t)])
                             (traced-add (traced-vec3-x c)
                                         (traced-add (traced-vec3-y c)
                                                     (traced-vec3-z c)))))
               (list 0 0 0 10 20 30 0.5))))

(test-group traced-vec3-values
            "Verify correctness of computed values using non-traced vec3"
            
            (define-test traced-vec3-dot-value
              ;; (1,2,3)·(4,5,6) = 4+10+18 = 32
              (let ([result (vec3-dot (vec3 1 2 3) (vec3 4 5 6))])
                   (assert-true (< (abs (- result 32)) 1e-10))))
            
            (define-test traced-vec3-cross-value
              ;; (1,0,0)×(0,1,0) = (0,0,1)
              (let ([c (vec3-cross (vec3 1 0 0) (vec3 0 1 0))])
                   (and (assert-true (< (abs (vec3-x c)) 1e-10))
                        (assert-true (< (abs (vec3-y c)) 1e-10))
                        (assert-true (< (abs (- (vec3-z c) 1)) 1e-10)))))
            
            (define-test traced-vec3-magnitude-value
              ;; |(3,4,0)| = 5
              (let ([result (vec3-magnitude (vec3 3 4 0))])
                   (assert-true (< (abs (- result 5)) 1e-10))))
            
            (define-test traced-vec3-cross-general-value
              ;; (1,2,3)×(4,5,6) = (2*6-3*5, 3*4-1*6, 1*5-2*4) = (-3, 6, -3)
              (let ([c (vec3-cross (vec3 1 2 3) (vec3 4 5 6))])
                   (and (assert-true (< (abs (- (vec3-x c) -3)) 1e-10))
                        (assert-true (< (abs (- (vec3-y c) 6)) 1e-10))
                        (assert-true (< (abs (- (vec3-z c) -3)) 1e-10))))))
