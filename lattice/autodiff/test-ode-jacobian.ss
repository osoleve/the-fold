(load "core/testing/test-framework.ss")
(load "lattice/autodiff/ode-jacobian.ss")

;;; ====
;;; Exact Jacobian — Simple Functions
;;; ====

(test-group "autodiff-jacobian"

  ;; Identity: f(x) = x, J = I
  (define-test "identity has identity Jacobian"
    (let ([J (autodiff-jacobian (lambda (y) y) '(1.0 2.0) 2)])
      (assert-true (< (abs (- (matrix-ref J 0 0) 1.0)) 1e-10))
      (assert-true (< (abs (matrix-ref J 0 1)) 1e-10))
      (assert-true (< (abs (matrix-ref J 1 0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 1 1) 1.0)) 1e-10))))

  ;; Linear: f(x,y) = (2x+3y, x-y)
  ;; J = [[2,3],[1,-1]]
  (define-test "linear function has constant Jacobian"
    (let ([J (autodiff-jacobian
               (lambda (y)
                 (let ([x (car y)] [v (cadr y)])
                   (list (traced-add (traced-add x x)
                                     (traced-add v (traced-add v v)))  ; 2x + 3y
                         (traced-sub x v))))                            ; x - y
               '(5.0 7.0) 2)])
      (assert-true (< (abs (- (matrix-ref J 0 0) 2.0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 0 1) 3.0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 1 0) 1.0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 1 1) -1.0)) 1e-10))))

  ;; Scalar: f(x) = (x^2), J = [[2x]] = [[6]] at x=3
  (define-test "scalar quadratic"
    (let ([J (autodiff-jacobian
               (lambda (y) (let ([x (car y)]) (list (traced-mul x x))))
               '(3.0) 1)])
      (assert-true (< (abs (- (matrix-ref J 0 0) 6.0)) 1e-10))))

  ;; Nonlinear 2D: f(x,y) = (x*y, x^2 + y^2)
  ;; J = [[y, x], [2x, 2y]] at (2,3) = [[3,2],[4,6]]
  (define-test "nonlinear 2D function"
    (let ([J (autodiff-jacobian
               (lambda (y)
                 (let ([x (car y)] [v (cadr y)])
                   (list (traced-mul x v)
                         (traced-add (traced-mul x x) (traced-mul v v)))))
               '(2.0 3.0) 2)])
      (assert-true (< (abs (- (matrix-ref J 0 0) 3.0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 0 1) 2.0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 1 0) 4.0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 1 1) 6.0)) 1e-10)))))

;;; ====
;;; ODE Jacobian
;;; ====

(test-group "ode-jacobian"

  ;; Harmonic oscillator: dy/dt = (v, -x)
  ;; J = [[0, 1], [-1, 0]]
  (define-test "harmonic oscillator Jacobian"
    (let ([J (ode-jacobian
               (lambda (t y)
                 (let ([x (car y)] [v (cadr y)])
                   (list v (traced-neg x))))
               0.0 '(1.0 0.0))])
      (assert-true (< (abs (matrix-ref J 0 0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 0 1) 1.0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 1 0) -1.0)) 1e-10))
      (assert-true (< (abs (matrix-ref J 1 1)) 1e-10))))

  ;; Nonlinear: f(x,y) = (x*y, -x^2)
  ;; J = [[y, x], [-2x, 0]] at (2,3)
  (define-test "nonlinear ODE Jacobian"
    (let ([J (ode-jacobian
               (lambda (t y)
                 (let ([x (car y)] [v (cadr y)])
                   (list (traced-mul x v)
                         (traced-neg (traced-mul x x)))))
               0.0 '(2.0 3.0))])
      (assert-true (< (abs (- (matrix-ref J 0 0) 3.0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 0 1) 2.0)) 1e-10))
      (assert-true (< (abs (- (matrix-ref J 1 0) -4.0)) 1e-10))
      (assert-true (< (abs (matrix-ref J 1 1)) 1e-10))))

  ;; Dimension check: 3D system
  (define-test "3D system has 3x3 Jacobian"
    (let ([J (ode-jacobian
               (lambda (t y)
                 (let ([a (car y)] [b (cadr y)] [c (caddr y)])
                   (list (traced-add a b)
                         (traced-sub b c)
                         (traced-mul a c))))
               0.0 '(1.0 2.0 3.0))])
      (assert-equal 3 (matrix-rows J))
      (assert-equal 3 (matrix-cols J)))))

;;; ====
;;; Jacobian Check (exact vs numerical)
;;; ====

(test-group "jacobian-check"

  (define-test "exact matches numerical for linear system"
    (let ([result (jacobian-check
                    ;; Numeric version: (v, -x)
                    (lambda (t y)
                      (let ([x (car y)] [v (cadr y)])
                        (list v (- x))))
                    ;; Traced version: (v, -x)
                    (lambda (t y)
                      (let ([x (car y)] [v (cadr y)])
                        (list v (traced-neg x))))
                    0.0 '(1.0 2.0))])
      (let ([max-diff (cdr (assq 'max-diff result))])
        (assert-true (< max-diff 1e-6)))))

  (define-test "exact matches numerical for product"
    (let ([result (jacobian-check
                    (lambda (t y)
                      (let ([x (car y)] [v (cadr y)])
                        (list (* x v) (- (* x x)))))
                    (lambda (t y)
                      (let ([x (car y)] [v (cadr y)])
                        (list (traced-mul x v)
                              (traced-neg (traced-mul x x)))))
                    0.0 '(2.0 3.0))])
      (let ([max-diff (cdr (assq 'max-diff result))])
        (assert-true (< max-diff 1e-5))))))

;;; ====
;;; Stiffness Detection
;;; ====

(test-group "stiffness"

  (define-test "spectral radius of identity is 1"
    (let ([I (matrix-from-lists '((1.0 0.0) (0.0 1.0)))])
      (let ([rho (spectral-radius-estimate I 50)])
        (assert-true (< (abs (- rho 1.0)) 1e-6)))))

  (define-test "spectral radius of scaled identity"
    (let ([M (matrix-from-lists '((5.0 0.0) (0.0 5.0)))])
      (let ([rho (spectral-radius-estimate M 50)])
        (assert-true (< (abs (- rho 5.0)) 1e-4)))))

  (define-test "spectral radius of diagonal matrix"
    (let ([M (matrix-from-lists '((10.0 0.0) (0.0 1.0)))])
      (let ([rho (spectral-radius-estimate M 50)])
        (assert-true (< (abs (- rho 10.0)) 1e-3)))))

  (define-test "stiffness estimate on harmonic oscillator"
    (let ([est (stiffness-estimate
                 (lambda (t y)
                   (let ([x (car y)] [v (cadr y)])
                     (list v (traced-neg x))))
                 0.0 '(1.0 0.0))])
      ;; Eigenvalues of [[0,1],[-1,0]] are ±i, |λ| = 1
      (assert-true (< (abs (- est 1.0)) 0.1)))))

;;; ====
;;; Linearization
;;; ====

(test-group "linearization"

  ;; f(x,y) = (y, -x) linearized at origin
  ;; A = [[0,1],[-1,0]]
  (define-test "linearize harmonic oscillator"
    (let ([result (autodiff-linearize
                    (lambda (state)
                      (let ([x (car state)] [y (cadr state)])
                        (list y (traced-neg x))))
                    '(0.0 0.0))])
      (let ([A (cdr (assq 'A result))]
            [dim (cdr (assq 'dimension result))])
        (assert-equal 2 dim)
        (assert-equal 2 (matrix-rows A))
        (assert-true (< (abs (matrix-ref A 0 0)) 1e-10))
        (assert-true (< (abs (- (matrix-ref A 0 1) 1.0)) 1e-10))
        (assert-true (< (abs (- (matrix-ref A 1 0) -1.0)) 1e-10))
        (assert-true (< (abs (matrix-ref A 1 1)) 1e-10)))))

  (define-test "linearize preserves equilibrium point"
    (let ([result (autodiff-linearize
                    (lambda (state)
                      (let ([x (car state)])
                        (list (traced-neg x))))
                    '(0.0))])
      (assert-equal '(0.0) (cdr (assq 'equilibrium result))))))

(run-all-tests)
