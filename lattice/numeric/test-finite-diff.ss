;;; lattice/numeric/test-finite-diff.ss — Tests for finite difference methods

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/numeric/finite-diff.ss")

(test-group "finite-diff"

  ;;; ====
  ;;; Difference Operators
  ;;; ====

  (define-test "diff-forward computes forward differences"
    (let* ([u '#(0.0 1.0 4.0 9.0)]  ; u = x²
           [h 1.0]
           [du (diff-forward u h)])
      ;; du = [1-0, 4-1, 9-4] = [1, 3, 5]
      (assert-equal 3 (vector-length du))
      (assert-true (< (abs (- (vector-ref du 0) 1.0)) 0.001))
      (assert-true (< (abs (- (vector-ref du 1) 3.0)) 0.001))
      (assert-true (< (abs (- (vector-ref du 2) 5.0)) 0.001))))

  (define-test "diff-backward computes backward differences"
    (let* ([u '#(0.0 1.0 4.0 9.0)]  ; u = x² at x=0,1,2,3
           [h 1.0]
           [du (diff-backward u h)])
      ;; Backward diff at grid point j: (u_j - u_{j-1}) / h
      ;; result[0] = backward at j=1: (1-0)/1 = 1  (approx u'(1) = 2)
      ;; result[1] = backward at j=2: (4-1)/1 = 3  (approx u'(2) = 4)
      ;; result[2] = backward at j=3: (9-4)/1 = 5  (approx u'(3) = 6)
      ;; Same values as forward, but grid-aligned to points 1,2,3 not 0,1,2
      (assert-equal 3 (vector-length du))
      (assert-true (< (abs (- (vector-ref du 0) 1.0)) 0.001))
      (assert-true (< (abs (- (vector-ref du 1) 3.0)) 0.001))
      (assert-true (< (abs (- (vector-ref du 2) 5.0)) 0.001))))

  (define-test "diff-central computes central differences"
    (let* ([u '#(0.0 1.0 4.0 9.0 16.0)]  ; u = x²
           [h 1.0]
           [du (diff-central u h)])
      ;; Central diff at i=1: (4-0)/(2*1) = 2
      ;; Central diff at i=2: (9-1)/(2*1) = 4
      ;; Central diff at i=3: (16-4)/(2*1) = 6
      (assert-equal 3 (vector-length du))
      (assert-true (< (abs (- (vector-ref du 0) 2.0)) 0.001))
      (assert-true (< (abs (- (vector-ref du 1) 4.0)) 0.001))
      (assert-true (< (abs (- (vector-ref du 2) 6.0)) 0.001))))

  (define-test "diff2-central computes second differences"
    (let* ([u '#(0.0 1.0 4.0 9.0 16.0)]  ; u = x², u'' = 2
           [h 1.0]
           [d2u (diff2-central u h)])
      ;; (u[i+1] - 2*u[i] + u[i-1])/h² = (4 - 2*1 + 0)/1 = 2
      (assert-equal 3 (vector-length d2u))
      (assert-true (< (abs (- (vector-ref d2u 0) 2.0)) 0.001))
      (assert-true (< (abs (- (vector-ref d2u 1) 2.0)) 0.001))
      (assert-true (< (abs (- (vector-ref d2u 2) 2.0)) 0.001))))

  ;;; ====
  ;;; Boundary Conditions
  ;;; ====

  (define-test "apply-dirichlet-1d sets boundary values"
    (let* ([u '#(1.0 2.0 3.0 4.0 5.0)]
           [result (apply-dirichlet-1d u 0.0 10.0)])
      (assert-true (= (vector-ref result 0) 0.0))
      (assert-true (= (vector-ref result 4) 10.0))
      (assert-true (= (vector-ref result 2) 3.0))))  ; Interior unchanged

  (define-test "apply-periodic-1d wraps boundaries"
    (let* ([u '#(0.0 1.0 2.0 3.0 0.0)]  ; Endpoints will be overwritten
           [result (apply-periodic-1d u)])
      ;; u[0] = u[n-2] = u[3] = 3
      ;; u[n-1] = u[1] = 1
      (assert-true (= (vector-ref result 0) 3.0))
      (assert-true (= (vector-ref result 4) 1.0))))

  ;;; ====
  ;;; Laplacian Matrix
  ;;; ====

  (define-test "laplacian-matrix-1d has correct structure"
    (let* ([A (laplacian-matrix-1d 3 1.0)]
           [row-ptrs (list-ref A 3)]
           [col-indices (list-ref A 4)]
           [values (list-ref A 5)])
      ;; 3x3 tridiagonal: should have 7 non-zeros (3*3 - 2)
      (assert-equal 7 (vector-length col-indices))
      ;; Row 0 has no left neighbor, so: values[0]=diagonal(-2), values[1]=right(+1)
      ;; Row 1 has all three: values[2]=left(+1), values[3]=diag(-2), values[4]=right(+1)
      (assert-true (< (abs (+ (vector-ref values 0) 2.0)) 0.001))  ; Row 0 diagonal = -2
      (assert-true (< (abs (- (vector-ref values 1) 1.0)) 0.001))  ; Row 0 right neighbor = +1
      (assert-true (< (abs (+ (vector-ref values 3) 2.0)) 0.001)))) ; Row 1 diagonal = -2

  ;;; ====
  ;;; Heat Equation
  ;;; ====

  (define-test "heat-step-explicit-1d diffuses correctly"
    ;; Start with a spike in the middle, should spread out
    (let* ([u '#(0.0 0.0 0.0 1.0 0.0 0.0 0.0)]
           [h 1.0]
           [dt 0.1]
           [alpha 1.0]
           [u1 (heat-step-explicit-1d u h dt alpha 0.0 0.0)])
      ;; The spike should decrease, neighbors should increase
      (assert-true (< (vector-ref u1 3) 1.0))   ; Center decreases
      (assert-true (> (vector-ref u1 2) 0.0))   ; Neighbors increase
      (assert-true (> (vector-ref u1 4) 0.0))))

  (define-test "heat-step-explicit-1d respects CFL"
    ;; With safe CFL, solution should stay bounded
    (let* ([n 11]
           [u (make-vector n 0.0)]
           [h 0.1]
           [alpha 1.0]
           [dt (* 0.4 (/ (* h h) (* 2.0 alpha)))])  ; Safe CFL
      (vector-set! u 5 1.0)  ; Initial spike
      (let loop ([step 0] [v u])
        (if (= step 10)
            ;; After 10 steps, check solution is bounded
            (assert-true (< (apply max (vector->list v)) 2.0))
            (loop (+ step 1) (heat-step-explicit-1d v h dt alpha 0.0 0.0))))))

  ;;; ====
  ;;; Wave Equation
  ;;; ====

  (define-test "wave-step-explicit-1d propagates correctly"
    ;; Initialize with a Gaussian pulse
    (let* ([n 21]
           [u0 (make-vector n 0.0)]
           [v0 (make-vector n 0.0)]  ; Zero initial velocity
           [h 0.1]
           [c 1.0]
           [dt (* 0.9 (/ h c))])  ; CFL safe
      ;; Set initial Gaussian centered at i=10
      (do ([i 0 (+ i 1)])
          ((= i n))
        (let ([x (- i 10)])
          (vector-set! u0 i (exp (- (* 0.5 x x))))))
      ;; Initialize u-prev for velocity
      (let ([u-prev (make-vector n 0.0)])
        (do ([i 0 (+ i 1)])
            ((= i n))
          (vector-set! u-prev i (- (vector-ref u0 i) (* dt (vector-ref v0 i)))))
        ;; Take a step
        (let ([u1 (wave-step-explicit-1d u-prev u0 h dt c 0.0 0.0)])
          ;; Solution should still be bounded and have structure
          (assert-true (< (apply max (vector->list u1)) 2.0))
          (assert-true (> (apply max (vector->list u1)) 0.1))))))

  ;;; ====
  ;;; Poisson Solver
  ;;; ====

  (define-test "solve-poisson-1d solves simple case"
    ;; Solve u'' = -2 with u(0)=0, u(1)=0
    ;; Analytical: u = x(1-x)
    (let* ([n 11]
           [h (/ 1.0 (- n 1))]
           [f (make-vector n -2.0)])  ; Constant RHS
      (let-values ([(sol residual iters) (solve-poisson-1d f h 0.0 0.0 1e-8 1000)])
        ;; Check residual is small
        (assert-true (< residual 1e-4))
        ;; Check solution at midpoint: u(0.5) = 0.5*0.5 = 0.25
        (let ([mid-idx 5])
          (assert-true (< (abs (- (vector-ref sol mid-idx) 0.25)) 0.05))))))

)

(run-all-tests)
