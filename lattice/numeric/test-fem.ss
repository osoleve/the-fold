;;; lattice/numeric/test-fem.ss --- Tests for finite element method
;;; @requires fem test-framework

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/numeric/fem.ss")

(printf "~n=== FEM Module Tests ===~n~n")

;;; ============================================================
;;; Test: Mesh Construction
;;; ============================================================

(test-group "fem-mesh"

  (define-test "make-fem-mesh creates valid structure"
    (let* ([pts (list (make-point2 0.0 0.0)
                      (make-point2 1.0 0.0)
                      (make-point2 0.0 1.0)
                      (make-point2 1.0 1.0))]
           [tris (delaunay-triangulate pts)]
           [mesh (make-fem-mesh tris)])
      (assert-true (fem-mesh? mesh))
      (assert-equal 4 (fem-mesh-num-nodes mesh))
      (assert-equal 2 (fem-mesh-num-elements mesh))))

  (define-test "unit square mesh has correct node count"
    (let ([mesh (make-unit-square-mesh 5)])
      (assert-equal 25 (fem-mesh-num-nodes mesh))))

  (define-test "disk mesh creates valid mesh"
    (let ([mesh (make-disk-mesh 1.0 20)])
      (assert-true (fem-mesh? mesh))
      (assert-true (> (fem-mesh-num-nodes mesh) 10))
      (assert-true (> (fem-mesh-num-elements mesh) 5)))))

;;; ============================================================
;;; Test: Element Computations
;;; ============================================================

(test-group "element-matrices"

  (define-test "element-area computes correct area"
    (let* ([p1 (make-point2 0.0 0.0)]
           [p2 (make-point2 1.0 0.0)]
           [p3 (make-point2 0.0 1.0)]
           [area (element-area p1 p2 p3)])
      (assert-true (< (abs (- area 0.5)) 1e-10))))

  (define-test "element-area sign indicates orientation"
    (let* ([p1 (make-point2 0.0 0.0)]
           [p2 (make-point2 1.0 0.0)]
           [p3 (make-point2 0.0 1.0)]
           [ccw-area (element-area p1 p2 p3)]
           [cw-area (element-area p1 p3 p2)])
      (assert-true (> ccw-area 0))
      (assert-true (< cw-area 0))))

  (define-test "basis-gradients sum to zero"
    ;; ∇φ₁ + ∇φ₂ + ∇φ₃ = 0 (partition of unity)
    (let* ([p1 (make-point2 0.0 0.0)]
           [p2 (make-point2 2.0 0.0)]
           [p3 (make-point2 1.0 1.5)]
           [grads (basis-gradients p1 p2 p3)]
           [sum-x (+ (vector-ref (list-ref grads 0) 0)
                     (vector-ref (list-ref grads 1) 0)
                     (vector-ref (list-ref grads 2) 0))]
           [sum-y (+ (vector-ref (list-ref grads 0) 1)
                     (vector-ref (list-ref grads 1) 1)
                     (vector-ref (list-ref grads 2) 1))])
      (assert-true (< (abs sum-x) 1e-10))
      (assert-true (< (abs sum-y) 1e-10))))

  (define-test "stiffness matrix is symmetric"
    (let* ([p1 (make-point2 0.0 0.0)]
           [p2 (make-point2 1.0 0.0)]
           [p3 (make-point2 0.5 0.8)]
           [K (element-stiffness p1 p2 p3)])
      (assert-true (< (abs (- (matrix-ref K 0 1) (matrix-ref K 1 0))) 1e-10))
      (assert-true (< (abs (- (matrix-ref K 0 2) (matrix-ref K 2 0))) 1e-10))
      (assert-true (< (abs (- (matrix-ref K 1 2) (matrix-ref K 2 1))) 1e-10))))

  (define-test "stiffness matrix row sum is zero"
    ;; Stiffness matrix has zero row sums (constant functions have zero energy)
    (let* ([p1 (make-point2 0.0 0.0)]
           [p2 (make-point2 1.0 0.0)]
           [p3 (make-point2 0.5 0.8)]
           [K (element-stiffness p1 p2 p3)])
      (do ([i 0 (+ i 1)])
          ((= i 3))
        (let ([row-sum (+ (matrix-ref K i 0) (matrix-ref K i 1) (matrix-ref K i 2))])
          (assert-true (< (abs row-sum) 1e-10))))))

  (define-test "mass matrix is symmetric positive"
    (let* ([p1 (make-point2 0.0 0.0)]
           [p2 (make-point2 1.0 0.0)]
           [p3 (make-point2 0.5 0.8)]
           [M (element-mass p1 p2 p3)])
      ;; Symmetry
      (assert-true (< (abs (- (matrix-ref M 0 1) (matrix-ref M 1 0))) 1e-10))
      ;; Positive diagonal
      (assert-true (> (matrix-ref M 0 0) 0))
      (assert-true (> (matrix-ref M 1 1) 0))
      (assert-true (> (matrix-ref M 2 2) 0))))

  (define-test "mass matrix integrates to area"
    ;; Integral of φ_i over element is A/3
    (let* ([p1 (make-point2 0.0 0.0)]
           [p2 (make-point2 1.0 0.0)]
           [p3 (make-point2 0.0 1.0)]
           [M (element-mass p1 p2 p3)]
           [area 0.5]  ; Right triangle
           ;; Row sum = ∫φ_i dΩ = A/3
           [row-sum (+ (matrix-ref M 0 0) (matrix-ref M 0 1) (matrix-ref M 0 2))])
      (assert-true (< (abs (- row-sum (/ area 3.0))) 1e-10)))))

;;; ============================================================
;;; Test: Assembly
;;; ============================================================

(test-group "assembly"

  (define-test "assembled stiffness has correct dimensions"
    (let* ([mesh (make-unit-square-mesh 3)]
           [K (assemble-stiffness mesh)])
      (assert-equal (fem-mesh-num-nodes mesh) (sparse-coo-rows K))
      (assert-equal (fem-mesh-num-nodes mesh) (sparse-coo-cols K))))

  (define-test "assembled mass has correct dimensions"
    (let* ([mesh (make-unit-square-mesh 3)]
           [M (assemble-mass mesh)])
      (assert-equal (fem-mesh-num-nodes mesh) (sparse-coo-rows M))
      (assert-equal (fem-mesh-num-nodes mesh) (sparse-coo-cols M))))

  (define-test "load vector assembles correctly"
    (let* ([mesh (make-unit-square-mesh 3)]
           [f (lambda (x y) 1.0)]  ; Constant source
           [F (assemble-load mesh f)]
           ;; Total load should equal domain area for f=1
           [total (let loop ([i 0] [sum 0.0])
                    (if (= i (vector-length F))
                        sum
                        (loop (+ i 1) (+ sum (vector-ref F i)))))])
      ;; Area of unit square is 1
      (assert-true (< (abs (- total 1.0)) 0.1)))))

;;; ============================================================
;;; Test: Boundary Detection
;;; ============================================================

(test-group "boundary"

  (define-test "finds boundary nodes on unit square"
    (let* ([mesh (make-unit-square-mesh 3)]
           [boundary (find-boundary-nodes mesh)]
           [n (fem-mesh-num-nodes mesh)])
      ;; 3x3 grid: 9 nodes, 8 on boundary, 1 interior
      (assert-equal 8 (length boundary)))))

;;; ============================================================
;;; Test: Poisson Solver
;;; ============================================================

(test-group "poisson-solver"

  (define-test "solves Laplace equation (f=0, linear BC)"
    ;; -∇²u = 0 with u = x on boundary should give u = x everywhere
    (let* ([mesh (make-unit-square-mesh 5)]
           [f (lambda (x y) 0.0)]           ; No source
           [g (lambda (x y) x)]              ; Linear BC
           [u (fem-solve-poisson mesh f g)]
           ;; Check solution at interior nodes
           [max-error 0.0])
      (do ([i 0 (+ i 1)])
          ((= i (fem-mesh-num-nodes mesh)))
        (let* ([p (fem-mesh-node mesh i)]
               [x (point2-x p)]
               [exact x]
               [err (abs (- (vector-ref u i) exact))])
          (when (> err max-error)
            (set! max-error err))))
      ;; Should be very accurate for linear solution
      (assert-true (< max-error 0.01))))

  (define-test "solves Poisson with known solution"
    ;; u = sin(πx)sin(πy) satisfies -∇²u = 2π²sin(πx)sin(πy)
    ;; with u = 0 on boundary of [0,1]²
    (let* ([pi 3.141592653589793]
           [mesh (make-unit-square-mesh 8)]
           [f (lambda (x y) (* 2.0 pi pi (sin (* pi x)) (sin (* pi y))))]
           [g (lambda (x y) 0.0)]
           [exact (lambda (x y) (* (sin (* pi x)) (sin (* pi y))))]
           [u (fem-solve-poisson mesh f g)]
           [l2-err (fem-l2-error mesh u exact)])
      ;; L² error should be reasonably small
      (assert-true (< l2-err 0.1)))))

;;; ============================================================
;;; Test: Solution Interpolation
;;; ============================================================

(test-group "interpolation"

  (define-test "interpolates at mesh nodes exactly"
    (let* ([mesh (make-unit-square-mesh 3)]
           [n (fem-mesh-num-nodes mesh)]
           [solution (make-vector n 0.0)])
      ;; Set arbitrary values
      (do ([i 0 (+ i 1)])
          ((= i n))
        (vector-set! solution i (exact->inexact i)))
      ;; Check interpolation at nodes
      (do ([i 0 (+ i 1)])
          ((= i n))
        (let* ([p (fem-mesh-node mesh i)]
               [interp (fem-solution-at mesh solution (point2-x p) (point2-y p))])
          (assert-true (< (abs (- interp (exact->inexact i))) 0.01)))))))

;;; ============================================================
;;; Test: Visualization
;;; ============================================================

(test-group "visualization"

  (define-test "render produces non-empty output"
    (let* ([mesh (make-unit-square-mesh 5)]
           [f (lambda (x y) 1.0)]
           [g (lambda (x y) 0.0)]
           [u (fem-solve-poisson mesh f g)]
           [render (fem-render-solution mesh u 20 10)])
      (assert-true (> (string-length render) 100)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)
