;;; user/demos/fem-heat-demo.ss --- Visual FEM demonstration
;;;
;;; Solves the Poisson equation on various domains with interesting
;;; boundary conditions and source terms.

(load "lattice/numeric/fem.ss")

;;; ============================================================
;;; Demo 1: Hot Spot in Center
;;; ============================================================
;;;
;;; -∇²u = δ(center)  (point heat source)
;;; u = 0 on boundary (cold boundary)
;;;
;;; This simulates a heat source in the middle of a plate
;;; with the edges held at zero temperature.

(define (demo-hotspot)
  (printf "~n═══════════════════════════════════════════════════════════~n")
  (printf "   FEM Demo: Hot Spot (Point Source)~n")
  (printf "═══════════════════════════════════════════════════════════~n~n")

  (let* ([mesh (make-unit-square-mesh 15)]
         ;; Approximate point source at center
         [sigma 0.05]
         [f (lambda (x y)
              (let ([dx (- x 0.5)]
                    [dy (- y 0.5)])
                (* 100.0 (exp (- (/ (+ (* dx dx) (* dy dy))
                                    (* 2 sigma sigma)))))))]
         [g (lambda (x y) 0.0)]  ; Cold boundary
         [solution (fem-solve-poisson mesh f g)])

    (printf "Mesh: ~a nodes, ~a elements~n"
            (fem-mesh-num-nodes mesh)
            (fem-mesh-num-elements mesh))
    (printf "~nHeat distribution (point source at center):~n~n")
    (display (fem-render-solution mesh solution 60 25))
    (printf "~nLegend: ' '=cold  ░▒▓█=increasingly hot~n")))

;;; ============================================================
;;; Demo 2: Two Hot Corners
;;; ============================================================
;;;
;;; u = 1 on top-left and bottom-right corners
;;; u = 0 elsewhere on boundary

(define (demo-corners)
  (printf "~n═══════════════════════════════════════════════════════════~n")
  (printf "   FEM Demo: Diagonal Hot Corners~n")
  (printf "═══════════════════════════════════════════════════════════~n~n")

  (let* ([mesh (make-unit-square-mesh 15)]
         [f (lambda (x y) 0.0)]  ; No internal source
         ;; Hot at (0,1) and (1,0) corners
         [g (lambda (x y)
              (cond
                [(and (< x 0.15) (> y 0.85)) 1.0]
                [(and (> x 0.85) (< y 0.15)) 1.0]
                [else 0.0]))]
         [solution (fem-solve-poisson mesh f g)])

    (printf "Boundary: hot at top-left and bottom-right corners~n")
    (printf "~nTemperature distribution:~n~n")
    (display (fem-render-solution mesh solution 60 25))
    (printf "~nNotice the diagonal heat flow pattern~n")))

;;; ============================================================
;;; Demo 3: Circular Domain with Radial Heat
;;; ============================================================

(define (demo-disk)
  (printf "~n═══════════════════════════════════════════════════════════~n")
  (printf "   FEM Demo: Circular Domain~n")
  (printf "═══════════════════════════════════════════════════════════~n~n")

  (let* ([mesh (make-disk-mesh 1.0 200)]
         ;; Heat source proportional to radius
         [f (lambda (x y)
              (let ([r (sqrt (+ (* x x) (* y y)))])
                (* 10.0 r)))]
         [g (lambda (x y) 0.0)]  ; Cold boundary
         [solution (fem-solve-poisson mesh f g)])

    (printf "Mesh: ~a nodes, ~a elements~n"
            (fem-mesh-num-nodes mesh)
            (fem-mesh-num-elements mesh))
    (printf "Source: f(r) = 10r (more heat toward edge)~n")
    (printf "~nTemperature distribution:~n~n")
    (display (fem-render-solution mesh solution 50 25))
    (printf "~n")))

;;; ============================================================
;;; Demo 4: Sinusoidal Boundary
;;; ============================================================

(define (demo-wave-boundary)
  (printf "~n═══════════════════════════════════════════════════════════~n")
  (printf "   FEM Demo: Sinusoidal Boundary Conditions~n")
  (printf "═══════════════════════════════════════════════════════════~n~n")

  (let* ([pi 3.141592653589793]
         [mesh (make-unit-square-mesh 20)]
         [f (lambda (x y) 0.0)]  ; Laplace equation
         ;; Sinusoidal temperature on boundary
         [g (lambda (x y)
              (cond
                [(< y 0.01) (sin (* 2 pi x))]      ; Bottom edge
                [(> y 0.99) (- (sin (* 2 pi x)))]  ; Top edge (opposite phase)
                [(< x 0.01) 0.0]                    ; Left edge
                [(> x 0.99) 0.0]                    ; Right edge
                [else 0.0]))]
         [solution (fem-solve-poisson mesh f g)])

    (printf "Boundary: sin(2πx) on bottom, -sin(2πx) on top~n")
    (printf "This creates an interesting wave-like pattern~n")
    (printf "~nTemperature distribution:~n~n")
    (display (fem-render-solution mesh solution 60 30))
    (printf "~n")))

;;; ============================================================
;;; Demo 5: L-Shaped Domain (Re-entrant Corner)
;;; ============================================================

(define (make-L-mesh n)
  ;; Create L-shaped domain by generating points and triangulating
  ;; L = [0,1]² minus [0.5,1]×[0.5,1]
  (let* ([h (/ 1.0 n)]
         [pts '()])
    ;; Generate grid points in L shape
    (do ([i 0 (+ i 1)])
        ((> i n))
      (do ([j 0 (+ j 1)])
          ((> j n))
        (let ([x (* i h)]
              [y (* j h)])
          ;; Exclude upper-right quadrant
          (unless (and (> x 0.49) (> y 0.49))
            (set! pts (cons (make-point2 x y) pts))))))
    (let ([triangles (delaunay-triangulate pts)])
      ;; Filter out triangles with centroids in excluded region
      (let ([valid-tris
             (filter (lambda (tri)
                       (let* ([p1 (tri2-p1 tri)]
                              [p2 (tri2-p2 tri)]
                              [p3 (tri2-p3 tri)]
                              [cx (/ (+ (point2-x p1) (point2-x p2) (point2-x p3)) 3.0)]
                              [cy (/ (+ (point2-y p1) (point2-y p2) (point2-y p3)) 3.0)])
                         (not (and (> cx 0.49) (> cy 0.49)))))
                     triangles)])
        (make-fem-mesh valid-tris)))))

(define (demo-L-shape)
  (printf "~n═══════════════════════════════════════════════════════════~n")
  (printf "   FEM Demo: L-Shaped Domain (Corner Singularity)~n")
  (printf "═══════════════════════════════════════════════════════════~n~n")

  (let* ([mesh (make-L-mesh 12)]
         [f (lambda (x y) 1.0)]  ; Uniform heat source
         [g (lambda (x y) 0.0)]  ; Cold boundary
         [solution (fem-solve-poisson mesh f g)])

    (printf "L-shaped domain: [0,1]² with upper-right removed~n")
    (printf "The re-entrant corner creates a singularity~n")
    (printf "Mesh: ~a nodes, ~a elements~n"
            (fem-mesh-num-nodes mesh)
            (fem-mesh-num-elements mesh))
    (printf "~nTemperature distribution:~n~n")
    (display (fem-render-solution mesh solution 45 30))
    (printf "~nNotice the gradient singularity at the corner~n")))

;;; ============================================================
;;; Demo 6: Convergence Study
;;; ============================================================

(define (demo-convergence)
  (printf "~n═══════════════════════════════════════════════════════════~n")
  (printf "   FEM Demo: Mesh Convergence Study~n")
  (printf "═══════════════════════════════════════════════════════════~n~n")

  (let* ([pi 3.141592653589793]
         ;; Known solution: u = sin(πx)sin(πy)
         ;; satisfies -∇²u = 2π²sin(πx)sin(πy)
         [f (lambda (x y) (* 2.0 pi pi (sin (* pi x)) (sin (* pi y))))]
         [g (lambda (x y) 0.0)]
         [exact (lambda (x y) (* (sin (* pi x)) (sin (* pi y))))])

    (printf "Testing: -∇²u = 2π²sin(πx)sin(πy), u=0 on boundary~n")
    (printf "Exact solution: u = sin(πx)sin(πy)~n~n")
    (printf "  n     nodes    elements    L² error    rate~n")
    (printf "─────────────────────────────────────────────────~n")

    (let loop ([ns '(5 7 10 14 20)] [prev-err #f] [prev-h #f])
      (unless (null? ns)
        (let* ([n (car ns)]
               [mesh (make-unit-square-mesh n)]
               [solution (fem-solve-poisson mesh f g)]
               [err (fem-l2-error mesh solution exact)]
               [h (/ 1.0 (- n 1))]
               [rate (if (and prev-err prev-h)
                         (/ (log (/ prev-err err)) (log (/ prev-h h)))
                         0.0)])
          (printf "~3d    ~5d      ~5d      ~,2e    ~,2f~n"
                  n
                  (fem-mesh-num-nodes mesh)
                  (fem-mesh-num-elements mesh)
                  err
                  rate)
          (loop (cdr ns) err h))))

    (printf "~nExpected rate ≈ 2 for P1 elements (quadratic convergence)~n")))

;;; ============================================================
;;; Run All Demos
;;; ============================================================

(define (run-all-demos)
  (demo-hotspot)
  (demo-corners)
  (demo-wave-boundary)
  (demo-L-shape)
  (demo-convergence)
  (demo-disk)
  (printf "~n═══════════════════════════════════════════════════════════~n")
  (printf "   All FEM demos complete!~n")
  (printf "═══════════════════════════════════════════════════════════~n"))

;; Run by default
(run-all-demos)
