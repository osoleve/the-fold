(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'matrix)
(require 'vec)
(require 'matrix-decomp)

(doc 'module 'charts)
(doc 'description "Coordinate Charts and Atlases - Foundation for smooth manifold representation")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "A chart (U, φ) consists of: domain (open set), coordinate map φ: U → R^n, inverse map φ⁻¹: R^n → U")
(doc 'note "An atlas is a collection of compatible charts covering a manifold")
(doc 'note "Charts are compatible if their transition functions are smooth")

(doc 'section 'chart-representation)
(doc 'note "A chart is: (chart name dim domain-pred coord-map inverse-map)")
(doc 'note "name: Symbol identifying the chart")
(doc 'note "dim: Dimension of the coordinate space (n for R^n)")
(doc 'note "domain-pred: Predicate (point → bool) defining domain membership")
(doc 'note "coord-map: Function (point → R^n coords) - the chart map φ")
(doc 'note "inverse-map: Function (R^n coords → point) - the inverse φ⁻¹")

(doc chart? 'type '(-> Any Boolean))
(define (chart? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'chart)
       (= (length x) 6)
       (symbol? (chart-name x))
       (integer? (chart-dim x))
       (procedure? (chart-domain-pred x))
       (procedure? (chart-coord-map x))
       (procedure? (chart-inverse-map x))))

(doc chart-name 'type '(-> Chart Symbol))
(define (chart-name c)
  (doc 'export #t)
  (list-ref c 1))

(doc chart-dim 'type '(-> Chart Nat))
(define (chart-dim c)
  (doc 'export #t)
  (list-ref c 2))

(doc chart-domain-pred 'type '(-> Chart (-> Point Bool)))
(define (chart-domain-pred c)
  (doc 'export #t)
  (list-ref c 3))

(doc chart-coord-map 'type '(-> Chart (-> Point Vec)))
(define (chart-coord-map c)
  (doc 'export #t)
  (list-ref c 4))

(doc chart-inverse-map 'type '(-> Chart (-> Vec Point)))
(define (chart-inverse-map c)
  (doc 'export #t)
  (list-ref c 5))

(doc 'section 'chart-construction)

(doc make-chart 'type '(-> Symbol Nat (-> Point Bool) (-> Point Vec) (-> Vec Point) Chart))
(doc make-chart 'description "Create a coordinate chart")
(define (make-chart name dim domain-pred coord-map inverse-map)
  (doc 'export #t)
  (list 'chart name dim domain-pred coord-map inverse-map))

(doc 'section 'chart-operations)

(doc chart-contains? 'type '(-> Chart Point Bool))
(doc chart-contains? 'description "Check if a point is in the chart's domain")
(define (chart-contains? chart point)
  (doc 'export #t)
  ((chart-domain-pred chart) point))

(doc chart-apply 'type '(-> Chart Point (Or Vec Error)))
(doc chart-apply 'description "Apply the coordinate map to get coordinates")
(define (chart-apply chart point)
  (doc 'export #t)
  (if (chart-contains? chart point)
      ((chart-coord-map chart) point)
      `(error point-not-in-domain ,(chart-name chart))))

(doc chart-apply-inverse 'type '(-> Chart Vec Point))
(doc chart-apply-inverse 'description "Apply the inverse map to get a point from coordinates")
(define (chart-apply-inverse chart coords)
  (doc 'export #t)
  ((chart-inverse-map chart) coords))

(doc 'section 'transition-functions)
(doc 'note "A transition function τ_αβ = φ_β ∘ φ_α⁻¹ converts coordinates from chart α to chart β")

(doc make-transition 'type '(-> Chart Chart (Or (-> Vec Vec) Bool)))
(doc make-transition 'description "Create a transition function from chart-from to chart-to")
(doc make-transition 'returns "Function or #f if charts have different dimensions")
(doc make-transition 'note "Returned function does NOT verify domain membership (for performance)")
(doc make-transition 'see 'transition-apply)
(define (make-transition chart-from chart-to)
  (doc 'export #t)
  (if (not (= (chart-dim chart-from) (chart-dim chart-to)))
      #f
      (lambda (coords)
        (let ([point ((chart-inverse-map chart-from) coords)])
          ((chart-coord-map chart-to) point)))))

(doc transition-apply 'type '(-> Chart Chart Vec (Or Vec Error)))
(doc transition-apply 'description "Apply the transition function from chart-from to chart-to")
(doc transition-apply 'note "Verifies that the intermediate point lies in the target chart's domain")
(define (transition-apply chart-from chart-to coords)
  (doc 'export #t)
  (if (not (= (chart-dim chart-from) (chart-dim chart-to)))
      `(error dimension-mismatch ,(chart-dim chart-from) ,(chart-dim chart-to))
      (let ([point ((chart-inverse-map chart-from) coords)])
        ;; Verify point is in target chart's domain
        (if (not ((chart-domain-pred chart-to) point))
            `(error point-not-in-target-domain ,(chart-name chart-to) ,point)
            ((chart-coord-map chart-to) point)))))

(doc 'section 'jacobian-computation)
(doc 'note "The Jacobian of a transition function is the matrix of partial derivatives J_ij = ∂(τ_i)/∂(x_j)")
(doc 'note "We compute it numerically using central differences")

(doc *jacobian-epsilon* 'description "Default step size for numerical differentiation")
(define *jacobian-epsilon* 1e-7)

(doc jacobian-numerical 'type '(-> (-> Vec Vec) Vec (Optional Num) Matrix))
(doc jacobian-numerical 'description "Compute the Jacobian matrix numerically at a point")
(doc jacobian-numerical 'param "f: R^n → R^m")
(doc jacobian-numerical 'param "coords: R^n")
(doc jacobian-numerical 'returns "m×n matrix")
(define (jacobian-numerical f coords . epsilon-arg)
  (doc 'export #t)
  (let* ([eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))]
         [n (vector-length coords)]
         [f0 (f coords)]
         [m (vector-length f0)]
         [result (make-matrix m n 0)])
    ;; Compute each column j by perturbing coord j
    (do ([j 0 (+ j 1)])
        ((= j n) result)
        ;; Create perturbed coordinate vectors
        (let ([coords-plus (vec-copy coords)]
              [coords-minus (vec-copy coords)])
          (vector-set! coords-plus j (+ (vector-ref coords j) eps))
          (vector-set! coords-minus j (- (vector-ref coords j) eps))
          (let ([f-plus (f coords-plus)]
                [f-minus (f coords-minus)])
            ;; Central difference for each output component
            (do ([i 0 (+ i 1)])
                ((= i m))
                (let ([deriv (/ (- (vector-ref f-plus i) (vector-ref f-minus i))
                                (* 2 eps))])
                  ;; Set J[i,j]
                  (vector-set! (matrix-data result)
                               (+ (* i n) j)
                               deriv))))))))

(doc transition-jacobian 'type '(-> Chart Chart Vec (Optional Num) Matrix))
(doc transition-jacobian 'description "Compute the Jacobian of the transition function at given coordinates")
(define (transition-jacobian chart-from chart-to coords . epsilon-arg)
  (doc 'export #t)
  (let ([transition (make-transition chart-from chart-to)]
        [eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))])
    (if transition
        (jacobian-numerical transition coords eps)
        `(error dimension-mismatch ,(chart-dim chart-from) ,(chart-dim chart-to)))))

(doc 'section 'atlas-representation)
(doc 'note "An atlas is: (atlas name charts)")
(doc 'note "name: Symbol identifying the atlas")
(doc 'note "charts: List of charts")

(doc atlas? 'type '(-> Any Boolean))
(define (atlas? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'atlas)
       (= (length x) 3)
       (symbol? (atlas-name x))
       (list? (atlas-charts x))))

;;; atlas-name : Atlas → Symbol
(define (atlas-name a)
  (doc 'export #t)
  (list-ref a 1))

;;; atlas-charts : Atlas → (List Chart)
(define (atlas-charts a)
  (doc 'export #t)
  (list-ref a 2))

;;; ====
;;; Atlas Construction
;;; ====

;;; make-atlas : Symbol × (List Chart) → Atlas | Error
;;; Create an atlas from a list of charts.
;;; All charts must have the same dimension.
(define (make-atlas name charts)
  (doc 'export #t)
  (if (null? charts)
      (list 'atlas name '())
      (let ([dim (chart-dim (car charts))])
        (if (for-all (lambda (c) (= (chart-dim c) dim)) charts)
            (list 'atlas name charts)
            `(error inconsistent-dimensions)))))

;;; atlas-dim : Atlas → Nat | #f
;;; Get the dimension of the atlas (from its charts).
(define (atlas-dim a)
  (doc 'export #t)
  (let ([charts (atlas-charts a)])
    (if (null? charts)
        #f
        (chart-dim (car charts)))))

;;; atlas-chart-count : Atlas → Nat
(define (atlas-chart-count a)
  (doc 'export #t)
  (length (atlas-charts a)))

;;; ====
;;; Atlas Operations
;;; ====

;;; atlas-add-chart : Atlas × Chart → Atlas | Error
;;; Add a chart to an atlas.
(define (atlas-add-chart atlas chart)
  (doc 'export #t)
  (let ([charts (atlas-charts atlas)]
        [dim (atlas-dim atlas)])
    (cond
     [(and dim (not (= dim (chart-dim chart))))
      `(error dimension-mismatch ,dim ,(chart-dim chart))]
     [else
      (list 'atlas (atlas-name atlas) (cons chart charts))])))

;;; atlas-find-chart : Atlas × Point → Chart | #f
;;; Find a chart containing the given point.
(define (atlas-find-chart atlas point)
  (doc 'export #t)
  (let loop ([charts (atlas-charts atlas)])
    (cond
     [(null? charts) #f]
     [(chart-contains? (car charts) point) (car charts)]
     [else (loop (cdr charts))])))

;;; atlas-find-chart-by-name : Atlas × Symbol → Chart | #f
;;; Find a chart by name.
(define (atlas-find-chart-by-name atlas name)
  (doc 'export #t)
  (let loop ([charts (atlas-charts atlas)])
    (cond
     [(null? charts) #f]
     [(eq? (chart-name (car charts)) name) (car charts)]
     [else (loop (cdr charts))])))

;;; atlas-coords : Atlas × Point → (Symbol . Vec) | Error
;;; Get coordinates for a point in some chart.
;;; Returns (chart-name . coords) pair.
(define (atlas-coords atlas point)
  (doc 'export #t)
  (let ([chart (atlas-find-chart atlas point)])
    (if chart
        (cons (chart-name chart) (chart-apply chart point))
        `(error point-not-covered))))

;;; ====
;;; Chart Compatibility
;;; ====

;;; charts-overlap? : Chart × Chart × (List Vec) → Bool
;;; Check if two charts have overlapping domains.
;;; Uses a list of test points (in chart-a's coordinates).
(define (charts-overlap? chart-a chart-b test-coords)
  (doc 'export #t)
  (let loop ([coords test-coords])
    (if (null? coords)
        #f
        (let ([point (chart-apply-inverse chart-a (car coords))])
          (if (chart-contains? chart-b point)
              #t
              (loop (cdr coords)))))))

;;; transition-smooth? : Chart × Chart × (List Vec) × [Num] → Bool
;;; Check if the transition between charts is smooth at test points.
;;; "Smooth" here means the Jacobian exists and is non-singular.
(define (transition-smooth? chart-from chart-to test-coords . epsilon-arg)
  (doc 'export #t)
  (let ([eps (if (null? epsilon-arg) *jacobian-epsilon* (car epsilon-arg))])
    (let loop ([coords test-coords])
      (if (null? coords)
          #t  ; All test points passed
          (let* ([c (car coords)]
                 [J (transition-jacobian chart-from chart-to c eps)]
                 [det (jacobian-determinant J)])
            ;; Handle error results from jacobian-determinant (e.g., from matrix-det)
            (cond
             [(and (pair? det) (eq? (car det) 'error)) #f]  ; Error → not smooth
             [(< (abs det) 1e-10) #f]  ; Singular Jacobian → not smooth
             [else (loop (cdr coords))]))))))

;;; jacobian-determinant : Matrix → Num | Error
;;; Compute the determinant of a square matrix.
;;; For small matrices (up to 3×3), use explicit formulas for efficiency.
;;; For larger matrices, use LU decomposition via matrix-det.
(define (jacobian-determinant J)
  (doc 'export #t)
  (let ([n (matrix-rows J)])
    (cond
     [(not (= n (matrix-cols J)))
      `(error not-square ,(matrix-rows J) ,(matrix-cols J))]
     [(= n 1)
      (matrix-ref J 0 0)]
     [(= n 2)
      (- (* (matrix-ref J 0 0) (matrix-ref J 1 1))
         (* (matrix-ref J 0 1) (matrix-ref J 1 0)))]
     [(= n 3)
      (let ([a (matrix-ref J 0 0)] [b (matrix-ref J 0 1)] [c (matrix-ref J 0 2)]
            [d (matrix-ref J 1 0)] [e (matrix-ref J 1 1)] [f (matrix-ref J 1 2)]
            [g (matrix-ref J 2 0)] [h (matrix-ref J 2 1)] [i (matrix-ref J 2 2)])
        (+ (* a (- (* e i) (* f h)))
           (- (* b (- (* d i) (* f g))))
           (* c (- (* d h) (* e g)))))]
     [else
      ;; For larger matrices (n > 3), use LU decomposition
      (matrix-det J)])))

;;; ====
;;; Standard Chart Constructors
;;; ====

;;; These create charts for common coordinate systems.

;;; make-identity-chart : Symbol × Nat → Chart
;;; Create a chart where coordinates equal the point (R^n identity).
(define (make-identity-chart name dim)
  (doc 'export #t)
  (make-chart name dim
              (lambda (p) #t)           ; All R^n is the domain
              (lambda (p) p)            ; Identity map
              (lambda (c) c)))          ; Identity inverse

;;; make-polar-chart : → Chart
;;; Create a 2D polar coordinate chart.
;;; Domain: R^2 \ {origin and negative x-axis}
;;; Coordinates: (r, θ) where r > 0 and θ ∈ (-π, π]
(define (make-polar-chart)
  (doc 'export #t)
  (make-chart 'polar 2
              ;; Domain: not origin, not negative x-axis
              (lambda (p)
                (let ([x (vector-ref p 0)]
                      [y (vector-ref p 1)])
                  (not (and (<= x 0) (= y 0)))))
              ;; Coord map: (x,y) → (r, θ)
              (lambda (p)
                (let ([x (vector-ref p 0)]
                      [y (vector-ref p 1)])
                  (vector (sqrt (+ (* x x) (* y y)))
                          (atan y x))))
              ;; Inverse: (r, θ) → (x, y)
              (lambda (c)
                (let ([r (vector-ref c 0)]
                      [theta (vector-ref c 1)])
                  (vector (* r (cos theta))
                          (* r (sin theta)))))))

;;; make-cartesian-chart : → Chart
;;; Create a 2D Cartesian coordinate chart (identity on R^2).
(define (make-cartesian-chart)
  (doc 'export #t)
  (make-identity-chart 'cartesian 2))

;;; make-spherical-chart : → Chart
;;; Create a 3D spherical coordinate chart.
;;; Domain: R^3 \ {z-axis ∪ negative x-axis half-plane}
;;; Excludes z-axis (singularity) and atan branch cut (y=0, x≤0)
;;; Coordinates: (r, θ, φ) where r > 0, θ ∈ (0, π), φ ∈ (-π, π)
(define (make-spherical-chart)
  (doc 'export #t)
  (make-chart 'spherical 3
              ;; Domain: not z-axis AND not atan branch cut
              (lambda (p)
                (let ([x (vector-ref p 0)]
                      [y (vector-ref p 1)])
                  (and (not (and (= x 0) (= y 0)))      ; Exclude z-axis
                       (not (and (<= x 0) (= y 0))))))
              ;; Coord map: (x,y,z) → (r, θ, φ)
              (lambda (p)
                (let* ([x (vector-ref p 0)]
                       [y (vector-ref p 1)]
                       [z (vector-ref p 2)]
                       [r (sqrt (+ (* x x) (* y y) (* z z)))]
                       [theta (acos (/ z r))]
                       [phi (atan y x)])
                  (vector r theta phi)))
              ;; Inverse: (r, θ, φ) → (x, y, z)
              (lambda (c)
                (let ([r (vector-ref c 0)]
                      [theta (vector-ref c 1)]
                      [phi (vector-ref c 2)])
                  (vector (* r (sin theta) (cos phi))
                          (* r (sin theta) (sin phi))
                          (* r (cos theta)))))))

;;; make-cylindrical-chart : → Chart
;;; Create a 3D cylindrical coordinate chart.
;;; Domain: R^3 \ {z-axis ∪ negative x-axis half-plane}
;;; Excludes z-axis (singularity) and atan branch cut (y=0, x≤0)
;;; Coordinates: (ρ, φ, z) where ρ > 0, φ ∈ (-π, π)
(define (make-cylindrical-chart)
  (doc 'export #t)
  (make-chart 'cylindrical 3
              ;; Domain: not z-axis AND not atan branch cut
              (lambda (p)
                (let ([x (vector-ref p 0)]
                      [y (vector-ref p 1)])
                  (and (not (and (= x 0) (= y 0)))      ; Exclude z-axis
                       (not (and (<= x 0) (= y 0))))))
              ;; Coord map: (x,y,z) → (ρ, φ, z)
              (lambda (p)
                (let ([x (vector-ref p 0)]
                      [y (vector-ref p 1)]
                      [z (vector-ref p 2)])
                  (vector (sqrt (+ (* x x) (* y y)))
                          (atan y x)
                          z)))
              ;; Inverse: (ρ, φ, z) → (x, y, z)
              (lambda (c)
                (let ([rho (vector-ref c 0)]
                      [phi (vector-ref c 1)]
                      [z (vector-ref c 2)])
                  (vector (* rho (cos phi))
                          (* rho (sin phi))
                          z)))))

;;; ====
;;; Utilities
;;; ====

;;; for-all : (a → Bool) × (List a) → Bool
(define (for-all pred lst)
  (cond
   [(null? lst) #t]
   [(not (pred (car lst))) #f]
   [else (for-all pred (cdr lst))]))

;;; vec-copy : Vec → Vec
;;; Copy a vector (if not already defined)
(define (vec-copy v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
        (vector-set! result i (vector-ref v i)))))

;;; ====
;;; REPL Interface
;;; ====

(printf "charts.ss loaded — Coordinate Charts and Atlases\n")
(printf "  (make-chart name dim domain coord-map inverse) - Create chart\n")
(printf "  (make-atlas name charts)                       - Create atlas\n")
(printf "  (chart-apply chart point)                      - Get coordinates\n")
(printf "  (transition-apply from to coords)              - Transform coords\n")
(printf "  (transition-jacobian from to coords)           - Jacobian matrix\n")
(printf "  (make-polar-chart), (make-spherical-chart)     - Standard charts\n")
