;;; lattice/numeric/fem.ss --- Finite Element Method for 2D PDEs
;;; @module fem
;;; @requires prelude linalg/vec linalg/matrix linalg/sparse linalg/iterative-solvers geometry/mesh-gen

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/sparse.ss")
(load "lattice/linalg/iterative-solvers.ss")
(load "lattice/geometry/mesh-gen.ss")

(doc 'module 'fem)
(doc 'description "Finite Element Method: P1 elements on triangular meshes for elliptic PDEs")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Section: FEM Mesh Structure
;;; ============================================================
;;;
;;; The FEM mesh wraps raw triangles with indexed nodes and elements.
;;; This enables efficient assembly of global matrices.

(doc 'section 'mesh)

(doc make-fem-mesh 'type '(-> (List Triangle2) FEMMesh))
(doc make-fem-mesh 'description "Convert Delaunay triangulation to indexed FEM mesh")
(define (make-fem-mesh triangles)
  ;; Extract unique nodes with tolerance-based deduplication
  (define tolerance 1e-10)

  (define (point-key p)
    ;; Round to tolerance for hashing
    (cons (round (/ (point2-x p) tolerance))
          (round (/ (point2-y p) tolerance))))

  ;; Collect all unique points
  (define all-points
    (apply append (map tri2-points triangles)))

  ;; Build node list (unique points) and index map
  (define-values (nodes node-map)
    (let loop ([pts all-points] [nodes '()] [idx 0] [seen '()])
      (if (null? pts)
          (values (reverse nodes) seen)
          (let* ([p (car pts)]
                 [key (point-key p)]
                 [existing (assoc key seen)])
            (if existing
                (loop (cdr pts) nodes idx seen)
                (loop (cdr pts)
                      (cons p nodes)
                      (+ idx 1)
                      (cons (cons key idx) seen)))))))

  ;; Convert triangles to element index triples
  (define (point->index p)
    (cdr (assoc (point-key p) node-map)))

  (define elements
    (map (lambda (tri)
           (vector (point->index (tri2-p1 tri))
                   (point->index (tri2-p2 tri))
                   (point->index (tri2-p3 tri))))
         triangles))

  (list 'fem-mesh
        (list->vector nodes)    ; node coordinates
        (list->vector elements) ; element connectivity
        (length nodes)          ; num-nodes
        (length elements)))     ; num-elements

(define (fem-mesh? m) (and (pair? m) (eq? (car m) 'fem-mesh)))
(define (fem-mesh-nodes m) (list-ref m 1))
(define (fem-mesh-elements m) (list-ref m 2))
(define (fem-mesh-num-nodes m) (list-ref m 3))
(define (fem-mesh-num-elements m) (list-ref m 4))

(doc fem-mesh-node 'type '(-> FEMMesh Nat Point2))
(doc fem-mesh-node 'description "Get node coordinates by index")
(define (fem-mesh-node mesh i)
  (vector-ref (fem-mesh-nodes mesh) i))

(doc fem-mesh-element 'type '(-> FEMMesh Nat (Vector Nat Nat Nat)))
(doc fem-mesh-element 'description "Get element node indices")
(define (fem-mesh-element mesh e)
  (vector-ref (fem-mesh-elements mesh) e))

(doc fem-mesh-element-nodes 'type '(-> FEMMesh Nat (List Point2)))
(doc fem-mesh-element-nodes 'description "Get element node coordinates")
(define (fem-mesh-element-nodes mesh e)
  (let ([elem (fem-mesh-element mesh e)]
        [nodes (fem-mesh-nodes mesh)])
    (list (vector-ref nodes (vector-ref elem 0))
          (vector-ref nodes (vector-ref elem 1))
          (vector-ref nodes (vector-ref elem 2)))))

;;; ============================================================
;;; Section: P1 Linear Basis Functions
;;; ============================================================
;;;
;;; P1 elements use linear basis functions on triangles.
;;; The basis function φ_i equals 1 at node i and 0 at other nodes.
;;; These are the barycentric coordinates.

(doc 'section 'basis)

(doc element-area 'type '(-> Point2 Point2 Point2 Number))
(doc element-area 'description "Compute signed area of triangle element")
(define (element-area p1 p2 p3)
  (let ([x1 (point2-x p1)] [y1 (point2-y p1)]
        [x2 (point2-x p2)] [y2 (point2-y p2)]
        [x3 (point2-x p3)] [y3 (point2-y p3)])
    (* 0.5 (- (* (- x2 x1) (- y3 y1))
              (* (- x3 x1) (- y2 y1))))))

(doc basis-gradients 'type '(-> Point2 Point2 Point2 (List (Vector Number Number))))
(doc basis-gradients 'description "Compute gradients of P1 basis functions on element")
(define (basis-gradients p1 p2 p3)
  ;; For P1 elements, gradients are constant over the element.
  ;; ∇φ_i = (1/2A) * [y_j - y_k, x_k - x_j] for cyclic (i,j,k)
  (let* ([x1 (point2-x p1)] [y1 (point2-y p1)]
         [x2 (point2-x p2)] [y2 (point2-y p2)]
         [x3 (point2-x p3)] [y3 (point2-y p3)]
         [area2 (* 2.0 (element-area p1 p2 p3))]
         ;; Gradient of φ_1 (opposite to edge 2-3)
         [grad1 (vector (/ (- y2 y3) area2) (/ (- x3 x2) area2))]
         ;; Gradient of φ_2 (opposite to edge 3-1)
         [grad2 (vector (/ (- y3 y1) area2) (/ (- x1 x3) area2))]
         ;; Gradient of φ_3 (opposite to edge 1-2)
         [grad3 (vector (/ (- y1 y2) area2) (/ (- x2 x1) area2))])
    (list grad1 grad2 grad3)))

;;; ============================================================
;;; Section: Local Element Matrices
;;; ============================================================
;;;
;;; The stiffness matrix K measures how much neighboring nodes
;;; influence each other through the Laplacian operator.
;;; The mass matrix M measures the "overlap" of basis functions.

(doc 'section 'element-matrices)

(doc element-stiffness 'type '(-> Point2 Point2 Point2 Matrix))
(doc element-stiffness 'description "Compute 3x3 local stiffness matrix for P1 element")
(define (element-stiffness p1 p2 p3)
  ;; K_ij = ∫_Ω ∇φ_i · ∇φ_j dΩ = A * (∇φ_i · ∇φ_j)
  ;; For P1 elements, gradients are constant, so integral is trivial.
  (let* ([area (abs (element-area p1 p2 p3))]
         [grads (basis-gradients p1 p2 p3)]
         [g1 (list-ref grads 0)]
         [g2 (list-ref grads 1)]
         [g3 (list-ref grads 2)])
    (define (dot g-a g-b)
      (+ (* (vector-ref g-a 0) (vector-ref g-b 0))
         (* (vector-ref g-a 1) (vector-ref g-b 1))))
    ;; Build 3x3 element stiffness
    (matrix-from-lists
     (list (list (* area (dot g1 g1)) (* area (dot g1 g2)) (* area (dot g1 g3)))
           (list (* area (dot g2 g1)) (* area (dot g2 g2)) (* area (dot g2 g3)))
           (list (* area (dot g3 g1)) (* area (dot g3 g2)) (* area (dot g3 g3)))))))

(doc element-mass 'type '(-> Point2 Point2 Point2 Matrix))
(doc element-mass 'description "Compute 3x3 local mass matrix for P1 element")
(define (element-mass p1 p2 p3)
  ;; M_ij = ∫_Ω φ_i φ_j dΩ
  ;; For P1 elements: M_ii = A/6, M_ij = A/12 (i≠j)
  (let ([area (abs (element-area p1 p2 p3))])
    (matrix-from-lists
     (list (list (/ area 6.0)  (/ area 12.0) (/ area 12.0))
           (list (/ area 12.0) (/ area 6.0)  (/ area 12.0))
           (list (/ area 12.0) (/ area 12.0) (/ area 6.0))))))

(doc element-load 'type '(-> Point2 Point2 Point2 (-> Number Number Number) Vector))
(doc element-load 'description "Compute 3-element local load vector for source term f(x,y)")
(define (element-load p1 p2 p3 f)
  ;; F_i = ∫_Ω f φ_i dΩ ≈ (A/3) * f(centroid) for all i (equal weights)
  ;; More accurate: use quadrature, but this is fine for smooth f.
  (let* ([area (abs (element-area p1 p2 p3))]
         [cx (/ (+ (point2-x p1) (point2-x p2) (point2-x p3)) 3.0)]
         [cy (/ (+ (point2-y p1) (point2-y p2) (point2-y p3)) 3.0)]
         [f-val (f cx cy)]
         [contrib (/ (* area f-val) 3.0)])
    (vector contrib contrib contrib)))

;;; ============================================================
;;; Section: Global Assembly
;;; ============================================================
;;;
;;; Assemble local element matrices into global sparse system.
;;; This is the heart of FEM - the connectivity determines sparsity.

(doc 'section 'assembly)

(doc assemble-stiffness 'type '(-> FEMMesh SparseCOO))
(doc assemble-stiffness 'description "Assemble global stiffness matrix in COO format")
(define (assemble-stiffness mesh)
  (let* ([n (fem-mesh-num-nodes mesh)]
         [ne (fem-mesh-num-elements mesh)]
         [triplets '()])
    ;; Loop over elements
    (do ([e 0 (+ e 1)])
        ((= e ne))
      (let* ([elem (fem-mesh-element mesh e)]
             [pts (fem-mesh-element-nodes mesh e)]
             [Ke (element-stiffness (car pts) (cadr pts) (caddr pts))]
             [i0 (vector-ref elem 0)]
             [i1 (vector-ref elem 1)]
             [i2 (vector-ref elem 2)]
             [indices (vector i0 i1 i2)])
        ;; Add contributions to global matrix
        (do ([li 0 (+ li 1)])
            ((= li 3))
          (do ([lj 0 (+ lj 1)])
              ((= lj 3))
            (let ([gi (vector-ref indices li)]
                  [gj (vector-ref indices lj)]
                  [val (matrix-ref Ke li lj)])
              (set! triplets (cons (list gi gj val) triplets)))))))
    ;; Convert to sparse COO
    (sparse-coo-from-triplets n n triplets)))

(doc assemble-mass 'type '(-> FEMMesh SparseCOO))
(doc assemble-mass 'description "Assemble global mass matrix in COO format")
(define (assemble-mass mesh)
  (let* ([n (fem-mesh-num-nodes mesh)]
         [ne (fem-mesh-num-elements mesh)]
         [triplets '()])
    (do ([e 0 (+ e 1)])
        ((= e ne))
      (let* ([elem (fem-mesh-element mesh e)]
             [pts (fem-mesh-element-nodes mesh e)]
             [Me (element-mass (car pts) (cadr pts) (caddr pts))]
             [i0 (vector-ref elem 0)]
             [i1 (vector-ref elem 1)]
             [i2 (vector-ref elem 2)]
             [indices (vector i0 i1 i2)])
        (do ([li 0 (+ li 1)])
            ((= li 3))
          (do ([lj 0 (+ lj 1)])
              ((= lj 3))
            (let ([gi (vector-ref indices li)]
                  [gj (vector-ref indices lj)]
                  [val (matrix-ref Me li lj)])
              (set! triplets (cons (list gi gj val) triplets)))))))
    (sparse-coo-from-triplets n n triplets)))

(doc assemble-load 'type '(-> FEMMesh (-> Number Number Number) Vector))
(doc assemble-load 'description "Assemble global load vector for source term f(x,y)")
(define (assemble-load mesh f)
  (let* ([n (fem-mesh-num-nodes mesh)]
         [ne (fem-mesh-num-elements mesh)]
         [F (make-vector n 0.0)])
    (do ([e 0 (+ e 1)])
        ((= e ne))
      (let* ([elem (fem-mesh-element mesh e)]
             [pts (fem-mesh-element-nodes mesh e)]
             [Fe (element-load (car pts) (cadr pts) (caddr pts) f)]
             [i0 (vector-ref elem 0)]
             [i1 (vector-ref elem 1)]
             [i2 (vector-ref elem 2)])
        (vector-set! F i0 (+ (vector-ref F i0) (vector-ref Fe 0)))
        (vector-set! F i1 (+ (vector-ref F i1) (vector-ref Fe 1)))
        (vector-set! F i2 (+ (vector-ref F i2) (vector-ref Fe 2)))))
    F))

;;; ============================================================
;;; Section: Boundary Conditions
;;; ============================================================
;;;
;;; Dirichlet BCs: fix solution values at boundary nodes.
;;; We modify the system to enforce u = g at boundary.

(doc 'section 'boundary)

(doc find-boundary-nodes 'type '(-> FEMMesh (List Nat)))
(doc find-boundary-nodes 'description "Find indices of nodes on mesh boundary")
(define (find-boundary-nodes mesh)
  ;; A boundary edge appears in exactly one triangle.
  ;; Boundary nodes are endpoints of boundary edges.
  (let* ([ne (fem-mesh-num-elements mesh)]
         [edge-counts (make-hashtable equal-hash equal?)])
    ;; Count edge occurrences
    (do ([e 0 (+ e 1)])
        ((= e ne))
      (let* ([elem (fem-mesh-element mesh e)]
             [i0 (vector-ref elem 0)]
             [i1 (vector-ref elem 1)]
             [i2 (vector-ref elem 2)]
             ;; Edges as sorted pairs for canonical form
             [e01 (if (< i0 i1) (cons i0 i1) (cons i1 i0))]
             [e12 (if (< i1 i2) (cons i1 i2) (cons i2 i1))]
             [e20 (if (< i2 i0) (cons i2 i0) (cons i0 i2))])
        (hashtable-update! edge-counts e01 (lambda (c) (+ c 1)) 0)
        (hashtable-update! edge-counts e12 (lambda (c) (+ c 1)) 0)
        (hashtable-update! edge-counts e20 (lambda (c) (+ c 1)) 0)))
    ;; Collect boundary nodes
    (let ([boundary-set (make-hashtable equal-hash equal?)])
      (vector-for-each
       (lambda (edge)
         (when (= 1 (hashtable-ref edge-counts edge 0))
           (hashtable-set! boundary-set (car edge) #t)
           (hashtable-set! boundary-set (cdr edge) #t)))
       (hashtable-keys edge-counts))
      (sort < (vector->list (hashtable-keys boundary-set))))))

(doc apply-dirichlet-bc! 'type '(-> SparseCOO Vector (List Nat) (-> Number Number Number) Void))
(doc apply-dirichlet-bc! 'description "Apply Dirichlet BC by modifying matrix and RHS in place")
(define (apply-dirichlet-bc! K F boundary-nodes g mesh)
  ;; For each boundary node i with u_i = g(x_i, y_i):
  ;; 1. Set row i of K to zero except K_ii = 1
  ;; 2. Set F_i = g(x_i, y_i)
  ;; 3. Modify other rows: F_j -= K_ji * g_i, K_ji = 0
  ;;
  ;; For efficiency with COO, we'll build a new triplet list.
  ;; This is the "big number" method alternative: set K_ii = BIG, F_i = BIG * g

  (let* ([n (sparse-coo-rows K)]
         [bc-set (make-hashtable equal-hash equal?)]
         [bc-values (make-vector n 0.0)])

    ;; Mark boundary nodes and compute their values
    (for-each
     (lambda (i)
       (let ([p (fem-mesh-node mesh i)])
         (hashtable-set! bc-set i #t)
         (vector-set! bc-values i (g (point2-x p) (point2-y p)))))
     boundary-nodes)

    ;; Use penalty method: K_ii = 1e30, F_i = 1e30 * g_i
    ;; This preserves sparsity pattern and is simple.
    (let ([big 1e30])
      (for-each
       (lambda (i)
         ;; We need to add a large diagonal entry
         ;; Sparse COO allows duplicates that get summed
         (let ([rows (sparse-coo-row-indices K)]
               [cols (sparse-coo-col-indices K)]
               [vals (sparse-coo-values K)])
           ;; Zero out row i contributions (set to near-zero)
           (do ([k 0 (+ k 1)])
               ((= k (vector-length rows)))
             (when (= (vector-ref rows k) i)
               (vector-set! vals k 0.0)))
           ;; Zero out column i contributions and adjust RHS
           (do ([k 0 (+ k 1)])
               ((= k (vector-length rows)))
             (let ([row (vector-ref rows k)]
                   [col (vector-ref cols k)]
                   [val (vector-ref vals k)])
               (when (and (= col i) (not (hashtable-ref bc-set row #f)))
                 ;; Subtract contribution from RHS
                 (vector-set! F row (- (vector-ref F row)
                                       (* val (vector-ref bc-values i))))
                 (vector-set! vals k 0.0))))))
       boundary-nodes)

      ;; Set boundary values in RHS
      (for-each
       (lambda (i)
         (vector-set! F i (vector-ref bc-values i)))
       boundary-nodes))))

(doc apply-dirichlet-penalty! 'type '(-> SparseCOO Vector (List Nat) (-> Number Number Number) FEMMesh Void))
(doc apply-dirichlet-penalty! 'description "Apply Dirichlet BC using penalty method (simpler, preserves symmetry)")
(define (apply-dirichlet-penalty! K F boundary-nodes g mesh)
  ;; Penalty method: Add large value to diagonal, scale RHS accordingly
  ;; K_ii += penalty, F_i = penalty * g_i
  ;; This approximately enforces u_i ≈ g_i when penalty >> other entries
  ;;
  ;; IMPORTANT: COO may have duplicate diagonal entries from assembly.
  ;; We must add penalty to exactly ONE occurrence, not all of them,
  ;; since duplicates get summed during COO→CSR conversion.
  (let ([penalty 1e15])
    (for-each
     (lambda (i)
       (let* ([p (fem-mesh-node mesh i)]
              [g-val (g (point2-x p) (point2-y p))]
              [rows (sparse-coo-row-indices K)]
              [cols (sparse-coo-col-indices K)]
              [vals (sparse-coo-values K)]
              [nnz (vector-length rows)])
         ;; Find FIRST diagonal entry and modify only that one
         (let loop ([k 0])
           (cond
             [(= k nnz) (void)]  ; No diagonal found (shouldn't happen)
             [(and (= (vector-ref rows k) i) (= (vector-ref cols k) i))
              ;; Found first diagonal - add penalty and stop
              (vector-set! vals k (+ (vector-ref vals k) penalty))]
             [else (loop (+ k 1))]))
         ;; Set RHS
         (vector-set! F i (* penalty g-val))))
     boundary-nodes)))

;;; ============================================================
;;; Section: Sparse Conjugate Gradient
;;; ============================================================
;;;
;;; FEM systems are large and sparse - we need a sparse-aware CG solver.
;;; This implements CG that operates directly on CSR matrices.

(doc 'section 'sparse-cg)

(define *fem-tolerance* 1e-10)
(define *fem-max-iterations* 10000)

(doc sparse-cg 'type '(-> SparseCSR Vector Vector (List Vector Number Nat)))
(doc sparse-cg 'description "Conjugate gradient for sparse CSR matrices")
(define (sparse-cg A b x0)
  ;; CG for Ax = b where A is sparse CSR
  (let* ([n (sparse-csr-rows A)]
         [tol *fem-tolerance*]
         [max-iter (min *fem-max-iterations* (* 2 n))]
         ;; r = b - Ax
         [Ax0 (sparse-csr-vec-mul A x0)]
         [r (vec-sub b Ax0)]
         [p (vector-copy r)]
         [rr (vec-dot r r)])
    (if (< (sqrt rr) tol)
        (list x0 (sqrt rr) 0)
        (sparse-cg-loop A b (vector-copy x0) r p rr 0 max-iter tol n))))

(define (sparse-cg-loop A b x r p rr iter max-iter tol n)
  (let ([r-norm (sqrt rr)])
    (cond
      [(< r-norm tol)
       (list x r-norm iter)]
      [(>= iter max-iter)
       ;; Return best solution found, not error
       (list x r-norm iter)]
      [else
       (let* ([Ap (sparse-csr-vec-mul A p)]
              [pAp (vec-dot p Ap)])
         (if (< (abs pAp) 1e-30)
             ;; Breakdown - return current solution
             (list x r-norm iter)
             (let* ([alpha (/ rr pAp)]
                    [x-new (vec-add x (vec-scale alpha p))]
                    [r-new (vec-sub r (vec-scale alpha Ap))]
                    [rr-new (vec-dot r-new r-new)]
                    [beta (/ rr-new rr)]
                    [p-new (vec-add r-new (vec-scale beta p))])
               (sparse-cg-loop A b x-new r-new p-new rr-new
                               (+ iter 1) max-iter tol n))))])))

;;; Vector operations for CG (if not already available)
(define (vec-dot v1 v2)
  (let ([n (vector-length v1)])
    (let loop ([i 0] [sum 0.0])
      (if (= i n)
          sum
          (loop (+ i 1) (+ sum (* (vector-ref v1 i) (vector-ref v2 i))))))))

(define (vec-add v1 v2)
  (let* ([n (vector-length v1)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (+ (vector-ref v1 i) (vector-ref v2 i))))))

(define (vec-sub v1 v2)
  (let* ([n (vector-length v1)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (- (vector-ref v1 i) (vector-ref v2 i))))))

(define (vec-scale s v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (* s (vector-ref v i))))))

;;; ============================================================
;;; Section: FEM Solver
;;; ============================================================

(doc 'section 'solver)

(doc fem-solve-poisson 'type '(-> FEMMesh (-> Number Number Number) (-> Number Number Number) Vector))
(doc fem-solve-poisson 'description "Solve -∇²u = f with u = g on boundary")
(define (fem-solve-poisson mesh f g)
  ;; Assemble system
  (let* ([K (assemble-stiffness mesh)]
         [F (assemble-load mesh f)]
         [boundary (find-boundary-nodes mesh)])
    ;; Apply boundary conditions
    (apply-dirichlet-penalty! K F boundary g mesh)
    ;; Convert to CSR for efficient solving
    (let* ([K-csr (coo->csr K)]
           [n (fem-mesh-num-nodes mesh)]
           [u0 (make-vector n 0.0)]
           ;; Solve with sparse conjugate gradient
           [result (sparse-cg K-csr F u0)])
      (car result))))  ; Return solution vector

(doc fem-solve-poisson-full 'type '(-> FEMMesh (-> Number Number Number) (-> Number Number Number) (Values Vector Number Nat)))
(doc fem-solve-poisson-full 'description "Solve Poisson and return (solution residual iterations)")
(define (fem-solve-poisson-full mesh f g)
  (let* ([K (assemble-stiffness mesh)]
         [F (assemble-load mesh f)]
         [boundary (find-boundary-nodes mesh)])
    (apply-dirichlet-penalty! K F boundary g mesh)
    (let* ([K-csr (coo->csr K)]
           [n (fem-mesh-num-nodes mesh)]
           [u0 (make-vector n 0.0)]
           [result (sparse-cg K-csr F u0)])
      (values (car result)      ; solution
              (cadr result)     ; residual
              (caddr result)))));iterations

;;; ============================================================
;;; Section: Solution Visualization
;;; ============================================================

(doc 'section 'visualization)

(doc fem-solution-at 'type '(-> FEMMesh Vector Number Number Number))
(doc fem-solution-at 'description "Interpolate FEM solution at point (x,y)")
(define (fem-solution-at mesh solution x y)
  ;; Find containing element and interpolate using basis functions
  (let ([ne (fem-mesh-num-elements mesh)])
    (let loop ([e 0])
      (if (= e ne)
          0.0  ; Point not in mesh
          (let* ([pts (fem-mesh-element-nodes mesh e)]
                 [p1 (car pts)]
                 [p2 (cadr pts)]
                 [p3 (caddr pts)]
                 ;; Compute barycentric coordinates
                 [area (element-area p1 p2 p3)]
                 [area1 (element-area (make-point2 x y) p2 p3)]
                 [area2 (element-area p1 (make-point2 x y) p3)]
                 [area3 (element-area p1 p2 (make-point2 x y))]
                 [l1 (/ area1 area)]
                 [l2 (/ area2 area)]
                 [l3 (/ area3 area)])
            ;; Check if point is inside (all barycentric coords non-negative)
            (if (and (>= l1 -1e-10) (>= l2 -1e-10) (>= l3 -1e-10))
                ;; Interpolate
                (let* ([elem (fem-mesh-element mesh e)]
                       [u1 (vector-ref solution (vector-ref elem 0))]
                       [u2 (vector-ref solution (vector-ref elem 1))]
                       [u3 (vector-ref solution (vector-ref elem 2))])
                  (+ (* l1 u1) (* l2 u2) (* l3 u3)))
                (loop (+ e 1))))))))

(doc fem-render-solution 'type '(-> FEMMesh Vector Nat Nat String))
(doc fem-render-solution 'description "Render FEM solution as ASCII heatmap")
(define (fem-render-solution mesh solution width height)
  (let* ([nodes (fem-mesh-nodes mesh)]
         [n (fem-mesh-num-nodes mesh)]
         [xs (let loop ([i 0] [acc '()])
               (if (= i n) acc
                   (loop (+ i 1) (cons (point2-x (vector-ref nodes i)) acc))))]
         [ys (let loop ([i 0] [acc '()])
               (if (= i n) acc
                   (loop (+ i 1) (cons (point2-y (vector-ref nodes i)) acc))))]
         [x-min (apply min xs)]
         [x-max (apply max xs)]
         [y-min (apply min ys)]
         [y-max (apply max ys)]
         [u-min (let loop ([i 0] [m +inf.0])
                  (if (= i n) m
                      (loop (+ i 1) (min m (vector-ref solution i)))))]
         [u-max (let loop ([i 0] [m -inf.0])
                  (if (= i n) m
                      (loop (+ i 1) (max m (vector-ref solution i)))))]
         [u-range (max 1e-10 (- u-max u-min))]
         [chars " ░▒▓█"]
         [nchars (string-length chars)])
    (let loop-y ([j 0] [lines '()])
      (if (= j height)
          (apply string-append (reverse lines))
          (let ([y (+ y-min (* (- y-max y-min) (/ (- height 1 j) (- height 1))))])
            (let loop-x ([i 0] [row '()])
              (if (= i width)
                  (loop-y (+ j 1) (cons (string-append (list->string (reverse row)) "\n") lines))
                  (let* ([x (+ x-min (* (- x-max x-min) (/ i (- width 1))))]
                         [u (fem-solution-at mesh solution x y)]
                         [level (min (- nchars 1)
                                     (max 0 (inexact->exact
                                             (floor (* nchars (/ (- u u-min) u-range))))))])
                    (loop-x (+ i 1) (cons (string-ref chars level) row))))))))))

;;; ============================================================
;;; Section: Convenience Functions
;;; ============================================================

(doc 'section 'convenience)

(doc make-unit-square-mesh 'type '(-> Nat FEMMesh))
(doc make-unit-square-mesh 'description "Create mesh on unit square [0,1]² with n² points")
(define (make-unit-square-mesh n)
  ;; Generate regular grid points
  (let* ([h (/ 1.0 (- n 1))]
         [pts (let loop-i ([i 0] [acc '()])
                (if (= i n)
                    acc
                    (let loop-j ([j 0] [row-acc acc])
                      (if (= j n)
                          (loop-i (+ i 1) row-acc)
                          (loop-j (+ j 1)
                                  (cons (make-point2 (* i h) (* j h)) row-acc))))))]
         [triangles (delaunay-triangulate pts)])
    (make-fem-mesh triangles)))

(doc make-disk-mesh 'type '(-> Number Nat FEMMesh))
(doc make-disk-mesh 'description "Create mesh on disk of given radius with n points")
(define (make-disk-mesh radius n)
  ;; Generate points in disk using rejection sampling + boundary points
  (let* ([boundary-n (max 8 (inexact->exact (round (sqrt n))))]
         [boundary-pts (let loop ([i 0] [acc '()])
                         (if (= i boundary-n)
                             acc
                             (let ([theta (* 2.0 3.141592653589793 (/ i boundary-n))])
                               (loop (+ i 1)
                                     (cons (make-point2 (* radius (cos theta))
                                                        (* radius (sin theta)))
                                           acc)))))]
         [interior-n (- n boundary-n)]
         [interior-pts (let loop ([i 0] [acc '()])
                         (if (>= i interior-n)
                             acc
                             (let* ([r (* radius (sqrt (/ (random 10000) 10000.0)))]
                                    [theta (* 2.0 3.141592653589793 (/ (random 10000) 10000.0))])
                               (loop (+ i 1)
                                     (cons (make-point2 (* r (cos theta))
                                                        (* r (sin theta)))
                                           acc)))))]
         [all-pts (append boundary-pts interior-pts)]
         [triangles (delaunay-triangulate all-pts)])
    (make-fem-mesh triangles)))

(doc fem-l2-error 'type '(-> FEMMesh Vector (-> Number Number Number) Number))
(doc fem-l2-error 'description "Compute L² error between FEM solution and exact solution")
(define (fem-l2-error mesh solution exact)
  ;; ∫(u - u_exact)² dΩ ≈ Σ_e (A_e/3) Σ_i (u_i - exact(x_i, y_i))²
  (let ([ne (fem-mesh-num-elements mesh)]
        [error-sq 0.0])
    (do ([e 0 (+ e 1)])
        ((= e ne) (sqrt error-sq))
      (let* ([pts (fem-mesh-element-nodes mesh e)]
             [elem (fem-mesh-element mesh e)]
             [area (abs (element-area (car pts) (cadr pts) (caddr pts)))])
        (do ([i 0 (+ i 1)])
            ((= i 3))
          (let* ([p (list-ref pts i)]
                 [idx (vector-ref elem i)]
                 [u-fem (vector-ref solution idx)]
                 [u-exact (exact (point2-x p) (point2-y p))]
                 [diff (- u-fem u-exact)])
            (set! error-sq (+ error-sq (* (/ area 3.0) (* diff diff))))))))))

(printf "fem.ss loaded~n")
(printf "  (make-fem-mesh triangles)           - Create indexed FEM mesh~n")
(printf "  (fem-solve-poisson mesh f g)        - Solve -∇²u = f, u|∂Ω = g~n")
(printf "  (fem-render-solution mesh u w h)    - ASCII heatmap~n")
(printf "  (make-unit-square-mesh n)           - Unit square mesh~n")
(printf "  (make-disk-mesh r n)                - Disk mesh~n")
