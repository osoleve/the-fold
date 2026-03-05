;;; lattice/pde/fem.ss --- Finite Element Method for 2D PDEs
;;; @module fem
;;; @requires prelude sort vec matrix sparse iterative-solvers mesh-gen hamt iteration

(require 'prelude)
(require 'sort)
(require 'vec)
(require 'matrix)
(require 'sparse)
(require 'iterative-solvers)
(require 'mesh-gen)
(require 'hamt)
(require 'iteration)

(doc 'module 'fem)
(doc 'description "Finite Element Method: P1 elements on triangular meshes for elliptic PDEs")
(doc 'layer 'lattice)
(doc 'purity 'partial)

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
  (doc 'export #t)
  ;; Extract unique nodes with tolerance-based deduplication
  ;; Uses HAMT for O(log32 n) lookup instead of O(N) assoc
  (let* ([tolerance 1e-10]
         [point-key (lambda (p)
                      ;; Round to tolerance for hashing
                      (cons (round (/ (point2-x p) tolerance))
                            (round (/ (point2-y p) tolerance))))]
         ;; Collect all unique points
         [all-points (append-map tri2-points triangles)]
         ;; Build node list and index map using threaded HAMT
         [nodes-vec (make-vector (length all-points) #f)]  ; Upper bound
         ;; Thread HAMT and counter through fold
         [result
          (fold-left
           (lambda (acc p)
             (let ([key (point-key p)]
                   [node-map (car acc)]
                   [idx (cdr acc)])
               (if (hamt-has-key? key node-map)
                   acc
                   (begin
                     (vector-set! nodes-vec idx p)
                     (cons (hamt-assoc key idx node-map)
                           (+ idx 1))))))
           (cons hamt-empty 0)
           all-points)]
         [node-map (car result)]
         [node-count (cdr result)])

    (let* ([;; Trim nodes vector to actual size
            nodes (vec-tabulate node-count i (vector-ref nodes-vec i))]
           ;; Convert triangles to element index triples
           [point->index (lambda (p) (hamt-lookup (point-key p) node-map))]
           [elements (map (lambda (tri)
                            (vector (point->index (tri2-p1 tri))
                                    (point->index (tri2-p2 tri))
                                    (point->index (tri2-p3 tri))))
                          triangles)])

      (list 'fem-mesh
            nodes                   ; node coordinates (vector)
            (list->vector elements) ; element connectivity
            node-count              ; num-nodes
            (length elements)))))   ; num-elements

(define (fem-mesh? m) (and (pair? m) (eq? (car m) 'fem-mesh)))
(define (fem-mesh-nodes m) (list-ref m 1))
(define (fem-mesh-elements m) (list-ref m 2))
(define (fem-mesh-num-nodes m) (list-ref m 3))
(define (fem-mesh-num-elements m) (list-ref m 4))

(doc fem-mesh-node 'type '(-> FEMMesh Nat Point2))
(doc fem-mesh-node 'description "Get node coordinates by index")
(define (fem-mesh-node mesh i)
  (doc 'export #t)
  (vector-ref (fem-mesh-nodes mesh) i))

(doc fem-mesh-element 'type '(-> FEMMesh Nat (Vector Nat Nat Nat)))
(doc fem-mesh-element 'description "Get element node indices")
(define (fem-mesh-element mesh e)
  (doc 'export #t)
  (vector-ref (fem-mesh-elements mesh) e))

(doc fem-mesh-element-nodes 'type '(-> FEMMesh Nat (List Point2)))
(doc fem-mesh-element-nodes 'description "Get element node coordinates")
(define (fem-mesh-element-nodes mesh e)
  (doc 'export #t)
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
  (doc 'export #t)
  (let ([x1 (point2-x p1)] [y1 (point2-y p1)]
        [x2 (point2-x p2)] [y2 (point2-y p2)]
        [x3 (point2-x p3)] [y3 (point2-y p3)])
    (* 0.5 (- (* (- x2 x1) (- y3 y1))
              (* (- x3 x1) (- y2 y1))))))

(doc basis-gradients 'type '(-> Point2 Point2 Point2 (List (Vector Number Number))))
(doc basis-gradients 'description "Compute gradients of P1 basis functions on element")
(define (basis-gradients p1 p2 p3)
  (doc 'export #t)
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
  (doc 'export #t)
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
  (doc 'export #t)
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
  (doc 'export #t)
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
  (doc 'export #t)
  (let ([n (fem-mesh-num-nodes mesh)]
        [ne (fem-mesh-num-elements mesh)])
    ;; Accumulate triplets over elements via named let
    (let loop-e ([e 0] [triplets '()])
      (if (= e ne)
          (sparse-coo-from-triplets n n triplets)
          (let* ([elem (fem-mesh-element mesh e)]
                 [pts (fem-mesh-element-nodes mesh e)]
                 [Ke (element-stiffness (car pts) (cadr pts) (caddr pts))]
                 [i0 (vector-ref elem 0)]
                 [i1 (vector-ref elem 1)]
                 [i2 (vector-ref elem 2)]
                 [indices (vector i0 i1 i2)])
            ;; Add contributions to global matrix
            (loop-e (+ e 1)
                    (let loop-i ([li 0] [triplets triplets])
                      (if (= li 3)
                          triplets
                          (loop-i (+ li 1)
                                  (let loop-j ([lj 0] [triplets triplets])
                                    (if (= lj 3)
                                        triplets
                                        (loop-j (+ lj 1)
                                                (cons (list (vector-ref indices li)
                                                            (vector-ref indices lj)
                                                            (matrix-ref Ke li lj))
                                                      triplets)))))))))))))

(doc assemble-mass 'type '(-> FEMMesh SparseCOO))
(doc assemble-mass 'description "Assemble global mass matrix in COO format")
(define (assemble-mass mesh)
  (doc 'export #t)
  (let ([n (fem-mesh-num-nodes mesh)]
        [ne (fem-mesh-num-elements mesh)])
    (let loop-e ([e 0] [triplets '()])
      (if (= e ne)
          (sparse-coo-from-triplets n n triplets)
          (let* ([elem (fem-mesh-element mesh e)]
                 [pts (fem-mesh-element-nodes mesh e)]
                 [Me (element-mass (car pts) (cadr pts) (caddr pts))]
                 [i0 (vector-ref elem 0)]
                 [i1 (vector-ref elem 1)]
                 [i2 (vector-ref elem 2)]
                 [indices (vector i0 i1 i2)])
            (loop-e (+ e 1)
                    (let loop-i ([li 0] [triplets triplets])
                      (if (= li 3)
                          triplets
                          (loop-i (+ li 1)
                                  (let loop-j ([lj 0] [triplets triplets])
                                    (if (= lj 3)
                                        triplets
                                        (loop-j (+ lj 1)
                                                (cons (list (vector-ref indices li)
                                                            (vector-ref indices lj)
                                                            (matrix-ref Me li lj))
                                                      triplets)))))))))))))

(doc assemble-load 'type '(-> FEMMesh (-> Number Number Number) Vector))
(doc assemble-load 'description "Assemble global load vector for source term f(x,y)")
(define (assemble-load mesh f)
  (doc 'export #t)
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
  (doc 'export #t)
  ;; A boundary edge appears in exactly one triangle.
  ;; Boundary nodes are endpoints of boundary edges.
  (let* ([ne (fem-mesh-num-elements mesh)]
         ;; Count edge occurrences using threaded HAMT
         [edge-counts
          (let loop ([e 0] [ec hamt-empty])
            (if (= e ne)
                ec
                (let* ([elem (fem-mesh-element mesh e)]
                       [i0 (vector-ref elem 0)]
                       [i1 (vector-ref elem 1)]
                       [i2 (vector-ref elem 2)]
                       ;; Edges as sorted pairs for canonical form
                       [e01 (if (< i0 i1) (cons i0 i1) (cons i1 i0))]
                       [e12 (if (< i1 i2) (cons i1 i2) (cons i2 i1))]
                       [e20 (if (< i2 i0) (cons i2 i0) (cons i0 i2))]
                       [ec (hamt-assoc e01 (+ (hamt-lookup-or e01 ec 0) 1) ec)]
                       [ec (hamt-assoc e12 (+ (hamt-lookup-or e12 ec 0) 1) ec)]
                       [ec (hamt-assoc e20 (+ (hamt-lookup-or e20 ec 0) 1) ec)])
                  (loop (+ e 1) ec))))]
         ;; Collect boundary nodes from edges with count = 1
         [boundary-set
          (hamt-fold
           (lambda (bset edge count)
             (if (= 1 count)
                 (hamt-assoc (cdr edge)
                             #t
                             (hamt-assoc (car edge) #t bset))
                 bset))
           hamt-empty
           edge-counts)])
    (sort-by < (hamt-keys boundary-set))))

(doc apply-dirichlet-bc! 'type '(-> SparseCOO Vector (List Nat) (-> Number Number Number) FEMMesh Void))
(doc apply-dirichlet-bc! 'description "Apply Dirichlet BC by elimination method (zeros row/col, sets diagonal to 1)")
(define (apply-dirichlet-bc! K F boundary-nodes g mesh)
  (doc 'export #t)
  ;; For each boundary node i with u_i = g(x_i, y_i):
  ;; 1. Zero row i of K (all entries)
  ;; 2. Zero column i of K, adjusting RHS: F_j -= K_ji * g_i
  ;; 3. Set K_ii = 1 (CRITICAL - without this, matrix is singular!)
  ;; 4. Set F_i = g(x_i, y_i)

  (let* ([n (sparse-coo-rows K)]
         [bc-set (fold-left (lambda (h i) (hamt-assoc i #t h))
                            hamt-empty boundary-nodes)]
         [bc-values (make-vector n 0.0)]
         [rows (sparse-coo-row-indices K)]
         [cols (sparse-coo-col-indices K)]
         [vals (sparse-coo-values K)]
         [nnz (vector-length rows)])

    ;; Compute boundary values
    (for-each
     (lambda (i)
       (let ([p (fem-mesh-node mesh i)])
         (vector-set! bc-values i (g (point2-x p) (point2-y p)))))
     boundary-nodes)

    ;; Process each boundary node
    (for-each
     (lambda (i)
       (let ([g-val (vector-ref bc-values i)]
             [diag-found #f])
         ;; Pass 1: Zero row i and find diagonal
         (do ([k 0 (+ k 1)])
             ((= k nnz))
           (when (= (vector-ref rows k) i)
             (if (= (vector-ref cols k) i)
                 ;; Diagonal entry: set to 1.0
                 (begin
                   (vector-set! vals k 1.0)
                   (set! diag-found #t))
                 ;; Off-diagonal in row: zero it
                 (vector-set! vals k 0.0))))

         ;; Pass 2: Zero column i (non-boundary rows) and adjust RHS
         (do ([k 0 (+ k 1)])
             ((= k nnz))
           (let ([row (vector-ref rows k)]
                 [col (vector-ref cols k)]
                 [val (vector-ref vals k)])
             (when (and (= col i)
                        (not (= row i))
                        (not (hamt-lookup-or row bc-set #f)))
               ;; Subtract contribution from RHS before zeroing
               (vector-set! F row (- (vector-ref F row) (* val g-val)))
               (vector-set! vals k 0.0))))

         ;; Set RHS for this boundary node
         (vector-set! F i g-val)))
     boundary-nodes)))

(doc apply-dirichlet-penalty! 'type '(-> SparseCOO Vector (List Nat) (-> Number Number Number) FEMMesh Void))
(doc apply-dirichlet-penalty! 'description "Apply Dirichlet BC using penalty method (simpler, preserves symmetry)")
(define (apply-dirichlet-penalty! K F boundary-nodes g mesh)
  (doc 'export #t)
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
  (doc 'export #t)
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
  (doc 'export #t)
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

;;; Preconditioned Conjugate Gradient (Jacobi)
;;;
;;; Jacobi preconditioning uses M = diag(A). This is simple but effective
;;; for FEM stiffness matrices, reducing condition number significantly.

(doc sparse-csr-diagonal 'type '(-> SparseCSR Vector))
(doc sparse-csr-diagonal 'description "Extract diagonal of sparse CSR matrix")
(define (sparse-csr-diagonal A)
  (doc 'export #t)
  (let* ([n (sparse-csr-rows A)]
         [row-ptrs (sparse-csr-row-ptrs A)]
         [col-indices (sparse-csr-col-indices A)]
         [vals (sparse-csr-values A)]
         [diag (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) diag)
      (let ([start (vector-ref row-ptrs i)]
            [end (vector-ref row-ptrs (+ i 1))])
        (do ([k start (+ k 1)])
            ((= k end))
          (when (= (vector-ref col-indices k) i)
            (vector-set! diag i (vector-ref vals k))))))))

(doc vec-div-pointwise 'type '(-> Vector Vector Vector))
(doc vec-div-pointwise 'description "Element-wise division z_i = x_i / y_i")
(define (vec-div-pointwise x y)
  (doc 'export #t)
  (vec-tabulate (vector-length x) i
    (let ([yi (vector-ref y i)])
      (if (< (abs yi) 1e-30)
          0.0
          (/ (vector-ref x i) yi)))))

(doc sparse-pcg 'type '(-> SparseCSR Vector Vector (List Vector Number Nat)))
(doc sparse-pcg 'description "Preconditioned conjugate gradient with Jacobi preconditioning")
(define (sparse-pcg A b x0)
  (doc 'export #t)
  ;; PCG for Ax = b where A is sparse CSR, M = diag(A)
  (let* ([n (sparse-csr-rows A)]
         [tol *fem-tolerance*]
         [max-iter (min *fem-max-iterations* (* 2 n))]
         ;; Jacobi preconditioner: M = diag(A)
         [M (sparse-csr-diagonal A)]
         ;; r = b - Ax
         [Ax0 (sparse-csr-vec-mul A x0)]
         [r (vec-sub b Ax0)]
         ;; z = M^{-1} r
         [z (vec-div-pointwise r M)]
         [p (vector-copy z)]
         [rz (vec-dot r z)])
    (if (< (sqrt (vec-dot r r)) tol)
        (list x0 (sqrt (vec-dot r r)) 0)
        (sparse-pcg-loop A b (vector-copy x0) r z p rz M 0 max-iter tol n))))

(define (sparse-pcg-loop A b x r z p rz M iter max-iter tol n)
  (doc 'export #t)
  (let ([r-norm (sqrt (vec-dot r r))])
    (cond
      [(< r-norm tol)
       (list x r-norm iter)]
      [(>= iter max-iter)
       (list x r-norm iter)]
      [else
       (let* ([Ap (sparse-csr-vec-mul A p)]
              [pAp (vec-dot p Ap)])
         (if (< (abs pAp) 1e-30)
             (list x r-norm iter)
             (let* ([alpha (/ rz pAp)]
                    [x-new (vec-add x (vec-scale alpha p))]
                    [r-new (vec-sub r (vec-scale alpha Ap))]
                    ;; z = M^{-1} r
                    [z-new (vec-div-pointwise r-new M)]
                    [rz-new (vec-dot r-new z-new)]
                    [beta (/ rz-new rz)]
                    [p-new (vec-add z-new (vec-scale beta p))])
               (sparse-pcg-loop A b x-new r-new z-new p-new rz-new M
                                (+ iter 1) max-iter tol n))))])))

;;; Vector operations for CG (if not already available)
(define (vec-dot v1 v2)
  (doc 'export #t)
  (let ([n (vector-length v1)])
    (let loop ([i 0] [sum 0.0])
      (if (= i n)
          sum
          (loop (+ i 1) (+ sum (* (vector-ref v1 i) (vector-ref v2 i))))))))

(define (vec-add v1 v2)
  (doc 'export #t)
  (vec-tabulate (vector-length v1) i
    (+ (vector-ref v1 i) (vector-ref v2 i))))

(define (vec-sub v1 v2)
  (doc 'export #t)
  (vec-tabulate (vector-length v1) i
    (- (vector-ref v1 i) (vector-ref v2 i))))

(define (vec-scale s v)
  (doc 'export #t)
  (vec-tabulate (vector-length v) i
    (* s (vector-ref v i))))

;;; ============================================================
;;; Section: FEM Solver
;;; ============================================================

(doc 'section 'solver)

(doc fem-solve-poisson 'type '(-> FEMMesh (-> Number Number Number) (-> Number Number Number) Vector))
(doc fem-solve-poisson 'description "Solve -∇²u = f with u = g on boundary")
(define (fem-solve-poisson mesh f g)
  (doc 'export #t)
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
           ;; Solve with preconditioned conjugate gradient (Jacobi)
           [result (sparse-pcg K-csr F u0)])
      (car result))))  ; Return solution vector

(doc fem-solve-poisson-full 'type '(-> FEMMesh (-> Number Number Number) (-> Number Number Number) (Values Vector Number Nat)))
(doc fem-solve-poisson-full 'description "Solve Poisson and return (solution residual iterations)")
(define (fem-solve-poisson-full mesh f g)
  (doc 'export #t)
  (let* ([K (assemble-stiffness mesh)]
         [F (assemble-load mesh f)]
         [boundary (find-boundary-nodes mesh)])
    (apply-dirichlet-penalty! K F boundary g mesh)
    (let* ([K-csr (coo->csr K)]
           [n (fem-mesh-num-nodes mesh)]
           [u0 (make-vector n 0.0)]
           ;; Use preconditioned CG for better convergence
           [result (sparse-pcg K-csr F u0)])
      (values (car result)      ; solution
              (cadr result)     ; residual
              (caddr result)))));iterations

;;; ============================================================
;;; Section: Spatial Index for Point Location
;;; ============================================================
;;;
;;; Grid-based spatial hash for O(1) average point location.
;;; Essential for efficient rendering and solution interpolation.

(doc 'section 'spatial-index)

(doc make-fem-spatial-index 'type '(-> FEMMesh Nat FEMSpatialIndex))
(doc make-fem-spatial-index 'description "Build grid-based spatial index for fast point location")
(define (make-fem-spatial-index mesh grid-size)
  (doc 'export #t)
  ;; Compute mesh bounding box
  (let* ([nodes (fem-mesh-nodes mesh)]
         [n (fem-mesh-num-nodes mesh)]
         [ne (fem-mesh-num-elements mesh)])
    ;; Find bounds
    (define x-min +inf.0) (define x-max -inf.0)
    (define y-min +inf.0) (define y-max -inf.0)
    (do ([i 0 (+ i 1)])
        ((= i n))
      (let ([p (vector-ref nodes i)])
        (set! x-min (min x-min (point2-x p)))
        (set! x-max (max x-max (point2-x p)))
        (set! y-min (min y-min (point2-y p)))
        (set! y-max (max y-max (point2-y p)))))
    ;; Add small padding to handle boundary points
    (let* ([pad 1e-10]
           [x-min (- x-min pad)] [x-max (+ x-max pad)]
           [y-min (- y-min pad)] [y-max (+ y-max pad)]
           [dx (/ (- x-max x-min) grid-size)]
           [dy (/ (- y-max y-min) grid-size)]
           ;; Grid cells: vector of lists of element indices
           [grid (make-vector (* grid-size grid-size) '())])
      ;; Insert each element into cells that its bounding box overlaps
      (do ([e 0 (+ e 1)])
          ((= e ne))
        (let* ([pts (fem-mesh-element-nodes mesh e)]
               [p1 (car pts)] [p2 (cadr pts)] [p3 (caddr pts)]
               [ex-min (min (point2-x p1) (point2-x p2) (point2-x p3))]
               [ex-max (max (point2-x p1) (point2-x p2) (point2-x p3))]
               [ey-min (min (point2-y p1) (point2-y p2) (point2-y p3))]
               [ey-max (max (point2-y p1) (point2-y p2) (point2-y p3))]
               ;; Grid cell range
               [ci-min (max 0 (inexact->exact (floor (/ (- ex-min x-min) dx))))]
               [ci-max (min (- grid-size 1) (inexact->exact (floor (/ (- ex-max x-min) dx))))]
               [cj-min (max 0 (inexact->exact (floor (/ (- ey-min y-min) dy))))]
               [cj-max (min (- grid-size 1) (inexact->exact (floor (/ (- ey-max y-min) dy))))])
          (do ([ci ci-min (+ ci 1)])
              ((> ci ci-max))
            (do ([cj cj-min (+ cj 1)])
                ((> cj cj-max))
              (let ([idx (+ (* cj grid-size) ci)])
                (vector-set! grid idx (cons e (vector-ref grid idx))))))))
      ;; Return index structure
      (list 'fem-spatial-index mesh grid grid-size
            x-min x-max y-min y-max dx dy))))

(define (fem-spatial-index? idx) (and (pair? idx) (eq? (car idx) 'fem-spatial-index)))
(define (fem-spatial-index-mesh idx) (list-ref idx 1))
(define (fem-spatial-index-grid idx) (list-ref idx 2))
(define (fem-spatial-index-grid-size idx) (list-ref idx 3))
(define (fem-spatial-index-x-min idx) (list-ref idx 4))
(define (fem-spatial-index-x-max idx) (list-ref idx 5))
(define (fem-spatial-index-y-min idx) (list-ref idx 6))
(define (fem-spatial-index-y-max idx) (list-ref idx 7))
(define (fem-spatial-index-dx idx) (list-ref idx 8))
(define (fem-spatial-index-dy idx) (list-ref idx 9))

(doc fem-point-in-element? 'type '(-> FEMMesh Nat Number Number (Or #f (List Number Number Number))))
(doc fem-point-in-element? 'description "Test if point is in element, return barycentric coords or #f")
(define (fem-point-in-element? mesh e x y)
  (doc 'export #t)
  (let* ([pts (fem-mesh-element-nodes mesh e)]
         [p1 (car pts)] [p2 (cadr pts)] [p3 (caddr pts)]
         [area (element-area p1 p2 p3)]
         [area1 (element-area (make-point2 x y) p2 p3)]
         [area2 (element-area p1 (make-point2 x y) p3)]
         [area3 (element-area p1 p2 (make-point2 x y))]
         [l1 (/ area1 area)]
         [l2 (/ area2 area)]
         [l3 (/ area3 area)])
    (if (and (>= l1 -1e-10) (>= l2 -1e-10) (>= l3 -1e-10))
        (list l1 l2 l3)
        #f)))

(doc fem-locate-point 'type '(-> FEMSpatialIndex Number Number (Or #f (List Nat Number Number Number))))
(doc fem-locate-point 'description "Find element containing point using spatial index, return (element l1 l2 l3) or #f")
(define (fem-locate-point index x y)
  (doc 'export #t)
  (let* ([mesh (fem-spatial-index-mesh index)]
         [grid (fem-spatial-index-grid index)]
         [gs (fem-spatial-index-grid-size index)]
         [x-min (fem-spatial-index-x-min index)]
         [y-min (fem-spatial-index-y-min index)]
         [dx (fem-spatial-index-dx index)]
         [dy (fem-spatial-index-dy index)]
         ;; Find grid cell
         [ci (inexact->exact (floor (/ (- x x-min) dx)))]
         [cj (inexact->exact (floor (/ (- y y-min) dy)))])
    (if (or (< ci 0) (>= ci gs) (< cj 0) (>= cj gs))
        #f  ; Outside grid
        (let ([candidates (vector-ref grid (+ (* cj gs) ci))])
          ;; Check each candidate element
          (let loop ([elems candidates])
            (if (null? elems)
                #f
                (let ([bary (fem-point-in-element? mesh (car elems) x y)])
                  (if bary
                      (cons (car elems) bary)
                      (loop (cdr elems))))))))))

(doc fem-solution-at-indexed 'type '(-> FEMSpatialIndex Vector Number Number Number))
(doc fem-solution-at-indexed 'description "Interpolate FEM solution using spatial index (O(1) average)")
(define (fem-solution-at-indexed index solution x y)
  (doc 'export #t)
  (let ([loc (fem-locate-point index x y)])
    (if (not loc)
        0.0  ; Point not in mesh
        (let* ([e (car loc)]
               [l1 (cadr loc)]
               [l2 (caddr loc)]
               [l3 (cadddr loc)]
               [mesh (fem-spatial-index-mesh index)]
               [elem (fem-mesh-element mesh e)]
               [u1 (vector-ref solution (vector-ref elem 0))]
               [u2 (vector-ref solution (vector-ref elem 1))]
               [u3 (vector-ref solution (vector-ref elem 2))])
          (+ (* l1 u1) (* l2 u2) (* l3 u3))))))

;;; ============================================================
;;; Section: Solution Visualization
;;; ============================================================

(doc 'section 'visualization)

(doc fem-solution-at 'type '(-> FEMMesh Vector Number Number Number))
(doc fem-solution-at 'description "Interpolate FEM solution at point (x,y) - O(N) linear search")
(define (fem-solution-at mesh solution x y)
  (doc 'export #t)
  ;; Find containing element and interpolate using basis functions
  ;; NOTE: For repeated queries, use fem-solution-at-indexed instead
  (let ([ne (fem-mesh-num-elements mesh)])
    (let loop ([e 0])
      (if (= e ne)
          0.0  ; Point not in mesh
          (let ([bary (fem-point-in-element? mesh e x y)])
            (if bary
                (let* ([elem (fem-mesh-element mesh e)]
                       [u1 (vector-ref solution (vector-ref elem 0))]
                       [u2 (vector-ref solution (vector-ref elem 1))]
                       [u3 (vector-ref solution (vector-ref elem 2))])
                  (+ (* (car bary) u1) (* (cadr bary) u2) (* (caddr bary) u3)))
                (loop (+ e 1))))))))

(doc fem-render-solution 'type '(-> FEMMesh Vector Nat Nat String))
(doc fem-render-solution 'description "Render FEM solution as ASCII heatmap (uses spatial index)")
(define (fem-render-solution mesh solution width height)
  (doc 'export #t)
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
         [nchars (string-length chars)]
         ;; Build spatial index for O(1) point location instead of O(N)
         [grid-size (max 10 (inexact->exact (ceiling (sqrt (fem-mesh-num-elements mesh)))))]
         [index (make-fem-spatial-index mesh grid-size)])
    (let loop-y ([j 0] [lines '()])
      (if (= j height)
          (apply string-append (reverse lines))
          (let ([y (+ y-min (* (- y-max y-min) (/ (- height 1 j) (- height 1))))])
            (let loop-x ([i 0] [row '()])
              (if (= i width)
                  (loop-y (+ j 1) (cons (string-append (list->string (reverse row)) "\n") lines))
                  (let* ([x (+ x-min (* (- x-max x-min) (/ i (- width 1))))]
                         ;; Use indexed lookup: O(1) instead of O(N)
                         [u (fem-solution-at-indexed index solution x y)]
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
  (doc 'export #t)
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
         [tri-record (delaunay-triangulate pts)]
         [triangles (triangulation-triangles tri-record)])
    (make-fem-mesh triangles)))

(doc make-disk-mesh 'type '(-> Number Nat FEMMesh))
(doc make-disk-mesh 'description "Create mesh on disk of given radius with n points")
(define (make-disk-mesh radius n)
  (doc 'export #t)
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
         [tri-record (delaunay-triangulate all-pts)]
         [triangles (triangulation-triangles tri-record)])
    (make-fem-mesh triangles)))

(doc fem-l2-error 'type '(-> FEMMesh Vector (-> Number Number Number) Number))
(doc fem-l2-error 'description "Compute L² error between FEM solution and exact solution")
(define (fem-l2-error mesh solution exact)
  (doc 'export #t)
  ;; L2 error: integral (u - u_exact)^2 dO, approximated per element
  (let ([ne (fem-mesh-num-elements mesh)])
    (sqrt
      (let loop-e ([e 0] [esq 0.0])
        (if (= e ne)
            esq
            (let* ([pts (fem-mesh-element-nodes mesh e)]
                   [elem (fem-mesh-element mesh e)]
                   [area (abs (element-area (car pts) (cadr pts) (caddr pts)))])
              (loop-e
                (+ e 1)
                (let loop-i ([i 0] [esq esq])
                  (if (= i 3)
                      esq
                      (let* ([p (list-ref pts i)]
                             [idx (vector-ref elem i)]
                             [u-fem (vector-ref solution idx)]
                             [u-exact (exact (point2-x p) (point2-y p))]
                             [diff (- u-fem u-exact)])
                        (loop-i (+ i 1)
                                (+ esq (* (/ area 3.0) (* diff diff)))))))))))
      )))

