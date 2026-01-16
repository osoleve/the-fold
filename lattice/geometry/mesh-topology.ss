;;; lattice/geometry/mesh-topology.ss — Topological Analysis of Triangle Meshes
;;;
;;; Bridges computational geometry with computational topology by converting
;;; triangle meshes to simplicial complexes for homological analysis.
;;;
;;; This enables:
;;;   1. Mesh integrity validation via Betti numbers
;;;      - B_0: Connected components (should be 1 for a single solid)
;;;      - B_1: Handles/tunnels (genus detection)
;;;      - B_2: Enclosed voids (should be 0 for watertight mesh)
;;;   2. Non-manifold geometry detection
;;;      - Edges shared by >2 triangles
;;;      - Vertices with non-disk-like neighborhoods
;;;   3. Topology verification for procedural meshes (marching cubes, etc.)
;;;
;;; Key insight: A triangle mesh IS a 2-dimensional simplicial complex embedded
;;; in 3D space. We abstract away the geometry, keeping only connectivity.
;;;
;;; TIER: 1 (depends on topology/homology, geometry/mesh-sdf)

(load "lattice/topology/homology.ss")
(load "lattice/geometry/mesh-sdf.ss")

;;; ============================================================
;;; VERTEX DEDUPLICATION
;;; ============================================================

;;; Meshes may have duplicate vertices at the same position. We need to
;;; identify them as a single topological vertex.
;;;
;;; Strategy: Quantize coordinates to a grid for hash-based deduplication.
;;; This handles floating-point imprecision while maintaining correctness.

;;; Default precision: positions within 1e-9 are considered identical
(define mesh-topology-epsilon 1e-9)

;;; quantize : Number → Integer
;;; Map floating point to quantized integer for hashing.
(define (quantize x)
  (inexact->exact (round (/ x mesh-topology-epsilon))))

;;; vec3-key : Vec3 → (List Integer Integer Integer)
;;; Create a hashable key from a vec3 position.
(define (vec3-key v)
  (list (quantize (vec3-x v))
        (quantize (vec3-y v))
        (quantize (vec3-z v))))

;;; build-vertex-map : (List Triangle3) → (values HashTable (Vector Vec3))
;;; Build a map from vec3 positions to unique vertex indices.
;;; Returns:
;;;   1. Hash table: vec3-key → vertex-index
;;;   2. Vector: vertex-index → vec3 (for debugging/visualization)
(define (build-vertex-map triangles)
  (let ([key->idx (make-hashtable equal-hash equal?)]
        [vertices '()]
        [next-idx 0])
    ;; Collect all triangle vertices
    (for-each
      (lambda (tri)
        (for-each
          (lambda (v)
            (let ([key (vec3-key v)])
              (unless (hashtable-ref key->idx key #f)
                (hashtable-set! key->idx key next-idx)
                (set! vertices (cons v vertices))
                (set! next-idx (+ next-idx 1)))))
          (list (triangle3-p1 tri)
                (triangle3-p2 tri)
                (triangle3-p3 tri))))
      triangles)
    (values key->idx (list->vector (reverse vertices)))))

;;; ============================================================
;;; MESH → SIMPLICIAL COMPLEX CONVERSION
;;; ============================================================

;;; mesh->simplicial-complex : Mesh → SC
;;; Convert a triangle mesh to a 2-dimensional simplicial complex.
;;;
;;; The conversion:
;;;   - Each unique vertex position → 0-simplex
;;;   - Each edge (shared positions) → 1-simplex
;;;   - Each triangle → 2-simplex
;;;
;;; The simplicial complex automatically includes all faces due to
;;; the closure property in sc-from-simplices.
(define (mesh->simplicial-complex mesh)
  (triangles->simplicial-complex (mesh-triangles mesh)))

;;; triangles->simplicial-complex : (List Triangle3) → SC
;;; Convert raw triangle list to simplicial complex.
(define (triangles->simplicial-complex triangles)
  (let-values ([(key->idx vertices) (build-vertex-map triangles)])
    ;; Convert each triangle to a 2-simplex
    (let ([simplices
           (map (lambda (tri)
                  (let ([i1 (hashtable-ref key->idx (vec3-key (triangle3-p1 tri)) #f)]
                        [i2 (hashtable-ref key->idx (vec3-key (triangle3-p2 tri)) #f)]
                        [i3 (hashtable-ref key->idx (vec3-key (triangle3-p3 tri)) #f)])
                    ;; Skip degenerate triangles (duplicate vertices)
                    (if (and (not (= i1 i2))
                             (not (= i2 i3))
                             (not (= i1 i3)))
                        (make-simplex (list i1 i2 i3))
                        #f)))
                triangles)])
      ;; Filter out degenerate triangles and build complex
      (sc-from-simplices (filter identity simplices)))))

;;; ============================================================
;;; TOPOLOGICAL INVARIANTS FOR MESHES
;;; ============================================================

;;; mesh-betti-numbers : Mesh → (List Integer)
;;; Compute Betti numbers (B_0, B_1, B_2) for a mesh.
;;;
;;; Interpretation:
;;;   B_0 = number of connected components
;;;   B_1 = number of "tunnels" or handles (genus-related)
;;;   B_2 = number of enclosed voids
(define (mesh-betti-numbers mesh)
  (sc-betti-numbers (mesh->simplicial-complex mesh)))

;;; mesh-euler-characteristic : Mesh → Integer
;;; Compute Euler characteristic: χ = V - E + F
;;; For a closed surface: χ = 2 - 2g where g is the genus.
(define (mesh-euler-characteristic mesh)
  (sc-euler (mesh->simplicial-complex mesh)))

;;; mesh-f-vector : Mesh → (V E F)
;;; Get the f-vector: (vertices, edges, faces).
(define (mesh-f-vector mesh)
  (sc-f-vector (mesh->simplicial-complex mesh)))

;;; mesh-connected-components : Mesh → Integer
;;; Count connected components (same as B_0 but via union-find).
(define (mesh-connected-components mesh)
  (sc-connected-components (mesh->simplicial-complex mesh)))

;;; ============================================================
;;; GENUS COMPUTATION
;;; ============================================================

;;; mesh-genus : Mesh → Integer | 'non-orientable | 'open
;;; Compute the genus of a closed orientable surface.
;;;
;;; For closed orientable surfaces:
;;;   χ = 2 - 2g  →  g = (2 - χ) / 2
;;;
;;; Returns:
;;;   - Integer genus for closed orientable surfaces
;;;   - 'open if B_2 ≠ 1 (not a closed surface)
;;;   - 'non-orientable if genus computation gives non-integer (can happen over Z_2)
(define (mesh-genus mesh)
  (let* ([betti (mesh-betti-numbers mesh)]
         [b0 (if (pair? betti) (car betti) 0)]
         [b2 (if (>= (length betti) 3) (caddr betti) 0)]
         [euler (mesh-euler-characteristic mesh)])
    (cond
      ;; Must be connected (B_0 = 1) and closed (B_2 = 1) for genus to make sense
      [(not (= b0 1)) 'disconnected]
      [(not (= b2 1)) 'open]
      [else
       (let ([g (/ (- 2 euler) 2)])
         (if (integer? g)
             (inexact->exact g)
             'non-orientable))])))

;;; ============================================================
;;; MANIFOLD VALIDATION
;;; ============================================================

;;; A 2-manifold (surface) must satisfy:
;;;   1. Every edge is shared by exactly 2 triangles (interior) or 1 (boundary)
;;;   2. The neighborhood of every vertex is homeomorphic to a disk (or half-disk)

;;; mesh-edge-counts : Mesh → HashTable
;;; Count how many triangles share each edge.
;;; Key: sorted pair of vertex indices, Value: count
(define (mesh-edge-counts mesh)
  (let-values ([(key->idx vertices) (build-vertex-map (mesh-triangles mesh))])
    (let ([edge-count (make-hashtable equal-hash equal?)])
      (for-each
        (lambda (tri)
          (let* ([i1 (hashtable-ref key->idx (vec3-key (triangle3-p1 tri)) #f)]
                 [i2 (hashtable-ref key->idx (vec3-key (triangle3-p2 tri)) #f)]
                 [i3 (hashtable-ref key->idx (vec3-key (triangle3-p3 tri)) #f)]
                 [edges (list (edge-key i1 i2)
                              (edge-key i2 i3)
                              (edge-key i3 i1))])
            (for-each
              (lambda (e)
                (let ([count (hashtable-ref edge-count e 0)])
                  (hashtable-set! edge-count e (+ count 1))))
              edges)))
        (mesh-triangles mesh))
      edge-count)))

;;; edge-key : Int × Int → (Int . Int)
;;; Create canonical edge key (smaller index first).
(define (edge-key i j)
  (if (< i j) (cons i j) (cons j i)))

;;; mesh-is-manifold? : Mesh → Boolean
;;; Check if mesh is a valid 2-manifold.
;;; Every edge must be shared by exactly 1 or 2 triangles.
(define (mesh-is-manifold? mesh)
  (let ([edge-counts (mesh-edge-counts mesh)])
    (let-values ([(keys) (hashtable-keys edge-counts)])
      (let loop ([i 0])
        (if (>= i (vector-length keys))
            #t
            (let ([count (hashtable-ref edge-counts (vector-ref keys i) 0)])
              (if (and (>= count 1) (<= count 2))
                  (loop (+ i 1))
                  #f)))))))

;;; mesh-is-closed? : Mesh → Boolean
;;; Check if mesh is a closed surface (no boundary edges).
;;; Every edge must be shared by exactly 2 triangles.
(define (mesh-is-closed? mesh)
  (let ([edge-counts (mesh-edge-counts mesh)])
    (let-values ([(keys) (hashtable-keys edge-counts)])
      (let loop ([i 0])
        (if (>= i (vector-length keys))
            #t
            (let ([count (hashtable-ref edge-counts (vector-ref keys i) 0)])
              (if (= count 2)
                  (loop (+ i 1))
                  #f)))))))

;;; mesh-boundary-edges : Mesh → (List (Int . Int))
;;; Find all boundary edges (edges shared by only 1 triangle).
(define (mesh-boundary-edges mesh)
  (let ([edge-counts (mesh-edge-counts mesh)])
    (let-values ([(keys) (hashtable-keys edge-counts)])
      (let loop ([i 0] [result '()])
        (if (>= i (vector-length keys))
            (reverse result)
            (let* ([edge (vector-ref keys i)]
                   [count (hashtable-ref edge-counts edge 0)])
              (loop (+ i 1)
                    (if (= count 1)
                        (cons edge result)
                        result))))))))

;;; mesh-non-manifold-edges : Mesh → (List (Int . Int))
;;; Find non-manifold edges (shared by >2 triangles).
(define (mesh-non-manifold-edges mesh)
  (let ([edge-counts (mesh-edge-counts mesh)])
    (let-values ([(keys) (hashtable-keys edge-counts)])
      (let loop ([i 0] [result '()])
        (if (>= i (vector-length keys))
            (reverse result)
            (let* ([edge (vector-ref keys i)]
                   [count (hashtable-ref edge-counts edge 0)])
              (loop (+ i 1)
                    (if (> count 2)
                        (cons edge result)
                        result))))))))

;;; ============================================================
;;; MESH TOPOLOGY SUMMARY
;;; ============================================================

;;; mesh-topology-summary : Mesh → Void
;;; Print a comprehensive topological analysis of a mesh.
(define (mesh-topology-summary mesh)
  (let* ([sc (mesh->simplicial-complex mesh)]
         [f-vec (sc-f-vector sc)]
         [betti (sc-betti-numbers sc)]
         [euler (sc-euler sc)]
         [genus (mesh-genus mesh)]
         [manifold? (mesh-is-manifold? mesh)]
         [closed? (mesh-is-closed? mesh)]
         [boundary (mesh-boundary-edges mesh)]
         [non-manifold (mesh-non-manifold-edges mesh)])
    (printf "~n=== Mesh Topology Summary ===~n")
    (printf "Triangle count:     ~a~n" (mesh-triangle-count mesh))
    (printf "f-vector (V,E,F):   ~a~n" f-vec)
    (printf "Betti numbers:      ~a~n" betti)
    (printf "Euler characteristic: ~a~n" euler)
    (printf "Genus:              ~a~n" genus)
    (printf "Is manifold?        ~a~n" manifold?)
    (printf "Is closed?          ~a~n" closed?)
    (when (not (null? boundary))
      (printf "Boundary edges:     ~a~n" (length boundary)))
    (when (not (null? non-manifold))
      (printf "Non-manifold edges: ~a~n" non-manifold))
    (printf "==============================~n")))

;;; ============================================================
;;; VALIDATION PREDICATES
;;; ============================================================

;;; mesh-is-watertight? : Mesh → Boolean
;;; Check if mesh is watertight (closed manifold with no voids).
;;; A watertight mesh has B_0=1, B_1=0 (for sphere-like), B_2=1.
(define (mesh-is-watertight? mesh)
  (and (mesh-is-manifold? mesh)
       (mesh-is-closed? mesh)
       (let ([betti (mesh-betti-numbers mesh)])
         (and (= (length betti) 3)
              (= (car betti) 1)      ; B_0 = 1 (connected)
              (= (caddr betti) 1))))) ; B_2 = 1 (single void)

;;; mesh-is-sphere-topology? : Mesh → Boolean
;;; Check if mesh has the topology of a sphere.
;;; Sphere: B = (1, 0, 1), χ = 2
(define (mesh-is-sphere-topology? mesh)
  (let ([betti (mesh-betti-numbers mesh)])
    (and (= (length betti) 3)
         (= (car betti) 1)     ; B_0 = 1
         (= (cadr betti) 0)    ; B_1 = 0
         (= (caddr betti) 1)   ; B_2 = 1
         (= (mesh-euler-characteristic mesh) 2))))

;;; mesh-is-torus-topology? : Mesh → Boolean
;;; Check if mesh has the topology of a torus.
;;; Torus (over Z_2): B = (1, 2, 1), χ = 0
(define (mesh-is-torus-topology? mesh)
  (let ([betti (mesh-betti-numbers mesh)])
    (and (= (length betti) 3)
         (= (car betti) 1)     ; B_0 = 1
         (= (cadr betti) 2)    ; B_1 = 2
         (= (caddr betti) 1)   ; B_2 = 1
         (= (mesh-euler-characteristic mesh) 0))))
