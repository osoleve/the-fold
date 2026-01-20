(load "lattice/topology/homology.ss")
(load "lattice/geometry/mesh-sdf.ss")

(doc 'module 'mesh-topology)
(doc 'description "Topological analysis of triangle meshes via simplicial complex conversion")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'provides "1. Mesh integrity validation via Betti numbers (B_0: connected components, B_1: handles/tunnels, B_2: enclosed voids)
2. Non-manifold geometry detection (edges shared by >2 triangles, vertices with non-disk-like neighborhoods)
3. Topology verification for procedural meshes")
(doc 'note "Key insight: A triangle mesh IS a 2-dimensional simplicial complex embedded in 3D space - we abstract away geometry, keeping only connectivity")

(doc 'section 'vertex-deduplication)
(doc 'note "Meshes may have duplicate vertices at same position; strategy: quantize coordinates to grid for hash-based deduplication, handling floating-point imprecision")

(define mesh-topology-epsilon 1e-9)
(doc mesh-topology-epsilon 'description "Default precision: positions within 1e-9 are considered identical")

(define (quantize x)
  (doc 'type '(-> Number Integer))
  (doc 'description "Map floating point to quantized integer for hashing")
  (inexact->exact (round (/ x mesh-topology-epsilon))))

(define (vec3-key v)
  (doc 'type '(-> Vec3 (List Integer Integer Integer)))
  (doc 'description "Create a hashable key from a vec3 position")
  (list (quantize (vec3-x v))
        (quantize (vec3-y v))
        (quantize (vec3-z v))))

(define (build-vertex-map triangles)
  (doc 'type '(-> (List Triangle3) (Values HashTable (Vector Vec3))))
  (doc 'description "Build a map from vec3 positions to unique vertex indices")
  (doc 'returns "1. Hash table: vec3-key → vertex-index, 2. Vector: vertex-index → vec3 (for debugging/visualization)")
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

(doc 'section 'mesh-to-simplicial-complex-conversion)

(define (mesh->simplicial-complex mesh)
  (doc 'type '(-> Mesh SC))
  (doc 'description "Convert a triangle mesh to a 2-dimensional simplicial complex")
  (doc 'note "Conversion: each unique vertex position → 0-simplex, each edge → 1-simplex, each triangle → 2-simplex; simplicial complex automatically includes all faces due to closure property")
  (triangles->simplicial-complex (mesh-triangles mesh)))

(define (triangles->simplicial-complex triangles)
  (doc 'type '(-> (List Triangle3) SC))
  (doc 'description "Convert raw triangle list to simplicial complex")
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

(doc 'section 'topological-invariants)

(define (mesh-betti-numbers mesh)
  (doc 'type '(-> Mesh (List Integer)))
  (doc 'description "Compute Betti numbers (B_0, B_1, B_2) for a mesh")
  (doc 'note "Interpretation: B_0 = connected components, B_1 = tunnels/handles (genus-related), B_2 = enclosed voids")
  (sc-betti-numbers (mesh->simplicial-complex mesh)))

(define (mesh-euler-characteristic mesh)
  (doc 'type '(-> Mesh Integer))
  (doc 'description "Compute Euler characteristic: χ = V - E + F; for closed surface: χ = 2 - 2g where g is genus")
  (sc-euler (mesh->simplicial-complex mesh)))

(define (mesh-f-vector mesh)
  (doc 'type '(-> Mesh (List Integer Integer Integer)))
  (doc 'description "Get the f-vector: (vertices, edges, faces)")
  (sc-f-vector (mesh->simplicial-complex mesh)))

(define (mesh-connected-components mesh)
  (doc 'type '(-> Mesh Integer))
  (doc 'description "Count connected components (same as B_0 but via union-find)")
  (sc-connected-components (mesh->simplicial-complex mesh)))

(doc 'section 'genus-computation)

(define (mesh-genus mesh)
  (doc 'type '(-> Mesh (Union Integer Symbol)))
  (doc 'description "Compute the genus of a closed orientable surface: χ = 2 - 2g → g = (2 - χ) / 2")
  (doc 'returns "Integer genus | 'disconnected | 'open if B_2 ≠ 1 | 'non-orientable if non-integer genus")
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
;;; Must satisfy:
;;;   1. Edge Condition: Every edge is shared by 1 or 2 triangles.
;;;   2. Vertex Condition: The link of every vertex is connected (1 component).
;;;
;;; The vertex condition catches "pinch points" where multiple surfaces
;;; meet at a single vertex (e.g., hourglass shape = two cones at one point).
(define (mesh-is-manifold? mesh)
  (and (mesh-edges-are-manifold? mesh)
       (mesh-vertices-are-manifold? mesh)))

;;; mesh-edges-are-manifold? : Mesh → Boolean
;;; Check edge condition: every edge shared by 1-2 triangles.
(define (mesh-edges-are-manifold? mesh)
  (let ([edge-counts (mesh-edge-counts mesh)])
    (let-values ([(keys) (hashtable-keys edge-counts)])
      (let loop ([i 0])
        (if (>= i (vector-length keys))
            #t
            (let ([count (hashtable-ref edge-counts (vector-ref keys i) 0)])
              (if (and (>= count 1) (<= count 2))
                  (loop (+ i 1))
                  #f)))))))

;;; mesh-vertices-are-manifold? : Mesh → Boolean
;;; Check vertex condition: the link of every vertex is connected.
;;; For a 2-manifold, each vertex link should be homeomorphic to a circle
;;; (interior vertex) or line segment (boundary vertex) - both are connected.
;;; Disconnected links indicate "pinch points" (non-manifold vertices).
(define (mesh-vertices-are-manifold? mesh)
  (let* ([sc (mesh->simplicial-complex mesh)]
         [vertices (sc-vertices sc)])
    (let loop ([vs vertices])
      (if (null? vs)
          #t
          (let* ([v (car vs)]
                 ;; Get edges and vertices of the link
                 [link-data (vertex-link-edges sc v)]
                 [link-verts (car link-data)]
                 [link-edges (cadr link-data)]
                 ;; Count connected components via union-find
                 [n-comps (count-components-uf link-verts link-edges)])
            (if (<= n-comps 1)  ; 0 for empty link, 1 for manifold
                (loop (cdr vs))
                #f))))))

;;; vertex-link-edges : SC × Vertex → ((List Vertex) (List (Vertex Vertex)))
;;; Get the vertices and edges of the link of a vertex.
;;; Returns (link-vertices link-edges) where edges are pairs.
;;; NOTE: Uses list-member? to avoid shadowed set-member? from homology.ss
(define (vertex-link-edges sc v)
  (let* ([triangles (sc-simplices-dim sc 2)]
         [incident (filter (lambda (t) (list-member? v (simplex-vertices t))) triangles)]
         ;; Each incident triangle contributes an edge to the link (the opposite edge)
         [link-edges
          (map (lambda (t)
                 (let ([verts (filter (lambda (x) (not (equal? x v)))
                                      (simplex-vertices t))])
                   (cons (car verts) (cadr verts))))
               incident)]
         ;; Collect all link vertices
         [link-verts (unique-list (apply append (map (lambda (e) (list (car e) (cdr e))) link-edges)))])
    (list link-verts link-edges)))

;;; list-member? : α × (List α) → Boolean
;;; Check if element is in list (avoids shadowed set-member?).
(define (list-member? x lst)
  (cond
    [(null? lst) #f]
    [(equal? x (car lst)) #t]
    [else (list-member? x (cdr lst))]))

;;; unique-list : (List α) → (List α)
;;; Remove duplicates from a list (preserves first occurrence).
(define (unique-list lst)
  (let loop ([remaining lst] [seen '()] [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [(member (car remaining) seen)
       (loop (cdr remaining) seen acc)]
      [else
       (loop (cdr remaining)
             (cons (car remaining) seen)
             (cons (car remaining) acc))])))

;;; count-components-uf : (List Vertex) × (List (Vertex . Vertex)) → Integer
;;; Count connected components using union-find.
(define (count-components-uf vertices edges)
  (if (null? vertices)
      0
      (let* ([n (length vertices)]
             [idx-map (build-index-map vertices)]
             [parent (make-vector n)]
             [rank-vec (make-vector n 0)])
        ;; Initialize: each vertex is its own component
        (do ([i 0 (+ i 1)]) [(= i n)]
          (vector-set! parent i i))
        ;; Union-find helpers
        (letrec ([find (lambda (x)
                         (if (= (vector-ref parent x) x)
                             x
                             (let ([root (find (vector-ref parent x))])
                               (vector-set! parent x root)
                               root)))]
                 [union (lambda (x y)
                          (let ([px (find x)] [py (find y)])
                            (unless (= px py)
                              (let ([rx (vector-ref rank-vec px)]
                                    [ry (vector-ref rank-vec py)])
                                (cond
                                  [(< rx ry) (vector-set! parent px py)]
                                  [(> rx ry) (vector-set! parent py px)]
                                  [else
                                   (vector-set! parent py px)
                                   (vector-set! rank-vec px (+ rx 1))])))))])
          ;; Process edges
          (for-each
            (lambda (edge)
              (let ([i1 (hashtable-ref idx-map (car edge) #f)]
                    [i2 (hashtable-ref idx-map (cdr edge) #f)])
                (when (and i1 i2)
                  (union i1 i2))))
            edges)
          ;; Count distinct roots
          (let ([roots (make-hashtable equal-hash equal?)])
            (do ([i 0 (+ i 1)]) [(= i n)]
              (hashtable-set! roots (find i) #t))
            (hashtable-size roots))))))

;;; build-index-map : (List α) → HashTable
;;; Build a map from elements to their indices.
(define (build-index-map lst)
  (let ([ht (make-hashtable equal-hash equal?)])
    (let loop ([remaining lst] [i 0])
      (unless (null? remaining)
        (hashtable-set! ht (car remaining) i)
        (loop (cdr remaining) (+ i 1))))
    ht))

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
