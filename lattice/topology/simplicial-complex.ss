;;; lattice/topology/simplicial-complex.ss — Simplicial Complex Data Structures
;;;
;;; Core data structures for computational topology.
;;;
;;; A simplex is a generalization of a triangle to arbitrary dimensions:
;;;   0-simplex = vertex (point)
;;;   1-simplex = edge (line segment)
;;;   2-simplex = triangle
;;;   3-simplex = tetrahedron
;;;   n-simplex = n+1 vertices in general position
;;;
;;; A simplicial complex is a collection of simplices satisfying:
;;;   - Closure: Every face of a simplex in the complex is also in the complex
;;;   - Intersection: Two simplices intersect in a common face (or not at all)
;;;
;;; This module provides:
;;;   1. Simplex representation and operations
;;;   2. Simplicial complex construction and manipulation
;;;   3. Face enumeration and boundary computation
;;;   4. Star, link, and skeleton operations
;;;   5. Filtration support for persistent homology
;;;
;;; TIER: 1 (depends on data/set, data/sort)

(load "lattice/data/set.ss")
(load "lattice/data/sort.ss")

;;; ============================================================
;;; GENERIC COMPARISON
;;; ============================================================

;;; generic<? : α × α → Boolean
;;; Universal comparison function for canonical ordering.
;;; Handles: numbers, symbols, strings, and lists (lexicographically).
(define (generic<? a b)
  (cond
    ; Both numbers
    [(and (number? a) (number? b))
     (< a b)]
    ; Both symbols
    [(and (symbol? a) (symbol? b))
     (string<? (symbol->string a) (symbol->string b))]
    ; Both strings
    [(and (string? a) (string? b))
     (string<? a b)]
    ; Both lists (lexicographic)
    [(and (list? a) (list? b))
     (cond
       [(null? a) (not (null? b))]
       [(null? b) #f]
       [(generic<? (car a) (car b)) #t]
       [(generic<? (car b) (car a)) #f]
       [else (generic<? (cdr a) (cdr b))])]
    ; Mixed types: order by type
    [else
     (< (type-order a) (type-order b))]))

;;; type-order : α → Integer
;;; Assign numeric order to types for consistent mixed-type comparison.
(define (type-order x)
  (cond
    [(number? x) 0]
    [(symbol? x) 1]
    [(string? x) 2]
    [(list? x) 3]
    [else 4]))

;;; ============================================================
;;; SIMPLEX — The Fundamental Building Block
;;; ============================================================

;;; Simplex = (simplex vertices)
;;; where vertices is a sorted list of vertex labels (any comparable values)

;;; make-simplex : (List Vertex) → Simplex
;;; Create a simplex from a list of vertices.
;;; Vertices are sorted for canonical representation.
;;; Duplicate vertices are removed (degenerate simplices normalized).
(define (make-simplex vertices)
  (list 'simplex (unique-sorted (sort-by generic<? vertices))))

;;; simplex? : α → Boolean
;;; Check if value is a simplex.
(define (simplex? x)
  (and (pair? x) (eq? (car x) 'simplex)))

;;; simplex-vertices : Simplex → (List Vertex)
;;; Get the sorted vertex list of a simplex.
(define (simplex-vertices s)
  (cadr s))

;;; simplex-dim : Simplex → Integer
;;; Get the dimension of a simplex (number of vertices - 1).
;;; A 0-simplex (single vertex) has dimension 0.
(define (simplex-dim s)
  (- (length (simplex-vertices s)) 1))

;;; simplex-empty? : Simplex → Boolean
;;; Check if simplex is the empty simplex (no vertices).
(define (simplex-empty? s)
  (null? (simplex-vertices s)))

;;; simplex-equal? : Simplex × Simplex → Boolean
;;; Check if two simplices are equal (same vertices).
;;; Since vertices are sorted, we can use equal?.
(define (simplex-equal? s1 s2)
  (equal? (simplex-vertices s1) (simplex-vertices s2)))

;;; simplex-contains-vertex? : Simplex × Vertex → Boolean
;;; Check if a simplex contains a specific vertex.
(define (simplex-contains-vertex? s v)
  (set-member? v (simplex-vertices s)))

;;; simplex-face? : Simplex × Simplex → Boolean
;;; Check if s1 is a face of s2 (s1's vertices are a subset of s2's).
(define (simplex-face? s1 s2)
  (set-subset? (simplex-vertices s1) (simplex-vertices s2)))

;;; ============================================================
;;; FACE ENUMERATION
;;; ============================================================

;;; remove-at : (List α) × Integer → (List α)
;;; Remove element at index i from list.
(define (remove-at lst i)
  (let loop ([remaining lst] [idx 0] [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [(= idx i) (loop (cdr remaining) (+ idx 1) acc)]
      [else (loop (cdr remaining) (+ idx 1) (cons (car remaining) acc))])))

;;; simplex-facets : Simplex → (List Simplex)
;;; Get all facets (codimension-1 faces) of a simplex.
;;; Each facet is obtained by removing one vertex.
;;; Returns list of (dim-1)-simplices.
;;; A vertex (0-simplex) has no facets.
(define (simplex-facets s)
  (let* ([verts (simplex-vertices s)]
         [n (length verts)])
    (if (<= n 1)
        '()  ; Empty simplex and vertices have no facets
        (let loop ([i 0] [acc '()])
          (if (>= i n)
              (reverse acc)
              (loop (+ i 1)
                    (cons (make-simplex (remove-at verts i)) acc)))))))

;;; simplex-faces-of-dim : Simplex × Integer → (List Simplex)
;;; Get all k-dimensional faces of a simplex.
;;; A k-face has k+1 vertices.
(define (simplex-faces-of-dim s k)
  (let ([verts (simplex-vertices s)]
        [needed (+ k 1)])
    (if (or (< k 0) (> needed (length verts)))
        '()
        (map make-simplex (combinations verts needed)))))

;;; combinations : (List α) × Integer → (List (List α))
;;; Generate all k-combinations of elements from a list.
(define (combinations lst k)
  (cond
    [(= k 0) '(())]
    [(null? lst) '()]
    [else
     (append
       ; Combinations including first element
       (map (lambda (c) (cons (car lst) c))
            (combinations (cdr lst) (- k 1)))
       ; Combinations not including first element
       (combinations (cdr lst) k))]))

;;; simplex-all-faces : Simplex → (List Simplex)
;;; Get all faces of a simplex, including the simplex itself.
;;; Ordered from dimension 0 up to the simplex's dimension.
(define (simplex-all-faces s)
  (let ([d (simplex-dim s)])
    (apply append
           (map (lambda (k) (simplex-faces-of-dim s k))
                (iota (+ d 1))))))

;;; simplex-proper-faces : Simplex → (List Simplex)
;;; Get all proper faces (faces excluding the simplex itself).
(define (simplex-proper-faces s)
  (let ([d (simplex-dim s)])
    (if (< d 0)
        '()
        (apply append
               (map (lambda (k) (simplex-faces-of-dim s k))
                    (iota d))))))

;;; iota : Integer → (List Integer)
;;; Generate list (0 1 2 ... n-1).
(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; SIMPLICIAL COMPLEX
;;; ============================================================

;;; Simplicial Complex = (sc simplices-by-dim max-dim)
;;; where simplices-by-dim is a vector of simplex lists indexed by dimension

;;; sc-empty : SimplicalComplex
;;; The empty simplicial complex.
(define sc-empty
  (list 'sc (vector) -1))

;;; sc? : α → Boolean
(define (sc? x)
  (and (pair? x) (eq? (car x) 'sc)))

;;; sc-simplices-by-dim : SC → Vector
(define (sc-simplices-by-dim sc)
  (cadr sc))

;;; sc-max-dim : SC → Integer
(define (sc-max-dim sc)
  (caddr sc))

;;; sc-ensure-dim : SC × Integer → SC
;;; Ensure the complex has room for simplices of given dimension.
(define (sc-ensure-dim sc dim)
  (let* ([vec (sc-simplices-by-dim sc)]
         [current-len (vector-length vec)])
    (if (< dim current-len)
        sc
        (let* ([new-len (+ dim 1)]
               [new-vec (make-vector new-len '())])
          ; Copy existing entries
          (let loop ([i 0])
            (when (< i current-len)
              (vector-set! new-vec i (vector-ref vec i))
              (loop (+ i 1))))
          (list 'sc new-vec (max (sc-max-dim sc) dim))))))

;;; sc-add-simplex-only : SC × Simplex → SC
;;; Add a simplex WITHOUT adding its faces (internal use).
(define (sc-add-simplex-only sc s)
  (let* ([dim (simplex-dim s)]
         [sc2 (sc-ensure-dim sc dim)]
         [vec (sc-simplices-by-dim sc2)]
         [existing (vector-ref vec dim)])
    ; Check if already present
    (if (simplex-in-list? s existing)
        sc2
        (let ([new-vec (vector-copy vec)])
          (vector-set! new-vec dim (cons s existing))
          (list 'sc new-vec (max (sc-max-dim sc2) dim))))))

;;; simplex-in-list? : Simplex × (List Simplex) → Boolean
(define (simplex-in-list? s lst)
  (cond
    [(null? lst) #f]
    [(simplex-equal? s (car lst)) #t]
    [else (simplex-in-list? s (cdr lst))]))

;;; sc-add-simplex : SC × Simplex → SC
;;; Add a simplex and all its faces to the complex.
;;; This ensures the closure property is maintained.
(define (sc-add-simplex sc s)
  (let loop ([sc sc] [to-add (cons s (simplex-proper-faces s))])
    (if (null? to-add)
        sc
        (loop (sc-add-simplex-only sc (car to-add))
              (cdr to-add)))))

;;; sc-from-simplices : (List Simplex) → SC
;;; Build a simplicial complex from a list of simplices.
;;; Automatically adds all faces to maintain closure property.
(define (sc-from-simplices simplices)
  (let loop ([sc sc-empty] [remaining simplices])
    (if (null? remaining)
        sc
        (loop (sc-add-simplex sc (car remaining))
              (cdr remaining)))))

;;; sc-contains? : SC × Simplex → Boolean
;;; Check if a simplex is in the complex.
(define (sc-contains? sc s)
  (let* ([dim (simplex-dim s)]
         [vec (sc-simplices-by-dim sc)])
    (if (>= dim (vector-length vec))
        #f
        (simplex-in-list? s (vector-ref vec dim)))))

;;; sc-simplices : SC → (List Simplex)
;;; Get all simplices in the complex as a flat list.
(define (sc-simplices sc)
  (let* ([vec (sc-simplices-by-dim sc)]
         [n (vector-length vec)])
    (let loop ([i 0] [acc '()])
      (if (>= i n)
          (reverse acc)
          (loop (+ i 1) (append (reverse (vector-ref vec i)) acc))))))

;;; sc-simplices-dim : SC × Integer → (List Simplex)
;;; Get all simplices of a specific dimension.
(define (sc-simplices-dim sc k)
  (let ([vec (sc-simplices-by-dim sc)])
    (if (or (< k 0) (>= k (vector-length vec)))
        '()
        (vector-ref vec k))))

;;; sc-vertices : SC → (List Vertex)
;;; Get all vertices (0-simplices) in the complex.
(define (sc-vertices sc)
  (map (lambda (s) (car (simplex-vertices s)))
       (sc-simplices-dim sc 0)))

;;; sc-edges : SC → (List Simplex)
;;; Get all edges (1-simplices) in the complex.
(define (sc-edges sc)
  (sc-simplices-dim sc 1))

;;; sc-faces : SC → (List Simplex)
;;; Get all faces (2-simplices) in the complex.
(define (sc-faces sc)
  (sc-simplices-dim sc 2))

;;; sc-count : SC → Integer
;;; Count total number of simplices.
(define (sc-count sc)
  (length (sc-simplices sc)))

;;; sc-count-dim : SC × Integer → Integer
;;; Count simplices of a given dimension.
(define (sc-count-dim sc k)
  (length (sc-simplices-dim sc k)))

;;; ============================================================
;;; SKELETON AND SUBCOMPLEX OPERATIONS
;;; ============================================================

;;; sc-skeleton : SC × Integer → SC
;;; Get the k-skeleton: all simplices of dimension ≤ k.
(define (sc-skeleton sc k)
  (let* ([vec (sc-simplices-by-dim sc)]
         [n (min (+ k 1) (vector-length vec))]
         [new-vec (make-vector n '())])
    (let loop ([i 0])
      (when (< i n)
        (vector-set! new-vec i (vector-ref vec i))
        (loop (+ i 1))))
    (list 'sc new-vec k)))

;;; sc-star : SC × Simplex → (List Simplex)
;;; Get the star of a simplex: all simplices that contain it as a face.
(define (sc-star sc s)
  (filter (lambda (t) (simplex-face? s t))
          (sc-simplices sc)))

;;; sc-closed-star : SC × Simplex → SC
;;; Get the closed star: the smallest subcomplex containing the star.
(define (sc-closed-star sc s)
  (sc-from-simplices (sc-star sc s)))

;;; sc-link : SC × Simplex → SC
;;; Get the link of a simplex: faces of the closed star that don't intersect s.
;;; Link(s) = {t ∈ Cl(St(s)) | t ∩ s = ∅}
(define (sc-link sc s)
  (let* ([closed-star (sc-closed-star sc s)]
         [s-verts (simplex-vertices s)])
    (sc-from-simplices
      (filter (lambda (t)
                (null? (set-intersection (simplex-vertices t) s-verts)))
              (sc-simplices closed-star)))))

;;; ============================================================
;;; BOUNDARY OPERATOR
;;; ============================================================

;;; A chain is a formal linear combination of simplices.
;;; Chain = ((coeff . simplex) ...)
;;; For now we use integer coefficients (Z-chains).

;;; make-chain : (List (Pair Integer Simplex)) → Chain
(define (make-chain terms)
  terms)

;;; chain-empty : Chain
(define chain-empty '())

;;; chain-add-term : Chain × Integer × Simplex → Chain
;;; Add a term to a chain, combining coefficients if simplex already present.
(define (chain-add-term chain coeff s)
  (let loop ([remaining chain] [acc '()] [found #f])
    (cond
      [(null? remaining)
       (if found
           (reverse acc)
           (reverse (cons (cons coeff s) acc)))]
      [(simplex-equal? s (cdar remaining))
       (let ([new-coeff (+ coeff (caar remaining))])
         (if (= new-coeff 0)
             (loop (cdr remaining) acc #t)  ; Remove zero terms
             (loop (cdr remaining) (cons (cons new-coeff (cdar remaining)) acc) #t)))]
      [else
       (loop (cdr remaining) (cons (car remaining) acc) found)])))

;;; simplex-boundary : Simplex → Chain
;;; Compute the boundary of a simplex as a chain.
;;; ∂[v0, v1, ..., vn] = Σ (-1)^i [v0, ..., v̂i, ..., vn]
;;; where v̂i means vi is omitted.
(define (simplex-boundary s)
  (let* ([verts (simplex-vertices s)]
         [n (length verts)])
    (if (<= n 1)
        chain-empty  ; Boundary of a vertex is empty
        (let loop ([i 0] [chain chain-empty])
          (if (>= i n)
              chain
              (let* ([sign (if (even? i) 1 -1)]
                     [face (make-simplex (remove-at verts i))])
                (loop (+ i 1) (chain-add-term chain sign face))))))))

;;; chain-boundary : Chain → Chain
;;; Extend boundary operator linearly to chains.
;;; ∂(Σ ai·σi) = Σ ai·∂σi
(define (chain-boundary chain)
  (let loop ([remaining chain] [result chain-empty])
    (if (null? remaining)
        result
        (let* ([term (car remaining)]
               [coeff (car term)]
               [s (cdr term)]
               [bd (simplex-boundary s)])
          ; Scale boundary by coefficient and add to result
          (loop (cdr remaining)
                (chain-add-terms result
                                 (map (lambda (t) (cons (* coeff (car t)) (cdr t)))
                                      bd)))))))

;;; chain-add-terms : Chain × (List Term) → Chain
(define (chain-add-terms chain terms)
  (let loop ([remaining terms] [result chain])
    (if (null? remaining)
        result
        (let ([term (car remaining)])
          (loop (cdr remaining)
                (chain-add-term result (car term) (cdr term)))))))

;;; ============================================================
;;; TOPOLOGICAL INVARIANTS
;;; ============================================================

;;; sc-euler : SC → Integer
;;; Compute Euler characteristic: χ = Σ (-1)^k f_k
;;; where f_k is the number of k-simplices.
(define (sc-euler sc)
  (let ([max-d (sc-max-dim sc)])
    (let loop ([k 0] [sum 0])
      (if (> k max-d)
          sum
          (let ([sign (if (even? k) 1 -1)]
                [count (sc-count-dim sc k)])
            (loop (+ k 1) (+ sum (* sign count))))))))

;;; sc-f-vector : SC → (List Integer)
;;; Get the f-vector: (f_0, f_1, ..., f_d) where f_k = number of k-simplices.
(define (sc-f-vector sc)
  (let ([max-d (sc-max-dim sc)])
    (if (< max-d 0)
        '()
        (map (lambda (k) (sc-count-dim sc k))
             (iota (+ max-d 1))))))

;;; ============================================================
;;; FILTRATION SUPPORT
;;; ============================================================

;;; A filtered simplex carries a filtration value (birth time).
;;; Filtered Simplex = (filtered-simplex simplex value)

;;; make-filtered-simplex : Simplex × Number → FilteredSimplex
(define (make-filtered-simplex s value)
  (list 'filtered-simplex s value))

;;; filtered-simplex? : α → Boolean
(define (filtered-simplex? x)
  (and (pair? x) (eq? (car x) 'filtered-simplex)))

;;; filtered-simplex-base : FilteredSimplex → Simplex
(define (filtered-simplex-base fs)
  (cadr fs))

;;; filtered-simplex-value : FilteredSimplex → Number
(define (filtered-simplex-value fs)
  (caddr fs))

;;; ============================================================
;;; CONVENIENCE CONSTRUCTORS
;;; ============================================================

;;; vertex : Vertex → Simplex
;;; Create a 0-simplex (single vertex).
(define (vertex v)
  (make-simplex (list v)))

;;; edge : Vertex × Vertex → Simplex
;;; Create a 1-simplex from two vertices.
(define (edge v1 v2)
  (make-simplex (list v1 v2)))

;;; triangle : Vertex × Vertex × Vertex → Simplex
;;; Create a 2-simplex from three vertices.
(define (triangle v1 v2 v3)
  (make-simplex (list v1 v2 v3)))

;;; tetrahedron : Vertex × Vertex × Vertex × Vertex → Simplex
;;; Create a 3-simplex from four vertices.
(define (tetrahedron v1 v2 v3 v4)
  (make-simplex (list v1 v2 v3 v4)))

;;; ============================================================
;;; STANDARD SIMPLICIAL COMPLEXES
;;; ============================================================

;;; sc-simplex : Integer → SC
;;; Create the standard n-simplex with vertices 0, 1, ..., n.
(define (sc-simplex n)
  (sc-from-simplices (list (make-simplex (iota (+ n 1))))))

;;; sc-boundary-of-simplex : Integer → SC
;;; Create the boundary of the standard n-simplex.
;;; This is homeomorphic to S^(n-1).
(define (sc-boundary-of-simplex n)
  (let ([s (make-simplex (iota (+ n 1)))])
    (sc-from-simplices (simplex-facets s))))

;;; sc-discrete : (List Vertex) → SC
;;; Create a discrete complex (just vertices, no higher simplices).
(define (sc-discrete vertices)
  (sc-from-simplices (map vertex vertices)))
