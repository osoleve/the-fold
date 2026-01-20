(load "lattice/data/set.ss")
(load "lattice/data/sort.ss")

(doc 'module 'simplicial-complex)
(doc 'description "Core data structures for computational topology")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'tier 1)
(doc 'dependencies '(data/set data/sort))

(doc 'note "A simplex is a generalization of a triangle to arbitrary dimensions:
  0-simplex = vertex (point)
  1-simplex = edge (line segment)
  2-simplex = triangle
  3-simplex = tetrahedron
  n-simplex = n+1 vertices in general position")

(doc 'note "A simplicial complex is a collection of simplices satisfying:
  - Closure: Every face of a simplex in the complex is also in the complex
  - Intersection: Two simplices intersect in a common face (or not at all)")

(doc 'provides "1. Simplex representation and operations
2. Simplicial complex construction and manipulation
3. Face enumeration and boundary computation
4. Star, link, and skeleton operations
5. Filtration support for persistent homology")

(doc 'section 'generic-comparison)

(define (generic<? a b)
  (doc 'type '(-> α α Boolean))
  (doc 'description "Universal comparison function for canonical ordering")
  (doc 'note "Handles: numbers, symbols, strings, bytevectors, and lists (lexicographically)")
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
    ; Both bytevectors (lexicographic on bytes)
    [(and (bytevector? a) (bytevector? b))
     (bytevector<? a b)]
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

(define (bytevector<? a b)
  (doc 'type '(-> Bytevector Bytevector Boolean))
  (doc 'description "Lexicographic comparison of bytevectors")
  (doc 'note "Compares byte-by-byte; shorter vector is less if prefix matches")
  (let ([len-a (bytevector-length a)]
        [len-b (bytevector-length b)])
    (let loop ([i 0])
      (cond
        [(= i len-a) (< len-a len-b)]  ; a exhausted, a < b iff b is longer
        [(= i len-b) #f]                ; b exhausted first, a >= b
        [else
         (let ([byte-a (bytevector-u8-ref a i)]
               [byte-b (bytevector-u8-ref b i)])
           (cond
             [(< byte-a byte-b) #t]
             [(> byte-a byte-b) #f]
             [else (loop (+ i 1))]))]))))

(define (type-order x)
  (doc 'type '(-> α Integer))
  (doc 'description "Assign numeric order to types for consistent mixed-type comparison")
  (cond
    [(number? x) 0]
    [(symbol? x) 1]
    [(string? x) 2]
    [(bytevector? x) 3]
    [(list? x) 4]
    [else 5]))

(doc 'section 'simplex)

(doc 'note "Simplex = (simplex vertices) where vertices is a sorted list of vertex labels (any comparable values)")

(define (make-simplex vertices)
  (doc 'type '(-> (List Vertex) Simplex))
  (doc 'description "Create a simplex from a list of vertices")
  (doc 'note "Vertices are sorted for canonical representation. Duplicate vertices are removed (degenerate simplices normalized)")
  (list 'simplex (unique-sorted (sort-by generic<? vertices))))

(define (simplex? x)
  (doc 'type '(-> α Boolean))
  (doc 'description "Check if value is a simplex")
  (and (pair? x) (eq? (car x) 'simplex)))

(define (simplex-vertices s)
  (doc 'type '(-> Simplex (List Vertex)))
  (doc 'description "Get the sorted vertex list of a simplex")
  (cadr s))

(define (simplex-dim s)
  (doc 'type '(-> Simplex Integer))
  (doc 'description "Get the dimension of a simplex (number of vertices - 1)")
  (doc 'note "A 0-simplex (single vertex) has dimension 0")
  (- (length (simplex-vertices s)) 1))

(define (simplex-empty? s)
  (doc 'type '(-> Simplex Boolean))
  (doc 'description "Check if simplex is the empty simplex (no vertices)")
  (null? (simplex-vertices s)))

(define (simplex-equal? s1 s2)
  (doc 'type '(-> Simplex Simplex Boolean))
  (doc 'description "Check if two simplices are equal (same vertices)")
  (doc 'note "Since vertices are sorted, we can use equal?")
  (equal? (simplex-vertices s1) (simplex-vertices s2)))

(define (simplex-contains-vertex? s v)
  (doc 'type '(-> Simplex Vertex Boolean))
  (doc 'description "Check if a simplex contains a specific vertex")
  (set-member? v (simplex-vertices s)))

(define (simplex-face? s1 s2)
  (doc 'type '(-> Simplex Simplex Boolean))
  (doc 'description "Check if s1 is a face of s2 (s1's vertices are a subset of s2's)")
  (set-subset? (simplex-vertices s1) (simplex-vertices s2)))

(doc 'section 'face-enumeration)

(define (remove-at lst i)
  (doc 'type '(-> (List α) Integer (List α)))
  (doc 'description "Remove element at index i from list")
  (let loop ([remaining lst] [idx 0] [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [(= idx i) (loop (cdr remaining) (+ idx 1) acc)]
      [else (loop (cdr remaining) (+ idx 1) (cons (car remaining) acc))])))

(define (simplex-facets s)
  (doc 'type '(-> Simplex (List Simplex)))
  (doc 'description "Get all facets (codimension-1 faces) of a simplex")
  (doc 'note "Each facet is obtained by removing one vertex. Returns list of (dim-1)-simplices. A vertex (0-simplex) has no facets")
  (let* ([verts (simplex-vertices s)]
         [n (length verts)])
    (if (<= n 1)
        '()  ; Empty simplex and vertices have no facets
        (let loop ([i 0] [acc '()])
          (if (>= i n)
              (reverse acc)
              (loop (+ i 1)
                    (cons (make-simplex (remove-at verts i)) acc)))))))

(define (simplex-faces-of-dim s k)
  (doc 'type '(-> Simplex Integer (List Simplex)))
  (doc 'description "Get all k-dimensional faces of a simplex")
  (doc 'note "A k-face has k+1 vertices")
  (let ([verts (simplex-vertices s)]
        [needed (+ k 1)])
    (if (or (< k 0) (> needed (length verts)))
        '()
        (map make-simplex (combinations verts needed)))))

(define (combinations lst k)
  (doc 'type '(-> (List α) Integer (List (List α))))
  (doc 'description "Generate all k-combinations of elements from a list")
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

(define (simplex-all-faces s)
  (doc 'type '(-> Simplex (List Simplex)))
  (doc 'description "Get all faces of a simplex, including the simplex itself")
  (doc 'note "Ordered from dimension 0 up to the simplex's dimension")
  (let ([d (simplex-dim s)])
    (apply append
           (map (lambda (k) (simplex-faces-of-dim s k))
                (iota (+ d 1))))))

(define (simplex-proper-faces s)
  (doc 'type '(-> Simplex (List Simplex)))
  (doc 'description "Get all proper faces (faces excluding the simplex itself)")
  (let ([d (simplex-dim s)])
    (if (< d 0)
        '()
        (apply append
               (map (lambda (k) (simplex-faces-of-dim s k))
                    (iota d))))))

(define (iota n)
  (doc 'type '(-> Integer (List Integer)))
  (doc 'description "Generate list (0 1 2 ... n-1)")
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

(doc 'section 'simplicial-complex)

(doc 'note "Simplicial Complex = (sc simplices-by-dim max-dim) where simplices-by-dim is a vector of simplex lists indexed by dimension")

(doc sc-empty 'type 'SimplicalComplex)
(doc sc-empty 'description "The empty simplicial complex")
(define sc-empty
  (list 'sc (vector) -1))

(define (sc? x)
  (doc 'type '(-> α Boolean))
  (and (pair? x) (eq? (car x) 'sc)))

(define (sc-simplices-by-dim sc)
  (doc 'type '(-> SC Vector))
  (cadr sc))

(define (sc-max-dim sc)
  (doc 'type '(-> SC Integer))
  (caddr sc))

(define (sc-ensure-dim sc dim)
  (doc 'type '(-> SC Integer SC))
  (doc 'description "Ensure the complex has room for simplices of given dimension")
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

(define (sc-add-simplex-only sc s)
  (doc 'type '(-> SC Simplex SC))
  (doc 'description "Add a simplex WITHOUT adding its faces (internal use)")
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

(define (simplex-in-list? s lst)
  (doc 'type '(-> Simplex (List Simplex) Boolean))
  (cond
    [(null? lst) #f]
    [(simplex-equal? s (car lst)) #t]
    [else (simplex-in-list? s (cdr lst))]))

(define (sc-add-simplex sc s)
  (doc 'type '(-> SC Simplex SC))
  (doc 'description "Add a simplex and all its faces to the complex")
  (doc 'note "This ensures the closure property is maintained")
  (let loop ([sc sc] [to-add (cons s (simplex-proper-faces s))])
    (if (null? to-add)
        sc
        (loop (sc-add-simplex-only sc (car to-add))
              (cdr to-add)))))

(define (sc-from-simplices simplices)
  (doc 'type '(-> (List Simplex) SC))
  (doc 'description "Build a simplicial complex from a list of simplices")
  (doc 'note "Automatically adds all faces to maintain closure property")
  (let loop ([sc sc-empty] [remaining simplices])
    (if (null? remaining)
        sc
        (loop (sc-add-simplex sc (car remaining))
              (cdr remaining)))))

(define (sc-contains? sc s)
  (doc 'type '(-> SC Simplex Boolean))
  (doc 'description "Check if a simplex is in the complex")
  (let* ([dim (simplex-dim s)]
         [vec (sc-simplices-by-dim sc)])
    (if (>= dim (vector-length vec))
        #f
        (simplex-in-list? s (vector-ref vec dim)))))

(define (sc-simplices sc)
  (doc 'type '(-> SC (List Simplex)))
  (doc 'description "Get all simplices in the complex as a flat list")
  (let* ([vec (sc-simplices-by-dim sc)]
         [n (vector-length vec)])
    (let loop ([i 0] [acc '()])
      (if (>= i n)
          (reverse acc)
          (loop (+ i 1) (append (reverse (vector-ref vec i)) acc))))))

(define (sc-simplices-dim sc k)
  (doc 'type '(-> SC Integer (List Simplex)))
  (doc 'description "Get all simplices of a specific dimension")
  (let ([vec (sc-simplices-by-dim sc)])
    (if (or (< k 0) (>= k (vector-length vec)))
        '()
        (vector-ref vec k))))

(define (sc-vertices sc)
  (doc 'type '(-> SC (List Vertex)))
  (doc 'description "Get all vertices (0-simplices) in the complex")
  (map (lambda (s) (car (simplex-vertices s)))
       (sc-simplices-dim sc 0)))

(define (sc-edges sc)
  (doc 'type '(-> SC (List Simplex)))
  (doc 'description "Get all edges (1-simplices) in the complex")
  (sc-simplices-dim sc 1))

(define (sc-faces sc)
  (doc 'type '(-> SC (List Simplex)))
  (doc 'description "Get all faces (2-simplices) in the complex")
  (sc-simplices-dim sc 2))

(define (sc-count sc)
  (doc 'type '(-> SC Integer))
  (doc 'description "Count total number of simplices")
  (length (sc-simplices sc)))

(define (sc-count-dim sc k)
  (doc 'type '(-> SC Integer Integer))
  (doc 'description "Count simplices of a given dimension")
  (length (sc-simplices-dim sc k)))

(doc 'section 'skeleton-subcomplex)

(define (sc-skeleton sc k)
  (doc 'type '(-> SC Integer SC))
  (doc 'description "Get the k-skeleton: all simplices of dimension ≤ k")
  (let* ([vec (sc-simplices-by-dim sc)]
         [n (min (+ k 1) (vector-length vec))]
         [new-vec (make-vector n '())])
    (let loop ([i 0])
      (when (< i n)
        (vector-set! new-vec i (vector-ref vec i))
        (loop (+ i 1))))
    (list 'sc new-vec k)))

(define (sc-star sc s)
  (doc 'type '(-> SC Simplex (List Simplex)))
  (doc 'description "Get the star of a simplex: all simplices that contain it as a face")
  (filter (lambda (t) (simplex-face? s t))
          (sc-simplices sc)))

(define (sc-closed-star sc s)
  (doc 'type '(-> SC Simplex SC))
  (doc 'description "Get the closed star: the smallest subcomplex containing the star")
  (sc-from-simplices (sc-star sc s)))

(define (sc-link sc s)
  (doc 'type '(-> SC Simplex SC))
  (doc 'description "Get the link of a simplex: faces of the closed star that don't intersect s")
  (doc 'note "Link(s) = {t ∈ Cl(St(s)) | t ∩ s = ∅}")
  (let* ([closed-star (sc-closed-star sc s)]
         [s-verts (simplex-vertices s)])
    (sc-from-simplices
      (filter (lambda (t)
                (null? (set-intersection (simplex-vertices t) s-verts)))
              (sc-simplices closed-star)))))

(doc 'section 'boundary-operator)

(doc 'note "A chain is a formal linear combination of simplices.
Chain = ((coeff . simplex) ...)
For now we use integer coefficients (Z-chains)")

(define (make-chain terms)
  (doc 'type '(-> (List (Pair Integer Simplex)) Chain))
  terms)

(doc chain-empty 'type 'Chain)
(define chain-empty '())

(define (chain-add-term chain coeff s)
  (doc 'type '(-> Chain Integer Simplex Chain))
  (doc 'description "Add a term to a chain, combining coefficients if simplex already present")
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

(define (simplex-boundary s)
  (doc 'type '(-> Simplex Chain))
  (doc 'description "Compute the boundary of a simplex as a chain")
  (doc 'note "∂[v0, v1, ..., vn] = Σ (-1)^i [v0, ..., v̂i, ..., vn] where v̂i means vi is omitted")
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

(define (chain-boundary chain)
  (doc 'type '(-> Chain Chain))
  (doc 'description "Extend boundary operator linearly to chains")
  (doc 'note "∂(Σ ai·σi) = Σ ai·∂σi")
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

(define (chain-add-terms chain terms)
  (doc 'type '(-> Chain (List Term) Chain))
  (let loop ([remaining terms] [result chain])
    (if (null? remaining)
        result
        (let ([term (car remaining)])
          (loop (cdr remaining)
                (chain-add-term result (car term) (cdr term)))))))

(doc 'section 'topological-invariants)

(define (sc-euler sc)
  (doc 'type '(-> SC Integer))
  (doc 'description "Compute Euler characteristic: χ = Σ (-1)^k f_k")
  (doc 'note "where f_k is the number of k-simplices")
  (let ([max-d (sc-max-dim sc)])
    (let loop ([k 0] [sum 0])
      (if (> k max-d)
          sum
          (let ([sign (if (even? k) 1 -1)]
                [count (sc-count-dim sc k)])
            (loop (+ k 1) (+ sum (* sign count))))))))

(define (sc-f-vector sc)
  (doc 'type '(-> SC (List Integer)))
  (doc 'description "Get the f-vector: (f_0, f_1, ..., f_d)")
  (doc 'note "where f_k = number of k-simplices")
  (let ([max-d (sc-max-dim sc)])
    (if (< max-d 0)
        '()
        (map (lambda (k) (sc-count-dim sc k))
             (iota (+ max-d 1))))))

(doc 'section 'filtration-support)

(doc 'note "A filtered simplex carries a filtration value (birth time).
Filtered Simplex = (filtered-simplex simplex value)")

(define (make-filtered-simplex s value)
  (doc 'type '(-> Simplex Number FilteredSimplex))
  (list 'filtered-simplex s value))

(define (filtered-simplex? x)
  (doc 'type '(-> α Boolean))
  (and (pair? x) (eq? (car x) 'filtered-simplex)))

(define (filtered-simplex-base fs)
  (doc 'type '(-> FilteredSimplex Simplex))
  (cadr fs))

(define (filtered-simplex-value fs)
  (doc 'type '(-> FilteredSimplex Number))
  (caddr fs))

(doc 'section 'convenience-constructors)

(define (vertex v)
  (doc 'type '(-> Vertex Simplex))
  (doc 'description "Create a 0-simplex (single vertex)")
  (make-simplex (list v)))

(define (edge v1 v2)
  (doc 'type '(-> Vertex Vertex Simplex))
  (doc 'description "Create a 1-simplex from two vertices")
  (make-simplex (list v1 v2)))

(define (triangle v1 v2 v3)
  (doc 'type '(-> Vertex Vertex Vertex Simplex))
  (doc 'description "Create a 2-simplex from three vertices")
  (make-simplex (list v1 v2 v3)))

(define (tetrahedron v1 v2 v3 v4)
  (doc 'type '(-> Vertex Vertex Vertex Vertex Simplex))
  (doc 'description "Create a 3-simplex from four vertices")
  (make-simplex (list v1 v2 v3 v4)))

(doc 'section 'standard-complexes)

(define (sc-simplex n)
  (doc 'type '(-> Integer SC))
  (doc 'description "Create the standard n-simplex with vertices 0, 1, ..., n")
  (sc-from-simplices (list (make-simplex (iota (+ n 1))))))

(define (sc-boundary-of-simplex n)
  (doc 'type '(-> Integer SC))
  (doc 'description "Create the boundary of the standard n-simplex")
  (doc 'note "This is homeomorphic to S^(n-1)")
  (let ([s (make-simplex (iota (+ n 1)))])
    (sc-from-simplices (simplex-facets s))))

(define (sc-discrete vertices)
  (doc 'type '(-> (List Vertex) SC))
  (doc 'description "Create a discrete complex (just vertices, no higher simplices)")
  (sc-from-simplices (map vertex vertices)))
