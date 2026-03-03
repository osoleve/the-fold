;;; lattice/topology/persistent.ss — Persistent Homology for TDA
;;; @module persistent
;;; @requires simplicial-complex hamt

(require 'hamt)

(doc 'module 'persistent)
(doc 'description "Persistent homology for topological data analysis")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'tier 1)
(doc 'dependencies '(topology/homology topology/simplicial-complex))

(doc 'note "Persistent homology tracks how topological features (connected components,
loops, voids) are born and die as we grow a simplicial complex through a filtration.

Key concepts:
  - Filtration: nested sequence K_0 ⊆ K_1 ⊆ ... ⊆ K_n of simplicial complexes
  - Birth: a feature appears when a cycle is created that isn't a boundary
  - Death: a feature disappears when that cycle becomes a boundary
  - Persistence: death_time - birth_time (how long a feature persists)

Output:
  - Persistence diagram: set of (birth, death) points in the plane
  - Barcode: collection of intervals [birth, death)")

;;; ============================================================
;;; Filtration Data Structure
;;; ============================================================

(doc 'section 'filtration)

(doc 'note "A filtration is a sequence of simplices ordered by their birth time.
We represent it as a sorted list of (simplex . birth-time) pairs.
Lower dimensions appear before higher dimensions at the same birth time
(to ensure faces are added before their cofaces).")

(define (make-filtration pairs)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Simplex Number)) Filtration))
  (doc 'description "Create a filtration from simplex-birthtime pairs")
  (doc 'note "Pairs are sorted by (birth-time, dimension, vertices) for canonical order")
  (list 'filtration
        (sort-by filtration-order pairs)))

(define (filtration? x)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (pair? x) (eq? (car x) 'filtration)))

(define (filtration-pairs f)
  (doc 'export #t)
  (doc 'type '(-> Filtration (List (Pair Simplex Number))))
  (cadr f))

(define (filtration-order p1 p2)
  (doc 'type '(-> (Pair Simplex Number) (Pair Simplex Number) Boolean))
  (doc 'description "Ordering: first by birth time, then by dimension, then by vertices")
  (let ([t1 (cdr p1)] [t2 (cdr p2)]
        [s1 (car p1)] [s2 (car p2)])
    (cond
      [(< t1 t2) #t]
      [(> t1 t2) #f]
      ; Same birth time: lower dimension first (faces before cofaces)
      [(< (simplex-dim s1) (simplex-dim s2)) #t]
      [(> (simplex-dim s1) (simplex-dim s2)) #f]
      ; Same dimension: lexicographic on vertices
      [else (lex<? (simplex-vertices s1) (simplex-vertices s2))])))

(define (lex<? lst1 lst2)
  (doc 'type '(-> (List α) (List α) Boolean))
  (cond
    [(null? lst1) (not (null? lst2))]
    [(null? lst2) #f]
    [(< (car lst1) (car lst2)) #t]
    [(> (car lst1) (car lst2)) #f]
    [else (lex<? (cdr lst1) (cdr lst2))]))

(define (filtration-simplices f)
  (doc 'export #t)
  (doc 'type '(-> Filtration (List Simplex)))
  (doc 'description "Get simplices in filtration order")
  (map car (filtration-pairs f)))

(define (filtration-size f)
  (doc 'export #t)
  (doc 'type '(-> Filtration Integer))
  (length (filtration-pairs f)))

;;; ============================================================
;;; Vietoris-Rips Complex Construction
;;; ============================================================

(doc 'section 'vietoris-rips)

(doc 'note "The Vietoris-Rips complex at scale ε includes a simplex [v0,...,vk]
if every pairwise distance d(vi,vj) ≤ ε. This is computationally simpler than
the Čech complex (which requires all points to fit in a ball of radius ε/2).")

(define (euclidean-distance p1 p2)
  (doc 'export #t)
  (doc 'type '(-> Point Point Number))
  (doc 'description "Euclidean distance between two points (represented as lists)")
  (sqrt (apply + (map (lambda (a b) (* (- a b) (- a b))) p1 p2))))

(define (rips-filtration points max-dim max-epsilon distance-fn)
  (doc 'export #t)
  (doc 'type '(-> (List Point) Integer Number (-> Point Point Number) Filtration))
  (doc 'description "Build Vietoris-Rips filtration from point cloud")
  (doc 'note "Returns a filtration where each simplex has birth time = max pairwise distance")
  (doc 'note "Complexity: O(n^(d+1)) where n = #points, d = max-dim. Use max-dim ≤ 2 for large clouds")
  (let* ([n (length points)]
         [point-vec (list->vector points)]
         ; Compute all pairwise distances
         [dist-matrix (build-distance-matrix point-vec n distance-fn)]
         ; Build simplices up to max-dim
         [pairs (build-rips-pairs point-vec dist-matrix n max-dim max-epsilon)])
    (make-filtration pairs)))

(define (build-distance-matrix points n distance-fn)
  (doc 'type '(-> Vector Integer (-> α α Number) Vector))
  (doc 'description "Build upper-triangular distance matrix")
  (let ([matrix (make-vector n)])
    (do ([i 0 (+ i 1)])
        [(= i n) matrix]
      (let ([row (make-vector n 0.0)])
        (do ([j (+ i 1) (+ j 1)])
            [(= j n)]
          (vector-set! row j (distance-fn (vector-ref points i)
                                          (vector-ref points j))))
        (vector-set! matrix i row)))))

(define (dist-ref matrix i j)
  (doc 'type '(-> Vector Integer Integer Number))
  (if (< i j)
      (vector-ref (vector-ref matrix i) j)
      (vector-ref (vector-ref matrix j) i)))

(define (build-rips-pairs points dist-matrix n max-dim max-epsilon)
  (doc 'type '(-> Vector Vector Integer Integer Number (List (Pair Simplex Number))))
  (doc 'description "Build all Rips simplices up to max-dim with birth times")
  (doc 'note "Uses recursive combination generation for arbitrary dimensions")
  ;; Collect vertex pairs, then append k-simplex pairs for each dimension
  (let ([vertex-pairs
         (let loop ([i 0] [acc '()])
           (if (= i n)
               acc
               (loop (+ i 1) (cons (cons (make-simplex (list i)) 0.0) acc))))])
    (let loop ([k 1] [result vertex-pairs])
      (if (> k max-dim)
          result
          (loop (+ k 1)
                (append result (rips-k-simplices dist-matrix n k max-epsilon)))))))

(define (rips-k-simplices dist-matrix n k max-epsilon)
  (doc 'type '(-> Vector Integer Integer Number (List (Pair Simplex Number))))
  (doc 'description "Build all k-simplices (k+1 vertices) for Rips complex")
  (fold-combinations
    (iota n) (+ k 1)
    (lambda (acc vertices)
      (let ([max-d (max-pairwise-distance dist-matrix vertices)])
        (if (<= max-d max-epsilon)
            (cons (cons (make-simplex vertices) max-d) acc)
            acc)))
    '()))

(define (max-pairwise-distance dist-matrix vertices)
  (doc 'type '(-> Vector (List Integer) Number))
  (doc 'description "Maximum pairwise distance among vertices (Rips diameter)")
  (let loop-i ([vs vertices] [max-d 0.0])
    (if (null? vs)
        max-d
        (let ([i (car vs)])
          (let loop-j ([ws (cdr vs)] [max-d max-d])
            (if (null? ws)
                (loop-i (cdr vs) max-d)
                (loop-j (cdr ws)
                        (max max-d (dist-ref dist-matrix i (car ws))))))))))

(define (for-each-combination lst k proc)
  (doc 'type '(-> (List α) Integer (-> (List α) Void) Void))
  (doc 'description "Call proc on each k-combination of lst")
  (when (>= (length lst) k)
    (if (= k 0)
        (proc '())
        (if (= k (length lst))
            (proc lst)
            (let ([first (car lst)]
                  [rest (cdr lst)])
              ; Combinations including first
              (for-each-combination rest (- k 1)
                (lambda (combo) (proc (cons first combo))))
              ; Combinations not including first
              (for-each-combination rest k proc))))))

(define (fold-combinations lst k f init)
  (doc 'type '(-> (List α) Integer (-> β (List α) β) β β))
  (doc 'description "Fold f over each k-combination of lst, threading accumulator")
  (if (< (length lst) k)
      init
      (if (= k 0)
          (f init '())
          (if (= k (length lst))
              (f init lst)
              (let ([first (car lst)]
                    [rest (cdr lst)])
                ;; Fold over combinations including first, then those without
                (let ([acc (fold-combinations rest (- k 1)
                             (lambda (a combo) (f a (cons first combo)))
                             init)])
                  (fold-combinations rest k f acc)))))))

;;; ============================================================
;;; Persistence Algorithm (Standard Reduction)
;;; ============================================================

(doc 'section 'persistence-algorithm)

(doc 'note "The standard algorithm for persistent homology:
1. Order simplices by filtration value (birth time)
2. Build boundary matrix D where D[i,j] = 1 if simplex_i is a facet of simplex_j
3. Reduce D from left to right using column operations (over Z_2)
4. Pair simplices: if column j reduces to have lowest 1 at row i, then (i,j) is a pair
5. Unpaired columns in dimension k represent infinite persistence (essential features)

The reduction maintains: D_reduced = D * R where R is upper-triangular.
Column j's 'low' (index of lowest 1) gives the pairing.")

(define (persistence-reduce filtration)
  (doc 'export #t)
  (doc 'type '(-> Filtration PersistenceResult))
  (doc 'description "Run the standard persistence algorithm")
  (doc 'returns "(persistence-result pairs essential birth-times)")
  (let* ([pairs (filtration-pairs filtration)]
         [n (length pairs)]
         [simplices (list->vector (map car pairs))]
         [birth-times (list->vector (map cdr pairs))]
         ; Build index tables for fast lookup
         [simplex-to-idx (build-simplex-index simplices n)]
         ; Initialize boundary matrix as sparse columns
         [columns (build-boundary-columns simplices n simplex-to-idx)]
         ; Track low[j] = index of lowest 1 in column j, or -1 if zero
         [low (make-vector n -1)]
         ; Paired simplices
         [paired (make-vector n #f)])
    ; Reduce columns left to right, threading low-to-col HAMT
    (let ([result-pairs
           (let loop ([j 0] [low-to-col hamt-empty] [rp '()])
             (if (= j n)
                 rp
                 (let ([new-low-to-col (reduce-column! columns j low low-to-col)])
                   (let ([low-j (vector-ref low j)])
                     (if (>= low-j 0)
                         (begin
                           ; Column j has low = i, forming a persistence pair
                           (vector-set! paired j #t)
                           (vector-set! paired low-j #t)
                           (loop (+ j 1) new-low-to-col (cons (cons low-j j) rp)))
                         (loop (+ j 1) new-low-to-col rp))))))])
      ; Collect essential (unpaired) simplices by dimension
      (let ([essential (collect-essential simplices paired n)])
        (make-persistence-result
          (reverse result-pairs)
          essential
          birth-times
          simplices)))))

(define (build-simplex-index simplices n)
  (doc 'type '(-> Vector Integer HAMT))
  (doc 'description "Map simplex vertices -> filtration index")
  (let loop ([i 0] [table hamt-empty])
    (if (= i n) table
        (loop (+ i 1)
              (hamt-assoc (simplex-vertices (vector-ref simplices i)) i table)))))

(define (build-boundary-columns simplices n simplex-to-idx)
  (doc 'type '(-> Vector Integer HAMT Vector))
  (doc 'description "Build boundary matrix as sparse columns (sorted index lists)")
  (let ([columns (make-vector n '())])
    (do ([j 0 (+ j 1)])
        [(= j n) columns]
      (let* ([s (vector-ref simplices j)]
             [facets (simplex-facets s)]
             [indices (filter-map
                        (lambda (f)
                          (hamt-lookup (simplex-vertices f) simplex-to-idx))
                        facets)])
        ; Store as sorted list (descending for easy low access)
        (vector-set! columns j (sort-by > indices))))))

;;; filter-map provided by prelude

(define (reduce-column! columns j low low-to-col)
  (doc 'type '(-> Vector Integer Vector HAMT HAMT))
  (doc 'description "Reduce column j until its low is unique or zero. Returns updated low-to-col HAMT.")
  (let loop ([low-to-col low-to-col])
    (let ([col-j (vector-ref columns j)])
      (if (null? col-j)
          ; Column is zero
          (begin (vector-set! low j -1)
                 low-to-col)
          (let ([low-j (car col-j)])  ; First element is lowest (descending order)
            ; Check if another column has the same low
            (let ([other (hamt-lookup low-j low-to-col)])
              (if other
                  ; Add column 'other' to column j (XOR in Z_2)
                  (begin
                    (vector-set! columns j
                                 (symmetric-difference col-j (vector-ref columns other)))
                    (loop low-to-col))
                  ; Column j's low is unique
                  (begin
                    (vector-set! low j low-j)
                    (hamt-assoc low-j j low-to-col)))))))))

(define (symmetric-difference lst1 lst2)
  (doc 'type '(-> (List Integer) (List Integer) (List Integer)))
  (doc 'description "XOR of two sorted descending lists (Z_2 addition)")
  (let loop ([l1 lst1] [l2 lst2] [acc '()])
    (cond
      [(null? l1) (append (reverse acc) l2)]
      [(null? l2) (append (reverse acc) l1)]
      [(= (car l1) (car l2))
       ; Same element: cancels in Z_2
       (loop (cdr l1) (cdr l2) acc)]
      [(> (car l1) (car l2))
       (loop (cdr l1) l2 (cons (car l1) acc))]
      [else
       (loop l1 (cdr l2) (cons (car l2) acc))])))

(define (collect-essential simplices paired n)
  (doc 'type '(-> Vector Vector Integer (List (List Integer))))
  (doc 'description "Collect unpaired simplices grouped by dimension")
  (let ([by-dim
         (let loop ([i 0] [acc hamt-empty])
           (if (= i n) acc
               (if (vector-ref paired i)
                   (loop (+ i 1) acc)
                   (let* ([s (vector-ref simplices i)]
                          [d (simplex-dim s)]
                          [existing (hamt-lookup-or d acc '())])
                     (loop (+ i 1) (hamt-assoc d (cons i existing) acc))))))])
    ; Convert to sorted list of (dim . indices)
    (let ([dims (sort-by < (hamt-keys by-dim))])
      (map (lambda (d) (cons d (reverse (hamt-lookup-or d by-dim '()))))
           dims))))

;;; ============================================================
;;; Persistence Result Structure
;;; ============================================================

(doc 'section 'persistence-result)

(define (make-persistence-result pairs essential birth-times simplices)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Int Int)) (List (Pair Int (List Int))) Vector Vector PersistenceResult))
  (list 'persistence-result pairs essential birth-times simplices))

(define (persistence-result? x)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (pair? x) (eq? (car x) 'persistence-result)))

(define (persistence-pairs pr)
  (doc 'export #t)
  (doc 'type '(-> PersistenceResult (List (Pair Int Int))))
  (doc 'description "Get birth-death index pairs")
  (cadr pr))

(define (persistence-essential pr)
  (doc 'export #t)
  (doc 'type '(-> PersistenceResult (List (Pair Int (List Int)))))
  (doc 'description "Get essential (infinite persistence) indices grouped by dimension")
  (caddr pr))

(define (persistence-birth-times pr)
  (doc 'type '(-> PersistenceResult Vector))
  (cadddr pr))

(define (persistence-simplices pr)
  (doc 'type '(-> PersistenceResult Vector))
  (car (cddddr pr)))

;;; ============================================================
;;; Persistence Diagram
;;; ============================================================

(doc 'section 'persistence-diagram)

(doc 'note "A persistence diagram is a multiset of points in the extended plane:
  - Each point (b, d) represents a feature born at time b, dying at time d
  - Points on the diagonal b=d have zero persistence (trivial)
  - Points with d=∞ represent essential features (never die)
  - Distance from diagonal = persistence = d - b")

(define (make-persistence-diagram pr)
  (doc 'export #t)
  (doc 'type '(-> PersistenceResult PersistenceDiagram))
  (doc 'description "Extract persistence diagram from reduction result")
  (let* ([pairs (persistence-pairs pr)]
         [essential (persistence-essential pr)]
         [times (persistence-birth-times pr)]
         [simplices (persistence-simplices pr)]
         ; Finite points: (birth, death, dimension)
         [finite-points
          (map (lambda (pair)
                 (let* ([i (car pair)]
                        [j (cdr pair)]
                        [birth (vector-ref times i)]
                        [death (vector-ref times j)]
                        [dim (simplex-dim (vector-ref simplices i))])
                   (list birth death dim)))
               pairs)]
         ; Essential points: (birth, +inf, dimension)
         [essential-points
          (append-map (lambda (dim-group)
                        (let ([dim (car dim-group)]
                              [indices (cdr dim-group)])
                          (map (lambda (i)
                                 (list (vector-ref times i) +inf.0 dim))
                               indices)))
                      essential)])
    (list 'persistence-diagram
          (append finite-points essential-points))))

(define (persistence-diagram? x)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (pair? x) (eq? (car x) 'persistence-diagram)))

(define (diagram-points pd)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram (List (List Number Number Integer))))
  (doc 'description "Get points as (birth death dimension) triples")
  (cadr pd))

(define (diagram-points-dim pd dim)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram Integer (List (List Number Number))))
  (doc 'description "Get points for a specific dimension as (birth death) pairs")
  (filter-map (lambda (pt)
                (if (= (caddr pt) dim)
                    (list (car pt) (cadr pt))
                    #f))
              (diagram-points pd)))

(define (diagram-betti pd dim time)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram Integer Number Integer))
  (doc 'description "Compute Betti number at a given time")
  (doc 'note "Counts features alive at time t: born <= t < death")
  (length (filter (lambda (pt)
                    (and (= (caddr pt) dim)
                         (<= (car pt) time)
                         (> (cadr pt) time)))
                  (diagram-points pd))))

;;; ============================================================
;;; Barcode Representation
;;; ============================================================

(doc 'section 'barcode)

(define (diagram->barcode pd)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram Barcode))
  (doc 'description "Convert persistence diagram to barcode format")
  (doc 'note "Barcode groups intervals by dimension")
  (let* ([points (diagram-points pd)]
         [max-dim (apply max 0 (map caddr points))]
         [by-dim (make-vector (+ max-dim 1) '())])
    (for-each (lambda (pt)
                (let ([dim (caddr pt)]
                      [interval (list (car pt) (cadr pt))])
                  (vector-set! by-dim dim
                               (cons interval (vector-ref by-dim dim)))))
              points)
    ; Sort intervals by birth time within each dimension
    (list 'barcode
          (let loop ([d 0] [acc '()])
            (if (> d max-dim)
                (reverse acc)
                (loop (+ d 1)
                      (cons (cons d (sort-by car< (vector-ref by-dim d)))
                            acc)))))))

(define (car< p1 p2) (< (car p1) (car p2)))

(define (barcode? x)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (pair? x) (eq? (car x) 'barcode)))

(define (barcode-intervals bc dim)
  (doc 'export #t)
  (doc 'type '(-> Barcode Integer (List (List Number Number))))
  (doc 'description "Get intervals for dimension k as (birth death) pairs")
  (let ([entry (assv dim (cadr bc))])
    (if entry (cdr entry) '())))

(define (barcode-print bc)
  (doc 'export #t)
  (doc 'type '(-> Barcode Void))
  (doc 'description "Print barcode in ASCII format")
  (printf "~n=== Persistence Barcode ===~n")
  (for-each
    (lambda (dim-entry)
      (let ([dim (car dim-entry)]
            [intervals (cdr dim-entry)])
        (printf "~nH_~a (~a intervals):~n" dim (length intervals))
        (for-each
          (lambda (int)
            (let* ([birth (car int)]
                   [death (cadr int)]
                   [pers (if (infinite? death) "+inf" (- death birth))])
              (printf "  [~,3f, ~a) persistence=~a~n"
                      birth
                      (if (infinite? death) "+inf" (format "~,3f" death))
                      pers)))
          intervals)))
    (cadr bc))
  (printf "~n===========================~n"))

;;; ============================================================
;;; Bottleneck Distance
;;; ============================================================

(doc 'section 'distance-metrics)

(doc 'note "The bottleneck distance measures the similarity of two persistence diagrams.
It finds the optimal matching between points (plus diagonal projections) that
minimizes the maximum distance between matched pairs.

d_B(D1, D2) = inf_{γ} sup_x ||x - γ(x)||_∞

where γ ranges over all bijections and ||·||_∞ is the L∞ distance.")

(define (diagram-bottleneck d1 d2 dim)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram PersistenceDiagram Integer Number))
  (doc 'description "Compute bottleneck distance between diagrams in dimension dim")
  (doc 'note "Uses a simple O(n³) algorithm. For large diagrams, use Hungarian method")
  (let* ([pts1 (filter-finite (diagram-points-dim d1 dim))]
         [pts2 (filter-finite (diagram-points-dim d2 dim))]
         [n1 (length pts1)]
         [n2 (length pts2)])
    (if (and (= n1 0) (= n2 0))
        0.0
        ; Simple greedy approximation (not optimal but gives upper bound)
        (bottleneck-greedy pts1 pts2))))

(define (filter-finite pts)
  (doc 'type '(-> (List (List Number Number)) (List (List Number Number))))
  (filter (lambda (pt) (not (infinite? (cadr pt)))) pts))

(define (point-distance p1 p2)
  (doc 'type '(-> (List Number Number) (List Number Number) Number))
  (doc 'description "L∞ distance between persistence points")
  (max (abs (- (car p1) (car p2)))
       (abs (- (cadr p1) (cadr p2)))))

(define (diagonal-distance pt)
  (doc 'type '(-> (List Number Number) Number))
  (doc 'description "Distance from point to diagonal (persistence / 2)")
  (/ (- (cadr pt) (car pt)) 2))

(define (bottleneck-greedy pts1 pts2)
  (doc 'type '(-> (List Point) (List Point) Number))
  (doc 'description "Greedy matching approximation to bottleneck distance")
  (doc 'note "Greedily matches each point to its closest available partner or diagonal.
This gives an O(n²) 2-approximation to the true bottleneck distance.")
  (let* ([n1 (length pts1)]
         [n2 (length pts2)]
         [vec1 (list->vector pts1)]
         [vec2 (list->vector pts2)]
         ; Track which points are matched
         [matched1 (make-vector n1 #f)]
         [matched2 (make-vector n2 #f)]
         [max-cost 0.0])
    ; Greedy: for each point in pts1, find best match in pts2 or use diagonal
    (do ([i 0 (+ i 1)])
        [(= i n1)]
      (let* ([p1 (vector-ref vec1 i)]
             [diag-cost (diagonal-distance p1)]
             [best-j #f]
             [best-cost diag-cost])
        ; Find closest unmatched point in pts2
        (do ([j 0 (+ j 1)])
            [(= j n2)]
          (unless (vector-ref matched2 j)
            (let ([cost (point-distance p1 (vector-ref vec2 j))])
              (when (< cost best-cost)
                (set! best-j j)
                (set! best-cost cost)))))
        ; Make the match
        (vector-set! matched1 i #t)
        (when best-j
          (vector-set! matched2 best-j #t))
        (set! max-cost (max max-cost best-cost))))
    ; Remaining unmatched points in pts2 go to diagonal
    (do ([j 0 (+ j 1)])
        [(= j n2)]
      (unless (vector-ref matched2 j)
        (set! max-cost (max max-cost (diagonal-distance (vector-ref vec2 j))))))
    max-cost))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(doc 'section 'convenience)

(define (compute-persistence points max-dim max-epsilon)
  (doc 'export #t)
  (doc 'type '(-> (List Point) Integer Number PersistenceDiagram))
  (doc 'description "One-shot: compute persistence diagram from point cloud")
  (let* ([filtration (rips-filtration points max-dim max-epsilon euclidean-distance)]
         [result (persistence-reduce filtration)]
         [diagram (make-persistence-diagram result)])
    diagram))

(define (persistence-summary pd)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram Void))
  (doc 'description "Print summary of persistence diagram")
  (let* ([points (diagram-points pd)]
         [max-dim (if (null? points) -1 (apply max (map caddr points)))])
    (printf "~n=== Persistence Summary ===~n")
    (printf "Total features: ~a~n" (length points))
    (do ([d 0 (+ d 1)])
        [(> d max-dim)]
      (let* ([dim-pts (filter (lambda (p) (= (caddr p) d)) points)]
             [finite (filter (lambda (p) (not (infinite? (cadr p)))) dim-pts)]
             [essential (filter (lambda (p) (infinite? (cadr p))) dim-pts)])
        (printf "H_~a: ~a finite, ~a essential~n"
                d (length finite) (length essential))))
    (printf "===========================~n")))
