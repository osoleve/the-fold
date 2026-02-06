;;; lattice/data/graph-algorithms.ss — Pure Graph Data Structures & Homology
;;; @module graph-algorithms
;;; @requires prelude sort

(load "core/base/prelude.ss")
(load "lattice/data/sort.ss")

(doc 'module 'graph-algorithms)
(doc 'description "Pure graph data structures and homology-based cycle analysis.
  Provides visited sets, queues, stacks, hash utilities, and algebraic topology
  tools for analyzing graph structure via simplicial homology.")
(doc 'layer 'lattice)

(doc 'note "DESIGN PRINCIPLES:
  - Pure functional — no I/O or store access
  - Efficient visited-set tracking to avoid cycles
  - Composable data structures for graph algorithms
  - Homology-based analysis via topology/homology.ss")

(doc 'note "Store-dependent traversal and analysis functions (BFS, DFS, pathfinding,
  connected components, centrality, etc.) live in boundary/data/graph-traversal.ss")

(load "lattice/data/collection-utils.ss")

;;; Dependencies for homology-based analysis (Section 8)
;;; Uses topology/homology.ss for Z_2 homology computation (canonical implementation)
;;; All homology functions now use Z_2 arithmetic via this module
(load "lattice/topology/homology.ss")

(doc bytevector-hash 'type '(-> Bytevector Integer))
(doc bytevector-hash 'description "Hash function for bytevectors (FNV-1a inspired)")
(define (bytevector-hash bv)
  (let ([len (bytevector-length bv)])
       (let loop ([i 0] [h 2166136261])  ; FNV offset basis
            (if (>= i len)
                (bitwise-and h #xFFFFFFFF)  ; Keep 32-bit
                (loop (+ i 1)
                      (bitwise-and
                       (* (bitwise-xor h (bytevector-u8-ref bv i))
                          16777619)  ; FNV prime
                       #xFFFFFFFF))))))

;;; make-visited : → HashTable
;;; Create an empty visited set (hash table).
(define (make-visited)
  (make-hashtable bytevector-hash bytevector=?))

;;; visited-add : HashTable Hash → HashTable
;;; Add a hash to the visited set. Mutates and returns the table.
(define (visited-add visited hash)
  (hashtable-set! visited hash #t)
  visited)

;;; visited-contains? : HashTable Hash → Boolean
;;; Check if hash is in visited set. O(1) lookup.
(define (visited-contains? visited hash)
  (hashtable-ref visited hash #f))

;;; visited-remove : HashTable Hash → HashTable
;;; Remove a hash from the visited set. Used for backtracking.
(define (visited-remove visited hash)
  (hashtable-delete! visited hash)
  visited)

;;; --- Queue Operations (for BFS) ---
;;; Using Okasaki's two-list queue for amortized O(1) operations.
;;; Queue = (front-list . back-list)
;;; - Enqueue: cons to back
;;; - Dequeue: pop from front, reverse back if front empty

;;; make-queue : → Queue
;;; Create an empty queue.
(define (make-queue)
  (cons '() '()))

;;; queue-empty? : Queue → Boolean
;;; Check if queue is empty.
(define (queue-empty? q)
  (and (null? (car q)) (null? (cdr q))))

;;; queue-normalize : Queue → Queue
;;; Internal: ensure front is non-empty if possible.
(define (queue-normalize q)
  (if (null? (car q))
      (cons (reverse (cdr q)) '())
      q))

;;; queue-enqueue : Queue Any → Queue
;;; Add element to back of queue. O(1).
(define (queue-enqueue q item)
  (queue-normalize (cons (car q) (cons item (cdr q)))))

;;; queue-enqueue-all : Queue (List Any) → Queue
;;; Add all elements to back of queue. O(k) where k = length of items.
(define (queue-enqueue-all q items)
  (queue-normalize (cons (car q) (append (reverse items) (cdr q)))))

;;; queue-dequeue : Queue → (Values Any Queue)
;;; Remove and return front element. Amortized O(1).
(define (queue-dequeue q)
  (let ([nq (queue-normalize q)])
       (values (car (car nq))
               (queue-normalize (cons (cdr (car nq)) (cdr nq))))))

;;; --- Stack Operations (for DFS) ---

;;; make-stack : → (List Any)
;;; Create an empty stack (as a list).
(define (make-stack)
  '())

;;; stack-empty? : (List Any) → Boolean
;;; Check if stack is empty.
(define (stack-empty? s)
  (null? s))

;;; stack-push : (List Any) Any → (List Any)
;;; Push element onto stack.
(define (stack-push s item)
  (cons item s))

;;; stack-push-all : (List Any) (List Any) → (List Any)
;;; Push all elements onto stack (first in list will be on top).
(define (stack-push-all s items)
  (append items s))

;;; stack-pop : (List Any) → (Values Any (List Any))
;;; Pop and return top element.
(define (stack-pop s)
  (values (car s) (cdr s)))

;;; --- Hash Utilities ---

;;; hash-equal? : Hash Hash → Boolean
;;; Compare two hashes for equality.
(define (hash-equal? h1 h2)
  (bytevector=? h1 h2))

;;; hash-in-list? : Hash (List Hash) → Boolean
;;; Check if hash is in a list of hashes.
(define (hash-in-list? hash lst)
  (let loop ([l lst])
       (cond
        [(null? l) #f]
        [(bytevector=? (car l) hash) #t]
        [else (loop (cdr l))])))

;;; remove-hash : Hash (List Hash) → (List Hash)
;;; Remove a hash from a list.
(define (remove-hash hash lst)
  (filter (lambda (h) (not (bytevector=? h hash))) lst))

;;; unique-hashes : (List Hash) → (List Hash)
;;; Remove duplicate hashes from a list.
;;; Uses bytevector=? for hash comparison (not equal?), so cannot use prelude's unique.
(define (unique-hashes hashes)
  (let loop ([remaining hashes]
             [seen '()]
             [result '()])
       (if (null? remaining)
           (reverse result)
           (let ([h (car remaining)])
                (if (hash-in-list? h seen)
                    (loop (cdr remaining) seen result)
                    (loop (cdr remaining)
                          (cons h seen)
                          (cons h result)))))))

;;; --- List Utilities ---

;;; take-up-to : (List A) Integer → (List A)
;;; Take up to n elements from list.
(define (take-up-to lst n)
  (let loop ([l lst] [count n] [result '()])
       (if (or (null? l) (<= count 0))
           (reverse result)
           (loop (cdr l) (- count 1) (cons (car l) result)))))

;;; path-length : (List Hash) → Integer
;;; Get length of a path (number of edges).
(define (path-length path)
  (if (null? path)
      0
      (- (length path) 1)))

;;; --- Cycle Utilities ---

;;; unique-cycles : (List (List Hash)) → (List (List Hash))
;;; Remove duplicate cycles from list.
;;; Two cycles are duplicates if they contain the same nodes (set equality).
(define (unique-cycles cycles)
  (let loop ([remaining cycles]
             [result '()])
       (if (null? remaining)
           result
           (let ([cycle (car remaining)])
                (if (cycle-in-list? cycle result)
                    (loop (cdr remaining) result)
                    (loop (cdr remaining) (cons cycle result)))))))

;;; cycle-in-list? : (List Hash) (List (List Hash)) → Boolean
;;; Check if cycle is already in list (as same set of nodes).
(define (cycle-in-list? cycle cycles)
  (let ([cycle-set (cdr cycle)])  ; Remove duplicate endpoint
       (let loop ([cs cycles])
            (if (null? cs)
                #f
                (let ([other-set (cdr (car cs))])
                     (if (same-hash-set? cycle-set other-set)
                         #t
                         (loop (cdr cs))))))))

;;; same-hash-set? : (List Hash) (List Hash) → Boolean
;;; Check if two lists contain the same hashes (as sets).
;;; Uses hash table for O(N) complexity instead of O(N*M).
(define (same-hash-set? lst1 lst2)
  (and (= (length lst1) (length lst2))
       (let ([hash-set (make-hashtable equal-hash equal?)])
            (for-each (lambda (h) (hashtable-set! hash-set h #t)) lst2)
            (let loop ([l lst1])
                 (if (null? l)
                     #t
                     (and (hashtable-contains? hash-set (car l))
                          (loop (cdr l))))))))


;;; ====
;;; Section 8: Homology-Based Cycle Analysis
;;; ====

(doc 'section 'homology-based-cycle-analysis)
(doc 'description "Algebraic topology tools for analyzing cycles in graphs using simplicial homology")
(doc 'note "H_0 (0-th homology) captures connected components; H_1 (1st homology) captures independent cycles")
(doc 'note "Betti numbers: beta_0 = number of connected components, beta_1 = number of independent cycles")
(doc 'note "All homology functions use canonical Z_2 implementation from topology/homology.ss for exact mod-2 arithmetic")

(doc graph->simplicial-complex 'type '(-> (List Edge) (List Vertex) SC))
(doc graph->simplicial-complex 'description "Convert graph to 1-dimensional simplicial complex; vertices become 0-simplices, edges become 1-simplices")
(doc graph->simplicial-complex 'note "Edge format: (v1 . v2) or (v1 v2) - both are supported")
(define (graph->simplicial-complex edges vertices)
  (let* ([vertex-simplices (map (lambda (v) (make-simplex (list v))) vertices)]
         [edge-simplices (map (lambda (e)
                                (let ([v1 (car e)]
                                      [v2 (if (pair? (cdr e))
                                              (cadr e)
                                              (cdr e))])
                                  (make-simplex (list v1 v2))))
                              edges)])
    (sc-from-simplices (append vertex-simplices edge-simplices))))

;;; graph-adjacency->simplicial-complex : AdjList → SC
;;; Convert adjacency list to simplicial complex.
;;; AdjList format: ((vertex neighbor1 neighbor2 ...) ...)
(define (graph-adjacency->simplicial-complex adj)
  (let* ([vertices (map car adj)]
         [edges '()])
    (for-each
     (lambda (entry)
       (let ([v (car entry)]
             [neighbors (cdr entry)])
         (for-each
          (lambda (n)
            (when (< v n)
              (set! edges (cons (cons v n) edges))))
          neighbors)))
     adj)
    (graph->simplicial-complex edges vertices)))

;;; --- Boundary Matrix Construction ---

;;; build-boundary-matrix-1 : SC → Matrix
;;; Build the boundary matrix ∂_1 : C_1 → C_0 for a 1-dimensional complex.
;;; Rows correspond to 0-simplices (vertices), columns to 1-simplices (edges).
(define (build-boundary-matrix-1 sc)
  (let* ([vertices (sc-vertices sc)]
         [edges (sc-edges sc)]
         [n-vertices (length vertices)]
         [n-edges (length edges)]
         [vertex-index (make-hashtable equal-hash equal?)])
    (let loop ([vs vertices] [i 0])
      (unless (null? vs)
        (hashtable-set! vertex-index (car vs) i)
        (loop (cdr vs) (+ i 1))))
    (let ([mat (make-matrix n-vertices n-edges 0)])
      (let edge-loop ([es edges] [col 0])
        (unless (null? es)
          (let* ([edge (car es)]
                 [vs (simplex-vertices edge)]
                 [v0 (car vs)]
                 [v1 (cadr vs)]
                 [row0 (hashtable-ref vertex-index v0 #f)]
                 [row1 (hashtable-ref vertex-index v1 #f)])
            (when row0 (matrix-set! mat row0 col -1))
            (when row1 (matrix-set! mat row1 col 1))
            (edge-loop (cdr es) (+ col 1)))))
      mat)))

(doc graph-betti-numbers 'type '(-> (List Edge) (List Vertex) (Pair Nat Nat)))
(doc graph-betti-numbers 'description "Compute Betti numbers using Z_2 homology: beta_0 (components), beta_1 (independent cycles)")
(doc graph-betti-numbers 'note "Uses canonical Z_2 homology implementation from topology/homology.ss with exact mod-2 arithmetic")
(define (graph-betti-numbers edges vertices)
  (let ([sc (graph->simplicial-complex edges vertices)])
    (if (null? edges)
        (cons (length vertices) 0)
        (let ([betti (sc-betti-numbers sc)])
          (cons (if (pair? betti) (car betti) 0)
                (if (and (pair? betti) (pair? (cdr betti)))
                    (cadr betti)
                    0))))))

;;; graph-betti-numbers-from-adjacency : AdjList → (beta0 . beta1)
;;; Compute Betti numbers from adjacency list representation.
(define (graph-betti-numbers-from-adjacency adj)
  (let* ([vertices (map car adj)]
         [edges '()])
    (for-each
     (lambda (entry)
       (let ([v (car entry)]
             [neighbors (cdr entry)])
         (for-each
          (lambda (n)
            (when (< v n)
              (set! edges (cons (cons v n) edges))))
          neighbors)))
     adj)
    (graph-betti-numbers edges vertices)))

(doc cycle-basis-homology 'type '(-> (List Edge) (List Vertex) (List Cycle)))
(doc cycle-basis-homology 'description "Compute basis for H_1 using Z_2 homology; returns fundamental cycles (number equals beta_1)")
(doc cycle-basis-homology 'note "Algorithm: Build Z_2 boundary matrix, find null space via z2-null-space, convert to edge lists")
(define (cycle-basis-homology edges vertices)
  (let* ([sc (graph->simplicial-complex edges vertices)]
         [n-edges (length edges)]
         [edge-list (sc-edges sc)])
    (if (= n-edges 0)
        '()
        (let* ([boundary-1 (sc-boundary-matrix sc 1)]
               [null-basis (z2-null-space boundary-1)])
          (map (lambda (null-vec)
                 (edges-from-z2-null-vector null-vec edge-list))
               null-basis)))))

;;; edges-from-z2-null-vector : (List {0,1}) × (List Simplex) → (List Edge)
;;; Convert a Z_2 null space vector to a list of edges.
(define (edges-from-z2-null-vector coeffs edge-simplices)
  (let loop ([cs coeffs] [edges edge-simplices] [result '()])
    (if (or (null? cs) (null? edges))
        (reverse result)
        (let ([coeff (car cs)]
              [edge (car edges)])
          (if (= coeff 1)
              (let* ([vs (simplex-vertices edge)]
                     [v0 (car vs)]
                     [v1 (cadr vs)])
                (loop (cdr cs) (cdr edges) (cons (cons v0 v1) result)))
              (loop (cdr cs) (cdr edges) result))))))

;;; --- Convenience Functions ---

;;; graph-euler-characteristic : (List Edge) × (List Vertex) → Integer
;;; Compute Euler characteristic: χ = V - E
(define (graph-euler-characteristic edges vertices)
  (- (length vertices) (length edges)))

;;; graph-cycle-rank : (List Edge) × (List Vertex) → Integer
;;; Compute the cycle rank (cyclomatic number): beta_1 = E - V + beta_0
(define (graph-cycle-rank edges vertices)
  (let ([betti (graph-betti-numbers edges vertices)])
    (cdr betti)))

;;; graph-is-tree? : (List Edge) × (List Vertex) → Boolean
;;; A graph is a tree iff it is connected (beta_0 = 1) and acyclic (beta_1 = 0).
(define (graph-is-tree? edges vertices)
  (let ([betti (graph-betti-numbers edges vertices)])
    (and (= (car betti) 1)
         (= (cdr betti) 0))))

;;; graph-is-forest? : (List Edge) × (List Vertex) → Boolean
;;; A graph is a forest iff it is acyclic (beta_1 = 0).
(define (graph-is-forest? edges vertices)
  (let ([betti (graph-betti-numbers edges vertices)])
    (= (cdr betti) 0)))


;;; ====
;;; Load Complete
;;; ====

(printf "✓ Graph algorithms loaded (pure)
")
(printf "  Data structures: visited-set, queue, stack, hash-utils
")
(printf "  Homology:        graph-betti-numbers, cycle-basis-homology, graph-is-tree?
")
