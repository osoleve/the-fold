;;; fabric/stitches/fp/graph-matrix.ss — Matrix-Based Graph Representations
;;;
;;; Convert graphs to adjacency matrices and related matrix representations.
;;; Enables linear algebra-based graph algorithms.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Adjacency matrix conversion (dense)
;;;   - Weighted and unweighted graphs
;;;   - Directed and undirected support
;;;   - Degree matrix
;;;   - Incidence matrix
;;;   - Matrix to graph conversion
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/graph.ss
;;;   - vec.ss
;;;   - matrix.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/graph.ss")
(load "fabric/stitches/vec.ss")
(load "fabric/stitches/matrix.ss")

;;; ============================================================
;;; Adjacency Matrix
;;; ============================================================
;;;
;;; The adjacency matrix A of a graph G = (V, E) is an |V| × |V| matrix where:
;;;   - A[i,j] = weight of edge (i,j) if edge exists
;;;   - A[i,j] = 0 otherwise
;;;
;;; For undirected graphs, A is symmetric: A[i,j] = A[j,i]

;;; graph->adjacency-matrix : Graph -> Matrix
;;; Convert graph to adjacency matrix.
;;; Vertices are ordered as they appear in (graph-vertices g).
;;; Returns a square matrix of size |V| × |V|.
(define (graph->adjacency-matrix g)
  (let* ([vertices (graph-vertices g)]
         [n (length vertices)]
         [edges (graph-edges g)]
         [vertex-index (make-vertex-index vertices)])
        (if (= n 0)
            (make-matrix 0 0 0)
            (let* ([rows (build-adjacency-rows vertices edges vertex-index)])
                  (matrix-from-lists rows)))))

;;; graph->adjacency-matrix-unweighted : Graph -> Matrix
;;; Convert graph to unweighted adjacency matrix (entries are 0 or 1).
(define (graph->adjacency-matrix-unweighted g)
  (let* ([vertices (graph-vertices g)]
         [n (length vertices)]
         [edges (graph-edges g)]
         [vertex-index (make-vertex-index vertices)])
        (if (= n 0)
            (make-matrix 0 0 0)
            (let* ([rows (build-adjacency-rows-unweighted vertices edges vertex-index)])
                  (matrix-from-lists rows)))))

;;; build-adjacency-rows-unweighted : List Vertex -> AdjList -> IndexFn -> List (List Real)
(define (build-adjacency-rows-unweighted vertices edges vertex-index)
  (map (lambda (from-vertex)
               (let* ([neighbors (get-neighbors edges from-vertex)]
                      [n (length vertices)])
                     (build-row-unweighted n neighbors vertex-index)))
       vertices))

;;; build-row-unweighted : Nat -> List (Vertex . Weight) -> IndexFn -> List Real
(define (build-row-unweighted n neighbors vertex-index)
  (let ([row (make-list n 0)])
       (fold-left (lambda (r neighbor-edge)
                          (let ([j (vertex-index (car neighbor-edge))])
                               (list-set r j 1)))
                  row
                  neighbors)))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; build-adjacency-rows : List Vertex -> AdjList -> IndexFn -> List (List Real)
;;; Build adjacency matrix rows as lists.
(define (build-adjacency-rows vertices edges vertex-index)
  (map (lambda (from-vertex)
               (let* ([i (vertex-index from-vertex)]
                      [neighbors (get-neighbors edges from-vertex)]
                      [n (length vertices)])
                     (build-row n neighbors vertex-index)))
       vertices))

;;; build-row : Nat -> List (Vertex . Weight) -> IndexFn -> List Real
;;; Build a single row of the adjacency matrix.
(define (build-row n neighbors vertex-index)
  (let ([row (make-list n 0)])
       (fold-left (lambda (r neighbor-edge)
                          (let* ([neighbor (car neighbor-edge)]
                                 [weight (cdr neighbor-edge)]
                                 [j (vertex-index neighbor)])
                                (list-set r j weight)))
                  row
                  neighbors)))

;;; list-set : List a -> Nat -> a -> List a
;;; Functionally update list at index (returns new list).
(define (list-set lst idx val)
  (let loop ([i 0] [xs lst])
       (cond
        [(null? xs) '()]
        [(= i idx) (cons val (cdr xs))]
        [else (cons (car xs) (loop (+ i 1) (cdr xs)))])))

;;; make-vertex-index : List Vertex -> (Vertex -> Nat)
;;; Create a function that maps vertices to their index in the list.
(define (make-vertex-index vertices)
  (let ([index-map (make-index-map vertices 0)])
       (lambda (v)
               (let ([result (assoc v index-map)])
                    (if result
                        (cdr result)
                        0)))))  ; Fallback (shouldn't happen)

;;; make-index-map : List Vertex -> Nat -> Alist
(define (make-index-map vertices i)
  (if (null? vertices)
      '()
      (cons (cons (car vertices) i)
            (make-index-map (cdr vertices) (+ i 1)))))

;;; get-neighbors : AdjList -> Vertex -> List (Vertex . Weight)
;;; Get the neighbor list for a vertex from the adjacency list.
(define (get-neighbors edges vertex)
  (let ([found (assoc vertex edges)])
       (if found
           (cdr found)
           '())))

;;; ============================================================
;;; Degree Matrix
;;; ============================================================
;;;
;;; The degree matrix D is a diagonal matrix where:
;;;   - D[i,i] = degree of vertex i
;;;   - D[i,j] = 0 for i ≠ j

;;; graph->degree-matrix : Graph -> Matrix
;;; Convert graph to degree matrix.
;;; For directed graphs, uses out-degree.
(define (graph->degree-matrix g)
  (let* ([vertices (graph-vertices g)]
         [n (length vertices)]
         [edges (graph-edges g)])
        (if (= n 0)
            (make-matrix 0 0 '())
            (let* ([vertex-index (make-vertex-index vertices)]
                   [degrees (map (lambda (v)
                                         (degree-of-vertex edges v))
                                 vertices)]
                   [m (make-matrix n n 0)])
                  ;; Fill diagonal with degrees
                  (let loop ([i 0] [degs degrees] [mat m])
                       (if (null? degs)
                           mat
                           (loop (+ i 1)
                                 (cdr degs)
                                 (matrix-set mat i i (car degs)))))))))

;;; degree-of-vertex : AdjList -> Vertex -> Real
;;; Calculate the total degree (sum of edge weights) for a vertex.
(define (degree-of-vertex edges vertex)
  (let ([neighbors (get-neighbors edges vertex)])
       (fold-left (lambda (sum edge)
                          (+ sum (cdr edge)))
                  0
                  neighbors)))

;;; graph->degree-matrix-unweighted : Graph -> Matrix
;;; Degree matrix for unweighted graph (count of edges).
(define (graph->degree-matrix-unweighted g)
  (let* ([vertices (graph-vertices g)]
         [n (length vertices)]
         [edges (graph-edges g)])
        (if (= n 0)
            (make-matrix 0 0 '())
            (let* ([vertex-index (make-vertex-index vertices)]
                   [degrees (map (lambda (v)
                                         (length (get-neighbors edges v)))
                                 vertices)]
                   [m (make-matrix n n 0)])
                  (let loop ([i 0] [degs degrees] [mat m])
                       (if (null? degs)
                           mat
                           (loop (+ i 1)
                                 (cdr degs)
                                 (matrix-set mat i i (car degs)))))))))

;;; ============================================================
;;; Incidence Matrix
;;; ============================================================
;;;
;;; The incidence matrix B is an |V| × |E| matrix where:
;;;   - For undirected: B[v,e] = 1 if vertex v is incident to edge e
;;;   - For directed: B[v,e] = -1 if v is source, +1 if v is target

;;; graph->incidence-matrix : Graph -> Matrix
;;; Convert graph to incidence matrix.
;;; For directed graphs: -1 for source, +1 for target.
;;; For undirected graphs: 1 for both endpoints.
(define (graph->incidence-matrix g)
  (let* ([vertices (graph-vertices g)]
         [n (length vertices)]
         [vertex-index (make-vertex-index vertices)]
         [edge-list (graph->edge-list g)]
         [m-edges (length edge-list)])
        (if (or (= n 0) (= m-edges 0))
            (make-matrix n 0 '())
            (let ([directed? (graph-directed? g)])
                 (let loop ([row 0] [mat (make-matrix n m-edges 0)])
                      (if (>= row n)
                          mat
                          (let ([vertex (list-ref vertices row)])
                               (loop (+ row 1)
                                     (fill-incidence-row mat row vertex
                                                         edge-list vertex-index
                                                         directed?)))))))))

;;; fill-incidence-row : Matrix -> Nat -> Vertex -> EdgeList -> IndexFn -> Bool -> Matrix
(define (fill-incidence-row mat row vertex edge-list vertex-index directed?)
  (let loop ([col 0] [edges edge-list] [m mat])
       (if (null? edges)
           m
           (let* ([edge (car edges)]
                  [src (car edge)]
                  [dst (cadr edge)]
                  [new-m (cond
                          [(equal? vertex src)
                           (if directed?
                               (matrix-set m row col -1)
                               (matrix-set m row col 1))]
                          [(equal? vertex dst)
                           (matrix-set m row col 1)]
                          [else m])])
                 (loop (+ col 1) (cdr edges) new-m)))))

;;; graph->edge-list : Graph -> List (Vertex Vertex Weight)
;;; Convert graph edges to a list of triples.
(define (graph->edge-list g)
  (let ([edges (graph-edges g)]
        [directed? (graph-directed? g)])
       (fold-left
        (lambda (acc vertex-edges)
                (let ([src (car vertex-edges)]
                      [neighbors (cdr vertex-edges)])
                     (fold-left
                      (lambda (acc2 neighbor-edge)
                              (let* ([dst (car neighbor-edge)]
                                     [weight (cdr neighbor-edge)]
                                     [edge (list src dst weight)])
                                    ;; For undirected, only include each edge once
                                    (if (and (not directed?)
                                             (vertex<? dst src))  ; Skip reverse edge
                                        acc2
                                        (cons edge acc2))))
                      acc
                      neighbors)))
        '()
        edges)))

;;; vertex<? : Vertex -> Vertex -> Boolean
;;; Lexicographic comparison for vertices (used for canonical edge ordering).
(define (vertex<? v1 v2)
  (string<? (format "~a" v1) (format "~a" v2)))

;;; ============================================================
;;; Matrix to Graph Conversion
;;; ============================================================

;;; adjacency-matrix->graph : Matrix -> Boolean -> List Vertex -> Graph
;;; Convert adjacency matrix to graph.
;;; threshold: treat values > threshold as edges (for weighted graphs, use 0)
(define (adjacency-matrix->graph m directed? vertices . threshold-opt)
  (let ([n (matrix-rows m)]
        [threshold (if (null? threshold-opt) 0 (car threshold-opt))])
       (if (not (= n (length vertices)))
           (error 'adjacency-matrix->graph
                  "Matrix dimension doesn't match vertex count")
           (let ([g (graph-add-vertices (make-graph directed?) vertices)])
                (let loop ([i 0] [graph g])
                     (if (>= i n)
                         graph
                         (let inner ([j 0] [g2 graph])
                              (if (>= j n)
                                  (loop (+ i 1) g2)
                                  (let ([weight (matrix-ref m i j)])
                                       (if (> weight threshold)
                                           (inner (+ j 1)
                                                  (graph-add-edge g2
                                                                  (list-ref vertices i)
                                                                  (list-ref vertices j)
                                                                  weight))
                                           (inner (+ j 1) g2)))))))))))

;;; ============================================================
;;; Matrix Properties and Queries
;;; ============================================================

;;; adjacency-matrix-is-symmetric? : Matrix -> Boolean
;;; Check if adjacency matrix is symmetric (indicates undirected graph).
(define (adjacency-matrix-is-symmetric? m)
  (matrix-symmetric? m))

;;; get-edge-weight : Matrix -> Nat -> Nat -> Real
;;; Get edge weight from adjacency matrix.
(define (get-edge-weight m i j)
  (matrix-ref m i j))

;;; has-edge? : Matrix -> Nat -> Nat -> Boolean
;;; Check if edge exists in adjacency matrix (weight > 0).
(define (has-edge? m i j)
  (> (matrix-ref m i j) 0))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; graph-vertices->indices : Graph -> Alist
;;; Get vertex to index mapping for a graph.
(define (graph-vertices->indices g)
  (make-index-map (graph-vertices g) 0))

;;; print-adjacency-matrix : Graph -> Void
;;; Print adjacency matrix in readable format (for debugging).
(define (print-adjacency-matrix g)
  (let* ([m (graph->adjacency-matrix g)]
         [vertices (graph-vertices g)]
         [n (length vertices)])
        (display "Adjacency Matrix:\n")
        (display "    ")
        (for-each (lambda (v) (display (format "~a " v)))
                  vertices)
        (newline)
        (let loop ([i 0])
             (when (< i n)
                   (display (format "~a: " (list-ref vertices i)))
                   (let inner ([j 0])
                        (when (< j n)
                              (display (format "~a " (matrix-ref m i j)))
                              (inner (+ j 1))))
                   (newline)
                   (loop (+ i 1))))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================
;;;
;;; Adjacency Matrix:
;;;   graph->adjacency-matrix
;;;   graph->adjacency-matrix-unweighted
;;;   adjacency-matrix->graph
;;;   get-edge-weight
;;;   has-edge?
;;;
;;; Degree Matrix:
;;;   graph->degree-matrix
;;;   graph->degree-matrix-unweighted
;;;
;;; Incidence Matrix:
;;;   graph->incidence-matrix
;;;
;;; Utilities:
;;;   graph-vertices->indices
;;;   print-adjacency-matrix
;;;   adjacency-matrix-is-symmetric?
