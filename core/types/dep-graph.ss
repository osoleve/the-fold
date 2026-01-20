(load "core/base/prelude.ss")
(load "core/types/dep-types.ss")

(doc 'module 'dep-graph)
(doc 'description "Provides dependent type constructors for encoding and verifying graph properties statically.")
(doc 'layer 'core)

(doc 'section 'graph-property-type-constructors)

(doc 'section 'type-predicates)

(define (acyclic-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if this is an acyclic graph proof type: (Acyclic g).")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) 'Acyclic)))

(define (connected-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if this is a connected graph proof type: (Connected g).")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) 'Connected)))

(define (tree-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if this is a tree proof type: (Tree g).")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) 'Tree)))

(define (dag-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if this is a DAG proof type: (DAG g).")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) 'DAG)))

(define (path-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if this is a path proof type: (Path g v1 v2).")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) 'Path)))

(define (graph-property-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if this is any graph property type.")
  (doc 'export #t)
  (or (acyclic-type? t)
      (connected-type? t)
      (tree-type? t)
      (dag-type? t)
      (path-type? t)))

(doc 'section 'well-formedness-checks)

(define (acyclic-type-well-formed? t)
  (doc 'type (-> SExpr Boolean))
  (doc 'description "Check if (Acyclic g) is well-formed.")
  (doc 'export #t)
  (and (pair? t)
       (eq? (car t) 'Acyclic)
       (= (length t) 2)))

(define (connected-type-well-formed? t)
  (doc 'type (-> SExpr Boolean))
  (doc 'description "Check if (Connected g) is well-formed.")
  (doc 'export #t)
  (and (pair? t)
       (eq? (car t) 'Connected)
       (= (length t) 2)))

(define (tree-type-well-formed? t)
  (doc 'type (-> SExpr Boolean))
  (doc 'description "Check if (Tree g) is well-formed.")
  (doc 'export #t)
  (and (pair? t)
       (eq? (car t) 'Tree)
       (= (length t) 2)))

(define (dag-type-well-formed? t)
  (doc 'type (-> SExpr Boolean))
  (doc 'description "Check if (DAG g) is well-formed.")
  (doc 'export #t)
  (and (pair? t)
       (eq? (car t) 'DAG)
       (= (length t) 2)))

(define (path-type-well-formed? t)
  (doc 'type (-> SExpr Boolean))
  (doc 'description "Check if (Path g v1 v2) is well-formed.")
  (doc 'export #t)
  (and (pair? t)
       (eq? (car t) 'Path)
       (= (length t) 4)))

(doc 'section 'type-extractors)

(define (acyclic-graph t)
  (doc 'type (-> Type Expr))
  (doc 'description "Get the graph from (Acyclic g).")
  (doc 'export #t)
  (if (acyclic-type? t)
      (cadr t)
      #f))

(define (connected-graph t)
  (doc 'type (-> Type Expr))
  (doc 'description "Get the graph from (Connected g).")
  (doc 'export #t)
  (if (connected-type? t)
      (cadr t)
      #f))

(define (tree-graph t)
  (doc 'type (-> Type Expr))
  (doc 'description "Get the graph from (Tree g).")
  (doc 'export #t)
  (if (tree-type? t)
      (cadr t)
      #f))

(define (dag-graph t)
  (doc 'type (-> Type Expr))
  (doc 'description "Get the graph from (DAG g).")
  (doc 'export #t)
  (if (dag-type? t)
      (cadr t)
      #f))

(define (path-graph t)
  (doc 'type (-> Type Expr))
  (doc 'description "Get the graph from (Path g v1 v2).")
  (doc 'export #t)
  (if (path-type? t)
      (cadr t)
      #f))

(define (path-source t)
  (doc 'type (-> Type Expr))
  (doc 'description "Get the source vertex from (Path g v1 v2).")
  (doc 'export #t)
  (if (path-type? t)
      (caddr t)
      #f))

(define (path-target t)
  (doc 'type (-> Type Expr))
  (doc 'description "Get the target vertex from (Path g v1 v2).")
  (doc 'export #t)
  (if (path-type? t)
      (cadddr t)
      #f))

(doc 'section 'type-constructors)

(define (make-acyclic-type g)
  (doc 'type (-> Expr Type))
  (doc 'description "Construct an acyclic graph type.")
  (doc 'export #t)
  `(Acyclic ,g))

(define (make-connected-type g)
  (doc 'type (-> Expr Type))
  (doc 'description "Construct a connected graph type.")
  (doc 'export #t)
  `(Connected ,g))

(define (make-tree-type g)
  (doc 'type (-> Expr Type))
  (doc 'description "Construct a tree type.")
  (doc 'export #t)
  `(Tree ,g))

(define (make-dag-type g)
  (doc 'type (-> Expr Type))
  (doc 'description "Construct a DAG type.")
  (doc 'export #t)
  `(DAG ,g))

(define (make-path-type g v1 v2)
  (doc 'type (-> Expr Expr Expr Type))
  (doc 'description "Construct a path type (Path g v1 v2).")
  (doc 'export #t)
  `(Path ,g ,v1 ,v2))

(doc 'section 'type-relationships)

(define (tree-implies-acyclic tree-type)
  (doc 'type (-> Type Type))
  (doc 'description "A tree is always acyclic.")
  (doc 'export #t)
  (if (tree-type? tree-type)
      (make-acyclic-type (tree-graph tree-type))
      #f))

(define (tree-implies-connected tree-type)
  (doc 'type (-> Type Type))
  (doc 'description "A tree is always connected.")
  (doc 'export #t)
  (if (tree-type? tree-type)
      (make-connected-type (tree-graph tree-type))
      #f))

(define (dag-implies-acyclic dag-type)
  (doc 'type (-> Type Type))
  (doc 'description "A DAG is always acyclic.")
  (doc 'export #t)
  (if (dag-type? dag-type)
      (make-acyclic-type (dag-graph dag-type))
      #f))

(define (acyclic-connected-implies-tree acyclic-proof connected-proof)
  (doc 'type (-> Type Type Type))
  (doc 'description "An acyclic connected graph is a tree. Requires both proofs to be for the same graph.")
  (doc 'export #t)
  (if (and (acyclic-type? acyclic-proof)
           (connected-type? connected-proof)
           (equal? (acyclic-graph acyclic-proof) (connected-graph connected-proof)))
      (make-tree-type (acyclic-graph acyclic-proof))
      #f))

(define (path-reflexive g v)
  (doc 'type (-> Graph Node Type))
  (doc 'description "Every node has a path to itself (empty path).")
  (doc 'export #t)
  (make-path-type g v v))

(define (path-transitive path1 path2)
  (doc 'type (-> Type Type Type))
  (doc 'description "Paths are transitive (concatenation).")
  (doc 'export #t)
  (if (and (path-type? path1)
           (path-type? path2)
           (equal? (path-graph path1) (path-graph path2))
           (equal? (path-target path1) (path-source path2)))
      (make-path-type (path-graph path1)
                      (path-source path1)
                      (path-target path2))
      #f))

(doc 'section 'type-signatures-for-graph-operations)

(doc dep-graph-types 'type (List (Pair Symbol Type)))
(doc dep-graph-types 'description "Type signatures for graph operations.")
(doc dep-graph-types 'export #t)
(define dep-graph-types
  '(;; Acyclicity verification
    ;; check-acyclic : Π g:Graph. Either (Acyclic g) (CycleProof g)
    (check-acyclic . (Π ((g : Graph))
                        (+ (Acyclic g)
                           (CycleProof g))))

    ;; Connectivity verification
    ;; check-connected : Π g:Graph. Either (Connected g) (DisconnectedProof g)
    (check-connected . (Π ((g : Graph))
                          (+ (Connected g)
                             (DisconnectedProof g))))

    ;; Tree verification
    ;; check-tree : Π g:Graph. Either (Tree g) (NotTreeProof g)
    (check-tree . (Π ((g : Graph))
                     (+ (Tree g)
                        (NotTreeProof g))))

    ;; DAG verification
    ;; check-dag : Π g:Graph. Either (DAG g) (CycleProof g)
    (check-dag . (Π ((g : Graph))
                    (+ (DAG g)
                       (CycleProof g))))

    ;; Path existence check
    ;; path-exists : Π g:Graph. Π v1:(Node g). Π v2:(Node g). Either (Path g v1 v2) (NoPathProof g v1 v2)
    (path-exists . (Π ((g : Graph))
                      (Π ((v1 : (Node g)))
                         (Π ((v2 : (Node g)))
                            (+ (Path g v1 v2)
                               (NoPathProof g v1 v2))))))

    ;; Property-preserving add-edge
    ;; add-edge-acyclic : Π g:Graph. Π e:Edge. (Acyclic g) → Either (Acyclic (add-edge g e)) (CycleProof (add-edge g e))
    (add-edge-acyclic . (Π ((g : Graph))
                           (Π ((e : Edge))
                              (-> (Acyclic g)
                                  (+ (Acyclic (add-edge g e))
                                     (CycleProof (add-edge g e)))))))

    ;; Topological sort (requires DAG)
    ;; topological-sort-dag : Π g:Graph. (DAG g) → (SortedNodes g)
    (topological-sort-dag . (Π ((g : Graph))
                               (-> (DAG g)
                                   (SortedNodes g))))

    ;; Path concatenation
    ;; concat-paths : Π g:Graph. Π v1:(Node g). Π v2:(Node g). Π v3:(Node g).
    ;;                (Path g v1 v2) → (Path g v2 v3) → (Path g v1 v3)
    (concat-paths . (Π ((g : Graph))
                       (Π ((v1 : (Node g)))
                          (Π ((v2 : (Node g)))
                             (Π ((v3 : (Node g)))
                                (-> (Path g v1 v2)
                                    (Path g v2 v3)
                                    (Path g v1 v3)))))))

    ;; Connected implies all pairs have paths
    ;; connected-path : Π g:Graph. (Connected g) → Π v1:(Node g). Π v2:(Node g). (Path g v1 v2)
    (connected-path . (Π ((g : Graph))
                         (-> (Connected g)
                             (Π ((v1 : (Node g)))
                                (Π ((v2 : (Node g)))
                                   (Path g v1 v2))))))

    ;; Extract acyclic proof from tree
    ;; tree-to-acyclic : Π g:Graph. (Tree g) → (Acyclic g)
    (tree-to-acyclic . (Π ((g : Graph))
                          (-> (Tree g) (Acyclic g))))

    ;; Extract connected proof from tree
    ;; tree-to-connected : Π g:Graph. (Tree g) → (Connected g)
    (tree-to-connected . (Π ((g : Graph))
                            (-> (Tree g) (Connected g))))

    ;; Extract acyclic proof from DAG
    ;; dag-to-acyclic : Π g:Graph. (DAG g) → (Acyclic g)
    (dag-to-acyclic . (Π ((g : Graph))
                         (-> (DAG g) (Acyclic g))))

    ;; Spanning tree extraction
    ;; spanning-tree : Π g:Graph. (Connected g) → (Σ ((t : Graph)) (Tree t))
    (spanning-tree . (Π ((g : Graph))
                        (-> (Connected g)
                            (Σ ((t : Graph)) (Tree t)))))
    ))

(doc 'section 'runtime-verification-functions)

(define (verify-acyclic g)
  (doc 'type (-> Graph (Either (Acyclic g) (CycleProof g))))
  (doc 'description "Verify that a graph is acyclic. Returns a proof of acyclicity or a cycle witness.")
  (doc 'export #t)
  ;; This would call find-cycles from graph-algorithms.ss
  ;; For now, we return a tagged result
  `(verify-acyclic-result ,g))

(define (verify-connected g)
  (doc 'type (-> Graph (Either (Connected g) (DisconnectedProof g))))
  (doc 'description "Verify that a graph is connected.")
  (doc 'export #t)
  ;; This would call connected-components from graph-algorithms.ss
  `(verify-connected-result ,g))

(define (verify-dag g)
  (doc 'type (-> Graph (Either (DAG g) (CycleProof g))))
  (doc 'description "Verify that a graph is a DAG.")
  (doc 'export #t)
  ;; This would call topological-sort from graph-algorithms.ss
  `(verify-dag-result ,g))

(define (find-path g v1 v2)
  (doc 'type (-> Graph Node Node (Either (Path g v1 v2) (NoPath g v1 v2))))
  (doc 'description "Find a path between two nodes.")
  (doc 'export #t)
  ;; This would call path-exists? from graph-algorithms.ss
  `(find-path-result ,g ,v1 ,v2))

(doc 'section 'pretty-printing)

(define (graph-property-type->string t)
  (doc 'type (-> Type String))
  (doc 'description "Convert a graph property type to a readable string.")
  (doc 'export #t)
  (cond
   [(acyclic-type? t)
    (string-append "Acyclic(" (format "~a" (acyclic-graph t)) ")")]
   [(connected-type? t)
    (string-append "Connected(" (format "~a" (connected-graph t)) ")")]
   [(tree-type? t)
    (string-append "Tree(" (format "~a" (tree-graph t)) ")")]
   [(dag-type? t)
    (string-append "DAG(" (format "~a" (dag-graph t)) ")")]
   [(path-type? t)
    (string-append "Path(" (format "~a" (path-graph t))
                   ", " (format "~a" (path-source t))
                   " → " (format "~a" (path-target t)) ")")]
   [else (format "~s" t)]))

(doc 'section 'type-context-integration)

(define (make-dep-graph-ctx)
  (doc 'type (-> (List (Pair Symbol Type))))
  (doc 'description "Creates a type context with all dependent graph operations.")
  (doc 'export #t)
  dep-graph-types)

(define (extend-ctx-with-dep-graph ctx)
  (doc 'type (-> (List (Pair Symbol Type)) (List (Pair Symbol Type))))
  (doc 'description "Extends an existing context with dependent graph types.")
  (doc 'export #t)
  (append dep-graph-types ctx))
