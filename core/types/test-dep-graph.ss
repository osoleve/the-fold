;;; core/types/test-dep-graph.ss — Tests for Dependent Graph Types
;;;
;;; Tests for graph property type constructors and operations.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/dep-graph.ss")

;;; ====
;;; Test Framework
;;; ====

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "  ✗ ")
       (display name)
       (newline)
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (run-tests)
  (display "
=== Dependent Graph Types Test Suite ===

")
  
  (display "--- Type Predicates ---
")
  
  ;; Acyclic type tests
  (test-true "acyclic-type? on (Acyclic g)"
             (acyclic-type? '(Acyclic my-graph)))
  (test-false "acyclic-type? on (Connected g)"
              (acyclic-type? '(Connected my-graph)))
  (test-false "acyclic-type? on symbol"
              (acyclic-type? 'Graph))
  
  ;; Connected type tests
  (test-true "connected-type? on (Connected g)"
             (connected-type? '(Connected my-graph)))
  (test-false "connected-type? on (Acyclic g)"
              (connected-type? '(Acyclic my-graph)))
  
  ;; Tree type tests
  (test-true "tree-type? on (Tree g)"
             (tree-type? '(Tree my-graph)))
  (test-false "tree-type? on (DAG g)"
              (tree-type? '(DAG my-graph)))
  
  ;; DAG type tests
  (test-true "dag-type? on (DAG g)"
             (dag-type? '(DAG my-graph)))
  (test-false "dag-type? on (Tree g)"
              (dag-type? '(Tree my-graph)))
  
  ;; Path type tests
  (test-true "path-type? on (Path g v1 v2)"
             (path-type? '(Path my-graph a b)))
  (test-false "path-type? on (Acyclic g)"
              (path-type? '(Acyclic my-graph)))
  
  ;; graph-property-type? tests
  (test-true "graph-property-type? on Acyclic"
             (graph-property-type? '(Acyclic g)))
  (test-true "graph-property-type? on Connected"
             (graph-property-type? '(Connected g)))
  (test-true "graph-property-type? on Tree"
             (graph-property-type? '(Tree g)))
  (test-true "graph-property-type? on DAG"
             (graph-property-type? '(DAG g)))
  (test-true "graph-property-type? on Path"
             (graph-property-type? '(Path g a b)))
  (test-false "graph-property-type? on arrow type"
              (graph-property-type? '(-> Graph Bool)))
  
  (display "
--- Well-Formedness Checks ---
")
  
  ;; Acyclic well-formedness
  (test-true "acyclic-type-well-formed? valid"
             (acyclic-type-well-formed? '(Acyclic g)))
  (test-false "acyclic-type-well-formed? wrong arity"
              (acyclic-type-well-formed? '(Acyclic)))
  (test-false "acyclic-type-well-formed? extra arg"
              (acyclic-type-well-formed? '(Acyclic g h)))
  
  ;; Connected well-formedness
  (test-true "connected-type-well-formed? valid"
             (connected-type-well-formed? '(Connected g)))
  (test-false "connected-type-well-formed? wrong arity"
              (connected-type-well-formed? '(Connected)))
  
  ;; Tree well-formedness
  (test-true "tree-type-well-formed? valid"
             (tree-type-well-formed? '(Tree g)))
  (test-false "tree-type-well-formed? wrong arity"
              (tree-type-well-formed? '(Tree g h)))
  
  ;; DAG well-formedness
  (test-true "dag-type-well-formed? valid"
             (dag-type-well-formed? '(DAG g)))
  (test-false "dag-type-well-formed? wrong arity"
              (dag-type-well-formed? '(DAG)))
  
  ;; Path well-formedness
  (test-true "path-type-well-formed? valid"
             (path-type-well-formed? '(Path g v1 v2)))
  (test-false "path-type-well-formed? missing target"
              (path-type-well-formed? '(Path g v1)))
  (test-false "path-type-well-formed? extra arg"
              (path-type-well-formed? '(Path g v1 v2 v3)))
  
  (display "
--- Type Extractors ---
")
  
  ;; Acyclic extractors
  (test "acyclic-graph extracts graph"
        'my-graph
        (acyclic-graph '(Acyclic my-graph)))
  (test "acyclic-graph on non-Acyclic"
        #f
        (acyclic-graph '(Connected g)))
  
  ;; Connected extractors
  (test "connected-graph extracts graph"
        'my-graph
        (connected-graph '(Connected my-graph)))
  (test "connected-graph on non-Connected"
        #f
        (connected-graph '(Acyclic g)))
  
  ;; Tree extractors
  (test "tree-graph extracts graph"
        'my-tree
        (tree-graph '(Tree my-tree)))
  
  ;; DAG extractors
  (test "dag-graph extracts graph"
        'my-dag
        (dag-graph '(DAG my-dag)))
  
  ;; Path extractors
  (test "path-graph extracts graph"
        'g
        (path-graph '(Path g start end)))
  (test "path-source extracts source"
        'start
        (path-source '(Path g start end)))
  (test "path-target extracts target"
        'end
        (path-target '(Path g start end)))
  (test "path-source on non-Path"
        #f
        (path-source '(Acyclic g)))
  
  (display "
--- Type Constructors ---
")
  
  ;; make-acyclic-type
  (test "make-acyclic-type constructs correctly"
        '(Acyclic my-graph)
        (make-acyclic-type 'my-graph))
  (test "make-acyclic-type passes well-formedness"
        #t
        (acyclic-type-well-formed? (make-acyclic-type 'g)))
  
  ;; make-connected-type
  (test "make-connected-type constructs correctly"
        '(Connected my-graph)
        (make-connected-type 'my-graph))
  
  ;; make-tree-type
  (test "make-tree-type constructs correctly"
        '(Tree my-tree)
        (make-tree-type 'my-tree))
  
  ;; make-dag-type
  (test "make-dag-type constructs correctly"
        '(DAG my-dag)
        (make-dag-type 'my-dag))
  
  ;; make-path-type
  (test "make-path-type constructs correctly"
        '(Path g a b)
        (make-path-type 'g 'a 'b))
  (test "make-path-type passes well-formedness"
        #t
        (path-type-well-formed? (make-path-type 'g 'v1 'v2)))
  
  (display "
--- Type Relationships ---
")
  
  ;; tree-implies-acyclic
  (test "tree-implies-acyclic extracts Acyclic"
        '(Acyclic my-tree)
        (tree-implies-acyclic '(Tree my-tree)))
  (test "tree-implies-acyclic on non-Tree"
        #f
        (tree-implies-acyclic '(DAG my-dag)))
  
  ;; tree-implies-connected
  (test "tree-implies-connected extracts Connected"
        '(Connected my-tree)
        (tree-implies-connected '(Tree my-tree)))
  (test "tree-implies-connected on non-Tree"
        #f
        (tree-implies-connected '(Acyclic g)))
  
  ;; dag-implies-acyclic
  (test "dag-implies-acyclic extracts Acyclic"
        '(Acyclic my-dag)
        (dag-implies-acyclic '(DAG my-dag)))
  (test "dag-implies-acyclic on non-DAG"
        #f
        (dag-implies-acyclic '(Tree t)))
  
  ;; acyclic-connected-implies-tree
  (test "acyclic-connected-implies-tree with same graph"
        '(Tree g)
        (acyclic-connected-implies-tree '(Acyclic g) '(Connected g)))
  (test "acyclic-connected-implies-tree with different graphs"
        #f
        (acyclic-connected-implies-tree '(Acyclic g1) '(Connected g2)))
  (test "acyclic-connected-implies-tree with wrong types"
        #f
        (acyclic-connected-implies-tree '(Tree t) '(Connected g)))
  
  ;; path-reflexive
  (test "path-reflexive creates self-path"
        '(Path g v v)
        (path-reflexive 'g 'v))
  
  ;; path-transitive
  (test "path-transitive concatenates paths"
        '(Path g a c)
        (path-transitive '(Path g a b) '(Path g b c)))
  (test "path-transitive with mismatched endpoints"
        #f
        (path-transitive '(Path g a b) '(Path g c d)))
  (test "path-transitive with different graphs"
        #f
        (path-transitive '(Path g1 a b) '(Path g2 b c)))
  
  (display "
--- Type Signatures ---
")
  
  ;; dep-graph-types structure
  (test-true "dep-graph-types is a valid alist"
             (and (list? dep-graph-types)
                  (andmap (lambda (entry)
                                  (and (pair? entry)
                                       (symbol? (car entry))))
                          dep-graph-types)))
  
  (test-true "dep-graph-types has expected entries"
             (>= (length dep-graph-types) 10))
  
  ;; Check specific type signatures exist
  (test-true "check-acyclic type exists"
             (if (assq 'check-acyclic dep-graph-types) #t #f))
  (test-true "check-connected type exists"
             (if (assq 'check-connected dep-graph-types) #t #f))
  (test-true "topological-sort-dag type exists"
             (if (assq 'topological-sort-dag dep-graph-types) #t #f))
  (test-true "concat-paths type exists"
             (if (assq 'concat-paths dep-graph-types) #t #f))
  
  (display "
--- Pretty Printing ---
")
  
  (test "graph-property-type->string Acyclic"
        "Acyclic(my-graph)"
        (graph-property-type->string '(Acyclic my-graph)))
  (test "graph-property-type->string Connected"
        "Connected(my-graph)"
        (graph-property-type->string '(Connected my-graph)))
  (test "graph-property-type->string Tree"
        "Tree(my-tree)"
        (graph-property-type->string '(Tree my-tree)))
  (test "graph-property-type->string DAG"
        "DAG(my-dag)"
        (graph-property-type->string '(DAG my-dag)))
  (test "graph-property-type->string Path"
        "Path(g, a → b)"
        (graph-property-type->string '(Path g a b)))
  
  (display "
--- Context Integration ---
")
  
  (test-true "make-dep-graph-ctx returns valid context"
             (list? (make-dep-graph-ctx)))
  (test-true "extend-ctx-with-dep-graph extends context"
             (let ([extended (extend-ctx-with-dep-graph '((foo . Int)))])
                  (and (if (assq 'check-acyclic extended) #t #f)
                       (if (assq 'foo extended) #t #f))))
  
  ;; Summary
  (display "
=== Test Summary ===
")
  (display "Total: ")
  (display *test-count*)
  (display ", Passed: ")
  (display *pass-count*)
  (display ", Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!
")
      (display "Some tests failed.
"))
  
  (= *fail-count* 0))

;; Run tests
(run-tests)
