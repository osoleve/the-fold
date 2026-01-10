;;; core/types/dep-graph.ss — Dependent Types for Graph Properties
;;;
;;; This module provides dependent type constructors for encoding and
;;; verifying graph properties statically. It wraps the graph algorithms
;;; library with a type layer that enables verified graph operations.
;;;
;;; Graph Property Types:
;;;   - (Acyclic g)    — Proof that graph g has no cycles
;;;   - (Connected g)  — Proof that graph g is connected
;;;   - (Tree g)       — Proof that g is a tree (connected and acyclic)
;;;   - (DAG g)        — Proof that g is a directed acyclic graph
;;;   - (Path g v1 v2) — Proof of path from v1 to v2 in g
;;;
;;; These types enable:
;;;   1. Property-preserving operations (e.g., add-edge with cycle check)
;;;   2. Verified topological sort (requires DAG proof)
;;;   3. Path-dependent properties
;;;   4. Compile-time verification of graph constraints
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - dep-types.ss

(load "core/base/prelude.ss")
(load "core/types/dep-types.ss")

;;; ============================================================
;;; Graph Property Type Constructors
;;; ============================================================
;;;
;;; Each property type encodes a graph-theoretic property.
;;; Values of these types are proofs that the property holds.
;;;
;;; Type Formation Rules:
;;;   Γ ⊢ g : Graph
;;;   ─────────────────
;;;   Γ ⊢ (Acyclic g) : Type
;;;
;;;   Γ ⊢ g : Graph
;;;   ─────────────────
;;;   Γ ⊢ (Connected g) : Type
;;;
;;;   Γ ⊢ g : Graph
;;;   ──────────────────────────────────────────
;;;   Γ ⊢ (Tree g) : Type   ≡ (Σ ((_ : (Acyclic g))) (Connected g))
;;;
;;;   Γ ⊢ g : Graph
;;;   ─────────────────────────────────────────
;;;   Γ ⊢ (DAG g) : Type   ≡ (× (Directed g) (Acyclic g))
;;;
;;;   Γ ⊢ g : Graph   Γ ⊢ v1 : Node g   Γ ⊢ v2 : Node g
;;;   ──────────────────────────────────────────────────
;;;   Γ ⊢ (Path g v1 v2) : Type

;;; ============================================================
;;; Type Predicates
;;; ============================================================

;;; acyclic-type? : Type → Boolean
;;; Check if this is an acyclic graph proof type: (Acyclic g)
(define (acyclic-type? t)
  (and (pair? t) (eq? (car t) 'Acyclic)))

;;; connected-type? : Type → Boolean
;;; Check if this is a connected graph proof type: (Connected g)
(define (connected-type? t)
  (and (pair? t) (eq? (car t) 'Connected)))

;;; tree-type? : Type → Boolean
;;; Check if this is a tree proof type: (Tree g)
(define (tree-type? t)
  (and (pair? t) (eq? (car t) 'Tree)))

;;; dag-type? : Type → Boolean
;;; Check if this is a DAG proof type: (DAG g)
(define (dag-type? t)
  (and (pair? t) (eq? (car t) 'DAG)))

;;; path-type? : Type → Boolean
;;; Check if this is a path proof type: (Path g v1 v2)
(define (path-type? t)
  (and (pair? t) (eq? (car t) 'Path)))

;;; graph-property-type? : Type → Boolean
;;; Check if this is any graph property type.
(define (graph-property-type? t)
  (or (acyclic-type? t)
      (connected-type? t)
      (tree-type? t)
      (dag-type? t)
      (path-type? t)))

;;; ============================================================
;;; Well-Formedness Checks
;;; ============================================================

;;; acyclic-type-well-formed? : SExpr → Boolean
;;; Check if (Acyclic g) is well-formed.
(define (acyclic-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Acyclic)
       (= (length t) 2)))

;;; connected-type-well-formed? : SExpr → Boolean
;;; Check if (Connected g) is well-formed.
(define (connected-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Connected)
       (= (length t) 2)))

;;; tree-type-well-formed? : SExpr → Boolean
;;; Check if (Tree g) is well-formed.
(define (tree-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Tree)
       (= (length t) 2)))

;;; dag-type-well-formed? : SExpr → Boolean
;;; Check if (DAG g) is well-formed.
(define (dag-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'DAG)
       (= (length t) 2)))

;;; path-type-well-formed? : SExpr → Boolean
;;; Check if (Path g v1 v2) is well-formed.
(define (path-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Path)
       (= (length t) 4)))

;;; ============================================================
;;; Type Extractors
;;; ============================================================

;;; acyclic-graph : Type → Expr
;;; Get the graph from (Acyclic g).
(define (acyclic-graph t)
  (if (acyclic-type? t)
      (cadr t)
      #f))

;;; connected-graph : Type → Expr
;;; Get the graph from (Connected g).
(define (connected-graph t)
  (if (connected-type? t)
      (cadr t)
      #f))

;;; tree-graph : Type → Expr
;;; Get the graph from (Tree g).
(define (tree-graph t)
  (if (tree-type? t)
      (cadr t)
      #f))

;;; dag-graph : Type → Expr
;;; Get the graph from (DAG g).
(define (dag-graph t)
  (if (dag-type? t)
      (cadr t)
      #f))

;;; path-graph : Type → Expr
;;; Get the graph from (Path g v1 v2).
(define (path-graph t)
  (if (path-type? t)
      (cadr t)
      #f))

;;; path-source : Type → Expr
;;; Get the source vertex from (Path g v1 v2).
(define (path-source t)
  (if (path-type? t)
      (caddr t)
      #f))

;;; path-target : Type → Expr
;;; Get the target vertex from (Path g v1 v2).
(define (path-target t)
  (if (path-type? t)
      (cadddr t)
      #f))

;;; ============================================================
;;; Type Constructors
;;; ============================================================

;;; make-acyclic-type : Expr → Type
;;; Construct an acyclic graph type.
(define (make-acyclic-type g)
  `(Acyclic ,g))

;;; make-connected-type : Expr → Type
;;; Construct a connected graph type.
(define (make-connected-type g)
  `(Connected ,g))

;;; make-tree-type : Expr → Type
;;; Construct a tree type.
(define (make-tree-type g)
  `(Tree ,g))

;;; make-dag-type : Expr → Type
;;; Construct a DAG type.
(define (make-dag-type g)
  `(DAG ,g))

;;; make-path-type : Expr × Expr × Expr → Type
;;; Construct a path type (Path g v1 v2).
(define (make-path-type g v1 v2)
  `(Path ,g ,v1 ,v2))

;;; ============================================================
;;; Type Relationships
;;; ============================================================
;;;
;;; These functions encode the logical relationships between
;;; graph property types.

;;; tree-implies-acyclic : (Tree g) → (Acyclic g)
;;; A tree is always acyclic.
(define (tree-implies-acyclic tree-type)
  (if (tree-type? tree-type)
      (make-acyclic-type (tree-graph tree-type))
      #f))

;;; tree-implies-connected : (Tree g) → (Connected g)
;;; A tree is always connected.
(define (tree-implies-connected tree-type)
  (if (tree-type? tree-type)
      (make-connected-type (tree-graph tree-type))
      #f))

;;; dag-implies-acyclic : (DAG g) → (Acyclic g)
;;; A DAG is always acyclic.
(define (dag-implies-acyclic dag-type)
  (if (dag-type? dag-type)
      (make-acyclic-type (dag-graph dag-type))
      #f))

;;; acyclic-connected-implies-tree : (Acyclic g) × (Connected g) → (Tree g)
;;; An acyclic connected graph is a tree.
;;; Note: Requires both proofs to be for the same graph.
(define (acyclic-connected-implies-tree acyclic-proof connected-proof)
  (if (and (acyclic-type? acyclic-proof)
           (connected-type? connected-proof)
           (equal? (acyclic-graph acyclic-proof) (connected-graph connected-proof)))
      (make-tree-type (acyclic-graph acyclic-proof))
      #f))

;;; path-reflexive : Graph × Node → (Path g v v)
;;; Every node has a path to itself (empty path).
(define (path-reflexive g v)
  (make-path-type g v v))

;;; path-transitive : (Path g v1 v2) × (Path g v2 v3) → (Path g v1 v3)
;;; Paths are transitive (concatenation).
(define (path-transitive path1 path2)
  (if (and (path-type? path1)
           (path-type? path2)
           (equal? (path-graph path1) (path-graph path2))
           (equal? (path-target path1) (path-source path2)))
      (make-path-type (path-graph path1)
                      (path-source path1)
                      (path-target path2))
      #f))

;;; ============================================================
;;; Type Signatures for Graph Operations
;;; ============================================================
;;;
;;; These type signatures are for use with the dependent type
;;; checking system (dep-infer.ss).

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

;;; ============================================================
;;; Runtime Verification Functions
;;; ============================================================
;;;
;;; These functions perform runtime verification and return
;;; proof terms (or proof of failure).

;;; verify-acyclic : Graph → Either (Acyclic g) (CycleProof g)
;;; Verify that a graph is acyclic.
;;; Returns a proof of acyclicity or a cycle witness.
(define (verify-acyclic g)
  ;; This would call find-cycles from graph-algorithms.ss
  ;; For now, we return a tagged result
  `(verify-acyclic-result ,g))

;;; verify-connected : Graph → Either (Connected g) (DisconnectedProof g)
;;; Verify that a graph is connected.
(define (verify-connected g)
  ;; This would call connected-components from graph-algorithms.ss
  `(verify-connected-result ,g))

;;; verify-dag : Graph → Either (DAG g) (CycleProof g)
;;; Verify that a graph is a DAG.
(define (verify-dag g)
  ;; This would call topological-sort from graph-algorithms.ss
  `(verify-dag-result ,g))

;;; find-path : Graph × Node × Node → Either (Path g v1 v2) (NoPath g v1 v2)
;;; Find a path between two nodes.
(define (find-path g v1 v2)
  ;; This would call path-exists? from graph-algorithms.ss
  `(find-path-result ,g ,v1 ,v2))

;;; ============================================================
;;; Pretty Printing
;;; ============================================================

;;; graph-property-type->string : Type → String
;;; Convert a graph property type to a readable string.
(define (graph-property-type->string t)
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

;;; ============================================================
;;; Type Context Integration
;;; ============================================================

;;; make-dep-graph-ctx : Unit → (List (Pair Symbol Type))
;;; Creates a type context with all dependent graph operations.
(define (make-dep-graph-ctx)
  dep-graph-types)

;;; extend-ctx-with-dep-graph : (List (Pair Symbol Type)) → (List (Pair Symbol Type))
;;; Extends an existing context with dependent graph types.
(define (extend-ctx-with-dep-graph ctx)
  (append dep-graph-types ctx))
