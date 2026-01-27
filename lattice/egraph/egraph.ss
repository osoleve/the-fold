;;; lattice/egraph/egraph.ss — E-Graph with Hashconsing and Rebuilding
;;;
;;; The e-graph is the central data structure for equality saturation.
;;; It combines:
;;;   - Union-find for e-class equivalence
;;;   - E-class store for per-class data (nodes, parents)
;;;   - Hashcons table for e-node deduplication
;;;
;;; Key operations:
;;;   - egraph-add: Add a term, return its e-class ID
;;;   - egraph-merge: Assert two e-classes are equivalent
;;;   - egraph-rebuild: Re-canonicalize after merges (worklist algorithm)
;;;   - egraph-lookup: Find e-class for a term (if exists)
;;;
;;; Design notes:
;;;   - Hashcons is local to e-graph (transient, not global CAS)
;;;   - Rebuilding uses worklist to process affected parents
;;;   - All e-nodes in hashcons are canonical (children are roots)
;;;
;;; This is Lattice code: pure operations where possible, mutation clearly marked.

(load "core/base/prelude.ss")
(load "lattice/egraph/union-find.ss")
(load "lattice/egraph/eclass.ss")

(doc 'module 'egraph/egraph)
(doc 'description "E-graph with hashconsing and rebuilding for equality saturation")
(doc 'layer 'lattice)
(doc 'tier 1)
(doc 'purity 'partial)  ; Mutation for efficiency

;;; ============================================================
;;; E-Graph Structure
;;; ============================================================

(doc 'section 'structure)

;;; An e-graph consists of:
;;;   - uf: Union-find for e-class equivalence
;;;   - classes: E-class store (nodes + parents per class)
;;;   - hashcons: Hashtable mapping canonical e-nodes to e-class IDs
;;;   - dirty: List of e-class IDs needing rebuild (worklist)
;;;   - stats: Performance counters

(define egraph-tag 'egraph)

(define (make-egraph)
  (doc 'type (-> EGraph))
  (doc 'description "Create an empty e-graph.")
  (doc 'export #t)
  (vector egraph-tag
          (make-uf)              ; union-find
          (make-eclass-store)    ; e-class data
          (make-hashtable        ; hashcons: enode → class-id
           enode-hash
           enode-equal?)
          (make-eqv-hashtable)   ; dirty set: class-id → #t (O(1) membership)
          (vector 0 0 0 0)))     ; stats: [adds, merges, rebuilds, hashcons-hits]

(define (egraph? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if x is an e-graph.")
  (doc 'export #t)
  (and (vector? x)
       (>= (vector-length x) 6)
       (eq? (vector-ref x 0) egraph-tag)))

(define (egraph-uf eg) (vector-ref eg 1))
(define (egraph-classes eg) (vector-ref eg 2))
(define (egraph-hashcons eg) (vector-ref eg 3))
(define (egraph-dirty-set eg) (vector-ref eg 4))
(define (egraph-stats eg) (vector-ref eg 5))

;;; Stats accessors
(define (egraph-stat-adds eg) (vector-ref (egraph-stats eg) 0))
(define (egraph-stat-merges eg) (vector-ref (egraph-stats eg) 1))
(define (egraph-stat-rebuilds eg) (vector-ref (egraph-stats eg) 2))
(define (egraph-stat-hits eg) (vector-ref (egraph-stats eg) 3))

(define (egraph-inc-stat! eg idx)
  (let ([stats (egraph-stats eg)])
    (vector-set! stats idx (+ (vector-ref stats idx) 1))))

;;; ============================================================
;;; Dirty Worklist Management
;;; ============================================================

(doc 'section 'worklist)

;;; The dirty worklist tracks e-classes that need rebuilding.
;;; After a merge, affected parent classes are added to the worklist.
;;; Uses hashtable for O(1) membership check (avoids O(N²) marking).

(define (egraph-dirty eg)
  (vector->list (hashtable-keys (egraph-dirty-set eg))))

(define (egraph-mark-dirty! eg class-id)
  (hashtable-set! (egraph-dirty-set eg) class-id #t))

(define (egraph-clear-dirty! eg)
  (hashtable-clear! (egraph-dirty-set eg)))

(define (egraph-pop-dirty! eg)
  (let ([ht (egraph-dirty-set eg)])
    (if (zero? (hashtable-size ht))
        #f
        (let ([keys (hashtable-keys ht)])
          (let ([id (vector-ref keys 0)])
            (hashtable-delete! ht id)
            id)))))

;;; ============================================================
;;; Core Operations
;;; ============================================================

(doc 'section 'operations)

;;; egraph-find : EGraph × EClassId → EClassId
;;; Find the canonical (root) e-class ID.
(define (egraph-find eg id)
  (doc 'type (-> EGraph EClassId EClassId))
  (doc 'description "Find canonical e-class ID (union-find root).")
  (doc 'export #t)
  (uf-find (egraph-uf eg) id))

;;; egraph-lookup : EGraph × ENode → EClassId | #f
;;; Look up an e-node in the hashcons table.
;;; Returns the e-class ID if found, #f otherwise.
(define (egraph-lookup eg enode)
  (doc 'type (-> EGraph ENode (Or EClassId #f)))
  (doc 'description "Look up e-node in hashcons, return e-class ID or #f.")
  (doc 'export #t)
  (let* ([uf (egraph-uf eg)]
         [canonical (enode-canonicalize enode uf)]
         [hashcons (egraph-hashcons eg)])
    (hashtable-ref hashcons canonical #f)))

;;; egraph-add-enode! : EGraph × ENode → EClassId
;;; Add an e-node to the e-graph.
;;; If already present (via hashcons), return existing e-class.
;;; Otherwise, create new e-class and add to hashcons.
(define (egraph-add-enode! eg enode)
  (doc 'type (-> EGraph ENode EClassId))
  (doc 'description "Add e-node, return its e-class ID (new or existing).")
  (doc 'export #t)
  (let* ([uf (egraph-uf eg)]
         [canonical (enode-canonicalize enode uf)]
         [hashcons (egraph-hashcons eg)]
         [existing (hashtable-ref hashcons canonical #f)])
    (if existing
        (begin
          (egraph-inc-stat! eg 3)  ; hashcons hit
          existing)
        (let* ([classes (egraph-classes eg)]
               [new-id (uf-make-set! uf)])
          (egraph-inc-stat! eg 0)  ; add
          ;; Add to hashcons
          (hashtable-set! hashcons canonical new-id)
          ;; Add to e-class store
          (eclass-add-node! classes new-id canonical)
          ;; Register as parent of children
          (let ([children (enode-children canonical)])
            (do ([i 0 (+ i 1)])
                ((>= i (vector-length children)))
              (let ([child-id (egraph-find eg (vector-ref children i))])
                (eclass-add-parent! classes child-id new-id))))
          new-id))))

;;; egraph-merge! : EGraph × EClassId × EClassId → EClassId
;;; Merge two e-classes, asserting they are equivalent.
;;; Returns the canonical ID of the merged class.
;;; Marks affected parents as dirty for rebuilding.
(define (egraph-merge! eg id1 id2)
  (doc 'type (-> EGraph EClassId EClassId EClassId))
  (doc 'description "Merge two e-classes. Returns merged class ID.")
  (doc 'export #t)
  (let ([uf (egraph-uf eg)]
        [classes (egraph-classes eg)])
    (let ([root1 (uf-find uf id1)]
          [root2 (uf-find uf id2)])
      (if (= root1 root2)
          root1  ; Already equivalent
          (begin
            (egraph-inc-stat! eg 1)  ; merge
            ;; Merge in union-find
            (let ([new-root (uf-union! uf id1 id2)])
              ;; Merge e-class data, get affected parents
              (let ([affected (eclass-merge! classes uf id1 id2)])
                ;; Mark affected parents as dirty
                (for-each (lambda (p) (egraph-mark-dirty! eg p))
                          affected))
              new-root))))))

;;; ============================================================
;;; Rebuilding
;;; ============================================================

(doc 'section 'rebuild)

;;; After merges, the hashcons table may have stale entries.
;;; E-nodes that referenced merged classes need re-canonicalization.
;;;
;;; Algorithm:
;;; 1. Pop a dirty class from worklist
;;; 2. For each e-node in that class:
;;;    a. Canonicalize it (children → their roots)
;;;    b. If canonical form already in hashcons → merge with that class
;;;    c. Otherwise, re-insert into hashcons
;;; 3. Repeat until worklist empty

;;; egraph-rebuild! : EGraph → Nat
;;; Process all dirty e-classes, re-canonicalizing their e-nodes.
;;; Returns number of e-classes processed.
(define (egraph-rebuild! eg)
  (doc 'type (-> EGraph Nat))
  (doc 'description "Rebuild e-graph after merges. Returns classes processed.")
  (doc 'export #t)
  (let ([uf (egraph-uf eg)]
        [classes (egraph-classes eg)]
        [hashcons (egraph-hashcons eg)]
        [count 0])
    (let loop ()
      (let ([dirty-id (egraph-pop-dirty! eg)])
        (if (not dirty-id)
            count
            (let ([root (uf-find uf dirty-id)])
              ;; Only process if this is the canonical representative
              (when (= dirty-id root)
                (egraph-inc-stat! eg 2)  ; rebuild
                (set! count (+ count 1))
                ;; Get all e-nodes in this class
                (let ([nodes (eclass-get-nodes classes root)])
                  (for-each
                   (lambda (enode)
                     (let ([canonical (enode-canonicalize enode uf)])
                       ;; Remove old entry if different
                       (unless (enode-equal? enode canonical)
                         (hashtable-delete! hashcons enode))
                       ;; Check if canonical already exists
                       (let ([existing (hashtable-ref hashcons canonical #f)])
                         (cond
                           [(not existing)
                            ;; Not in hashcons, add it
                            (hashtable-set! hashcons canonical root)]
                           [(not (= existing root))
                            ;; Exists in different class, need to merge
                            ;; Update hashcons to point to new root
                            (let ([new-root (egraph-merge! eg root existing)])
                              (hashtable-set! hashcons canonical new-root))]
                           ;; else: already points to this class, no action
                           ))))
                   nodes)))
              (loop)))))))

;;; egraph-saturate-rebuild! : EGraph → Nat
;;; Keep rebuilding until no more dirty classes.
;;; Returns total classes processed.
(define (egraph-saturate-rebuild! eg)
  (doc 'type (-> EGraph Nat))
  (doc 'description "Rebuild until fixpoint. Returns total classes processed.")
  (doc 'export #t)
  (let loop ([total 0])
    (let ([processed (egraph-rebuild! eg)])
      (if (zero? processed)
          total
          (loop (+ total processed))))))

;;; ============================================================
;;; Convenience: Add Terms (S-expressions)
;;; ============================================================

(doc 'section 'terms)

;;; Convert S-expression terms to e-nodes and add to e-graph.
;;; Leaves (symbols/numbers) become nullary e-nodes.
;;; Applications (op arg ...) become e-nodes with children.

;;; egraph-add-term! : EGraph × Term → EClassId
;;; Recursively add a term (S-expression) to the e-graph.
(define (egraph-add-term! eg term)
  (doc 'type (-> EGraph Term EClassId))
  (doc 'description "Add S-expression term to e-graph, return its e-class ID.")
  (doc 'export #t)
  (cond
    [(pair? term)
     ;; Application: (op args ...)
     (let* ([op (car term)]
            [args (cdr term)]
            ;; Recursively add children, get their class IDs
            [child-ids (map (lambda (arg) (egraph-add-term! eg arg)) args)]
            [enode (make-enode op (list->vector child-ids))])
       (egraph-add-enode! eg enode))]
    [else
     ;; Leaf: symbol, number, string, boolean, char, or other literal
     ;; Use the literal itself as the e-node operator with no children
     (egraph-add-enode! eg (make-enode term (vector)))]))

;;; ============================================================
;;; Introspection
;;; ============================================================

(doc 'section 'introspection)

;;; egraph-class-count : EGraph → Nat
;;; Number of distinct e-classes.
(define (egraph-class-count eg)
  (doc 'type (-> EGraph Nat))
  (doc 'description "Number of distinct e-classes in the e-graph.")
  (doc 'export #t)
  (uf-count (egraph-uf eg)))

;;; egraph-node-count : EGraph → Nat
;;; Total number of e-nodes.
(define (egraph-node-count eg)
  (doc 'type (-> EGraph Nat))
  (doc 'description "Total number of e-nodes in the e-graph.")
  (doc 'export #t)
  (eclass-node-count (egraph-classes eg) (egraph-uf eg)))

;;; egraph-size : EGraph → Nat
;;; Total elements in union-find (may include non-root classes).
(define (egraph-size eg)
  (doc 'type (-> EGraph Nat))
  (doc 'description "Total elements created in the e-graph.")
  (doc 'export #t)
  (uf-size (egraph-uf eg)))

;;; egraph-stats-report : EGraph → Alist
;;; Get statistics as association list.
(define (egraph-stats-report eg)
  (doc 'type (-> EGraph Alist))
  (doc 'description "Get e-graph statistics as alist.")
  (doc 'export #t)
  `((classes . ,(egraph-class-count eg))
    (nodes . ,(egraph-node-count eg))
    (adds . ,(egraph-stat-adds eg))
    (merges . ,(egraph-stat-merges eg))
    (rebuilds . ,(egraph-stat-rebuilds eg))
    (hashcons-hits . ,(egraph-stat-hits eg))
    (dirty . ,(length (egraph-dirty eg)))))

;;; egraph-class-nodes : EGraph × EClassId → (List ENode)
;;; Get all e-nodes in an e-class.
(define (egraph-class-nodes eg id)
  (doc 'type (-> EGraph EClassId (List ENode)))
  (doc 'description "Get all e-nodes in an e-class.")
  (doc 'export #t)
  (let ([root (egraph-find eg id)])
    (eclass-get-nodes (egraph-classes eg) root)))

;;; ============================================================
;;; Debugging
;;; ============================================================

(doc 'section 'debug)

;;; egraph-debug : EGraph → Void
;;; Print e-graph state for debugging.
(define (egraph-debug eg)
  (doc 'type (-> EGraph Void))
  (doc 'description "Print e-graph state for debugging.")
  (doc 'export #t)
  (let ([stats (egraph-stats-report eg)])
    (printf "E-Graph:~n")
    (printf "  Classes: ~a~n" (cdr (assq 'classes stats)))
    (printf "  Nodes: ~a~n" (cdr (assq 'nodes stats)))
    (printf "  Adds: ~a, Merges: ~a, Rebuilds: ~a~n"
            (cdr (assq 'adds stats))
            (cdr (assq 'merges stats))
            (cdr (assq 'rebuilds stats)))
    (printf "  Hashcons hits: ~a~n" (cdr (assq 'hashcons-hits stats)))
    (printf "  Dirty: ~a~n" (cdr (assq 'dirty stats)))
    ;; Print each class
    (printf "~nClasses:~n")
    (let* ([uf (egraph-uf eg)]
           [classes (egraph-classes eg)]
           [all (eclass-all-nodes classes uf)])
      (for-each
       (lambda (entry)
         (let ([id (car entry)]
               [nodes (cdr entry)])
           (printf "  e~a: ~a~n" id
                   (map enode->string nodes))))
       all))))

