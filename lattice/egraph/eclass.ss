;;; lattice/egraph/eclass.ss — E-Node and E-Class Representation
;;;
;;; E-nodes are operators applied to e-class IDs (not recursive terms).
;;; E-classes are sets of equivalent e-nodes, managed by union-find.
;;;
;;; Design notes:
;;;   - E-nodes are immutable: (op . children-vector)
;;;   - E-class data stored separately from union-find (parallel arrays)
;;;   - Canonical form: children are uf-find'd before comparison
;;;   - Parents tracking enables efficient rebuilding after merges
;;;
;;; This is Lattice code: pure operations where possible, mutation clearly marked.

(load "core/base/prelude.ss")
(load "lattice/egraph/union-find.ss")

(doc 'module 'egraph/eclass)
(doc 'description "E-node and e-class representation for e-graphs")
(doc 'layer 'lattice)
(doc 'tier 1)
(doc 'purity 'partial)  ; Mutation for efficiency

;;; ============================================================
;;; E-Node: Operator + Children (E-Class IDs)
;;; ============================================================

(doc 'section 'enode)

;;; E-nodes are the atomic terms in an e-graph.
;;; Each e-node is (op . children) where:
;;;   - op: symbol identifying the operation
;;;   - children: vector of e-class IDs (integers)
;;;
;;; E-nodes are structurally compared for hashconsing.
;;; Two e-nodes are equal if same op and same children (after canonicalization).

(define enode-tag 'enode)

(define (make-enode op children)
  (doc 'type (-> Symbol (Vector EClassId) ENode))
  (doc 'description "Create an e-node with operator and child e-class IDs.")
  (doc 'export #t)
  (cons enode-tag (cons op (if (vector? children) children (list->vector children)))))

(define (enode? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if x is an e-node.")
  (doc 'export #t)
  (and (pair? x) (eq? (car x) enode-tag)))

(define (enode-op enode)
  (doc 'type (-> ENode Symbol))
  (doc 'description "Get the operator of an e-node.")
  (doc 'export #t)
  (cadr enode))

(define (enode-children enode)
  (doc 'type (-> ENode (Vector EClassId)))
  (doc 'description "Get the children vector of an e-node.")
  (doc 'export #t)
  (cddr enode))

(define (enode-arity enode)
  (doc 'type (-> ENode Nat))
  (doc 'description "Number of children in the e-node.")
  (doc 'export #t)
  (vector-length (enode-children enode)))

;;; ============================================================
;;; E-Node Canonicalization
;;; ============================================================

(doc 'section 'canonical)

;;; To properly hashcons e-nodes, we need canonical forms where
;;; all children are resolved to their union-find roots.

(define (enode-canonicalize enode uf)
  (doc 'type (-> ENode UnionFind ENode))
  (doc 'description "Return e-node with children resolved to their canonical e-class IDs.")
  (doc 'export #t)
  (let* ([children (enode-children enode)]
         [n (vector-length children)]
         [canonical (make-vector n #f)])
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (vector-set! canonical i (uf-find uf (vector-ref children i))))
    (make-enode (enode-op enode) canonical)))

;;; enode-equal? : ENode × ENode → Boolean
;;; Structural equality (assumes already canonicalized or same UF context).
;;; Uses equal? for op comparison to handle all literal types (strings, etc.).
(define (enode-equal? e1 e2)
  (doc 'type (-> ENode ENode Boolean))
  (doc 'description "Check structural equality of two e-nodes.")
  (doc 'export #t)
  (and (equal? (enode-op e1) (enode-op e2))
       (let ([c1 (enode-children e1)]
             [c2 (enode-children e2)])
         (and (= (vector-length c1) (vector-length c2))
              (let loop ([i 0])
                (or (>= i (vector-length c1))
                    (and (= (vector-ref c1 i) (vector-ref c2 i))
                         (loop (+ i 1)))))))))

;;; enode-hash : ENode → Fixnum
;;; Hash function for e-nodes (for hashcons table).
;;; Bounded to avoid bignum promotion in hashtable lookups.
;;; Supports symbols, numbers, strings, booleans, and characters.
(define (enode-hash enode)
  (doc 'type (-> ENode Fixnum))
  (doc 'description "Compute hash of an e-node for hashconsing.")
  (doc 'export #t)
  (let* ([op (enode-op enode)]
         [children (enode-children enode)]
         [h (fxand (cond
                     [(symbol? op) (symbol-hash op)]
                     [(number? op) (abs (exact (truncate op)))]
                     [(string? op) (string-hash op)]
                     [(boolean? op) (if op 1 0)]
                     [(char? op) (char->integer op)]
                     [else (equal-hash op)])  ; Fallback for other types
                   #xFFFFFF)])
    (do ([i 0 (+ i 1)]
         [acc h (fxand (+ (* acc 31) (vector-ref children i)) #xFFFFFF)])
        ((>= i (vector-length children)) acc))))

;;; ============================================================
;;; E-Class: Set of Equivalent E-Nodes
;;; ============================================================

(doc 'section 'eclass)

;;; An e-class is identified by its union-find ID.
;;; Additional data per e-class:
;;;   - nodes: list of e-nodes in this class
;;;   - parents: set of (enode . eclass-id) pairs that reference this class
;;;
;;; The parents set enables efficient rebuilding: when classes merge,
;;; we need to update all e-nodes that referenced the old class IDs.

(define eclass-tag 'eclass)

(define (make-eclass-data)
  (doc 'type (-> EClassData))
  (doc 'description "Create empty e-class data (nodes list, parents set).")
  (vector eclass-tag
          '()                      ; nodes: list of e-nodes
          (make-eqv-hashtable)))   ; parents: hashtable of parent class IDs

(define (eclass-data? x)
  (and (vector? x)
       (>= (vector-length x) 3)
       (eq? (vector-ref x 0) eclass-tag)))

(define (eclass-nodes data) (vector-ref data 1))
(define (eclass-parents data) (vector-ref data 2))

(define (eclass-set-nodes! data nodes)
  (vector-set! data 1 nodes))

;;; ============================================================
;;; E-Class Store: Parallel Array to Union-Find
;;; ============================================================

(doc 'section 'store)

;;; The e-class store holds per-class data indexed by e-class ID.
;;; Grows in parallel with union-find.

(define eclass-store-tag 'eclass-store)

(define (make-eclass-store)
  (doc 'type (-> EClassStore))
  (doc 'description "Create an empty e-class store.")
  (doc 'export #t)
  (vector eclass-store-tag
          (make-vector 64 #f)))  ; data array (parallel to union-find)

(define (eclass-store? x)
  (doc 'type (-> Any Boolean))
  (doc 'export #t)
  (and (vector? x)
       (>= (vector-length x) 2)
       (eq? (vector-ref x 0) eclass-store-tag)))

(define (eclass-store-data store) (vector-ref store 1))

;;; ensure-eclass-capacity! : EClassStore × Id → Void
;;; Grow data array if needed.
(define (ensure-eclass-capacity! store id)
  (let ([data (eclass-store-data store)])
    (when (>= id (vector-length data))
      (let* ([old-len (vector-length data)]
             [new-len (max (* 2 old-len) (+ id 1))]
             [new-data (make-vector new-len #f)])
        (do ([i 0 (+ i 1)])
            ((>= i old-len))
          (vector-set! new-data i (vector-ref data i)))
        (vector-set! store 1 new-data)))))

;;; eclass-get : EClassStore × Id → EClassData | #f
;;; Get e-class data for an ID.
(define (eclass-get store id)
  (doc 'type (-> EClassStore EClassId (Or EClassData #f)))
  (doc 'description "Get e-class data for given ID, or #f if none.")
  (doc 'export #t)
  (let ([data (eclass-store-data store)])
    (if (< id (vector-length data))
        (vector-ref data id)
        #f)))

;;; eclass-get-or-create! : EClassStore × Id → EClassData
;;; Get or create e-class data.
(define (eclass-get-or-create! store id)
  (doc 'type (-> EClassStore EClassId EClassData))
  (doc 'description "Get e-class data, creating if needed.")
  (doc 'export #t)
  (ensure-eclass-capacity! store id)
  (let ([data (eclass-store-data store)])
    (or (vector-ref data id)
        (let ([new-data (make-eclass-data)])
          (vector-set! data id new-data)
          new-data))))

;;; ============================================================
;;; E-Class Operations
;;; ============================================================

(doc 'section 'operations)

;;; eclass-add-node! : EClassStore × Id × ENode → Void
;;; Add an e-node to an e-class.
(define (eclass-add-node! store class-id enode)
  (doc 'type (-> EClassStore EClassId ENode Void))
  (doc 'description "Add an e-node to the specified e-class.")
  (doc 'export #t)
  (let ([data (eclass-get-or-create! store class-id)])
    (eclass-set-nodes! data (cons enode (eclass-nodes data)))))

;;; eclass-add-parent! : EClassStore × Id × Id → Void
;;; Record that parent-class-id contains an e-node referencing child-class-id.
(define (eclass-add-parent! store child-class-id parent-class-id)
  (doc 'type (-> EClassStore EClassId EClassId Void))
  (doc 'description "Record that parent-class references child-class.")
  (doc 'export #t)
  (let ([data (eclass-get-or-create! store child-class-id)])
    (hashtable-set! (eclass-parents data) parent-class-id #t)))

;;; eclass-get-nodes : EClassStore × Id → (List ENode)
;;; Get all e-nodes in an e-class.
(define (eclass-get-nodes store class-id)
  (doc 'type (-> EClassStore EClassId (List ENode)))
  (doc 'description "Get all e-nodes in the specified e-class.")
  (doc 'export #t)
  (let ([data (eclass-get store class-id)])
    (if data (eclass-nodes data) '())))

;;; eclass-get-parents : EClassStore × Id → (List Id)
;;; Get all parent e-class IDs that reference this class.
(define (eclass-get-parents store class-id)
  (doc 'type (-> EClassStore EClassId (List EClassId)))
  (doc 'description "Get all e-class IDs whose e-nodes reference this class.")
  (doc 'export #t)
  (let ([data (eclass-get store class-id)])
    (if data
        (vector->list (hashtable-keys (eclass-parents data)))
        '())))

;;; ============================================================
;;; E-Class Merging
;;; ============================================================

(doc 'section 'merge)

;;; When two e-classes merge in union-find, we need to:
;;; 1. Combine their node lists
;;; 2. Combine their parent sets
;;; 3. Return the list of affected parent class IDs (for rebuilding)
;;;
;;; Note: Actual rebuilding is done by egraph.ss which has the hashcons.

;;; eclass-merge! : EClassStore × UnionFind × Id × Id → (List Id)
;;; Merge e-class data after union-find merge.
;;; Returns list of parent class IDs that need rebuilding.
;;; Note: Call AFTER uf-union! - we merge data from original IDs into the new root.
;;;
;;; This function:
;;; 1. Combines node lists from both classes into root
;;; 2. Merges parent hashtables into root's parent hashtable
;;; 3. Returns affected parents for rebuilding (O(1) deduplication via hashtable)
(define (eclass-merge! store uf id1 id2)
  (doc 'type (-> EClassStore UnionFind EClassId EClassId (List EClassId)))
  (doc 'description "Merge e-class data, returns parent IDs needing rebuild.")
  (doc 'export #t)
  ;; Get the canonical root (same for both after union)
  (let* ([new-root (uf-find uf id1)]
         ;; Get data from ORIGINAL locations
         [data1 (eclass-get store id1)]
         [data2 (eclass-get store id2)])
    (cond
      ;; If both are the same class and at root, nothing to do
      [(and (= id1 id2) (= id1 new-root))
       '()]
      ;; Merge data into root location
      [else
       (let* ([target-data (eclass-get-or-create! store new-root)]
              [target-parents (eclass-parents target-data)]
              ;; Use hashtable for O(1) deduplication of affected parents
              [affected-set (make-eqv-hashtable)])

         ;; Helper: merge parents from a data block into both target-parents and affected-set
         (define (merge-parents-from! data)
           (when data
             (for-each
              (lambda (p)
                (hashtable-set! target-parents p #t)
                (hashtable-set! affected-set p #t))
              (vector->list (hashtable-keys (eclass-parents data))))))

         ;; Merge nodes and parents from data1 if not at root
         (when (and data1 (not (= id1 new-root)))
           (eclass-set-nodes! target-data
                             (append (eclass-nodes data1) (eclass-nodes target-data)))
           (merge-parents-from! data1))

         ;; Merge nodes and parents from data2 if not at root and not same as data1
         (when (and data2 (not (= id2 new-root)) (not (= id2 id1)))
           (eclass-set-nodes! target-data
                             (append (eclass-nodes data2) (eclass-nodes target-data)))
           (merge-parents-from! data2))

         ;; Also include parents that were already at root (before merge started)
         ;; Note: target-parents is already the root's hashtable, so these are already there
         ;; Just need to add them to affected-set if not already present
         (for-each
          (lambda (p) (hashtable-set! affected-set p #t))
          (vector->list (hashtable-keys target-parents)))

         ;; Return affected parents as list
         (vector->list (hashtable-keys affected-set)))])))

;;; ============================================================
;;; Introspection
;;; ============================================================

(doc 'section 'introspection)

;;; eclass-node-count : EClassStore × UnionFind → Nat
;;; Total number of e-nodes across all e-classes.
(define (eclass-node-count store uf)
  (doc 'type (-> EClassStore UnionFind Nat))
  (doc 'description "Count total e-nodes in all e-classes.")
  (doc 'export #t)
  (let ([data-vec (eclass-store-data store)]
        [n (uf-size uf)]
        [count 0])
    ;; Only count nodes at canonical roots to avoid double-counting
    (do ([i 0 (+ i 1)])
        ((>= i n) count)
      (when (and (< i (vector-length data-vec))
                 (= i (uf-find uf i)))  ; Only count at roots
        (let ([data (vector-ref data-vec i)])
          (when data
            (set! count (+ count (length (eclass-nodes data))))))))))

;;; eclass-all-nodes : EClassStore × UnionFind → (List (Pair Id (List ENode)))
;;; Get all e-classes with their nodes. O(N) single pass.
(define (eclass-all-nodes store uf)
  (doc 'type (-> EClassStore UnionFind (List (Pair EClassId (List ENode)))))
  (doc 'description "Get all e-classes with their nodes as (id . nodes) pairs.")
  (doc 'export #t)
  (let ([data-vec (eclass-store-data store)]
        [n (uf-size uf)]
        [result '()])
    (do ([i 0 (+ i 1)])
        ((>= i n) (reverse result))
      (when (and (< i (vector-length data-vec))
                 (= i (uf-find uf i)))  ; Only roots
        (let ([data (vector-ref data-vec i)])
          (when (and data (pair? (eclass-nodes data)))
            (set! result (cons (cons i (eclass-nodes data)) result))))))))

;;; ============================================================
;;; Debugging
;;; ============================================================

(doc 'section 'debug)

;;; enode->string : ENode → String
;;; Convert e-node to readable string.
(define (op->string op)
  (cond
    [(symbol? op) (symbol->string op)]
    [(number? op) (number->string op)]
    [(string? op) (string-append "\"" op "\"")]
    [(boolean? op) (if op "#t" "#f")]
    [(char? op) (string-append "#\\" (string op))]
    [else (format "~s" op)]))

(define (enode->string enode)
  (doc 'type (-> ENode String))
  (doc 'description "Convert e-node to string representation.")
  (doc 'export #t)
  (let ([children (enode-children enode)])
    (if (zero? (vector-length children))
        (op->string (enode-op enode))
        (string-append "("
                       (op->string (enode-op enode))
                       " "
                       (let loop ([i 0] [acc ""])
                         (if (>= i (vector-length children))
                             acc
                             (loop (+ i 1)
                                   (string-append acc
                                                  (if (> i 0) " " "")
                                                  "e"
                                                  (number->string (vector-ref children i))))))
                       ")"))))

;;; eclass-debug : EClassStore × UnionFind × Id → Void
;;; Print e-class contents for debugging.
(define (eclass-debug store uf id)
  (doc 'type (-> EClassStore UnionFind EClassId Void))
  (doc 'description "Print e-class contents.")
  (doc 'export #t)
  (let* ([root (uf-find uf id)]
         [data (eclass-get store root)])
    (printf "E-Class ~a (root: ~a)~n" id root)
    (if data
        (begin
          (printf "  Nodes: ~a~n" (length (eclass-nodes data)))
          (for-each (lambda (n) (printf "    ~a~n" (enode->string n)))
                    (eclass-nodes data))
          (printf "  Parents: ~a~n" (hashtable-keys (eclass-parents data))))
        (printf "  (empty)~n"))))

