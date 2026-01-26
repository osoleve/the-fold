;;; lattice/egraph/union-find.ss — Disjoint Set (Union-Find) Data Structure
;;;
;;; Classic union-find with path compression and union by rank.
;;; Foundation for e-graph e-class management.
;;;
;;; Complexity:
;;;   make-set!  O(1)
;;;   find       O(α(n)) amortized (inverse Ackermann, effectively constant)
;;;   union!     O(α(n)) amortized
;;;   same-set?  O(α(n)) amortized
;;;
;;; Design notes:
;;;   - Uses vectors for parent/rank storage (O(1) access)
;;;   - Elements are integers 0..n-1 (e-class IDs)
;;;   - Path compression in find (flatten tree on lookup)
;;;   - Union by rank (attach smaller tree under larger)
;;;
;;; This is Lattice code: pure operations where possible, mutation clearly marked.

(load "core/base/prelude.ss")

(doc 'module 'egraph/union-find)
(doc 'description "Disjoint set data structure for e-class equivalence tracking")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'partial)  ; Mutation for efficiency

;;; ============================================================
;;; Data Structure
;;; ============================================================

(doc 'section 'structure)

;;; Union-Find structure:
;;;   - parent: vector where parent[i] is parent of element i (or i if root)
;;;   - rank: vector where rank[i] is upper bound on tree height at i
;;;   - count: number of distinct sets
;;;   - next-id: next element ID to allocate

(define uf-tag 'union-find)

(define (make-uf)
  (doc 'type (-> UnionFind))
  (doc 'description "Create an empty union-find structure.")
  (doc 'export #t)
  (vector uf-tag
          (make-vector 64 #f)   ; parent (growable)
          (make-vector 64 0)    ; rank
          (box 0)               ; count of distinct sets
          (box 0)))             ; next-id

(define (uf? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if x is a union-find structure.")
  (doc 'export #t)
  (and (vector? x)
       (>= (vector-length x) 5)
       (eq? (vector-ref x 0) uf-tag)))

(define (uf-parent uf) (vector-ref uf 1))
(define (uf-rank uf) (vector-ref uf 2))
(define (uf-count-box uf) (vector-ref uf 3))
(define (uf-next-id-box uf) (vector-ref uf 4))

(define (uf-count uf)
  (doc 'type (-> UnionFind Nat))
  (doc 'description "Number of distinct sets in the union-find.")
  (doc 'export #t)
  (unbox (uf-count-box uf)))

(define (uf-size uf)
  (doc 'type (-> UnionFind Nat))
  (doc 'description "Total number of elements in the union-find.")
  (doc 'export #t)
  (unbox (uf-next-id-box uf)))

;;; ============================================================
;;; Internal: Vector Growth
;;; ============================================================

(doc 'section 'internal)

;;; ensure-capacity! : UnionFind × Nat → Void
;;; Grow vectors if needed to accommodate element id.
(define (ensure-capacity! uf id)
  (let ([parent (uf-parent uf)]
        [rank (uf-rank uf)])
    (when (>= id (vector-length parent))
      (let* ([old-len (vector-length parent)]
             [new-len (max (* 2 old-len) (+ id 1))]
             [new-parent (make-vector new-len #f)]
             [new-rank (make-vector new-len 0)])
        ;; Copy old data
        (do ([i 0 (+ i 1)])
            ((>= i old-len))
          (vector-set! new-parent i (vector-ref parent i))
          (vector-set! new-rank i (vector-ref rank i)))
        ;; Replace vectors in structure
        (vector-set! uf 1 new-parent)
        (vector-set! uf 2 new-rank)))))

;;; ============================================================
;;; Core Operations
;;; ============================================================

(doc 'section 'operations)

;;; uf-make-set! : UnionFind → Id
;;; Create a new singleton set, return its ID.
(define (uf-make-set! uf)
  (doc 'type (-> UnionFind Id))
  (doc 'description "Add a new element as its own singleton set. Returns the element's ID.")
  (doc 'export #t)
  (let* ([id-box (uf-next-id-box uf)]
         [id (unbox id-box)])
    (ensure-capacity! uf id)
    (vector-set! (uf-parent uf) id id)  ; Parent is self (root)
    (vector-set! (uf-rank uf) id 0)     ; Rank 0
    (set-box! id-box (+ id 1))
    (set-box! (uf-count-box uf) (+ (unbox (uf-count-box uf)) 1))
    id))

;;; uf-find : UnionFind × Id → Id
;;; Find the representative (root) of the set containing id.
;;; Uses path compression: all nodes on path point directly to root after.
(define (uf-find uf id)
  (doc 'type (-> UnionFind Id Id))
  (doc 'description "Find representative of set containing id. Path compression applied.")
  (doc 'export #t)
  (let ([parent (uf-parent uf)])
    (let loop ([i id])
      (let ([p (vector-ref parent i)])
        (if (= p i)
            i  ; Found root
            (let ([root (loop p)])
              ;; Path compression: point directly to root
              (vector-set! parent i root)
              root))))))

;;; uf-union! : UnionFind × Id × Id → Id
;;; Merge the sets containing id1 and id2.
;;; Returns the representative of the merged set.
;;; Uses union by rank: attach smaller tree under larger.
(define (uf-union! uf id1 id2)
  (doc 'type (-> UnionFind Id Id Id))
  (doc 'description "Merge sets containing id1 and id2. Returns merged set's representative.")
  (doc 'export #t)
  (let ([root1 (uf-find uf id1)]
        [root2 (uf-find uf id2)])
    (if (= root1 root2)
        root1  ; Already in same set
        (let ([parent (uf-parent uf)]
              [rank (uf-rank uf)]
              [rank1 (vector-ref (uf-rank uf) root1)]
              [rank2 (vector-ref (uf-rank uf) root2)])
          ;; Decrement set count (merging two sets into one)
          (set-box! (uf-count-box uf) (- (unbox (uf-count-box uf)) 1))
          ;; Union by rank
          (cond
            [(< rank1 rank2)
             ;; Attach root1 under root2
             (vector-set! parent root1 root2)
             root2]
            [(> rank1 rank2)
             ;; Attach root2 under root1
             (vector-set! parent root2 root1)
             root1]
            [else
             ;; Equal rank: attach root2 under root1, increment root1's rank
             (vector-set! parent root2 root1)
             (vector-set! rank root1 (+ rank1 1))
             root1])))))

;;; uf-same-set? : UnionFind × Id × Id → Boolean
;;; Check if two elements are in the same set.
(define (uf-same-set? uf id1 id2)
  (doc 'type (-> UnionFind Id Id Boolean))
  (doc 'description "Check if id1 and id2 are in the same equivalence class.")
  (doc 'export #t)
  (= (uf-find uf id1) (uf-find uf id2)))

;;; ============================================================
;;; Introspection
;;; ============================================================

(doc 'section 'introspection)

;;; uf-roots : UnionFind → (List Id)
;;; Get all root elements (set representatives).
(define (uf-roots uf)
  (doc 'type (-> UnionFind (List Id)))
  (doc 'description "List all set representatives (roots).")
  (doc 'export #t)
  (let ([parent (uf-parent uf)]
        [n (uf-size uf)])
    (let loop ([i 0] [acc '()])
      (if (>= i n)
          (reverse acc)
          (loop (+ i 1)
                (if (= (vector-ref parent i) i)
                    (cons i acc)
                    acc))))))

;;; uf-all-sets-table : UnionFind → Hashtable
;;; Internal: Build hashtable mapping root → list of members in O(N).
(define (uf-all-sets-table uf)
  (let ([n (uf-size uf)]
        [table (make-eqv-hashtable)])
    (do ([i 0 (+ i 1)])
        ((>= i n) table)
      (let ([root (uf-find uf i)])
        (hashtable-set! table root
                        (cons i (hashtable-ref table root '())))))))

;;; uf-all-sets : UnionFind → (List (List Id))
;;; Get all sets as lists of members. O(N) single pass.
(define (uf-all-sets uf)
  (doc 'type (-> UnionFind (List (List Id))))
  (doc 'description "List all equivalence classes as lists of member IDs. O(N) complexity.")
  (doc 'export #t)
  (let ([table (uf-all-sets-table uf)])
    (map (lambda (root) (reverse (hashtable-ref table root '())))
         (uf-roots uf))))

;;; uf-set-members : UnionFind × Id → (List Id)
;;; Get all members of the set containing id. O(N) via table lookup.
(define (uf-set-members uf id)
  (doc 'type (-> UnionFind Id (List Id)))
  (doc 'description "List all elements in the same set as id. O(N) complexity.")
  (doc 'export #t)
  (let ([root (uf-find uf id)]
        [table (uf-all-sets-table uf)])
    (reverse (hashtable-ref table root '()))))

;;; uf-set-size : UnionFind × Id → Nat
;;; Get the size of the set containing id.
(define (uf-set-size uf id)
  (doc 'type (-> UnionFind Id Nat))
  (doc 'description "Number of elements in the set containing id.")
  (doc 'export #t)
  (length (uf-set-members uf id)))

;;; ============================================================
;;; Debugging
;;; ============================================================

(doc 'section 'debug)

;;; uf-debug : UnionFind → Void
;;; Print union-find state for debugging.
(define (uf-debug uf)
  (doc 'type (-> UnionFind Void))
  (doc 'description "Print union-find internal state.")
  (doc 'export #t)
  (let ([n (uf-size uf)])
    (printf "UnionFind: ~a elements, ~a sets~n" n (uf-count uf))
    (printf "  ID | Parent | Rank | Root~n")
    (printf "  ---|--------|------|-----~n")
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (printf "  ~a  |   ~a    |  ~a   |  ~a~n"
              i
              (vector-ref (uf-parent uf) i)
              (vector-ref (uf-rank uf) i)
              (uf-find uf i)))))
