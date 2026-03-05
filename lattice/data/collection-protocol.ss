;;; lattice/data/collection-protocol.ss — Unified Collection Protocols
;;; @module collection-protocol
;;; @requires prelude protocol

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'protocol)

(doc 'module 'collection-protocol)
(doc 'description "Protocol-based interface for collections enabling generic algorithms.

Three protocol families:
  1. Core Collection - empty?, size, fold, to-list (all collections)
  2. Keyed Collection - lookup, insert, delete, contains? (associative structures)
  3. Spatial Collection - nearest, range, radius (spatial indices)
  4. Priority Collection - peek, pop, insert (priority queues)

Collections implement relevant protocols. Generic algorithms use protocol dispatch.

Example:
  (coll-size my-avl)      ; works on AVL
  (coll-size my-heap)     ; works on heap
  (coll-size my-kdtree)   ; works on kdtree

  ;; Generic filter-to-list using fold
  (coll-filter my-coll pred)  ; works on any collection with fold")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude.ss lattice/fp/protocol.ss))

;;; ============================================================
;;; Core Collection Protocols
;;; ============================================================
;;; Every collection should implement these.

(doc 'section 'core-protocols)

(define-protocol (coll-empty? coll)
  "Check if collection is empty. Returns #t if no elements.")

(define-protocol (coll-size coll)
  "Return number of elements in collection. O(1) to O(n) depending on impl.")

(define-protocol (coll-fold coll fn init)
  "Left fold over collection elements.
   fn: (acc element) -> acc

   Element type varies by collection:
     - AVL: (cons key value)
     - Heap: element
     - KDTree: point
     - Quadtree: (x y data)

   Traversal order (deterministic per collection type):
     - AVL: in-order by key (sorted ascending)
     - Heap: unspecified tree traversal (NOT heap/priority order)
     - KDTree: in-order (left, node, right)
     - Quadtree: spatial order (SW -> SE -> NW -> NE)

   For heap priority-order traversal, use repeated prio-pop.")

(define-protocol (coll-to-list coll)
  "Convert collection to a list of elements.")

;;; ============================================================
;;; Generic Operations (built on core protocols)
;;; ============================================================
;;; These work on ANY collection implementing the core protocols.

(doc 'section 'generic-operations)

(define (coll-count coll pred)
  (doc 'export #t)
  (doc 'type '(-> Collection (-> Element Boolean) Nat))
  (doc 'description "Count elements satisfying predicate")
  (coll-fold coll
             (lambda (acc elem)
               (if (pred elem) (+ acc 1) acc))
             0))

(define (coll-any? coll pred)
  (doc 'export #t)
  (doc 'type '(-> Collection (-> Element Boolean) Boolean))
  (doc 'description "Check if any element satisfies predicate. Skips pred after first match via or short-circuit.")
  (doc 'note "Predicate calls are O(k) but fold still traverses O(n) elements.
True early termination requires a coll-fold-while protocol (future work).")
  (if (coll-fold coll
                 (lambda (acc elem)
                   (or acc (pred elem)))
                 #f)
      #t #f))

(define (coll-all? coll pred)
  (doc 'export #t)
  (doc 'type '(-> Collection (-> Element Boolean) Boolean))
  (doc 'description "Check if all elements satisfy predicate. Skips pred after first mismatch via and short-circuit.")
  (doc 'note "Predicate calls are O(k) but fold still traverses O(n) elements.
True early termination requires a coll-fold-while protocol (future work).")
  (if (coll-fold coll
                 (lambda (acc elem)
                   (and acc (pred elem)))
                 #t)
      #t #f))

(define (coll-filter-list coll pred)
  (doc 'export #t)
  (doc 'type '(-> Collection (-> Element Boolean) (List Element)))
  (doc 'description "Return list of elements satisfying predicate")
  (reverse
   (coll-fold coll
              (lambda (acc elem)
                (if (pred elem) (cons elem acc) acc))
              '())))

(define (coll-map-list coll fn)
  (doc 'export #t)
  (doc 'type '(-> Collection (-> Element a) (List a)))
  (doc 'description "Map function over elements, returning a list")
  (reverse
   (coll-fold coll
              (lambda (acc elem)
                (cons (fn elem) acc))
              '())))

(define (coll-find coll pred)
  (doc 'export #t)
  (doc 'type '(-> Collection (-> Element Boolean) (Maybe Element)))
  (doc 'description "Find first element satisfying predicate, or #f. Skips pred after match found.")
  (doc 'note "Predicate calls are O(k) but fold still traverses O(n) elements.
True early termination requires a coll-fold-while protocol (future work).")
  (coll-fold coll
             (lambda (acc elem)
               (if acc acc
                   (if (pred elem) elem #f)))
             #f))

(define (coll-partition coll pred)
  (doc 'export #t)
  (doc 'type '(-> Collection (-> Element Boolean) (Values (List Element) (List Element))))
  (doc 'description "Partition into (satisfying, not-satisfying) lists")
  (let ([result (coll-fold coll
                           (lambda (acc elem)
                             (if (pred elem)
                                 (cons (cons elem (car acc)) (cdr acc))
                                 (cons (car acc) (cons elem (cdr acc)))))
                           (cons '() '()))])
    (values (reverse (car result)) (reverse (cdr result)))))

(define (coll-sum coll)
  (doc 'export #t)
  (doc 'type '(-> Collection Num))
  (doc 'description "Sum numeric elements")
  (coll-fold coll (lambda (acc elem) (+ acc elem)) 0))

(define (coll-product coll)
  (doc 'export #t)
  (doc 'type '(-> Collection Num))
  (doc 'description "Product of numeric elements")
  (coll-fold coll (lambda (acc elem) (* acc elem)) 1))

;;; ============================================================
;;; Keyed Collection Protocols
;;; ============================================================
;;; For associative data structures (maps, dictionaries, trees).

(doc 'section 'keyed-protocols)

(define-protocol (keyed-lookup coll key)
  "Look up value by key. Returns #f if not found.
   WARNING: Cannot distinguish 'not found' from 'value is #f'.
   Use keyed-ref for unambiguous lookups, or keyed-contains? to check existence.")

(define-protocol (keyed-ref coll key)
  "Look up value by key. Returns (just value) if found, nothing (#f) if not.
   Unlike keyed-lookup, this unambiguously distinguishes missing keys from #f values.
   Requires: lattice/fp/meta/combinators.ss for just/nothing.")

(define-protocol (keyed-insert coll key value)
  "Insert key-value pair, returning new collection.")

(define-protocol (keyed-delete coll key)
  "Delete key, returning new collection.")

(define-protocol (keyed-contains? coll key)
  "Check if key exists in collection.")

(define-protocol (keyed-keys coll)
  "Return list of all keys.")

(define-protocol (keyed-values coll)
  "Return list of all values.")

;;; ============================================================
;;; Spatial Collection Protocols
;;; ============================================================
;;; For spatial indices (kd-tree, quadtree, octree, R-tree).

(doc 'section 'spatial-protocols)

(define-protocol (spatial-nearest coll query)
  "Find nearest element to query point. Returns element or #f if empty.")

(define-protocol (spatial-knn coll query k)
  "Find k nearest elements to query point. Returns list.")

(define-protocol (spatial-range coll min-bound max-bound)
  "Find all elements within axis-aligned bounding box.")

(define-protocol (spatial-radius coll center radius)
  "Find all elements within radius of center point.")

(define-protocol (spatial-contains? coll point)
  "Check if point exists in spatial index.")

;;; ============================================================
;;; Priority Collection Protocols
;;; ============================================================
;;; For heaps and priority queues.

(doc 'section 'priority-protocols)

(define-protocol (prio-peek coll)
  "Get highest-priority element without removing. Error if empty.")

(define-protocol (prio-pop coll)
  "Remove highest-priority element, return (values new-coll element).")

(define-protocol (prio-insert coll elem)
  "Insert element, returning new collection.")

(define-protocol (prio-merge coll1 coll2)
  "Merge two priority collections into one.")

;;; ============================================================
;;; Introspection
;;; ============================================================

(doc 'section 'introspection)

(define (coll-protocols type-tag)
  (doc 'export #t)
  (doc 'type '(-> Symbol (List Symbol)))
  (doc 'description "List protocols implemented by a type")
  (filter (lambda (proto)
            (type-implements? type-tag proto))
          '(coll-empty? coll-size coll-fold coll-to-list
            keyed-lookup keyed-insert keyed-delete keyed-contains?
            keyed-keys keyed-values
            spatial-nearest spatial-knn spatial-range spatial-radius spatial-contains?
            prio-peek prio-pop prio-insert prio-merge)))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

(doc 'exports "Core Collection:
  coll-empty?        - Is collection empty?
  coll-size          - Element count
  coll-fold          - Left fold
  coll-to-list       - Convert to list

Generic (work on any collection with fold):
  coll-count         - Count matching elements
  coll-any?          - Any match predicate?
  coll-all?          - All match predicate?
  coll-filter-list   - Filter to list
  coll-map-list      - Map to list
  coll-find          - Find first match
  coll-partition     - Split by predicate
  coll-sum           - Sum elements
  coll-product       - Product of elements

Keyed Collection:
  keyed-lookup       - Get by key
  keyed-insert       - Insert key-value
  keyed-delete       - Delete by key
  keyed-contains?    - Key exists?
  keyed-keys         - All keys
  keyed-values       - All values

Spatial Collection:
  spatial-nearest    - Nearest neighbor
  spatial-knn        - K nearest neighbors
  spatial-range      - Box query
  spatial-radius     - Radius query
  spatial-contains?  - Point membership

Priority Collection:
  prio-peek          - View top element
  prio-pop           - Remove top
  prio-insert        - Add element
  prio-merge         - Merge two queues

Introspection:
  coll-protocols     - List implemented protocols")

