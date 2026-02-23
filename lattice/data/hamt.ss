(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module hamt
;;; @requires prelude dict
(require 'prelude)
(require 'dict)

(doc 'module 'hamt)
(doc 'description "Persistent Hash Array Mapped Trie (HAMT)")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'partial)
(doc 'note "Immutable persistent hash map with O(log32 n) lookup/insert/delete.")
(doc 'note "32-way branching trie indexed by hash bits. Fully transparent to fuel instrumentation.")

;;; ============================================================
;;; Constants
;;; ============================================================

(define *hamt-bits* 5)          ; bits per level
(define *hamt-width* 32)        ; branching factor (2^5)
(define *hamt-mask* #b11111)    ; 5-bit mask
(define *hamt-hash-mask* #x7FFFFFFF) ; positive 31-bit fixnum

;;; ============================================================
;;; Node Types
;;; ============================================================
;;;
;;; Three node types, all tagged lists:
;;;   'hamt-empty                                    — empty trie
;;;   (hamt-leaf hash key value)                     — single entry
;;;   (hamt-collision hash ((k . v) ...))            — hash collision
;;;   (hamt-node bitmap child ...)                   — branch node
;;;
;;; The tag is always (car node), enabling O(1) dispatch.

(define hamt-empty 'hamt-empty)

(define (hamt-empty? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if HAMT is empty")
  (eq? x 'hamt-empty))

(define (hamt-leaf? x)
  (and (pair? x) (eq? (car x) 'hamt-leaf)))

(define (hamt-collision? x)
  (and (pair? x) (eq? (car x) 'hamt-collision)))

(define (hamt-node? x)
  (and (pair? x) (eq? (car x) 'hamt-node)))

;;; Constructors (internal)
(define (make-hamt-leaf hash key value)
  (list 'hamt-leaf hash key value))

(define (make-hamt-collision hash entries)
  (list 'hamt-collision hash entries))

(define (make-hamt-node bitmap children)
  (cons 'hamt-node (cons bitmap children)))

;;; Accessors (internal)
(define (hamt-leaf-hash n) (cadr n))
(define (hamt-leaf-key n) (caddr n))
(define (hamt-leaf-value n) (cadddr n))

(define (hamt-collision-hash n) (cadr n))
(define (hamt-collision-entries n) (caddr n))

(define (hamt-node-bitmap n) (cadr n))
(define (hamt-node-children n) (cddr n))

;;; ============================================================
;;; Bit Manipulation
;;; ============================================================

(define (hash-fragment shift hash)
  ;; Extract 5-bit chunk at given shift level
  (fxlogand (fxarithmetic-shift-right hash shift) *hamt-mask*))

(define (bit-pos fragment)
  ;; Bitmap position for a hash fragment
  (fxarithmetic-shift-left 1 fragment))

(define (bitmap-index bitmap bit)
  ;; Compressed array index: count set bits below this position
  (fxbit-count (fxlogand bitmap (fx- bit 1))))

(define (bitmap-has? bitmap bit)
  ;; Is this bit set in the bitmap?
  (not (fxzero? (fxlogand bitmap bit))))

;;; ============================================================
;;; List Helpers (for children manipulation)
;;; ============================================================

(define (list-set lst idx val)
  ;; Return new list with element at idx replaced
  (let loop ([l lst] [i 0] [acc '()])
    (cond
      [(null? l) (reverse acc)]
      [(= i idx) (loop (cdr l) (+ i 1) (cons val acc))]
      [else (loop (cdr l) (+ i 1) (cons (car l) acc))])))

(define (list-insert lst idx val)
  ;; Return new list with val inserted at idx
  (let loop ([l lst] [i 0] [acc '()])
    (cond
      [(= i idx) (append (reverse (cons val acc)) l)]
      [(null? l) (reverse (cons val acc))]
      [else (loop (cdr l) (+ i 1) (cons (car l) acc))])))

(define (list-remove lst idx)
  ;; Return new list with element at idx removed
  (let loop ([l lst] [i 0] [acc '()])
    (cond
      [(null? l) (reverse acc)]
      [(= i idx) (append (reverse acc) (cdr l))]
      [else (loop (cdr l) (+ i 1) (cons (car l) acc))])))

;;; ============================================================
;;; Hash Function
;;; ============================================================

(define (hamt-hash-key key)
  ;; Default hash: Chez equal-hash, clamped to positive fixnum.
  ;; The HAMT structure is fully transparent; only this entry point
  ;; touches the opaque Chez builtin.
  (fxlogand (equal-hash key) *hamt-hash-mask*))

;;; ============================================================
;;; Core Operations
;;; ============================================================

(define (hamt-lookup key hamt)
  (doc 'export #t)
  (doc 'type '(-> Key HAMT (Maybe Value)))
  (doc 'description "Look up value by key. Returns #f if not found.")
  (let ([hash (hamt-hash-key key)])
    (hamt-lookup-hash hash key hamt 0)))

(define (hamt-lookup-hash hash key node shift)
  (cond
    [(hamt-empty? node) #f]
    [(hamt-leaf? node)
     (if (equal? key (hamt-leaf-key node))
         (hamt-leaf-value node)
         #f)]
    [(hamt-collision? node)
     (let ([pair (assoc key (hamt-collision-entries node))])
       (if pair (cdr pair) #f))]
    [(hamt-node? node)
     (let* ([frag (hash-fragment shift hash)]
            [bit (bit-pos frag)]
            [bitmap (hamt-node-bitmap node)])
       (if (bitmap-has? bitmap bit)
           (let ([idx (bitmap-index bitmap bit)])
             (hamt-lookup-hash hash key
                               (list-ref (hamt-node-children node) idx)
                               (fx+ shift *hamt-bits*)))
           #f))]
    [else #f]))

(define (hamt-has-key? key hamt)
  (doc 'export #t)
  (doc 'type '(-> Key HAMT Boolean))
  (doc 'description "Check if key exists in HAMT")
  ;; Use a sentinel to distinguish "key maps to #f" from "key absent"
  (let ([sentinel (list 'not-found)])
    (not (eq? sentinel (hamt-lookup-or key hamt sentinel)))))

(define (hamt-lookup-or key hamt default)
  ;; Lookup with explicit default (internal, avoids #f ambiguity)
  (let ([hash (hamt-hash-key key)])
    (hamt-lookup-or-hash hash key hamt 0 default)))

(define (hamt-lookup-or-hash hash key node shift default)
  (cond
    [(hamt-empty? node) default]
    [(hamt-leaf? node)
     (if (equal? key (hamt-leaf-key node))
         (hamt-leaf-value node)
         default)]
    [(hamt-collision? node)
     (let ([pair (assoc key (hamt-collision-entries node))])
       (if pair (cdr pair) default))]
    [(hamt-node? node)
     (let* ([frag (hash-fragment shift hash)]
            [bit (bit-pos frag)]
            [bitmap (hamt-node-bitmap node)])
       (if (bitmap-has? bitmap bit)
           (let ([idx (bitmap-index bitmap bit)])
             (hamt-lookup-or-hash hash key
                                  (list-ref (hamt-node-children node) idx)
                                  (fx+ shift *hamt-bits*) default))
           default))]
    [else default]))

;;; ------------------------------------------------------------
;;; Insert / Update
;;; ------------------------------------------------------------

(define (hamt-assoc key value hamt)
  (doc 'export #t)
  (doc 'type '(-> Key Value HAMT HAMT))
  (doc 'description "Associate key with value, returning new HAMT")
  (let ([hash (hamt-hash-key key)])
    (hamt-assoc-hash hash key value hamt 0)))

(define (hamt-assoc-hash hash key value node shift)
  (cond
    [(hamt-empty? node)
     (make-hamt-leaf hash key value)]

    [(hamt-leaf? node)
     (let ([existing-hash (hamt-leaf-hash node)]
           [existing-key (hamt-leaf-key node)])
       (cond
         [(equal? key existing-key)
          (make-hamt-leaf hash key value)]
         [(= hash existing-hash)
          (make-hamt-collision hash
            (list (cons key value)
                  (cons existing-key (hamt-leaf-value node))))]
         [else
          (create-branch existing-hash node hash
                         (make-hamt-leaf hash key value)
                         shift)]))]

    [(hamt-collision? node)
     (if (= hash (hamt-collision-hash node))
         (make-hamt-collision hash
           (collision-assoc key value (hamt-collision-entries node)))
         (create-branch (hamt-collision-hash node) node
                        hash (make-hamt-leaf hash key value)
                        shift))]

    [(hamt-node? node)
     (let* ([frag (hash-fragment shift hash)]
            [bit (bit-pos frag)]
            [bitmap (hamt-node-bitmap node)]
            [children (hamt-node-children node)])
       (if (bitmap-has? bitmap bit)
           (let* ([idx (bitmap-index bitmap bit)]
                  [child (list-ref children idx)]
                  [new-child (hamt-assoc-hash hash key value child
                                              (fx+ shift *hamt-bits*))])
             (make-hamt-node bitmap (list-set children idx new-child)))
           (let* ([idx (bitmap-index bitmap bit)]
                  [new-bitmap (fxlogor bitmap bit)])
             (make-hamt-node new-bitmap
                             (list-insert children idx
                                          (make-hamt-leaf hash key value))))))]

    [else (make-hamt-leaf hash key value)]))

(define (create-branch hash1 node1 hash2 node2 shift)
  ;; Create a branch containing two nodes with different hashes.
  ;; If hash fragments collide at this level, recurse deeper.
  (let* ([frag1 (hash-fragment shift hash1)]
         [frag2 (hash-fragment shift hash2)]
         [bit1 (bit-pos frag1)]
         [bit2 (bit-pos frag2)])
    (if (= frag1 frag2)
        ;; Same fragment at this level — recurse one level deeper
        (let ([child (create-branch hash1 node1 hash2 node2
                                    (fx+ shift *hamt-bits*))])
          (make-hamt-node bit1 (list child)))
        ;; Different fragments — two-child branch
        (if (< frag1 frag2)
            (make-hamt-node (fxlogor bit1 bit2) (list node1 node2))
            (make-hamt-node (fxlogor bit1 bit2) (list node2 node1))))))

(define (collision-assoc key value entries)
  ;; Update or add entry in a collision list
  (let loop ([es entries] [acc '()] [found #f])
    (cond
      [(null? es)
       (if found
           (reverse acc)
           (reverse (cons (cons key value) acc)))]
      [(equal? key (caar es))
       (loop (cdr es) (cons (cons key value) acc) #t)]
      [else
       (loop (cdr es) (cons (car es) acc) found)])))

;;; ------------------------------------------------------------
;;; Delete
;;; ------------------------------------------------------------

(define (hamt-dissoc key hamt)
  (doc 'export #t)
  (doc 'type '(-> Key HAMT HAMT))
  (doc 'description "Remove key from HAMT, returning new HAMT")
  (let ([hash (hamt-hash-key key)])
    (hamt-dissoc-hash hash key hamt 0)))

(define (hamt-dissoc-hash hash key node shift)
  (cond
    [(hamt-empty? node) hamt-empty]

    [(hamt-leaf? node)
     (if (equal? key (hamt-leaf-key node))
         hamt-empty
         node)]

    [(hamt-collision? node)
     (if (= hash (hamt-collision-hash node))
         (let ([new-entries (collision-dissoc key (hamt-collision-entries node))])
           (cond
             [(null? new-entries) hamt-empty]
             [(null? (cdr new-entries))
              ;; Single entry left: demote to leaf
              (make-hamt-leaf hash (caar new-entries) (cdar new-entries))]
             [else
              (make-hamt-collision hash new-entries)]))
         node)]

    [(hamt-node? node)
     (let* ([frag (hash-fragment shift hash)]
            [bit (bit-pos frag)]
            [bitmap (hamt-node-bitmap node)]
            [children (hamt-node-children node)])
       (if (bitmap-has? bitmap bit)
           (let* ([idx (bitmap-index bitmap bit)]
                  [child (list-ref children idx)]
                  [new-child (hamt-dissoc-hash hash key child
                                               (fx+ shift *hamt-bits*))])
             (cond
               ;; Child became empty: remove from branch
               [(hamt-empty? new-child)
                (let ([new-bitmap (fxlogand bitmap (fxlognot bit))])
                  (if (fxzero? new-bitmap)
                      hamt-empty
                      (let ([new-children (list-remove children idx)])
                        ;; If single child remains and it's a leaf/collision,
                        ;; collapse the branch
                        (if (and (null? (cdr new-children))
                                 (not (hamt-node? (car new-children))))
                            (car new-children)
                            (make-hamt-node new-bitmap new-children)))))]
               ;; Child is a leaf/collision that could collapse the branch
               [(and (not (hamt-node? new-child))
                     (= 1 (length children)))
                new-child]
               ;; Normal case: update child in place
               [else
                (make-hamt-node bitmap (list-set children idx new-child))]))
           ;; Key not in this branch
           node))]

    [else node]))

(define (collision-dissoc key entries)
  ;; Remove entry from collision list
  (let loop ([es entries] [acc '()])
    (cond
      [(null? es) (reverse acc)]
      [(equal? key (caar es)) (loop (cdr es) acc)]
      [else (loop (cdr es) (cons (car es) acc))])))

;;; ============================================================
;;; Derived Operations
;;; ============================================================

(define (hamt-fold f init hamt)
  (doc 'export #t)
  (doc 'type '(-> (-> Acc Key Value Acc) Acc HAMT Acc))
  (doc 'description "Fold over all key-value pairs in the HAMT")
  (cond
    [(hamt-empty? hamt) init]
    [(hamt-leaf? hamt)
     (f init (hamt-leaf-key hamt) (hamt-leaf-value hamt))]
    [(hamt-collision? hamt)
     (fold-left (lambda (acc pair) (f acc (car pair) (cdr pair)))
                init (hamt-collision-entries hamt))]
    [(hamt-node? hamt)
     (fold-left (lambda (acc child) (hamt-fold f acc child))
                init (hamt-node-children hamt))]
    [else init]))

(define (hamt-size hamt)
  (doc 'export #t)
  (doc 'type '(-> HAMT Integer))
  (doc 'description "Count key-value pairs (O(n) traversal)")
  (hamt-fold (lambda (acc k v) (+ acc 1)) 0 hamt))

(define (hamt-keys hamt)
  (doc 'export #t)
  (doc 'type '(-> HAMT (List Key)))
  (doc 'description "Get list of all keys")
  (hamt-fold (lambda (acc k v) (cons k acc)) '() hamt))

(define (hamt-values hamt)
  (doc 'export #t)
  (doc 'type '(-> HAMT (List Value)))
  (doc 'description "Get list of all values")
  (hamt-fold (lambda (acc k v) (cons v acc)) '() hamt))

(define (hamt-entries hamt)
  (doc 'export #t)
  (doc 'type '(-> HAMT (List (Pair Key Value))))
  (doc 'description "Get list of all key-value pairs")
  (hamt-fold (lambda (acc k v) (cons (cons k v) acc)) '() hamt))

(define (hamt-map-values f hamt)
  (doc 'export #t)
  (doc 'type '(-> (-> Value Value) HAMT HAMT))
  (doc 'description "Apply function to all values, preserving structure")
  (cond
    [(hamt-empty? hamt) hamt-empty]
    [(hamt-leaf? hamt)
     (make-hamt-leaf (hamt-leaf-hash hamt)
                     (hamt-leaf-key hamt)
                     (f (hamt-leaf-value hamt)))]
    [(hamt-collision? hamt)
     (make-hamt-collision
       (hamt-collision-hash hamt)
       (map (lambda (pair) (cons (car pair) (f (cdr pair))))
            (hamt-collision-entries hamt)))]
    [(hamt-node? hamt)
     (make-hamt-node
       (hamt-node-bitmap hamt)
       (map (lambda (child) (hamt-map-values f child))
            (hamt-node-children hamt)))]
    [else hamt]))

(define (hamt-filter pred hamt)
  (doc 'export #t)
  (doc 'type '(-> (-> Key Value Boolean) HAMT HAMT))
  (doc 'description "Filter HAMT by predicate on key-value pairs")
  (hamt-fold (lambda (acc k v)
               (if (pred k v) (hamt-assoc k v acc) acc))
             hamt-empty hamt))

(define (hamt-merge h1 h2)
  (doc 'export #t)
  (doc 'type '(-> HAMT HAMT HAMT))
  (doc 'description "Merge two HAMTs. h2 wins on key conflict.")
  (hamt-fold (lambda (acc k v) (hamt-assoc k v acc))
             h1 h2))

(define (hamt-merge-with f h1 h2)
  (doc 'export #t)
  (doc 'type '(-> (-> Value Value Value) HAMT HAMT HAMT))
  (doc 'description "Merge two HAMTs with conflict resolution function")
  (hamt-fold (lambda (acc k v)
               (let ([existing (hamt-lookup k acc)])
                 (if existing
                     (hamt-assoc k (f existing v) acc)
                     (hamt-assoc k v acc))))
             h1 h2))

;;; ============================================================
;;; Conversions
;;; ============================================================

(define (hamt->dict hamt)
  (doc 'export #t)
  (doc 'type '(-> HAMT Dict))
  (doc 'description "Convert HAMT to alist-based dict")
  (hamt-entries hamt))

(define (dict->hamt dict)
  (doc 'export #t)
  (doc 'type '(-> Dict HAMT))
  (doc 'description "Convert alist-based dict to HAMT")
  (fold-left (lambda (acc pair)
               (hamt-assoc (car pair) (cdr pair) acc))
             hamt-empty dict))

(define (alist->hamt alist)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Key Value)) HAMT))
  (doc 'description "Convert association list to HAMT")
  (dict->hamt alist))
