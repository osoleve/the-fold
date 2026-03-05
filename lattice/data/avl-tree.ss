;;; lattice/data/avl-tree.ss — AVL Tree
;;; @module avl-tree
;;; @requires prelude
;;; @description Self-balancing AVL tree with O(log n) operations
;;; @purity total
;;; @stability stable

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'avl-tree)
(doc 'description "AVL Tree (Self-Balancing Binary Search Tree) — Purely functional AVL tree with O(log n) insert, delete, and lookup. Maintains balance invariant: |height(left) - height(right)| <= 1. AVL α = Empty | (Node height key value left right). Keys must be comparable with < (uses default ordering). For custom ordering, use avl-*-by functions with a comparator.")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'total)

(doc 'section 'core-representation)

(define avl-empty 'avl-empty)
(doc avl-empty 'export #t)
(doc avl-empty 'type 'AVL)
(doc avl-empty 'description "The empty AVL tree")

(define (avl-empty? tree)
  (doc 'export #t)
  (doc 'type '(-> AVL Boolean))
  (doc 'description "Check if tree is empty")
  (eq? tree 'avl-empty))

(define (avl-node height key value left right)
  (doc 'type '(-> Nat κ α AVL AVL AVL))
  (doc 'description "Internal node constructor. Height is cached for O(1) access")
  (list 'avl-node height key value left right))

(define (avl-node? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'avl-node)))

(define (avl-height tree)
  (doc 'export #t)
  (if (avl-empty? tree) 0 (cadr tree)))

(define (avl-key tree)
  (doc 'export #t)
  (caddr tree))

(define (avl-value tree)
  (doc 'export #t)
  (cadddr tree))

(define (avl-left tree)
  (doc 'export #t)
  (car (cddddr tree)))

(define (avl-right tree)
  (doc 'export #t)
  (cadr (cddddr tree)))

(doc 'section 'smart-constructor-and-rotations)

(define (avl-balance-factor tree)
  (doc 'export #t)
  (doc 'type '(-> AVL Int))
  (doc 'description "Compute balance factor: height(left) - height(right). AVL invariant: -1 <= balance-factor <= 1")
  (if (avl-empty? tree)
      0
      (- (avl-height (avl-left tree))
         (avl-height (avl-right tree)))))

(define (make-avl-node key value left right)
  (doc 'export #t)
  (doc 'type '(-> κ α AVL AVL AVL))
  (doc 'description "Smart constructor that recomputes height")
  (avl-node (+ 1 (max (avl-height left) (avl-height right)))
            key value left right))

(define (rotate-left tree)
  (doc 'type '(-> AVL AVL))
  (doc 'description "Left rotation for right-heavy trees")
  (let ([x-key (avl-key tree)]
        [x-val (avl-value tree)]
        [a (avl-left tree)]
        [y (avl-right tree)])
    (let ([y-key (avl-key y)]
          [y-val (avl-value y)]
          [b (avl-left y)]
          [c (avl-right y)])
      (make-avl-node y-key y-val
                     (make-avl-node x-key x-val a b)
                     c))))

(define (rotate-right tree)
  (doc 'type '(-> AVL AVL))
  (doc 'description "Right rotation for left-heavy trees")
  (let ([y-key (avl-key tree)]
        [y-val (avl-value tree)]
        [x (avl-left tree)]
        [c (avl-right tree)])
    (let ([x-key (avl-key x)]
          [x-val (avl-value x)]
          [a (avl-left x)]
          [b (avl-right x)])
      (make-avl-node x-key x-val
                     a
                     (make-avl-node y-key y-val b c)))))

(define (rebalance tree)
  (doc 'type '(-> AVL AVL))
  (doc 'description "Rebalance tree after insertion or deletion")
  (let ([bf (avl-balance-factor tree)])
    (cond
      [(> bf 1)
       (if (< (avl-balance-factor (avl-left tree)) 0)
           (rotate-right (make-avl-node (avl-key tree) (avl-value tree)
                                        (rotate-left (avl-left tree))
                                        (avl-right tree)))
           (rotate-right tree))]
      [(< bf -1)
       (if (> (avl-balance-factor (avl-right tree)) 0)
           (rotate-left (make-avl-node (avl-key tree) (avl-value tree)
                                       (avl-left tree)
                                       (rotate-right (avl-right tree))))
           (rotate-left tree))]
      [else tree])))

(doc 'section 'basic-operations)

(define (avl-lookup key tree)
  (doc 'export #t)
  (doc 'type '(-> κ AVL (Maybe α)))
  (doc 'description "Look up value by key. Returns #f if not found")
  (avl-lookup-by < key tree))

(define (avl-lookup-by cmp key tree)
  (doc 'export #t)
  (doc 'type '(-> (-> κ κ Boolean) κ AVL (Maybe α)))
  (doc 'description "Look up with custom comparator")
  (if (avl-empty? tree)
      #f
      (let ([k (avl-key tree)])
        (cond
          [(cmp key k) (avl-lookup-by cmp key (avl-left tree))]
          [(cmp k key) (avl-lookup-by cmp key (avl-right tree))]
          [else (avl-value tree)]))))

(define (avl-contains? key tree)
  (doc 'export #t)
  (doc 'type '(-> κ AVL Boolean))
  (doc 'description "Check if key exists in tree")
  (avl-contains-by? < key tree))

(define (avl-contains-by? cmp key tree)
  (doc 'export #t)
  (doc 'type '(-> (-> κ κ Boolean) κ AVL Boolean))
  (if (avl-empty? tree)
      #f
      (let ([k (avl-key tree)])
        (cond
          [(cmp key k) (avl-contains-by? cmp key (avl-left tree))]
          [(cmp k key) (avl-contains-by? cmp key (avl-right tree))]
          [else #t]))))

(define (avl-insert key value tree)
  (doc 'export #t)
  (doc 'type '(-> κ α AVL AVL))
  (doc 'description "Insert key-value pair. If key exists, updates value")
  (avl-insert-by < key value tree))

(define (avl-insert-by cmp key value tree)
  (doc 'export #t)
  (doc 'type '(-> (-> κ κ Boolean) κ α AVL AVL))
  (doc 'description "Insert with custom comparator")
  (if (avl-empty? tree)
      (make-avl-node key value avl-empty avl-empty)
      (let ([k (avl-key tree)]
            [v (avl-value tree)]
            [left (avl-left tree)]
            [right (avl-right tree)])
        (cond
          [(cmp key k)
           (rebalance (make-avl-node k v (avl-insert-by cmp key value left) right))]
          [(cmp k key)
           (rebalance (make-avl-node k v left (avl-insert-by cmp key value right)))]
          [else
           (make-avl-node key value left right)]))))

(doc 'section 'deletion)

(define (avl-min-node tree)
  (doc 'export #t)
  (doc 'type '(-> AVL (Pair κ α)))
  (doc 'description "Find minimum key-value pair")
  (if (avl-empty? tree)
      (error 'avl-min-node "Cannot find min in empty tree")
      (if (avl-empty? (avl-left tree))
          (cons (avl-key tree) (avl-value tree))
          (avl-min-node (avl-left tree)))))

(define (avl-max-node tree)
  (doc 'export #t)
  (doc 'type '(-> AVL (Pair κ α)))
  (doc 'description "Find maximum key-value pair")
  (if (avl-empty? tree)
      (error 'avl-max-node "Cannot find max in empty tree")
      (if (avl-empty? (avl-right tree))
          (cons (avl-key tree) (avl-value tree))
          (avl-max-node (avl-right tree)))))

(define (avl-delete-min tree)
  (doc 'export #t)
  (doc 'type '(-> AVL AVL))
  (doc 'description "Delete minimum element")
  (avl-delete-min-by < tree))

(define (avl-delete-min-by cmp tree)
  (doc 'export #t)
  (doc 'type '(-> (-> κ κ Boolean) AVL AVL))
  (if (avl-empty? tree)
      (error 'avl-delete-min "Cannot delete from empty tree")
      (if (avl-empty? (avl-left tree))
          (avl-right tree)
          (rebalance (make-avl-node (avl-key tree) (avl-value tree)
                                    (avl-delete-min-by cmp (avl-left tree))
                                    (avl-right tree))))))

(define (avl-delete key tree)
  (doc 'export #t)
  (doc 'type '(-> κ AVL AVL))
  (doc 'description "Delete key from tree. Returns unchanged tree if key not found")
  (avl-delete-by < key tree))

(define (avl-delete-by cmp key tree)
  (doc 'export #t)
  (doc 'type '(-> (-> κ κ Boolean) κ AVL AVL))
  (if (avl-empty? tree)
      tree
      (let ([k (avl-key tree)]
            [v (avl-value tree)]
            [left (avl-left tree)]
            [right (avl-right tree)])
        (cond
          [(cmp key k)
           (rebalance (make-avl-node k v (avl-delete-by cmp key left) right))]
          [(cmp k key)
           (rebalance (make-avl-node k v left (avl-delete-by cmp key right)))]
          [else
           (cond
             [(avl-empty? left) right]
             [(avl-empty? right) left]
             [else
              (let ([succ (avl-min-node right)])
                (rebalance (make-avl-node (car succ) (cdr succ)
                                          left
                                          (avl-delete-min-by cmp right))))])]))))

(doc 'section 'accessors)

(define (avl-min tree)
  (doc 'export #t)
  (doc 'type '(-> AVL κ))
  (doc 'description "Get minimum key")
  (car (avl-min-node tree)))

(define (avl-max tree)
  (doc 'export #t)
  (doc 'type '(-> AVL κ))
  (doc 'description "Get maximum key")
  (car (avl-max-node tree)))

(define (avl-min-value tree)
  (doc 'export #t)
  (doc 'type '(-> AVL α))
  (doc 'description "Get value associated with minimum key")
  (cdr (avl-min-node tree)))

(define (avl-max-value tree)
  (doc 'export #t)
  (doc 'type '(-> AVL α))
  (doc 'description "Get value associated with maximum key")
  (cdr (avl-max-node tree)))

(define (avl-size tree)
  (doc 'export #t)
  (doc 'type '(-> AVL Nat))
  (doc 'description "Get number of elements. O(n)")
  (if (avl-empty? tree)
      0
      (+ 1 (avl-size (avl-left tree)) (avl-size (avl-right tree)))))

(doc 'section 'conversions)

(define (avl->list tree)
  (doc 'export #t)
  (doc 'type '(-> AVL (List (Pair κ α))))
  (doc 'description "Convert to sorted association list (in-order traversal)")
  (if (avl-empty? tree)
      '()
      (append (avl->list (avl-left tree))
              (list (cons (avl-key tree) (avl-value tree)))
              (avl->list (avl-right tree)))))

(define (avl-keys tree)
  (doc 'export #t)
  (doc 'type '(-> AVL (List κ)))
  (doc 'description "Get all keys in sorted order")
  (map car (avl->list tree)))

(define (avl-values tree)
  (doc 'export #t)
  (doc 'type '(-> AVL (List α)))
  (doc 'description "Get all values in key-sorted order")
  (map cdr (avl->list tree)))

(define (list->avl lst)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair κ α)) AVL))
  (doc 'description "Build tree from association list")
  (fold-left (lambda (tree pair)
               (avl-insert (car pair) (cdr pair) tree))
             avl-empty
             lst))

(define (avl-from-keys keys)
  (doc 'export #t)
  (doc 'type '(-> (List κ) AVL))
  (doc 'description "Build tree from keys (values are the keys themselves)")
  (fold-left (lambda (tree key)
               (avl-insert key key tree))
             avl-empty
             keys))

(doc 'section 'higher-order-operations)

(define (avl-fold f init tree)
  (doc 'export #t)
  (doc 'type '(-> (-> β κ α β) β AVL β))
  (doc 'description "Fold over tree in key order (left-to-right)")
  (if (avl-empty? tree)
      init
      (avl-fold f
                (f (avl-fold f init (avl-left tree))
                   (avl-key tree)
                   (avl-value tree))
                (avl-right tree))))

(define (avl-fold-right f init tree)
  (doc 'export #t)
  (doc 'type '(-> (-> κ α β β) β AVL β))
  (doc 'description "Fold over tree in reverse key order (right-to-left)")
  (if (avl-empty? tree)
      init
      (avl-fold-right f
                      (f (avl-key tree)
                         (avl-value tree)
                         (avl-fold-right f init (avl-right tree)))
                      (avl-left tree))))

(define (avl-map f tree)
  (doc 'export #t)
  (doc 'type '(-> (-> α β) AVL[κ,α] AVL[κ,β]))
  (doc 'description "Map function over values, preserving tree structure")
  (if (avl-empty? tree)
      avl-empty
      (make-avl-node (avl-key tree)
                     (f (avl-value tree))
                     (avl-map f (avl-left tree))
                     (avl-map f (avl-right tree)))))

(define (avl-filter pred tree)
  (doc 'export #t)
  (doc 'type '(-> (-> κ α Boolean) AVL AVL))
  (doc 'description "Filter tree by predicate on key-value pairs")
  (avl-fold (lambda (acc k v)
              (if (pred k v)
                  (avl-insert k v acc)
                  acc))
            avl-empty
            tree))

(doc 'section 'range-queries)

(define (avl-range lo hi tree)
  (doc 'export #t)
  (doc 'type '(-> κ κ AVL (List (Pair κ α))))
  (doc 'description "Get all key-value pairs where lo <= key <= hi")
  (avl-range-by < lo hi tree))

(define (avl-range-by cmp lo hi tree)
  (doc 'export #t)
  (doc 'type '(-> (-> κ κ Boolean) κ κ AVL (List (Pair κ α))))
  (if (avl-empty? tree)
      '()
      (let ([k (avl-key tree)]
            [v (avl-value tree)])
        (append
         (if (cmp lo k)
             (avl-range-by cmp lo hi (avl-left tree))
             '())
         (if (and (not (cmp k lo)) (not (cmp hi k)))
             (list (cons k v))
             '())
         (if (cmp k hi)
             (avl-range-by cmp lo hi (avl-right tree))
             '())))))

(define (avl-keys-between lo hi tree)
  (doc 'export #t)
  (doc 'type '(-> κ κ AVL (List κ)))
  (doc 'description "Get all keys in range [lo, hi]")
  (map car (avl-range lo hi tree)))

(define (avl-less-than bound tree)
  (doc 'export #t)
  (doc 'type '(-> κ AVL (List (Pair κ α))))
  (doc 'description "Get all pairs with key < bound")
  (if (avl-empty? tree)
      '()
      (let ([k (avl-key tree)])
        (if (< k bound)
            (append (avl-less-than bound (avl-left tree))
                    (list (cons k (avl-value tree)))
                    (avl-less-than bound (avl-right tree)))
            (avl-less-than bound (avl-left tree))))))

(define (avl-greater-than bound tree)
  (doc 'export #t)
  (doc 'type '(-> κ AVL (List (Pair κ α))))
  (doc 'description "Get all pairs with key > bound")
  (if (avl-empty? tree)
      '()
      (let ([k (avl-key tree)])
        (if (> k bound)
            (append (avl-greater-than bound (avl-left tree))
                    (list (cons k (avl-value tree)))
                    (avl-greater-than bound (avl-right tree)))
            (avl-greater-than bound (avl-right tree))))))

(doc 'section 'set-operations)

(define (avl-set-insert key tree)
  (doc 'export #t)
  (doc 'type '(-> κ AVL AVL))
  (doc 'description "Insert key as a set element (value = key)")
  (avl-insert key key tree))

(define avl-set-delete avl-delete)
(doc 'avl-set-delete 'export #t)

(define avl-set-member? avl-contains?)
(doc 'avl-set-member? 'export #t)

(define (avl-set-union t1 t2)
  (doc 'export #t)
  (doc 'type '(-> AVL AVL AVL))
  (doc 'description "Union of two sets")
  (avl-fold (lambda (acc k v) (avl-set-insert k acc)) t1 t2))

(define (avl-set-intersection t1 t2)
  (doc 'export #t)
  (doc 'type '(-> AVL AVL AVL))
  (doc 'description "Intersection of two sets")
  (avl-filter (lambda (k v) (avl-contains? k t2)) t1))

(define (avl-set-difference t1 t2)
  (doc 'export #t)
  (doc 'type '(-> AVL AVL AVL))
  (doc 'description "Set difference (t1 - t2)")
  (avl-filter (lambda (k v) (not (avl-contains? k t2))) t1))

(doc 'section 'verification)

(define (avl-valid? tree)
  (doc 'export #t)
  (doc 'type '(-> AVL Boolean))
  (doc 'description "Check if tree satisfies AVL invariant")
  (if (avl-empty? tree)
      #t
      (let ([bf (avl-balance-factor tree)])
        (and (>= bf -1)
             (<= bf 1)
             (avl-valid? (avl-left tree))
             (avl-valid? (avl-right tree))))))

(define (avl-bst-valid? tree)
  (doc 'export #t)
  (doc 'type '(-> AVL Boolean))
  (doc 'description "Check if tree satisfies BST property")
  (avl-bst-valid-range? tree #f #f))

(define (avl-bst-valid-range? tree lo hi)
  (if (avl-empty? tree)
      #t
      (let ([k (avl-key tree)])
        (and (or (not lo) (< lo k))
             (or (not hi) (< k hi))
             (avl-bst-valid-range? (avl-left tree) lo k)
             (avl-bst-valid-range? (avl-right tree) k hi)))))
