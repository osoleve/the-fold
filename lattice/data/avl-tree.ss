;;; lattice/data/avl-tree.ss — AVL Tree (Self-Balancing Binary Search Tree)
;;;
;;; Purely functional AVL tree with O(log n) insert, delete, and lookup.
;;; Maintains balance invariant: |height(left) - height(right)| <= 1
;;;
;;; AVL α = Empty | (Node height key value left right)
;;;
;;; Keys must be comparable with < (uses default ordering).
;;; For custom ordering, use avl-*-by functions with a comparator.
;;;
;;; TIER: 0 (no lattice dependencies)

;;;============================================================================
;;; Core Representation
;;;============================================================================

;;; avl-empty : AVL
;;; The empty AVL tree.
(define avl-empty 'avl-empty)

;;; avl-empty? : AVL → Boolean
;;; Check if tree is empty.
(define (avl-empty? tree)
  (eq? tree 'avl-empty))

;;; avl-node : Nat × κ × α × AVL × AVL → AVL
;;; Internal node constructor. Height is cached for O(1) access.
(define (avl-node height key value left right)
  (list 'avl-node height key value left right))

;;; avl-node? : Any → Boolean
(define (avl-node? x)
  (and (pair? x) (eq? (car x) 'avl-node)))

;;; Accessors
(define (avl-height tree)
  (if (avl-empty? tree) 0 (cadr tree)))

(define (avl-key tree)
  (caddr tree))

(define (avl-value tree)
  (cadddr tree))

(define (avl-left tree)
  (car (cddddr tree)))

(define (avl-right tree)
  (cadr (cddddr tree)))

;;;============================================================================
;;; Smart Constructor and Rotations
;;;============================================================================

;;; avl-balance-factor : AVL → Int
;;; Compute balance factor: height(left) - height(right).
;;; AVL invariant: -1 <= balance-factor <= 1
(define (avl-balance-factor tree)
  (if (avl-empty? tree)
      0
      (- (avl-height (avl-left tree))
         (avl-height (avl-right tree)))))

;;; make-avl-node : κ × α × AVL × AVL → AVL
;;; Smart constructor that recomputes height.
(define (make-avl-node key value left right)
  (avl-node (+ 1 (max (avl-height left) (avl-height right)))
            key value left right))

;;; rotate-left : AVL → AVL
;;; Left rotation for right-heavy trees.
;;;       x                 y
;;;      / \               / \
;;;     a   y     =>      x   c
;;;        / \           / \
;;;       b   c         a   b
(define (rotate-left tree)
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

;;; rotate-right : AVL → AVL
;;; Right rotation for left-heavy trees.
;;;         y               x
;;;        / \             / \
;;;       x   c    =>     a   y
;;;      / \                 / \
;;;     a   b               b   c
(define (rotate-right tree)
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

;;; rebalance : AVL → AVL
;;; Rebalance tree after insertion or deletion.
(define (rebalance tree)
  (let ([bf (avl-balance-factor tree)])
    (cond
      ;; Left-heavy
      [(> bf 1)
       (if (< (avl-balance-factor (avl-left tree)) 0)
           ;; Left-Right case: rotate left child left, then rotate right
           (rotate-right (make-avl-node (avl-key tree) (avl-value tree)
                                        (rotate-left (avl-left tree))
                                        (avl-right tree)))
           ;; Left-Left case: rotate right
           (rotate-right tree))]
      ;; Right-heavy
      [(< bf -1)
       (if (> (avl-balance-factor (avl-right tree)) 0)
           ;; Right-Left case: rotate right child right, then rotate left
           (rotate-left (make-avl-node (avl-key tree) (avl-value tree)
                                       (avl-left tree)
                                       (rotate-right (avl-right tree))))
           ;; Right-Right case: rotate left
           (rotate-left tree))]
      ;; Balanced
      [else tree])))

;;;============================================================================
;;; Basic Operations
;;;============================================================================

;;; avl-lookup : κ × AVL → α | #f
;;; Look up value by key. Returns #f if not found.
(define (avl-lookup key tree)
  (avl-lookup-by < key tree))

;;; avl-lookup-by : (κ × κ → Boolean) × κ × AVL → α | #f
;;; Look up with custom comparator.
(define (avl-lookup-by cmp key tree)
  (if (avl-empty? tree)
      #f
      (let ([k (avl-key tree)])
        (cond
          [(cmp key k) (avl-lookup-by cmp key (avl-left tree))]
          [(cmp k key) (avl-lookup-by cmp key (avl-right tree))]
          [else (avl-value tree)]))))

;;; avl-contains? : κ × AVL → Boolean
;;; Check if key exists in tree.
(define (avl-contains? key tree)
  (avl-contains-by? < key tree))

;;; avl-contains-by? : (κ × κ → Boolean) × κ × AVL → Boolean
(define (avl-contains-by? cmp key tree)
  (if (avl-empty? tree)
      #f
      (let ([k (avl-key tree)])
        (cond
          [(cmp key k) (avl-contains-by? cmp key (avl-left tree))]
          [(cmp k key) (avl-contains-by? cmp key (avl-right tree))]
          [else #t]))))

;;; avl-insert : κ × α × AVL → AVL
;;; Insert key-value pair. If key exists, updates value.
(define (avl-insert key value tree)
  (avl-insert-by < key value tree))

;;; avl-insert-by : (κ × κ → Boolean) × κ × α × AVL → AVL
;;; Insert with custom comparator.
(define (avl-insert-by cmp key value tree)
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
           ;; Key exists, update value
           (make-avl-node key value left right)]))))

;;;============================================================================
;;; Deletion
;;;============================================================================

;;; avl-min-node : AVL → (κ × α)
;;; Find minimum key-value pair.
(define (avl-min-node tree)
  (if (avl-empty? tree)
      (error 'avl-min-node "Cannot find min in empty tree")
      (if (avl-empty? (avl-left tree))
          (cons (avl-key tree) (avl-value tree))
          (avl-min-node (avl-left tree)))))

;;; avl-max-node : AVL → (κ × α)
;;; Find maximum key-value pair.
(define (avl-max-node tree)
  (if (avl-empty? tree)
      (error 'avl-max-node "Cannot find max in empty tree")
      (if (avl-empty? (avl-right tree))
          (cons (avl-key tree) (avl-value tree))
          (avl-max-node (avl-right tree)))))

;;; avl-delete-min : AVL → AVL
;;; Delete minimum element.
(define (avl-delete-min tree)
  (avl-delete-min-by < tree))

;;; avl-delete-min-by : (κ × κ → Boolean) × AVL → AVL
(define (avl-delete-min-by cmp tree)
  (if (avl-empty? tree)
      (error 'avl-delete-min "Cannot delete from empty tree")
      (if (avl-empty? (avl-left tree))
          (avl-right tree)
          (rebalance (make-avl-node (avl-key tree) (avl-value tree)
                                    (avl-delete-min-by cmp (avl-left tree))
                                    (avl-right tree))))))

;;; avl-delete : κ × AVL → AVL
;;; Delete key from tree. Returns unchanged tree if key not found.
(define (avl-delete key tree)
  (avl-delete-by < key tree))

;;; avl-delete-by : (κ × κ → Boolean) × κ × AVL → AVL
(define (avl-delete-by cmp key tree)
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
           ;; Found key to delete
           (cond
             [(avl-empty? left) right]
             [(avl-empty? right) left]
             [else
              ;; Replace with in-order successor (min of right subtree)
              (let ([succ (avl-min-node right)])
                (rebalance (make-avl-node (car succ) (cdr succ)
                                          left
                                          (avl-delete-min-by cmp right))))])]))))

;;;============================================================================
;;; Accessors
;;;============================================================================

;;; avl-min : AVL → κ
;;; Get minimum key.
(define (avl-min tree)
  (car (avl-min-node tree)))

;;; avl-max : AVL → κ
;;; Get maximum key.
(define (avl-max tree)
  (car (avl-max-node tree)))

;;; avl-min-value : AVL → α
;;; Get value associated with minimum key.
(define (avl-min-value tree)
  (cdr (avl-min-node tree)))

;;; avl-max-value : AVL → α
;;; Get value associated with maximum key.
(define (avl-max-value tree)
  (cdr (avl-max-node tree)))

;;; avl-size : AVL → Nat
;;; Get number of elements. O(n).
(define (avl-size tree)
  (if (avl-empty? tree)
      0
      (+ 1 (avl-size (avl-left tree)) (avl-size (avl-right tree)))))

;;;============================================================================
;;; Conversions
;;;============================================================================

;;; avl->list : AVL → (List (κ . α))
;;; Convert to sorted association list (in-order traversal).
(define (avl->list tree)
  (if (avl-empty? tree)
      '()
      (append (avl->list (avl-left tree))
              (list (cons (avl-key tree) (avl-value tree)))
              (avl->list (avl-right tree)))))

;;; avl-keys : AVL → (List κ)
;;; Get all keys in sorted order.
(define (avl-keys tree)
  (map car (avl->list tree)))

;;; avl-values : AVL → (List α)
;;; Get all values in key-sorted order.
(define (avl-values tree)
  (map cdr (avl->list tree)))

;;; list->avl : (List (κ . α)) → AVL
;;; Build tree from association list.
(define (list->avl lst)
  (fold-left (lambda (tree pair)
               (avl-insert (car pair) (cdr pair) tree))
             avl-empty
             lst))

;;; avl-from-keys : (List κ) → AVL
;;; Build tree from keys (values are the keys themselves).
(define (avl-from-keys keys)
  (fold-left (lambda (tree key)
               (avl-insert key key tree))
             avl-empty
             keys))

;;;============================================================================
;;; Higher-Order Operations
;;;============================================================================

;;; avl-fold : (β × κ × α → β) × β × AVL → β
;;; Fold over tree in key order (left-to-right).
(define (avl-fold f init tree)
  (if (avl-empty? tree)
      init
      (avl-fold f
                (f (avl-fold f init (avl-left tree))
                   (avl-key tree)
                   (avl-value tree))
                (avl-right tree))))

;;; avl-fold-right : (κ × α × β → β) × β × AVL → β
;;; Fold over tree in reverse key order (right-to-left).
(define (avl-fold-right f init tree)
  (if (avl-empty? tree)
      init
      (avl-fold-right f
                      (f (avl-key tree)
                         (avl-value tree)
                         (avl-fold-right f init (avl-right tree)))
                      (avl-left tree))))

;;; avl-map : (α → β) × AVL κ α → AVL κ β
;;; Map function over values, preserving tree structure.
(define (avl-map f tree)
  (if (avl-empty? tree)
      avl-empty
      (make-avl-node (avl-key tree)
                     (f (avl-value tree))
                     (avl-map f (avl-left tree))
                     (avl-map f (avl-right tree)))))

;;; avl-filter : ((κ × α) → Boolean) × AVL → AVL
;;; Filter tree by predicate on key-value pairs.
(define (avl-filter pred tree)
  (avl-fold (lambda (acc k v)
              (if (pred k v)
                  (avl-insert k v acc)
                  acc))
            avl-empty
            tree))

;;;============================================================================
;;; Range Queries
;;;============================================================================

;;; avl-range : κ × κ × AVL → (List (κ . α))
;;; Get all key-value pairs where lo <= key <= hi.
(define (avl-range lo hi tree)
  (avl-range-by < lo hi tree))

;;; avl-range-by : (κ × κ → Boolean) × κ × κ × AVL → (List (κ . α))
(define (avl-range-by cmp lo hi tree)
  (if (avl-empty? tree)
      '()
      (let ([k (avl-key tree)]
            [v (avl-value tree)])
        (append
         ;; Left subtree (only if lo < k)
         (if (cmp lo k)
             (avl-range-by cmp lo hi (avl-left tree))
             '())
         ;; Current node (if lo <= k <= hi)
         (if (and (not (cmp k lo)) (not (cmp hi k)))
             (list (cons k v))
             '())
         ;; Right subtree (only if k < hi)
         (if (cmp k hi)
             (avl-range-by cmp lo hi (avl-right tree))
             '())))))

;;; avl-keys-between : κ × κ × AVL → (List κ)
;;; Get all keys in range [lo, hi].
(define (avl-keys-between lo hi tree)
  (map car (avl-range lo hi tree)))

;;; avl-less-than : κ × AVL → (List (κ . α))
;;; Get all pairs with key < bound.
(define (avl-less-than bound tree)
  (if (avl-empty? tree)
      '()
      (let ([k (avl-key tree)])
        (if (< k bound)
            (append (avl-less-than bound (avl-left tree))
                    (list (cons k (avl-value tree)))
                    (avl-less-than bound (avl-right tree)))
            (avl-less-than bound (avl-left tree))))))

;;; avl-greater-than : κ × AVL → (List (κ . α))
;;; Get all pairs with key > bound.
(define (avl-greater-than bound tree)
  (if (avl-empty? tree)
      '()
      (let ([k (avl-key tree)])
        (if (> k bound)
            (append (avl-greater-than bound (avl-left tree))
                    (list (cons k (avl-value tree)))
                    (avl-greater-than bound (avl-right tree)))
            (avl-greater-than bound (avl-right tree))))))

;;;============================================================================
;;; Set Operations (key-only, value = key)
;;;============================================================================

;;; avl-set-insert : κ × AVL → AVL
;;; Insert key as a set element (value = key).
(define (avl-set-insert key tree)
  (avl-insert key key tree))

;;; avl-set-delete : κ × AVL → AVL
;;; Delete key from set.
(define avl-set-delete avl-delete)

;;; avl-set-member? : κ × AVL → Boolean
;;; Check if key is in set.
(define avl-set-member? avl-contains?)

;;; avl-set-union : AVL × AVL → AVL
;;; Union of two sets.
(define (avl-set-union t1 t2)
  (avl-fold (lambda (acc k v) (avl-set-insert k acc)) t1 t2))

;;; avl-set-intersection : AVL × AVL → AVL
;;; Intersection of two sets.
(define (avl-set-intersection t1 t2)
  (avl-filter (lambda (k v) (avl-contains? k t2)) t1))

;;; avl-set-difference : AVL × AVL → AVL
;;; Set difference (t1 - t2).
(define (avl-set-difference t1 t2)
  (avl-filter (lambda (k v) (not (avl-contains? k t2))) t1))

;;;============================================================================
;;; Verification (for testing)
;;;============================================================================

;;; avl-valid? : AVL → Boolean
;;; Check if tree satisfies AVL invariant.
(define (avl-valid? tree)
  (if (avl-empty? tree)
      #t
      (let ([bf (avl-balance-factor tree)])
        (and (>= bf -1)
             (<= bf 1)
             (avl-valid? (avl-left tree))
             (avl-valid? (avl-right tree))))))

;;; avl-bst-valid? : AVL → Boolean
;;; Check if tree satisfies BST property.
(define (avl-bst-valid? tree)
  (avl-bst-valid-range? tree #f #f))

(define (avl-bst-valid-range? tree lo hi)
  (if (avl-empty? tree)
      #t
      (let ([k (avl-key tree)])
        (and (or (not lo) (< lo k))
             (or (not hi) (< k hi))
             (avl-bst-valid-range? (avl-left tree) lo k)
             (avl-bst-valid-range? (avl-right tree) k hi)))))
