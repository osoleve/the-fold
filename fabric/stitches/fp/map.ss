;;; fabric/stitches/fp/map.ss — Functional Map (Dictionary) Library
;;;
;;; A purely functional map/dictionary implementation using AVL trees.
;;; Provides efficient key-value operations:
;;; - O(log n) insert, delete, lookup
;;; - O(n) map, filter, fold operations
;;; - Merge and set-like operations on maps
;;;
;;; This module builds on prelude.ss and combinators.ss.

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; AVL Tree Type (Internal)
;;; ============================================================

;;; AVL tree: empty | (avl height key value left right)

(define avl-empty '(avl-empty))

(define (avl-empty? t)
  (and (pair? t) (eq? (car t) 'avl-empty)))

(define (make-avl height key value left right)
  (list 'avl height key value left right))

(define (avl-node? t)
  (and (pair? t) (eq? (car t) 'avl)))

(define (avl-height t)
  (if (avl-empty? t) 0 (cadr t)))

(define (avl-key t)
  (caddr t))

(define (avl-value t)
  (cadddr t))

(define (avl-left t)
  (list-ref t 4))

(define (avl-right t)
  (list-ref t 5))

;;; ============================================================
;;; AVL Balancing
;;; ============================================================

(define (avl-balance-factor t)
  (if (avl-empty? t)
      0
      (- (avl-height (avl-left t)) (avl-height (avl-right t)))))

(define (avl-new-node key value left right)
  (make-avl (+ 1 (max (avl-height left) (avl-height right)))
            key value left right))

;;; Rotate right
(define (avl-rotate-right t)
  (let ([l (avl-left t)])
       (avl-new-node (avl-key l) (avl-value l)
                     (avl-left l)
                     (avl-new-node (avl-key t) (avl-value t)
                                   (avl-right l)
                                   (avl-right t)))))

;;; Rotate left
(define (avl-rotate-left t)
  (let ([r (avl-right t)])
       (avl-new-node (avl-key r) (avl-value r)
                     (avl-new-node (avl-key t) (avl-value t)
                                   (avl-left t)
                                   (avl-left r))
                     (avl-right r))))

;;; Balance tree
(define (avl-balance t)
  (let ([bf (avl-balance-factor t)])
       (cond
        ;; Left heavy
        [(> bf 1)
         (if (< (avl-balance-factor (avl-left t)) 0)
             ;; Left-Right case
             (avl-rotate-right (avl-new-node (avl-key t) (avl-value t)
                                             (avl-rotate-left (avl-left t))
                                             (avl-right t)))
             ;; Left-Left case
             (avl-rotate-right t))]
        ;; Right heavy
        [(< bf -1)
         (if (> (avl-balance-factor (avl-right t)) 0)
             ;; Right-Left case
             (avl-rotate-left (avl-new-node (avl-key t) (avl-value t)
                                            (avl-left t)
                                            (avl-rotate-right (avl-right t))))
             ;; Right-Right case
             (avl-rotate-left t))]
        [else t])))

;;; ============================================================
;;; Map Type (Wrapper around AVL tree)
;;; ============================================================

(define (make-map tree cmp)
  (list 'map tree cmp))

(define (map? x)
  (and (pair? x) (eq? (car x) 'map)))

(define (map-tree m)
  (cadr m))

(define (map-cmp m)
  (caddr m))

;;; ============================================================
;;; Map Constructors
;;; ============================================================

;;; Empty map with default key comparison
(define (map-empty)
  (make-map avl-empty default-key-cmp))

;;; Empty map with custom key comparison
(define (map-empty-with cmp)
  (make-map avl-empty cmp))

;;; Default key comparison function
(define (default-key-cmp a b)
  (cond
   [(and (number? a) (number? b))
    (cond [(< a b) -1] [(> a b) 1] [else 0])]
   [(and (string? a) (string? b))
    (cond [(string<? a b) -1] [(string>? a b) 1] [else 0])]
   [(and (symbol? a) (symbol? b))
    (let ([sa (symbol->string a)]
          [sb (symbol->string b)])
         (cond [(string<? sa sb) -1] [(string>? sa sb) 1] [else 0]))]
   [(and (char? a) (char? b))
    (cond [(char<? a b) -1] [(char>? a b) 1] [else 0])]
   [else 0]))

;;; Create singleton map
(define (map-singleton key value)
  (make-map (make-avl 1 key value avl-empty avl-empty) default-key-cmp))

;;; Create map from alist ((key . value) ...)
(define (alist->map alist)
  (fold-left (lambda (m pair) (map-insert m (car pair) (cdr pair)))
             (map-empty)
             alist))

;;; Create map from alist with custom comparator
(define (alist->map-with cmp alist)
  (fold-left (lambda (m pair) (map-insert m (car pair) (cdr pair)))
             (map-empty-with cmp)
             alist))

;;; ============================================================
;;; Core Operations
;;; ============================================================

;;; Insert or update key-value pair - O(log n)
(define (map-insert m key value)
  (let ([cmp (map-cmp m)])
       (make-map (avl-insert (map-tree m) key value cmp) cmp)))

(define (avl-insert t key value cmp)
  (cond
   [(avl-empty? t) (make-avl 1 key value avl-empty avl-empty)]
   [else
    (let ([c (cmp key (avl-key t))])
         (cond
          [(= c 0) (avl-new-node key value (avl-left t) (avl-right t))]  ; Update
          [(< c 0)
           (avl-balance (avl-new-node (avl-key t) (avl-value t)
                                      (avl-insert (avl-left t) key value cmp)
                                      (avl-right t)))]
          [else
           (avl-balance (avl-new-node (avl-key t) (avl-value t)
                                      (avl-left t)
                                      (avl-insert (avl-right t) key value cmp)))]))]))

;;; Delete key - O(log n)
(define (map-delete m key)
  (let ([cmp (map-cmp m)])
       (make-map (avl-delete (map-tree m) key cmp) cmp)))

(define (avl-delete t key cmp)
  (if (avl-empty? t)
      t
      (let ([c (cmp key (avl-key t))])
           (cond
            [(< c 0)
             (avl-balance (avl-new-node (avl-key t) (avl-value t)
                                        (avl-delete (avl-left t) key cmp)
                                        (avl-right t)))]
            [(> c 0)
             (avl-balance (avl-new-node (avl-key t) (avl-value t)
                                        (avl-left t)
                                        (avl-delete (avl-right t) key cmp)))]
            [else
             ;; Found it
             (cond
              [(avl-empty? (avl-left t)) (avl-right t)]
              [(avl-empty? (avl-right t)) (avl-left t)]
              [else
               ;; Two children - replace with minimum of right subtree
               (let* ([min-node (avl-min-node (avl-right t))]
                      [min-key (avl-key min-node)]
                      [min-val (avl-value min-node)])
                     (avl-balance (avl-new-node min-key min-val
                                                (avl-left t)
                                                (avl-delete (avl-right t) min-key cmp))))])]))))

;;; Find minimum node
(define (avl-min-node t)
  (if (avl-empty? (avl-left t))
      t
      (avl-min-node (avl-left t))))

;;; Lookup key - O(log n)
(define (map-lookup m key)
  (avl-lookup (map-tree m) key (map-cmp m)))

(define (avl-lookup t key cmp)
  (if (avl-empty? t)
      nothing
      (let ([c (cmp key (avl-key t))])
           (cond
            [(= c 0) (just (avl-value t))]
            [(< c 0) (avl-lookup (avl-left t) key cmp)]
            [else (avl-lookup (avl-right t) key cmp)]))))

;;; Check if key exists - O(log n)
(define (map-contains? m key)
  (just? (map-lookup m key)))

;;; Get value with default
(define (map-get m key default)
  (let ([result (map-lookup m key)])
       (if (just? result)
           (from-just result)
           default)))

;;; ============================================================
;;; Map Properties
;;; ============================================================

;;; Check if empty
(define (map-empty? m)
  (avl-empty? (map-tree m)))

;;; Get size - O(n)
(define (map-size m)
  (avl-size (map-tree m)))

(define (avl-size t)
  (if (avl-empty? t)
      0
      (+ 1 (avl-size (avl-left t)) (avl-size (avl-right t)))))

;;; ============================================================
;;; Conversions
;;; ============================================================

;;; Convert to alist (sorted by key)
(define (map->alist m)
  (avl->alist (map-tree m)))

(define (avl->alist t)
  (if (avl-empty? t)
      '()
      (append (avl->alist (avl-left t))
              (cons (cons (avl-key t) (avl-value t))
                    (avl->alist (avl-right t))))))

;;; Get all keys (sorted)
(define (map-keys m)
  (map car (map->alist m)))

;;; Get all values (in key order)
(define (map-values m)
  (map cdr (map->alist m)))

;;; ============================================================
;;; Transformations
;;; ============================================================

;;; Map function over values
(define (map-map f m)
  (alist->map-with (map-cmp m)
                   (map (lambda (pair) (cons (car pair) (f (cdr pair))))
                        (map->alist m))))

;;; Map function over key-value pairs
(define (map-map-kv f m)
  (alist->map-with (map-cmp m)
                   (map (lambda (pair) (cons (car pair) (f (car pair) (cdr pair))))
                        (map->alist m))))

;;; Filter by predicate on values
(define (map-filter pred m)
  (alist->map-with (map-cmp m)
                   (filter (lambda (pair) (pred (cdr pair)))
                           (map->alist m))))

;;; Filter by predicate on key-value pairs
(define (map-filter-kv pred m)
  (alist->map-with (map-cmp m)
                   (filter (lambda (pair) (pred (car pair) (cdr pair)))
                           (map->alist m))))

;;; Partition by predicate
(define (map-partition pred m)
  (let ([alist (map->alist m)])
       (cons (alist->map-with (map-cmp m) (filter (lambda (p) (pred (cdr p))) alist))
             (alist->map-with (map-cmp m) (filter (lambda (p) (not (pred (cdr p)))) alist)))))

;;; Fold over key-value pairs
(define (map-fold f init m)
  (fold-left (lambda (acc pair) (f acc (car pair) (cdr pair)))
             init
             (map->alist m)))

;;; ============================================================
;;; Merge Operations
;;; ============================================================

;;; Merge two maps (right map wins on conflict)
(define (map-merge m1 m2)
  (fold-left (lambda (m pair) (map-insert m (car pair) (cdr pair)))
             m1
             (map->alist m2)))

;;; Merge with combining function for conflicts
(define (map-merge-with f m1 m2)
  (fold-left (lambda (m pair)
                     (let* ([key (car pair)]
                            [val2 (cdr pair)]
                            [existing (map-lookup m key)])
                           (if (just? existing)
                               (map-insert m key (f (from-just existing) val2))
                               (map-insert m key val2))))
             m1
             (map->alist m2)))

;;; Union (alias for merge)
(define map-union map-merge)

;;; Intersection (keep keys present in both)
(define (map-intersection m1 m2)
  (map-filter-kv (lambda (k v) (map-contains? m2 k)) m1))

;;; Difference (keys in m1 but not in m2)
(define (map-difference m1 m2)
  (map-filter-kv (lambda (k v) (not (map-contains? m2 k))) m1))

;;; ============================================================
;;; Update Operations
;;; ============================================================

;;; Update value at key using function
(define (map-update m key f)
  (let ([existing (map-lookup m key)])
       (if (just? existing)
           (map-insert m key (f (from-just existing)))
           m)))

;;; Update with default if key doesn't exist
(define (map-update-default m key default f)
  (let ([existing (map-lookup m key)])
       (if (just? existing)
           (map-insert m key (f (from-just existing)))
           (map-insert m key (f default)))))

;;; Insert if absent
(define (map-insert-if-absent m key value)
  (if (map-contains? m key)
      m
      (map-insert m key value)))

;;; ============================================================
;;; Min/Max Operations
;;; ============================================================

;;; Get minimum key-value pair
(define (map-min m)
  (if (map-empty? m)
      nothing
      (let ([node (avl-min-node (map-tree m))])
           (just (cons (avl-key node) (avl-value node))))))

;;; Get maximum key-value pair
(define (map-max m)
  (if (map-empty? m)
      nothing
      (let ([node (avl-max-node (map-tree m))])
           (just (cons (avl-key node) (avl-value node))))))

(define (avl-max-node t)
  (if (avl-empty? (avl-right t))
      t
      (avl-max-node (avl-right t))))

;;; Delete minimum
(define (map-delete-min m)
  (let ([min-pair (map-min m)])
       (if (nothing? min-pair)
           m
           (map-delete m (car (from-just min-pair))))))

;;; Delete maximum
(define (map-delete-max m)
  (let ([max-pair (map-max m)])
       (if (nothing? max-pair)
           m
           (map-delete m (car (from-just max-pair))))))

;;; ============================================================
;;; Quantifiers
;;; ============================================================

;;; Check if any key-value satisfies predicate
(define (map-any? pred m)
  (fold-left (lambda (acc pair) (or acc (pred (car pair) (cdr pair))))
             #f
             (map->alist m)))

;;; Check if all key-values satisfy predicate
(define (map-all? pred m)
  (fold-left (lambda (acc pair) (and acc (pred (car pair) (cdr pair))))
             #t
             (map->alist m)))

;;; Find first key-value satisfying predicate
(define (map-find pred m)
  (let loop ([alist (map->alist m)])
       (cond
        [(null? alist) nothing]
        [(pred (caar alist) (cdar alist)) (just (car alist))]
        [else (loop (cdr alist))])))

;;; ============================================================
;;; Inversion
;;; ============================================================

;;; Invert map (swap keys and values)
;;; Note: may lose data if values are not unique
(define (map-invert m)
  (alist->map (map (lambda (pair) (cons (cdr pair) (car pair)))
                   (map->alist m))))

;;; ============================================================
;;; Grouping
;;; ============================================================

;;; Group list by key function
(define (group-by key-fn lst)
  (fold-left (lambda (m x)
                     (let* ([key (key-fn x)]
                            [existing (map-get m key '())])
                           (map-insert m key (cons x existing))))
             (map-empty)
             lst))

;;; Count occurrences
(define (frequencies lst)
  (fold-left (lambda (m x)
                     (map-update-default m x 0 (lambda (n) (+ n 1))))
             (map-empty)
             lst))

;;; ============================================================
;;; Display
;;; ============================================================

(define (map->string m)
  (let ([alist (map->alist m)])
       (string-append "{"
                      (fold-left (lambda (acc pair)
                                         (let ([entry (format "~a: ~a" (car pair) (cdr pair))])
                                              (if (string=? acc "")
                                                  entry
                                                  (string-append acc ", " entry))))
                                 ""
                                 alist)
                      "}")))

;;; ============================================================
;;; Comparison
;;; ============================================================

(define (map-equal? m1 m2)
  (equal? (map->alist m1) (map->alist m2)))

;;; ============================================================
;;; Monoid Operations
;;; ============================================================

(define (map-mempty) (map-empty))
(define map-mappend map-merge)

(define (map-mconcat maps)
  (fold-left map-merge (map-empty) maps))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; Constructors:
;;;   map-empty, map-empty-with, map-singleton, alist->map, alist->map-with
;;;
;;; Core:
;;;   map-insert, map-delete, map-lookup, map-contains?, map-get
;;;
;;; Properties:
;;;   map?, map-empty?, map-size
;;;
;;; Conversions:
;;;   map->alist, map-keys, map-values
;;;
;;; Transformations:
;;;   map-map, map-map-kv, map-filter, map-filter-kv, map-partition, map-fold
;;;
;;; Merge operations:
;;;   map-merge, map-merge-with, map-union, map-intersection, map-difference
;;;
;;; Update:
;;;   map-update, map-update-default, map-insert-if-absent
;;;
;;; Min/Max:
;;;   map-min, map-max, map-delete-min, map-delete-max
;;;
;;; Quantifiers:
;;;   map-any?, map-all?, map-find
;;;
;;; Advanced:
;;;   map-invert, group-by, frequencies
;;;
;;; Comparison:
;;;   map-equal?, map->string
;;;
;;; Monoid:
;;;   map-mempty, map-mappend, map-mconcat
