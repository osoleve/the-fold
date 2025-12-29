;;; fabric/stitches/fp/set.ss — Functional Set Library
;;;
;;; A purely functional set implementation using balanced trees (AVL).
;;; Provides efficient set operations:
;;; - O(log n) insert, delete, membership
;;; - O(n) union, intersection, difference
;;; - Set comprehensions and filtering
;;;
;;; This module builds on prelude.ss and combinators.ss.

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; AVL Tree Type (Internal)
;;; ============================================================

;;; AVL tree: empty | (avl height value left right)

(define avl-empty '(avl-empty))

(define (avl-empty? t)
  (and (pair? t) (eq? (car t) 'avl-empty)))

(define (make-avl height value left right)
  (list 'avl height value left right))

(define (avl-node? t)
  (and (pair? t) (eq? (car t) 'avl)))

(define (avl-height t)
  (if (avl-empty? t) 0 (cadr t)))

(define (avl-value t)
  (caddr t))

(define (avl-left t)
  (cadddr t))

(define (avl-right t)
  (car (cddddr t)))

;;; ============================================================
;;; AVL Balancing
;;; ============================================================

(define (avl-balance-factor t)
  (if (avl-empty? t)
      0
      (- (avl-height (avl-left t)) (avl-height (avl-right t)))))

(define (avl-new-node value left right)
  (make-avl (+ 1 (max (avl-height left) (avl-height right)))
            value left right))

;;; Rotate right
(define (avl-rotate-right t)
  (let ([l (avl-left t)])
       (avl-new-node (avl-value l)
                     (avl-left l)
                     (avl-new-node (avl-value t)
                                   (avl-right l)
                                   (avl-right t)))))

;;; Rotate left
(define (avl-rotate-left t)
  (let ([r (avl-right t)])
       (avl-new-node (avl-value r)
                     (avl-new-node (avl-value t)
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
             (avl-rotate-right (avl-new-node (avl-value t)
                                             (avl-rotate-left (avl-left t))
                                             (avl-right t)))
             ;; Left-Left case
             (avl-rotate-right t))]
        ;; Right heavy
        [(< bf -1)
         (if (> (avl-balance-factor (avl-right t)) 0)
             ;; Right-Left case
             (avl-rotate-left (avl-new-node (avl-value t)
                                            (avl-left t)
                                            (avl-rotate-right (avl-right t))))
             ;; Right-Right case
             (avl-rotate-left t))]
        [else t])))

;;; ============================================================
;;; Set Type (Wrapper around AVL tree)
;;; ============================================================

(define (make-set tree cmp)
  (list 'set tree cmp))

(define (set? x)
  (and (pair? x) (eq? (car x) 'set)))

(define (set-tree s)
  (cadr s))

(define (set-cmp s)
  (caddr s))

;;; ============================================================
;;; Set Constructors
;;; ============================================================

;;; Empty set with default comparison
(define (set-empty)
  (make-set avl-empty default-cmp))

;;; Empty set with custom comparison
(define (set-empty-with cmp)
  (make-set avl-empty cmp))

;;; Default comparison function
(define (default-cmp a b)
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
   [else 0]))  ; Fall back to equal

;;; Create singleton set
(define (set-singleton x)
  (make-set (make-avl 1 x avl-empty avl-empty) default-cmp))

;;; Create set from list
(define (list->set lst)
  (fold-left set-insert (set-empty) lst))

;;; Create set from list with custom comparator
(define (list->set-with cmp lst)
  (fold-left set-insert (set-empty-with cmp) lst))

;;; ============================================================
;;; Core Operations
;;; ============================================================

;;; Insert element - O(log n)
(define (set-insert s x)
  (let ([cmp (set-cmp s)])
       (make-set (avl-insert (set-tree s) x cmp) cmp)))

(define (avl-insert t x cmp)
  (cond
   [(avl-empty? t) (make-avl 1 x avl-empty avl-empty)]
   [else
    (let ([c (cmp x (avl-value t))])
         (cond
          [(= c 0) t]  ; Already exists
          [(< c 0)
           (avl-balance (avl-new-node (avl-value t)
                                      (avl-insert (avl-left t) x cmp)
                                      (avl-right t)))]
          [else
           (avl-balance (avl-new-node (avl-value t)
                                      (avl-left t)
                                      (avl-insert (avl-right t) x cmp)))]))]))

;;; Delete element - O(log n)
(define (set-delete s x)
  (let ([cmp (set-cmp s)])
       (make-set (avl-delete (set-tree s) x cmp) cmp)))

(define (avl-delete t x cmp)
  (if (avl-empty? t)
      t
      (let ([c (cmp x (avl-value t))])
           (cond
            [(< c 0)
             (avl-balance (avl-new-node (avl-value t)
                                        (avl-delete (avl-left t) x cmp)
                                        (avl-right t)))]
            [(> c 0)
             (avl-balance (avl-new-node (avl-value t)
                                        (avl-left t)
                                        (avl-delete (avl-right t) x cmp)))]
            [else
             ;; Found it
             (cond
              [(avl-empty? (avl-left t)) (avl-right t)]
              [(avl-empty? (avl-right t)) (avl-left t)]
              [else
               ;; Two children - replace with minimum of right subtree
               (let ([min-val (avl-min (avl-right t))])
                    (avl-balance (avl-new-node min-val
                                               (avl-left t)
                                               (avl-delete (avl-right t) min-val cmp))))])]))))

;;; Find minimum
(define (avl-min t)
  (if (avl-empty? (avl-left t))
      (avl-value t)
      (avl-min (avl-left t))))

;;; Check membership - O(log n)
(define (set-member? s x)
  (avl-member? (set-tree s) x (set-cmp s)))

(define (avl-member? t x cmp)
  (if (avl-empty? t)
      #f
      (let ([c (cmp x (avl-value t))])
           (cond
            [(= c 0) #t]
            [(< c 0) (avl-member? (avl-left t) x cmp)]
            [else (avl-member? (avl-right t) x cmp)]))))

;;; ============================================================
;;; Set Properties
;;; ============================================================

;;; Check if empty
(define (set-empty? s)
  (avl-empty? (set-tree s)))

;;; Get size - O(n)
(define (set-size s)
  (avl-size (set-tree s)))

(define (avl-size t)
  (if (avl-empty? t)
      0
      (+ 1 (avl-size (avl-left t)) (avl-size (avl-right t)))))

;;; ============================================================
;;; Set Operations
;;; ============================================================

;;; Union of two sets - O(n + m)
(define (set-union s1 s2)
  (fold-left set-insert s1 (set->list s2)))

;;; Intersection of two sets - O(n log m)
(define (set-intersection s1 s2)
  (list->set-with (set-cmp s1)
                  (filter (lambda (x) (set-member? s2 x))
                          (set->list s1))))

;;; Difference (s1 - s2) - O(n log m)
(define (set-difference s1 s2)
  (list->set-with (set-cmp s1)
                  (filter (lambda (x) (not (set-member? s2 x)))
                          (set->list s1))))

;;; Symmetric difference - O(n + m)
(define (set-symmetric-difference s1 s2)
  (set-union (set-difference s1 s2) (set-difference s2 s1)))

;;; Check if subset
(define (set-subset? s1 s2)
  (fold-left (lambda (acc x) (and acc (set-member? s2 x)))
             #t
             (set->list s1)))

;;; Check if proper subset
(define (set-proper-subset? s1 s2)
  (and (set-subset? s1 s2)
       (< (set-size s1) (set-size s2))))

;;; Check if sets are equal
(define (set-equal? s1 s2)
  (and (= (set-size s1) (set-size s2))
       (set-subset? s1 s2)))

;;; Check if disjoint
(define (set-disjoint? s1 s2)
  (set-empty? (set-intersection s1 s2)))

;;; ============================================================
;;; Conversions
;;; ============================================================

;;; Convert to sorted list
(define (set->list s)
  (avl->list (set-tree s)))

(define (avl->list t)
  (if (avl-empty? t)
      '()
      (append (avl->list (avl-left t))
              (cons (avl-value t) (avl->list (avl-right t))))))

;;; ============================================================
;;; Transformations
;;; ============================================================

;;; Map function over set (may reduce size if f is not injective)
(define (set-map f s)
  (list->set-with (set-cmp s) (map f (set->list s))))

;;; Filter set by predicate
(define (set-filter pred s)
  (list->set-with (set-cmp s) (filter pred (set->list s))))

;;; Partition set
(define (set-partition pred s)
  (let ([lst (set->list s)])
       (cons (list->set-with (set-cmp s) (filter pred lst))
             (list->set-with (set-cmp s) (filter (lambda (x) (not (pred x))) lst)))))

;;; Fold over set (in sorted order)
(define (set-fold f init s)
  (fold-left f init (set->list s)))

;;; ============================================================
;;; Min/Max Operations
;;; ============================================================

;;; Get minimum element
(define (set-min s)
  (if (set-empty? s)
      nothing
      (just (avl-min (set-tree s)))))

;;; Get maximum element
(define (set-max s)
  (if (set-empty? s)
      nothing
      (just (avl-max (set-tree s)))))

(define (avl-max t)
  (if (avl-empty? (avl-right t))
      (avl-value t)
      (avl-max (avl-right t))))

;;; Delete minimum
(define (set-delete-min s)
  (if (set-empty? s)
      s
      (let* ([min-val (from-just (set-min s))]
             [new-set (set-delete s min-val)])
            new-set)))

;;; Delete maximum
(define (set-delete-max s)
  (if (set-empty? s)
      s
      (let* ([max-val (from-just (set-max s))]
             [new-set (set-delete s max-val)])
            new-set)))

;;; ============================================================
;;; Quantifiers
;;; ============================================================

;;; Check if any element satisfies predicate
(define (set-any? pred s)
  (fold-left (lambda (acc x) (or acc (pred x)))
             #f
             (set->list s)))

;;; Check if all elements satisfy predicate
(define (set-all? pred s)
  (fold-left (lambda (acc x) (and acc (pred x)))
             #t
             (set->list s)))

;;; Find first element satisfying predicate
(define (set-find pred s)
  (let loop ([lst (set->list s)])
       (cond
        [(null? lst) nothing]
        [(pred (car lst)) (just (car lst))]
        [else (loop (cdr lst))])))

;;; ============================================================
;;; Display
;;; ============================================================

(define (set->string s)
  (let ([lst (set->list s)])
       (string-append "{"
                      (fold-left (lambda (acc x)
                                         (if (string=? acc "")
                                             (format "~a" x)
                                             (string-append acc ", " (format "~a" x))))
                                 ""
                                 lst)
                      "}")))

;;; ============================================================
;;; Set Builders
;;; ============================================================

;;; Build set from range [start, end)
(define (set-range start end)
  (if (>= start end)
      (set-empty)
      (set-insert (set-range (+ start 1) end) start)))

;;; Build set from generator (takes n elements from generator)
(define (set-generate n gen)
  (let loop ([i 0] [s (set-empty)])
       (if (>= i n)
           s
           (loop (+ i 1) (set-insert s (gen i))))))

;;; ============================================================
;;; Power Set
;;; ============================================================

;;; Generate power set (all subsets) - O(2^n)
(define (set-power-set s)
  (let ([lst (set->list s)])
       (map (lambda (sub) (list->set-with (set-cmp s) sub))
            (power-list lst))))

(define (power-list lst)
  (if (null? lst)
      '(())
      (let ([rest (power-list (cdr lst))])
           (append rest
                   (map (lambda (sub) (cons (car lst) sub)) rest)))))

;;; ============================================================
;;; Cartesian Product
;;; ============================================================

;;; Comparator for pairs
(define (pair-cmp a b)
  (let ([c1 (default-cmp (car a) (car b))])
       (if (= c1 0)
           (default-cmp (cdr a) (cdr b))
           c1)))

;;; Cartesian product of two sets
(define (set-product s1 s2)
  (let ([lst1 (set->list s1)]
        [lst2 (set->list s2)])
       (list->set-with pair-cmp
                       (apply append
                              (map (lambda (x)
                                           (map (lambda (y) (cons x y)) lst2))
                                   lst1)))))

;;; ============================================================
;;; Monoid Operations
;;; ============================================================

(define (set-mempty) (set-empty))
(define set-mappend set-union)

(define (set-mconcat sets)
  (fold-left set-union (set-empty) sets))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; Constructors:
;;;   set-empty, set-empty-with, set-singleton, list->set, list->set-with
;;;
;;; Core:
;;;   set-insert, set-delete, set-member?
;;;
;;; Properties:
;;;   set?, set-empty?, set-size
;;;
;;; Set operations:
;;;   set-union, set-intersection, set-difference,
;;;   set-symmetric-difference
;;;
;;; Comparisons:
;;;   set-subset?, set-proper-subset?, set-equal?, set-disjoint?
;;;
;;; Conversions:
;;;   set->list, set->string
;;;
;;; Transformations:
;;;   set-map, set-filter, set-partition, set-fold
;;;
;;; Min/Max:
;;;   set-min, set-max, set-delete-min, set-delete-max
;;;
;;; Quantifiers:
;;;   set-any?, set-all?, set-find
;;;
;;; Builders:
;;;   set-range, set-generate
;;;
;;; Advanced:
;;;   set-power-set, set-product
;;;
;;; Monoid:
;;;   set-mempty, set-mappend, set-mconcat
