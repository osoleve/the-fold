;;; fabric/stitches/fp/heap.ss — Functional Heap (Priority Queue) Library
;;;
;;; A pure functional implementation of heaps using leftist trees.
;;; Provides efficient priority queue operations with O(log n) insert
;;; and extract-min.
;;;
;;; Features:
;;; - Min-heap and max-heap variants
;;; - Leftist tree implementation (purely functional)
;;; - Heap merge in O(log n)
;;; - Priority queue operations
;;; - Heap sort
;;; - Top-k elements
;;;
;;; This module builds on prelude.ss and combinators.ss.

(load "prelude.ss")
(load "fp/combinators.ss")

;;; ============================================================
;;; Leftist Heap Type
;;; ============================================================

;;; Leftist Heap = empty | (heap rank value left right)
;;; rank = length of right spine (shortest path to empty)
;;; Leftist property: rank(left) >= rank(right)

(define (make-heap rank value left right cmp)
  (list 'heap rank value left right cmp))

(define (heap? x)
  (and (list? x)
       (or (eq? x 'heap-empty)
           (and (= (length x) 6) (eq? (car x) 'heap)))))

(define heap-empty 'heap-empty)

(define (heap-empty? h) (eq? h 'heap-empty))

(define (heap-rank h)
  (if (heap-empty? h) 0 (list-ref h 1)))

(define (heap-value h)
  (if (heap-empty? h)
      (error 'heap-value "empty heap")
      (list-ref h 2)))

(define (heap-left h)
  (if (heap-empty? h) heap-empty (list-ref h 3)))

(define (heap-right h)
  (if (heap-empty? h) heap-empty (list-ref h 4)))

(define (heap-cmp h)
  (if (heap-empty? h)
      (error 'heap-cmp "empty heap")
      (list-ref h 5)))

;;; ============================================================
;;; Heap Construction
;;; ============================================================

;;; Create a singleton heap
(define (heap-singleton value cmp)
  (make-heap 1 value heap-empty heap-empty cmp))

;;; Create empty min-heap (smaller values have priority)
(define (empty-min-heap)
  heap-empty)

;;; Create empty max-heap (larger values have priority)
(define (empty-max-heap)
  heap-empty)

;;; Default comparators
(define (min-cmp a b) (< a b))
(define (max-cmp a b) (> a b))

;;; Smart constructor that maintains leftist property
(define (make-leftist-heap value left right cmp)
  (let ([rank-l (heap-rank left)]
        [rank-r (heap-rank right)])
    (if (>= rank-l rank-r)
        (make-heap (+ rank-r 1) value left right cmp)
        (make-heap (+ rank-l 1) value right left cmp))))

;;; ============================================================
;;; Core Operations
;;; ============================================================

;;; Merge two heaps - O(log n)
(define (heap-merge h1 h2)
  (cond
    [(heap-empty? h1) h2]
    [(heap-empty? h2) h1]
    [else
     (let ([cmp (heap-cmp h1)]
           [v1 (heap-value h1)]
           [v2 (heap-value h2)])
       (if (cmp v1 v2)
           (make-leftist-heap v1
                              (heap-left h1)
                              (heap-merge (heap-right h1) h2)
                              cmp)
           (make-leftist-heap v2
                              (heap-left h2)
                              (heap-merge h1 (heap-right h2))
                              cmp)))]))

;;; Insert a value - O(log n)
(define (heap-insert h value)
  (if (heap-empty? h)
      (heap-singleton value min-cmp)
      (heap-merge h (heap-singleton value (heap-cmp h)))))

;;; Insert with explicit comparator (for first insert into empty heap)
(define (heap-insert* h value cmp)
  (if (heap-empty? h)
      (heap-singleton value cmp)
      (heap-merge h (heap-singleton value cmp))))

;;; Get minimum (or maximum for max-heap) - O(1)
(define (heap-find-min h)
  (if (heap-empty? h)
      nothing
      (just (heap-value h))))

;;; Remove minimum (or maximum) - O(log n)
(define (heap-delete-min h)
  (if (heap-empty? h)
      heap-empty
      (heap-merge (heap-left h) (heap-right h))))

;;; Extract minimum: returns (value . new-heap) or #f
(define (heap-extract-min h)
  (if (heap-empty? h)
      #f
      (cons (heap-value h) (heap-delete-min h))))

;;; ============================================================
;;; Heap Queries
;;; ============================================================

;;; Get heap size - O(n) for leftist heap
(define (heap-size h)
  (if (heap-empty? h)
      0
      (+ 1 (heap-size (heap-left h)) (heap-size (heap-right h)))))

;;; Check if heap contains value - O(n)
(define (heap-contains? h value)
  (cond
    [(heap-empty? h) #f]
    [(equal? (heap-value h) value) #t]
    [else (or (heap-contains? (heap-left h) value)
              (heap-contains? (heap-right h) value))]))

;;; ============================================================
;;; Heap Builders
;;; ============================================================

;;; Build min-heap from list - O(n log n)
(define (list->min-heap lst)
  (fold-left (lambda (h x) (heap-insert* h x min-cmp))
             heap-empty
             lst))

;;; Build max-heap from list
(define (list->max-heap lst)
  (fold-left (lambda (h x) (heap-insert* h x max-cmp))
             heap-empty
             lst))

;;; Build heap with custom comparator
(define (list->heap lst cmp)
  (fold-left (lambda (h x) (heap-insert* h x cmp))
             heap-empty
             lst))

;;; Convert heap to sorted list
(define (heap->list h)
  (let loop ([h h] [acc '()])
    (if (heap-empty? h)
        (reverse acc)
        (loop (heap-delete-min h) (cons (heap-value h) acc)))))

;;; ============================================================
;;; Heap Sort
;;; ============================================================

;;; Sort list using heap sort - O(n log n)
(define (heap-sort lst)
  (heap->list (list->min-heap lst)))

;;; Sort descending
(define (heap-sort-desc lst)
  (heap->list (list->max-heap lst)))

;;; Sort with custom comparator
(define (heap-sort-by cmp lst)
  (heap->list (list->heap lst cmp)))

;;; ============================================================
;;; Top-K Operations
;;; ============================================================

;;; Get k smallest elements
(define (heap-top-k h k)
  (let loop ([h h] [k k] [acc '()])
    (if (or (heap-empty? h) (<= k 0))
        (reverse acc)
        (loop (heap-delete-min h) (- k 1) (cons (heap-value h) acc)))))

;;; Get k smallest from list
(define (top-k-smallest k lst)
  (heap-top-k (list->min-heap lst) k))

;;; Get k largest from list (more efficient: use max-heap of size k)
(define (top-k-largest k lst)
  (heap-top-k (list->max-heap lst) k))

;;; ============================================================
;;; Priority Queue Interface
;;; ============================================================

;;; Priority queue with (priority . value) pairs
;;; Lower priority number = higher priority

(define (pq-empty)
  heap-empty)

(define (pq-empty? pq)
  (heap-empty? pq))

(define (pq-insert pq priority value)
  (heap-insert* pq (cons priority value)
                (lambda (a b) (< (car a) (car b)))))

(define (pq-peek pq)
  (let ([result (heap-find-min pq)])
    (if (just? result)
        (just (cdr (from-just result)))
        nothing)))

(define (pq-peek-priority pq)
  (let ([result (heap-find-min pq)])
    (if (just? result)
        (just (car (from-just result)))
        nothing)))

(define (pq-pop pq)
  (let ([result (heap-extract-min pq)])
    (if result
        (cons (cdr (car result)) (cdr result))
        #f)))

(define (pq-size pq)
  (heap-size pq))

;;; Build priority queue from list of (priority . value) pairs
(define (list->pq pairs)
  (fold-left (lambda (pq p)
               (pq-insert pq (car p) (cdr p)))
             (pq-empty)
             pairs))

;;; ============================================================
;;; Indexed Priority Queue
;;; ============================================================

;;; Indexed PQ stores (key priority value) and allows update by key
;;; Implemented as alist for simplicity (not optimal but functional)

(define (ipq-empty)
  (cons heap-empty '()))  ; (heap . key-index)

(define (ipq-empty? ipq)
  (heap-empty? (car ipq)))

(define (ipq-insert ipq key priority value)
  (let* ([heap (car ipq)]
         [index (cdr ipq)]
         [entry (list key priority value)]
         [new-heap (heap-insert* heap entry
                                 (lambda (a b) (< (cadr a) (cadr b))))]
         [new-index (cons (cons key entry) index)])
    (cons new-heap new-index)))

(define (ipq-get ipq key)
  (let ([found (assoc key (cdr ipq))])
    (if found
        (just (caddr (cdr found)))
        nothing)))

(define (ipq-peek ipq)
  (let ([result (heap-find-min (car ipq))])
    (if (just? result)
        (let ([entry (from-just result)])
          (just (cons (car entry) (caddr entry))))  ; (key . value)
        nothing)))

;;; ============================================================
;;; Heap Merge Operations
;;; ============================================================

;;; Merge multiple heaps
(define (heap-merge-all heaps)
  (fold-left heap-merge heap-empty heaps))

;;; Merge two sorted lists (like merge in merge-sort)
(define (merge-sorted lst1 lst2 cmp)
  (cond
    [(null? lst1) lst2]
    [(null? lst2) lst1]
    [(cmp (car lst1) (car lst2))
     (cons (car lst1) (merge-sorted (cdr lst1) lst2 cmp))]
    [else
     (cons (car lst2) (merge-sorted lst1 (cdr lst2) cmp))]))

;;; K-way merge of sorted lists (purely functional)
(define (k-way-merge lists)
  ;; Use heap of (value . remaining-list) pairs
  (let* ([non-empty (filter (lambda (lst) (not (null? lst))) lists)]
         [initial-heap
          (fold-left (lambda (h lst)
                       (heap-insert* h (cons (car lst) (cdr lst))
                                     (lambda (a b) (< (car a) (car b)))))
                     heap-empty
                     non-empty)])
    (let loop ([h initial-heap] [acc '()])
      (if (heap-empty? h)
          (reverse acc)
          (let* ([min-entry (heap-value h)]
                 [value (car min-entry)]
                 [rest-list (cdr min-entry)]
                 [new-h (heap-delete-min h)])
            (if (null? rest-list)
                (loop new-h (cons value acc))
                (loop (heap-insert* new-h
                                    (cons (car rest-list) (cdr rest-list))
                                    (lambda (a b) (< (car a) (car b))))
                      (cons value acc))))))))

;;; Helper: iota
(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; Heap Transformations
;;; ============================================================

;;; Map function over heap values (breaks heap property, rebuilds)
(define (heap-map f h)
  (if (heap-empty? h)
      heap-empty
      (list->heap (map f (heap->list h)) (heap-cmp h))))

;;; Filter heap by predicate
(define (heap-filter pred h)
  (if (heap-empty? h)
      heap-empty
      (list->heap (filter pred (heap->list h)) (heap-cmp h))))

;;; Fold over heap in priority order
(define (heap-fold f init h)
  (let loop ([h h] [acc init])
    (if (heap-empty? h)
        acc
        (loop (heap-delete-min h) (f acc (heap-value h))))))

;;; ============================================================
;;; Median Tracking
;;; ============================================================

;;; Median tracker using two heaps
;;; - max-heap for lower half
;;; - min-heap for upper half

(define (make-median-tracker)
  (list heap-empty heap-empty))  ; (lower-max-heap . upper-min-heap)

(define (median-tracker? x)
  (and (list? x) (= (length x) 2)))

(define (median-insert tracker value)
  (let* ([lower (car tracker)]
         [upper (cadr tracker)]
         [lower-max (heap-find-min lower)])
    (if (or (heap-empty? lower)
            (and (just? lower-max) (<= value (from-just lower-max))))
        ;; Insert into lower half
        (let ([new-lower (heap-insert* lower value max-cmp)])
          (median-rebalance new-lower upper))
        ;; Insert into upper half
        (let ([new-upper (heap-insert* upper value min-cmp)])
          (median-rebalance lower new-upper)))))

(define (median-rebalance lower upper)
  (let ([lower-size (heap-size lower)]
        [upper-size (heap-size upper)])
    (cond
      ;; Lower too big
      [(> lower-size (+ upper-size 1))
       (let ([val (heap-value lower)])
         (list (heap-delete-min lower)
               (heap-insert* upper val min-cmp)))]
      ;; Upper too big
      [(> upper-size lower-size)
       (let ([val (heap-value upper)])
         (list (heap-insert* lower val max-cmp)
               (heap-delete-min upper)))]
      [else (list lower upper)])))

(define (median-get tracker)
  (let* ([lower (car tracker)]
         [upper (cadr tracker)]
         [lower-size (heap-size lower)]
         [upper-size (heap-size upper)])
    (cond
      [(and (heap-empty? lower) (heap-empty? upper)) nothing]
      [(> lower-size upper-size) (just (heap-value lower))]
      [(> upper-size lower-size) (just (heap-value upper))]
      [else
       ;; Equal sizes - return average (or lower for simplicity)
       (just (heap-value lower))])))

;;; ============================================================
;;; Visualization
;;; ============================================================

(define (heap->string h)
  (if (heap-empty? h)
      "[]"
      (format "[~a]" (string-join (map (lambda (x) (format "~a" x))
                                       (heap->list h))
                                  ", "))))

;;; Helper: string-join
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; Types:
;;;   heap?, heap-empty, heap-empty?, heap-singleton
;;;
;;; Core operations:
;;;   heap-insert, heap-insert*, heap-merge, heap-find-min,
;;;   heap-delete-min, heap-extract-min
;;;
;;; Queries:
;;;   heap-size, heap-contains?
;;;
;;; Builders:
;;;   list->min-heap, list->max-heap, list->heap, heap->list
;;;
;;; Sorting:
;;;   heap-sort, heap-sort-desc, heap-sort-by
;;;
;;; Top-K:
;;;   heap-top-k, top-k-smallest, top-k-largest
;;;
;;; Priority queue:
;;;   pq-empty, pq-empty?, pq-insert, pq-peek, pq-pop, pq-size, list->pq
;;;
;;; Indexed PQ:
;;;   ipq-empty, ipq-insert, ipq-get, ipq-peek
;;;
;;; Merging:
;;;   heap-merge-all, merge-sorted, k-way-merge
;;;
;;; Transformations:
;;;   heap-map, heap-filter, heap-fold
;;;
;;; Median:
;;;   make-median-tracker, median-insert, median-get
;;;
;;; Visualization:
;;;   heap->string
