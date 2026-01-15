;;; lattice/data/heap.ss — Leftist Heap / Priority Queue
;;;
;;; Purely functional heap implementation using leftist tree structure.
;;; Provides O(log n) insert, delete-min, merge; O(1) peek-min.
;;;
;;; A leftist heap maintains the "leftist property": rank(left) >= rank(right)
;;; where rank is the length of the rightmost path to an empty node.
;;; This ensures the right spine is always short, making merge efficient.
;;;
;;; Heap α = Empty | (Node rank value left right)
;;;
;;; Default: min-heap (smallest element at root)
;;; For max-heap, use heap-max-* functions or provide custom comparator.
;;;
;;; TIER: 0 (no lattice dependencies)

;;;============================================================================
;;; Core Representation
;;;============================================================================

;;; heap-empty : Heap
;;; The empty heap.
(define heap-empty 'heap-empty)

;;; heap-empty? : Heap → Boolean
;;; Check if heap is empty.
(define (heap-empty? heap)
  (eq? heap 'heap-empty))

;;; heap-node : Nat × α × Heap × Heap → Heap
;;; Internal node constructor.
(define (heap-node rank value left right)
  (list 'heap-node rank value left right))

;;; heap-node? : Any → Boolean
;;; Check if value is a heap node.
(define (heap-node? x)
  (and (pair? x)
       (eq? (car x) 'heap-node)))

;;; Accessors for heap nodes
(define (heap-rank h)
  (if (heap-empty? h) 0 (cadr h)))

(define (heap-value h)
  (caddr h))

(define (heap-left h)
  (cadddr h))

(define (heap-right h)
  (car (cddddr h)))

;;;============================================================================
;;; Min-Heap Operations (default)
;;;============================================================================

;;; make-heap-node : α × Heap × Heap → Heap
;;; Smart constructor that maintains leftist property.
;;; Always puts the subtree with larger rank on the left.
(define (make-heap-node value left right)
  (let ([rank-l (heap-rank left)]
        [rank-r (heap-rank right)])
    (if (>= rank-l rank-r)
        (heap-node (+ 1 rank-r) value left right)
        (heap-node (+ 1 rank-l) value right left))))

;;; heap-merge : Heap × Heap → Heap
;;; Merge two min-heaps into one. O(log n) where n = total size.
(define (heap-merge h1 h2)
  (cond
    [(heap-empty? h1) h2]
    [(heap-empty? h2) h1]
    [else
     (let ([v1 (heap-value h1)]
           [v2 (heap-value h2)])
       (if (<= v1 v2)
           (make-heap-node v1 (heap-left h1) (heap-merge (heap-right h1) h2))
           (make-heap-node v2 (heap-left h2) (heap-merge h1 (heap-right h2)))))]))

;;; heap-insert : α × Heap → Heap
;;; Insert element into min-heap. O(log n).
(define (heap-insert elem heap)
  (heap-merge (heap-node 1 elem heap-empty heap-empty) heap))

;;; heap-min : Heap → α
;;; Get minimum element without removing. O(1).
;;; Error if heap is empty.
(define (heap-min heap)
  (if (heap-empty? heap)
      (error 'heap-min "Cannot get min of empty heap")
      (heap-value heap)))

;;; heap-peek : Heap → α
;;; Alias for heap-min.
(define heap-peek heap-min)

;;; heap-delete-min : Heap → Heap
;;; Remove minimum element from heap. O(log n).
;;; Error if heap is empty.
(define (heap-delete-min heap)
  (if (heap-empty? heap)
      (error 'heap-delete-min "Cannot delete from empty heap")
      (heap-merge (heap-left heap) (heap-right heap))))

;;; heap-pop : Heap → (Values Heap α)
;;; Remove and return minimum element. Returns (new-heap, min-element).
;;; Error if heap is empty.
(define (heap-pop heap)
  (if (heap-empty? heap)
      (error 'heap-pop "Cannot pop from empty heap")
      (values (heap-merge (heap-left heap) (heap-right heap))
              (heap-value heap))))

;;;============================================================================
;;; Max-Heap Operations
;;;============================================================================

;;; heap-merge-max : Heap × Heap → Heap
;;; Merge two max-heaps into one.
(define (heap-merge-max h1 h2)
  (cond
    [(heap-empty? h1) h2]
    [(heap-empty? h2) h1]
    [else
     (let ([v1 (heap-value h1)]
           [v2 (heap-value h2)])
       (if (>= v1 v2)
           (make-heap-node v1 (heap-left h1) (heap-merge-max (heap-right h1) h2))
           (make-heap-node v2 (heap-left h2) (heap-merge-max h1 (heap-right h2)))))]))

;;; heap-insert-max : α × Heap → Heap
;;; Insert element into max-heap.
(define (heap-insert-max elem heap)
  (heap-merge-max (heap-node 1 elem heap-empty heap-empty) heap))

;;; heap-max : Heap → α
;;; Get maximum element without removing.
(define heap-max heap-value)

;;; heap-delete-max : Heap → Heap
;;; Remove maximum element from max-heap.
(define (heap-delete-max heap)
  (if (heap-empty? heap)
      (error 'heap-delete-max "Cannot delete from empty heap")
      (heap-merge-max (heap-left heap) (heap-right heap))))

;;; heap-pop-max : Heap → (Values Heap α)
;;; Remove and return maximum element from max-heap.
(define (heap-pop-max heap)
  (if (heap-empty? heap)
      (error 'heap-pop-max "Cannot pop from empty heap")
      (values (heap-merge-max (heap-left heap) (heap-right heap))
              (heap-value heap))))

;;;============================================================================
;;; Generic Heap Operations (custom comparator)
;;;============================================================================

;;; heap-merge-by : (α × α → Boolean) × Heap × Heap → Heap
;;; Merge heaps using custom comparator (cmp a b) returns #t if a should be root.
(define (heap-merge-by cmp h1 h2)
  (cond
    [(heap-empty? h1) h2]
    [(heap-empty? h2) h1]
    [else
     (let ([v1 (heap-value h1)]
           [v2 (heap-value h2)])
       (if (cmp v1 v2)
           (make-heap-node v1 (heap-left h1) (heap-merge-by cmp (heap-right h1) h2))
           (make-heap-node v2 (heap-left h2) (heap-merge-by cmp h1 (heap-right h2)))))]))

;;; heap-insert-by : (α × α → Boolean) × α × Heap → Heap
;;; Insert element using custom comparator.
(define (heap-insert-by cmp elem heap)
  (heap-merge-by cmp (heap-node 1 elem heap-empty heap-empty) heap))

;;; heap-delete-top-by : (α × α → Boolean) × Heap → Heap
;;; Remove top element using custom comparator.
(define (heap-delete-top-by cmp heap)
  (if (heap-empty? heap)
      (error 'heap-delete-top-by "Cannot delete from empty heap")
      (heap-merge-by cmp (heap-left heap) (heap-right heap))))

;;;============================================================================
;;; Bulk Operations
;;;============================================================================

;;; heap-size : Heap → Nat
;;; Get number of elements in heap. O(n).
(define (heap-size heap)
  (if (heap-empty? heap)
      0
      (+ 1 (heap-size (heap-left heap)) (heap-size (heap-right heap)))))

;;; list->heap : (List α) → Heap
;;; Build min-heap from list. O(n log n).
(define (list->heap lst)
  (fold-left (lambda (h x) (heap-insert x h)) heap-empty lst))

;;; list->heap-max : (List α) → Heap
;;; Build max-heap from list.
(define (list->heap-max lst)
  (fold-left (lambda (h x) (heap-insert-max x h)) heap-empty lst))

;;; list->heap-by : (α × α → Boolean) × (List α) → Heap
;;; Build heap from list using custom comparator.
(define (list->heap-by cmp lst)
  (fold-left (lambda (h x) (heap-insert-by cmp x h)) heap-empty lst))

;;; heap->list : Heap → (List α)
;;; Extract all elements in min-first order. O(n log n).
(define (heap->list heap)
  (if (heap-empty? heap)
      '()
      (cons (heap-min heap)
            (heap->list (heap-delete-min heap)))))

;;; heap->list-max : Heap → (List α)
;;; Extract all elements in max-first order.
(define (heap->list-max heap)
  (if (heap-empty? heap)
      '()
      (cons (heap-max heap)
            (heap->list-max (heap-delete-max heap)))))

;;; heapify : (List α) → Heap
;;; Alias for list->heap.
(define heapify list->heap)

;;;============================================================================
;;; Priority Queue Interface
;;;============================================================================

;;; A priority queue is just a heap with more intuitive naming.
;;; Lower priority values come out first (min-priority-queue).

;;; pq-empty : PriorityQueue
(define pq-empty heap-empty)

;;; pq-empty? : PriorityQueue → Boolean
(define pq-empty? heap-empty?)

;;; pq-insert : α × PriorityQueue → PriorityQueue
;;; Insert element (elements must be comparable with <).
(define pq-insert heap-insert)

;;; pq-peek : PriorityQueue → α
;;; Get highest-priority (smallest) element.
(define pq-peek heap-min)

;;; pq-pop : PriorityQueue → (Values PriorityQueue α)
;;; Remove and return highest-priority element.
(define pq-pop heap-pop)

;;; pq-size : PriorityQueue → Nat
(define pq-size heap-size)

;;; pq-from-list : (List α) → PriorityQueue
(define pq-from-list list->heap)

;;; pq-to-list : PriorityQueue → (List α)
;;; Returns elements in priority order.
(define pq-to-list heap->list)

;;;============================================================================
;;; Heap Sort
;;;============================================================================

;;; heapsort : (List α) → (List α)
;;; Sort list in ascending order using heap. O(n log n).
(define (heapsort lst)
  (heap->list (list->heap lst)))

;;; heapsort-desc : (List α) → (List α)
;;; Sort list in descending order.
(define (heapsort-desc lst)
  (heap->list-max (list->heap-max lst)))

;;; heapsort-by : (α × α → Boolean) × (List α) → (List α)
;;; Sort list using custom comparator.
;;; (heapsort-by < lst) gives ascending order.
(define (heapsort-by cmp lst)
  (let loop ([h (list->heap-by cmp lst)] [acc '()])
    (if (heap-empty? h)
        (reverse acc)
        (loop (heap-delete-top-by cmp h) (cons (heap-value h) acc)))))
