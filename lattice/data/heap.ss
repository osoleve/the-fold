(load "core/base/prelude.ss")

(doc 'module 'heap)
(doc 'description
     "Purely functional heap implementation using leftist tree structure. Provides O(log n) insert, delete-min, merge; O(1) peek-min. A leftist heap maintains the leftist property: rank(left) >= rank(right) where rank is the length of the rightmost path to an empty node. This ensures the right spine is always short, making merge efficient. Heap α = Empty | (Node rank value left right). Default: min-heap (smallest element at root). For max-heap, use heap-max-* functions or provide custom comparator.")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'note "No lattice dependencies")

(doc 'section 'core-representation)

(define heap-empty 'heap-empty)
(doc heap-empty 'description "The empty heap.")

(define (heap-empty? heap)
  (doc 'type '(-> Heap Boolean))
  (doc 'description "Check if heap is empty.")
  (eq? heap 'heap-empty))

(define (heap-node rank value left right)
  (doc 'type '(-> Nat α Heap Heap Heap))
  (doc 'description "Internal node constructor.")
  (list 'heap-node rank value left right))

(define (heap-node? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if value is a heap node.")
  (and (pair? x)
       (eq? (car x) 'heap-node)))

(define (heap-rank h)
  (doc 'type '(-> Heap Nat))
  (doc 'description "Get rank of heap node.")
  (if (heap-empty? h) 0 (cadr h)))

(define (heap-value h)
  (doc 'type '(-> Heap α))
  (doc 'description "Get value at heap root.")
  (caddr h))

(define (heap-left h)
  (doc 'type '(-> Heap Heap))
  (doc 'description "Get left subtree.")
  (cadddr h))

(define (heap-right h)
  (doc 'type '(-> Heap Heap))
  (doc 'description "Get right subtree.")
  (car (cddddr h)))

(doc 'section 'min-heap-operations)

(define (make-heap-node value left right)
  (doc 'type '(-> α Heap Heap Heap))
  (doc 'description "Smart constructor that maintains leftist property. Always puts the subtree with larger rank on the left.")
  (let ([rank-l (heap-rank left)]
        [rank-r (heap-rank right)])
    (if (>= rank-l rank-r)
        (heap-node (+ 1 rank-r) value left right)
        (heap-node (+ 1 rank-l) value right left))))

(define (heap-merge h1 h2)
  (doc 'type '(-> Heap Heap Heap))
  (doc 'description "Merge two min-heaps into one.")
  (doc 'complexity "O(log n) where n = total size")
  (cond
    [(heap-empty? h1) h2]
    [(heap-empty? h2) h1]
    [else
     (let ([v1 (heap-value h1)]
           [v2 (heap-value h2)])
       (if (<= v1 v2)
           (make-heap-node v1 (heap-left h1) (heap-merge (heap-right h1) h2))
           (make-heap-node v2 (heap-left h2) (heap-merge h1 (heap-right h2)))))]))

(define (heap-insert elem heap)
  (doc 'type '(-> α Heap Heap))
  (doc 'description "Insert element into min-heap.")
  (doc 'complexity "O(log n)")
  (heap-merge (heap-node 1 elem heap-empty heap-empty) heap))

(define (heap-min heap)
  (doc 'type '(-> Heap α))
  (doc 'description "Get minimum element without removing. Error if heap is empty.")
  (doc 'complexity "O(1)")
  (if (heap-empty? heap)
      (error 'heap-min "Cannot get min of empty heap")
      (heap-value heap)))

(define heap-peek heap-min)
(doc heap-peek 'description "Alias for heap-min.")

(define (heap-delete-min heap)
  (doc 'type '(-> Heap Heap))
  (doc 'description "Remove minimum element from heap. Error if heap is empty.")
  (doc 'complexity "O(log n)")
  (if (heap-empty? heap)
      (error 'heap-delete-min "Cannot delete from empty heap")
      (heap-merge (heap-left heap) (heap-right heap))))

(define (heap-pop heap)
  (doc 'type '(-> Heap (Values Heap α)))
  (doc 'description "Remove and return minimum element. Returns (new-heap, min-element). Error if heap is empty.")
  (if (heap-empty? heap)
      (error 'heap-pop "Cannot pop from empty heap")
      (values (heap-merge (heap-left heap) (heap-right heap))
              (heap-value heap))))

(doc 'section 'max-heap-operations)

(define (heap-merge-max h1 h2)
  (doc 'type '(-> Heap Heap Heap))
  (doc 'description "Merge two max-heaps into one.")
  (cond
    [(heap-empty? h1) h2]
    [(heap-empty? h2) h1]
    [else
     (let ([v1 (heap-value h1)]
           [v2 (heap-value h2)])
       (if (>= v1 v2)
           (make-heap-node v1 (heap-left h1) (heap-merge-max (heap-right h1) h2))
           (make-heap-node v2 (heap-left h2) (heap-merge-max h1 (heap-right h2)))))]))

(define (heap-insert-max elem heap)
  (doc 'type '(-> α Heap Heap))
  (doc 'description "Insert element into max-heap.")
  (heap-merge-max (heap-node 1 elem heap-empty heap-empty) heap))

(define heap-max heap-value)
(doc heap-max 'description "Get maximum element without removing.")

(define (heap-delete-max heap)
  (doc 'type '(-> Heap Heap))
  (doc 'description "Remove maximum element from max-heap.")
  (if (heap-empty? heap)
      (error 'heap-delete-max "Cannot delete from empty heap")
      (heap-merge-max (heap-left heap) (heap-right heap))))

(define (heap-pop-max heap)
  (doc 'type '(-> Heap (Values Heap α)))
  (doc 'description "Remove and return maximum element from max-heap.")
  (if (heap-empty? heap)
      (error 'heap-pop-max "Cannot pop from empty heap")
      (values (heap-merge-max (heap-left heap) (heap-right heap))
              (heap-value heap))))

(doc 'section 'generic-heap-operations)
(doc 'note "Custom comparator")

(define (heap-merge-by cmp h1 h2)
  (doc 'type '(-> (-> α α Boolean) Heap Heap Heap))
  (doc 'description "Merge heaps using custom comparator (cmp a b) returns #t if a should be root.")
  (cond
    [(heap-empty? h1) h2]
    [(heap-empty? h2) h1]
    [else
     (let ([v1 (heap-value h1)]
           [v2 (heap-value h2)])
       (if (cmp v1 v2)
           (make-heap-node v1 (heap-left h1) (heap-merge-by cmp (heap-right h1) h2))
           (make-heap-node v2 (heap-left h2) (heap-merge-by cmp h1 (heap-right h2)))))]))

(define (heap-insert-by cmp elem heap)
  (doc 'type '(-> (-> α α Boolean) α Heap Heap))
  (doc 'description "Insert element using custom comparator.")
  (heap-merge-by cmp (heap-node 1 elem heap-empty heap-empty) heap))

(define (heap-delete-top-by cmp heap)
  (doc 'type '(-> (-> α α Boolean) Heap Heap))
  (doc 'description "Remove top element using custom comparator.")
  (if (heap-empty? heap)
      (error 'heap-delete-top-by "Cannot delete from empty heap")
      (heap-merge-by cmp (heap-left heap) (heap-right heap))))

(doc 'section 'bulk-operations)

(define (heap-size heap)
  (doc 'type '(-> Heap Nat))
  (doc 'description "Get number of elements in heap.")
  (doc 'complexity "O(n)")
  (if (heap-empty? heap)
      0
      (+ 1 (heap-size (heap-left heap)) (heap-size (heap-right heap)))))

(define (list->heap lst)
  (doc 'type '(-> (List α) Heap))
  (doc 'description "Build min-heap from list.")
  (doc 'complexity "O(n log n)")
  (fold-left (lambda (h x) (heap-insert x h)) heap-empty lst))

(define (list->heap-max lst)
  (doc 'type '(-> (List α) Heap))
  (doc 'description "Build max-heap from list.")
  (fold-left (lambda (h x) (heap-insert-max x h)) heap-empty lst))

(define (list->heap-by cmp lst)
  (doc 'type '(-> (-> α α Boolean) (List α) Heap))
  (doc 'description "Build heap from list using custom comparator.")
  (fold-left (lambda (h x) (heap-insert-by cmp x h)) heap-empty lst))

(define (heap->list heap)
  (doc 'type '(-> Heap (List α)))
  (doc 'description "Extract all elements in min-first order.")
  (doc 'complexity "O(n log n)")
  (if (heap-empty? heap)
      '()
      (cons (heap-min heap)
            (heap->list (heap-delete-min heap)))))

(define (heap->list-max heap)
  (doc 'type '(-> Heap (List α)))
  (doc 'description "Extract all elements in max-first order.")
  (if (heap-empty? heap)
      '()
      (cons (heap-max heap)
            (heap->list-max (heap-delete-max heap)))))

(define heapify list->heap)
(doc heapify 'description "Alias for list->heap.")

(doc 'section 'priority-queue-interface)
(doc 'note "A priority queue is just a heap with more intuitive naming. Lower priority values come out first (min-priority-queue).")

(define pq-empty heap-empty)
(doc pq-empty 'description "Empty priority queue.")

(define pq-empty? heap-empty?)
(doc pq-empty? 'description "Check if priority queue is empty.")

(define pq-insert heap-insert)
(doc pq-insert 'description "Insert element (elements must be comparable with <).")

(define pq-peek heap-min)
(doc pq-peek 'description "Get highest-priority (smallest) element.")

(define pq-pop heap-pop)
(doc pq-pop 'description "Remove and return highest-priority element.")

(define pq-size heap-size)
(doc pq-size 'description "Get priority queue size.")

(define pq-from-list list->heap)
(doc pq-from-list 'description "Build priority queue from list.")

(define pq-to-list heap->list)
(doc pq-to-list 'description "Returns elements in priority order.")

(doc 'section 'heap-sort)

(define (heapsort lst)
  (doc 'type '(-> (List α) (List α)))
  (doc 'description "Sort list in ascending order using heap.")
  (doc 'complexity "O(n log n)")
  (heap->list (list->heap lst)))

(define (heapsort-desc lst)
  (doc 'type '(-> (List α) (List α)))
  (doc 'description "Sort list in descending order.")
  (heap->list-max (list->heap-max lst)))

(define (heapsort-by cmp lst)
  (doc 'type '(-> (-> α α Boolean) (List α) (List α)))
  (doc 'description "Sort list using custom comparator. (heapsort-by < lst) gives ascending order.")
  (let loop ([h (list->heap-by cmp lst)] [acc '()])
    (if (heap-empty? h)
        (reverse acc)
        (loop (heap-delete-top-by cmp h) (cons (heap-value h) acc)))))
