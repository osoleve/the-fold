;;; lattice/data/sort.ss — Sorting Algorithms
;;;
;;; Purely functional sorting algorithms for lists.
;;; Provides merge sort (stable, O(n log n)) and quicksort (O(n log n) average).
;;;
;;; All sorting functions take an optional comparator; default is < (ascending).
;;; Comparator (cmp a b) should return #t if a should come before b.
;;;
;;; TIER: 0 (no lattice dependencies)

;;;============================================================================
;;; Merge Sort — Stable, O(n log n) guaranteed
;;;============================================================================

;;; merge : (List α) × (List α) × (α × α → Boolean) → (List α)
;;; Merge two sorted lists into one sorted list.
;;; For stability: when equal, take from left list first.
(define (merge xs ys cmp)
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    ;; Take from xs if xs <= ys (not strictly less, for stability)
    [(not (cmp (car ys) (car xs)))
     (cons (car xs) (merge (cdr xs) ys cmp))]
    [else
     (cons (car ys) (merge xs (cdr ys) cmp))]))

;;; split-at : (List α) × Nat → (List α) × (List α)
;;; Split list at position n. Returns (first-n-elements, rest).
(define (split-at lst n)
  (if (or (= n 0) (null? lst))
      (cons '() lst)
      (let ([rest (split-at (cdr lst) (- n 1))])
        (cons (cons (car lst) (car rest))
              (cdr rest)))))

;;; merge-sort-by : (α × α → Boolean) × (List α) → (List α)
;;; Sort list using merge sort with custom comparator.
;;; Stable: equal elements maintain their original order.
;;; Complexity: O(n log n) time, O(n) space.
(define (merge-sort-by cmp lst)
  (let ([len (length lst)])
    (if (<= len 1)
        lst
        (let* ([mid (quotient len 2)]
               [split (split-at lst mid)]
               [left (car split)]
               [right (cdr split)])
          (merge (merge-sort-by cmp left)
                 (merge-sort-by cmp right)
                 cmp)))))

;;; merge-sort : (List α) → (List α)
;;; Sort list in ascending order using merge sort.
(define (merge-sort lst)
  (merge-sort-by < lst))

;;; sort : (List α) → (List α)
;;; Sort list in ascending order (alias for merge-sort).
(define sort merge-sort)

;;; sort-by : (α × α → Boolean) × (List α) → (List α)
;;; Sort list with custom comparator (alias for merge-sort-by).
(define sort-by merge-sort-by)

;;; stable-sort : (List α) → (List α)
;;; Stable sort in ascending order (alias for merge-sort).
(define stable-sort merge-sort)

;;; stable-sort-by : (α × α → Boolean) × (List α) → (List α)
;;; Stable sort with custom comparator.
(define stable-sort-by merge-sort-by)

;;;============================================================================
;;; Quicksort — O(n log n) average, O(n²) worst case
;;;============================================================================

;;; partition : (α × α → Boolean) × α × (List α) → (List α) × (List α) × (List α)
;;; Partition list into (less-than, equal-to, greater-than) pivot.
;;; Three-way partition handles duplicates efficiently.
(define (partition cmp pivot lst)
  (let loop ([xs lst] [lt '()] [eq '()] [gt '()])
    (if (null? xs)
        (list (reverse lt) (reverse eq) (reverse gt))
        (let ([x (car xs)])
          (cond
            [(cmp x pivot)
             (loop (cdr xs) (cons x lt) eq gt)]
            [(cmp pivot x)
             (loop (cdr xs) lt eq (cons x gt))]
            [else
             (loop (cdr xs) lt (cons x eq) gt)])))))

;;; quicksort-by : (α × α → Boolean) × (List α) → (List α)
;;; Sort list using quicksort with custom comparator.
;;; Not stable: equal elements may be reordered.
;;; Complexity: O(n log n) average, O(n²) worst case.
(define (quicksort-by cmp lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let* ([pivot (car lst)]
             [parts (partition cmp pivot (cdr lst))]
             [lt (car parts)]
             [eq (cadr parts)]
             [gt (caddr parts)])
        (append (quicksort-by cmp lt)
                (cons pivot eq)
                (quicksort-by cmp gt)))))

;;; quicksort : (List α) → (List α)
;;; Sort list in ascending order using quicksort.
(define (quicksort lst)
  (quicksort-by < lst))

;;;============================================================================
;;; Insertion Sort — O(n²), but O(n) for nearly-sorted lists
;;;============================================================================

;;; insert-sorted : (α × α → Boolean) × α × (List α) → (List α)
;;; Insert element into sorted list maintaining order.
(define (insert-sorted cmp x sorted)
  (cond
    [(null? sorted) (list x)]
    [(cmp x (car sorted)) (cons x sorted)]
    [else (cons (car sorted) (insert-sorted cmp x (cdr sorted)))]))

;;; insertion-sort-by : (α × α → Boolean) × (List α) → (List α)
;;; Sort list using insertion sort with custom comparator.
;;; Stable: equal elements maintain their original order.
;;; Complexity: O(n²) average, O(n) for nearly-sorted input.
(define (insertion-sort-by cmp lst)
  (fold-left (lambda (sorted x) (insert-sorted cmp x sorted))
             '()
             lst))

;;; insertion-sort : (List α) → (List α)
;;; Sort list in ascending order using insertion sort.
(define (insertion-sort lst)
  (insertion-sort-by < lst))

;;;============================================================================
;;; Sort by Key — Sort by a derived value
;;;============================================================================

;;; sort-by-key : (α → β) × (List α) → (List α)
;;; Sort list by a key function (Schwartzian transform).
;;; Uses merge sort internally for stability.
;;; Example: (sort-by-key string-length '("aaa" "b" "cc")) => ("b" "cc" "aaa")
(define (sort-by-key key-fn lst)
  ;; Decorate
  (let* ([decorated (map (lambda (x) (cons (key-fn x) x)) lst)]
         ;; Sort by key
         [sorted (merge-sort-by (lambda (a b) (< (car a) (car b))) decorated)])
    ;; Undecorate
    (map cdr sorted)))

;;; sort-by-key-desc : (α → β) × (List α) → (List α)
;;; Sort list by key function in descending order.
(define (sort-by-key-desc key-fn lst)
  (let* ([decorated (map (lambda (x) (cons (key-fn x) x)) lst)]
         [sorted (merge-sort-by (lambda (a b) (> (car a) (car b))) decorated)])
    (map cdr sorted)))

;;;============================================================================
;;; Descending Order Variants
;;;============================================================================

;;; sort-desc : (List α) → (List α)
;;; Sort list in descending order.
(define (sort-desc lst)
  (merge-sort-by > lst))

;;; quicksort-desc : (List α) → (List α)
;;; Sort list in descending order using quicksort.
(define (quicksort-desc lst)
  (quicksort-by > lst))

;;;============================================================================
;;; Utility Functions
;;;============================================================================

;;; sorted? : (List α) → Boolean
;;; Check if list is sorted in ascending order.
(define (sorted? lst)
  (sorted-by? < lst))

;;; sorted-by? : (α × α → Boolean) × (List α) → Boolean
;;; Check if list is sorted according to comparator.
(define (sorted-by? cmp lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (not (cmp (cadr lst) (car lst)))  ; car <= cadr
           (sorted-by? cmp (cdr lst)))))

;;; sorted-desc? : (List α) → Boolean
;;; Check if list is sorted in descending order.
(define (sorted-desc? lst)
  (sorted-by? > lst))

;;; unique-sorted : (List α) → (List α)
;;; Remove adjacent duplicates from a sorted list.
(define (unique-sorted lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (if (equal? (car lst) (cadr lst))
          (unique-sorted (cdr lst))
          (cons (car lst) (unique-sorted (cdr lst))))))

;;; sort-unique : (List α) → (List α)
;;; Sort list and remove duplicates.
(define (sort-unique lst)
  (unique-sorted (sort lst)))

;;; nth-smallest : Nat × (List α) → α
;;; Find the nth smallest element (0-indexed).
;;; Uses quickselect for O(n) average performance.
(define (nth-smallest n lst)
  (if (null? lst)
      (error 'nth-smallest "List is empty")
      (let* ([pivot (car lst)]
             [parts (partition < pivot (cdr lst))]
             [lt (car parts)]
             [eq (cadr parts)]
             [gt (caddr parts)]
             [lt-len (length lt)])
        (cond
          [(< n lt-len)
           (nth-smallest n lt)]
          [(< n (+ lt-len 1 (length eq)))
           pivot]
          [else
           (nth-smallest (- n lt-len 1 (length eq)) gt)]))))

;;; median : (List α) → α
;;; Find the median element.
(define (median lst)
  (let ([len (length lst)])
    (if (= len 0)
        (error 'median "Cannot find median of empty list")
        (nth-smallest (quotient len 2) lst))))

;;; min-element : (List α) → α
;;; Find minimum element. O(n).
(define (min-element lst)
  (if (null? lst)
      (error 'min-element "Cannot find min of empty list")
      (fold-left (lambda (m x) (if (< x m) x m))
                 (car lst)
                 (cdr lst))))

;;; max-element : (List α) → α
;;; Find maximum element. O(n).
(define (max-element lst)
  (if (null? lst)
      (error 'max-element "Cannot find max of empty list")
      (fold-left (lambda (m x) (if (> x m) x m))
                 (car lst)
                 (cdr lst))))

;;; min-by : (α → β) × (List α) → α
;;; Find element with minimum key value.
(define (min-by key-fn lst)
  (if (null? lst)
      (error 'min-by "Cannot find min of empty list")
      (car (fold-left (lambda (best x)
                        (let ([k (key-fn x)])
                          (if (< k (cdr best))
                              (cons x k)
                              best)))
                      (cons (car lst) (key-fn (car lst)))
                      (cdr lst)))))

;;; max-by : (α → β) × (List α) → α
;;; Find element with maximum key value.
(define (max-by key-fn lst)
  (if (null? lst)
      (error 'max-by "Cannot find max of empty list")
      (car (fold-left (lambda (best x)
                        (let ([k (key-fn x)])
                          (if (> k (cdr best))
                              (cons x k)
                              best)))
                      (cons (car lst) (key-fn (car lst)))
                      (cdr lst)))))

;;;============================================================================
;;; Top-K and Bottom-K
;;;============================================================================

;;; take-sorted : Nat × (List α) → (List α)
;;; Get first n elements of sorted list.
(define (take-sorted n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take-sorted (- n 1) (cdr lst)))))

;;; top-k : Nat × (List α) → (List α)
;;; Get k largest elements (in descending order).
(define (top-k k lst)
  (take-sorted k (sort-desc lst)))

;;; bottom-k : Nat × (List α) → (List α)
;;; Get k smallest elements (in ascending order).
(define (bottom-k k lst)
  (take-sorted k (sort lst)))
