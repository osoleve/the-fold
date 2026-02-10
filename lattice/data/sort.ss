;;; lattice/data/sort.ss — Sorting Algorithms
;;; @module sort
;;; @requires prelude

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'sort)
(doc 'description "Sorting algorithms for lists and vectors")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'total)

(doc 'section 'merge-sort)

(define (merge xs ys cmp)
  (doc 'type (-> (List α) (List α) (-> α α Boolean) (List α)))
  (doc 'description "Merge two sorted lists into one sorted list")
  (doc 'note "For stability: when equal, take from left list first")
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    ;; Take from xs if xs <= ys (not strictly less, for stability)
    [(not (cmp (car ys) (car xs)))
     (cons (car xs) (merge (cdr xs) ys cmp))]
    [else
     (cons (car ys) (merge xs (cdr ys) cmp))]))

(define (split-at lst n)
  (doc 'type (-> (List α) Nat (Pair (List α) (List α))))
  (doc 'description "Split list at position n")
  (if (or (= n 0) (null? lst))
      (cons '() lst)
      (let ([rest (split-at (cdr lst) (- n 1))])
        (cons (cons (car lst) (car rest))
              (cdr rest)))))

(define (merge-sort-by cmp lst)
  (doc 'type (-> (-> α α Boolean) (List α) (List α)))
  (doc 'description "Sort list using merge sort with custom comparator")
  (doc 'note "Stable: equal elements maintain their original order")
  (doc 'complexity "O(n log n) time, O(n) space")
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

(define (merge-sort lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Sort list in ascending order using merge sort")
  (merge-sort-by < lst))

(define sort merge-sort)
(doc sort 'description "Sort list in ascending order (alias for merge-sort)")

(define sort-by merge-sort-by)
(doc sort-by 'description "Sort list with custom comparator (alias for merge-sort-by)")

(define stable-sort merge-sort)
(doc stable-sort 'description "Stable sort in ascending order (alias for merge-sort)")

(define stable-sort-by merge-sort-by)
(doc stable-sort-by 'description "Stable sort with custom comparator")

(doc 'section 'quicksort)

(define (partition cmp pivot lst)
  (doc 'type (-> (-> α α Boolean) α (List α) (List (List α) (List α) (List α))))
  (doc 'description "Partition list into (less-than, equal-to, greater-than) pivot")
  (doc 'note "Three-way partition handles duplicates efficiently")
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

(define (quicksort-by cmp lst)
  (doc 'type (-> (-> α α Boolean) (List α) (List α)))
  (doc 'description "Sort list using quicksort with custom comparator")
  (doc 'note "Not stable: equal elements may be reordered")
  (doc 'complexity "O(n log n) average, O(n²) worst case")
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

(define (quicksort lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Sort list in ascending order using quicksort")
  (quicksort-by < lst))

(doc 'section 'insertion-sort)

(define (insert-sorted cmp x sorted)
  (doc 'type (-> (-> α α Boolean) α (List α) (List α)))
  (doc 'description "Insert element into sorted list maintaining order")
  (cond
    [(null? sorted) (list x)]
    [(cmp x (car sorted)) (cons x sorted)]
    [else (cons (car sorted) (insert-sorted cmp x (cdr sorted)))]))

(define (insertion-sort-by cmp lst)
  (doc 'type (-> (-> α α Boolean) (List α) (List α)))
  (doc 'description "Sort list using insertion sort with custom comparator")
  (doc 'note "Stable: equal elements maintain their original order")
  (doc 'complexity "O(n²) average, O(n) for nearly-sorted input")
  (fold-left (lambda (sorted x) (insert-sorted cmp x sorted))
             '()
             lst))

(define (insertion-sort lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Sort list in ascending order using insertion sort")
  (insertion-sort-by < lst))

(doc 'section 'sort-by-key)

(define (sort-by-key key-fn lst)
  (doc 'type (-> (-> α β) (List α) (List α)))
  (doc 'description "Sort list by a key function (Schwartzian transform)")
  (doc 'note "Uses merge sort internally for stability")
  (doc 'example "(sort-by-key string-length '(\"aaa\" \"b\" \"cc\")) => (\"b\" \"cc\" \"aaa\")")
  ;; Decorate
  (let* ([decorated (map (lambda (x) (cons (key-fn x) x)) lst)]
         ;; Sort by key
         [sorted (merge-sort-by (lambda (a b) (< (car a) (car b))) decorated)])
    ;; Undecorate
    (map cdr sorted)))

(define (sort-by-key-desc key-fn lst)
  (doc 'type (-> (-> α β) (List α) (List α)))
  (doc 'description "Sort list by key function in descending order")
  (let* ([decorated (map (lambda (x) (cons (key-fn x) x)) lst)]
         [sorted (merge-sort-by (lambda (a b) (> (car a) (car b))) decorated)])
    (map cdr sorted)))

(doc 'section 'descending-variants)

(define (sort-desc lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Sort list in descending order")
  (merge-sort-by > lst))

(define (quicksort-desc lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Sort list in descending order using quicksort")
  (quicksort-by > lst))

(doc 'section 'utilities)

(define (sorted? lst)
  (doc 'type (-> (List α) Boolean))
  (doc 'description "Check if list is sorted in ascending order")
  (sorted-by? < lst))

(define (sorted-by? cmp lst)
  (doc 'type (-> (-> α α Boolean) (List α) Boolean))
  (doc 'description "Check if list is sorted according to comparator")
  (or (null? lst)
      (null? (cdr lst))
      (and (not (cmp (cadr lst) (car lst)))  ; car <= cadr
           (sorted-by? cmp (cdr lst)))))

(define (sorted-desc? lst)
  (doc 'type (-> (List α) Boolean))
  (doc 'description "Check if list is sorted in descending order")
  (sorted-by? > lst))

(define (unique-sorted lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Remove adjacent duplicates from a sorted list")
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (if (equal? (car lst) (cadr lst))
          (unique-sorted (cdr lst))
          (cons (car lst) (unique-sorted (cdr lst))))))

(define (sort-unique lst)
  (doc 'type (-> (List α) (List α)))
  (doc 'description "Sort list and remove duplicates")
  (unique-sorted (sort lst)))

(define (nth-smallest n lst)
  (doc 'type (-> Nat (List α) α))
  (doc 'description "Find the nth smallest element (0-indexed)")
  (doc 'note "Uses quickselect for O(n) average performance")
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

(define (median lst)
  (doc 'type (-> (List α) α))
  (doc 'description "Find the median element")
  (let ([len (length lst)])
    (if (= len 0)
        (error 'median "Cannot find median of empty list")
        (nth-smallest (quotient len 2) lst))))

(define (min-element lst)
  (doc 'type (-> (List α) α))
  (doc 'description "Find minimum element")
  (doc 'complexity "O(n)")
  (if (null? lst)
      (error 'min-element "Cannot find min of empty list")
      (fold-left (lambda (m x) (if (< x m) x m))
                 (car lst)
                 (cdr lst))))

(define (max-element lst)
  (doc 'type (-> (List α) α))
  (doc 'description "Find maximum element")
  (doc 'complexity "O(n)")
  (if (null? lst)
      (error 'max-element "Cannot find max of empty list")
      (fold-left (lambda (m x) (if (> x m) x m))
                 (car lst)
                 (cdr lst))))

(define (min-by key-fn lst)
  (doc 'type (-> (-> α β) (List α) α))
  (doc 'description "Find element with minimum key value")
  (if (null? lst)
      (error 'min-by "Cannot find min of empty list")
      (car (fold-left (lambda (best x)
                        (let ([k (key-fn x)])
                          (if (< k (cdr best))
                              (cons x k)
                              best)))
                      (cons (car lst) (key-fn (car lst)))
                      (cdr lst)))))

(define (max-by key-fn lst)
  (doc 'type (-> (-> α β) (List α) α))
  (doc 'description "Find element with maximum key value")
  (if (null? lst)
      (error 'max-by "Cannot find max of empty list")
      (car (fold-left (lambda (best x)
                        (let ([k (key-fn x)])
                          (if (> k (cdr best))
                              (cons x k)
                              best)))
                      (cons (car lst) (key-fn (car lst)))
                      (cdr lst)))))

(doc 'section 'top-k)

(define (take-sorted n lst)
  (doc 'type (-> Nat (List α) (List α)))
  (doc 'description "Get first n elements of sorted list")
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take-sorted (- n 1) (cdr lst)))))

(define (top-k k lst)
  (doc 'type (-> Nat (List α) (List α)))
  (doc 'description "Get k largest elements (in descending order)")
  (take-sorted k (sort-desc lst)))

(define (bottom-k k lst)
  (doc 'type (-> Nat (List α) (List α)))
  (doc 'description "Get k smallest elements (in ascending order)")
  (take-sorted k (sort lst)))

(doc 'section 'vector-sorting)

(define (vector-swap! vec i j)
  (doc 'type (-> (Vector α) Nat Nat Void))
  (doc 'description "Swap elements at indices i and j in-place")
  (let ([tmp (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))

(define (vector-sort-by! cmp vec)
  (doc 'type (-> (-> α α Boolean) (Vector α) Void))
  (doc 'description "Sort vector in-place using heapsort with custom comparator")
  (doc 'note "O(n log n) worst-case, O(1) extra space")
  (doc 'note "Transparent implementation using only vector-ref/vector-set!")
  (let ([n (vector-length vec)])
    (define (sift-down! root end)
      (let loop ([r root])
        (let ([left (+ (* 2 r) 1)])
          (when (< left end)
            (let* ([right (+ left 1)]
                   [largest r]
                   [largest (if (cmp (vector-ref vec largest)
                                     (vector-ref vec left))
                                left largest)]
                   [largest (if (and (< right end)
                                     (cmp (vector-ref vec largest)
                                          (vector-ref vec right)))
                                right largest)])
              (unless (= largest r)
                (vector-swap! vec r largest)
                (loop largest)))))))
    ;; Build max-heap
    (let loop ([start (- (quotient n 2) 1)])
      (when (>= start 0)
        (sift-down! start n)
        (loop (- start 1))))
    ;; Extract elements from heap
    (let loop ([end (- n 1)])
      (when (> end 0)
        (vector-swap! vec 0 end)
        (sift-down! 0 end)
        (loop (- end 1))))))

(define (vector-sort-by cmp vec)
  (doc 'type (-> (-> α α Boolean) (Vector α) (Vector α)))
  (doc 'description "Return new sorted vector using custom comparator")
  (doc 'note "Non-destructive: original vector is unchanged")
  (let* ([n (vector-length vec)]
         [copy (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (vector-set! copy i (vector-ref vec i)))
    (vector-sort-by! cmp copy)
    copy))
