;;; sort.sexp — Development tasks for lattice/data/sort.ss
;;;
;;; Module: sort (tier 0, depends only on prelude)
;;; 348 lines, ~25 exported functions
;;; The most-depended-upon module in the lattice (34 downstream).
;;;
;;; Test file: lattice/data/test-sort.ss (253 lines, ~71 tests)
;;;
;;; Git history:
;;;   d82c01f9 feat(data): Add sorting algorithms module
;;;   89839988 docs(lattice): Convert data files to doc forms
;;;   2aba9e45 refactor(lattice): Replace opaque vector-sort with transparent heapsort
;;;   596c1d4d feat(module): migrate data/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement the merge operation
  ;; ──────────────────────────────────────────────
  ;; The core building block of merge sort. The critical subtlety is stability:
  ;; when elements compare equal, take from the LEFT list first so that
  ;; original ordering is preserved. Getting the comparison direction wrong
  ;; produces a sort that works on numbers but silently breaks stability.

  (task
    (id "data/sort/merge-impl")
    (module sort)
    (tier 0)
    (type implement)
    (description "Implement the merge operation that combines two sorted lists into one sorted list. The function takes two sorted lists and a comparator (cmp), and must be STABLE: when two elements compare equal (neither cmp(a,b) nor cmp(b,a) is true), the element from the first list (xs) must come before the element from the second list (ys). This preserves original ordering of equal elements through merge sort.")
    (context "Available: cons, car, cdr, null?, list primitives from prelude. The comparator cmp is a strict less-than function (e.g., <). Two elements are equal when (not (cmp a b)) AND (not (cmp b a)).")
    (before "(define (merge xs ys cmp)
  (doc 'type (-> (List a) (List a) (-> a a Boolean) (List a)))
  (doc 'description \"Merge two sorted lists into one sorted list\")
  (doc 'note \"For stability: when equal, take from left list first\")
  ;; TODO: implement
  (append xs ys))")
    (test-file "lattice/data/test-sort.ss")
    (test-forms ("(assert-equal '() (merge-sort '()))"
                 "(assert-equal '(1 2 2 3 3 3 4) (merge-sort '(3 2 1 3 2 4 3)))"
                 "(let ([sorted-items (merge-sort-by (lambda (a b) (< (cdr a) (cdr b))) '((b . 1) (a . 1) (d . 2) (c . 2)))]) (assert-equal 'b (caar sorted-items)))"
                 "(let ([sorted-items (merge-sort-by (lambda (a b) (< (cdr a) (cdr b))) '((b . 1) (a . 1) (d . 2) (c . 2)))]) (assert-equal 'd (car (caddr sorted-items))))"))
    (available-modules (prelude))
    (hints ("Base cases: if either list is empty, return the other"
            "For stability, the condition for taking from xs is: xs's head is NOT greater than ys's head"
            "Express 'not greater than' as (not (cmp (car ys) (car xs))) — this takes from xs when equal"))
    (reference-solution "(define (merge xs ys cmp)
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    [(not (cmp (car ys) (car xs)))
     (cons (car xs) (merge (cdr xs) ys cmp))]
    [else
     (cons (car ys) (merge xs (cdr ys) cmp))]))")
    (alternatives ())
    (source-commit "d82c01f9")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement three-way partition
  ;; ──────────────────────────────────────────────
  ;; Three-way partition is the key to efficient quicksort with duplicates.
  ;; The naive two-way partition degrades to O(n²) on all-equal input.
  ;; By separating equal elements into their own bucket, quicksort stays
  ;; efficient. This also appears in the Dutch National Flag problem.

  (task
    (id "data/sort/partition-impl")
    (module sort)
    (tier 0)
    (type implement)
    (description "Implement three-way partition: given a comparator cmp, a pivot element, and a list, split the list into three groups: (less-than-pivot, equal-to-pivot, greater-than-pivot). Return them as a list of three lists. Use the comparator to determine ordering: x < pivot when (cmp x pivot) is true, x > pivot when (cmp pivot x) is true, otherwise x = pivot. Preserve original order within each group (reverse accumulation, then reverse at the end).")
    (context "Available: prelude functions. cmp is a strict less-than function.")
    (before "(define (partition cmp pivot lst)
  (doc 'type (-> (-> a a Boolean) a (List a) (List (List a) (List a) (List a))))
  (doc 'description \"Partition list into (less-than, equal-to, greater-than) pivot\")
  (doc 'note \"Three-way partition handles duplicates efficiently\")
  ;; TODO: implement
  (list lst '() '()))")
    (test-file "lattice/data/test-sort.ss")
    (test-forms ("(assert-equal '() (quicksort '()))"
                 "(assert-equal '(5 5 5 5 5) (quicksort '(5 5 5 5 5)))"
                 "(assert-equal '(1 2 2 3 3 3 4) (quicksort '(3 2 1 3 2 4 3)))"))
    (available-modules (prelude))
    (hints ("Use a tail-recursive loop with three accumulators: lt, eq, gt"
            "Check (cmp x pivot) for less-than, (cmp pivot x) for greater-than, else equal"
            "Reverse each accumulator at the end to preserve input order"))
    (reference-solution "(define (partition cmp pivot lst)
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
             (loop (cdr xs) lt (cons x eq) gt)])))))")
    (alternatives ())
    (source-commit "d82c01f9")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement Schwartzian transform (sort-by-key)
  ;; ──────────────────────────────────────────────
  ;; Named after Randal Schwartz (Perl), this pattern avoids recomputing
  ;; the key function on every comparison. Decorate → Sort → Undecorate.
  ;; O(n) key computations instead of O(n log n).

  (task
    (id "data/sort/sort-by-key-impl")
    (module sort)
    (tier 0)
    (type implement)
    (description "Implement sort-by-key using the Schwartzian transform (decorate-sort-undecorate). Given a key function and a list, sort the list by the values that key-fn produces, without calling key-fn more than once per element. Steps: (1) decorate each element x as a pair (key-fn x . x), (2) sort the decorated list by the car of each pair using merge-sort-by, (3) undecorate by extracting the cdr of each pair.")
    (context "Available: prelude functions, merge-sort-by (already defined in this module). merge-sort-by takes a comparator and a list.")
    (before "(define (sort-by-key key-fn lst)
  (doc 'type (-> (-> a b) (List a) (List a)))
  (doc 'description \"Sort list by a key function (Schwartzian transform)\")
  (doc 'note \"Uses merge sort internally for stability\")
  ;; TODO: implement decorate-sort-undecorate
  lst)")
    (test-file "lattice/data/test-sort.ss")
    (test-forms ("(assert-equal '(\"a\" \"bb\" \"ccc\" \"dddd\") (sort-by-key string-length '(\"ccc\" \"a\" \"dddd\" \"bb\")))"
                 "(assert-equal '(1 -2 3 -4 5) (sort-by-key (lambda (x) (abs x)) '(5 -2 3 -4 1)))"))
    (available-modules (prelude))
    (hints ("Decorate: (map (lambda (x) (cons (key-fn x) x)) lst)"
            "Sort by car of pairs: merge-sort-by with (lambda (a b) (< (car a) (car b)))"
            "Undecorate: (map cdr sorted-decorated)"))
    (reference-solution "(define (sort-by-key key-fn lst)
  (let* ([decorated (map (lambda (x) (cons (key-fn x) x)) lst)]
         [sorted (merge-sort-by (lambda (a b) (< (car a) (car b))) decorated)])
    (map cdr sorted)))")
    (alternatives ())
    (source-commit "d82c01f9")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Fix unstable merge
  ;; ──────────────────────────────────────────────
  ;; This is the kind of bug that passes all numeric sorting tests but
  ;; fails on stability. The broken merge uses (cmp (car xs) (car ys))
  ;; which takes from xs only when xs is STRICTLY LESS — when equal,
  ;; it takes from ys, breaking the stability guarantee.

  (task
    (id "data/sort/fix-unstable-merge")
    (module sort)
    (tier 0)
    (type fix)
    (description "The merge function below has a subtle bug: it produces correct sorted output for numbers, but it is NOT stable — equal elements from the first list don't always come before equal elements from the second list. Find and fix the comparison logic so that merge is stable. Hint: the stability tests use pairs sorted by their cdr values, and check that pairs with equal cdr values preserve their original xs-before-ys ordering.")
    (context "Available: prelude. The comparator cmp is strict less-than. The merge is used by merge-sort-by which promises stability.")
    (before "(define (merge xs ys cmp)
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    [(cmp (car xs) (car ys))
     (cons (car xs) (merge (cdr xs) ys cmp))]
    [else
     (cons (car ys) (merge xs (cdr ys) cmp))]))")
    (test-file "lattice/data/test-sort.ss")
    (test-forms ("(assert-equal '(1 2 2 3 3 3 4) (merge-sort '(3 2 1 3 2 4 3)))"
                 "(let ([s (merge-sort-by (lambda (a b) (< (cdr a) (cdr b))) '((b . 1) (a . 1) (d . 2) (c . 2)))]) (assert-equal 'b (caar s)))"
                 "(let ([s (merge-sort-by (lambda (a b) (< (cdr a) (cdr b))) '((b . 1) (a . 1) (d . 2) (c . 2)))]) (assert-equal 'a (car (cadr s))))"
                 "(let ([s (merge-sort-by (lambda (a b) (< (cdr a) (cdr b))) '((b . 1) (a . 1) (d . 2) (c . 2)))]) (assert-equal 'd (car (caddr s))))"
                 "(let ([s (merge-sort-by (lambda (a b) (< (cdr a) (cdr b))) '((b . 1) (a . 1) (d . 2) (c . 2)))]) (assert-equal 'c (car (cadddr s))))"))
    (available-modules (prelude))
    (hints ("The bug is in when we take from xs vs ys"
            "Currently takes from xs only when xs < ys — but we also want xs when xs = ys"
            "Change the condition to: take from xs when ys is NOT less than xs: (not (cmp (car ys) (car xs)))"))
    (reference-solution "(define (merge xs ys cmp)
  (cond
    [(null? xs) ys]
    [(null? ys) xs]
    [(not (cmp (car ys) (car xs)))
     (cons (car xs) (merge (cdr xs) ys cmp))]
    [else
     (cons (car ys) (merge xs (cdr ys) cmp))]))")
    (alternatives ())
    (source-commit #f)
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement quickselect (nth-smallest)
  ;; ──────────────────────────────────────────────
  ;; Quickselect reuses the three-way partition to find the k-th order
  ;; statistic in O(n) average time without sorting the full list.
  ;; The three-way partition is critical: it handles duplicates correctly
  ;; and ensures the equal-to-pivot elements are counted properly.

  (task
    (id "data/sort/quickselect-impl")
    (module sort)
    (tier 0)
    (type implement)
    (description "Implement nth-smallest using the quickselect algorithm. Given a 0-indexed position n and a list, find the element that would be at position n if the list were sorted in ascending order. Use the partition function (already available) which returns (less-than, equal-to, greater-than) sublists. The algorithm: pick a pivot (use car of list), partition the rest, then recurse into the appropriate sublist based on the sizes of the partitions. Error on empty list.")
    (context "Available: prelude, partition (returns (lt eq gt) as a list of three lists for a given comparator, pivot, and list). Use < as the comparator for ascending order.")
    (before "(define (nth-smallest n lst)
  (doc 'type (-> Nat (List a) a))
  (doc 'description \"Find the nth smallest element (0-indexed)\")
  (doc 'note \"Uses quickselect for O(n) average performance\")
  ;; partition is available: (partition < pivot rest) => (lt eq gt)
  ;; TODO: implement quickselect
  (error 'nth-smallest \"Not implemented\"))")
    (test-file "lattice/data/test-sort.ss")
    (test-forms ("(assert-equal 1 (nth-smallest 0 '(5 2 8 1 9 3 7)))"
                 "(assert-equal 2 (nth-smallest 1 '(5 2 8 1 9 3 7)))"
                 "(assert-equal 3 (nth-smallest 2 '(5 2 8 1 9 3 7)))"
                 "(assert-equal 9 (nth-smallest 6 '(5 2 8 1 9 3 7)))"
                 "(assert-equal 2 (nth-smallest 2 '(3 1 2 1 3)))"
                 "(assert-equal 5 (median '(5 2 8 1 9 3 7)))"
                 "(assert-error (nth-smallest 0 '()))"))
    (available-modules (prelude))
    (hints ("Pivot is (car lst), partition the (cdr lst)"
            "If n < (length lt), recurse into lt"
            "If n < (length lt) + 1 + (length eq), the answer is the pivot"
            "Otherwise recurse into gt with n adjusted by (length lt) + 1 + (length eq)"))
    (reference-solution "(define (nth-smallest n lst)
  (if (null? lst)
      (error 'nth-smallest \"List is empty\")
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
           (nth-smallest (- n lt-len 1 (length eq)) gt)]))))")
    (alternatives ())
    (source-commit "d82c01f9")
    (difficulty 5))

  ;; ──────────────────────────────────────────────
  ;; Task 6: Implement in-place heapsort for vectors
  ;; ──────────────────────────────────────────────
  ;; From real git history: commit 2aba9e45. Chez's built-in vector-sort
  ;; is a C-implemented opaque function invisible to fuel accounting and
  ;; codegen. This task replaces it with a transparent heapsort using
  ;; only vector-ref and vector-set!.
  ;;
  ;; Heapsort: O(n log n) worst case, O(1) extra space (in-place).
  ;; Two phases: build a max-heap, then repeatedly extract the maximum.

  (task
    (id "data/sort/heapsort-vector-impl")
    (module sort)
    (tier 0)
    (type extend)
    (description "Add in-place vector sorting to the sort module using heapsort. The Fold requires transparent implementations (no opaque built-ins) for fuel accounting and codegen. Implement vector-sort-by! that sorts a vector in-place using a custom comparator. Use the heapsort algorithm: (1) build a max-heap by sifting down from the middle, (2) repeatedly swap the root (maximum) to the end and sift down to restore the heap property. You'll need a helper vector-swap! and an internal sift-down! procedure. Only use vector-ref, vector-set!, and vector-length — no list conversion.")
    (context "Available: prelude, vector-ref, vector-set!, vector-length, make-vector. The module already has list-based sorting. This extends it to vectors.")
    (before ";; Add to sort.ss after the list-sorting functions:
;;
;; (doc 'section 'vector-sorting)
;;
;; vector-swap! — swap two elements in a vector
;; vector-sort-by! — sort vector in-place with custom comparator
;;
;; Must use heapsort (O(n log n) worst-case, O(1) extra space).
;; Only vector-ref/vector-set! allowed — no list conversion, no built-in sort.")
    (test-file "lattice/data/test-sort.ss")
    (test-forms ("(let ([v (vector 5 2 8 1 9 3 7)]) (vector-sort-by! < v) (assert-equal '#(1 2 3 5 7 8 9) v))"
                 "(let ([v (vector 1)]) (vector-sort-by! < v) (assert-equal '#(1) v))"
                 "(let ([v (vector 3 1 2 1 3)]) (vector-sort-by! < v) (assert-equal '#(1 1 2 3 3) v))"
                 "(let ([v (vector 5 2 8 1 9 3 7)]) (vector-sort-by! > v) (assert-equal '#(9 8 7 5 3 2 1) v))"
                 "(let ([v (make-vector 0)]) (vector-sort-by! < v) (assert-equal '#() v))"))
    (available-modules (prelude))
    (hints ("Implement vector-swap! first: save vec[i], set vec[i]=vec[j], set vec[j]=saved"
            "sift-down! takes a root index and end index, finds the largest of root and its children, swaps if needed, and recurses"
            "Build phase: sift-down from index (n/2 - 1) down to 0"
            "Extract phase: swap vec[0] with vec[end], sift-down on the reduced heap, decrement end"))
    (reference-solution "(define (vector-swap! vec i j)
  (let ([tmp (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))

(define (vector-sort-by! cmp vec)
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
        (loop (- end 1))))))")
    (alternatives ())
    (source-commit "2aba9e45")
    (difficulty 6))

) ;; end tasks
