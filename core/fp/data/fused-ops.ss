;;; core/fp/data/fused-ops.ss --- Fused Operation Primitives
;;;
;;; Efficient single-pass implementations of operations that fusion rules
;;; rewrite TO. These eliminate intermediate data structures by combining
;;; multiple traversals into one.
;;;
;;; When the rewrite engine applies rules like:
;;;   (map f (filter p xs)) -> (filter-map p f xs)
;;; the fused form must actually execute efficiently. This module provides
;;; those implementations.
;;;
;;; Key Operations:
;;;   - filter-map     : Filter then map in single traversal
;;;   - map-filter     : Map then filter in single traversal
;;;   - fold-filter    : Fold with integrated filtering
;;;   - flatMap        : Map and flatten in single pass (list monad bind)
;;;   - foldr-map      : Right fold with integrated mapping
;;;   - foldr-filter   : Right fold with integrated filtering
;;;   - take-while-map : Take-while with mapping in single pass
;;;   - drop-while-map : Drop-while with mapping in single pass
;;;
;;; Stream Variants:
;;;   - stream-filter-map : Lazy filter-map for streams
;;;   - stream-map-filter : Lazy map-filter for streams
;;;   - stream-take-map   : Take with mapping fused
;;;   - stream-drop-map   : Drop with mapping fused
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/fp/data/stream.ss (for stream operations)
;;;
;;; Test suite: core/fp/data/test-fused-ops.ss

(load "core/base/prelude.ss")
(load "core/fp/data/stream.ss")

;;; ============================================================
;;; List Fused Operations
;;; ============================================================

;;; filter-map : (a -> Bool) x (a -> b) x (List a) -> (List b)
;;; Combined filter and map in single traversal.
;;; Only applies f to elements that satisfy pred.
;;; Semantically equivalent to: (map f (filter pred xs))
;;;
;;; Example:
;;;   (filter-map even? (lambda (x) (* x 2)) '(1 2 3 4 5))
;;;   => '(4 8)  ; only evens, doubled
(define (filter-map pred f xs)
  (let loop ([xs xs] [acc '()])
       (cond
        [(null? xs) (reverse acc)]
        [(pred (car xs))
         (loop (cdr xs) (cons (f (car xs)) acc))]
        [else
         (loop (cdr xs) acc)])))

;;; map-filter : (a -> b) x (b -> Bool) x (List a) -> (List b)
;;; Map then filter in single traversal.
;;; Applies f to each element, keeps only results satisfying pred.
;;; Semantically equivalent to: (filter pred (map f xs))
;;;
;;; Example:
;;;   (map-filter (lambda (x) (* x x)) (lambda (y) (> y 10)) '(1 2 3 4 5))
;;;   => '(16 25)  ; squares > 10
(define (map-filter f pred xs)
  (let loop ([xs xs] [acc '()])
       (if (null? xs)
           (reverse acc)
           (let ([mapped (f (car xs))])
                (if (pred mapped)
                    (loop (cdr xs) (cons mapped acc))
                    (loop (cdr xs) acc))))))

;;; fold-filter : (b x a -> b) x b x (a -> Bool) x (List a) -> b
;;; Left fold with integrated filter.
;;; Only accumulates elements satisfying the predicate.
;;; Semantically equivalent to: (foldl f z (filter pred xs))
;;;
;;; Example:
;;;   (fold-filter + 0 even? '(1 2 3 4 5 6))
;;;   => 12  ; sum of evens
(define (fold-filter f z pred xs)
  (let loop ([xs xs] [acc z])
       (cond
        [(null? xs) acc]
        [(pred (car xs))
         (loop (cdr xs) (f acc (car xs)))]
        [else
         (loop (cdr xs) acc)])))

;;; fold-map : (b x c -> b) x b x (a -> c) x (List a) -> b
;;; Left fold with integrated map.
;;; Maps each element before folding.
;;; Semantically equivalent to: (foldl f z (map g xs))
;;;
;;; Example:
;;;   (fold-map + 0 (lambda (x) (* x x)) '(1 2 3))
;;;   => 14  ; sum of squares
(define (fold-map f z g xs)
  (let loop ([xs xs] [acc z])
       (if (null? xs)
           acc
           (loop (cdr xs) (f acc (g (car xs)))))))

;;; flatMap : (a -> (List b)) x (List a) -> (List b)
;;; Map and flatten in single pass.
;;; Also known as concatMap, bind (>>=) for the list monad.
;;; Semantically equivalent to: (flatten (map f xs))
;;;
;;; Example:
;;;   (flatMap (lambda (x) (list x x)) '(1 2 3))
;;;   => '(1 1 2 2 3 3)
(define (flatMap f xs)
  (let loop ([xs xs] [acc '()])
       (if (null? xs)
           (reverse acc)
           (let inner ([produced (f (car xs))] [acc acc])
                (if (null? produced)
                    (loop (cdr xs) acc)
                    (inner (cdr produced) (cons (car produced) acc)))))))

;;; flatMap/append : (a -> (List b)) x (List a) -> (List b)
;;; Alternative implementation using append (different space characteristics).
;;; Uses continuation-passing style to avoid multiple reverses.
(define (flatMap/append f xs)
  (if (null? xs)
      '()
      (append (f (car xs)) (flatMap/append f (cdr xs)))))

;;; ============================================================
;;; Right Fold Fused Variants
;;; ============================================================

;;; foldr-map : (c x b -> b) x b x (a -> c) x (List a) -> b
;;; Right fold with integrated map.
;;; Note: foldr has (element, accumulator) order.
;;; Semantically equivalent to: (foldr f z (map g xs))
;;;
;;; Example:
;;;   (foldr-map cons '() (lambda (x) (* x 2)) '(1 2 3))
;;;   => '(2 4 6)  ; doubling via foldr
(define (foldr-map f z g xs)
  (if (null? xs)
      z
      (f (g (car xs)) (foldr-map f z g (cdr xs)))))

;;; foldr-filter : (a x b -> b) x b x (a -> Bool) x (List a) -> b
;;; Right fold with integrated filter.
;;; Only folds elements satisfying the predicate.
;;; Semantically equivalent to: (foldr f z (filter pred xs))
;;;
;;; Example:
;;;   (foldr-filter cons '() even? '(1 2 3 4 5 6))
;;;   => '(2 4 6)  ; evens via foldr
(define (foldr-filter f z pred xs)
  (if (null? xs)
      z
      (if (pred (car xs))
          (f (car xs) (foldr-filter f z pred (cdr xs)))
          (foldr-filter f z pred (cdr xs)))))

;;; foldr-filter-map : (b x c -> c) x c x (a -> Bool) x (a -> b) x (List a) -> c
;;; Right fold with both filtering and mapping.
;;; Semantically equivalent to: (foldr f z (map g (filter pred xs)))
(define (foldr-filter-map f z pred g xs)
  (if (null? xs)
      z
      (if (pred (car xs))
          (f (g (car xs)) (foldr-filter-map f z pred g (cdr xs)))
          (foldr-filter-map f z pred g (cdr xs)))))

;;; ============================================================
;;; Take/Drop Fused Variants
;;; ============================================================

;;; take-map : Nat x (a -> b) x (List a) -> (List b)
;;; Take n elements while mapping.
;;; Semantically equivalent to: (map f (take n xs))
;;; Also equivalent to: (take n (map f xs)) by commutativity
;;;
;;; Example:
;;;   (take-map 3 (lambda (x) (* x 2)) '(1 2 3 4 5))
;;;   => '(2 4 6)
(define (take-map n f xs)
  (let loop ([n n] [xs xs] [acc '()])
       (if (or (<= n 0) (null? xs))
           (reverse acc)
           (loop (- n 1) (cdr xs) (cons (f (car xs)) acc)))))

;;; drop-map : Nat x (a -> b) x (List a) -> (List b)
;;; Drop n elements then map the rest.
;;; Semantically equivalent to: (map f (drop n xs))
;;;
;;; Example:
;;;   (drop-map 2 (lambda (x) (* x 2)) '(1 2 3 4 5))
;;;   => '(6 8 10)
(define (drop-map n f xs)
  (let loop ([n n] [xs xs])
       (cond
        [(null? xs) '()]
        [(<= n 0) (map f xs)]
        [else (loop (- n 1) (cdr xs))])))

;;; take-while-map : (a -> Bool) x (a -> b) x (List a) -> (List b)
;;; Take elements while predicate holds, mapping each.
;;; Semantically equivalent to: (map f (take-while pred xs))
;;;
;;; Example:
;;;   (take-while-map (lambda (x) (< x 4)) (lambda (x) (* x 2)) '(1 2 3 4 5))
;;;   => '(2 4 6)
(define (take-while-map pred f xs)
  (let loop ([xs xs] [acc '()])
       (cond
        [(null? xs) (reverse acc)]
        [(pred (car xs))
         (loop (cdr xs) (cons (f (car xs)) acc))]
        [else (reverse acc)])))

;;; drop-while-map : (a -> Bool) x (a -> b) x (List a) -> (List b)
;;; Drop elements while predicate holds, then map the rest.
;;; Semantically equivalent to: (map f (drop-while pred xs))
;;;
;;; Example:
;;;   (drop-while-map (lambda (x) (< x 3)) (lambda (x) (* x 2)) '(1 2 3 4 5))
;;;   => '(6 8 10)
(define (drop-while-map pred f xs)
  (let loop ([xs xs])
       (cond
        [(null? xs) '()]
        [(pred (car xs)) (loop (cdr xs))]
        [else (map f xs)])))

;;; ============================================================
;;; Multi-Filter Operations
;;; ============================================================

;;; filter-filter : (a -> Bool) x (a -> Bool) x (List a) -> (List a)
;;; Two filters in single pass.
;;; Semantically equivalent to: (filter p2 (filter p1 xs))
;;; Also equivalent to: (filter (lambda (x) (and (p1 x) (p2 x))) xs)
(define (filter-filter p1 p2 xs)
  (let loop ([xs xs] [acc '()])
       (if (null? xs)
           (reverse acc)
           (let ([x (car xs)])
                (if (and (p1 x) (p2 x))
                    (loop (cdr xs) (cons x acc))
                    (loop (cdr xs) acc))))))

;;; filter-all : (List (a -> Bool)) x (List a) -> (List a)
;;; Filter by conjunction of all predicates.
(define (filter-all preds xs)
  (filter (lambda (x) (andmap (lambda (p) (p x)) preds)) xs))

;;; filter-any : (List (a -> Bool)) x (List a) -> (List a)
;;; Filter by disjunction of predicates.
(define (filter-any preds xs)
  (filter (lambda (x) (ormap (lambda (p) (p x)) preds)) xs))

;;; ============================================================
;;; Stream Fused Operations
;;; ============================================================

;;; stream-filter-map : (a -> Bool) x (a -> b) x (Stream a) -> (Stream b)
;;; Lazy filter-map for streams.
;;; Combines stream-filter and stream-map in single traversal.
;;; Semantically equivalent to: (stream-map f (stream-filter pred s))
(define (stream-filter-map pred f s)
  (cond
   [(stream-nil? s) stream-nil]
   [(pred (stream-head s))
    (stream-cons (f (stream-head s))
                 (lambda () (stream-filter-map pred f (stream-tail s))))]
   [else (stream-filter-map pred f (stream-tail s))]))

;;; stream-map-filter : (a -> b) x (b -> Bool) x (Stream a) -> (Stream b)
;;; Lazy map-filter for streams.
;;; Maps each element, keeps only those where result satisfies predicate.
;;; Semantically equivalent to: (stream-filter pred (stream-map f s))
(define (stream-map-filter f pred s)
  (cond
   [(stream-nil? s) stream-nil]
   [else
    (let ([mapped (f (stream-head s))])
         (if (pred mapped)
             (stream-cons mapped
                          (lambda () (stream-map-filter f pred (stream-tail s))))
             (stream-map-filter f pred (stream-tail s))))]))

;;; stream-take-map : Nat x (a -> b) x (Stream a) -> (Stream b)
;;; Take n elements from stream while mapping.
;;; Semantically equivalent to: (stream-map f (stream-take n s))
(define (stream-take-map n f s)
  (if (or (<= n 0) (stream-nil? s))
      stream-nil
      (stream-cons (f (stream-head s))
                   (lambda () (stream-take-map (- n 1) f (stream-tail s))))))

;;; stream-drop-map : Nat x (a -> b) x (Stream a) -> (Stream b)
;;; Drop n elements then map the rest.
;;; Semantically equivalent to: (stream-map f (stream-drop n s))
(define (stream-drop-map n f s)
  (if (or (<= n 0) (stream-nil? s))
      (stream-map f s)
      (stream-drop-map (- n 1) f (stream-tail s))))

;;; stream-flatMap : (a -> (Stream b)) x (Stream a) -> (Stream b)
;;; Monadic bind for streams, fusing map and flatten.
;;; Uses fair interleaving to handle infinite inner streams.
;;; Semantically equivalent to: (stream-flatten (stream-map f s))
(define (stream-flatMap f s)
  (stream-flatten (stream-map f s)))

;;; stream-fold-map : (b x c -> b) x b x (a -> c) x Nat x (Stream a) -> b
;;; Stream fold with integrated mapping, using fuel for termination.
;;; Semantically equivalent to: (stream-fold f z n (stream-map g s))
(define (stream-fold-map f z g n s)
  (let loop ([acc z] [n n] [s s])
       (if (or (<= n 0) (stream-nil? s))
           acc
           (loop (f acc (g (stream-head s))) (- n 1) (stream-tail s)))))

;;; stream-fold-filter : (b x a -> b) x b x (a -> Bool) x Nat x (Stream a) -> b
;;; Stream fold with integrated filtering.
;;; Note: May consume more than n elements if many are filtered out.
;;; fuel limits total elements examined, not elements accumulated.
(define (stream-fold-filter f z pred fuel s)
  (let loop ([acc z] [fuel fuel] [s s])
       (cond
        [(<= fuel 0) acc]
        [(stream-nil? s) acc]
        [(pred (stream-head s))
         (loop (f acc (stream-head s)) (- fuel 1) (stream-tail s))]
        [else
         (loop acc (- fuel 1) (stream-tail s))])))

;;; ============================================================
;;; Specialized Fused Combinators
;;; ============================================================

;;; count-if : (a -> Bool) x (List a) -> Nat
;;; Count elements satisfying predicate (fused length + filter).
;;; Semantically equivalent to: (length (filter pred xs))
(define (count-if pred xs)
  (fold-filter (lambda (acc _) (+ acc 1)) 0 pred xs))

;;; sum-map : (a -> Num) x (List a) -> Num
;;; Sum of mapped values (fused fold + map).
;;; Semantically equivalent to: (apply + (map f xs))
(define (sum-map f xs)
  (fold-map + 0 f xs))

;;; product-map : (a -> Num) x (List a) -> Num
;;; Product of mapped values.
;;; Semantically equivalent to: (apply * (map f xs))
(define (product-map f xs)
  (fold-map * 1 f xs))

;;; maximum-by : (a -> Num) x (List a) -> a
;;; Find element that maximizes key function.
;;; Returns first element if list is singleton.
;;; On ties, returns first occurrence.
(define (maximum-by key xs)
  (if (null? xs)
      (error 'maximum-by "empty list")
      (let loop ([best (car xs)] [best-key (key (car xs))] [xs (cdr xs)])
           (if (null? xs)
               best
               (let ([k (key (car xs))])
                    (if (> k best-key)
                        (loop (car xs) k (cdr xs))
                        (loop best best-key (cdr xs))))))))

;;; minimum-by : (a -> Num) x (List a) -> a
;;; Find element that minimizes key function.
;;; On ties, returns first occurrence.
(define (minimum-by key xs)
  (if (null? xs)
      (error 'minimum-by "empty list")
      (let loop ([best (car xs)] [best-key (key (car xs))] [xs (cdr xs)])
           (if (null? xs)
               best
               (let ([k (key (car xs))])
                    (if (< k best-key)
                        (loop (car xs) k (cdr xs))
                        (loop best best-key (cdr xs))))))))

;;; find-map : (a -> (Maybe b)) x (List a) -> (Maybe b)
;;; Find first element where f returns (just ...).
;;; Fuses find with map transformation.
(define (find-map f xs)
  (let loop ([xs xs])
       (if (null? xs)
           nothing
           (let ([result (f (car xs))])
                (if (nothing? result)
                    (loop (cdr xs))
                    result)))))

;;; partition-map : (a -> (Either b c)) x (List a) -> (Pair (List b) (List c))
;;; Partition while transforming.
;;; f returns (left x) for first list, (right x) for second.
(define (partition-map f xs)
  (let loop ([xs xs] [lefts '()] [rights '()])
       (if (null? xs)
           (cons (reverse lefts) (reverse rights))
           (let ([result (f (car xs))])
                (cond
                 [(and (pair? result) (eq? (car result) 'left))
                  (loop (cdr xs) (cons (cadr result) lefts) rights)]
                 [(and (pair? result) (eq? (car result) 'right))
                  (loop (cdr xs) lefts (cons (cadr result) rights))]
                 [else
                  ;; Default: treat as right
                  (loop (cdr xs) lefts (cons result rights))])))))

;;; ============================================================
;;; Index-Aware Fused Operations
;;; ============================================================

;;; filter-mapi : (Nat x a -> Bool) x (Nat x a -> b) x (List a) -> (List b)
;;; Filter-map with index available to both functions.
(define (filter-mapi pred f xs)
  (let loop ([xs xs] [i 0] [acc '()])
       (cond
        [(null? xs) (reverse acc)]
        [(pred i (car xs))
         (loop (cdr xs) (+ i 1) (cons (f i (car xs)) acc))]
        [else
         (loop (cdr xs) (+ i 1) acc)])))

;;; mapi : (Nat x a -> b) x (List a) -> (List b)
;;; Map with index. Fuses enumerate + map.
;;; Semantically equivalent to: (map (lambda (p) (f (car p) (cdr p))) (enumerate xs))
(define (mapi f xs)
  (let loop ([xs xs] [i 0] [acc '()])
       (if (null? xs)
           (reverse acc)
           (loop (cdr xs) (+ i 1) (cons (f i (car xs)) acc)))))

;;; filteri : (Nat x a -> Bool) x (List a) -> (List a)
;;; Filter with index available.
(define (filteri pred xs)
  (let loop ([xs xs] [i 0] [acc '()])
       (cond
        [(null? xs) (reverse acc)]
        [(pred i (car xs))
         (loop (cdr xs) (+ i 1) (cons (car xs) acc))]
        [else
         (loop (cdr xs) (+ i 1) acc)])))

;;; foldli : (b x Nat x a -> b) x b x (List a) -> b
;;; Left fold with index available.
(define (foldli f z xs)
  (let loop ([xs xs] [i 0] [acc z])
       (if (null? xs)
           acc
           (loop (cdr xs) (+ i 1) (f acc i (car xs))))))

;;; ============================================================
;;; Chunking Fused Operations
;;; ============================================================

;;; chunk-map : Nat x ((List a) -> b) x (List a) -> (List b)
;;; Chunk list and map over chunks.
;;; Semantically equivalent to: (map f (chunk n xs))
(define (chunk-map n f xs)
  (if (or (null? xs) (<= n 0))
      '()
      (cons (f (take n xs))
            (chunk-map n f (drop n xs)))))

;;; window-map : Nat x ((List a) -> b) x (List a) -> (List b)
;;; Sliding window with mapping.
;;; Produces (- (length xs) (- n 1)) results for window size n.
(define (window-map n f xs)
  (let loop ([xs xs] [acc '()])
       (if (< (length xs) n)
           (reverse acc)
           (loop (cdr xs) (cons (f (take n xs)) acc)))))

;;; ============================================================
;;; Parallel-Ready Fused Operations
;;; ============================================================
;;; These don't actually parallelize but have signatures amenable
;;; to parallel execution (pure, associative reductions).

;;; reduce-map : (b x b -> b) x b x (a -> b) x (List a) -> b
;;; Map then reduce with an associative operation.
;;; For parallel execution, the combiner must be associative.
(define (reduce-map combine identity f xs)
  (fold-left combine identity (map f xs)))

;;; partition-reduce : (a -> Bool) x (b x a -> b) x (c x a -> c) x b x c x (List a) -> (Pair b c)
;;; Single pass: partition elements and reduce each partition.
(define (partition-reduce pred f-yes f-no z-yes z-no xs)
  (let loop ([xs xs] [yes z-yes] [no z-no])
       (if (null? xs)
           (cons yes no)
           (let ([x (car xs)])
                (if (pred x)
                    (loop (cdr xs) (f-yes yes x) no)
                    (loop (cdr xs) yes (f-no no x)))))))
