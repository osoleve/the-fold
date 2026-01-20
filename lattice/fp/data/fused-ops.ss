(load "core/base/prelude.ss")
(load "lattice/fp/data/stream.ss")

(doc 'module 'fused-ops)
(doc 'description "Fused Operation Primitives - Efficient single-pass implementations of operations that fusion rules rewrite TO. These eliminate intermediate data structures by combining multiple traversals into one.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "When the rewrite engine applies rules like (map f (filter p xs)) -> (filter-map p f xs), the fused form must actually execute efficiently. This module provides those implementations.")

(doc 'section 'list-fused-operations)

(define (filter-map pred f xs)
  (doc 'type '(-> (-> a Bool) (-> a b) (List a) (List b)))
  (doc 'description "Combined filter and map in single traversal. Only applies f to elements that satisfy pred. Semantically equivalent to: (map f (filter pred xs))")
  (let loop ([xs xs] [acc '()])
       (cond
        [(null? xs) (reverse acc)]
        [(pred (car xs))
         (loop (cdr xs) (cons (f (car xs)) acc))]
        [else
         (loop (cdr xs) acc)])))

(define (map-filter f pred xs)
  (doc 'type '(-> (-> a b) (-> b Bool) (List a) (List b)))
  (doc 'description "Map then filter in single traversal. Applies f to each element, keeps only results satisfying pred. Semantically equivalent to: (filter pred (map f xs))")
  (let loop ([xs xs] [acc '()])
       (if (null? xs)
           (reverse acc)
           (let ([mapped (f (car xs))])
                (if (pred mapped)
                    (loop (cdr xs) (cons mapped acc))
                    (loop (cdr xs) acc))))))

(define (fold-filter f z pred xs)
  (doc 'type '(-> (-> b a b) b (-> a Bool) (List a) b))
  (doc 'description "Left fold with integrated filter. Only accumulates elements satisfying the predicate. Semantically equivalent to: (foldl f z (filter pred xs))")
  (let loop ([xs xs] [acc z])
       (cond
        [(null? xs) acc]
        [(pred (car xs))
         (loop (cdr xs) (f acc (car xs)))]
        [else
         (loop (cdr xs) acc)])))

(define (fold-map f z g xs)
  (doc 'type '(-> (-> b c b) b (-> a c) (List a) b))
  (doc 'description "Left fold with integrated map. Maps each element before folding. Semantically equivalent to: (foldl f z (map g xs))")
  (let loop ([xs xs] [acc z])
       (if (null? xs)
           acc
           (loop (cdr xs) (f acc (g (car xs)))))))

(define (flatMap f xs)
  (doc 'type '(-> (-> a (List b)) (List a) (List b)))
  (doc 'description "Map and flatten in single pass. Also known as concatMap, bind (>>=) for the list monad. Semantically equivalent to: (flatten (map f xs))")
  (let loop ([xs xs] [acc '()])
       (if (null? xs)
           (reverse acc)
           (let inner ([produced (f (car xs))] [acc acc])
                (if (null? produced)
                    (loop (cdr xs) acc)
                    (inner (cdr produced) (cons (car produced) acc)))))))

(define (flatMap/append f xs)
  (doc 'type '(-> (-> a (List b)) (List a) (List b)))
  (doc 'description "Alternative implementation using append (different space characteristics). Uses continuation-passing style to avoid multiple reverses")
  (if (null? xs)
      '()
      (append (f (car xs)) (flatMap/append f (cdr xs)))))

(doc 'section 'right-fold-fused-variants)

(define (foldr-map f z g xs)
  (doc 'type '(-> (-> c b b) b (-> a c) (List a) b))
  (doc 'description "Right fold with integrated map. Note: foldr has (element, accumulator) order. Semantically equivalent to: (foldr f z (map g xs))")
  (if (null? xs)
      z
      (f (g (car xs)) (foldr-map f z g (cdr xs)))))

(define (foldr-filter f z pred xs)
  (doc 'type '(-> (-> a b b) b (-> a Bool) (List a) b))
  (doc 'description "Right fold with integrated filter. Only folds elements satisfying the predicate. Semantically equivalent to: (foldr f z (filter pred xs))")
  (if (null? xs)
      z
      (if (pred (car xs))
          (f (car xs) (foldr-filter f z pred (cdr xs)))
          (foldr-filter f z pred (cdr xs)))))

(define (foldr-filter-map f z pred g xs)
  (doc 'type '(-> (-> b c c) c (-> a Bool) (-> a b) (List a) c))
  (doc 'description "Right fold with both filtering and mapping. Semantically equivalent to: (foldr f z (map g (filter pred xs)))")
  (if (null? xs)
      z
      (if (pred (car xs))
          (f (g (car xs)) (foldr-filter-map f z pred g (cdr xs)))
          (foldr-filter-map f z pred g (cdr xs)))))

(doc 'section 'take-drop-fused-variants)

(define (take-map n f xs)
  (doc 'type '(-> Nat (-> a b) (List a) (List b)))
  (doc 'description "Take n elements while mapping. Semantically equivalent to: (map f (take n xs)). Also equivalent to: (take n (map f xs)) by commutativity")
  (let loop ([n n] [xs xs] [acc '()])
       (if (or (<= n 0) (null? xs))
           (reverse acc)
           (loop (- n 1) (cdr xs) (cons (f (car xs)) acc)))))

(define (drop-map n f xs)
  (doc 'type '(-> Nat (-> a b) (List a) (List b)))
  (doc 'description "Drop n elements then map the rest. Semantically equivalent to: (map f (drop n xs))")
  (let loop ([n n] [xs xs])
       (cond
        [(null? xs) '()]
        [(<= n 0) (map f xs)]
        [else (loop (- n 1) (cdr xs))])))

(define (take-while-map pred f xs)
  (doc 'type '(-> (-> a Bool) (-> a b) (List a) (List b)))
  (doc 'description "Take elements while predicate holds, mapping each. Semantically equivalent to: (map f (take-while pred xs))")
  (let loop ([xs xs] [acc '()])
       (cond
        [(null? xs) (reverse acc)]
        [(pred (car xs))
         (loop (cdr xs) (cons (f (car xs)) acc))]
        [else (reverse acc)])))

(define (drop-while-map pred f xs)
  (doc 'type '(-> (-> a Bool) (-> a b) (List a) (List b)))
  (doc 'description "Drop elements while predicate holds, then map the rest. Semantically equivalent to: (map f (drop-while pred xs))")
  (let loop ([xs xs])
       (cond
        [(null? xs) '()]
        [(pred (car xs)) (loop (cdr xs))]
        [else (map f xs)])))

(doc 'section 'multi-filter-operations)

(define (filter-filter p1 p2 xs)
  (doc 'type '(-> (-> a Bool) (-> a Bool) (List a) (List a)))
  (doc 'description "Two filters in single pass. Semantically equivalent to: (filter p2 (filter p1 xs)). Also equivalent to: (filter (lambda (x) (and (p1 x) (p2 x))) xs)")
  (let loop ([xs xs] [acc '()])
       (if (null? xs)
           (reverse acc)
           (let ([x (car xs)])
                (if (and (p1 x) (p2 x))
                    (loop (cdr xs) (cons x acc))
                    (loop (cdr xs) acc))))))

(define (filter-all preds xs)
  (doc 'type '(-> (List (-> a Bool)) (List a) (List a)))
  (doc 'description "Filter by conjunction of all predicates")
  (filter (lambda (x) (andmap (lambda (p) (p x)) preds)) xs))

(define (filter-any preds xs)
  (doc 'type '(-> (List (-> a Bool)) (List a) (List a)))
  (doc 'description "Filter by disjunction of predicates")
  (filter (lambda (x) (ormap (lambda (p) (p x)) preds)) xs))

(doc 'section 'stream-fused-operations)

(define (stream-filter-map pred f s)
  (doc 'type '(-> (-> a Bool) (-> a b) (Stream a) (Stream b)))
  (doc 'description "Lazy filter-map for streams. Combines stream-filter and stream-map in single traversal. Semantically equivalent to: (stream-map f (stream-filter pred s))")
  (cond
   [(stream-nil? s) stream-nil]
   [(pred (stream-head s))
    (stream-cons (f (stream-head s))
                 (lambda () (stream-filter-map pred f (stream-tail s))))]
   [else (stream-filter-map pred f (stream-tail s))]))

(define (stream-map-filter f pred s)
  (doc 'type '(-> (-> a b) (-> b Bool) (Stream a) (Stream b)))
  (doc 'description "Lazy map-filter for streams. Maps each element, keeps only those where result satisfies predicate. Semantically equivalent to: (stream-filter pred (stream-map f s))")
  (cond
   [(stream-nil? s) stream-nil]
   [else
    (let ([mapped (f (stream-head s))])
         (if (pred mapped)
             (stream-cons mapped
                          (lambda () (stream-map-filter f pred (stream-tail s))))
             (stream-map-filter f pred (stream-tail s))))]))

(define (stream-take-map n f s)
  (doc 'type '(-> Nat (-> a b) (Stream a) (Stream b)))
  (doc 'description "Take n elements from stream while mapping. Semantically equivalent to: (stream-map f (stream-take n s))")
  (if (or (<= n 0) (stream-nil? s))
      stream-nil
      (stream-cons (f (stream-head s))
                   (lambda () (stream-take-map (- n 1) f (stream-tail s))))))

(define (stream-drop-map n f s)
  (doc 'type '(-> Nat (-> a b) (Stream a) (Stream b)))
  (doc 'description "Drop n elements then map the rest. Semantically equivalent to: (stream-map f (stream-drop n s))")
  (if (or (<= n 0) (stream-nil? s))
      (stream-map f s)
      (stream-drop-map (- n 1) f (stream-tail s))))

(define (stream-flatMap f s)
  (doc 'type '(-> (-> a (Stream b)) (Stream a) (Stream b)))
  (doc 'description "Monadic bind for streams, fusing map and flatten. Uses fair interleaving to handle infinite inner streams. Semantically equivalent to: (stream-flatten (stream-map f s))")
  (stream-flatten (stream-map f s)))

(define (stream-fold-map f z g n s)
  (doc 'type '(-> (-> b c b) b (-> a c) Nat (Stream a) b))
  (doc 'description "Stream fold with integrated mapping, using fuel for termination. Semantically equivalent to: (stream-fold f z n (stream-map g s))")
  (let loop ([acc z] [n n] [s s])
       (if (or (<= n 0) (stream-nil? s))
           acc
           (loop (f acc (g (stream-head s))) (- n 1) (stream-tail s)))))

(define (stream-fold-filter f z pred fuel s)
  (doc 'type '(-> (-> b a b) b (-> a Bool) Nat (Stream a) b))
  (doc 'description "Stream fold with integrated filtering. Note: May consume more than n elements if many are filtered out. fuel limits total elements examined, not elements accumulated")
  (let loop ([acc z] [fuel fuel] [s s])
       (cond
        [(<= fuel 0) acc]
        [(stream-nil? s) acc]
        [(pred (stream-head s))
         (loop (f acc (stream-head s)) (- fuel 1) (stream-tail s))]
        [else
         (loop acc (- fuel 1) (stream-tail s))])))

(doc 'section 'specialized-fused-combinators)

(define (count-if pred xs)
  (doc 'type '(-> (-> a Bool) (List a) Nat))
  (doc 'description "Count elements satisfying predicate (fused length + filter). Semantically equivalent to: (length (filter pred xs))")
  (fold-filter (lambda (acc _) (+ acc 1)) 0 pred xs))

(define (sum-map f xs)
  (doc 'type '(-> (-> a Num) (List a) Num))
  (doc 'description "Sum of mapped values (fused fold + map). Semantically equivalent to: (apply + (map f xs))")
  (fold-map + 0 f xs))

(define (product-map f xs)
  (doc 'type '(-> (-> a Num) (List a) Num))
  (doc 'description "Product of mapped values. Semantically equivalent to: (apply * (map f xs))")
  (fold-map * 1 f xs))

(define (maximum-by key xs)
  (doc 'type '(-> (-> a Num) (List a) a))
  (doc 'description "Find element that maximizes key function. Returns first element if list is singleton. On ties, returns first occurrence")
  (if (null? xs)
      (error 'maximum-by "empty list")
      (let loop ([best (car xs)] [best-key (key (car xs))] [xs (cdr xs)])
           (if (null? xs)
               best
               (let ([k (key (car xs))])
                    (if (> k best-key)
                        (loop (car xs) k (cdr xs))
                        (loop best best-key (cdr xs))))))))

(define (minimum-by key xs)
  (doc 'type '(-> (-> a Num) (List a) a))
  (doc 'description "Find element that minimizes key function. On ties, returns first occurrence")
  (if (null? xs)
      (error 'minimum-by "empty list")
      (let loop ([best (car xs)] [best-key (key (car xs))] [xs (cdr xs)])
           (if (null? xs)
               best
               (let ([k (key (car xs))])
                    (if (< k best-key)
                        (loop (car xs) k (cdr xs))
                        (loop best best-key (cdr xs))))))))

(define (find-map f xs)
  (doc 'type '(-> (-> a (Maybe b)) (List a) (Maybe b)))
  (doc 'description "Find first element where f returns (just ...). Fuses find with map transformation")
  (let loop ([xs xs])
       (if (null? xs)
           nothing
           (let ([result (f (car xs))])
                (if (nothing? result)
                    (loop (cdr xs))
                    result)))))

(define (partition-map f xs)
  (doc 'type '(-> (-> a (Either b c)) (List a) (Pair (List b) (List c))))
  (doc 'description "Partition while transforming. f returns (left x) for first list, (right x) for second")
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
                  (loop (cdr xs) lefts (cons result rights))])))))

(doc 'section 'index-aware-fused-operations)

(define (filter-mapi pred f xs)
  (doc 'type '(-> (-> Nat a Bool) (-> Nat a b) (List a) (List b)))
  (doc 'description "Filter-map with index available to both functions")
  (let loop ([xs xs] [i 0] [acc '()])
       (cond
        [(null? xs) (reverse acc)]
        [(pred i (car xs))
         (loop (cdr xs) (+ i 1) (cons (f i (car xs)) acc))]
        [else
         (loop (cdr xs) (+ i 1) acc)])))

(define (mapi f xs)
  (doc 'type '(-> (-> Nat a b) (List a) (List b)))
  (doc 'description "Map with index. Fuses enumerate + map. Semantically equivalent to: (map (lambda (p) (f (car p) (cdr p))) (enumerate xs))")
  (let loop ([xs xs] [i 0] [acc '()])
       (if (null? xs)
           (reverse acc)
           (loop (cdr xs) (+ i 1) (cons (f i (car xs)) acc)))))

(define (filteri pred xs)
  (doc 'type '(-> (-> Nat a Bool) (List a) (List a)))
  (doc 'description "Filter with index available")
  (let loop ([xs xs] [i 0] [acc '()])
       (cond
        [(null? xs) (reverse acc)]
        [(pred i (car xs))
         (loop (cdr xs) (+ i 1) (cons (car xs) acc))]
        [else
         (loop (cdr xs) (+ i 1) acc)])))

(define (foldli f z xs)
  (doc 'type '(-> (-> b Nat a b) b (List a) b))
  (doc 'description "Left fold with index available")
  (let loop ([xs xs] [i 0] [acc z])
       (if (null? xs)
           acc
           (loop (cdr xs) (+ i 1) (f acc i (car xs))))))

(doc 'section 'chunking-fused-operations)

(define (chunk-map n f xs)
  (doc 'type '(-> Nat (-> (List a) b) (List a) (List b)))
  (doc 'description "Chunk list and map over chunks. Semantically equivalent to: (map f (chunk n xs))")
  (if (or (null? xs) (<= n 0))
      '()
      (cons (f (take n xs))
            (chunk-map n f (drop n xs)))))

(define (window-map n f xs)
  (doc 'type '(-> Nat (-> (List a) b) (List a) (List b)))
  (doc 'description "Sliding window with mapping. Produces (- (length xs) (- n 1)) results for window size n")
  (let loop ([xs xs] [acc '()])
       (if (< (length xs) n)
           (reverse acc)
           (loop (cdr xs) (cons (f (take n xs)) acc)))))

(doc 'section 'parallel-ready-fused-operations)
(doc 'note "These don't actually parallelize but have signatures amenable to parallel execution (pure, associative reductions)")

(define (reduce-map combine identity f xs)
  (doc 'type '(-> (-> b b b) b (-> a b) (List a) b))
  (doc 'description "Map then reduce with an associative operation. For parallel execution, the combiner must be associative")
  (fold-left combine identity (map f xs)))

(define (partition-reduce pred f-yes f-no z-yes z-no xs)
  (doc 'type '(-> (-> a Bool) (-> b a b) (-> c a c) b c (List a) (Pair b c)))
  (doc 'description "Single pass: partition elements and reduce each partition")
  (let loop ([xs xs] [yes z-yes] [no z-no])
       (if (null? xs)
           (cons yes no)
           (let ([x (car xs)])
                (if (pred x)
                    (loop (cdr xs) (f-yes yes x) no)
                    (loop (cdr xs) yes (f-no no x)))))))
