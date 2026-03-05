(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires optics hamt sort
(require 'optics)
(require 'hamt)
(require 'sort)

(doc 'module 'optic-query)
(doc 'description "Optic-Based Query Language")
(doc 'note "Build declarative queries using optics as the path language. Optics encode how to reach data; this module adds predicate filtering, projection, and aggregation.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'design-principles)
(doc 'description "Optics define the path through data structures. Predicates filter which targets to keep. Projectors transform results. Composable: build complex queries from simple parts.")

(doc 'section 'core-api)
(doc 'description "oquery, oquery-where, oquery-select, oquery-pipe")

(doc 'section 'combinators)
(doc 'description "optic-where (filtered traversal), optic-select (projected getter), optic-having (filter by nested value)")

(doc 'section 'aggregations)
(doc 'description "oquery-count, oquery-sum, oquery-any, oquery-all")

(doc 'section 'core-query-functions)

(define (oquery s optic)
  (doc 'export #t)
  (doc 'type '(-> s (Optic s a) (List a)))
  (doc 'description "Get all targets reachable via optic. Essentially (^.. s optic) with clearer query semantics.")
  (^.. s optic))

(define (oquery-where s optic pred)
  (doc 'export #t)
  (doc 'type '(-> s (Optic s a) (-> a Bool) (List a)))
  (doc 'description "Get targets matching a predicate")
  (filter pred (^.. s optic)))

(define (oquery-select s optic proj)
  (doc 'export #t)
  (doc 'type '(-> s (Optic s a) (-> a b) (List b)))
  (doc 'description "Project all targets through a function")
  (map proj (^.. s optic)))

(define (oquery-pipe s optic pred proj)
  (doc 'export #t)
  (doc 'type '(-> s (Optic s a) (-> a Bool) (-> a b) (List b)))
  (doc 'description "Filter then project: the most common query pattern")
  (map proj (filter pred (^.. s optic))))

(define (oquery-first s optic)
  (doc 'export #t)
  (doc 'type '(-> s (Optic s a) (Maybe a)))
  (doc 'description "Get first target if any exist")
  (^? s optic))

(define (oquery-first-where s optic pred)
  (doc 'export #t)
  (let loop ([targets (^.. s optic)])
    (cond
      [(null? targets) nothing]
      [(pred (car targets)) (just (car targets))]
      [else (loop (cdr targets))])))

;;; ============================================================
;;; Part 2: Optic Combinators for Queries
;;; ============================================================
;;;
;;; These create new optics that can be composed with >>>.
;;; They enable reusable, named query patterns.

;;; optic-where : Optic s a × (a → Bool) → Traversal s a
;;; Create a traversal that only yields targets matching predicate.
;;; The resulting traversal can be composed with other optics.
(define (optic-where optic pred)
  (doc 'export #t)
  (make-traversal
   ;; traverse: (a → b) → s → t
   (lambda (f s)
     ;; Get all targets, apply f only to matching ones
     (let ([trav (->traversal optic)])
       ((traversal-traverse trav)
        (lambda (a) (if (pred a) (f a) a))
        s)))
   ;; fold: s → List a
   (lambda (s)
     (filter pred (^.. s optic)))))

;;; optic-having : Optic s a × Optic a b × (b → Bool) → Traversal s a
;;; Filter targets by a predicate on a nested value.
;;; "Give me all a's where the b inside satisfies pred"
;;;
;;; Example:
;;;   (optic-having bodies-trav body-vel-y (lambda (vy) (> vy 0)))
;;;   ;; All bodies whose velocity y-component is positive
(define (optic-having optic inner-optic pred)
  (doc 'export #t)
  (optic-where optic
    (lambda (a)
      (let ([maybe-b (^? a inner-optic)])
        (and (just? maybe-b)
             (pred (from-just maybe-b)))))))

;;; optic-select : Optic s a × (a → b) → Fold s b
;;; Create a fold that projects targets through a function.
;;; Read-only: you can get values but not set them.
(define (optic-select optic proj)
  (doc 'export #t)
  (make-fold
   (lambda (s)
     (map proj (^.. s optic)))))

;;; optic-at-index : Nat → Affine (List a) a
;;; Focus on element at specific index (re-export for convenience).
(doc optic-at-index 'export #t)
(define optic-at-index affine-nth)

;;; optic-limit : Optic s a × Nat → Fold s a
;;; Take only the first n targets.
(define (optic-limit optic n)
  (doc 'export #t)
  (make-fold
   (lambda (s)
     (take n (^.. s optic)))))

;;; optic-skip : Optic s a × Nat → Fold s a
;;; Skip the first n targets.
(define (optic-skip optic n)
  (doc 'export #t)
  (make-fold
   (lambda (s)
     (drop-up-to n (^.. s optic)))))

;;; ============================================================
;;; Part 3: Aggregation Functions
;;; ============================================================

;;; oquery-count : s × Optic s a → Nat
;;; Count all targets.
(define (oquery-count s optic)
  (doc 'export #t)
  (length (^.. s optic)))

;;; oquery-count-where : s × Optic s a × (a → Bool) → Nat
;;; Count targets matching predicate.
(define (oquery-count-where s optic pred)
  (doc 'export #t)
  (length (filter pred (^.. s optic))))

;;; oquery-sum : s × Optic s Number → Number
;;; Sum all numeric targets.
(define (oquery-sum s optic)
  (doc 'export #t)
  (apply + (^.. s optic)))

;;; oquery-sum-by : s × Optic s a × (a → Number) → Number
;;; Sum values extracted from targets.
(define (oquery-sum-by s optic f)
  (doc 'export #t)
  (apply + (map f (^.. s optic))))

;;; oquery-any : s × Optic s a × (a → Bool) → Bool
;;; Check if any target matches predicate.
(define (oquery-any s optic pred)
  (doc 'export #t)
  (ormap pred (^.. s optic)))

;;; oquery-all : s × Optic s a × (a → Bool) → Bool
;;; Check if all targets match predicate.
(define (oquery-all s optic pred)
  (doc 'export #t)
  (andmap pred (^.. s optic)))

;;; oquery-min : s × Optic s Number → Maybe Number
;;; Get minimum target value.
(define (oquery-min s optic)
  (doc 'export #t)
  (let ([targets (^.. s optic)])
    (if (null? targets)
        nothing
        (just (apply min targets)))))

;;; oquery-max : s × Optic s Number → Maybe Number
;;; Get maximum target value.
(define (oquery-max s optic)
  (doc 'export #t)
  (let ([targets (^.. s optic)])
    (if (null? targets)
        nothing
        (just (apply max targets)))))

;;; oquery-min-by : s × Optic s a × (a → Number) → Maybe a
;;; Get target with minimum value according to function.
(define (oquery-min-by s optic f)
  (doc 'export #t)
  (let ([targets (^.. s optic)])
    (if (null? targets)
        nothing
        (just (let loop ([best (car targets)]
                         [best-val (f (car targets))]
                         [remaining (cdr targets)])
                (if (null? remaining)
                    best
                    (let ([val (f (car remaining))])
                      (if (< val best-val)
                          (loop (car remaining) val (cdr remaining))
                          (loop best best-val (cdr remaining))))))))))

;;; oquery-max-by : s × Optic s a × (a → Number) → Maybe a
;;; Get target with maximum value according to function.
(define (oquery-max-by s optic f)
  (doc 'export #t)
  (let ([targets (^.. s optic)])
    (if (null? targets)
        nothing
        (just (let loop ([best (car targets)]
                         [best-val (f (car targets))]
                         [remaining (cdr targets)])
                (if (null? remaining)
                    best
                    (let ([val (f (car remaining))])
                      (if (> val best-val)
                          (loop (car remaining) val (cdr remaining))
                          (loop best best-val (cdr remaining))))))))))

;;; ============================================================
;;; Part 4: Grouping and Partitioning
;;; ============================================================

;;; oquery-group-by : s × Optic s a × (a → k) → Alist k (List a)
;;; Group targets by a key function.
;;; Uses hashtable for O(N) performance.
(define (oquery-group-by s optic key-fn)
  (doc 'export #t)
  (let* ([targets (^.. s optic)]
         ;; Collect items into HAMT buckets
         [groups (fold-left
                  (lambda (acc target)
                    (let* ([key (key-fn target)]
                           [existing (hamt-lookup-or key acc '())])
                      (hamt-assoc key (cons target existing) acc)))
                  hamt-empty
                  targets)])
    ;; Convert HAMT to alist, reversing to preserve order
    (hamt-fold (lambda (acc k v)
                 (cons (cons k (reverse v)) acc))
               '() groups)))

;;; assoc-equal : k × Alist → Maybe (Pair k v)
(define (assoc-equal key alist)
  (let loop ([pairs alist])
    (cond
      [(null? pairs) #f]
      [(equal? (caar pairs) key) (car pairs)]
      [else (loop (cdr pairs))])))

;;; oquery-partition : s × Optic s a × (a → Bool) → (Pair (List a) (List a))
;;; Partition targets into (matching, not-matching).
(define (oquery-partition s optic pred)
  (doc 'export #t)
  (let loop ([targets (^.. s optic)]
             [yes '()]
             [no '()])
    (if (null? targets)
        (cons (reverse yes) (reverse no))
        (if (pred (car targets))
            (loop (cdr targets) (cons (car targets) yes) no)
            (loop (cdr targets) yes (cons (car targets) no))))))

;;; ============================================================
;;; Part 5: Joining Queries
;;; ============================================================
;;;
;;; Combine results from multiple optic paths.

;;; oquery-join : s × Optic s a × Optic s b × (a × b → Bool) → (List (Pair a b))
;;; Cross-product join with predicate filter.
(define (oquery-join s optic-a optic-b match-pred)
  (doc 'export #t)
  (let ([as (^.. s optic-a)]
        [bs (^.. s optic-b)])
    (let outer ([remaining-a as] [result '()])
      (if (null? remaining-a)
          (reverse result)
          (let ([a (car remaining-a)])
            (let inner ([remaining-b bs] [pairs result])
              (if (null? remaining-b)
                  (outer (cdr remaining-a) pairs)
                  (let ([b (car remaining-b)])
                    (if (match-pred a b)
                        (inner (cdr remaining-b) (cons (cons a b) pairs))
                        (inner (cdr remaining-b) pairs))))))))))

;;; oquery-zip : s × Optic s a × Optic s b → (List (Pair a b))
;;; Zip two optic results pairwise (shortest length).
(define (oquery-zip s optic-a optic-b)
  (doc 'export #t)
  (let ([as (^.. s optic-a)]
        [bs (^.. s optic-b)])
    (let loop ([remaining-a as]
               [remaining-b bs]
               [result '()])
      (if (or (null? remaining-a) (null? remaining-b))
          (reverse result)
          (loop (cdr remaining-a)
                (cdr remaining-b)
                (cons (cons (car remaining-a) (car remaining-b)) result))))))

;;; oquery-union : s × Optic s a × Optic s a → (List a)
;;; Combine targets from two optics (may have duplicates).
(define (oquery-union s optic-a optic-b)
  (doc 'export #t)
  (append (^.. s optic-a) (^.. s optic-b)))

;;; oquery-intersect : s × Optic s a × Optic s a × (a × a → Bool) → (List a)
;;; Keep only targets that appear in both optic results.
(define (oquery-intersect s optic-a optic-b equal?)
  (doc 'export #t)
  (let ([as (^.. s optic-a)]
        [bs (^.. s optic-b)])
    (filter (lambda (a)
              (ormap (lambda (b) (equal? a b)) bs))
            as)))

;;; ============================================================
;;; Part 6: Sorted Queries
;;; ============================================================

;;; oquery-sort-by : s × Optic s a × (a → Number) → (List a)
;;; Get targets sorted by a numeric key (ascending).
(define (oquery-sort-by s optic key-fn)
  (doc 'export #t)
  (sort-by-key key-fn (^.. s optic)))

;;; oquery-sort-by-desc : s × Optic s a × (a → Number) → (List a)
;;; Get targets sorted by a numeric key (descending).
(define (oquery-sort-by-desc s optic key-fn)
  (doc 'export #t)
  (sort-by-key-desc key-fn (^.. s optic)))

;;; ============================================================
;;; Part 7: Query Builder DSL
;;; ============================================================
;;;
;;; A more declarative interface for building queries.
;;; Uses chained operations that accumulate transformations.

;;; make-query : Optic s a → Query s a
;;; Start building a query from an optic.
(define (make-query optic)
  (doc 'export #t)
  (list 'query optic '() '()))

;;; query-builder? : α → Bool
(define (query-builder? x)
  (and (pair? x) (eq? (car x) 'query)))

;;; query-optic : Query → Optic
(define (query-optic q) (cadr q))

;;; query-filters : Query → (List (a → Bool))
(define (query-filters q) (caddr q))

;;; query-transforms : Query → (List (a → a))
(define (query-transforms q) (cadddr q))

;;; q-where : Query s a × (a → Bool) → Query s a
;;; Add a filter to the query.
(define (q-where query pred)
  (doc 'export #t)
  (list 'query
        (query-optic query)
        (cons pred (query-filters query))
        (query-transforms query)))

;;; q-map : Query s a × (a → b) → Query s b
;;; Add a transformation (projection).
(define (q-map query f)
  (doc 'export #t)
  (list 'query
        (query-optic query)
        (query-filters query)
        (cons f (query-transforms query))))

;;; q-run : s × Query s a → (List a)
;;; Execute the query.
(define (q-run s query)
  (doc 'export #t)
  (let* ([optic (query-optic query)]
         [filters (reverse (query-filters query))]
         [transforms (reverse (query-transforms query))]
         [targets (^.. s optic)]
         ;; Apply all filters
         [filtered (let loop ([preds filters] [data targets])
                     (if (null? preds)
                         data
                         (loop (cdr preds) (filter (car preds) data))))]
         ;; Apply all transforms
         [transformed (let loop ([fns transforms] [data filtered])
                        (if (null? fns)
                            data
                            (loop (cdr fns) (map (car fns) data))))])
    transformed))

;;; q-count : s × Query s a → Nat
;;; Count query results.
(define (q-count s query)
  (doc 'export #t)
  (length (q-run s query)))

;;; q-first : s × Query s a → Maybe a
;;; Get first result.
(define (q-first s query)
  (doc 'export #t)
  (let ([results (q-run s query)])
    (if (null? results) nothing (just (car results)))))

;;; ============================================================
;;; Part 8: Predicate Helpers
;;; ============================================================
;;;
;;; Common predicates for building queries.

;;; optic-eq? : Optic a b × b → (a → Bool)
;;; Create predicate that checks if optic target equals value.
(define (optic-eq? optic value)
  (doc 'export #t)
  (lambda (a)
    (let ([maybe-b (^? a optic)])
      (and (just? maybe-b)
           (equal? (from-just maybe-b) value)))))

;;; optic-matches? : Optic a b × (b → Bool) → (a → Bool)
;;; Create predicate that checks if optic target satisfies a condition.
(define (optic-matches? optic pred)
  (doc 'export #t)
  (lambda (a)
    (let ([maybe-b (^? a optic)])
      (and (just? maybe-b)
           (pred (from-just maybe-b))))))

;;; optic-exists? : Optic a b → (a → Bool)
;;; Create predicate that checks if optic has a target.
(define (optic-exists? optic)
  (doc 'export #t)
  (lambda (a)
    (just? (^? a optic))))

;;; optic-gt? : Optic a Number × Number → (a → Bool)
;;; Check if optic target is greater than value.
(define (optic-gt? optic value)
  (doc 'export #t)
  (optic-matches? optic (lambda (x) (> x value))))

;;; optic-lt? : Optic a Number × Number → (a → Bool)
;;; Check if optic target is less than value.
(define (optic-lt? optic value)
  (doc 'export #t)
  (optic-matches? optic (lambda (x) (< x value))))

;;; optic-gte? : Optic a Number × Number → (a → Bool)
;;; Check if optic target is >= value.
(define (optic-gte? optic value)
  (doc 'export #t)
  (optic-matches? optic (lambda (x) (>= x value))))

;;; optic-lte? : Optic a Number × Number → (a → Bool)
;;; Check if optic target is <= value.
(define (optic-lte? optic value)
  (doc 'export #t)
  (optic-matches? optic (lambda (x) (<= x value))))

;;; optic-between? : Optic a Number × Number × Number → (a → Bool)
;;; Check if optic target is between low and high (inclusive).
(define (optic-between? optic low high)
  (doc 'export #t)
  (optic-matches? optic (lambda (x) (and (>= x low) (<= x high)))))

;;; ============================================================
;;; Part 9: Convenience Lenses for Common Patterns
;;; ============================================================

;;; key-lens : Symbol → Lens Alist a
;;; Lens for accessing alist by key.
(define (key-lens key)
  (doc 'export #t)
  (make-lens
   (lambda (alist)
     (let ([pair (assq key alist)])
       (if pair (cdr pair) #f)))
   (lambda (val alist)
     (let loop ([remaining alist] [prefix '()] [found #f])
       (cond
         [(null? remaining)
          (if found
              (reverse prefix)
              (reverse (cons (cons key val) prefix)))]
         [(eq? (caar remaining) key)
          (loop (cdr remaining) (cons (cons key val) prefix) #t)]
         [else
          (loop (cdr remaining) (cons (car remaining) prefix) found)])))))
