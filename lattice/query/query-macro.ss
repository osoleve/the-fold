;;; lattice/query/query-macro.ss — Declarative Optic Query DSL
;;;
;;; A macro-based query language that compiles to optic-query primitives.
;;; Provides SQL-like syntax for querying data through optic paths.
;;;
;;; DESIGN GOALS:
;;;   - Declarative: Write WHAT you want, not HOW to get it
;;;   - Composable: Build complex queries from simple clauses
;;;   - Readable: SQL-like syntax that's easy to understand
;;;   - Efficient: Compiles to existing optimized primitives
;;;
;;; CORE SYNTAX:
;;;
;;;   (from source optic
;;;     (where pred)
;;;     (select proj)
;;;     (order-by key [direction])
;;;     (limit n)
;;;     (offset n))
;;;
;;; PREDICATE SHORTHANDS (using `it` to reference current target):
;;;
;;;   (where (> (@ it vel-y) 0))     ; Compare via optic
;;;   (where (=? name "alpha"))      ; Shorthand equality
;;;   (where (>? vel-y 0))           ; Shorthand comparison
;;;   (where (and? (>? x 0) (<? y 10))) ; Compound predicates
;;;
;;; EXAMPLES:
;;;
;;;   ;; Find bodies with positive y-velocity, return their positions
;;;   (from world world-each-body
;;;     (where (>? body-vel-y 0))
;;;     (select body-pos-lens))
;;;
;;;   ;; More complex query with ordering
;;;   (from world world-each-body
;;;     (where (and? (>? body-pos-x 0) (<? body-pos-x 100)))
;;;     (order-by body-pos-y)
;;;     (limit 10))
;;;
;;; Dependencies:
;;;   - lattice/query/optic-query.ss

(load "lattice/query/optic-query.ss")

;;; ============================================================
;;; Part 1: Predicate Combinators
;;; ============================================================
;;;
;;; These functions create predicates from optics and values.
;;; They're designed to be more concise than writing lambdas.

;;; =? : Optic × Value → (Target → Bool)
;;; Create equality predicate via optic.
;;; (=? name-lens "alpha") => (lambda (it) (equal? (^. it name-lens) "alpha"))
(define (=? optic value)
  (lambda (it)
    (let ([result (^? it optic)])
      (and (just? result)
           (equal? (from-just result) value)))))

;;; /=? : Optic × Value → (Target → Bool)
;;; Create inequality predicate via optic.
(define (/=? optic value)
  (lambda (it)
    (let ([result (^? it optic)])
      (and (just? result)
           (not (equal? (from-just result) value))))))

;;; >? : Optic × Number → (Target → Bool)
;;; Create greater-than predicate via optic.
(define (>? optic value)
  (lambda (it)
    (let ([result (^? it optic)])
      (and (just? result)
           (> (from-just result) value)))))

;;; <? : Optic × Number → (Target → Bool)
;;; Create less-than predicate via optic.
(define (<? optic value)
  (lambda (it)
    (let ([result (^? it optic)])
      (and (just? result)
           (< (from-just result) value)))))

;;; >=? : Optic × Number → (Target → Bool)
;;; Create greater-or-equal predicate via optic.
(define (>=? optic value)
  (lambda (it)
    (let ([result (^? it optic)])
      (and (just? result)
           (>= (from-just result) value)))))

;;; <=? : Optic × Number → (Target → Bool)
;;; Create less-or-equal predicate via optic.
(define (<=? optic value)
  (lambda (it)
    (let ([result (^? it optic)])
      (and (just? result)
           (<= (from-just result) value)))))

;;; between? : Optic × Number × Number → (Target → Bool)
;;; Create range predicate via optic (inclusive).
(define (between? optic low high)
  (lambda (it)
    (let ([result (^? it optic)])
      (and (just? result)
           (let ([v (from-just result)])
             (and (>= v low) (<= v high)))))))

;;; in? : Optic × (List Value) → (Target → Bool)
;;; Create set membership predicate via optic.
(define (in? optic values)
  (lambda (it)
    (let ([result (^? it optic)])
      (and (just? result)
           (if (member (from-just result) values) #t #f)))))

;;; like? : Optic × String → (Target → Bool)
;;; Create substring match predicate via optic.
(define (like? optic pattern)
  (lambda (it)
    (let ([result (^? it optic)])
      (and (just? result)
           (string? (from-just result))
           (string-contains-ci? (from-just result) pattern)))))

;;; string-contains-ci? : String × String → Bool
;;; Case-insensitive substring search.
(define (string-contains-ci? haystack needle)
  (let ([h (string-downcase haystack)]
        [n (string-downcase needle)])
    (string-contains? h n)))

;;; string-contains? : String × String → Bool
;;; Case-sensitive substring search.
;;; Uses character-by-character comparison to avoid substring allocations.
(define (string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
    (cond
      [(= n-len 0) #t]  ; Empty needle always matches
      [(> n-len h-len) #f]  ; Needle longer than haystack
      [else
       (let ([first-char (string-ref needle 0)])
         ;; Scan for first character match, then verify rest
         (let outer ([i 0])
           (cond
             [(> (+ i n-len) h-len) #f]
             [(char=? (string-ref haystack i) first-char)
              ;; First char matches, check remaining chars
              (if (let inner ([j 1])
                    (cond
                      [(= j n-len) #t]  ; All chars matched
                      [(char=? (string-ref haystack (+ i j))
                               (string-ref needle j))
                       (inner (+ j 1))]
                      [else #f]))
                  #t
                  (outer (+ i 1)))]
             [else (outer (+ i 1))])))])))  ; Fixed: removed extra parens

;;; string-downcase : String → String
;;; Convert string to lowercase.
(define (string-downcase s)
  (list->string
   (map char-downcase (string->list s))))

;;; null? : Optic → (Target → Bool)
;;; Check if optic target is null/nothing.
(define (null-at? optic)
  (lambda (it)
    (nothing? (^? it optic))))

;;; exists-at? : Optic → (Target → Bool)
;;; Check if optic target exists.
(define (exists-at? optic)
  (lambda (it)
    (just? (^? it optic))))

;;; ============================================================
;;; Part 2: Predicate Composition
;;; ============================================================

;;; and? : Pred ... → Pred
;;; Combine predicates with AND.
(define (and? . preds)
  (lambda (it)
    (andmap (lambda (p) (p it)) preds)))

;;; or? : Pred ... → Pred
;;; Combine predicates with OR.
(define (or? . preds)
  (lambda (it)
    (ormap (lambda (p) (p it)) preds)))

;;; not? : Pred → Pred
;;; Negate a predicate.
(define (not? pred)
  (lambda (it)
    (not (pred it))))

;;; ============================================================
;;; Part 3: The @ Operator (Optic View Shorthand)
;;; ============================================================
;;;
;;; The @ function provides concise optic access within predicates.
;;; (@ target optic) is equivalent to (^. target optic)
;;; (@ target op1 op2 ...) composes optics left-to-right

;;; @ : Target × Optic ... → Value
;;; View through one or more optics.
(define @
  (case-lambda
    [(target optic)
     (^. target optic)]
    [(target op1 op2)
     (^. target (>>> op1 op2))]
    [(target op1 op2 op3)
     (^. target (>>> op1 (>>> op2 op3)))]
    [(target . optics)
     (^. target (fold-left >>> (car optics) (cdr optics)))]))

;;; @? : Target × Optic ... → Maybe Value
;;; Preview through one or more optics.
(define @?
  (case-lambda
    [(target optic)
     (^? target optic)]
    [(target op1 op2)
     (^? target (>>> op1 op2))]
    [(target . optics)
     (^? target (fold-left >>> (car optics) (cdr optics)))]))

;;; ============================================================
;;; Part 4: Query Builder Record
;;; ============================================================
;;;
;;; A query is represented as a record with accumulated clauses.
;;; The `from` function starts a query, clause functions extend it.

;;; Query structure: (query source optic where-pred select-fn order-key order-dir limit-n offset-n)
(define (make-query-record source optic)
  (list 'query-record source optic #f #f #f 'asc #f #f))

(define (query-record? x)
  (and (pair? x) (eq? (car x) 'query-record)))

(define (qr-source q) (list-ref q 1))
(define (qr-optic q) (list-ref q 2))
(define (qr-where q) (list-ref q 3))
(define (qr-select q) (list-ref q 4))
(define (qr-order-key q) (list-ref q 5))
(define (qr-order-dir q) (list-ref q 6))
(define (qr-limit q) (list-ref q 7))
(define (qr-offset q) (list-ref q 8))

;;; update-query-record : Query × Symbol × Value → Query
;;; Update a specific field in the query record.
(define (update-query-record q field value)
  (let ([src (qr-source q)]
        [opt (qr-optic q)]
        [whr (qr-where q)]
        [sel (qr-select q)]
        [ord (qr-order-key q)]
        [dir (qr-order-dir q)]
        [lim (qr-limit q)]
        [off (qr-offset q)])
    (case field
      [(where) (list 'query-record src opt value sel ord dir lim off)]
      [(select) (list 'query-record src opt whr value ord dir lim off)]
      [(order-key) (list 'query-record src opt whr sel value dir lim off)]
      [(order-dir) (list 'query-record src opt whr sel ord value lim off)]
      [(limit) (list 'query-record src opt whr sel ord dir value off)]
      [(offset) (list 'query-record src opt whr sel ord dir lim value)]
      [else (error 'update-query-record "Unknown field" field)])))

;;; ============================================================
;;; Part 5: Query Clause Functions
;;; ============================================================

;;; from : Source × Optic → Query
;;; Start a query from a source through an optic.
(define (from source optic)
  (make-query-record source optic))

;;; where-clause : Query × Pred → Query
;;; Add a filter predicate to the query.
(define (where-clause q pred)
  (if (qr-where q)
      ;; Combine with existing predicate using AND
      (update-query-record q 'where (and? (qr-where q) pred))
      (update-query-record q 'where pred)))

;;; select-clause : Query × Proj → Query
;;; Add a projection function to the query.
(define (select-clause q proj)
  (update-query-record q 'select proj))

;;; order-by-clause : Query × KeyFn [× Direction] → Query
;;; Add ordering to the query.
(define order-by-clause
  (case-lambda
    [(q key-fn)
     (update-query-record q 'order-key key-fn)]
    [(q key-fn dir)
     (update-query-record
      (update-query-record q 'order-key key-fn)
      'order-dir dir)]))

;;; limit-clause : Query × Nat → Query
;;; Limit the number of results.
(define (limit-clause q n)
  (update-query-record q 'limit n))

;;; offset-clause : Query × Nat → Query
;;; Skip the first n results.
(define (offset-clause q n)
  (update-query-record q 'offset n))

;;; ============================================================
;;; Part 6: Query Execution
;;; ============================================================

;;; run-query : Query → (List Result)
;;; Execute a query and return results.
(define (run-query q)
  (let* ([source (qr-source q)]
         [optic (qr-optic q)]
         [where-pred (qr-where q)]
         [select-fn (qr-select q)]
         [order-key (qr-order-key q)]
         [order-dir (qr-order-dir q)]
         [limit-n (qr-limit q)]
         [offset-n (qr-offset q)]
         ;; Step 1: Get all targets
         [targets (^.. source optic)]
         ;; Step 2: Filter if where clause exists
         [filtered (if where-pred
                       (filter where-pred targets)
                       targets)]
         ;; Step 3: Order if order-by clause exists
         [ordered (if order-key
                      (let ([sorted (merge-sort-by order-key filtered)])
                        (if (eq? order-dir 'desc)
                            (reverse sorted)
                            sorted))
                      filtered)]
         ;; Step 4: Apply offset
         [after-offset (if offset-n
                           (drop-up-to offset-n ordered)
                           ordered)]
         ;; Step 5: Apply limit
         [limited (if limit-n
                      (take-up-to limit-n after-offset)
                      after-offset)]
         ;; Step 6: Project if select clause exists
         [projected (if select-fn
                        (map select-fn limited)
                        limited)])
    projected))

;;; q> : Query × Clause... → (List Result)
;;; Pipeline operator for chaining query clauses.
;;; (q> (from world bodies) (where pred) (select proj))
(define-syntax q>
  (syntax-rules ()
    [(q> query)
     (run-query query)]
    [(q> query clause clauses ...)
     (q> (clause query) clauses ...)]))

;;; ============================================================
;;; Part 7: Aggregation on Queries
;;; ============================================================

;;; count-query : Query → Nat
;;; Count results matching the query.
(define (count-query q)
  (length (run-query q)))

;;; sum-query : Query × KeyFn → Number
;;; Sum values extracted from query results.
(define (sum-query q key-fn)
  (apply + (map key-fn (run-query q))))

;;; avg-query : Query × KeyFn → Number
;;; Average values extracted from query results.
(define (avg-query q key-fn)
  (let ([results (run-query q)])
    (if (null? results)
        0
        (/ (apply + (map key-fn results))
           (length results)))))

;;; min-query : Query × KeyFn → Maybe Value
;;; Find minimum value in query results.
(define (min-query q key-fn)
  (let ([results (run-query q)])
    (if (null? results)
        nothing
        (just (apply min (map key-fn results))))))

;;; max-query : Query × KeyFn → Maybe Value
;;; Find maximum value in query results.
(define (max-query q key-fn)
  (let ([results (run-query q)])
    (if (null? results)
        nothing
        (just (apply max (map key-fn results))))))

;;; group-query : Query × KeyFn → Alist
;;; Group query results by a key function.
(define (group-query q key-fn)
  (let ([results (run-query q)]
        [groups (make-hashtable equal-hash equal?)])
    (for-each
     (lambda (item)
       (let* ([key (key-fn item)]
              [existing (hashtable-ref groups key '())])
         (hashtable-set! groups key (cons item existing))))
     results)
    (let-values ([(keys vals) (hashtable-entries groups)])
      (let loop ([i 0] [result '()])
        (if (>= i (vector-length keys))
            result
            (loop (+ i 1)
                  (cons (cons (vector-ref keys i)
                              (reverse (vector-ref vals i)))
                        result)))))))

;;; ============================================================
;;; Part 8: Convenience Macros
;;; ============================================================

;;; query-syntax : Declarative query form
;;; (query-syntax
;;;   (from source optic)
;;;   (where predicate)
;;;   (select projection)
;;;   (order-by key-fn [direction])
;;;   (limit n)
;;;   (offset n))
(define-syntax query-syntax
  (syntax-rules (from where select order-by limit offset)
    ;; Base case: just from clause
    [(query-syntax (from src opt))
     (run-query (from src opt))]

    ;; from + where
    [(query-syntax (from src opt) (where pred))
     (run-query (where-clause (from src opt) pred))]

    ;; from + where + select
    [(query-syntax (from src opt) (where pred) (select proj))
     (run-query (select-clause (where-clause (from src opt) pred) proj))]

    ;; from + select (no where)
    [(query-syntax (from src opt) (select proj))
     (run-query (select-clause (from src opt) proj))]

    ;; General case with multiple clauses - use procedural builder
    [(query-syntax (from src opt) clause ...)
     (run-query (apply-clauses (from src opt) (list (quote clause) ...)))]))

;;; apply-clauses : Query × (List QuotedClause) → Query
;;; Apply a list of quoted clauses to a query.
(define (apply-clauses q clauses)
  (if (null? clauses)
      q
      (let ([clause (car clauses)]
            [rest (cdr clauses)])
        (apply-clauses
         (case (car clause)
           [(where) (where-clause q (eval (cadr clause)))]
           [(select) (select-clause q (eval (cadr clause)))]
           [(order-by)
            (if (> (length clause) 2)
                (order-by-clause q (eval (cadr clause)) (caddr clause))
                (order-by-clause q (eval (cadr clause))))]
           [(limit) (limit-clause q (cadr clause))]
           [(offset) (offset-clause q (cadr clause))]
           [else (error 'apply-clauses "Unknown clause" (car clause))])
         rest))))

;;; ============================================================
;;; Part 9: Query Result Operations
;;; ============================================================

;;; first-result : Query → Maybe Result
;;; Get the first result from a query.
(define (first-result q)
  (let ([results (run-query q)])
    (if (null? results)
        nothing
        (just (car results)))))

;;; any-result? : Query → Bool
;;; Check if query has any results.
(define (any-result? q)
  (not (null? (run-query q))))

;;; all-match? : Query × Pred → Bool
;;; Check if all query results match a predicate.
(define (all-match? q pred)
  (andmap pred (run-query q)))

;;; ============================================================
;;; Part 10: Projection Helpers
;;; ============================================================

;;; select-fields : (List Optic) → (Target → Alist)
;;; Create a projection that extracts multiple fields.
(define (select-fields . optic-pairs)
  (lambda (it)
    (map (lambda (pair)
           (cons (car pair) (^. it (cdr pair))))
         optic-pairs)))

;;; pluck : Optic → (Target → Value)
;;; Create a projection that extracts a single field.
(define (pluck optic)
  (lambda (it)
    (^. it optic)))

;;; ============================================================
;;; Module Load Message
;;; ============================================================

(printf "Declarative Query DSL loaded.\n")
(printf "  Builder: from, where-clause, select-clause, order-by-clause\n")
(printf "  Execution: run-query, q>, first-result\n")
(printf "  Predicates: =?, >?, <?, >=?, <=?, between?, in?, like?\n")
(printf "  Combinators: and?, or?, not?\n")
(printf "  Aggregates: count-query, sum-query, avg-query, group-query\n")
