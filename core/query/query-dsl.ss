;;; core/query/query-dsl.ss --- Datalog-style Declarative Query Language for The Fold
;;;
;;; A composable, declarative query DSL for querying blocks in the store.
;;; Inspired by Datalog's pattern matching and relational algebra.
;;;
;;; DESIGN PRINCIPLES:
;;;   - Declarative: Describe WHAT you want, not HOW to get it
;;;   - Composable: Build complex queries from simple primitives
;;;   - Extensible: Easy to add new query operators
;;;   - Efficient: Leverage existing store-api operations
;;;
;;; QUERY LANGUAGE SYNTAX:
;;;
;;;   Pattern queries:
;;;     (match (tag . symbol))           ; Match blocks by tag
;;;     (match (payload-contains . str)) ; Match payload substring
;;;     (match (payload-matches . pred)) ; Match payload by predicate
;;;     (match (has-refs))               ; Match blocks with any refs
;;;     (match (refs-count . n))         ; Match blocks with exactly n refs
;;;
;;;   Compound queries:
;;;     (and query1 query2 ...)          ; All conditions must match
;;;     (or query1 query2 ...)           ; Any condition matches
;;;     (not query)                      ; Negation
;;;
;;;   Reference queries:
;;;     (refs-to hash)                   ; Blocks that reference this hash
;;;     (refs-from hash)                 ; Blocks referenced BY this hash
;;;
;;;   Projection queries:
;;;     (select (field1 field2 ...)      ; Return only these fields
;;;       (where query))                 ; From blocks matching query
;;;
;;;   Aggregation:
;;;     (count query)                    ; Count matching blocks
;;;     (group-by field query)           ; Group results by field
;;;
;;; USAGE:
;;;   (query fs '(match (tag . entity)))
;;;   (query fs '(and (tag . entity) (payload-contains . "Turing")))
;;;   (query fs '(select (tag payload-size) (where (tag . entity))))
;;;
;;; TIER ASSIGNMENT:
;;;   Tier 6-7: Query interpretation and execution (complex computation)
;;;
;;; Dependencies:
;;;   - thimble/store-api.ss (store-filter, store-find-by-tag, etc.)
;;;   - fabric/stitches/block.ss (block accessors)

;;; ============================================================
;;; Dependencies
;;; ============================================================

;;; Assumes these are already loaded:
;;;   - store-api.ss (store-filter, store-all-blocks, store-find-by-tag, etc.)
;;;   - block.ss (make-block, block-tag, block-payload, block-refs)

;;; ============================================================
;;; Section 1: Predicate Builders
;;; ============================================================
;;;
;;; These functions build predicates (Block -> Boolean) from query patterns.
;;; Each pattern type has a corresponding predicate builder.

;;; build-tag-predicate : Symbol -> (Block -> Boolean)
;;; Creates a predicate that matches blocks with the given tag.
;;;
;;; Example:
;;;   (build-tag-predicate 'entity) -> (lambda (b) (eq? (block-tag b) 'entity))
(define (build-tag-predicate tag)
  (lambda (block)
          (eq? (block-tag block) tag)))

;;; build-payload-contains-predicate : String -> (Block -> Boolean)
;;; Creates a predicate that matches blocks whose payload contains a substring.
;;; Payload is interpreted as UTF-8 string.
;;;
;;; Example:
;;;   (build-payload-contains-predicate "Turing")
;;;   -> (lambda (b) (string-contains? (utf8->string (block-payload b)) "Turing"))
(define (build-payload-contains-predicate substring)
  (lambda (block)
          (let ([payload-str (utf8->string (block-payload block))])
               (query-string-contains? payload-str substring))))

;;; build-payload-matches-predicate : (String -> Boolean) -> (Block -> Boolean)
;;; Creates a predicate using a custom payload matcher function.
;;; The matcher receives the payload as a UTF-8 string.
;;;
;;; Example:
;;;   (build-payload-matches-predicate (lambda (s) (> (string-length s) 100)))
(define (build-payload-matches-predicate matcher)
  (lambda (block)
          (let ([payload-str (utf8->string (block-payload block))])
               (matcher payload-str))))

;;; build-has-refs-predicate : -> (Block -> Boolean)
;;; Creates a predicate that matches blocks with at least one reference.
(define (build-has-refs-predicate)
  (lambda (block)
          (> (vector-length (block-refs block)) 0)))

;;; build-refs-count-predicate : Integer -> (Block -> Boolean)
;;; Creates a predicate that matches blocks with exactly n references.
(define (build-refs-count-predicate n)
  (lambda (block)
          (= (vector-length (block-refs block)) n)))

;;; build-refs-to-predicate : FSCap Hash -> (Block -> Boolean)
;;; Creates a predicate that matches blocks containing a reference to the given hash.
(define (build-refs-to-predicate target-hash)
  (lambda (block)
          (let ([refs (block-refs block)])
               (let check-refs ([i 0])
                    (cond
                     [(>= i (vector-length refs)) #f]
                     [(bytevector=? (vector-ref refs i) target-hash) #t]
                     [else (check-refs (+ i 1))])))))

;;; build-payload-size-predicate : (Integer Integer -> Boolean) Integer -> (Block -> Boolean)
;;; Creates a predicate that compares payload size using the given comparator.
;;;
;;; Example:
;;;   (build-payload-size-predicate > 1000) ; Payloads larger than 1000 bytes
(define (build-payload-size-predicate comparator size)
  (lambda (block)
          (comparator (bytevector-length (block-payload block)) size)))

;;; ============================================================
;;; Section 2: Query Expression Interpreter
;;; ============================================================
;;;
;;; The interpreter converts query expressions (s-expressions) into
;;; executable predicates. It handles the following expression types:
;;;
;;;   (match (field . value))    -> Pattern match predicate
;;;   (and q1 q2 ...)            -> Conjunction of predicates
;;;   (or q1 q2 ...)             -> Disjunction of predicates
;;;   (not q)                    -> Negation of predicate
;;;   (refs-to hash)             -> Reference-to predicate
;;;   (refs-from hash)           -> Handled specially (returns blocks)
;;;   (select fields (where q))  -> Projection query

;;; interpret-match : Alist -> (Block -> Boolean)
;;; Interpret a single match pattern and return a predicate.
;;;
;;; Patterns:
;;;   (tag . symbol)            - Match by tag
;;;   (payload-contains . str)  - Match payload substring
;;;   (payload-matches . pred)  - Match payload by predicate
;;;   (has-refs)                - Match blocks with refs (value ignored)
;;;   (refs-count . n)          - Match blocks with n refs
;;;   (payload-size-gt . n)     - Payload size greater than n
;;;   (payload-size-lt . n)     - Payload size less than n
(define (interpret-match pattern)
  (let ([key (car pattern)]
        [value (cdr pattern)])
       (case key
             [(tag)
              (build-tag-predicate value)]
             [(payload-contains)
              (build-payload-contains-predicate value)]
             [(payload-matches)
              (build-payload-matches-predicate value)]
             [(has-refs)
              (build-has-refs-predicate)]
             [(refs-count)
              (build-refs-count-predicate value)]
             [(payload-size-gt)
              (build-payload-size-predicate > value)]
             [(payload-size-lt)
              (build-payload-size-predicate < value)]
             [(payload-size-eq)
              (build-payload-size-predicate = value)]
             [else
              (error 'interpret-match "Unknown match pattern" key)])))

;;; interpret-query : QueryExpr -> (Block -> Boolean)
;;; Main interpreter entry point. Converts a query expression into a predicate.
;;;
;;; Handles:
;;;   (match pattern)           - Single pattern match
;;;   (and q1 q2 ...)          - All queries must match
;;;   (or q1 q2 ...)           - Any query matches
;;;   (not q)                  - Negate query result
;;;   (refs-to hash)           - Blocks referencing hash
;;;   Bare patterns            - Shorthand for (match pattern)
(define (interpret-query expr)
  (cond
   ;; Null or empty - match everything
   [(null? expr)
    (lambda (block) #t)]
   
   ;; Match expression
   [(and (pair? expr) (eq? (car expr) 'match))
    (interpret-match (cadr expr))]
   
   ;; AND combinator
   [(and (pair? expr) (eq? (car expr) 'and))
    (let ([sub-predicates (map interpret-query (cdr expr))])
         (lambda (block)
                 (and-all sub-predicates block)))]
   
   ;; OR combinator
   [(and (pair? expr) (eq? (car expr) 'or))
    (let ([sub-predicates (map interpret-query (cdr expr))])
         (lambda (block)
                 (or-any sub-predicates block)))]
   
   ;; NOT combinator
   [(and (pair? expr) (eq? (car expr) 'not))
    (let ([sub-predicate (interpret-query (cadr expr))])
         (lambda (block)
                 (not (sub-predicate block))))]
   
   ;; refs-to query (returns predicate)
   [(and (pair? expr) (eq? (car expr) 'refs-to))
    (build-refs-to-predicate (cadr expr))]
   
   ;; Bare pattern (shorthand for match)
   [(and (pair? expr) (symbol? (car expr)) (not (memq (car expr) '(match and or not refs-to refs-from select count group-by))))
    (interpret-match expr)]
   
   [else
    (error 'interpret-query "Unknown query expression" expr)]))

;;; and-all : (List (Block -> Boolean)) Block -> Boolean
;;; Return true if ALL predicates match the block.
(define (and-all predicates block)
  (let loop ([preds predicates])
       (or (null? preds)
           (and ((car preds) block)
                (loop (cdr preds))))))

;;; or-any : (List (Block -> Boolean)) Block -> Boolean
;;; Return true if ANY predicate matches the block.
(define (or-any predicates block)
  (let loop ([preds predicates])
       (and (pair? preds)
            (or ((car preds) block)
                (loop (cdr preds))))))

;;; ============================================================
;;; Section 3: Projection System
;;; ============================================================
;;;
;;; Projections allow returning specific fields from matching blocks
;;; instead of returning the full block structure.
;;;
;;; Available fields:
;;;   tag          - Block tag symbol
;;;   payload      - Raw payload bytevector
;;;   payload-str  - Payload as UTF-8 string
;;;   payload-size - Payload size in bytes
;;;   refs         - References vector
;;;   refs-count   - Number of references
;;;   hash         - Block hash (requires fs)

;;; extract-field : Block Symbol -> Any
;;; Extract a single field from a block.
;;;
;;; Fields:
;;;   tag          -> Symbol
;;;   payload      -> Bytevector
;;;   payload-str  -> String
;;;   payload-size -> Integer
;;;   refs         -> Vector
;;;   refs-count   -> Integer
(define (extract-field block field)
  (case field
        [(tag) (block-tag block)]
        [(payload) (block-payload block)]
        [(payload-str) (utf8->string (block-payload block))]
        [(payload-size) (bytevector-length (block-payload block))]
        [(refs) (block-refs block)]
        [(refs-count) (vector-length (block-refs block))]
        [else (error 'extract-field "Unknown field" field)]))

;;; extract-fields : Block (List Symbol) -> Alist
;;; Extract multiple fields from a block, returning an alist.
;;;
;;; Example:
;;;   (extract-fields block '(tag payload-size))
;;;   -> ((tag . entity) (payload-size . 256))
(define (extract-fields block fields)
  (map (lambda (field)
               (cons field (extract-field block field)))
       fields))

;;; project : (List Symbol) (List Block) -> (List Alist)
;;; Project specific fields from a list of blocks.
(define (project fields blocks)
  (map (lambda (block)
               (extract-fields block fields))
       blocks))

;;; ============================================================
;;; Section 4: Reference Traversal
;;; ============================================================
;;;
;;; Functions for navigating the block reference graph.

;;; refs-to-query : FSCap Hash -> (List Block)
;;; Find all blocks that contain a reference to the given hash.
;;; Uses store-find-by-ref from store-api.
(define (refs-to-query fs target-hash)
  (store-find-by-ref fs target-hash))

;;; refs-from-query : FSCap Hash -> (List Block)
;;; Find all blocks that are referenced BY the block at hash.
;;; Returns the blocks pointed to by the given block's refs.
(define (refs-from-query fs source-hash)
  (let ([source-block (store-get fs source-hash)])
       (if source-block
           (let ([refs (block-refs source-block)])
                (let collect-refs ([i 0]
                                   [result '()])
                     (if (>= i (vector-length refs))
                         (reverse result)
                         (let ([ref-hash (vector-ref refs i)])
                              (let ([ref-block (store-get fs ref-hash)])
                                   (collect-refs (+ i 1)
                                                 (if ref-block
                                                     (cons ref-block result)
                                                     result)))))))
           '())))

;;; refs-transitive : FSCap Hash Integer -> (List Block)
;;; Follow references transitively up to max-depth levels.
;;; Returns all reachable blocks within depth limit.
(define (refs-transitive fs start-hash max-depth)
  (let ([visited (make-hashtable bytevector-hash bytevector=?)]
        [result '()])
       (let bfs ([frontier (list start-hash)]
                 [depth 0])
            (if (or (null? frontier) (>= depth max-depth))
                (reverse result)
                (let ([next-frontier '()])
                     (for-each
                      (lambda (hash)
                              (unless (hashtable-ref visited hash #f)
                                      (hashtable-set! visited hash #t)
                                      (let ([block (store-get fs hash)])
                                           (when block
                                                 (set! result (cons block result))
                                                 (let ([refs (block-refs block)])
                                                      (let add-refs ([i 0])
                                                           (when (< i (vector-length refs))
                                                                 (let ([ref-hash (vector-ref refs i)])
                                                                      (unless (hashtable-ref visited ref-hash #f)
                                                                              (set! next-frontier (cons ref-hash next-frontier))))
                                                                 (add-refs (+ i 1)))))))))
                      frontier)
                     (bfs next-frontier (+ depth 1)))))))

;;; ============================================================
;;; Section 5: Aggregation Functions
;;; ============================================================
;;;
;;; Aggregate operations on query results.

;;; query-count : FSCap QueryExpr -> Integer
;;; Count blocks matching the query.
(define (query-count fs expr)
  (length (query fs expr)))

;;; query-group-by : FSCap Symbol QueryExpr -> Alist
;;; Group matching blocks by a field value.
;;; Returns: ((field-value . (list-of-blocks)) ...)
;;;
;;; Example:
;;;   (query-group-by fs 'tag '())  ; Group all blocks by tag
;;;   -> ((entity . [blocks...]) (relation . [blocks...]))
(define (query-group-by fs field expr)
  (let ([blocks (query fs expr)])
       (let loop ([blocks blocks]
                  [groups '()])
            (if (null? blocks)
                groups
                (let* ([block (car blocks)]
                       [key (extract-field block field)]
                       [existing (assoc-generic key groups)])
                      (if existing
                          (loop (cdr blocks)
                                (map (lambda (pair)
                                             (if (equal? (car pair) key)
                                                 (cons key (cons block (cdr pair)))
                                                 pair))
                                     groups))
                          (loop (cdr blocks)
                                (cons (cons key (list block)) groups))))))))

;;; assoc-generic : Any Alist -> (Maybe Pair)
;;; Association lookup using equal? for comparison.
(define (assoc-generic key alist)
  (let loop ([pairs alist])
       (cond
        [(null? pairs) #f]
        [(equal? (caar pairs) key) (car pairs)]
        [else (loop (cdr pairs))])))

;;; ============================================================
;;; Section 6: Main Query Entry Point
;;; ============================================================
;;;
;;; The `query` function is the main API entry point.
;;; It interprets query expressions and returns matching blocks.

;;; query : FSCap QueryExpr -> (List Block) or (List Alist)
;;; Execute a query and return matching results.
;;;
;;; Query expression types:
;;;
;;;   Pattern queries:
;;;     (query fs '(match (tag . entity)))
;;;     (query fs '(tag . entity))                    ; Shorthand
;;;     (query fs '(match (payload-contains . "x")))
;;;
;;;   Compound queries:
;;;     (query fs '(and (tag . entity) (payload-contains . "Turing")))
;;;     (query fs '(or (tag . entity) (tag . relation)))
;;;     (query fs '(not (tag . entity)))
;;;
;;;   Reference queries:
;;;     (query fs '(refs-to hash))
;;;     (query fs '(refs-from hash))
;;;
;;;   Projection queries:
;;;     (query fs '(select (tag payload-size) (where (tag . entity))))
;;;
;;;   Aggregation:
;;;     (query-count fs '(tag . entity))
;;;     (query-group-by fs 'tag '())
(define (query fs expr)
  (cond
   ;; Empty query - return all blocks
   [(null? expr)
    (store-all-blocks fs)]
   
   ;; refs-from special case (not a predicate)
   [(and (pair? expr) (eq? (car expr) 'refs-from))
    (refs-from-query fs (cadr expr))]
   
   ;; refs-to query
   [(and (pair? expr) (eq? (car expr) 'refs-to))
    (refs-to-query fs (cadr expr))]
   
   ;; refs-transitive query
   [(and (pair? expr) (eq? (car expr) 'refs-transitive))
    (let ([hash (cadr expr)]
          [depth (if (> (length expr) 2) (caddr expr) 3)])
         (refs-transitive fs hash depth))]
   
   ;; Projection query: (select (fields...) (where query))
   [(and (pair? expr) (eq? (car expr) 'select))
    (let* ([fields (cadr expr)]
           [where-clause (caddr expr)]
           [sub-query (cadr where-clause)]
           [blocks (query fs sub-query)])
          (project fields blocks))]
   
   ;; Count query: (count query)
   [(and (pair? expr) (eq? (car expr) 'count))
    (query-count fs (cadr expr))]
   
   ;; Group-by query: (group-by field query)
   [(and (pair? expr) (eq? (car expr) 'group-by))
    (let ([field (cadr expr)]
          [sub-query (caddr expr)])
         (query-group-by fs field sub-query))]
   
   ;; Standard predicate query
   [else
    (let ([predicate (interpret-query expr)])
         (store-filter fs predicate))]))

;;; ============================================================
;;; Section 7: Query Convenience Functions
;;; ============================================================
;;;
;;; Shorthand functions for common query patterns.

;;; find-entities : FSCap -> (List Block)
;;; Find all entity blocks.
(define (find-entities fs)
  (query fs '(tag . entity)))

;;; find-relations : FSCap -> (List Block)
;;; Find all relation blocks.
(define (find-relations fs)
  (query fs '(tag . relation)))

;;; find-collections : FSCap -> (List Block)
;;; Find all collection blocks.
(define (find-collections fs)
  (query fs '(tag . collection)))

;;; find-by-content : FSCap String -> (List Block)
;;; Find blocks whose payload contains the given string.
(define (find-by-content fs substring)
  (query fs `(payload-contains . ,substring)))

;;; find-with-refs : FSCap -> (List Block)
;;; Find all blocks that have at least one reference.
(define (find-with-refs fs)
  (query fs '(has-refs . #t)))

;;; find-orphans : FSCap -> (List Block)
;;; Find blocks that are not referenced by any other block.
;;; These are "root" blocks or potentially orphaned data.
(define (find-orphans fs)
  (let* ([all-blocks (store-all-blocks fs)]
         [all-hashes (map hash-block all-blocks)]
         [referenced-hashes (make-hashtable bytevector-hash bytevector=?)])
        ;; Collect all referenced hashes
        (for-each
         (lambda (block)
                 (let ([refs (block-refs block)])
                      (let loop ([i 0])
                           (when (< i (vector-length refs))
                                 (hashtable-set! referenced-hashes (vector-ref refs i) #t)
                                 (loop (+ i 1))))))
         all-blocks)
        ;; Filter to blocks not in referenced set
        (filter
         (lambda (block)
                 (not (hashtable-ref referenced-hashes (hash-block block) #f)))
         all-blocks)))

;;; ============================================================
;;; Section 8: Query Composition Helpers
;;; ============================================================
;;;
;;; Functions for building complex queries programmatically.

;;; make-tag-query : Symbol -> QueryExpr
;;; Create a tag match query expression.
(define (make-tag-query tag)
  `(tag . ,tag))

;;; make-content-query : String -> QueryExpr
;;; Create a content search query expression.
(define (make-content-query substring)
  `(payload-contains . ,substring))

;;; make-and-query : (List QueryExpr) -> QueryExpr
;;; Combine queries with AND.
(define (make-and-query queries)
  `(and ,@queries))

;;; make-or-query : (List QueryExpr) -> QueryExpr
;;; Combine queries with OR.
(define (make-or-query queries)
  `(or ,@queries))

;;; make-not-query : QueryExpr -> QueryExpr
;;; Negate a query.
(define (make-not-query query)
  `(not ,query))

;;; make-select-query : (List Symbol) QueryExpr -> QueryExpr
;;; Create a projection query.
(define (make-select-query fields where-query)
  `(select ,fields (where ,where-query)))

;;; ============================================================
;;; Section 9: Utility Functions
;;; ============================================================

;;; query-string-contains? : String String -> Boolean
;;; Check if haystack contains needle as a substring.
;;; Optimized: char-by-char comparison avoids O(N*M) substring allocations.
(define (query-string-contains? haystack needle)
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
                    [else (outer (+ i 1))])))]))
  
  ;;; bytevector-hash : Bytevector -> Integer
  ;;; Hash function for bytevectors (for use in hashtables).
  (define (bytevector-hash bv)
    (let ([len (bytevector-length bv)])
         (let loop ([i 0] [h 0])
              (if (>= i len)
                  (modulo (abs h) (expt 2 30))
                  (loop (+ i 1)
                        (+ (* h 31) (bytevector-u8-ref bv i)))))))
  
  ;;; ============================================================
  ;;; Section 10: Query Result Formatting
  ;;; ============================================================
  ;;;
  ;;; Functions for displaying query results.
  
  ;;; print-query-results : (List Block) -> Void
  ;;; Print a list of blocks in a human-readable format.
  (define (print-query-results blocks)
    (printf "Query returned ~a result(s):\n" (length blocks))
    (for-each
     (lambda (block)
             (printf "  [~a] ~a bytes, ~a refs\n"
                     (block-tag block)
                     (bytevector-length (block-payload block))
                     (vector-length (block-refs block))))
     blocks))
  
  ;;; print-projection-results : (List Alist) -> Void
  ;;; Print projection results (alists).
  (define (print-projection-results results)
    (printf "Query returned ~a result(s):\n" (length results))
    (for-each
     (lambda (alist)
             (printf "  {")
             (let loop ([pairs alist] [first #t])
                  (when (pair? pairs)
                        (unless first (printf ", "))
                        (printf "~a: ~a" (caar pairs) (cdar pairs))
                        (loop (cdr pairs) #f)))
             (printf "}\n"))
     results))
  
  ;;; ============================================================
  ;;; Module Export Summary
  ;;; ============================================================
  ;;;
  ;;; Main API:
  ;;;   query              - Execute a query expression
  ;;;   query-count        - Count matching blocks
  ;;;   query-group-by     - Group results by field
  ;;;
  ;;; Convenience functions:
  ;;;   find-entities      - Find entity blocks
  ;;;   find-relations     - Find relation blocks
  ;;;   find-collections   - Find collection blocks
  ;;;   find-by-content    - Find by payload content
  ;;;   find-with-refs     - Find blocks with references
  ;;;   find-orphans       - Find unreferenced blocks
  ;;;
  ;;; Query builders:
  ;;;   make-tag-query     - Create tag match query
  ;;;   make-content-query - Create content search query
  ;;;   make-and-query     - Create AND query
  ;;;   make-or-query      - Create OR query
  ;;;   make-not-query     - Create NOT query
  ;;;   make-select-query  - Create projection query
  ;;;
  ;;; Reference traversal:
  ;;;   refs-to-query      - Find blocks referencing hash
  ;;;   refs-from-query    - Find blocks referenced by hash
  ;;;   refs-transitive    - Transitive reference closure
  ;;;
  ;;; Display:
  ;;;   print-query-results      - Print block list
  ;;;   print-projection-results - Print projection results
  
  (printf "Query DSL loaded.\n")
  (printf "  Use (query fs expr) to execute queries.\n")
  (printf "  Examples:\n")
  (printf "    (query fs '(tag . entity))\n")
  (printf "    (query fs '(and (tag . entity) (payload-contains . \"Turing\")))\n")
  (printf "    (query fs '(select (tag payload-size) (where (tag . entity))))\n")
