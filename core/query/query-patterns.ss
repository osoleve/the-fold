;;; core/query/query-patterns.ss --- Datalog-style Pattern Matching with Variable Binding
;;;
;;; Extends query-dsl.ss with advanced pattern matching capabilities:
;;;   - Variable binding (symbols starting with ?)
;;;   - Multi-pattern joins based on shared variables
;;;   - Relation pattern matching
;;;   - Constraint evaluation
;;;
;;; SYNTAX:
;;;
;;;   Pattern matching with variables:
;;;     (?subject relation-type ?object)
;;;     (?entity attribute-name ?value)
;;;
;;;   Multi-pattern joins:
;;;     (pattern-query fs
;;;       '((?person invented ?concept))
;;;       '((?concept year ?y))
;;;       '(< ?y 1940))
;;;
;;;   Returns list of binding environments:
;;;     (((person . hash1) (concept . hash2) (y . 1936))
;;;      ((person . hash3) (concept . hash4) (y . 1938))
;;;      ...)
;;;
;;; DESIGN:
;;;   - Variables are symbols starting with ?
;;;   - Constants are other values
;;;   - Patterns match against relation blocks
;;;   - Shared variables create joins
;;;   - Constraints filter results
;;;
;;; TIER: 7-8 (Complex computation with search)

;;; ============================================================
;;; Variable Recognition
;;; ============================================================

;;; variable? : Any -> Boolean
;;; Check if a symbol represents a variable (starts with ?).
(define (variable? x)
  (and (symbol? x)
       (let ([str (symbol->string x)])
            (and (> (string-length str) 0)
                 (char=? (string-ref str 0) #\?)))))

;;; variable-name : Symbol -> String
;;; Extract the name of a variable (without the ?).
(define (variable-name var)
  (let ([str (symbol->string var)])
       (substring str 1 (string-length str))))

;;; ============================================================
;;; Binding Environments
;;; ============================================================

;;; An environment is an association list: ((var . value) ...)
;;; Variables map to block hashes or extracted values.

;;; empty-env : Env
(define empty-env '())

;;; extend-env : Env Symbol Value -> Env
;;; Add a new binding to the environment.
(define (extend-env env var value)
  (cons (cons var value) env))

;;; lookup-env : Env Symbol -> (Maybe Value)
;;; Look up a variable's value in the environment.
(define (lookup-env env var)
  (let ([binding (assq var env)])
       (if binding (cdr binding) #f)))

;;; env-bound? : Env Symbol -> Boolean
;;; Check if a variable is bound in the environment.
(define (env-bound? env var)
  (if (assq var env) #t #f))

;;; merge-envs : Env Env -> (Maybe Env)
;;; Merge two environments, return #f if incompatible.
(define (merge-envs env1 env2)
  (let loop ([pairs env2]
             [result env1])
       (if (null? pairs)
           result
           (let* ([var (caar pairs)]
                  [val (cdar pairs)]
                  [existing (lookup-env result var)])
                 (cond
                  [(not existing)
                   (loop (cdr pairs) (extend-env result var val))]
                  [(bytevector=? existing val)
                   (loop (cdr pairs) result)]
                  [else #f])))))

;;; ============================================================
;;; Pattern Matching
;;; ============================================================

;;; A pattern is: (?var1 relation-type ?var2)
;;; Matches relation blocks where:
;;;   - First ref binds to ?var1
;;;   - Payload contains relation-type
;;;   - Second ref binds to ?var2

;;; match-pattern : FSCap Pattern Env -> (List Env)
;;; Match a pattern against all blocks, return list of extended environments.
;;;
;;; Pattern forms:
;;;   (?subject relation-name ?object) - Match binary relation
;;;   (?entity attribute-name constant) - Match attribute with constant value
;;;   (constant attribute-name ?value)  - Match attribute of specific entity
(define (match-pattern fs pattern env)
  (cond
   ;; Binary relation pattern: (?x relation-type ?y)
   [(and (list? pattern) (= (length pattern) 3))
    (match-binary-relation fs pattern env)]
   
   ;; Other pattern types can be added here
   [else
    (error 'match-pattern "Unknown pattern type" pattern)]))

;;; match-binary-relation : FSCap Pattern Env -> (List Env)
;;; Match a binary relation pattern (?subject rel-type ?object).
(define (match-binary-relation fs pattern env)
  (let ([subject-var (car pattern)]
        [relation-type (cadr pattern)]
        [object-var (caddr pattern)])
       
       ;; Get all relation blocks
       (let ([relation-blocks (query fs '(tag . relation))])
            
            ;; Filter and bind
            (let loop ([blocks relation-blocks]
                       [results '()])
                 (if (null? blocks)
                     results
                     (let* ([block (car blocks)]
                            [payload-str (utf8->string (block-payload block))]
                            [refs (block-refs block)])
                           
                           ;; Check if payload matches relation type
                           (if (and (>= (vector-length refs) 2)
                                    (or (eq? relation-type '?)
                                        (query-string-contains? payload-str
                                                                (symbol->string relation-type))))
                               
                               ;; Try to bind subject and object
                               (let ([new-env (try-bind-relation env
                                                                 subject-var (vector-ref refs 0)
                                                                 object-var (vector-ref refs 1))])
                                    (if new-env
                                        (loop (cdr blocks) (cons new-env results))
                                        (loop (cdr blocks) results)))
                               
                               (loop (cdr blocks) results))))))))

;;; try-bind-relation : Env Symbol Hash Symbol Hash -> (Maybe Env)
;;; Try to bind subject and object variables to hashes.
;;; Returns extended environment or #f if binding conflicts.
(define (try-bind-relation env subject-var subject-hash object-var object-hash)
  (let ([env1 (try-bind-var env subject-var subject-hash)])
       (if env1
           (try-bind-var env1 object-var object-hash)
           #f)))

;;; try-bind-var : Env Symbol Hash -> (Maybe Env)
;;; Try to bind a variable to a hash value.
;;; If variable, check for conflicts and extend.
;;; If constant (hash), check for equality.
(define (try-bind-var env var value)
  (cond
   ;; Variable - check if already bound
   [(variable? var)
    (let ([existing (lookup-env env var)])
         (cond
          [(not existing) (extend-env env var value)]
          [(bytevector=? existing value) env]
          [else #f]))]
   
   ;; Constant hash - check equality
   [(bytevector? var)
    (if (bytevector=? var value) env #f)]
   
   ;; Wildcard
   [(eq? var '?) env]
   
   ;; Unknown
   [else #f]))

;;; ============================================================
;;; Multi-Pattern Join
;;; ============================================================

;;; join-patterns : FSCap (List Pattern) -> (List Env)
;;; Execute multiple patterns and join results based on shared variables.
;;;
;;; Example:
;;;   (join-patterns fs
;;;     '((?person invented ?concept)
;;;       (?concept year ?y)))
;;;
;;;   Returns environments where:
;;;     - ?person invented ?concept
;;;     - ?concept has year ?y
;;;     - Same ?concept binding in both patterns
(define (join-patterns fs patterns)
  (if (null? patterns)
      (list empty-env)
      (join-pattern-list fs patterns (list empty-env))))

;;; join-pattern-list : FSCap (List Pattern) (List Env) -> (List Env)
;;; Recursively join patterns, accumulating environments.
(define (join-pattern-list fs patterns envs)
  (if (null? patterns)
      envs
      (let ([pattern (car patterns)]
            [rest-patterns (cdr patterns)])
           
           ;; For each existing environment, match the pattern
           (let ([new-envs
                  (apply append
                         (map (lambda (env)
                                      (match-pattern fs pattern env))
                              envs))])
                
                ;; Continue with remaining patterns
                (join-pattern-list fs rest-patterns new-envs)))))

;;; ============================================================
;;; Constraint Evaluation
;;; ============================================================

;;; extract-numeric-value : Any -> Number | #f
;;; Extract a numeric value from various representations.
;;; Handles: numbers, bytevectors containing numeric S-expressions, strings.
;;;
;;; NOTE: Bytevectors are assumed to contain UTF-8 encoded text.
;;; Non-UTF-8 data or unparseable content returns #f (comparison fails).
;;; This is intentional - query constraints should fail safely on bad data.
(define (extract-numeric-value val)
  (cond
   [(number? val) val]
   [(bytevector? val)
    ;; Try to parse as S-expression containing a number
    (guard (e [else #f])
           (let ([parsed (read (open-string-input-port (utf8->string val)))])
                (if (number? parsed) parsed #f)))]
   [(string? val)
    ;; Try to parse string as number
    (guard (e [else #f])
           (let ([parsed (string->number val)])
                (if parsed parsed #f)))]
   [else #f]))

;;; eval-constraint : Constraint Env -> Boolean
;;; Evaluate a constraint against an environment.
;;;
;;; Constraints:
;;;   (< ?var constant)
;;;   (> ?var constant)
;;;   (= ?var constant)
;;;   (string=? ?var constant)
(define (eval-constraint constraint env)
  (if (not (list? constraint))
      #t
      (let ([op (car constraint)]
            [arg1 (cadr constraint)]
            [arg2 (caddr constraint)])
           
           ;; Resolve arguments
           (let ([val1 (if (variable? arg1) (lookup-env env arg1) arg1)]
                 [val2 (if (variable? arg2) (lookup-env env arg2) arg2)])
                
                ;; If either value not found, constraint fails
                (if (or (not val1) (not val2))
                    #f
                    
                    ;; Apply operator
                    (case op
                          [(< <= > >= =)
                           ;; Numeric comparison - properly extract numeric values
                           (let ([n1 (extract-numeric-value val1)]
                                 [n2 (extract-numeric-value val2)])
                                (if (and n1 n2)
                                    ((case op
                                           [(<) <]
                                           [(<=) <=]
                                           [(>) >]
                                           [(>=) >=]
                                           [(=) =]) n1 n2)
                                    #f))]  ; Comparison fails if can't extract numbers
                          
                          [(string=? string-contains?)
                           ;; String comparison
                           (let ([s1 (if (bytevector? val1) (utf8->string val1) val1)]
                                 [s2 (if (bytevector? val2) (utf8->string val2) val2)])
                                (if (eq? op 'string=?)
                                    (string=? s1 s2)
                                    (query-string-contains? s1 s2)))]
                          
                          [else #t]))))))

;;; ============================================================
;;; Main Pattern Query API
;;; ============================================================

;;; pattern-query : FSCap (List Pattern) [(List Constraint)] -> (List Env)
;;; Execute a multi-pattern query with optional constraints.
;;;
;;; Example:
;;;   (pattern-query fs
;;;     '((?person invented ?concept)
;;;       (?concept year ?y))
;;;     '((< ?y 1940)))
;;;
;;; Returns list of environments satisfying all patterns and constraints.
(define pattern-query
  (case-lambda
   [(fs patterns)
    (join-patterns fs patterns)]
   
   [(fs patterns constraints)
    (let ([envs (join-patterns fs patterns)])
         ;; Filter by constraints
         (filter (lambda (env)
                         (andmap (lambda (constraint)
                                         (eval-constraint constraint env))
                                 constraints))
                 envs))]))

;;; ============================================================
;;; Result Projection
;;; ============================================================

;;; project-vars : (List Env) (List Symbol) -> (List Alist)
;;; Project specific variables from environments.
;;;
;;; Example:
;;;   (project-vars envs '(?person ?concept))
;;;   -> (((person . hash1) (concept . hash2)) ...)
(define (project-vars envs vars)
  (map (lambda (env)
               (map (lambda (var)
                            (cons (string->symbol (variable-name var))
                                  (lookup-env env var)))
                    vars))
       envs))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; find-pattern : FSCap Pattern -> (List Hash)
;;; Find all blocks matching a single pattern, return bound values.
(define (find-pattern fs pattern)
  (let ([envs (match-pattern fs pattern empty-env)])
       (map (lambda (env)
                    (map cdr env))
            envs)))

;;; count-pattern : FSCap (List Pattern) -> Integer
;;; Count results matching pattern query.
(define (count-pattern fs patterns)
  (length (pattern-query fs patterns)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; query-string-contains? : String String -> Boolean
;;; Helper for string containment check (may already exist in query-dsl.ss).
(define (query-string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i n-len) h-len) #f]
             [(string=? (substring haystack i (+ i n-len)) needle) #t]
             [else (loop (+ i 1))]))))

(printf "Query pattern matching loaded\n")
(printf "  Advanced Datalog-style patterns with variable binding\n")
(printf "  Usage: (pattern-query fs '((?x rel ?y)) '((< ?y 100)))\n")
