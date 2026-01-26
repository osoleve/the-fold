(load "core/base/prelude.ss")
(load "core/types/types.ss")

(doc 'module 'pattern-check)
(doc 'description "Analyzes pattern matches for exhaustiveness, redundancy, and usefulness. Based on 'Warnings for Pattern Matching' (Maranget, JFP 2007).")
(doc 'layer 'core)
(doc 'requires '(prelude types))

(doc 'note "Analyzes pattern matches for: 1. Exhaustiveness - are all cases covered? 2. Redundancy - are any patterns unreachable? 3. Usefulness - does each pattern match something new?")

(doc 'section 'pattern-representation)

(doc 'note "Patterns are represented as: (ctor tag subpatterns...) - constructor pattern, (wildcard) - matches anything, (var name) - variable pattern (like wildcard), (literal value) - literal value, (or pat1 pat2 ...) - or-pattern (any of), (guard pat expr) - guarded pattern")

(define-record-type pattern
  (fields
   kind    ; 'ctor | 'wildcard | 'var | 'literal | 'or | 'guard
   data))  ; depends on kind

(doc 'note "Pattern constructors")

(define (make-ctor-pattern tag subpats)
  (doc 'type '(-> Symbol (List Pattern) Pattern))
  (doc 'description "Create a constructor pattern.")
  (doc 'export #t)
  (make-pattern 'ctor (cons tag subpats)))

(define (make-wildcard-pattern)
  (doc 'type '(-> Pattern))
  (doc 'description "Create a wildcard pattern.")
  (doc 'export #t)
  (make-pattern 'wildcard #f))

(define (make-var-pattern name)
  (doc 'type '(-> Symbol Pattern))
  (doc 'description "Create a variable pattern.")
  (doc 'export #t)
  (make-pattern 'var name))

(define (make-literal-pattern value)
  (doc 'type '(-> Any Pattern))
  (doc 'description "Create a literal pattern.")
  (doc 'export #t)
  (make-pattern 'literal value))

(define (make-or-pattern pats)
  (doc 'type '(-> (List Pattern) Pattern))
  (doc 'description "Create an or-pattern.")
  (doc 'export #t)
  (make-pattern 'or pats))

(define (make-guard-pattern pat guard)
  (doc 'type '(-> Pattern Expr Pattern))
  (doc 'description "Create a guarded pattern.")
  (doc 'export #t)
  (make-pattern 'guard (cons pat guard)))

(doc 'note "Pattern predicates")

(doc ctor-pattern? 'type '(-> Pattern Bool))
(doc ctor-pattern? 'export #t)
(define (ctor-pattern? p) (eq? (pattern-kind p) 'ctor))

(doc wildcard-pattern? 'type '(-> Pattern Bool))
(doc wildcard-pattern? 'export #t)
(define (wildcard-pattern? p) (eq? (pattern-kind p) 'wildcard))

(doc var-pattern? 'type '(-> Pattern Bool))
(doc var-pattern? 'export #t)
(define (var-pattern? p) (eq? (pattern-kind p) 'var))

(doc literal-pattern? 'type '(-> Pattern Bool))
(doc literal-pattern? 'export #t)
(define (literal-pattern? p) (eq? (pattern-kind p) 'literal))

(doc or-pattern? 'type '(-> Pattern Bool))
(doc or-pattern? 'export #t)
(define (or-pattern? p) (eq? (pattern-kind p) 'or))

(doc guard-pattern? 'type '(-> Pattern Bool))
(doc guard-pattern? 'export #t)
(define (guard-pattern? p) (eq? (pattern-kind p) 'guard))

(doc 'note "Pattern accessors")

(doc ctor-pattern-tag 'type '(-> Pattern Symbol))
(doc ctor-pattern-tag 'export #t)
(define (ctor-pattern-tag p)
  (let ([data (pattern-data p)])
       (if (pair? data)
           (car data)
           data)))

(doc ctor-pattern-subpats 'type '(-> Pattern (List Pattern)))
(doc ctor-pattern-subpats 'export #t)
(define (ctor-pattern-subpats p)
  (let ([data (pattern-data p)])
       (if (pair? data)
           (cdr data)
           '())))

(doc var-pattern-name 'type '(-> Pattern Symbol))
(doc var-pattern-name 'export #t)
(define (var-pattern-name p) (pattern-data p))

(doc literal-pattern-value 'type '(-> Pattern Any))
(doc literal-pattern-value 'export #t)
(define (literal-pattern-value p) (pattern-data p))

(doc or-pattern-alternatives 'type '(-> Pattern (List Pattern)))
(doc or-pattern-alternatives 'export #t)
(define (or-pattern-alternatives p) (pattern-data p))

(doc guard-pattern-pat 'type '(-> Pattern Pattern))
(doc guard-pattern-pat 'export #t)
(define (guard-pattern-pat p) (car (pattern-data p)))

(doc guard-pattern-guard 'type '(-> Pattern Expr))
(doc guard-pattern-guard 'export #t)
(define (guard-pattern-guard p) (cdr (pattern-data p)))

(define (wildcard? p)
  (doc 'type '(-> Pattern Bool))
  (doc 'description "Is this pattern equivalent to a wildcard (matches everything)?")
  (doc 'export #t)
  (or (wildcard-pattern? p)
      (var-pattern? p)))

(doc 'section 'type-information)

(doc 'note "Type constructors are defined as: (type-info name arity) where arity is the number of fields")

(define-record-type type-info
  (fields
   name            ; Symbol - type name
   constructors))  ; (List (ctor-name × arity))

(define-record-type ctor-info
  (fields
   name    ; Symbol
   arity)) ; Nat

(doc 'note "Built-in type information")

(doc bool-type-info 'type 'TypeInfo)
(doc bool-type-info 'export #t)
(define bool-type-info
  (make-type-info 'Bool
                  (list (make-ctor-info 'true 0)
                        (make-ctor-info 'false 0))))

(doc list-type-info 'type 'TypeInfo)
(doc list-type-info 'export #t)
(define list-type-info
  (make-type-info 'List
                  (list (make-ctor-info 'nil 0)
                        (make-ctor-info 'cons 2))))

(doc maybe-type-info 'type 'TypeInfo)
(doc maybe-type-info 'export #t)
(define maybe-type-info
  (make-type-info 'Maybe
                  (list (make-ctor-info 'nothing 0)
                        (make-ctor-info 'just 1))))

(doc either-type-info 'type 'TypeInfo)
(doc either-type-info 'export #t)
(define either-type-info
  (make-type-info 'Either
                  (list (make-ctor-info 'left 1)
                        (make-ctor-info 'right 1))))

(doc nat-type-info 'type 'TypeInfo)
(doc nat-type-info 'export #t)
(define nat-type-info
  (make-type-info 'Nat
                  (list (make-ctor-info 'zero 0)
                        (make-ctor-info 'succ 1))))

(doc pair-type-info 'type 'TypeInfo)
(doc pair-type-info 'export #t)
(define pair-type-info
  (make-type-info 'Pair
                  (list (make-ctor-info 'pair 2))))

(doc unit-type-info 'type 'TypeInfo)
(doc unit-type-info 'export #t)
(define unit-type-info
  (make-type-info 'Unit
                  (list (make-ctor-info 'unit 0))))

(doc *type-registry* 'type '(List (× Symbol TypeInfo)))
(doc *type-registry* 'description "Standard type registry.")
(doc *type-registry* 'export #t)
(define *type-registry*
  (list (cons 'Bool bool-type-info)
        (cons 'List list-type-info)
        (cons 'Maybe maybe-type-info)
        (cons 'Either either-type-info)
        (cons 'Nat nat-type-info)
        (cons 'Pair pair-type-info)
        (cons 'Unit unit-type-info)))

(define (lookup-type-info type-name)
  (doc 'type '(-> Symbol (+ TypeInfo #f)))
  (doc 'description "Look up type information by name.")
  (doc 'export #t)
  (let ([entry (assq type-name *type-registry*)])
       (if entry (cdr entry) #f)))

(define (register-type! name ctors)
  (doc 'type '(-> Symbol (List (× Symbol Nat)) Unit))
  (doc 'description "Register a new type in the type registry.")
  (doc 'export #t)
  (set! *type-registry*
        (cons (cons name
                    (make-type-info name
                                    (map (lambda (c)
                                                 (make-ctor-info (car c) (cdr c)))
                                         ctors)))
              *type-registry*)))

(define (type-constructors ti)
  (doc 'type '(-> TypeInfo (List CtorInfo)))
  (doc 'description "Extract constructors from type info.")
  (doc 'export #t)
  (type-info-constructors ti))

(define (known-ctor? name)
  (doc 'type '(-> Symbol Bool))
  (doc 'description "Check if a symbol is a registered constructor name.")
  (doc 'export #t)
  (if (not (symbol? name))
      #f
      (let loop ([types *type-registry*])
           (if (null? types)
               #f
               (let* ([type-info (cdar types)]
                      [ctors (type-info-constructors type-info)])
                     (if (ormap (lambda (c) (eq? (ctor-info-name c) name)) ctors)
                         #t
                         (loop (cdr types))))))))

(doc 'section 'pattern-parsing)

(define (parse-pattern sexpr)
  (doc 'type '(-> SExpr Pattern))
  (doc 'description "Convert S-expression pattern to Pattern record.")
  (doc 'export #t)
  (cond
   ;; Wildcard
   [(eq? sexpr '_)
    (make-wildcard-pattern)]
   
   ;; Boolean literals (check BEFORE variable pattern!)
   [(eq? sexpr '#t) (make-literal-pattern #t)]
   [(eq? sexpr '#f) (make-literal-pattern #f)]
   [(eq? sexpr 'true) (make-ctor-pattern 'true '())]
   [(eq? sexpr 'false) (make-ctor-pattern 'false '())]
   
   ;; Known nullary constructors
   [(memq sexpr '(nil nothing zero unit))
    (make-ctor-pattern sexpr '())]
   
   ;; Check if it's a registered constructor
   [(known-ctor? sexpr)
    (make-ctor-pattern sexpr '())]
   
   ;; Variable (lowercase symbol, but not a known constructor)
   [(and (symbol? sexpr)
         (char-lower-case? (string-ref (symbol->string sexpr) 0)))
    (make-var-pattern sexpr)]
   
   ;; Number literal
   [(number? sexpr)
    (make-literal-pattern sexpr)]
   
   ;; String literal
   [(string? sexpr)
    (make-literal-pattern sexpr)]
   
   ;; Empty list
   [(null? sexpr)
    (make-ctor-pattern 'nil '())]
   
   ;; Quote
   [(and (pair? sexpr) (eq? (car sexpr) 'quote))
    (let ([datum (cadr sexpr)])
         (if (null? datum)
             (make-ctor-pattern 'nil '())
             (make-literal-pattern datum)))]
   
   ;; Constructor pattern
   [(and (pair? sexpr) (symbol? (car sexpr)))
    (let ([tag (car sexpr)]
          [subpats (map parse-pattern (cdr sexpr))])
         (make-ctor-pattern tag subpats))]
   
   ;; Or-pattern
   [(and (pair? sexpr) (eq? (car sexpr) 'or))
    (make-or-pattern (map parse-pattern (cdr sexpr)))]
   
   ;; Guard
   [(and (pair? sexpr) (eq? (car sexpr) 'when))
    (make-guard-pattern (parse-pattern (cadr sexpr)) (caddr sexpr))]
   
   ;; Default: treat as constructor with no args
   [(symbol? sexpr)
    (make-ctor-pattern sexpr '())]
   
   [else
    (make-literal-pattern sexpr)]))

(doc 'section 'pattern-matrix)

(doc 'note "A pattern matrix is a list of pattern rows. Each row is a list of patterns (one per column).")

(define (specialize matrix tag arity)
  (doc 'type '(-> PatternMatrix Symbol Nat PatternMatrix))
  (doc 'description "Specialize matrix for constructor with given arity. Constructor patterns matching tag: replace with subpatterns. Wildcards: replace with arity wildcards. Other constructors: remove row.")
  (doc 'export #t)
  (filter-map
   (lambda (row)
           (let ([first (car row)]
                 [rest (cdr row)])
                (cond
                 ;; Constructor pattern with matching tag
                 [(and (ctor-pattern? first)
                       (eq? (ctor-pattern-tag first) tag))
                  (append (ctor-pattern-subpats first) rest)]
                 
                 ;; Wildcard or variable: expand to arity wildcards
                 [(wildcard? first)
                  (append (make-list arity (make-wildcard-pattern)) rest)]
                 
                 ;; Or-pattern: try each alternative
                 [(or-pattern? first)
                  (let ([specialized
                         (filter-map
                          (lambda (alt)
                                  (let ([alt-row (cons alt rest)])
                                       (specialize (list alt-row) tag arity)))
                          (or-pattern-alternatives first))])
                       (if (null? specialized)
                           #f
                           (car (car specialized))))]
                 
                 ;; Other constructor: doesn't match
                 [else #f])))
   matrix))

(define (default-matrix matrix)
  (doc 'type '(-> PatternMatrix PatternMatrix))
  (doc 'description "Get the default matrix (rows that match wildcards).")
  (doc 'export #t)
  (filter-map
   (lambda (row)
           (let ([first (car row)]
                 [rest (cdr row)])
                (cond
                 [(wildcard? first) rest]
                 [(or-pattern? first)
                  ;; Check if any alternative is a wildcard
                  (if (ormap wildcard? (or-pattern-alternatives first))
                      rest
                      #f)]
                 [else #f])))
   matrix))

(doc 'section 'exhaustiveness-checking)

(define (exhaustive? matrix types)
  (doc 'type '(-> PatternMatrix (List TypeInfo) Bool))
  (doc 'description "Check if pattern matrix is exhaustive for given types.")
  (doc 'export #t)
  (null? (find-missing-patterns matrix types)))

(define (find-missing-patterns matrix types)
  (doc 'type '(-> PatternMatrix (List TypeInfo) (List Pattern)))
  (doc 'description "Find patterns not covered by the matrix.")
  (doc 'export #t)
  (cond
   ;; No columns: exhaustive if any row, missing if no rows
   [(null? types)
    (if (null? matrix)
        (list '())  ; Missing: the empty pattern
        '())]       ; Exhaustive
   
   ;; No rows: not exhaustive - construct missing pattern
   [(null? matrix)
    (list (make-list (length types) (make-wildcard-pattern)))]
   
   [else
    (let* ([type (car types)]
           [rest-types (cdr types)]
           [ctors (if type (type-constructors type) '())])
          (if (null? ctors)
              ;; Unknown type: check default matrix
              (let ([missing (find-missing-patterns (default-matrix matrix) rest-types)])
                   (map (lambda (m) (cons (make-wildcard-pattern) m)) missing))
              ;; Known type: check each constructor
              (let loop ([cs ctors] [missing '()])
                   (if (null? cs)
                       missing
                       (let* ([ctor (car cs)]
                              [tag (ctor-info-name ctor)]
                              [arity (ctor-info-arity ctor)]
                              [spec-matrix (specialize matrix tag arity)]
                              [sub-types (make-list arity #f)]  ; Unknown subtypes
                              [ctor-missing
                               (find-missing-patterns spec-matrix
                                                      (append sub-types rest-types))])
                             (loop (cdr cs)
                                   (append missing
                                           (map (lambda (m)
                                                        (let ([subpats (take arity m)]
                                                              [rest (drop arity m)])
                                                             (cons (make-ctor-pattern tag subpats)
                                                                   rest)))
                                                ctor-missing))))))))]))

(doc 'section 'redundancy-checking)

(define (useful? row matrix types)
  (doc 'type '(-> Pattern PatternMatrix (List TypeInfo) Bool))
  (doc 'description "Is this pattern useful (matches something the matrix doesn't)?")
  (doc 'export #t)
  (cond
   ;; No columns: useful if matrix has no rows
   [(null? types)
    (null? matrix)]
   
   ;; No rows in matrix: pattern is useful (matches everything)
   [(null? matrix)
    #t]
   
   [else
    (let* ([pat (car row)]
           [rest (cdr row)]
           [type (car types)]
           [rest-types (cdr types)])
          (cond
           ;; Wildcard: useful if default matrix leaves gaps
           [(wildcard? pat)
            (let* ([ctors (if type (type-constructors type) '())]
                   [dm (default-matrix matrix)])
                  (if (null? ctors)
                      ;; Unknown type: check default
                      (useful? rest dm rest-types)
                      ;; Known type: useful if any ctor is useful
                      (ormap (lambda (ctor)
                                     (let* ([tag (ctor-info-name ctor)]
                                            [arity (ctor-info-arity ctor)]
                                            [sub-row (append (make-list arity (make-wildcard-pattern))
                                                             rest)]
                                            [spec-matrix (specialize matrix tag arity)]
                                            [sub-types (append (make-list arity #f) rest-types)])
                                           (useful? sub-row spec-matrix sub-types)))
                             ctors)))]
           
           ;; Constructor: specialize on this constructor
           [(ctor-pattern? pat)
            (let* ([tag (ctor-pattern-tag pat)]
                   [subpats (ctor-pattern-subpats pat)]
                   [arity (length subpats)]
                   [new-row (append subpats rest)]
                   [spec-matrix (specialize matrix tag arity)]
                   [sub-types (append (make-list arity #f) rest-types)])
                  (useful? new-row spec-matrix sub-types))]
           
           ;; Literal: treat as constructor with no args
           [(literal-pattern? pat)
            (let* ([value (literal-pattern-value pat)]
                   [spec-matrix (specialize matrix value 0)])
                  (useful? rest spec-matrix rest-types))]
           
           ;; Or-pattern: useful if any alternative is useful
           [(or-pattern? pat)
            (ormap (lambda (alt)
                           (useful? (cons alt rest) matrix types))
                   (or-pattern-alternatives pat))]
           
           ;; Guard: conservatively assume useful (guards aren't statically checkable)
           [(guard-pattern? pat)
            #t]
           
           [else #t]))]))

(define (find-redundant matrix types)
  (doc 'type '(-> PatternMatrix (List TypeInfo) (List Nat)))
  (doc 'description "Find indices of redundant rows in the matrix.")
  (doc 'export #t)
  (let loop ([rows matrix]
             [preceding '()]
             [idx 0]
             [redundant '()])
       (if (null? rows)
           (reverse redundant)
           (let* ([row (car rows)]
                  [is-useful (useful? row preceding types)]
                  [new-preceding (append preceding (list row))])
                 (loop (cdr rows)
                       new-preceding
                       (+ idx 1)
                       (if is-useful
                           redundant
                           (cons idx redundant)))))))

(doc 'section 'case-expression-analysis)

(define (analyze-case expr type-info)
  (doc 'type '(-> Expr TypeInfo CaseAnalysis))
  (doc 'description "Analyze a case expression for exhaustiveness and redundancy.")
  (doc 'export #t)
  (if (and (pair? expr) (eq? (car expr) 'case))
      (let* ([scrutinee (cadr expr)]
             [clauses (cddr expr)]
             [patterns (map (lambda (c) (list (parse-pattern (car c)))) clauses)]
             [missing (find-missing-patterns patterns (list type-info))]
             [redundant (find-redundant patterns (list type-info))])
            `((exhaustive . ,(null? missing))
              (missing . ,(map pattern->sexpr (flatten-patterns missing)))
              (redundant . ,redundant)
              (clause-count . ,(length clauses))))
      `((error . not-a-case-expression))))

(define (flatten-patterns pss)
  (doc 'type '(-> (List (List Pattern)) (List Pattern)))
  (doc 'description "Flatten pattern lists.")
  (map (lambda (ps) (if (= (length ps) 1) (car ps) ps)) pss))

(doc 'section 'pattern-pretty-printing)

(define (pattern->sexpr p)
  (doc 'type '(-> Pattern SExpr))
  (doc 'description "Convert pattern to S-expression for display.")
  (doc 'export #t)
  (cond
   [(wildcard-pattern? p) '_]
   [(var-pattern? p) (var-pattern-name p)]
   [(literal-pattern? p) (literal-pattern-value p)]
   [(ctor-pattern? p)
    (let ([tag (ctor-pattern-tag p)]
          [subs (ctor-pattern-subpats p)])
         (if (null? subs)
             tag
             (cons tag (map pattern->sexpr subs))))]
   [(or-pattern? p)
    (cons 'or (map pattern->sexpr (or-pattern-alternatives p)))]
   [(guard-pattern? p)
    (list 'when (pattern->sexpr (guard-pattern-pat p))
          (guard-pattern-guard p))]
   [else '?]))

(define (pattern->string p)
  (doc 'type '(-> Pattern String))
  (doc 'description "Convert pattern to string for display.")
  (doc 'export #t)
  (format "~s" (pattern->sexpr p)))

(doc 'section 'high-level-api)

(define (check-patterns patterns type-name)
  (doc 'type '(-> (List SExpr) Symbol CheckResult))
  (doc 'description "Check a list of patterns against a type.")
  (doc 'export #t)
  (let* ([type-info (lookup-type-info type-name)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [missing (find-missing-patterns parsed (list type-info))]
         [redundant (find-redundant parsed (list type-info))])
        `((exhaustive . ,(null? missing))
          (missing . ,(map pattern->sexpr (flatten-patterns missing)))
          (redundant . ,redundant))))

(define (check-case-expr expr type-name)
  (doc 'type '(-> Expr Symbol CheckResult))
  (doc 'description "Check a case expression against a scrutinee type.")
  (doc 'export #t)
  (let ([type-info (lookup-type-info type-name)])
       (analyze-case expr type-info)))

(doc 'section 'helper-functions)

(define (make-list n x)
  (doc 'type '(-> Nat α (List α)))
  (doc 'description "Create a list of n copies of x.")
  (if (<= n 0)
      '()
      (cons x (make-list (- n 1) x))))

;; take, drop provided by prelude

(define (filter-map f lst)
  (doc 'type '(-> (-> α (+ β #f)) (List α) (List β)))
  (doc 'description "Map function over list, filtering out #f results.")
  (let loop ([items lst] [result '()])
       (if (null? items)
           (reverse result)
           (let ([mapped (f (car items))])
                (if mapped
                    (loop (cdr items) (cons mapped result))
                    (loop (cdr items) result))))))

;; ormap is provided by prelude
