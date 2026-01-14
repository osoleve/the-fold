;;; core/types/pattern-check.ss — Pattern Match Exhaustiveness & Redundancy
;;;
;;; Analyzes pattern matches for:
;;;   1. Exhaustiveness - are all cases covered?
;;;   2. Redundancy - are any patterns unreachable?
;;;   3. Usefulness - does each pattern match something new?
;;;
;;; Based on "Warnings for Pattern Matching" (Maranget, JFP 2007)
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")

;;; ====
;;; Pattern Representation
;;; ====

;;; Patterns are represented as:
;;;   (ctor tag subpatterns...)  - constructor pattern
;;;   (wildcard)                 - matches anything
;;;   (var name)                 - variable pattern (like wildcard)
;;;   (literal value)            - literal value
;;;   (or pat1 pat2 ...)         - or-pattern (any of)
;;;   (guard pat expr)           - guarded pattern

(define-record-type pattern
  (fields
   kind    ; 'ctor | 'wildcard | 'var | 'literal | 'or | 'guard
   data))  ; depends on kind

;;; Constructors
(define (make-ctor-pattern tag subpats)
  (make-pattern 'ctor (cons tag subpats)))

(define (make-wildcard-pattern)
  (make-pattern 'wildcard #f))

(define (make-var-pattern name)
  (make-pattern 'var name))

(define (make-literal-pattern value)
  (make-pattern 'literal value))

(define (make-or-pattern pats)
  (make-pattern 'or pats))

(define (make-guard-pattern pat guard)
  (make-pattern 'guard (cons pat guard)))

;;; Predicates
(define (ctor-pattern? p) (eq? (pattern-kind p) 'ctor))
(define (wildcard-pattern? p) (eq? (pattern-kind p) 'wildcard))
(define (var-pattern? p) (eq? (pattern-kind p) 'var))
(define (literal-pattern? p) (eq? (pattern-kind p) 'literal))
(define (or-pattern? p) (eq? (pattern-kind p) 'or))
(define (guard-pattern? p) (eq? (pattern-kind p) 'guard))

;;; Accessors
(define (ctor-pattern-tag p)
  (let ([data (pattern-data p)])
       (if (pair? data)
           (car data)
           data)))  ;; Handle case where data is just the tag
(define (ctor-pattern-subpats p)
  (let ([data (pattern-data p)])
       (if (pair? data)
           (cdr data)
           '())))
(define (var-pattern-name p) (pattern-data p))
(define (literal-pattern-value p) (pattern-data p))
(define (or-pattern-alternatives p) (pattern-data p))
(define (guard-pattern-pat p) (car (pattern-data p)))
(define (guard-pattern-guard p) (cdr (pattern-data p)))

;;; wildcard? : Pattern → Boolean
;;; Is this pattern equivalent to a wildcard (matches everything)?
(define (wildcard? p)
  (or (wildcard-pattern? p)
      (var-pattern? p)))

;;; ====
;;; Type Information
;;; ====

;;; Type constructors are defined as:
;;;   (type-info name arity)
;;; where arity is the number of fields

(define-record-type type-info
  (fields
   name            ; Symbol - type name
   constructors))  ; (List (ctor-name × arity))

(define-record-type ctor-info
  (fields
   name    ; Symbol
   arity)) ; Nat

;;; Built-in type information
(define bool-type-info
  (make-type-info 'Bool
                  (list (make-ctor-info 'true 0)
                        (make-ctor-info 'false 0))))

(define list-type-info
  (make-type-info 'List
                  (list (make-ctor-info 'nil 0)
                        (make-ctor-info 'cons 2))))

(define maybe-type-info
  (make-type-info 'Maybe
                  (list (make-ctor-info 'nothing 0)
                        (make-ctor-info 'just 1))))

(define either-type-info
  (make-type-info 'Either
                  (list (make-ctor-info 'left 1)
                        (make-ctor-info 'right 1))))

(define nat-type-info
  (make-type-info 'Nat
                  (list (make-ctor-info 'zero 0)
                        (make-ctor-info 'succ 1))))

(define pair-type-info
  (make-type-info 'Pair
                  (list (make-ctor-info 'pair 2))))

(define unit-type-info
  (make-type-info 'Unit
                  (list (make-ctor-info 'unit 0))))

;;; Standard type registry
(define *type-registry*
  (list (cons 'Bool bool-type-info)
        (cons 'List list-type-info)
        (cons 'Maybe maybe-type-info)
        (cons 'Either either-type-info)
        (cons 'Nat nat-type-info)
        (cons 'Pair pair-type-info)
        (cons 'Unit unit-type-info)))

;;; lookup-type-info : Symbol → TypeInfo | #f
(define (lookup-type-info type-name)
  (let ([entry (assq type-name *type-registry*)])
       (if entry (cdr entry) #f)))

;;; register-type! : Symbol × (List (Symbol × Nat)) → Void
(define (register-type! name ctors)
  (set! *type-registry*
        (cons (cons name
                    (make-type-info name
                                    (map (lambda (c)
                                                 (make-ctor-info (car c) (cdr c)))
                                         ctors)))
              *type-registry*)))

;;; type-constructors : TypeInfo → (List CtorInfo)
(define (type-constructors ti)
  (type-info-constructors ti))

;;; known-ctor? : Symbol → Boolean
;;; Check if a symbol is a registered constructor name.
(define (known-ctor? name)
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

;;; ====
;;; Pattern Parsing
;;; ====

;;; parse-pattern : SExpr → Pattern
;;; Convert S-expression pattern to Pattern record.
(define (parse-pattern sexpr)
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

;;; ====
;;; Pattern Matrix
;;; ====

;;; A pattern matrix is a list of pattern rows.
;;; Each row is a list of patterns (one per column).

;;; specialize : PatternMatrix × Symbol × Nat → PatternMatrix
;;; Specialize matrix for constructor with given arity.
;;; - Constructor patterns matching tag: replace with subpatterns
;;; - Wildcards: replace with arity wildcards
;;; - Other constructors: remove row
(define (specialize matrix tag arity)
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

;;; default-matrix : PatternMatrix → PatternMatrix
;;; Get the default matrix (rows that match wildcards).
(define (default-matrix matrix)
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

;;; ====
;;; Exhaustiveness Checking
;;; ====

;;; exhaustive? : PatternMatrix × (List TypeInfo) → Boolean
;;; Check if pattern matrix is exhaustive for given types.
(define (exhaustive? matrix types)
  (null? (find-missing-patterns matrix types)))

;;; find-missing-patterns : PatternMatrix × (List TypeInfo) → (List Pattern)
;;; Find patterns not covered by the matrix.
(define (find-missing-patterns matrix types)
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
                                                        (let ([subpats (take m arity)]
                                                              [rest (drop m arity)])
                                                             (cons (make-ctor-pattern tag subpats)
                                                                   rest)))
                                                ctor-missing))))))))]))

;;; ====
;;; Redundancy Checking
;;; ====

;;; useful? : Pattern × PatternMatrix × (List TypeInfo) → Boolean
;;; Is this pattern useful (matches something the matrix doesn't)?
(define (useful? row matrix types)
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

;;; find-redundant : PatternMatrix × (List TypeInfo) → (List Nat)
;;; Find indices of redundant rows in the matrix.
(define (find-redundant matrix types)
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

;;; ====
;;; Case Expression Analysis
;;; ====

;;; analyze-case : Expr × TypeInfo → CaseAnalysis
;;; Analyze a case expression for exhaustiveness and redundancy.
(define (analyze-case expr type-info)
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

;;; flatten-patterns : (List (List Pattern)) → (List Pattern)
(define (flatten-patterns pss)
  (map (lambda (ps) (if (= (length ps) 1) (car ps) ps)) pss))

;;; ====
;;; Pattern Pretty Printing
;;; ====

;;; pattern->sexpr : Pattern → SExpr
(define (pattern->sexpr p)
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

;;; pattern->string : Pattern → String
(define (pattern->string p)
  (format "~s" (pattern->sexpr p)))

;;; ====
;;; High-Level API
;;; ====

;;; check-patterns : (List SExpr) × Symbol → CheckResult
;;; Check a list of patterns against a type.
(define (check-patterns patterns type-name)
  (let* ([type-info (lookup-type-info type-name)]
         [parsed (map (lambda (p) (list (parse-pattern p))) patterns)]
         [missing (find-missing-patterns parsed (list type-info))]
         [redundant (find-redundant parsed (list type-info))])
        `((exhaustive . ,(null? missing))
          (missing . ,(map pattern->sexpr (flatten-patterns missing)))
          (redundant . ,redundant))))

;;; check-case-expr : Expr × Symbol → CheckResult
;;; Check a case expression against a scrutinee type.
(define (check-case-expr expr type-name)
  (let ([type-info (lookup-type-info type-name)])
       (analyze-case expr type-info)))

;;; ====
;;; Helper Functions
;;; ====

;;; make-list : Nat × α → (List α)
(define (make-list n x)
  (if (<= n 0)
      '()
      (cons x (make-list (- n 1) x))))

;;; take : (List α) × Nat → (List α)
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;;; drop : (List α) × Nat → (List α)
(define (drop lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))

;;; filter-map : (α → β | #f) × (List α) → (List β)
(define (filter-map f lst)
  (let loop ([items lst] [result '()])
       (if (null? items)
           (reverse result)
           (let ([mapped (f (car items))])
                (if mapped
                    (loop (cdr items) (cons mapped result))
                    (loop (cdr items) result))))))

;;; ormap : (α → Boolean) × (List α) → Boolean
(define (ormap pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (ormap pred (cdr lst))]))
