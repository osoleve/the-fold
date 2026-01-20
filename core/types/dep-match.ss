(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/gadt.ss")

(doc 'module 'dep-match)
(doc 'description "Dependent pattern matching with type refinement. Extends pattern matching to handle dependent types, learning information about indices during matching. Includes coverage checking, inaccessible patterns, and impossible case detection")
(doc 'layer 'core)

(doc 'section 'dep-match-grammar)

(doc 'section 'type-refinement)
;;; When we match on a constructor with specific return type indices,
;;; we learn equations between type variables and specific types.
;;;
;;; Example:
;;;   Matching (cons x xs : Vec (succ n) A) against constructor
;;;   cons : ∀m A. A → Vec m A → Vec (succ m) A
;;;   We learn: n ~ m (from (succ n) ~ (succ m))

;;; Any: (List (Variable . Type))
;;; Maps type variables to their refined types.

(define-record-type type-refinement
  (fields
   substitution    ; (List (Symbol . Type)) - variable refinements
   constraints))   ; (List (Type × Type)) - remaining equality constraints

;;; empty-refinement : → Any
(define (empty-refinement)
  (make-type-refinement '() '()))

;;; refinement-add : Any × Symbol × Type → Any
(define (refinement-add ref var type)
  (make-type-refinement
   (cons (cons var type) (type-refinement-substitution ref))
   (type-refinement-constraints ref)))

;;; refinement-add-constraint : Any × Type × Type → Any
(define (refinement-add-constraint ref t1 t2)
  (make-type-refinement
   (type-refinement-substitution ref)
   (cons (cons t1 t2) (type-refinement-constraints ref))))

;;; apply-refinement : Any × Type → Type
(define (apply-refinement ref type)
  (let ([subst (type-refinement-substitution ref)])
       (fold-left (lambda (t pair)
                          (subst-type t (car pair) (cdr pair)))
                  type
                  subst)))

;;; ====
;;; Pattern Analysis
;;; ====

(define-record-type dep-pattern
  (fields
   kind        ; 'var | 'ctor | 'dot | 'wildcard
   data))      ; Symbol (var), (ctor . (List Any)), Expr (dot), or #f (wildcard)

;;; parse-dep-pattern : SExpr → Any
(define (parse-dep-pattern pat)
  (cond
   ;; Wildcard
   [(eq? pat '_)
    (make-dep-pattern 'wildcard #f)]
   
   ;; Variable (lowercase symbol)
   [(and (symbol? pat) (char-lower-case? (string-ref (symbol->string pat) 0)))
    (make-dep-pattern 'var pat)]
   
   ;; Dot pattern: (. expr)
   [(and (pair? pat) (eq? (car pat) (string->symbol ".")))
    (make-dep-pattern 'dot (cadr pat))]
   
   ;; Constructor pattern: (Ctor pat ...)
   [(pair? pat)
    (let ([ctor (car pat)]
          [subpats (map parse-dep-pattern (cdr pat))])
         (make-dep-pattern 'ctor (cons ctor subpats)))]
   
   ;; Literal/constant
   [else
    (make-dep-pattern 'var pat)]))

;;; dep-pattern-var? : Any → Boolean
(define (dep-pattern-var? p)
  (eq? (dep-pattern-kind p) 'var))

;;; dep-pattern-ctor? : Any → Boolean
(define (dep-pattern-ctor? p)
  (eq? (dep-pattern-kind p) 'ctor))

;;; dep-pattern-dot? : Any → Boolean
(define (dep-pattern-dot? p)
  (eq? (dep-pattern-kind p) 'dot))

;;; dep-pattern-wildcard? : Any → Boolean
(define (dep-pattern-wildcard? p)
  (eq? (dep-pattern-kind p) 'wildcard))

;;; dep-pattern-ctor-name : Any → Symbol | #f
(define (dep-pattern-ctor-name p)
  (if (dep-pattern-ctor? p)
      (car (dep-pattern-data p))
      #f))

;;; dep-pattern-subpatterns : Any → (List Any)
(define (dep-pattern-subpatterns p)
  (if (dep-pattern-ctor? p)
      (cdr (dep-pattern-data p))
      '()))

;;; ====
;;; Pattern Matching with Any
;;; ====

;;; match-pattern-with-refinement : Any × Type × Any → (Result (Any × Any) Error)
;;; Match a pattern against a scrutinee type, producing:
;;;   1. Value bindings (pattern variables to types)
;;;   2. Type refinements (learned from index unification)
(define (match-pattern-with-refinement pattern scrutinee-type data-decl)
  (cond
   ;; Variable pattern: binds the whole value
   [(dep-pattern-var? pattern)
    (let ([var (dep-pattern-data pattern)])
         `(ok ((,var . ,scrutinee-type)) ,(empty-refinement)))]
   
   ;; Wildcard: no bindings, no refinement
   [(dep-pattern-wildcard? pattern)
    `(ok () ,(empty-refinement))]
   
   ;; Dot pattern: must match exactly (for inaccessible indices)
   [(dep-pattern-dot? pattern)
    (let ([expected-expr (dep-pattern-data pattern)])
         ;; In a real implementation, we'd check that scrutinee equals expected
         `(ok () ,(empty-refinement)))]
   
   ;; Constructor pattern: extract bindings and refine types
   [(dep-pattern-ctor? pattern)
    (let* ([ctor-name (dep-pattern-ctor-name pattern)]
           [subpats (dep-pattern-subpatterns pattern)]
           ;; Look up constructor type
           [ctor-type (if data-decl
                          (data-constructor-type data-decl ctor-name)
                          #f)])
          (if (not ctor-type)
              `(error unknown-constructor ,ctor-name)
              ;; Compute refinement from constructor return type
              (let* ([ret-type (get-ctor-return-type ctor-type)]
                     [refinement (compute-refinement scrutinee-type ret-type)]
                     [param-types (get-ctor-param-types ctor-type)])
                    ;; Recursively match subpatterns
                    (match-subpatterns subpats param-types data-decl refinement))))]
   
   [else `(error invalid-pattern ,pattern)]))

;;; match-subpatterns : (List Any) × (List Type) × Any × Any → (Result (Any × Any) Error)
(define (match-subpatterns patterns types data-decl refinement)
  (if (not (= (length patterns) (length types)))
      `(error pattern-arity-mismatch
        (expected ,(length types))
        (got ,(length patterns)))
      (let loop ([pats patterns]
                 [tys types]
                 [bindings '()]
                 [ref refinement])
           (if (null? pats)
               `(ok ,bindings ,ref)
               (let ([result (match-pattern-with-refinement (car pats) (car tys) data-decl)])
                    (if (eq? (car result) 'error)
                        result
                        (let ([new-bindings (cadr result)]
                              [new-ref (caddr result)])
                             (loop (cdr pats)
                                   (cdr tys)
                                   (append new-bindings bindings)
                                   (merge-refinements ref new-ref)))))))))

;;; merge-refinements : Any × Any → Any
(define (merge-refinements r1 r2)
  (make-type-refinement
   (append (type-refinement-substitution r1)
           (type-refinement-substitution r2))
   (append (type-refinement-constraints r1)
           (type-refinement-constraints r2))))

;;; ====
;;; Any Computation
;;; ====

;;; compute-refinement : Type × Type → Any
;;; Compute the type refinement by unifying scrutinee type with constructor return type.
;;; E.g., unifying (Vec n A) with (Vec (succ m) B) gives n ~ (succ m), A ~ B
(define (compute-refinement scrutinee-type ctor-return-type)
  (let ([equations (unify-types scrutinee-type ctor-return-type)])
       (if (eq? equations 'mismatch)
           (empty-refinement)  ; Impossible case
           (make-type-refinement equations '()))))

;;; term-ctor? : Any → Boolean
;;; Is this a known term-level constructor (not a type variable)?
;;; These are used as indices in dependent types.
(define (term-ctor? x)
  (and (symbol? x)
       (memq x '(zero succ nil cons true false refl fzero fsucc))))

;;; unifiable-var? : Any → Boolean
;;; Is this a type variable that can be unified?
;;; Excludes term constructors that happen to start lowercase.
(define (unifiable-var? t)
  (and (type-var? t)
       (not (term-ctor? t))))

;;; unify-types : Type × Type → (List (Symbol . Type)) | 'mismatch
;;; Robinson-style unification with proper substitution propagation.
;;; Returns a most-general unifier (MGU) or 'mismatch if types are incompatible.
(define (unify-types t1 t2)
  (unify-with-subst t1 t2 '()))

;;; unify-with-subst : Type × Type × Subst → Subst | 'mismatch
;;; Unify two types under an existing substitution.
;;; The substitution is applied to both types before comparison.
(define (unify-with-subst t1 t2 subst)
  (let ([t1* (apply-subst-to-type subst t1)]
        [t2* (apply-subst-to-type subst t2)])
       (cond
        ;; Same type after substitution
        [(equal? t1* t2*) subst]
        
        ;; Type variable on left
        [(unifiable-var? t1*)
         (if (occurs-in? t1* t2*)
             'mismatch
             (extend-subst t1* t2* subst))]
        
        ;; Type variable on right
        [(unifiable-var? t2*)
         (if (occurs-in? t2* t1*)
             'mismatch
             (extend-subst t2* t1* subst))]
        
        ;; Both compound, same head and arity
        [(and (pair? t1*) (pair? t2*)
              (eq? (car t1*) (car t2*))
              (= (length t1*) (length t2*)))
         (unify-list (cdr t1*) (cdr t2*) subst)]
        
        [else 'mismatch])))

;;; unify-list : (List Type) × (List Type) × Subst → Subst | 'mismatch
;;; Unify corresponding elements of two lists, threading substitution.
(define (unify-list ts1 ts2 subst)
  (cond
   [(null? ts1) subst]
   [(eq? subst 'mismatch) 'mismatch]
   [else
    (let ([new-subst (unify-with-subst (car ts1) (car ts2) subst)])
         (if (eq? new-subst 'mismatch)
             'mismatch
             (unify-list (cdr ts1) (cdr ts2) new-subst)))]))

;;; apply-subst-to-type : Subst × Type → Type
;;; Apply a substitution to a type, replacing bound variables.
(define (apply-subst-to-type subst type)
  (cond
   [(and (symbol? type) (assq type subst))
    => (lambda (binding) (apply-subst-to-type subst (cdr binding)))]
   [(pair? type)
    (cons (car type)
          (map (lambda (t) (apply-subst-to-type subst t)) (cdr type)))]
   [else type]))

;;; extend-subst : Symbol × Type × Subst → Subst
;;; Add a new binding to the substitution.
;;; The new binding is composed with existing bindings.
(define (extend-subst var type subst)
  (cons (cons var type)
        (map (lambda (binding)
                     (cons (car binding)
                           (apply-subst-to-type (list (cons var type)) (cdr binding))))
             subst)))

;;; occurs-in? : Symbol × Type → Boolean
(define (occurs-in? var type)
  (cond
   [(type-var? type) (eq? var type)]
   [(not (pair? type)) #f]
   [else (ormap (lambda (t) (occurs-in? var t)) (cdr type))]))

;;; ====
;;; Constructor Type Accessors
;;; ====

;;; get-ctor-return-type : Type → Type
;;; Extract the return type from a constructor type.
(define (get-ctor-return-type type)
  (cond
   [(function-type? type)
    (function-return-type type)]
   [(pi-type? type)
    (get-ctor-return-type (pi-codomain type))]
   [(and (pair? type) (eq? (car type) '∀))
    (get-ctor-return-type (caddr type))]
   [else type]))

;;; get-ctor-param-types : Type → (List Type)
;;; Extract parameter types from a constructor type.
(define (get-ctor-param-types type)
  (cond
   [(function-type? type)
    (function-param-types type)]
   [(pi-type? type)
    (cons (pi-domain type) (get-ctor-param-types (pi-codomain type)))]
   [(and (pair? type) (eq? (car type) '∀))
    (get-ctor-param-types (caddr type))]
   [else '()]))

;;; ====
;;; Coverage Checking
;;; ====

;;; coverage-check : (List Any) × Type × Any → (Result 'complete Error)
;;; Check that patterns cover all constructors of the scrutinee type.
(define (coverage-check patterns scrutinee-type data-decl)
  (if (not data-decl)
      `(ok complete)  ; Non-inductive type, assume complete
      (let* ([ctors (data-constructor-names data-decl)]
             [covered (filter-map dep-pattern-ctor-name patterns)]
             [missing (filter (lambda (c) (not (memq c covered))) ctors)])
            (if (null? missing)
                `(ok complete)
                ;; Check if any pattern is exhaustive (wildcard or var covering all)
                (if (ormap (lambda (p)
                                   (or (dep-pattern-var? p)
                                       (dep-pattern-wildcard? p)))
                           patterns)
                    `(ok complete)
                    `(error missing-patterns ,missing))))))

;;; filter-map : (α → β | #f) × (List α) → (List β)
(define (filter-map f lst)
  (if (null? lst)
      '()
      (let ([result (f (car lst))])
           (if result
               (cons result (filter-map f (cdr lst)))
               (filter-map f (cdr lst))))))

;;; ====
;;; Impossible Case Detection
;;; ====

;;; is-impossible-case? : Any × Type × Any → Boolean
;;; Check if a pattern is impossible to match due to type constraints.
;;; E.g., matching (cons x xs) against (Vec 0 A) is impossible.
(define (is-impossible-case? pattern scrutinee-type data-decl)
  (cond
   [(dep-pattern-ctor? pattern)
    (let* ([ctor-name (dep-pattern-ctor-name pattern)]
           [ctor-type (if data-decl
                          (data-constructor-type data-decl ctor-name)
                          #f)])
          (if (not ctor-type)
              #f  ; Unknown constructor, assume possible
              (let* ([ret-type (get-ctor-return-type ctor-type)]
                     [equations (unify-types scrutinee-type ret-type)])
                    (eq? equations 'mismatch))))]
   [else #f]))

;;; ====
;;; Dependent Match Expression
;;; ====

(define-record-type dep-match-expr
  (fields
   scrutinee     ; The expression being matched
   scrutinee-type ; The type of the scrutinee
   clauses))     ; (List (Any × Expr))

;;; parse-dep-match : SExpr → Any
;;; Parse a dependent match expression.
;;; Form: (dep-match (scrutinee : Type) [pattern body] ...)
(define (parse-dep-match expr)
  (if (and (pair? expr) (eq? (car expr) 'dep-match))
      (let* ([scrutinee-with-type (cadr expr)]
             [scrutinee (car scrutinee-with-type)]
             [scrut-type (if (and (pair? (cdr scrutinee-with-type))
                                  (eq? (cadr scrutinee-with-type) ':))
                             (caddr scrutinee-with-type)
                             #f)]
             [clauses (map parse-dep-match-clause (cddr expr))])
            (make-dep-match-expr scrutinee scrut-type clauses))
      (error 'parse-dep-match "not a dep-match expression" expr)))

;;; parse-dep-match-clause : SExpr → (Any × Expr)
(define (parse-dep-match-clause clause)
  (if (and (list? clause) (= (length clause) 2))
      (cons (parse-dep-pattern (car clause)) (cadr clause))
      (error 'parse-dep-match-clause "invalid clause" clause)))

;;; ====
;;; Type Check Dependent Match
;;; ====

;;; check-dep-match : Any × Type × Any × Any → (Result Type Error)
;;; Type check a dependent match expression, returning the result type.
(define (check-dep-match dm expected-type data-decl env)
  (let* ([scrutinee-type (dep-match-expr-scrutinee-type dm)]
         [clauses (dep-match-expr-clauses dm)]
         [patterns (map car clauses)])
        ;; Check coverage
        (let ([cov (coverage-check patterns scrutinee-type data-decl)])
             (if (eq? (car cov) 'error)
                 cov
                 ;; Check each clause
                 (let loop ([cls clauses])
                      (if (null? cls)
                          `(ok ,expected-type)
                          (let* ([pat (caar cls)]
                                 [body (cdar cls)]
                                 ;; Get refinement and bindings
                                 [match-result (match-pattern-with-refinement pat scrutinee-type data-decl)])
                                (if (eq? (car match-result) 'error)
                                    match-result
                                    (let* ([bindings (cadr match-result)]
                                           [refinement (caddr match-result)]
                                           ;; Apply refinement to expected type
                                           [refined-expected (apply-refinement refinement expected-type)]
                                           ;; Extend environment with bindings
                                           [new-env (append bindings env)])
                                          ;; Check body against refined expected type
                                          ;; (In a full implementation, recursively type-check body)
                                          (loop (cdr cls)))))))))))

;;; ====
;;; Examples: Standard Indexed Types
;;; ====

;;; Vec : Nat → * → *
;;; (data (Vec n A)
;;;   [nil  : (Vec 0 A)]
;;;   [cons : (Π ((x : A)) (Π ((m : Nat)) (Π ((xs : (Vec m A))) (Vec (succ m) A))))])

(define Vec-decl
  (t-data 'Vec '((n : Nat) A)
          `((nil . (Vec 0 A))
            (cons . (Π ((x : A)) (Π ((m : Nat)) (Π ((xs : (Vec m A))) (Vec (succ m) A))))))))

;;; Fin : Nat → *
;;; Finite numbers bounded by n
;;; (data (Fin n)
;;;   [fzero : (Π ((m : Nat)) (Fin (succ m)))]
;;;   [fsucc : (Π ((m : Nat)) (Π ((i : (Fin m))) (Fin (succ m))))])

(define Fin-decl
  (t-data 'Fin '((n : Nat))
          `((fzero . (Π ((m : Nat)) (Fin (succ m))))
            (fsucc . (Π ((m : Nat)) (Π ((i : (Fin m))) (Fin (succ m))))))))

;;; ====
;;; Pattern Matching Compilation
;;; ====

;;; Compile dependent pattern matching to a case tree.
;;; This is where the real work happens for efficient matching.

(define-record-type case-tree
  (fields
   kind    ; 'leaf | 'split
   data))  ; Expr (leaf) or (scrut . (List (ctor . Any))) (split)

;;; make-leaf : Expr → Any
(define (make-leaf expr)
  (make-case-tree 'leaf expr))

;;; make-split : Symbol × (List (Symbol × Any)) → Any
(define (make-split scrut branches)
  (make-case-tree 'split (cons scrut branches)))

;;; case-tree-leaf? : Any → Boolean
(define (case-tree-leaf? ct)
  (eq? (case-tree-kind ct) 'leaf))

;;; case-tree-split? : Any → Boolean
(define (case-tree-split? ct)
  (eq? (case-tree-kind ct) 'split))

;;; compile-dep-match : Any → Any
;;; Compile dependent match to efficient case tree.
;;; Groups clauses by constructor and builds a decision tree.
(define (compile-dep-match dm)
  (let ([clauses (dep-match-expr-clauses dm)]
        [scrutinee (dep-match-expr-scrutinee dm)])
       (compile-clauses scrutinee clauses)))

;;; compile-clauses : Expr × (List (Any × Expr)) → Any
;;; Compile a list of clauses into a case tree.
(define (compile-clauses scrutinee clauses)
  (cond
   [(null? clauses)
    (make-leaf '(error no-matching-pattern))]
   
   ;; Check if first clause is a catch-all (var or wildcard)
   [(let ([pat (car (car clauses))])
         (or (dep-pattern-var? pat) (dep-pattern-wildcard? pat)))
    ;; Catch-all pattern - just return its body
    (make-leaf (cdr (car clauses)))]
   
   ;; First clause is a constructor pattern - group by constructor
   [(dep-pattern-ctor? (car (car clauses)))
    (let* ([grouped (group-clauses-by-ctor clauses)]
           [ctor-cases (map (lambda (group)
                                    (let ([ctor-name (car group)]
                                          [ctor-clauses (cdr group)])
                                         (cons ctor-name
                                               (compile-ctor-clauses ctor-clauses))))
                            grouped)]
           ;; Check if there's a default case (var/wildcard after constructors)
           [default-clauses (filter-default-clauses clauses)]
           [default-tree (if (null? default-clauses)
                             #f
                             (compile-clauses scrutinee default-clauses))])
          (make-split scrutinee (if default-tree
                                    (append ctor-cases (list (cons '_ default-tree)))
                                    ctor-cases)))]
   
   ;; Dot pattern - treat as specific value match
   [(dep-pattern-dot? (car (car clauses)))
    ;; For dot patterns, we need runtime equality check
    (let ([expected (dep-pattern-data (car (car clauses)))]
          [body (cdr (car clauses))]
          [rest (cdr clauses)])
         (make-split scrutinee
                     (list (cons expected (make-leaf body))
                           (cons '_ (compile-clauses scrutinee rest)))))]
   
   [else
    (make-leaf '(error invalid-pattern))]))

;;; group-clauses-by-ctor : (List (Any × Expr)) → (List (Symbol . (List (Any × Expr))))
;;; Group constructor clauses by their constructor name.
(define (group-clauses-by-ctor clauses)
  (let loop ([cls clauses] [groups '()])
       (if (null? cls)
           (reverse (map (lambda (g) (cons (car g) (reverse (cdr g)))) groups))
           (let* ([clause (car cls)]
                  [pat (car clause)])
                 (if (dep-pattern-ctor? pat)
                     (let* ([ctor-name (dep-pattern-ctor-name pat)]
                            [existing (assq ctor-name groups)])
                           (if existing
                               (loop (cdr cls)
                                     (map (lambda (g)
                                                  (if (eq? (car g) ctor-name)
                                                      (cons (car g) (cons clause (cdr g)))
                                                      g))
                                          groups))
                               (loop (cdr cls)
                                     (cons (list ctor-name clause) groups))))
                     ;; Non-constructor pattern, skip for grouping
                     (loop (cdr cls) groups))))))

;;; filter-default-clauses : (List (Any × Expr)) → (List (Any × Expr))
;;; Extract clauses with var or wildcard patterns (default cases).
(define (filter-default-clauses clauses)
  (filter (lambda (clause)
                  (let ([pat (car clause)])
                       (or (dep-pattern-var? pat) (dep-pattern-wildcard? pat))))
          clauses))

;;; compile-ctor-clauses : (List (Any × Expr)) → Any
;;; Compile clauses for a specific constructor.
;;; All clauses have the same top-level constructor.
(define (compile-ctor-clauses clauses)
  (if (= (length clauses) 1)
      ;; Single clause - just return its body
      (make-leaf (cdr (car clauses)))
      ;; Multiple clauses with same constructor - would need to match subpatterns
      ;; For now, use first match (could extend to recurse on subpatterns)
      (make-leaf (cdr (car clauses)))))

;;; ====
;;; With-Abstraction (Future Extension)
;;; ====

;;; With-abstraction allows us to match on intermediate computations
;;; while maintaining type refinement.
;;;
;;; (with (f x) as result [pattern body] ...)
;;;
;;; This is useful when we need to inspect a computed value and use
;;; the type information from the pattern in the body.

;;; ====
;;; Pretty Printing
;;; ====

;;; dep-pattern->string : Any → String
(define (dep-pattern->string pat)
  (cond
   [(dep-pattern-var? pat)
    (symbol->string (dep-pattern-data pat))]
   [(dep-pattern-wildcard? pat)
    "_"]
   [(dep-pattern-dot? pat)
    (format "(. ~s)" (dep-pattern-data pat))]
   [(dep-pattern-ctor? pat)
    (let ([name (dep-pattern-ctor-name pat)]
          [subpats (dep-pattern-subpatterns pat)])
         (if (null? subpats)
             (symbol->string name)
             (string-append "("
                            (symbol->string name)
                            " "
                            (join-strings " " (map dep-pattern->string subpats))
                            ")")))]
   [else "?"]))

;;; refinement->string : Any → String
(define (refinement->string ref)
  (let ([subst (type-refinement-substitution ref)])
       (if (null? subst)
           "(no refinements)"
           (string-append "{"
                          (join-strings ", "
                                        (map (lambda (p)
                                                     (format "~a ~~ ~s"
                                                             (car p) (cdr p)))
                                             subst))
                          "}"))))
