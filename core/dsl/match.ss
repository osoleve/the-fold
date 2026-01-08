;;; core/dsl/match.ss — Pattern Matching Compilation
;;;
;;; Efficient pattern matching for DSLs.
;;;
;;; Compiles pattern matches to decision trees:
;;;   (match expr
;;;     [(Lit n) ...]
;;;     [(Add (Lit 0) x) x]      ; Optimization rules
;;;     [(Add x (Lit 0)) x]
;;;     [(Add (Lit a) (Lit b)) (Lit (+ a b))]
;;;     [(Add x y) (Add x y)])
;;;
;;; Compilation Strategies:
;;;   1. Decision trees (minimize tests)
;;;   2. Backtracking automata
;;;   3. Guards and views
;;;   4. Exhaustiveness checking (via pattern-check.ss)
;;;   5. Redundancy detection (via pattern-check.ss)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types/pattern-check.ss
;;;   - fp/combinators.ss

(load "core/base/prelude.ss")
(load "core/fp/meta/combinators.ss")
(load "core/types/pattern-check.ss")

;;; ============================================================
;;; Pattern AST (Extended)
;;; ============================================================
;;;
;;; Additional pattern forms for DSL use:
;;;   (as name pattern)          - as-pattern, binds whole match
;;;   (view f pattern)           - view pattern, transforms then matches
;;;   (active name args)         - active pattern for extensibility

;;; make-as-pattern : Symbol × Pattern → Pattern
(define (make-as-pattern name inner)
  (make-pattern 'as (cons name inner)))

;;; make-view-pattern : Expr × Pattern → Pattern
(define (make-view-pattern view-fn inner)
  (make-pattern 'view (cons view-fn inner)))

;;; make-active-pattern : Symbol × (List Expr) → Pattern
(define (make-active-pattern name args)
  (make-pattern 'active (cons name args)))

;;; Predicates
(define (as-pattern? p) (eq? (pattern-kind p) 'as))
(define (view-pattern? p) (eq? (pattern-kind p) 'view))
(define (active-pattern? p) (eq? (pattern-kind p) 'active))

;;; Accessors
(define (as-pattern-name p) (car (pattern-data p)))
(define (as-pattern-inner p) (cdr (pattern-data p)))
(define (view-pattern-fn p) (car (pattern-data p)))
(define (view-pattern-inner p) (cdr (pattern-data p)))
(define (active-pattern-name p) (car (pattern-data p)))
(define (active-pattern-args p) (cdr (pattern-data p)))

;;; ============================================================
;;; Decision Tree Representation
;;; ============================================================
;;;
;;; Decision trees represent compiled pattern matches.
;;;
;;; DecisionTree ::= (leaf action bindings)
;;;                | (fail)
;;;                | (switch scrutinee branches default)
;;;                | (guard condition success failure)

;;; make-leaf : Expr × Bindings → DecisionTree
(define (make-leaf action bindings)
  (list 'leaf action bindings))

;;; make-fail : → DecisionTree
(define (make-fail)
  (list 'fail))

;;; make-switch : Expr × (List Branch) × DecisionTree → DecisionTree
(define (make-switch scrutinee branches default)
  (list 'switch scrutinee branches default))

;;; make-dt-guard : Expr × DecisionTree × DecisionTree → DecisionTree
(define (make-dt-guard condition success failure)
  (list 'dt-guard condition success failure))

;;; Predicates
(define (leaf? dt) (and (pair? dt) (eq? (car dt) 'leaf)))
(define (fail? dt) (and (pair? dt) (eq? (car dt) 'fail)))
(define (switch? dt) (and (pair? dt) (eq? (car dt) 'switch)))
(define (dt-guard? dt) (and (pair? dt) (eq? (car dt) 'dt-guard)))

;;; Accessors
(define (leaf-action dt) (cadr dt))
(define (leaf-bindings dt) (caddr dt))
(define (switch-scrutinee dt) (cadr dt))
(define (switch-branches dt) (caddr dt))
(define (switch-default dt) (cadddr dt))
(define (dt-guard-condition dt) (cadr dt))
(define (dt-guard-success dt) (caddr dt))
(define (dt-guard-failure dt) (cadddr dt))

;;; ============================================================
;;; Branch Representation
;;; ============================================================
;;;
;;; Branch ::= (branch tag arity subtree)

;;; make-branch : Symbol × Nat × DecisionTree → Branch
(define (make-branch tag arity subtree)
  (list 'branch tag arity subtree))

;;; branch? : α → Boolean
(define (branch? x)
  (and (pair? x) (eq? (car x) 'branch)))

;;; Accessors
(define (branch-tag b) (cadr b))
(define (branch-arity b) (caddr b))
(define (branch-subtree b) (cadddr b))

;;; ============================================================
;;; Match Clause Representation
;;; ============================================================
;;;
;;; MatchClause ::= (clause pattern guard body)

;;; make-clause : Pattern × (Option Expr) × Expr → MatchClause
(define (make-clause pattern guard body)
  (list 'clause pattern guard body))

;;; clause? : α → Boolean
(define (clause? x)
  (and (pair? x) (eq? (car x) 'clause)))

;;; Accessors
(define (clause-pattern cl) (cadr cl))
(define (clause-guard cl) (caddr cl))
(define (clause-body cl) (cadddr cl))

;;; ============================================================
;;; Pattern Parsing (Extended)
;;; ============================================================

;;; parse-pattern-extended : SExpr → Pattern
;;; Parse pattern with extended forms.
(define (parse-pattern-extended sexpr)
  (cond
   ;; As-pattern: (as name pattern)
   [(and (pair? sexpr)
         (eq? (car sexpr) 'as)
         (>= (length sexpr) 3))
    (make-as-pattern (cadr sexpr)
                     (parse-pattern-extended (caddr sexpr)))]
   
   ;; View pattern: (view fn pattern)
   [(and (pair? sexpr)
         (eq? (car sexpr) 'view)
         (>= (length sexpr) 3))
    (make-view-pattern (cadr sexpr)
                       (parse-pattern-extended (caddr sexpr)))]
   
   ;; Active pattern: (active name args...)
   [(and (pair? sexpr)
         (eq? (car sexpr) 'active))
    (make-active-pattern (cadr sexpr) (cddr sexpr))]
   
   ;; Delegate to base parser
   [else (parse-pattern sexpr)]))

;;; parse-clause : SExpr → MatchClause
;;; Parse a match clause: (pattern body) or (pattern guard body)
(define (parse-clause sexpr)
  (cond
   ;; With guard: (pattern (when guard) body)
   [(and (>= (length sexpr) 3)
         (pair? (cadr sexpr))
         (eq? (caadr sexpr) 'when))
    (make-clause (parse-pattern-extended (car sexpr))
                 (cadadr sexpr)
                 (caddr sexpr))]
   
   ;; Without guard: (pattern body)
   [(>= (length sexpr) 2)
    (make-clause (parse-pattern-extended (car sexpr))
                 #f
                 (cadr sexpr))]
   
   [else
    (error 'parse-clause "invalid clause" sexpr)]))

;;; ============================================================
;;; Pattern Binding Collection
;;; ============================================================

;;; collect-bindings : Pattern × Expr → Bindings
;;; Collect variable bindings from a pattern.
(define (collect-bindings pattern access-expr)
  (cond
   ;; Variable pattern: bind to access expression
   [(var-pattern? pattern)
    (list (cons (var-pattern-name pattern) access-expr))]
   
   ;; Wildcard: no bindings
   [(wildcard-pattern? pattern)
    '()]
   
   ;; Literal: no bindings
   [(literal-pattern? pattern)
    '()]
   
   ;; Constructor: recurse into subpatterns
   [(ctor-pattern? pattern)
    (let loop ([subpats (ctor-pattern-subpats pattern)]
               [idx 0]
               [bindings '()])
         (if (null? subpats)
             (reverse bindings)
             (let* ([subpat (car subpats)]
                    [sub-access `(field ,access-expr ,idx)]
                    [sub-bindings (collect-bindings subpat sub-access)])
                   (loop (cdr subpats)
                         (+ idx 1)
                         (append (reverse sub-bindings) bindings)))))]
   
   ;; As-pattern: bind whole and recurse
   [(as-pattern? pattern)
    (cons (cons (as-pattern-name pattern) access-expr)
          (collect-bindings (as-pattern-inner pattern) access-expr))]
   
   ;; View pattern: transform and recurse
   [(view-pattern? pattern)
    (collect-bindings (view-pattern-inner pattern)
                      `(,(view-pattern-fn pattern) ,access-expr))]
   
   ;; Or-pattern: take first alternative's bindings
   [(or-pattern? pattern)
    (if (null? (or-pattern-alternatives pattern))
        '()
        (collect-bindings (car (or-pattern-alternatives pattern)) access-expr))]
   
   ;; Guard: recurse into inner pattern
   [(guard-pattern? pattern)
    (collect-bindings (guard-pattern-pat pattern) access-expr)]
   
   [else '()]))

;;; ============================================================
;;; Decision Tree Compilation
;;; ============================================================
;;;
;;; Compile clauses to a decision tree using column selection heuristics.

;;; compile-match : Expr × (List MatchClause) → DecisionTree
;;; Compile match expression to decision tree.
(define (compile-match scrutinee clauses)
  (if (null? clauses)
      (make-fail)
      (compile-clauses (list scrutinee)
                       (map (lambda (cl)
                                    ;; Wrap pattern in list for uniform handling
                                    (list (list (clause-pattern cl))
                                          (clause-guard cl)
                                          (clause-body cl)))
                            clauses))))

;;; compile-clauses : (List Expr) × (List (Pattern × Guard × Body)) → DecisionTree
;;; Core compilation algorithm.
(define (compile-clauses access-exprs rows)
  (cond
   ;; No rows: failure
   [(null? rows)
    (make-fail)]
   
   ;; No columns: first row matches (check guard)
   [(null? access-exprs)
    (let* ([first-row (car rows)]
           [guard (cadr first-row)]
           [body (caddr first-row)])
          (if guard
              (make-dt-guard guard
                             (make-leaf body '())
                             (compile-clauses access-exprs (cdr rows)))
              (make-leaf body '())))]
   
   ;; Non-empty: select column and compile
   [else
    (let* ([col-idx (select-column rows)]
           [access-expr (list-ref access-exprs col-idx)]
           [first-col (map (lambda (row) (list-ref (car row) col-idx)) rows)])
          (compile-column access-expr access-exprs col-idx rows first-col))]))

;;; select-column : (List Row) → Nat
;;; Select the best column to match on.
;;; Heuristic: prefer columns with constructors, then non-wildcards.
(define (select-column rows)
  (let* ([first-row (car rows)]
         [patterns (if (list? first-row) (car first-row) first-row)]
         [num-cols (if (list? patterns) (length patterns) 1)])
        (if (= num-cols 1)
            0
            ;; Find first column with a constructor pattern
            (let loop ([idx 0])
                 (if (>= idx num-cols)
                     0  ; Default to first column
                     (let ([col-patterns (map (lambda (row)
                                                      (let ([pats (if (list? row) (car row) row)])
                                                           (if (list? pats)
                                                               (list-ref pats idx)
                                                               pats)))
                                              rows)])
                          (if (any ctor-pattern? col-patterns)
                              idx
                              (loop (+ idx 1)))))))))

;;; compile-column : Expr × (List Expr) × Nat × (List Row) × (List Pattern) → DecisionTree
;;; Compile a specific column.
(define (compile-column access-expr access-exprs col-idx rows col-patterns)
  ;; Collect constructor tags in this column
  (let* ([ctor-tags (collect-ctor-tags col-patterns)]
         [branches (map (lambda (tag)
                                (compile-branch access-expr access-exprs col-idx rows tag))
                        ctor-tags)]
         [default (compile-default access-exprs col-idx rows)])
        (if (null? ctor-tags)
            ;; No constructors: all wildcards, use default
            default
            (make-switch access-expr branches default))))

;;; collect-ctor-tags : (List Pattern) → (List Symbol)
;;; Collect unique constructor tags from patterns.
(define (collect-ctor-tags patterns)
  (let loop ([remaining patterns] [seen '()] [result '()])
       (if (null? remaining)
           (reverse result)
           (let ([pat (car remaining)])
                (if (and (ctor-pattern? pat)
                         (not (memq (ctor-pattern-tag pat) seen)))
                    (let ([tag (ctor-pattern-tag pat)])
                         (loop (cdr remaining) (cons tag seen) (cons tag result)))
                    (loop (cdr remaining) seen result))))))

;;; compile-branch : Expr × (List Expr) × Nat × (List Row) × Symbol → Branch
;;; Compile a branch for a specific constructor.
(define (compile-branch access-expr access-exprs col-idx rows tag)
  (let* ([arity (get-ctor-arity tag)]
         [matching-rows (filter-rows-for-tag rows col-idx tag)]
         [new-access (generate-field-accesses access-expr arity)]
         [new-access-exprs (splice-at access-exprs col-idx new-access)]
         [new-rows (expand-rows-for-tag matching-rows col-idx arity)]
         [subtree (compile-clauses new-access-exprs new-rows)])
        (make-branch tag arity subtree)))

;;; compile-default : (List Expr) × Nat × (List Row) → DecisionTree
;;; Compile the default case (wildcards).
(define (compile-default access-exprs col-idx rows)
  (let* ([wildcard-rows (filter-wildcard-rows rows col-idx)]
         [new-access-exprs (remove-at access-exprs col-idx)]
         [new-rows (remove-col-from-rows wildcard-rows col-idx)])
        (compile-clauses new-access-exprs new-rows)))

;;; ============================================================
;;; Row Manipulation Helpers
;;; ============================================================

;;; filter-rows-for-tag : (List Row) × Nat × Symbol → (List Row)
(define (filter-rows-for-tag rows col-idx tag)
  (filter
   (lambda (row)
           (let* ([patterns (car row)]
                  [pat (list-ref patterns col-idx)])
                 (or (wildcard? pat)
                     (and (ctor-pattern? pat)
                          (eq? (ctor-pattern-tag pat) tag)))))
   rows))

;;; filter-wildcard-rows : (List Row) × Nat → (List Row)
(define (filter-wildcard-rows rows col-idx)
  (filter
   (lambda (row)
           (let* ([patterns (car row)]
                  [pat (list-ref patterns col-idx)])
                 (wildcard? pat)))
   rows))

;;; expand-rows-for-tag : (List Row) × Nat × Nat → (List Row)
(define (expand-rows-for-tag rows col-idx arity)
  (map
   (lambda (row)
           (let* ([patterns (car row)]
                  [pat (list-ref patterns col-idx)]
                  [guard (cadr row)]
                  [body (caddr row)]
                  [subpats (if (ctor-pattern? pat)
                               (ctor-pattern-subpats pat)
                               (make-list arity (make-wildcard-pattern)))]
                  [new-patterns (splice-at patterns col-idx subpats)])
                 (list new-patterns guard body)))
   rows))

;;; remove-col-from-rows : (List Row) × Nat → (List Row)
(define (remove-col-from-rows rows col-idx)
  (map
   (lambda (row)
           (let* ([patterns (car row)]
                  [guard (cadr row)]
                  [body (caddr row)])
                 (list (remove-at patterns col-idx) guard body)))
   rows))

;;; ============================================================
;;; List Manipulation Helpers
;;; ============================================================

;;; list-ref : (List α) × Nat → α
;;; (Built-in, but ensure it exists)

;;; remove-at : (List α) × Nat → (List α)
(define (remove-at lst idx)
  (if (= idx 0)
      (cdr lst)
      (cons (car lst) (remove-at (cdr lst) (- idx 1)))))

;;; splice-at : (List α) × Nat × (List α) → (List α)
;;; Replace element at idx with elements from new-elems.
(define (splice-at lst idx new-elems)
  (if (= idx 0)
      (append new-elems (cdr lst))
      (cons (car lst) (splice-at (cdr lst) (- idx 1) new-elems))))

;;; ============================================================
;;; Constructor Info Helpers
;;; ============================================================

;;; get-ctor-arity : Symbol → Nat
;;; Look up constructor arity from registry.
(define (get-ctor-arity tag)
  (let loop ([types *type-registry*])
       (if (null? types)
           0  ; Default to 0 for unknown constructors
           (let* ([type-info (cdar types)]
                  [ctors (type-info-constructors type-info)]
                  [ctor (find (lambda (c) (eq? (ctor-info-name c) tag)) ctors)])
                 (if ctor
                     (ctor-info-arity ctor)
                     (loop (cdr types)))))))

;;; generate-field-accesses : Expr × Nat → (List Expr)
;;; Generate field access expressions.
(define (generate-field-accesses base arity)
  (let loop ([i 0] [result '()])
       (if (>= i arity)
           (reverse result)
           (loop (+ i 1) (cons `(field ,base ,i) result)))))

;;; ============================================================
;;; Decision Tree to Code
;;; ============================================================

;;; dt-to-code : DecisionTree × Bindings → Expr
;;; Convert decision tree to executable code.
(define (dt-to-code dt env-bindings)
  (cond
   [(leaf? dt)
    (let* ([action (leaf-action dt)]
           [bindings (leaf-bindings dt)]
           [all-bindings (append bindings env-bindings)])
          (if (null? all-bindings)
              action
              `(let ,(map (lambda (b) (list (car b) (cdr b))) all-bindings)
                ,action)))]
   
   [(fail? dt)
    '(error 'match "no matching clause")]
   
   [(switch? dt)
    (let* ([scrut (switch-scrutinee dt)]
           [branches (switch-branches dt)]
           [default (switch-default dt)])
          `(case (tag ,scrut)
            ,@(map (lambda (b)
                           `((,(branch-tag b))
                             ,(dt-to-code (branch-subtree b) env-bindings)))
                   branches)
            (else ,(dt-to-code default env-bindings))))]
   
   [(dt-guard? dt)
    `(if ,(dt-guard-condition dt)
      ,(dt-to-code (dt-guard-success dt) env-bindings)
      ,(dt-to-code (dt-guard-failure dt) env-bindings))]
   
   [else
    (error 'dt-to-code "unknown decision tree form" dt)]))

;;; ============================================================
;;; Match Expression Compilation
;;; ============================================================

;;; compile-match-expr : SExpr → Expr
;;; Compile a match expression from S-expression syntax.
(define (compile-match-expr sexpr)
  (if (and (pair? sexpr) (eq? (car sexpr) 'match))
      (let* ([scrutinee (cadr sexpr)]
             [clause-sexprs (cddr sexpr)]
             [clauses (map parse-clause clause-sexprs)]
             [dt (compile-match scrutinee clauses)])
            (dt-to-code dt '()))
      (error 'compile-match-expr "expected match expression" sexpr)))

;;; ============================================================
;;; First-Class Patterns
;;; ============================================================
;;;
;;; Patterns as values for reuse and composition.

;;; make-first-class-pattern : Symbol × Pattern → FirstClassPattern
(define (make-first-class-pattern name pattern)
  (list 'fcp name pattern))

;;; fcp? : α → Boolean
(define (fcp? x)
  (and (pair? x) (eq? (car x) 'fcp)))

;;; fcp-name : FirstClassPattern → Symbol
(define (fcp-name fcp) (cadr fcp))

;;; fcp-pattern : FirstClassPattern → Pattern
(define (fcp-pattern fcp) (caddr fcp))

;;; Pattern registry for named patterns
(define *pattern-registry* '())

;;; register-pattern! : Symbol × SExpr → Void
(define (register-pattern! name pattern-sexpr)
  (let ([pattern (parse-pattern-extended pattern-sexpr)])
       (set! *pattern-registry*
             (cons (cons name (make-first-class-pattern name pattern))
                   *pattern-registry*))))

;;; lookup-pattern : Symbol → (Option FirstClassPattern)
(define (lookup-pattern name)
  (let ([entry (assq name *pattern-registry*)])
       (if entry (just (cdr entry)) nothing)))

;;; ============================================================
;;; Pattern Combinators
;;; ============================================================

;;; pat-and : Pattern × Pattern → Pattern
;;; Match if both patterns match.
(define (pat-and p1 p2)
  (make-guard-pattern p1 `(matches? scrutinee ,p2)))

;;; pat-not : Pattern → Pattern
;;; Match if pattern does not match.
(define (pat-not p)
  (make-guard-pattern (make-wildcard-pattern)
                      `(not (matches? scrutinee ,p))))

;;; pat-repeat : Pattern × Nat × Nat → Pattern
;;; Match pattern n to m times (for lists).
(define (pat-repeat p min-count max-count)
  (make-active-pattern 'repeat (list p min-count max-count)))

;;; ============================================================
;;; Active Pattern Support
;;; ============================================================
;;;
;;; Active patterns allow custom matching logic.

;;; make-active-pattern-def : Symbol × Procedure → ActivePatternDef
(define (make-active-pattern-def name matcher)
  (list 'active-def name matcher))

;;; Active pattern registry
(define *active-patterns* '())

;;; register-active-pattern! : Symbol × Procedure → Void
(define (register-active-pattern! name matcher)
  (set! *active-patterns*
        (cons (cons name (make-active-pattern-def name matcher))
              *active-patterns*)))

;;; lookup-active-pattern : Symbol → (Option ActivePatternDef)
(define (lookup-active-pattern name)
  (let ([entry (assq name *active-patterns*)])
       (if entry (just (cdr entry)) nothing)))

;;; ============================================================
;;; Built-in Active Patterns
;;; ============================================================

;;; Register some useful active patterns
(register-active-pattern! 'even
                          (lambda (n) (and (number? n) (even? n))))

(register-active-pattern! 'odd
                          (lambda (n) (and (number? n) (odd? n))))

(register-active-pattern! 'positive
                          (lambda (n) (and (number? n) (> n 0))))

(register-active-pattern! 'negative
                          (lambda (n) (and (number? n) (< n 0))))

(register-active-pattern! 'empty
                          (lambda (x) (or (null? x) (and (string? x) (= (string-length x) 0)))))

;;; ============================================================
;;; Match Statistics
;;; ============================================================
;;;
;;; Analyze decision trees for optimization.

;;; dt-size : DecisionTree → Nat
;;; Count nodes in decision tree.
(define (dt-size dt)
  (cond
   [(leaf? dt) 1]
   [(fail? dt) 1]
   [(switch? dt)
    (+ 1 (apply + (map (lambda (b) (dt-size (branch-subtree b)))
                       (switch-branches dt)))
       (dt-size (switch-default dt)))]
   [(dt-guard? dt)
    (+ 1 (dt-size (dt-guard-success dt))
       (dt-size (dt-guard-failure dt)))]
   [else 0]))

;;; dt-depth : DecisionTree → Nat
;;; Maximum depth of decision tree.
(define (dt-depth dt)
  (cond
   [(leaf? dt) 1]
   [(fail? dt) 1]
   [(switch? dt)
    (+ 1 (apply max
                (cons (dt-depth (switch-default dt))
                      (map (lambda (b) (dt-depth (branch-subtree b)))
                           (switch-branches dt)))))]
   [(dt-guard? dt)
    (+ 1 (max (dt-depth (dt-guard-success dt))
              (dt-depth (dt-guard-failure dt))))]
   [else 0]))

;;; ============================================================
;;; Exports
;;; ============================================================

;;; Main entry points:
;;;   - compile-match, compile-match-expr
;;;   - parse-pattern-extended, parse-clause
;;;   - dt-to-code
;;;   - make-leaf, make-fail, make-switch, make-dt-guard
;;;   - register-pattern!, lookup-pattern
;;;   - register-active-pattern!, lookup-active-pattern
;;;   - collect-bindings
;;;   - dt-size, dt-depth
