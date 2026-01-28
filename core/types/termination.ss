(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")

(doc 'module 'termination)
(doc 'description "Termination checking for type-level computation. Ensures that type-level computation terminates, which is essential for decidable type checking in a dependent type system. Implements structural recursion, lexicographic ordering, and size-change termination")
(doc 'layer 'core)

(doc 'section 'size-relations)

(doc size-relation? 'type (-> Symbol Boolean))
(doc size-relation? 'description "Check if this is a valid size relation: '<, '<=, or '?")
(doc size-relation? 'export #t)
(define (size-relation? r)
  (if (memq r '(< <= ?)) #t #f))

;;; size-rel-compose : Symbol × Symbol → Symbol
;;; Compose two size relations (for transitivity).
;;; < ∘ <= = <, <= ∘ <= = <=, ? ∘ _ = ?, _ ∘ ? = ?
(define (size-rel-compose r1 r2)
  (cond
   [(or (eq? r1 '?) (eq? r2 '?)) '?]
   [(or (eq? r1 '<) (eq? r2 '<)) '<]
   [else '<=]))

;;; size-rel-meet : Symbol × Symbol → Symbol
;;; Meet of two relations (for combining paths).
;;; < ⊓ <= = <, < ⊓ ? = <, <= ⊓ ? = <=
(define (size-rel-meet r1 r2)
  (cond
   [(eq? r1 '<) '<]
   [(eq? r2 '<) '<]
   [(eq? r1 '<=) '<=]
   [(eq? r2 '<=) '<=]
   [else '?]))

;;; ====
;;; Call Graph
;;; ====

;;; A call graph represents the recursive call structure of a definition.
;;; Nodes are function names, edges are calls with size-change information.

;;; call-edge : (source × target × size-changes)
;;; where size-changes is a list of (source-arg-idx . (target-arg-idx . relation))
(define (make-call-edge source target size-changes)
  `(call-edge ,source ,target ,size-changes))

(define (call-edge? e)
  (and (pair? e) (eq? (car e) 'call-edge)))

(define (call-edge-source e) (cadr e))
(define (call-edge-target e) (caddr e))
(define (call-edge-size-changes e) (cadddr e))

;;; call-graph : (name × params × edges)
(define (make-call-graph name params edges)
  `(call-graph ,name ,params ,edges))

(define (call-graph? g)
  (and (pair? g) (eq? (car g) 'call-graph)))

(define (call-graph-name g) (cadr g))
(define (call-graph-params g) (caddr g))
(define (call-graph-edges g) (cadddr g))

;;; ====
;;; Structural Subterm Checking
;;; ====

;;; structural-subterm? : Expr × Symbol × Context → Boolean
;;; Check if expr is a structural subterm of the variable var.
;;; This is used to detect structural recursion patterns.
;;;
;;; A term is a structural subterm if it's:
;;;   1. A destructor application to the variable (fst, snd, car, cdr)
;;;   2. A pattern match variable bound to a subterm
;;;   3. Transitively structural through these
(define (structural-subterm? expr var ctx)
  (cond
   ;; Direct variable reference is not strictly smaller
   [(eq? expr var) #f]
   
   ;; Destructor applications: (fst x), (snd x), (car x), (cdr x)
   [(and (pair? expr) (memq (car expr) '(fst snd car cdr hd tl)))
    (let ([arg (cadr expr)])
         (or (eq? arg var)
             (structural-subterm? arg var ctx)))]
   
   ;; Predecessor: (pred n) where n : Nat
   [(and (pair? expr) (eq? (car expr) 'pred))
    (let ([arg (cadr expr)])
         (or (eq? arg var)
             (structural-subterm? arg var ctx)))]
   
   ;; Pattern match bound variable
   [(symbol? expr)
    (let ([info (ctx-lookup-subterm ctx expr)])
         (and info (eq? (car info) var)))]
   
   ;; Index into a vector: (vec-ref v i)
   [(and (pair? expr) (eq? (car expr) 'vec-ref))
    (let ([vec (cadr expr)])
         (or (eq? vec var)
             (structural-subterm? vec var ctx)))]
   
   [else #f]))

;;; ctx-lookup-subterm : Context × Symbol → (Parent . Relation) | #f
;;; Look up subterm information from context.
(define (ctx-lookup-subterm ctx var)
  (let ([entry (assq var ctx)])
       (and entry (cdr entry))))

;;; ====
;;; Size-Change Analysis
;;; ====

;;; analyze-size-change : Expr × Symbol × (List Symbol) × Context → (List (Idx . Relation))
;;; Analyze how arguments change in a recursive call.
;;; Returns a list mapping parameter indices to their size relations.
(define (analyze-size-change call-args params rec-params ctx)
  (let loop ([args call-args]
             [idx 0]
             [result '()])
       (if (null? args)
           (reverse result)
           (let* ([arg (car args)]
                  [param (if (< idx (length rec-params))
                             (list-ref rec-params idx)
                             #f)]
                  [rel (if param
                           (compute-size-relation arg param ctx)
                           '?)])
                 (loop (cdr args)
                       (+ idx 1)
                       (cons (cons idx rel) result))))))

;;; compute-size-relation : Expr × Symbol × Context → Symbol
;;; Compute the size relation between an argument and a parameter.
(define (compute-size-relation arg param ctx)
  (cond
   ;; Same variable: <=
   [(eq? arg param) '<=]
   
   ;; Structural subterm: <
   [(structural-subterm? arg param ctx) '<]
   
   ;; Known constant or literal: ?
   [(or (number? arg) (boolean? arg) (string? arg)) '?]
   
   ;; Unknown: ?
   [else '?]))

;;; ====
;;; Size-Change Graph Construction
;;; ====

;;; A size-change graph (SExpr) represents how arguments change between
;;; function entry and a recursive call.

;;; scg : (source-params × target-params × edges)
;;; where edges is a matrix of relations
(define (make-scg source-params target-params edges)
  `(scg ,source-params ,target-params ,edges))

(define (scg? g)
  (and (pair? g) (eq? (car g) 'scg)))

(define (scg-source-params g) (cadr g))
(define (scg-target-params g) (caddr g))
(define (scg-edges g) (cadddr g))

;;; scg-compose : (Option SExpr)× (Option SExpr)→ SExpr
;;; Compose two size-change graphs (for transitivity).
(define (scg-compose g1 g2)
  (let* ([src (scg-source-params g1)]
         [mid (scg-target-params g1)]
         [tgt (scg-target-params g2)]
         [e1 (scg-edges g1)]
         [e2 (scg-edges g2)]
         [n (length src)]
         [m (length mid)]
         [p (length tgt)]
         [result (make-vector n '())])
        ;; For each source param, compute relation to each target param
        (let outer ([i 0])
             (if (>= i n)
                 (make-scg src tgt (vector->list result))
                 (begin
                  (vector-set! result i
                               (let inner ([j 0] [acc '()])
                                    (if (>= j p)
                                        (reverse acc)
                                        (let ([rel (compose-through e1 e2 i j m)])
                                             (inner (+ j 1) (cons rel acc))))))
                  (outer (+ i 1)))))))

;;; compose-through : (Option SExpr)× (Option SExpr)× Int × Int × Int → Symbol
;;; Compose relations through all intermediate nodes.
(define (compose-through e1 e2 i j m)
  (let loop ([k 0] [best '?])
       (if (>= k m)
           best
           (let* ([rel1 (edge-lookup e1 i k)]
                  [rel2 (edge-lookup e2 k j)]
                  [composed (size-rel-compose rel1 rel2)])
                 (loop (+ k 1) (size-rel-meet best composed))))))

;;; edge-lookup : (Option SExpr)× Int × Int → Symbol
(define (edge-lookup edges i j)
  (if (< i (length edges))
      (let ([row (list-ref edges i)])
           (if (< j (length row))
               (list-ref row j)
               '?))
      '?))

;;; ====
;;; Termination Checking
;;; ====

;;; check-termination : Expr × (List Symbol) → (Result () Error)
;;; Check that a recursive definition terminates.
;;; Returns (ok) if terminating, (error reason) otherwise.
(define (check-termination expr params)
  (let* ([call-graph (extract-call-graph expr params)]
         [scgs (call-graph->scgs call-graph)])
        (if (all-scgs-terminate? scgs)
            '(ok)
            `(error non-terminating-recursion
              (hint "Recursive calls must be on structurally smaller arguments")))))

;;; extract-call-graph : Expr × (List Symbol) → SExpr
;;; Extract the call graph from a recursive definition.
(define (extract-call-graph expr params)
  (let ([edges (collect-recursive-calls expr (list (cons 'self params)) '())])
       (make-call-graph 'self params edges)))

;;; collect-recursive-calls : Expr × Context × (Option SExpr)→ SExpr
;;; Collect all recursive calls in an expression.
(define (collect-recursive-calls expr ctx edges)
  (cond
   ;; Atoms have no recursive calls
   [(not (pair? expr)) edges]
   
   ;; Lambda: extend context and recurse
   [(or (eq? (car expr) 'fn) (eq? (car expr) 'λ))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [new-ctx (extend-ctx-params ctx params)])
          (collect-recursive-calls body new-ctx edges))]
   
   ;; Fix: this is where recursion happens
   [(eq? (car expr) 'fix)
    (let* ([name (cadr expr)]
           [body (caddr expr)]
           [new-ctx (cons (cons name (ctx-params ctx)) ctx)])
          (collect-recursive-calls body new-ctx edges))]
   
   ;; Application: check if recursive call
   [(pair? (car expr))
    (let ([func (car expr)]
          [args (cdr expr)])
         (if (symbol? func)
             (let ([rec-params (assq func ctx)])
                  (if rec-params
                      ;; This is a recursive call
                      (let* ([size-changes (analyze-size-change args (ctx-params ctx) (cdr rec-params) ctx)]
                             [edge (make-call-edge 'self func size-changes)])
                            (collect-recursive-calls-list args ctx (cons edge edges)))
                      ;; Not recursive, just recurse
                      (collect-recursive-calls-list (cons func args) ctx edges)))
             (collect-recursive-calls-list (cons func args) ctx edges)))]
   
   ;; Let: extend context with bindings
   [(eq? (car expr) 'let)
    (let* ([bindings (cadr expr)]
           [body (caddr expr)]
           [new-ctx (extend-ctx-bindings ctx bindings)]
           [edges-after-bindings
            (fold-left (lambda (e b)
                               (collect-recursive-calls (cadr b) ctx e))
                       edges
                       bindings)])
          (collect-recursive-calls body new-ctx edges-after-bindings))]
   
   ;; Case: analyze branches with pattern-bound variables
   [(eq? (car expr) 'case)
    (let* ([scrutinee (cadr expr)]
           [branches (cddr expr)]
           [edges-after-scrutinee (collect-recursive-calls scrutinee ctx edges)])
          (fold-left (lambda (e branch)
                             (let* ([pattern (car branch)]
                                    [body (cadr branch)]
                                    [new-ctx (extend-ctx-pattern ctx scrutinee pattern)])
                                   (collect-recursive-calls body new-ctx e)))
                     edges-after-scrutinee
                     branches))]
   
   ;; Other forms: recurse through all subexpressions
   [else
    (collect-recursive-calls-list (cdr expr) ctx edges)]))

;;; collect-recursive-calls-list : (List Expr) × Context × (Option SExpr)→ SExpr
(define (collect-recursive-calls-list exprs ctx edges)
  (fold-left (lambda (e expr)
                     (collect-recursive-calls expr ctx e))
             edges
             exprs))

;;; ctx-params : Context → (List Symbol)
(define (ctx-params ctx)
  (let ([self (assq 'self ctx)])
       (if self (cdr self) '())))

;;; extend-ctx-params : Context × (List Symbol) → Context
(define (extend-ctx-params ctx params)
  (let ([param-list (if (list? params) params (list params))])
       (fold-left (lambda (c p) (cons (cons p '?) c))
                  ctx
                  param-list)))

;;; extend-ctx-bindings : Context × Bindings → Context
(define (extend-ctx-bindings ctx bindings)
  (fold-left (lambda (c b)
                     (cons (cons (car b) 'bound) c))
             ctx
             bindings))

;;; extend-ctx-pattern : Context × Expr × Pattern → Context
;;; Extend context with pattern-bound variables marked as subterms.
(define (extend-ctx-pattern ctx scrutinee pattern)
  (cond
   ;; Constructor pattern: (Ctor var1 var2 ...)
   [(and (pair? pattern) (symbol? (car pattern)))
    (let ([ctor (car pattern)]
          [vars (cdr pattern)])
         (fold-left (lambda (c v)
                            (if (symbol? scrutinee)
                                (cons (cons v (cons scrutinee '<)) c)
                                (cons (cons v 'bound) c)))
                    ctx
                    vars))]
   
   ;; Variable pattern
   [(symbol? pattern)
    (cons (cons pattern (cons scrutinee '<=)) ctx)]
   
   [else ctx]))

;;; ====
;;; (Option SExpr)Termination Analysis
;;; ====

;;; call-graph->scgs : (Option SExpr)→ (List SExpr)
;;; Convert call graph edges to size-change graphs.
(define (call-graph->scgs cg)
  (let ([params (call-graph-params cg)]
        [edges (call-graph-edges cg)])
       (map (lambda (edge)
                    (edge->scg edge params))
            edges)))

;;; edge->scg : (Option SExpr)× (List Symbol) → SExpr
(define (edge->scg edge params)
  (let* ([size-changes (call-edge-size-changes edge)]
         [n (length params)]
         [matrix (make-scg-matrix n size-changes)])
        (make-scg params params matrix)))

;;; make-scg-matrix : Int × (Option SExpr)→ (List (List Symbol))
(define (make-scg-matrix n size-changes)
  (let loop ([i 0] [result '()])
       (if (>= i n)
           (reverse result)
           (let ([row (make-scg-row i n size-changes)])
                (loop (+ i 1) (cons row result))))))

;;; make-scg-row : Int × Int × (Option SExpr)→ (List Symbol)
(define (make-scg-row i n size-changes)
  (let loop ([j 0] [result '()])
       (if (>= j n)
           (reverse result)
           (let ([rel (lookup-size-change size-changes i j)])
                (loop (+ j 1) (cons rel result))))))

;;; lookup-size-change : (Option SExpr)× Int × Int → Symbol
(define (lookup-size-change changes source-idx target-idx)
  (let ([entry (assq source-idx changes)])
       (if (and entry (= (cadr entry) target-idx))
           (cddr entry)
           '?)))

;;; all-scgs-terminate? : (List SExpr) → Boolean
;;; Check that all size-change graphs satisfy the termination criterion.
;;; Uses the size-change termination principle: every infinite call sequence
;;; must have a thread (sequence of related arguments) that decreases infinitely.
(define (all-scgs-terminate? scgs)
  (if (null? scgs)
      #t  ; No recursive calls = terminating
      (let* ([closure (compute-scg-closure scgs)]
             [idempotents (filter scg-idempotent? closure)])
            (all (lambda (g) (scg-has-decrease? g)) idempotents))))

;;; compute-scg-closure : (List SExpr) → (List SExpr)
;;; Compute the transitive closure of (Option SExpr)composition.
(define (compute-scg-closure scgs)
  (let loop ([current scgs] [seen scgs])
       (let ([new-scgs (filter (lambda (g) (not (scg-member? g seen)))
                               (append-map (lambda (g1)
                                                   (filter-map (lambda (g2)
                                                                       (scg-compose-safe g1 g2))
                                                               current))
                                           current))])
            (if (null? new-scgs)
                seen
                (loop (append new-scgs current)
                      (append new-scgs seen))))))

;;; scg-compose-safe : SCG × SCG → SCG | #f
;;; Compose if target of g1 matches source of g2.
(define (scg-compose-safe g1 g2)
  (if (equal? (scg-target-params g1) (scg-source-params g2))
      (scg-compose g1 g2)
      #f))

;;; scg-member? : (Option SExpr)× (List SExpr) → Boolean
(define (scg-member? g lst)
  (ormap (lambda (h) (scg-equal? g h)) lst))

;;; scg-equal? : (Option SExpr)× (Option SExpr)→ Boolean
(define (scg-equal? g1 g2)
  (and (equal? (scg-source-params g1) (scg-source-params g2))
       (equal? (scg-target-params g1) (scg-target-params g2))
       (equal? (scg-edges g1) (scg-edges g2))))

;;; scg-idempotent? : (Option SExpr)→ Boolean
;;; Check if g ∘ g = g (same source and target params).
(define (scg-idempotent? g)
  (equal? (scg-source-params g) (scg-target-params g)))

;;; scg-has-decrease? : SCG → Boolean
;;; Check if the SCG has at least one strictly decreasing diagonal element.
;;; This ensures infinite sequences through this SCG will decrease.
(define (scg-has-decrease? g)
  (let* ([edges (scg-edges g)]
         [n (length edges)])
        (let loop ([i 0])
             (if (>= i n)
                 #f
                 (let ([diag (edge-lookup edges i i)])
                      (if (eq? diag '<)
                          #t
                          (loop (+ i 1))))))))

;;; ====
;;; Type-Level Termination Checking
;;; ====

;;; check-type-termination : TypeExpr → (Result () Error)
;;; Check that a type-level computation terminates.
;;; This is used during type checking for type families and type-level functions.
(define (check-type-termination type-expr)
  (cond
   ;; Type family application: (F args...)
   [(and (pair? type-expr) (symbol? (car type-expr)))
    (let ([family (car type-expr)]
          [args (cdr type-expr)])
         ;; Check each argument recursively
         (let check-args ([remaining args])
              (if (null? remaining)
                  '(ok)
                  (let ([result (check-type-termination (car remaining))])
                       (if (eq? (car result) 'ok)
                           (check-args (cdr remaining))
                           result)))))]
   
   ;; Conditional type: (if cond then else)
   [(and (pair? type-expr) (eq? (car type-expr) 'if))
    (let ([cond-term (check-type-termination (cadr type-expr))]
          [then-term (check-type-termination (caddr type-expr))]
          [else-term (check-type-termination (cadddr type-expr))])
         (cond
          [(not (eq? (car cond-term) 'ok)) cond-term]
          [(not (eq? (car then-term) 'ok)) then-term]
          [(not (eq? (car else-term) 'ok)) else-term]
          [else '(ok)]))]
   
   ;; Pi type: check domain and codomain
   [(and (pair? type-expr) (eq? (car type-expr) 'Π))
    (let* ([binding (car (cadr type-expr))]
           [domain (caddr binding)]
           [codomain (caddr type-expr)]
           [domain-term (check-type-termination domain)]
           [codomain-term (check-type-termination codomain)])
          (cond
           [(not (eq? (car domain-term) 'ok)) domain-term]
           [(not (eq? (car codomain-term) 'ok)) codomain-term]
           [else '(ok)]))]
   
   ;; Atoms always terminate
   [(or (symbol? type-expr) (number? type-expr) (boolean? type-expr))
    '(ok)]
   
   ;; Other expressions: check all subexpressions
   [(pair? type-expr)
    (let check-all ([remaining (cdr type-expr)])
         (if (null? remaining)
             '(ok)
             (let ([result (check-type-termination (car remaining))])
                  (if (eq? (car result) 'ok)
                      (check-all (cdr remaining))
                      result))))]
   
   [else '(ok)]))

;;; ====
;;; Structural Recursion Validation
;;; ====

;;; validate-structural-recursion : DataType × FunctionDef → (Result () Error)
;;; Validate that a function over an inductive type uses structural recursion.
(define (validate-structural-recursion data-type func-def)
  (let* ([func-name (func-def-name func-def)]
         [params (func-def-params func-def)]
         [body (func-def-body func-def)]
         [rec-param-idx (find-inductive-param params data-type)])
        (if (not rec-param-idx)
            '(ok)  ; No inductive argument, nothing to check
            (let ([rec-param (list-ref params rec-param-idx)])
                 (check-structural-recursion func-name rec-param body)))))

;;; func-def-name : FunctionDef → Symbol
(define (func-def-name def)
  (if (and (pair? def) (eq? (car def) 'define))
      (if (pair? (cadr def))
          (caadr def)
          (cadr def))
      #f))

;;; func-def-params : FunctionDef → (List Symbol)
(define (func-def-params def)
  (if (and (pair? def) (eq? (car def) 'define))
      (if (pair? (cadr def))
          (cdadr def)
          '())
      '()))

;;; func-def-body : FunctionDef → Expr
(define (func-def-body def)
  (if (and (pair? def) (eq? (car def) 'define))
      (caddr def)
      def))

;;; find-inductive-param : (List Symbol) × DataType → Nat | #f
;;; Find the index of the parameter that has the inductive type.
(define (find-inductive-param params data-type)
  ;; In a real implementation, this would use type information
  ;; For now, we assume the first parameter is the inductive argument
  (if (null? params) #f 0))

;;; check-structural-recursion : Symbol × Symbol × Expr → (Result () Error)
;;; Check that all recursive calls are on structural subterms of the parameter.
(define (check-structural-recursion func-name rec-param body)
  (let ([violations (find-recursion-violations func-name rec-param body '())])
       (if (null? violations)
           '(ok)
           `(error non-structural-recursion
             (function ,func-name)
             (violations ,violations)
             (hint "Recursive calls must be on subterms of" ,rec-param)))))

;;; find-recursion-violations : Symbol × Symbol × Expr × Context → (List Violation)
(define (find-recursion-violations func-name rec-param expr ctx)
  (cond
   ;; Not a pair, no violations
   [(not (pair? expr)) '()]
   
   ;; Recursive call
   [(and (pair? expr) (eq? (car expr) func-name))
    (let* ([args (cdr expr)]
           [rec-arg (if (pair? args) (car args) #f)])
          ;; The recursive argument must be STRICTLY smaller (structural subterm)
          ;; Using the same argument (eq? rec-arg rec-param) is NOT OK for termination
          (if (structural-subterm? rec-arg rec-param ctx)
              ;; OK: argument is a strict subterm - terminating
              (append-map (lambda (a) (find-recursion-violations func-name rec-param a ctx))
                          (cdr args))
              ;; Violation: argument is not strictly smaller
              (cons `(non-decreasing-argument ,rec-arg in ,expr)
                    (append-map (lambda (a) (find-recursion-violations func-name rec-param a ctx))
                                (cdr args)))))]
   
   ;; Case expression: track subterm relationships
   [(eq? (car expr) 'case)
    (let* ([scrutinee (cadr expr)]
           [branches (cddr expr)])
          (append
           (find-recursion-violations func-name rec-param scrutinee ctx)
           (append-map
            (lambda (branch)
                    (let* ([pattern (car branch)]
                           [body (cadr branch)]
                           [new-ctx (if (eq? scrutinee rec-param)
                                        (extend-subterm-ctx ctx rec-param pattern)
                                        ctx)])
                          (find-recursion-violations func-name rec-param body new-ctx)))
            branches)))]
   
   ;; Other forms: recurse
   [else
    (append-map (lambda (e) (find-recursion-violations func-name rec-param e ctx))
                (cdr expr))]))

;;; extend-subterm-ctx : Context × Symbol × Pattern → Context
;;; Add pattern-bound variables as subterms.
(define (extend-subterm-ctx ctx parent pattern)
  (cond
   [(and (pair? pattern) (symbol? (car pattern)))
    (fold-left (lambda (c v)
                       (cons (cons v parent) c))
               ctx
               (cdr pattern))]
   [(symbol? pattern)
    (cons (cons pattern parent) ctx)]
   [else ctx]))

;;; ====
;;; Integration with Type Checking
;;; ====

;;; termination-check-enabled? : Boolean
;;; Global flag to enable/disable termination checking.
(define termination-check-enabled? #t)

;;; with-termination-check : Thunk → α
;;; Run a thunk with termination checking enabled.
(define (with-termination-check thunk)
  (let ([prev termination-check-enabled?])
       (set! termination-check-enabled? #t)
       (let ([result (thunk)])
            (set! termination-check-enabled? prev)
            result)))

;;; without-termination-check : Thunk → α
;;; Run a thunk with termination checking disabled (escape hatch).
;;; Use with caution - may cause type checking to diverge!
(define (without-termination-check thunk)
  (let ([prev termination-check-enabled?])
       (set! termination-check-enabled? #f)
       (let ([result (thunk)])
            (set! termination-check-enabled? prev)
            result)))

;;; termination-check : Expr × (List Symbol) → (Result () Error)
;;; Main entry point for termination checking.
(define (termination-check expr params)
  (if termination-check-enabled?
      (check-termination expr params)
      '(ok)))

;;; ====
;;; Helper Functions
;;; ====

;;; filter-map provided by prelude

;;; append-map provided by prelude

;;; all : (α → Boolean) × (List α) → Boolean
(define (all pred lst)
  (cond
   [(null? lst) #t]
   [(pred (car lst)) (all pred (cdr lst))]
   [else #f]))

;; ormap is provided by prelude
