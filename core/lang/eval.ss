;;; core/lang/eval.ss — The Evaluator with Fuel
;;; @module eval
;;; @requires prelude block prim reverse-diff
;;;
;;; The lens that lets the telescope see.
;;;
;;; Every evaluation takes fuel. When fuel exhausts, we return
;;; a suspension — the remaining expression and zero fuel.
;;; This makes Core total: evaluation always terminates.
;;;
;;; Forms:
;;;   (quote datum)           → datum
;;;   (fn (x ...) body)       → closure
;;;   (call f args...)        → apply f to args
;;;   (let ((x e) ...) body)  → bind and evaluate
;;;   (fix name (fn ...))     → recursive binding
;;;   (case e ((tag vars) body) ...) → pattern match on Block tag
;;;   (prim 'op args...)      → pure primitive
;;;   (if test then else)     → conditional
;;;   x                       → variable lookup
;;;
;;; Values:
;;;   numbers, strings, symbols, booleans, bytevectors
;;;   closures: (closure (params...) body env)
;;;   blocks: as produced by make-block
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - block.ss
;;;   - prim.ss
;;;   - reverse-diff.ss (for traced evaluation)

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/lang/prim.ss")
(load "core/autodiff/reverse-diff.ss")

;;; ============================================================
;;; Fuel
;;; ============================================================

;;; Fuel is a natural number. Each eval call costs 1 fuel.
;;; Primitives consume 0 fuel. Suspension happens only at eval boundaries.

(define (fuel? n) (and (integer? n) (>= n 0)))

(define (out-of-fuel? n) (zero? n))

;;; ============================================================
;;; Values
;;; ============================================================

;;; A value is something that doesn't reduce further.

(define (value? v)
  (or (number? v)
      (string? v)
      (boolean? v)
      (null? v)
      (bytevector? v)
      (closure? v)
      (block? v)))

(define (closure? v)
  (and (pair? v) (eq? (car v) 'closure)))

(define (closure-params c) (cadr c))
(define (closure-body c) (caddr c))
(define (closure-env c) (cadddr c))

(define (make-closure params body env)
  `(closure ,params ,body ,env))

;;; ============================================================
;;; Environments
;;; ============================================================

;;; An environment is an alist mapping symbols to values.

(define empty-env '())

(define (env-lookup env name)
  (let ([entry (assq name env)])
       (if entry
           `(ok ,(cdr entry))
           `(error unbound-variable ,name))))

(define (env-extend env name value)
  (cons (cons name value) env))

(define (env-extend* env names values)
  (if (null? names)
      env
      (env-extend* (env-extend env (car names) (car values))
                   (cdr names)
                   (cdr values))))

(define (env-extend-alist env alist)
  (append alist env))

;;; ============================================================
;;; Evaluation Context (for unified traced/untraced evaluation)
;;; ============================================================
;;;
;;; The evaluation context encapsulates mode-specific state:
;;;   - Untraced: ctx is #f
;;;   - Traced: ctx is the autodiff tape
;;;
;;; All internal evaluator functions take ctx as final parameter.
;;; Public API functions (eval-expr, eval-expr-traced) wrap these.

(define (ctx-traced? ctx) (not (eq? ctx #f)))

;;; Result constructors that include context
(define (make-ok value fuel ctx)
  `(ok ,value ,fuel ,ctx))

(define (make-suspended expr env fuel ctx)
  `(suspended ,expr ,env ,fuel ,ctx))

;;; Result accessors
(define (result-ok? r) (eq? (car r) 'ok))
(define (result-suspended? r) (eq? (car r) 'suspended))
(define (result-error? r) (eq? (car r) 'error))
(define (result-value r) (cadr r))
(define (result-fuel r) (caddr r))
(define (result-ctx r) (cadddr r))
(define (result-expr r) (cadr r))   ; for suspended
(define (result-env r) (caddr r))   ; for suspended

;;; ============================================================
;;; The Evaluator (Unified Core)
;;; ============================================================
;;;
;;; eval-core : Expr × Env × Fuel × Ctx → (ok Value Fuel Ctx) | (suspended Expr Env Fuel Ctx) | (error ...)
;;;
;;; Internal unified evaluator. Use eval-expr or eval-expr-traced for public API.

(define (eval-core expr env fuel ctx)
  (cond
   ;; Out of fuel — suspend
   [(out-of-fuel? fuel)
    (make-suspended expr env fuel ctx)]
   
   [else
    ;; Consume fuel for this eval call.
    (let ([remaining (- fuel 1)])
         (cond
          ;; Already a value
          [(value? expr)
           (make-ok expr remaining ctx)]
          
          ;; Variable reference
          [(symbol? expr)
           (let ([result (env-lookup env expr)])
                (if (eq? (car result) 'ok)
                    (make-ok (cadr result) remaining ctx)
                    result))]
          
          ;; Must be a compound form
          [(not (pair? expr))
           `(error invalid-expression ,expr)]
          
          [else
           (let ([head (car expr)])
                (cond
                 ;; Quote — return datum as-is
                 [(eq? head 'quote)
                  (make-ok (cadr expr) remaining ctx)]
                 
                 ;; Lambda — create closure
                 [(eq? head 'fn)
                  (let ([params (cadr expr)]
                        [body (caddr expr)])
                       (make-ok (make-closure params body env) remaining ctx))]
                 
                 ;; Let — evaluate bindings, extend env, evaluate body
                 [(eq? head 'let)
                  (eval-let* (cadr expr) (caddr expr) env remaining ctx)]
                 
                 ;; Fix — recursive binding
                 [(eq? head 'fix)
                  (eval-fix* (cadr expr) (caddr expr) env remaining ctx)]
                 
                 ;; If — conditional
                 [(eq? head 'if)
                  (eval-if* (cadr expr) (caddr expr) (cadddr expr) env remaining ctx)]
                 
                 ;; Case — pattern match on block tag
                 [(eq? head 'case)
                  (eval-case* (cadr expr) (cddr expr) env remaining ctx)]
                 
                 ;; Prim — pure primitive
                 [(eq? head 'prim)
                  (eval-prim* (cadr expr) (cddr expr) env remaining ctx)]
                 
                 ;; Par — parallel evaluation hint
                 [(eq? head 'par)
                  (eval-par* (cadr expr) (caddr expr) env remaining ctx)]
                 
                 ;; Pseq — sequential evaluation
                 [(eq? head 'pseq)
                  (eval-pseq* (cadr expr) (caddr expr) env remaining ctx)]
                 
                 ;; Call — explicit application
                 [(eq? head 'call)
                  (eval-call* (cadr expr) (cddr expr) env remaining ctx)]
                 
                 ;; Implicit application — (f args...)
                 [else
                  (eval-call* (car expr) (cdr expr) env remaining ctx)]))]))]))

;;; ============================================================
;;; Public API (backward-compatible wrappers)
;;; ============================================================

;;; eval-expr : Expr × Env × Fuel → (ok Value Fuel) | (suspended Expr Env) | (error ...)
;;; Standard evaluation without tracing.
(define (eval-expr expr env fuel)
  (let ([result (eval-core expr env fuel #f)])
       (cond
        [(result-ok? result)
         `(ok ,(result-value result) ,(result-fuel result))]
        [(result-suspended? result)
         `(suspended ,(result-expr result) ,(result-env result))]
        [else result])))

;;; eval-expr-traced : Expr × Env × Fuel × Tape → (ok Value Fuel Tape) | (suspended Expr Env Fuel Tape) | (error ...)
;;; Traced evaluation for automatic differentiation.
(define (eval-expr-traced expr env fuel tape)
  (let ([result (eval-core expr env fuel tape)])
       (cond
        [(result-ok? result)
         `(ok ,(result-value result) ,(result-fuel result) ,(result-ctx result))]
        [(result-suspended? result)
         `(suspended ,(result-expr result) ,(result-env result) ,(result-fuel result) ,(result-ctx result))]
        [else result])))

;;; ============================================================
;;; Par/Pseq Evaluation (Unified)
;;; ============================================================
;;;
;;; NOTE ON CURRENT IMPLEMENTATION:
;;; eval-par and eval-pseq currently have identical implementations —
;;; both evaluate a then b sequentially. This is intentional.
;;;
;;; These forms are SEMANTIC HINTS for future parallel execution:
;;;   - (par a b)  = "a and b can run in parallel; return b"
;;;   - (pseq a b) = "force a to complete before starting b; return b"
;;;
;;; In the current sequential evaluator, both behave identically.
;;; In a future parallel runtime:
;;;   - par would enable speculative/concurrent evaluation of a
;;;   - pseq would enforce strict ordering (useful when a has effects
;;;     that b depends on, or for controlling evaluation order)
;;;
;;; The distinction matters for:
;;;   1. Documenting programmer intent about parallelizability
;;;   2. Future optimization passes that could parallelize par
;;;   3. Reasoning about evaluation order in a parallel context
;;;
;;; ============================================================

;;; eval-par* : Expr × Expr × Env × Fuel × Ctx → Result
;;; Parallel evaluation hint: (par a b)
;;; Evaluate both a and b, return b.
;;; Currently evaluates sequentially, but provides a hint for
;;; future parallel execution strategies.
(define (eval-par* a-expr b-expr env fuel ctx)
  ;; Evaluate a (for side-effect of forcing computation)
  (let ([a-result (eval-core a-expr env fuel ctx)])
       (cond
        [(result-ok? a-result)
         ;; Now evaluate b with remaining fuel and updated ctx
         (eval-core b-expr env (result-fuel a-result) (result-ctx a-result))]
        [else a-result])))  ; Forward suspension or error

;;; eval-pseq* : Expr × Expr × Env × Fuel × Ctx → Result
;;; Sequential evaluation: (pseq a b)
;;; Force evaluation of a, then evaluate b, return b.
;;; Ensures a is strictly evaluated before b.
(define (eval-pseq* a-expr b-expr env fuel ctx)
  ;; Evaluate a first (strict evaluation)
  (let ([a-result (eval-core a-expr env fuel ctx)])
       (cond
        [(result-ok? a-result)
         ;; a is fully evaluated, now evaluate b
         (eval-core b-expr env (result-fuel a-result) (result-ctx a-result))]
        [else a-result])))  ; Forward suspension or error

;;; ============================================================
;;; Let Evaluation (Unified)
;;; ============================================================

(define (eval-let* bindings body env fuel ctx)
  (if (out-of-fuel? fuel)
      (make-suspended `(let ,bindings ,body) env fuel ctx)
      (eval-let-bindings* bindings body env '() fuel ctx)))

(define (eval-let-bindings* bindings body env acc fuel ctx)
  (if (null? bindings)
      ;; All bindings evaluated — evaluate body in extended env
      (eval-core body (env-extend-alist env (reverse acc)) fuel ctx)
      ;; Evaluate next binding
      (let* ([binding (car bindings)]
             [name (car binding)]
             [expr (cadr binding)]
             [result (eval-core expr env fuel ctx)])
            (cond
             [(result-ok? result)
              (eval-let-bindings*
               (cdr bindings) body env
               (cons (cons name (result-value result)) acc)
               (result-fuel result)
               (result-ctx result))]
             [(result-suspended? result)
              ;; Preserve context: rebuild let with evaluated bindings quoted
              (let* ([evaluated-bindings (map (lambda (p)
                                                      (list (car p) (list 'quote (cdr p))))
                                              (reverse acc))]
                     [suspended-binding (list name (result-expr result))]
                     [remaining-bindings (cdr bindings)]
                     [new-bindings (append evaluated-bindings
                                           (list suspended-binding)
                                           remaining-bindings)])
                    (make-suspended `(let ,new-bindings ,body) (result-env result)
                                    (result-fuel result) (result-ctx result)))]
             [else result]))))

;;; ============================================================
;;; Fix Evaluation (Recursion)
;;; ============================================================
;;;
;;; IMPORTANT: MUTATION EXCEPTION
;;; ============================
;;; This is the ONLY use of mutation (set-cdr!) in all of core/.
;;;
;;; Rationale: Recursive closures require a cyclic structure where
;;; the closure's environment contains a reference to the closure itself.
;;; In a purely functional setting, this requires either:
;;;   1. Lazy evaluation (not our strategy - we use call-by-value)
;;;   2. Explicit fixed-point combinators (Y combinator - less efficient)
;;;   3. A single, localized mutation to tie the knot
;;;
;;; We chose option 3 because:
;;;   - It's a single, well-understood pattern
;;;   - The mutation is localized and not observable from outside
;;;   - It matches how most Scheme implementations handle letrec
;;;   - Alternatives are significantly more complex or less efficient
;;;
;;; The mutation is semantically pure: the cyclic structure IS the
;;; fixed point; we're just constructing it directly rather than
;;; computing it through repeated application.
;;;
;;; See forum/engineering/0011-adr-002-eval-mutation-exception.sexp
;;; ============================================================

(define (eval-fix* name fn-expr env fuel ctx)
  ;; fn-expr should be (fn (params...) body)
  (if (and (pair? fn-expr) (eq? (car fn-expr) 'fn))
      (let* ([params (cadr fn-expr)]
             [body (caddr fn-expr)]
             ;; Create a recursive closure by including itself in its env
             [rec-env (env-extend env name 'placeholder)]
             [closure (make-closure params body rec-env)])
            ;; MUTATION: Tie the knot - see rationale above
            (set-cdr! (car rec-env) closure)
            (make-ok closure fuel ctx))
      `(error fix-requires-fn ,fn-expr)))

;;; ============================================================
;;; If Evaluation (Unified)
;;; ============================================================

(define (eval-if* test-expr then-expr else-expr env fuel ctx)
  (if (out-of-fuel? fuel)
      (make-suspended `(if ,test-expr ,then-expr ,else-expr) env fuel ctx)
      (let ([test-result (eval-core test-expr env fuel ctx)])
           (cond
            [(result-ok? test-result)
             (let* ([test-val (result-value test-result)]
                    [remaining (result-fuel test-result)]
                    [ctx* (result-ctx test-result)]
                    ;; Extract primal value from traced values for condition
                    [test-bool (if (and (ctx-traced? ctx*) (traced? test-val))
                                   (traced-value test-val)
                                   test-val)])
                   (if test-bool
                       (eval-core then-expr env remaining ctx*)
                       (eval-core else-expr env remaining ctx*)))]
            [(result-suspended? test-result)
             (make-suspended `(if ,(result-expr test-result) ,then-expr ,else-expr)
                             (result-env test-result)
                             (result-fuel test-result)
                             (result-ctx test-result))]
            [else test-result]))))

;;; ============================================================
;;; Case Evaluation (Pattern Matching) (Unified)
;;; ============================================================

;;; (case expr
;;;   ((Tag1 x y) body1)
;;;   ((Tag2 z) body2)
;;;   ...)
;;;
;;; Matches on block tag, binds payload/refs to variables.

(define (eval-case* scrutinee clauses env fuel ctx)
  (if (out-of-fuel? fuel)
      (make-suspended `(case ,scrutinee ,@clauses) env fuel ctx)
      (let ([scrut-result (eval-core scrutinee env fuel ctx)])
           (cond
            [(result-ok? scrut-result)
             (let ([val (result-value scrut-result)]
                   [remaining (result-fuel scrut-result)]
                   [ctx* (result-ctx scrut-result)])
                  (if (block? val)
                      (match-clauses* val clauses env remaining ctx*)
                      `(error case-requires-block ,val)))]
            [(result-suspended? scrut-result)
             (make-suspended `(case ,(result-expr scrut-result) ,@clauses)
                             (result-env scrut-result)
                             (result-fuel scrut-result)
                             (result-ctx scrut-result))]
            [else scrut-result]))))

(define (match-clauses* block clauses env fuel ctx)
  (if (null? clauses)
      `(error no-matching-clause ,(block-tag block))
      (let* ([clause (car clauses)]
             [pattern (car clause)]
             [body (cadr clause)]
             [tag (car pattern)]
             [vars (cdr pattern)])
            (if (eq? tag (block-tag block))
                ;; Match! Bind refs to vars
                (let ([refs (block-refs-list block)])
                     (if (= (length vars) (length refs))
                         (eval-core body (env-extend* env vars refs) fuel ctx)
                         `(error pattern-arity-mismatch ,tag)))
                ;; No match, try next clause
                (match-clauses* block (cdr clauses) env fuel ctx)))))

;;; Helper: get refs as a list
(define (block-refs-list blk)
  (let ([refs (block-refs blk)])
       (let loop ([i 0] [acc '()])
            (if (>= i (vector-length refs))
                (reverse acc)
                (loop (+ i 1) (cons (vector-ref refs i) acc))))))

;;; ============================================================
;;; Primitive Evaluation (Unified)
;;; ============================================================

;;; Primitives consume no fuel themselves; only argument evaluation costs fuel.
;;; In traced mode, differentiable primitives use traced-* operations.

(define (eval-prim* op args env fuel ctx)
  (if (and (out-of-fuel? fuel) (pair? args))
      (make-suspended `(prim ,op ,@args) env fuel ctx)
      (eval-prim-args* op args env '() fuel ctx)))

(define (eval-prim-args* op remaining env acc fuel ctx)
  (if (null? remaining)
      ;; All args evaluated — apply primitive (traced or normal based on ctx)
      (let* ([op-sym (if (and (pair? op) (eq? (car op) 'quote))
                         (cadr op)
                         op)]
             [arg-vals (reverse acc)])
            (apply-prim* op-sym arg-vals fuel ctx))
      ;; Evaluate next arg
      (let ([result (eval-core (car remaining) env fuel ctx)])
           (cond
            [(result-ok? result)
             (eval-prim-args* op (cdr remaining) env
                              (cons (result-value result) acc)
                              (result-fuel result)
                              (result-ctx result))]
            [(result-suspended? result)
             ;; Preserve context: quote evaluated args
             (let* ([evaluated-args (map (lambda (v) (list 'quote v))
                                         (reverse acc))]
                    [suspended-arg (result-expr result)]
                    [remaining-args (cdr remaining)]
                    [new-args (append evaluated-args
                                      (list suspended-arg)
                                      remaining-args)])
                   (make-suspended `(prim ,op ,@new-args) (result-env result)
                                   (result-fuel result) (result-ctx result)))]
            [else result]))))

;;; apply-prim* : Symbol × Values × Fuel × Ctx → Result
;;; Apply a primitive. In traced mode, use traced-* for differentiable ops.
(define (apply-prim* op args fuel ctx)
  (if (ctx-traced? ctx)
      (apply-prim-traced* op args fuel ctx)
      ;; Untraced: just apply the primitive
      (make-ok (apply prim (cons op args)) fuel ctx)))

;;; apply-prim-traced* : Symbol × Values × Fuel × Tape → Result
;;; Apply a primitive with tracing for autodiff.
(define (apply-prim-traced* op args fuel tape)
  (case op
        ;; Differentiable Arithmetic
        [(add)
         (if (>= (length args) 2)
             (make-ok (traced-add (car args) (cadr args)) fuel tape)
             `(error prim-arity ,op))]
        [(sub)
         (if (>= (length args) 2)
             (make-ok (traced-sub (car args) (cadr args)) fuel tape)
             `(error prim-arity ,op))]
        [(mul)
         (if (>= (length args) 2)
             (make-ok (traced-mul (car args) (cadr args)) fuel tape)
             `(error prim-arity ,op))]
        [(div)
         (if (>= (length args) 2)
             (let ([y (cadr args)])
                  (let ([y-val (if (traced? y) (traced-value y) y)])
                       (if (zero? y-val)
                           `(error div-by-zero)
                           (make-ok (traced-div (car args) y) fuel tape))))
             `(error prim-arity ,op))]
        [(neg)
         (if (= (length args) 1)
             (make-ok (traced-neg (car args)) fuel tape)
             `(error prim-arity ,op))]
        ;; Differentiable Transcendentals
        [(sqrt)
         (if (= (length args) 1)
             (make-ok (traced-sqrt (car args)) fuel tape)
             `(error prim-arity ,op))]
        [(exp)
         (if (= (length args) 1)
             (make-ok (traced-exp (car args)) fuel tape)
             `(error prim-arity ,op))]
        [(log)
         (if (= (length args) 1)
             (make-ok (traced-log (car args)) fuel tape)
             `(error prim-arity ,op))]
        [(sin)
         (if (= (length args) 1)
             (make-ok (traced-sin (car args)) fuel tape)
             `(error prim-arity ,op))]
        [(cos)
         (if (= (length args) 1)
             (make-ok (traced-cos (car args)) fuel tape)
             `(error prim-arity ,op))]
        ;; Non-differentiable: extract primal values, apply normally
        [else
         (let* ([primal-args (map (lambda (v)
                                          (if (traced? v)
                                              (traced-value v)
                                              v))
                                  args)]
                [result (apply prim (cons op primal-args))])
               (make-ok result fuel tape))]))

;;; ============================================================
;;; Call Evaluation (Application) (Unified)
;;; ============================================================

(define (eval-call* fn-expr arg-exprs env fuel ctx)
  (if (out-of-fuel? fuel)
      (make-suspended `(call ,fn-expr ,@arg-exprs) env fuel ctx)
      ;; Evaluate the function
      (let ([fn-result (eval-core fn-expr env fuel ctx)])
           (cond
            [(result-ok? fn-result)
             (let ([fn-val (result-value fn-result)]
                   [remaining (result-fuel fn-result)]
                   [ctx* (result-ctx fn-result)])
                  (if (closure? fn-val)
                      (eval-call-args* fn-val arg-exprs env '() remaining ctx*)
                      `(error not-a-function ,fn-val)))]
            [(result-suspended? fn-result)
             (make-suspended `(call ,(result-expr fn-result) ,@arg-exprs)
                             (result-env fn-result)
                             (result-fuel fn-result)
                             (result-ctx fn-result))]
            [else fn-result]))))

(define (eval-call-args* closure remaining env acc fuel ctx)
  (if (null? remaining)
      ;; All args evaluated — apply the closure
      (let* ([params (closure-params closure)]
             [body (closure-body closure)]
             [closure-env (closure-env closure)]
             [arg-vals (reverse acc)])
            (if (= (length params) (length arg-vals))
                (eval-core body (env-extend* closure-env params arg-vals) fuel ctx)
                `(error arity-mismatch (expected ,(length params)) (got ,(length arg-vals)))))
      ;; Evaluate next arg
      (let ([result (eval-core (car remaining) env fuel ctx)])
           (cond
            [(result-ok? result)
             (eval-call-args* closure (cdr remaining) env
                              (cons (result-value result) acc)
                              (result-fuel result)
                              (result-ctx result))]
            [(result-suspended? result)
             ;; Preserve context: quote closure and evaluated args
             (let* ([evaluated-args (map (lambda (v) (list 'quote v))
                                         (reverse acc))]
                    [suspended-arg (result-expr result)]
                    [remaining-args (cdr remaining)]
                    [new-args (append evaluated-args
                                      (list suspended-arg)
                                      remaining-args)])
                   (make-suspended `(call (quote ,closure) ,@new-args)
                                   (result-env result)
                                   (result-fuel result)
                                   (result-ctx result)))]
            [else result]))))

;;; ============================================================
;;; Convenience API
;;; ============================================================

;;; run : Expr × Fuel → (ok Value) | (suspended Expr) | (error ...)
;;; Evaluate an expression with empty environment.
(define (run expr fuel)
  (let ([result (eval-expr expr empty-env fuel)])
       (cond
        [(eq? (car result) 'ok)
         `(ok ,(cadr result))]
        [(eq? (car result) 'suspended)
         `(suspended ,(cadr result))]
        [else result])))

;;; run-to-completion : Expr × MaxFuel [× MaxRetries] → (ok Value) | (error ...)
;;; Keep running until completion or error (not suspension).
;;; Uses max-fuel for each resumption attempt, with configurable retry limit.
;;; Default max-retries is 10000; use #f for unlimited (trusted code only).
(define run-to-completion
  (case-lambda
   [(expr max-fuel)
    (run-to-completion expr max-fuel 10000)]
   [(expr max-fuel max-retries)
    (let loop ([expr expr] [retries max-retries])
         (if (and retries (zero? retries))
             `(error retry-limit-exceeded ,expr)
             (let ([result (eval-expr expr empty-env max-fuel)])
                  (cond
                   [(eq? (car result) 'ok)
                    `(ok ,(cadr result))]
                   [(eq? (car result) 'suspended)
                    ;; Give fresh fuel budget for resumption
                    (loop (cadr result) (and retries (- retries 1)))]
                   [else result]))))]))

;;; eval-with-env : Expr × Env × Fuel → Result
;;; Evaluate with a given environment.
(define (eval-with-env expr env fuel)
  (eval-expr expr env fuel))

;;; ============================================================
;;; Standard Prelude
;;; ============================================================

;;; Some useful functions defined in The Fold's own language.
;;; These are defined as expressions to be evaluated, ensuring
;;; proper closure semantics.

(define prelude-defs
  '(;; Basic combinators
    (id      . (fn (x) x))
    (const   . (fn (x) (fn (y) x)))
    (compose . (fn (f) (fn (g) (fn (x) (f (g x))))))
    (flip    . (fn (f) (fn (x) (fn (y) ((f y) x)))))
    (on      . (fn (f) (fn (g) (fn (x) (fn (y) ((f (g x)) (g y)))))))
    
    ;; Pair operations (pairs as 2-element lists)
    (fst     . (fn (p) (prim 'car p)))
    (snd     . (fn (p) (prim 'car (prim 'cdr p))))
    (pair    . (fn (a) (fn (b) (prim 'list a b))))
    
    ;; Boolean combinators
    (bool-not . (fn (b) (if b #f #t)))
    (bool-and . (fn (a) (fn (b) (if a b #f))))
    (bool-or  . (fn (a) (fn (b) (if a #t b))))
    
    ;; List operations (defined with fix for recursion)
    (map     . (fix map (fn (f xs)
                            (if (prim 'null? xs)
                                '()
                                (prim 'cons (f (prim 'car xs))
                                      (map f (prim 'cdr xs)))))))
    
    (filter  . (fix filter (fn (p xs)
                               (if (prim 'null? xs)
                                   '()
                                   (let ((x (prim 'car xs))
                                         (rest (filter p (prim 'cdr xs))))
                                        (if (p x)
                                            (prim 'cons x rest)
                                            rest))))))
    
    (foldl   . (fix foldl (fn (f acc xs)
                              (if (prim 'null? xs)
                                  acc
                                  (foldl f (f acc (prim 'car xs)) (prim 'cdr xs))))))
    
    (foldr   . (fix foldr (fn (f acc xs)
                              (if (prim 'null? xs)
                                  acc
                                  (f (prim 'car xs) (foldr f acc (prim 'cdr xs)))))))
    
    (scanl   . (fix scanl (fn (f acc xs)
                              (prim 'cons acc
                                    (if (prim 'null? xs)
                                        '()
                                        (let ((new-acc (f acc (prim 'car xs))))
                                             (scanl f new-acc (prim 'cdr xs))))))))
    
    (scanr   . (fix scanr (fn (f acc xs)
                              (if (prim 'null? xs)
                                  (prim 'list acc)
                                  (let ((rest (scanr f acc (prim 'cdr xs))))
                                       (prim 'cons (f (prim 'car xs) (prim 'car rest))
                                             rest))))))
    
    (take    . (fix take (fn (n xs)
                             (if (prim 'zero? n)
                                 '()
                                 (if (prim 'null? xs)
                                     '()
                                     (prim 'cons (prim 'car xs)
                                           (take (prim 'sub n 1) (prim 'cdr xs))))))))
    
    (drop    . (fix drop (fn (n xs)
                             (if (prim 'zero? n)
                                 xs
                                 (if (prim 'null? xs)
                                     '()
                                     (drop (prim 'sub n 1) (prim 'cdr xs)))))))
    
    (zip     . (fix zip (fn (xs ys)
                            (if (prim 'null? xs)
                                '()
                                (if (prim 'null? ys)
                                    '()
                                    (prim 'cons (prim 'list (prim 'car xs) (prim 'car ys))
                                          (zip (prim 'cdr xs) (prim 'cdr ys))))))))
    
    (range   . (fix range (fn (start end)
                              (if (prim 'ge? start end)
                                  '()
                                  (prim 'cons start (range (prim 'add start 1) end))))))
    
    (sum     . (fix sum (fn (xs)
                            (if (prim 'null? xs)
                                0
                                (prim 'add (prim 'car xs) (sum (prim 'cdr xs)))))))
    
    (product . (fix product (fn (xs)
                                (if (prim 'null? xs)
                                    1
                                    (prim 'mul (prim 'car xs) (product (prim 'cdr xs)))))))
    
    ;; Advanced list operations
    (flatten . (fix flatten (fn (xss)
                                (if (prim 'null? xss)
                                    '()
                                    (prim 'append (prim 'car xss)
                                          (flatten (prim 'cdr xss)))))))
    
    (flatMap . (fn (f)
                   (fix flatMap-rec (fn (xs)
                                        (if (prim 'null? xs)
                                            '()
                                            (prim 'append (f (prim 'car xs))
                                                  (flatMap-rec (prim 'cdr xs))))))))
    
    (any     . (fix any (fn (p xs)
                            (if (prim 'null? xs)
                                #f
                                (if (p (prim 'car xs))
                                    #t
                                    (any p (prim 'cdr xs)))))))
    
    (all     . (fix all (fn (p xs)
                            (if (prim 'null? xs)
                                #t
                                (if (p (prim 'car xs))
                                    (all p (prim 'cdr xs))
                                    #f)))))
    
    (elem    . (fix elem (fn (x xs)
                             (if (prim 'null? xs)
                                 #f
                                 (if (prim 'eq? x (prim 'car xs))
                                     #t
                                     (elem x (prim 'cdr xs)))))))
    
    (replicate . (fix replicate (fn (n x)
                                    (if (prim 'zero? n)
                                        '()
                                        (prim 'cons x (replicate (prim 'sub n 1) x))))))
    
    (takeWhile . (fix takeWhile (fn (p xs)
                                    (if (prim 'null? xs)
                                        '()
                                        (if (p (prim 'car xs))
                                            (prim 'cons (prim 'car xs)
                                                  (takeWhile p (prim 'cdr xs)))
                                            '())))))
    
    (dropWhile . (fix dropWhile (fn (p xs)
                                    (if (prim 'null? xs)
                                        '()
                                        (if (p (prim 'car xs))
                                            (dropWhile p (prim 'cdr xs))
                                            xs)))))
    
    (span    . (fix span (fn (p xs)
                             (if (prim 'null? xs)
                                 (prim 'list '() '())
                                 (if (p (prim 'car xs))
                                     (let ((rest (span p (prim 'cdr xs))))
                                          (prim 'list
                                                (prim 'cons (prim 'car xs) (prim 'car rest))
                                                (prim 'car (prim 'cdr rest))))
                                     (prim 'list '() xs))))))
    
    (break   . (fn (p)
                   (fix break-rec (fn (xs)
                                      (if (prim 'null? xs)
                                          (prim 'list '() '())
                                          (if (p (prim 'car xs))
                                              (prim 'list '() xs)
                                              (let ((rest (break-rec (prim 'cdr xs))))
                                                   (prim 'list
                                                         (prim 'cons (prim 'car xs) (prim 'car rest))
                                                         (prim 'car (prim 'cdr rest))))))))))
    
    (partition . (fix partition (fn (p xs)
                                    (if (prim 'null? xs)
                                        (prim 'list '() '())
                                        (let ((x (prim 'car xs))
                                              (rest (partition p (prim 'cdr xs))))
                                             (if (p x)
                                                 (prim 'list
                                                       (prim 'cons x (prim 'car rest))
                                                       (prim 'car (prim 'cdr rest)))
                                                 (prim 'list
                                                       (prim 'car rest)
                                                       (prim 'cons x (prim 'car (prim 'cdr rest))))))))))
    
    (zipWith . (fix zipWith (fn (f xs ys)
                                (if (prim 'null? xs)
                                    '()
                                    (if (prim 'null? ys)
                                        '()
                                        (prim 'cons (f (prim 'car xs) (prim 'car ys))
                                              (zipWith f (prim 'cdr xs) (prim 'cdr ys))))))))
    
    (unzip   . (fix unzip (fn (pairs)
                              (if (prim 'null? pairs)
                                  (prim 'list '() '())
                                  (let ((p (prim 'car pairs))
                                        (rest (unzip (prim 'cdr pairs))))
                                       (prim 'list
                                             (prim 'cons (prim 'car p)
                                                   (prim 'car rest))
                                             (prim 'cons (prim 'car (prim 'cdr p))
                                                   (prim 'car (prim 'cdr rest)))))))))
    
    (intersperse . (fix intersperse (fn (sep xs)
                                        (if (prim 'null? xs)
                                            '()
                                            (if (prim 'null? (prim 'cdr xs))
                                                xs
                                                (prim 'cons (prim 'car xs)
                                                      (prim 'cons sep
                                                            (intersperse sep (prim 'cdr xs)))))))))
    
    (group   . (fix group (fn (xs)
                              (if (prim 'null? xs)
                                  '()
                                  (let ((x (prim 'car xs)))
                                       (let ((rest-result (span (fn (y) (prim 'eq? x y)) (prim 'cdr xs))))
                                            (prim 'cons
                                                  (prim 'cons x (prim 'car rest-result))
                                                  (group (prim 'car (prim 'cdr rest-result))))))))))
    
    (nub     . (fix nub (fn (xs)
                            (if (prim 'null? xs)
                                '()
                                (let ((x (prim 'car xs)))
                                     (prim 'cons x (nub (filter (fn (y) (prim 'not (prim 'eq? x y))) (prim 'cdr xs)))))))))
    
    (find    . (fix find (fn (p xs)
                             (if (prim 'null? xs)
                                 'none
                                 (if (p (prim 'car xs))
                                     (prim 'cons 'some (prim 'car xs))
                                     (find p (prim 'cdr xs)))))))
    
    (splitAt . (fix splitAt (fn (n xs)
                                (if (prim 'zero? n)
                                    (prim 'list '() xs)
                                    (if (prim 'null? xs)
                                        (prim 'list '() '())
                                        (let ((rest (splitAt (prim 'sub n 1) (prim 'cdr xs))))
                                             (prim 'list
                                                   (prim 'cons (prim 'car xs) (prim 'car rest))
                                                   (prim 'car (prim 'cdr rest)))))))))
    
    ;; ================================================================
    ;; Type Class Method Implementations
    ;; ================================================================
    ;; These implement the methods declared in core/resolve.ss
    ;; Names match those used in instance definitions.
    
    ;; --- Functor ---
    ;; list-fmap is 'map' (already defined above)
    (list-fmap . (fix list-fmap (fn (f xs)
                                    (if (prim 'null? xs)
                                        '()
                                        (prim 'cons (f (prim 'car xs))
                                              (list-fmap f (prim 'cdr xs)))))))
    
    ;; option-fmap: Option is (+ (None) (Some a))
    ;; Represented as 'none or ('some . value)
    (option-fmap . (fn (f opt)
                       (if (prim 'eq? opt 'none)
                           'none
                           (prim 'cons 'some (f (prim 'cdr opt))))))
    
    ;; either-fmap: Either is (+ (Left e) (Right a))
    ;; Represented as ('left . e) or ('right . a)
    (either-fmap . (fn (f e)
                       (if (prim 'eq? (prim 'car e) 'left)
                           e
                           (prim 'cons 'right (f (prim 'cdr e))))))
    
    ;; --- Applicative ---
    (list-pure . (fn (x) (prim 'list x)))
    
    ;; list-ap: Apply list of functions to list of values
    (list-ap . (fix list-ap (fn (fs xs)
                                (if (prim 'null? fs)
                                    '()
                                    (prim 'append
                                          (map (prim 'car fs) xs)
                                          (list-ap (prim 'cdr fs) xs))))))
    
    (option-pure . (fn (x) (prim 'cons 'some x)))
    
    (option-ap . (fn (mf mx)
                     (if (prim 'eq? mf 'none)
                         'none
                         (if (prim 'eq? mx 'none)
                             'none
                             (prim 'cons 'some
                                   ((prim 'cdr mf) (prim 'cdr mx)))))))
    
    ;; --- Monad ---
    (list-bind . (fix list-bind (fn (xs f)
                                    (if (prim 'null? xs)
                                        '()
                                        (prim 'append (f (prim 'car xs))
                                              (list-bind (prim 'cdr xs) f))))))
    
    (list-return . (fn (x) (prim 'list x)))
    
    (option-bind . (fn (mx f)
                       (if (prim 'eq? mx 'none)
                           'none
                           (f (prim 'cdr mx)))))
    
    (option-return . (fn (x) (prim 'cons 'some x)))
    
    (either-bind . (fn (mx f)
                       (if (prim 'eq? (prim 'car mx) 'left)
                           mx
                           (f (prim 'cdr mx)))))
    
    (either-return . (fn (x) (prim 'cons 'right x)))
    
    ;; --- Eq ---
    (nat-eq . (fn (a b) (prim 'eq? a b)))
    (nat-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    (int-eq . (fn (a b) (prim 'eq? a b)))
    (int-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    (bool-eq . (fn (a b) (prim 'eq? a b)))
    (bool-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    (char-eq . (fn (a b) (prim 'char=? a b)))
    (char-neq . (fn (a b) (prim 'not (prim 'char=? a b))))
    (string-eq . (fn (a b) (prim 'string=? a b)))
    (string-neq . (fn (a b) (prim 'not (prim 'string=? a b))))
    (symbol-eq . (fn (a b) (prim 'eq? a b)))
    (symbol-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    
    ;; list-eq: Requires Eq on elements (recursive)
    (list-eq . (fix list-eq (fn (xs ys)
                                (if (prim 'null? xs)
                                    (prim 'null? ys)
                                    (if (prim 'null? ys)
                                        #f
                                        (if (prim 'eq? (prim 'car xs) (prim 'car ys))
                                            (list-eq (prim 'cdr xs) (prim 'cdr ys))
                                            #f))))))
    
    (list-neq . (fn (xs ys) (prim 'not (list-eq xs ys))))
    
    ;; option-eq: Requires Eq on element
    (option-eq . (fn (a b)
                     (if (prim 'eq? a 'none)
                         (prim 'eq? b 'none)
                         (if (prim 'eq? b 'none)
                             #f
                             (prim 'eq? (prim 'cdr a) (prim 'cdr b))))))
    
    ;; --- Ord ---
    ;; compare returns 'LT, 'EQ, or 'GT
    (nat-compare . (fn (a b)
                       (if (prim 'lt? a b) 'LT
                           (if (prim 'eq? a b) 'EQ 'GT))))
    (nat-lt . (fn (a b) (prim 'lt? a b)))
    (nat-lte . (fn (a b) (prim 'le? a b)))
    (nat-gt . (fn (a b) (prim 'gt? a b)))
    (nat-gte . (fn (a b) (prim 'ge? a b)))
    
    (int-compare . (fn (a b)
                       (if (prim 'lt? a b) 'LT
                           (if (prim 'eq? a b) 'EQ 'GT))))
    (int-lt . (fn (a b) (prim 'lt? a b)))
    (int-lte . (fn (a b) (prim 'le? a b)))
    (int-gt . (fn (a b) (prim 'gt? a b)))
    (int-gte . (fn (a b) (prim 'ge? a b)))
    
    (char-compare . (fn (a b)
                        (if (prim 'char<? a b) 'LT
                            (if (prim 'char=? a b) 'EQ 'GT))))
    (char-lt . (fn (a b) (prim 'char<? a b)))
    (char-lte . (fn (a b) (prim 'or (prim 'char<? a b) (prim 'char=? a b))))
    (char-gt . (fn (a b) (prim 'char<? b a)))
    (char-gte . (fn (a b) (prim 'or (prim 'char<? b a) (prim 'char=? a b))))
    
    (string-compare . (fn (a b)
                          (if (prim 'string<? a b) 'LT
                              (if (prim 'string=? a b) 'EQ 'GT))))
    (string-lt . (fn (a b) (prim 'string<? a b)))
    (string-lte . (fn (a b) (prim 'or (prim 'string<? a b) (prim 'string=? a b))))
    (string-gt . (fn (a b) (prim 'string>? a b)))
    (string-gte . (fn (a b) (prim 'or (prim 'string>? a b) (prim 'string=? a b))))
    
    ;; --- Show ---
    ;; Note: These require number->string which we add to prim.ss
    (nat-show . (fn (n) (prim 'number->string n)))
    (int-show . (fn (n) (prim 'number->string n)))
    (bool-show . (fn (b) (if b "#t" "#f")))
    (char-show . (fn (c) (prim 'list->string (prim 'list c))))
    (string-show . (fn (s) s))  ; String shows as itself
    (symbol-show . (fn (s) (prim 'symbol->string s)))
    
    ;; list-show: Requires Show on elements
    (list-show . (fix list-show (fn (xs)
                                    (if (prim 'null? xs)
                                        "()"
                                        (prim 'string-append
                                              "("
                                              (prim 'string-append
                                                    (nat-show (prim 'car xs))
                                                    (prim 'string-append
                                                          (list-show-rest (prim 'cdr xs))
                                                          ")")))))))
    
    (list-show-rest . (fix list-show-rest (fn (xs)
                                              (if (prim 'null? xs)
                                                  ""
                                                  (prim 'string-append
                                                        " "
                                                        (prim 'string-append
                                                              (nat-show (prim 'car xs))
                                                              (list-show-rest (prim 'cdr xs))))))))
    
    ;; --- Semigroup ---
    (list-append . (fn (xs ys) (prim 'append xs ys)))
    ;; string-append is already a primitive
    
    ;; --- Monoid ---
    (list-empty . '())
    ;; string-empty would be ""
    ))

;;; Build the prelude environment by evaluating definitions
(define (build-prelude-env fuel)
  (let loop ([defs prelude-defs] [env empty-env] [remaining fuel])
       (if (null? defs)
           env
           (let* ([def (car defs)]
                  [name (car def)]
                  [expr (cdr def)]
                  [result (eval-expr expr env remaining)])
                 (if (eq? (car result) 'ok)
                     (loop (cdr defs)
                           (env-extend env name (cadr result))
                           (caddr result))
                     env)))))

;;; run-prelude : Expr × Fuel → Result
;;; Evaluate with the standard prelude.
(define (run-prelude expr fuel)
  (let ([prelude-env (build-prelude-env 1000)])
       (eval-expr expr prelude-env fuel)))

;;; ============================================================
;;; High-Level Traced Evaluation API
;;; ============================================================

;;; eval-and-grad : Expr × Env × VarNames × Values × Fuel → (Value, Gradients)
;;; Evaluate expression and compute gradients w.r.t. named variables.
(define (eval-and-grad expr env var-names var-values fuel)
  (let* ([tape (make-reverse-tape)]
         ;; Create pairs of (name . value)
         [pairs (let loop ([names var-names] [vals var-values])
                     (if (null? names)
                         '()
                         (cons (cons (car names) (car vals))
                               (loop (cdr names) (cdr vals)))))]
         ;; Extend environment with traced variables
         [env* (fold-left (lambda (e name-val)
                                  (let ([name (car name-val)]
                                        [val (cdr name-val)])
                                       (env-extend e name (make-traced-var val tape))))
                          env
                          pairs)]
         ;; Traced evaluation
         [result (eval-expr-traced expr env* fuel tape)])
        (cond
         [(eq? (car result) 'ok)
          (let ([traced-val (cadr result)])
               (if (traced? traced-val)
                   ;; Backward pass
                   (let* ([output-id (traced-id traced-val)]
                          [grads-table (backward tape output-id 1)]
                          ;; Extract gradients for input variables
                          [grads (map (lambda (name)
                                              (let ([var (cadr (env-lookup env* name))])
                                                   (if (traced? var)
                                                       (hashtable-ref grads-table (traced-id var) 0)
                                                       0)))
                                      var-names)]
                          [primal (traced-value traced-val)])
                         (values primal grads))
                   ;; Constant result - all gradients are 0
                   (values traced-val (map (lambda (_) 0) var-names))))]
         [(eq? (car result) 'suspended)
          (error 'eval-and-grad "fuel exhausted during traced evaluation")]
         [else
          (error 'eval-and-grad "evaluation error" result)])))

;;; run-traced : Expr × Fuel → (ok TracedValue) | (suspended Expr) | (error ...)
;;; Evaluate with tracing enabled and empty environment.
(define (run-traced expr fuel)
  (let* ([tape (make-reverse-tape)]
         [result (eval-expr-traced expr empty-env fuel tape)])
        (cond
         [(eq? (car result) 'ok)
          `(ok ,(cadr result))]
         [(eq? (car result) 'suspended)
          `(suspended ,(cadr result))]
         [else result])))
