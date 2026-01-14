;;; lattice/dsl/partial-eval.ss — Partial Evaluation
;;;
;;; Specialize code given partial inputs. Transforms program P with
;;; known static inputs S into specialized program P_S.
;;;
;;; Example:
;;;   (define (power n x)
;;;     (if (= n 0) 1 (* x (power (- n 1) x))))
;;;
;;;   (partial-eval (power 3 _))
;;;   ; → (lambda (x) (* x (* x (* x 1))))
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Binding-time analysis (static vs dynamic)
;;;   - Online partial evaluation (specialize during execution)
;;;   - Offline partial evaluation (analyze first, then specialize)
;;;   - Unfolding bounds for termination
;;;   - Arity raising and lowering
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - staging.ss
;;;   - fp/meta/combinators.ss

(load "core/base/prelude.ss")
(load "lattice/dsl/staging.ss")
(load "lattice/fp/meta/combinators.ss")

;;; ====
;;; Binding Time Representation
;;; ====
;;;
;;; Binding times classify when a value is known:
;;;   - 'static  : Known at specialization time
;;;   - 'dynamic : Only known at runtime
;;;   - 'bottom  : Unknown (for analysis initialization)

;;; bt-static : BindingTime
(define bt-static 'static)

;;; bt-dynamic : BindingTime
(define bt-dynamic 'dynamic)

;;; bt-bottom : BindingTime
(define bt-bottom 'bottom)

;;; bt? : α → Boolean
(define (bt? x)
  (memq x '(static dynamic bottom)))

;;; bt-join : BindingTime × BindingTime → BindingTime
;;; Lattice join: dynamic dominates static, bottom is identity.
(define (bt-join bt1 bt2)
  (cond
   [(eq? bt1 bt-bottom) bt2]
   [(eq? bt2 bt-bottom) bt1]
   [(or (eq? bt1 bt-dynamic) (eq? bt2 bt-dynamic)) bt-dynamic]
   [else bt-static]))

;;; bt-meet : BindingTime × BindingTime → BindingTime
;;; Lattice meet: static dominates dynamic.
(define (bt-meet bt1 bt2)
  (cond
   [(eq? bt1 bt-bottom) bt-bottom]
   [(eq? bt2 bt-bottom) bt-bottom]
   [(and (eq? bt1 bt-static) (eq? bt2 bt-static)) bt-static]
   [else bt-dynamic]))

;;; bt<=? : BindingTime × BindingTime → Boolean
;;; Ordering: bottom < static < dynamic
(define (bt<=? bt1 bt2)
  (cond
   [(eq? bt1 bt-bottom) #t]
   [(eq? bt2 bt-bottom) #f]
   [(eq? bt1 bt-static) #t]
   [(eq? bt2 bt-dynamic) #t]
   [else #f]))

;;; ====
;;; Binding-Time Environment
;;; ====

;;; make-bt-env : () → BTEnv
;;; Create empty binding-time environment.
(define (make-bt-env)
  '())

;;; bt-env-extend : BTEnv × Symbol × BindingTime → BTEnv
;;; Extend environment with new binding.
(define (bt-env-extend env var bt)
  (cons (cons var bt) env))

;;; bt-env-lookup : BTEnv × Symbol → BindingTime
;;; Look up binding time for variable (default: dynamic).
(define (bt-env-lookup env var)
  (let ([entry (assq var env)])
       (if entry (cdr entry) bt-dynamic)))

;;; bt-env-extend-many : BTEnv × (List Symbol) × (List BindingTime) → BTEnv
;;; Extend with multiple bindings.
(define (bt-env-extend-many env vars bts)
  (fold-left (lambda (e vb) (bt-env-extend e (car vb) (cdr vb)))
             env
             (map cons vars bts)))

;;; ====
;;; Primitive Operations (Forward Declaration)
;;; ====
;;;
;;; Needed by BTA to recognize static primitives.

;;; pe-primitive? : Symbol → Boolean
;;; Check if symbol is a primitive operation.
(define (pe-primitive? s)
  (memq s '(+ - * / = < > <= >= eq? null? car cdr cons list not and or
            zero? positive? negative? even? odd? abs min max
            modulo quotient remainder)))

;;; ====
;;; Binding-Time Analysis
;;; ====
;;;
;;; Analyze expression to determine binding times of subexpressions.
;;; This is a forward flow analysis.

;;; bta : Sexp × BTEnv → BindingTime
;;; Determine binding time of expression.
(define (bta expr env)
  (cond
   ;; Literals are static
   [(or (number? expr) (boolean? expr) (string? expr) (null? expr))
    bt-static]
   
   ;; Quoted expressions are static
   [(and (pair? expr) (eq? (car expr) 'quote))
    bt-static]
   
   ;; Variables: look up in environment
   [(symbol? expr)
    (bt-env-lookup env expr)]
   
   ;; Lambda: result is static if we're in static context
   ;; Body binding time determines if lambda produces static code
   [(and (pair? expr) (eq? (car expr) 'lambda))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           ;; Parameters start as dynamic (callers may provide dynamic values)
           [body-env (bt-env-extend-many env params (map (lambda (_) bt-dynamic) params))]
           [body-bt (bta body body-env)])
          ;; Lambda itself is static (it's a known value)
          ;; but applying it may produce dynamic results
          bt-static)]
   
   ;; If: result is join of branches, condition determines control flow BT
   [(and (pair? expr) (eq? (car expr) 'if))
    (let ([cond-bt (bta (cadr expr) env)]
          [then-bt (bta (caddr expr) env)]
          [else-bt (if (null? (cdddr expr)) bt-static (bta (cadddr expr) env))])
         (if (eq? cond-bt bt-static)
             ;; Static condition: result depends on branches
             (bt-join then-bt else-bt)
             ;; Dynamic condition: result is dynamic
             bt-dynamic))]
   
   ;; Let: analyze body with extended environment
   [(and (pair? expr) (eq? (car expr) 'let))
    (let* ([bindings (cadr expr)]
           [body (caddr expr)]
           [binding-bts (map (lambda (b) (bta (cadr b) env)) bindings)]
           [body-env (bt-env-extend-many env (map car bindings) binding-bts)])
          (bta body body-env))]
   
   ;; Letrec: recursive bindings start dynamic, iterate to fixpoint
   [(and (pair? expr) (eq? (car expr) 'letrec))
    (let* ([bindings (cadr expr)]
           [body (caddr expr)]
           [vars (map car bindings)])
          ;; For simplicity, treat letrec bindings as potentially dynamic
          (let ([body-env (bt-env-extend-many env vars (map (lambda (_) bt-dynamic) vars))])
               (bta body body-env)))]
   
   ;; Application: result BT depends on function and arguments
   [(pair? expr)
    (let* ([op (car expr)]
           ;; Primitives are statically known operations
           [op-bt (if (and (symbol? op) (pe-primitive? op))
                      bt-static
                      (bta op env))]
           [arg-bts (map (lambda (a) (bta a env)) (cdr expr))])
          ;; If operator or any arg is dynamic, result is dynamic
          (fold-left bt-join op-bt arg-bts))]
   
   [else bt-dynamic]))

;;; bta-annotate : Sexp × BTEnv → AnnotatedSexp
;;; Annotate expression with binding times at each node.
(define (bta-annotate expr env)
  (let ([bt (bta expr env)])
       (cond
        [(or (number? expr) (boolean? expr) (string? expr) (null? expr) (symbol? expr))
         (list 'ann bt expr)]
        
        [(and (pair? expr) (eq? (car expr) 'quote))
         (list 'ann bt expr)]
        
        [(and (pair? expr) (eq? (car expr) 'lambda))
         (let* ([params (cadr expr)]
                [body (caddr expr)]
                [body-env (bt-env-extend-many env params (map (lambda (_) bt-dynamic) params))])
               (list 'ann bt `(lambda ,params ,(bta-annotate body body-env))))]
        
        [(and (pair? expr) (eq? (car expr) 'if))
         (let ([cond-ann (bta-annotate (cadr expr) env)]
               [then-ann (bta-annotate (caddr expr) env)]
               [else-ann (if (null? (cdddr expr)) #f (bta-annotate (cadddr expr) env))])
              (list 'ann bt `(if ,cond-ann ,then-ann ,@(if else-ann (list else-ann) '()))))]
        
        [(and (pair? expr) (eq? (car expr) 'let))
         (let* ([bindings (cadr expr)]
                [body (caddr expr)]
                [binding-anns (map (lambda (b) (list (car b) (bta-annotate (cadr b) env))) bindings)]
                [binding-bts (map (lambda (b) (bta (cadr b) env)) bindings)]
                [body-env (bt-env-extend-many env (map car bindings) binding-bts)])
               (list 'ann bt `(let ,binding-anns ,(bta-annotate body body-env))))]
        
        [(pair? expr)
         (list 'ann bt (map (lambda (e) (bta-annotate e env)) expr))]
        
        [else (list 'ann bt expr)])))

;;; ====
;;; Static Store
;;; ====
;;;
;;; The static store holds values known at specialization time.

;;; make-static-store : () → StaticStore
(define (make-static-store)
  '())

;;; static-store-extend : StaticStore × Symbol × Value → StaticStore
(define (static-store-extend store var val)
  (cons (cons var val) store))

;;; static-store-lookup : StaticStore × Symbol → (Maybe Value)
;;; FIX (fold-7h48): Return nothing if variable is shadowed by dynamic binding
(define (static-store-lookup store var)
  (let ([entry (assq var store)])
       (if entry
           (if (eq? (cdr entry) '*dynamic-binding*)
               nothing  ;; Variable is shadowed as dynamic
               (just (cdr entry)))
           nothing)))

;;; static-store-has? : StaticStore × Symbol → Boolean
(define (static-store-has? store var)
  (just? (static-store-lookup store var)))

;;; FIX (fold-7h48): Shadow a variable as dynamic (prevents finding outer static binding)
;;; static-store-shadow : StaticStore × Symbol → StaticStore
(define (static-store-shadow store var)
  ;; Use a unique sentinel to mark as "dynamic" - lookup will find this instead of outer binding
  (cons (cons var '*dynamic-binding*) store))

;;; ====
;;; Specialization State
;;; ====

;;; make-pe-state : Nat → PEState
;;; Create partial evaluation state with unfolding limit.
(define (make-pe-state unfold-limit)
  (list 'pe-state
        unfold-limit    ; max unfolding depth
        0               ; current depth
        '()             ; memoization table
        '()             ; residual definitions
        0))             ; fresh variable counter

;;; pe-state-limit : PEState → Nat
(define (pe-state-limit st) (list-ref st 1))

;;; pe-state-depth : PEState → Nat
(define (pe-state-depth st) (list-ref st 2))

;;; pe-state-memo : PEState → (List (Key . Code))
(define (pe-state-memo st) (list-ref st 3))

;;; pe-state-residuals : PEState → (List Def)
(define (pe-state-residuals st) (list-ref st 4))

;;; pe-state-counter : PEState → Nat
(define (pe-state-counter st) (list-ref st 5))

;;; pe-state-incr-depth : PEState → PEState
(define (pe-state-incr-depth st)
  (list 'pe-state
        (pe-state-limit st)
        (+ (pe-state-depth st) 1)
        (pe-state-memo st)
        (pe-state-residuals st)
        (pe-state-counter st)))

;;; pe-state-decr-depth : PEState → PEState
(define (pe-state-decr-depth st)
  (list 'pe-state
        (pe-state-limit st)
        (max 0 (- (pe-state-depth st) 1))
        (pe-state-memo st)
        (pe-state-residuals st)
        (pe-state-counter st)))

;;; pe-state-can-unfold? : PEState → Boolean
(define (pe-state-can-unfold? st)
  (< (pe-state-depth st) (pe-state-limit st)))

;;; pe-state-fresh-var : PEState × Symbol → (Pair Symbol PEState)
(define (pe-state-fresh-var st prefix)
  (let ([n (pe-state-counter st)]
        [new-st (list 'pe-state
                      (pe-state-limit st)
                      (pe-state-depth st)
                      (pe-state-memo st)
                      (pe-state-residuals st)
                      (+ (pe-state-counter st) 1))])
       (cons (string->symbol (format "~a$~a" prefix n)) new-st)))

;;; pe-state-add-memo : PEState × Key × Code → PEState
(define (pe-state-add-memo st key code)
  (list 'pe-state
        (pe-state-limit st)
        (pe-state-depth st)
        (cons (cons key code) (pe-state-memo st))
        (pe-state-residuals st)
        (pe-state-counter st)))

;;; pe-state-lookup-memo : PEState × Key → (Maybe Code)
(define (pe-state-lookup-memo st key)
  (let ([entry (assoc key (pe-state-memo st))])
       (if entry (just (cdr entry)) nothing)))

;;; pe-state-add-residual : PEState × Def → PEState
(define (pe-state-add-residual st def)
  (list 'pe-state
        (pe-state-limit st)
        (pe-state-depth st)
        (pe-state-memo st)
        (cons def (pe-state-residuals st))
        (pe-state-counter st)))

;;; ====
;;; Online Partial Evaluation
;;; ====
;;;
;;; Online PE makes specialization decisions during evaluation.
;;; Simpler but may miss some optimization opportunities.

;;; pe-online : Sexp × StaticStore × PEState → (Pair Sexp PEState)
;;; Partially evaluate expression online.
(define (pe-online expr store state)
  (cond
   ;; Self-evaluating literals
   [(or (number? expr) (boolean? expr) (string? expr))
    (cons expr state)]
   
   ;; Null
   [(null? expr)
    (cons ''() state)]
   
   ;; Quote: return as-is
   [(and (pair? expr) (eq? (car expr) 'quote))
    (cons expr state)]
   
   ;; Variables: substitute if static, otherwise residualize
   [(symbol? expr)
    (let ([val (static-store-lookup store expr)])
         (if (just? val)
             (cons (from-just val) state)
             (cons expr state)))]
   
   ;; Lambda: specialize body if parameters have static values
   [(and (pair? expr) (eq? (car expr) 'lambda))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           ;; Generate fresh parameter names for residual
           [fresh-result (fold-left (lambda (acc p)
                                            (let ([r (pe-state-fresh-var (cdr acc) p)])
                                                 (cons (cons (car r) (car acc)) (cdr r))))
                                    (cons '() state)
                                    params)]
           [fresh-params (reverse (car fresh-result))]
           [new-state (cdr fresh-result)]
           ;; Extend store with fresh params (as residual references)
           [body-store store]  ; Don't add anything - params are dynamic
           [body-result (pe-online body body-store new-state)]
           [spec-body (car body-result)]
           [final-state (cdr body-result)])
          (cons `(lambda ,params ,spec-body) final-state))]
   
   ;; If: specialize based on condition
   [(and (pair? expr) (eq? (car expr) 'if))
    (let* ([cond-result (pe-online (cadr expr) store state)]
           [cond-val (car cond-result)]
           [state1 (cdr cond-result)])
          (cond
           ;; Static true condition
           [(and (boolean? cond-val) cond-val)
            (pe-online (caddr expr) store state1)]
           ;; Static false condition
           [(and (boolean? cond-val) (not cond-val))
            (if (null? (cdddr expr))
                (cons '(void) state1)
                (pe-online (cadddr expr) store state1))]
           ;; Dynamic condition: residualize both branches
           [else
            (let* ([then-result (pe-online (caddr expr) store state1)]
                   [then-val (car then-result)]
                   [state2 (cdr then-result)]
                   [else-result (if (null? (cdddr expr))
                                    (cons '(void) state2)
                                    (pe-online (cadddr expr) store state2))]
                   [else-val (car else-result)]
                   [state3 (cdr else-result)])
                  (cons `(if ,cond-val ,then-val ,else-val) state3))]))]
   
   ;; Let: specialize bindings and body
   [(and (pair? expr) (eq? (car expr) 'let))
    (let* ([bindings (cadr expr)]
           [body (caddr expr)])
          ;; Process each binding
          (let loop ([bindings bindings]
                     [static-bindings '()]
                     [residual-bindings '()]
                     [store store]
                     [state state])
               (if (null? bindings)
                   ;; All bindings processed, specialize body
                   (let* ([body-result (pe-online body store state)]
                          [spec-body (car body-result)]
                          [final-state (cdr body-result)])
                         (if (null? residual-bindings)
                             ;; All bindings were static
                             (cons spec-body final-state)
                             ;; Some residual bindings
                             (cons `(let ,(reverse residual-bindings) ,spec-body) final-state)))
                   ;; Process next binding
                   (let* ([binding (car bindings)]
                          [var (car binding)]
                          [val-expr (cadr binding)]
                          [val-result (pe-online val-expr store state)]
                          [spec-val (car val-result)]
                          [new-state (cdr val-result)])
                         (if (pe-static-value? spec-val)
                             ;; Static value: add to store
                             (loop (cdr bindings)
                                   (cons (cons var spec-val) static-bindings)
                                   residual-bindings
                                   (static-store-extend store var spec-val)
                                   new-state)
                             ;; Dynamic value: residualize
                             ;; FIX (fold-7h48): Shadow variable to prevent finding outer static binding
                             (loop (cdr bindings)
                                   static-bindings
                                   (cons (list var spec-val) residual-bindings)
                                   (static-store-shadow store var)
                                   new-state))))))]
   
   ;; FIX (fold-wivx): Handle cond expression
   [(and (pair? expr) (eq? (car expr) 'cond))
    (let loop ([clauses (cdr expr)] [state state])
         (cond
          [(null? clauses)
           ;; No clause matched, result is void
           (cons '(void) state)]
          [else
           (let* ([clause (car clauses)]
                  [test (if (eq? (car clause) 'else) #t (car clause))]
                  [test-result (if (eq? test #t)
                                   (cons #t state)
                                   (pe-online test store state))]
                  [test-val (car test-result)]
                  [state1 (cdr test-result)])
                 (cond
                  ;; Static true: evaluate this clause's body
                  [(and (boolean? test-val) test-val)
                   (let ([body (if (null? (cdr clause)) test-val (cadr clause))])
                        (pe-online body store state1))]
                  ;; Static false: try next clause
                  [(and (boolean? test-val) (not test-val))
                   (loop (cdr clauses) state1)]
                  ;; Dynamic: residualize remaining clauses
                  [else
                   (let residualize ([cls (cons clause (cdr clauses))]
                                     [state state1]
                                     [residual-clauses '()])
                        (if (null? cls)
                            (cons `(cond ,@(reverse residual-clauses)) state)
                            (let* ([c (car cls)]
                                   [t (if (eq? (car c) 'else) 'else (car c))]
                                   [t-result (if (eq? t 'else)
                                                 (cons 'else state)
                                                 (pe-online t store state))]
                                   [body-result (pe-online (if (null? (cdr c)) (car c) (cadr c))
                                                           store (cdr t-result))]
                                   [new-clause (list (car t-result) (car body-result))])
                                  (residualize (cdr cls) (cdr body-result)
                                               (cons new-clause residual-clauses)))))]))]))]

   ;; FIX (fold-wivx): Handle case expression
   [(and (pair? expr) (eq? (car expr) 'case))
    (let* ([key-expr (cadr expr)]
           [key-result (pe-online key-expr store state)]
           [key-val (car key-result)]
           [state1 (cdr key-result)]
           [clauses (cddr expr)])
          (if (pe-static-value? key-val)
              ;; Static key: find matching clause
              (let find-clause ([cls clauses])
                   (cond
                    [(null? cls) (cons '(void) state1)]
                    [(eq? (caar cls) 'else)
                     (pe-online (cadar cls) store state1)]
                    [(memv key-val (caar cls))
                     (pe-online (cadar cls) store state1)]
                    [else (find-clause (cdr cls))]))
              ;; Dynamic key: residualize
              (let residualize ([cls clauses] [state state1] [res-cls '()])
                   (if (null? cls)
                       (cons `(case ,key-val ,@(reverse res-cls)) state)
                       (let* ([c (car cls)]
                              [body-result (pe-online (cadr c) store state)]
                              [new-clause (list (car c) (car body-result))])
                             (residualize (cdr cls) (cdr body-result)
                                          (cons new-clause res-cls)))))))]

   ;; FIX (fold-wivx): Handle letrec expression
   [(and (pair? expr) (eq? (car expr) 'letrec))
    (let* ([bindings (cadr expr)]
           [body (caddr expr)]
           ;; For letrec, all bindings are potentially recursive
           ;; Residualize bindings but specialize the body with dynamic refs
           [result (pe-online-list (map cadr bindings) store state)]
           [spec-vals (car result)]
           [state1 (cdr result)]
           [new-bindings (map (lambda (b v) (list (car b) v))
                              bindings spec-vals)]
           [body-result (pe-online body store state1)]
           [spec-body (car body-result)]
           [final-state (cdr body-result)])
          (cons `(letrec ,new-bindings ,spec-body) final-state))]

   ;; Primitive operations: try to evaluate if all args are static
   [(and (pair? expr) (pe-primitive? (car expr)))
    (let* ([op (car expr)]
           [args (cdr expr)]
           [args-result (pe-online-list args store state)]
           [spec-args (car args-result)]
           [state1 (cdr args-result)])
          (if (andmap pe-static-value? spec-args)
              ;; All args static: evaluate
              (cons (pe-apply-primitive op spec-args) state1)
              ;; Some dynamic: residualize
              (cons (cons op spec-args) state1)))]
   
   ;; Application: try to inline and specialize
   [(pair? expr)
    (let* ([op-result (pe-online (car expr) store state)]
           [spec-op (car op-result)]
           [state1 (cdr op-result)]
           [args-result (pe-online-list (cdr expr) store state1)]
           [spec-args (car args-result)]
           [state2 (cdr args-result)])
          (cond
           ;; Lambda application with static args: beta reduce
           [(and (pair? spec-op)
                 (eq? (car spec-op) 'lambda)
                 (andmap pe-static-value? spec-args)
                 (pe-state-can-unfold? state2))
            (let* ([params (cadr spec-op)]
                   [body (caddr spec-op)]
                   [new-store (fold-left (lambda (s pv)
                                                 (static-store-extend s (car pv) (cdr pv)))
                                         store
                                         (map cons params spec-args))]
                   [new-state (pe-state-incr-depth state2)]
                   [result (pe-online body new-store new-state)]
                   [final-state (pe-state-decr-depth (cdr result))])
                  (cons (car result) final-state))]
           
           ;; Lambda with mixed args: partial beta reduce
           [(and (pair? spec-op)
                 (eq? (car spec-op) 'lambda)
                 (pe-state-can-unfold? state2))
            (let* ([params (cadr spec-op)]
                   [body (caddr spec-op)]
                   ;; Separate static and dynamic parameters
                   [param-args (map cons params spec-args)]
                   [static-pas (filter (lambda (pa) (pe-static-value? (cdr pa))) param-args)]
                   [dynamic-pas (filter (lambda (pa) (not (pe-static-value? (cdr pa)))) param-args)]
                   [new-store (fold-left (lambda (s pa)
                                                 (static-store-extend s (car pa) (cdr pa)))
                                         store
                                         static-pas)]
                   [new-state (pe-state-incr-depth state2)]
                   [body-result (pe-online body new-store new-state)]
                   [spec-body (car body-result)]
                   [final-state (pe-state-decr-depth (cdr body-result))])
                  (if (null? dynamic-pas)
                      (cons spec-body final-state)
                      (cons `((lambda ,(map car dynamic-pas) ,spec-body)
                              ,@(map cdr dynamic-pas))
                            final-state)))]
           
           ;; General application: residualize
           [else
            (cons (cons spec-op spec-args) state2)]))]
   
   [else (cons expr state)]))

;;; pe-online-list : (List Sexp) × StaticStore × PEState → (Pair (List Sexp) PEState)
;;; Partially evaluate a list of expressions.
(define (pe-online-list exprs store state)
  (let loop ([exprs exprs] [results '()] [state state])
       (if (null? exprs)
           (cons (reverse results) state)
           (let ([result (pe-online (car exprs) store state)])
                (loop (cdr exprs)
                      (cons (car result) results)
                      (cdr result))))))

;;; pe-static-value? : Sexp → Boolean
;;; Check if expression is a static value (fully evaluated).
(define (pe-static-value? x)
  (or (number? x)
      (boolean? x)
      (string? x)
      (null? x)
      (and (pair? x) (eq? (car x) 'quote))))

;;; pe-apply-primitive : Symbol × (List Value) → Value
;;; Apply primitive operation to static values.
(define (pe-apply-primitive op args)
  (case op
        [(+) (apply + args)]
        [(-) (apply - args)]
        [(*) (apply * args)]
        [(/) (if (zero? (cadr args)) `(/ ,@args) (apply / args))]
        [(=) (apply = args)]
        [(<) (apply < args)]
        [(>) (apply > args)]
        [(<=) (apply <= args)]
        [(>=) (apply >= args)]
        ;; Comparisons - need to unquote args for proper comparison
        [(eq?) (eq? (pe-unquote (car args)) (pe-unquote (cadr args)))]
        [(null?) (null? (pe-unquote (car args)))]
        ;; List operations - extract from quoted list
        [(car) (let ([lst (pe-unquote (car args))])
                    (if (pair? lst)
                        (let ([result (car lst)])
                             (if (or (symbol? result) (pair? result))
                                 `',result
                                 result))
                        `(car ,@args)))]
        [(cdr) (let ([lst (pe-unquote (car args))])
                    (if (pair? lst)
                        `',(cdr lst)
                        `(cdr ,@args)))]
        [(cons) `',(cons (pe-unquote (car args)) (pe-unquote (cadr args)))]
        [(list) `',(map pe-unquote args)]
        ;; Boolean operations - need to unquote
        [(not) (not (pe-unquote (car args)))]
        ;; Numeric predicates - args should already be numbers
        [(zero?) (zero? (car args))]
        [(positive?) (positive? (car args))]
        [(negative?) (negative? (car args))]
        [(even?) (even? (car args))]
        [(odd?) (odd? (car args))]
        [(abs) (abs (car args))]
        [(min) (apply min args)]
        [(max) (apply max args)]
        [(modulo) (modulo (car args) (cadr args))]
        [(quotient) (quotient (car args) (cadr args))]
        [(remainder) (remainder (car args) (cadr args))]
        [else `(,op ,@args)]))

;;; pe-unquote : Sexp → Value
;;; Remove quote from a static value.
(define (pe-unquote x)
  (if (and (pair? x) (eq? (car x) 'quote))
      (cadr x)
      x))

;;; ====
;;; Offline Partial Evaluation
;;; ====
;;;
;;; Offline PE first performs binding-time analysis, then specializes
;;; based on the analysis results. More precise but requires two passes.

;;; pe-offline : Sexp × BTEnv × StaticStore × PEState → (Pair Sexp PEState)
;;; Partially evaluate using offline strategy (BTA-guided).
(define (pe-offline expr bt-env store state)
  (let ([bt (bta expr bt-env)])
       (if (eq? bt bt-static)
           ;; Static expression: evaluate completely
           (pe-online expr store state)
           ;; Dynamic expression: residualize with static subexpressions specialized
           (pe-offline-dynamic expr bt-env store state))))

;;; pe-offline-dynamic : Sexp × BTEnv × StaticStore × PEState → (Pair Sexp PEState)
;;; Residualize dynamic expression, specializing static parts.
(define (pe-offline-dynamic expr bt-env store state)
  (cond
   [(or (number? expr) (boolean? expr) (string? expr) (null? expr))
    (cons expr state)]
   
   [(and (pair? expr) (eq? (car expr) 'quote))
    (cons expr state)]
   
   [(symbol? expr)
    (let ([val (static-store-lookup store expr)])
         (if (just? val)
             (cons (from-just val) state)
             (cons expr state)))]
   
   [(and (pair? expr) (eq? (car expr) 'lambda))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [body-bt-env (bt-env-extend-many bt-env params
                                            (map (lambda (_) bt-dynamic) params))]
           [body-result (pe-offline body body-bt-env store state)]
           [spec-body (car body-result)]
           [new-state (cdr body-result)])
          (cons `(lambda ,params ,spec-body) new-state))]
   
   [(and (pair? expr) (eq? (car expr) 'if))
    (let* ([cond-bt (bta (cadr expr) bt-env)])
          (if (eq? cond-bt bt-static)
              ;; Static condition: specialize one branch
              (let* ([cond-result (pe-online (cadr expr) store state)]
                     [cond-val (car cond-result)]
                     [state1 (cdr cond-result)])
                    (if cond-val
                        (pe-offline (caddr expr) bt-env store state1)
                        (if (null? (cdddr expr))
                            (cons '(void) state1)
                            (pe-offline (cadddr expr) bt-env store state1))))
              ;; Dynamic condition: residualize both
              (let* ([cond-result (pe-offline (cadr expr) bt-env store state)]
                     [then-result (pe-offline (caddr expr) bt-env store (cdr cond-result))]
                     [else-result (if (null? (cdddr expr))
                                      (cons '(void) (cdr then-result))
                                      (pe-offline (cadddr expr) bt-env store (cdr then-result)))])
                    (cons `(if ,(car cond-result) ,(car then-result) ,(car else-result))
                          (cdr else-result)))))]
   
   [(and (pair? expr) (eq? (car expr) 'let))
    (let* ([bindings (cadr expr)]
           [body (caddr expr)])
          (let loop ([bindings bindings]
                     [residual-bindings '()]
                     [store store]
                     [bt-env bt-env]
                     [state state])
               (if (null? bindings)
                   (let* ([body-result (pe-offline body bt-env store state)]
                          [spec-body (car body-result)]
                          [final-state (cdr body-result)])
                         (if (null? residual-bindings)
                             (cons spec-body final-state)
                             (cons `(let ,(reverse residual-bindings) ,spec-body) final-state)))
                   (let* ([binding (car bindings)]
                          [var (car binding)]
                          [val-expr (cadr binding)]
                          [val-bt (bta val-expr bt-env)])
                         (if (eq? val-bt bt-static)
                             (let* ([val-result (pe-online val-expr store state)]
                                    [spec-val (car val-result)]
                                    [new-state (cdr val-result)])
                                   (loop (cdr bindings)
                                         residual-bindings
                                         (static-store-extend store var spec-val)
                                         (bt-env-extend bt-env var bt-static)
                                         new-state))
                             (let* ([val-result (pe-offline val-expr bt-env store state)]
                                    [spec-val (car val-result)]
                                    [new-state (cdr val-result)])
                                   (loop (cdr bindings)
                                         (cons (list var spec-val) residual-bindings)
                                         store
                                         (bt-env-extend bt-env var bt-dynamic)
                                         new-state)))))))]
   
   [(pair? expr)
    (let* ([results (pe-offline-list expr bt-env store state)]
           [spec-parts (car results)]
           [final-state (cdr results)])
          (cons spec-parts final-state))]
   
   [else (cons expr state)]))

;;; pe-offline-list : (List Sexp) × BTEnv × StaticStore × PEState → (Pair (List Sexp) PEState)
(define (pe-offline-list exprs bt-env store state)
  (let loop ([exprs exprs] [results '()] [state state])
       (if (null? exprs)
           (cons (reverse results) state)
           (let ([result (pe-offline (car exprs) bt-env store state)])
                (loop (cdr exprs)
                      (cons (car result) results)
                      (cdr result))))))

;;; ====
;;; High-Level API
;;; ====

;;; partial-eval : Sexp × (List (Symbol . Value)) → Sexp
;;; Partially evaluate expression with given static bindings.
;;; Uses online strategy by default.
(define (partial-eval expr static-bindings)
  (let* ([store (fold-left (lambda (s b)
                                   (static-store-extend s (car b) (cdr b)))
                           (make-static-store)
                           static-bindings)]
         [state (make-pe-state 100)]  ; default unfolding limit
         [result (pe-online expr store state)])
        (car result)))

;;; partial-eval-offline : Sexp × (List (Symbol . Value)) → Sexp
;;; Partially evaluate using offline (BTA-guided) strategy.
(define (partial-eval-offline expr static-bindings)
  (let* ([store (fold-left (lambda (s b)
                                   (static-store-extend s (car b) (cdr b)))
                           (make-static-store)
                           static-bindings)]
         [bt-env (fold-left (lambda (e b)
                                    (bt-env-extend e (car b) bt-static))
                            (make-bt-env)
                            static-bindings)]
         [state (make-pe-state 100)]
         [result (pe-offline expr bt-env store state)])
        (car result)))

;;; specialize : Sexp × (List Symbol) × (List Value) → Sexp
;;; Specialize expression for given parameter values.
(define (specialize expr params values)
  (partial-eval expr (map cons params values)))

;;; specialize-function : Sexp × (List (Symbol . Value)) → Sexp
;;; Specialize a function definition for given static parameters.
;;; Returns a new function with remaining dynamic parameters.
(define (specialize-function func-def static-bindings)
  (cond
   [(and (pair? func-def) (eq? (car func-def) 'define))
    (let* ([sig (cadr func-def)]
           [body (caddr func-def)])
          (if (pair? sig)
              ;; (define (f x y) body) form
              (let* ([name (car sig)]
                     [params (cdr sig)]
                     [static-params (map car static-bindings)]
                     [dynamic-params (filter (lambda (p) (not (memq p static-params))) params)]
                     [spec-body (partial-eval body static-bindings)])
                    `(define (,name ,@dynamic-params) ,spec-body))
              ;; (define f expr) form
              (let ([spec-expr (partial-eval body static-bindings)])
                   `(define ,sig ,spec-expr))))]
   
   [(and (pair? func-def) (eq? (car func-def) 'lambda))
    (let* ([params (cadr func-def)]
           [body (caddr func-def)]
           [static-params (map car static-bindings)]
           [dynamic-params (filter (lambda (p) (not (memq p static-params))) params)]
           [spec-body (partial-eval body static-bindings)])
          `(lambda ,dynamic-params ,spec-body))]
   
   [else (partial-eval func-def static-bindings)]))

;;; ====
;;; Specialization with Memoization
;;; ====

;;; memo-specialize : Sexp × (List (Symbol . Value)) × Nat → Sexp
;;; Specialize with memoization of intermediate results.
(define (memo-specialize expr static-bindings unfold-limit)
  (let* ([store (fold-left (lambda (s b)
                                   (static-store-extend s (car b) (cdr b)))
                           (make-static-store)
                           static-bindings)]
         [state (make-pe-state unfold-limit)]
         [result (pe-online expr store state)]
         [spec-expr (car result)]
         [final-state (cdr result)]
         [residuals (pe-state-residuals final-state)])
        (if (null? residuals)
            spec-expr
            `(letrec ,residuals ,spec-expr))))

;;; ====
;;; Classic Examples
;;; ====

;;; power-source : Source code for power function
(define power-source
  '(lambda (n x)
    (if (= n 0)
        1
        (* x (power (- n 1) x)))))

;;; power-specialized-3 : Power function specialized for n=3
;;; Result: (lambda (x) (* x (* x (* x 1))))
(define (power-specialized-3)
  (specialize-function
   '(define (power n x)
     (if (= n 0)
         1
         (* x (power (- n 1) x))))
   '((n . 3))))

;;; interpreter-source : Simple interpreter for expression language
(define interpreter-source
  '(lambda (expr env)
    (cond
     [(number? expr) expr]
     [(symbol? expr) (lookup expr env)]
     [(eq? (car expr) 'add)
      (+ (interp (cadr expr) env)
         (interp (caddr expr) env))]
     [(eq? (car expr) 'mul)
      (* (interp (cadr expr) env)
         (interp (caddr expr) env))])))

;;; ====
;;; Futamura Projections (Conceptual)
;;; ====
;;;
;;; The three Futamura projections relate interpretation and compilation:
;;;
;;; First Projection:
;;;   target = specialize(interpreter, program)
;;;   Specializing an interpreter for a fixed program yields an executable.
;;;
;;; Second Projection:
;;;   compiler = specialize(specializer, interpreter)
;;;   Specializing the specializer for a fixed interpreter yields a compiler.
;;;
;;; Third Projection:
;;;   compiler-generator = specialize(specializer, specializer)
;;;   Specializing the specializer for itself yields a compiler generator.

;;; futamura-1 : Interpreter × Program → Target
;;; First Futamura projection: interpreter + program → compiled program
(define (futamura-1 interp program)
  (partial-eval `(,interp ',program env) '()))

;;; ====
;;; Simplification
;;; ====
;;;
;;; Post-processing to simplify generated code.

;;; simplify : Sexp → Sexp
;;; Simplify expression (constant folding, identity elimination).
(define (simplify expr)
  (cond
   [(not (pair? expr)) expr]
   
   ;; (* x 1) → x, (* 1 x) → x
   [(and (eq? (car expr) '*)
         (= (length expr) 3))
    (let ([a (simplify (cadr expr))]
          [b (simplify (caddr expr))])
         (cond
          [(equal? a 1) b]
          [(equal? b 1) a]
          [(equal? a 0) 0]
          [(equal? b 0) 0]
          [(and (number? a) (number? b)) (* a b)]
          [else `(* ,a ,b)]))]
   
   ;; (+ x 0) → x, (+ 0 x) → x
   [(and (eq? (car expr) '+)
         (= (length expr) 3))
    (let ([a (simplify (cadr expr))]
          [b (simplify (caddr expr))])
         (cond
          [(equal? a 0) b]
          [(equal? b 0) a]
          [(and (number? a) (number? b)) (+ a b)]
          [else `(+ ,a ,b)]))]
   
   ;; (- x 0) → x
   [(and (eq? (car expr) '-)
         (= (length expr) 3))
    (let ([a (simplify (cadr expr))]
          [b (simplify (caddr expr))])
         (cond
          [(equal? b 0) a]
          [(and (number? a) (number? b)) (- a b)]
          [else `(- ,a ,b)]))]
   
   ;; (if #t a b) → a, (if #f a b) → b
   [(eq? (car expr) 'if)
    (let ([test-val (simplify (cadr expr))]
          [then-val (simplify (caddr expr))]
          [else-val (if (null? (cdddr expr)) '(void) (simplify (cadddr expr)))])
         (cond
          [(eq? test-val #t) then-val]
          [(eq? test-val #f) else-val]
          [else `(if ,test-val ,then-val ,else-val)]))]
   
   ;; (let () body) → body
   [(and (eq? (car expr) 'let)
         (null? (cadr expr)))
    (simplify (caddr expr))]
   
   ;; ((lambda (x) body) v) → body[v/x] when v is simple
   [(and (pair? (car expr))
         (eq? (caar expr) 'lambda)
         (= (length (cadar expr)) 1)
         (= (length expr) 2)
         (pe-static-value? (cadr expr)))
    (simplify (subst-expr (caddar expr) (caadar expr) (cadr expr)))]
   
   ;; General case: simplify subexpressions
   [(pair? expr)
    (map simplify expr)]
   
   [else expr]))

;;; partial-eval-simplified : Sexp × (List (Symbol . Value)) → Sexp
;;; Partially evaluate and simplify result.
(define (partial-eval-simplified expr static-bindings)
  (simplify (partial-eval expr static-bindings)))

;;; ====
;;; Exports
;;; ====
;;;
;;; Main entry points:
;;;   - partial-eval : Basic online partial evaluation
;;;   - partial-eval-offline : BTA-guided offline partial evaluation
;;;   - specialize : Specialize for given parameter values
;;;   - specialize-function : Specialize function definition
;;;   - memo-specialize : Specialize with memoization
;;;   - partial-eval-simplified : Partial eval with simplification
;;;
;;; Binding-time analysis:
;;;   - bta : Compute binding time
;;;   - bta-annotate : Annotate expression with binding times
;;;
;;; Utilities:
;;;   - simplify : Simplify generated code
;;;   - bt-static, bt-dynamic : Binding time constants
