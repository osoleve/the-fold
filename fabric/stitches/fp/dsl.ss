;;; fabric/stitches/fp/dsl.ss — DSL Builder Utilities
;;;
;;; High-level utilities for building domain-specific languages.
;;; Combines Free monads, interpreters, and compositional patterns.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - DSL definition macros/helpers
;;;   - Interpreter composition
;;;   - Command pattern implementation
;;;   - Expression builders
;;;   - Smart constructors
;;;   - DSL combinators
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/free.ss
;;;   - fp/effects.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/free.ss")
(load "fabric/stitches/fp/effects.ss")

;;; ============================================================
;;; Command/Instruction Definitions
;;; ============================================================
;;;
;;; A DSL is defined by its instruction set.
;;; Each instruction has a tag, parameters, and a continuation.
;;; This follows the Free monad command pattern:
;;;   (tag payload continuation)
;;; where continuation receives the result of interpreting the instruction.

;;; make-instruction : Symbol -> Any -> Continuation -> Instruction
(define (make-instruction tag payload k)
  (list tag payload k))

;;; instruction-tag : Instruction -> Symbol
(define (instruction-tag instr)
  (car instr))

;;; instruction-payload : Instruction -> Any
(define (instruction-payload instr)
  (cadr instr))

;;; instruction-cont : Instruction -> (a -> Free f b)
(define (instruction-cont instr)
  (caddr instr))

;;; ============================================================
;;; DSL Functor
;;; ============================================================
;;;
;;; The DSL functor maps over the continuation.

;;; dsl-fmap : (a -> b) -> Instruction a -> Instruction b
(define (dsl-fmap f instr)
  (make-instruction
   (instruction-tag instr)
   (instruction-payload instr)
   (lambda (x) (f ((instruction-cont instr) x)))))

;;; ============================================================
;;; DSL Program Type
;;; ============================================================
;;;
;;; A DSL program is a Free monad over instructions.

;;; dsl-pure : a -> DSL f a
(define dsl-pure pure-free)

;;; dsl-pure? : DSL f a -> Boolean
(define dsl-pure? pure-free?)

;;; dsl-suspended? : DSL f a -> Boolean
(define dsl-suspended? free-suspended?)

;;; dsl-pure-value : DSL f a -> a
(define dsl-pure-value from-pure-free)

;;; dsl-instruction : DSL f a -> Instruction
(define dsl-instruction from-free)

;;; dsl-bind : DSL f a -> (a -> DSL f b) -> DSL f b
(define (dsl-bind m f)
  (free-bind dsl-fmap m f))

;;; dsl-map : (a -> b) -> DSL f a -> DSL f b
(define (dsl-map f m)
  (free-map f dsl-fmap m))

;;; dsl-emit : Symbol -> Any -> DSL Instruction ()
;;; Emit an instruction that returns unit.
(define (dsl-emit tag payload)
  (free (make-instruction tag payload pure-free)))

;;; dsl-request : Symbol -> Any -> DSL Instruction Response
;;; Emit an instruction and get a response.
(define (dsl-request tag payload)
  (free (make-instruction tag payload pure-free)))

;;; ============================================================
;;; DSL Syntax Sugars
;;; ============================================================

;;; define-instruction : (Name Param ...) -> Tag -> DSL Instruction ()
(define-syntax define-instruction
  (syntax-rules ()
                [(_ (name arg ...) tag)
                 (define (name arg ...)
                   (dsl-emit tag (list arg ...)))]
                [(_ name tag)
                 (define (name arg)
                   (dsl-emit tag arg))]))

;;; define-request : (Name Param ...) -> Tag -> DSL Instruction Response
(define-syntax define-request
  (syntax-rules ()
                [(_ (name arg ...) tag)
                 (define (name arg ...)
                   (dsl-request tag (list arg ...)))]
                [(_ name tag)
                 (define (name arg)
                   (dsl-request tag arg))]))

;;; dsl-do : Monadic do-notation for DSL programs
;;;
;;; Usage:
;;;   (dsl-do
;;;     (x <- action1)
;;;     (y <- action2)
;;;     (dsl-pure (+ x y)))
;;;
;;; Expands to:
;;;   (dsl-bind action1
;;;     (lambda (x)
;;;       (dsl-bind action2
;;;         (lambda (y)
;;;           (dsl-pure (+ x y))))))
;;;
;;; Supports:
;;;   (var <- action)  — bind result to var
;;;   action           — execute, discard result (implicitly binds to _)
(define-syntax dsl-do
  (syntax-rules (<-)
                ;; Base case: single expression (no arrow)
                [(_ expr)
                 expr]
                ;; Binding with arrow: (var <- action) rest ...
                [(_ (var <- action) rest ...)
                 (dsl-bind action (lambda (var) (dsl-do rest ...)))]
                ;; Expression without binding (discard result)
                [(_ action rest ...)
                 (dsl-bind action (lambda (_) (dsl-do rest ...)))]))

;;; ============================================================
;;; Interpreter Builder
;;; ============================================================
;;;
;;; An interpreter maps instructions to effects/values.

;;; make-interpreter : (Symbol -> Payload -> a) -> Interpreter
(define (make-interpreter handler)
  (list 'interpreter handler))

;;; interpreter? : Any -> Boolean
(define (interpreter? x)
  (and (pair? x) (eq? (car x) 'interpreter)))

;;; interpreter-handler : Interpreter -> (Symbol -> Payload -> a)
(define (interpreter-handler interp)
  (cadr interp))

;;; run-dsl : Interpreter -> DSL Instruction a -> a
;;; Run a DSL program with an interpreter.
(define (run-dsl interp program)
  (let ([handler (interpreter-handler interp)])
       (run-dsl-helper handler program)))

(define (run-dsl-helper handler program)
  (cond
   [(pure-free? program)
    (from-pure-free program)]
   [(free-suspended? program)
    (let* ([instr (from-free program)]
           [tag (instruction-tag instr)]
           [payload (instruction-payload instr)]
           [cont (instruction-cont instr)]
           [result (handler tag payload)])
          (run-dsl-helper handler (cont result)))]))

;;; ============================================================
;;; Effectful Interpreter
;;; ============================================================
;;;
;;; Interpreters that produce effects (using Eff monad).

;;; run-dsl-eff : (Symbol -> Payload -> Eff e a) -> DSL Instruction b -> Eff e b
;;; Run DSL with effectful interpreter.
(define (run-dsl-eff handler program)
  (cond
   [(pure-free? program)
    (eff-return (from-pure-free program))]
   [(free-suspended? program)
    (let* ([instr (from-free program)]
           [tag (instruction-tag instr)]
           [payload (instruction-payload instr)]
           [cont (instruction-cont instr)])
          (eff-bind (handler tag payload)
                    (lambda (result)
                            (run-dsl-eff handler (cont result)))))]))

;;; run-dsl-state : (Symbol -> Payload -> State -> (Response . State)) -> State -> DSL Instruction a -> (a . State)
;;; Pure interpreter with state passing.
(define (run-dsl-state handler state program)
  (cond
   [(pure-free? program)
    (cons (from-pure-free program) state)]
   [(free-suspended? program)
    (let* ([instr (from-free program)]
           [tag (instruction-tag instr)]
           [payload (instruction-payload instr)]
           [cont (instruction-cont instr)]
           [res (handler tag payload state)]
           [val (car res)]
           [new-state (cdr res)])
          (run-dsl-state handler new-state (cont val)))]))

;;; dsl-trace : Interpreter -> Interpreter
;;; Wrap an interpreter to trace instructions.
(define (dsl-trace interp)
  (let ([handler (interpreter-handler interp)])
       (make-interpreter
        (lambda (tag payload)
                (display (format "[DSL-TRACE] ~a: ~s\n" tag payload))
                (let ([result (handler tag payload)])
                     (display (format "[DSL-TRACE]   => ~s\n" result))
                     result)))))

;;; ============================================================
;;; Interpreter Composition
;;; ============================================================

;;; layered-interpreter : List (Symbol, (Payload -> a)) -> Interpreter
;;; Create interpreter from list of (tag, handler) pairs.
(define (layered-interpreter handlers)
  (make-interpreter
   (lambda (tag payload)
           (let ([handler-pair (assoc tag handlers)])
                (if handler-pair
                    ((cdr handler-pair) payload)
                    (error 'layered-interpreter "Unhandled instruction" tag))))))

;;; composed-interpreter : Interpreter -> Interpreter -> Interpreter
;;; Compose two interpreters. If the first fails, the second is used.
;;; Note: Requires the handlers to signal "unhandled" somehow, or we use a guard.
(define (composed-interpreter primary secondary)
  (make-interpreter
   (lambda (tag payload)
           (guard (ex [else ((interpreter-handler secondary) tag payload)])
                  ((interpreter-handler primary) tag payload)))))

;;; ============================================================
;;; Expression DSL Builder
;;; ============================================================
;;;
;;; Helpers for building expression-based DSLs.

;;; Literal expression
(define (expr-lit value)
  (list 'expr-lit value))

;;; Variable reference
(define (expr-var name)
  (list 'expr-var name))

;;; Binary operation
(define (expr-binop op left right)
  (list 'expr-binop op left right))

;;; Unary operation
(define (expr-unop op operand)
  (list 'expr-unop op operand))

;;; Function application
(define (expr-app fn args)
  (list 'expr-app fn args))

;;; Conditional
(define (expr-if cond then else)
  (list 'expr-if cond then else))

;;; Let binding
(define (expr-let name value body)
  (list 'expr-let name value body))

;;; Lambda
(define (expr-lambda params body)
  (list 'expr-lambda params body))

;;; Expression predicates
(define (expr-lit? e) (and (pair? e) (eq? (car e) 'expr-lit)))
(define (expr-var? e) (and (pair? e) (eq? (car e) 'expr-var)))
(define (expr-binop? e) (and (pair? e) (eq? (car e) 'expr-binop)))
(define (expr-unop? e) (and (pair? e) (eq? (car e) 'expr-unop)))
(define (expr-app? e) (and (pair? e) (eq? (car e) 'expr-app)))
(define (expr-if? e) (and (pair? e) (eq? (car e) 'expr-if)))
(define (expr-let? e) (and (pair? e) (eq? (car e) 'expr-let)))
(define (expr-lambda? e) (and (pair? e) (eq? (car e) 'expr-lambda)))

;;; Expression accessors
(define (expr-lit-value e) (list-ref e 1))
(define (expr-var-name e) (list-ref e 1))
(define (expr-binop-op e) (list-ref e 1))
(define (expr-binop-left e) (list-ref e 2))
(define (expr-binop-right e) (list-ref e 3))
(define (expr-unop-op e) (list-ref e 1))
(define (expr-unop-operand e) (list-ref e 2))
(define (expr-app-fn e) (list-ref e 1))
(define (expr-app-args e) (list-ref e 2))
(define (expr-if-cond e) (list-ref e 1))
(define (expr-if-then e) (list-ref e 2))
(define (expr-if-else e) (list-ref e 3))
(define (expr-let-name e) (list-ref e 1))
(define (expr-let-value e) (list-ref e 2))
(define (expr-let-body e) (list-ref e 3))
(define (expr-lambda-params e) (list-ref e 1))
(define (expr-lambda-body e) (list-ref e 2))

;;; ============================================================
;;; Expression Evaluator
;;; ============================================================

;;; eval-expr : Env -> Expr -> Value
;;; Evaluate an expression in an environment.
(define (eval-expr env expr)
  (cond
   [(expr-lit? expr)
    (expr-lit-value expr)]
   [(expr-var? expr)
    (let ([binding (assoc (expr-var-name expr) env)])
         (if binding
             (cdr binding)
             (error 'eval-expr "Unbound variable" (expr-var-name expr))))]
   [(expr-binop? expr)
    (let ([op (expr-binop-op expr)]
          [left (eval-expr env (expr-binop-left expr))]
          [right (eval-expr env (expr-binop-right expr))])
         (apply-binop op left right))]
   [(expr-unop? expr)
    (let ([op (expr-unop-op expr)]
          [operand (eval-expr env (expr-unop-operand expr))])
         (apply-unop op operand))]
   [(expr-if? expr)
    (if (eval-expr env (expr-if-cond expr))
        (eval-expr env (expr-if-then expr))
        (eval-expr env (expr-if-else expr)))]
   [(expr-let? expr)
    (let* ([name (expr-let-name expr)]
           [value (eval-expr env (expr-let-value expr))]
           [new-env (cons (cons name value) env)])
          (eval-expr new-env (expr-let-body expr)))]
   [(expr-lambda? expr)
    (list 'closure (expr-lambda-params expr) (expr-lambda-body expr) env)]
   [(expr-app? expr)
    (let ([fn (eval-expr env (expr-app-fn expr))]
          [args (map (lambda (a) (eval-expr env a)) (expr-app-args expr))])
         (apply-closure fn args))]
   [else
    (error 'eval-expr "Unknown expression type" expr)]))

;;; apply-binop : Symbol -> Value -> Value -> Value
(define (apply-binop op left right)
  (case op
        [(+) (+ left right)]
        [(-) (- left right)]
        [(*) (* left right)]
        [(/) (/ left right)]
        [(%) (modulo left right)]
        [(=) (= left right)]
        [(<) (< left right)]
        [(>) (> left right)]
        [(<=) (<= left right)]
        [(>=) (>= left right)]
        [(and) (and left right)]
        [(or) (or left right)]
        [(cons) (cons left right)]
        [else (error 'apply-binop "Unknown operator" op)]))

;;; apply-unop : Symbol -> Value -> Value
(define (apply-unop op operand)
  (case op
        [(not) (not operand)]
        [(neg) (- operand)]
        [(car) (car operand)]
        [(cdr) (cdr operand)]
        [(null?) (null? operand)]
        [else (error 'apply-unop "Unknown operator" op)]))

;;; apply-closure : Closure -> List Value -> Value
(define (apply-closure closure args)
  (if (and (pair? closure) (eq? (car closure) 'closure))
      (let ([params (list-ref closure 1)]
            [body (list-ref closure 2)]
            [env (list-ref closure 3)])
           (let ([new-env (append (map cons params args) env)])
                (eval-expr new-env body)))
      (error 'apply-closure "Not a closure" closure)))

;;; ============================================================
;;; Statement DSL Builder
;;; ============================================================
;;;
;;; For imperative-style DSLs.

;;; Statement types
(define (stmt-assign var expr)
  (list 'stmt-assign var expr))

(define (stmt-seq stmts)
  (list 'stmt-seq stmts))

(define (stmt-if cond then else)
  (list 'stmt-if cond then else))

(define (stmt-while cond body)
  (list 'stmt-while cond body))

(define (stmt-return expr)
  (list 'stmt-return expr))

(define (stmt-expr expr)
  (list 'stmt-expr expr))

;;; Statement predicates
(define (stmt-assign? s) (and (pair? s) (eq? (car s) 'stmt-assign)))
(define (stmt-seq? s) (and (pair? s) (eq? (car s) 'stmt-seq)))
(define (stmt-if? s) (and (pair? s) (eq? (car s) 'stmt-if)))
(define (stmt-while? s) (and (pair? s) (eq? (car s) 'stmt-while)))
(define (stmt-return? s) (and (pair? s) (eq? (car s) 'stmt-return)))
(define (stmt-expr? s) (and (pair? s) (eq? (car s) 'stmt-expr)))

;;; Statement accessors
(define (stmt-assign-var s) (list-ref s 1))
(define (stmt-assign-expr s) (list-ref s 2))
(define (stmt-seq-stmts s) (list-ref s 1))
(define (stmt-if-cond s) (list-ref s 1))
(define (stmt-if-then s) (list-ref s 2))
(define (stmt-if-else s) (list-ref s 3))
(define (stmt-while-cond s) (list-ref s 1))
(define (stmt-while-body s) (list-ref s 2))
(define (stmt-return-expr s) (list-ref s 1))
(define (stmt-expr-expr s) (list-ref s 1))

;;; ============================================================
;;; Statement Interpreter
;;; ============================================================

;;; run-stmt : Env -> Stmt -> (Env, Maybe Value)
;;; Run a statement, returning updated env and optional return value.
(define (run-stmt env stmt)
  (cond
   [(stmt-assign? stmt)
    (let* ([var (stmt-assign-var stmt)]
           [val (eval-expr env (stmt-assign-expr stmt))]
           [new-env (cons (cons var val)
                          (filter (lambda (p) (not (eq? (car p) var))) env))])
          (cons new-env nothing))]
   [(stmt-seq? stmt)
    (run-stmt-seq env (stmt-seq-stmts stmt))]
   [(stmt-if? stmt)
    (if (eval-expr env (stmt-if-cond stmt))
        (run-stmt env (stmt-if-then stmt))
        (run-stmt env (stmt-if-else stmt)))]
   [(stmt-while? stmt)
    (run-stmt-while env (stmt-while-cond stmt) (stmt-while-body stmt) 1000)]
   [(stmt-return? stmt)
    (cons env (just (eval-expr env (stmt-return-expr stmt))))]
   [(stmt-expr? stmt)
    (eval-expr env (stmt-expr-expr stmt))
    (cons env nothing)]
   [else
    (error 'run-stmt "Unknown statement type" stmt)]))

;;; run-stmt-seq : Env -> List Stmt -> (Env, Maybe Value)
(define (run-stmt-seq env stmts)
  (if (null? stmts)
      (cons env nothing)
      (let ([result (run-stmt env (car stmts))])
           (if (just? (cdr result))
               result  ; Early return
               (run-stmt-seq (car result) (cdr stmts))))))

;;; run-stmt-while : Env -> Expr -> Stmt -> Fuel -> (Env, Maybe Value)
(define (run-stmt-while env cond body fuel)
  (if (<= fuel 0)
      (cons env nothing)  ; Out of fuel
      (if (eval-expr env cond)
          (let ([result (run-stmt env body)])
               (if (just? (cdr result))
                   result  ; Return in loop
                   (run-stmt-while (car result) cond body (- fuel 1))))
          (cons env nothing))))

;;; ============================================================
;;; DSL Combinators
;;; ============================================================

;;; dsl-sequence : List (DSL f a) -> DSL f (List a)
(define (dsl-sequence programs)
  (if (null? programs)
      (dsl-pure '())
      (dsl-bind (car programs)
                (lambda (x)
                        (dsl-bind (dsl-sequence (cdr programs))
                                  (lambda (xs)
                                          (dsl-pure (cons x xs))))))))

;;; dsl-for-each : (a -> DSL f ()) -> List a -> DSL f ()
(define (dsl-for-each f lst)
  (if (null? lst)
      (dsl-pure '())
      (dsl-bind (f (car lst))
                (lambda (_)
                        (dsl-for-each f (cdr lst))))))

;;; dsl-fold : (b -> a -> DSL f b) -> b -> List a -> DSL f b
(define (dsl-fold f init lst)
  (if (null? lst)
      (dsl-pure init)
      (dsl-bind (f init (car lst))
                (lambda (acc)
                        (dsl-fold f acc (cdr lst))))))

;;; dsl-when : Bool -> DSL f () -> DSL f ()
(define (dsl-when pred action)
  (if pred action (dsl-pure '())))

;;; dsl-unless : Bool -> DSL f () -> DSL f ()
(define (dsl-unless pred action)
  (dsl-when (not pred) action))

;;; dsl-replicate : Int -> DSL f a -> DSL f (List a)
(define (dsl-replicate n action)
  (if (<= n 0)
      (dsl-pure '())
      (dsl-bind action
                (lambda (x)
                        (dsl-bind (dsl-replicate (- n 1) action)
                                  (lambda (xs)
                                          (dsl-pure (cons x xs))))))))

;;; ============================================================
;;; Applicative Combinators
;;; ============================================================
;;;
;;; Complete the Applicative interface for DSL programs.

;;; dsl-ap : DSL f (a -> b) -> DSL f a -> DSL f b
;;; Apply a function in the DSL context to a value in the DSL context.
(define (dsl-ap mf ma)
  (dsl-bind mf (lambda (f) (dsl-map f ma))))

;;; dsl-ap2 : (a b -> c) -> DSL f a -> DSL f b -> DSL f c
;;; Lift a binary function into DSL context.
;;; Note: Scheme doesn't curry, so we use explicit bind.
(define (dsl-ap2 f ma mb)
  (dsl-bind ma
            (lambda (a)
                    (dsl-bind mb
                              (lambda (b)
                                      (dsl-pure (f a b)))))))

;;; dsl-ap3 : (a b c -> d) -> DSL f a -> DSL f b -> DSL f c -> DSL f d
;;; Lift a ternary function into DSL context.
(define (dsl-ap3 f ma mb mc)
  (dsl-bind ma
            (lambda (a)
                    (dsl-bind mb
                              (lambda (b)
                                      (dsl-bind mc
                                                (lambda (c)
                                                        (dsl-pure (f a b c)))))))))

;;; dsl-join : DSL f (DSL f a) -> DSL f a
;;; Flatten nested DSL programs.
(define (dsl-join mma)
  (dsl-bind mma identity))

;;; dsl-kleisli : (a -> DSL f b) -> (b -> DSL f c) -> (a -> DSL f c)
;;; Kleisli composition for DSL functions.
(define (dsl-kleisli f g)
  (lambda (a)
          (dsl-bind (f a) g)))

;;; dsl-fish : (a -> DSL f b) -> (b -> DSL f c) -> (a -> DSL f c)
;;; Alias for dsl-kleisli (the "fish" operator >=>).
(define dsl-fish dsl-kleisli)

;;; dsl-compose : (b -> DSL f c) -> (a -> DSL f b) -> (a -> DSL f c)
;;; Reversed Kleisli composition (<=<).
(define (dsl-compose g f)
  (dsl-kleisli f g))

;;; ============================================================
;;; Tagless Final Support
;;; ============================================================
;;;
;;; Alternative to Free monad: define DSLs as interfaces.
;;; More efficient (no intermediate structure) but less inspectable.
;;;
;;; Pattern: Each DSL is a record of operations that work over
;;; an abstract carrier type. Interpreters instantiate the carrier.

;;; make-tagless-dsl : List (Symbol . (Interpreter -> Procedure)) -> TaglessDSL
;;; Create a tagless DSL definition from operation specifications.
(define (make-tagless-dsl ops)
  (list 'tagless-dsl ops))

;;; tagless-dsl? : Any -> Boolean
(define (tagless-dsl? x)
  (and (pair? x) (eq? (car x) 'tagless-dsl)))

;;; tagless-ops : TaglessDSL -> List
(define (tagless-ops dsl)
  (cadr dsl))

;;; instantiate-tagless : TaglessDSL -> Interpreter -> Record
;;; Create concrete operations from DSL definition and interpreter.
(define (instantiate-tagless dsl interp)
  (map (lambda (op-spec)
               (cons (car op-spec) ((cdr op-spec) interp)))
       (tagless-ops dsl)))

;;; tagless-op : Record -> Symbol -> Procedure
;;; Get an operation from an instantiated tagless DSL.
(define (tagless-op record name)
  (let ([pair (assoc name record)])
       (if pair (cdr pair)
           (error 'tagless-op "Unknown operation" name))))

;;; Example: Arithmetic Tagless DSL
;;;
;;; (define arith-tagless
;;;   (make-tagless-dsl
;;;    (list (cons 'lit (lambda (i) (lambda (n) n)))
;;;          (cons 'add (lambda (i) (lambda (a b) (+ a b))))
;;;          (cons 'mul (lambda (i) (lambda (a b) (* a b)))))))
;;;
;;; ;; Eval interpreter (identity)
;;; (define arith-eval (instantiate-tagless arith-tagless 'eval))
;;; ((tagless-op arith-eval 'add)
;;;  ((tagless-op arith-eval 'lit) 5)
;;;  ((tagless-op arith-eval 'lit) 3))
;;; ;; => 8

;;; ============================================================
;;; Source Location Tracking
;;; ============================================================
;;;
;;; Attach source positions to DSL programs for better errors.

;;; make-source-pos : Int -> Int -> Int -> SourcePos
;;; Create source position (line, column, offset).
(define (make-source-pos line col offset)
  (list 'source-pos line col offset))

;;; source-pos? : Any -> Boolean
(define (source-pos? x)
  (and (pair? x) (eq? (car x) 'source-pos)))

;;; source-pos-line : SourcePos -> Int
(define (source-pos-line p) (list-ref p 1))

;;; source-pos-col : SourcePos -> Int
(define (source-pos-col p) (list-ref p 2))

;;; source-pos-offset : SourcePos -> Int
(define (source-pos-offset p) (list-ref p 3))

;;; no-source-pos : SourcePos
(define no-source-pos (make-source-pos 0 0 0))

;;; make-located : a -> SourcePos -> Located a
;;; Attach source location to a value.
(define (make-located value pos)
  (list 'located value pos))

;;; located? : Any -> Boolean
(define (located? x)
  (and (pair? x) (eq? (car x) 'located)))

;;; located-value : Located a -> a
(define (located-value loc) (list-ref loc 1))

;;; located-pos : Located a -> SourcePos
(define (located-pos loc) (list-ref loc 2))

;;; make-located-instruction : Symbol -> Any -> Continuation -> SourcePos -> LocatedInstruction
;;; Create instruction with source location.
(define (make-located-instruction tag payload k pos)
  (list tag payload k pos))

;;; instruction-pos : LocatedInstruction -> SourcePos
(define (instruction-pos instr)
  (if (>= (length instr) 4)
      (list-ref instr 3)
      no-source-pos))

;;; dsl-emit-at : Symbol -> Any -> SourcePos -> DSL Instruction ()
;;; Emit instruction with source location.
(define (dsl-emit-at tag payload pos)
  (free (make-located-instruction tag payload pure-free pos)))

;;; dsl-request-at : Symbol -> Any -> SourcePos -> DSL Instruction Response
;;; Request with source location.
(define (dsl-request-at tag payload pos)
  (free (make-located-instruction tag payload pure-free pos)))

;;; format-source-pos : SourcePos -> String
;;; Format source position for error messages.
(define (format-source-pos pos)
  (if (source-pos? pos)
      (format "~a:~a" (source-pos-line pos) (source-pos-col pos))
      "<unknown>"))

;;; dsl-error-at : SourcePos -> Symbol -> String -> DSL Instruction a
;;; Emit error with location.
(define (dsl-error-at pos tag message)
  (dsl-emit 'dsl-error (list pos tag message)))

;;; ============================================================
;;; Interpreter Middleware
;;; ============================================================
;;;
;;; Composable interpreter transformations.

;;; Middleware : Interpreter -> Interpreter
;;; A middleware wraps an interpreter, adding behavior.

;;; middleware-identity : Middleware
;;; The identity middleware (does nothing).
(define (middleware-identity interp)
  interp)

;;; middleware-compose : Middleware -> Middleware -> Middleware
;;; Compose two middlewares (outer wraps inner).
(define (middleware-compose outer inner)
  (lambda (interp)
          (outer (inner interp))))

;;; middleware-stack : List Middleware -> Middleware
;;; Compose a list of middlewares into one.
(define (middleware-stack middlewares)
  (fold-right middleware-compose middleware-identity middlewares))

;;; middleware-logging : (Symbol -> Any -> Any -> ()) -> Middleware
;;; Log all instruction executions.
(define (middleware-logging log-fn)
  (lambda (interp)
          (let ([handler (interpreter-handler interp)])
               (make-interpreter
                (lambda (tag payload)
                        (let ([result (handler tag payload)])
                             (log-fn tag payload result)
                             result))))))

;;; middleware-timing : (Symbol -> Number -> ()) -> Middleware
;;; Time instruction execution (requires current-time).
(define (middleware-timing on-timing)
  (lambda (interp)
          (let ([handler (interpreter-handler interp)])
               (make-interpreter
                (lambda (tag payload)
                        (let* ([start (current-time)]
                               [result (handler tag payload)]
                               [end (current-time)]
                               [elapsed (- end start)])
                              (on-timing tag elapsed)
                              result))))))

;;; current-time : -> Number
;;; Get current time in milliseconds (placeholder - use system time).
(define (current-time)
  ;; In a real implementation, this would use system time
  0)

;;; middleware-cache : HashTable -> List Symbol -> Middleware
;;; Cache results for specified instruction tags.
;;; Note: Only valid for pure, deterministic instructions.
(define (middleware-cache cache cacheable-tags)
  (lambda (interp)
          (let ([handler (interpreter-handler interp)])
               (make-interpreter
                (lambda (tag payload)
                        (if (memq tag cacheable-tags)
                            (let ([key (cons tag payload)])
                                 (let ([cached (hashtable-ref cache key 'not-found)])
                                      (if (eq? cached 'not-found)
                                          (let ([result (handler tag payload)])
                                               (hashtable-set! cache key result)
                                               result)
                                          cached)))
                            (handler tag payload)))))))

;;; middleware-guard : (Symbol -> Any -> Boolean) -> (Symbol -> Any -> a) -> Middleware
;;; Guard instructions with a predicate; call fallback on failure.
(define (middleware-guard pred on-fail)
  (lambda (interp)
          (let ([handler (interpreter-handler interp)])
               (make-interpreter
                (lambda (tag payload)
                        (if (pred tag payload)
                            (handler tag payload)
                            (on-fail tag payload)))))))

;;; middleware-transform : (Symbol -> Any -> (Symbol . Any)) -> Middleware
;;; Transform instructions before passing to handler.
(define (middleware-transform xform)
  (lambda (interp)
          (let ([handler (interpreter-handler interp)])
               (make-interpreter
                (lambda (tag payload)
                        (let ([transformed (xform tag payload)])
                             (handler (car transformed) (cdr transformed))))))))

;;; with-middleware : Middleware -> Interpreter -> DSL a -> a
;;; Run DSL program with middleware-wrapped interpreter.
(define (with-middleware middleware interp program)
  (run-dsl (middleware interp) program))

;;; ============================================================
;;; Program Introspection & Optimization
;;; ============================================================
;;;
;;; Tools for analyzing and transforming DSL programs.

;;; dsl-fold-program : (a -> r) -> (Symbol -> Any -> r -> r) -> DSL f a -> Int -> r
;;; Fold over program structure (catamorphism).
;;; on-pure: handle terminal value
;;; on-instr: handle instruction (tag, payload, recursive result)
(define (dsl-fold-program on-pure on-instr program fuel)
  (cond
   [(<= fuel 0) (on-pure 'fuel-exhausted)]
   [(dsl-pure? program) (on-pure (dsl-pure-value program))]
   [else
    (let* ([instr (dsl-instruction program)]
           [tag (instruction-tag instr)]
           [payload (instruction-payload instr)]
           [cont (instruction-cont instr)]
           ;; Continue with unit to get next instruction
           [next-result (dsl-fold-program on-pure on-instr (cont '()) (- fuel 1))])
          (on-instr tag payload next-result))]))

;;; dsl-instruction-list : DSL f a -> Int -> List (Symbol . Any)
;;; Extract list of (tag . payload) pairs from program.
(define (dsl-instruction-list program fuel)
  (dsl-fold-program
   (lambda (_) '())
   (lambda (tag payload rest) (cons (cons tag payload) rest))
   program
   fuel))

;;; dsl-program-depth : DSL f a -> Int -> Int
;;; Count instruction depth (with fuel limit).
(define (dsl-program-depth program fuel)
  (dsl-fold-program
   (lambda (_) 0)
   (lambda (tag payload rest) (+ 1 rest))
   program
   fuel))

;;; dsl-uses-tag? : Symbol -> DSL f a -> Int -> Boolean
;;; Check if program uses a specific instruction tag.
(define (dsl-uses-tag? target-tag program fuel)
  (dsl-fold-program
   (lambda (_) #f)
   (lambda (tag payload rest) (or (eq? tag target-tag) rest))
   program
   fuel))

;;; dsl-tag-histogram : DSL f a -> Int -> Alist
;;; Count occurrences of each instruction tag.
(define (dsl-tag-histogram program fuel)
  (let ([counts '()])
       (dsl-fold-program
        (lambda (_) counts)
        (lambda (tag payload rest)
                (let ([existing (assoc tag rest)])
                     (if existing
                         (map (lambda (p)
                                      (if (eq? (car p) tag)
                                          (cons tag (+ 1 (cdr p)))
                                          p))
                              rest)
                         (cons (cons tag 1) rest))))
        program
        fuel)))

;;; dsl-optimize-dead-code : (Symbol -> Boolean) -> DSL f a -> Int -> DSL f a
;;; Remove instructions with side-effect-free tags whose results are unused.
;;; Note: This is a simplified optimization; real DCE needs data flow analysis.
(define (dsl-optimize-dead-code pure-tag? program fuel)
  ;; For now, just return the program unchanged
  ;; Full implementation requires tracking value usage
  program)

;;; dsl-fuse : (Symbol -> Symbol -> Maybe Symbol) -> DSL f a -> Int -> DSL f a
;;; Fuse adjacent instructions when possible.
;;; fuse-fn returns Just new-tag if two instructions can be fused.
(define (dsl-fuse fuse-fn program fuel)
  ;; Placeholder - full implementation needs instruction rewriting
  program)

;;; ============================================================
;;; Recursive DSL Programs
;;; ============================================================
;;;
;;; Support for recursive DSL definitions without stack overflow.

;;; dsl-fix : ((a -> DSL f b) -> (a -> DSL f b)) -> (a -> DSL f b)
;;; Fixed-point combinator for recursive DSL functions.
(define (dsl-fix f)
  (lambda args
          (apply (f (dsl-fix f)) args)))

;;; dsl-lazy : (() -> DSL f a) -> DSL f a
;;; Delay DSL program construction.
(define (dsl-lazy thunk)
  ;; We emit a special 'lazy instruction that forces the thunk
  (dsl-emit 'dsl-lazy thunk))

;;; Trampoline for deep recursion
;;; Bounce : (() -> Trampoline a)
;;; Done : a -> Trampoline a

;;; make-bounce : (() -> Trampoline a) -> Trampoline a
(define (make-bounce thunk)
  (list 'bounce thunk))

;;; make-done : a -> Trampoline a
(define (make-done value)
  (list 'done value))

;;; bounce? : Trampoline a -> Boolean
(define (bounce? t)
  (and (pair? t) (eq? (car t) 'bounce)))

;;; done? : Trampoline a -> Boolean
(define (done? t)
  (and (pair? t) (eq? (car t) 'done)))

;;; bounce-thunk : Trampoline a -> (() -> Trampoline a)
(define (bounce-thunk t) (cadr t))

;;; done-value : Trampoline a -> a
(define (done-value t) (cadr t))

;;; run-trampoline : Trampoline a -> a
;;; Execute trampoline until completion.
(define (run-trampoline t)
  (if (done? t)
      (done-value t)
      (run-trampoline ((bounce-thunk t)))))

;;; run-dsl-trampolined : Interpreter -> DSL f a -> a
;;; Run DSL with trampolining to avoid stack overflow.
(define (run-dsl-trampolined interp program)
  (run-trampoline (run-dsl-trampoline-helper (interpreter-handler interp) program)))

(define (run-dsl-trampoline-helper handler program)
  (cond
   [(pure-free? program)
    (make-done (from-pure-free program))]
   [(free-suspended? program)
    (let* ([instr (from-free program)]
           [tag (instruction-tag instr)]
           [payload (instruction-payload instr)]
           [cont (instruction-cont instr)]
           [result (handler tag payload)])
          (make-bounce
           (lambda ()
                   (run-dsl-trampoline-helper handler (cont result)))))]))

;;; ============================================================
;;; Testing Utilities
;;; ============================================================
;;;
;;; Helpers for testing DSL interpreters and programs.

;;; make-mock-interpreter : List (Symbol . Any) -> Interpreter
;;; Create interpreter that returns predefined values.
;;; mock-values is an alist of:
;;;   (tag . value)              - return fixed value
;;;   (tag . (fn . procedure))   - call procedure with payload
;;;   (tag . (values v1 v2 ...)) - cycle through values
(define (make-mock-interpreter mock-values)
  (let ([call-counts (map (lambda (p) (cons (car p) 0)) mock-values)])
       (make-interpreter
        (lambda (tag payload)
                ;; Increment call count
                (set! call-counts
                      (map (lambda (p)
                                   (if (eq? (car p) tag)
                                       (cons tag (+ 1 (cdr p)))
                                       p))
                           call-counts))
                ;; Return mock value
                (let ([mock (assoc tag mock-values)])
                     (if mock
                         (let ([val (cdr mock)])
                              (cond
                               ;; Function mock: (fn . procedure)
                               [(and (pair? val) (eq? (car val) 'fn))
                                ((cdr val) payload)]
                               ;; Multiple return values: (values v1 v2 ...)
                               [(and (pair? val) (eq? (car val) 'values))
                                (let* ([vals (cdr val)]
                                       [count (cdr (assoc tag call-counts))]
                                       [idx (modulo (- count 1) (length vals))])
                                      (list-ref vals idx))]
                               ;; Plain value
                               [else val]))
                         (error 'mock-interpreter "No mock for instruction" tag)))))))

;;; dsl-test-case : String -> DSL f a -> a -> Interpreter -> Boolean
;;; Test that running program produces expected result.
(define (dsl-test-case name program expected interp)
  (let ([result (run-dsl interp program)])
       (if (equal? result expected)
           (begin
            (display (format "[PASS] ~a\n" name))
            #t)
           (begin
            (display (format "[FAIL] ~a: expected ~s, got ~s\n" name expected result))
            #f))))

;;; dsl-test-error : String -> DSL f a -> Symbol -> Interpreter -> Boolean
;;; Test that running program raises an error with given tag.
(define (dsl-test-error name program expected-tag interp)
  (guard (ex [else
              (let ([is-match (and (error-object? ex)
                                   (eq? (error-object-message ex) expected-tag))])
                   (if is-match
                       (begin
                        (display (format "[PASS] ~a (error)\n" name))
                        #t)
                       (begin
                        (display (format "[FAIL] ~a: wrong error\n" name))
                        #f)))])
         (run-dsl interp program)
         (display (format "[FAIL] ~a: no error raised\n" name))
         #f))

;;; dsl-programs-equiv? : Interpreter -> DSL f a -> DSL f a -> Boolean
;;; Test if two programs produce the same result.
(define (dsl-programs-equiv? interp p1 p2)
  (equal? (run-dsl interp p1) (run-dsl interp p2)))

;;; dsl-interpreter-equiv? : DSL f a -> Interpreter -> Interpreter -> Boolean
;;; Test if two interpreters produce the same result for a program.
(define (dsl-interpreter-equiv? program i1 i2)
  (equal? (run-dsl i1 program) (run-dsl i2 program)))

;;; make-recording-interpreter : Interpreter -> (Interpreter . (() -> List))
;;; Wrap interpreter to record all instruction calls.
;;; Returns (wrapped-interpreter . get-log-fn).
(define (make-recording-interpreter base-interp)
  (let ([log '()]
        [handler (interpreter-handler base-interp)])
       (cons
        (make-interpreter
         (lambda (tag payload)
                 (let ([result (handler tag payload)])
                      (set! log (cons (list tag payload result) log))
                      result)))
        (lambda () (reverse log)))))

;;; verify-instruction-sequence : DSL f a -> Interpreter -> List Symbol -> Boolean
;;; Verify that program executes instructions in expected order.
(define (verify-instruction-sequence program interp expected-tags)
  (let* ([recording (make-recording-interpreter interp)]
         [wrapped-interp (car recording)]
         [get-log (cdr recording)]
         [_ (run-dsl wrapped-interp program)]
         [log (get-log)]
         [actual-tags (map car log)])
        (equal? actual-tags expected-tags)))

;;; ============================================================
;;; Example: Simple Calculator DSL
;;; ============================================================

;;; Calculator DSL operations
(define (calc-push! n)
  (dsl-emit 'calc-push n))

(define (calc-add!)
  (dsl-emit 'calc-add '()))

(define (calc-sub!)
  (dsl-emit 'calc-sub '()))

(define (calc-mul!)
  (dsl-emit 'calc-mul '()))

(define (calc-div!)
  (dsl-emit 'calc-div '()))

(define (calc-dup!)
  (dsl-emit 'calc-dup '()))

(define (calc-swap!)
  (dsl-emit 'calc-swap '()))

(define (calc-pop!)
  (dsl-request 'calc-pop '()))

;;; Calculator interpreter (stack-based)
(define (make-calc-interpreter)
  (let ([stack '()])
       (make-interpreter
        (lambda (tag payload)
                (case tag
                      [(calc-push)
                       (set! stack (cons payload stack))
                       '()]
                      [(calc-add)
                       (let ([a (car stack)]
                             [b (cadr stack)])
                            (set! stack (cons (+ b a) (cddr stack)))
                            '())]
                      [(calc-sub)
                       (let ([a (car stack)]
                             [b (cadr stack)])
                            (set! stack (cons (- b a) (cddr stack)))
                            '())]
                      [(calc-mul)
                       (let ([a (car stack)]
                             [b (cadr stack)])
                            (set! stack (cons (* b a) (cddr stack)))
                            '())]
                      [(calc-div)
                       (let ([a (car stack)]
                             [b (cadr stack)])
                            (set! stack (cons (/ b a) (cddr stack)))
                            '())]
                      [(calc-dup)
                       (set! stack (cons (car stack) stack))
                       '()]
                      [(calc-swap)
                       (let ([a (car stack)]
                             [b (cadr stack)])
                            (set! stack (cons b (cons a (cddr stack))))
                            '())]
                      [(calc-pop)
                       (let ([top (car stack)])
                            (set! stack (cdr stack))
                            top)]
                      [else
                       (error 'calc-interpreter "Unknown instruction" tag)])))))

;;; run-calc : DSL Instruction a -> a
(define (run-calc program)
  (run-dsl (make-calc-interpreter) program))

;;; ============================================================
;;; Example: Logo/Turtle DSL
;;; ============================================================

;;; Turtle DSL operations
(define (forward! n)
  (dsl-emit 'turtle-forward n))

(define (back! n)
  (dsl-emit 'turtle-back n))

(define (left! deg)
  (dsl-emit 'turtle-left deg))

(define (right! deg)
  (dsl-emit 'turtle-right deg))

(define (penup!)
  (dsl-emit 'turtle-penup '()))

(define (pendown!)
  (dsl-emit 'turtle-pendown '()))

(define (getpos!)
  (dsl-request 'turtle-getpos '()))

;;; Turtle interpreter
(define (make-turtle-interpreter)
  (let ([x 0]
        [y 0]
        [angle 0]
        [pen-down #t]
        [lines '()])
       (make-interpreter
        (lambda (tag payload)
                (case tag
                      [(turtle-forward)
                       (let* ([dist payload]
                              [rad (* angle (/ 3.14159265 180))]
                              [new-x (+ x (* dist (cos rad)))]
                              [new-y (+ y (* dist (sin rad)))])
                             (when pen-down
                                   (set! lines (cons (list x y new-x new-y) lines)))
                             (set! x new-x)
                             (set! y new-y)
                             '())]
                      [(turtle-back)
                       (let* ([dist payload]
                              [rad (* angle (/ 3.14159265 180))]
                              [new-x (- x (* dist (cos rad)))]
                              [new-y (- y (* dist (sin rad)))])
                             (when pen-down
                                   (set! lines (cons (list x y new-x new-y) lines)))
                             (set! x new-x)
                             (set! y new-y)
                             '())]
                      [(turtle-left)
                       (set! angle (+ angle payload))
                       '()]
                      [(turtle-right)
                       (set! angle (- angle payload))
                       '()]
                      [(turtle-penup)
                       (set! pen-down #f)
                       '()]
                      [(turtle-pendown)
                       (set! pen-down #t)
                       '()]
                      [(turtle-getpos)
                       (cons x y)]
                      [else
                       (error 'turtle-interpreter "Unknown instruction")])))))

;;; run-turtle : DSL Instruction a -> a
(define (run-turtle program)
  (run-dsl (make-turtle-interpreter) program))

;;; Helper for turtle graphics
(define (repeat n action)
  (dsl-replicate n action))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Calculator DSL
;;; (run-calc
;;;   (dsl-bind (calc-push! 10)
;;;             (lambda (_)
;;;               (dsl-bind (calc-push! 20)
;;;                         (lambda (_)
;;;                           (dsl-bind (calc-add!)
;;;                                     (lambda (_)
;;;                                       (calc-pop!))))))))
;;; ; => 30
;;;
;;; ;; Expression evaluation
;;; (eval-expr '()
;;;   (expr-let 'x (expr-lit 10)
;;;     (expr-binop '+ (expr-var 'x) (expr-lit 5))))
;;; ; => 15
;;;
;;; ;; Turtle graphics (square)
;;; (run-turtle
;;;   (repeat 4
;;;     (dsl-bind (forward! 100)
;;;               (lambda (_) (right! 90)))))
;;;
;;; ;; Using dsl-do notation (new!)
;;; (run-calc
;;;   (dsl-do
;;;     (_ <- (calc-push! 10))
;;;     (_ <- (calc-push! 20))
;;;     (_ <- (calc-add!))
;;;     (calc-pop!)))
;;; ; => 30

;;; ============================================================
;;; Error Handling
;;; ============================================================
;;;
;;; Structured error handling for DSL programs.
;;; Uses Result type to represent success/failure.

;;; Result type constructors
(define (dsl-ok value)
  (list 'dsl-ok value))

(define (dsl-err tag message)
  (list 'dsl-err tag message))

;;; Result predicates
(define (dsl-ok? r)
  (and (pair? r) (eq? (car r) 'dsl-ok)))

(define (dsl-err? r)
  (and (pair? r) (eq? (car r) 'dsl-err)))

;;; Result accessors
(define (dsl-ok-value r)
  (cadr r))

(define (dsl-err-tag r)
  (cadr r))

(define (dsl-err-message r)
  (caddr r))

;;; Result mapping
(define (dsl-result-map f r)
  (if (dsl-ok? r)
      (dsl-ok (f (dsl-ok-value r)))
      r))

(define (dsl-result-bind r f)
  (if (dsl-ok? r)
      (f (dsl-ok-value r))
      r))

;;; run-dsl-safe : Interpreter -> DSL Instruction a -> Result a
;;; Execute a DSL program, catching runtime errors.
(define (run-dsl-safe interp program)
  (guard (ex [else (dsl-err 'runtime
                            (if (message-condition? ex)
                                (condition-message ex)
                                "Unknown error"))])
         (dsl-ok (run-dsl interp program))))

;;; dsl-fail : Symbol -> String -> DSL Instruction a
;;; Emit a failure instruction.
(define (dsl-fail tag message)
  (dsl-emit 'dsl-fail (list tag message)))

;;; dsl-assert : Boolean -> Symbol -> String -> DSL Instruction ()
;;; Assert a condition, failing if false.
(define (dsl-assert condition tag message)
  (if condition
      (dsl-pure '())
      (dsl-fail tag message)))

;;; make-fail-handler : (Symbol -> String -> a) -> (Symbol -> Payload -> a)
;;; Create a handler that intercepts dsl-fail instructions.
(define (make-fail-handler on-fail default-handler)
  (lambda (tag payload)
          (if (eq? tag 'dsl-fail)
              (on-fail (car payload) (cadr payload))
              (default-handler tag payload))))

;;; ============================================================
;;; Debugging Tools
;;; ============================================================
;;;
;;; Tools for inspecting and debugging DSL programs.

;;; dsl-peek : DSL Instruction a -> (Symbol . Any)
;;; Inspect the first instruction without executing.
(define (dsl-peek program)
  (if (dsl-pure? program)
      (cons 'pure (dsl-pure-value program))
      (let ([instr (dsl-instruction program)])
           (cons (instruction-tag instr)
                 (instruction-payload instr)))))

;;; dsl-count-instructions : DSL Instruction a -> Int -> Int
;;; Count instructions in a program (with fuel limit).
(define (dsl-count-instructions program fuel)
  (if (or (<= fuel 0) (dsl-pure? program))
      0
      (let* ([instr (dsl-instruction program)]
             [cont (instruction-cont instr)])
            (+ 1 (dsl-count-instructions (cont '()) (- fuel 1))))))

;;; dsl-trace-when : (Symbol -> Any -> Bool) -> Interpreter -> Interpreter
;;; Wrap an interpreter to trace instructions matching a predicate.
(define (dsl-trace-when pred interp)
  (let ([handler (interpreter-handler interp)])
       (make-interpreter
        (lambda (tag payload)
                (when (pred tag payload)
                      (display (format "[DSL] ~a: ~s\n" tag payload)))
                (let ([result (handler tag payload)])
                     (when (pred tag payload)
                           (display (format "[DSL]   => ~s\n" result)))
                     result)))))

;;; dsl-trace-tags : List Symbol -> Interpreter -> Interpreter
;;; Trace only specific instruction tags.
(define (dsl-trace-tags tags interp)
  (dsl-trace-when (lambda (tag _) (memq tag tags)) interp))

;;; dsl-pretty-print : DSL Instruction a -> String
;;; Pretty-print the first instruction of a program.
(define (dsl-pretty-print program)
  (cond
   [(dsl-pure? program)
    (format "(pure ~s)" (dsl-pure-value program))]
   [else
    (let ([instr (dsl-instruction program)])
         (format "(~a ~s ...)"
                 (instruction-tag instr)
                 (instruction-payload instr)))]))

;;; dsl-collect-tags : DSL Instruction a -> Int -> List Symbol
;;; Collect all instruction tags in a program (with fuel limit).
(define (dsl-collect-tags program fuel)
  (if (or (<= fuel 0) (dsl-pure? program))
      '()
      (let* ([instr (dsl-instruction program)]
             [tag (instruction-tag instr)]
             [cont (instruction-cont instr)])
            (cons tag (dsl-collect-tags (cont '()) (- fuel 1))))))

;;; ============================================================
;;; DSL Combination
;;; ============================================================
;;;
;;; Tools for combining multiple DSLs into one.

;;; Sum type tags for combined instructions
(define (dsl-left-tag tag) (list 'dsl-left tag))
(define (dsl-right-tag tag) (list 'dsl-right tag))

(define (dsl-left-tag? x)
  (and (pair? x) (eq? (car x) 'dsl-left)))

(define (dsl-right-tag? x)
  (and (pair? x) (eq? (car x) 'dsl-right)))

(define (dsl-unwrap-tag x)
  (cadr x))

;;; dsl-inject-left : DSL Instruction a -> DSL (Left Instruction) a
;;; Lift a program to the left side of a union.
(define (dsl-inject-left program)
  (if (dsl-pure? program)
      program
      (let* ([instr (dsl-instruction program)]
             [tag (instruction-tag instr)]
             [payload (instruction-payload instr)]
             [cont (instruction-cont instr)])
            (free (make-instruction
                   (dsl-left-tag tag)
                   payload
                   (lambda (r) (dsl-inject-left (cont r))))))))

;;; dsl-inject-right : DSL Instruction a -> DSL (Right Instruction) a
;;; Lift a program to the right side of a union.
(define (dsl-inject-right program)
  (if (dsl-pure? program)
      program
      (let* ([instr (dsl-instruction program)]
             [tag (instruction-tag instr)]
             [payload (instruction-payload instr)]
             [cont (instruction-cont instr)])
            (free (make-instruction
                   (dsl-right-tag tag)
                   payload
                   (lambda (r) (dsl-inject-right (cont r))))))))

;;; union-interpreter : Interpreter -> Interpreter -> Interpreter
;;; Combine two interpreters into one that handles both DSLs.
(define (union-interpreter left-interp right-interp)
  (let ([left-handler (interpreter-handler left-interp)]
        [right-handler (interpreter-handler right-interp)])
       (make-interpreter
        (lambda (tag payload)
                (cond
                 [(dsl-left-tag? tag)
                  (left-handler (dsl-unwrap-tag tag) payload)]
                 [(dsl-right-tag? tag)
                  (right-handler (dsl-unwrap-tag tag) payload)]
                 [else
                  (error 'union-interpreter "Unknown instruction side" tag)])))))

;;; ============================================================
;;; Program Validation
;;; ============================================================
;;;
;;; Validate DSL programs before execution.

;;; make-dsl-schema : List (Symbol . Int) -> Schema
;;; Create a schema from list of (tag . arity) pairs.
(define (make-dsl-schema instructions)
  (list 'dsl-schema instructions))

(define (dsl-schema? x)
  (and (pair? x) (eq? (car x) 'dsl-schema)))

(define (dsl-schema-instructions schema)
  (cadr schema))

;;; dsl-validate : Schema -> DSL Instruction a -> Int -> Result (DSL Instruction a)
;;; Validate a program against a schema.
(define (dsl-validate schema program fuel)
  (cond
   [(<= fuel 0)
    (dsl-ok program)]  ; Assume valid if out of fuel
   [(dsl-pure? program)
    (dsl-ok program)]
   [else
    (let* ([instr (dsl-instruction program)]
           [tag (instruction-tag instr)]
           [payload (instruction-payload instr)]
           [cont (instruction-cont instr)]
           [valid-tags (dsl-schema-instructions schema)])
          (if (assoc tag valid-tags)
              (dsl-validate schema (cont '()) (- fuel 1))
              (dsl-err 'invalid-instruction
                       (format "Unknown instruction: ~a" tag))))]))

;;; Calculator DSL schema
(define calc-schema
  (make-dsl-schema
   '((calc-push . 1)
     (calc-add . 0)
     (calc-sub . 0)
     (calc-mul . 0)
     (calc-div . 0)
     (calc-dup . 0)
     (calc-swap . 0)
     (calc-pop . 0))))

;;; Turtle DSL schema
(define turtle-schema
  (make-dsl-schema
   '((turtle-forward . 1)
     (turtle-back . 1)
     (turtle-left . 1)
     (turtle-right . 1)
     (turtle-penup . 0)
     (turtle-pendown . 0)
     (turtle-getpos . 0))))

;;; ============================================================
;;; Resource Management
;;; ============================================================
;;;
;;; Patterns for managing resources with cleanup.

;;; dsl-finally : DSL Instruction () -> DSL Instruction a -> DSL Instruction a
;;; Execute cleanup after program, regardless of result.
(define (dsl-finally cleanup program)
  (dsl-bind program
            (lambda (result)
                    (dsl-bind cleanup
                              (lambda (_) (dsl-pure result))))))

;;; dsl-bracket : DSL Instruction r -> (r -> DSL Instruction ()) -> (r -> DSL Instruction a) -> DSL Instruction a
;;; Acquire resource, use it, then release.
(define (dsl-bracket acquire release use)
  (dsl-do
   (resource <- acquire)
   (dsl-finally (release resource) (use resource))))

;;; dsl-with-resource macro
;;; Usage: (dsl-with-resource (var acquire release) body ...)
(define-syntax dsl-with-resource
  (syntax-rules ()
                [(_ (var acquire release) body ...)
                 (dsl-bracket acquire
                              (lambda (var) release)
                              (lambda (var) (dsl-do body ...)))]))

;;; ============================================================
;;; Algebraic Effects Integration
;;; ============================================================
;;;
;;; Bridge between DSL and algebraic effects system.

;;; dsl-to-eff : DSL Instruction a -> Eff e a
;;; Convert a DSL program to an effectful computation.
(define (dsl-to-eff program)
  (cond
   [(dsl-pure? program)
    (eff-return (dsl-pure-value program))]
   [else
    (let* ([instr (dsl-instruction program)]
           [tag (instruction-tag instr)]
           [payload (instruction-payload instr)]
           [cont (instruction-cont instr)])
          (eff-bind
           (perform (make-effect tag payload))
           (lambda (result)
                   (dsl-to-eff (cont result)))))]))

;;; make-eff-interpreter : (Symbol -> Payload -> a) -> Effect Handler
;;; Create an effect handler from a DSL interpreter function.
(define (make-eff-interpreter handler)
  (lambda (effect k)
          (let* ([tag (effect-tag effect)]
                 [payload (effect-payload effect)]
                 [result (handler tag payload)])
                (k result))))

;;; run-dsl-as-eff : (Symbol -> Payload -> a) -> DSL Instruction b -> Eff e b
;;; Run a DSL program using the effects system.
(define (run-dsl-as-eff handler program)
  (dsl-to-eff program))
