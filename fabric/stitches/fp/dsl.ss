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
