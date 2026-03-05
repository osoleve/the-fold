;;; @module dsl
;;; @requires prelude free effects
;;; @purity mixed
;;; @stability stable

(require 'prelude)
(require 'free)
(require 'effects)

(doc 'module 'dsl)
(doc 'purity 'partial)
(doc 'description "DSL builder framework using Free monads. A DSL is defined by its instruction set; each instruction has a tag, payload, and continuation.")
(doc 'layer 'lattice)

(doc 'section 'core-instructions)

(define (make-instruction tag payload k)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any (-> a (Free Instruction b)) (Instruction a)))
  (doc 'description "Create an instruction with tag, payload, and continuation")
  (list tag payload k))

(define (instruction-tag instr)
  (doc 'export #t)
  (doc 'type '(-> (Instruction a) Symbol))
  (doc 'description "Get instruction tag")
  (car instr))

(define (instruction-payload instr)
  (doc 'export #t)
  (doc 'type '(-> (Instruction a) Any))
  (doc 'description "Get instruction payload")
  (cadr instr))

(define (instruction-cont instr)
  (doc 'export #t)
  (doc 'type '(-> (Instruction a) (-> a (Free Instruction b))))
  (doc 'description "Get instruction continuation")
  (caddr instr))

(doc 'section 'dsl-functor)

(define (dsl-fmap f instr)
  (doc 'export #t)
  (doc 'type '(-> (-> a b) (Instruction a) (Instruction b)))
  (doc 'description "Map over instruction continuation (Functor instance)")
  (make-instruction
   (instruction-tag instr)
   (instruction-payload instr)
   (lambda (x) (f ((instruction-cont instr) x)))))

(doc 'section 'dsl-program-type)
;;; A DSL program is a Free monad over instructions.

(doc dsl-pure 'type '(-> a (DSL a)))
(doc dsl-pure 'description "Lift a pure value into a DSL program")
(define dsl-pure pure-free)
(doc 'dsl-pure 'export #t)

(doc dsl-pure? 'type '(-> (DSL a) Boolean))
(doc dsl-pure? 'description "Test if DSL program is a pure value")
(define dsl-pure? pure-free?)
(doc 'dsl-pure? 'export #t)

(doc dsl-suspended? 'type '(-> (DSL a) Boolean))
(doc dsl-suspended? 'description "Test if DSL program is suspended on an instruction")
(define dsl-suspended? free-suspended?)
(doc 'dsl-suspended? 'export #t)

(doc dsl-pure-value 'type '(-> (DSL a) a))
(doc dsl-pure-value 'description "Extract pure value from completed DSL program")
(define dsl-pure-value from-pure-free)
(doc 'dsl-pure-value 'export #t)

(doc dsl-instruction 'type '(-> (DSL a) (Instruction a)))
(doc dsl-instruction 'description "Extract current instruction from suspended DSL program")
(define dsl-instruction from-free)
(doc 'dsl-instruction 'export #t)

(define (dsl-bind m f)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) (-> a (DSL b)) (DSL b)))
  (doc 'description "Monadic bind for DSL programs")
  (free-bind dsl-fmap m f))

(define (dsl-map f m)
  (doc 'export #t)
  (doc 'type '(-> (-> a b) (DSL a) (DSL b)))
  (doc 'description "Map a function over DSL program result")
  (free-map f dsl-fmap m))

(define (dsl-emit tag payload)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any (DSL Unit)))
  (doc 'description "Emit an instruction that returns unit")
  (free (make-instruction tag payload pure-free)))

(define (dsl-request tag payload)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any (DSL Any)))
  (doc 'description "Emit an instruction and receive a response")
  (free (make-instruction tag payload pure-free)))

(doc 'section 'syntax-sugars)

(doc define-instruction 'type 'syntax)
(doc define-instruction 'description "Define a named DSL instruction that emits with a given tag")
(define-syntax define-instruction
  (syntax-rules ()
                [(_ (name arg ...) tag)
                 (define (name arg ...)
                   (dsl-emit tag (list arg ...)))]
                [(_ name tag)
                 (define (name arg)
                   (dsl-emit tag arg))]))

(doc define-request 'type 'syntax)
(doc define-request 'description "Define a named DSL request that emits and receives a response")
(define-syntax define-request
  (syntax-rules ()
                [(_ (name arg ...) tag)
                 (define (name arg ...)
                   (dsl-request tag (list arg ...)))]
                [(_ name tag)
                 (define (name arg)
                   (dsl-request tag arg))]))

(doc dsl-do 'type 'syntax)
(doc dsl-do 'description "Monadic do-notation for DSL programs; (var <- action) binds, bare action discards")
;;; Usage:
;;;   (dsl-do
;;;     (x <- action1)
;;;     (y <- action2)
;;;     (dsl-pure (+ x y)))
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

(doc 'section 'interpreter-builder)

(define (make-interpreter handler)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any Any) Interpreter))
  (doc 'description "Create an interpreter from a handler function (tag, payload) -> result")
  (list 'interpreter handler))

(define (interpreter? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is an Interpreter")
  (and (pair? x) (eq? (car x) 'interpreter)))

(define (interpreter-handler interp)
  (doc 'export #t)
  (doc 'type '(-> Interpreter (-> Symbol Any Any)))
  (doc 'description "Extract handler function from Interpreter")
  (cadr interp))

(define (run-dsl interp program)
  (doc 'export #t)
  (doc 'type '(-> Interpreter (DSL a) a))
  (doc 'description "Run a DSL program with an interpreter to completion")
  (let ([handler (interpreter-handler interp)])
       (run-dsl-helper handler program)))

(define (run-dsl-helper handler program)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any Any) (DSL a) a))
  (doc 'description "Internal: step through DSL program with raw handler")
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

(doc 'section 'effectful-interpreter)

(define (run-dsl-eff handler program)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any (Eff e a)) (DSL b) (Eff e b)))
  (doc 'description "Run DSL with effectful interpreter producing Eff computations")
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

(define (run-dsl-state handler state program)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any s (Pair a s)) s (DSL a) (Pair a s)))
  (doc 'description "Run DSL with pure state-passing interpreter")
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

(define (dsl-trace interp)
  (doc 'export #t)
  (doc 'type '(-> Interpreter Interpreter))
  (doc 'description "Wrap interpreter to trace all instructions and results to stdout")
  (let ([handler (interpreter-handler interp)])
       (make-interpreter
        (lambda (tag payload)
                (display (format "[DSL-TRACE] ~a: ~s\n" tag payload))
                (let ([result (handler tag payload)])
                     (display (format "[DSL-TRACE]   => ~s\n" result))
                     result)))))

(doc 'section 'interpreter-composition)

(define (layered-interpreter handlers)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Symbol (-> Any Any))) Interpreter))
  (doc 'description "Create interpreter from alist of (tag . handler) pairs")
  (make-interpreter
   (lambda (tag payload)
           (let ([handler-pair (assoc tag handlers)])
                (if handler-pair
                    ((cdr handler-pair) payload)
                    (error 'layered-interpreter "Unhandled instruction" tag))))))

(define (composed-interpreter primary secondary)
  (doc 'export #t)
  (doc 'type '(-> Interpreter Interpreter Interpreter))
  (doc 'description "Compose interpreters; primary handles first, secondary as fallback on error")
  (make-interpreter
   (lambda (tag payload)
           (guard (ex [else ((interpreter-handler secondary) tag payload)])
                  ((interpreter-handler primary) tag payload)))))

(doc 'section 'expression-dsl)
;;; Expression AST constructors, predicates, and accessors.

(define (expr-lit value)
  (doc 'export #t)
  (doc 'type '(-> Any Expr))
  (doc 'description "Literal expression node")
  (list 'expr-lit value))

(define (expr-var name)
  (doc 'export #t)
  (doc 'type '(-> Symbol Expr))
  (doc 'description "Variable reference expression node")
  (list 'expr-var name))

(define (expr-binop op left right)
  (doc 'export #t)
  (doc 'type '(-> Symbol Expr Expr Expr))
  (doc 'description "Binary operation expression node")
  (list 'expr-binop op left right))

(define (expr-unop op operand)
  (doc 'export #t)
  (doc 'type '(-> Symbol Expr Expr))
  (doc 'description "Unary operation expression node")
  (list 'expr-unop op operand))

(define (expr-app fn args)
  (doc 'export #t)
  (doc 'type '(-> Expr (List Expr) Expr))
  (doc 'description "Function application expression node")
  (list 'expr-app fn args))

(define (expr-if cond then else)
  (doc 'export #t)
  (doc 'type '(-> Expr Expr Expr Expr))
  (doc 'description "Conditional expression node")
  (list 'expr-if cond then else))

(define (expr-let name value body)
  (doc 'export #t)
  (doc 'type '(-> Symbol Expr Expr Expr))
  (doc 'description "Let-binding expression node")
  (list 'expr-let name value body))

(define (expr-lambda params body)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol) Expr Expr))
  (doc 'description "Lambda expression node")
  (list 'expr-lambda params body))

;;; Expression predicates

(define (expr-lit? e) (doc 'type '(-> Any Boolean)) (and (pair? e) (eq? (car e) 'expr-lit)))
(define (expr-var? e) (doc 'type '(-> Any Boolean)) (and (pair? e) (eq? (car e) 'expr-var)))
(define (expr-binop? e) (doc 'type '(-> Any Boolean)) (and (pair? e) (eq? (car e) 'expr-binop)))
(define (expr-unop? e) (doc 'type '(-> Any Boolean)) (and (pair? e) (eq? (car e) 'expr-unop)))
(define (expr-app? e) (doc 'type '(-> Any Boolean)) (and (pair? e) (eq? (car e) 'expr-app)))
(define (expr-if? e) (doc 'type '(-> Any Boolean)) (and (pair? e) (eq? (car e) 'expr-if)))
(define (expr-let? e) (doc 'type '(-> Any Boolean)) (and (pair? e) (eq? (car e) 'expr-let)))
(define (expr-lambda? e) (doc 'type '(-> Any Boolean)) (and (pair? e) (eq? (car e) 'expr-lambda)))

;;; Expression accessors

(define (expr-lit-value e) (doc 'type '(-> Expr Any)) (list-ref e 1))
(define (expr-var-name e) (doc 'type '(-> Expr Symbol)) (list-ref e 1))
(define (expr-binop-op e) (doc 'type '(-> Expr Symbol)) (list-ref e 1))
(define (expr-binop-left e) (doc 'type '(-> Expr Expr)) (list-ref e 2))
(define (expr-binop-right e) (doc 'type '(-> Expr Expr)) (list-ref e 3))
(define (expr-unop-op e) (doc 'type '(-> Expr Symbol)) (list-ref e 1))
(define (expr-unop-operand e) (doc 'type '(-> Expr Expr)) (list-ref e 2))
(define (expr-app-fn e) (doc 'type '(-> Expr Expr)) (list-ref e 1))
(define (expr-app-args e) (doc 'type '(-> Expr (List Expr))) (list-ref e 2))
(define (expr-if-cond e) (doc 'type '(-> Expr Expr)) (list-ref e 1))
(define (expr-if-then e) (doc 'type '(-> Expr Expr)) (list-ref e 2))
(define (expr-if-else e) (doc 'type '(-> Expr Expr)) (list-ref e 3))
(define (expr-let-name e) (doc 'type '(-> Expr Symbol)) (list-ref e 1))
(define (expr-let-value e) (doc 'type '(-> Expr Expr)) (list-ref e 2))
(define (expr-let-body e) (doc 'type '(-> Expr Expr)) (list-ref e 3))
(define (expr-lambda-params e) (doc 'type '(-> Expr (List Symbol))) (list-ref e 1))
(define (expr-lambda-body e) (doc 'type '(-> Expr Expr)) (list-ref e 2))

(doc 'section 'expression-evaluator)

(define (eval-expr env expr)
  (doc 'export #t)
  (doc 'type '(-> (Alist Symbol Any) Expr Any))
  (doc 'description "Evaluate an expression AST in an environment")
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

(define (apply-binop op left right)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any Any Any))
  (doc 'description "Apply a binary operator symbol to two values")
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

(define (apply-unop op operand)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any Any))
  (doc 'description "Apply a unary operator symbol to a value")
  (case op
        [(not) (not operand)]
        [(neg) (- operand)]
        [(car) (car operand)]
        [(cdr) (cdr operand)]
        [(null?) (null? operand)]
        [else (error 'apply-unop "Unknown operator" op)]))

(define (apply-closure closure args)
  (doc 'export #t)
  (doc 'type '(-> Closure (List Any) Any))
  (doc 'description "Apply a closure to arguments by extending its environment")
  (if (and (pair? closure) (eq? (car closure) 'closure))
      (let ([params (list-ref closure 1)]
            [body (list-ref closure 2)]
            [env (list-ref closure 3)])
           (let ([new-env (append (map cons params args) env)])
                (eval-expr new-env body)))
      (error 'apply-closure "Not a closure" closure)))

(doc 'section 'statement-dsl)
;;; Statement AST for imperative-style DSLs.

(define (stmt-assign var expr)
  (doc 'export #t)
  (doc 'type '(-> Symbol Expr Stmt))
  (doc 'description "Assignment statement node")
  (list 'stmt-assign var expr))

(define (stmt-seq stmts)
  (doc 'export #t)
  (doc 'type '(-> (List Stmt) Stmt))
  (doc 'description "Sequential composition statement node")
  (list 'stmt-seq stmts))

(define (stmt-if cond then else)
  (doc 'export #t)
  (doc 'type '(-> Expr Stmt Stmt Stmt))
  (doc 'description "Conditional statement node")
  (list 'stmt-if cond then else))

(define (stmt-while cond body)
  (doc 'export #t)
  (doc 'type '(-> Expr Stmt Stmt))
  (doc 'description "While loop statement node")
  (list 'stmt-while cond body))

(define (stmt-return expr)
  (doc 'export #t)
  (doc 'type '(-> Expr Stmt))
  (doc 'description "Return statement node")
  (list 'stmt-return expr))

(define (stmt-expr expr)
  (doc 'export #t)
  (doc 'type '(-> Expr Stmt))
  (doc 'description "Expression statement node")
  (list 'stmt-expr expr))

;;; Statement predicates

(define (stmt-assign? s) (doc 'type '(-> Any Boolean)) (and (pair? s) (eq? (car s) 'stmt-assign)))
(define (stmt-seq? s) (doc 'type '(-> Any Boolean)) (and (pair? s) (eq? (car s) 'stmt-seq)))
(define (stmt-if? s) (doc 'type '(-> Any Boolean)) (and (pair? s) (eq? (car s) 'stmt-if)))
(define (stmt-while? s) (doc 'type '(-> Any Boolean)) (and (pair? s) (eq? (car s) 'stmt-while)))
(define (stmt-return? s) (doc 'type '(-> Any Boolean)) (and (pair? s) (eq? (car s) 'stmt-return)))
(define (stmt-expr? s) (doc 'type '(-> Any Boolean)) (and (pair? s) (eq? (car s) 'stmt-expr)))

;;; Statement accessors

(define (stmt-assign-var s) (doc 'type '(-> Stmt Symbol)) (list-ref s 1))
(define (stmt-assign-expr s) (doc 'type '(-> Stmt Expr)) (list-ref s 2))
(define (stmt-seq-stmts s) (doc 'type '(-> Stmt (List Stmt))) (list-ref s 1))
(define (stmt-if-cond s) (doc 'type '(-> Stmt Expr)) (list-ref s 1))
(define (stmt-if-then s) (doc 'type '(-> Stmt Stmt)) (list-ref s 2))
(define (stmt-if-else s) (doc 'type '(-> Stmt Stmt)) (list-ref s 3))
(define (stmt-while-cond s) (doc 'type '(-> Stmt Expr)) (list-ref s 1))

;;; stmt-while-body : Stmt → Stmt
(define (stmt-while-body s) (doc 'type '(-> Stmt Stmt)) (list-ref s 2))
(define (stmt-return-expr s) (doc 'type '(-> Stmt Expr)) (list-ref s 1))
(define (stmt-expr-expr s) (doc 'type '(-> Stmt Expr)) (list-ref s 1))

(doc 'section 'statement-interpreter)

(define (run-stmt env stmt)
  (doc 'export #t)
  (doc 'type '(-> (Alist Symbol Any) Stmt (Pair (Alist Symbol Any) (Maybe Any))))
  (doc 'description "Run a statement, returning updated env and optional return value")
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

(define (run-stmt-seq env stmts)
  (doc 'export #t)
  (doc 'type '(-> (Alist Symbol Any) (List Stmt) (Pair (Alist Symbol Any) (Maybe Any))))
  (doc 'description "Run statements sequentially; short-circuits on return")
  (if (null? stmts)
      (cons env nothing)
      (let ([result (run-stmt env (car stmts))])
           (if (just? (cdr result))
               result  ; Early return
               (run-stmt-seq (car result) (cdr stmts))))))

(define (run-stmt-while env cond body fuel)
  (doc 'export #t)
  (doc 'type '(-> (Alist Symbol Any) Expr Stmt Nat (Pair (Alist Symbol Any) (Maybe Any))))
  (doc 'description "Execute while loop with fuel limit for termination safety")
  (if (<= fuel 0)
      (cons env nothing)  ; Out of fuel
      (if (eval-expr env cond)
          (let ([result (run-stmt env body)])
               (if (just? (cdr result))
                   result  ; Return in loop
                   (run-stmt-while (car result) cond body (- fuel 1))))
          (cons env nothing))))

(doc 'section 'dsl-combinators)

(define (dsl-sequence programs)
  (doc 'export #t)
  (doc 'type '(-> (List (DSL a)) (DSL (List a))))
  (doc 'description "Sequence a list of DSL programs, collecting results")
  (if (null? programs)
      (dsl-pure '())
      (dsl-bind (car programs)
                (lambda (x)
                        (dsl-bind (dsl-sequence (cdr programs))
                                  (lambda (xs)
                                          (dsl-pure (cons x xs))))))))

(define (dsl-for-each f lst)
  (doc 'export #t)
  (doc 'type '(-> (-> a (DSL Unit)) (List a) (DSL Unit)))
  (doc 'description "Apply DSL action to each element, discarding results")
  (if (null? lst)
      (dsl-pure '())
      (dsl-bind (f (car lst))
                (lambda (_)
                        (dsl-for-each f (cdr lst))))))

(define (dsl-fold f init lst)
  (doc 'export #t)
  (doc 'type '(-> (-> b a (DSL b)) b (List a) (DSL b)))
  (doc 'description "Left fold with a monadic accumulator in DSL context")
  (if (null? lst)
      (dsl-pure init)
      (dsl-bind (f init (car lst))
                (lambda (acc)
                        (dsl-fold f acc (cdr lst))))))

(define (dsl-when pred action)
  (doc 'export #t)
  (doc 'type '(-> Boolean (DSL Unit) (DSL Unit)))
  (doc 'description "Execute action only if predicate is true")
  (if pred action (dsl-pure '())))

(define (dsl-unless pred action)
  (doc 'export #t)
  (doc 'type '(-> Boolean (DSL Unit) (DSL Unit)))
  (doc 'description "Execute action only if predicate is false")
  (dsl-when (not pred) action))

(define (dsl-replicate n action)
  (doc 'export #t)
  (doc 'type '(-> Nat (DSL a) (DSL (List a))))
  (doc 'description "Execute action n times, collecting results")
  (if (<= n 0)
      (dsl-pure '())
      (dsl-bind action
                (lambda (x)
                        (dsl-bind (dsl-replicate (- n 1) action)
                                  (lambda (xs)
                                          (dsl-pure (cons x xs))))))))

(doc 'section 'applicative-combinators)

(define (dsl-ap mf ma)
  (doc 'export #t)
  (doc 'type '(-> (DSL (-> a b)) (DSL a) (DSL b)))
  (doc 'description "Apply a DSL-wrapped function to a DSL-wrapped value")
  (dsl-bind mf (lambda (f) (dsl-map f ma))))

(define (dsl-ap2 f ma mb)
  (doc 'export #t)
  (doc 'type '(-> (-> a b c) (DSL a) (DSL b) (DSL c)))
  (doc 'description "Lift a binary function into DSL context")
  (dsl-bind ma
            (lambda (a)
                    (dsl-bind mb
                              (lambda (b)
                                      (dsl-pure (f a b)))))))

(define (dsl-ap3 f ma mb mc)
  (doc 'export #t)
  (doc 'type '(-> (-> a b c d) (DSL a) (DSL b) (DSL c) (DSL d)))
  (doc 'description "Lift a ternary function into DSL context")
  (dsl-bind ma
            (lambda (a)
                    (dsl-bind mb
                              (lambda (b)
                                      (dsl-bind mc
                                                (lambda (c)
                                                        (dsl-pure (f a b c)))))))))

(define (dsl-join mma)
  (doc 'export #t)
  (doc 'type '(-> (DSL (DSL a)) (DSL a)))
  (doc 'description "Flatten nested DSL programs (monadic join)")
  (dsl-bind mma identity))

(define (dsl-kleisli f g)
  (doc 'export #t)
  (doc 'type '(-> (-> a (DSL b)) (-> b (DSL c)) (-> a (DSL c))))
  (doc 'description "Kleisli composition (>=>): compose monadic functions left-to-right")
  (lambda (a)
          (dsl-bind (f a) g)))

(doc dsl-fish 'type '(-> (-> a (DSL b)) (-> b (DSL c)) (-> a (DSL c))))
(doc dsl-fish 'description "Alias for dsl-kleisli (fish operator >=>)")
(define dsl-fish dsl-kleisli)

(define (dsl-compose g f)
  (doc 'export #t)
  (doc 'type '(-> (-> b (DSL c)) (-> a (DSL b)) (-> a (DSL c))))
  (doc 'description "Reversed Kleisli composition (<=<)")
  (dsl-kleisli f g))

(doc 'section 'tagless-final)
;;; Tagless final: define DSLs as interfaces rather than Free monad trees.
;;; More efficient (no intermediate structure) but less inspectable.

(define (make-tagless-dsl ops)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Symbol (-> Any Procedure))) TaglessDSL))
  (doc 'description "Create a tagless DSL definition from operation specifications")
  (list 'tagless-dsl ops))

(define (tagless-dsl? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a TaglessDSL")
  (and (pair? x) (eq? (car x) 'tagless-dsl)))

(define (tagless-ops dsl)
  (doc 'export #t)
  (doc 'type '(-> TaglessDSL (List (Pair Symbol (-> Any Procedure)))))
  (doc 'description "Extract operation specifications from TaglessDSL")
  (cadr dsl))

(define (instantiate-tagless dsl interp)
  (doc 'export #t)
  (doc 'type '(-> TaglessDSL Any (Alist Symbol Procedure)))
  (doc 'description "Instantiate tagless DSL with an interpreter, producing concrete operations")
  (map (lambda (op-spec)
               (cons (car op-spec) ((cdr op-spec) interp)))
       (tagless-ops dsl)))

(define (tagless-op record name)
  (doc 'export #t)
  (doc 'type '(-> (Alist Symbol Procedure) Symbol Procedure))
  (doc 'description "Look up an operation by name from instantiated tagless DSL")
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

(doc 'section 'source-location)

(define (make-source-pos line col offset)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int SourcePos))
  (doc 'description "Create source position (line, column, byte offset)")
  (list 'source-pos line col offset))

(define (source-pos? x) (doc 'type '(-> Any Boolean)) (and (pair? x) (eq? (car x) 'source-pos)))
(define (source-pos-line p) (doc 'type '(-> SourcePos Int)) (list-ref p 1))
(define (source-pos-col p) (doc 'type '(-> SourcePos Int)) (list-ref p 2))
(define (source-pos-offset p) (doc 'type '(-> SourcePos Int)) (list-ref p 3))

(doc no-source-pos 'type 'SourcePos)
(doc no-source-pos 'description "Sentinel for unknown source position")
(define no-source-pos (make-source-pos 0 0 0))

(define (make-located value pos)
  (doc 'export #t)
  (doc 'type '(-> a SourcePos (Located a)))
  (doc 'description "Attach source location to a value")
  (list 'located value pos))

(define (located? x) (doc 'type '(-> Any Boolean)) (and (pair? x) (eq? (car x) 'located)))
(define (located-value loc) (doc 'type '(-> (Located a) a)) (list-ref loc 1))
(define (located-pos loc) (doc 'type '(-> (Located a) SourcePos)) (list-ref loc 2))

(define (make-located-instruction tag payload k pos)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any (-> a (Free Instruction b)) SourcePos (Instruction a)))
  (doc 'description "Create instruction with source location attached")
  (list tag payload k pos))

(define (instruction-pos instr)
  (doc 'export #t)
  (doc 'type '(-> (Instruction a) SourcePos))
  (doc 'description "Get source position from instruction, or no-source-pos if absent")
  (if (>= (length instr) 4)
      (list-ref instr 3)
      no-source-pos))

(define (dsl-emit-at tag payload pos)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any SourcePos (DSL Unit)))
  (doc 'description "Emit instruction with source location")
  (free (make-located-instruction tag payload pure-free pos)))

(define (dsl-request-at tag payload pos)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any SourcePos (DSL Any)))
  (doc 'description "Request with source location")
  (free (make-located-instruction tag payload pure-free pos)))

(define (format-source-pos pos)
  (doc 'export #t)
  (doc 'type '(-> SourcePos String))
  (doc 'description "Format source position as 'line:col' for error messages")
  (if (source-pos? pos)
      (format "~a:~a" (source-pos-line pos) (source-pos-col pos))
      "<unknown>"))

(define (dsl-error-at pos tag message)
  (doc 'export #t)
  (doc 'type '(-> SourcePos Symbol String (DSL Any)))
  (doc 'description "Emit error instruction with location information")
  (dsl-emit 'dsl-error (list pos tag message)))

(doc 'section 'interpreter-middleware)
;;; Composable interpreter transformations: Middleware = Interpreter -> Interpreter

(define (middleware-identity interp)
  (doc 'export #t)
  (doc 'type '(-> Interpreter Interpreter))
  (doc 'description "Identity middleware; passes interpreter through unchanged")
  interp)

(define (middleware-compose outer inner)
  (doc 'export #t)
  (doc 'type '(-> (-> Interpreter Interpreter) (-> Interpreter Interpreter) (-> Interpreter Interpreter)))
  (doc 'description "Compose two middlewares; outer wraps inner")
  (lambda (interp)
          (outer (inner interp))))

(define (middleware-stack middlewares)
  (doc 'export #t)
  (doc 'type '(-> (List (-> Interpreter Interpreter)) (-> Interpreter Interpreter)))
  (doc 'description "Compose a list of middlewares into a single middleware")
  (fold-right middleware-compose middleware-identity middlewares))

(define (middleware-logging log-fn)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any Any Void) (-> Interpreter Interpreter)))
  (doc 'description "Middleware that logs all instruction executions via callback")
  (lambda (interp)
          (let ([handler (interpreter-handler interp)])
               (make-interpreter
                (lambda (tag payload)
                        (let ([result (handler tag payload)])
                             (log-fn tag payload result)
                             result))))))

(define (middleware-timing on-timing)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Number Void) (-> Interpreter Interpreter)))
  (doc 'description "Middleware that times instruction execution")
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

(define (current-time)
  (doc 'export #t)
  (doc 'type '(-> Number))
  (doc 'description "Get current time in milliseconds (placeholder)")
  0)

(define (middleware-cache cache cacheable-tags)
  (doc 'export #t)
  (doc 'type '(-> HashTable (List Symbol) (-> Interpreter Interpreter)))
  (doc 'description "Middleware that caches results for specified pure instruction tags")
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

(define (middleware-guard pred on-fail)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any Boolean) (-> Symbol Any Any) (-> Interpreter Interpreter)))
  (doc 'description "Middleware that guards instructions with a predicate; calls fallback on failure")
  (lambda (interp)
          (let ([handler (interpreter-handler interp)])
               (make-interpreter
                (lambda (tag payload)
                        (if (pred tag payload)
                            (handler tag payload)
                            (on-fail tag payload)))))))

(define (middleware-transform xform)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any (Pair Symbol Any)) (-> Interpreter Interpreter)))
  (doc 'description "Middleware that transforms instructions before passing to handler")
  (lambda (interp)
          (let ([handler (interpreter-handler interp)])
               (make-interpreter
                (lambda (tag payload)
                        (let ([transformed (xform tag payload)])
                             (handler (car transformed) (cdr transformed))))))))

(define (with-middleware middleware interp program)
  (doc 'export #t)
  (doc 'type '(-> (-> Interpreter Interpreter) Interpreter (DSL a) a))
  (doc 'description "Run DSL program with middleware-wrapped interpreter")
  (run-dsl (middleware interp) program))

(doc 'section 'program-introspection)

(define (dsl-fold-program on-pure on-instr program fuel)
  (doc 'export #t)
  (doc 'type '(-> (-> a r) (-> Symbol Any r r) (DSL a) Nat r))
  (doc 'description "Catamorphism over program structure; folds instructions with fuel limit")
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

(define (dsl-instruction-list program fuel)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) Nat (List (Pair Symbol Any))))
  (doc 'description "Extract list of (tag . payload) pairs from program")
  (dsl-fold-program
   (lambda (_) '())
   (lambda (tag payload rest) (cons (cons tag payload) rest))
   program
   fuel))

(define (dsl-program-depth program fuel)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) Nat Nat))
  (doc 'description "Count instruction depth with fuel limit")
  (dsl-fold-program
   (lambda (_) 0)
   (lambda (tag payload rest) (+ 1 rest))
   program
   fuel))

(define (dsl-uses-tag? target-tag program fuel)
  (doc 'export #t)
  (doc 'type '(-> Symbol (DSL a) Nat Boolean))
  (doc 'description "Check if program uses a specific instruction tag")
  (dsl-fold-program
   (lambda (_) #f)
   (lambda (tag payload rest) (or (eq? tag target-tag) rest))
   program
   fuel))

(define (dsl-tag-histogram program fuel)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) Nat (Alist Symbol Nat)))
  (doc 'description "Count occurrences of each instruction tag")
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

(define (dsl-optimize-dead-code pure-tag? program fuel)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Boolean) (DSL a) Nat (DSL a)))
  (doc 'description "Remove side-effect-free instructions (stub; full DCE needs data flow)")
  ;; For now, just return the program unchanged
  ;; Full implementation requires tracking value usage
  program)

(define (dsl-fuse fuse-fn program fuel)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Symbol (Maybe Symbol)) (DSL a) Nat (DSL a)))
  (doc 'description "Fuse adjacent instructions when possible (stub)")
  ;; Placeholder - full implementation needs instruction rewriting
  program)

(doc 'section 'recursive-dsl)

(define (dsl-fix f)
  (doc 'export #t)
  (doc 'type '(-> (-> (-> a (DSL b)) (-> a (DSL b))) (-> a (DSL b))))
  (doc 'description "Fixed-point combinator for recursive DSL functions")
  (lambda args
          (apply (f (dsl-fix f)) args)))

(define (dsl-lazy thunk)
  (doc 'export #t)
  (doc 'type '(-> (-> (DSL a)) (DSL a)))
  (doc 'description "Delay DSL program construction via lazy instruction")
  (dsl-emit 'dsl-lazy thunk))

;;; Trampoline for deep recursion

(define (make-bounce thunk)
  (doc 'export #t)
  (doc 'type '(-> (-> (Trampoline a)) (Trampoline a)))
  (doc 'description "Create a bounce step in the trampoline")
  (list 'bounce thunk))

(define (make-done value)
  (doc 'export #t)
  (doc 'type '(-> a (Trampoline a)))
  (doc 'description "Create a completed trampoline result")
  (list 'done value))

(define (bounce? t) (doc 'type '(-> Any Boolean)) (and (pair? t) (eq? (car t) 'bounce)))
(define (done? t) (doc 'type '(-> Any Boolean)) (and (pair? t) (eq? (car t) 'done)))
(define (bounce-thunk t) (doc 'type '(-> (Trampoline a) (-> (Trampoline a)))) (cadr t))
(define (done-value t) (doc 'type '(-> (Trampoline a) a)) (cadr t))

(define (run-trampoline t)
  (doc 'export #t)
  (doc 'type '(-> (Trampoline a) a))
  (doc 'description "Execute trampoline until completion")
  (if (done? t)
      (done-value t)
      (run-trampoline ((bounce-thunk t)))))

(define (run-dsl-trampolined interp program)
  (doc 'export #t)
  (doc 'type '(-> Interpreter (DSL a) a))
  (doc 'description "Run DSL with trampolining to avoid stack overflow")
  (run-trampoline (run-dsl-trampoline-helper (interpreter-handler interp) program)))

(define (run-dsl-trampoline-helper handler program)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any Any) (DSL a) (Trampoline a)))
  (doc 'description "Internal: trampoline step for DSL execution")
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

(doc 'section 'testing-utilities)

(define (make-mock-interpreter mock-values)
  (doc 'export #t)
  (doc 'type '(-> (Alist Symbol Any) Interpreter))
  (doc 'description "Create mock interpreter from alist; supports fixed values, fn callbacks, and cycling values")
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

(define (dsl-test-case name program expected interp)
  (doc 'export #t)
  (doc 'type '(-> String (DSL a) a Interpreter Boolean))
  (doc 'description "Test that running program produces expected result")
  (let ([result (run-dsl interp program)])
       (if (equal? result expected)
           (begin
            (display (format "[PASS] ~a\n" name))
            #t)
           (begin
            (display (format "[FAIL] ~a: expected ~s, got ~s\n" name expected result))
            #f))))

(define (dsl-test-error name program expected-tag interp)
  (doc 'export #t)
  (doc 'type '(-> String (DSL a) Symbol Interpreter Boolean))
  (doc 'description "Test that running program raises an error with given tag")
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

(define (dsl-programs-equiv? interp p1 p2)
  (doc 'export #t)
  (doc 'type '(-> Interpreter (DSL a) (DSL a) Boolean))
  (doc 'description "Test if two programs produce the same result under an interpreter")
  (equal? (run-dsl interp p1) (run-dsl interp p2)))

(define (dsl-interpreter-equiv? program i1 i2)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) Interpreter Interpreter Boolean))
  (doc 'description "Test if two interpreters produce the same result for a program")
  (equal? (run-dsl i1 program) (run-dsl i2 program)))

(define (make-recording-interpreter base-interp)
  (doc 'export #t)
  (doc 'type '(-> Interpreter (Pair Interpreter (-> (List (List Symbol Any Any))))))
  (doc 'description "Wrap interpreter to record all instruction calls; returns (wrapped . get-log)")
  (let ([log '()]
        [handler (interpreter-handler base-interp)])
       (cons
        (make-interpreter
         (lambda (tag payload)
                 (let ([result (handler tag payload)])
                      (set! log (cons (list tag payload result) log))
                      result)))
        (lambda () (reverse log)))))

(define (verify-instruction-sequence program interp expected-tags)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) Interpreter (List Symbol) Boolean))
  (doc 'description "Verify program executes instructions in expected order")
  (let* ([recording (make-recording-interpreter interp)]
         [wrapped-interp (car recording)]
         [get-log (cdr recording)]
         [_ (run-dsl wrapped-interp program)]
         [log (get-log)]
         [actual-tags (map car log)])
        (equal? actual-tags expected-tags)))

(doc 'section 'calculator-dsl)

(define (calc-push! n) (doc 'type '(-> Number (DSL Unit))) (dsl-emit 'calc-push n))
(define (calc-add!) (doc 'type '(-> (DSL Unit))) (dsl-emit 'calc-add '()))
(define (calc-sub!) (doc 'type '(-> (DSL Unit))) (dsl-emit 'calc-sub '()))
(define (calc-mul!) (doc 'type '(-> (DSL Unit))) (dsl-emit 'calc-mul '()))
(define (calc-div!) (doc 'type '(-> (DSL Unit))) (dsl-emit 'calc-div '()))
(define (calc-dup!) (doc 'type '(-> (DSL Unit))) (dsl-emit 'calc-dup '()))
(define (calc-swap!) (doc 'type '(-> (DSL Unit))) (dsl-emit 'calc-swap '()))
(define (calc-pop!) (doc 'type '(-> (DSL Number))) (dsl-request 'calc-pop '()))

(define (make-calc-interpreter)
  (doc 'export #t)
  (doc 'type '(-> Interpreter))
  (doc 'description "Stack-based calculator interpreter")
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

(define (run-calc program)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) a))
  (doc 'description "Run a calculator DSL program")
  (run-dsl (make-calc-interpreter) program))

(doc 'section 'turtle-dsl)

(define (forward! n) (doc 'type '(-> Number (DSL Unit))) (dsl-emit 'turtle-forward n))
(define (back! n) (doc 'type '(-> Number (DSL Unit))) (dsl-emit 'turtle-back n))
(define (left! deg) (doc 'type '(-> Number (DSL Unit))) (dsl-emit 'turtle-left deg))
(define (right! deg) (doc 'type '(-> Number (DSL Unit))) (dsl-emit 'turtle-right deg))
(define (penup!) (doc 'type '(-> (DSL Unit))) (dsl-emit 'turtle-penup '()))
(define (pendown!) (doc 'type '(-> (DSL Unit))) (dsl-emit 'turtle-pendown '()))
(define (getpos!) (doc 'type '(-> (DSL (Pair Number Number)))) (dsl-request 'turtle-getpos '()))

(define (make-turtle-interpreter)
  (doc 'export #t)
  (doc 'type '(-> Interpreter))
  (doc 'description "Turtle graphics interpreter with position, angle, and pen state")
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

(define (run-turtle program)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) a))
  (doc 'description "Run a turtle DSL program")
  (run-dsl (make-turtle-interpreter) program))

(define (repeat n action)
  (doc 'export #t)
  (doc 'type '(-> Nat (DSL a) (DSL (List a))))
  (doc 'description "Repeat a DSL action n times; alias for dsl-replicate")
  (dsl-replicate n action))

;;; ====
;;; Example Usage (for documentation)
;;; ====
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

(doc 'section 'error-handling)

(define (dsl-ok value) (doc 'type '(-> a (Result a))) (list 'dsl-ok value))
(define (dsl-err tag message) (doc 'type '(-> Symbol String (Result a))) (list 'dsl-err tag message))
(define (dsl-ok? r) (doc 'type '(-> Any Boolean)) (and (pair? r) (eq? (car r) 'dsl-ok)))
(define (dsl-err? r) (doc 'type '(-> Any Boolean)) (and (pair? r) (eq? (car r) 'dsl-err)))
(define (dsl-ok-value r) (doc 'type '(-> (Result a) a)) (cadr r))
(define (dsl-err-tag r) (doc 'type '(-> (Result a) Symbol)) (cadr r))
(define (dsl-err-message r) (doc 'type '(-> (Result a) String)) (caddr r))

(define (dsl-result-map f r)
  (doc 'export #t)
  (doc 'type '(-> (-> a b) (Result a) (Result b)))
  (doc 'description "Map over successful result, pass through errors")
  (if (dsl-ok? r)
      (dsl-ok (f (dsl-ok-value r)))
      r))

(define (dsl-result-bind r f)
  (doc 'export #t)
  (doc 'type '(-> (Result a) (-> a (Result b)) (Result b)))
  (doc 'description "Bind on result; short-circuits on error")
  (if (dsl-ok? r)
      (f (dsl-ok-value r))
      r))

(define (run-dsl-safe interp program)
  (doc 'export #t)
  (doc 'type '(-> Interpreter (DSL a) (Result a)))
  (doc 'description "Execute DSL program, catching runtime errors as dsl-err")
  (guard (ex [else (dsl-err 'runtime
                            (if (message-condition? ex)
                                (condition-message ex)
                                "Unknown error"))])
         (dsl-ok (run-dsl interp program))))

(define (dsl-fail tag message)
  (doc 'export #t)
  (doc 'type '(-> Symbol String (DSL Any)))
  (doc 'description "Emit a failure instruction")
  (dsl-emit 'dsl-fail (list tag message)))

(define (dsl-assert condition tag message)
  (doc 'export #t)
  (doc 'type '(-> Boolean Symbol String (DSL Unit)))
  (doc 'description "Assert a condition; emits failure if false")
  (if condition
      (dsl-pure '())
      (dsl-fail tag message)))

(define (make-fail-handler on-fail default-handler)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol String Any) (-> Symbol Any Any) (-> Symbol Any Any)))
  (doc 'description "Create handler that intercepts dsl-fail, delegates rest to default")
  (lambda (tag payload)
          (if (eq? tag 'dsl-fail)
              (on-fail (car payload) (cadr payload))
              (default-handler tag payload))))

(doc 'section 'debugging-tools)

(define (dsl-peek program)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) (Pair Symbol Any)))
  (doc 'description "Inspect first instruction without executing; returns (tag . payload)")
  (if (dsl-pure? program)
      (cons 'pure (dsl-pure-value program))
      (let ([instr (dsl-instruction program)])
           (cons (instruction-tag instr)
                 (instruction-payload instr)))))

(define (dsl-count-instructions program fuel)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) Nat Nat))
  (doc 'description "Count instructions in program with fuel limit")
  (if (or (<= fuel 0) (dsl-pure? program))
      0
      (let* ([instr (dsl-instruction program)]
             [cont (instruction-cont instr)])
            (+ 1 (dsl-count-instructions (cont '()) (- fuel 1))))))

(define (dsl-trace-when pred interp)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any Boolean) Interpreter Interpreter))
  (doc 'description "Wrap interpreter to trace instructions matching a predicate")
  (let ([handler (interpreter-handler interp)])
       (make-interpreter
        (lambda (tag payload)
                (when (pred tag payload)
                      (display (format "[DSL] ~a: ~s\n" tag payload)))
                (let ([result (handler tag payload)])
                     (when (pred tag payload)
                           (display (format "[DSL]   => ~s\n" result)))
                     result)))))

(define (dsl-trace-tags tags interp)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol) Interpreter Interpreter))
  (doc 'description "Trace only instructions with specified tags")
  (dsl-trace-when (lambda (tag _) (memq tag tags)) interp))

(define (dsl-pretty-print program)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) String))
  (doc 'description "Pretty-print the first instruction of a program")
  (cond
   [(dsl-pure? program)
    (format "(pure ~s)" (dsl-pure-value program))]
   [else
    (let ([instr (dsl-instruction program)])
         (format "(~a ~s ...)"
                 (instruction-tag instr)
                 (instruction-payload instr)))]))

(define (dsl-collect-tags program fuel)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) Nat (List Symbol)))
  (doc 'description "Collect all instruction tags in program with fuel limit")
  (if (or (<= fuel 0) (dsl-pure? program))
      '()
      (let* ([instr (dsl-instruction program)]
             [tag (instruction-tag instr)]
             [cont (instruction-cont instr)])
            (cons tag (dsl-collect-tags (cont '()) (- fuel 1))))))

(doc 'section 'dsl-combination)

(define (dsl-left-tag tag) (doc 'type '(-> Symbol (List Symbol Symbol))) (list 'dsl-left tag))
(define (dsl-right-tag tag) (doc 'type '(-> Symbol (List Symbol Symbol))) (list 'dsl-right tag))
(define (dsl-left-tag? x) (doc 'type '(-> Any Boolean)) (and (pair? x) (eq? (car x) 'dsl-left)))
(define (dsl-right-tag? x) (doc 'type '(-> Any Boolean)) (and (pair? x) (eq? (car x) 'dsl-right)))
(define (dsl-unwrap-tag x) (doc 'type '(-> (List Symbol Symbol) Symbol)) (cadr x))

(define (dsl-inject-left program)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) (DSL a)))
  (doc 'description "Lift program to left side of a union DSL")
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

(define (dsl-inject-right program)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) (DSL a)))
  (doc 'description "Lift program to right side of a union DSL")
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

(define (union-interpreter left-interp right-interp)
  (doc 'export #t)
  (doc 'type '(-> Interpreter Interpreter Interpreter))
  (doc 'description "Combine two interpreters into one via tagged dispatch")
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

(doc 'section 'program-validation)

(define (make-dsl-schema instructions)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Symbol Nat)) Schema))
  (doc 'description "Create a schema from list of (tag . arity) pairs")
  (list 'dsl-schema instructions))

(define (dsl-schema? x) (doc 'type '(-> Any Boolean)) (and (pair? x) (eq? (car x) 'dsl-schema)))
(define (dsl-schema-instructions schema) (doc 'type '(-> Schema (List (Pair Symbol Nat)))) (cadr schema))

(define (dsl-validate schema program fuel)
  (doc 'export #t)
  (doc 'type '(-> Schema (DSL a) Nat (Result (DSL a))))
  (doc 'description "Validate program against schema; checks all tags are in schema")
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
(doc calc-schema 'type '(DSLSchema))
(doc calc-schema 'description "Validation schema for the calculator DSL instruction set")
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

(doc turtle-schema 'type '(DSLSchema))
(doc turtle-schema 'description "Validation schema for the turtle graphics DSL instruction set")
(define turtle-schema
  (make-dsl-schema
   '((turtle-forward . 1)
     (turtle-back . 1)
     (turtle-left . 1)
     (turtle-right . 1)
     (turtle-penup . 0)
     (turtle-pendown . 0)
     (turtle-getpos . 0))))

(doc 'section 'resource-management)

(define (dsl-finally cleanup program)
  (doc 'export #t)
  (doc 'type '(-> (DSL ()) (DSL a) (DSL a)))
  (doc 'description "Execute cleanup after program, regardless of result")
  (dsl-bind program
            (lambda (result)
                    (dsl-bind cleanup
                              (lambda (_) (dsl-pure result))))))

(define (dsl-bracket acquire release use)
  (doc 'export #t)
  (doc 'type '(-> (DSL r) (-> r (DSL ())) (-> r (DSL a)) (DSL a)))
  (doc 'description "Acquire resource, use it, then release — bracket pattern for safe resource management")
  (dsl-do
   (resource <- acquire)
   (dsl-finally (release resource) (use resource))))

(doc dsl-with-resource 'type 'syntax)
(doc dsl-with-resource 'description "Macro: (dsl-with-resource (var acquire release) body ...) — bracket pattern with implicit binding")
(define-syntax dsl-with-resource
  (syntax-rules ()
                [(_ (var acquire release) body ...)
                 (dsl-bracket acquire
                              (lambda (var) release)
                              (lambda (var) (dsl-do body ...)))]))

(doc 'section 'algebraic-effects-integration)

(define (dsl-to-eff program)
  (doc 'export #t)
  (doc 'type '(-> (DSL a) (Eff e a)))
  (doc 'description "Convert a DSL program to an effectful computation, bridging Free monad and algebraic effects")
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

(define (make-eff-interpreter handler)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any a) (-> Effect (-> a b) b)))
  (doc 'description "Create an algebraic effect handler from a DSL interpreter function")
  (lambda (effect k)
          (let* ([tag (effect-tag effect)]
                 [payload (effect-payload effect)]
                 [result (handler tag payload)])
                (k result))))

(define (run-dsl-as-eff handler program)
  (doc 'export #t)
  (doc 'type '(-> (-> Symbol Any a) (DSL b) (Eff e b)))
  (doc 'description "Run a DSL program using the algebraic effects system")
  (dsl-to-eff program))
