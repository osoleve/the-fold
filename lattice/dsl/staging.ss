;;; @module staging
;;; @requires prelude combinators
(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'combinators)

(doc 'module 'staging)
(doc 'description "Multi-Stage Programming - Compile-time code generation with type safety")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Staging Annotations:
  <expr>   ; Quote: code to be generated (bracket/lift)
  ~expr    ; Splice: insert computed code (escape/run)
  !expr    ; Run: execute at current stage (run/exec)")

(doc 'note "Example - Compile-time power function:
  (define (power n)
    (if (= n 0)
        <1>
        <(* ~(power (- n 1)) x)>))

  (power 3) ; Generates: <(* (* (* 1 x) x) x)>")

(doc 'note "Builds on quasiquotation for the underlying expansion")

(doc 'section 'staging-annotations)
(doc 'note "We use explicit forms rather than reader syntax for Core:
  (stage-quote expr)     ; <expr>
  (stage-splice expr)    ; ~expr
  (stage-run expr)       ; !expr")

(doc 'section 'code-type-representation)
(doc 'note "Code values wrap expressions with their stage level.
(Code n a) represents code at stage n with result type a")

(doc make-code 'type (-> Int Sexp Code))
(doc 'description "Create a code value at a given stage")
(define (make-code stage expr)
  (doc 'export #t)
  (list 'Code stage expr))

(define (code? x)
  (doc 'export #t)
  (doc 'type (-> Any Boolean))
  (and (pair? x) (eq? (car x) 'Code)))

(define (code-stage c)
  (doc 'export #t)
  (doc 'type (-> Code Int))
  (if (code? c) (cadr c) 0))

(define (code-expr c)
  (doc 'export #t)
  (doc 'type (-> Code Sexp))
  (if (code? c) (caddr c) c))

(doc 'section 'staging-detection)

(define (stage-quote? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if expression is a staging quote: (stage-quote expr) or <expr>")
  (and (pair? x)
       (or (eq? (car x) 'stage-quote)
           (eq? (car x) 'bracket)
           (eq? (car x) '<))))

;;; stage-splice? : α → Boolean
;;; Check if expression is a staging splice: (stage-splice expr) or ~expr
(define (stage-splice? x)
  (and (pair? x)
       (or (eq? (car x) 'stage-splice)
           (eq? (car x) 'escape)
           (eq? (car x) '~))))

;;; stage-run? : α → Boolean
;;; Check if expression is a staging run: (stage-run expr) or !expr
(define (stage-run? x)
  (and (pair? x)
       (or (eq? (car x) 'stage-run)
           (eq? (car x) 'run)
           (eq? (car x) '!))))

;;; ====
;;; Stage Environment
;;; ====
;;;
;;; The stage environment tracks:
;;;   - Current stage level (0 = runtime, 1+ = compile-time)
;;;   - Cross-stage persistent bindings
;;;   - Generated code fragments

;;; make-stage-env : Int → StageEnv
(define (make-stage-env level)
  (list 'stage-env level '() '()))

;;; stage-env? : α → Boolean
(define (stage-env? x)
  (and (pair? x) (eq? (car x) 'stage-env)))

;;; stage-env-level : StageEnv → Int
(define (stage-env-level env)
  (cadr env))

;;; stage-env-bindings : StageEnv → (List (Symbol . Value))
(define (stage-env-bindings env)
  (caddr env))

;;; stage-env-fragments : StageEnv → (List Code)
(define (stage-env-fragments env)
  (cadddr env))

;;; stage-env-incr : StageEnv → StageEnv
;;; Increment stage level (entering a bracket).
(define (stage-env-incr env)
  (make-stage-env (+ (stage-env-level env) 1)))

;;; stage-env-decr : StageEnv → StageEnv
;;; Decrement stage level (entering an escape).
(define (stage-env-decr env)
  (make-stage-env (max 0 (- (stage-env-level env) 1))))

;;; stage-env-bind : StageEnv × Symbol × Value → StageEnv
;;; Add a cross-stage persistent binding.
(define (stage-env-bind env sym val)
  (list 'stage-env
        (stage-env-level env)
        (cons (cons sym val) (stage-env-bindings env))
        (stage-env-fragments env)))

;;; stage-env-lookup : StageEnv × Symbol → (Option Value)
(define (stage-env-lookup env sym)
  (let ([entry (assq sym (stage-env-bindings env))])
       (if entry (just (cdr entry)) nothing)))

;;; stage-env-add-fragment : StageEnv × Code → StageEnv
;;; Add a generated code fragment.
(define (stage-env-add-fragment env fragment)
  (list 'stage-env
        (stage-env-level env)
        (stage-env-bindings env)
        (cons fragment (stage-env-fragments env))))

;;; ====
;;; Staging Expansion
;;; ====
;;;
;;; The staging expander transforms staged code into code generators.
;;; Similar to quasiquote but with stage tracking.

;;; stage-expand : Sexp × Int → Sexp
;;; Expand staged expression at given level.
(define (stage-expand expr level)
  (doc 'export #t)
  (cond
   ;; Stage quote: <expr> → (make-code (+ level 1) (stage-expand expr (+ level 1)))
   [(stage-quote? expr)
    (let ([inner (cadr expr)])
         (if (= level 0)
             ;; At runtime, create code value
             `(make-code 1 ',(stage-expand inner 1))
             ;; At compile-time, just quote
             `(quote ,(stage-expand inner (+ level 1)))))]
   
   ;; Stage splice: ~expr → run expr and splice result
   [(stage-splice? expr)
    (let ([inner (cadr expr)])
         (if (= level 1)
             ;; Exiting quoted context: evaluate and splice
             inner
             ;; Still in quoted context: descend
             `(stage-splice ,(stage-expand inner (- level 1)))))]
   
   ;; Stage run: !expr → execute immediately
   [(stage-run? expr)
    (let ([inner (cadr expr)])
         `(stage-exec ,(stage-expand inner level)))]
   
   ;; Lambda: preserve structure
   [(and (pair? expr) (eq? (car expr) 'lambda))
    `(lambda ,(cadr expr)
      ,@(map (lambda (e) (stage-expand e level)) (cddr expr)))]
   
   ;; Let binding
   [(and (pair? expr) (eq? (car expr) 'let))
    (let ([bindings (cadr expr)]
          [body (cddr expr)])
         `(let ,(map (lambda (b)
                             (list (car b) (stage-expand (cadr b) level)))
                     bindings)
           ,@(map (lambda (e) (stage-expand e level)) body)))]
   
   ;; If expression
   [(and (pair? expr) (eq? (car expr) 'if))
    `(if ,(stage-expand (cadr expr) level)
      ,(stage-expand (caddr expr) level)
      ,@(if (null? (cdddr expr))
            '()
            (list (stage-expand (cadddr expr) level))))]
   
   ;; Application: expand all parts
   [(pair? expr)
    (map (lambda (e) (stage-expand e level)) expr)]
   
   ;; Atoms: quote at higher stages
   [(or (symbol? expr) (null? expr))
    (if (> level 0)
        `(quote ,expr)
        expr)]
   
   ;; Self-quoting atoms
   [else expr]))

;;; ====
;;; Code Composition
;;; ====
;;;
;;; Operations for combining code values.

;;; code-combine : Code × Code × (α × β → γ) → Code
;;; Combine two code values with a binary operation.
(define (code-combine c1 c2 op-name)
  (doc 'export #t)
  (let ([s1 (code-stage c1)]
        [s2 (code-stage c2)]
        [e1 (code-expr c1)]
        [e2 (code-expr c2)])
       (if (= s1 s2)
           (make-code s1 `(,op-name ,e1 ,e2))
           (error 'code-combine "stage mismatch" s1 s2))))

;;; code-lift : Value → Code
;;; Lift a runtime value to stage-1 code.
(define (code-lift val)
  (doc 'export #t)
  (make-code 1 `(quote ,val)))

;;; code-unlift : Code → Value
;;; Extract value from stage-0 code (must be a literal).
(define (code-unlift c)
  (doc 'export #t)
  (if (and (code? c) (= (code-stage c) 0))
      (code-expr c)
      (error 'code-unlift "not stage-0 code" c)))

;;; ====
;;; Code Type Operations
;;; ====

;;; code-type : Code → Type
;;; Get the type of a code value.
(define (code-type c)
  `(Code ,(code-stage c)))

;;; code-well-staged? : Code × Int → Boolean
;;; Check if code is at or below the expected stage.
(define (code-well-staged? c expected)
  (doc 'export #t)
  (<= (code-stage c) expected))

;;; ====
;;; Staging Primitives
;;; ====
;;;
;;; Built-in operations for staged code.

;;; stage-add : Code × Code → Code
;;; Staged addition.
(define (stage-add c1 c2)
  (doc 'export #t)
  (code-combine c1 c2 '+))

;;; stage-mul : Code × Code → Code
;;; Staged multiplication.
(define (stage-mul c1 c2)
  (doc 'export #t)
  (code-combine c1 c2 '*))

;;; stage-sub : Code × Code → Code
;;; Staged subtraction.
(define (stage-sub c1 c2)
  (code-combine c1 c2 '-))

;;; stage-div : Code × Code → Code
;;; Staged division.
(define (stage-div c1 c2)
  (code-combine c1 c2 '/))

;;; stage-if : Code × Code × Code → Code
;;; Staged conditional.
(define (stage-if cond-c then-c else-c)
  (doc 'export #t)
  (let ([s (code-stage cond-c)])
       (if (and (= s (code-stage then-c))
                (= s (code-stage else-c)))
           (make-code s `(if ,(code-expr cond-c)
                          ,(code-expr then-c)
                          ,(code-expr else-c)))
           (error 'stage-if "stage mismatch"))))

;;; stage-let : Symbol × Code × Code → Code
;;; Staged let binding.
(define (stage-let var val-c body-c)
  (doc 'export #t)
  (let ([s (code-stage val-c)])
       (if (= s (code-stage body-c))
           (make-code s `(let ([,var ,(code-expr val-c)])
                          ,(code-expr body-c)))
           (error 'stage-let "stage mismatch"))))

;;; stage-lambda : (List Symbol) × Code → Code
;;; Staged lambda.
(define (stage-lambda params body-c)
  (doc 'export #t)
  (make-code (code-stage body-c)
             `(lambda ,params ,(code-expr body-c))))

;;; stage-app : Code × (List Code) → Code
;;; Staged function application.
(define (stage-app func-c args-c)
  (doc 'export #t)
  (let ([s (code-stage func-c)])
       (if (andmap (lambda (c) (= s (code-stage c))) args-c)
           (make-code s `(,(code-expr func-c) ,@(map code-expr args-c)))
           (error 'stage-app "stage mismatch"))))

;;; ====
;;; Cross-Stage Persistence
;;; ====
;;;
;;; CSP allows values computed at one stage to be available at later stages.

;;; make-csp : Symbol × Value → CSPBinding
;;; Create a cross-stage persistent binding.
(define (make-csp name value)
  (list 'csp name value))

;;; csp? : α → Boolean
(define (csp? x)
  (and (pair? x) (eq? (car x) 'csp)))

;;; csp-name : CSPBinding → Symbol
(define (csp-name csp)
  (cadr csp))

;;; csp-value : CSPBinding → Value
(define (csp-value csp)
  (caddr csp))

;;; lift-csp : CSPBinding → Code
;;; Lift a CSP binding to code.
(define (lift-csp binding)
  (make-code 1 `(csp-ref ',(csp-name binding))))

;;; ====
;;; Power Example (from specification)
;;; ====
;;;
;;; This demonstrates compile-time specialization.

;;; power-staged : Int → Code
;;; Generate code for x^n.
(define (power-staged n)
  (doc 'export #t)
  (if (= n 0)
      (make-code 1 '1)
      (make-code 1 `(* ,(code-expr (power-staged (- n 1))) x))))

;;; power-specialized : Int → (Int → Int)
;;; Return a specialized power function.
(define (power-specialized n)
  (doc 'export #t)
  (let ([code (power-staged n)])
       (list 'specialized-power n (code-expr code))))

;;; ====
;;; Code Pretty Printing
;;; ====

;;; code->string : Code → String
;;; Convert code to a readable string.
(define (code->string c)
  (if (code? c)
      (format "<~a:~a>" (code-stage c) (code-expr c))
      (format "~a" c)))

;;; ====
;;; Staging Compilation
;;; ====
;;;
;;; Compile staged expressions to executable code.

;;; compile-staged : Sexp → Sexp
;;; Compile a staged expression.
(define (compile-staged expr)
  (doc 'export #t)
  (stage-expand expr 0))

;;; run-staged : Code → Value
;;; Run stage-0 code.
(define (run-staged c)
  (doc 'export #t)
  (if (= (code-stage c) 0)
      (code-expr c)
      (error 'run-staged "cannot run higher-stage code" c)))

;;; ====
;;; Staged Pattern Matching
;;; ====
;;;
;;; Pattern matching that works across stages.

;;; stage-match-clause? : Sexp → Boolean
(define (stage-match-clause? x)
  (and (pair? x)
       (>= (length x) 2)))

;;; stage-match : Code × (List Clause) → Code
;;; Staged pattern match.
(define (stage-match scrut-c clauses)
  (doc 'export #t)
  (let ([s (code-stage scrut-c)])
       (make-code s
                  `(match ,(code-expr scrut-c)
                    ,@clauses))))

;;; ====
;;; Staged Recursion
;;; ====
;;;
;;; Support for staged recursive definitions.

;;; stage-fix : (Code → Code) → Code
;;; Staged fixed-point combinator.
(define (stage-fix f)
  (doc 'export #t)
  (doc 'note "Uses reserved name %__stage-fix-rec__ in generated letrec.
User staged expressions must not bind this symbol. Deterministic naming
is required for content-addressable code generation.")
  (let* ([rec-name '%__stage-fix-rec__]
         [dummy (make-code 1 rec-name)]
         [body (f dummy)])
        (make-code (code-stage body)
                   `(letrec ([,rec-name ,(code-expr body)])
                     ,rec-name))))

;;; ====
;;; Type Checking Support
;;; ====
;;;
;;; Types for staged expressions.

;;; code-type? : Type → Boolean
;;; Check if a type is a Code type.
(define (code-type-syntax? t)
  (and (pair? t)
       (eq? (car t) 'Code)))

;;; code-type-stage : Type → Int
;;; Extract stage from a Code type.
(define (code-type-stage t)
  (if (code-type-syntax? t)
      (cadr t)
      0))

;;; code-type-inner : Type → Type
;;; Extract inner type from a Code type.
(define (code-type-inner t)
  (if (and (code-type-syntax? t) (>= (length t) 3))
      (caddr t)
      'Any))

;;; infer-staged-type : Sexp × TypeEnv × Int → Type
;;; Infer the type of a staged expression.
(define (infer-staged-type expr env level)
  (doc 'export #t)
  (cond
   [(stage-quote? expr)
    `(Code ,(+ level 1) ,(infer-staged-type (cadr expr) env (+ level 1)))]
   
   [(stage-splice? expr)
    (let ([inner-type (infer-staged-type (cadr expr) env (- level 1))])
         (if (code-type-syntax? inner-type)
             (code-type-inner inner-type)
             inner-type))]
   
   [(number? expr) 'Int]
   [(string? expr) 'String]
   [(boolean? expr) 'Bool]
   
   [else 'Unknown]))

;;; ====
;;; Staging Transformations
;;; ====
;;;
;;; Transformations for optimization and analysis.

;;; inline-staged : Sexp → Sexp
;;; Inline staged computations where possible.
(define (inline-staged expr)
  (doc 'export #t)
  (cond
   [(stage-quote? expr)
    (let ([inner (inline-staged (cadr expr))])
         ;; If inner is just a quote, flatten
         (if (and (pair? inner) (eq? (car inner) 'quote))
             inner
             `(stage-quote ,inner)))]
   
   [(stage-splice? expr)
    (let ([inner (inline-staged (cadr expr))])
         `(stage-splice ,inner))]
   
   [(pair? expr)
    (map inline-staged expr)]
   
   [else expr]))

;;; beta-reduce-staged : Sexp → Sexp
;;; Beta reduce staged lambdas where possible.
(define (beta-reduce-staged expr)
  (cond
   ;; ((lambda (x) body) arg) → body[arg/x]
   [(and (pair? expr)
         (pair? (car expr))
         (eq? (caar expr) 'lambda)
         (= (length expr) 2))
    (let* ([lam (car expr)]
           [params (cadr lam)]
           [body (caddr lam)]
           [arg (beta-reduce-staged (cadr expr))])
          (if (and (list? params) (= (length params) 1))
              (subst-expr body (car params) arg)
              expr))]
   
   [(pair? expr)
    (map beta-reduce-staged expr)]
   
   [else expr]))

;;; subst-expr : Sexp × Symbol × Sexp → Sexp
;;; Substitute var with replacement in expr.
(define (subst-expr expr var replacement)
  (cond
   [(eq? expr var) replacement]
   [(and (pair? expr) (eq? (car expr) 'lambda))
    ;; Don't substitute in shadowed binding
    (if (memq var (cadr expr))
        expr
        `(lambda ,(cadr expr)
          ,@(map (lambda (e) (subst-expr e var replacement)) (cddr expr))))]
   [(and (pair? expr) (eq? (car expr) 'let))
    ;; Don't substitute in shadowed bindings
    (let* ([bindings (cadr expr)]
           [bound-vars (map car bindings)])
          (if (memq var bound-vars)
              expr
              `(let ,(map (lambda (b)
                                  (list (car b) (subst-expr (cadr b) var replacement)))
                          bindings)
                ,@(map (lambda (e) (subst-expr e var replacement)) (cddr expr)))))]
   [(pair? expr)
    (map (lambda (e) (subst-expr e var replacement)) expr)]
   [else expr]))

;;; ====
;;; Exports
;;; ====

;;; Main entry points:
;;;   - make-code, code?, code-stage, code-expr
;;;   - stage-expand, compile-staged
;;;   - stage-quote?, stage-splice?, stage-run?
;;;   - code-combine, code-lift, code-unlift
;;;   - power-staged (example)
;;;   - stage-add, stage-mul, stage-sub, stage-div
;;;   - stage-if, stage-let, stage-lambda, stage-app
