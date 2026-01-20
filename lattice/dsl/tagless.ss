(load "core/base/prelude.ss")

(doc 'module 'tagless)
(doc 'description "Tagless Final DSL Pattern - Alternative to Free monads that avoids intermediate data structures")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Key Concepts:
  - DSL defined as an \"algebra\" (type class interface)
  - Programs are functions parameterized by algebra dictionary
  - Interpreters are algebra instances (different dictionaries)
  - Extensibility via dictionary composition")

(doc 'note "Advantages over Free:
  - No intermediate AST construction
  - Direct interpretation (better performance)
  - Extensible without modifying original definition
  - Modular interpreters via dictionary composition")

(doc 'note "Disadvantages:
  - Programs cannot be inspected/transformed as data
  - Requires discipline to thread dictionaries")

(doc 'section 'algebra-definition)
(doc 'note "An algebra is a dictionary of operations")

(doc 'note "define-algebra creates:
  - A constructor (make-<name>-dict ...)
  - Accessors for each operation (<name>-<op> dict)
  - A predicate (<name>-dict? x)")

(define-syntax define-algebra
  (syntax-rules ()
                [(_ name (op ...))
                 (begin
                  ;; Constructor
                  (define (make-algebra-constructor op ...)
                    (vector 'name op ...))
                  ;; Predicate
                  (define (name-dict? x)
                    (and (vector? x)
                         (> (vector-length x) 0)
                         (eq? (vector-ref x 0) 'name)))
                  ;; Define the constructor with proper name
                  (define make-name-dict make-algebra-constructor)
                  ;; Accessors will be defined separately
                  )]))

;;; For now, we'll use a simpler record-based approach
;;; that doesn't require macros for each algebra.

(doc 'section 'generic-dictionary-operations)

(define (make-dict tag ops)
  (doc 'export #t)
  (doc 'type (-> Symbol (List (Pair Symbol Procedure)) Dict))
  (doc 'description "Create a dictionary with named operations")
  (cons tag ops))

;;; dict-tag : Dict → Symbol
(define (dict-tag d) (car d))

;;; dict-ops : Dict → (List (Symbol . Procedure))
(define (dict-ops d) (cdr d))

;;; dict-ref : Dict × Symbol → Procedure
;;; Get an operation from a dictionary.
(define (dict-ref d name)
  (doc 'export #t)
  (let ([pair (assq name (dict-ops d))])
       (if pair
           (cdr pair)
           (error 'dict-ref "Operation not found" name (dict-tag d)))))

;;; dict-has? : Dict × Symbol → Boolean
(define (dict-has? d name)
  (and (assq name (dict-ops d)) #t))

;;; dict-extend : Dict × (List (Symbol . Procedure)) → Dict
;;; Extend a dictionary with additional operations.
(define (dict-extend d new-ops)
  (doc 'export #t)
  (make-dict (dict-tag d)
             (append new-ops (dict-ops d))))

;;; dict-merge : Dict × Dict → Dict
;;; Merge two dictionaries (second overrides first).
(define (dict-merge d1 d2)
  (doc 'export #t)
  (make-dict (list (dict-tag d1) (dict-tag d2))
             (append (dict-ops d2) (dict-ops d1))))

;;; ====
;;; Expression Algebra (Classic Example)
;;; ====
;;;
;;; The classic tagless final example: arithmetic expressions.
;;;
;;; Operations:
;;;   lit : Int -> repr Int
;;;   add : repr Int -> repr Int -> repr Int
;;;   neg : repr Int -> repr Int

;;; make-expr-dict : (Int → α) × (α × α → α) × (α → α) → Dict
(define (make-expr-dict lit add neg)
  (doc 'export #t)
  (make-dict 'expr
             `((lit . ,lit)
               (add . ,add)
               (neg . ,neg))))

;;; expr-lit : Dict × Int → α
(define (expr-lit d n) ((dict-ref d 'lit) n))
(doc 'export #t)
;;; expr-add : Dict × α × α → α
(define (expr-add d x y) ((dict-ref d 'add) x y))
(doc 'export #t)
;;; expr-neg : Dict × α → α
(define (expr-neg d x) ((dict-ref d 'neg) x))
(doc 'export #t)

;;; ====
;;; Expression Interpreters
;;; ====

;;; eval-expr-dict : ExprDict
;;; Evaluator - returns the computed value.
(doc eval-expr-dict 'export #t)
(define eval-expr-dict
  (make-expr-dict
   (lambda (n) n)                    ; lit: just the number
   (lambda (x y) (+ x y))            ; add: sum
   (lambda (x) (- x))))              ; neg: negate

;;; pretty-expr-dict : ExprDict
;;; Pretty printer - returns a string representation.
(define pretty-expr-dict
  (make-expr-dict
   (lambda (n) (number->string n))
   (lambda (x y) (string-append "(" x " + " y ")"))
   (lambda (x) (string-append "(-" x ")"))))

;;; count-expr-dict : ExprDict
;;; Counts the number of operations.
(define count-expr-dict
  (make-expr-dict
   (lambda (n) 0)                    ; lit: 0 operations
   (lambda (x y) (+ 1 x y))          ; add: 1 + subtree counts
   (lambda (x) (+ 1 x))))            ; neg: 1 + subtree count

;;; depth-expr-dict : ExprDict
;;; Computes expression depth.
(define depth-expr-dict
  (make-expr-dict
   (lambda (n) 1)                    ; lit: depth 1
   (lambda (x y) (+ 1 (max x y)))    ; add: 1 + max of children
   (lambda (x) (+ 1 x))))            ; neg: 1 + child depth

;;; ====
;;; Example Programs
;;; ====

;;; A program is a function that takes a dictionary and returns a result.
;;; The program doesn't know what the operations do - it just uses them.

;;; example-expr-1 : ExprDict -> a
;;; Represents: 8 + (-(1 + 2)) = 8 + (-3) = 5
(define (example-expr-1 d)
  (expr-add d
            (expr-lit d 8)
            (expr-neg d
                      (expr-add d
                                (expr-lit d 1)
                                (expr-lit d 2)))))

;;; example-expr-2 : ExprDict -> a
;;; Represents: -(-(5)) = 5
(define (example-expr-2 d)
  (expr-neg d (expr-neg d (expr-lit d 5))))

;;; Running programs with different interpreters:
;;;   (example-expr-1 eval-expr-dict)   => 5
;;;   (example-expr-1 pretty-expr-dict) => "(8 + (-(1 + 2)))"
;;;   (example-expr-1 count-expr-dict)  => 4
;;;   (example-expr-1 depth-expr-dict)  => 4

;;; ====
;;; Extensibility: Adding Multiplication
;;; ====
;;;
;;; The key advantage of tagless final: we can add new operations
;;; without modifying the original algebra definition.

;;; make-mul-dict : (α × α → α) → Dict
;;; Extension for multiplication.
(define (make-mul-dict mul)
  (make-dict 'mul `((mul . ,mul))))

;;; expr-mul : Dict × α × α → α
(define (expr-mul d x y) ((dict-ref d 'mul) x y))

;;; Extended interpreters
(define eval-mul-dict
  (dict-merge eval-expr-dict
              (make-mul-dict (lambda (x y) (* x y)))))

(define pretty-mul-dict
  (dict-merge pretty-expr-dict
              (make-mul-dict (lambda (x y)
                                     (string-append "(" x " * " y ")")))))

;;; example-mul : Dict → α
;;; Represents: (2 * 3) + 4 = 10
(define (example-mul d)
  (expr-add d
            (expr-mul d (expr-lit d 2) (expr-lit d 3))
            (expr-lit d 4)))

;;; ====
;;; Boolean Algebra
;;; ====

;;; make-bool-dict : (Boolean → α) × (α × α → α) × (α × α → α) × (α → α) → Dict
(define (make-bool-dict bool-lit and-op or-op not-op)
  (doc 'export #t)
  (make-dict 'bool
             `((bool-lit . ,bool-lit)
               (and . ,and-op)
               (or . ,or-op)
               (not . ,not-op))))

;;; bool-lit : Dict × Boolean → α
(define (bool-lit d b) ((dict-ref d 'bool-lit) b))
(doc 'export #t)
;;; bool-and : Dict × α × α → α
(define (bool-and d x y) ((dict-ref d 'and) x y))
(doc 'export #t)
;;; bool-or : Dict × α × α → α
(define (bool-or d x y) ((dict-ref d 'or) x y))
(doc 'export #t)
;;; bool-not : Dict × α → α
(define (bool-not d x) ((dict-ref d 'not) x))
(doc 'export #t)

;;; Boolean evaluator
(define eval-bool-dict
  (make-bool-dict
   (lambda (b) b)
   (lambda (x y) (and x y))
   (lambda (x y) (or x y))
   (lambda (x) (not x))))

;;; Boolean pretty printer
(define pretty-bool-dict
  (make-bool-dict
   (lambda (b) (if b "true" "false"))
   (lambda (x y) (string-append "(" x " && " y ")"))
   (lambda (x y) (string-append "(" x " || " y ")"))
   (lambda (x) (string-append "(!" x ")"))))

;;; ====
;;; Comparison Extension (Combines Expr and Bool)
;;; ====

;;; make-cmp-dict : (α × α → β) × (α × α → β) × (α × α → β) → Dict
(define (make-cmp-dict eq-op lt-op gt-op)
  (make-dict 'cmp
             `((eq . ,eq-op)
               (lt . ,lt-op)
               (gt . ,gt-op))))

;;; cmp-eq : Dict × α × α → β
(define (cmp-eq d x y) ((dict-ref d 'eq) x y))
;;; cmp-lt : Dict × α × α → β
(define (cmp-lt d x y) ((dict-ref d 'lt) x y))
;;; cmp-gt : Dict × α × α → β
(define (cmp-gt d x y) ((dict-ref d 'gt) x y))

;;; Full language dict combines expr + bool + cmp
(define eval-full-dict
  (dict-merge
   (dict-merge eval-expr-dict eval-bool-dict)
   (make-cmp-dict
    (lambda (x y) (= x y))
    (lambda (x y) (< x y))
    (lambda (x y) (> x y)))))

;;; ====
;;; Conditional Extension
;;; ====

;;; IMPORTANT: For correct conditional semantics, use cond-if-lazy with
;;; eval-cond-dict. The thunked version prevents eager evaluation of
;;; both branches.

;;; make-cond-dict : (Boolean × (() → α) × (() → α) → α) → Dict
(define (make-cond-dict if-op)
  (doc 'export #t)
  (make-dict 'cond `((if . ,if-op))))

;;; cond-if-lazy : Dict × Boolean × (() → α) × (() → α) → α
;;; Correct conditional that takes thunks to prevent eager evaluation.
(define (cond-if-lazy d c t-thunk e-thunk)
  ((dict-ref d 'if) c t-thunk e-thunk))

;;; cond-if : Dict × Boolean × α × α → α
;;; DEPRECATED: Both branches are eagerly evaluated before the condition
;;; is checked. Use cond-if-lazy instead for correct short-circuit behavior.
(define (cond-if d c t e) ((dict-ref d 'if) c (lambda () t) (lambda () e)))
(doc 'export #t)

;;; Evaluator uses thunks for short-circuit semantics
(define eval-cond-dict
  (make-cond-dict
   (lambda (c t-thunk e-thunk) (if c (t-thunk) (e-thunk)))))

;;; For pretty-printing, both branches are shown (intentional)
(define pretty-cond-dict
  (make-cond-dict
   (lambda (c t-thunk e-thunk)
           (string-append "(if " c " then " (t-thunk) " else " (e-thunk) ")"))))

;;; ====
;;; Lazy/Thunked Conditionals
;;; ====
;;;
;;; For proper short-circuit evaluation, we need thunks.

;;; make-lazy-cond-dict : (Boolean × (() → α) × (() → α) → α) → Dict
(define (make-lazy-cond-dict if-op)
  (make-dict 'lazy-cond `((if-lazy . ,if-op))))

;;; lazy-if : Dict × Boolean × (() → α) × (() → α) → α
(define (lazy-if d c t-thunk e-thunk)
  ((dict-ref d 'if-lazy) c t-thunk e-thunk))

(define eval-lazy-cond-dict
  (make-lazy-cond-dict
   (lambda (c t-thunk e-thunk)
           (if c (t-thunk) (e-thunk)))))

;;; ====
;;; Variable Binding Extension
;;; ====

;;; make-let-dict : (Symbol × α × (α → β) → β) × (Symbol → α) → Dict
(define (make-let-dict let-op var-op)
  (doc 'export #t)
  (make-dict 'let
             `((let . ,let-op)
               (var . ,var-op))))

;;; tl-let : Dict × Symbol × α × (α → β) → β
(define (tl-let d name val body)
  (doc 'export #t)
  ((dict-ref d 'let) name val body))

;;; tl-var : Dict × Symbol → α
(define (tl-var d name)
  (doc 'export #t)
  ((dict-ref d 'var) name))

;;; For the evaluator, we thread an environment
;;; This shows how tagless final handles state.

;;; make-env-expr-dict : Unit → Dict
;;; Creates an expression dictionary that uses an environment.
(define (make-env-expr-dict)
  (make-dict 'env-expr
             `((lit . ,(lambda (n) (lambda (env) n)))
               (add . ,(lambda (x y) (lambda (env) (+ (x env) (y env)))))
               (neg . ,(lambda (x) (lambda (env) (- (x env)))))
               (let . ,(lambda (name val body)
                               (lambda (env)
                                       (let ([v (val env)])
                                            (body (cons (cons name v) env))))))
               (var . ,(lambda (name)
                               (lambda (env)
                                       (let ([pair (assq name env)])
                                            (if pair (cdr pair)
                                                (error 'var "Unbound variable" name)))))))))

;;; env-lit : Dict × Int → (Env → α)
(define (env-lit d n) ((dict-ref d 'lit) n))
;;; env-add : Dict × (Env → α) × (Env → α) → (Env → α)
(define (env-add d x y) ((dict-ref d 'add) x y))
;;; env-neg : Dict × (Env → α) → (Env → α)
(define (env-neg d x) ((dict-ref d 'neg) x))
;;; env-let : Dict × Symbol × (Env → α) × (Env → β) → (Env → β)
(define (env-let d name val body) ((dict-ref d 'let) name val body))
;;; env-var : Dict × Symbol → (Env → α)
(define (env-var d name) ((dict-ref d 'var) name))

;;; run-env-expr : Dict × (Dict → (Env → α)) → α
;;; Run with empty initial environment
;;; program is a function (dict -> (env -> result)), so apply dict first, then env
(define (run-env-expr d program)
  ((program d) '()))

;;; example-let : Dict → (Env → α)
;;; Example: let x = 5 in let y = 3 in x + y
(define (example-let d)
  (env-let d 'x (env-lit d 5)
           (env-let d 'y (env-lit d 3)
                    (env-add d (env-var d 'x) (env-var d 'y)))))

;;; ====
;;; State Monad Integration
;;; ====
;;;
;;; For stateful DSLs, programs return state transformers.

;;; StatefulDict operations return (State s a) = s -> (a, s)

;;; make-stateful-dict : (() → (σ → (σ . σ))) × (σ → (σ → (Unit . σ))) × (α → (σ → (α . σ))) × ((σ → (α . σ)) × (α → (σ → (β . σ))) → (σ → (β . σ))) → Dict
(define (make-stateful-dict get-op put-op pure-op bind-op)
  (doc 'export #t)
  (make-dict 'state
             `((get . ,get-op)
               (put . ,put-op)
               (pure . ,pure-op)
               (bind . ,bind-op))))

;;; state-get : Dict → (σ → (σ . σ))
(define (state-get d) ((dict-ref d 'get)))
(doc 'export #t)
;;; state-put : Dict × σ → (σ → (Unit . σ))
(define (state-put d s) ((dict-ref d 'put) s))
(doc 'export #t)
;;; state-pure : Dict × α → (σ → (α . σ))
(define (state-pure d a) ((dict-ref d 'pure) a))
(doc 'export #t)
;;; state-bind : Dict × (σ → (α . σ)) × (α → (σ → (β . σ))) → (σ → (β . σ))
(define (state-bind d ma f) ((dict-ref d 'bind) ma f))
(doc 'export #t)

;;; State monad implementation
(define state-monad-dict
  (make-stateful-dict
   ;; get : () -> State s s
   (lambda () (lambda (s) (cons s s)))
   ;; put : s -> State s ()
   (lambda (new-s) (lambda (s) (cons '() new-s)))
   ;; pure : a -> State s a
   (lambda (a) (lambda (s) (cons a s)))
   ;; bind : State s a -> (a -> State s b) -> State s b
   (lambda (ma f)
           (lambda (s)
                   (let* ([result (ma s)]
                          [a (car result)]
                          [s2 (cdr result)])
                         ((f a) s2))))))

;;; run-state : (σ → (α . σ)) × σ → (α . σ)
(define (run-state computation initial-state)
  (computation initial-state))

;;; ====
;;; Chronicle DSL (Tagless Final Version)
;;; ====
;;;
;;; Narrative engine operations as a tagless final algebra.

;;; make-chronicle-dict : (Symbol × String × (List Choice) → Scene) × (String × Symbol → Choice) × (Predicate → Guard) × (Effect → Effect) × (String → Text) × (Chronicle → Run) × (Run × Int → Run) × (Run → Sexp) → Dict
(define (make-chronicle-dict
         scene-op choice-op guard-op effect-op
         text-op start-op choose-op render-op)
  (make-dict 'chronicle
             `((scene . ,scene-op)
               (choice . ,choice-op)
               (guard . ,guard-op)
               (effect . ,effect-op)
               (text . ,text-op)
               (start . ,start-op)
               (choose . ,choose-op)
               (render . ,render-op))))

;;; ch-scene : Dict × Symbol × String × (List Choice) → Scene
(define (ch-scene d id text . choices)
  (doc 'export #t)
  (apply (dict-ref d 'scene) id text choices))

;;; ch-choice : Dict × String × Symbol → Choice
(define (ch-choice d label target . opts)
  (doc 'export #t)
  (apply (dict-ref d 'choice) label target opts))

;;; ch-guard : Dict × Predicate → Guard
(define (ch-guard d pred)
  ((dict-ref d 'guard) pred))

;;; ch-effect : Dict × Effect → Effect
(define (ch-effect d eff)
  ((dict-ref d 'effect) eff))

;;; ch-text : Dict × String → Text
(define (ch-text d template)
  ((dict-ref d 'text) template))

;;; ch-start : Dict × Chronicle → Run
(define (ch-start d chronicle)
  (doc 'export #t)
  ((dict-ref d 'start) chronicle))

;;; ch-choose : Dict × Run × Int → Run
(define (ch-choose d run choice-idx)
  (doc 'export #t)
  ((dict-ref d 'choose) run choice-idx))

;;; ch-render : Dict × Run → Sexp
(define (ch-render d run)
  ((dict-ref d 'render) run))

;;; ====
;;; Chronicle Interpreters
;;; ====

;;; Validation interpreter - checks structure without running
(define validate-chronicle-dict
  (make-chronicle-dict
   ;; scene: collect scene id
   (lambda (id text . choices)
           `(scene ,id ,(length choices) choices))
   ;; choice: validate target
   (lambda (label target . opts)
           `(choice ,label -> ,target))
   ;; guard: identity
   (lambda (pred) pred)
   ;; effect: identity
   (lambda (eff) eff)
   ;; text: identity
   (lambda (template) template)
   ;; start: return initial state
   (lambda (chronicle) '(run initial))
   ;; choose: validate choice exists
   (lambda (run idx) `(chose ,idx))
   ;; render: show structure
   (lambda (run) `(render ,run))))

;;; Graph interpreter - extract scene graph
(define graph-chronicle-dict
  (make-chronicle-dict
   ;; scene: return (id . targets)
   (lambda (id text . choices)
           (cons id (map (lambda (c) (list-ref c 2)) choices)))
   ;; choice: return (label . target)
   (lambda (label target . opts)
           (list 'choice label target))
   ;; guard/effect/text: ignored for graph
   (lambda (pred) '())
   (lambda (eff) '())
   (lambda (template) "")
   ;; start/choose/render: identity for graph building
   (lambda (chronicle) chronicle)
   (lambda (run idx) run)
   (lambda (run) run)))

;;; ====
;;; Dictionary Transformers
;;; ====
;;;
;;; Functions that transform dictionaries to add cross-cutting concerns.

;;; with-logging : (Symbol × α → Unit) × Dict → Dict
;;; Wrap all operations to log calls.
(define (with-logging log-fn d)
  (doc 'export #t)
  (make-dict (list 'logged (dict-tag d))
             (map (lambda (op)
                          (cons (car op)
                                (lambda args
                                        (log-fn (car op) args)
                                        (apply (cdr op) args))))
                  (dict-ops d))))

;;; with-tracing : Dict → Dict
;;; Wrap all operations to print traces.
(define (with-tracing d)
  (doc 'export #t)
  (with-logging
   (lambda (name args)
           (display (format "[TRACE] ~a: ~s\n" name args)))
   d))

;;; with-memoization : Dict → Dict
;;; Memoize pure operations (requires operations to be referentially transparent).
(define (with-memoization d)
  (doc 'export #t)
  (let ([cache (make-hashtable equal-hash equal?)])
       (make-dict (list 'memoized (dict-tag d))
                  (map (lambda (op)
                               (cons (car op)
                                     (lambda args
                                             (let ([key (cons (car op) args)])
                                                  (let ([cached (hashtable-ref cache key 'not-found)])
                                                       (if (eq? cached 'not-found)
                                                           (let ([result (apply (cdr op) args)])
                                                                (hashtable-set! cache key result)
                                                                result)
                                                           cached))))))
                       (dict-ops d)))))

;;; ====
;;; Composition Utilities
;;; ====

;;; run-both : (Dict → α) × Dict × Dict → (α . β)
;;; Run a program with two dictionaries, returning pair of results.
;;; This is the correct way to get multiple interpretations.
(define (run-both program d1 d2)
  (cons (program d1) (program d2)))

;;; Note: A true "product" interpreter that threads pairs through
;;; operations would require all operations to work on pairs,
;;; which breaks the abstraction. Use run-both instead.

;;; Example: eval and count simultaneously
;;; (example-expr-1 (product-dict eval-expr-dict count-expr-dict))
;;; => (5 . 4)

;;; ====
;;; Higher-Order Programs
;;; ====
;;;
;;; Programs can be composed and transformed.

;;; twice : (Dict × α → α) × Dict × α → α
;;; Apply an operation twice.
(define (twice op d x)
  (op d (op d x)))

;;; Example: double negation
;;; (twice expr-neg eval-expr-dict 5) => 5

;;; fold-expr : Dict × (α → β) × (β × β → β) × (β → β) → Dict
;;; Generic fold over expression structure.
(define (fold-expr d on-lit on-add on-neg)
  (make-expr-dict on-lit on-add on-neg))

;;; ====
;;; Tagless Final + Free Monad Bridge
;;; ====
;;;
;;; Convert between the two representations.

;;; tagless-to-free : (Dict → α) → Sexp
;;; Convert a tagless program to Free monad representation.
;;; This enables inspection at the cost of building intermediate structure.
(define (tagless-to-free program)
  (doc 'export #t)
  ;; Create a dictionary that builds Free monad terms
  (let ([free-dict
         (make-expr-dict
          (lambda (n) (list 'lit n))
          (lambda (x y) (list 'add x y))
          (lambda (x) (list 'neg x)))])
       (program free-dict)))

;;; free-to-tagless : Sexp → (Dict → α)
;;; Convert Free monad term back to tagless.
(define (free-to-tagless term)
  (doc 'export #t)
  (lambda (d)
          (cond
           [(and (pair? term) (eq? (car term) 'lit))
            (expr-lit d (cadr term))]
           [(and (pair? term) (eq? (car term) 'add))
            (expr-add d
                      ((free-to-tagless (cadr term)) d)
                      ((free-to-tagless (caddr term)) d))]
           [(and (pair? term) (eq? (car term) 'neg))
            (expr-neg d ((free-to-tagless (cadr term)) d))]
           [else (error 'free-to-tagless "Unknown term" term)])))

;;; ====
;;; Optimization via Dictionary Transformation
;;; ====

;;; Double negation elimination
;;; optimize-neg-neg : Dict → Dict
(define (optimize-neg-neg base-dict)
  (make-dict 'optimized
             `((lit . ,(dict-ref base-dict 'lit))
               (add . ,(dict-ref base-dict 'add))
               (neg . ,(lambda (x)
                               ;; Check if x is already a negation
                               ;; This requires some way to inspect x...
                               ;; In pure tagless, we can't! This shows a limitation.
                               ((dict-ref base-dict 'neg) x))))))

;;; For optimization, we can use a staged approach:
;;; First interpret to an AST (using tagless-to-free),
;;; then optimize, then interpret the result.

;;; optimize-expr : Sexp → Sexp
(define (optimize-expr term)
  (doc 'export #t)
  (cond
   ;; Double negation: (neg (neg x)) -> x
   [(and (pair? term) (eq? (car term) 'neg)
         (pair? (cadr term)) (eq? (car (cadr term)) 'neg))
    (optimize-expr (cadr (cadr term)))]
   ;; Add with zero: (add x 0) -> x, (add 0 x) -> x
   [(and (pair? term) (eq? (car term) 'add))
    (let ([l (optimize-expr (cadr term))]
          [r (optimize-expr (caddr term))])
         (cond
          [(and (pair? l) (eq? (car l) 'lit) (= (cadr l) 0)) r]
          [(and (pair? r) (eq? (car r) 'lit) (= (cadr r) 0)) l]
          [else (list 'add l r)]))]
   ;; Recurse into subterms
   [(and (pair? term) (eq? (car term) 'neg))
    (list 'neg (optimize-expr (cadr term)))]
   [(and (pair? term) (eq? (car term) 'lit))
    term]
   [else term]))

;;; run-optimized : (Dict → α) × Dict → α
;;; Run program with optimization.
(define (run-optimized program eval-dict)
  (let* ([ast (tagless-to-free program)]
         [opt-ast (optimize-expr ast)])
        ((free-to-tagless opt-ast) eval-dict)))

;;; ====
;;; Partial Evaluation
;;; ====
;;;
;;; Specialize programs based on known values.

;;; A partially-evaluated value is either:
;;;   (static n)   - known at specialization time
;;;   (dynamic f)  - unknown, f is a function to compute it

;;; make-pe-dict : Unit → Dict
(define (make-pe-dict)
  (make-dict 'partial-eval
             `((lit . ,(lambda (n) (list 'static n)))
               (add . ,(lambda (x y)
                               (cond
                                [(and (eq? (car x) 'static) (eq? (car y) 'static))
                                 (list 'static (+ (cadr x) (cadr y)))]
                                [else
                                 (list 'dynamic
                                       (lambda ()
                                               (+ (if (eq? (car x) 'static) (cadr x) ((cadr x)))
                                                  (if (eq? (car y) 'static) (cadr y) ((cadr y))))))])))
               (neg . ,(lambda (x)
                               (if (eq? (car x) 'static)
                                   (list 'static (- (cadr x)))
                                   (list 'dynamic
                                         (lambda () (- ((cadr x)))))))))))

;;; ====
;;; Type-Safe DSL Construction
;;; ====
;;;
;;; Use phantom types for type safety (approximated in Scheme).

;;; In a typed language, we'd have:
;;;   lit : Int -> repr TInt
;;;   add : repr TInt -> repr TInt -> repr TInt
;;;   eq  : repr TInt -> repr TInt -> repr TBool
;;;
;;; In Scheme, we simulate with runtime tags.

;;; make-typed-expr-dict : (Int → Typed) × (Typed × Typed → Typed) × (Typed → Typed) × (Typed × Typed → Typed) × (Typed × (() → α) × (() → α) → α) → Dict
(define (make-typed-expr-dict lit add neg eq-op if-op)
  (make-dict 'typed-expr
             `((lit . ,lit)
               (add . ,add)
               (neg . ,neg)
               (eq . ,eq-op)
               (if . ,if-op))))

;;; typed-int : Int → Typed
(define (typed-int n) (list 'int n))
;;; typed-bool : Boolean → Typed
(define (typed-bool b) (list 'bool b))
;;; typed-value : Typed → α
(define (typed-value v) (cadr v))
;;; typed-type : Typed → Symbol
(define (typed-type v) (car v))

(define eval-typed-dict
  (make-typed-expr-dict
   (lambda (n) (typed-int n))
   (lambda (x y)
           (if (and (eq? (typed-type x) 'int) (eq? (typed-type y) 'int))
               (typed-int (+ (typed-value x) (typed-value y)))
               (error 'add "Type error: expected int")))
   (lambda (x)
           (if (eq? (typed-type x) 'int)
               (typed-int (- (typed-value x)))
               (error 'neg "Type error: expected int")))
   (lambda (x y)
           (if (and (eq? (typed-type x) 'int) (eq? (typed-type y) 'int))
               (typed-bool (= (typed-value x) (typed-value y)))
               (error 'eq "Type error: expected int")))
   ;; Note: if takes thunks to prevent eager evaluation of branches
   (lambda (c t-thunk e-thunk)
           (if (eq? (typed-type c) 'bool)
               (if (typed-value c) (t-thunk) (e-thunk))
               (error 'if "Type error: expected bool")))))

;;; ====
;;; Summary: The Pattern
;;; ====
;;;
;;; 1. Define algebra as dictionary constructor:
;;;    (make-foo-dict op1 op2 ...) -> FooDict
;;;
;;; 2. Define accessors:
;;;    (foo-op1 dict args ...) -> result
;;;
;;; 3. Create interpreter dictionaries:
;;;    (define eval-foo-dict (make-foo-dict ...))
;;;    (define pretty-foo-dict (make-foo-dict ...))
;;;
;;; 4. Write programs as functions:
;;;    (define (my-program d)
;;;      (foo-op1 d (foo-op2 d ...)))
;;;
;;; 5. Run with different interpreters:
;;;    (my-program eval-foo-dict)
;;;    (my-program pretty-foo-dict)
;;;
;;; 6. Extend without modifying:
;;;    (dict-merge base-dict extension-dict)
;;;
;;; 7. Add cross-cutting concerns:
;;;    (with-logging log-fn dict)
;;;    (with-tracing dict)
;;;    (product-dict dict1 dict2)
