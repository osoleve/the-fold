;;; core/dsl/compose.ss --- Modular DSL Composition
;;;
;;; Enables combining independent DSLs without modification.
;;; The core problem: Two DSLs (e.g., Arithmetic and State) need to work
;;; together, but neither knows about the other.
;;;
;;; This module provides three composition strategies:
;;;
;;; 1. Data Types à la Carte (Swierstra's pattern)
;;;    - Combine functors via coproduct
;;;    - Type-safe injection via row polymorphism
;;;    - Free monad over coproduct = composed DSL
;;;
;;; 2. Effect Composition
;;;    - Stack effect handlers
;;;    - Effect row manipulation
;;;    - Lift between effect rows
;;;
;;; 3. Tagless Composition
;;;    - Dictionary intersection
;;;    - Modular interpreters
;;;    - Constraint satisfaction
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - dsl/tagless.ss
;;;   - fp/control/free.ss
;;;   - fp/control/effects.ss

(load "core/base/prelude.ss")
(load "core/dsl/tagless.ss")
(load "core/fp/control/free.ss")
(load "core/fp/control/effects.ss")

;;; ============================================================
;;; Part 1: Data Types à la Carte
;;; ============================================================
;;;
;;; The key insight: if we have functors F and G, their coproduct
;;; F + G is also a functor. We can then use Free (F + G) to get
;;; a DSL with operations from both F and G.
;;;
;;; Functor coproduct: (+ f g) a = Inl (f a) | Inr (g a)

;;; ------------------------------------------------------------
;;; Functor Coproduct
;;; ------------------------------------------------------------

;;; inl : f a -> (+ f g) a
;;; Inject into left of coproduct
(define (inl fa)
  (list 'inl fa))

;;; inr : g a -> (+ f g) a
;;; Inject into right of coproduct
(define (inr ga)
  (list 'inr ga))

;;; inl? : (+ f g) a -> Boolean
(define (inl? x)
  (and (pair? x) (eq? (car x) 'inl)))

;;; inr? : (+ f g) a -> Boolean
(define (inr? x)
  (and (pair? x) (eq? (car x) 'inr)))

;;; from-inl : (+ f g) a -> f a
(define (from-inl x) (cadr x))

;;; from-inr : (+ f g) a -> g a
(define (from-inr x) (cadr x))

;;; coproduct-fmap : (fa -> fb) -> (ga -> gb) -> (+ f g) a -> (+ f g) b
;;; Functor instance for coproduct
(define (coproduct-fmap fmap-f fmap-g fg)
  (if (inl? fg)
      (inl (fmap-f (from-inl fg)))
      (inr (fmap-g (from-inr fg)))))

;;; make-coproduct-fmap : (fa -> fb) -> (ga -> gb) -> ((+ f g) a -> (+ f g) b)
;;; Create a combined fmap for a coproduct functor
(define (make-coproduct-fmap fmap-f fmap-g)
  (lambda (f fg)
          (if (inl? fg)
              (inl (fmap-f f (from-inl fg)))
              (inr (fmap-g f (from-inr fg))))))

;;; ------------------------------------------------------------
;;; N-ary Coproduct Fmap (Row-based)
;;; ------------------------------------------------------------
;;;
;;; Generate fmap for arbitrary functor rows, not just binary coproducts.

;;; make-row-fmap : FunctorRow -> Alist(Symbol, Fmap) -> ((a -> b) -> Row a -> Row b)
;;; Build an fmap that handles any functor in the row.
;;; fmap-alist maps functor tags to their fmap implementations.
(define (make-row-fmap row fmap-alist)
  (lambda (f value)
          (row-fmap-helper f value 0 (functor-row-functors row) fmap-alist)))

;;; row-fmap-helper : (a -> b) -> Row a -> Nat -> (List Symbol) -> Alist -> Row b
(define (row-fmap-helper f value depth functors fmap-alist)
  (cond
   [(null? functors)
    (error 'row-fmap "Value not in row" value)]
   [(inl? value)
    ;; We're at this functor's level
    (let* ([tag (car functors)]
           [fmap-entry (assq tag fmap-alist)])
          (if fmap-entry
              (inl ((cdr fmap-entry) f (from-inl value)))
              (error 'row-fmap "No fmap for functor" tag)))]
   [(inr? value)
    ;; Descend into the next level
    (inr (row-fmap-helper f (from-inr value) (+ depth 1) (cdr functors) fmap-alist))]
   [else
    (error 'row-fmap "Malformed coproduct value" value)]))

;;; register-fmap : Alist -> Symbol -> Fmap -> Alist
;;; Add a functor's fmap to the registry
(define (register-fmap alist tag fmap-fn)
  (cons (cons tag fmap-fn) alist))

;;; ------------------------------------------------------------
;;; Functor Rows (Type-safe Injection)
;;; ------------------------------------------------------------
;;;
;;; To enable clean injection like `(inject (lit 5))` without
;;; manually tracking coproduct structure, we use functor rows.
;;;
;;; A functor row is like an effect row but for functors:
;;;   (functor-row (F G H ...))
;;;
;;; Injection finds the functor in the row and builds the path.

;;; make-functor-row : (List Symbol) -> FunctorRow
(define (make-functor-row functors)
  (list 'functor-row functors))

;;; functor-row? : Any -> Boolean
(define (functor-row? x)
  (and (pair? x) (eq? (car x) 'functor-row)))

;;; functor-row-functors : FunctorRow -> (List Symbol)
(define (functor-row-functors row)
  (cadr row))

;;; functor-row-contains? : FunctorRow -> Symbol -> Boolean
(define (functor-row-contains? row functor)
  (if (memq functor (functor-row-functors row)) #t #f))

;;; functor-row-index : FunctorRow -> Symbol -> Nat | #f
;;; Find the index of a functor in the row
(define (functor-row-index row functor)
  (let loop ([fs (functor-row-functors row)] [i 0])
       (cond
        [(null? fs) #f]
        [(eq? (car fs) functor) i]
        [else (loop (cdr fs) (+ i 1))])))

;;; functor-row-extend : FunctorRow -> Symbol -> FunctorRow
(define (functor-row-extend row functor)
  (if (functor-row-contains? row functor)
      row
      (make-functor-row (cons functor (functor-row-functors row)))))

;;; functor-row-union : FunctorRow -> FunctorRow -> FunctorRow
(define (functor-row-union row1 row2)
  (let loop ([fs (functor-row-functors row2)] [result row1])
       (if (null? fs)
           result
           (loop (cdr fs) (functor-row-extend result (car fs))))))

;;; ------------------------------------------------------------
;;; Path-based Injection
;;; ------------------------------------------------------------
;;;
;;; Given a functor row (F G H), to inject into G:
;;;   - G is at index 1
;;;   - Path is: inr (inl x)
;;;   - For index n: apply inr n times, then inl once

;;; build-injection-path : Nat -> a -> (+ ... a)
;;; Build nested inl/inr structure for index n
(define (build-injection-path index value)
  (if (= index 0)
      (inl value)
      (inr (build-injection-path (- index 1) value))))

;;; inject-by-index : Nat -> f a -> (+ row) a
;;; Inject a functor value at the given index
(define inject-by-index build-injection-path)

;;; ------------------------------------------------------------
;;; Smart Injection with Tags
;;; ------------------------------------------------------------
;;;
;;; For convenience, we tag functor values so injection can be automatic.

;;; make-tagged-functor : Symbol -> Any -> TaggedFunctor
(define (make-tagged-functor tag value)
  (list 'tagged-functor tag value))

;;; tagged-functor? : Any -> Boolean
(define (tagged-functor? x)
  (and (pair? x) (eq? (car x) 'tagged-functor)))

;;; tagged-functor-tag : TaggedFunctor -> Symbol
(define (tagged-functor-tag tf) (cadr tf))

;;; tagged-functor-value : TaggedFunctor -> Any
(define (tagged-functor-value tf) (caddr tf))

;;; inject : FunctorRow -> TaggedFunctor -> (+ row) a
;;; Inject a tagged functor into a row
(define (inject row tf)
  (let ([tag (tagged-functor-tag tf)]
        [value (tagged-functor-value tf)])
       (let ([index (functor-row-index row tag)])
            (if index
                (build-injection-path index value)
                (error 'inject "Functor not in row" tag row)))))

;;; ------------------------------------------------------------
;;; Example: Arithmetic Functor
;;; ------------------------------------------------------------

;;; ArithF a = LitF Int a | AddF a a a | NegF a a
;;; (where the last 'a' in each is the continuation)

;;; lit-f : Int -> (a -> ArithF a)
;;; Create a literal (returns wrapped value via continuation)
(define (lit-f n)
  (make-tagged-functor 'Arith (list 'lit n)))

;;; add-f : a -> a -> ArithF a
(define (add-f x y)
  (make-tagged-functor 'Arith (list 'add x y)))

;;; neg-f : a -> ArithF a
(define (neg-f x)
  (make-tagged-functor 'Arith (list 'neg x)))

;;; arith-fmap : (a -> b) -> ArithF a -> ArithF b
(define (arith-fmap f cmd)
  (let ([tag (car cmd)])
       (case tag
             [(lit) cmd]  ; No recursion needed for literal
             [(add) (list 'add (f (cadr cmd)) (f (caddr cmd)))]
             [(neg) (list 'neg (f (cadr cmd)))]
             [else (error 'arith-fmap "Unknown arithmetic operation" tag)])))

;;; ------------------------------------------------------------
;;; Example: State Functor
;;; ------------------------------------------------------------

;;; StateF s a = GetF (s -> a) | PutF s a

;;; get-f : StateF s (Free StateF s)
(define (get-f)
  (make-tagged-functor 'State (list 'get)))

;;; put-f : s -> StateF s (Free StateF ())
(define (put-f s)
  (make-tagged-functor 'State (list 'put s)))

;;; state-f-fmap : (a -> b) -> StateF s a -> StateF s b
(define (state-f-fmap f cmd)
  (let ([tag (car cmd)])
       (case tag
             [(get) (list 'get (lambda (s) (f ((cadr cmd) s))))]
             [(put) (list 'put (cadr cmd) (f (caddr cmd)))]
             [else (error 'state-f-fmap "Unknown state operation" tag)])))

;;; Standard fmap registry for common functors
;;; Maps functor tags to their fmap implementations
(define standard-fmap-registry
  (list (cons 'Arith arith-fmap)
        (cons 'State state-f-fmap)))

;;; ------------------------------------------------------------
;;; Free over Coproduct
;;; ------------------------------------------------------------

;;; lift-arith : ArithF (Free (Arith + r) a) -> Free (Arith + r) a
;;; Lift arithmetic into Free over a coproduct containing Arith
(define (lift-arith row cmd)
  (free (inject row (make-tagged-functor 'Arith cmd))))

;;; lift-state : StateF s (Free (State + r) a) -> Free (State + r) a
(define (lift-state row cmd)
  (free (inject row (make-tagged-functor 'State cmd))))

;;; ------------------------------------------------------------
;;; Combined DSL Operations
;;; ------------------------------------------------------------

;;; For a combined Arith+State DSL, we need operations that work
;;; with the coproduct functor.

;;; Define the combined row
(define arith-state-row (make-functor-row '(Arith State)))

;;; Build the row-aware fmap for this combination
(define arith-state-fmap
  (make-row-fmap arith-state-row standard-fmap-registry))

;;; Arithmetic operations in combined DSL
;;; Uses tag-based injection for row polymorphism
(define (dsl-lit row n)
  (free (inject row (make-tagged-functor 'Arith (list 'lit n)))))

;;; dsl-add : Row -> Free Row a -> Free Row a -> Free Row a
;;; Builds an 'add AST node containing the two sub-expressions.
;;; This is a constructor, not an evaluator - it preserves structure.
(define (dsl-add row x y)
  (free (inject row (make-tagged-functor 'Arith (list 'add x y)))))

;;; State operations in combined DSL
;;; Uses tag-based injection for row polymorphism
(define (dsl-get row)
  (free (inject row (make-tagged-functor 'State (list 'get pure-free)))))

(define (dsl-put row s)
  (free (inject row (make-tagged-functor 'State (list 'put s (pure-free '()))))))

;;; Convenience wrappers for the default arith-state-row
;;; Note: prefixed with 'free-' to avoid shadowing effect operations
(define (free-arith-lit n) (dsl-lit arith-state-row n))
(define (free-arith-add x y) (dsl-add arith-state-row x y))
(define free-state-get (dsl-get arith-state-row))
(define (free-state-put s) (dsl-put arith-state-row s))

;;; ============================================================
;;; Part 2: Effect Composition
;;; ============================================================
;;;
;;; Building on the effect system, we provide utilities for
;;; composing effect handlers in a modular way.

;;; ------------------------------------------------------------
;;; Effect Handler Stack
;;; ------------------------------------------------------------

;;; A handler stack is an ordered list of handlers.
;;; Effects are handled from top to bottom.

;;; make-handler-stack : (List Handler) -> HandlerStack
(define (make-handler-stack handlers)
  (list 'handler-stack handlers))

;;; handler-stack? : Any -> Boolean
(define (handler-stack? x)
  (and (pair? x) (eq? (car x) 'handler-stack)))

;;; handler-stack-handlers : HandlerStack -> (List Handler)
(define (handler-stack-handlers stack)
  (cadr stack))

;;; run-with-stack : HandlerStack -> Eff e a -> b
;;; Run computation with a stack of handlers (iterative approach)
(define (run-with-stack stack eff)
  (let loop ([handlers (handler-stack-handlers stack)] [computation eff])
       (if (null? handlers)
           (if (eff-pure? computation)
               (eff-pure-value computation)
               (error 'run-with-stack "Unhandled effect" (eff-op-effect computation)))
           (loop (cdr handlers)
                 (handle (car handlers) computation)))))

;;; run-with-composed-stack : HandlerStack -> Eff e a -> b
;;; Run computation with composed handlers (single-pass, more efficient)
;;; Composes all handlers into one before running.
(define (run-with-composed-stack stack eff)
  (let ([handlers (handler-stack-handlers stack)])
       (if (null? handlers)
           (if (eff-pure? eff)
               (eff-pure-value eff)
               (error 'run-with-composed-stack "Unhandled effect" (eff-op-effect eff)))
           (let ([composed (combine-effect-handlers handlers)])
                (handle composed eff)))))

;;; push-handler : Handler -> HandlerStack -> HandlerStack
(define (push-handler handler stack)
  (make-handler-stack (cons handler (handler-stack-handlers stack))))

;;; pop-handler : HandlerStack -> HandlerStack
(define (pop-handler stack)
  (make-handler-stack (cdr (handler-stack-handlers stack))))

;;; ------------------------------------------------------------
;;; Effect Row Operations for Composition
;;; ------------------------------------------------------------

;;; The key insight: when we handle an effect, we remove it from
;;; the effect row. This enables modular reasoning about effects.

;;; effect-row-handled : EffectRow -> Symbol -> EffectRow
;;; Remove an effect from the row (it's been handled)
(define effect-row-handled row-remove)

;;; effect-row-requires : EffectRow -> Symbol -> Boolean
;;; Check if computation requires this effect
(define effect-row-requires row-contains?)

;;; effect-row-satisfies : EffectRow -> EffectRow -> Boolean
;;; Check if handlers satisfy required effects
(define (effect-row-satisfies provided required)
  (let loop ([effects (effect-row-effects required)])
       (or (null? effects)
           (and (row-contains? provided (car effects))
                (loop (cdr effects))))))

;;; ------------------------------------------------------------
;;; Effect Lifting
;;; ------------------------------------------------------------
;;;
;;; When composing effects, we need to lift computations from
;;; smaller effect rows to larger ones.

;;; eff-lift-row : Eff e1 a -> Eff (e1 + e2) a
;;; Lift a computation to a larger effect row.
;;; (This is identity since our representation is the same)
(define (eff-lift-row eff)
  eff)

;;; eff-restrict : (List Symbol) -> Eff e a -> Eff e' a
;;; Restrict to a subset of effects (runtime check)
(define (eff-restrict allowed-effects eff)
  (cond
   [(eff-pure? eff) eff]
   [(eff-op? eff)
    (let ([label (effect-label (eff-op-effect eff))])
         (if (memq label allowed-effects)
             (make-eff-op (eff-op-effect eff)
                          (lambda (resp)
                                  (eff-restrict allowed-effects
                                                ((eff-op-cont eff) resp))))
             (error 'eff-restrict "Effect not allowed" label allowed-effects)))]))

;;; ------------------------------------------------------------
;;; Modular Handler Definition
;;; ------------------------------------------------------------

;;; define-effect-handler : Symbol -> (a -> b) -> (List (Symbol . Handler)) -> Handler
;;; Define a handler for a single effect
(define (define-effect-handler effect-name return-case operations)
  (deep-handler return-case operations))

;;; combine-effect-handlers : (List Handler) -> Handler
;;; Combine multiple independent effect handlers
(define (combine-effect-handlers handlers)
  (if (null? handlers)
      (deep-handler identity '())
      (fold-right compose-handlers
                  (deep-handler identity '())
                  handlers)))

;;; ------------------------------------------------------------
;;; Common Combined Handlers
;;; ------------------------------------------------------------

;;; state+reader-handler : s -> r -> Handler
;;; Handle both State and Reader effects
(define (state+reader-handler init-state env)
  (deep-handler
   (lambda (a) (cons a init-state))
   `((state-get . ,(lambda (payload k)
                           (lambda (s) ((k s) s))))
     (state-put . ,(lambda (payload k)
                           (lambda (s) ((k '()) payload))))
     (reader-ask . ,(lambda (payload k)
                            (lambda (s) ((k env) s))))
     (reader-local . ,(lambda (payload k)
                              ;; payload is the modify function
                              (lambda (s) ((k '()) s)))))))

;;; state+writer-handler : s -> Handler
;;; Handle both State and Writer effects
(define (state+writer-handler init-state)
  (deep-handler
   (lambda (a) (list a init-state '()))  ; (value, state, log)
   `((state-get . ,(lambda (payload k)
                           (lambda (s log) ((k s) s log))))
     (state-put . ,(lambda (payload k)
                           (lambda (s log) ((k '()) payload log))))
     (writer-tell . ,(lambda (payload k)
                             (lambda (s log) ((k '()) s (cons payload log))))))))

;;; state+exception-handler : s -> Handler
;;; Handle both State and Exception effects
(define (state+exception-handler init-state)
  (deep-handler
   (lambda (a) (right (cons a init-state)))
   `((state-get . ,(lambda (payload k)
                           (lambda (s) ((k s) s))))
     (state-put . ,(lambda (payload k)
                           (lambda (s) ((k '()) payload))))
     (throw . ,(lambda (payload k)
                       (lambda (s) (left payload)))))))

;;; ============================================================
;;; Part 3: Tagless Composition
;;; ============================================================
;;;
;;; Tagless final enables extensibility via dictionary composition.
;;; This section provides utilities for modular tagless DSL design.

;;; ------------------------------------------------------------
;;; Dictionary Algebra
;;; ------------------------------------------------------------

;;; Dictionaries form a monoid under merge:
;;;   - Identity: empty dict
;;;   - Operation: dict-merge

;;; empty-dict : Dict
(define empty-dict (make-dict 'empty '()))

;;; dict-singleton : Symbol -> (Symbol . Procedure) -> Dict
;;; Create a dictionary with a single operation
(define (dict-singleton tag op-pair)
  (make-dict tag (list op-pair)))

;;; dict-from-list : Symbol -> (List (Symbol . Procedure)) -> Dict
(define dict-from-list make-dict)

;;; dict-concat : (List Dict) -> Dict
;;; Merge many dictionaries
(define (dict-concat dicts)
  (fold-right dict-merge empty-dict dicts))

;;; ------------------------------------------------------------
;;; Interface Constraints
;;; ------------------------------------------------------------
;;;
;;; A program may require certain operations. We represent this
;;; as a constraint that dictionaries must satisfy.

;;; make-interface : Symbol -> (List Symbol) -> Interface
;;; Define an interface as a named set of required operations
(define (make-interface name operations)
  (list 'interface name operations))

;;; interface? : Any -> Boolean
(define (interface? x)
  (and (pair? x) (eq? (car x) 'interface)))

;;; interface-name : Interface -> Symbol
(define (interface-name iface) (cadr iface))

;;; interface-operations : Interface -> (List Symbol)
(define (interface-operations iface) (caddr iface))

;;; dict-satisfies? : Dict -> Interface -> Boolean
;;; Check if a dictionary implements an interface
(define (dict-satisfies? d iface)
  (let loop ([ops (interface-operations iface)])
       (or (null? ops)
           (and (dict-has? d (car ops))
                (loop (cdr ops))))))

;;; assert-interface : Dict -> Interface -> Dict
;;; Assert that dict satisfies interface, error otherwise
(define (assert-interface d iface)
  (if (dict-satisfies? d iface)
      d
      (error 'assert-interface
             "Dictionary does not satisfy interface"
             (dict-tag d)
             (interface-name iface)
             (filter (lambda (op) (not (dict-has? d op)))
                     (interface-operations iface)))))

;;; ------------------------------------------------------------
;;; Standard Interfaces
;;; ------------------------------------------------------------

(define iface-expr
  (make-interface 'Expr '(lit add neg)))

(define iface-bool
  (make-interface 'Bool '(bool-lit and or not)))

(define iface-cmp
  (make-interface 'Cmp '(eq lt gt)))

(define iface-cond
  (make-interface 'Cond '(if)))

(define iface-state
  (make-interface 'State '(get put pure bind)))

;;; interface-union : Interface -> Interface -> Interface
;;; Combine two interfaces
(define (interface-union i1 i2)
  (make-interface
   (list (interface-name i1) (interface-name i2))
   (append (interface-operations i1)
           (filter (lambda (op)
                           (not (memq op (interface-operations i1))))
                   (interface-operations i2)))))

;;; ------------------------------------------------------------
;;; Modular Interpreter Construction
;;; ------------------------------------------------------------

;;; An interpreter factory creates a dictionary for a specific
;;; representation type. This enables modular interpreter definition.

;;; make-interpreter : Symbol -> (Unit -> Dict) -> Interpreter
(define (make-interpreter name make-dict-fn)
  (list 'interpreter name make-dict-fn))

;;; interpreter? : Any -> Boolean
(define (interpreter? x)
  (and (pair? x) (eq? (car x) 'interpreter)))

;;; interpreter-name : Interpreter -> Symbol
(define (interpreter-name interp) (cadr interp))

;;; interpreter-make : Interpreter -> Dict
(define (interpreter-make interp) ((caddr interp)))

;;; combine-interpreters : (List Interpreter) -> Dict
;;; Combine multiple interpreters into one dictionary
(define (combine-interpreters interps)
  (dict-concat (map interpreter-make interps)))

;;; ------------------------------------------------------------
;;; Algebra Transformers
;;; ------------------------------------------------------------

;;; An algebra transformer wraps a base algebra to add behavior.
;;; Examples: logging, caching, validation, profiling.

;;; make-transformer : Symbol -> (Dict -> Dict) -> Transformer
(define (make-transformer name transform-fn)
  (list 'transformer name transform-fn))

;;; transformer? : Any -> Boolean
(define (transformer? x)
  (and (pair? x) (eq? (car x) 'transformer)))

;;; transformer-name : Transformer -> Symbol
(define (transformer-name t) (cadr t))

;;; transformer-apply : Transformer -> Dict -> Dict
(define (transformer-apply t d) ((caddr t) d))

;;; chain-transformers : (List Transformer) -> Dict -> Dict
;;; Apply multiple transformers in sequence
(define (chain-transformers transformers d)
  (fold-left (lambda (dict trans)
                     (transformer-apply trans dict))
             d
             transformers))

;;; Standard transformers

(define (with-logging-default d)
  (with-logging
   (lambda (name args)
           (display (format "[~a] ~s\n" name args)))
   d))

(define logging-transformer
  (make-transformer 'logging with-logging-default))

(define memoizing-transformer
  (make-transformer 'memoizing with-memoization))

;;; ------------------------------------------------------------
;;; DSL Composition Patterns
;;; ------------------------------------------------------------

;;; Pattern 1: Extension
;;; Add new operations to an existing DSL

;;; extend-dsl : Dict -> (List (Symbol . Procedure)) -> Dict
(define (extend-dsl base-dict new-ops)
  (dict-extend base-dict new-ops))

;;; Pattern 2: Restriction
;;; Hide some operations from a DSL

;;; restrict-dsl : Dict -> (List Symbol) -> Dict
;;; Keep only the specified operations
(define (restrict-dsl d allowed-ops)
  (make-dict (dict-tag d)
             (filter (lambda (pair)
                             (memq (car pair) allowed-ops))
                     (dict-ops d))))

;;; Pattern 3: Renaming
;;; Rename operations in a DSL

;;; rename-dsl : Dict -> (List (Symbol . Symbol)) -> Dict
;;; Rename operations according to mapping
(define (rename-dsl d renamings)
  (make-dict (dict-tag d)
             (map (lambda (pair)
                          (let ([new-name (assq (car pair) renamings)])
                               (if new-name
                                   (cons (cdr new-name) (cdr pair))
                                   pair)))
                  (dict-ops d))))

;;; Pattern 4: Override
;;; Replace specific operations

;;; override-dsl : Dict -> (List (Symbol . Procedure)) -> Dict
(define (override-dsl d overrides)
  (make-dict (dict-tag d)
             (map (lambda (pair)
                          (let ([override (assq (car pair) overrides)])
                               (if override
                                   override
                                   pair)))
                  (dict-ops d))))

;;; ============================================================
;;; Integration: Free + Tagless Bridge
;;; ============================================================
;;;
;;; Convert between Free monad and Tagless final representations.
;;; This enables using both approaches together.

;;; ------------------------------------------------------------
;;; Generic Tagless -> Free Conversion
;;; ------------------------------------------------------------

;;; tagless->free : (Dict -> a) -> Dict -> a
;;; Convert a tagless program to Free representation using a provided
;;; AST-building dictionary. The dict-factory should return a dictionary
;;; whose operations build AST nodes instead of computing values.
(define (tagless->free program ast-dict)
  (program ast-dict))

;;; make-ast-dict : (List (Symbol . (args -> AST))) -> Dict
;;; Create an AST-building dictionary from operation specs.
;;; Each spec maps an operation name to a function that builds an AST node.
(define (make-ast-dict tag specs)
  (make-dict tag
             (map (lambda (spec)
                          (cons (car spec)
                                (cdr spec)))
                  specs)))

;;; Standard AST-building dictionary for expressions
(define expr-ast-dict
  (make-ast-dict 'expr-ast
                 `((lit . ,(lambda (n) (list 'lit n)))
                   (add . ,(lambda (x y) (list 'add x y)))
                   (neg . ,(lambda (x) (list 'neg x))))))

;;; tagless-program->free : (Dict -> a) -> Free F a
;;; Convert an expression program to Free representation.
;;; Convenience wrapper using the standard expr AST dict.
(define (tagless-program->free program)
  (tagless->free program expr-ast-dict))

;;; ------------------------------------------------------------
;;; Generic Free -> Tagless Conversion
;;; ------------------------------------------------------------

;;; free->tagless : (Term -> Dict -> a) -> Term -> (Dict -> a)
;;; Convert a Free term to a tagless program using a provided interpreter.
(define (free->tagless interpret-fn term)
  (lambda (d)
          (interpret-fn term d)))

;;; make-term-interpreter : (List (Symbol . (Dict -> Args -> Result))) -> (Term -> Dict -> Result)
;;; Create an interpreter from operation handlers.
(define (make-term-interpreter handlers)
  (lambda (term d)
          (term-interpret-with-handlers handlers term d)))

(define (term-interpret-with-handlers handlers term d)
  (cond
   [(pure-free? term)
    (from-pure-free term)]
   [(free-suspended? term)
    (term-interpret-with-handlers handlers (from-free term) d)]
   [(pair? term)
    (let ([handler (assq (car term) handlers)])
         (if handler
             ((cdr handler) d term
              (lambda (sub) (term-interpret-with-handlers handlers sub d)))
             (error 'term-interpret "Unknown term type" (car term))))]
   [else term]))

;;; Standard expression interpreter handlers
(define expr-interpret-handlers
  `((lit . ,(lambda (d term recurse)
                    (expr-lit d (cadr term))))
    (add . ,(lambda (d term recurse)
                    (expr-add d (recurse (cadr term)) (recurse (caddr term)))))
    (neg . ,(lambda (d term recurse)
                    (expr-neg d (recurse (cadr term)))))))

;;; free-term->tagless : Free F a -> (Dict -> a)
;;; Convert an expression term to a tagless program.
;;; Convenience wrapper using standard expr interpreter.
(define (free-term->tagless term)
  (free->tagless
   (make-term-interpreter expr-interpret-handlers)
   term))

;;; interpret-arith-term : Term -> Dict -> Result
;;; Direct interpreter for arithmetic terms (legacy compatibility)
(define (interpret-arith-term term d)
  ((make-term-interpreter expr-interpret-handlers) term d))

;;; ============================================================
;;; Integration: Effect + Tagless Bridge
;;; ============================================================
;;;
;;; Run tagless programs with effect handlers.

;;; run-tagless-with-effects : Dict -> (Dict -> Eff e a) -> Eff e a
;;; Run a tagless program that produces effects
(define (run-tagless-with-effects dict program)
  (program dict))

;;; make-effectful-dict : Dict -> (Op -> Eff e Result) -> Dict
;;; Wrap a dictionary to produce effects
(define (make-effectful-dict base-dict wrap-op)
  (make-dict (list 'effectful (dict-tag base-dict))
             (map (lambda (pair)
                          (cons (car pair)
                                (lambda args
                                        (wrap-op (car pair)
                                                 (apply (cdr pair) args)))))
                  (dict-ops base-dict))))

;;; with-logging-effects : Dict -> Dict
;;; Wrap dictionary operations to log via Writer effect
(define (with-logging-effects d)
  (make-effectful-dict d
                       (lambda (op-name result)
                               (eff-bind (writer-tell (list 'op op-name result))
                                         (lambda (_) (eff-return result))))))

;;; ============================================================
;;; Composition Verification
;;; ============================================================

;;; verify-composition : Dict -> (List Interface) -> Boolean
;;; Verify that a composed dictionary satisfies all interfaces
(define (verify-composition d interfaces)
  (let loop ([ifaces interfaces])
       (or (null? ifaces)
           (and (dict-satisfies? d (car ifaces))
                (loop (cdr ifaces))))))

;;; composition-report : Dict -> (List Interface) -> String
;;; Generate a report of which interfaces are satisfied
(define (composition-report d interfaces)
  (let ([results (map (lambda (iface)
                              (list (interface-name iface)
                                    (dict-satisfies? d iface)))
                      interfaces)])
       (apply string-append
              (map (lambda (r)
                           (format "~a: ~a\n"
                                   (car r)
                                   (if (cadr r) "satisfied" "MISSING")))
                   results))))

;;; ============================================================
;;; Example: Complete Composition
;;; ============================================================
;;;
;;; Demonstrate combining Arithmetic + State + Logging

;;; Define individual interpreters
(define arith-eval-interp
  (make-interpreter 'arith-eval
                    (lambda ()
                            (make-expr-dict
                             (lambda (n) n)
                             (lambda (x y) (+ x y))
                             (lambda (x) (- x))))))

(define arith-pretty-interp
  (make-interpreter 'arith-pretty
                    (lambda ()
                            (make-expr-dict
                             (lambda (n) (number->string n))
                             (lambda (x y) (string-append "(" x " + " y ")"))
                             (lambda (x) (string-append "(-" x ")"))))))

;;; Combine for a full language
(define (make-full-language-dict mode)
  (case mode
        [(eval) (combine-interpreters (list arith-eval-interp))]
        [(pretty) (combine-interpreters (list arith-pretty-interp))]
        [else (error 'make-full-language-dict "Unknown mode" mode)]))

;;; ============================================================
;;; Summary
;;; ============================================================
;;;
;;; This module provides three ways to compose DSLs:
;;;
;;; 1. Data Types à la Carte:
;;;    - inl, inr for coproduct injection
;;;    - make-coproduct-fmap for combined functors
;;;    - functor-row for type-safe injection
;;;    - lift-* functions for Free monad operations
;;;
;;; 2. Effect Composition:
;;;    - make-handler-stack for handler stacking
;;;    - run-with-stack for execution
;;;    - Combined handlers: state+reader, state+writer, state+exception
;;;    - Effect row operations for modular reasoning
;;;
;;; 3. Tagless Composition:
;;;    - dict-merge, dict-concat for dictionary combination
;;;    - make-interface, dict-satisfies? for constraints
;;;    - make-interpreter, combine-interpreters for modular construction
;;;    - make-transformer, chain-transformers for cross-cutting concerns
;;;    - extend-dsl, restrict-dsl, rename-dsl, override-dsl for adaptation
;;;
;;; Bridges:
;;;    - tagless-program->free, free-term->tagless
;;;    - run-tagless-with-effects, make-effectful-dict
