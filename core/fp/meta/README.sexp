((name "meta")
 (description "Metaprogramming utilities, DSL construction tools, and
  logic programming primitives. Provides the building blocks for
  creating embedded domain-specific languages.")

 (modules
  ((file "combinators.ss")
   (purpose "Classic functional combinators and utility types")
   (exports
    ;; Function combinators
    (id "Identity function: (id x) => x")
    (const "Constant function: ((const x) y) => x")
    (flip "Flip arguments: ((flip f) x y) => (f y x)")
    (compose "Function composition: ((compose f g) x) => (f (g x))")
    (pipe "Left-to-right composition: ((pipe f g) x) => (g (f x))")
    (curry "Convert (f x y) to ((curry f) x) y")
    (uncurry "Convert ((f x) y) to (uncurry f) (x . y)")
    (on "Binary combinator: ((on f g) x y) => (f (g x) (g y))")
    ;; Maybe type
    (nothing "The absent value")
    (just "Wrap a value: (just x)")
    (nothing? "Test for nothing")
    (just? "Test for just")
    (from-just "Extract value or error")
    (maybe-default "Extract value or return default")
    (maybe-map "Map over Maybe: (maybe-map f (just x)) => (just (f x))")
    (maybe-bind "Monadic bind for Maybe")
    ;; Either type
    (left "Left (error) case: (left e)")
    (right "Right (success) case: (right x)")
    (left? "Test for left")
    (right? "Test for right")
    (either "Case analysis: (either f g e)")
    (either-map "Map over right value")
    (either-bind "Monadic bind for Either")
    ;; List utilities
    (foldl "Left fold")
    (foldr "Right fold")
    (filter "Filter by predicate")
    (take "Take first n elements")
    (drop "Drop first n elements")
    (zip "Zip two lists")
    (zip-with "Zip with combining function")
    ;; Monad utilities
    (do-monad "Macro for monadic do-notation"))
   (example
    ";; Compose functions right-to-left
     ((compose add1 (* 2)) 3)  ;; => 7

     ;; Safe division with Maybe
     (define (safe-div x y)
       (if (zero? y) (nothing) (just (/ x y))))
     (maybe-bind (safe-div 10 2)
                 (lambda (x) (safe-div x 0)))
     ;; => (nothing)"))

  ((file "dsl.ss")
   (purpose "DSL construction toolkit using free monads")
   (exports
    (make-instruction "Create a DSL instruction type")
    (dsl-emit "Emit an instruction in a DSL program")
    (dsl-request "Emit instruction and capture response")
    (dsl-pure "Lift pure value into DSL")
    (dsl-bind "Sequence DSL operations")
    (dsl-do "Macro for DSL do-notation")
    (make-interpreter "Create an interpreter from handlers")
    (run-dsl "Execute DSL program with interpreter")
    (dsl-expr "Build expression nodes")
    (dsl-stmt "Build statement nodes")
    (dsl-block "Build block nodes"))
   (example
    ";; Define a simple logging DSL
     (define log-instr (make-instruction 'log))
     (define (log-msg msg) (dsl-emit (log-instr msg)))

     (define prog
       (dsl-do
         (log-msg \"Starting\")
         (log-msg \"Working\")
         (dsl-pure 'done)))

     (run-dsl (make-interpreter
                `((log . ,(lambda (msg k)
                            (display msg) (newline)
                            (k 'logged)))))
              prog)"))

  ((file "logic.ss")
   (purpose "Logic programming primitives (miniKanren-style)")
   (exports
    (make-lvar "Create a logic variable")
    (lvar? "Test for logic variable")
    (walk "Walk a term through substitution")
    (unify "Unify two terms, returning extended substitution or #f")
    (empty-subst "The empty substitution")
    (== "Goal: unify two terms")
    (succeed "Goal that always succeeds")
    (fail "Goal that always fails")
    (conj "Conjunction of goals (and)")
    (disj "Disjunction of goals (or)")
    (call/fresh "Introduce a fresh logic variable")
    (run* "Run goal and return all solutions")
    (run "Run goal and return first n solutions")
    (conde "Macro for multiple clauses (or of ands)")
    (fresh "Macro for introducing multiple fresh variables")
    ;; Common relations
    (appendo "Relational append")
    (membero "Relational membership")
    (reverseo "Relational reverse"))
   (example
    ";; Find pairs that sum to 5
     (run* (q)
       (fresh (x y)
         (== q (list x y))
         (conde
           ((== x 1) (== y 4))
           ((== x 2) (== y 3))
           ((== x 3) (== y 2)))))
     ;; => ((1 4) (2 3) (3 2))

     ;; Relational append
     (run* (q) (appendo '(a b) '(c d) q))
     ;; => ((a b c d))

     ;; Run backwards
     (run* (q) (appendo q '(c d) '(a b c d)))
     ;; => ((a b))")))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("core/fp/control/free.ss" "Free monad for DSL"))

 (notes
  "The meta module provides three complementary approaches to abstraction:

   1. Combinators: Point-free function manipulation
   2. DSL toolkit: Build interpreters using free monads
   3. Logic programming: Declarative relational programming

   Logic programming is particularly powerful for:
   - Running programs 'backwards' (synthesis from specification)
   - Non-deterministic search
   - Constraint satisfaction"))
