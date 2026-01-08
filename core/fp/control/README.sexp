((name "control")
 (description "Effect systems, continuations, and monadic control flow constructs.
  This module provides algebraic effects via the Eff monad, delimited continuations,
  free monads for building interpreters, and the state monad.")

 (modules
  ((file "effects.ss")
   (purpose "Algebraic effects via the Eff monad")
   (exports
    (make-effect "Create a named effect type")
    (perform "Raise an effect to be handled by an enclosing handler")
    (handle "Install an effect handler around a computation")
    (pure-eff "Wrap a pure value in the Eff monad")
    (eff-bind "Sequence effectful computations (>>=)"))
   (example
    "(define read-effect (make-effect 'read))
     (define (read-line) (perform read-effect 'line))
     (handle (lambda (eff k)
               (if (eq? (effect-name eff) 'read)
                   (k \"hello\")
                   (perform eff)))
             (read-line))"))

  ((file "continuation.ss")
   (purpose "First-class and delimited continuations")
   (exports
    (call/cc "Call with current continuation (undelimited)")
    (reset "Establish a continuation delimiter (prompt)")
    (shift "Capture the continuation up to the nearest reset")
    (make-prompt "Create a named prompt tag")
    (push-prompt "Push a prompt onto the control stack")
    (take-subcont "Capture continuation up to a prompt"))
   (example
    "(reset
      (+ 1 (shift k (k (k 5)))))
     ;; => 7 (applies continuation twice)"))

  ((file "free.ss")
   (purpose "Free monads for building interpreters and DSLs")
   (exports
    (pure-free "Lift a pure value into Free")
    (free "Construct a Free computation from a functor value")
    (free-bind "Sequence Free computations")
    (free-map "Map over the result of a Free computation")
    (run-free "Interpret a Free computation with a handler"))
   (example
    "(define (emit x) (free (list 'emit x)))
     (define prog (free-bind (emit \"hello\")
                             (lambda (_) (emit \"world\"))))
     (run-free (lambda (f k)
                 (display (cadr f))
                 (k 'done))
               prog)"))

  ((file "state.ss")
   (purpose "State monad for threading state through computations")
   (exports
    (state-return "Wrap a value in the State monad")
    (state-bind "Sequence stateful computations")
    (state-get "Retrieve the current state")
    (state-put "Replace the current state")
    (state-modify "Apply a function to the current state")
    (run-state "Execute a stateful computation with initial state")
    (eval-state "Run and return only the result")
    (exec-state "Run and return only the final state"))
   (example
    "(define counter
       (state-bind (state-get)
                   (lambda (n)
                     (state-bind (state-put (+ n 1))
                                 (lambda (_) (state-return n))))))
     (run-state counter 0)  ;; => (0 . 1)"))

  ((file "random-effect.ss")
   (purpose "Random number generation as an algebraic effect")
   (exports
    (random-effect "The random number generation effect")
    (random-int "Request a random integer in range")
    (random-choice "Choose randomly from a list")
    (run-random "Handle random effect with a PRNG state"))))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("core/fp/meta/combinators.ss" "Function composition"))

 (notes
  "The Eff monad provides a principled way to handle side effects.
   Effects are first-class values that can be handled by different interpreters,
   enabling dependency injection and testability.

   Free monads are useful for building DSLs where you want to separate
   syntax (the program structure) from semantics (the interpretation)."))
