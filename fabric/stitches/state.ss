;;; fabric/stitches/state.ss — The State Monad
;;;
;;; Pure state threading for The Fold.
;;;
;;; State computations are functions: State → (Value × State)
;;; We represent pairs as 2-element lists for simplicity.
;;;
;;; This module provides:
;;;   - State monad combinators (return, bind, get, put, modify)
;;;   - State monad runners (run-state, eval-state, exec-state)
;;;   - Utility combinators (sequence, traverse, etc.)
;;;
;;; The State monad enables:
;;;   - Game state management without mutation
;;;   - Pure simulation loops
;;;   - Deterministic replay (same input state = same output)
;;;
;;; Type: State s a = s → (a × s)
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "prelude.ss")

;;; ============================================================
;;; State Monad Primitives (as Fold expressions)
;;; ============================================================

;;; These definitions are Fold expressions that can be evaluated
;;; by the eval.ss evaluator. They're defined here as quoted
;;; S-expressions for inclusion in the prelude.

;;; state-return : a → State s a
;;; Wrap a value in a state computation that passes state through.
(define state-return-def
  '(state-return . (fn (a)
                     (fn (s) (prim 'list a s)))))

;;; state-bind : State s a → (a → State s b) → State s b
;;; Sequence two state computations, threading state.
(define state-bind-def
  '(state-bind . (fn (ma)
                   (fn (f)
                     (fn (s)
                       (let ((result (ma s)))
                         (let ((a (prim 'car result))
                               (s1 (prim 'car (prim 'cdr result))))
                           ((f a) s1))))))))

;;; state-get : State s s
;;; Read the current state.
(define state-get-def
  '(state-get . (fn (s) (prim 'list s s))))

;;; state-put : s → State s ()
;;; Replace the current state.
(define state-put-def
  '(state-put . (fn (new-s)
                  (fn (old-s) (prim 'list '() new-s)))))

;;; state-modify : (s → s) → State s ()
;;; Apply a function to the current state.
(define state-modify-def
  '(state-modify . (fn (f)
                     (fn (s) (prim 'list '() (f s))))))

;;; state-gets : (s → a) → State s a
;;; Extract a value from the current state.
(define state-gets-def
  '(state-gets . (fn (f)
                   (fn (s) (prim 'list (f s) s)))))

;;; ============================================================
;;; State Runners
;;; ============================================================

;;; run-state : State s a → s → (a × s)
;;; Run a state computation with an initial state.
(define run-state-def
  '(run-state . (fn (ma)
                  (fn (init-s)
                    (ma init-s)))))

;;; eval-state : State s a → s → a
;;; Run and return only the final value.
(define eval-state-def
  '(eval-state . (fn (ma)
                   (fn (init-s)
                     (prim 'car (ma init-s))))))

;;; exec-state : State s a → s → s
;;; Run and return only the final state.
(define exec-state-def
  '(exec-state . (fn (ma)
                   (fn (init-s)
                     (prim 'car (prim 'cdr (ma init-s)))))))

;;; ============================================================
;;; State Monad Combinators
;;; ============================================================

;;; state-map : (a → b) → State s a → State s b
;;; Apply a function to the result of a state computation.
(define state-map-def
  '(state-map . (fn (f)
                  (fn (ma)
                    (fn (s)
                      (let ((result (ma s)))
                        (let ((a (prim 'car result))
                              (s1 (prim 'car (prim 'cdr result))))
                          (prim 'list (f a) s1))))))))

;;; state-ap : State s (a → b) → State s a → State s b
;;; Applicative application for state computations.
(define state-ap-def
  '(state-ap . (fn (mf)
                 (fn (ma)
                   (fn (s)
                     (let ((rf (mf s)))
                       (let ((f (prim 'car rf))
                             (s1 (prim 'car (prim 'cdr rf))))
                         (let ((ra (ma s1)))
                           (let ((a (prim 'car ra))
                                 (s2 (prim 'car (prim 'cdr ra))))
                             (prim 'list (f a) s2))))))))))

;;; state-join : State s (State s a) → State s a
;;; Flatten nested state computations.
(define state-join-def
  '(state-join . (fn (mma)
                   (fn (s)
                     (let ((r1 (mma s)))
                       (let ((ma (prim 'car r1))
                             (s1 (prim 'car (prim 'cdr r1))))
                         (ma s1)))))))

;;; ============================================================
;;; Utility: Do-notation via nested binds
;;; ============================================================

;;; For convenience, we can chain operations:
;;; (state-do ((x <- m1) (y <- m2)) (state-return (+ x y)))
;;;
;;; This is sugar that expands to:
;;; ((state-bind m1) (fn (x) ((state-bind m2) (fn (y) (state-return (+ x y))))))
;;;
;;; Note: This requires a macro, which we don't have yet.
;;; For now, use explicit bind chains.

;;; ============================================================
;;; State Monad Laws
;;; ============================================================

;;; The state monad satisfies the monad laws:
;;;
;;; 1. Left identity:  (bind (return a) f) ≡ (f a)
;;; 2. Right identity: (bind m return) ≡ m
;;; 3. Associativity:  (bind (bind m f) g) ≡ (bind m (λx. bind (f x) g))
;;;
;;; These can be verified by evaluation.

;;; ============================================================
;;; Practical Combinators
;;; ============================================================

;;; state-sequence : List (State s a) → State s (List a)
;;; Run a list of state computations in sequence.
(define state-sequence-def
  '(state-sequence . (fix seq (fn (ms)
                        (if (prim 'null? ms)
                            (fn (s) (prim 'list '() s))
                            ((state-bind (prim 'car ms))
                             (fn (x)
                               ((state-map (fn (xs) (prim 'cons x xs)))
                                (seq (prim 'cdr ms))))))))))

;;; state-traverse : (a → State s b) → List a → State s (List b)
;;; Map a stateful function over a list.
(define state-traverse-def
  '(state-traverse . (fix trav (fn (f xs)
                        (if (prim 'null? xs)
                            (fn (s) (prim 'list '() s))
                            ((state-bind (f (prim 'car xs)))
                             (fn (y)
                               ((state-map (fn (ys) (prim 'cons y ys)))
                                (trav f (prim 'cdr xs))))))))))

;;; state-replicate : Nat → State s a → State s (List a)
;;; Run a computation n times, collecting results.
;;; Curried: (state-replicate n) returns a function waiting for ma.
(define state-replicate-def
  '(state-replicate . (fn (n)
                        (fn (ma)
                          (let ((go (fix go (fn (remaining)
                                       (if (prim 'zero? remaining)
                                           (fn (s) (prim 'list '() s))
                                           ((state-bind ma)
                                            (fn (x)
                                              ((state-map (fn (xs) (prim 'cons x xs)))
                                               (go (prim 'sub remaining 1))))))))))
                            (go n))))))

;;; state-when : Bool → State s () → State s ()
;;; Conditionally run a state computation.
(define state-when-def
  '(state-when . (fn (cond)
                   (fn (action)
                     (if cond
                         action
                         (fn (s) (prim 'list '() s)))))))

;;; state-unless : Bool → State s () → State s ()
;;; Run a state computation unless condition is true.
(define state-unless-def
  '(state-unless . (fn (cond)
                     (fn (action)
                       (if cond
                           (fn (s) (prim 'list '() s))
                           action)))))

;;; ============================================================
;;; Stateful Counter Example
;;; ============================================================

;;; inc-counter : State Int Int
;;; Increment a counter and return the old value.
(define inc-counter-def
  '(inc-counter . (fn (n)
                    (prim 'list n (prim 'add n 1)))))

;;; dec-counter : State Int Int
;;; Decrement a counter and return the old value.
(define dec-counter-def
  '(dec-counter . (fn (n)
                    (prim 'list n (prim 'sub n 1)))))

;;; ============================================================
;;; Game State Example: Position
;;; ============================================================

;;; A game state might be a position: (x y)
;;; These combinators work with such state.

;;; move-right : Int → State (x y) ()
(define move-right-def
  '(move-right . (fn (dx)
                   (fn (pos)
                     (let ((x (prim 'car pos))
                           (y (prim 'car (prim 'cdr pos))))
                       (prim 'list '() (prim 'list (prim 'add x dx) y)))))))

;;; move-up : Int → State (x y) ()
(define move-up-def
  '(move-up . (fn (dy)
                (fn (pos)
                  (let ((x (prim 'car pos))
                        (y (prim 'car (prim 'cdr pos))))
                    (prim 'list '() (prim 'list x (prim 'add y dy))))))))

;;; get-x : State (x y) Int
(define get-x-def
  '(get-x . (fn (pos)
              (prim 'list (prim 'car pos) pos))))

;;; get-y : State (x y) Int
(define get-y-def
  '(get-y . (fn (pos)
              (prim 'list (prim 'car (prim 'cdr pos)) pos))))

;;; ============================================================
;;; All State Definitions (for prelude)
;;; ============================================================

(define state-prelude-defs
  (list
    state-return-def
    state-bind-def
    state-get-def
    state-put-def
    state-modify-def
    state-gets-def
    run-state-def
    eval-state-def
    exec-state-def
    state-map-def
    state-ap-def
    state-join-def
    state-sequence-def
    state-traverse-def
    state-replicate-def
    state-when-def
    state-unless-def
    inc-counter-def
    dec-counter-def
    move-right-def
    move-up-def
    get-x-def
    get-y-def))

;;; ============================================================
;;; Type Signatures
;;; ============================================================

;;; For type checking, these are the expected types:
;;;
;;; state-return  : ∀ s a. a → (s → (a × s))
;;; state-bind    : ∀ s a b. (s → (a × s)) → (a → s → (b × s)) → (s → (b × s))
;;; state-get     : ∀ s. s → (s × s)
;;; state-put     : ∀ s. s → (s → (() × s))
;;; state-modify  : ∀ s. (s → s) → (s → (() × s))
;;; state-gets    : ∀ s a. (s → a) → (s → (a × s))
;;; run-state     : ∀ s a. (s → (a × s)) → s → (a × s)
;;; eval-state    : ∀ s a. (s → (a × s)) → s → a
;;; exec-state    : ∀ s a. (s → (a × s)) → s → s
;;; state-map     : ∀ s a b. (a → b) → (s → (a × s)) → (s → (b × s))
;;; state-ap      : ∀ s a b. (s → ((a → b) × s)) → (s → (a × s)) → (s → (b × s))
;;; state-join    : ∀ s a. (s → ((s → (a × s)) × s)) → (s → (a × s))

;;; ============================================================
;;; Integration with eval.ss
;;; ============================================================

;;; To use these in the evaluator, add state-prelude-defs to
;;; the prelude-defs list in eval.ss, or build a separate
;;; state-env using build-prelude-env pattern.

;;; Example usage (in Fold language):
;;;
;;; ;; Increment counter 3 times
;;; (let ((program
;;;        ((state-bind inc-counter)
;;;         (fn (a)
;;;           ((state-bind inc-counter)
;;;            (fn (b)
;;;              ((state-bind inc-counter)
;;;               (fn (c)
;;;                 (state-return (prim 'list a b c))))))))))
;;;   ((run-state program) 0))
;;;
;;; ;; Returns: ((0 1 2) 3)
;;; ;; - Results collected: 0, 1, 2 (old values before each increment)
;;; ;; - Final state: 3 (counter after 3 increments)
