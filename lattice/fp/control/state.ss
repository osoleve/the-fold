;;; fabric/stitches/fp/state.ss — State Monad
;;;
;;; The State monad threads state through a computation without explicit
;;; passing. It enables pure, composable stateful computations.
;;;
;;; State s a = s -> (a, s)
;;;
;;; A State computation takes an initial state and returns a value
;;; along with a (possibly modified) state.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Core State operations (run-state, get, put, modify)
;;;   - Monad operations (state-pure, state-bind, state-map)
;;;   - Convenience (eval-state, exec-state)
;;;   - State combinators (gets, state-sequence, state-map-m)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")

;;; ============================================================
;;; State Representation
;;; ============================================================
;;;
;;; State s a is represented as a tagged function:
;;;   ('state . (s -> (a . s)))
;;;
;;; The function takes a state and returns a pair of (value . new-state).

;;; make-state : (s -> (a . s)) -> State s a
;;; Create a State computation from a function.
(define (make-state run-fn)
  (cons 'state run-fn))

;;; state? : Any -> Boolean
(define (state? x)
  (and (pair? x) (eq? (car x) 'state)))

;;; state-fn : State s a -> (s -> (a . s))
;;; Extract the underlying function.
(define (state-fn st)
  (cdr st))

;;; ============================================================
;;; Running State Computations
;;; ============================================================

;;; run-state : State s a -> s -> (a . s)
;;; Run a State computation with an initial state.
;;; Returns a pair of (result . final-state).
(define (run-state st initial-state)
  ((state-fn st) initial-state))

;;; eval-state : State s a -> s -> a
;;; Run a State computation and return only the result.
(define (eval-state st initial-state)
  (car (run-state st initial-state)))

;;; exec-state : State s a -> s -> s
;;; Run a State computation and return only the final state.
(define (exec-state st initial-state)
  (cdr (run-state st initial-state)))

;;; ============================================================
;;; Core State Operations
;;; ============================================================

;;; get : State s s
;;; Get the current state.
(define state-get
  (make-state (lambda (s) (cons s s))))

;;; put : s -> State s ()
;;; Replace the current state.
(define (state-put new-state)
  (make-state (lambda (s) (cons '() new-state))))

;;; modify : (s -> s) -> State s ()
;;; Modify the state using a function.
(define (state-modify f)
  (make-state (lambda (s) (cons '() (f s)))))

;;; gets : (s -> a) -> State s a
;;; Get a value derived from the current state.
(define (state-gets f)
  (make-state (lambda (s) (cons (f s) s))))

;;; ============================================================
;;; Monad Operations
;;; ============================================================

;;; state-pure : a -> State s a
;;; Lift a value into State (return/pure).
(define (state-pure x)
  (make-state (lambda (s) (cons x s))))

;;; state-bind : State s a -> (a -> State s b) -> State s b
;;; Sequence two State computations (>>=).
(define (state-bind st f)
  (make-state
   (lambda (s)
           (let* ([result (run-state st s)]
                  [a (car result)]
                  [s2 (cdr result)])
                 (run-state (f a) s2)))))

;;; state-then : State s a -> State s b -> State s b
;;; Sequence two State computations, discarding the first result (>>).
(define (state-then st1 st2)
  (state-bind st1 (lambda (_) st2)))

;;; state-map : (a -> b) -> State s a -> State s b
;;; Map a function over the result of a State computation.
(define (state-map f st)
  (state-bind st (lambda (a) (state-pure (f a)))))

;;; state-ap : State s (a -> b) -> State s a -> State s b
;;; Apply a State-wrapped function to a State-wrapped value.
(define (state-ap st-f st-a)
  (state-bind st-f
              (lambda (f)
                      (state-bind st-a
                                  (lambda (a)
                                          (state-pure (f a)))))))

;;; ============================================================
;;; State Combinators
;;; ============================================================

;;; state-sequence : (List (State s a)) -> State s (List a)
;;; Sequence a list of State computations, collecting results.
(define (state-sequence states)
  (if (null? states)
      (state-pure '())
      (state-bind (car states)
                  (lambda (x)
                          (state-bind (state-sequence (cdr states))
                                      (lambda (xs)
                                              (state-pure (cons x xs))))))))

;;; state-map-m : (a -> State s b) -> (List a) -> State s (List b)
;;; Map a State-returning function over a list.
(define (state-map-m f lst)
  (state-sequence (map f lst)))

;;; state-for-each : (List a) -> (a -> State s ()) -> State s ()
;;; Execute a State action for each element (for side effects).
(define (state-for-each lst f)
  (if (null? lst)
      (state-pure '())
      (state-then (f (car lst))
                  (state-for-each (cdr lst) f))))

;;; state-when : Boolean -> State s () -> State s ()
;;; Execute State action only when condition is true.
(define (state-when condition action)
  (if condition
      action
      (state-pure '())))

;;; state-unless : Boolean -> State s () -> State s ()
;;; Execute State action only when condition is false.
(define (state-unless condition action)
  (state-when (not condition) action))

;;; ============================================================
;;; State with Lenses
;;; ============================================================
;;;
;;; These combinators work with lenses from fp/lens.ss
;;; to focus on parts of the state.

;;; state-view : Lens s a -> State s a
;;; View a part of the state through a lens.
;;; Requires: (load "lattice/fp/optics/lens.ss")
(define (state-view lens)
  (state-gets (lambda (s) (view lens s))))

;;; state-set : Lens s a -> a -> State s ()
;;; Set a part of the state through a lens.
;;; Requires: (load "lattice/fp/optics/lens.ss")
(define (state-set lens val)
  (state-modify (lambda (s) (set lens val s))))

;;; state-over : Lens s a -> (a -> a) -> State s ()
;;; Modify a part of the state through a lens.
;;; Requires: (load "lattice/fp/optics/lens.ss")
(define (state-over lens f)
  (state-modify (lambda (s) (over lens f s))))

;;; ============================================================
;;; Labeled State (Named State Monad)
;;; ============================================================
;;;
;;; For computations with multiple named state components,
;;; use an alist as state with key-lens from lens.ss.

;;; state-get-key : k -> State (Alist k v) (Maybe v)
;;; Get a value by key from an alist state.
(define (state-get-key key)
  (state-gets (lambda (alist)
                      (let ([pair (assoc key alist)])
                           (if pair (just (cdr pair)) nothing)))))

;;; state-put-key : k -> v -> State (Alist k v) ()
;;; Set a value by key in an alist state.
(define (state-put-key key val)
  (state-modify (lambda (alist)
                        (let ([new-pair (cons key val)])
                             (let loop ([lst alist] [acc '()])
                                  (cond
                                   [(null? lst) (reverse (cons new-pair acc))]
                                   [(equal? (caar lst) key)
                                    (append (reverse acc) (cons new-pair (cdr lst)))]
                                   [else (loop (cdr lst) (cons (car lst) acc))]))))))

;;; state-modify-key : k -> (v -> v) -> v -> State (Alist k v) ()
;;; Modify a value by key, using default if key doesn't exist.
(define (state-modify-key key f default)
  (state-bind (state-get-key key)
              (lambda (maybe-val)
                      (let ([val (if (just? maybe-val)
                                     (from-just maybe-val)
                                     default)])
                           (state-put-key key (f val))))))

;;; ============================================================
;;; Counter State (Common Pattern)
;;; ============================================================

;;; state-inc : State Nat Nat
;;; Increment counter state, return old value.
(define state-inc
  (state-bind state-get
              (lambda (n)
                      (state-then (state-put (+ n 1))
                                  (state-pure n)))))

;;; state-dec : State Nat Nat
;;; Decrement counter state, return old value.
(define state-dec
  (state-bind state-get
              (lambda (n)
                      (state-then (state-put (- n 1))
                                  (state-pure n)))))

;;; state-add : Nat -> State Nat ()
;;; Add to counter state.
(define (state-add n)
  (state-modify (lambda (x) (+ x n))))

;;; fresh : State Nat Nat
;;; Generate a fresh unique number.
(define fresh state-inc)

;;; ============================================================
;;; Stack State (Common Pattern)
;;; ============================================================

;;; state-push : a -> State (List a) ()
;;; Push a value onto a stack state.
(define (state-push x)
  (state-modify (lambda (stack) (cons x stack))))

;;; state-pop : State (List a) (Maybe a)
;;; Pop a value from a stack state.
(define state-pop
  (state-bind state-get
              (lambda (stack)
                      (if (null? stack)
                          (state-pure nothing)
                          (state-then (state-put (cdr stack))
                                      (state-pure (just (car stack))))))))

;;; state-peek : State (List a) (Maybe a)
;;; Peek at the top of a stack state.
(define state-peek
  (state-gets (lambda (stack)
                      (if (null? stack)
                          nothing
                          (just (car stack))))))

;;; ============================================================
;;; Accumulator State (Common Pattern - O(1) Writer Pattern)
;;; ============================================================
;;;
;;; Performance note: This implementation uses cons-based accumulation
;;; for O(1) single-element appends, but requires O(N) reversal when
;;; reading the final result. For write-heavy workloads, this is much
;;; faster than repeated O(N) appends.
;;;
;;; The accumulated list is stored in reverse order internally.
;;; Use `state-get-log` to retrieve in correct order.

;;; state-tell : a -> State (List a) ()
;;; Append a value to an accumulator (like Writer).
;;; O(1) operation using cons-based accumulation.
(define (state-tell x)
  (state-modify (lambda (acc) (cons x acc))))

;;; state-tell-all : (List a) -> State (List a) ()
;;; Append all values to an accumulator.
;;; O(N) where N is length of xs.
(define (state-tell-all xs)
  (state-modify (lambda (acc)
                        ;; Add xs in reverse order to maintain final order after reversal
                        (let loop ([remaining xs] [result acc])
                             (if (null? remaining)
                                 result
                                 (loop (cdr remaining) (cons (car remaining) result)))))))

;;; state-listen : State (List a) (List a)
;;; Get the current accumulator in correct order.
;;; O(N) reversal to restore order.
(define state-listen
  (state-bind state-get
              (lambda (acc) (state-pure (reverse acc)))))

;;; state-get-log : State (List a) (List a)
;;; Alias for state-listen - get accumulator in correct order.
(define state-get-log state-listen)

;;; state-clear : State (List a) (List a)
;;; Clear the accumulator, returning old value in correct order.
;;; O(N) reversal to restore order.
(define state-clear
  (state-bind state-get
              (lambda (acc)
                      (state-then (state-put '())
                                  (state-pure (reverse acc))))))

;;; ============================================================
;;; do-notation Simulation
;;; ============================================================
;;;
;;; Since we don't have macros, we use explicit bind chains.
;;; The pattern is:
;;;
;;; (state-bind action1
;;;             (lambda (x)
;;;                     (state-bind action2
;;;                                 (lambda (y)
;;;                                         (state-pure (f x y))))))
;;;
;;; Or using state-let* helper:

;;; state-let* helper not possible without macros, but we can
;;; use nested lambdas as shown above.

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Simple counter
;;; (define count-to-3
;;;   (state-bind fresh
;;;               (lambda (a)
;;;                       (state-bind fresh
;;;                                   (lambda (b)
;;;                                           (state-bind fresh
;;;                                                       (lambda (c)
;;;                                                               (state-pure (list a b c)))))))))
;;; (eval-state count-to-3 0)  ; => (0 1 2)
;;; (exec-state count-to-3 0)  ; => 3
;;;
;;; ;; Stack operations
;;; (define stack-ops
;;;   (state-then (state-push 1)
;;;               (state-then (state-push 2)
;;;                           (state-then (state-push 3)
;;;                                       state-pop))))
;;; (run-state stack-ops '())  ; => ((just . 3) . (2 1))
;;;
;;; ;; Accumulator (Writer pattern)
;;; (define log-ops
;;;   (state-then (state-tell "started")
;;;               (state-then (state-tell "working")
;;;                           (state-then (state-tell "done")
;;;                                       state-listen))))
;;; (eval-state log-ops '())  ; => ("started" "working" "done")
