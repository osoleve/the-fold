;;; lattice/fp/control/state.ss — State Monad
;;; @module state
;;; @requires prelude combinators

(require 'prelude)
(require 'combinators)

(doc 'module 'state)
(doc 'description "The State monad threads state through a computation without explicit passing, enabling pure composable stateful computations.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'type "State s a = s -> (a, s)")
(doc 'note "A State computation takes an initial state and returns a value along with a possibly modified state")

(doc 'section 'state-representation)
(doc 'note "State s a is represented as a tagged function: ('state . (s -> (a . s)))")

(define (make-state run-fn)
  (doc 'export #t)
  (doc 'type '(-> (-> s (Pair a s)) (State s a)))
  (doc 'description "Create a State computation from a function")
  (cons 'state run-fn))

(define (state? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'state)))

(define (state-fn st)
  (doc 'export #t)
  (doc 'type '(-> (State s a) (-> s (Pair a s))))
  (doc 'description "Extract the underlying function")
  (cdr st))

(doc 'section 'running-state)

(define (run-state st initial-state)
  (doc 'export #t)
  (doc 'type '(-> (State s a) s (Pair a s)))
  (doc 'description "Run a State computation with an initial state, returns (result . final-state)")
  ((state-fn st) initial-state))

(define (eval-state st initial-state)
  (doc 'export #t)
  (doc 'type '(-> (State s a) s a))
  (doc 'description "Run a State computation and return only the result")
  (car (run-state st initial-state)))

(define (exec-state st initial-state)
  (doc 'export #t)
  (doc 'type '(-> (State s a) s s))
  (doc 'description "Run a State computation and return only the final state")
  (cdr (run-state st initial-state)))

(doc 'section 'core-state-operations)

(define state-get
  (make-state (lambda (s) (cons s s))))
(doc state-get 'type '(State s s))
(doc state-get 'description "Get the current state")

(define (state-put new-state)
  (doc 'export #t)
  (doc 'type '(-> s (State s ())))
  (doc 'description "Replace the current state")
  (make-state (lambda (s) (cons '() new-state))))

(define (state-modify f)
  (doc 'export #t)
  (doc 'type '(-> (-> s s) (State s ())))
  (doc 'description "Modify the state using a function")
  (make-state (lambda (s) (cons '() (f s)))))

(define (state-gets f)
  (doc 'export #t)
  (doc 'type '(-> (-> s a) (State s a)))
  (doc 'description "Get a value derived from the current state")
  (make-state (lambda (s) (cons (f s) s))))

(doc 'section 'monad-operations)

(define (state-pure x)
  (doc 'export #t)
  (doc 'type '(-> a (State s a)))
  (doc 'description "Lift a value into State (return/pure)")
  (make-state (lambda (s) (cons x s))))

(define (state-bind st f)
  (doc 'export #t)
  (doc 'type '(-> (State s a) (-> a (State s b)) (State s b)))
  (doc 'description "Sequence two State computations (>>=)")
  (make-state
   (lambda (s)
           (let* ([result (run-state st s)]
                  [a (car result)]
                  [s2 (cdr result)])
                 (run-state (f a) s2)))))

(define (state-then st1 st2)
  (doc 'export #t)
  (doc 'type '(-> (State s a) (State s b) (State s b)))
  (doc 'description "Sequence two State computations, discarding the first result (>>)")
  (state-bind st1 (lambda (_) st2)))

(define (state-map f st)
  (doc 'export #t)
  (doc 'type '(-> (-> a b) (State s a) (State s b)))
  (doc 'description "Map a function over the result of a State computation")
  (state-bind st (lambda (a) (state-pure (f a)))))

(define (state-ap st-f st-a)
  (doc 'export #t)
  (doc 'type '(-> (State s (-> a b)) (State s a) (State s b)))
  (doc 'description "Apply a State-wrapped function to a State-wrapped value")
  (state-bind st-f
              (lambda (f)
                      (state-bind st-a
                                  (lambda (a)
                                          (state-pure (f a)))))))

(doc 'section 'state-combinators)

(define (state-sequence states)
  (doc 'export #t)
  (doc 'type '(-> (List (State s a)) (State s (List a))))
  (doc 'description "Sequence a list of State computations, collecting results")
  (if (null? states)
      (state-pure '())
      (state-bind (car states)
                  (lambda (x)
                          (state-bind (state-sequence (cdr states))
                                      (lambda (xs)
                                              (state-pure (cons x xs))))))))

(define (state-map-m f lst)
  (doc 'export #t)
  (doc 'type '(-> (-> a (State s b)) (List a) (State s (List b))))
  (doc 'description "Map a State-returning function over a list")
  (state-sequence (map f lst)))

(define (state-for-each lst f)
  (doc 'export #t)
  (doc 'type '(-> (List a) (-> a (State s ())) (State s ())))
  (doc 'description "Execute a State action for each element (for side effects)")
  (if (null? lst)
      (state-pure '())
      (state-then (f (car lst))
                  (state-for-each (cdr lst) f))))

(define (state-when condition action)
  (doc 'export #t)
  (doc 'type '(-> Boolean (State s ()) (State s ())))
  (doc 'description "Execute State action only when condition is true")
  (if condition
      action
      (state-pure '())))

(define (state-unless condition action)
  (doc 'export #t)
  (doc 'type '(-> Boolean (State s ()) (State s ())))
  (doc 'description "Execute State action only when condition is false")
  (state-when (not condition) action))

(doc 'section 'state-with-lenses)
(doc 'note "These combinators work with lenses from fp/lens.ss to focus on parts of the state")

(define (state-view lens)
  (doc 'export #t)
  (doc 'type '(-> (Lens s a) (State s a)))
  (doc 'description "View a part of the state through a lens - requires fp/optics/lens.ss")
  (state-gets (lambda (s) (view lens s))))

(define (state-set lens val)
  (doc 'export #t)
  (doc 'type '(-> (Lens s a) a (State s ())))
  (doc 'description "Set a part of the state through a lens - requires fp/optics/lens.ss")
  (state-modify (lambda (s) (set lens val s))))

(define (state-over lens f)
  (doc 'export #t)
  (doc 'type '(-> (Lens s a) (-> a a) (State s ())))
  (doc 'description "Modify a part of the state through a lens - requires fp/optics/lens.ss")
  (state-modify (lambda (s) (over lens f s))))

(doc 'section 'labeled-state)
(doc 'note "For computations with multiple named state components, use an alist as state with key-lens from lens.ss")

(define (state-get-key key)
  (doc 'export #t)
  (doc 'type '(-> k (State (Alist k v) (Maybe v))))
  (doc 'description "Get a value by key from an alist state")
  (state-gets (lambda (alist)
                      (let ([pair (assoc key alist)])
                           (if pair (just (cdr pair)) nothing)))))

(define (state-put-key key val)
  (doc 'export #t)
  (doc 'type '(-> k v (State (Alist k v) ())))
  (doc 'description "Set a value by key in an alist state")
  (state-modify (lambda (alist)
                        (let ([new-pair (cons key val)])
                             (let loop ([lst alist] [acc '()])
                                  (cond
                                   [(null? lst) (reverse (cons new-pair acc))]
                                   [(equal? (caar lst) key)
                                    (append (reverse acc) (cons new-pair (cdr lst)))]
                                   [else (loop (cdr lst) (cons (car lst) acc))]))))))

(define (state-modify-key key f default)
  (doc 'export #t)
  (doc 'type '(-> k (-> v v) v (State (Alist k v) ())))
  (doc 'description "Modify a value by key, using default if key doesn't exist")
  (state-bind (state-get-key key)
              (lambda (maybe-val)
                      (let ([val (if (just? maybe-val)
                                     (from-just maybe-val)
                                     default)])
                           (state-put-key key (f val))))))

(doc 'section 'counter-state)
(doc 'note "Common pattern for counter state")

(define state-inc
  (state-bind state-get
              (lambda (n)
                      (state-then (state-put (+ n 1))
                                  (state-pure n)))))
(doc state-inc 'type '(State Nat Nat))
(doc state-inc 'description "Increment counter state, return old value")

(define state-dec
  (state-bind state-get
              (lambda (n)
                      (state-then (state-put (- n 1))
                                  (state-pure n)))))
(doc state-dec 'type '(State Nat Nat))
(doc state-dec 'description "Decrement counter state, return old value")

(define (state-add n)
  (doc 'export #t)
  (doc 'type '(-> Nat (State Nat ())))
  (doc 'description "Add to counter state")
  (state-modify (lambda (x) (+ x n))))

(define fresh state-inc)
(doc fresh 'export #t)
(doc fresh 'type '(State Nat Nat))
(doc fresh 'description "Generate a fresh unique number")

(doc 'section 'stack-state)
(doc 'note "Common pattern for stack state")

(define (state-push x)
  (doc 'export #t)
  (doc 'type '(-> a (State (List a) ())))
  (doc 'description "Push a value onto a stack state")
  (state-modify (lambda (stack) (cons x stack))))

(define state-pop
  (state-bind state-get
              (lambda (stack)
                      (if (null? stack)
                          (state-pure nothing)
                          (state-then (state-put (cdr stack))
                                      (state-pure (just (car stack))))))))
(doc state-pop 'type '(State (List a) (Maybe a)))
(doc state-pop 'description "Pop a value from a stack state")

(define state-peek
  (state-gets (lambda (stack)
                      (if (null? stack)
                          nothing
                          (just (car stack))))))
(doc state-peek 'type '(State (List a) (Maybe a)))
(doc state-peek 'description "Peek at the top of a stack state")

(doc 'section 'accumulator-state)
(doc 'note "Common pattern - O(1) Writer Pattern")
(doc 'note "Uses cons-based accumulation for O(1) single-element appends")
(doc 'note "Requires O(N) reversal when reading final result")
(doc 'note "For write-heavy workloads this is much faster than repeated O(N) appends")
(doc 'note "The accumulated list is stored in reverse order internally - use state-get-log to retrieve in correct order")

(define (state-tell x)
  (doc 'export #t)
  (doc 'type '(-> a (State (List a) ())))
  (doc 'description "Append a value to an accumulator (like Writer) - O(1) operation using cons-based accumulation")
  (state-modify (lambda (acc) (cons x acc))))

(define (state-tell-all xs)
  (doc 'export #t)
  (doc 'type '(-> (List a) (State (List a) ())))
  (doc 'description "Append all values to an accumulator - O(N) where N is length of xs")
  (state-modify (lambda (acc)
                        (let loop ([remaining xs] [result acc])
                             (if (null? remaining)
                                 result
                                 (loop (cdr remaining) (cons (car remaining) result)))))))

(define state-listen
  (state-bind state-get
              (lambda (acc) (state-pure (reverse acc)))))
(doc state-listen 'type '(State (List a) (List a)))
(doc state-listen 'description "Get the current accumulator in correct order - O(N) reversal to restore order")

(define state-get-log state-listen)
(doc state-get-log 'export #t)
(doc state-get-log 'type '(State (List a) (List a)))
(doc state-get-log 'description "Alias for state-listen - get accumulator in correct order")

(define state-clear
  (state-bind state-get
              (lambda (acc)
                      (state-then (state-put '())
                                  (state-pure (reverse acc))))))
(doc state-clear 'type '(State (List a) (List a)))
(doc state-clear 'description "Clear the accumulator, returning old value in correct order - O(N) reversal to restore order")
