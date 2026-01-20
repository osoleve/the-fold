(load "core/base/prelude.ss")

(doc 'module 'state)
(doc 'description "The State Monad. Pure state threading for The Fold. State computations are functions: State → (Value × State). We represent pairs as 2-element lists for simplicity. This module provides: State monad combinators (return, bind, get, put, modify), State monad runners (run-state, eval-state, exec-state), Utility combinators (sequence, traverse, etc.). The State monad enables: game state management without mutation, pure simulation loops, deterministic replay (same input state = same output). Type: State s a = s → (a × s).")
(doc 'layer 'core)

(doc 'section 'state-monad-primitives)

(doc state-return-def 'type Sexp)
(doc state-return-def 'description "State monad return (pure) as Fold expression for inclusion in prelude.")
(doc state-return-def 'export #t)
(define state-return-def
  '(state-return . (fn (a)
                       (fn (s) (prim 'list a s)))))

;;; state-bind-def : Sexp
(define state-bind-def
  '(state-bind . (fn (ma)
                     (fn (f)
                         (fn (s)
                             (let ((result (ma s)))
                                  (let ((a (prim 'car result))
                                        (s1 (prim 'car (prim 'cdr result))))
                                       ((f a) s1))))))))

;;; state-get-def : Sexp
(define state-get-def
  '(state-get . (fn (s) (prim 'list s s))))

;;; state-put-def : Sexp
(define state-put-def
  '(state-put . (fn (new-s)
                    (fn (old-s) (prim 'list '() new-s)))))

;;; state-modify-def : Sexp
(define state-modify-def
  '(state-modify . (fn (f)
                       (fn (s) (prim 'list '() (f s))))))

;;; state-gets-def : Sexp
(define state-gets-def
  '(state-gets . (fn (f)
                     (fn (s) (prim 'list (f s) s)))))

(doc 'section 'state-runners)

(doc run-state-def 'type Sexp)
(doc run-state-def 'description "State monad runner that returns both value and final state.")
(doc run-state-def 'export #t)
(define run-state-def
  '(run-state . (fn (ma)
                    (fn (init-s)
                        (ma init-s)))))

;;; eval-state-def : Sexp
(define eval-state-def
  '(eval-state . (fn (ma)
                     (fn (init-s)
                         (prim 'car (ma init-s))))))

;;; exec-state-def : Sexp
(define exec-state-def
  '(exec-state . (fn (ma)
                     (fn (init-s)
                         (prim 'car (prim 'cdr (ma init-s)))))))

(doc 'section 'state-monad-combinators)

(doc state-map-def 'type Sexp)
(doc state-map-def 'description "State monad fmap (functor map).")
(doc state-map-def 'export #t)
(define state-map-def
  '(state-map . (fn (f)
                    (fn (ma)
                        (fn (s)
                            (let ((result (ma s)))
                                 (let ((a (prim 'car result))
                                       (s1 (prim 'car (prim 'cdr result))))
                                      (prim 'list (f a) s1))))))))

;;; state-ap-def : Sexp
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

;;; state-join-def : Sexp
(define state-join-def
  '(state-join . (fn (mma)
                     (fn (s)
                         (let ((r1 (mma s)))
                              (let ((ma (prim 'car r1))
                                    (s1 (prim 'car (prim 'cdr r1))))
                                   (ma s1)))))))

;;; ====
;;; Utility: Do-notation via nested binds
;;; ====

;;; For convenience, we can chain operations:
;;; (state-do ((x <- m1) (y <- m2)) (state-return (+ x y)))
;;;
;;; This is sugar that expands to:
;;; ((state-bind m1) (fn (x) ((state-bind m2) (fn (y) (state-return (+ x y))))))
;;;
;;; Note: This requires a macro, which we don't have yet.
;;; For now, use explicit bind chains.

;;; ====
;;; State Monad Laws
;;; ====

;;; The state monad satisfies the monad laws:
;;;
;;; 1. Left identity:  (bind (return a) f) ≡ (f a)
;;; 2. Right identity: (bind m return) ≡ m
;;; 3. Associativity:  (bind (bind m f) g) ≡ (bind m (λx. bind (f x) g))
;;;
;;; These can be verified by evaluation.

(doc 'section 'practical-combinators)

(doc state-sequence-def 'type Sexp)
(doc state-sequence-def 'description "Sequence a list of state computations.")
(doc state-sequence-def 'export #t)
(define state-sequence-def
  '(state-sequence . (fix seq (fn (ms)
                                  (if (prim 'null? ms)
                                      (fn (s) (prim 'list '() s))
                                      ((state-bind (prim 'car ms))
                                       (fn (x)
                                           ((state-map (fn (xs) (prim 'cons x xs)))
                                            (seq (prim 'cdr ms))))))))))

;;; state-traverse-def : Sexp
(define state-traverse-def
  '(state-traverse . (fix trav (fn (f xs)
                                   (if (prim 'null? xs)
                                       (fn (s) (prim 'list '() s))
                                       ((state-bind (f (prim 'car xs)))
                                        (fn (y)
                                            ((state-map (fn (ys) (prim 'cons y ys)))
                                             (trav f (prim 'cdr xs))))))))))

;;; state-replicate-def : Sexp
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

;;; state-when-def : Sexp
(define state-when-def
  '(state-when . (fn (cond)
                     (fn (action)
                         (if cond
                             action
                             (fn (s) (prim 'list '() s)))))))

;;; state-unless-def : Sexp
(define state-unless-def
  '(state-unless . (fn (cond)
                       (fn (action)
                           (if cond
                               (fn (s) (prim 'list '() s))
                               action)))))

;;; ====
;;; Stateful Counter Example
;;; ====

;;; inc-counter-def : Sexp
(define inc-counter-def
  '(inc-counter . (fn (n)
                      (prim 'list n (prim 'add n 1)))))

;;; dec-counter-def : Sexp
(define dec-counter-def
  '(dec-counter . (fn (n)
                      (prim 'list n (prim 'sub n 1)))))

;;; ====
;;; Game State Example: Position
;;; ====

;;; A game state might be a position: (x y)
;;; These combinators work with such state.

;;; move-right-def : Sexp
(define move-right-def
  '(move-right . (fn (dx)
                     (fn (pos)
                         (let ((x (prim 'car pos))
                               (y (prim 'car (prim 'cdr pos))))
                              (prim 'list '() (prim 'list (prim 'add x dx) y)))))))

;;; move-up-def : Sexp
(define move-up-def
  '(move-up . (fn (dy)
                  (fn (pos)
                      (let ((x (prim 'car pos))
                            (y (prim 'car (prim 'cdr pos))))
                           (prim 'list '() (prim 'list x (prim 'add y dy))))))))

;;; get-x-def : Sexp
(define get-x-def
  '(get-x . (fn (pos)
                (prim 'list (prim 'car pos) pos))))

;;; get-y-def : Sexp
(define get-y-def
  '(get-y . (fn (pos)
                (prim 'list (prim 'car (prim 'cdr pos)) pos))))

(doc 'section 'prelude-integration)

(doc state-prelude-defs 'type (List Sexp))
(doc state-prelude-defs 'description "All state monad definitions for inclusion in prelude.")
(doc state-prelude-defs 'export #t)
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

;;; ====
;;; Type Signatures
;;; ====

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

;;; ====
;;; Integration with eval.ss
;;; ====

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
