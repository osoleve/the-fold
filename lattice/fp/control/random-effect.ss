;;; core/fp/control/random-effect.ss --- Random Effect for Algebraic Effects
;;;
;;; The Random effect provides pure, reproducible random number generation
;;; within effectful computations. It threads PRNG state implicitly,
;;; enabling composable stochastic simulations.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - random-uniform: Generate float in [0, 1)
;;;   - random-int: Generate integer in [lo, hi]
;;;   - random-choice: Pick random element from list
;;;   - random-sample-dist: Sample from a distribution (State computation)
;;;   - random-shuffle-eff: Fisher-Yates shuffle
;;;   - Deterministic, reproducible results given same seed
;;;
;;; Usage:
;;;   (run-random seed computation)
;;;   (handle-random seed computation)  ; alias
;;;
;;; Dependencies:
;;;   - effects.ss (algebraic effects framework)
;;;   - random/prng.ss (PRNG implementations)
;;;
;;; IMPORTANT: Load order matters! prng.ss uses state.ss which defines
;;; run-state for State monad. effects.ss defines run-state for State effect.
;;; This file loads prng.ss first, then effects.ss, so effects.ss wins.

;; Load effects FIRST
;; effects.ss defines run-state for handling State effects
(load "lattice/fp/control/effects.ss")

;; Now load PRNG - this will load state.ss which redefines run-state
;; for the State monad. This is the order we want: state.ss's run-state
;; should be the final definition so that State monad closures work correctly.
(load "lattice/random/prng.ss")

;; Alias for State monad's run-state (now the current definition from state.ss)
(define (run-state-monad st initial-state)
  ((state-fn st) initial-state))

;; Preserve effects.ss's State effect handler before it gets shadowed
;; This uses run-state-helper which is still available
(define (run-state-eff init-state eff)
  (run-state-helper init-state eff))

;; Alias the prng functions for use in the handler
;; These are now correctly defined after prng.ss is loaded
(define prng-random-float random-float)
(define prng-random-int-range random-int-range)
(define prng-random-float-range random-float-range)
(define prng-random-bool random-bool)

;;; ============================================================
;;; Random Effect Signature
;;; ============================================================

;;; Random effect signature
(define sig-Random
  (make-effect-sig 'Random
                   (list (make-operation 'uniform 'Unit 'Float)
                         (make-operation 'int-range '(Int Int) 'Int)
                         (make-operation 'choice '(List a) 'a)
                         (make-operation 'sample '(State g a) 'a)
                         (make-operation 'bool 'Unit 'Boolean)
                         (make-operation 'float-range '(Float Float) 'Float))))

;;; ============================================================
;;; Random Effect Operations
;;; ============================================================

;;; random-uniform-eff : Eff Random Float
;;; Generate a random float in [0, 1).
(define random-uniform-eff
  (perform (make-effect 'random-uniform '())))

;;; random-int-eff : Int -> Int -> Eff Random Int
;;; Generate a random integer in [lo, hi] (inclusive).
(define (random-int-eff lo hi)
  (perform (make-effect 'random-int (cons lo hi))))

;;; random-choice-eff : List a -> Eff Random a
;;; Pick a random element from a non-empty list.
(define (random-choice-eff lst)
  (if (null? lst)
      (eff-throw "random-choice-eff: empty list")
      (perform (make-effect 'random-choice lst))))

;;; random-bool-eff : Eff Random Boolean
;;; Generate a random boolean.
(define random-bool-eff
  (perform (make-effect 'random-bool '())))

;;; random-float-eff : Float -> Float -> Eff Random Float
;;; Generate a random float in [lo, hi).
(define (random-float-eff lo hi)
  (perform (make-effect 'random-float (cons lo hi))))

;;; random-sample-dist : (State GenState a) -> Eff Random a
;;; Sample from a distribution (State monad computation).
;;; This bridges the existing distributions to the effect system.
(define (random-sample-dist dist)
  (perform (make-effect 'random-sample dist)))

;;; ============================================================
;;; Derived Random Operations
;;; ============================================================

;;; random-bernoulli-eff : Float -> Eff Random Boolean
;;; Sample from Bernoulli(p) - true with probability p.
(define (random-bernoulli-eff p)
  (eff-bind random-uniform-eff
            (lambda (u)
                    (eff-return (< u p)))))

;;; random-weighted-eff : List (a . Number) -> Eff Random a
;;; Pick an element weighted by associated numbers.
(define (random-weighted-eff weighted-list)
  (if (null? weighted-list)
      (eff-throw "random-weighted-eff: empty list")
      (let ([total (fold-left + 0 (map cdr weighted-list))])
           (if (<= total 0)
               (eff-throw "random-weighted-eff: total weight must be positive")
               (eff-bind (random-float-eff 0.0 total)
                         (lambda (r)
                                 (eff-return
                                  (let loop ([lst weighted-list] [acc 0])
                                       (let ([new-acc (+ acc (cdar lst))])
                                            (if (or (< r new-acc) (null? (cdr lst)))
                                                (caar lst)
                                                (loop (cdr lst) new-acc)))))))))))

;;; random-list-eff : Nat -> Eff Random a -> Eff Random (List a)
;;; Generate a list of n random values.
(define (random-list-eff n gen)
  (eff-replicate n gen))

;;; shuffle-with-swaps : List a -> List (Int . Int) -> List a
;;; Apply Fisher-Yates swaps to a list using vector for O(1) access.
;;; Takes a list and a list of (i . j) swap pairs.
(define (shuffle-with-swaps lst swaps)
  (let ([vec (list->vector lst)])
       ;; Apply each swap in order
       (for-each
        (lambda (swap)
                (let ([i (car swap)]
                      [j (cdr swap)])
                     (unless (= i j)
                             (let ([tmp (vector-ref vec i)])
                                  (vector-set! vec i (vector-ref vec j))
                                  (vector-set! vec j tmp)))))
        swaps)
       (vector->list vec)))

;;; random-shuffle-eff : List a -> Eff Random (List a)
;;; Fisher-Yates shuffle using Random effect.
;;;
;;; This implementation converts to a vector, performs O(1) swaps, then
;;; converts back to a list, achieving O(N) total complexity.
;;;
;;; The shuffle is performed by collecting all random indices first,
;;; then applying them to a mutable vector. This ensures determinism
;;; and compatibility with the effect system.
(define (random-shuffle-eff lst)
  (let ([n (length lst)])
       (if (<= n 1)
           (eff-return lst)
           ;; Collect all random indices needed for the shuffle
           (letrec ([collect-indices
                     (lambda (i acc)
                             (if (>= i (- n 1))
                                 (eff-return (reverse acc))
                                 (eff-bind (random-int-eff i (- n 1))
                                           (lambda (j)
                                                   (collect-indices (+ i 1) (cons (cons i j) acc))))))])
                   (eff-bind (collect-indices 0 '())
                             (lambda (swaps)
                                     (eff-return (shuffle-with-swaps lst swaps))))))))

;;; ============================================================
;;; Random Effect Handler
;;; ============================================================

;;; run-random : Integer -> Eff Random a -> a
;;; Handle Random effect with a PCG generator seeded from given value.
;;; Returns the final value (discards generator state).
(define (run-random seed eff)
  (run-random-with-gen (make-pcg seed 1) eff))

;;; run-random-with-state-eff : Integer -> Eff Random a -> (a . GenState)
;;; Handle Random effect, returning both value and final generator state.
(define (run-random-with-state-eff seed eff)
  (run-random-helper (make-pcg seed 1) eff))

;;; run-random-with-gen : GenState -> Eff Random a -> a
;;; Handle Random effect with an existing generator.
(define (run-random-with-gen gen eff)
  (car (run-random-helper gen eff)))

;;; run-random-helper : GenState -> Eff Random a -> (a . GenState)
;;; Core handler implementation.
(define (run-random-helper gen eff)
  (cond
   [(eff-pure? eff)
    (cons (eff-pure-value eff) gen)]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               ;; Uniform float in [0, 1)
               [(random-uniform)
                (let* ([result (run-state-monad prng-random-float gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]
               
               ;; Integer in [lo, hi]
               [(random-int)
                (let* ([bounds (effect-payload effect)]
                       [lo (car bounds)]
                       [hi (cdr bounds)]
                       [result (run-state-monad (prng-random-int-range lo hi) gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]
               
               ;; Random element from list
               [(random-choice)
                (let* ([lst (effect-payload effect)]
                       [n (length lst)]
                       [result (run-state-monad (prng-random-int-range 0 (- n 1)) gen)]
                       [idx (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k (list-ref lst idx))))]
               
               ;; Random boolean
               [(random-bool)
                (let* ([result (run-state-monad prng-random-bool gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]
               
               ;; Float in [lo, hi)
               [(random-float)
                (let* ([bounds (effect-payload effect)]
                       [lo (car bounds)]
                       [hi (cdr bounds)]
                       [result (run-state-monad (prng-random-float-range lo hi) gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]
               
               ;; Sample from distribution (State computation)
               [(random-sample)
                (let* ([dist (effect-payload effect)]
                       [result (run-state-monad dist gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]
               
               ;; Split generator into two independent streams
               [(random-split)
                (let ([children (gen-split gen)])
                     ;; Return the pair of children, continue with second child
                     ;; so returned generators and continuation use independent streams
                     (run-random-helper (cadr children) (k children)))]
               
               ;; Generate random bytes
               [(random-bytes)
                (let* ([n (effect-payload effect)]
                       [result (run-state-monad (random-bytes n) gen)]
                       [bytes (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k bytes)))]
               
               ;; Get current generator state (for serialization)
               [(get-random-state)
                (run-random-helper gen (k gen))]
               
               ;; Unknown effect - pass through
               [else
                (make-eff-op effect
                             (lambda (resp)
                                     (run-random-helper gen (k resp))))]))]))

;;; handle-random : Integer -> Eff Random a -> a
;;; Alias for run-random for API consistency with issue description.
(define handle-random run-random)

;;; with-random-eff : Integer -> Eff Random a -> Eff e a
;;; Run Random effect in a scope, returning only the result.
(define (with-random-eff seed eff)
  (eff-return (run-random seed eff)))

;;; ============================================================
;;; Split Operation (for Parallel Simulations)
;;; ============================================================

;;; random-split-eff : Eff Random (GenState . GenState)
;;; Split the current generator into two independent streams.
;;; Returns a pair of generator states for use in parallel computations.
(define random-split-eff
  (perform (make-effect 'random-split '())))

;;; ============================================================
;;; Random Bytes (for UUIDs, Crypto Placeholders, etc.)
;;; ============================================================

;;; random-bytes-eff : Nat -> Eff Random Bytevector
;;; Generate a bytevector of n random bytes.
(define (random-bytes-eff n)
  (perform (make-effect 'random-bytes n)))

;;; ============================================================
;;; Serialization API (for Resumable Sessions)
;;; ============================================================

;;; get-random-state-eff : Eff Random GenState
;;; Get the current generator state (for serialization).
(define get-random-state-eff
  (perform (make-effect 'get-random-state '())))

;;; serialize-random-state : GenState -> String
;;; Convert generator state to a storable string.
(define serialize-random-state gen->string)

;;; deserialize-random-state : String -> GenState
;;; Parse generator state from string.
(define deserialize-random-state string->gen)

;;; run-random-resume : String -> Eff Random a -> a
;;; Resume a random computation from serialized state.
(define (run-random-resume state-string eff)
  (run-random-with-gen (deserialize-random-state state-string) eff))

;;; ============================================================
;;; Example Usage
;;; ============================================================
;;;
;;; ;; Basic uniform random
;;; (run-random 42 random-uniform-eff)
;;; ; => 0.123... (deterministic for seed 42)
;;;
;;; ;; Random integer in range
;;; (run-random 42 (random-int-eff 1 6))
;;; ; => some integer 1-6
;;;
;;; ;; Multiple random values
;;; (run-random 42
;;;   (eff-bind random-uniform-eff
;;;             (lambda (u1)
;;;               (eff-bind random-uniform-eff
;;;                         (lambda (u2)
;;;                           (eff-return (list u1 u2)))))))
;;;
;;; ;; Monte Carlo simulation
;;; (run-random 42
;;;   (eff-bind (random-list-eff 1000
;;;               (eff-bind random-uniform-eff
;;;                         (lambda (x)
;;;                           (eff-bind random-uniform-eff
;;;                                     (lambda (y)
;;;                                       (eff-return (if (<= (+ (* x x) (* y y)) 1) 1 0)))))))
;;;             (lambda (hits)
;;;               (eff-return (* 4.0 (/ (fold-left + 0 hits) 1000))))))
;;; ; => approximately 3.14

