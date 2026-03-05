;;; @module random-effect
;;; @requires effects prng

(require 'effects)
(require 'prng)

(doc 'module 'random-effect)
(doc 'description "The Random effect provides pure, reproducible random number generation within effectful computations, threading PRNG state implicitly.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'features "random-uniform, random-int, random-choice, random-sample-dist, random-shuffle-eff, deterministic reproducible results given same seed")
(doc 'note "Load order matters - effects.ss defines run-state for State effect, prng.ss uses state.ss which defines run-state for State monad")

(define (run-state-monad st initial-state)
  (doc 'export #t)
  (doc 'type '(-> (State s a) s (Pair a s)))
  (doc 'description "Alias for State monad's run-state - now the current definition from state.ss")
  ((state-fn st) initial-state))

(define (run-state-eff init-state eff)
  (doc 'export #t)
  (doc 'type '(-> s (Eff State a) (Pair a s)))
  (doc 'description "Preserve effects.ss State effect handler before it gets shadowed - uses run-state-helper")
  (run-state-helper init-state eff))

(define prng-random-float random-float)
(doc prng-random-float 'export #t)
(doc prng-random-float 'description "Alias the prng functions for use in the handler")

(define prng-random-int-range random-int-range)
(define prng-random-float-range random-float-range)
(define prng-random-bool random-bool)

(doc 'section 'random-effect-signature)

(define sig-Random
  (make-effect-sig 'Random
                   (list (make-operation 'uniform 'Unit 'Float)
                         (make-operation 'int-range '(Int Int) 'Int)
                         (make-operation 'choice '(List a) 'a)
                         (make-operation 'sample '(State g a) 'a)
                         (make-operation 'bool 'Unit 'Boolean)
                         (make-operation 'float-range '(Float Float) 'Float))))
(doc sig-Random 'type 'EffectSig)
(doc sig-Random 'description "Random effect signature")

(doc 'section 'random-effect-operations)

(define random-uniform-eff
  (perform (make-effect 'random-uniform '())))
(doc random-uniform-eff 'type '(Eff Random Float))
(doc random-uniform-eff 'description "Generate a random float in [0, 1)")

(define (random-int-eff lo hi)
  (doc 'export #t)
  (doc 'type '(-> Int Int (Eff Random Int)))
  (doc 'description "Generate a random integer in [lo, hi] (inclusive)")
  (perform (make-effect 'random-int (cons lo hi))))

(define (random-choice-eff lst)
  (doc 'export #t)
  (doc 'type '(-> (List a) (Eff Random a)))
  (doc 'description "Pick a random element from a non-empty list")
  (if (null? lst)
      (eff-throw "random-choice-eff: empty list")
      (perform (make-effect 'random-choice lst))))

(define random-bool-eff
  (perform (make-effect 'random-bool '())))
(doc random-bool-eff 'type '(Eff Random Boolean))
(doc random-bool-eff 'description "Generate a random boolean")

(define (random-float-eff lo hi)
  (doc 'export #t)
  (doc 'type '(-> Float Float (Eff Random Float)))
  (doc 'description "Generate a random float in [lo, hi)")
  (perform (make-effect 'random-float (cons lo hi))))

(define (random-sample-dist dist)
  (doc 'export #t)
  (doc 'type '(-> (State GenState a) (Eff Random a)))
  (doc 'description "Sample from a distribution (State monad computation) - bridges existing distributions to the effect system")
  (perform (make-effect 'random-sample dist)))

(doc 'section 'derived-random-operations)

(define (random-bernoulli-eff p)
  (doc 'export #t)
  (doc 'type '(-> Float (Eff Random Boolean)))
  (doc 'description "Sample from Bernoulli(p) - true with probability p")
  (eff-bind random-uniform-eff
            (lambda (u)
                    (eff-return (< u p)))))

(define (random-weighted-eff weighted-list)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair a Number)) (Eff Random a)))
  (doc 'description "Pick an element weighted by associated numbers")
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

(define (random-list-eff n gen)
  (doc 'export #t)
  (doc 'type '(-> Nat (Eff Random a) (Eff Random (List a))))
  (doc 'description "Generate a list of n random values")
  (eff-replicate n gen))

(define (shuffle-with-swaps lst swaps)
  (doc 'export #t)
  (doc 'type '(-> (List a) (List (Pair Int Int)) (List a)))
  (doc 'description "Apply Fisher-Yates swaps to a list using vector for O(1) access - takes a list and a list of (i . j) swap pairs")
  (let ([vec (list->vector lst)])
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

(define (random-shuffle-eff lst)
  (doc 'export #t)
  (doc 'type '(-> (List a) (Eff Random (List a))))
  (doc 'description "Fisher-Yates shuffle using Random effect - O(N) total complexity")
  (doc 'note "Converts to vector, performs O(1) swaps, then converts back to list")
  (doc 'note "Collects all random indices first then applies them to a mutable vector for determinism and effect system compatibility")
  (let ([n (length lst)])
       (if (<= n 1)
           (eff-return lst)
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

(doc 'section 'random-effect-handler)

(define (run-random seed eff)
  (doc 'export #t)
  (doc 'type '(-> Integer (Eff Random a) a))
  (doc 'description "Handle Random effect with a PCG generator seeded from given value - returns the final value (discards generator state)")
  (run-random-with-gen (make-pcg seed 1) eff))

(define (run-random-with-state-eff seed eff)
  (doc 'export #t)
  (doc 'type '(-> Integer (Eff Random a) (Pair a GenState)))
  (doc 'description "Handle Random effect, returning both value and final generator state")
  (run-random-helper (make-pcg seed 1) eff))

(define (run-random-with-gen gen eff)
  (doc 'export #t)
  (doc 'type '(-> GenState (Eff Random a) a))
  (doc 'description "Handle Random effect with an existing generator")
  (car (run-random-helper gen eff)))

(define (run-random-helper gen eff)
  (doc 'export #t)
  (doc 'type '(-> GenState (Eff Random a) (Pair a GenState)))
  (doc 'description "Core handler implementation")
  (cond
   [(eff-pure? eff)
    (cons (eff-pure-value eff) gen)]
   [(eff-queue? eff)
    (run-random-helper gen (eff-normalize eff))]
   [(eff-op? eff)
    (let ([effect (eff-op-effect eff)]
          [k (eff-op-cont eff)])
         (case (effect-tag effect)
               [(random-uniform)
                (let* ([result (run-state-monad prng-random-float gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]

               [(random-int)
                (let* ([bounds (effect-payload effect)]
                       [lo (car bounds)]
                       [hi (cdr bounds)]
                       [result (run-state-monad (prng-random-int-range lo hi) gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]

               [(random-choice)
                (let* ([lst (effect-payload effect)]
                       [n (length lst)]
                       [result (run-state-monad (prng-random-int-range 0 (- n 1)) gen)]
                       [idx (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k (list-ref lst idx))))]

               [(random-bool)
                (let* ([result (run-state-monad prng-random-bool gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]

               [(random-float)
                (let* ([bounds (effect-payload effect)]
                       [lo (car bounds)]
                       [hi (cdr bounds)]
                       [result (run-state-monad (prng-random-float-range lo hi) gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]

               [(random-sample)
                (let* ([dist (effect-payload effect)]
                       [result (run-state-monad dist gen)]
                       [val (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k val)))]

               [(random-split)
                (let ([children (gen-split gen)])
                     (run-random-helper (cadr children) (k children)))]

               [(random-bytes)
                (let* ([n (effect-payload effect)]
                       [result (run-state-monad (random-bytes n) gen)]
                       [bytes (car result)]
                       [new-gen (cdr result)])
                      (run-random-helper new-gen (k bytes)))]

               [(get-random-state)
                (run-random-helper gen (k gen))]

               [else
                (make-eff-op effect
                             (lambda (resp)
                                     (run-random-helper gen (k resp))))]))]))

(define handle-random run-random)
(doc handle-random 'export #t)
(doc handle-random 'type '(-> Integer (Eff Random a) a))
(doc handle-random 'description "Alias for run-random for API consistency")

(define (with-random-eff seed eff)
  (doc 'export #t)
  (doc 'type '(-> Integer (Eff Random a) (Eff e a)))
  (doc 'description "Run Random effect in a scope, returning only the result")
  (eff-return (run-random seed eff)))

(doc 'section 'split-operation)
(doc 'note "For parallel simulations")

(define random-split-eff
  (perform (make-effect 'random-split '())))
(doc random-split-eff 'type '(Eff Random (Pair GenState GenState)))
(doc random-split-eff 'description "Split the current generator into two independent streams - returns a pair of generator states for use in parallel computations")

(doc 'section 'random-bytes)
(doc 'note "For UUIDs, crypto placeholders, etc")

(define (random-bytes-eff n)
  (doc 'export #t)
  (doc 'type '(-> Nat (Eff Random Bytevector)))
  (doc 'description "Generate a bytevector of n random bytes")
  (perform (make-effect 'random-bytes n)))

(doc 'section 'serialization-api)
(doc 'note "For resumable sessions")

(define get-random-state-eff
  (perform (make-effect 'get-random-state '())))
(doc get-random-state-eff 'type '(Eff Random GenState))
(doc get-random-state-eff 'description "Get the current generator state (for serialization)")

(define serialize-random-state gen->string)
(doc serialize-random-state 'export #t)
(doc serialize-random-state 'type '(-> GenState String))
(doc serialize-random-state 'description "Convert generator state to a storable string")

(define deserialize-random-state string->gen)
(doc deserialize-random-state 'export #t)
(doc deserialize-random-state 'type '(-> String GenState))
(doc deserialize-random-state 'description "Parse generator state from string")

(define (run-random-resume state-string eff)
  (doc 'export #t)
  (doc 'type '(-> String (Eff Random a) a))
  (doc 'description "Resume a random computation from serialized state")
  (run-random-with-gen (deserialize-random-state state-string) eff))
