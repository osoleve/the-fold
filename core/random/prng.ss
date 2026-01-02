;;; fabric/stitches/random/prng.ss — Pseudorandom Number Generation
;;;
;;; Pure, deterministic pseudorandom number generators using the State monad.
;;; All generators are fully reproducible given the same seed.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - PCG (Permuted Congruential Generator) - high quality, fast
;;;   - Xorshift128+ - fast, good statistical properties
;;;   - Splitmix64 - excellent for seeding other generators
;;;   - State monad integration for composable random computations
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/state.ss

(load "core/base/prelude.ss")
(load "core/fp/control/state.ss")

;;; ============================================================
;;; Bit Manipulation Helpers
;;; ============================================================
;;;
;;; Scheme uses arbitrary precision integers, but we simulate
;;; 64-bit and 32-bit operations for PRNG algorithms.

;;; Masks for fixed-width arithmetic
(define mask-32 (- (expt 2 32) 1))
(define mask-64 (- (expt 2 64) 1))

;;; u32 : Integer -> Integer
;;; Truncate to unsigned 32-bit.
(define (u32 n)
  (bitwise-and n mask-32))

;;; u64 : Integer -> Integer
;;; Truncate to unsigned 64-bit.
(define (u64 n)
  (bitwise-and n mask-64))

;;; rotl32 : Integer x Integer -> Integer
;;; 32-bit left rotation.
(define (rotl32 x k)
  (let ([x (u32 x)]
        [k (modulo k 32)])
       (u32 (bitwise-ior (ash x k)
                         (ash x (- k 32))))))

;;; rotr32 : Integer x Integer -> Integer
;;; 32-bit right rotation.
(define (rotr32 x k)
  (rotl32 x (- 32 k)))

;;; rotl64 : Integer x Integer -> Integer
;;; 64-bit left rotation.
(define (rotl64 x k)
  (let ([x (u64 x)]
        [k (modulo k 64)])
       (u64 (bitwise-ior (ash x k)
                         (ash x (- k 64))))))

;;; rotr64 : Integer x Integer -> Integer
;;; 64-bit right rotation.
(define (rotr64 x k)
  (rotl64 x (- 64 k)))

;;; ============================================================
;;; Splitmix64 Generator
;;; ============================================================
;;;
;;; Simple, high-quality generator often used to initialize
;;; other generators from a single seed.
;;;
;;; State: single 64-bit integer

;;; make-splitmix : Integer -> SplitmixState
;;; Create a splitmix64 state from a seed.
(define (make-splitmix seed)
  (list 'splitmix (u64 seed)))

;;; splitmix? : Any -> Boolean
(define (splitmix? x)
  (and (pair? x) (eq? (car x) 'splitmix)))

;;; splitmix-state : SplitmixState -> Integer
(define (splitmix-state sm)
  (cadr sm))

;;; splitmix-next : SplitmixState -> (Integer . SplitmixState)
;;; Generate next random 64-bit integer.
(define (splitmix-next sm)
  (let* ([s (u64 (+ (splitmix-state sm) #x9e3779b97f4a7c15))]
         [z (u64 (* (bitwise-xor s (ash s -30))
                    #xbf58476d1ce4e5b9))]
         [z (u64 (* (bitwise-xor z (ash z -27))
                    #x94d049bb133111eb))]
         [z (bitwise-xor z (ash z -31))])
        (cons z (make-splitmix s))))

;;; splitmix : State SplitmixState Integer
;;; Splitmix as a State monad computation.
(define splitmix-random
  (make-state splitmix-next))

;;; ============================================================
;;; PCG (Permuted Congruential Generator)
;;; ============================================================
;;;
;;; High-quality, statistically excellent generator.
;;; We implement PCG-XSH-RR (32-bit output, 64-bit state).
;;;
;;; State: (state . inc) where both are 64-bit

;;; make-pcg : Integer x Integer -> PCGState
;;; Create PCG state from seed and stream id.
;;; Different stream IDs give independent sequences.
(define (make-pcg seed stream)
  (let* ([inc (u64 (bitwise-ior (ash stream 1) 1))]
         [state0 0]
         ;; Initialize: run one step from 0, add seed, run again
         [state1 (u64 (+ (* state0 #x5851f42d4c957f2d) inc))]
         [state2 (u64 (+ state1 (u64 seed)))]
         [state3 (u64 (+ (* state2 #x5851f42d4c957f2d) inc))])
        (list 'pcg state3 inc)))

;;; pcg? : Any -> Boolean
(define (pcg? x)
  (and (pair? x) (eq? (car x) 'pcg)))

;;; pcg-state : PCGState -> Integer
(define (pcg-state p) (cadr p))

;;; pcg-inc : PCGState -> Integer
(define (pcg-inc p) (caddr p))

;;; pcg-next : PCGState -> (Integer . PCGState)
;;; Generate next random 32-bit integer.
(define (pcg-next p)
  (let* ([state (pcg-state p)]
         [inc (pcg-inc p)]
         ;; XSH-RR output function
         [xorshifted (u32 (ash
                           (bitwise-xor (ash state -18) state)
                           -27))]
         [rot (ash state -59)]
         [output (rotr32 xorshifted rot)]
         ;; LCG step
         [new-state (u64 (+ (* state #x5851f42d4c957f2d) inc))])
        (cons output (list 'pcg new-state inc))))

;;; pcg-random : State PCGState Integer
;;; PCG as a State monad computation.
(define pcg-random
  (make-state pcg-next))

;;; ============================================================
;;; Xorshift128+ Generator
;;; ============================================================
;;;
;;; Fast generator with good statistical properties.
;;; State: pair of 64-bit integers (s0, s1)

;;; make-xorshift128 : Integer -> Xorshift128State
;;; Create xorshift128+ state from a seed.
;;; Uses splitmix64 to generate the two state words.
(define (make-xorshift128 seed)
  (let* ([sm0 (make-splitmix seed)]
         [r1 (splitmix-next sm0)]
         [s0 (car r1)]
         [sm1 (cdr r1)]
         [r2 (splitmix-next sm1)]
         [s1 (car r2)])
        ;; Ensure non-zero state
        (list 'xorshift128
              (if (= s0 0) 1 s0)
              (if (= s1 0) 1 s1))))

;;; xorshift128? : Any -> Boolean
(define (xorshift128? x)
  (and (pair? x) (eq? (car x) 'xorshift128)))

;;; xorshift128-s0 : Xorshift128State -> Integer
(define (xorshift128-s0 xs) (cadr xs))

;;; xorshift128-s1 : Xorshift128State -> Integer
(define (xorshift128-s1 xs) (caddr xs))

;;; xorshift128-next : Xorshift128State -> (Integer . Xorshift128State)
;;; Generate next random 64-bit integer.
(define (xorshift128-next xs)
  (let* ([s0 (xorshift128-s0 xs)]
         [s1 (xorshift128-s1 xs)]
         [result (u64 (+ s0 s1))]
         ;; Xorshift operations
         [s1-new (bitwise-xor s0 s1)]
         [new-s0 (u64 (bitwise-xor
                       (bitwise-xor (rotl64 s0 24) s1-new)
                       (ash s1-new 16)))]
         [new-s1 (rotl64 s1-new 37)])
        (cons result (list 'xorshift128 new-s0 new-s1))))

;;; xorshift128-random : State Xorshift128State Integer
;;; Xorshift128+ as a State monad computation.
(define xorshift128-random
  (make-state xorshift128-next))

;;; ============================================================
;;; Uniform Random Number Generation
;;; ============================================================

;;; random-u32 : Generator -> State GenState Integer
;;; Generate a random 32-bit unsigned integer.
;;; Works with any generator (auto-detects type).
(define (random-u32-from gen-state)
  (cond
   [(pcg? gen-state) pcg-random]
   [(splitmix? gen-state)
    (state-map u32 splitmix-random)]
   [(xorshift128? gen-state)
    (state-map u32 xorshift128-random)]
   [else (error 'random-u32 "unknown generator type" gen-state)]))

;;; random-u64 : Generator -> State GenState Integer
;;; Generate a random 64-bit unsigned integer.
(define (random-u64-from gen-state)
  (cond
   [(pcg? gen-state)
    ;; PCG produces 32 bits, combine two
    (state-bind pcg-random
                (lambda (hi)
                        (state-bind pcg-random
                                    (lambda (lo)
                                            (state-pure
                                             (u64 (bitwise-ior (ash hi 32) lo)))))))]
   [(splitmix? gen-state) splitmix-random]
   [(xorshift128? gen-state) xorshift128-random]
   [else (error 'random-u64 "unknown generator type" gen-state)]))

;;; random-float : State GenState Float
;;; Generate a random float in [0, 1).
;;; Uses full precision from each generator type.
(define random-float
  (make-state
   (lambda (gen)
           (cond
            [(pcg? gen)
             ;; PCG produces 32 bits
             (let* ([result (pcg-next gen)]
                    [bits (car result)]
                    [new-gen (cdr result)]
                    [scaled (/ bits (expt 2.0 32))])
                   (cons scaled new-gen))]
            [(splitmix? gen)
             ;; Splitmix produces 64 bits, use top 53 for precision
             (let* ([result (splitmix-next gen)]
                    [bits (car result)]
                    [new-gen (cdr result)]
                    [scaled (/ (ash bits -11) (expt 2.0 53))])
                   (cons scaled new-gen))]
            [(xorshift128? gen)
             ;; Xorshift128+ produces 64 bits, use top 53 for precision
             (let* ([result (xorshift128-next gen)]
                    [bits (car result)]
                    [new-gen (cdr result)]
                    [scaled (/ (ash bits -11) (expt 2.0 53))])
                   (cons scaled new-gen))]
            [else (error 'random-float "unknown generator" gen)]))))

;;; random-float-range : Float x Float -> State GenState Float
;;; Generate a random float in [lo, hi).
(define (random-float-range lo hi)
  (state-map (lambda (u) (+ lo (* u (- hi lo))))
             random-float))

;;; random-int-range : Integer x Integer -> State GenState Integer
;;; Generate a random integer in [lo, hi] (inclusive).
;;; Uses rejection sampling to avoid modulo bias.
(define (random-int-range lo hi)
  (if (> lo hi)
      (error 'random-int-range "lo must be <= hi" lo hi)
      (let* ([range (+ (- hi lo) 1)]
             [threshold (modulo (- (expt 2 64)) range)])
            (make-state
             (lambda (gen)
                     (let loop ([g gen])
                          (let* ([result (cond
                                          [(pcg? g)
                                           (let* ([r1 (pcg-next g)]
                                                  [r2 (pcg-next (cdr r1))])
                                                 (cons (bitwise-ior
                                                        (ash (car r1) 32)
                                                        (car r2))
                                                       (cdr r2)))]
                                          [(splitmix? g) (splitmix-next g)]
                                          [(xorshift128? g) (xorshift128-next g)]
                                          [else (error 'random-int-range "unknown gen" g)])]
                                 [bits (car result)]
                                 [new-gen (cdr result)])
                                (if (>= bits threshold)
                                    (cons (+ lo (modulo bits range)) new-gen)
                                    (loop new-gen)))))))))

;;; random-bool : State GenState Boolean
;;; Generate a random boolean.
(define random-bool
  (state-map (lambda (n) (odd? n))
             (make-state
              (lambda (gen)
                      (cond
                       [(pcg? gen) (pcg-next gen)]
                       [(splitmix? gen) (splitmix-next gen)]
                       [(xorshift128? gen) (xorshift128-next gen)]
                       [else (error 'random-bool "unknown generator" gen)])))))

;;; ============================================================
;;; Sampling Utilities
;;; ============================================================

;;; random-element : List a -> State GenState a
;;; Pick a random element from a non-empty list.
(define (random-element lst)
  (if (null? lst)
      (error 'random-element "empty list")
      (state-map (lambda (i) (list-ref lst i))
                 (random-int-range 0 (- (length lst) 1)))))

;;; shuffle : List a -> State GenState (List a)
;;; Fisher-Yates shuffle. Returns a random permutation.
(define (shuffle lst)
  (let ([vec (list->vector lst)]
        [n (length lst)])
       (letrec ([shuffle-step
                 (lambda (i)
                         (if (>= i (- n 1))
                             (state-pure (vector->list vec))
                             (state-bind (random-int-range i (- n 1))
                                         (lambda (j)
                                                 ;; Swap vec[i] and vec[j]
                                                 (let ([tmp (vector-ref vec i)])
                                                      (vector-set! vec i (vector-ref vec j))
                                                      (vector-set! vec j tmp)
                                                      (shuffle-step (+ i 1)))))))])
               (shuffle-step 0))))

;;; sample : Integer x List a -> State GenState (List a)
;;; Sample k elements without replacement.
;;; Uses reservoir sampling for efficiency.
(define (sample k lst)
  (let ([n (length lst)])
       (cond
        [(> k n) (error 'sample "k > list length" k n)]
        [(= k n) (shuffle lst)]
        [(= k 0) (state-pure '())]
        [else
         ;; Reservoir sampling
         (let ([reservoir (take k lst)]
               [rest (drop k lst)])
              (letrec ([fill-reservoir
                        (lambda (res rem i)
                                (if (null? rem)
                                    (state-pure res)
                                    (state-bind (random-int-range 0 i)
                                                (lambda (j)
                                                        (if (< j k)
                                                            (fill-reservoir
                                                             (list-set res j (car rem))
                                                             (cdr rem)
                                                             (+ i 1))
                                                            (fill-reservoir
                                                             res
                                                             (cdr rem)
                                                             (+ i 1)))))))])
                      (fill-reservoir reservoir rest k)))])))

;;; Helper: list-set (immutable update)
(define (list-set lst i val)
  (if (= i 0)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- i 1) val))))

;;; weighted-choice : List (a . Number) -> State GenState a
;;; Pick an element weighted by associated numbers.
;;; Input: ((elem1 . weight1) (elem2 . weight2) ...)
(define (weighted-choice weighted-list)
  (if (null? weighted-list)
      (error 'weighted-choice "empty list")
      (let* ([total (fold-left + 0 (map cdr weighted-list))])
            (if (<= total 0)
                (error 'weighted-choice "total weight must be positive" total)
                (state-bind (random-float-range 0 total)
                            (lambda (r)
                                    (state-pure
                                     (let loop ([lst weighted-list] [acc 0])
                                          (let ([new-acc (+ acc (cdar lst))])
                                               (if (or (< r new-acc) (null? (cdr lst)))
                                                   (caar lst)
                                                   (loop (cdr lst) new-acc)))))))))))

;;; ============================================================
;;; Generating Multiple Random Values
;;; ============================================================

;;; random-list : Integer x State g a -> State g (List a)
;;; Generate a list of n random values.
(define (random-list n gen)
  (if (<= n 0)
      (state-pure '())
      (state-bind gen
                  (lambda (x)
                          (state-bind (random-list (- n 1) gen)
                                      (lambda (xs)
                                              (state-pure (cons x xs))))))))

;;; random-vector : Integer x State g a -> State g (Vector a)
;;; Generate a vector of n random values.
(define (random-vector n gen)
  (state-map list->vector (random-list n gen)))

;;; ============================================================
;;; Convenience Functions (Run with Default Generator)
;;; ============================================================

;;; with-random : Integer x State g a -> a
;;; Run a random computation with a PCG generator seeded from given value.
(define (with-random seed computation)
  (eval-state computation (make-pcg seed 1)))

;;; with-random-stream : Integer x Integer x State g a -> a
;;; Run with PCG using specific seed and stream.
(define (with-random-stream seed stream computation)
  (eval-state computation (make-pcg seed stream)))

;;; ============================================================
;;; Generator Statistics (for Testing)
;;; ============================================================

;;; gen-advance : Integer x GenState -> GenState
;;; Advance generator n steps (discard n values).
(define (gen-advance n gen)
  (if (<= n 0)
      gen
      (let ([next (cond
                   [(pcg? gen) pcg-next]
                   [(splitmix? gen) splitmix-next]
                   [(xorshift128? gen) xorshift128-next]
                   [else (error 'gen-advance "unknown generator" gen)])])
           (gen-advance (- n 1) (cdr (next gen))))))

