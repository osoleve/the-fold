;;; lattice/fp/markov.ss -- Discrete Markov Chains
;;; @module markov
;;; @requires prelude vec matrix transcendental

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'transcendental)

(doc 'module 'markov)
(doc 'description "Discrete-time Markov chain analysis: stationary distributions, mixing times, ergodicity, entropy rates")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "A Markov chain is represented as a row-stochastic transition matrix P
  where P(i,j) = Pr(X_{n+1} = j | X_n = i). Each row sums to 1.
  State distributions are row vectors: pi * P = pi at stationarity.")

;;; ====
;;; Construction
;;; ====

(doc make-markov-chain 'type '(-> Matrix (Union MarkovChain Error)))
(doc make-markov-chain 'description "Construct a Markov chain from a row-stochastic transition matrix.
  Validates that the matrix is square and each row sums to ~1.")
(define (make-markov-chain P)
  (let ([r (matrix-rows P)]
        [c (matrix-cols P)])
    (cond
      [(not (= r c))
       `(error not-square ,r ,c)]
      [(not (rows-stochastic? P 1e-8))
       `(error not-row-stochastic)]
      [else
       (list 'markov-chain P)])))

(doc markov-chain? 'type '(-> Any Bool))
(doc markov-chain? 'description "Check if value is a Markov chain")
(define (markov-chain? x)
  (and (pair? x)
       (eq? (car x) 'markov-chain)
       (pair? (cdr x))
       (matrix? (cadr x))))

(doc markov-transition-matrix 'type '(-> MarkovChain Matrix))
(doc markov-transition-matrix 'description "Extract the transition matrix from a Markov chain")
(define (markov-transition-matrix mc)
  (cadr mc))

(doc markov-num-states 'type '(-> MarkovChain Nat))
(doc markov-num-states 'description "Number of states in the Markov chain")
(define (markov-num-states mc)
  (matrix-rows (markov-transition-matrix mc)))

;;; ====
;;; Validation Helpers
;;; ====

(define (rows-stochastic? P tol)
  (doc 'description "Check that every row of P sums to approximately 1")
  (let ([r (matrix-rows P)]
        [c (matrix-cols P)]
        [data (matrix-data P)])
    (let loop ([i 0])
      (if (= i r)
          #t
          (let row-sum ([j 0] [s 0])
            (if (= j c)
                (if (< (abs (- s 1.0)) tol)
                    (loop (+ i 1))
                    #f)
                (let ([v (vector-ref data (+ (* i c) j))])
                  (if (< v (- 0 tol))
                      #f  ;; negative entry
                      (row-sum (+ j 1) (+ s v))))))))))

;;; ====
;;; Single-Step and Multi-Step Evolution
;;; ====

(doc markov-step 'type '(-> Vec Matrix Vec))
(doc markov-step 'description "Evolve a state distribution by one step: pi' = pi * P.
  pi is a row vector (length n), P is n x n row-stochastic.")
(define (markov-step pi P)
  (vec-matrix-mul pi P))

(doc markov-steps 'type '(-> Vec Matrix Nat Vec))
(doc markov-steps 'description "Evolve a state distribution by n steps: pi * P^n.
  Uses repeated multiplication (not matrix exponentiation).")
(define (markov-steps pi P n)
  (let loop ([v pi] [k n])
    (if (<= k 0)
        v
        (loop (vec-matrix-mul v P) (- k 1)))))

;;; ====
;;; Stationary Distribution
;;; ====

(doc stationary-distribution 'type '(-> Matrix [Num] [Nat] (Union Vec Error)))
(doc stationary-distribution 'description "Compute the stationary distribution via power iteration.
  Starts from uniform distribution and iterates pi = pi * P until convergence.
  Returns the eigenvector of P^T with eigenvalue 1 (left eigenvector of P).")
(doc stationary-distribution 'param 'P "Row-stochastic transition matrix")
(doc stationary-distribution 'param 'tol "Convergence tolerance on L1 norm of change (default 1e-10)")
(doc stationary-distribution 'param 'max-iter "Maximum iterations (default 10000)")
(define (stationary-distribution P . opts)
  (let* ([tol (if (and (pair? opts) (number? (car opts)))
                  (car opts)
                  1e-10)]
         [rest1 (if (and (pair? opts) (number? (car opts)))
                    (cdr opts)
                    opts)]
         [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                       (car rest1)
                       10000)]
         [n (matrix-rows P)]
         [pi (make-vector n (/ 1.0 n))])
    (let loop ([v pi] [iter 0])
      (if (>= iter max-iter)
          `(error no-convergence ,max-iter)
          (let* ([v-new (vec-matrix-mul v P)]
                 ;; Renormalize to guard against floating-point drift
                 [s (vec-sum-raw v-new)]
                 [v-norm (if (> (abs s) 1e-15)
                             (vec-scale (/ 1.0 s) v-new)
                             v-new)]
                 [change (l1-distance v-norm v)])
            (if (< change tol)
                v-norm
                (loop v-norm (+ iter 1))))))))

;;; ====
;;; Mixing Time
;;; ====

(doc mixing-time 'type '(-> Matrix Num [Num] [Nat] (Union Nat Error)))
(doc mixing-time 'description "Estimate the mixing time: the number of steps until the total
  variation distance between the evolved distribution and the stationary
  distribution is below epsilon, starting from the worst-case initial state.
  Total variation distance = (1/2) * ||mu - pi||_1.")
(doc mixing-time 'param 'P "Row-stochastic transition matrix")
(doc mixing-time 'param 'epsilon "Target total variation distance threshold")
(doc mixing-time 'param 'tol "Tolerance for stationary distribution convergence (default 1e-12)")
(doc mixing-time 'param 'max-steps "Maximum steps to try (default 100000)")
(define (mixing-time P epsilon . opts)
  (let* ([tol (if (and (pair? opts) (number? (car opts)))
                  (car opts)
                  1e-12)]
         [rest1 (if (and (pair? opts) (number? (car opts)))
                    (cdr opts)
                    opts)]
         [max-steps (if (and (pair? rest1) (integer? (car rest1)))
                        (car rest1)
                        100000)]
         [n (matrix-rows P)]
         [pi-star (stationary-distribution P tol)])
    (if (and (pair? pi-star) (eq? (car pi-star) 'error))
        pi-star
        ;; Try each basis vector as starting state, take worst case
        (let outer ([i 0] [worst-time 0])
          (if (= i n)
              worst-time
              (let* ([e-i (vec-unit n i)])
                (let inner ([v e-i] [step 0])
                  (if (>= step max-steps)
                      `(error mixing-time-exceeded ,max-steps)
                      (let ([tv (total-variation-distance v pi-star)])
                        (if (< tv epsilon)
                            (outer (+ i 1) (max worst-time step))
                            (inner (vec-matrix-mul v P) (+ step 1))))))))))))

;;; ====
;;; Ergodicity
;;; ====

(doc is-ergodic? 'type '(-> Matrix Bool))
(doc is-ergodic? 'description "Check if a Markov chain is ergodic (irreducible and aperiodic).
  Irreducible: all states reachable from all other states.
  Aperiodic: gcd of return times to each state is 1.
  We check irreducibility via the reachability matrix (P + I)^n > 0,
  and aperiodicity by verifying some power P^k has all positive entries.")
(define (is-ergodic? P)
  (let ([n (matrix-rows P)])
    (and (is-irreducible? P)
         (is-aperiodic? P))))

(doc is-irreducible? 'type '(-> Matrix Bool))
(doc is-irreducible? 'description "Check if the chain is irreducible: every state is reachable from every other state")
(define (is-irreducible? P)
  (let* ([n (matrix-rows P)]
         ;; Build a binary adjacency matrix: 1 where P(i,j) > 0
         [adj-data (make-vector (* n n) 0)]
         [_ (let fill ([k 0])
              (when (< k (* n n))
                (when (> (vector-ref (matrix-data P) k) 0)
                  (vector-set! adj-data k 1))
                (fill (+ k 1))))]
         [adj (list 'matrix n n adj-data)]
         ;; Compute (I + A)^(n-1) -- if all entries > 0, the chain is irreducible
         [I+A (matrix-add (matrix-identity n) adj)]
         [R (matrix-power-fast* I+A (- n 1))])
    (all-positive? R)))

(doc is-aperiodic? 'type '(-> Matrix Bool))
(doc is-aperiodic? 'description "Check if the chain is aperiodic: gcd of return times = 1 for all states.
  Sufficient condition: some diagonal entry of P is positive (self-loop) in an
  irreducible chain. Otherwise, check if P^k has all positive entries for some k.")
(define (is-aperiodic? P)
  (let ([n (matrix-rows P)])
    ;; Quick check: if any state has a self-loop, aperiodicity is guaranteed
    ;; for an irreducible chain
    (or (has-self-loop? P)
        ;; Otherwise check if P^k has all positive entries for some k up to n^2
        ;; (For an irreducible chain of period d, P^k is block-structured with
        ;; zeros for k not divisible by d. All-positive requires d=1.)
        (let try ([k 2] [Pk (matrix-mul P P)])
          (if (> k (* n n))
              #f
              (if (all-positive? Pk)
                  #t
                  (try (+ k 1) (matrix-mul Pk P))))))))

;;; ====
;;; Entropy Rate
;;; ====

(doc markov-entropy-rate 'type '(-> Matrix [Vec] (Union Num Error)))
(doc markov-entropy-rate 'description "Entropy rate of a stationary Markov chain:
  H(X_{n+1} | X_n) = -sum_i pi_i sum_j P(i,j) log2 P(i,j)
  Uses natural log internally, converts to bits.
  If stationary distribution pi is not provided, computes it via power iteration.")
(doc markov-entropy-rate 'param 'P "Row-stochastic transition matrix")
(doc markov-entropy-rate 'param 'pi "Optional stationary distribution vector")
(define (markov-entropy-rate P . opts)
  (let* ([pi (if (and (pair? opts) (vector? (car opts)))
                 (car opts)
                 (stationary-distribution P))]
         [n (matrix-rows P)]
         [data (matrix-data P)])
    (if (and (pair? pi) (eq? (car pi) 'error))
        pi
        (let loop ([i 0] [h 0])
          (if (= i n)
              h
              (let ([pi-i (vector-ref pi i)])
                (if (<= pi-i 0)
                    (loop (+ i 1) h)
                    (let row-loop ([j 0] [row-h 0])
                      (if (= j n)
                          (loop (+ i 1) (+ h (* (- pi-i) row-h)))
                          (let ([p-ij (vector-ref data (+ (* i n) j))])
                            (row-loop (+ j 1)
                                      (+ row-h (if (<= p-ij 0)
                                                    0
                                                    (* p-ij (log2 p-ij))))))
                          )))))))))

;;; ====
;;; Internal Helpers
;;; ====

(define (vec-sum-raw v)
  (doc 'description "Sum of vector elements (no error wrapping)")
  (let ([n (vector-length v)])
    (let loop ([i 0] [s 0])
      (if (= i n) s
          (loop (+ i 1) (+ s (vector-ref v i)))))))

(define (l1-distance v1 v2)
  (doc 'description "L1 distance between two vectors")
  (let ([n (vector-length v1)])
    (let loop ([i 0] [s 0])
      (if (= i n) s
          (loop (+ i 1) (+ s (abs (- (vector-ref v1 i) (vector-ref v2 i)))))))))

(define (total-variation-distance v1 v2)
  (doc 'description "Total variation distance = (1/2) * L1 distance")
  (* 0.5 (l1-distance v1 v2)))

(define (all-positive? M)
  (doc 'description "Check if all entries in a matrix are positive (> 0)")
  (let ([data (matrix-data M)]
        [n (vector-length (matrix-data M))])
    (let loop ([i 0])
      (cond
        [(= i n) #t]
        [(<= (vector-ref data i) 0) #f]
        [else (loop (+ i 1))]))))

(define (all-positive-diagonal? M)
  (doc 'description "Check if all diagonal entries are positive")
  (let ([n (matrix-rows M)]
        [data (matrix-data M)])
    (let loop ([i 0])
      (if (= i n) #t
          (if (<= (vector-ref data (+ (* i n) i)) 0)
              #f
              (loop (+ i 1)))))))

(define (has-self-loop? P)
  (doc 'description "Check if any state has P(i,i) > 0")
  (let ([n (matrix-rows P)]
        [data (matrix-data P)])
    (let loop ([i 0])
      (if (= i n) #f
          (if (> (vector-ref data (+ (* i n) i)) 0)
              #t
              (loop (+ i 1)))))))

(define (matrix-power-fast* M k)
  (doc 'description "Fast matrix power via repeated squaring (local, avoids depending on graph-matrix)")
  (cond [(= k 0) (matrix-identity (matrix-rows M))]
        [(= k 1) M]
        [(even? k)
         (let ([half (matrix-power-fast* M (/ k 2))])
           (matrix-mul half half))]
        [else
         (matrix-mul M (matrix-power-fast* M (- k 1)))]))
