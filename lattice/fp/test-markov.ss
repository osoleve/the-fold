;;; test-markov.ss -- Tests for lattice/fp/markov.ss

(load "core/testing/test-framework.ss")
(load "lattice/fp/markov.ss")

;;; ====
;;; Helper: approximate equality for vectors
;;; ====

(define (vec-approx? v1 v2 tol)
  (and (= (vector-length v1) (vector-length v2))
       (let ([n (vector-length v1)])
         (let loop ([i 0])
           (if (= i n) #t
               (if (< (abs (- (vector-ref v1 i) (vector-ref v2 i))) tol)
                   (loop (+ i 1))
                   #f))))))

(define (vec-sums-to-one? v tol)
  (let ([n (vector-length v)])
    (let loop ([i 0] [s 0])
      (if (= i n)
          (< (abs (- s 1.0)) tol)
          (loop (+ i 1) (+ s (vector-ref v i)))))))

(define (vec-all-nonneg? v)
  (let ([n (vector-length v)])
    (let loop ([i 0])
      (if (= i n) #t
          (if (>= (vector-ref v i) 0)
              (loop (+ i 1))
              #f)))))

;;; ====
;;; Test: Construction
;;; ====

(test-group "markov-construction"

  (define-test "make-markov-chain with valid 2x2 matrix"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [mc (make-markov-chain P)])
      (assert-true (markov-chain? mc))))

  (define-test "make-markov-chain rejects non-square"
    (let* ([P (matrix-from-lists '((0.5 0.5) (0.3 0.7) (0.1 0.9)))]
           [result (make-markov-chain P)])
      (assert-true (and (pair? result) (eq? (car result) 'error)))))

  (define-test "make-markov-chain rejects non-stochastic rows"
    (let* ([P (matrix-from-lists '((0.5 0.3) (0.4 0.6)))]
           [result (make-markov-chain P)])
      (assert-true (and (pair? result) (eq? (car result) 'error)))))

  (define-test "markov-num-states returns correct count"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [mc (make-markov-chain P)])
      (assert-equal 2 (markov-num-states mc))))

)

;;; ====
;;; Test: Single step and multi-step
;;; ====

(test-group "markov-evolution"

  (define-test "markov-step produces valid distribution"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (vector 1.0 0.0)]
           [pi-next (markov-step pi P)])
      ;; Starting from state 0: pi' = [0.7, 0.3]
      (assert-true (vec-approx? pi-next (vector 0.7 0.3) 1e-10))
      (assert-true (vec-sums-to-one? pi-next 1e-10))
      (assert-true (vec-all-nonneg? pi-next))))

  (define-test "markov-step from state 1"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (vector 0.0 1.0)]
           [pi-next (markov-step pi P)])
      (assert-true (vec-approx? pi-next (vector 0.4 0.6) 1e-10))))

  (define-test "markov-steps n=0 returns input"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (vector 0.5 0.5)]
           [result (markov-steps pi P 0)])
      (assert-true (vec-approx? result pi 1e-10))))

  (define-test "markov-steps n=1 equals markov-step"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (vector 1.0 0.0)]
           [one-step (markov-step pi P)]
           [n-steps (markov-steps pi P 1)])
      (assert-true (vec-approx? one-step n-steps 1e-10))))

  (define-test "markov-steps n=2 equals two applications"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (vector 1.0 0.0)]
           [two-steps (markov-steps pi P 2)]
           [manual (markov-step (markov-step pi P) P)])
      (assert-true (vec-approx? two-steps manual 1e-10))))

)

;;; ====
;;; Test: Stationary distribution
;;; ====

(test-group "stationary-distribution"

  (define-test "2-state chain: known stationary distribution"
    ;; P = [[0.7, 0.3], [0.4, 0.6]]
    ;; Stationary: pi_0 = 0.4/(0.3+0.4) = 4/7, pi_1 = 3/7
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (stationary-distribution P)]
           [expected (vector (/ 4.0 7.0) (/ 3.0 7.0))])
      (assert-true (vec-approx? pi expected 1e-8))))

  (define-test "stationary distribution sums to 1"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (stationary-distribution P)])
      (assert-true (vec-sums-to-one? pi 1e-10))))

  (define-test "stationary distribution is non-negative"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (stationary-distribution P)])
      (assert-true (vec-all-nonneg? pi))))

  (define-test "stationary distribution is a fixed point: pi*P = pi"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (stationary-distribution P)]
           [pi-next (vec-matrix-mul pi P)])
      (assert-true (vec-approx? pi pi-next 1e-8))))

  (define-test "3-state chain: stationary distribution sums to 1"
    (let* ([P (matrix-from-lists '((0.5 0.25 0.25)
                                   (0.2 0.6  0.2)
                                   (0.1 0.3  0.6)))]
           [pi (stationary-distribution P)])
      (assert-true (vec-sums-to-one? pi 1e-8))
      (assert-true (vec-all-nonneg? pi))))

  (define-test "3-state chain: stationary is fixed point"
    (let* ([P (matrix-from-lists '((0.5 0.25 0.25)
                                   (0.2 0.6  0.2)
                                   (0.1 0.3  0.6)))]
           [pi (stationary-distribution P)]
           [pi-next (vec-matrix-mul pi P)])
      (assert-true (vec-approx? pi pi-next 1e-8))))

  (define-test "identity matrix: uniform stationary distribution"
    ;; I is row-stochastic; any distribution is stationary.
    ;; Power iteration from uniform stays uniform.
    (let* ([I (matrix-from-lists '((1 0 0) (0 1 0) (0 0 1)))]
           [pi (stationary-distribution I)]
           [expected (vector (/ 1.0 3) (/ 1.0 3) (/ 1.0 3))])
      (assert-true (vec-approx? pi expected 1e-8))))

  (define-test "doubly stochastic: uniform is stationary"
    ;; A doubly-stochastic matrix has uniform stationary distribution
    (let* ([P (matrix-from-lists '((0.5 0.25 0.25)
                                   (0.25 0.5 0.25)
                                   (0.25 0.25 0.5)))]
           [pi (stationary-distribution P)]
           [expected (vector (/ 1.0 3) (/ 1.0 3) (/ 1.0 3))])
      (assert-true (vec-approx? pi expected 1e-8))))

)

;;; ====
;;; Test: Ergodicity
;;; ====

(test-group "ergodicity"

  (define-test "fully connected chain with self-loops is ergodic"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))])
      (assert-true (is-ergodic? P))))

  (define-test "irreducible chain with self-loops is ergodic"
    (let* ([P (matrix-from-lists '((0.5 0.25 0.25)
                                   (0.2 0.6  0.2)
                                   (0.1 0.3  0.6)))])
      (assert-true (is-ergodic? P))))

  (define-test "periodic chain is not ergodic"
    ;; 0 -> 1 -> 0 -> 1 -> ... (period 2)
    (let* ([P (matrix-from-lists '((0 1) (1 0)))])
      (assert-false (is-ergodic? P))))

  (define-test "periodic chain is irreducible but not aperiodic"
    (let* ([P (matrix-from-lists '((0 1) (1 0)))])
      (assert-true (is-irreducible? P))
      (assert-false (is-aperiodic? P))))

  (define-test "absorbing chain is not irreducible"
    ;; State 0 is absorbing: once you enter, you can't leave
    (let* ([P (matrix-from-lists '((1.0 0.0) (0.5 0.5)))])
      (assert-false (is-irreducible? P))))

  (define-test "3-state directed cycle is irreducible but periodic"
    ;; 0->1->2->0
    (let* ([P (matrix-from-lists '((0 1 0) (0 0 1) (1 0 0)))])
      (assert-true (is-irreducible? P))
      (assert-false (is-aperiodic? P))
      (assert-false (is-ergodic? P))))

)

;;; ====
;;; Test: Mixing time
;;; ====

(test-group "mixing-time"

  (define-test "identity matrix has infinite mixing time (exceeds bound)"
    ;; Starting from delta_0, TV distance to uniform never decreases
    ;; Actually, identity has every distribution as stationary, so
    ;; starting from delta_0, TV to uniform = 1 - 1/n, never changes
    (let* ([I (matrix-from-lists '((1 0) (0 1)))]
           [result (mixing-time I 0.1 1e-12 100)])
      ;; Should fail to converge or return error
      (assert-true (or (and (pair? result) (eq? (car result) 'error))
                       ;; If stationary-distribution returns uniform, TV from
                       ;; delta_0 to uniform = (1-1/2)*2/2 = 0.5, > 0.1
                       ;; so we should get an error
                       (> result 50)))))

  (define-test "fast-mixing chain has small mixing time"
    ;; Nearly uniform transition: mixes fast
    (let* ([P (matrix-from-lists '((0.5 0.5) (0.5 0.5)))]
           [t (mixing-time P 0.01)])
      (assert-true (and (integer? t) (< t 5)))))

  (define-test "mixing time for ergodic chain is finite"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [t (mixing-time P 0.01)])
      (assert-true (and (integer? t) (> t 0)))))

)

;;; ====
;;; Test: Entropy rate
;;; ====

(test-group "entropy-rate"

  (define-test "entropy rate of deterministic chain is 0"
    ;; Deterministic: 0->1->0->1...
    ;; Each row has exactly one 1, so H(row) = 0 for each state
    (let* ([P (matrix-from-lists '((0 1) (1 0)))]
           [h (markov-entropy-rate P)])
      (assert-true (< (abs h) 1e-10))))

  (define-test "entropy rate of uniform random walk = log2(n)"
    ;; Doubly stochastic uniform: P(i,j) = 1/n for all i,j
    ;; Each row has entropy log2(n)
    ;; Stationary distribution is uniform (1/n each)
    ;; Entropy rate = sum_i (1/n) * log2(n) = log2(n)
    (let* ([n 4]
           [p (/ 1.0 n)]
           [row (make-list n p)]
           [rows (make-list n row)]
           [P (matrix-from-lists rows)]
           [h (markov-entropy-rate P)]
           [expected (log2 n)])
      (assert-true (< (abs (- h expected)) 1e-8))))

  (define-test "entropy rate of 2-state symmetric chain"
    ;; P = [[0.5 0.5] [0.5 0.5]]
    ;; Stationary = [0.5 0.5]
    ;; Each row has entropy = 1 bit
    ;; Entropy rate = 0.5*1 + 0.5*1 = 1
    (let* ([P (matrix-from-lists '((0.5 0.5) (0.5 0.5)))]
           [h (markov-entropy-rate P)])
      (assert-true (< (abs (- h 1.0)) 1e-8))))

  (define-test "entropy rate is non-negative"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [h (markov-entropy-rate P)])
      (assert-true (>= h 0))))

  (define-test "entropy rate <= log2(n)"
    ;; Entropy rate is bounded above by log2(number of states)
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [h (markov-entropy-rate P)])
      (assert-true (<= h (+ (log2 2) 1e-10)))))

  (define-test "entropy rate with provided stationary distribution"
    (let* ([P (matrix-from-lists '((0.7 0.3) (0.4 0.6)))]
           [pi (stationary-distribution P)]
           [h1 (markov-entropy-rate P)]
           [h2 (markov-entropy-rate P pi)])
      (assert-true (< (abs (- h1 h2)) 1e-8))))

)

;;; ====
;;; Test: Absorbing chains
;;; ====

(test-group "absorbing-chains"

  (define-test "absorbing chain stationary: mass on absorbing state"
    ;; State 0 absorbing, state 1 has P(1,0)=0.5, P(1,1)=0.5
    ;; Stationary should put all mass on state 0
    (let* ([P (matrix-from-lists '((1.0 0.0) (0.5 0.5)))]
           [pi (stationary-distribution P)])
      (assert-true (< (abs (- (vector-ref pi 0) 1.0)) 1e-6))
      (assert-true (< (abs (vector-ref pi 1)) 1e-6))))

  (define-test "two absorbing states: stationary depends on initial"
    ;; Both states absorbing: P = I
    ;; Power iteration from uniform gives uniform
    (let* ([P (matrix-from-lists '((1 0) (0 1)))]
           [pi (stationary-distribution P)])
      ;; From uniform start, stays uniform
      (assert-true (vec-approx? pi (vector 0.5 0.5) 1e-8))))

)

;;; ====
;;; Run
;;; ====

(run-all-tests)
