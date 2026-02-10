(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'normal-form)

(doc 'module 'evolutionary)
(doc 'description "Evolutionary Game Theory: replicator dynamics, evolutionarily stable strategies (ESS), population games, and invasion dynamics")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'tier 1)
(doc 'see-also '(normal-form.ss coop-games.ss))

;;; ============================================================================
;;; Section 1: Symmetric Games
;;; ============================================================================

(doc 'section 'symmetric-games)
(doc 'note "A symmetric game has the same strategy set for all players.")
(doc 'note "Payoff matrix A where A[i][j] = payoff to row player using i against column player using j.")
(doc 'note "Symmetry: interchanging players' strategies gives same payoffs.")

;;; make-symmetric-game : (List Symbol) × (List (List Number)) → SymmetricGame
;;; Create a symmetric 2-player game from strategy names and payoff matrix.
;;; Matrix[i][j] = payoff when playing strategy i against strategy j.
(define (make-symmetric-game strategies payoff-matrix)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol) (List (List Number)) SymmetricGame))
  (list 'symmetric-game
        (list->vector strategies)
        (list->vector (map list->vector payoff-matrix))))

(define (symmetric-game? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'symmetric-game)))

(define (sg-strategies game) (cadr game))
(define (sg-payoffs game) (caddr game))

(define (sg-num-strategies game)
  (doc 'export #t)
  (vector-length (sg-strategies game)))

;;; sg-payoff : SymmetricGame × Nat × Nat → Number
;;; Payoff for playing strategy i against strategy j.
(define (sg-payoff game i j)
  (doc 'export #t)
  (vector-ref (vector-ref (sg-payoffs game) i) j))

;;; sg-strategy-name : SymmetricGame × Nat → Symbol
(define (sg-strategy-name game i)
  (doc 'export #t)
  (vector-ref (sg-strategies game) i))

;;; ============================================================================
;;; Section 2: Classic Symmetric Games
;;; ============================================================================

(doc 'section 'classic-symmetric-games)

;;; Hawk-Dove (Chicken) game
;;; V = value of resource, C = cost of fighting
;;; Hawk vs Hawk: (V-C)/2 each (fight, split value minus cost)
;;; Hawk vs Dove: Hawk gets V, Dove gets 0
;;; Dove vs Dove: V/2 each (share)
;;; Standard parameters: V=2, C=4
(doc hawk-dove-game 'export #t)
(define hawk-dove-game
  (make-symmetric-game
   '(hawk dove)
   '((-1  2)    ; Hawk: (V-C)/2=-1 vs Hawk, V=2 vs Dove
     ( 0  1)))) ; Dove: 0 vs Hawk, V/2=1 vs Dove

;;; Stag Hunt (coordination game)
(doc stag-hunt-symmetric 'export #t)
(define stag-hunt-symmetric
  (make-symmetric-game
   '(stag hare)
   '((4 0)      ; Stag: 4 if both hunt stag, 0 if partner hunts hare
     (3 3))))   ; Hare: always get 3

;;; Prisoner's Dilemma (symmetric version)
(doc pd-symmetric 'export #t)
(define pd-symmetric
  (make-symmetric-game
   '(cooperate defect)
   '((3 0)      ; Cooperate: 3 vs C, 0 vs D (sucker's payoff)
     (5 1))))   ; Defect: 5 vs C (temptation), 1 vs D (punishment)

;;; Rock-Paper-Scissors
(doc rps-game 'export #t)
(define rps-game
  (make-symmetric-game
   '(rock paper scissors)
   '(( 0 -1  1)   ; Rock: tie, lose to paper, beat scissors
     ( 1  0 -1)   ; Paper: beat rock, tie, lose to scissors
     (-1  1  0)))) ; Scissors: lose to rock, beat paper, tie

;;; ============================================================================
;;; Section 3: Population State
;;; ============================================================================

(doc 'section 'population-state)
(doc 'note "A population state is a probability distribution over strategies.")
(doc 'note "x = (x₁, x₂, ..., xₙ) where xᵢ ≥ 0 and Σxᵢ = 1.")

;;; make-population : (List Number) → Population | (error 'empty-population)
;;; Create a population state. Automatically normalizes to sum to 1.
;;; Returns error if weights list is empty.
(define (make-population weights)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Population))
  (if (null? weights)
      (list 'error 'empty-population)
      (let* ([n (length weights)]
             [total (apply + weights)]
             [normalized (if (= total 0)
                             (make-list n (/ 1.0 n))
                             (map (lambda (w) (/ w total)) weights))])
        (list 'population (list->vector normalized)))))

(define (population? x)
  (and (pair? x) (eq? (car x) 'population)))

(define (pop-vec pop) (cadr pop))

(define (pop-size pop)
  (doc 'export #t)
  (vector-length (pop-vec pop)))

;;; pop-freq : Population × Nat → Number
;;; Get frequency of strategy i in population.
(define (pop-freq pop i)
  (doc 'export #t)
  (vector-ref (pop-vec pop) i))

;;; pop->list : Population → (List Number)
(define (pop->list pop)
  (doc 'export #t)
  (vector->list (pop-vec pop)))

;;; uniform-population : Nat → Population
;;; Create uniform distribution over n strategies.
(define (uniform-population n)
  (doc 'export #t)
  (make-population (make-list n 1)))

;;; pure-population : Nat × Nat → Population
;;; Create population where everyone plays strategy i (out of n).
(define (pure-population n i)
  (doc 'export #t)
  (make-population
   (map (lambda (j) (if (= j i) 1 0)) (iota n))))

;;; ============================================================================
;;; Section 4: Fitness Functions
;;; ============================================================================

(doc 'section 'fitness)
(doc 'note "Fitness is expected payoff against the population.")
(doc 'note "fᵢ(x) = Σⱼ A[i][j] × xⱼ where A is payoff matrix, x is population.")

;;; strategy-fitness : SymmetricGame × Population × Nat → Number
;;; Expected payoff of playing strategy i against population.
(define (strategy-fitness game pop i)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame Population Nat Number))
  (let ([n (sg-num-strategies game)])
    (let loop ([j 0] [sum 0])
      (if (>= j n)
          sum
          (loop (+ j 1)
                (+ sum (* (sg-payoff game i j)
                          (pop-freq pop j))))))))

;;; all-fitnesses : SymmetricGame × Population → (List Number)
;;; Compute fitness of each strategy against the population.
(define (all-fitnesses game pop)
  (doc 'export #t)
  (let ([n (sg-num-strategies game)])
    (map (lambda (i) (strategy-fitness game pop i))
         (iota n))))

;;; average-fitness : SymmetricGame × Population → Number
;;; Population average fitness: f̄(x) = Σᵢ xᵢ × fᵢ(x)
(define (average-fitness game pop)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame Population Number))
  (average-fitness-from-list (all-fitnesses game pop) pop))

;;; average-fitness-from-list : (List Number) × Population → Number
;;; Compute average fitness from pre-computed fitness list (avoids redundant computation).
(define (average-fitness-from-list fitnesses pop)
  (doc 'export #t)
  (let ([n (length fitnesses)])
    (let loop ([i 0] [sum 0])
      (if (>= i n)
          sum
          (loop (+ i 1)
                (+ sum (* (pop-freq pop i)
                          (list-ref fitnesses i))))))))

;;; ============================================================================
;;; Section 5: Replicator Dynamics
;;; ============================================================================

(doc 'section 'replicator-dynamics)
(doc 'note "Replicator dynamics: ẋᵢ = xᵢ(fᵢ(x) - f̄(x))")
(doc 'note "Strategies with above-average fitness grow; below-average shrink.")
(doc 'note "This is the canonical dynamics for evolutionary game theory.")

;;; replicator-derivative : SymmetricGame × Population → (List Number)
;;; Compute ẋᵢ = xᵢ(fᵢ - f̄) for each strategy.
(define (replicator-derivative game pop)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame Population (List Number)))
  (let* ([n (sg-num-strategies game)]
         [fitnesses (all-fitnesses game pop)]
         [avg-fit (average-fitness-from-list fitnesses pop)])  ; Use pre-computed fitnesses
    (map (lambda (i)
           (let ([xi (pop-freq pop i)]
                 [fi (list-ref fitnesses i)])
             (* xi (- fi avg-fit))))
         (iota n))))

;;; replicator-step : SymmetricGame × Population × Number → Population
;;; One Euler step of replicator dynamics with step size dt.
;;; x(t+dt) = x(t) + dt × ẋ(t)
(define (replicator-step game pop dt)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame Population Number Population))
  (let* ([derivs (replicator-derivative game pop)]
         [new-freqs (map (lambda (i)
                           (max 0 (+ (pop-freq pop i)
                                     (* dt (list-ref derivs i)))))
                         (iota (pop-size pop)))])
    (make-population new-freqs)))

;;; replicator-trajectory : SymmetricGame × Population × Number × Nat → (List Population)
;;; Simulate replicator dynamics for n steps.
(define (replicator-trajectory game pop dt steps)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame Population Number Nat (List Population)))
  (let loop ([current pop] [remaining steps] [acc (list pop)])
    (if (<= remaining 0)
        (reverse acc)
        (let ([next (replicator-step game current dt)])
          (loop next (- remaining 1) (cons next acc))))))

;;; replicator-converge : SymmetricGame × Population × Number × Number × Nat → (ok Population Nat) | (error 'max-iter Population Nat)
;;; Run replicator dynamics until convergence or max iterations.
;;; Converged when max derivative < tolerance.
;;; Returns (ok population iterations) on convergence, (error 'max-iter population iterations) if max iterations reached.
(define (replicator-converge game pop dt tolerance max-iter)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame Population Number Number Nat Result))
  (let loop ([current pop] [iter 0])
    (if (>= iter max-iter)
        (list 'error 'max-iter current iter)  ; Did not converge
        (let* ([derivs (replicator-derivative game current)]
               [max-deriv (apply max (map abs derivs))])
          (if (< max-deriv tolerance)
              (list 'ok current iter)  ; Converged
              (loop (replicator-step game current dt) (+ iter 1)))))))

;;; ============================================================================
;;; Section 6: Evolutionarily Stable Strategies (ESS)
;;; ============================================================================

(doc 'section 'ess)
(doc 'note "A strategy i is an ESS if it cannot be invaded by any mutant.")
(doc 'note "Formally: For all j ≠ i, either:")
(doc 'note "  1. A[i][i] > A[j][i]  (strict Nash against itself), or")
(doc 'note "  2. A[i][i] = A[j][i] and A[i][j] > A[j][j]  (stability condition)")
(doc 'note "An ESS is a refinement of Nash equilibrium.")

;;; is-ess? : SymmetricGame × Nat → Boolean
;;; Check if strategy i is an ESS (cannot be invaded).
(define (is-ess? game i)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame Nat Boolean))
  (let ([n (sg-num-strategies game)]
        [aii (sg-payoff game i i)])
    (let loop ([j 0])
      (cond
        [(>= j n) #t]  ; Checked all strategies
        [(= j i) (loop (+ j 1))]  ; Skip self
        [else
         (let ([aji (sg-payoff game j i)]
               [aij (sg-payoff game i j)]
               [ajj (sg-payoff game j j)])
           (and (or (> aii aji)                    ; Strict Nash condition
                    (and (= aii aji)               ; Equal payoff vs incumbent
                         (> aij ajj)))             ; But incumbent does better vs mutant
                (loop (+ j 1))))]))))

;;; find-all-ess : SymmetricGame → (List Nat)
;;; Find all pure strategy ESS in the game.
(define (find-all-ess game)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame (List Nat)))
  (filter (lambda (i) (is-ess? game i))
          (iota (sg-num-strategies game))))

;;; is-nash-equilibrium? : SymmetricGame × Nat → Boolean
;;; Check if pure strategy i is a symmetric Nash equilibrium.
;;; i is Nash iff A[i][i] >= A[j][i] for all j.
(define (is-nash-equilibrium? game i)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame Nat Boolean))
  (let ([n (sg-num-strategies game)]
        [aii (sg-payoff game i i)])
    (let loop ([j 0])
      (cond
        [(>= j n) #t]
        [(= j i) (loop (+ j 1))]
        [else
         (and (>= aii (sg-payoff game j i))
              (loop (+ j 1)))]))))

;;; find-pure-nash : SymmetricGame → (List Nat)
;;; Find all pure symmetric Nash equilibria.
(define (find-pure-nash game)
  (doc 'export #t)
  (filter (lambda (i) (is-nash-equilibrium? game i))
          (iota (sg-num-strategies game))))

;;; ============================================================================
;;; Section 7: Invasion Dynamics
;;; ============================================================================

(doc 'section 'invasion)
(doc 'note "Invasion analysis: can a small population of mutants invade?")
(doc 'note "A mutant j can invade incumbent i if f_j((1-ε)i + εj) > f_i((1-ε)i + εj)")
(doc 'note "for small ε > 0. This simplifies to checking A[j][i] > A[i][i]")
(doc 'note "or (A[j][i] = A[i][i] and A[j][j] > A[i][j]).")

;;; can-invade? : SymmetricGame × Nat × Nat → Boolean
;;; Can mutant strategy j invade a population of strategy i?
(define (can-invade? game incumbent mutant)
  (doc 'export #t)
  (doc 'type '(-> SymmetricGame Nat Nat Boolean))
  (let ([aii (sg-payoff game incumbent incumbent)]
        [aji (sg-payoff game mutant incumbent)]
        [aij (sg-payoff game incumbent mutant)]
        [ajj (sg-payoff game mutant mutant)])
    (or (> aji aii)
        (and (= aji aii)
             (> ajj aij)))))

;;; invasion-fitness : SymmetricGame × Nat × Nat × Number → Number
;;; Fitness of mutant j in population (1-ε)i + εj.
(define (invasion-fitness game incumbent mutant epsilon)
  (doc 'export #t)
  (let ([aij (sg-payoff game mutant incumbent)]
        [ajj (sg-payoff game mutant mutant)])
    (+ (* (- 1 epsilon) aij)
       (* epsilon ajj))))

;;; incumbent-fitness : SymmetricGame × Nat × Nat × Number → Number
;;; Fitness of incumbent i in population (1-ε)i + εj.
(define (incumbent-fitness game incumbent mutant epsilon)
  (let ([aii (sg-payoff game incumbent incumbent)]
        [aij (sg-payoff game incumbent mutant)])
    (+ (* (- 1 epsilon) aii)
       (* epsilon aij))))

;;; invasion-gradient : SymmetricGame × Nat × Nat × Number → Number
;;; Difference in fitness: f_mutant - f_incumbent at invasion proportion ε.
(define (invasion-gradient game incumbent mutant epsilon)
  (doc 'export #t)
  (- (invasion-fitness game incumbent mutant epsilon)
     (incumbent-fitness game incumbent mutant epsilon)))

;;; ============================================================================
;;; Section 8: Mixed Strategy Analysis
;;; ============================================================================

(doc 'section 'mixed-strategies)
(doc 'note "Mixed ESS and interior equilibria analysis.")

;;; is-interior-equilibrium? : SymmetricGame × Population × Number → Boolean
;;; Check if population is an interior equilibrium (all strategies have equal fitness).
(define (is-interior-equilibrium? game pop tolerance)
  (doc 'export #t)
  (let* ([fitnesses (all-fitnesses game pop)]
         [avg (average-fitness game pop)])
    (andmap (lambda (f) (< (abs (- f avg)) tolerance))
            fitnesses)))

;;; hawk-dove-mixed-ess : Number × Number → Number | (error 'zero-cost)
;;; For Hawk-Dove with value V and cost C, the mixed ESS has p(Hawk) = V/C.
;;; Requires C > 0 (if fighting has no cost, Hawks always dominate).
(define (hawk-dove-mixed-ess V C)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number))
  (doc 'description "Proportion of Hawks at mixed ESS")
  (if (= C 0)
      (list 'error 'zero-cost)  ; No mixed ESS when fighting is costless
      (/ V C)))

;;; ============================================================================
;;; Section 9: Payoff Matrix Operations
;;; ============================================================================

(doc 'section 'payoff-operations)

;;; sg-payoff-matrix->list : SymmetricGame → (List (List Number))
;;; Extract payoff matrix as nested lists.
(define (sg-payoff-matrix->list game)
  (doc 'export #t)
  (map vector->list
       (vector->list (sg-payoffs game))))

;;; sg-transpose : SymmetricGame → SymmetricGame
;;; Transpose the payoff matrix (swap row/column player perspectives).
(define (sg-transpose game)
  (doc 'export #t)
  (let* ([n (sg-num-strategies game)]
         [new-payoffs
          (map (lambda (i)
                 (map (lambda (j) (sg-payoff game j i))
                      (iota n)))
               (iota n))])
    (make-symmetric-game
     (vector->list (sg-strategies game))
     new-payoffs)))

;;; ============================================================================
;;; Section 10: Display and Analysis
;;; ============================================================================

(doc 'section 'display)

;;; population->string : Population → String
(define (population->string pop)
  (doc 'export #t)
  (let ([freqs (pop->list pop)])
    (string-append
     "("
     (apply string-append
            (map (lambda (f) (format "~,3f " f)) freqs))
     ")")))

;;; analyze-game : SymmetricGame → Void
;;; Print analysis of the game: Nash equilibria, ESS, etc.
(define (analyze-game game)
  (doc 'export #t)
  (let ([n (sg-num-strategies game)]
        [nash (find-pure-nash game)]
        [ess (find-all-ess game)])
    (display "=== Game Analysis ===\n")
    (display (format "Strategies: ~a\n" (vector->list (sg-strategies game))))
    (display "Payoff matrix:\n")
    (for-each
     (lambda (i)
       (display "  ")
       (for-each
        (lambda (j)
          (display (format "~5,1f " (sg-payoff game i j))))
        (iota n))
       (newline))
     (iota n))
    (display (format "Pure Nash equilibria: ~a\n"
                     (map (lambda (i) (sg-strategy-name game i)) nash)))
    (display (format "ESS: ~a\n"
                     (map (lambda (i) (sg-strategy-name game i)) ess)))))

;;; ============================================================================
;;; Display
;;; ============================================================================

(display "Key functions: make-symmetric-game, replicator-step, is-ess?, can-invade?\n")
