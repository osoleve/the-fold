;;; evolutionary.sexp — Development tasks for lattice/game-theory/evolutionary.ss
;;;
;;; Module: evolutionary (tier 1, depends on normal-form)
;;; 471 lines, ~25 exported functions
;;; Evolutionary game theory: symmetric games, population state, fitness
;;; functions, replicator dynamics, ESS (evolutionarily stable strategies),
;;; invasion dynamics, mixed strategy analysis.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/game-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement strategy-fitness
  ;; ──────────────────────────────────────────────
  ;; Expected payoff of a strategy against a population.

  (task
    (id "game-theory/evolutionary/strategy-fitness-impl")
    (module evolutionary)
    (tier 0)
    (type implement)
    (description "Implement strategy-fitness: compute the expected payoff of playing strategy i against a population. This is the core fitness calculation in evolutionary game theory. Fitness f_i(x) = sum_j A[i][j] * x_j, where A is the payoff matrix and x is the population frequency vector. Loop over all strategies j, accumulating the product of the payoff A[i][j] (via sg-payoff) and the population frequency x_j (via pop-freq). Use a named let with index j and running sum.")
    (context "Available: sg-num-strategies, sg-payoff, pop-freq, >=, +, *, let. Named let loop with two accumulators (j, sum).")
    (before "(define (strategy-fitness game pop i)
  ;; TODO: f_i(x) = sum_j A[i][j] * x_j
  0)")
    (test-file "lattice/game-theory/test-evolutionary.ss")
    (test-forms (";; Hawk fitness in 50/50 population: (-1)*0.5 + 2*0.5 = 0.5
(let ([pop (make-population '(0.5 0.5))])
  (assert-equal 0.5 (strategy-fitness hawk-dove-game pop 0)))"
                 ";; Dove fitness in 50/50 population: 0*0.5 + 1*0.5 = 0.5
(let ([pop (make-population '(0.5 0.5))])
  (assert-equal 0.5 (strategy-fitness hawk-dove-game pop 1)))"
                 ";; Pure Hawk population: Hawk fitness = -1, Dove fitness = 0
(let ([pop (pure-population 2 0)])
  (assert-equal -1 (strategy-fitness hawk-dove-game pop 0))
  (assert-equal 0 (strategy-fitness hawk-dove-game pop 1)))"
                 ";; Defect fitness in pure cooperator population = 5 (temptation)
(let ([pop (pure-population 2 0)])
  (assert-equal 5 (strategy-fitness pd-symmetric pop 1)))"))
    (available-modules (prelude normal-form))
    (hints ("n = (sg-num-strategies game)"
            "Named let loop: (let loop ([j 0] [sum 0]) ...)"
            "Base: (>= j n) -> sum"
            "Step: (+ sum (* (sg-payoff game i j) (pop-freq pop j)))"))
    (reference-solution "(define (strategy-fitness game pop i)
  (let ([n (sg-num-strategies game)])
    (let loop ([j 0] [sum 0])
      (if (>= j n)
          sum
          (loop (+ j 1)
                (+ sum (* (sg-payoff game i j)
                          (pop-freq pop j))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement replicator-derivative
  ;; ──────────────────────────────────────────────
  ;; The canonical dynamics of evolutionary game theory.

  (task
    (id "game-theory/evolutionary/replicator-derivative-impl")
    (module evolutionary)
    (tier 0)
    (type implement)
    (description "Implement replicator-derivative: compute the time derivative of each strategy's frequency under replicator dynamics. The replicator equation is dx_i/dt = x_i * (f_i(x) - f_bar(x)), where f_i is strategy i's fitness and f_bar is the population average fitness. Strategies with above-average fitness grow; below-average fitness shrink. Steps: (1) Compute all fitnesses via all-fitnesses. (2) Compute average fitness via average-fitness-from-list. (3) For each strategy i, compute x_i * (f_i - f_bar). Return as a list.")
    (context "Available: sg-num-strategies, all-fitnesses, average-fitness-from-list, pop-freq, list-ref, map, iota, *, -, let*. Three computations then a map.")
    (before "(define (replicator-derivative game pop)
  ;; TODO: dx_i/dt = x_i * (f_i - f_bar) for each i
  '())")
    (test-file "lattice/game-theory/test-evolutionary.ss")
    (test-forms (";; At 50/50 Hawk-Dove equilibrium, all derivatives = 0
(let ([pop (make-population '(0.5 0.5))])
  (assert-equal '(0.0 0.0) (replicator-derivative hawk-dove-game pop)))"
                 ";; Pure population: derivatives are 0 (no selection pressure)
(let ([pop (pure-population 2 0)])
  (assert-equal '(0 0) (replicator-derivative hawk-dove-game pop)))"
                 ";; Off-equilibrium: Hawk-heavy should push toward equilibrium
(let* ([pop (make-population '(0.7 0.3))]
       [derivs (replicator-derivative hawk-dove-game pop)])
  ;; Hawk frequency should decrease (negative derivative)
  (assert-true (< (car derivs) 0)))"))
    (available-modules (prelude normal-form))
    (hints ("fitnesses = (all-fitnesses game pop)"
            "avg-fit = (average-fitness-from-list fitnesses pop)"
            "Map over (iota n): for each i, (* (pop-freq pop i) (- (list-ref fitnesses i) avg-fit))"))
    (reference-solution "(define (replicator-derivative game pop)
  (let* ([n (sg-num-strategies game)]
         [fitnesses (all-fitnesses game pop)]
         [avg-fit (average-fitness-from-list fitnesses pop)])
    (map (lambda (i)
           (let ([xi (pop-freq pop i)]
                 [fi (list-ref fitnesses i)])
             (* xi (- fi avg-fit))))
         (iota n))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement is-ess?
  ;; ──────────────────────────────────────────────
  ;; The two-condition ESS check — a refinement of Nash.

  (task
    (id "game-theory/evolutionary/is-ess-impl")
    (module evolutionary)
    (tier 0)
    (type implement)
    (description "Implement is-ess?: check if pure strategy i is an Evolutionarily Stable Strategy. An ESS cannot be invaded by any mutant. For each alternative strategy j != i, check two conditions: (1) Strict Nash: A[i][i] > A[j][i] — the incumbent does strictly better against itself than any mutant does. OR (2) Stability: A[i][i] = A[j][i] AND A[i][j] > A[j][j] — if the mutant does equally well against the incumbent, the incumbent must do better against the mutant than the mutant does against itself. If ALL j pass, return #t. Use a named let loop over j, skipping j=i.")
    (context "Available: sg-num-strategies, sg-payoff, >=, =, >, and, or, cond, let, not. Named let with index j. Four payoff lookups per iteration: A[i][i], A[j][i], A[i][j], A[j][j].")
    (before "(define (is-ess? game i)
  ;; TODO: check ESS conditions for all j != i
  #f)")
    (test-file "lattice/game-theory/test-evolutionary.ss")
    (test-forms (";; Defect is ESS in Prisoner's Dilemma
(assert-true (is-ess? pd-symmetric 1))"
                 ";; Cooperate is NOT ESS in PD (Defect invades)
(assert-false (is-ess? pd-symmetric 0))"
                 ";; Neither Hawk nor Dove is ESS in Hawk-Dove
(assert-false (is-ess? hawk-dove-game 0))
(assert-false (is-ess? hawk-dove-game 1))"
                 ";; No pure ESS in Rock-Paper-Scissors
(assert-false (is-ess? rps-game 0))
(assert-false (is-ess? rps-game 1))
(assert-false (is-ess? rps-game 2))"))
    (available-modules (prelude normal-form))
    (hints ("aii = (sg-payoff game i i) — cache outside the loop"
            "For each j: aji = (sg-payoff game j i), aij = (sg-payoff game i j), ajj = (sg-payoff game j j)"
            "Pass condition: (or (> aii aji) (and (= aii aji) (> aij ajj)))"
            "Use (and pass-condition (loop (+ j 1))) for short-circuit"))
    (reference-solution "(define (is-ess? game i)
  (let ([n (sg-num-strategies game)]
        [aii (sg-payoff game i i)])
    (let loop ([j 0])
      (cond
        [(>= j n) #t]
        [(= j i) (loop (+ j 1))]
        [else
         (let ([aji (sg-payoff game j i)]
               [aij (sg-payoff game i j)]
               [ajj (sg-payoff game j j)])
           (and (or (> aii aji)
                    (and (= aii aji)
                         (> aij ajj)))
                (loop (+ j 1))))]))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement can-invade?
  ;; ──────────────────────────────────────────────
  ;; Invasion dynamics — can a mutant take over?

  (task
    (id "game-theory/evolutionary/can-invade-impl")
    (module evolutionary)
    (tier 0)
    (type implement)
    (description "Implement can-invade?: determine if a small population of mutant strategy j can invade a population of incumbent strategy i. A mutant can invade if it does better in a population that is mostly incumbent. Formally, mutant j invades incumbent i when either: (1) A[j][i] > A[i][i] — the mutant does strictly better against the incumbent than the incumbent does against itself, OR (2) A[j][i] = A[i][i] AND A[j][j] > A[i][j] — equal performance against incumbent, but mutant does better against itself than the incumbent does against the mutant. This is the negation of the ESS condition from the mutant's perspective.")
    (context "Available: sg-payoff, >, =, and, or, let. Four payoff lookups and two conditions.")
    (before "(define (can-invade? game incumbent mutant)
  ;; TODO: can mutant j invade incumbent i?
  #f)")
    (test-file "lattice/game-theory/test-evolutionary.ss")
    (test-forms (";; Defect can invade Cooperate in PD
(assert-true (can-invade? pd-symmetric 0 1))"
                 ";; Cooperate cannot invade Defect in PD
(assert-false (can-invade? pd-symmetric 1 0))"
                 ";; Both can invade each other in Hawk-Dove (no pure ESS)
(assert-true (can-invade? hawk-dove-game 0 1))
(assert-true (can-invade? hawk-dove-game 1 0))"
                 ";; Rock-Paper-Scissors: cyclic invasion
(assert-true (can-invade? rps-game 0 1))   ;; Paper invades Rock
(assert-true (can-invade? rps-game 1 2))   ;; Scissors invades Paper
(assert-true (can-invade? rps-game 2 0))")) ;; Rock invades Scissors
    (available-modules (prelude normal-form))
    (hints ("aii = (sg-payoff game incumbent incumbent)"
            "aji = (sg-payoff game mutant incumbent)"
            "aij = (sg-payoff game incumbent mutant)"
            "ajj = (sg-payoff game mutant mutant)"
            "Return: (or (> aji aii) (and (= aji aii) (> ajj aij)))"))
    (reference-solution "(define (can-invade? game incumbent mutant)
  (let ([aii (sg-payoff game incumbent incumbent)]
        [aji (sg-payoff game mutant incumbent)]
        [aij (sg-payoff game incumbent mutant)]
        [ajj (sg-payoff game mutant mutant)])
    (or (> aji aii)
        (and (= aji aii)
             (> ajj aij)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement average-fitness-from-list
  ;; ──────────────────────────────────────────────
  ;; Weighted average of pre-computed fitnesses.

  (task
    (id "game-theory/evolutionary/average-fitness-impl")
    (module evolutionary)
    (tier 0)
    (type implement)
    (description "Implement average-fitness-from-list: compute the population average fitness from a pre-computed list of strategy fitnesses. The average fitness is f_bar(x) = sum_i x_i * f_i, where x_i is the frequency of strategy i and f_i is its fitness. This function takes the fitness list (already computed by all-fitnesses) to avoid redundant computation. Use a named let loop with index i and running sum, accessing population frequencies via pop-freq and fitness values via list-ref.")
    (context "Available: length, pop-freq, list-ref, >=, +, *, let. Named let loop with index and accumulator.")
    (before "(define (average-fitness-from-list fitnesses pop)
  ;; TODO: f_bar = sum_i x_i * f_i
  0)")
    (test-file "lattice/game-theory/test-evolutionary.ss")
    (test-forms (";; Uniform population: average = mean of fitnesses
(let* ([pop (make-population '(0.5 0.5))]
       [fits (all-fitnesses hawk-dove-game pop)])
  (assert-equal 0.5 (average-fitness-from-list fits pop)))"
                 ";; Pure Hawk population: average = Hawk fitness
(let* ([pop (pure-population 2 0)]
       [fits (all-fitnesses hawk-dove-game pop)])
  (assert-equal -1 (average-fitness-from-list fits pop)))"
                 ";; Pure Dove population: average = Dove fitness
(let* ([pop (pure-population 2 1)]
       [fits (all-fitnesses hawk-dove-game pop)])
  (assert-equal 1 (average-fitness-from-list fits pop)))"
                 ";; PD uniform: avg = 0.5*(3+0) + 0.5*(5+1) / something...
;; fitnesses: C=0.5*3+0.5*0=1.5, D=0.5*5+0.5*1=3 → avg=0.5*1.5+0.5*3=2.25
(let* ([pop (make-population '(0.5 0.5))]
       [fits (all-fitnesses pd-symmetric pop)])
  (assert-equal 2.25 (average-fitness-from-list fits pop)))"))
    (available-modules (prelude normal-form))
    (hints ("n = (length fitnesses)"
            "Named let loop: (let loop ([i 0] [sum 0]) ...)"
            "Base: (>= i n) -> sum"
            "Step: (+ sum (* (pop-freq pop i) (list-ref fitnesses i)))"))
    (reference-solution "(define (average-fitness-from-list fitnesses pop)
  (let ([n (length fitnesses)])
    (let loop ([i 0] [sum 0])
      (if (>= i n)
          sum
          (loop (+ i 1)
                (+ sum (* (pop-freq pop i)
                          (list-ref fitnesses i))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
