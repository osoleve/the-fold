;;; normal-form.sexp — Development tasks for lattice/game-theory/normal-form.ss
;;;
;;; Module: normal-form (tier 0, depends on prelude)
;;; 406 lines, ~25 exported functions
;;; Normal form (strategic) games with payoff matrices. Best response,
;;; dominated strategy elimination (IESDS), pure Nash equilibria,
;;; mixed strategies, expected payoffs, 2x2 Nash solver.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement best-response-p1
  ;; ──────────────────────────────────────────────
  ;; Which strategies maximize P1's payoff against a known P2 choice?

  (task
    (id "game-theory/normal-form/best-response-p1-impl")
    (module normal-form)
    (tier 0)
    (type implement)
    (description "Implement best-response-p1: find Player 1's best responses when Player 2 plays strategy j. Compute P1's payoff for each of P1's strategies against j, find the maximum, then return all strategies that achieve that maximum (there may be ties). Steps: (1) Get n = number of P1 strategies. (2) Map each strategy index i to its payoff game-payoff-p1(game, i, j). (3) Find max-payoff via apply max. (4) Filter strategy indices where payoff equals max. Return the list of best-response indices.")
    (context "Available: game-num-strategies, game-payoff-p1, iota, map, apply, max, filter, lambda, =, list-ref, let*. Map payoffs, find max, filter ties.")
    (before "(define (best-response-p1 game j)
  ;; TODO: find P1's best responses to P2 playing j
  '())")
    (test-file "lattice/game-theory/test-normal-form.ss")
    (test-forms (";; Prisoner's Dilemma: defect always best
(assert-equal '(1) (best-response-p1 prisoners-dilemma 0))
(assert-equal '(1) (best-response-p1 prisoners-dilemma 1))"
                 ";; Matching Pennies: match opponent's choice
(assert-equal '(0) (best-response-p1 matching-pennies 0))
(assert-equal '(1) (best-response-p1 matching-pennies 1))"
                 ";; Battle of Sexes: coordinate
(assert-equal '(0) (best-response-p1 battle-of-sexes 0))
(assert-equal '(1) (best-response-p1 battle-of-sexes 1))"))
    (available-modules (prelude))
    (hints ("n = (game-num-strategies game 0)"
            "payoffs = (map (lambda (i) (game-payoff-p1 game i j)) (iota n))"
            "max-payoff = (apply max payoffs)"
            "Filter: (filter (lambda (i) (= (list-ref payoffs i) max-payoff)) (iota n))"))
    (reference-solution "(define (best-response-p1 game j)
  (let ([n (game-num-strategies game 0)])
       (if (= n 0)
           '()
           (let* ([payoffs (map (lambda (i) (game-payoff-p1 game i j))
                                (iota n))]
                  [max-payoff (apply max payoffs)])
                 (filter (lambda (i) (= (list-ref payoffs i) max-payoff))
                         (iota n))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement is-pure-nash?
  ;; ──────────────────────────────────────────────
  ;; The equilibrium check — no player wants to deviate.

  (task
    (id "game-theory/normal-form/is-pure-nash-impl")
    (module normal-form)
    (tier 0)
    (type implement)
    (description "Implement is-pure-nash?: check if strategy profile (i, j) is a pure strategy Nash equilibrium. A Nash equilibrium is a profile where no player can improve their payoff by unilaterally changing their strategy. Formally: i must be a best response to j (for P1) AND j must be a best response to i (for P2). Use best-response-p1 and best-response-p2 to check membership. Return #t only if both conditions hold, #f otherwise. The (and ... #t) ensures a boolean return rather than a list tail from member.")
    (context "Available: best-response-p1, best-response-p2, member, and. Two membership checks.")
    (before "(define (is-pure-nash? game i j)
  ;; TODO: i in BR1(j) and j in BR2(i)
  #f)")
    (test-file "lattice/game-theory/test-normal-form.ss")
    (test-forms (";; PD: (Defect, Defect) is Nash
(assert-true (is-pure-nash? prisoners-dilemma 1 1))"
                 ";; PD: (Cooperate, Cooperate) is NOT Nash
(assert-false (is-pure-nash? prisoners-dilemma 0 0))"
                 ";; BoS: both (Opera, Opera) and (Football, Football) are Nash
(assert-true (is-pure-nash? battle-of-sexes 0 0))
(assert-true (is-pure-nash? battle-of-sexes 1 1))"
                 ";; Matching Pennies: no pure Nash exists
(assert-false (is-pure-nash? matching-pennies 0 0))
(assert-false (is-pure-nash? matching-pennies 0 1))
(assert-false (is-pure-nash? matching-pennies 1 0))
(assert-false (is-pure-nash? matching-pennies 1 1))"))
    (available-modules (prelude))
    (hints ("Check: (member i (best-response-p1 game j))"
            "Check: (member j (best-response-p2 game i))"
            "Return: (and <both> #t) — the #t converts truthy member result to boolean"))
    (reference-solution "(define (is-pure-nash? game i j)
  (and (member i (best-response-p1 game j))
       (member j (best-response-p2 game i))
       #t))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement find-pure-nash
  ;; ──────────────────────────────────────────────
  ;; Enumerate all pure Nash equilibria by exhaustive search.

  (task
    (id "game-theory/normal-form/find-pure-nash-impl")
    (module normal-form)
    (tier 0)
    (type implement)
    (description "Implement find-pure-nash: find all pure strategy Nash equilibria by checking every strategy profile. Generate all (i, j) pairs where i ranges over P1's strategies and j ranges over P2's strategies. Filter to those where is-pure-nash? returns true. Return a list of (i . j) pairs. Use append-map to generate the cross product and filter to select equilibria. Games may have 0 (matching pennies), 1 (prisoner's dilemma), or multiple (battle of sexes) pure Nash equilibria.")
    (context "Available: game-num-strategies, is-pure-nash?, iota, map, cons, append-map, filter, lambda, car, cdr. Cross-product enumeration + filter.")
    (before "(define (find-pure-nash game)
  ;; TODO: enumerate all (i,j) and filter Nash equilibria
  '())")
    (test-file "lattice/game-theory/test-normal-form.ss")
    (test-forms (";; PD: unique Nash at (1,1)
(let ([nash (find-pure-nash prisoners-dilemma)])
  (assert-equal 1 (length nash))
  (assert-equal '(1 . 1) (car nash)))"
                 ";; BoS: two Nash equilibria
(let ([nash (find-pure-nash battle-of-sexes)])
  (assert-equal 2 (length nash)))"
                 ";; Matching Pennies: no pure Nash
(let ([nash (find-pure-nash matching-pennies)])
  (assert-equal 0 (length nash)))"
                 ";; Stag Hunt: two Nash equilibria
(let ([nash (find-pure-nash stag-hunt)])
  (assert-equal 2 (length nash)))"))
    (available-modules (prelude))
    (hints ("n1 = (game-num-strategies game 0), n2 = (game-num-strategies game 1)"
            "All profiles: (append-map (lambda (i) (map (lambda (j) (cons i j)) (iota n2))) (iota n1))"
            "Filter: (filter (lambda (profile) (is-pure-nash? game (car profile) (cdr profile))) profiles)"))
    (reference-solution "(define (find-pure-nash game)
  (let ([n1 (game-num-strategies game 0)]
        [n2 (game-num-strategies game 1)])
       (filter (lambda (profile)
                       (is-pure-nash? game (car profile) (cdr profile)))
               (append-map (lambda (i)
                                   (map (lambda (j) (cons i j))
                                        (iota n2)))
                           (iota n1)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement expected-payoff-p1
  ;; ──────────────────────────────────────────────
  ;; Expected payoff under mixed strategies — the bridge to mixed Nash.

  (task
    (id "game-theory/normal-form/expected-payoff-p1-impl")
    (module normal-form)
    (tier 0)
    (type implement)
    (description "Implement expected-payoff-p1: compute Player 1's expected payoff when both players use mixed strategies. A mixed strategy is a probability vector over pure strategies. The expected payoff is: E[u1] = sum_i sum_j sigma1[i] * sigma2[j] * payoff1(i,j). Double-loop over all strategy pairs, weighting each payoff by the probability of both players choosing that pair. Use nested map + apply + to compute the sum. sigma1 and sigma2 are vectors — use vector-ref for indexing and vector-length for bounds.")
    (context "Available: vector-length, vector-ref, game-payoff-p1, iota, map, apply, +, *, lambda. Nested summation over strategy pairs.")
    (before "(define (expected-payoff-p1 game sigma1 sigma2)
  ;; TODO: sum_i sum_j sigma1[i] * sigma2[j] * u1(i,j)
  0)")
    (test-file "lattice/game-theory/test-normal-form.ss")
    (test-forms (";; Pure strategies: payoff equals cell value
(let ([s1 (pure-strategy 0 2)]
      [s2 (pure-strategy 1 2)])
  ;; (Cooperate, Defect) → P1 gets 0
  (assert-equal 0 (expected-payoff-p1 prisoners-dilemma s1 s2)))"
                 ";; Uniform mix in PD: average of all cells
(let ([s (uniform-strategy 2)])
  ;; P1 payoffs: 3, 0, 5, 1 → avg = 9/4
  (assert-equal 9/4 (expected-payoff-p1 prisoners-dilemma s s)))"
                 ";; Matching pennies uniform: expected payoff = 0
(let ([s (uniform-strategy 2)])
  (assert-equal 0 (expected-payoff-p1 matching-pennies s s)))"))
    (available-modules (prelude))
    (hints ("n1 = (vector-length sigma1), n2 = (vector-length sigma2)"
            "Outer sum over i: (apply + (map ... (iota n1)))"
            "Inner sum over j: (apply + (map ... (iota n2)))"
            "Each term: (* (vector-ref sigma1 i) (vector-ref sigma2 j) (game-payoff-p1 game i j))"))
    (reference-solution "(define (expected-payoff-p1 game sigma1 sigma2)
  (let ([n1 (vector-length sigma1)]
        [n2 (vector-length sigma2)])
       (apply + (map (lambda (i)
                             (apply + (map (lambda (j)
                                                   (* (vector-ref sigma1 i)
                                                      (vector-ref sigma2 j)
                                                      (game-payoff-p1 game i j)))
                                           (iota n2))))
                     (iota n1)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement find-dominated-strategies
  ;; ──────────────────────────────────────────────
  ;; Find strategies that are always suboptimal.

  (task
    (id "game-theory/normal-form/find-dominated-impl")
    (module normal-form)
    (tier 0)
    (type implement)
    (description "Implement find-dominated-strategies: find all strictly dominated strategies for both players. A strategy is strictly dominated if there exists another strategy that yields strictly higher payoff against ALL opponent strategies. Return a pair (p1-dominated . p2-dominated) where each is a list of dominated strategy indices. Use is-strictly-dominated-p1? and is-strictly-dominated-p2? to check each strategy. In the Prisoner's Dilemma, Cooperate (index 0) is strictly dominated by Defect (index 1) for both players. In Matching Pennies, no strategy is dominated.")
    (context "Available: game-num-strategies, is-strictly-dominated-p1?, is-strictly-dominated-p2?, iota, filter, cons, lambda. Two filters paired.")
    (before "(define (find-dominated-strategies game)
  ;; TODO: return (p1-dominated . p2-dominated)
  (cons '() '()))")
    (test-file "lattice/game-theory/test-normal-form.ss")
    (test-forms (";; PD: Cooperate is dominated for both players
(let ([dom (find-dominated-strategies prisoners-dilemma)])
  (assert-equal '(0) (car dom))
  (assert-equal '(0) (cdr dom)))"
                 ";; Matching Pennies: no dominated strategies
(let ([dom (find-dominated-strategies matching-pennies)])
  (assert-true (null? (car dom)))
  (assert-true (null? (cdr dom))))"
                 ";; Battle of Sexes: no dominated strategies
(let ([dom (find-dominated-strategies battle-of-sexes)])
  (assert-true (null? (car dom)))
  (assert-true (null? (cdr dom))))"))
    (available-modules (prelude))
    (hints ("n1 = (game-num-strategies game 0), n2 = (game-num-strategies game 1)"
            "P1 dominated: (filter (lambda (i) (is-strictly-dominated-p1? game i)) (iota n1))"
            "P2 dominated: (filter (lambda (j) (is-strictly-dominated-p2? game j)) (iota n2))"
            "Return: (cons p1-list p2-list)"))
    (reference-solution "(define (find-dominated-strategies game)
  (let ([n1 (game-num-strategies game 0)]
        [n2 (game-num-strategies game 1)])
       (cons (filter (lambda (i) (is-strictly-dominated-p1? game i)) (iota n1))
             (filter (lambda (j) (is-strictly-dominated-p2? game j)) (iota n2)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
