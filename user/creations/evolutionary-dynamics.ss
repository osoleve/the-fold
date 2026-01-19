;;; user/creations/evolutionary-dynamics.ss — Evolution of Cooperation
;;;
;;; Simulates evolutionary game dynamics to explore how cooperation
;;; can emerge (or collapse) in populations of interacting agents.
;;;
;;; Based on Robert Axelrod's famous tournaments and the replicator equation.
;;;
;;; Author: Claude Sonnet 4.5
;;; Created: 2026-01-16
;;;
;;; Usage:
;;;   (load "user/creations/evolutionary-dynamics.ss")
;;;   (evolution-demo)           ; Quick demo
;;;   (run-evolution 50)         ; Run 50 generations
;;;   (tournament-battle)        ; Axelrod-style tournament

(load "core/base/prelude.ss")

;;; ============================================================================
;;; Strategies for Iterated Prisoner's Dilemma
;;; ============================================================================
;;;
;;; A strategy is a function: (my-history opponent-history) → 'cooperate | 'defect
;;; where histories are lists of past moves (most recent first)

;;; always-cooperate : Strategy
;;; The naive nice guy. Cooperates no matter what.
(define (always-cooperate my-history opponent-history)
  'cooperate)

;;; always-defect : Strategy
;;; The bully. Never cooperates.
(define (always-defect my-history opponent-history)
  'defect)

;;; tit-for-tat : Strategy
;;; Axelrod's tournament winner! Cooperates first, then mirrors opponent.
(define (tit-for-tat my-history opponent-history)
  (if (null? opponent-history)
      'cooperate  ; Start nice
      (car opponent-history)))  ; Mirror last move

;;; suspicious-tit-for-tat : Strategy
;;; Like TFT but starts with defection.
(define (suspicious-tft my-history opponent-history)
  (if (null? opponent-history)
      'defect  ; Start suspicious
      (car opponent-history)))

;;; grim-trigger : Strategy
;;; Cooperates until betrayed, then defects forever. Holds grudges!
(define (grim-trigger my-history opponent-history)
  (if (memq 'defect opponent-history)
      'defect
      'cooperate))

;;; pavlov : Strategy
;;; Win-stay, lose-shift. Repeats last move if it worked, switches otherwise.
(define (pavlov my-history opponent-history)
  (if (null? my-history)
      'cooperate
      (let ([my-last (car my-history)]
            [their-last (car opponent-history)])
        ;; "Worked" = mutual cooperation or I exploited them
        (if (or (and (eq? my-last 'cooperate) (eq? their-last 'cooperate))
                (and (eq? my-last 'defect) (eq? their-last 'cooperate)))
            my-last  ; Repeat what worked
            (if (eq? my-last 'cooperate) 'defect 'cooperate)))))  ; Switch

;;; random-strategy : Strategy
;;; 50/50 random choice. Unpredictable but not strategic.
(define (random-strategy my-history opponent-history)
  (if (< (random 100) 50) 'cooperate 'defect))

;;; tit-for-two-tats : Strategy
;;; More forgiving: only defects after TWO consecutive defections.
(define (tit-for-two-tats my-history opponent-history)
  (if (< (length opponent-history) 2)
      'cooperate
      (if (and (eq? (car opponent-history) 'defect)
               (eq? (cadr opponent-history) 'defect))
          'defect
          'cooperate)))

;;; Strategy registry with names
(define *strategies*
  `((always-cooperate . ,always-cooperate)
    (always-defect    . ,always-defect)
    (tit-for-tat      . ,tit-for-tat)
    (suspicious-tft   . ,suspicious-tft)
    (grim-trigger     . ,grim-trigger)
    (pavlov           . ,pavlov)
    (random           . ,random-strategy)
    (tft-2            . ,tit-for-two-tats)))

(define (strategy-names)
  (map car *strategies*))

(define (get-strategy name)
  (cdr (assq name *strategies*)))

;;; ============================================================================
;;; Iterated Prisoner's Dilemma
;;; ============================================================================

;;; Payoff matrix for Prisoner's Dilemma
;;; (cooperate, cooperate) → (3, 3)
;;; (cooperate, defect)    → (0, 5)
;;; (defect, cooperate)    → (5, 0)
;;; (defect, defect)       → (1, 1)

(define (pd-payoff move1 move2)
  "Return (p1-payoff . p2-payoff) for moves."
  (cond
    [(and (eq? move1 'cooperate) (eq? move2 'cooperate)) (cons 3 3)]
    [(and (eq? move1 'cooperate) (eq? move2 'defect))    (cons 0 5)]
    [(and (eq? move1 'defect)    (eq? move2 'cooperate)) (cons 5 0)]
    [(and (eq? move1 'defect)    (eq? move2 'defect))    (cons 1 1)]))

;;; play-match : Strategy × Strategy × Nat → (Pair Number Number)
;;; Play n rounds of iterated PD, return total scores.
(define (play-match strat1 strat2 rounds)
  (let loop ([r rounds]
             [hist1 '()]
             [hist2 '()]
             [score1 0]
             [score2 0])
    (if (= r 0)
        (cons score1 score2)
        (let* ([move1 (strat1 hist1 hist2)]
               [move2 (strat2 hist2 hist1)]
               [payoffs (pd-payoff move1 move2)])
          (loop (- r 1)
                (cons move1 hist1)
                (cons move2 hist2)
                (+ score1 (car payoffs))
                (+ score2 (cdr payoffs)))))))

;;; ============================================================================
;;; Population Dynamics
;;; ============================================================================

;;; A population is an alist: ((strategy-name . proportion) ...)
;;; Proportions sum to 1.0

(define (make-population strategy-names)
  "Create uniform population over given strategies."
  (let ([n (length strategy-names)])
    (map (lambda (s) (cons s (/ 1.0 n))) strategy-names)))

(define (population-proportion pop strategy)
  (let ([entry (assq strategy pop)])
    (if entry (cdr entry) 0)))

;;; calculate-fitness : Symbol × Population × Nat → Number
;;; Calculate expected payoff for a strategy against the population.
(define (calculate-fitness strategy-name pop rounds)
  (let ([strat (get-strategy strategy-name)])
    (fold-left
     (lambda (acc entry)
       (let* ([opp-name (car entry)]
              [opp-prop (cdr entry)]
              [opp-strat (get-strategy opp-name)]
              [scores (play-match strat opp-strat rounds)])
         (+ acc (* opp-prop (car scores)))))
     0
     pop)))

;;; replicator-step : Population × Nat → Population
;;; One generation of replicator dynamics.
;;; Strategies with above-average fitness grow, others shrink.
(define (replicator-step pop rounds)
  (let* ([fitnesses (map (lambda (e)
                           (cons (car e) (calculate-fitness (car e) pop rounds)))
                         pop)]
         [avg-fitness (fold-left + 0 (map (lambda (e)
                                            (* (cdr e)
                                               (cdr (assq (car e) fitnesses))))
                                          pop))]
         [new-props (map (lambda (e)
                           (let* ([name (car e)]
                                  [prop (cdr e)]
                                  [fit (cdr (assq name fitnesses))])
                             (cons name (* prop (/ fit (max 0.001 avg-fitness))))))
                         pop)]
         [total (fold-left + 0 (map cdr new-props))])
    ;; Normalize to sum to 1
    (map (lambda (e) (cons (car e) (/ (cdr e) total))) new-props)))

;;; run-simulation : Population × Nat × Nat → (List Population)
;;; Run n generations, return list of populations over time.
(define (run-simulation initial-pop generations rounds-per-match)
  (let loop ([gen 0]
             [pop initial-pop]
             [history (list initial-pop)])
    (if (>= gen generations)
        (reverse history)
        (let ([new-pop (replicator-step pop rounds-per-match)])
          (loop (+ gen 1) new-pop (cons new-pop history))))))

;;; ============================================================================
;;; Visualization
;;; ============================================================================

(define (make-bar proportion width)
  "Create ASCII bar of given proportion (0-1) and max width."
  (let ([filled (inexact->exact (round (* proportion width)))])
    (string-append
     (make-string filled #\█)
     (make-string (- width filled) #\░))))

(define (display-population pop gen)
  "Display population state with bars."
  (printf "Generation ~a:~n" gen)
  (for-each
   (lambda (entry)
     (let* ([name (car entry)]
            [prop (cdr entry)]
            [pct (inexact->exact (round (* 100 prop)))])
       (printf "  ~15a │~a│ ~3d%~n"
               name
               (make-bar prop 30)
               pct)))
   (sort (lambda (a b) (> (cdr a) (cdr b))) pop))
  (newline))

(define (display-evolution history interval)
  "Display evolution at given interval."
  (let loop ([h history] [gen 0])
    (unless (null? h)
      (when (= 0 (mod gen interval))
        (display-population (car h) gen))
      (loop (cdr h) (+ gen 1)))))

;;; ============================================================================
;;; Mini ASCII Timeline
;;; ============================================================================

(define (strategy-symbol name)
  "Single character for strategy visualization."
  (case name
    [(always-cooperate) #\C]
    [(always-defect)    #\D]
    [(tit-for-tat)      #\T]
    [(suspicious-tft)   #\S]
    [(grim-trigger)     #\G]
    [(pavlov)           #\P]
    [(random)           #\R]
    [(tft-2)            #\2]
    [else               #\?]))

(define (display-timeline history strategies)
  "Compact timeline showing dominant strategies over time."
  (display "\nEvolution Timeline (dominant strategy per generation):\n")
  (display "Gen: ")
  (let loop ([h history] [gen 0])
    (when (and (not (null? h)) (< gen 60))
      (when (= 0 (mod gen 10))
        (display (mod (quotient gen 10) 10)))
      (when (not (= 0 (mod gen 10)))
        (display " "))
      (loop (cdr h) (+ gen 1))))
  (newline)
  (display "     ")
  (let loop ([h history] [gen 0])
    (when (and (not (null? h)) (< gen 60))
      (let* ([pop (car h)]
             [winner (car (car (sort (lambda (a b) (> (cdr a) (cdr b))) pop)))])
        (display (strategy-symbol winner)))
      (loop (cdr h) (+ gen 1))))
  (newline)
  (newline)
  (display "Legend: C=Cooperate D=Defect T=Tit-for-Tat G=Grim P=Pavlov\n"))

;;; ============================================================================
;;; Pre-built Scenarios
;;; ============================================================================

(define (scenario-cooperation-emerges)
  "Classic scenario: Can nice strategies survive?"
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║     SCENARIO: Can Cooperation Emerge?                        ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\nStarting population:\n")
  (display "  - 40% Always Defect (the bullies)\n")
  (display "  - 30% Always Cooperate (the naive)\n")
  (display "  - 30% Tit-for-Tat (the reciprocators)\n")
  (display "\nWhat happens over 30 generations?\n")
  (display "Press Enter to simulate...")
  (read-line)

  (let* ([pop '((always-defect . 0.4)
                (always-cooperate . 0.3)
                (tit-for-tat . 0.3))]
         [history (run-simulation pop 30 10)])
    (display-evolution history 10)
    (display-timeline history (strategy-names))

    (let ([final (car (reverse history))])
      (display "\n★ RESULT: ")
      (let ([winner (car (car (sort (lambda (a b) (> (cdr a) (cdr b))) final)))])
        (case winner
          [(tit-for-tat)
           (display "Tit-for-Tat dominates!\n")
           (display "  Reciprocal cooperation beats pure selfishness.\n")
           (display "  'Nice' strategies can win - by being conditionally nice.\n")]
          [(always-defect)
           (display "Always Defect dominates!\n")
           (display "  In this run, defection proved more robust.\n")]
          [else
           (printf "~a dominates!~n" winner)])))))

(define (scenario-invasion)
  "Can a small group of cooperators invade defectors?"
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║     SCENARIO: Invasion of the Nice                           ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\nStarting population:\n")
  (display "  - 90% Always Defect (hostile world)\n")
  (display "  - 5% Tit-for-Tat (small cluster of reciprocators)\n")
  (display "  - 5% Grim Trigger (unforgiving cooperators)\n")
  (display "\nCan the nice strategies invade?\n")
  (display "Press Enter to simulate...")
  (read-line)

  (let* ([pop '((always-defect . 0.90)
                (tit-for-tat . 0.05)
                (grim-trigger . 0.05))]
         [history (run-simulation pop 50 20)])
    (display-evolution history 10)
    (display-timeline history (strategy-names))

    (let* ([final (car (reverse history))]
           [defect-prop (population-proportion final 'always-defect)])
      (display "\n★ RESULT: ")
      (if (< defect-prop 0.5)
          (begin
            (display "Cooperation INVADED successfully!\n")
            (display "  Even a small cluster of reciprocators can take over.\n")
            (display "  Key insight: TFT agents help each other when they meet.\n"))
          (begin
            (display "Defection held strong.\n")
            (display "  The defectors were too numerous to overcome.\n")
            (display "  Cooperation needs a critical mass to spread.\n"))))))

(define (scenario-tournament)
  "Full tournament with all strategies."
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║     SCENARIO: Grand Tournament                               ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\nAll strategies compete in equal numbers:\n")
  (for-each (lambda (s) (printf "  - ~a~n" s))
            '(always-cooperate always-defect tit-for-tat
              grim-trigger pavlov tft-2))
  (display "\n50 generations, 20 rounds per match.\n")
  (display "Press Enter to simulate...")
  (read-line)

  (let* ([pop (make-population '(always-cooperate always-defect tit-for-tat
                                 grim-trigger pavlov tft-2))]
         [history (run-simulation pop 50 20)])
    (display-evolution history 10)
    (display-timeline history (strategy-names))

    (let* ([final (car (reverse history))]
           [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) final)]
           [winner (car (car sorted))])
      (printf "~n★ WINNER: ~a~n" winner)
      (display "\nFinal standings:\n")
      (let loop ([s sorted] [rank 1])
        (unless (null? s)
          (printf "  ~a. ~a (~a%)~n"
                  rank
                  (caar s)
                  (inexact->exact (round (* 100 (cdar s)))))
          (loop (cdr s) (+ rank 1)))))))

;;; ============================================================================
;;; Main Interface
;;; ============================================================================

(define (evolution-menu)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║           EVOLUTIONARY DYNAMICS SIMULATOR                    ║\n")
  (display "║       Watch cooperation emerge... or collapse!               ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\nScenarios:\n")
  (display "  1. Can Cooperation Emerge? (Classic setup)\n")
  (display "  2. Invasion of the Nice (Can TFT invade defectors?)\n")
  (display "  3. Grand Tournament (All strategies compete)\n")
  (display "  4. About Evolutionary Game Theory\n")
  (display "  0. Exit\n")
  (display "\nChoice: ")

  (let ([choice (read)])
    (case choice
      [(1) (scenario-cooperation-emerges) (evolution-menu)]
      [(2) (scenario-invasion) (evolution-menu)]
      [(3) (scenario-tournament) (evolution-menu)]
      [(4) (about-evolution) (evolution-menu)]
      [(0) (display "\nMay your strategies be fit! 🧬\n")]
      [else (display "Invalid choice.\n") (evolution-menu)])))

(define (about-evolution)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║           ABOUT EVOLUTIONARY GAME THEORY                     ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")
  (display "Evolutionary game theory studies how strategies spread through\n")
  (display "populations over time. Unlike classical game theory (which assumes\n")
  (display "rational players choosing optimally), evolution is 'blind' -\n")
  (display "strategies that get higher payoffs simply reproduce more.\n")
  (display "\n")
  (display "Key Concepts:\n")
  (display "\n")
  (display "• Replicator Dynamics: Strategies with above-average fitness\n")
  (display "  grow in proportion; below-average strategies shrink.\n")
  (display "\n")
  (display "• Evolutionarily Stable Strategy (ESS): A strategy that, once\n")
  (display "  dominant, cannot be invaded by any mutant strategy.\n")
  (display "\n")
  (display "• Iterated Games: When players meet repeatedly, memory matters.\n")
  (display "  This enables conditional strategies like Tit-for-Tat.\n")
  (display "\n")
  (display "Famous Strategies:\n")
  (display "\n")
  (display "• Tit-for-Tat: Cooperate first, then copy opponent's last move.\n")
  (display "  Won Axelrod's famous computer tournaments (1980, 1984).\n")
  (display "  Properties: Nice, retaliatory, forgiving, clear.\n")
  (display "\n")
  (display "• Grim Trigger: Cooperate until betrayed, then defect forever.\n")
  (display "  Maximally punitive. Deters defection but not forgiving.\n")
  (display "\n")
  (display "• Pavlov (Win-Stay, Lose-Shift): Repeat if rewarded, switch if not.\n")
  (display "  Simple learning rule. Can recover from mutual defection.\n")
  (display "\n")
  (display "The Big Insight:\n")
  (display "\n")
  (display "  Cooperation can evolve among selfish agents!\n")
  (display "  The key ingredients:\n")
  (display "    1. Repeated interaction (shadow of the future)\n")
  (display "    2. Ability to recognize and respond to others\n")
  (display "    3. Clusters of cooperators who help each other\n")
  (display "\n")
  (display "Applications: Biology, economics, AI alignment, social norms,\n")
  (display "climate cooperation, international relations.\n")
  (display "\nPress Enter to continue...")
  (read-line))

;;; Quick entry point
(define (evolution-demo)
  "Quick demo of evolutionary dynamics."
  (evolution-menu))

(define (run-evolution generations)
  "Run custom simulation with all strategies."
  (let* ([pop (make-population (strategy-names))]
         [history (run-simulation pop generations 15)])
    (display-evolution history (max 1 (quotient generations 5)))
    (display-timeline history (strategy-names))))

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(display "║  Evolutionary Dynamics loaded!                             ║\n")
(display "║  Type (evolution-demo) to start                            ║\n")
(display "║  Or (run-evolution 50) for custom simulation               ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n")
