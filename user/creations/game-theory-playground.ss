;;; user/creations/game-theory-playground.ss — Interactive Game Theory Explorer
;;;
;;; An interactive playground for exploring game theory concepts including
;;; Nash equilibria, Shapley values, fair division, and voting power.
;;;
;;; Author: Claude Sonnet 4.5
;;; Created: 2026-01-16
;;;
;;; Usage:
;;;   (load "user/creations/game-theory-playground.ss")
;;;   (game-theory-playground)
;;;
;;; Features:
;;;   - Explore classic games (Prisoner's Dilemma, Stag Hunt, etc.)
;;;   - Build custom 2x2 games and find Nash equilibria
;;;   - Fair division with cake cutting algorithms
;;;   - Coalition power analysis (Shapley value, power indices)

;;; Load dependencies
(load "lattice/fp/game/normal-form.ss")
(load "lattice/fp/game/coop-games.ss")
(load "lattice/fp/game/fair-division.ss")
(load "lattice/fp/game/voting.ss")
(load "user/creations/evolutionary-dynamics.ss")

;;; ============================================================================
;;; Display Utilities
;;; ============================================================================

(define (box-text text)
  "Draw a box around text."
  (let* ([lines (string-split text #\newline)]
         [width (apply max (map string-length lines))]
         [border (make-string (+ width 4) #\═)])
    (display "╔") (display border) (display "╗") (newline)
    (for-each
     (lambda (line)
       (display "║ ")
       (display line)
       (display (make-string (- width (string-length line)) #\space))
       (display " ║")
       (newline))
     lines)
    (display "╚") (display border) (display "╝") (newline)))

(define (section-header text)
  "Display a section header."
  (newline)
  (display "┌─ ") (display text) (display " ")
  (display (make-string (max 0 (- 60 (string-length text) 4)) #\─))
  (newline))

(define (display-payoff-matrix game title)
  "Display a 2x2 payoff matrix in readable form."
  (let* ([s1 (game%-strategies1 game)]
         [s2 (game%-strategies2 game)]
         [n1 (vector-length s1)]
         [n2 (vector-length s2)])
    (section-header title)
    (newline)
    (display "           ")
    (for-each (lambda (j)
                (display (format "~12a" (vector-ref s2 j))))
              (iota n2))
    (newline)
    (display (make-string 70 #\─))
    (newline)
    (for-each
     (lambda (i)
       (display (format "~10a │" (vector-ref s1 i)))
       (for-each
        (lambda (j)
          (let ([pair (game-payoff game i j)])
            (display (format " (~a, ~a)   " (car pair) (cdr pair)))))
        (iota n2))
       (newline))
     (iota n1))))

;;; ============================================================================
;;; Classic Games Explorer
;;; ============================================================================

(define (describe-game name game description)
  "Display game info and Nash equilibria."
  (box-text (format "~a\n\n~a" name description))
  (display-payoff-matrix game "Payoff Matrix")
  (newline)

  ;; Find pure Nash equilibria
  (let ([nash (find-pure-nash game)])
    (if (null? nash)
        (display "→ No pure strategy Nash equilibrium exists.\n")
        (begin
          (display "→ Pure Nash Equilibrium:\n")
          (for-each
           (lambda (eq)
             (let ([i (car eq)]
                   [j (cdr eq)]
                   [s1 (game%-strategies1 game)]
                   [s2 (game%-strategies2 game)])
               (display (format "   (~a, ~a) with payoffs ~a\n"
                               (vector-ref s1 i)
                               (vector-ref s2 j)
                               (game-payoff game i j)))))
           nash))))

  ;; Check for dominated strategies
  (let ([dominated (find-dominated-strategies game)])
    (when (not (and (null? (car dominated))
                    (null? (cdr dominated))))
      (display "\n→ Dominated Strategies:\n")
      (unless (null? (car dominated))
        (display (format "   Player 1: ~a\n"
                        (map (lambda (i)
                               (vector-ref (game%-strategies1 game) i))
                             (car dominated)))))
      (unless (null? (cdr dominated))
        (display (format "   Player 2: ~a\n"
                        (map (lambda (j)
                               (vector-ref (game%-strategies2 game) j))
                             (cdr dominated)))))))
  (newline))

(define (explore-classic-games)
  "Interactive exploration of classic games."
  (display "\n")
  (box-text "CLASSIC GAMES EXPLORER")
  (display "\nExplore famous 2x2 games from game theory:\n\n")
  (display "  1. Prisoner's Dilemma\n")
  (display "  2. Stag Hunt\n")
  (display "  3. Battle of the Sexes\n")
  (display "  4. Chicken (Hawk-Dove)\n")
  (display "  5. Matching Pennies\n")
  (display "  6. All Games\n")
  (display "  0. Back to Main Menu\n")
  (display "\nChoice: ")

  (let ([choice (read)])
    (case choice
      [(1) (describe-game
            "Prisoner's Dilemma"
            prisoners-dilemma
            "Two criminals are arrested. Each can cooperate (stay silent)\nor defect (betray the other). Mutual defection is the Nash\nequilibrium, but mutual cooperation would be better for both!")]
      [(2) (describe-game
            "Stag Hunt"
            stag-hunt
            "Two hunters can cooperate to hunt a stag (high reward) or\nindividually hunt a hare (safe, low reward). Multiple equilibria\nexist, showing the challenge of coordination.")]
      [(3) (describe-game
            "Battle of the Sexes"
            battle-of-sexes
            "A couple wants to spend time together, but one prefers opera\nand the other prefers football. Multiple Nash equilibria exist,\nraising the question: which is fair?")]
      [(4) (describe-game
            "Chicken (Hawk-Dove)"
            chicken
            "Two drivers drive toward each other. Each can swerve (chicken)\nor stay (hawk). Staying when the other swerves wins, but mutual\nhawk leads to disaster!")]
      [(5) (describe-game
            "Matching Pennies"
            matching-pennies
            "A zero-sum game with no pure Nash equilibrium. Players must\nuse mixed strategies (randomization) to avoid being exploited.")]
      [(6) (begin
             (describe-game "Prisoner's Dilemma" prisoners-dilemma "The tragedy of rational self-interest.")
             (describe-game "Stag Hunt" stag-hunt "The challenge of coordination.")
             (describe-game "Battle of the Sexes" battle-of-sexes "Conflict with common interest.")
             (describe-game "Chicken (Hawk-Dove)" chicken "The game of brinkmanship.")
             (describe-game "Matching Pennies" matching-pennies "Pure conflict, no pure equilibrium."))]
      [(0) 'back]
      [else (display "\nInvalid choice.\n")])

    (unless (eq? choice 0)
      (display "\nPress Enter to continue...")
      (read-line)
      (explore-classic-games))))

;;; ============================================================================
;;; Custom Game Builder
;;; ============================================================================

(define (build-custom-game)
  "Interactive 2x2 game builder."
  (display "\n")
  (box-text "CUSTOM GAME BUILDER")
  (display "\nBuild your own 2x2 game!\n")
  (display "\nEnter strategy names for Player 1 (e.g., cooperate defect): ")
  (let* ([input1 (read-line)]
         [strats1 (string-split input1 #\space)])
    (if (not (= 2 (length strats1)))
        (begin (display "Need exactly 2 strategies!\n") (build-custom-game))
        (begin
          (display "Enter strategy names for Player 2: ")
          (let* ([input2 (read-line)]
                 [strats2 (string-split input2 #\space)])
            (if (not (= 2 (length strats2)))
                (begin (display "Need exactly 2 strategies!\n") (build-custom-game))
                (begin
                  (display "\nNow enter payoffs (format: p1 p2)\n")
                  (display (format "~a vs ~a: " (car strats1) (car strats2)))
                  (let* ([p11 (read)] [p12 (read)])
                    (display (format "~a vs ~a: " (car strats1) (cadr strats2)))
                    (let* ([p21 (read)] [p22 (read)])
                      (display (format "~a vs ~a: " (cadr strats1) (car strats2)))
                      (let* ([p31 (read)] [p32 (read)])
                        (display (format "~a vs ~a: " (cadr strats1) (cadr strats2)))
                        (let* ([p41 (read)] [p42 (read)]
                               [game (make-game
                                      (map string->symbol strats1)
                                      (map string->symbol strats2)
                                      (list (list (cons p11 p12) (cons p21 p22))
                                            (list (cons p31 p32) (cons p41 p42))))])
                          (describe-game "Your Custom Game" game "Let's analyze what you built!")
                          (display "\nPress Enter to continue...")
                          (read-line))))))))))))

;;; ============================================================================
;;; Fair Division Wizard
;;; ============================================================================

(define (fair-division-demo)
  "Demonstrate fair division algorithms."
  (display "\n")
  (box-text "FAIR DIVISION WIZARD")
  (display "\nDivide a cake fairly between 2 people!\n")
  (display "\nScenario: Alice and Bob must share a cake.\n")
  (display "Alice loves chocolate (left half), Bob loves vanilla (right half).\n\n")

  ;; Create cake with opposing valuations
  (let ([cake (make-cake 2)])
    ;; Alice prefers left (chocolate) - values it twice as much
    (cake-set-valuation! cake 0 (lambda (x) (if (< x 0.5) 2.0 0.5)))
    ;; Bob prefers right (vanilla) - values it twice as much
    (cake-set-valuation! cake 1 (lambda (x) (if (< x 0.5) 0.5 2.0)))

    (display "Alice's valuation: 2.0 for [0, 0.5], 0.5 for [0.5, 1.0]\n")
    (display "Bob's valuation:   0.5 for [0, 0.5], 2.0 for [0.5, 1.0]\n\n")

    ;; Cut and Choose
    (display "Algorithm: Cut and Choose\n")
    (display "→ Alice cuts the cake in half at x = 0.5\n")
    (display "→ Bob chooses the right piece (his favorite!)\n")
    (display "→ Alice gets the left piece (her favorite!)\n\n")

    (let ([alice-value (cake-valuation cake 0 0 0.5)]
          [bob-value (cake-valuation cake 1 0.5 1)])
      (display (format "Alice's utility: ~a (~a% of total)\n"
                      alice-value
                      (inexact->exact (round (* 100 (/ alice-value 1.25))))))
      (display (format "Bob's utility:   ~a (~a% of total)\n"
                      bob-value
                      (inexact->exact (round (* 100 (/ bob-value 1.25))))))
      (display "\n→ Both get 80% of their total possible value!\n")
      (display "→ This is ENVY-FREE: neither wants the other's piece.\n")
      (display "→ Both are happy! 🎂\n")))

  (display "\nPress Enter to continue...")
  (read-line))

;;; ============================================================================
;;; Coalition Power Calculator
;;; ============================================================================

(define (coalition-power-demo)
  "Demonstrate coalition power analysis."
  (display "\n")
  (box-text "COALITION POWER CALCULATOR")
  (display "\nAnalyze voting power in weighted voting games!\n\n")
  (display "Example: UN Security Council (simplified)\n")
  (display "  - 5 permanent members (veto power)\n")
  (display "  - Need all 5 to pass a resolution\n\n")

  ;; Create a simple unanimity game for 3 players
  (let* ([n 3]
         [game (make-unanimity-game n (list 0 1 2))])
    (display "Simpler example: 3-player unanimity game\n")
    (display "A coalition wins only if it includes ALL players.\n\n")

    (display "Shapley Value (fair power distribution):\n")
    (let ([sv (shapley-value game n)])
      (for-each
       (lambda (i)
         (display (format "  Player ~a: ~a (~a%)\n"
                         i
                         (vector-ref sv i)
                         (inexact->exact (round (* 100 (vector-ref sv i)))))))
       (iota n)))

    (display "\n→ Each player has equal power (33.3%) because all are\n")
    (display "  essential to form a winning coalition!\n"))

  (display "\nPress Enter to continue...")
  (read-line))

;;; ============================================================================
;;; Main Menu
;;; ============================================================================

(define (display-main-menu)
  "Display the main playground menu."
  (display "\n\n")
  (box-text "GAME THEORY PLAYGROUND\nExplore strategic thinking through games!")
  (display "\n")
  (display "  1. Classic Games Explorer (Nash Equilibria)\n")
  (display "  2. Custom Game Builder\n")
  (display "  3. Fair Division Wizard (Cake Cutting)\n")
  (display "  4. Coalition Power Calculator (Shapley Value)\n")
  (display "  5. Evolutionary Dynamics (Watch Cooperation Evolve!)\n")
  (display "  6. About Game Theory\n")
  (display "  0. Exit\n")
  (display "\nChoice: "))

(define (about-game-theory)
  "Display information about game theory."
  (display "\n")
  (box-text "ABOUT GAME THEORY")
  (display "\n")
  (display "Game theory studies strategic decision-making when the outcome\n")
  (display "depends on choices made by multiple rational agents.\n\n")
  (display "Key Concepts:\n\n")
  (display "• Nash Equilibrium: A strategy profile where no player can\n")
  (display "  improve by unilaterally changing their strategy.\n\n")
  (display "• Dominated Strategy: A strategy that is always worse than\n")
  (display "  another available strategy, regardless of opponents' actions.\n\n")
  (display "• Shapley Value: A fair way to distribute total gains among\n")
  (display "  coalition members based on marginal contributions.\n\n")
  (display "• Envy-Free Allocation: A division where no agent prefers\n")
  (display "  another agent's share to their own.\n\n")
  (display "Applications: Economics, political science, biology, computer\n")
  (display "science, AI alignment, and mechanism design!\n")
  (display "\nPress Enter to continue...")
  (read-line))

(define (game-theory-playground)
  "Main entry point for the playground."
  (display-main-menu)
  (let ([choice (read)])
    (case choice
      [(1) (explore-classic-games) (game-theory-playground)]
      [(2) (build-custom-game) (game-theory-playground)]
      [(3) (fair-division-demo) (game-theory-playground)]
      [(4) (coalition-power-demo) (game-theory-playground)]
      [(5) (evolution-menu) (game-theory-playground)]
      [(6) (about-game-theory) (game-theory-playground)]
      [(0) (display "\nThanks for playing! Strategic thinking leads to better outcomes. 🎮\n\n")]
      [else (display "\nInvalid choice. Try again.\n") (game-theory-playground)])))

;;; Quick start function
(define (play)
  "Shorthand for starting the playground."
  (game-theory-playground))

(display "\n╔════════════════════════════════════════════════════════════╗\n")
(display "║  Game Theory Playground loaded!                            ║\n")
(display "║  Type (game-theory-playground) or (play) to start          ║\n")
(display "╚════════════════════════════════════════════════════════════╝\n")
