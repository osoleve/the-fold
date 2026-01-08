((name "game")
 (description "Game theory foundations including normal form games,
  Nash equilibrium computation, and strategic analysis tools.")

 (modules
  ((file "normal-form.ss")
   (purpose "Normal form (strategic form) games and equilibrium")
   (exports
    (make-player "Create a player with name and strategy set")
    (player-name "Get player name")
    (player-strategies "Get available strategies")
    (make-game "Create a game from players and payoff function")
    (game-players "Get list of players")
    (game-payoff "Get payoff for a strategy profile")
    (payoff-matrix "Generate full payoff matrix for 2-player game")
    (best-response "Find best response to opponent strategy")
    (best-responses "Find all best responses (may be multiple)")
    (is-nash-equilibrium? "Check if strategy profile is Nash equilibrium")
    (find-pure-nash "Find all pure strategy Nash equilibria")
    (dominated-strategies "Find dominated strategies for a player")
    (iterated-elimination "Eliminate dominated strategies iteratively")
    (mixed-strategy "Create a probability distribution over strategies")
    (expected-payoff "Compute expected payoff under mixed strategies")
    (support "Get strategies with positive probability in mixed strategy"))
   (example
    ";; Prisoner's Dilemma
     (define p1 (make-player 'alice '(cooperate defect)))
     (define p2 (make-player 'bob '(cooperate defect)))
     (define pd-payoffs
       '(((cooperate cooperate) (3 3))
         ((cooperate defect) (0 5))
         ((defect cooperate) (5 0))
         ((defect defect) (1 1))))
     (define pd (make-game (list p1 p2)
                           (lambda (profile)
                             (assoc-ref pd-payoffs profile))))
     (find-pure-nash pd)
     ;; => ((defect defect))"))

  ((file "physics-dsl.ss")
   (purpose "Domain-specific language for physics simulations")
   (exports
    (make-body "Create a physical body with mass and position")
    (make-force "Create a force vector")
    (apply-force "Apply force to body, return updated body")
    (simulate-step "Advance simulation by one time step")
    (gravity "Gravitational force between two bodies"))))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("core/linalg/vec.ss" "Vector operations")
  ("core/linalg/matrix.ss" "Matrix operations"))

 (notes
  "Normal form games represent strategic situations where players
   choose simultaneously. A Nash equilibrium is a strategy profile
   where no player can unilaterally improve their payoff.

   The module focuses on finite games with pure and mixed strategies.
   For games with continuous strategy spaces, see optimization modules.

   Test suite: 26 tests covering equilibrium finding and dominance."))
