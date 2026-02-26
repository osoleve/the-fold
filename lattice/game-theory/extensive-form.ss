(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires prelude
(require 'prelude)

(doc 'module 'extensive-form)
(doc 'description "Extensive form (sequential) games with game trees. Supports backward induction, subgame perfect equilibrium, information sets, and chance nodes.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "An extensive form game represents sequential decision-making with:
- Players: indexed 0, 1, ... n-1, plus 'nature' for chance moves
- Game tree: nodes connected by action edges
- Information sets: nodes indistinguishable to a player
- Payoffs: utility vectors at terminal nodes

Perfect information games have singleton information sets (each player knows
exactly where they are in the tree). Imperfect information games have
information sets containing multiple nodes that the player cannot distinguish.

Key solution concept: Subgame Perfect Equilibrium (SPE), found via backward
induction for perfect information games. SPE requires players to play
optimally in every subgame, not just on the equilibrium path.")

;;; ============================================================================
;;; Node Types
;;; ============================================================================

(doc 'section 'node-types)

(doc 'note "Three types of nodes in a game tree:
1. Terminal: leaf nodes with payoff vectors
2. Decision: internal nodes where a player chooses an action
3. Chance: internal nodes where nature moves with given probabilities")

;; Terminal node: game ends with payoffs
(define (make-terminal payoffs)
  (doc 'export #t)
  (doc 'type '(-> (List Number) TerminalNode))
  (doc 'description "Create a terminal node with payoffs for each player")
  (doc 'param 'payoffs "List of utilities, one per player, in player order")
  (list 'terminal payoffs))

(define (terminal? node)
  (doc 'export #t)
  (doc 'type '(-> Node Bool))
  (and (pair? node) (eq? (car node) 'terminal)))

(define (terminal-payoffs node)
  (doc 'export #t)
  (doc 'type '(-> TerminalNode (List Number)))
  (cadr node))

(define (terminal-payoff node player)
  (doc 'export #t)
  (doc 'type '(-> TerminalNode Nat Number))
  (list-ref (terminal-payoffs node) player))

;; Decision node: player chooses an action
(define (make-decision player actions children)
  (doc 'export #t)
  (doc 'type '(-> Nat (List Symbol) (List Node) DecisionNode))
  (doc 'description "Create a decision node where player chooses from actions")
  (doc 'param 'player "Player index (0, 1, ...)")
  (doc 'param 'actions "List of action labels (symbols)")
  (doc 'param 'children "Child nodes corresponding to each action")
  (when (not (= (length actions) (length children)))
        (error 'make-decision "actions and children must have same length"
               (length actions) (length children)))
  (list 'decision player actions children))

(define (decision? node)
  (doc 'export #t)
  (doc 'type '(-> Node Bool))
  (and (pair? node) (eq? (car node) 'decision)))

(define (decision-player node)
  (doc 'export #t)
  (doc 'type '(-> DecisionNode Nat))
  (cadr node))

(define (decision-actions node)
  (doc 'export #t)
  (doc 'type '(-> DecisionNode (List Symbol)))
  (caddr node))

(define (decision-children node)
  (doc 'export #t)
  (doc 'type '(-> DecisionNode (List Node)))
  (cadddr node))

(define (decision-child node action)
  (doc 'export #t)
  (doc 'type '(-> DecisionNode Symbol Node))
  (doc 'description "Get the child node reached by taking action")
  (let loop ([as (decision-actions node)]
             [cs (decision-children node)])
       (cond
         [(null? as) (error 'decision-child "action not found" action)]
         [(eq? (car as) action) (car cs)]
         [else (loop (cdr as) (cdr cs))])))

;; Chance node: nature moves with probabilities
(define (make-chance outcomes probs children)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol) (List Number) (List Node) ChanceNode))
  (doc 'description "Create a chance node where nature picks outcome with given probabilities")
  (doc 'param 'outcomes "List of outcome labels")
  (doc 'param 'probs "List of probabilities (must sum to 1)")
  (doc 'param 'children "Child nodes corresponding to each outcome")
  (when (not (= (length outcomes) (length probs) (length children)))
        (error 'make-chance "outcomes, probs, and children must have same length"))
  ;; Verify probabilities sum to 1 (with tolerance)
  (let ([sum (fold-right + 0 probs)])
       (when (> (abs (- sum 1.0)) 1e-10)
             (error 'make-chance "probabilities must sum to 1" sum)))
  (list 'chance outcomes probs children))

(define (chance? node)
  (doc 'export #t)
  (doc 'type '(-> Node Bool))
  (and (pair? node) (eq? (car node) 'chance)))

(define (chance-outcomes node)
  (doc 'export #t)
  (doc 'type '(-> ChanceNode (List Symbol)))
  (cadr node))

(define (chance-probs node)
  (doc 'export #t)
  (doc 'type '(-> ChanceNode (List Number)))
  (caddr node))

(define (chance-children node)
  (doc 'export #t)
  (doc 'type '(-> ChanceNode (List Node)))
  (cadddr node))

;;; ============================================================================
;;; Game Construction
;;; ============================================================================

(doc 'section 'game-construction)

(define (make-extensive-game num-players root)
  (doc 'export #t)
  (doc 'type '(-> Nat Node ExtensiveGame))
  (doc 'description "Create an extensive form game from root node")
  (doc 'param 'num-players "Number of players in the game")
  (doc 'param 'root "Root node of the game tree")
  (list 'extensive-game num-players root '()))  ; empty info-sets initially

(define (extensive-game? x)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? x) (eq? (car x) 'extensive-game)))

(define (game-num-players game)
  (doc 'export #t)
  (doc 'type '(-> ExtensiveGame Nat))
  (cadr game))

(define (game-root game)
  (doc 'export #t)
  (doc 'type '(-> ExtensiveGame Node))
  (caddr game))

(define (game-info-sets game)
  (doc 'export #t)
  (doc 'type '(-> ExtensiveGame (List InfoSet)))
  (cadddr game))

;;; ============================================================================
;;; Game Tree Utilities
;;; ============================================================================

(doc 'section 'tree-utilities)

(define (tree-depth node)
  (doc 'export #t)
  (doc 'type '(-> Node Nat))
  (doc 'description "Compute the depth of the game tree")
  (cond
    [(terminal? node) 0]
    [(decision? node)
     (+ 1 (apply max (cons 0 (map tree-depth (decision-children node)))))]
    [(chance? node)
     (+ 1 (apply max (cons 0 (map tree-depth (chance-children node)))))]
    [else (error 'tree-depth "unknown node type" node)]))

(define (tree-size node)
  (doc 'export #t)
  (doc 'type '(-> Node Nat))
  (doc 'description "Count total number of nodes in the game tree")
  (cond
    [(terminal? node) 1]
    [(decision? node)
     (+ 1 (fold-right + 0 (map tree-size (decision-children node))))]
    [(chance? node)
     (+ 1 (fold-right + 0 (map tree-size (chance-children node))))]
    [else (error 'tree-size "unknown node type" node)]))

(define (terminal-nodes node)
  (doc 'export #t)
  (doc 'type '(-> Node (List TerminalNode)))
  (doc 'description "Collect all terminal nodes in the tree")
  (cond
    [(terminal? node) (list node)]
    [(decision? node)
     (apply append (map terminal-nodes (decision-children node)))]
    [(chance? node)
     (apply append (map terminal-nodes (chance-children node)))]))

;;; ============================================================================
;;; Backward Induction
;;; ============================================================================

(doc 'section 'backward-induction)

(doc 'note "Backward induction solves perfect-information extensive form games by:
1. Starting at terminal nodes
2. Working backward to the root
3. At each decision node, the player picks the action maximizing their payoff
4. At chance nodes, compute the expected value

The result is a subgame-perfect equilibrium (SPE) — an optimal strategy for every
subgame, even those not reached on the equilibrium path.")

(define (backward-induction node)
  (doc 'export #t)
  (doc 'type '(-> Node (Values (List Number) (List (Pair Nat Symbol)))))
  (doc 'description "Solve by backward induction. Returns (payoffs, strategy-profile)")
  (doc 'returns "Two values: equilibrium payoffs and list of (player . action) pairs")
  (backward-induction-helper node '()))

(define (backward-induction-helper node path)
  ;; Returns: (values payoffs strategy-profile)
  ;; payoffs: list of payoffs for each player
  ;; strategy-profile: list of (player . action) representing equilibrium play
  (cond
    [(terminal? node)
     (values (terminal-payoffs node) '())]

    [(decision? node)
     (let* ([player (decision-player node)]
            [actions (decision-actions node)]
            [children (decision-children node)]
            ;; Recursively solve all children
            [results (map (lambda (child action)
                           (let-values ([(payoffs strat)
                                        (backward-induction-helper child
                                          (cons (cons player action) path))])
                                       (list payoffs strat action)))
                         children actions)]
            ;; Find action that maximizes this player's payoff
            [best (fold-right
                   (lambda (r best)
                     (if (or (not best)
                             (> (list-ref (car r) player)
                                (list-ref (car best) player)))
                         r
                         best))
                   #f
                   results)])
           (values (car best)  ; payoffs from best action
                   (cons (cons player (caddr best))  ; add this decision
                         (cadr best))))]              ; strategy from subgame

    [(chance? node)
     (let* ([outcomes (chance-outcomes node)]
            [probs (chance-probs node)]
            [children (chance-children node)]
            ;; Recursively solve all branches
            [results (map (lambda (child)
                           (let-values ([(payoffs strat)
                                        (backward-induction-helper child path)])
                                       payoffs))
                         children)]
            ;; Compute expected payoffs
            [n-players (length (car results))]
            [expected (let loop ([i 0] [acc '()])
                           (if (>= i n-players)
                               (reverse acc)
                               (let ([exp-i (fold-right + 0
                                             (map (lambda (p payoff-vec)
                                                    (* p (list-ref payoff-vec i)))
                                                  probs results))])
                                    (loop (+ i 1) (cons exp-i acc)))))])
           (values expected '()))]  ; no strategic choice at chance nodes

    [else (error 'backward-induction "unknown node type" node)]))

;;; ============================================================================
;;; Subgame Perfect Equilibrium
;;; ============================================================================

(doc 'section 'subgame-perfect-equilibrium)

(define (solve-spe game)
  (doc 'export #t)
  (doc 'type '(-> ExtensiveGame SPEResult))
  (doc 'description "Find the subgame perfect equilibrium via backward induction")
  (doc 'note "For perfect information games, SPE is unique (generically)")
  (let-values ([(payoffs strategy) (backward-induction (game-root game))])
              (list 'spe-result payoffs strategy)))

(define (spe-result? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'spe-result)))

(define (spe-payoffs result)
  (doc 'export #t)
  (doc 'type '(-> SPEResult (List Number)))
  (cadr result))

(define (spe-strategy result)
  (doc 'export #t)
  (doc 'type '(-> SPEResult (List (Pair Nat Symbol))))
  (doc 'description "List of (player . action) pairs representing equilibrium play")
  (caddr result))

;;; ============================================================================
;;; Expected Utility Calculation
;;; ============================================================================

(doc 'section 'expected-utility)

(define (expected-utility node player strategy)
  (doc 'export #t)
  (doc 'type '(-> Node Nat (-> Nat (List Symbol) Symbol) Number))
  (doc 'description "Compute expected utility for a player given a behavioral strategy")
  (doc 'param 'node "Root node")
  (doc 'param 'player "Player index")
  (doc 'param 'strategy "Function: (player, available-actions) -> chosen action")
  (cond
    [(terminal? node)
     (terminal-payoff node player)]

    [(decision? node)
     (let* ([acting-player (decision-player node)]
            [actions (decision-actions node)]
            [chosen (strategy acting-player actions)]
            [child (decision-child node chosen)])
           (expected-utility child player strategy))]

    [(chance? node)
     (let* ([probs (chance-probs node)]
            [children (chance-children node)]
            [utils (map (lambda (c) (expected-utility c player strategy))
                       children)])
           (fold-right + 0 (map * probs utils)))]

    [else (error 'expected-utility "unknown node type" node)]))

;;; ============================================================================
;;; Classic Extensive Form Games
;;; ============================================================================

(doc 'section 'classic-games)

;; Ultimatum Game: P1 proposes split, P2 accepts or rejects
(doc ultimatum-game 'export #t)
(doc ultimatum-game 'description "Ultimatum Game: P1 proposes (keep, give), P2 accepts or rejects. If P2 rejects, both get 0. SPE: P1 offers minimum, P2 accepts.")
(define ultimatum-game
  (make-extensive-game 2
    (make-decision 0 '(offer-90-10 offer-50-50 offer-10-90)
      (list
        ;; P1 keeps 90, offers 10
        (make-decision 1 '(accept reject)
          (list (make-terminal '(90 10))
                (make-terminal '(0 0))))
        ;; P1 keeps 50, offers 50
        (make-decision 1 '(accept reject)
          (list (make-terminal '(50 50))
                (make-terminal '(0 0))))
        ;; P1 keeps 10, offers 90
        (make-decision 1 '(accept reject)
          (list (make-terminal '(10 90))
                (make-terminal '(0 0))))))))

;; Centipede Game: alternating cooperation with increasing payoffs
(define (make-centipede-game n)
  (doc 'export #t)
  (doc 'type '(-> Nat ExtensiveGame))
  (doc 'description "Create an n-round centipede game")
  (doc 'note "SPE is immediate defection despite Pareto-improving continuation. This is a canonical example of backward induction paradox.")
  ;; Standard centipede payoff structure (Rosenthal 1981):
  ;; At round k, if player takes: taker gets k+1, other gets k-1
  ;; If everyone passes to terminal: both get n
  ;; Backward induction: taking is always better than continuing
  (letrec ([build-tree
            (lambda (round)
              (if (> round n)
                  ;; Terminal: both get n
                  (make-terminal (list n n))
                  (let ([player (remainder (- round 1) 2)])  ; P0 on odd rounds, P1 on even
                       (make-decision player '(take pass)
                         (list
                           ;; Take: taker gets round+1, other gets round-1
                           (if (= player 0)
                               (make-terminal (list (+ round 1) (- round 1)))
                               (make-terminal (list (- round 1) (+ round 1))))
                           ;; Pass: continue to next round
                           (build-tree (+ round 1)))))))])
          (make-extensive-game 2 (build-tree 1))))

(doc centipede-4 'export #t)
(doc centipede-4 'description "4-round centipede game. SPE: P0 takes immediately for (2, 0).")
(define centipede-4 (make-centipede-game 4))

;; Entry Deterrence: Incumbent vs Potential Entrant
(doc entry-deterrence 'export #t)
(doc entry-deterrence 'description "Entry Deterrence Game: P0 (entrant) decides to enter or stay out. If enters, P1 (incumbent) fights or accommodates. Fight hurts both, accommodate shares market. SPE: enter, accommodate.")
(define entry-deterrence
  (make-extensive-game 2
    (make-decision 0 '(enter stay-out)
      (list
        ;; Entrant enters
        (make-decision 1 '(fight accommodate)
          (list
            (make-terminal '(-1 -1))   ; Fight: both lose
            (make-terminal '(2 2))))   ; Accommodate: share market
        ;; Entrant stays out
        (make-terminal '(0 5))))))     ; Incumbent keeps monopoly

;; Stackelberg Duopoly: Leader-follower quantity competition
(doc stackelberg-simple 'export #t)
(doc stackelberg-simple 'description "Simplified Stackelberg duopoly with discrete quantities. P0 (leader) chooses first, P1 (follower) responds. Payoffs = quantity * (10 - total quantity).")
(define stackelberg-simple
  (let* ([quantities '(1 2 3 4)]
         [payoff (lambda (q1 q2)
                   (let ([total (+ q1 q2)])
                        (* q1 (max 0 (- 10 total)))))]
         [make-follower-decision
          (lambda (q1)
            (make-decision 1 quantities
              (map (lambda (q2)
                     (make-terminal (list (payoff q1 q2) (payoff q2 q1))))
                   (map (lambda (q) (+ q 1)) (iota 4)))))])
        (make-extensive-game 2
          (make-decision 0 quantities
            (map make-follower-decision (map (lambda (q) (+ q 1)) (iota 4)))))))

;; Signaling Game with Chance Node: Type-dependent behavior
(doc signaling-simple 'export #t)
(doc signaling-simple 'description "Simple signaling game. Nature picks sender type (high/low). Sender sends signal (cost depends on type). Receiver responds. Illustrates separating vs pooling equilibria.")
(define signaling-simple
  (make-extensive-game 2
    (make-chance '(high-type low-type) '(0.5 0.5)
      (list
        ;; High type
        (make-decision 0 '(signal no-signal)
          (list
            ;; Signal (costly for high type: -1)
            (make-decision 1 '(accept reject)
              (list (make-terminal '(9 10))    ; high type accepted
                    (make-terminal '(-1 0))))
            ;; No signal
            (make-decision 1 '(accept reject)
              (list (make-terminal '(10 10))
                    (make-terminal '(0 0))))))
        ;; Low type
        (make-decision 0 '(signal no-signal)
          (list
            ;; Signal (very costly for low type: -5)
            (make-decision 1 '(accept reject)
              (list (make-terminal '(5 -5))    ; low type accepted but loses
                    (make-terminal '(-5 0))))
            ;; No signal
            (make-decision 1 '(accept reject)
              (list (make-terminal '(10 -5))   ; low type accepted
                    (make-terminal '(0 0))))))))))

;;; ============================================================================
;;; Information Sets (for imperfect information)
;;; ============================================================================

(doc 'section 'information-sets)

(doc 'note "Information sets group decision nodes that a player cannot distinguish.
In perfect information games, each info set contains exactly one node.
In imperfect information games (e.g., poker), info sets may contain multiple nodes.

When nodes share an information set:
- Same player must be acting at all nodes
- Same actions must be available at all nodes
- The player's strategy must choose the same action for all nodes in the set")

(define (make-info-set player label nodes)
  (doc 'export #t)
  (doc 'type '(-> Nat Symbol (List DecisionNode) InfoSet))
  (doc 'description "Create an information set for a player")
  (doc 'param 'player "Player who cannot distinguish these nodes")
  (doc 'param 'label "Label for this information set")
  (doc 'param 'nodes "List of decision nodes in this set")
  ;; Verify all nodes have same player and same actions
  (when (not (null? nodes))
        (let ([expected-actions (decision-actions (car nodes))])
             (for-each
              (lambda (n)
                (when (not (= (decision-player n) player))
                      (error 'make-info-set "all nodes must have same player"))
                (when (not (equal? (decision-actions n) expected-actions))
                      (error 'make-info-set "all nodes must have same actions")))
              nodes)))
  (list 'info-set player label nodes))

(define (info-set? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'info-set)))

(define (info-set-player iset)
  (doc 'export #t)
  (cadr iset))

(define (info-set-label iset)
  (doc 'export #t)
  (caddr iset))

(define (info-set-nodes iset)
  (doc 'export #t)
  (cadddr iset))

;;; ============================================================================
;;; Game with Information Sets
;;; ============================================================================

(define (add-info-set game info-set)
  (doc 'export #t)
  (doc 'type '(-> ExtensiveGame InfoSet ExtensiveGame))
  (doc 'description "Add an information set to the game")
  (list 'extensive-game
        (game-num-players game)
        (game-root game)
        (cons info-set (game-info-sets game))))

(define (perfect-information? game)
  (doc 'export #t)
  (doc 'type '(-> ExtensiveGame Bool))
  (doc 'description "Check if game has perfect information (no non-trivial info sets)")
  (null? (game-info-sets game)))

;;; ============================================================================
;;; Game Tree Printing
;;; ============================================================================

(doc 'section 'display)

(define (tree->string node depth)
  (doc 'type '(-> Node Nat String))
  (let ([indent (make-string (* 2 depth) #\space)])
       (cond
         [(terminal? node)
          (format "~aTerminal: ~a\n" indent (terminal-payoffs node))]

         [(decision? node)
          (string-append
           (format "~aP~a chooses:\n" indent (decision-player node))
           (apply string-append
                  (map (lambda (a c)
                         (string-append
                          (format "~a  ~a ->\n" indent a)
                          (tree->string c (+ depth 2))))
                       (decision-actions node)
                       (decision-children node))))]

         [(chance? node)
          (string-append
           (format "~aNature:\n" indent)
           (apply string-append
                  (map (lambda (o p c)
                         (string-append
                          (format "~a  ~a (~a) ->\n" indent o p)
                          (tree->string c (+ depth 2))))
                       (chance-outcomes node)
                       (chance-probs node)
                       (chance-children node))))]

         [else "Unknown node\n"])))

(define (print-tree node)
  (doc 'export #t)
  (doc 'type '(-> Node Void))
  (doc 'description "Print a game tree to console")
  (display (tree->string node 0)))
