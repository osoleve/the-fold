;;; extensive-form.sexp — Development tasks for lattice/game-theory/extensive-form.ss
;;;
;;; Module: extensive-form (tier 0, depends on prelude)
;;; 584 lines, ~30 exported functions
;;; Extensive form (sequential) games with game trees. Three node types
;;; (terminal, decision, chance), backward induction, subgame perfect
;;; equilibrium, information sets, expected utility, and classic games.
;;;
;;; Git history:
;;;   c6dffccb refactor(lattice): reorganize skills, consolidate graph modules
;;;   e68b9849 feat(module): migrate game-theory to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement decision-child
  ;; ──────────────────────────────────────────────
  ;; Find the child node reached by taking a specific action.

  (task
    (id "game-theory/extensive-form/decision-child-impl")
    (module extensive-form)
    (tier 0)
    (type implement)
    (description "Implement decision-child: given a decision node and an action symbol, return the child node reached by that action. Walk the actions list and children list in parallel using a named let loop. If the current action matches (via eq?), return the corresponding child. If the actions list is exhausted without finding the action, signal an error. This is the fundamental navigation primitive for traversing game trees — backward induction, expected utility, and strategy evaluation all call it.")
    (context "Available: decision-actions, decision-children, car, cdr, null?, eq?, error, let loop. Parallel list walk with eq? matching.")
    (before "(define (decision-child node action)
  ;; TODO: find child reached by action
  (error 'decision-child \"not implemented\"))")
    (test-file "lattice/game-theory/test-extensive-form.ss")
    (test-forms (";; Find correct child by action label
(let* ([t1 (make-terminal '(10 0))]
       [t2 (make-terminal '(0 10))]
       [d (make-decision 0 '(take share) (list t1 t2))])
  (assert-equal t1 (decision-child d 'take))
  (assert-equal t2 (decision-child d 'share)))"
                 ";; First action in a 3-action node
(let* ([t1 (make-terminal '(1 0))]
       [t2 (make-terminal '(0 1))]
       [t3 (make-terminal '(2 2))]
       [d (make-decision 0 '(a b c) (list t1 t2 t3))])
  (assert-equal t2 (decision-child d 'b)))"
                 ";; Missing action signals error
(let ([d (make-decision 0 '(left right)
           (list (make-terminal '(1 0))
                 (make-terminal '(0 1))))])
  (assert-error (lambda () (decision-child d 'up))))"))
    (available-modules (prelude))
    (hints ("Parallel walk: (let loop ([as (decision-actions node)] [cs (decision-children node)]) ...)"
            "Base case: (null? as) -> error, action not found"
            "Match: (eq? (car as) action) -> (car cs)"
            "Continue: (loop (cdr as) (cdr cs))"))
    (reference-solution "(define (decision-child node action)
  (let loop ([as (decision-actions node)]
             [cs (decision-children node)])
       (cond
         [(null? as) (error 'decision-child \"action not found\" action)]
         [(eq? (car as) action) (car cs)]
         [else (loop (cdr as) (cdr cs))])))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement tree-size
  ;; ──────────────────────────────────────────────
  ;; Count total nodes in a game tree.

  (task
    (id "game-theory/extensive-form/tree-size-impl")
    (module extensive-form)
    (tier 0)
    (type implement)
    (description "Implement tree-size: count the total number of nodes in a game tree. Three-way cond dispatch on node type: (1) terminal? -> 1 (leaf). (2) decision? -> 1 plus the sum of sizes of all children. (3) chance? -> 1 plus the sum of sizes of all children. The recursive step is the same for decision and chance: map tree-size over the children list, then sum with (fold-right + 0 ...). The only difference is how to get the children: decision-children vs chance-children. This is the simplest non-trivial recursion over the game tree structure.")
    (context "Available: terminal?, decision?, chance?, decision-children, chance-children, map, fold-right, +, cond. Three-way dispatch with recursive map + sum.")
    (before "(define (tree-size node)
  ;; TODO: count all nodes in the game tree
  0)")
    (test-file "lattice/game-theory/test-extensive-form.ss")
    (test-forms (";; Terminal node is 1
(assert-equal 1 (tree-size (make-terminal '(3 5))))"
                 ";; Single decision with 2 terminals = 3
(assert-equal 3
  (tree-size (make-decision 0 '(a b)
    (list (make-terminal '(1 0))
          (make-terminal '(0 1))))))"
                 ";; Nested game: 1 root + 1 decision + 3 terminals = 5
(assert-equal 5
  (tree-size (make-decision 0 '(a b)
    (list
      (make-decision 1 '(c d)
        (list (make-terminal '(1 1))
              (make-terminal '(2 2))))
      (make-terminal '(0 0))))))"
                 ";; Chance node counts too
(assert-equal 3
  (tree-size (make-chance '(h t) '(0.5 0.5)
    (list (make-terminal '(10 0))
          (make-terminal '(0 10))))))"))
    (available-modules (prelude))
    (hints ("Terminal: 1"
            "Decision: (+ 1 (fold-right + 0 (map tree-size (decision-children node))))"
            "Chance: (+ 1 (fold-right + 0 (map tree-size (chance-children node))))"
            "Use cond with three branches"))
    (reference-solution "(define (tree-size node)
  (cond
    [(terminal? node) 1]
    [(decision? node)
     (+ 1 (fold-right + 0 (map tree-size (decision-children node))))]
    [(chance? node)
     (+ 1 (fold-right + 0 (map tree-size (chance-children node))))]
    [else (error 'tree-size \"unknown node type\" node)]))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement terminal-nodes
  ;; ──────────────────────────────────────────────
  ;; Collect all leaf nodes from a game tree.

  (task
    (id "game-theory/extensive-form/terminal-nodes-impl")
    (module extensive-form)
    (tier 0)
    (type implement)
    (description "Implement terminal-nodes: collect all terminal (leaf) nodes from a game tree into a flat list. Three-way dispatch: (1) terminal? -> wrap in a singleton list. (2) decision? -> recursively collect from all children, then flatten with (apply append ...). (3) chance? -> same as decision but using chance-children. This produces a pre-order list of all leaves. The flattening step is key: each recursive call returns a list, so we have a list of lists that needs (apply append ...) to merge into one flat list.")
    (context "Available: terminal?, decision?, chance?, decision-children, chance-children, list, map, apply, append, cond. Recursive collection with list flattening.")
    (before "(define (terminal-nodes node)
  ;; TODO: collect all terminal nodes
  '())")
    (test-file "lattice/game-theory/test-extensive-form.ss")
    (test-forms (";; Single terminal returns itself
(let ([t (make-terminal '(7 3))])
  (assert-equal 1 (length (terminal-nodes t)))
  (assert-true (terminal? (car (terminal-nodes t)))))"
                 ";; Decision with 2 terminals
(let ([game (make-decision 0 '(a b)
              (list (make-terminal '(1 0))
                    (make-terminal '(0 1))))])
  (assert-equal 2 (length (terminal-nodes game))))"
                 ";; Nested: 3 terminals at different depths
(let ([game (make-decision 0 '(a b)
              (list (make-terminal '(1 1))
                    (make-decision 1 '(c d)
                      (list (make-terminal '(2 2))
                            (make-terminal '(3 3))))))])
  (assert-equal 3 (length (terminal-nodes game))))"
                 ";; Chance node children collected too
(let ([game (make-chance '(h t) '(0.5 0.5)
              (list (make-terminal '(10 0))
                    (make-decision 0 '(a)
                      (list (make-terminal '(5 5))))))])
  (assert-equal 2 (length (terminal-nodes game))))"))
    (available-modules (prelude))
    (hints ("Terminal: (list node) — wrap in singleton"
            "Decision: (apply append (map terminal-nodes (decision-children node)))"
            "Chance: (apply append (map terminal-nodes (chance-children node)))"
            "apply append flattens list-of-lists into one list"))
    (reference-solution "(define (terminal-nodes node)
  (cond
    [(terminal? node) (list node)]
    [(decision? node)
     (apply append (map terminal-nodes (decision-children node)))]
    [(chance? node)
     (apply append (map terminal-nodes (chance-children node)))]))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement tree-depth
  ;; ──────────────────────────────────────────────
  ;; Maximum depth of a game tree.

  (task
    (id "game-theory/extensive-form/tree-depth-impl")
    (module extensive-form)
    (tier 0)
    (type implement)
    (description "Implement tree-depth: compute the maximum depth of a game tree, where terminal nodes have depth 0. Three-way dispatch: (1) terminal? -> 0. (2) decision? -> 1 plus the maximum depth among children. (3) chance? -> same structure with chance-children. The maximum over children uses (apply max (cons 0 (map tree-depth children))). The (cons 0 ...) handles the edge case where the children list could be empty (though it shouldn't be in a well-formed game), ensuring max always has at least one argument. This measures the longest path from root to any terminal node.")
    (context "Available: terminal?, decision?, chance?, decision-children, chance-children, map, apply, max, cons, +, cond. Three-way dispatch with recursive max.")
    (before "(define (tree-depth node)
  ;; TODO: maximum depth (terminal = 0)
  0)")
    (test-file "lattice/game-theory/test-extensive-form.ss")
    (test-forms (";; Terminal has depth 0
(assert-equal 0 (tree-depth (make-terminal '(1 2))))"
                 ";; Single decision is depth 1
(assert-equal 1
  (tree-depth (make-decision 0 '(a b)
    (list (make-terminal '(1 0))
          (make-terminal '(0 1))))))"
                 ";; Nested game is depth 2
(assert-equal 2
  (tree-depth (make-decision 0 '(a b)
    (list
      (make-decision 1 '(c d)
        (list (make-terminal '(1 1))
              (make-terminal '(2 2))))
      (make-terminal '(0 0))))))"
                 ";; Asymmetric: depth follows longest path
(assert-equal 3
  (tree-depth (make-decision 0 '(a b)
    (list
      (make-decision 1 '(c d)
        (list (make-decision 0 '(e)
                (list (make-terminal '(1 1))))
              (make-terminal '(2 2))))
      (make-terminal '(0 0))))))"))
    (available-modules (prelude))
    (hints ("Terminal: 0"
            "Decision: (+ 1 (apply max (cons 0 (map tree-depth (decision-children node)))))"
            "Chance: same pattern with chance-children"
            "(cons 0 ...) ensures max always has at least one argument"))
    (reference-solution "(define (tree-depth node)
  (cond
    [(terminal? node) 0]
    [(decision? node)
     (+ 1 (apply max (cons 0 (map tree-depth (decision-children node)))))]
    [(chance? node)
     (+ 1 (apply max (cons 0 (map tree-depth (chance-children node)))))]
    [else (error 'tree-depth \"unknown node type\" node)]))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement expected-utility
  ;; ──────────────────────────────────────────────
  ;; Compute expected payoff under a behavioral strategy.

  (task
    (id "game-theory/extensive-form/expected-utility-impl")
    (module extensive-form)
    (tier 0)
    (type implement)
    (description "Implement expected-utility: compute the expected payoff for a specific player given a behavioral strategy function. The strategy is a function (player, available-actions) -> chosen-action that specifies what each player does. Three-way recursive dispatch: (1) terminal? -> return this player's payoff via terminal-payoff. (2) decision? -> call the strategy to determine which action the acting player takes, find the corresponding child via decision-child, and recurse. (3) chance? -> compute the expected value over all branches: for each child, recursively compute utility, multiply by the branch probability, and sum. Uses (fold-right + 0 (map * probs utils)) to compute the weighted sum.")
    (context "Available: terminal?, decision?, chance?, terminal-payoff, decision-player, decision-actions, decision-child, chance-probs, chance-children, map, fold-right, +, *, let*, cond. Three-way dispatch with strategy application and probability weighting.")
    (before "(define (expected-utility node player strategy)
  ;; TODO: expected payoff for player under strategy
  0)")
    (test-file "lattice/game-theory/test-extensive-form.ss")
    (test-forms (";; Terminal returns player's payoff directly
(let ([t (make-terminal '(7 3))])
  (assert-equal 7 (expected-utility t 0 (lambda (p as) (car as))))
  (assert-equal 3 (expected-utility t 1 (lambda (p as) (car as)))))"
                 ";; Strategy picks first action -> left branch
(let ([game (make-decision 0 '(left right)
              (list (make-terminal '(10 0))
                    (make-terminal '(0 10))))])
  (assert-equal 10 (expected-utility game 0 (lambda (p as) (car as))))
  (assert-equal 0 (expected-utility game 0 (lambda (p as) (cadr as)))))"
                 ";; Chance node computes expected value
(let ([game (make-chance '(a b) '(0.5 0.5)
              (list (make-terminal '(10 0))
                    (make-terminal '(0 10))))])
  (assert-true (< (abs (- (expected-utility game 0 (lambda (p as) (car as))) 5)) 0.01)))"
                 ";; Two-level game with strategy
(let ([game (make-decision 0 '(a b)
              (list
                (make-decision 1 '(c d)
                  (list (make-terminal '(4 4))
                        (make-terminal '(2 6))))
                (make-terminal '(3 3))))])
  ;; Strategy: always pick first action. P0 picks a, P1 picks c -> (4, 4)
  (assert-equal 4 (expected-utility game 0 (lambda (p as) (car as)))))"))
    (available-modules (prelude))
    (hints ("Terminal: (terminal-payoff node player)"
            "Decision: find chosen action via (strategy (decision-player node) (decision-actions node))"
            "Then recurse: (expected-utility (decision-child node chosen) player strategy)"
            "Chance: compute utils for all children, multiply by probs, sum"
            "(fold-right + 0 (map * (chance-probs node) utils))"))
    (reference-solution "(define (expected-utility node player strategy)
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
    [else (error 'expected-utility \"unknown node type\" node)]))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 2))

) ;; end tasks
