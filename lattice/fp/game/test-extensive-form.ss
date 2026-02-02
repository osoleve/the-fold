(load "core/testing/test-framework.ss")
(load "lattice/fp/game/extensive-form.ss")

;;; ============================================================================
;;; Node Construction Tests
;;; ============================================================================

(test-group "node-construction"
  (define-test "terminal node stores payoffs"
    (let ([t (make-terminal '(3 5))])
         (assert-true (terminal? t))
         (assert-equal '(3 5) (terminal-payoffs t))
         (assert-equal 3 (terminal-payoff t 0))
         (assert-equal 5 (terminal-payoff t 1))))

  (define-test "decision node stores player and actions"
    (let* ([t1 (make-terminal '(1 0))]
           [t2 (make-terminal '(0 1))]
           [d (make-decision 0 '(left right) (list t1 t2))])
          (assert-true (decision? d))
          (assert-equal 0 (decision-player d))
          (assert-equal '(left right) (decision-actions d))
          (assert-equal 2 (length (decision-children d)))))

  (define-test "decision-child retrieves correct child"
    (let* ([t1 (make-terminal '(10 0))]
           [t2 (make-terminal '(0 10))]
           [d (make-decision 0 '(take share) (list t1 t2))])
          (assert-equal t1 (decision-child d 'take))
          (assert-equal t2 (decision-child d 'share))))

  (define-test "chance node stores probabilities"
    (let* ([t1 (make-terminal '(5 5))]
           [t2 (make-terminal '(0 0))]
           [c (make-chance '(heads tails) '(0.5 0.5) (list t1 t2))])
          (assert-true (chance? c))
          (assert-equal '(heads tails) (chance-outcomes c))
          (assert-equal '(0.5 0.5) (chance-probs c))))

  (define-test "chance node rejects invalid probabilities"
    (let ([t (make-terminal '(0 0))])
         (assert-error (lambda () (make-chance '(a b) '(0.6 0.6) (list t t))))))

  (define-test "decision node rejects mismatched actions/children"
    (let ([t (make-terminal '(0 0))])
         (assert-error (lambda () (make-decision 0 '(a b c) (list t t)))))))

;;; ============================================================================
;;; Tree Utility Tests
;;; ============================================================================

(test-group "tree-utilities"
  (define-test "tree-depth of terminal is 0"
    (assert-equal 0 (tree-depth (make-terminal '(1 2)))))

  (define-test "tree-depth of single decision is 1"
    (let ([d (make-decision 0 '(a b)
               (list (make-terminal '(1 0))
                     (make-terminal '(0 1))))])
         (assert-equal 1 (tree-depth d))))

  (define-test "tree-depth of nested game"
    (let ([game (make-decision 0 '(a b)
                  (list
                    (make-decision 1 '(c d)
                      (list (make-terminal '(1 1))
                            (make-terminal '(2 2))))
                    (make-terminal '(0 0))))])
         (assert-equal 2 (tree-depth game))))

  (define-test "tree-size counts all nodes"
    (let ([game (make-decision 0 '(a b)
                  (list
                    (make-decision 1 '(c d)
                      (list (make-terminal '(1 1))
                            (make-terminal '(2 2))))
                    (make-terminal '(0 0))))])
         ;; 1 root + 1 decision + 3 terminals = 5
         (assert-equal 5 (tree-size game))))

  (define-test "terminal-nodes collects all leaves"
    (let ([game (make-decision 0 '(a b)
                  (list (make-terminal '(1 1))
                        (make-decision 1 '(c d)
                          (list (make-terminal '(2 2))
                                (make-terminal '(3 3))))))])
         (assert-equal 3 (length (terminal-nodes game))))))

;;; ============================================================================
;;; Backward Induction Tests
;;; ============================================================================

(test-group "backward-induction"
  (define-test "backward induction on terminal returns payoffs"
    (let-values ([(payoffs strategy) (backward-induction (make-terminal '(7 3)))])
                (assert-equal '(7 3) payoffs)
                (assert-equal '() strategy)))

  (define-test "backward induction picks best action"
    ;; P0 chooses between (10, 0) and (5, 5)
    (let* ([game (make-decision 0 '(selfish fair)
                   (list (make-terminal '(10 0))
                         (make-terminal '(5 5))))])
          (let-values ([(payoffs strategy) (backward-induction game)])
                      ;; P0 picks selfish for payoff 10
                      (assert-equal '(10 0) payoffs)
                      (assert-equal '((0 . selfish)) strategy))))

  (define-test "backward induction handles two-level game"
    ;; P0 moves first, P1 responds
    (let* ([game (make-decision 0 '(left right)
                   (list
                     ;; left -> P1 chooses
                     (make-decision 1 '(up down)
                       (list (make-terminal '(4 4))
                             (make-terminal '(2 6))))
                     ;; right -> terminal
                     (make-terminal '(3 3))))])
          (let-values ([(payoffs strategy) (backward-induction game)])
                      ;; P1 picks down (6 > 4), so left gives P0 only 2
                      ;; P0 picks right (3 > 2)
                      (assert-equal '(3 3) payoffs))))

  (define-test "backward induction with chance node"
    ;; Chance: 50% heads (10, 0), 50% tails (0, 10)
    (let ([game (make-chance '(heads tails) '(0.5 0.5)
                  (list (make-terminal '(10 0))
                        (make-terminal '(0 10))))])
         (let-values ([(payoffs strategy) (backward-induction game)])
                     ;; Expected value for each player is 5
                     (assert-true (< (abs (- (car payoffs) 5)) 0.01))
                     (assert-true (< (abs (- (cadr payoffs) 5)) 0.01))))))

;;; ============================================================================
;;; Classic Game Tests
;;; ============================================================================

(test-group "classic-games"
  (define-test "ultimatum game structure"
    (assert-true (extensive-game? ultimatum-game))
    (assert-equal 2 (game-num-players ultimatum-game))
    (let ([root (game-root ultimatum-game)])
         (assert-true (decision? root))
         (assert-equal 0 (decision-player root))))

  (define-test "ultimatum game SPE"
    ;; P1 offers 90-10 (minimum acceptable), P2 accepts anything > 0
    (let ([result (solve-spe ultimatum-game)])
         (assert-true (spe-result? result))
         ;; P2 accepts any offer, so P1 should offer worst acceptable
         (let ([payoffs (spe-payoffs result)])
              ;; SPE: P1 offers 90-10, P2 accepts -> (90, 10)
              (assert-equal 90 (car payoffs))
              (assert-equal 10 (cadr payoffs)))))

  (define-test "centipede game SPE is immediate take"
    (let* ([result (solve-spe centipede-4)]
           [payoffs (spe-payoffs result)])
          ;; Backward induction leads to P0 taking at round 1
          ;; Round 1: take gives (2, 0) for P0
          (assert-equal 2 (car payoffs))
          (assert-equal 0 (cadr payoffs))))

  (define-test "entry deterrence SPE"
    (let* ([result (solve-spe entry-deterrence)]
           [payoffs (spe-payoffs result)]
           [strategy (spe-strategy result)])
          ;; SPE: Entrant enters (2 > 0), Incumbent accommodates (2 > -1)
          (assert-equal '(2 2) payoffs)
          ;; Strategy should include both decisions
          (assert-equal 2 (length strategy)))))

;;; ============================================================================
;;; Information Set Tests
;;; ============================================================================

(test-group "information-sets"
  (define-test "perfect information check"
    (assert-true (perfect-information? ultimatum-game)))

  (define-test "info set construction"
    (let* ([d1 (make-decision 1 '(up down)
                 (list (make-terminal '(1 0))
                       (make-terminal '(0 1))))]
           [d2 (make-decision 1 '(up down)
                 (list (make-terminal '(2 0))
                       (make-terminal '(0 2))))]
           [iset (make-info-set 1 'ambiguous (list d1 d2))])
          (assert-true (info-set? iset))
          (assert-equal 1 (info-set-player iset))
          (assert-equal 'ambiguous (info-set-label iset))
          (assert-equal 2 (length (info-set-nodes iset)))))

  (define-test "info set rejects mismatched actions"
    (let* ([d1 (make-decision 1 '(up down)
                 (list (make-terminal '(1 0))
                       (make-terminal '(0 1))))]
           [d2 (make-decision 1 '(left right)  ; different actions!
                 (list (make-terminal '(2 0))
                       (make-terminal '(0 2))))])
          (assert-error (lambda () (make-info-set 1 'bad (list d1 d2))))))

  (define-test "adding info set marks imperfect information"
    (let* ([root (make-decision 0 '(a) (list (make-terminal '(0 0))))]
           [game (make-extensive-game 2 root)]
           [iset (make-info-set 0 'test (list root))]
           [game2 (add-info-set game iset)])
          (assert-true (perfect-information? game))
          (assert-false (perfect-information? game2)))))

;;; ============================================================================
;;; Expected Utility Tests
;;; ============================================================================

(test-group "expected-utility"
  (define-test "expected utility at terminal"
    (let ([t (make-terminal '(7 3))])
         (assert-equal 7 (expected-utility t 0 (lambda (p as) (car as))))
         (assert-equal 3 (expected-utility t 1 (lambda (p as) (car as))))))

  (define-test "expected utility follows strategy"
    (let ([game (make-decision 0 '(left right)
                  (list (make-terminal '(10 0))
                        (make-terminal '(0 10))))])
         ;; Strategy: always pick first action
         (assert-equal 10
           (expected-utility game 0 (lambda (p as) (car as))))
         ;; Strategy: always pick second action
         (assert-equal 0
           (expected-utility game 0 (lambda (p as) (cadr as))))))

  (define-test "expected utility with chance node"
    (let ([game (make-chance '(a b) '(0.5 0.5)
                  (list (make-terminal '(10 0))
                        (make-terminal '(0 10))))])
         (assert-true (< (abs (- (expected-utility game 0 (lambda (p as) (car as))) 5)) 0.01)))))

;;; ============================================================================
;;; Stackelberg Game Tests
;;; ============================================================================

(test-group "stackelberg"
  (define-test "stackelberg game structure"
    (assert-true (extensive-game? stackelberg-simple))
    (assert-equal 2 (game-num-players stackelberg-simple))
    (let ([root (game-root stackelberg-simple)])
         (assert-true (decision? root))
         (assert-equal 0 (decision-player root))
         (assert-equal 4 (length (decision-actions root)))))

  (define-test "stackelberg SPE exists"
    (let* ([result (solve-spe stackelberg-simple)]
           [payoffs (spe-payoffs result)]
           [strategy (spe-strategy result)])
          (assert-true (spe-result? result))
          ;; Leader should get higher payoff than follower (first-mover advantage)
          (assert-true (> (car payoffs) 0))
          (assert-true (> (cadr payoffs) 0)))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
