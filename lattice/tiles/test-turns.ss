;;; lattice/tiles/test-turns.ss — Tests for Turn-Based Game System
;;; Run with: scheme --script lattice/tiles/test-turns.ss

(load "core/testing/test-framework.ss")
(load "lattice/tiles/core.ss")
(load "lattice/tiles/square.ss")
(load "lattice/tiles/pathfinding.ss")
(load "lattice/tiles/visibility.ss")
(load "lattice/tiles/units.ss")
(load "lattice/tiles/turns.ss")

;;; ============================================================
;;; Helpers
;;; ============================================================

(define (make-max-actions . pairs)
  ;; pairs: (id ap id ap ...)
  (let ([ht (make-hashtable equal-hash equal?)])
    (let loop ([ps pairs])
      (if (or (null? ps) (null? (cdr ps)))
          ht
          (begin
            (hashtable-set! ht (car ps) (cadr ps))
            (loop (cddr ps)))))))

;;; ============================================================
;;; Turn State Construction
;;; ============================================================

(test-group "turn-state-construction"
  (define-test "creates initial state"
    (let* ([max-ap (make-max-actions 'alice 3 'bob 2)]
           [ts (make-turn-state '(alice bob) max-ap)])
      (assert-equal 1 (turn-number ts))
      (assert-equal 'alice (turn-current-unit ts))
      (assert-equal 'movement (turn-current-phase ts))))

  (define-test "action points initialized from table"
    (let* ([max-ap (make-max-actions 'alice 3 'bob 2)]
           [ts (make-turn-state '(alice bob) max-ap)])
      (assert-equal 3 (turn-actions-remaining ts 'alice))
      (assert-equal 2 (turn-actions-remaining ts 'bob))))

  (define-test "empty order has no active unit"
    (let* ([max-ap (make-hashtable equal-hash equal?)]
           [ts (make-turn-state '() max-ap)])
      (assert-false (turn-current-unit ts))))

  (define-test "default AP is 2 for unlisted units"
    (let* ([max-ap (make-hashtable equal-hash equal?)]
           [ts (make-turn-state '(alice) max-ap)])
      (assert-equal 2 (turn-actions-remaining ts 'alice))))

  (define-test "history starts empty"
    (let* ([max-ap (make-max-actions 'alice 2)]
           [ts (make-turn-state '(alice) max-ap)])
      (assert-equal '() (turn-history ts)))))

;;; ============================================================
;;; Queries
;;; ============================================================

(test-group "turn-queries"
  (define max-ap (make-max-actions 'alice 3 'bob 2))
  (define ts (make-turn-state '(alice bob) max-ap))

  (define-test "turn-can-act? true for active unit with AP"
    (assert-true (turn-can-act? ts 'alice)))

  (define-test "turn-can-act? false for non-active unit"
    (assert-false (turn-can-act? ts 'bob)))

  (define-test "turn-actions-remaining for unknown unit"
    (assert-equal 0 (turn-actions-remaining ts 'nobody))))

;;; ============================================================
;;; Spending Action Points
;;; ============================================================

(test-group "turn-spend-action"
  (define max-ap (make-max-actions 'alice 3 'bob 2))
  (define ts (make-turn-state '(alice bob) max-ap))

  (define-test "spending reduces AP"
    (let ([ts2 (turn-spend-action ts 'alice 1)])
      (assert-equal 2 (turn-actions-remaining ts2 'alice))))

  (define-test "spending multiple AP"
    (let ([ts2 (turn-spend-action ts 'alice 2)])
      (assert-equal 1 (turn-actions-remaining ts2 'alice))))

  (define-test "spending all AP"
    (let ([ts2 (turn-spend-action ts 'alice 3)])
      (assert-equal 0 (turn-actions-remaining ts2 'alice))))

  (define-test "overspending clamps to 0"
    (let ([ts2 (turn-spend-action ts 'alice 10)])
      (assert-equal 0 (turn-actions-remaining ts2 'alice))))

  (define-test "spending doesn't affect other units"
    (let ([ts2 (turn-spend-action ts 'alice 2)])
      (assert-equal 2 (turn-actions-remaining ts2 'bob))))

  (define-test "immutability: original unchanged"
    (let ([ts2 (turn-spend-action ts 'alice 1)])
      (assert-equal 3 (turn-actions-remaining ts 'alice)))))

;;; ============================================================
;;; Phase Advancement
;;; ============================================================

(test-group "turn-phases"
  (define max-ap (make-max-actions 'alice 2))
  (define ts (make-turn-state '(alice) max-ap))

  (define-test "starts at movement"
    (assert-equal 'movement (turn-current-phase ts)))

  (define-test "movement -> action"
    (let ([ts2 (turn-next-phase ts)])
      (assert-equal 'action (turn-current-phase ts2))))

  (define-test "action -> end"
    (let ([ts2 (turn-next-phase (turn-next-phase ts))])
      (assert-equal 'end (turn-current-phase ts2))))

  (define-test "end -> movement (cycle)"
    (let ([ts2 (turn-next-phase (turn-next-phase (turn-next-phase ts)))])
      (assert-equal 'movement (turn-current-phase ts2))))

  (define-test "phase advancement preserves active unit"
    (let ([ts2 (turn-next-phase ts)])
      (assert-equal 'alice (turn-current-unit ts2))))

  (define-test "phase advancement preserves AP"
    (let ([ts2 (turn-next-phase ts)])
      (assert-equal 2 (turn-actions-remaining ts2 'alice)))))

;;; ============================================================
;;; Turn Advancement (Next Unit)
;;; ============================================================

(test-group "turn-advancement"
  (define max-ap (make-max-actions 'alice 3 'bob 2 'carol 4))
  (define ts (make-turn-state '(alice bob carol) max-ap))

  (define-test "advances to next unit"
    (let ([ts2 (turn-next-unit ts max-ap)])
      (assert-equal 'bob (turn-current-unit ts2))))

  (define-test "wraps back to first unit"
    (let* ([ts2 (turn-next-unit ts max-ap)]
           [ts3 (turn-next-unit ts2 max-ap)]
           [ts4 (turn-next-unit ts3 max-ap)])
      (assert-equal 'alice (turn-current-unit ts4))))

  (define-test "turn number increments on wrap"
    (let* ([ts2 (turn-next-unit ts max-ap)]
           [ts3 (turn-next-unit ts2 max-ap)]
           [ts4 (turn-next-unit ts3 max-ap)])
      ;; alice->bob (turn 1), bob->carol (turn 1), carol->alice (turn 2)
      (assert-equal 1 (turn-number ts2))
      (assert-equal 1 (turn-number ts3))
      (assert-equal 2 (turn-number ts4))))

  (define-test "AP refreshed for next unit"
    ;; Spend alice's AP, then advance to bob
    (let* ([ts2 (turn-spend-action ts 'alice 3)]
           [ts3 (turn-next-unit ts2 max-ap)])
      ;; Bob should have fresh AP
      (assert-equal 2 (turn-actions-remaining ts3 'bob))
      ;; Alice's AP still spent
      (assert-equal 0 (turn-actions-remaining ts3 'alice))))

  (define-test "resets to movement phase"
    (let* ([ts2 (turn-next-phase ts)]  ; movement -> action
           [ts3 (turn-next-unit ts2 max-ap)])
      (assert-equal 'movement (turn-current-phase ts3))))

  (define-test "turn-end-turn is alias for next-unit"
    (let ([ts2 (turn-end-turn ts max-ap)])
      (assert-equal 'bob (turn-current-unit ts2)))))

;;; ============================================================
;;; History
;;; ============================================================

(test-group "turn-history"
  (define max-ap (make-max-actions 'alice 3))
  (define ts (make-turn-state '(alice) max-ap))

  (define-test "log adds entry"
    (let ([ts2 (turn-log-action ts 'move '(1 2))])
      (assert-equal 1 (length (turn-history ts2)))))

  (define-test "log entry has correct structure"
    (let* ([ts2 (turn-log-action ts 'move '(1 2))]
           [entry (car (turn-history ts2))])
      ;; (turn-number active-unit action-type details)
      (assert-equal 1 (car entry))        ; turn number
      (assert-equal 'alice (cadr entry))   ; active unit
      (assert-equal 'move (caddr entry))   ; action type
      (assert-equal '(1 2) (cadddr entry)))) ; details

  (define-test "multiple logs in LIFO order"
    (let* ([ts2 (turn-log-action ts 'move '(1 2))]
           [ts3 (turn-log-action ts2 'attack 'goblin)])
      (assert-equal 2 (length (turn-history ts3)))
      (assert-equal 'attack (caddr (car (turn-history ts3))))  ; most recent
      (assert-equal 'move (caddr (cadr (turn-history ts3)))))) ; older

  (define-test "turn-recent-actions returns N entries"
    (let* ([ts2 (turn-log-action ts 'a 1)]
           [ts3 (turn-log-action ts2 'b 2)]
           [ts4 (turn-log-action ts3 'c 3)]
           [recent (turn-recent-actions ts4 2)])
      (assert-equal 2 (length recent))
      ;; Recent returns most recent first (from history head)
      (assert-equal 'c (caddr (car recent))))))

;;; ============================================================
;;; Initiative
;;; ============================================================

(test-group "initiative"
  (define-test "higher initiative goes first"
    (let* ([u1 (make-unit 'slow 'warrior 'red '((initiative . 3)))]
           [u2 (make-unit 'fast 'rogue 'red '((initiative . 8)))]
           [order (calculate-initiative-order (list u1 u2))])
      (assert-equal 'fast (car order))
      (assert-equal 'slow (cadr order))))

  (define-test "ties broken alphabetically by ID"
    (let* ([u1 (make-unit 'beta 'warrior 'red '((initiative . 5)))]
           [u2 (make-unit 'alpha 'warrior 'red '((initiative . 5)))]
           [order (calculate-initiative-order (list u1 u2))])
      (assert-equal 'alpha (car order))
      (assert-equal 'beta (cadr order))))

  (define-test "default initiative is 0"
    (let* ([u1 (make-unit 'a 'warrior 'red '())]
           [u2 (make-unit 'b 'warrior 'red '((initiative . 1)))]
           [order (calculate-initiative-order (list u1 u2))])
      (assert-equal 'b (car order)))))

;;; ============================================================
;;; Game Integration
;;; ============================================================

(test-group "game-integration"
  (define board (make-square-board 5 5 (make-tile 'floor '())))
  (define u1 (make-unit 'warrior 'fighter 'red '((hp . 10))))
  (define u2 (make-unit 'mage 'caster 'blue '((hp . 6))))

  (define-test "make-game-with-turns creates combined state"
    (let* ([gs (make-game-state board)]
           [gs (game-place-unit gs u1 (square-coord 0 0))]
           [gs (game-place-unit gs u2 (square-coord 4 4))]
           [gwt (make-game-with-turns gs)])
      (assert-true (game-with-turns%? gwt))))

  (define-test "game-execute-action succeeds for active unit"
    (let* ([gs (make-game-state board)]
           [gs (game-place-unit gs u1 (square-coord 0 0))]
           [gs (game-place-unit gs u2 (square-coord 4 4))]
           [gwt (make-game-with-turns gs)]
           [result (game-execute-action gwt 'move identity 1)])
      ;; Should succeed (returns new gwt, not #f)
      (assert-true (game-with-turns%? result))))

  (define-test "game-execute-action deducts AP"
    (let* ([gs (make-game-state board)]
           [gs (game-place-unit gs u1 (square-coord 0 0))]
           [gs (game-place-unit gs u2 (square-coord 4 4))]
           [gwt (make-game-with-turns gs)]
           [result (game-execute-action gwt 'move identity 1)]
           [ts (game-with-turns%-turns result)]
           [active (turn-current-unit ts)])
      ;; Started with 2 AP, spent 1
      (assert-equal 1 (turn-actions-remaining ts active))))

  (define-test "game-execute-action fails when no AP"
    (let* ([gs (make-game-state board)]
           [gs (game-place-unit gs u1 (square-coord 0 0))]
           [gwt (make-game-with-turns gs)]
           ;; Spend all AP
           [r1 (game-execute-action gwt 'a identity 1)]
           [r2 (game-execute-action r1 'b identity 1)]
           ;; Third action should fail (0 AP remaining)
           [r3 (game-execute-action r2 'c identity 1)])
      (assert-false r3))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
