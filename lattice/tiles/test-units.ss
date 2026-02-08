;;; lattice/tiles/test-units.ss — Tests for Unit/Entity Management
;;; Run with: scheme --script lattice/tiles/test-units.ss

(load "core/testing/test-framework.ss")
(load "lattice/tiles/core.ss")
(load "lattice/tiles/square.ss")
(load "lattice/tiles/pathfinding.ss")
(load "lattice/tiles/visibility.ss")
(load "lattice/tiles/units.ss")

;;; ============================================================
;;; Helpers
;;; ============================================================

(define board (make-square-board 5 5 (make-tile 'floor '())))

(define (sq-line c1 c2) (square-line c1 c2))
(define (sq-neighbors-ortho c) (square-neighbors c 'ortho))

;;; ============================================================
;;; Unit Construction
;;; ============================================================

(test-group "unit-construction"
  (define-test "make-unit creates unit"
    (let ([u (make-unit 'warrior 'fighter 'red '((hp . 10)))])
      (assert-equal 'warrior (unit%-id u))
      (assert-equal 'fighter (unit%-type u))
      (assert-equal 'red (unit%-team u))))

  (define-test "unit-get-prop existing"
    (let ([u (make-unit 'w 'fighter 'red '((hp . 10) (attack . 5)))])
      (assert-equal 10 (unit-get-prop u 'hp 0))
      (assert-equal 5 (unit-get-prop u 'attack 0))))

  (define-test "unit-get-prop default"
    (let ([u (make-unit 'w 'fighter 'red '())])
      (assert-equal 99 (unit-get-prop u 'hp 99))))

  (define-test "unit-set-prop returns new unit"
    (let* ([u1 (make-unit 'w 'fighter 'red '((hp . 10)))]
           [u2 (unit-set-prop u1 'hp 5)])
      ;; New unit has updated prop
      (assert-equal 5 (unit-get-prop u2 'hp 0))
      ;; Original unchanged
      (assert-equal 10 (unit-get-prop u1 'hp 0))))

  (define-test "unit-set-prop adds new property"
    (let* ([u1 (make-unit 'w 'fighter 'red '())]
           [u2 (unit-set-prop u1 'speed 3)])
      (assert-equal 3 (unit-get-prop u2 'speed 0))))

  (define-test "unit-set-prop preserves other properties"
    (let* ([u1 (make-unit 'w 'fighter 'red '((hp . 10) (attack . 5)))]
           [u2 (unit-set-prop u1 'hp 7)])
      (assert-equal 5 (unit-get-prop u2 'attack 0)))))

;;; ============================================================
;;; Game State
;;; ============================================================

(test-group "game-state"
  (define-test "make-game-state creates empty state"
    (let ([gs (make-game-state board)])
      (assert-equal '() (game-all-units gs))))

  (define-test "game-place-unit adds unit"
    (let* ([u (make-unit 'w 'fighter 'red '())]
           [gs (make-game-state board)]
           [gs (game-place-unit gs u (square-coord 1 1))])
      (assert-equal 1 (length (game-all-units gs)))))

  (define-test "game-get-unit-at retrieves unit"
    (let* ([u (make-unit 'w 'fighter 'red '((hp . 10)))]
           [gs (make-game-state board)]
           [gs (game-place-unit gs u (square-coord 2 3))]
           [found (game-get-unit-at gs (square-coord 2 3))])
      (assert-equal 'w (unit%-id found))
      (assert-equal 10 (unit-get-prop found 'hp 0))))

  (define-test "game-get-unit-at returns #f for empty"
    (let ([gs (make-game-state board)])
      (assert-false (game-get-unit-at gs (square-coord 0 0)))))

  (define-test "game-get-unit-coord finds unit position"
    (let* ([u (make-unit 'w 'fighter 'red '())]
           [gs (make-game-state board)]
           [gs (game-place-unit gs u (square-coord 3 4))]
           [coord (game-get-unit-coord gs 'w)])
      (assert-equal (square-coord 3 4) coord)))

  (define-test "game-get-unit-coord returns #f for unknown"
    (let ([gs (make-game-state board)])
      (assert-false (game-get-unit-coord gs 'nobody)))))

;;; ============================================================
;;; Placement and Movement
;;; ============================================================

(test-group "placement-movement"
  (define-test "placing unit again moves it"
    (let* ([u (make-unit 'w 'fighter 'red '())]
           [gs (make-game-state board)]
           [gs (game-place-unit gs u (square-coord 0 0))]
           [gs (game-place-unit gs u (square-coord 4 4))])
      ;; Should be at new position
      (assert-equal (square-coord 4 4) (game-get-unit-coord gs 'w))
      ;; Old position should be empty
      (assert-false (game-get-unit-at gs (square-coord 0 0)))
      ;; Still just one unit
      (assert-equal 1 (length (game-all-units gs)))))

  (define-test "game-remove-unit removes"
    (let* ([u (make-unit 'w 'fighter 'red '())]
           [gs (make-game-state board)]
           [gs (game-place-unit gs u (square-coord 1 1))]
           [gs (game-remove-unit gs 'w)])
      (assert-equal '() (game-all-units gs))
      (assert-false (game-get-unit-at gs (square-coord 1 1)))))

  (define-test "game-remove-unit for unknown is no-op"
    (let* ([u (make-unit 'w 'fighter 'red '())]
           [gs (make-game-state board)]
           [gs (game-place-unit gs u (square-coord 1 1))]
           [gs (game-remove-unit gs 'nobody)])
      (assert-equal 1 (length (game-all-units gs))))))

;;; ============================================================
;;; Multiple Units
;;; ============================================================

(test-group "multiple-units"
  (define u1 (make-unit 'warrior 'fighter 'red '((hp . 10))))
  (define u2 (make-unit 'mage 'caster 'blue '((hp . 6))))
  (define u3 (make-unit 'rogue 'scout 'red '((hp . 8))))

  (define-test "multiple units coexist"
    (let* ([gs (make-game-state board)]
           [gs (game-place-unit gs u1 (square-coord 0 0))]
           [gs (game-place-unit gs u2 (square-coord 4 4))]
           [gs (game-place-unit gs u3 (square-coord 2 2))])
      (assert-equal 3 (length (game-all-units gs)))))

  (define-test "game-units-by-team filters correctly"
    (let* ([gs (make-game-state board)]
           [gs (game-place-unit gs u1 (square-coord 0 0))]
           [gs (game-place-unit gs u2 (square-coord 4 4))]
           [gs (game-place-unit gs u3 (square-coord 2 2))]
           [reds (game-units-by-team gs 'red)]
           [blues (game-units-by-team gs 'blue)])
      (assert-equal 2 (length reds))
      (assert-equal 1 (length blues))))

  (define-test "each unit at distinct position"
    (let* ([gs (make-game-state board)]
           [gs (game-place-unit gs u1 (square-coord 0 0))]
           [gs (game-place-unit gs u2 (square-coord 4 4))])
      (assert-equal 'warrior (unit%-id (game-get-unit-at gs (square-coord 0 0))))
      (assert-equal 'mage (unit%-id (game-get-unit-at gs (square-coord 4 4)))))))

;;; ============================================================
;;; Unit Display
;;; ============================================================

(test-group "unit-display"
  (define-test "unit-char from property"
    (let ([u (make-unit 'w 'fighter 'red '((char . "W")))])
      (assert-equal "W" (unit-char u))))

  (define-test "unit-char fallback to type initial"
    (let ([u (make-unit 'w 'fighter 'red '())])
      (assert-equal "f" (unit-char u))))

  (define-test "game-render-overlay-coords lists all positions"
    (let* ([u1 (make-unit 'a 'fighter 'red '())]
           [u2 (make-unit 'b 'mage 'blue '())]
           [gs (make-game-state board)]
           [gs (game-place-unit gs u1 (square-coord 0 0))]
           [gs (game-place-unit gs u2 (square-coord 3 3))]
           [coords (game-render-overlay-coords gs)])
      (assert-equal 2 (length coords)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
