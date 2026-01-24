;;; lattice/tiles/test-board-size.ss — Tests for board-size and capacity
;;;
;;; Verifies that:
;;; - board-size returns 0 for empty boards (no tiles stored)
;;; - board-size returns correct count when tiles are stored
;;; - hex-board-capacity returns geometric capacity based on radius
;;; - square-board-capacity returns width * height

(load "lattice/tiles/boardcraft.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (display "  PASS: ")
       (display name)
       (newline)
       (set! tests-passed (+ tests-passed 1)))
      (begin
       (display "  FAIL: ")
       (display name)
       (display " - expected ")
       (display expected)
       (display " but got ")
       (display actual)
       (newline)
       (set! tests-failed (+ tests-failed 1)))))

(display "
")
(display "=== Board Size and Capacity Tests ===

")

;;; ====
;;; Test 1: Empty hex board (no default tile)
;;; ====
(display "1. Empty hex board size
")

(define hex-empty (make-hex-board 'axial 3))
(test "empty hex board has size 0" 0 (board-size hex-empty))
(test "empty hex board is empty" #t (board-empty? hex-empty))
(test "hex board radius is 3" 3 (hex-board-radius hex-empty))
(test "hex board capacity is 37" 37 (hex-board-capacity hex-empty))

;;; ====
;;; Test 2: Hex board with default tile
;;; ====
(display "
2. Pre-populated hex board size
")

(define floor-tile (make-tile 'floor '()))
(define hex-filled (make-hex-board 'axial 3 floor-tile))
(test "filled hex board has size 37" 37 (board-size hex-filled))
(test "filled hex board is not empty" #f (board-empty? hex-filled))
(test "size equals capacity when filled" (hex-board-capacity hex-filled) (board-size hex-filled))

;;; ====
;;; Test 3: Hex capacity formula for various radii
;;; ====
(display "
3. Hex capacity formula
")

(define (test-hex-capacity r expected)
  (let ([board (make-hex-board 'axial r)])
       (test (string-append "hex capacity for radius " (number->string r))
             expected
             (hex-board-capacity board))))

(test-hex-capacity 0 1)    ; Just center
(test-hex-capacity 1 7)    ; 1 + 6
(test-hex-capacity 2 19)   ; 1 + 6 + 12
(test-hex-capacity 3 37)   ; 1 + 6 + 12 + 18
(test-hex-capacity 4 61)   ; 1 + 6 + 12 + 18 + 24

;;; ====
;;; Test 4: Empty square board
;;; ====
(display "
4. Empty square board size
")

(define sq-empty (make-square-board 8 8))
(test "empty square board has size 0" 0 (board-size sq-empty))
(test "empty square board is empty" #t (board-empty? sq-empty))
(test "square board capacity is 64" 64 (square-board-capacity sq-empty))

;;; ====
;;; Test 5: Square board with default tile
;;; ====
(display "
5. Pre-populated square board size
")

(define sq-filled (make-square-board 8 8 floor-tile))
(test "filled square board has size 64" 64 (board-size sq-filled))
(test "filled square board is not empty" #f (board-empty? sq-filled))
(test "size equals capacity when filled" (square-board-capacity sq-filled) (board-size sq-filled))

;;; ====
;;; Test 6: Incrementally adding tiles
;;; ====
(display "
6. Incrementally adding tiles
")

(define hex-partial hex-empty)
(test "start with 0 tiles" 0 (board-size hex-partial))

(set! hex-partial (board-set hex-partial (axial-coord 0 0) floor-tile))
(test "after adding 1 tile" 1 (board-size hex-partial))

(set! hex-partial (board-set hex-partial (axial-coord 1 0) floor-tile))
(test "after adding 2 tiles" 2 (board-size hex-partial))

(set! hex-partial (board-set hex-partial (axial-coord 0 1) floor-tile))
(test "after adding 3 tiles" 3 (board-size hex-partial))

;;; ====
;;; Summary
;;; ====
(newline)
(display "=== Test Summary ===
")
(display "Passed: ")
(display tests-passed)
(newline)
(display "Failed: ")
(display tests-failed)
(newline)

(if (= tests-failed 0)
    (display "
All tests passed!
")
    (display "
Some tests failed!
"))
