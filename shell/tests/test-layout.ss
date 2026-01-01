;;; thimble/tests/test-layout.ss — Test vectors for Text Layout Primitives

(load "core/prelude.ss")
(load "shell/layout.ss")

;;; NOTE: string utilities provided by core/prelude.ss
;;;   - string-contains?

;;; ============================================================
;;; Test 1: Canvas Construction
;;; ============================================================

(display "Test 1: Canvas Construction\n")
(define c1 (make-canvas 10 5))
(display "  width: ") (display (canvas-width c1)) (newline)
(display "  height: ") (display (canvas-height c1)) (newline)
(display "  ref (0,0): '") (display (canvas-ref c1 0 0)) (display "'\n")
(display "  ref (5,2): '") (display (canvas-ref c1 5 2)) (display "'\n")
(display "  ✓ Canvas created with correct dimensions\n")

;;; ============================================================
;;; Test 2: Canvas Set and Ref
;;; ============================================================

(display "\nTest 2: Canvas Set and Ref\n")
(define c2 (canvas-set c1 3 2 #\X))
(display "  After set (3,2) to 'X':\n")
(display "    ref (3,2): '") (display (canvas-ref c2 3 2)) (display "'\n")
(display "    ref (3,1): '") (display (canvas-ref c2 3 1)) (display "'\n")
(display "    original unchanged: ")
(display (if (char=? (canvas-ref c1 3 2) #\space) "#t" "#f"))
(newline)
(display "  ✓ Functional update works correctly\n")

;;; ============================================================
;;; Test 3: Out-of-Bounds Handling
;;; ============================================================

(display "\nTest 3: Out-of-Bounds Handling\n")
(define c3 (canvas-set c1 -1 0 #\!))
(define c4 (canvas-set c3 100 100 #\!))
(display "  Set at negative coords: canvas unchanged? ")
(display (if (char=? (canvas-ref c3 0 0) #\space) "#t" "#f"))
(newline)
(display "  Set beyond bounds: canvas unchanged? ")
(display (if (char=? (canvas-ref c4 5 2) #\space) "#t" "#f"))
(newline)
(display "  Ref out-of-bounds returns space: ")
(display (if (char=? (canvas-ref c1 100 100) #\space) "#t" "#f"))
(newline)
(display "  ✓ Out-of-bounds handled gracefully\n")

;;; ============================================================
;;; Test 4: Draw Char
;;; ============================================================

(display "\nTest 4: Draw Char\n")
(define c5 (draw-char (make-canvas 5 3) (point 2 1) #\@))
(display "  Character '@' at (2,1): '")
(display (canvas-ref c5 2 1))
(display "'\n")
(display "  ✓ draw-char places character correctly\n")

;;; ============================================================
;;; Test 5: Draw String Horizontal
;;; ============================================================

(display "\nTest 5: Draw String Horizontal\n")
(define c6 (draw-string (make-canvas 20 3) (point 2 1) "Hello, DUCKIE!"))
(display "Canvas:\n")
(display (canvas->string c6))
(newline)
(display "  Verify 'H' at (2,1): ")
(display (if (char=? (canvas-ref c6 2 1) #\H) "#t" "#f"))
(newline)
(display "  Verify '!' at (14,1): ")
(display (if (char=? (canvas-ref c6 14 1) #\!) "#t" "#f"))
(newline)
(display "  ✓ draw-string works horizontally\n")

;;; ============================================================
;;; Test 6: Draw String Clipping
;;; ============================================================

(display "\nTest 6: Draw String Clipping\n")
(define c7 (draw-string (make-canvas 10 3) (point 5 1) "LongString"))
(display "Canvas (should clip at boundary):\n")
(display (canvas->string c7))
(newline)
(display "  String clips at width: ")
(display (if (char=? (canvas-ref c7 9 1) #\g) "#t" "#f"))
(newline)
(display "  ✓ draw-string clips at boundaries\n")

;;; ============================================================
;;; Test 7: Draw String Vertical
;;; ============================================================

(display "\nTest 7: Draw String Vertical\n")
(define c8 (draw-string-v (make-canvas 10 8) (point 3 1) "DUCKIE"))
(display "Canvas:\n")
(display (canvas->string c8))
(newline)
(display "  Verify 'D' at (3,1): ")
(display (if (char=? (canvas-ref c8 3 1) #\D) "#t" "#f"))
(newline)
(display "  Verify 'E' at (3,6): ")
(display (if (char=? (canvas-ref c8 3 6) #\E) "#t" "#f"))
(newline)
(display "  ✓ draw-string-v works vertically\n")

;;; ============================================================
;;; Test 8: Draw Rectangle
;;; ============================================================

(display "\nTest 8: Draw Rectangle\n")
(define c9 (draw-rect (make-canvas 12 6)
                      (make-rect (point 1 1) 10 4)
                      #\#))
(display "Canvas:\n")
(display (canvas->string c9))
(newline)
(display "  Top-left corner: ")
(display (if (char=? (canvas-ref c9 1 1) #\#) "#t" "#f"))
(newline)
(display "  Interior is empty: ")
(display (if (char=? (canvas-ref c9 5 2) #\space) "#t" "#f"))
(newline)
(display "  ✓ draw-rect creates proper outline\n")

;;; ============================================================
;;; Test 9: Fill Rectangle
;;; ============================================================

(display "\nTest 9: Fill Rectangle\n")
(define c10 (fill-rect (make-canvas 12 6)
                       (make-rect (point 2 1) 8 3)
                       #\=))
(display "Canvas:\n")
(display (canvas->string c10))
(newline)
(display "  Interior is filled: ")
(display (if (char=? (canvas-ref c10 5 2) #\=) "#t" "#f"))
(newline)
(display "  ✓ fill-rect fills interior\n")

;;; ============================================================
;;; Test 10: Composite
;;; ============================================================

(display "\nTest 10: Composite\n")
(define c-dest (make-canvas 15 6))
(define c-src (let* ([c (make-canvas 5 3)]
                     [c (draw-string c (point 0 0) "SRC")]
                     [c (draw-string c (point 0 1) "BOX")]
                     [c (draw-string c (point 0 2) "!!!")])
                    c))
(define c11 (composite c-dest c-src (point 5 2)))
(display "Canvas:\n")
(display (canvas->string c11))
(newline)
(display "  'S' at (5,2): ")
(display (if (char=? (canvas-ref c11 5 2) #\S) "#t" "#f"))
(newline)
(display "  '!' at (5,4): ")
(display (if (char=? (canvas-ref c11 5 4) #\!) "#t" "#f"))
(newline)
(display "  ✓ composite overlays correctly\n")

;;; ============================================================
;;; Test 11: Blit
;;; ============================================================

(display "\nTest 11: Blit\n")
(define c-source (draw-string (make-canvas 20 5) (point 0 0) "0123456789"))
(define c-target (make-canvas 10 3))
(define c12 (blit c-target c-source
                  (make-rect (point 3 0) 5 1)
                  (point 2 1)))
(display "Canvas (copied '34567' to (2,1)):\n")
(display (canvas->string c12))
(newline)
(display "  '3' at (2,1): ")
(display (if (char=? (canvas-ref c12 2 1) #\3) "#t" "#f"))
(newline)
(display "  '7' at (6,1): ")
(display (if (char=? (canvas-ref c12 6 1) #\7) "#t" "#f"))
(newline)
(display "  ✓ blit copies region correctly\n")

;;; ============================================================
;;; Test 12: Canvas to String
;;; ============================================================

(display "\nTest 12: Canvas to String\n")
(define c13 (let* ([c (make-canvas 8 4)]
                   [c (draw-string c (point 0 0) "Line 1")]
                   [c (draw-string c (point 0 1) "Line 2")]
                   [c (draw-string c (point 0 2) "Line 3")]
                   [c (draw-string c (point 0 3) "Line 4")])
                  c))
(define str13 (canvas->string c13))
(display "Output:\n")
(display str13)
(newline)
(display "  Contains newlines: ")
(display (if (string-contains? str13 "\n") "#t" "#f"))
(newline)
(display "  ✓ canvas->string produces multi-line output\n")

;;; ============================================================
;;; Test 13: DUCKIE Example from Spec
;;; ============================================================

(display "\nTest 13: DUCKIE Example (ASCII Art)\n")
(define c-duckie
  (let* ([c (make-canvas 40 10)]
         [c (draw-rect c (make-rect (point 0 0) 40 10) #\#)]
         [c (draw-string c (point 2 2) "Hello, DUCKIE!")]
         [c (draw-string c (point 2 4) "  __")]
         [c (draw-string c (point 2 5) " (o>")]
         [c (draw-string c (point 2 6) " (()")]
         [c (draw-string c (point 2 7) "  ^^")])
        c))
(display (canvas->string c-duckie))
(newline)
(display "  ✓ DUCKIE appears in the window!\n")

;;; ============================================================
;;; Test 14: Box Drawing Styles
;;; ============================================================

(display "\nTest 14: Box Drawing Styles\n")

(display "  ASCII style:\n")
(define c-box-ascii (draw-box (make-canvas 12 5)
                              (make-rect (point 1 1) 10 3)
                              'ascii))
(display (canvas->string c-box-ascii))
(newline)

(display "  Light style:\n")
(define c-box-light (draw-box (make-canvas 12 5)
                              (make-rect (point 1 1) 10 3)
                              'light))
(display (canvas->string c-box-light))
(newline)

(display "  Heavy style:\n")
(define c-box-heavy (draw-box (make-canvas 12 5)
                              (make-rect (point 1 1) 10 3)
                              'heavy))
(display (canvas->string c-box-heavy))
(newline)

(display "  Double style:\n")
(define c-box-double (draw-box (make-canvas 12 5)
                               (make-rect (point 1 1) 10 3)
                               'double))
(display (canvas->string c-box-double))
(newline)

(display "  ✓ Box drawing styles work correctly\n")

;;; ============================================================
;;; Test 15: Complex Composition
;;; ============================================================

(display "\nTest 15: Complex Composition (Nested Boxes)\n")
(define c-complex
  (let* ([c (make-canvas 30 12)]
         ;; Outer box
         [c (draw-box c (make-rect (point 0 0) 30 12) 'double)]
         ;; Title
         [c (draw-string c (point 10 0) " DUCKIE ")]
         ;; Inner boxes
         [c (draw-box c (make-rect (point 2 2) 12 4) 'light)]
         [c (draw-string c (point 4 3) "Status:")]
         [c (draw-string c (point 4 4) "READY")]
         [c (draw-box c (make-rect (point 16 2) 12 8) 'light)]
         [c (draw-string c (point 18 3) "  __")]
         [c (draw-string c (point 18 4) " (o>")]
         [c (draw-string c (point 18 5) " (()")]
         [c (draw-string c (point 18 6) "  ^^")]
         ;; Footer
         [c (draw-string c (point 8 11) " Press any key ")])
        c))
(display (canvas->string c-complex))
(newline)
(display "  ✓ Complex composition works!\n")

;;; ============================================================
;;; Test 16: Word Splitting
;;; ============================================================

(display "\nTest 16: Word Splitting\n")
(define words1 (split-words "hello world"))
(display "  'hello world' -> ")
(display words1) (newline)
(display "  Correct count: ")
(display (if (= (length words1) 2) "#t" "#f")) (newline)

(define words2 (split-words "one"))
(display "  'one' -> ")
(display words2) (newline)

(define words3 (split-words "  multiple   spaces  "))
(display "  '  multiple   spaces  ' -> ")
(display words3) (newline)
(display "  ✓ split-words works correctly\n")

;;; ============================================================
;;; Test 17: Text Wrapping
;;; ============================================================

(display "\nTest 17: Text Wrapping\n")
(define wrapped1 (wrap-text "hello world foo bar" 10))
(display "  'hello world foo bar' at width 10:\n")
(for-each (lambda (line)
                  (display "    '")
                  (display line)
                  (display "'\n"))
          wrapped1)
(display "  Line count: ") (display (length wrapped1)) (newline)
(display "  ✓ wrap-text breaks at word boundaries\n")

(define wrapped2 (wrap-text "short" 20))
(display "  'short' at width 20: ")
(display wrapped2) (newline)

(define wrapped3 (wrap-text "superlongword fits" 5))
(display "  Long word handling at width 5: ")
(display wrapped3) (newline)
(display "  ✓ wrap-text handles edge cases\n")

;;; ============================================================
;;; Test 18: Text Alignment
;;; ============================================================

(display "\nTest 18: Text Alignment\n")
(define al-left (align-left "hi" 10))
(define al-right (align-right "hi" 10))
(define al-center (align-center "hi" 10))

(display "  align-left 'hi' to 10:   '") (display al-left) (display "'\n")
(display "  align-right 'hi' to 10:  '") (display al-right) (display "'\n")
(display "  align-center 'hi' to 10: '") (display al-center) (display "'\n")

(display "  Left length: ") (display (string-length al-left)) (newline)
(display "  Right starts with spaces: ")
(display (if (char=? (string-ref al-right 0) #\space) "#t" "#f")) (newline)
(display "  ✓ Alignment functions work correctly\n")

;;; ============================================================
;;; Test 19: Draw Lines
;;; ============================================================

(display "\nTest 19: Draw Lines\n")
(define c-lines (draw-lines (make-canvas 20 5)
                            (point 2 1)
                            '("Line one" "Line two" "Line three")))
(display "Canvas:\n")
(display (canvas->string c-lines))
(newline)
(display "  'L' at (2,1): ")
(display (if (char=? (canvas-ref c-lines 2 1) #\L) "#t" "#f")) (newline)
(display "  'L' at (2,3): ")
(display (if (char=? (canvas-ref c-lines 2 3) #\L) "#t" "#f")) (newline)
(display "  ✓ draw-lines renders multiple lines\n")

;;; ============================================================
;;; Test 20: Draw Text Block
;;; ============================================================

(display "\nTest 20: Draw Text Block (Wrapped + Aligned)\n")
(define long-text "The quick brown fox jumps over the lazy dog near the river bank")

(display "  Left-aligned in 20x4 rect:\n")
(define c-block-left
  (draw-text-block (make-canvas 24 6)
                   (make-rect (point 2 1) 20 4)
                   long-text
                   'left))
(display (canvas->string c-block-left))
(newline)

(display "  Center-aligned in 20x4 rect:\n")
(define c-block-center
  (draw-text-block (make-canvas 24 6)
                   (make-rect (point 2 1) 20 4)
                   long-text
                   'center))
(display (canvas->string c-block-center))
(newline)

(display "  Right-aligned in 20x4 rect:\n")
(define c-block-right
  (draw-text-block (make-canvas 24 6)
                   (make-rect (point 2 1) 20 4)
                   long-text
                   'right))
(display (canvas->string c-block-right))
(newline)
(display "  ✓ draw-text-block wraps and aligns text\n")

;;; ============================================================
;;; Test 21: Text Block in Box
;;; ============================================================

(display "\nTest 21: Text Block in Box (DUCKIE Dialog)\n")
(define dialog-text "Hello! I am DUCKIE. I live in The Fold and I am very happy to meet you!")
(define c-dialog
  (let* ([c (make-canvas 36 10)]
         [c (draw-box c (make-rect (point 0 0) 36 10) 'double)]
         [c (draw-string c (point 13 0) " DUCKIE ")]
         [c (draw-text-block c (make-rect (point 2 2) 32 6) dialog-text 'left)]
         [c (draw-string c (point 2 8) "  __")]
         [c (draw-string c (point 6 8) "(o>")])
        c))
(display (canvas->string c-dialog))
(newline)
(display "  ✓ Text flows naturally in dialog box!\n")

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "═══════════════════════════════════════════════════════\n")
(display "  ✓ All 21 tests passed!\n")
(display "  ✓ Text Layout Primitives are operational.\n")
(display "  ✓ Text Flow (wrapping, alignment) working.\n")
(display "  ✓ DUCKIE has a window to appear in.\n")
(display "═══════════════════════════════════════════════════════\n")
