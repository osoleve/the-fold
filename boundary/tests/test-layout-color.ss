(doc 'note "Run from project root: scheme --script boundary/tests/test-layout-color.ss")

(load "core/test-framework.ss")
(load "core/base/prelude.ss")
(load "boundary/ui/color.ss")
(load "boundary/ui/layout-color.ss")

(doc 'module 'test-layout-color)
(doc 'description "Tests for the color-enabled canvas layout system")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "\n")
(display "====\n")
(display "         COLOR LAYOUT SYSTEM TESTS\n")
(display "====\n")

(doc 'section 'point-and-rect-tests)

(test-group point-and-rect
            (define-test point-construction
              (let ([p (point 5 10)])
                   (assert-equal 5 (point-x p))
                   (assert-equal 10 (point-y p))))
            
            (define-test rect-construction
              (let ([r (make-rect (point 2 3) 10 5)])
                   (assert-equal 2 (point-x (rect-origin r)))
                   (assert-equal 3 (point-y (rect-origin r)))
                   (assert-equal 10 (rect-width r))
                   (assert-equal 5 (rect-height r)))))

(doc 'section 'canvas-construction-tests)

(test-group canvas-construction
            (define-test make-canvas-basic
              (let ([c (make-canvas 20 10)])
                   (assert-equal 20 (canvas-width c))
                   (assert-equal 10 (canvas-height c))))
            
            (define-test canvas-ref-default
              ;; New canvas cells have default cell values
              (let* ([c (make-canvas 5 5)]
                     [cell (canvas-ref c 2 2)])
                    (assert-true (cell%? cell))
                    (assert-equal #\space (cell%-char cell))))
            
            (define-test canvas-ref-out-of-bounds
              ;; Out of bounds returns default cell
              (let* ([c (make-canvas 5 5)]
                     [cell (canvas-ref c 100 100)])
                    (assert-true (cell%? cell))
                    (assert-equal #\space (cell%-char cell))))
            
            (define-test canvas-ref-char-basic
              ;; canvas-ref-char extracts just the character
              (let* ([c (make-canvas 5 5)]
                     [ch (canvas-ref-char c 2 2)])
                    (assert-equal #\space ch))))

(doc 'section 'canvas-set-tests)

(test-group canvas-set
            (define-test canvas-set-basic
              ;; Set character with default colors
              (let* ([c (make-canvas 5 5)]
                     [c (canvas-set c 2 2 #\X)])
                    (assert-equal #\X (canvas-ref-char c 2 2))))
            
            (define-test canvas-set-out-of-bounds
              ;; Out of bounds set returns unchanged canvas
              (let* ([c (make-canvas 5 5)]
                     [c2 (canvas-set c -1 -1 #\X)])
                    (assert-equal #\space (canvas-ref-char c2 0 0))))
            
            (define-test canvas-set-cell-basic
              ;; Set cell with colors
              (let* ([c (make-canvas 5 5)]
                     [cell (make-cell #\Y color-red color-blue)]
                     [c (canvas-set-cell c 1 1 cell)]
                     [result (canvas-ref c 1 1)])
                    (assert-equal #\Y (cell%-char result))
                    (assert-equal color-red (cell%-fg result))
                    (assert-equal color-blue (cell%-bg result)))))

(doc 'section 'drawing-primitives-tests)

(test-group drawing-primitives
            (define-test draw-char-basic
              (let* ([c (make-canvas 10 5)]
                     [c (draw-char c (point 3 2) #\@)])
                    (assert-equal #\@ (canvas-ref-char c 3 2))))
            
            (define-test draw-char-colored-basic
              (let* ([c (make-canvas 10 5)]
                     [c (draw-char-colored c (point 3 2) #\@ color-green color-black)]
                     [cell (canvas-ref c 3 2)])
                    (assert-equal #\@ (cell%-char cell))
                    (assert-equal color-green (cell%-fg cell))
                    (assert-equal color-black (cell%-bg cell))))
            
            (define-test draw-string-basic
              (let* ([c (make-canvas 20 5)]
                     [c (draw-string c (point 2 1) "Hello")])
                    (assert-equal #\H (canvas-ref-char c 2 1))
                    (assert-equal #\e (canvas-ref-char c 3 1))
                    (assert-equal #\l (canvas-ref-char c 4 1))
                    (assert-equal #\l (canvas-ref-char c 5 1))
                    (assert-equal #\o (canvas-ref-char c 6 1))))
            
            (define-test draw-string-colored-basic
              (let* ([c (make-canvas 20 5)]
                     [c (draw-string-colored c (point 2 1) "Hi" color-yellow color-default)]
                     [cell (canvas-ref c 2 1)])
                    (assert-equal #\H (cell%-char cell))
                    (assert-equal color-yellow (cell%-fg cell))))
            
            (define-test draw-string-clips-at-boundary
              ;; String clips at canvas edge
              (let* ([c (make-canvas 10 5)]
                     [c (draw-string c (point 7 2) "LongText")])
                    ;; Only "Lon" fits
                    (assert-equal #\L (canvas-ref-char c 7 2))
                    (assert-equal #\o (canvas-ref-char c 8 2))
                    (assert-equal #\n (canvas-ref-char c 9 2))))
            
            (define-test draw-string-v-basic
              ;; Vertical string drawing
              (let* ([c (make-canvas 10 10)]
                     [c (draw-string-v c (point 3 1) "ABC")])
                    (assert-equal #\A (canvas-ref-char c 3 1))
                    (assert-equal #\B (canvas-ref-char c 3 2))
                    (assert-equal #\C (canvas-ref-char c 3 3)))))

(doc 'section 'rectangle-drawing-tests)

(test-group rectangle-drawing
            (define-test draw-rect-basic
              ;; Draw rectangle outline
              (let* ([c (make-canvas 12 8)]
                     [r (make-rect (point 2 1) 8 5)]
                     [c (draw-rect c r #\#)])
                    ;; Top edge
                    (assert-equal #\# (canvas-ref-char c 2 1))
                    (assert-equal #\# (canvas-ref-char c 5 1))
                    (assert-equal #\# (canvas-ref-char c 9 1))
                    ;; Bottom edge
                    (assert-equal #\# (canvas-ref-char c 2 5))
                    ;; Left edge
                    (assert-equal #\# (canvas-ref-char c 2 3))
                    ;; Right edge
                    (assert-equal #\# (canvas-ref-char c 9 3))
                    ;; Interior should be empty
                    (assert-equal #\space (canvas-ref-char c 5 3))))
            
            (define-test fill-rect-basic
              ;; Fill rectangle
              (let* ([c (make-canvas 12 8)]
                     [r (make-rect (point 2 1) 5 3)]
                     [c (fill-rect c r #\=)])
                    ;; All interior cells should be filled
                    (assert-equal #\= (canvas-ref-char c 2 1))
                    (assert-equal #\= (canvas-ref-char c 4 2))
                    (assert-equal #\= (canvas-ref-char c 6 3))))
            
            (define-test fill-rect-colored-basic
              ;; Fill rectangle with colors
              (let* ([c (make-canvas 12 8)]
                     [r (make-rect (point 1 1) 4 3)]
                     [c (fill-rect-colored c r #\. color-blue color-cyan)]
                     [cell (canvas-ref c 2 2)])
                    (assert-equal #\. (cell%-char cell))
                    (assert-equal color-blue (cell%-fg cell))
                    (assert-equal color-cyan (cell%-bg cell))))
            
            (define-test fill-rect-empty
              ;; Empty rect should be no-op
              (let* ([c (make-canvas 5 5)]
                     [r (make-rect (point 0 0) 0 0)]
                     [c (fill-rect c r #\X)])
                    (assert-equal #\space (canvas-ref-char c 0 0)))))

(doc 'section 'composition-tests)

(test-group composition
            (define-test composite-basic
              ;; Composite source onto destination
              (let* ([dest (make-canvas 15 10)]
                     [src (make-canvas 5 3)]
                     [src (draw-string src (point 0 0) "SRC")]
                     [result (composite dest src (point 3 2))])
                    (assert-equal #\S (canvas-ref-char result 3 2))
                    (assert-equal #\R (canvas-ref-char result 4 2))
                    (assert-equal #\C (canvas-ref-char result 5 2))))
            
            (define-test composite-with-transparency-basic
              ;; Transparent chars don't overwrite
              (let* ([dest (make-canvas 10 5)]
                     [dest (draw-string dest (point 0 0) "DEST")]
                     [src (make-canvas 10 5)]
                     [src (canvas-set src 0 0 #\space)]  ; transparent
                     [src (canvas-set src 1 0 #\X)]
                     [src (canvas-set src 2 0 #\space)]  ; transparent
                     [result (composite-with-transparency dest src (point 0 0) #\space)])
                    ;; D should be preserved (src has space)
                    (assert-equal #\D (canvas-ref-char result 0 0))
                    ;; E should be replaced by X
                    (assert-equal #\X (canvas-ref-char result 1 0))
                    ;; S should be preserved
                    (assert-equal #\S (canvas-ref-char result 2 0))))
            
            (define-test blit-basic
              ;; Copy region from source to destination
              (let* ([src (make-canvas 20 10)]
                     [src (draw-string src (point 5 3) "REGION")]
                     [dest (make-canvas 10 5)]
                     [region (make-rect (point 5 3) 6 1)]
                     [result (blit dest src region (point 2 1))])
                    ;; "REGION" should appear at (2,1)
                    (assert-equal #\R (canvas-ref-char result 2 1))
                    (assert-equal #\E (canvas-ref-char result 3 1))
                    (assert-equal #\G (canvas-ref-char result 4 1)))))

(doc 'section 'box-drawing-tests)

(test-group box-drawing
            (define-test draw-box-ascii
              ;; ASCII style box
              (let* ([c (make-canvas 12 6)]
                     [r (make-rect (point 1 1) 10 4)]
                     [c (draw-box c r 'ascii)])
                    ;; Corners
                    (assert-equal #\+ (canvas-ref-char c 1 1))
                    (assert-equal #\+ (canvas-ref-char c 10 1))
                    (assert-equal #\+ (canvas-ref-char c 1 4))
                    (assert-equal #\+ (canvas-ref-char c 10 4))
                    ;; Horizontal edges
                    (assert-equal #\- (canvas-ref-char c 5 1))
                    ;; Vertical edges
                    (assert-equal #\| (canvas-ref-char c 1 2))))
            
            (define-test draw-box-light
              ;; Unicode light style
              (let* ([c (make-canvas 12 6)]
                     [r (make-rect (point 1 1) 10 4)]
                     [c (draw-box c r 'light)])
                    ;; Check it rendered something
                    (assert-true (char? (canvas-ref-char c 1 1)))
                    (assert-true (char? (canvas-ref-char c 5 1)))))
            
            (define-test draw-box-colored-basic
              ;; Colored box
              (let* ([c (make-canvas 12 6)]
                     [r (make-rect (point 1 1) 10 4)]
                     [c (draw-box-colored c r 'ascii color-green color-default)]
                     [cell (canvas-ref c 1 1)])
                    (assert-equal #\+ (cell%-char cell))
                    (assert-equal color-green (cell%-fg cell))))
            
            (define-test draw-box-too-small
              ;; Box too small returns unchanged
              (let* ([c (make-canvas 5 5)]
                     [r (make-rect (point 0 0) 1 1)]
                     [c (draw-box c r 'ascii)])
                    ;; No crash, just unchanged
                    (assert-true (canvas%? c)))))

(doc 'section 'canvas-to-string-tests)

(test-group canvas-to-string
            (define-test canvas-to-string-basic
              ;; Convert canvas to string with ANSI codes
              (let* ([c (make-canvas 5 2)]
                     [c (draw-string c (point 0 0) "Hi")]
                     [str (canvas->string c)])
                    (assert-true (string? str))
                    ;; Should contain "Hi" somewhere
                    (assert-true (string-contains? str "H"))
                    (assert-true (string-contains? str "i"))))
            
            (define-test canvas-to-string-includes-reset
              ;; Output should include ANSI reset
              (let* ([c (make-canvas 3 1)]
                     [str (canvas->string c)])
                    ;; Should end with reset code
                    (assert-true (string-contains? str ansi-reset)))))

(doc 'section 'summary)

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All layout-color tests passed.\n")
    (display "\n[FAILURE] Some layout-color tests failed.\n"))
