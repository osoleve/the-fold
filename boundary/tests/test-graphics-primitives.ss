(doc 'note "Run from project root: scheme --script boundary/tests/test-graphics-primitives.ss")

(load "core/test-framework.ss")
(load "core/base/prelude.ss")
(load "boundary/ui/layout.ss")
(import (shell easing))
(import (shell graphics-primitives))

(doc 'module 'test-graphics-primitives)
(doc 'description "Tests for advanced graphics primitives: lines, circles, gradients, polygons")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "\n")
(display "====\n")
(display "         GRAPHICS PRIMITIVES TESTS\n")
(display "====\n")

(doc 'section 'intensity-palette-tests)

(test-group intensity-palettes
            (define-test palette-simple-exists
              (assert-true (list? intensity-palette-simple))
              (assert-equal 10 (length intensity-palette-simple)))
            
            (define-test palette-blocks-exists
              (assert-true (list? intensity-palette-blocks))
              (assert-equal 5 (length intensity-palette-blocks)))
            
            (define-test palette-dots-exists
              (assert-true (list? intensity-palette-dots))
              (assert-equal 6 (length intensity-palette-dots)))
            
            (define-test get-intensity-char-zero
              ;; Intensity 0 should return first character (darkest)
              (assert-equal #\space (get-intensity-char intensity-palette-simple 0.0)))
            
            (define-test get-intensity-char-one
              ;; Intensity 1 should return last character (brightest)
              (assert-equal #\@ (get-intensity-char intensity-palette-simple 1.0)))
            
            (define-test get-intensity-char-middle
              ;; Middle intensity should return middle character
              (let ([ch (get-intensity-char intensity-palette-simple 0.5)])
                   (assert-true (memv ch intensity-palette-simple))))
            
            (define-test get-intensity-char-clamps-negative
              ;; Negative values should clamp to 0
              (assert-equal #\space (get-intensity-char intensity-palette-simple -0.5)))
            
            (define-test get-intensity-char-clamps-over-one
              ;; Values > 1 should clamp to 1
              (assert-equal #\@ (get-intensity-char intensity-palette-simple 1.5))))

(doc 'section 'line-drawing-tests)

(test-group line-drawing
            (define-test draw-line-horizontal
              ;; Draw a horizontal line
              (let* ([c (make-canvas 10 5)]
                     [c (draw-line c (point 1 2) (point 8 2) #\-)])
                    ;; Check start
                    (assert-equal #\- (canvas-ref c 1 2))
                    ;; Check middle
                    (assert-equal #\- (canvas-ref c 4 2))
                    ;; Check end
                    (assert-equal #\- (canvas-ref c 8 2))
                    ;; Check above/below are untouched
                    (assert-equal #\space (canvas-ref c 4 1))
                    (assert-equal #\space (canvas-ref c 4 3))))
            
            (define-test draw-line-vertical
              ;; Draw a vertical line
              (let* ([c (make-canvas 10 10)]
                     [c (draw-line c (point 5 1) (point 5 8) #\|)])
                    (assert-equal #\| (canvas-ref c 5 1))
                    (assert-equal #\| (canvas-ref c 5 4))
                    (assert-equal #\| (canvas-ref c 5 8))
                    (assert-equal #\space (canvas-ref c 4 4))
                    (assert-equal #\space (canvas-ref c 6 4))))
            
            (define-test draw-line-diagonal
              ;; Draw a diagonal line
              (let* ([c (make-canvas 10 10)]
                     [c (draw-line c (point 0 0) (point 5 5) #\*)])
                    (assert-equal #\* (canvas-ref c 0 0))
                    (assert-equal #\* (canvas-ref c 5 5))))
            
            (define-test draw-line-gradient-basic
              ;; Draw a gradient line
              (let* ([c (make-canvas 10 5)]
                     [palette '(#\. #\: #\= #\#)]
                     [c (draw-line-gradient c (point 0 2) (point 9 2) palette)])
                    ;; Start should be dark
                    (assert-equal #\. (canvas-ref c 0 2))
                    ;; End should be brighter
                    (assert-true (memv (canvas-ref c 9 2) palette)))))

(doc 'section 'rounded-box-tests)

(test-group rounded-box
            (define-test draw-rounded-box-ascii
              ;; Draw a rounded box with ASCII style
              (let* ([c (make-canvas 12 6)]
                     [r (make-rect (point 1 1) 10 4)]
                     [c (draw-rounded-box c r 'ascii)])
                    ;; Check corners have rounded chars
                    (assert-equal #\/ (canvas-ref c 1 1))
                    (assert-equal #\\ (canvas-ref c 10 1))
                    (assert-equal #\\ (canvas-ref c 1 4))
                    (assert-equal #\/ (canvas-ref c 10 4))
                    ;; Check horizontal edges
                    (assert-equal #\- (canvas-ref c 5 1))
                    (assert-equal #\- (canvas-ref c 5 4))
                    ;; Check vertical edges
                    (assert-equal #\| (canvas-ref c 1 2))
                    (assert-equal #\| (canvas-ref c 10 2))))
            
            (define-test draw-rounded-box-light
              ;; Draw with Unicode light style
              (let* ([c (make-canvas 12 6)]
                     [r (make-rect (point 1 1) 10 4)]
                     [c (draw-rounded-box c r 'light)])
                    ;; Unicode rounded corners
                    (assert-true (char? (canvas-ref c 1 1)))))
            
            (define-test draw-rounded-box-too-small
              ;; Box too small for rounded corners falls back to draw-box
              (let* ([c (make-canvas 5 3)]
                     [r (make-rect (point 0 0) 2 2)]
                     [c (draw-rounded-box c r 'ascii)])
                    ;; Should still render something
                    (assert-true (char? (canvas-ref c 0 0))))))

(doc 'section 'circle-drawing-tests)

(test-group circle-drawing
            (define-test draw-circle-basic
              ;; Draw a circle outline
              (let* ([c (make-canvas 20 15)]
                     [center (point 10 7)]
                     [c (draw-circle c center 5 #\o)])
                    ;; Check that some points on circle are drawn
                    (assert-equal #\o (canvas-ref c 10 2))  ; Top
                    (assert-equal #\o (canvas-ref c 10 12)) ; Bottom
                    (assert-equal #\o (canvas-ref c 5 7))   ; Left
                    (assert-equal #\o (canvas-ref c 15 7))  ; Right
                    ;; Center should be empty
                    (assert-equal #\space (canvas-ref c 10 7))))
            
            (define-test draw-filled-circle-basic
              ;; Draw a filled circle
              (let* ([c (make-canvas 20 15)]
                     [center (point 10 7)]
                     [c (draw-filled-circle c center 3 #\*)])
                    ;; Center should be filled
                    (assert-equal #\* (canvas-ref c 10 7))
                    ;; Points within radius should be filled
                    (assert-equal #\* (canvas-ref c 9 7))
                    (assert-equal #\* (canvas-ref c 11 7))
                    (assert-equal #\* (canvas-ref c 10 6))
                    (assert-equal #\* (canvas-ref c 10 8))))
            
            (define-test draw-circle-zero-radius
              ;; Zero radius circle should draw just center point
              (let* ([c (make-canvas 5 5)]
                     [c (draw-circle c (point 2 2) 0 #\o)])
                    (assert-equal #\o (canvas-ref c 2 2)))))

(doc 'section 'gradient-fill-tests)

(test-group gradient-fills
            (define-test gradient-fill-horizontal-basic
              ;; Horizontal gradient should vary left to right
              (let* ([c (make-canvas 10 3)]
                     [r (make-rect (point 0 0) 10 3)]
                     [palette intensity-palette-simple]
                     [c (gradient-fill-horizontal c r 0.0 1.0 ease-linear palette)])
                    ;; Left side should be dark
                    (assert-equal #\space (canvas-ref c 0 1))
                    ;; Right side should be bright
                    (assert-equal #\@ (canvas-ref c 9 1))))
            
            (define-test gradient-fill-vertical-basic
              ;; Vertical gradient should vary top to bottom
              (let* ([c (make-canvas 5 10)]
                     [r (make-rect (point 0 0) 5 10)]
                     [palette intensity-palette-simple]
                     [c (gradient-fill-vertical c r 0.0 1.0 ease-linear palette)])
                    ;; Top should be dark
                    (assert-equal #\space (canvas-ref c 2 0))
                    ;; Bottom should be bright
                    (assert-equal #\@ (canvas-ref c 2 9))))
            
            (define-test gradient-fill-radial-basic
              ;; Radial gradient: center bright, edges dark
              (let* ([c (make-canvas 15 15)]
                     [center (point 7 7)]
                     [c (gradient-fill-radial c center 7 intensity-palette-simple ease-linear)])
                    ;; Center should be bright
                    (assert-equal #\@ (canvas-ref c 7 7))))
            
            (define-test gradient-fill-empty-rect
              ;; Empty rect should return unchanged canvas
              (let* ([c (make-canvas 5 5)]
                     [r (make-rect (point 0 0) 0 0)]
                     [c-result (gradient-fill-horizontal c r 0.0 1.0 ease-linear intensity-palette-simple)])
                    ;; Should not crash, return canvas unchanged
                    (assert-equal #\space (canvas-ref c-result 0 0)))))

(doc 'section 'polyline-polygon-tests)

(test-group polyline-polygon
            (define-test draw-polyline-triangle
              ;; Draw three connected line segments
              (let* ([c (make-canvas 15 12)]
                     [pts (list (point 1 1) (point 13 1) (point 7 10))]
                     [c (draw-polyline c pts #\*)])
                    ;; Check points along the lines
                    (assert-equal #\* (canvas-ref c 1 1))
                    (assert-equal #\* (canvas-ref c 13 1))
                    (assert-equal #\* (canvas-ref c 7 10))))
            
            (define-test draw-polygon-closes
              ;; Polygon should close back to start
              (let* ([c (make-canvas 10 10)]
                     [pts (list (point 1 1) (point 8 1) (point 8 8) (point 1 8))]
                     [c (draw-polygon c pts #\#)])
                    ;; All corners should be drawn
                    (assert-equal #\# (canvas-ref c 1 1))
                    (assert-equal #\# (canvas-ref c 8 1))
                    (assert-equal #\# (canvas-ref c 8 8))
                    (assert-equal #\# (canvas-ref c 1 8))))
            
            (define-test draw-polyline-single-point
              ;; Single point should return canvas unchanged
              (let* ([c (make-canvas 5 5)]
                     [c (draw-polyline c (list (point 2 2)) #\*)])
                    ;; Should not crash
                    (assert-equal #\space (canvas-ref c 2 2))))
            
            (define-test draw-polygon-too-few-points
              ;; Less than 3 points should return canvas unchanged
              (let* ([c (make-canvas 5 5)]
                     [c (draw-polygon c (list (point 1 1) (point 3 3)) #\*)])
                    (assert-equal #\space (canvas-ref c 1 1)))))

(doc 'section 'filled-polygon-tests)
(doc 'note "AET Algorithm")

(test-group filled-polygon
            (define-test draw-filled-polygon-square
              ;; Fill a square polygon
              (let* ([c (make-canvas 10 10)]
                     [pts (list (point 2 2) (point 7 2) (point 7 7) (point 2 7))]
                     [c (draw-filled-polygon c pts #\=)])
                    ;; Corners should be filled
                    (assert-equal #\= (canvas-ref c 2 2))
                    (assert-equal #\= (canvas-ref c 7 7))
                    ;; Interior should be filled
                    (assert-equal #\= (canvas-ref c 4 4))
                    (assert-equal #\= (canvas-ref c 5 5))
                    ;; Outside should be empty
                    (assert-equal #\space (canvas-ref c 0 0))
                    (assert-equal #\space (canvas-ref c 9 9))))
            
            (define-test draw-filled-polygon-triangle
              ;; Fill a triangle
              (let* ([c (make-canvas 12 10)]
                     [pts (list (point 6 1) (point 1 8) (point 11 8))]
                     [c (draw-filled-polygon c pts #\X)])
                    ;; Top point
                    (assert-equal #\X (canvas-ref c 6 1))
                    ;; Bottom corners
                    (assert-equal #\X (canvas-ref c 1 8))
                    (assert-equal #\X (canvas-ref c 11 8))
                    ;; Interior point
                    (assert-equal #\X (canvas-ref c 6 5)))))

(doc 'section 'style-system-tests)

(test-group style-system
            (define-test make-style-basic
              ;; Create a style and check fields
              (let ([s (make-style #\# #\* 1.0)])
                   (assert-equal #\# (style-fill s))
                   (assert-equal #\* (style-stroke s))
                   (assert-equal 1.0 (style-opacity s))))
            
            (define-test apply-opacity-block-zero
              ;; Opacity 0 should return space
              (assert-equal #\space (apply-opacity-block 0.0)))
            
            (define-test apply-opacity-block-one
              ;; Opacity 1 should return solid block
              (assert-equal (string-ref "\u2588" 0) (apply-opacity-block 1.0)))
            
            (define-test apply-opacity-block-middle
              ;; Middle opacity should return medium shade
              (let ([ch (apply-opacity-block 0.5)])
                   (assert-true (char? ch)))))

(doc 'section 'path-drawing-tests)

(test-group path-drawing
            (define-test draw-path-move-and-line
              ;; Path with move-to and line-to commands
              (let* ([c (make-canvas 10 10)]
                     [cmds (list '(move-to (point 1 1))
                                 '(line-to (point 8 1))
                                 '(line-to (point 8 8)))]
                     [c (draw-path c
                                   (list (list 'move-to (point 1 1))
                                         (list 'line-to (point 8 1))
                                         (list 'line-to (point 8 8)))
                                   #\*)])
                    ;; Path should connect the points
                    (assert-equal #\* (canvas-ref c 1 1))
                    (assert-equal #\* (canvas-ref c 8 1))
                    (assert-equal #\* (canvas-ref c 8 8))))
            
            (define-test draw-path-close
              ;; Path with close command
              (let* ([c (make-canvas 10 10)]
                     [c (draw-path c
                                   (list (list 'move-to (point 1 1))
                                         (list 'line-to (point 5 1))
                                         (list 'line-to (point 5 5))
                                         '(close))
                                   #\#)])
                    ;; Should close back to start
                    (assert-equal #\# (canvas-ref c 1 1))
                    (assert-equal #\# (canvas-ref c 5 5)))))

(doc 'section 'draw-with-style-tests)

(test-group draw-with-style
            (define-test draw-circle-with-style
              ;; Draw a circle using style
              (let* ([c (make-canvas 15 15)]
                     [style (make-style #\O #\* 1.0)]
                     [shape (list 'circle (point 7 7) 3)]
                     [c (draw-with-style c shape style)])
                    ;; Center should have fill char
                    (assert-equal #\O (canvas-ref c 7 7))))
            
            (define-test draw-rect-with-style
              ;; Draw a rect using style
              (let* ([c (make-canvas 12 8)]
                     [style (make-style #\. #\# 1.0)]
                     [shape (list 'rect (point 2 2) 8 4)]
                     [c (draw-with-style c shape style)])
                    ;; Interior should have fill
                    (assert-equal #\. (canvas-ref c 5 3)))))

(doc 'section 'summary)

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All graphics primitives tests passed.\n")
    (display "\n[FAILURE] Some graphics primitives tests failed.\n"))
