;;; shell/tests/test-turtle-svg.ss — Tests for turtle-svg.ss
;;;
;;; Tests for SVG generation from turtle graphics drawings.
;;;
;;; NOTE: Run from project root: scheme --script shell/tests/test-turtle-svg.ss

(load "core/test-framework.ss")
(load "core/base/prelude.ss")
(load "shell/ui/turtle-color.ss")
(load "shell/ui/turtle-path.ss")
(load "shell/ui/turtle.ss")
(load "shell/ui/turtle-svg.ss")

(display "\n")
(display "====\n")
(display "         TURTLE SVG GENERATION TESTS\n")
(display "====\n")

;;; ====
;;; SVG Header/Footer Tests
;;; ====

(test-group svg-structure
            (define-test svg-header-basic
              ;; Header should include width, height, viewBox
              (let ([header (svg-header 640 480)])
                   (assert-true (string? header))
                   (assert-true (string-contains? header "xml version"))
                   (assert-true (string-contains? header "svg xmlns"))
                   (assert-true (string-contains? header "640"))
                   (assert-true (string-contains? header "480"))
                   (assert-true (string-contains? header "viewBox"))))
            
            (define-test svg-header-custom-size
              ;; Custom dimensions
              (let ([header (svg-header 1024 768)])
                   (assert-true (string-contains? header "1024"))
                   (assert-true (string-contains? header "768"))))
            
            (define-test svg-footer-basic
              (let ([footer (svg-footer)])
                   (assert-true (string? footer))
                   (assert-true (string-contains? footer "</svg>"))))
            
            (define-test svg-background-basic
              (let ([bg (svg-background 640 480 "#FFFFFF")])
                   (assert-true (string? bg))
                   (assert-true (string-contains? bg "<rect"))
                   (assert-true (string-contains? bg "640"))
                   (assert-true (string-contains? bg "480"))
                   (assert-true (string-contains? bg "#FFFFFF")))))

;;; ====
;;; Number Formatting Tests
;;; ====

(test-group number-formatting
            (define-test format-number-integer
              ;; Integer should have no decimals
              (assert-equal "42" (format-number 42.0))
              (assert-equal "0" (format-number 0.0))
              (assert-equal "100" (format-number 100.0)))
            
            (define-test format-number-decimal
              ;; Decimal should be trimmed
              (let ([result (format-number 3.14)])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "3"))))
            
            (define-test format-number-small-decimal
              ;; Small decimal values
              (let ([result (format-number 0.5)])
                   (assert-true (string? result)))))

;;; ====
;;; Coordinate Formatting Tests
;;; ====

(test-group coord-formatting
            (define-test format-coord-move
              (let ([result (format-coord "M" 100.0 200.0)])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "M"))
                   (assert-true (string-contains? result "100"))
                   (assert-true (string-contains? result "200"))))
            
            (define-test format-coord-line
              (let ([result (format-coord "L" 50.5 75.25)])
                   (assert-true (string-contains? result "L")))))

;;; ====
;;; Path Command Recognition Tests
;;; ====

(test-group path-commands
            (define-test move-to-recognized
              (let ([cmd (make-move-to 100 200)])
                   (assert-true (move-to? cmd))
                   (assert-equal 100 (move-to-x cmd))
                   (assert-equal 200 (move-to-y cmd))))
            
            (define-test line-to-recognized
              (let ([cmd (make-line-to 150 250 color12-black 2)])
                   (assert-true (line-to? cmd))
                   (assert-equal 150 (line-to-x cmd))
                   (assert-equal 250 (line-to-y cmd))
                   (assert-equal 2 (line-to-width cmd))))
            
            (define-test circle-recognized
              (let ([cmd (make-circle 100 100 50 color12-red 1 #f)])
                   (assert-true (circle? cmd))
                   (assert-equal 100 (circle-cx cmd))
                   (assert-equal 100 (circle-cy cmd))
                   (assert-equal 50 (circle-radius cmd))
                   (assert-false (circle-fill? cmd))))
            
            (define-test polygon-recognized
              (let ([cmd (make-polygon '((0 . 0) (100 . 0) (50 . 100)) color12-blue 2 #t)])
                   (assert-true (polygon? cmd))
                   (assert-equal 3 (length (polygon-points cmd)))
                   (assert-true (polygon-fill? cmd))))
            
            (define-test arc-recognized
              (let ([cmd (make-arc 100 100 50 0 90 color12-green 1)])
                   (assert-true (arc? cmd))
                   (assert-equal 100 (arc-cx cmd))
                   (assert-equal 50 (arc-radius cmd))
                   (assert-equal 0 (arc-start-angle cmd))
                   (assert-equal 90 (arc-end-angle cmd)))))

;;; ====
;;; Circle to SVG Tests
;;; ====

(test-group circle-svg
            (define-test circle-svg-basic
              (let* ([cmd (make-circle 320 240 100 color12-red 2 #f)]
                     [svg (circle->svg cmd)])
                    (assert-true (string? svg))
                    (assert-true (string-contains? svg "<circle"))
                    (assert-true (string-contains? svg "cx="))
                    (assert-true (string-contains? svg "cy="))
                    (assert-true (string-contains? svg "r="))
                    (assert-true (string-contains? svg "stroke="))))
            
            (define-test circle-svg-filled
              (let* ([cmd (make-circle 100 100 50 color12-blue 1 #t)]
                     [svg (circle->svg cmd)])
                    ;; Fill should have color, not "none"
                    (assert-true (string-contains? svg "fill="))
                    (assert-false (string-contains? svg "fill=\"none\"")))))

;;; ====
;;; Polygon to SVG Tests
;;; ====

(test-group polygon-svg
            (define-test polygon-svg-basic
              (let* ([pts '((0 . 0) (100 . 0) (50 . 100))]
                     [cmd (make-polygon pts color12-green 2 #f)]
                     [svg (polygon->svg cmd)])
                    (assert-true (string? svg))
                    (assert-true (string-contains? svg "<polygon"))
                    (assert-true (string-contains? svg "points="))
                    (assert-true (string-contains? svg "stroke-linejoin"))))
            
            (define-test points-to-svg-string-basic
              (let* ([pts '((10 . 20) (30 . 40) (50 . 60))]
                     [result (points->svg-string pts)])
                    (assert-true (string? result))
                    (assert-true (string-contains? result "10"))
                    (assert-true (string-contains? result "20")))))

;;; ====
;;; Arc to SVG Tests
;;; ====

(test-group arc-svg
            (define-test arc-svg-basic
              (let* ([cmd (make-arc 200 200 100 0 90 color12-black 2)]
                     [svg (arc->svg cmd)])
                    (assert-true (string? svg))
                    (assert-true (string-contains? svg "<path"))
                    (assert-true (string-contains? svg "d="))
                    (assert-true (string-contains? svg "A"))  ; Arc command
                    (assert-true (string-contains? svg "fill=\"none\""))))
            
            (define-test arc-svg-large-arc
              ;; Arc > 180 degrees
              (let* ([cmd (make-arc 200 200 100 0 270 color12-red 1)]
                     [svg (arc->svg cmd)])
                    (assert-true (string-contains? svg "1"))  ; large-arc flag
                    )))

;;; ====
;;; Path Segment Tests
;;; ====

(test-group path-segments
            (define-test segment-to-path-d-basic
              (let* ([cmds (list (make-move-to 0 0)
                                 (make-line-to 100 0 color12-black 1)
                                 (make-line-to 100 100 color12-black 1))]
                     [d (segment->path-d cmds)])
                    (assert-true (string? d))
                    (assert-true (string-contains? d "M"))
                    (assert-true (string-contains? d "L"))))
            
            (define-test flush-segment-empty
              ;; Empty segment returns empty string
              (let ([result (flush-segment '() #f #f)])
                   (assert-equal "" result)))
            
            (define-test flush-segment-with-content
              (let* ([cmds (list (make-move-to 0 0)
                                 (make-line-to 50 50 color12-red 2))]
                     [result (flush-segment cmds color12-red 2)])
                    (assert-true (string? result))
                    (assert-true (string-contains? result "<path"))
                    (assert-true (string-contains? result "stroke="))
                    (assert-true (string-contains? result "stroke-width=")))))

;;; ====
;;; Paths to SVG Tests
;;; ====

(test-group paths-to-svg
            (define-test paths-to-svg-empty
              (assert-equal "" (paths->svg '())))
            
            (define-test paths-to-svg-simple-line
              (let* ([cmds (list (make-move-to 10 10)
                                 (make-line-to 100 100 color12-black 1))]
                     [svg (paths->svg cmds)])
                    (assert-true (string? svg))
                    (assert-true (string-contains? svg "<path"))))
            
            (define-test paths-to-svg-with-circle
              (let* ([cmds (list (make-move-to 0 0)
                                 (make-line-to 50 50 color12-black 1)
                                 (make-circle 100 100 25 color12-red 2 #f))]
                     [svg (paths->svg cmds)])
                    (assert-true (string-contains? svg "<path"))
                    (assert-true (string-contains? svg "<circle")))))

;;; ====
;;; Drawing to SVG Tests
;;; ====

(test-group drawing-to-svg
            (define-test drawing-to-svg-empty
              ;; Empty drawing produces valid SVG
              (let* ([t (make-turtle)]
                     [d (turtle->drawing t)]
                     [svg (drawing->svg d)])
                    (assert-true (string? svg))
                    (assert-true (string-contains? svg "<?xml"))
                    (assert-true (string-contains? svg "<svg"))
                    (assert-true (string-contains? svg "</svg>"))))
            
            (define-test drawing-to-svg-with-line
              ;; Drawing with a line
              (let* ([t (make-turtle)]
                     [t (forward t 100)]
                     [d (turtle->drawing t)]
                     [svg (drawing->svg d)])
                    (assert-true (string-contains? svg "<svg"))
                    ;; Should have some path content
                    (assert-true (> (string-length svg) 200))))
            
            (define-test turtle-to-svg-convenience
              ;; turtle->svg convenience wrapper
              (let* ([t (make-turtle)]
                     [svg (turtle->svg t)])
                    (assert-true (string? svg))
                    (assert-true (string-contains? svg "<svg")))))

;;; ====
;;; Utility Tests
;;; ====

(test-group utilities
            (define-test exists-predicate
              ;; exists should find matching element
              (assert-true (exists even? '(1 3 5 6 7)))
              (assert-false (exists even? '(1 3 5 7 9))))
            
            (define-test deg-to-rad-basic
              ;; 180 degrees = pi
              (let ([result (deg->rad 180)])
                   (assert-true (< (abs (- result pi)) 0.001)))
              ;; 90 degrees = pi/2
              (let ([result (deg->rad 90)])
                   (assert-true (< (abs (- result (/ pi 2))) 0.001)))))

;;; ====
;;; Color Conversion Tests
;;; ====

(test-group color-conversion
            (define-test color12-to-svg-hex-black
              (let ([hex (color12->svg-hex color12-black)])
                   (assert-equal "#000000" hex)))
            
            (define-test color12-to-svg-hex-white
              (let ([hex (color12->svg-hex color12-white)])
                   (assert-equal "#FFFFFF" hex)))
            
            (define-test color12-to-svg-hex-red
              (let ([hex (color12->svg-hex color12-red)])
                   (assert-true (string? hex))
                   (assert-true (string-contains? hex "#"))
                   (assert-equal 7 (string-length hex)))))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All turtle-svg tests passed.\n")
    (display "\n[FAILURE] Some turtle-svg tests failed.\n"))
