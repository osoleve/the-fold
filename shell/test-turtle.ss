;;; shell/test-turtle.ss — Turtle Graphics Test Suite
;;;
;;; Comprehensive tests for the turtle graphics system.
;;;
;;; Run with: scheme --script test-turtle.ss
;;;
;;; Dependencies: All turtle modules

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

;;; Helper function for string tests
(define (string-contains? str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
    (let loop ([i 0])
      (cond
        [(> (+ i sub-len) str-len) #f]
        [(string=? (substring str i (+ i sub-len)) substr) #t]
        [else (loop (+ i 1))]))))

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS")
        (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL")
        (newline)
        (display "    expected: ")
        (write expected)
        (newline)
        (display "    actual:   ")
        (write actual)
        (newline))))

(define (test-approx name expected actual epsilon)
  (set! *test-count* (+ *test-count* 1))
  (display "  ")
  (display name)
  (display ": ")
  (if (< (abs (- expected actual)) epsilon)
      (begin
        (set! *pass-count* (+ *pass-count* 1))
        (display "PASS")
        (newline))
      (begin
        (set! *fail-count* (+ *fail-count* 1))
        (display "FAIL")
        (newline)
        (display "    expected: ")
        (write expected)
        (newline)
        (display "    actual:   ")
        (write actual)
        (newline))))

(define (test-section name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

(define (test-summary)
  (newline)
  (display "----------------------------")
  (newline)
  (display "Total: ")
  (display *test-count*)
  (display " | Pass: ")
  (display *pass-count*)
  (display " | Fail: ")
  (display *fail-count*)
  (newline)
  (if (= *fail-count* 0)
      (display "All tests passed!\n")
      (display "Some tests failed.\n")))

;;; ============================================================
;;; Load Turtle Modules
;;; ============================================================

(display "Loading turtle modules...\n")
(load "turtle-color.ss")
(load "turtle-path.ss")
(load "turtle.ss")
(load "turtle-svg.ss")
(display "Modules loaded.\n")

;;; ============================================================
;;; Color Tests
;;; ============================================================

(test-section "Color12 Construction")

(let ([c (make-color12 15 8 0)])
  (test "color12-r" 15 (color12-r c))
  (test "color12-g" 8 (color12-g c))
  (test "color12-b" 0 (color12-b c))
  (test "color12?" #t (color12? c)))

(test "color12? false for list" #f (color12? '(1 2 3)))
(test "color12? false for string" #f (color12? "red"))

(test-section "Color12 Clamping")

(let ([c (make-color12 20 -5 8)])
  (test "clamp high" 15 (color12-r c))
  (test "clamp low" 0 (color12-g c))
  (test "normal" 8 (color12-b c)))

(test-section "Color12 Integer Conversion")

(let ([c (color12-from-int #xF80)])
  (test "from-int r" 15 (color12-r c))
  (test "from-int g" 8 (color12-g c))
  (test "from-int b" 0 (color12-b c)))

(test "to-int" #xF80 (color12->int (make-color12 15 8 0)))
(test "round-trip" #xABC (color12->int (color12-from-int #xABC)))

(test-section "Color12 Named Colors")

(let ([red (color12-from-name 'red)])
  (test "named red exists" #t (color12? red))
  (test "named red r" 15 (color12-r red))
  (test "named red g" 0 (color12-g red))
  (test "named red b" 0 (color12-b red)))

(test "invalid name" #f (color12-from-name 'notacolor))

(test-section "Color12 SVG Hex")

(test "black hex" "#000000" (color12->svg-hex (make-color12 0 0 0)))
(test "white hex" "#FFFFFF" (color12->svg-hex (make-color12 15 15 15)))
(test "red hex" "#FF0000" (color12->svg-hex (make-color12 15 0 0)))
(test "mid gray" "#888888" (color12->svg-hex (make-color12 8 8 8)))

(test-section "Color12 Parse")

(let ([c1 (parse-color12 '(15 0 0))]
      [c2 (parse-color12 #xF00)]
      [c3 (parse-color12 'red)]
      [c4 (parse-color12 (make-color12 15 0 0))])
  (test "parse triplet" #t (color12? c1))
  (test "parse int" #t (color12? c2))
  (test "parse name" #t (color12? c3))
  (test "parse passthrough" #t (color12? c4))
  (test "all equal r" 15 (color12-r c1))
  (test "all equal g" 0 (color12-g c1))
  (test "all equal b" 0 (color12-b c1)))

;;; ============================================================
;;; Path Command Tests
;;; ============================================================

(test-section "Path Commands")

(let ([m (make-move-to 100 200)])
  (test "move-to?" #t (move-to? m))
  (test "move-to-x" 100 (move-to-x m))
  (test "move-to-y" 200 (move-to-y m)))

(let ([l (make-line-to 50 75 color12-red 2)])
  (test "line-to?" #t (line-to? l))
  (test "line-to-x" 50 (line-to-x l))
  (test "line-to-y" 75 (line-to-y l))
  (test "line-to-width" 2 (line-to-width l)))

(let ([c (make-circle 100 200 50 color12-blue 1 #t)])
  (test "circle?" #t (circle? c))
  (test "circle-cx" 100 (circle-cx c))
  (test "circle-radius" 50 (circle-radius c))
  (test "circle-fill?" #t (circle-fill? c)))

;;; ============================================================
;;; Turtle State Tests
;;; ============================================================

(test-section "Turtle Default State")

(let ([t (make-turtle)])
  (test "default x" 320.0 (turtle-x t))
  (test "default y" 240.0 (turtle-y t))
  (test "default heading" 0.0 (turtle-heading t))
  (test "default pen-down" #t (turtle-pen-down? t))
  (test "default visible" #t (turtle-visible? t))
  (test "default paths" '() (turtle-paths t)))

(test-section "Turtle Movement - Forward")

(let* ([t (make-turtle)]
       [t2 (fd t 100)])
  ;; Heading 0 = north, so y decreases
  (test-approx "fd north x" 320.0 (turtle-x t2) 0.001)
  (test-approx "fd north y" 140.0 (turtle-y t2) 0.001)
  (test "fd creates path" 2 (length (turtle-paths t2))))  ; move-to + line-to

(let* ([t (rt (make-turtle) 90)]  ; Face east
       [t2 (fd t 100)])
  (test-approx "fd east x" 420.0 (turtle-x t2) 0.001)
  (test-approx "fd east y" 240.0 (turtle-y t2) 0.001))

(test-section "Turtle Movement - Back")

(let* ([t (make-turtle)]
       [t2 (bk t 50)])
  ;; Back from north = south, y increases
  (test-approx "bk y" 290.0 (turtle-y t2) 0.001))

(test-section "Turtle Rotation")

(let* ([t (make-turtle)]
       [t2 (rt t 90)])
  (test-approx "right 90" 90.0 (turtle-heading t2) 0.001))

(let* ([t (make-turtle)]
       [t2 (lt t 90)])
  (test-approx "left 90" 270.0 (turtle-heading t2) 0.001))

(let* ([t (make-turtle)]
       [t2 (rt t 450)])  ; More than 360
  (test-approx "wrap heading" 90.0 (turtle-heading t2) 0.001))

(test-section "Turtle Pen")

(let* ([t (make-turtle)]
       [t2 (pu t)])
  (test "pen up" #f (turtle-pen-down? t2)))

(let* ([t (pu (make-turtle))]
       [t2 (pd t)])
  (test "pen down" #t (turtle-pen-down? t2)))

(let* ([t (pu (make-turtle))]
       [t2 (fd t 100)]
       [paths (turtle-paths t2)])
  (test "pen up creates move-to" #t (move-to? (car paths))))

(let* ([t (make-turtle)]
       [t2 (fd t 100)]
       [paths (turtle-paths t2)])
  (test "pen down creates line-to" #t (line-to? (car paths))))

(test-section "Turtle Home and Clear")

(let* ([t (fd (fd (make-turtle) 100) 50)]
       [t2 (home t)])
  (test-approx "home x" 320.0 (turtle-x t2) 0.001)
  (test-approx "home y" 240.0 (turtle-y t2) 0.001)
  (test-approx "home heading" 0.0 (turtle-heading t2) 0.001)
  (test "home adds path" 4 (length (turtle-paths t2))))  ; move-to + 2 fd + home

(let* ([t (fd (fd (make-turtle) 100) 50)]
       [t2 (cs t)])
  (test-approx "cs x" 320.0 (turtle-x t2) 0.001)
  (test "cs clears paths" 0 (length (turtle-paths t2))))

(test-section "Turtle Color")

(let* ([t (make-turtle)]
       [t2 (setpc t 'blue)])
  (test "setpc blue" 15 (color12-b (turtle-pen-color t2))))

(let* ([t (make-turtle)]
       [t2 (setpc t #xF00)])
  (test "setpc int" 15 (color12-r (turtle-pen-color t2))))

;;; ============================================================
;;; Drawing Tests
;;; ============================================================

(test-section "Drawing Extraction")

(let* ([t (fd (make-turtle) 100)]
       [d (turtle->drawing t)])
  (test "drawing width" 640 (drawing-width d))
  (test "drawing height" 480 (drawing-height d))
  (test "drawing has paths" 2 (length (drawing-paths d))))  ; move-to + line-to

;;; ============================================================
;;; SVG Generation Tests
;;; ============================================================

(test-section "SVG Structure")

(let ([header (svg-header 640 480)])
  (test "header has xml decl" #t (string-contains? header "<?xml"))
  (test "header has svg tag" #t (string-contains? header "<svg"))
  (test "header has width" #t (string-contains? header "640"))
  (test "header has height" #t (string-contains? header "480")))

(let ([footer (svg-footer)])
  (test "footer closes svg" #t (string-contains? footer "</svg>")))

(test-section "Full SVG Output")

(let* ([t (fd (make-turtle) 100)]
       [d (turtle->drawing t)]
       [svg (drawing->svg d)])
  (test "svg has header" #t (string-contains? svg "<svg"))
  (test "svg has background" #t (string-contains? svg "<rect"))
  (test "svg has path" #t (string-contains? svg "<path"))
  (test "svg closes" #t (string-contains? svg "</svg>")))

;;; ============================================================
;;; Shape Tests
;;; ============================================================

(test-section "Shapes")

(let* ([t (make-turtle)]
       [t2 (circle t 50)]
       [paths (turtle-paths t2)])
  (test "circle adds path" 1 (length paths))
  (test "circle cmd" #t (circle? (car paths))))

(let* ([t (make-turtle)]
       [t2 (polygon t 4 50)]  ; Square
       [paths (turtle-paths t2)])
  (test "polygon adds path" 1 (length paths))
  (test "polygon cmd" #t (polygon? (car paths)))
  (test "polygon points" 4 (length (polygon-points (car paths)))))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(test-section "Convenience Shapes")

(let* ([t (make-turtle)]
       [t2 (square t 50)]
       [paths (turtle-paths t2)])
  ;; Square draws: initial move-to + 4 lines
  (test "square draws" 5 (length paths)))

(let* ([t (make-turtle)]
       [t2 (star t 50)]
       [paths (turtle-paths t2)])
  ;; Star draws: initial move-to + 5 lines
  (test "star draws" 6 (length paths)))

;;; ============================================================
;;; Summary
;;; ============================================================

(test-summary)
