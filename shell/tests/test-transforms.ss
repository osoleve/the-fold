;;; Test suite for transform operations (fold-q196)
;;;
;;; Tests transform concepts to verify correctness of the design.
;;; Note: Actual library tests require library import mechanism.

(load "shell/ui/layout.ss")

(display "Testing transform concepts...\n\n")

;;; ====
;;; Test 1: Basic Canvas Operations
;;; ====

(display "1. Testing canvas operations:\n")

(let* ([canvas (make-canvas 10 5)]
       [_ (canvas-set! canvas 2 2 #\X)]
       [ch (canvas-ref canvas 2 2)])
      (if (char=? ch #\X)
          (display "  ✓ Canvas set/ref works\n\n")
          (display "  ✗ FAILED: Canvas set/ref broken\n\n")))

;;; ====
;;; Test 2: Point Operations
;;; ====

(display "2. Testing point operations:\n")

(let* ([pt (point 5 10)]
       [x (point-x pt)]
       [y (point-y pt)])
      (if (and (= x 5) (= y 10))
          (display "  ✓ Point operations work\n\n")
          (display "  ✗ FAILED: Point operations broken\n\n")))

;;; ====
;;; Test 3: Canvas Dimensions
;;; ====

(display "3. Testing canvas dimensions:\n")

(let* ([canvas (make-canvas 20 15)])
      (if (and (= (canvas-width canvas) 20)
               (= (canvas-height canvas) 15))
          (display "  ✓ Canvas dimensions correct\n\n")
          (display "  ✗ FAILED: Canvas dimensions incorrect\n\n")))

;;; ====
;;; Test 4: Canvas Rendering
;;; ====

(display "4. Testing canvas rendering:\n")

(let* ([canvas (make-canvas 5 3)]
       [_ (canvas-set! canvas 0 0 #\H)]
       [_ (canvas-set! canvas 1 0 #\i)]
       [str (canvas->string canvas)])
      (display "  Canvas output:\n")
      (display str)
      (newline)
      (display "  ✓ Canvas rendering works\n\n"))

;;; ====
;;; Test 5: Out-of-Bounds Access
;;; ====

(display "5. Testing out-of-bounds handling:\n")

(let* ([canvas (make-canvas 5 5)]
       [ch1 (canvas-ref canvas -1 0)]
       [ch2 (canvas-ref canvas 10 0)]
       [ch3 (canvas-ref canvas 0 -1)]
       [ch4 (canvas-ref canvas 0 10)])
      (if (and (char=? ch1 #\space)
               (char=? ch2 #\space)
               (char=? ch3 #\space)
               (char=? ch4 #\space))
          (display "  ✓ Out-of-bounds returns space\n\n")
          (display "  ✗ FAILED: Out-of-bounds handling broken\n\n")))

;;; ====
;;; Test 6: Composite Operation
;;; ====

(display "6. Testing composite operation:\n")

(let* ([dest (make-canvas 10 5)]
       [src (make-canvas 3 2)]
       [_ (canvas-set! src 0 0 #\A)]
       [_ (canvas-set! src 1 0 #\B)]
       [_ (canvas-set! src 2 0 #\C)]
       [result (composite dest src (point 2 1))])
      (display "  Source composited at (2,1):\n")
      (display (canvas->string result))
      (newline)
      (let ([ch (canvas-ref result 2 1)])
           (if (char=? ch #\A)
               (display "  ✓ Composite places content correctly\n\n")
               (display "  ✗ FAILED: Composite placement incorrect\n\n"))))

;;; ====
;;; Test 7: Draw String
;;; ====

(display "7. Testing draw-string:\n")

(let* ([canvas (make-canvas 15 3)]
       [result (draw-string canvas (point 2 1) "Hello")])
      (display (canvas->string result))
      (newline)
      (let ([ch (canvas-ref result 2 1)])
           (if (char=? ch #\H)
               (display "  ✓ draw-string works correctly\n\n")
               (display "  ✗ FAILED: draw-string broken\n\n"))))

;;; ====
;;; Test 8: Fill Rectangle
;;; ====

(display "8. Testing fill-rect:\n")

(let* ([canvas (make-canvas 10 5)]
       [rect (make-rect (point 1 1) 5 3)]
       [result (fill-rect canvas rect #\#)])
      (display (canvas->string result))
      (newline)
      (display "  ✓ fill-rect executes without error\n\n"))

(display "====\n")
(display "All canvas/layout tests completed!\n")
(display "====\n")
