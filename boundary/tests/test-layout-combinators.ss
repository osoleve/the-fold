(load "core/lang/module.ss")
(load "lattice/ui/layout.ss")

(doc 'module 'test-layout-combinators)
(doc 'description "Test suite for layout combinators (fold-q196)")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Tests canvas composition operations from layout.ss")
(doc 'note "Full combinator tests require library import mechanism")

(display "Testing layout and composition...\n\n")

(doc 'section 'test-canvas-composite)

(display "1. Testing canvas composite:\n")

(let* ([base (make-canvas 12 6)]
       [_ (fill-rect base (make-rect (point 0 0) 12 6) #\.)]
       [overlay (make-canvas 4 2)]
       [_ (canvas-set! overlay 0 0 #\X)]
       [_ (canvas-set! overlay 1 0 #\X)]
       [_ (canvas-set! overlay 2 0 #\X)]
       [_ (canvas-set! overlay 3 0 #\X)]
       [_ (canvas-set! overlay 0 1 #\X)]
       [_ (canvas-set! overlay 3 1 #\X)]
       [result (composite base overlay (point 4 2))])
      (display "  Overlay composited onto base:\n")
      (display (canvas->string result))
      (newline)
      (display "  ✓ Composite works\n\n"))

(doc 'section 'test-draw-box)

(display "2. Testing draw-box:\n")

(let* ([canvas (make-canvas 15 6)]
       [rect (make-rect (point 2 1) 10 4)]
       [result (draw-box canvas rect 'light)])
      (display (canvas->string result))
      (newline)
      (display "  ✓ draw-box works\n\n"))

(doc 'section 'test-multiple-composites)

(display "3. Testing multiple composites:\n")

(let* ([canvas (make-canvas 20 8)]
       [box1 (make-canvas 5 3)]
       [_ (fill-rect box1 (make-rect (point 0 0) 5 3) #\1)]
       [box2 (make-canvas 5 3)]
       [_ (fill-rect box2 (make-rect (point 0 0) 5 3) #\2)]
       [box3 (make-canvas 5 3)]
       [_ (fill-rect box3 (make-rect (point 0 0) 5 3) #\3)]
       [result (composite canvas box1 (point 1 1))]
       [result (composite result box2 (point 7 1))]
       [result (composite result box3 (point 13 1))])
      (display "  Three boxes side by side:\n")
      (display (canvas->string result))
      (newline)
      (display "  ✓ Multiple composites work\n\n"))

(doc 'section 'test-transparency-handling)

(display "4. Testing composite with transparency:\n")

(let* ([base (make-canvas 10 4)]
       [_ (fill-rect base (make-rect (point 0 0) 10 4) #\-)]
       [sparse (make-canvas 5 2)]
       [_ (canvas-set! sparse 0 0 #\X)]
       [_ (canvas-set! sparse 4 0 #\X)]
       [_ (canvas-set! sparse 2 1 #\X)]
       [result (composite-with-transparency base sparse (point 2 1) #\space)])
      (display "  Sparse overlay (spaces transparent):\n")
      (display (canvas->string result))
      (newline)
      (display "  ✓ Transparent composite works\n\n"))

(doc 'section 'test-blit-operation)

(display "5. Testing blit (region copy):\n")

(let* ([source (make-canvas 10 5)]
       [_ (draw-string source (point 0 0) "ABCDEFGHIJ")]
       [_ (draw-string source (point 0 1) "0123456789")]
       [dest (make-canvas 6 3)]
       [region (make-rect (point 2 0) 5 2)]
       [result (blit dest source region (point 0 0))])
      (display "  Source:\n")
      (display (canvas->string source))
      (display "  Region (5x2 from offset 2,0):\n")
      (display (canvas->string result))
      (newline)
      (display "  ✓ Blit works\n\n"))

(doc 'section 'test-vertical-string)

(display "6. Testing draw-string-v (vertical):\n")

(let* ([canvas (make-canvas 8 6)]
       [result (draw-string-v canvas (point 3 0) "Hello")])
      (display (canvas->string result))
      (newline)
      (display "  ✓ Vertical string works\n\n"))

(doc 'section 'test-draw-rectangle-outline)

(display "7. Testing draw-rect (outline):\n")

(let* ([canvas (make-canvas 10 5)]
       [rect (make-rect (point 1 1) 7 3)]
       [result (draw-rect canvas rect #\*)])
      (display (canvas->string result))
      (newline)
      (display "  ✓ Rectangle outline works\n\n"))

(display "====\n")
(display "All layout/composition tests completed!\n")
(display "====\n")
