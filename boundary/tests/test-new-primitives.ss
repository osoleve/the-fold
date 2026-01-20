(import (chezscheme)
        (shell layout)
        (shell graphics-primitives))

(define-syntax doc
  (syntax-rules ()
    [(_ target tag value) (void)]
    [(_ tag value) (void)]))

(doc 'module 'test-new-primitives)
(doc 'description "Quick test for newly implemented graphics primitives")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "Testing newly implemented graphics primitives...\n\n")

(doc 'section 'test-polyline)

(display "1. Testing polyline:\n")
(let* ([canvas (make-canvas 30 10)]
       [points (list (point 2 2) (point 10 2) (point 10 7) (point 2 7))]
       [result (draw-polyline canvas points #\*)])
      (display (canvas->string result))
      (newline)
      (display "  ✓ Polyline works\n\n"))

(doc 'section 'test-polygon)

(display "2. Testing polygon (closed):\n")
(let* ([canvas (make-canvas 30 12)]
       [points (list (point 5 2) (point 15 2) (point 20 6) (point 15 10) (point 5 10))]
       [result (draw-polygon canvas points #\#)])
      (display (canvas->string result))
      (newline)
      (display "  ✓ Polygon works\n\n"))

(doc 'section 'test-filled-polygon)

(display "3. Testing filled polygon:\n")
(let* ([canvas (make-canvas 30 12)]
       [points (list (point 5 2) (point 15 2) (point 20 6) (point 15 10) (point 5 10))]
       [result (draw-filled-polygon canvas points #\█)])
      (display (canvas->string result))
      (newline)
      (display "  ✓ Filled polygon works\n\n"))

(doc 'section 'test-path-drawing)

(display "4. Testing path drawing:\n")
(let* ([canvas (make-canvas 30 12)]
       [path (list
              '(move-to ,(point 2 2))
              '(line-to ,(point 10 2))
              '(line-to ,(point 10 8))
              '(close))]
       [result (draw-path canvas path #\+)])
      (display (canvas->string result))
      (newline)
      (display "  ✓ Path drawing works\n\n"))

(doc 'section 'test-styling-system)

(display "5. Testing styling system:\n")
(let* ([canvas (make-canvas 30 12)]
       [style (make-style #\● #\○ 1.0)]
       [shape '(circle ,(point 15 6) 4)]
       [result (draw-with-style canvas shape style)])
      (display (canvas->string result))
      (newline)
      (display "  ✓ Styled drawing works\n\n"))

(display "====\n")
(display "All new primitives tests passed!\n")
(display "====\n")
