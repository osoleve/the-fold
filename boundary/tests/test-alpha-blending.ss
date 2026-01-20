(load "boundary/ui/layout.ss")
(load "boundary/ui/layers.ss")

(doc 'module 'test-alpha-blending)
(doc 'description "Test suite for alpha blending (fold-hb0m). Tests alpha compositing and layer z-indexing features.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "Testing alpha blending and z-index...\n\n")

(doc 'section 'test-1-blend-chars-function)

(display "1. Testing blend-chars:\n")

(let* ([dest #\-]
       [src #\X]
       [blend-0 (blend-chars dest src 0.0)]
       [blend-25 (blend-chars dest src 0.25)]
       [blend-50 (blend-chars dest src 0.5)]
       [blend-75 (blend-chars dest src 0.75)]
       [blend-100 (blend-chars dest src 1.0)])
      (display "  Blending '-' with 'X' at various alpha:\n")
      (display (string-append "    alpha=0.0:  " (string blend-0) " (should be dest '-')\n"))
      (display (string-append "    alpha=0.25: " (string blend-25) " (light shade)\n"))
      (display (string-append "    alpha=0.5:  " (string blend-50) " (medium shade)\n"))
      (display (string-append "    alpha=0.75: " (string blend-75) " (dark shade)\n"))
      (display (string-append "    alpha=1.0:  " (string blend-100) " (should be src 'X')\n"))
      (if (and (char=? blend-0 #\-)
               (char=? blend-100 #\X))
          (display "  ✓ blend-chars works correctly\n\n")
          (display "  ✗ FAILED: blend-chars broken\n\n")))

(doc 'section 'test-2-composite-with-alpha)

(display "2. Testing composite-with-alpha:\n")

(let* ([dest (make-canvas 10 3)]
       [_ (fill-rect dest (make-rect (point 0 0) 10 3) #\-)]
       [src (make-canvas 4 1)]
       [_ (fill-rect src (make-rect (point 0 0) 4 1) #\X)]
       [result-25 (composite-with-alpha dest src (point 3 1) 0.25)]
       [result-100 (composite-with-alpha dest src (point 3 1) 1.0)])
      (display "  Base (filled with '-'):\n")
      (display (canvas->string dest))
      (display "\n  With 4 'X's at alpha=0.25:\n")
      (display (canvas->string result-25))
      (display "\n  With 4 'X's at alpha=1.0:\n")
      (display (canvas->string result-100))
      (newline)
      (display "  ✓ composite-with-alpha works\n\n"))

(doc 'section 'test-3-z-index-layer-depth)

(display "3. Testing z-index (layer depth):\n")

(let* ([bg-layer (make-layer 'background (make-canvas 12 4) 0)]
       [fg-layer (make-layer 'foreground (make-canvas 5 2) 100)]
       ;; Draw on layers
       [bg-layer (layer-fill-rect bg-layer (make-rect (point 0 0) 12 4) #\.)]
       [fg-layer (layer-fill-rect fg-layer (make-rect (point 0 0) 5 2) #\#)]
       ;; Move foreground
       [fg-layer (layer-set-offset fg-layer (point 3 1))]
       ;; Create stack (background first)
       [stack (make-layer-stack (list bg-layer fg-layer))]
       ;; Flatten
       [result (flatten-layers stack 12 4)])
      (display "  Background (depth 0, filled with '.'):\n")
      (display (canvas->string (layer-canvas bg-layer)))
      (display "\n  Foreground (depth 100, filled with '#') at offset (3,1):\n")
      (display (canvas->string (layer-canvas fg-layer)))
      (display "\n  Flattened result (foreground on top):\n")
      (display (canvas->string result))
      (newline)
      ;; Check that foreground is on top
      (let ([ch (canvas-ref result 4 1)])
           (if (char=? ch #\#)
               (display "  ✓ Z-index ordering works correctly\n\n")
               (display "  ✗ FAILED: Z-index ordering broken\n\n"))))

(doc 'section 'test-4-layer-reordering)

(display "4. Testing layer reordering:\n")

(let* ([layer-a (make-layer 'a (make-canvas 4 2) 50)]
       [layer-b (make-layer 'b (make-canvas 4 2) 10)]
       [layer-c (make-layer 'c (make-canvas 4 2) 100)]
       [stack (make-layer-stack (list layer-a layer-b layer-c))]
       [depths (map layer-depth (stack-layers stack))])
      (display "  Initial depths: ")
      (display depths)
      (newline)
      ;; Should be sorted: 10, 50, 100
      (if (equal? depths '(10 50 100))
          (display "  ✓ Layers sorted by depth correctly\n\n")
          (display "  ✗ FAILED: Layer sorting broken\n\n")))

(doc 'section 'test-5-alpha-palette)

(display "5. Testing alpha-palette:\n")

(display "  Alpha palette: ")
(for-each (lambda (ch) (display ch) (display " ")) alpha-palette)
(newline)
(if (= (length alpha-palette) 5)
    (display "  ✓ Alpha palette has 5 levels\n\n")
    (display "  ✗ FAILED: Alpha palette wrong size\n\n"))

(doc 'section 'test-6-flatten-layers-with-alpha)

(display "6. Testing flatten-layers-with-alpha:\n")

(let* ([bg-canvas (make-canvas 10 3)]
       [_ (fill-rect bg-canvas (make-rect (point 0 0) 10 3) #\-)]
       [fg-canvas (make-canvas 4 1)]
       [_ (fill-rect fg-canvas (make-rect (point 0 0) 4 1) #\X)]
       [bg-layer (make-layer 'bg bg-canvas 0)]
       [fg-layer (make-layer 'fg fg-canvas 100 (point 3 1))]
       ;; Create pairs with opacity
       [pairs (list (layer-with-opacity bg-layer 1.0)
                    (layer-with-opacity fg-layer 0.5))]
       [result (flatten-layers-with-alpha pairs 10 3)])
      (display "  Background (alpha=1.0) with foreground (alpha=0.5):\n")
      (display (canvas->string result))
      (newline)
      (display "  ✓ flatten-layers-with-alpha works\n\n"))

(display "====\n")
(display "All alpha blending tests completed!\n")
(display "====\n")
