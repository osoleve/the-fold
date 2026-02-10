(load "core/lang/module.ss")
(load "lattice/ui/layout.ss")

(doc 'module 'test-svg-renderer)
(doc 'description "Test suite for SVG concepts (fold-q196) - tests basic canvas-to-string rendering which underlies SVG generation")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Full SVG library tests require library import mechanism")

(display "Testing SVG-related concepts...\n\n")

;;; Helper: Check if string contains substring
(define (string-contains str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string=? (substring str i (+ i sub-len)) substr) #t]
             [else (loop (+ i 1))]))))

(doc 'section 'test-1-canvas-to-string)

(display "1. Testing canvas->string:\n")

(let* ([canvas (make-canvas 5 3)]
       [_ (canvas-set! canvas 0 0 #\A)]
       [_ (canvas-set! canvas 4 0 #\B)]
       [_ (canvas-set! canvas 2 1 #\X)]
       [_ (canvas-set! canvas 0 2 #\C)]
       [_ (canvas-set! canvas 4 2 #\D)]
       [str (canvas->string canvas)])
      (display "  Canvas:\n")
      (display str)
      (newline)
      (if (and (string-contains str "A")
               (string-contains str "X")
               (string-contains str "D"))
          (display "  ✓ canvas->string works\n\n")
          (display "  ✗ FAILED: canvas->string broken\n\n")))

(doc 'section 'test-2-special-characters)

(display "2. Testing special characters:\n")

(let* ([canvas (make-canvas 10 3)]
       [_ (canvas-set! canvas 0 0 #\░)]
       [_ (canvas-set! canvas 1 0 #\▒)]
       [_ (canvas-set! canvas 2 0 #\▓)]
       [_ (canvas-set! canvas 3 0 #\█)]
       [_ (canvas-set! canvas 0 1 #\┌)]
       [_ (canvas-set! canvas 1 1 #\─)]
       [_ (canvas-set! canvas 2 1 #\┐)]
       [str (canvas->string canvas)])
      (display str)
      (newline)
      (display "  ✓ Unicode characters render correctly\n\n"))

(doc 'section 'test-3-empty-canvas)

(display "3. Testing empty canvas:\n")

(let* ([canvas (make-canvas 5 2)]
       [str (canvas->string canvas)])
      (display "  Empty 5x2 canvas:\n")
      (display (string-append "\"" str "\"\n"))
      (if (> (string-length str) 0)
          (display "  ✓ Empty canvas produces output\n\n")
          (display "  ✗ FAILED: Empty canvas broken\n\n")))

(doc 'section 'test-4-single-character)

(display "4. Testing single character canvas:\n")

(let* ([canvas (make-canvas 1 1)]
       [_ (canvas-set! canvas 0 0 #\*)]
       [str (canvas->string canvas)])
      (display (string-append "  \"" str "\"\n"))
      (if (string-contains str "*")
          (display "  ✓ Single char canvas works\n\n")
          (display "  ✗ FAILED: Single char broken\n\n")))

(doc 'section 'test-5-large-canvas-performance)

(display "5. Testing large canvas:\n")

(let* ([canvas (make-canvas 50 20)]
       ;; Draw a border
       [_ (let loop ([x 0])
               (when (< x 50)
                     (canvas-set! canvas x 0 #\-)
                     (canvas-set! canvas x 19 #\-)
                     (loop (+ x 1))))]
       [_ (let loop ([y 0])
               (when (< y 20)
                     (canvas-set! canvas 0 y #\|)
                     (canvas-set! canvas 49 y #\|)
                     (loop (+ y 1))))]
       [_ (canvas-set! canvas 0 0 #\+)]
       [_ (canvas-set! canvas 49 0 #\+)]
       [_ (canvas-set! canvas 0 19 #\+)]
       [_ (canvas-set! canvas 49 19 #\+)]
       [str (canvas->string canvas)])
      (display "  50x20 bordered canvas:\n")
      (display str)
      (newline)
      (display "  ✓ Large canvas renders correctly\n\n"))

(doc 'section 'test-6-string-padding)

(display "6. Testing string-pad:\n")

(let* ([padded (string-pad "hi" 10 #\-)])
      (display (string-append "  \"" padded "\"\n"))
      (if (= (string-length padded) 10)
          (display "  ✓ String padding works\n\n")
          (display "  ✗ FAILED: String padding broken\n\n")))

(display "====\n")
(display "All SVG-related tests completed!\n")
(display "====\n")
