(load "core/lang/module.ss")
(load "lattice/ui/layout.ss")

(doc 'module 'test-polygon-fill)
(doc 'description "Tests the Active Edge Table (AET) polygon fill algorithm concepts")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "The actual optimized implementation is in boundary/ui/graphics-primitives.ss")
(doc 'see "fold-k60j")

(display "Testing AET polygon fill concepts...\n\n")

(doc 'section 'aet-helper-functions)

(doc 'note "Mirrors graphics-primitives.ss")

(doc 'type "Point × Point → EdgeEntry | #f")
(doc 'description "Create an edge entry for the Active Edge Table")
(define (make-edge-entry p1 p2)
(define (make-edge-entry p1 p2)
  (let ([y1 (point-y p1)]
        [y2 (point-y p2)]
        [x1 (point-x p1)]
        [x2 (point-x p2)])
       (cond
        [(= y1 y2) #f]  ; horizontal edge
        [(< y1 y2)
         (list y1 y2 x1 (/ (- x2 x1) (- y2 y1)))]
        [else
         (list y2 y1 x2 (/ (- x1 x2) (- y1 y2)))])))

(define (edge-y-min e) (car e))
(define (edge-y-max e) (cadr e))
(define (edge-x e) (caddr e))
(define (edge-inverse-slope e) (cadddr e))

(define (build-edge-table points)
  (doc 'type "(List Point) → (List EdgeEntry)")
  (doc 'description "Build sorted edge table from polygon points")
  (let loop ([pts points]
             [first-pt (car points)]
             [result '()])
       (if (null? (cdr pts))
           (let ([entry (make-edge-entry (car pts) first-pt)])
                (list-sort (lambda (a b) (< (edge-y-min a) (edge-y-min b)))
                           (if entry (cons entry result) result)))
           (let ([entry (make-edge-entry (car pts) (cadr pts))])
                (loop (cdr pts)
                      first-pt
                      (if entry (cons entry result) result))))))

(doc 'section 'test-1-edge-entry-creation)

(display "1. Testing edge entry creation:\n")

(let* ([p1 (point 0 0)]
       [p2 (point 10 5)]
       [entry (make-edge-entry p1 p2)])
      (display "  Edge from (0,0) to (10,5):\n")
      (display (string-append "    y-min: " (number->string (edge-y-min entry)) "\n"))
      (display (string-append "    y-max: " (number->string (edge-y-max entry)) "\n"))
      (display (string-append "    x-start: " (number->string (edge-x entry)) "\n"))
      (display (string-append "    dx/dy: " (number->string (edge-inverse-slope entry)) "\n"))
      (if (and (= (edge-y-min entry) 0)
               (= (edge-y-max entry) 5)
               (= (edge-x entry) 0)
               (= (edge-inverse-slope entry) 2))
          (display "  ✓ Edge entry correct\n\n")
          (display "  ✗ FAILED: Edge entry incorrect\n\n")))

(doc 'section 'test-2-horizontal-edge-skipping)

(display "2. Testing horizontal edge skipping:\n")

(let* ([p1 (point 0 5)]
       [p2 (point 10 5)]
       [entry (make-edge-entry p1 p2)])
      (if (not entry)
          (display "  ✓ Horizontal edge correctly returns #f\n\n")
          (display "  ✗ FAILED: Should have returned #f\n\n")))

(doc 'section 'test-3-edge-table-triangle)

(display "3. Testing edge table for triangle:\n")

(let* ([triangle (list (point 5 0)    ; top
                       (point 0 10)   ; bottom-left
                       (point 10 10))] ; bottom-right
       [edge-table (build-edge-table triangle)])
      (display (string-append "  Number of edges: " (number->string (length edge-table)) "\n"))
      (display "  Edge y-mins (should be sorted): ")
      (for-each (lambda (e) (display (edge-y-min e)) (display " ")) edge-table)
      (newline)
      ;; Triangle has 3 edges, but bottom is horizontal, so 2 non-horizontal
      (if (= (length edge-table) 2)
          (display "  ✓ Edge table correctly built (2 non-horizontal edges)\n\n")
          (display "  ✗ FAILED: Wrong number of edges\n\n")))

(doc 'section 'test-4-edge-table-rectangle)

(display "4. Testing edge table for rectangle:\n")

(let* ([rect (list (point 0 0)
                   (point 10 0)
                   (point 10 5)
                   (point 0 5))]
       [edge-table (build-edge-table rect)])
      (display (string-append "  Number of edges: " (number->string (length edge-table)) "\n"))
      ;; Rectangle: 2 horizontal (skipped) + 2 vertical = 2 edges
      (display "  Edge y-mins: ")
      (for-each (lambda (e) (display (edge-y-min e)) (display " ")) edge-table)
      (newline)
      (if (= (length edge-table) 2)
          (display "  ✓ Rectangle edge table correct\n\n")
          (display "  ✗ FAILED: Wrong number of edges\n\n")))

(doc 'section 'test-5-x-intersection)

(display "5. Testing x-intersection along scanline:\n")

(let* ([p1 (point 0 0)]
       [p2 (point 10 10)]
       [entry (make-edge-entry p1 p2)]
       ;; At y=5, x should be 5 (45 degree line)
       [x-at-y5 (+ (edge-x entry) (* 5 (edge-inverse-slope entry)))])
      (display (string-append "  Edge (0,0)→(10,10) at y=5: x = " (number->string x-at-y5) "\n"))
      (if (= x-at-y5 5)
          (display "  ✓ X-intersection correct\n\n")
          (display "  ✗ FAILED: X-intersection wrong\n\n")))

(doc 'section 'test-6-sorted-edge-table)

(display "6. Testing edge table sorting by y-min:\n")

(let* ([pentagon (list (point 5 2)    ; top
                       (point 9 5)    ; right
                       (point 7 9)    ; bottom-right
                       (point 3 9)    ; bottom-left
                       (point 1 5))]  ; left
       [edge-table (build-edge-table pentagon)]
       [y-mins (map edge-y-min edge-table)])
      (display "  Pentagon y-mins: ")
      (for-each (lambda (y) (display y) (display " ")) y-mins)
      (newline)
      ;; Check if sorted
      (let check ([lst y-mins])
           (if (or (null? lst) (null? (cdr lst)))
               (display "  ✓ Edge table correctly sorted\n\n")
               (if (<= (car lst) (cadr lst))
                   (check (cdr lst))
                   (display "  ✗ FAILED: Not sorted\n\n")))))

(doc 'section 'test-7-visual-simple-fill)

(display "7. Visual test - manual scanline fill:\n")

;; Manually implement a simple fill to verify concept
(let* ([canvas (make-canvas 12 8)]
       ;; Fill a triangle manually using edge tracking
       ;; Triangle: top at (6,1), bottom-left (1,6), bottom-right (11,6)
       ;; Left edge: dx/dy = (1-6)/(6-1) = -1
       ;; Right edge: dx/dy = (11-6)/(6-1) = 1
       [_ (let fill-y ([y 1] [left-x 6.0] [right-x 6.0])
               (when (<= y 6)
                     (let fill-x ([x (inexact->exact (floor left-x))])
                          (when (<= x (inexact->exact (ceiling right-x)))
                                (canvas-set! canvas x y #\*)
                                (fill-x (+ x 1))))
                     (fill-y (+ y 1) (- left-x 1) (+ right-x 1))))])
      (display (canvas->string canvas))
      (newline)
      (display "  ✓ Manual scanline fill demonstrates concept\n\n"))

(display "====\n")
(display "All AET concept tests completed!\n")
(display "====\n")
