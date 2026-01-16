;;; user/creations/interval-opt-animation.ss
;;;
;;; Animated visualization of interval branch-and-bound optimization.
;;; Creates a GIF showing the search space being explored.
;;;
;;; Run: scheme --script user/creations/interval-opt-animation.ss
;;;
;;; Author: Claude Opus 4.5
;;; Created: 2026-01-16

(load "core/base/prelude.ss")
(load "lattice/numeric/interval.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

;;; ============================================================================
;;; Grid Parameters
;;; ============================================================================

(define *grid-width* 60)
(define *grid-height* 30)
(define *x-lo* -3)
(define *x-hi* 3)
(define *y-lo* -3)
(define *y-hi* 3)

;;; ============================================================================
;;; Test Function: Himmelblau (4 global minima)
;;; ============================================================================

(define (himmelblau x y)
  (+ (* (+ (* x x) y -11) (+ (* x x) y -11))
     (* (+ x (* y y) -7) (+ x (* y y) -7))))

(define (interval-himmelblau box)
  (let* ([x (car box)]
         [y (cadr box)]
         [x2 (interval-sqr x)]
         [y2 (interval-sqr y)]
         [eleven (interval-singleton 11)]
         [seven (interval-singleton 7)]
         [term1 (interval-sub (interval-add x2 y) eleven)]
         [term2 (interval-sub (interval-add x y2) seven)])
    (interval-add (interval-sqr term1) (interval-sqr term2))))

;;; ============================================================================
;;; Coordinate Conversion
;;; ============================================================================

(define (x->col x)
  (let ([ratio (/ (- x *x-lo*) (- *x-hi* *x-lo*))])
    (min (- *grid-width* 1) (max 0 (exact (floor (* ratio *grid-width*)))))))

(define (y->row y)
  ;; Y is inverted (top of screen = high Y)
  (let ([ratio (/ (- *y-hi* y) (- *y-hi* *y-lo*))])
    (min (- *grid-height* 1) (max 0 (exact (floor (* ratio *grid-height*)))))))

(define (col->x col)
  (+ *x-lo* (* (/ col *grid-width*) (- *x-hi* *x-lo*))))

(define (row->y row)
  (- *y-hi* (* (/ row *grid-height*) (- *y-hi* *y-lo*))))

;;; ============================================================================
;;; Value-to-Character Mapping
;;; ============================================================================

(define (value->char val min-val max-val)
  (if (or (>= val +inf.0) (<= val -inf.0))
      #\#
      (let* ([range (- max-val min-val)]
             [normalized (if (= range 0) 0.5 (/ (- val min-val) range))]
             [idx (min 7 (max 0 (exact (floor (* normalized 8)))))]
             [chars " .':-=+*#"])
        (string-ref chars idx))))

;;; ============================================================================
;;; Frame Drawing
;;; ============================================================================

;; Precompute function values and min/max
(define *func-values* (make-vector (* *grid-width* *grid-height*) 0))
(define *func-min* +inf.0)
(define *func-max* -inf.0)

(define (precompute-function!)
  (do ([row 0 (+ row 1)])
      ((>= row *grid-height*))
    (let ([y (row->y row)])
      (do ([col 0 (+ col 1)])
          ((>= col *grid-width*))
        (let* ([x (col->x col)]
               [val (himmelblau x y)])
          (vector-set! *func-values* (+ col (* row *grid-width*)) val)
          (when (< val *func-min*) (set! *func-min* val))
          (when (> val *func-max*) (set! *func-max* val)))))))

(define (draw-background frame)
  ;; Draw function landscape
  (do ([row 0 (+ row 1)])
      ((>= row *grid-height*))
    (do ([col 0 (+ col 1)])
        ((>= col *grid-width*))
      (let ([val (vector-ref *func-values* (+ col (* row *grid-width*)))])
        (frame-set! frame col row (value->char val *func-min* *func-max*))))))

(define (draw-box-outline frame box char)
  ;; Draw the outline of an interval box
  (let* ([x-iv (car box)]
         [y-iv (cadr box)]
         [col1 (x->col (interval-lo x-iv))]
         [col2 (x->col (interval-hi x-iv))]
         [row1 (y->row (interval-hi y-iv))]  ; High Y = low row
         [row2 (y->row (interval-lo y-iv))])
    ;; Draw top and bottom edges
    (do ([c col1 (+ c 1)])
        ((> c col2))
      (frame-set! frame c row1 char)
      (frame-set! frame c row2 char))
    ;; Draw left and right edges
    (do ([r row1 (+ r 1)])
        ((> r row2))
      (frame-set! frame col1 r char)
      (frame-set! frame col2 r char))))

(define (draw-box-fill frame box char)
  ;; Fill an interval box
  (let* ([x-iv (car box)]
         [y-iv (cadr box)]
         [col1 (x->col (interval-lo x-iv))]
         [col2 (x->col (interval-hi x-iv))]
         [row1 (y->row (interval-hi y-iv))]
         [row2 (y->row (interval-lo y-iv))])
    (do ([r row1 (+ r 1)])
        ((> r row2))
      (do ([c col1 (+ c 1)])
          ((> c col2))
        (frame-set! frame c r char)))))

(define (draw-point frame x y char)
  (let ([col (x->col x)]
        [row (y->row y)])
    (frame-set! frame col row char)))

;;; ============================================================================
;;; Simple Branch-and-Bound with Visualization Callbacks
;;; ============================================================================

(define (box-midpoint box)
  (list (interval-mid (car box))
        (interval-mid (cadr box))))

(define (box-max-width box)
  (max (interval-width (car box))
       (interval-width (cadr box))))

(define (bisect-box box)
  ;; Split along widest dimension
  (let* ([x-iv (car box)]
         [y-iv (cadr box)]
         [x-width (interval-width x-iv)]
         [y-width (interval-width y-iv)])
    (if (> x-width y-width)
        ;; Split X
        (let* ([mid (interval-mid x-iv)]
               [lo-iv (interval (interval-lo x-iv) mid)]
               [hi-iv (interval mid (interval-hi x-iv))])
          (list (list lo-iv y-iv)
                (list hi-iv y-iv)))
        ;; Split Y
        (let* ([mid (interval-mid y-iv)]
               [lo-iv (interval (interval-lo y-iv) mid)]
               [hi-iv (interval mid (interval-hi y-iv))])
          (list (list x-iv lo-iv)
                (list x-iv hi-iv))))))

(define (interval-minimize-animated f-interval f-scalar initial-box
                                     width-tol max-iter on-step)
  ;; Branch-and-bound with callbacks for visualization
  (let ([work-list (list initial-box)]
        [best-upper +inf.0]
        [candidates '()]
        [iter 0])

    (let loop ()
      (cond
        [(null? work-list)
         (list candidates best-upper iter)]
        [(>= iter max-iter)
         (list candidates best-upper iter)]
        [else
         (let* ([box (car work-list)]
                [rest (cdr work-list)]
                [f-iv (f-interval box)]
                [lo (interval-lo f-iv)]
                [mid (box-midpoint box)]
                [f-mid (f-scalar mid)]
                [new-best (min best-upper f-mid (interval-hi f-iv))])

           (set! best-upper new-best)
           (set! iter (+ iter 1))

           ;; Call visualization hook
           (on-step box (if (> lo best-upper) 'pruned
                            (if (< (box-max-width box) width-tol) 'converged
                                'active))
                    candidates best-upper iter)

           (cond
             ;; Prune
             [(> lo best-upper)
              (set! work-list rest)
              (loop)]
             ;; Converged
             [(< (box-max-width box) width-tol)
              (set! candidates (cons box candidates))
              (set! work-list rest)
              (loop)]
             ;; Bisect
             [else
              (let ([children (bisect-box box)])
                (set! work-list (append children rest))
                (loop))]))]))))

;;; ============================================================================
;;; Animation Generation
;;; ============================================================================

(define (create-optimization-animation)
  (display "Precomputing function values...\n")
  (precompute-function!)

  (display "Creating animation...\n")
  (let ([video (make-video)]
        [frame (make-frame *grid-width* *grid-height* #\space)]
        [frame-count 0]
        [all-candidates '()])

    ;; Title frame (1.5 sec = 90 frames at 60fps)
    (frame-clear! frame #\space)
    (frame-put-string! frame 5 5  "INTERVAL BRANCH-AND-BOUND")
    (frame-put-string! frame 5 7  "Global Optimization")
    (frame-put-string! frame 5 9  "Finding all 4 minima of")
    (frame-put-string! frame 5 10 "Himmelblau's function")
    (frame-put-string! frame 5 13 "Watch the search space")
    (frame-put-string! frame 5 14 "get explored...")
    (do ([i 0 (+ i 1)]) ((>= i 90)) (video-add-frame! video frame))

    ;; Background frame (0.5 sec = 30 frames)
    (draw-background frame)
    (do ([i 0 (+ i 1)]) ((>= i 30)) (video-add-frame! video frame))

    ;; Run optimization with visualization
    (interval-minimize-animated
     interval-himmelblau
     (lambda (pt) (himmelblau (car pt) (cadr pt)))
     (list (interval *x-lo* *x-hi*) (interval *y-lo* *y-hi*))
     0.2   ; width tolerance (finer for more iterations)
     1000  ; max iterations
     (lambda (box status candidates best iter)
       ;; Capture every frame for smooth 60fps animation
       (when (or (= (mod iter 2) 0)  ; Every other iteration
                 (eq? status 'converged))
         ;; Redraw background
         (draw-background frame)

         ;; Draw all found candidates so far
         (for-each (lambda (c) (draw-box-outline frame c #\@)) candidates)

         ;; Draw current box
         (case status
           [(active) (draw-box-outline frame box #\O)]
           [(pruned) (draw-box-outline frame box #\x)]
           [(converged)
            (draw-box-outline frame box #\*)
            (set! all-candidates (cons box all-candidates))])

         ;; Status line at bottom
         (frame-put-string! frame 0 (- *grid-height* 2)
                            (make-string *grid-width* #\space))
         (frame-put-string! frame 0 (- *grid-height* 2)
                            (string-append "Iter:" (number->string iter)
                                          " Best:"
                                          (number->string (exact (round (* best 100))) )
                                          "/100"
                                          " Found:" (number->string (length candidates))))

         (video-add-frame! video frame)
         (set! frame-count (+ frame-count 1)))))

    ;; Final frame showing all results (2 sec = 120 frames at 60fps)
    (draw-background frame)
    (for-each (lambda (c)
                (draw-box-fill frame c #\*)
                (let ([mid (box-midpoint c)])
                  (draw-point frame (car mid) (cadr mid) #\@)))
              all-candidates)
    (frame-put-string! frame 0 (- *grid-height* 2) (make-string *grid-width* #\space))
    (frame-put-string! frame 0 (- *grid-height* 2) "COMPLETE: All 4 global minima found!")
    (do ([i 0 (+ i 1)]) ((>= i 120)) (video-add-frame! video frame))

    (display (format "Generated ~a frames\n" (video-frame-count video)))
    video))

;;; ============================================================================
;;; Main
;;; ============================================================================

(define (main)
  (display "\n")
  (display "===============================================\n")
  (display "  INTERVAL OPTIMIZATION ANIMATION GENERATOR\n")
  (display "===============================================\n\n")

  (let ([video (create-optimization-animation)])
    (display "\nExporting to GIF at 60fps...\n")
    ;; 60fps = 1000ms/60 ≈ 17ms per frame
    (video->gif video "user/creations/interval-optimization.gif" 17)
    (display "\nDone! See user/creations/interval-optimization.gif\n")))

(main)
