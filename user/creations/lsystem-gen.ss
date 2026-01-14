;;; lsystem-gen.ss - L-System Fractal Generator
;;; Generates fractal patterns using Lindenmayer systems

;;; ====
;;; L-SYSTEM ENGINE
;;; ====

(define (apply-rule rules char)
  "Apply production rules to a single character"
  (let ((rule (assoc char rules)))
       (if rule
           (string->list (cdr rule))
           (list char))))

(define (lsystem-step rules state)
  "Apply all production rules to generate next iteration"
  (apply append
         (map (lambda (c) (apply-rule rules c))
              (string->list state))))

(define (lsystem-generate rules axiom iterations)
  "Generate L-system string after n iterations"
  (let loop ((n 0) (state axiom))
       (if (>= n iterations)
           state
           (loop (+ n 1)
                 (list->string (lsystem-step rules state))))))

;;; ====
;;; TURTLE GRAPHICS INTERPRETER
;;; ====

(define (deg->rad deg)
  (* deg (/ 3.141592653589793 180.0)))

(define (interpret-lsystem str angle)
  "Interpret L-system string as turtle graphics commands
   F = forward, + = turn right, - = turn left, [ = push, ] = pop"
  (let loop ((chars (string->list str))
             (x 0.0) (y 0.0)
             (heading 90.0)
             (stack '())
             (path (list (cons 0.0 0.0))))
       (if (null? chars)
           (reverse path)
           (let ((cmd (car chars)))
                (case cmd
                      ((#\F)
                       (let* ((rad (deg->rad heading))
                              (new-x (+ x (cos rad)))
                              (new-y (+ y (sin rad))))
                             (loop (cdr chars) new-x new-y heading stack
                                   (cons (cons new-x new-y) path))))
                      ((#\+)
                       (loop (cdr chars) x y (- heading angle) stack path))
                      ((#\-)
                       (loop (cdr chars) x y (+ heading angle) stack path))
                      ((#\[)
                       (loop (cdr chars) x y heading
                             (cons (list x y heading) stack) path))
                      ((#\])
                       (if (null? stack)
                           (loop (cdr chars) x y heading stack path)
                           (let ((state (car stack)))
                                (loop (cdr chars)
                                      (car state)
                                      (cadr state)
                                      (caddr state)
                                      (cdr stack)
                                      path))))
                      (else
                       (loop (cdr chars) x y heading stack path)))))))

;;; ====
;;; ASCII RENDERING
;;; ====

(define (path-bounds path)
  "Find min/max coordinates in path"
  (let loop ((pts path)
             (min-x 0.0) (max-x 0.0)
             (min-y 0.0) (max-y 0.0))
       (if (null? pts)
           (list min-x max-x min-y max-y)
           (let* ((pt (car pts))
                  (x (car pt))
                  (y (cdr pt)))
                 (loop (cdr pts)
                       (min min-x x) (max max-x x)
                       (min min-y y) (max max-y y))))))

(define (render-path-ascii path width height)
  "Render path as ASCII art"
  (let* ((bounds (path-bounds path))
         (min-x (car bounds))
         (max-x (cadr bounds))
         (min-y (caddr bounds))
         (max-y (cadddr bounds))
         (range-x (- max-x min-x))
         (range-y (- max-y min-y))
         (grid (make-vector (* width height) #\space)))
        
        ;; Plot path points
        (for-each
         (lambda (pt)
                 (let* ((x (car pt))
                        (y (cdr pt))
                        (norm-x (if (zero? range-x) 0.5
                                    (/ (- x min-x) range-x)))
                        (norm-y (if (zero? range-y) 0.5
                                    (/ (- y min-y) range-y)))
                        (grid-x (inexact->exact
                                 (floor (* norm-x (- width 1)))))
                        (grid-y (inexact->exact
                                 (floor (* norm-y (- height 1)))))
                        (idx (+ grid-x (* grid-y width))))
                       (when (and (>= idx 0) (< idx (* width height)))
                             (vector-set! grid idx #\*))))
         path)
        
        ;; Render grid
        (let loop ((y (- height 1)) (lines '()))
             (if (< y 0)
                 (apply string-append lines)
                 (loop (- y 1)
                       (cons
                        (string-append
                         (list->string
                          (let row ((x 0) (chars '()))
                               (if (>= x width)
                                   (reverse chars)
                                   (row (+ x 1)
                                        (cons (vector-ref grid (+ x (* y width)))
                                              chars)))))
                         "\n")
                        lines))))))

;;; ====
;;; CLASSIC L-SYSTEMS
;;; ====

(define algae-rules
  '((#\A . "AB")
    (#\B . "A")))

(define koch-curve-rules
  '((#\F . "F+F-F-F+F")))

(define sierpinski-triangle-rules
  '((#\F . "F-G+F+G-F")
    (#\G . "GG")))

(define dragon-curve-rules
  '((#\F . "F+G")
    (#\G . "F-G")))

(define plant-rules
  '((#\F . "FF")
    (#\X . "F+[[X]-X]-F[-FX]+X")))

;;; ====
;;; DEMONSTRATIONS
;;; ====

(define (show-lsystem name rules axiom angle iters width height)
  (display (string-append "\n=== " name " ===\n"))
  (let* ((pattern (lsystem-generate rules axiom iters))
         (path (interpret-lsystem pattern angle)))
        (display (string-append "Iteration " (number->string iters)
                                ", length " (number->string (string-length pattern))
                                "\n\n"))
        (display (render-path-ascii path width height))
        (display "\n")))

(define (lsystem-demo)
  (display "\n=== L-SYSTEM FRACTAL GENERATOR ===\n")
  (display "Generating fractal patterns using Lindenmayer systems...\n")
  
  (show-lsystem "KOCH SNOWFLAKE"
                koch-curve-rules "F" 90 3 40 20)
  
  (show-lsystem "DRAGON CURVE"
                dragon-curve-rules "F" 90 10 40 30)
  
  (show-lsystem "PLANT GROWTH (Fractal Tree)"
                plant-rules "X" 25 5 50 25)
  
  (display "\nL-Systems available:\n")
  (display "  algae-rules, koch-curve-rules\n")
  (display "  sierpinski-triangle-rules, dragon-curve-rules\n")
  (display "  plant-rules\n\n")
  (display "Try: (lsystem-generate plant-rules \"X\" 6)\n"))

;; Run demo
(lsystem-demo)
